#' Feishu Bitable ETL helpers (R translation of feishu.py)
#'
#' Provides functions:
#' - load_config(path)
#' - get_tenant_access_token(app_id, app_secret)
#' - fetch_records(app_token, bitable_app_token, table_id, limit)
#' - records_to_csv(records, filename)
#' - etl_pipeline(cfg)

library(jsonlite)
library(httr)
library(dplyr)
library(purrr)
library(readr)
library(tibble)

`%||%` <- function(a, b) if (is.null(a)) b else a

load_config <- function(path = "config.json") {
  jsonlite::fromJSON(path)
}

get_tenant_access_token <- function(app_id, app_secret) {
  url <- "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal/"
  resp <- httr::POST(
    url,
    body = list(app_id = app_id, app_secret = app_secret),
    encode = "json"
  )
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  if (!is.null(parsed$code) && parsed$code != 0) {
    stop(sprintf(
      "获取token失败: %s",
      jsonlite::toJSON(parsed, auto_unbox = TRUE)
    ))
  }
  parsed$tenant_access_token
}

fetch_records <- function(app_token, bitable_app_token, table_id, limit = 500) {
  url <- sprintf(
    "https://open.feishu.cn/open-apis/bitable/v1/apps/%s/tables/%s/records",
    bitable_app_token,
    table_id
  )
  headers <- httr::add_headers(Authorization = paste("Bearer", app_token))
  params <- list(page_size = limit)
  records <- list()
  page_token <- NULL

  repeat {
    if (!is.null(page_token)) {
      params$page_token <- page_token
    }
    resp <- httr::GET(url, headers, query = params)
    parsed <- httr::content(resp, as = "parsed", type = "application/json")
    if (!is.null(parsed$code) && parsed$code != 0) {
      stop(sprintf(
        "获取数据失败: %s",
        jsonlite::toJSON(parsed, auto_unbox = TRUE)
      ))
    }
    items <- parsed$data$items
    if (is.null(items) || length(items) == 0) {
      break
    }
    records <- c(records, items)
    page_token <- parsed$data$page_token
    if (is.null(page_token) || identical(page_token, "")) break
  }
  records
}

records_to_csv <- function(records, filename) {
  # If no records, create empty CSV
  if (length(records) == 0) {
    readr::write_csv(tibble::tibble(), filename)
    return(invisible(NULL))
  }

  # Collect field names
  fieldnames <- unique(unlist(lapply(records, function(rec) {
    if (!is.null(rec$fields)) names(rec$fields) else NULL
  })))

  cols <- c("record_id", fieldnames)

  rows <- lapply(records, function(rec) {
    row <- as.list(setNames(rep(NA_character_, length(cols)), cols))
    row[["record_id"]] <- rec$record_id %||% NA_character_
    if (!is.null(rec$fields)) {
      fnames <- names(rec$fields)
      for (n in fnames) {
        # convert atomic values to character for CSV stability
        val <- rec$fields[[n]]
        if (is.atomic(val) && length(val) == 1) {
          row[[n]] <- as.character(val)
        } else {
          # If val is a named atomic vector, jsonlite will warn that named
          # vectors will be converted to arrays in future versions. Convert
          # named atomic vectors to lists so jsonlite emits a JSON object.
          val_for_json <- val
          if (is.atomic(val) && !is.null(names(val))) {
            val_for_json <- as.list(val)
          }
          # toJSON may still produce an object of class 'json'; coerce to
          # plain character to avoid vctrs/dplyr class mismatches on bind_rows.
          row[[n]] <- as.character(jsonlite::toJSON(
            val_for_json,
            auto_unbox = TRUE
          ))
        }
      }
    }
    tibble::as_tibble(row)
  })

  df <- dplyr::bind_rows(rows)
  readr::write_csv(df, filename)
  invisible(df)
}

etl_pipeline <- function(cfg) {
  start_time <- Sys.time()
  result <- list(
    success = FALSE,
    duration = 0,
    start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
    message = "",
    details = ""
  )

  tryCatch(
    {
      token <- get_tenant_access_token(cfg$app_id, cfg$app_secret)
      message("===== [ETL START] =====")

      source_records <- fetch_records(
        token,
        cfg$bitable_app_token,
        cfg$source_table
      )
      message(sprintf("[INFO] 抓取源表 %d 条", length(source_records)))

      if (!is.null(cfg$csv_output) && cfg$csv_output) {
        csv_filename <- cfg$csv_file_name %||% "output.csv"
        records_to_csv(source_records, csv_filename)
        message(sprintf("[INFO] 已将记录保存至 %s", csv_filename))
      }

      result$success <- TRUE
    },
    error = function(e) {
      result$message <- conditionMessage(e)
      message(sprintf("[ERROR] ETL流程执行失败: %s", result$message))
      stop(e)
    },
    finally = {
      end_time <- Sys.time()
      result$duration <- as.numeric(difftime(
        end_time,
        start_time,
        units = "secs"
      ))
      message("===== [ETL DONE] =====")
    }
  )

  result
}
