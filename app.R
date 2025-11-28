## Ensure runtime dependencies (install missing packages interactively)
source("R/utils_install.R")
ensure_installed(
  c(
    "shiny",
    "dplyr",
    "tidyr",
    "ggplot2",
    "lubridate",
    "forcats",
    "scales",
    "viridis",
    "DT",
    "janitor",
    "readr",
    "stringr"
  ),
  ask = interactive(),
  auto_install = FALSE
)

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(scales)
library(viridis)
library(DT)
library(janitor)

# Source the helper functions
source("R/feishu.R")

ui <- shiny::fluidPage(
  titlePanel("设计研究部项目工时分析（Feishu_Shiny）"),
  sidebarLayout(
    sidebarPanel(
      fileInput("config_file", "Upload config.json", accept = c(".json")),
      checkboxInput(
        "use_workspace_config",
        "Use workspace config.json",
        value = FALSE
      ),
      actionButton("run", "Run ETL"),
      hr(),
      fileInput(
        "csv_file",
        "Or upload output.csv directly",
        accept = c(".csv")
      ),
      actionButton("load_csv", "Load CSV"),
      br(),
      # downloadButton("download_csv", "Download CSV")
      width = 3
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("datatable")),
        tabPanel(
          "Project Analysis",
          DT::dataTableOutput("project_table"),
          plotOutput("project_rank_plot", height = "600px"),
          plotOutput("project_trend_plot", height = "400px"),
          plotOutput("project_heatmap", height = "600px")
        ),
        tabPanel(
          "User Analysis",
          DT::dataTableOutput("user_table"),
          plotOutput("user_bar_plot", height = "600px"),
          plotOutput("user_heatmap", height = "600px")
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  rv <- shiny::reactiveValues(status = "Idle", csv_path = NULL, df = NULL)

  # Run ETL (existing behavior)
  shiny::observeEvent(input$run, {
    rv$status <- "Starting ETL..."

    cfg_path <- NULL
    if (isTruthy(input$config_file)) {
      cfg_path <- input$config_file$datapath
    } else if (
      isTRUE(input$use_workspace_config) && file.exists("config.json")
    ) {
      cfg_path <- "config.json"
    }

    if (is.null(cfg_path)) {
      rv$status <- "No config provided. Upload a config.json or check 'Use workspace config.json'."
      return()
    }

    cfg <- tryCatch(load_config(cfg_path), error = function(e) {
      rv$status <- paste("Failed to read config:", conditionMessage(e))
      NULL
    })
    if (is.null(cfg)) {
      return()
    }

    res <- tryCatch(etl_pipeline(cfg), error = function(e) {
      rv$status <- paste("ETL failed:", conditionMessage(e))
      NULL
    })
    if (is.null(res)) {
      return()
    }

    rv$status <- paste0(
      "Finished. success=",
      res$success,
      ", duration=",
      round(res$duration, 2),
      "s"
    )

    csv_file <- cfg$csv_file_name %||% "output.csv"
    if (file.exists(csv_file)) {
      rv$csv_path <- csv_file
      rv$df <- tryCatch(
        {
          df <- readr::read_csv(csv_file, sep = ",", fileEncoding = "UTF-8-BOM")
          # clean names and normalize separators
          df <- df %>%
            janitor::clean_names() %>%
            rename_with(~ gsub("[\\s\\-]+", "_", .x))

          # normalize 项目名称 / 项目工时 columns to 项目名称_1, 项目工时_1, etc.
          for (i in 1:3) {
            mi <- grep(paste0("项目.*名.*", i), names(df), value = TRUE)
            if (length(mi) > 0) {
              names(df)[names(df) == mi[1]] <- paste0("项目名称_", i)
            }
            wi <- grep(paste0("项目.*工时.*", i), names(df), value = TRUE)
            if (length(wi) > 0) {
              names(df)[names(df) == wi[1]] <- paste0("项目工时_", i)
            }
          }
          df
        },
        error = function(e) NULL
      )
    }
  })

  # Load CSV directly from upload
  shiny::observeEvent(input$load_csv, {
    if (isTruthy(input$csv_file)) {
      file <- input$csv_file$datapath
      rv$df <- tryCatch(
        {
          df <- readr::read_csv(file, show_col_types = FALSE)
          df <- df %>%
            # janitor::clean_names() %>%
            rename_with(~ gsub("[\\s\\-]+", "_", .x))
          for (i in 1:3) {
            mi <- grep(paste0("项目.*名.*", i), names(df), value = TRUE)
            if (length(mi) > 0) {
              names(df)[names(df) == mi[1]] <- paste0("项目名称_", i)
            }
            wi <- grep(paste0("项目.*工时.*", i), names(df), value = TRUE)
            if (length(wi) > 0) {
              names(df)[names(df) == wi[1]] <- paste0("项目工时_", i)
            }
          }
          df
        },
        error = function(e) {
          rv$status <- paste("Failed to read CSV:", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(rv$df)) rv$status <- "CSV loaded"
    }
  })

  # Data transformation from project_hour.qmd
  transfered <- shiny::reactive({
    req(rv$df)
    df <- rv$df
    df <- df %>%
      filter(is.na(检查) | 检查 == "{}") %>%
      mutate(
        p1 = stringr::str_c(
          项目名称_1,
          as.character(项目工时_1),
          sep = ":"
        ),
        p2 = stringr::str_c(
          项目名称_2,
          as.character(项目工时_2),
          sep = ":"
        ),
        p3 = stringr::str_c(项目名称_3, as.character(项目工时_3), sep = ":")
      ) %>%
      select(
        -c(
          项目名称_1,
          项目工时_1,
          项目名称_2,
          项目工时_2,
          项目名称_3,
          项目工时_3
        )
      ) %>%
      pivot_longer(
        cols = c(p1, p2, p3),
        names_to = "del",
        values_to = "项目工时"
      ) %>%
      select(-del) %>%
      separate(
        项目工时,
        into = c("项目名称", "项目工时"),
        sep = ":",
        fill = "right",
        extra = "merge"
      ) %>%
      mutate(
        项目名称 = stringr::str_trim(项目名称),
        项目工时 = as.numeric(项目工时),
        填报日期 = lubridate::as_datetime(
          as.numeric(填报日期) / 1000,
          tz = "UTC"
        ) %>%
          as.Date()
      ) %>%
      filter(!is.na(项目名称), 项目名称 != "{}") %>%
      select(-c(检查, record_id, 提交人, 提交时间, 自动编号)) %>%
      relocate(填报日期, 姓名)
    df
  })

  project_hour <- shiny::reactive({
    req(transfered())
    # compute month as integer and then set an ordered factor so plots use natural month order
    x <- transfered() %>%
      mutate(
        Month = as.integer(lubridate::month(填报日期)),
        Quarter = as.character(lubridate::quarter(填报日期)),
        Year = as.character(lubridate::year(填报日期))
      ) %>%
      group_by(Month, 项目名称) %>%
      summarise(总工时 = sum(项目工时, na.rm = TRUE), .groups = "drop")
    x %>%
      mutate(Month = factor(Month, levels = sort(unique(as.integer(Month)))))
  })

  user_hour <- shiny::reactive({
    req(transfered())
    y <- transfered() %>%
      mutate(
        Month = as.integer(lubridate::month(填报日期)),
        Quarter = as.character(lubridate::quarter(填报日期)),
        Year = as.character(lubridate::year(填报日期))
      ) %>%
      group_by(Month, 姓名) %>%
      summarise(总工时 = sum(项目工时, na.rm = TRUE), .groups = "drop")
    y %>%
      mutate(Month = factor(Month, levels = sort(unique(as.integer(Month)))))
  })

  output$datatable <- DT::renderDataTable({
    req(transfered())
    DT::datatable(
      transfered(),
      filter = "top",
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$project_table <- DT::renderDataTable({
    req(project_hour())
    DT::datatable(
      project_hour(),
      filter = "top",
      options = list(pageLength = 20)
    )
  })

  output$user_table <- DT::renderDataTable({
    req(user_hour())
    DT::datatable(
      user_hour(),
      filter = "top",
      options = list(pageLength = 20)
    )
  })

  output$project_rank_plot <- shiny::renderPlot({
    req(project_hour())
    df <- project_hour() %>%
      group_by(项目名称) %>%
      summarise(总工时 = sum(总工时, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(总工时))
    ggplot(df, aes(x = forcats::fct_reorder(项目名称, 总工时), y = 总工时)) +
      geom_col(fill = "#2b8cbe") +
      coord_flip() +
      theme_minimal() +
      labs(title = "项目总工时排名", x = "项目名称", y = "总工时")
  })

  output$project_trend_plot <- shiny::renderPlot({
    req(project_hour())
    df <- project_hour()
    top_projects <- df %>%
      group_by(项目名称) %>%
      summarise(total = sum(总工时, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(项目名称) %>%
      head(10)
    df2 <- df %>%
      filter(项目名称 %in% top_projects) %>%
      group_by(Month, 项目名称) %>%
      summarise(总工时 = sum(总工时, na.rm = TRUE), .groups = "drop")
    ggplot(
      df2,
      aes(x = Month, y = 总工时, color = 项目名称, group = 项目名称)
    ) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = sprintf("前 %d 项目月度工时趋势", length(top_projects)),
        x = "Month",
        y = "总工时"
      )
  })

  output$project_heatmap <- shiny::renderPlot({
    req(project_hour())
    df <- project_hour()
    df_tile <- df %>% tidyr::complete(项目名称, Month, fill = list(总工时 = 0))
    ggplot(df_tile, aes(x = Month, y = 项目名称, fill = 总工时)) +
      geom_tile(colour = "grey90") +
      scale_fill_viridis_c(option = "magma", labels = scales::comma) +
      theme_minimal() +
      labs(
        title = "项目-月份工时热力图",
        x = "Month",
        y = "项目名称",
        fill = "总工时"
      )
  })

  output$user_bar_plot <- shiny::renderPlot({
    req(user_hour())
    df <- user_hour()
    df_prep <- df %>%
      mutate(
        Month = as.integer(as.character(Month)),
        Month = factor(Month, levels = sort(unique(Month)))
      )
    name_order <- df_prep %>%
      group_by(姓名) %>%
      summarise(total = sum(总工时, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(姓名)
    df_prep <- df_prep %>% mutate(姓名 = factor(姓名, levels = name_order))
    ggplot(df_prep, aes(x = 姓名, y = 总工时, fill = Month)) +
      geom_col(position = position_dodge(width = 0.9), width = 0.8) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "每月用户工时（按月份着色，DODGE）",
        x = "姓名",
        y = "总工时",
        fill = "Month"
      )
  })

  output$user_heatmap <- shiny::renderPlot({
    req(user_hour())
    df <- user_hour()
    df_prep <- df %>%
      mutate(
        Month = as.integer(as.character(Month)),
        Month = factor(Month, levels = sort(unique(Month)))
      )
    df_tile <- df_prep %>% tidyr::complete(姓名, Month, fill = list(总工时 = 0))
    ggplot(df_tile, aes(x = Month, y = 姓名, fill = 总工时)) +
      geom_tile(colour = "grey90") +
      scale_fill_viridis_c(option = "magma", labels = scales::comma) +
      theme_minimal() +
      labs(
        title = "用户-月份工时热力图",
        x = "Month",
        y = "姓名",
        fill = "总工时"
      )
  })

  output$status <- shiny::renderText({
    rv$status
  })

  # output$download_csv <- downloadHandler(
  #   filename = function() {
  #     if (!is.null(rv$csv_path)) basename(rv$csv_path) else "output.csv"
  #   },
  #   content = function(file) {
  #     if (is.null(rv$csv_path)) {
  #       stop("No CSV to download")
  #     }
  #     file.copy(rv$csv_path, file, overwrite = TRUE)
  #   }
  # )
}

## Start app in a way that allows LAN access when running locally.
# Use environment variables `SHINY_HOST` and `SHINY_PORT` to override defaults.
shiny_host <- Sys.getenv("SHINY_HOST", "0.0.0.0")
shiny_port <- as.integer(Sys.getenv("SHINY_PORT", "3838"))

if (interactive()) {
  message(sprintf(
    "Starting Shiny on %s:%d — accessible from LAN",
    shiny_host,
    shiny_port
  ))
  shiny::runApp(
    list(ui = ui, server = server),
    host = shiny_host,
    port = shiny_port
  )
} else {
  # When run in a non-interactive deployment (e.g. shiny-server), use shinyApp()
  shiny::shinyApp(ui, server)
}
