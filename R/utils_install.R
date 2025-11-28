## Helper: ensure required packages are installed at runtime
#
# Usage: call ensure_installed(c("pkg1", "pkg2")) early in your app or script.
# This will install missing CRAN packages when running interactively, or
# install them non-interactively if `auto_install = TRUE` is set.

ensure_installed <- function(
  packages,
  repos = getOption("repos"),
  ask = interactive(),
  auto_install = FALSE
) {
  if (is.null(repos) || identical(repos, "")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  installed <- rownames(utils::installed.packages())
  missing <- setdiff(unique(packages), installed)
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  if (!ask && !auto_install) {
    stop(sprintf(
      "Missing required packages: %s. Set auto_install = TRUE to install automatically.",
      paste(missing, collapse = ", ")
    ))
  }

  # If interactive, ask the user for consent before installing
  if (ask && !auto_install) {
    prompt <- sprintf(
      "The following packages are required but not installed: %s\nInstall now? [y/N]: ",
      paste(missing, collapse = ", ")
    )
    ans <- tolower(trimws(readline(prompt)))
    if (ans != "y" && ans != "yes") stop("Required packages not installed")
  }

  # Install from CRAN
  utils::install.packages(missing, repos = repos)
  invisible(TRUE)
}
