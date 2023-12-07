j_DEBUG <- T
j_INFO  <- T
t_INFO  <- F
fleWOS  <- ""
# NB for Windows users: Feel free to change the path and name of the log file as you like.
if (.Platform$OS.type == "windows") fleWOS <- file.path(base::Sys.getenv("TEMP"), "jTransform.log")

#### Helper functions for debugging

tinfo <- function(...) {
    if (!t_INFO) return(invisible(NULL))

    if (nzchar(fleWOS)) base::sink(file = fleWOS, append = TRUE)

    cat(paste(list(...)))
    cat("\n")

    if (nzchar(fleWOS)) base::sink()
}

jinfo <- function(...) {
    if (!j_INFO) return(invisible(NULL))

    if (nzchar(fleWOS)) base::sink(file = fleWOS, append = TRUE)

    cat("\n")
    cat(paste(list(...)))
    cat("\n")

    if (nzchar(fleWOS)) base::sink()
}

mark <- function(...) {
    if (!j_DEBUG) return(invisible(NULL))
  
    if (nzchar(fleWOS)) base::sink(file = fleWOS, append = TRUE)

    if (missing(...)) {
        cat("Mark here\n")
        return(invisible(NULL))
    }
  
    items <- list(...)
    cat("______begin________\n\n")
    for (a in items)
        if (is.character(a)) cat(a, "\n") else print(a)
    cat("_____end_______\n\n")

    if (nzchar(fleWOS)) base::sink()
}
