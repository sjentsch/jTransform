j_DEBUG <- T
j_INFO  <- T
t_INFO  <- F

#### Helper functions for debugging

tinfo <- function(...) {
    if (!t_INFO) return(invisible(NULL))

    cat(paste(list(...)))
    cat("\n")
}

jinfo <- function(...) {
    if (!j_INFO) return(invisible(NULL))

    cat("\n")
    cat(paste(list(...)))
    cat("\n")
}

mark <- function(...) {
    if (!j_DEBUG) return(invisible(NULL))
  
    if (missing(...)) {
        cat("Mark here\n")
        return(invisible(NULL))
    }
  
    items <- list(...)
    cat("______begin________\n\n")
    for (a in items)
        if (is.character(a)) cat(a, "\n") else print(a)
    cat("_____end_______\n\n")
}
