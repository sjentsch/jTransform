# define global environment for log flags
logFlags            <- new.env(parent = emptyenv())
logFlags$j_DEBUG    <- FALSE
logFlags$j_INFO     <- FALSE
logFlags$log_active <- FALSE  # Tracks if the log file is currently open

# determine the appropriate log file path based on the OS
logFlags$fleWUD <- switch(jmvReadWrite:::getOS(),
                          "windows" = file.path(Sys.getenv("TEMP"), "jTransform.log"),
                          "unix"    = file.path(Sys.getenv("HOME"), ".local", "share", "jamovi", "jTransform.log"),
                          "macos"   = file.path(Sys.getenv("HOME"), "Library", "Logs", "jamovi", "jTransform.log"),
                          file.path(tempdir(), "jTransform.log")  # Default to tempdir() if OS is unrecognized
)

# ensure the log directory exists
ensure_log_dir <- function(log_path) {
    log_dir <- dirname(log_path)
    if (!dir.exists(log_dir)) {
        tryCatch({
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
        }, error = function(e) {
            # Handle errors silently
        })
    }
}

# helper function to get current timestamp
current_time <- function() {
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# update logging flags dynamically
set_logflags <- function(jxfLog) {
    if (jxfLog && !logFlags$log_active) {
        open_log()  # Open the log only if it is not already active
    } else if (!jxfLog && logFlags$log_active) {
        close_log()  # Close the log only if it is currently active
    }

    logFlags$j_DEBUG <- jxfLog
    logFlags$j_INFO  <- jxfLog
}

# if not already open, open the log file
open_log <- function() {
    ensure_log_dir(logFlags$fleWUD)
    if (nzchar(logFlags$fleWUD) && !logFlags$log_active) {
        tryCatch({
            if (!file.exists(logFlags$fleWUD)) file.create(logFlags$fleWUD)
            cat(paste0("Logging started at  ",
                       current_time(),
                       "\n---------------------------------------\n"),
                file = logFlags$fleWUD, append = TRUE)
            logFlags$log_active <- TRUE
        }, error = function(e) {
            # Optional: Handle errors silently
        })
    }
}

# close the log file
close_log <- function() {
    if (nzchar(logFlags$fleWUD) && logFlags$log_active) {
        tryCatch({
            cat(paste0("---------------------------------------\n",
                       "Logging disabled at ", current_time(), "\n\n"),
                file = logFlags$fleWUD, append = TRUE)
            logFlags$log_active <- FALSE
        }, error = function(e) {
            # Optional: Handle errors silently
        })
    }
}

# write info messages to the log
jinfo <- function(...) {
    if (!logFlags$j_INFO || !logFlags$log_active) return(invisible(NULL))
    tryCatch({
        if (nzchar(logFlags$fleWUD)) {
            if (!file.exists(logFlags$fleWUD)) file.create(logFlags$fleWUD)
            cat(paste(..., collapse = " "), "\n",
                file = logFlags$fleWUD, append = TRUE)
        }
    }, error = function(e) {
        # Optional: Log the error internally
    })
}

# write debug marks to the log
# to be used in interactive debugging - DO NOT REMOVE
mark <- function(...) {
    if (!logFlags$j_DEBUG || !logFlags$log_active) return(invisible(NULL))
    tryCatch({
        if (nzchar(logFlags$fleWUD)) {
            if (!file.exists(logFlags$fleWUD)) file.create(logFlags$fleWUD)
            cat("______begin________\n", file = logFlags$fleWUD, append = TRUE)
            lapply(list(...), function(a) {
                if (is.character(a)) {
                    cat(a, "\n", file = logFlags$fleWUD, append = TRUE)
                } else {
                    cat(paste(utils::capture.output(print(a)), collapse = "\n"), "\n",
                        file = logFlags$fleWUD, append = TRUE)
                }
            })
            cat("_______end_________\n", file = logFlags$fleWUD, append = TRUE)
        }
    }, error = function(e) {
        # Optional: Log the error internally
    })
}
