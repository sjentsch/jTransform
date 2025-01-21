#' @importFrom jmvcore .
descDistancesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "descDistancesClass",
    inherit = jtDistancesClass,
    private = list(
        .nonLtd = TRUE,

        .init = function() {
            super$.init()
            self$results$setTitle(.("Distances / Proximities"))
            self$results$fmtHTM$setContent(" ")
            self$results$pvwDta$setTitle(.("Distances / Proximities"))
        }

    ),

    public = list(

        initialize = function(...) {
            super$initialize(...)
            private$.name <- "descDistances"
        }

    )
)
