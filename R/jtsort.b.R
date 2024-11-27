#' @importFrom jmvcore .
jtSortClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(
        .crrCmd = "jmvReadWrite::sort_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varSrt) >= 1)
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            list(dtaInp = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset(),
                 varSrt = vapply(self$options$ordSrt,
                                 function(x) { paste0(gsub("descend", "-", gsub("ascend", "", x[["order"]])), x[["var"]]) },
                                 character(1)))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
