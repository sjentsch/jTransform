#' @importFrom jmvcore .
jtTransposeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(
        .crrCmd = "jmvReadWrite::transpose_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varOth) > 1)
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            dtaFrm <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            list(dtaInp = dtaFrm[, c(self$options$varNme, self$options$varOth)],
                 varNme = ifelse(is.null(self$options$varNme), "", self$options$varNme))
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
