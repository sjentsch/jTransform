#' @importFrom jmvcore .
jtTransposeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(
        .crrCmd = "jmvReadWrite::transpose_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .sfxTtl = "xpsd",

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varOth) > 1)
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            c(if (getDta) private$.getDta(c(self$options$varNme, self$options$varOth)),
              list(varNme = ifelse(is.null(self$options$varNme), "", self$options$varNme)))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,
        .runXfm = commonFunc$private_methods$.runXfm

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
