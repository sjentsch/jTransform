#' @importFrom jmvcore .
jtTransposeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(
        .crrCmd = "jmvReadWrite::transpose_omv",
        .nonLtd = FALSE,
        .sfxTtl = "xpsd",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = incFnc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varOth) > 1)
        },

        .colFst = incFnc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            varNme <- self$options$varNme
            varOth <- self$options$varOth
            if (getDta) {
                dtaInp <- private$.getDta(c(varNme, varOth))$dtaInp
                # update .xfmCol and .xfmRow to the value after the transformation
                private$.xfmRow <- ncol(dtaInp) - length(varNme)
                private$.xfmCol <- c("ID", if (!is.null(varNme)) as.character(dtaInp[, varNme]) else sprintf("V_%d", seq_len(private$.xfmRow)))
                list(dtaInp = dtaInp, varNme = ifelse(is.null(varNme), "", varNme))
            } else {
                list(varNme = ifelse(is.null(varNme), "", varNme))
            }
        },

        .crtMsg = incFnc$private_methods$.crtMsg,
        .dtaInf = incFnc$private_methods$.dtaInf,
        .dtaMsg = incFnc$private_methods$.dtaMsg,
        .getDta = incFnc$private_methods$.getDta,
        .nteRnC = incFnc$private_methods$.nteRnC,
        .runXfm = incFnc$private_methods$.runXfm

    ),

    public = list(

        asSource = incFnc$public_methods$asSource

    )
)
