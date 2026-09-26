#' @importFrom jmvcore .
jtArrangeColsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(
        .crrCmd = "jmvReadWrite::arrange_cols_omv",
        .nonLtd = FALSE,
        .sfxTtl = "arr_cols",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = incFnc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varOrd) + as.integer(self$options$blnAll) >= 2)
        },

        .colFst = incFnc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            varOrd <- unique(c(self$options$varOrd, rep(self$options$varAll, self$options$blnAll)))
            if (getDta) {
                dtaInp <- private$.getDta()$dtaInp
                private$.xfmCol <- varOrd  # update target column order (.xfmCol is first filled in .getDta())
                list(dtaInp = dtaInp, varOrd = varOrd)
            } else {
                list(varOrd = varOrd)
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
