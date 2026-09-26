#' @importFrom jmvcore .
jtSortClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(
        .crrCmd = "jmvReadWrite::sort_omv",
        .nonLtd = FALSE,
        .sfxTtl = "sort",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = incFnc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varSrt) >=  1)
        },

        .colFst = incFnc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            c(if (getDta) private$.getDta(),
              list(varSrt = vapply(self$options$ordSrt,
                                   function(x) paste0(gsub("descend", "-", gsub("ascend", "", x[["order"]])), x[["var"]]),
                                   character(1))))
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
