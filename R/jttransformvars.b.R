#' @importFrom jmvcore .
jtTransformVarsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransformVarsClass",
    inherit = jtTransformVarsBase,
    private = list(
        .crrCmd = "jmvReadWrite::transform_vars_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .sfxTtl = "transform_vars",

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$posSqr) >=  1 || length(self$options$negSqr) >=  1 ||
             length(self$options$posLog) >=  1 || length(self$options$negLog) >=  1 ||
             length(self$options$posInv) >=  1 || length(self$options$negInv) >=  1)
        },

        .colFst = function() {
            inpDta <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            varLst <- c(setdiff(names(private$.crrDta), names(inpDta)), names(inpDta))

            ln1FtN <- ifelse(length(varLst) > 1,
                             jmvcore::format(.("The columns {} are shown first in this preview."), paste0(varLst, collapse = ", ")),
                             jmvcore::format(.("The column {} is shown first in this preview."), varLst))
            ln2FtN <- .("In the created data set, the variable order is as shown in \"Variables in the Output Data Set\" above this table.")
            attr(varLst, "note") <- paste(ln1FtN, ln2FtN)

            varLst
        },

        .crrArg = function(getDta = TRUE) {
            nmeXfm <- c("posSqr", "negSqr", "posLog", "negLog", "posInv", "negInv")
            varXfm <- stats::setNames(lapply(nmeXfm, function(x) self$options[[x]]), nmeXfm)
            c(if (getDta) private$.getDta(unique(unlist(varXfm))), list(varXfm = varXfm))
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
