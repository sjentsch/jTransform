#' @importFrom jmvcore .
jtTransformVarsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransformVarsClass",
    inherit = jtTransformVarsBase,
    private = list(
        .crrCmd = "jmvReadWrite::transform_vars_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

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

            crrFtN <- sprintf(paste(.("The column%s %s %s shown first in this preview. In the"),
                                    .("created data set, the variable order is as shown in"),
                                    .("\"Variables in the Output Data Set\" above this table.")),
                              ifelse(length(varLst) > 1, "s", ""),
                              paste0(varLst, collapse = ", "),
                              ifelse(length(varLst) > 1, "are", "is"))
            attr(varLst, "note") <- crrFtN

            varLst
        },

        .crrArg = function() {
            nmeXfm <- c("posSqr", "negSqr", "posLog", "negLog", "posInv", "negInv")
            list(dtaInp = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset(),
                 varXfm = setNames(lapply(nmeXfm, function(x) self$options[[x]]), nmeXfm))
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
