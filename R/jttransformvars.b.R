#' @importFrom jmvcore .
jtTransformVarsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtTransformVarsClass",
    inherit = jtTransformVarsBase,
    private = list(
        .crrCmd = "jmvReadWrite::transform_vars_omv",
        .nonLtd = FALSE,
        .sfxTtl = "transform_vars",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

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
            inpDta <- if (!is.null(self$data) && nrow(self$data) > 0) self$data else self$readDataset()
            varLst <- c(setdiff(private$.xfmCol, names(inpDta)), names(inpDta))

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
            varXfm <- varXfm[!vapply(varXfm, is.null, logical(1))]
            if (getDta) {
                dtaInp <- private$.getDta(unique(unlist(varXfm)))$dtaInp
                # update target column order (.xfmCol is first filled in .getDta())
                private$.xfmCol <- c(names(dtaInp),
                                     unlist(lapply(names(varXfm), function(n) paste0(varXfm[[n]], "_", toupper(gsub("^pos|^neg", "", n))))))
                list(dtaInp = dtaInp, varXfm = varXfm)
            } else {
                list(varXfm = varXfm)
            }
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
