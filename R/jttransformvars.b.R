
# This file is a generated template, your changes will not be overwritten

jtTransformVarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransformVarsClass",
    inherit = jtTransformVarsBase,
    private = list(
        .xfmDta = NULL,
        .inpDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.xfmDta <- do.call(jmvReadWrite::transform_vars_omv, private$.crrArg())
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.xfmDta, colFst = private$.colFst())
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least two variables in varOrd and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::transform_vars_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, dtaFrm = private$.xfmDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.xfmDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpXfV)
            }
        },

        .chkVar = function() {
            (length(self$options$posSqr) >= 1 || length(self$options$negSqr) >= 1 ||
             length(self$options$posLog) >= 1 || length(self$options$negLog) >= 1 ||
             length(self$options$posInv) >= 1 || length(self$options$negInv) >= 1)
        },

        .colFst = function() {
            c(setdiff(names(private$.xfmDta), names(private$.inpDta)), names(private$.inpDta))
        },

        .crrArg = function() {
            if (!is.null(self$data) && dim(self$data)[1] > 0) private$.inpDta <- self$data else private$.inpDta <- self$readDataset()
            crrXfm <- setNames(lapply(c("posSqr", "negSqr", "posLog", "negLog", "posInv", "negInv"), function(x) self$options[[x]]),
                        c("posSqr", "negSqr", "posLog", "negLog", "posInv", "negInv"))
            list(dtaInp = private$.inpDta, fleOut = NULL, varXfm = crrXfm)
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::arrange_cols_omv", private$.crrArg()[c(-1, -2)])
        }

    )
)
