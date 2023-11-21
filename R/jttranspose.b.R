jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(
        .xpsDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.xpsDta <- do.call(jmvReadWrite::transpose_omv, private$.crrArg())
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.xpsDta)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least two variables in varOrd and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if CREATE was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::transpose_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use CREATE as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, dtaFrm = private$.xpsDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.xpsDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpXps)
            }
        },

        .chkVar = function() {
            (length(self$options$varOth) > 1)
        },

        .crrArg = function() {
            list(dtaInp = self$readDataset()[, c(self$options$varNme, self$options$varOth)], fleOut = NULL,
                 varNme = ifelse(is.null(self$options$varNme), "", self$options$varNme))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::transpose_omv", private$.crrArg()[c(-1, -2)])
        }

    )
)
