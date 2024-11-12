jtSortClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(

        .init = function() {
            if (private$.chkVar()) {
                # resize / prepare the output table (prpPvw in utils.R)
                if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = dtaFrm, colFst = self$options$varSrt)
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
                    do.call(jmvReadWrite::sort_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    srtDta <- do.call(jmvReadWrite::sort_omv, private$.crrArg())
                    crtInf(crrInf = self$results$genInf, dtaFrm = srtDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = srtDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpSrt)
            }
        },

        .chkVar = function() {
            (length(self$options$varSrt) >= 1)
        },

        .crrArg = function() {
            varSrt <- vapply(self$options$ordSrt, function(x) {
                                                       paste0(gsub("descend", "-", gsub("ascend", "", x[["order"]])), x[["var"]])
                                                  }, character(1))
            list(dtaInp = self$data, fleOut = NULL, varSrt = varSrt)
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::sort_omv", private$.crrArg()[-seq(2)])
        }

    )
)
