jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(

        .init = function() {
            if (length(self$options$varSrt) >= 1) {
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = self$readDataset(), colFst = self$options$varSrt)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least two variables in varOrd and that the data set has at least one row
            if (length(self$options$varSrt) >= 1 && dim(self$data)[1] >= 1) {
                # assemble the arguments for sort_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varSrt = paste0(gsub("descend", "-", gsub("ascend", "",
                            sapply(self$options$ordSrt, "[[", "order"))), sapply(self$options$ordSrt, "[[", "var")))
                # if CREATE was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::sort_omv, crrArg[-2])
                # if not, show the variable list and how to use CREATE as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    srtDta <- do.call(jmvReadWrite::sort_omv, crrArg)
                    crtInf(crrInf = self$results$genInf, dtaFrm = srtDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = srtDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpSrt)
            }
        }
        
    )
)
