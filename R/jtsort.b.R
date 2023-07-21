jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varSrt) > 0) {

                # add column attributes (measureType and dataTye)
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                
                # assemble and run jmvReadWrite command
                varSrt <- paste0(gsub("descend", "-", gsub("ascend", "", sapply(self$options$ordSrt, "[[", "order"))),
                                 sapply(self$options$ordSrt, "[[", "var"))
                dtaFrm <- jmvReadWrite::sort_omv(dtaInp = crrDta, fleOut = fleOut, varSrt = varSrt)

                # preview the data (crtPvw in utils.R)
                self$results$txtPvw$setContent(crtPvw(dtaFrm))

                # if CREATE was pressed (blnOut == TRUE), open a new jamovi session with the data
                if (self$options$blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_sort"), "\" ", tmpOut))
                }

            }

        })
)
