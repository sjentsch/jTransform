jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(

        .run = function() {
            self$results$txtOut$setContent(" ")

            # check whether all required variables are present
            if (length(self$options$varOrd) > 0) {

                # add column attributes (measureType and dataTye)
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                
                # assemble and run jmvReadWrite command
                dtaFrm <- jmvReadWrite::arrange_cols_omv(crrDta, varOrd = self$options$varOrd)

                # depending on whether CREATE was pressed (blnOut == TRUE), either use the data
                # for preview or open a new jamovi session with them
                if (blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_arrCol"), "\" ", tmpOut))
                } else {
                    self$results$txtPvw$setContent(paste0("Variables in the output file:\n", strSpl(names(dtaFrm), maxLng = 80), print(data[1:10, 1:8], )))
                }

            } else {

                self$results$txtOut$setContent("<p>“Desired order of variable(s)” is required and must not be empty.</p>")

            }

        })
)
