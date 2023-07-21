jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varOrd) > 0) {

                # add column attributes (measureType and dataTye)
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                
                # assemble and run jmvReadWrite command
                dtaFrm <- jmvReadWrite::arrange_cols_omv(crrDta, varOrd = self$options$varOrd)

                # preview the data
                self$results$txtPvw$setContent(sprintf("Variables in the output file:\n%s\n\n%s\n\n(max. 10 rows and max. 8 variables are shown)\n",
                                                 paste(splStr(names(dtaFrm), maxLng = 80), collapse = ",\n"),
                                                 paste(capture.output(print(dtaFrm[seq(min(10, dim(dtaFrm)[1])), seq(min(8, dim(dtaFrm)[2]))], row.names = FALSE)), collapse="\n")))

                # if CREATE was pressed (blnOut == TRUE), open a new jamovi session with the data
                if (self$options$blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_arrCol"), "\" ", tmpOut))
                }

            }

        })
)
