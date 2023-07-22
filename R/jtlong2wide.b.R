jtLong2WideClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varID) > 0 && length(self$options$varTme) > 0 && length(self$options$varTgt) > 0 && nzchar(self$options$varSep)) {

                # add column attributes (measureType and dataTye)
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                
                # assemble and run jmvReadWrite command
                dtaFrm <- jmvReadWrite::long2wide_omv(dtaInp = crrDta, varID = self$options$varID, varTme = self$options$varTme,
                                                      varTgt = self$options$varTgt, varExc = self$options$varExc, varSep = self$options$varSep,
                                                      varOrd = self$options$varOrd, varAgg = self$options$varAgg)

                # preview the data (crtPvw in utils.R)
                self$results$txtPvw$setContent(crtPvw(dtaFrm))

                # if CREATE was pressed (blnOut == TRUE), open a new jamovi session with the data
                if (self$options$blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_wide"), "\" ", tmpOut))
                }

            }

        })
)
