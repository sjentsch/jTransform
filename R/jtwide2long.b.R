jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varID) > 0 && length(self$options$varLst) > 0 && nzchar(self$options$varTme) && nzchar(self$options$varSep)) {

                # add column attributes (measureType and dataTye)
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                
                # assemble and run jmvReadWrite command
                excLvl <- eval(parse(text = paste0("as.integer(c(", self$options$excLvl, "))")))
                dtaFrm <- jmvReadWrite::wide2long_omv(dtaInp = crrDta, fleOut = fleOut, varLst = self$options$varLst, varID = self$options$varID,
                                            varTme = self$options$varTme, varExc = self$options$varExc, varSep = self$options$varSep, excLvl = excLvl)

                # preview the data (crtPvw in utils.R)
                self$results$txtPvw$setContent(crtPvw(dtaFrm))

                # if CREATE was pressed (blnOut == TRUE), open a new jamovi session with the data
                if (self$options$blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_long"), "\" ", tmpOut))
                }

            }

        })
)
