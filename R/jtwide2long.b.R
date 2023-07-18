jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(

        .run = function() {

            # reset the output text, if not blnOut -> exit
            if (!self$options$blnOut) {
                self$results$txtOut$setContent(" ")
                return()
            }
            txtOut <- c()

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)

            # assemble and check output file name (chkFle in utils.R)
            fleOut <- chkFle(self$options$fleOut)

            # check whether all required variables are present; assemble and run jmvReadWrite command if the output file doesn't already exist
            if (!file.exists(fleOut) && length(self$options$varID) > 0 && length(self$options$varLst) > 0 && nzchar(self$options$varTme) && nzchar(self$options$varSep)) {
                excLvl <- eval(parse(text = paste0("as.integer(c(", self$options$excLvl, "))")))
                jmvReadWrite::wide2long_omv(dtaInp = crrDta, fleOut = fleOut, varLst = self$options$varLst, varID = self$options$varID,
                                            varTme = self$options$varTme, varExc = self$options$varExc, varSep = self$options$varSep, excLvl = excLvl)
                if (file.exists(fleOut)) {
                    txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(txtOut, collapse = "<br><br>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<b>Error</b> when writing %s to %s.", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(sprintf("<b>%s already exists</b>. Change the name of the output file or remove the exisiting file.", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("“Variables that identify the same unit”, “Variables to be transformed”, “Prefix”, and “Separator” are required and must not be empty.")
            }

        })
)
