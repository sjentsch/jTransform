jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(

        .run = function() {

#      default: "Dataset_long.omv"

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
                    txtOut <- c(txtOut, sprintf("<strong>%s</strong> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(paste0("<p>", txtOut, "</p>"), collapse = "<p></p>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<p><strong>Error</strong> when writing %s to %s.</p>", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(
                  sprintf("<p><strong>%s already exists</strong>. Change the name of the output file or remove the exisiting file.</p>", basename(fleOut)))
            } else {
                self$results$txtOut$setContent(paste("<p>“Variables that identify the same unit”, “Variables to be transformed”,",
                                                     "“Prefix”, and “Separator” are required and must not be empty.</p>"))
            }

        })
)
