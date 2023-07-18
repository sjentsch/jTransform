jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(

        .run = function() {

            # reset the output text, if not blnOut -> exit
            if (!self$options$blnOut) {
                self$results$txtOut$setContent(" ")
                return()
            }
            txtOut <- c()

            # assemble (add directory - HOME on *nix, USERPROFILE on Windows -
            # if necessary) and check existence of the directory, that the file
            # doesn't exist already and the correct file extension
            fleOut <- chkFle(self$options$fleOut)
            
            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)

            # check whether all required variables are present; assemble and run jmvReadWrite command if the output file doesn't already exist
            if (!file.exists(fleOut) && length(self$options$varOrd) > 0) {
                jmvReadWrite::arrange_cols_omv(crrDta, fleOut = fleOut, varOrd = self$options$varOrd)
                if (file.exists(fleOut)) {
                    txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(txtOut, collapse = "<br><br>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<b>Error</b> when writing %s to %s.", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(sprintf("<b>%s already exists</b>. Change the name of the output file or remove the exisiting file.", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("“Desired order of variable(s)” is required and must not be empty.")
            }

        })
)
