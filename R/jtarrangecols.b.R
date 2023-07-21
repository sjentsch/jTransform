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
                    txtOut <- c(txtOut, sprintf("<strong>%s</strong> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(paste0("<p>", txtOut, "</p>"), collapse = "<p></p>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<p><strong>Error</strong> when writing %s to %s.</p>", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(
                  sprintf("<p><strong>%s already exists</strong>. Change the name of the output file or remove the exisiting file.</p>", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("<p>“Desired order of variable(s)” is required and must not be empty.</p>")
            }

        })
)
