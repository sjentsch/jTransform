jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(

        .run = function() {
print("jtTranspose")
print(self$options$blnOut)
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
            if (!file.exists(fleOut) && length(self$options$varOth) > 0) {
                varNme <- ifelse(is.null(self$options$varNme), "", self$options$varNme)
                jmvReadWrite::transpose_omv(dtaInp = crrDta, fleOut = fleOut, varNme = varNme, varOth = self$options$varOth)
                if (file.exists(fleOut)) {
                    txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(txtOut, collapse = "<br><br>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<b>Error</b> when writing %s to %s.", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(sprintf("<b>%s already exists</b>. Change the name of the output file or remove the exisiting file.", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("“Variables to be transposed” are required and must not be empty.")
            }
            
        })
)
