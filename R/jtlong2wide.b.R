jtLong2WideClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(
        .run = function() {
            # reset the output text, if not blnOut -> exit
            self$results$txtOut$setContent("")
            Sys.sleep(0.01)
            if (!self$options$blnOut) return()

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)

            # assemble and check output file name (chkFle in utils.R)
            fleOut <- chkFle(self$options$fleOut)

            txtOut <- c()

            # assemble and run jmvReadWrite command
# TO-DO: check whether all required variables are present
            jmvReadWrite::long2wide_omv(dtaInp = crrDta, fleOut = fleOut, varID = self$options$varID, varTme = self$options$varTme,
                                        varTgt = self$options$varTgt, varExc = self$options$varExc, varSep = self$options$varSep,
                                        varOrd = self$options$varOrd, varAgg = self$options$varAgg)
            while (!file.exists(fleOut)) Sys.sleep(0.01)
            if (file.exists(fleOut)) {
                txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))
            } else {
                jmvcore::reject(.("Error when writing '{file}' to '{dir}'."), code = '', file = basename(fleOut), dir = dirname(fleOut))
            }

        })
)
