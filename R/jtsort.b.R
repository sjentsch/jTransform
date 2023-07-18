jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
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

            # issue a warning if varOth is left empty
            if (length(self$options$varOth) == 0)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in the output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Further variables in the output”."))
            
            # check whether all required variables are present; assemble and run jmvReadWrite command if the output file doesn't already exist
            if (!file.exists(fleOut) && !length(self$options$varSrt) > 0) {
                # assemble vector for sorting
                varSrt <- paste0(gsub("descend", "-", gsub("ascend", "",
                            sapply(self$options$ordSrt, "[[", "order"))),
                            sapply(self$options$ordSrt, "[[", "var"))
                jmvReadWrite::sort_omv(dtaInp = crrDta, fleOut = fleOut, varSrt = varSrt)
                if (file.exists(fleOut)) {
                    txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(txtOut, collapse = "<br><br>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<b>Error</b> when writing %s to %s.", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(sprintf("<b>%s already exists</b>. Change the name of the output file or remove the exisiting file.", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("“Variable(s) to sort after” is required and must not be empty.")
            }

        })
)
