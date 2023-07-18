
# This file is a generated template, your changes will not be overwritten

jtMergeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,
    private = list(

        .run = function() {

            # reset the output text, if not blnOut -> exit
            if (!self$options$blnOut) {
                self$results$txtOut$setContent(" ")
                return()
            }
            txtOut <- c()

            self$results$txtOut$setContent("Finding input files...")
            fleInp <- trimws(strsplit(self$options$fleInp, ";")[[1]])
            for (i in seq_along(fleInp)) {
                if (dirname(fleInp[i]) == ".") {
                    tmpFlN <- list.files(path = hmeDir(), pattern = fleInp[i], recursive = TRUE);
                    if (length(tmpFlN) == 1) {
                        fleInp[i] <- file.path(hmeDir(), tmpFlN);
                    } else {
                        self$results$txtOut$setContent(sprintf("%s either doesn't exists or exists more than once in the home directory.\nPlease add the exact path under “Input file(s)”.", fleInp[i]))
                        return()
                    }
                } else {
                    fleInp[i] <- jmvReadWrite:::nrmFle(fleInp[i]);
                }            
            }

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)
            attr(crrDta, "fleInp") <- fleInp

            # assemble and check output file name (chkFle in utils.R)
            fleOut <- chkFle(self$options$fleOut)
            
            # issue a warning if varOth is left empty
            if (length(self$options$varOth) == 0)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in the output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Further variables in the output”."))
            
            # check whether all required variables are present; assemble and run jmvReadWrite command if the output file doesn't already exist
            if (!file.exists(fleOut) && length(self$options$varBy)) {
                jmvReadWrite::merge_cols_omv(dtaInp = crrDta, fleOut = fleOut, varBy = self$options$varBy, typMrg = self$options$typMrg)
                if (file.exists(fleOut)) {
                    txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                    self$results$txtOut$setContent(paste(txtOut, collapse = "<br><br>\n"))
                } else {
                    self$results$txtOut$setContent(sprintf("<b>Error</b> when writing %s to %s.", basename(fleOut), dirname(fleOut)))
                }
            } else if (file.exists(fleOut)) {
                self$results$txtOut$setContent(sprintf("<b>%s already exists</b>. Change the name of the output file or remove the exisiting file.", basename(fleOut)))
            } else {
                self$results$txtOut$setContent("“Variable(s) to match the data sets by” is required and must not be empty.")
            }

        })
)
