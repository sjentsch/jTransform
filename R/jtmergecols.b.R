
# This file is a generated template, your changes will not be overwritten

jtMergeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,
    private = list(
        .run = function() {
            # reset the output text, if not blnOut -> exit
            self$results$txtOut$setContent("")
            Sys.sleep(0.01)
            if (!self$options$blnOut) return()

            self$results$txtOut$setContent("Finding input files...")
            fleInp <- trimws(strsplit(self$options$fleInp, ";")[[1]])
            for (i in seq_along(fleInp)) {
                if (dirname(fleInp[i]) == ".") {
                    tmpFlN <- list.files(path = hmeDir(), pattern = fleInp[i], recursive = TRUE);
                    if (length(tmpFlN) == 1) {
                        fleInp[i] <- file.path(hmeDir(), tmpFlN);
                    } else {
#                       jmvcore::reject("'{file}' either doesn't exists or exists more than once in the home directory.\nPlease add the exact path under “Input file(s)”.", code = '', file = fleInp[i])
                        jmvcore::reject(.("'{file}' either doesn't exists or exists more than once in the home directory.\nPlease add the exact path under “Input file(s)”."), code = '', file = fleInp[i])
                    }
                } else {
                    fleInp[i] <- jmvReadWrite:::nrmFle(fleInp[i]);
                }            
            }
            self$results$txtOut$setContent("")

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)
            attr(crrDta, "fleInp") <- fleInp

            # assemble and check output file name (chkFle in utils.R)
            fleOut <- chkFle(self$options$fleOut)
            
            txtOut <- c()
            # issue a warning if only a very small number of variables is in the output dataset
            if (dim(crrDta)[2] - length(self$options$varBy) < 5)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in th output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Further variables in the output”."))
            
            # assemble and run jmvReadWrite command
            jmvReadWrite::merge_cols_omv(dtaInp = crrDta, fleOut = fleOut, varBy = self$options$varBy, typMrg = self$options$typMrg)
            while (!file.exists(fleOut)) Sys.sleep(0.01)
            if (file.exists(fleOut)) {
                txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))
            } else {
                jmvcore::reject(.("Error when writing '{file}' to '{dir}'."), code = '', file = basename(fleOut), dir = dirname(fleOut))
            }

        })
)
