jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
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
            # issue a warning if only a very small number of variables is in the output dataset
            if (dim(crrDta)[2] - length(self$options$varNme) < 5)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in th output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Variables to be transposed”."))

            # ensure the correct format of the variable
            varNme <- ifelse(is.null(self$options$varNme), "", self$options$varNme)
            
            # assemble and run jmvReadWrite command
            jmvReadWrite::transpose_omv(dtaInp = crrDta, fleOut = fleOut, varNme = varNme)
            while (!file.exists(fleOut)) Sys.sleep(0.01)
            if (file.exists(fleOut)) {
                txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))
            } else {
                jmvcore::reject(.("Error when writing '{file}' to '{dir}'."), code = '', file = basename(fleOut), dir = dirname(fleOut))
            }
            
        })
)
