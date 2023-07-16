jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
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

            # convert excLvl to a number
            excLvl <- eval(parse(text = paste0("as.integer(c(", self$options$excLvl, "))")))
            
            # check whether all required variables are present and assemble and run jmvReadWrite command
            if (length(self$options$varLst) < 1 | length(self$options$varID) < 1 | length(self$options$varTme) < 1) {
                jmvcore::reject(.("“Variables that identify the same unit”, and “Variables to be transformed” are required and must not be empty."), code = "")
            }

            txtOut <- c()
            jmvReadWrite::wide2long_omv(dtaInp = crrDta, fleOut = fleOut, varLst = self$options$varLst, varID = self$options$varID,
                                        varTme = self$options$varTme, varExc = self$options$varExc, varSep = self$options$varSep, excLvl = excLvl)

            while (!file.exists(fleOut)) Sys.sleep(0.01)
            if (file.exists(fleOut)) {
                txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
                self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))
            } else {
                jmvcore::reject(.("Error when writing '{file}' to '{dir}'."), code = '', file = basename(fleOut), dir = dirname(fleOut))
            }

        })
)
