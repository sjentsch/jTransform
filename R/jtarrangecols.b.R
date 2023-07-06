jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(

        .run = function() {
            # if not blnOut -> exit
            if (!self$options$blnOut) return()

            # assemble (add directory - HOME on *nix, USERPROFILE on Windows -
            # if necessary) and check existence of the directory, that the file
            # doesn't exist already and the correct file extension
            fleOut <- chkFle(self$options$fleOut)
            
            # add column attributes (measureType and dataTye)
            crrDta <- addAtt(self$data)

            txtOut <- c()
            # assemble and run jmvReadWrite command
            eval(parse(text = "jmvReadWrite::arrange_cols_omv(crrDta, fleOut = fleOut, varOrd = self$options$varOrd)"))
            txtOut <- c(txtOut, sprintf("%s successfully written to %s.", basename(fleOut), dirname(fleOut)))
            
            self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))

        })
)
