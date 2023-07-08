jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(

        .run = function() {
            # if not blnOut -> exit
print("jtTranspose")
print(self$options$blnOut)
print(str(self$options))
print(self$options$.getData())
            if (!self$options$blnOut) return()

            # assemble (add directory - HOME on *nix, USERPROFILE on Windows -
            # if necessary) and check existence of the directory, that the file
            # doesn't exist already and the correct file extension
            fleOut <- chkFle(self$options$fleOut)

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)
            
            txtOut <- c()
            # issue a warning if only a very small number of variables is in the output dataset
            if (dim(crrDta)[2] - length(self$options$varNme) < 5)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in th output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Variables to be transposed”."))

            # ensure the correct format of the variable
            varNme <- ifelse(is.null(self$options$varNme), "", self$options$varNme)
            
            # assemble and run jmvReadWrite command
            eval(parse(text = "jmvReadWrite::transpose_omv(dtaInp = crrDta, fleOut = fleOut, varNme = varNme)"))
            txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
            
            self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))
            self$options$initialize()
            
        })
)
