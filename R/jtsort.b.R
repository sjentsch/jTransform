jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(

        .run = function() {
            # if not blnOut -> exit
            if (!self$options$blnOut) return()

            # assemble (add directory - HOME on *nix, USERPROFILE on Windows -
            # if necessary) and check existence of the directory, that the file
            # doesn't exist already and the correct file extension
            fleOut <- chkFle(self$options$fleOut)

            # add column attributes (measureType and dataTye)
            crrDta <- jmvReadWrite:::jmvAtt(self$data)
            
            txtOut <- c()
            # issue a warning if only a very small number of variables is in the output dataset
            if (dim(crrDta)[2] - length(self$options$varSrt) < 5)
                txtOut <- c(txtOut, paste("<b>You only included are very small number of variables in th output data set.</b>",
                                          "Was this intended? Remember to assign variables to be included in the output to",
                                          "“Further variables in the output”."))
            
            # assemble vector for sorting
            varSrt <- paste0(gsub("descend", "-", gsub("ascend", "",
                          sapply(self$options$ordSrt, "[[", "order"))),
                          sapply(self$options$ordSrt, "[[", "var"))

            # assemble and run jmvReadWrite command
            eval(parse(text = "jmvReadWrite::sort_omv(dtaInp = crrDta, fleOut = fleOut, varSrt = varSrt)"))
            txtOut <- c(txtOut, sprintf("<b>%s</b> successfully written to %s.", basename(fleOut), dirname(fleOut)))
            
            self$results$txtOut$setContent(paste(txtOut, collapse = "<br>\n"))

        })
)
