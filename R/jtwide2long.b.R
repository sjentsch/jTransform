jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varID) > 0 && length(self$options$varLst) > 0 && nzchar(self$options$varTme) && nzchar(self$options$varSep)) {
                # assemble the arguments for wide2long_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varID  = self$options$varID, varTme = self$options$varTme,
                               varLst = self$options$varLst, varExc = self$options$varExc, varSep = self$options$varSep,
                               excLvl = eval(parse(text = paste0("as.integer(c(", self$options$excLvl, "))"))))

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::wide2long_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(oldPvw(do.call(jmvReadWrite::wide2long_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
