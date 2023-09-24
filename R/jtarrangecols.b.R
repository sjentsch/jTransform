jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(
        .varLst = NULL,
        .notRun = TRUE,
        
        .init = function() {
print(rep(names(self$data), self$options$blnAll))
            private$.varLst <- unique(c(self$options$varOrd, rep(names(self$data), self$options$blnAll)))
print(private$.varLst)
            # initialize the variable information and the data preview
            self$results$varInf$setContent(paste0("<p><strong>Variables in the Output Dataset:</strong></p><p>",
                                                  paste(private$.varLst, collapse = ", "), "</p>"))
#           rszTbl()
            private$.notRun <- FALSE
        },

        .run = function() {
            if (private$.notRun) return()

            # check whether all required variables are present
            if (length(self$options$varOrd) > 1 && dim(self$data)[1] > 0) {
                # assemble the arguments for arrange_cols_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varOrd = private$.varLst)

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::arrange_cols_omv, crrArg[-2])
# perhaps: setState
#                   self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    dtaFrm <- do.call(jmvReadWrite::arrange_cols_omv, crrArg)
                    crtPvw(self$results$pvwDta, dtaFrm[, private$.varLst])
                    # self$results$txtPvw$setContent(oldPvw())
                }
            }

        }

    )
)
