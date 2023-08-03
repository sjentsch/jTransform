jtMergeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varBy) > 0 && nzchar(self$options$fleInp) && dim(self$data)[2] > 1) {
                # attach further input files as attribute fleInp to the data frame
                # and assemble the arguments for merge_cols_omv
                crrDta <- self$data
                attr(crrDta, "fleInp") <- private$.fndFlI()
                crrArg <- list(dtaInp = crrDta, fleOut = NULL, varBy = self$options$varBy, typMrg = self$options$typMrg)

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::merge_cols_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::merge_cols_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }
            
        },

		.fndFlI = function() {
			fleInp <- trimws(strsplit(self$options$fleInp, ";")[[1]])
			for (i in seq_along(fleInp)) {
				if (dirname(fleInp[i]) == ".") {
				    tmpFlN <- list.files(path = hmeDir(), pattern = fleInp[i], recursive = TRUE);
				    if (length(tmpFlN) == 1) {
				        fleInp[i] <- file.path(hmeDir(), tmpFlN);
				    } else {
				        jmvcore::reject("'{file}' either doesn't exists or exists more than once in the home directory. Please add the exact path under “Input file(s)”.", file = fleInp[i])
				    }
				} else {
				    fleInp[i] <- jmvReadWrite:::nrmFle(fleInp[i]);
				}
			}
			return(fleInp)
		}
        
    )
)
