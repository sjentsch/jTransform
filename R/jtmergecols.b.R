jtMergeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varBy) > 0 && dim(self$data)[2] > 1 && nzchar(self$options$fleInp)) {

                # add column attributes (measureType and dataTye)
                # and attach further input files as attribute fleInp
                crrDta <- jmvReadWrite:::jmvAtt(self$data)
                attr(crrDta, "fleInp") <- private$.fndFlI()

                # assemble and run jmvReadWrite command
                dtaFrm <- jmvReadWrite::merge_cols_omv(dtaInp = crrDta, varBy = self$options$varBy, typMrg = self$options$typMrg)

                # preview the data (crtPvw in utils.R)
                self$results$txtPvw$setContent(crtPvw(dtaFrm))

                # if CREATE was pressed (blnOut == TRUE), open a new jamovi session with the data
                if (self$options$blnOut) {
                    tmpOut <- paste0(tempfile(), ".omv")
                    jmvReadWrite::write_omv(dtaFrm, fleOut = tmpOut)
# TO-DO: replace Dataset with the name of the current data set
                    system(paste0(jmvEXE(), " --temp --title=\"", paste0("Dataset", "_mergeCols"), "\" ", tmpOut))
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
