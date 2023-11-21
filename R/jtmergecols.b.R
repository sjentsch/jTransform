jtMergeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(
        .fleInp = NULL,
        .mrgDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.mrgDta <- do.call(jmvReadWrite::merge_cols_omv, private$.crrArg())
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.mrgDta, colFst = private$.colFst())
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least one variable in varBy, that fleInp isn't empty,
            # and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if CREATE was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::merge_cols_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use CREATE as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, dtaFrm = private$.mrgDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.mrgDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpMrg)
            }
        },
        
		.chkVar = function() {
            if (!is.null(self$options$fleInp) && !is.null(private$.fleInp) && all(sapply(private$.fleInp, grepl, self$options$fleInp, USE.NAMES=FALSE))) {
                return(length(self$options$varBy) > 0 && TRUE)
			} else if (!is.null(self$options$fleInp) && nzchar(self$options$fleInp)) {
			    fleInp <- trimws(strsplit(self$options$fleInp, ";")[[1]])
			    for (i in seq_along(fleInp)) {
			        if (!file.exists(fleInp[i])) jmvcore::reject("'{file}' doesn't exists.", file = fleInp[i])
			        fleInp[i] <- jmvReadWrite:::nrmFle(fleInp[i]);
			    }
			    private$.fleInp <- fleInp
			    return(length(self$options$varBy) > 0 && TRUE)
			} else {
			    private$.fleInp <- NULL
			    return(FALSE)
			}
        },

        .colFst = function() {
            colNme <- names(private$.mrgDta)
            colBy  <- self$options$varBy
            colDta <- setdiff(names(self$data), colBy)
            colMrg <- setdiff(colNme, c(colBy, colDta))
            numOth <- (maxCol - length(colBy))
            numHlO <- numOth / 2
            numDta <- length(colDta)
            numMrg <- length(colMrg)
            numOfs <- ifelse(length(colNme) > maxCol, 1, 0)
            if (all(c(numDta, numMrg) >= numHlO)) {
                c(colBy, colDta[seq(floor(numHlO))], colMrg[seq(ceiling(numHlO))])
            } else if (numDta >= numHlO) {
                c(colBy, colDta[seq(numOth - numMrg - numOfs)], colMrg)
            } else if (numMrg >= numHlO) {
                c(colBy, colDta, colMrg[seq(numOth - numDta - numOfs)])
            } else {
                c(colBy, colDta, colMrg)
            }
        },
         
        .crrArg = function() {
            # attach further input files as attribute fleInp to the data frame
            # and assemble the arguments for merge_cols_omv
            crrDta <- self$readDataset()
            attr(crrDta, "fleInp") <- private$.fleInp
            list(dtaInp = crrDta, fleOut = NULL, varBy = self$options$varBy, typMrg = self$options$typMrg)
		}
        
    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) {
                paste0("attr(data, \"fleInp\") <- c(\n    \"", paste0(private$.fleInp, collapse = "\",\n    \""), "\")\n",
                       fmtSrc("jmvReadWrite::merge_cols_omv", private$.crrArg()[c(-1, -2)]))
            }
        }

    )
)
