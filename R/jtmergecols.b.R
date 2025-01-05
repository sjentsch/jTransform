#' @importFrom jmvcore .
jtMergeColsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(
        .crrCmd = "jmvReadWrite::merge_cols_omv",
        .crrDta = NULL,
        .fleInp = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkFle = function(crrFle = "") {
            if (!file.exists(crrFle) || !jmvReadWrite:::hasExt(crrFle, jmvReadWrite:::vldExt)) {
                jmvcore::reject(.("'{file}' doesn't exists or has an unsupported file type."), file = crrFle)
            }

            jmvReadWrite:::nrmFle(crrFle)
        },

        .chkVar = function() {
            if (!is.null(self$options$fleInp) && !is.null(private$.fleInp) && all(vapply(private$.fleInp, grepl, logical(1), self$options$fleInp))) {
                return(length(self$options$varBy) > 0)
            } else if (!is.null(self$options$fleInp) && nzchar(self$options$fleInp)) {
                private$.fleInp <- vapply(trimws(strsplit(self$options$fleInp, ";")[[1]]), private$.chkFle, character(1), USE.NAMES = FALSE)
                return(length(self$options$varBy) > 0)
            } else {
                private$.fleInp <- NULL
                return(FALSE)
            }
        },

        .colFst = function() {
            dtaFrm <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            colNme <- names(private$.crrDta)
            colBy  <- self$options$varBy
            colDta <- setdiff(names(dtaFrm), colBy)
            colMrg <- setdiff(colNme, c(colBy, colDta))
            numOth <- (maxCol - length(colBy))
            numHlO <- numOth / 2
            numDta <- length(colDta)
            numMrg <- length(colMrg)
            numOfs <- ifelse(length(colNme) > maxCol, 1, 0)
            if (all(c(numDta, numMrg) >=  numHlO)) {
                varLst <- c(colBy, colDta[seq(floor(numHlO))], colMrg[seq(ceiling(numHlO))])
            } else if (numDta >=  numHlO) {
                varLst <- c(colBy, colDta[seq(numOth - numMrg - numOfs)], colMrg)
            } else if (numMrg >=  numHlO) {
                varLst <- c(colBy, colDta, colMrg[seq(numOth - numDta - numOfs)])
            } else {
                varLst <- c(colBy, colDta, colMrg)
            }

            crrFtN <- sprintf(paste(.("The column%s %s %s shown first in this preview. In the"),
                                    .("created data set, the variable order is as shown in"),
                                    .("\"Variables in the Output Data Set\" above this table.")),
                              ifelse(length(varLst) > 1, "s", ""),
                              paste0(varLst, collapse = ", "),
                              ifelse(length(varLst) > 1, "are", "is"))
            attr(varLst, "note") <- crrFtN

            varLst
        },

        .crrArg = function() {
            # attach further input files as attribute fleInp to the data frame
            # and assemble the arguments for merge_cols_omv
            dtaFrm <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            attr(dtaFrm, "fleInp") <- private$.fleInp
            list(dtaInp = dtaFrm, varBy = self$options$varBy, typMrg = self$options$typMrg)
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) {
                paste0("attr(data, \"fleInp\") <- c(\n    \"", paste0(private$.fleInp, collapse = "\",\n    \""), "\")\n",
                       fmtSrc(private$.crrCmd, private$.crrArg()[-1]))
            }
        }

    )
)
