#' @importFrom jmvcore .
jtMergeColsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(
        .crrCmd = "jmvReadWrite::merge_cols_omv",
        .crrDta = NULL,
        .fleInp = NULL,
        .nonLtd = FALSE,

        .init = function() {
            if (private$.chkVar()) {
                # calculate the current data
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)), c(private$.crrArg(), list(fleOut = NULL)))
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(), nonLtd = private$.nonLtd)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # assemble or reset data set / create information
            private$.dtaInf()
            if (private$.chkVar()) {
                # assemble data set / create information
                self$results$dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                # hide general / additional information and show data set / create information
                private$.hlpI2D()
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                # if not, create a preview of the data (fllPvw in utils.R)
                } else {
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                }
            } else {
                # show general / additional information and hide data set / create information
                private$.hlpD2I()
            }
        },

        .chkFle = function(crrFle = "") {
            if (!file.exists(crrFle) || !jmvReadWrite:::hasExt(crrFle, jmvReadWrite:::vldExt)) {
                jmvcore::reject(.("'{file}' doesn't exists or has an unsupported file type."), file = crrFle)
            }

            jmvReadWrite:::nrmFle(crrFle)
        },

        .chkVar = function() {
            if (!is.null(self$options$fleInp) && !is.null(private$.fleInp) && all(vapply(private$.fleInp, grepl, logical(1), self$options$fleInp))) {
                return(length(self$options$varBy) > 0 && dim(self$readDataset())[1] >= 1)
            } else if (!is.null(self$options$fleInp) && nzchar(self$options$fleInp)) {
                private$.fleInp <- vapply(trimws(strsplit(self$options$fleInp, ";")[[1]]), private$.chkFle, character(1), USE.NAMES = FALSE)
                return(length(self$options$varBy) > 0 && dim(self$readDataset())[1] >= 1)
            } else {
                private$.fleInp <- NULL
                return(FALSE)
            }
        },

        .colFst = function() {
            colNme <- names(private$.crrDta)
            colBy  <- self$options$varBy
            colDta <- setdiff(names(self$readDataset()), colBy)
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
            dtaFrm <- self$readDataset()
            attr(dtaFrm, "fleInp") <- private$.fleInp
            list(dtaInp = dtaFrm, varBy = self$options$varBy, typMrg = self$options$typMrg)
        },

        .crtMsg = function() {
            if (self$options$btnCrt) return(NULL)

            sprintf("%s <strong>%s</strong> %s", .("Pressing the"), .("\"Create\"-button opens the modified data set"),
                    .(" in a new jamovi window."))
        },

        .dtaInf = function() {
            if (private$.chkVar()) {
                self$results$dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                self$results$dtaInf$setVisible(TRUE)
            } else {
                self$results$dtaInf$setVisible(FALSE)
            }
        },

        .dtaMsg = function() {
            sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", "))
        }

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
