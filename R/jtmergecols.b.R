#' @importFrom jmvcore .
jtMergeColsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtMergeColsClass",
    inherit = jtMergeColsBase,

    private = list(
        .crrCmd = "jmvReadWrite::merge_cols_omv",
        .fleInp = NULL,
        .nonLtd = FALSE,
        .sfxTtl = "mrg_cols",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = TRUE, # run data transformation at .init() - difficult to figure out the rows / columns after transformation
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = incFnc$private_methods$.chkDtF,

        .chkFle = function(crrFle = "") {
            if (file.exists(crrFle) && jmvReadWrite:::hasExt(crrFle, jmvReadWrite:::vldExt)) {
                TRUE
            } else {
                jmvcore::reject(.("'{file}' doesn't exist or has an unsupported file type."), file = crrFle)
            }
        },

        .chkVar = function() {
            if (length(self$options$fleInp) > 0) {
                tmpFlI <- vapply(self$options$fleInp, "[[", character(1), "path")
                if (is.null(private$.fleInp) || !identical(private$.fleInp, tmpFlI)) {
                    private$.fleInp <- tmpFlI
                }
                all(vapply(private$.fleInp, private$.chkFle, logical(1))) && length(self$options$varBy) > 0
            } else {
                private$.fleInp <- NULL
                FALSE
            }
        },

        .colFst = function() {
            dtaFrm <- if (!is.null(self$data) && nrow(self$data) > 0) self$data else self$readDataset()
            colNme <- names(private$.xfmDta)
            colBy  <- self$options$varBy
            colDta <- setdiff(names(dtaFrm), colBy)
            colMrg <- setdiff(colNme, c(colBy, colDta))
            numOth <- (maxCol - length(colBy))
            numHlO <- numOth / 2
            numDta <- length(colDta)
            numMrg <- length(colMrg)
            numOfs <- ifelse(length(colNme) > maxCol, 1, 0)
            if (all(c(numDta, numMrg) >=  numHlO)) {
                varLst <- c(colBy, colDta[seq_len(max(0, floor(numHlO)))], colMrg[seq_len(ceiling(numHlO))])
            } else if (numDta >=  numHlO) {
                varLst <- c(colBy, colDta[seq_len(max(0, numOth - numMrg - numOfs))], colMrg)
            } else if (numMrg >=  numHlO) {
                varLst <- c(colBy, colDta, colMrg[seq_len(max(0, numOth - numDta - numOfs))])
            } else {
                varLst <- c(colBy, colDta, colMrg)
            }

            if (length(varLst) > 1) {
                ln1FtN <- .("The columns {} are shown first in this preview.")
            } else {
                ln1FtN <- .("The column {} is shown first in this preview.")
            }
            ln2FtN <- .("In the created data set, the variable order is as shown in \"Variables in the Output Data Set\" above this table.")
            attr(varLst, "note") <- paste(jmvcore::format(ln1FtN, paste0(varLst, collapse = ", ")), ln2FtN)

            varLst
        },

        .crrArg = function(getDta = TRUE) {
            # attach further input files as attribute fleInp to the data frame
            # and assemble the arguments for merge_cols_omv
            if (getDta) {
                dtaFrm <- private$.getDta(self$options$varBy)$dtaInp
                attr(dtaFrm, "fleInp") <- private$.fleInp
                list(dtaInp = dtaFrm, varBy = self$options$varBy, typMrg = self$options$typMrg)
            } else {
                list(varBy = self$options$varBy, typMrg = self$options$typMrg)
            }
        },

        .crtMsg = incFnc$private_methods$.crtMsg,
        .dtaInf = incFnc$private_methods$.dtaInf,
        .dtaMsg = incFnc$private_methods$.dtaMsg,
        .getDta = incFnc$private_methods$.getDta,
        .nteRnC = incFnc$private_methods$.nteRnC,
        .runXfm = incFnc$private_methods$.runXfm

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) {
                paste0("# the syntax below assumes that the files you want to merge are in the working directory",
                       "# if this is not the case, you have to add a path",
                       "attr(data, \"fleInp\") <- c(\n    \"", paste0(private$.fleInp, collapse = "\",\n    \""), "\")\n",
                       fmtSrc(private$.crrCmd, private$.crrArg(FALSE)))
            }
        }

    )
)
