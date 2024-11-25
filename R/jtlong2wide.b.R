#' @importFrom jmvcore .
jtLong2WideClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(
        .crrCmd = "jmvReadWrite::long2wide_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                # calculate the current data
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)), c(private$.crrArg(), list(fleOut = NULL)))
                private$.rpmDta <- private$.prpRpM(xfmDta = private$.crrDta)
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(), nonLtd = private$.nonLtd)
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta,                             nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # assemble or reset data set / create information
            private$.dtaInf()
            if (private$.chkVar()) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                # if not, create a preview of the data and of the repeated measurement levels (fllPvw in utils.R)
                } else {
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                    fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta)
                }
            }
        },

        .chkVar = function() {
            (length(self$options$varID) > 0 &&
             length(self$options$varTme) > 0 &&
             length(self$options$varTgt) > 0 &&
             nzchar(self$options$varSep) &&
             dim(self$readDataset())[1] >= 1)
        },

        .colFst = function() {
            colNme <- names(private$.crrDta)
            colOth <- c(self$options$varID, self$options$varExc)
            colTgt <- self$options$varTgt
            numRmg <- (min(c(length(colNme), maxCol)) - length(colOth))
            numTgt <- length(colTgt)
            lngTgt <- floor(rep(numRmg / numTgt, numTgt))
            if (sum(lngTgt) < numRmg) lngTgt[numTgt] <- lngTgt[numTgt] + 1
            varLst <- colOth
            for (i in seq(numTgt)) {
                if (sum(lngTgt) < numRmg) lngTgt[i] <- lngTgt[i] + 1
                crrTgt <- colNme[grepl(paste0("^", colTgt[i]), colNme)]
                varLst <- c(varLst, crrTgt[seq(lngTgt[i])])
            }
            varLst
        },

        .crrArg = function() {
            list(dtaInp = self$readDataset(),  varID  = self$options$varID,  varTme = self$options$varTme,
                 varTgt = self$options$varTgt, varExc = self$options$varExc, varSep = self$options$varSep,
                 varOrd = self$options$varOrd, varAgg = self$options$varAgg)
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
        },

        .prpRpM = function(xfmDta = NULL) {
            # exclude self$options$varID and self$options$varExc
            #
            # self$options$varTgt -> names / grepl
            varTme <- self$options$varTme
            numTme <- length(varTme)
            orgDta <- self$readDataset()
            tblFrq <- as.data.frame(table(orgDta[, varTme[seq(numTme, 1)], drop = FALSE]))[, seq(numTme + 1, 1)]
            varTgt <- sort(self$options$varTgt)
            nmeTgt <- sort(names(xfmDta)[grepl(paste0(paste0("^", varTgt), collapse = "|"), names(xfmDta))])
            nmeTgt <- as.data.frame(apply(matrix(nmeTgt, ncol = length(varTgt), dimnames = list(c(), varTgt)), 2, sort), row.names = NULL)
            cbind(tblFrq[, -1, drop = FALSE], nmeTgt, tblFrq[, 1, drop = FALSE])
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg()[-1])
        }

    )
)
