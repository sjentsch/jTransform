#' @importFrom jmvcore .
jtLong2WideClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(
        .crrDta = NULL,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.crrDta <- do.call(jmvReadWrite::long2wide_omv, private$.crrArg())
                private$.rpmDta <- private$.prpRpM(xfmDta = private$.crrDta)
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst())
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least one variable in varBy, that fleInp isn't empty,
            # and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::long2wide_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, infMsg = private$.crtMsg())
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                    fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, infMsg = private$.infMsg())
            }
        },

        .chkVar = function() {
            length(self$options$varID) > 0 && length(self$options$varTme) > 0 && length(self$options$varTgt) > 0 && nzchar(self$options$varSep)
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
            if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
            list(dtaInp = dtaFrm,  fleOut = NULL, varID  = self$options$varID, varTme = self$options$varTme,
                 varTgt = self$options$varTgt, varExc = self$options$varExc, varSep = self$options$varSep,
                 varOrd = self$options$varOrd, varAgg = self$options$varAgg)
        },

        .crtMsg = function() {
            crtMsg <- sprintf("%s <strong>%s</strong> %s", .("Pressing the"),
                              .("\"Create\"-button opens the modified data set"), .(" in a new jamovi window."))
            if (!is.null(private$.crrDta)) {
                c(sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", ")),
                  crtMsg)
            } else {
                crtMsg
            }
        },

        .infMsg = function() {
            c(paste(.("Please assign the variables that identify participant (or another measurement unit; e.g., a"),
                    .("number or an ID) to \"Variables that identify the same unit\", and those that are unique to"),
                    .("an unit but not an identifier (e.g., gender, age group) to \"Variables NOT to be Transformed\"."),
                    .("\"Variables That Differentiate Within a Unit\" typically contain different (e.g., experimental)"),
                    .("conditions, and \"Variables To Be Transformed\" are the actual measurements (e.g., responses,"),
                    .("reaction times, etc.).")),
              paste(.("For an example about a typical long-to-wide-transformation, see the last paragraph in"),
                    .("\"Details\" underneath the output tables.")))
        },

        .prpRpM = function(xfmDta = NULL) {
            # exclude self$options$varID and self$options$varExc
            #
            # self$options$varTgt -> names / grepl
            varTme <- self$options$varTme
            numTme <- length(varTme)
            if (!is.null(self$data) && dim(self$data)[1] > 0) orgDta <- self$data else orgDta <- self$readDataset()
            tblFrq <- as.data.frame(table(orgDta[, varTme[seq(numTme, 1)], drop = FALSE]))[, seq(numTme + 1, 1)]
            varTgt <- sort(self$options$varTgt)
            nmeTgt <- sort(names(xfmDta)[grepl(paste0(paste0("^", varTgt), collapse = "|"), names(xfmDta))])
            nmeTgt <- as.data.frame(apply(matrix(nmeTgt, ncol = length(varTgt), dimnames = list(c(), varTgt)), 2, sort), row.names = NULL)
            cbind(tblFrq[, -1, drop = FALSE], nmeTgt, tblFrq[, 1, drop = FALSE])
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::long2wide_omv", private$.crrArg()[-seq(2)])
        }

    )
)
