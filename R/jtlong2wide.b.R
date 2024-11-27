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

        # common functions are in incFnc.R
        .run = commonFunc$private_methods$.runRpM,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varID) > 0 &&
             length(self$options$varTme) > 0 &&
             length(self$options$varTgt) > 0 &&
             nzchar(self$options$varSep))
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
            list(dtaInp = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset(),
                 varID  = self$options$varID,  varTme = self$options$varTme, varTgt = self$options$varTgt,
                 varExc = self$options$varExc, varSep = self$options$varSep, varOrd = self$options$varOrd,
                 varAgg = self$options$varAgg)
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .prpRpM = function(xfmDta = NULL) {
            # exclude self$options$varID and self$options$varExc
            #
            # self$options$varTgt -> names / grepl
            varTme <- self$options$varTme
            numTme <- length(varTme)
            orgDta <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            tblFrq <- as.data.frame(table(orgDta[, varTme[seq(numTme, 1)], drop = FALSE]))[, seq(numTme + 1, 1)]
            varTgt <- sort(self$options$varTgt)
            nmeTgt <- sort(names(xfmDta)[grepl(paste0(paste0("^", varTgt), collapse = "|"), names(xfmDta))])
            nmeTgt <- as.data.frame(apply(matrix(nmeTgt, ncol = length(varTgt), dimnames = list(c(), varTgt)), 2, sort), row.names = NULL)
            cbind(tblFrq[, -1, drop = FALSE], nmeTgt, tblFrq[, 1, drop = FALSE])
        }

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
