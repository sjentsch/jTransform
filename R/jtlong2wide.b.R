#' @importFrom jmvcore .
jtLong2WideClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(
        .crrCmd = "jmvReadWrite::long2wide_omv",
        .nonLtd = FALSE,
        .rpmDta = NULL,
        .sfxTtl = "wide",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = TRUE, # run data transformation at .init() - difficult to figure out the rows / columns after transformation  
        .xfmRow = NA,

        .init = function() {
            # update logging flags during the init phase
            set_logflags(self$options$jxfLog)
            jinfo(sprintf("[%s]: jTransform: init phase started", private$.name))

            if (private$.chkVar()) {
                # calculate the transformed data (if requested by .xfmFst and if .xfmDta is NULL)
                if (private$.xfmFst && is.null(private$.xfmDta)) {
                    private$.xfmDta <- private$.runXfm()
                    private$.rpmDta <- private$.prpRpM(runXfm = private$.xfmDta)
                }
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, numRow = nrow(private$.xfmDta),
                       colAll = names(private$.xfmDta), colFst = private$.colFst(), nonLtd = private$.nonLtd)
                prpPvw(crrTbl = self$results$pvwLvl, numRow = nrow(private$.rpmDta),
                       colAll = names(private$.rpmDta),                             nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
            jinfo(sprintf("[%s]: jTransform: init phase ended", private$.name))
        },

        # common functions are in incFnc.R
        .run = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$varID) > 0 &&
             length(self$options$varTme) > 0 &&
             length(self$options$varTgt) > 0 &&
             nzchar(self$options$varSep))
        },

        .colFst = function() {
            colNme <- names(private$.xfmDta)
            colOth <- c(self$options$varID, self$options$varExc)
            colTgt <- self$options$varTgt
            numRmg <- (min(c(length(colNme), maxCol)) - length(colOth))
            numTgt <- length(colTgt)
            lngTgt <- pmax(0L, floor(rep(numRmg / numTgt, numTgt)))
            if (sum(lngTgt) < numRmg) lngTgt[numTgt] <- lngTgt[numTgt] + 1
            varLst <- colOth
            for (i in seq_len(numTgt)) {
                if (sum(lngTgt) < numRmg) lngTgt[i] <- lngTgt[i] + 1
                crrTgt <- colNme[startsWith(colNme, colTgt[i])]
                varLst <- c(varLst, crrTgt[seq_len(lngTgt[i])])
            }

            ln1FtN <- ifelse(length(varLst) > 1,
                             jmvcore::format(.("The columns {} are shown first in this preview."), paste0(varLst, collapse = ", ")),
                             jmvcore::format(.("The column {} is shown first in this preview."), varLst))
            ln2FtN <- .("In the created data set, the variable order is as shown in \"Variables in the Output Data Set\" above this table.")
            attr(varLst, "note") <- paste(ln1FtN, ln2FtN)

            varLst
        },

        .crrArg = function(getDta = TRUE) {
            c(if (getDta) private$.getDta(c(self$options$varID, self$options$varTme, self$options$varTgt, self$options$varExc)),
              list(varID  = self$options$varID,  varTme = self$options$varTme, varTgt = self$options$varTgt,
                   varExc = self$options$varExc, varSep = self$options$varSep, varOrd = self$options$varOrd,
                   varAgg = self$options$varAgg))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .prpRpM = function(runXfm = NULL) {
            # exclude self$options$varID and self$options$varExc
            #
            # self$options$varTgt -> names / grepl
            varTme <- self$options$varTme
            numTme <- length(varTme)
            orgDta <- if (!is.null(self$data) && nrow(self$data) > 0) self$data else self$readDataset()
            tblFrq <- as.data.frame(table(orgDta[, varTme[seq(numTme, 1)], drop = FALSE]))[, seq(numTme + 1, 1)]
            varTgt <- sort(self$options$varTgt)
            selTgt <- Reduce(`|`, lapply(varTgt, function(p) startsWith(names(runXfm), p)))
            nmeTgt <- sort(names(runXfm)[selTgt])
            nmeTgt <- as.data.frame(apply(matrix(nmeTgt, ncol = length(varTgt), dimnames = list(c(), varTgt)), 2, sort), row.names = NULL)
            cbind(tblFrq[, -1, drop = FALSE], nmeTgt, tblFrq[, 1, drop = FALSE])
        },
        
        .runXfm = commonFunc$private_methods$.runXfm

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
