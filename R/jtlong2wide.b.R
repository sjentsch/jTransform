jtLong2WideClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(
        .l2wDta = NULL,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.l2wDta <- do.call(jmvReadWrite::long2wide_omv, private$.crrArg())
                private$.rpmDta <- private$.prpRpM(dtaFrm = private$.l2wDta)
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.l2wDta, colFst = private$.colFst())
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
                    crtInf(crrInf = self$results$genInf, dtaFrm = private$.l2wDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.l2wDta)
                    fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpL2W)
            }
        },

        .chkVar = function() {
            length(self$options$varID) > 0 && length(self$options$varTme) > 0 && length(self$options$varTgt) > 0 && nzchar(self$options$varSep)
        },

        .colFst = function() {
            colNme <- names(private$.l2wDta)
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
            list(dtaInp = self$readDataset(),  fleOut = NULL, varID  = self$options$varID,  varTme = self$options$varTme,
                 varTgt = self$options$varTgt, varExc = self$options$varExc, varSep = self$options$varSep,
                 varOrd = self$options$varOrd, varAgg = self$options$varAgg)
        },
        
        .prpRpM = function(dtaFrm = NULL) {
            # exclude self$options$varID and self$options$varExc
            # 
            # self$options$varTgt -> names / grepl
            varTme <- self$options$varTme
            numTme <- length(varTme)
            tblFrq <- as.data.frame(table(self$readDataset()[, varTme[seq(numTme, 1)]]))[, seq(numTme + 1, 1)]
            varTgt <- sort(self$options$varTgt)
            nmeTgt <- sort(names(dtaFrm)[grepl(paste0(paste0("^", varTgt), collapse = "|"), names(dtaFrm))])
            nmeTgt <- as.data.frame(apply(matrix(nmeTgt, ncol = length(varTgt), dimnames = list(c(), varTgt)), 2, sort))
            cbind(tblFrq[-1], nmeTgt, tblFrq[1])
        }        

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::long2wide_omv", private$.crrArg()[c(-1, -2)])
        }

    )
)
