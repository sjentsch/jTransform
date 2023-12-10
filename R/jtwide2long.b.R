jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .w2lDta = NULL,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.w2lDta <- private$.adjRes(dtaFrm = do.call(jmvReadWrite::wide2long_omv, private$.crrArg()))
                private$.rpmDta <- private$.prpRpM(dtaFrm = private$.w2lDta)
                # resize / prepare the output table (prpPvw in utils.R) for both data preview and rep. measures overview
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.w2lDta)
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether the required variables are present and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::wide2long_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information, and create
                # a preview of the data and of the repeated measurement levels (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, dtaFrm = private$.w2lDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.w2lDta)
                    fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta)
                }
            } else {
                # show getting started as general information and further details as additional information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = c(hlpW2L,             eval(parse(text = paste0("gen", self$options$mdeW2L)))))
                crtInf(crrInf = self$results$addInf, hlpMsg = c("<h2>Details</h2>", eval(parse(text = paste0("dtl", self$options$mdeW2L)))))
            }
        },

        .adjRes = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L == "NSA") {
                selClm <- grepl("^cond[0-9]", names(dtaFrm))
                dtaFrm[selClm] <- sapply(dtaFrm[selClm], function(x) as.integer(as.character(x)), simplify = FALSE)
                names(dtaFrm)[selClm] <- sapply(self$options$idxNSA, "[[", "var")
            } else if (self$options$mdeW2L == "NSS") {
                selClm <- grepl("^cond$", names(dtaFrm))
                dtaFrm[selClm] <- sapply(dtaFrm[selClm], function(x) as.integer(as.character(x)), simplify = FALSE)
                names(dtaFrm)[selClm] <- self$options$idxNSS
            }
            dtaFrm
        },

        .chkNSA = function() {
            idxNSA <- self$options$idxNSA
            xfmNSA <- self$options$xfmNSA
            resNSA <- sapply(xfmNSA, "[[", "vars")
            (is.list(xfmNSA) && length(xfmNSA) > 0 && is.matrix(resNSA) && all(dim(resNSA) >= c(1, 1)) &&
             is.list(idxNSA) && length(idxNSA) > 0 && !any(sapply(sapply(idxNSA, "[[", "levels"), is.null)) &&
               all(sapply(idxNSA, "[[", "levels") > 0) && prod(sapply(idxNSA, "[[", "levels")) == dim(resNSA)[1])
        },
        
        .chkSep = function() {
            xfmSep <- self$options$xfmSep
            chrSep <- self$options$chrSep
            length(xfmSep) > 0 && nzchar(self$options$pfxSep) && nzchar(chrSep) && all(grepl(chrSep, xfmSep))
        },
        
        .chkVar = function() {
            (self$options$mdeW2L == "Sep" && private$.chkSep()) ||
            (self$options$mdeW2L == "NSS" && length(self$options$xfmNSS) > 0 && nzchar(self$options$idxNSS) && nzchar(self$options$tgtNSS)) ||
            (self$options$mdeW2L == "NSA" && private$.chkNSA())
        },
         
        .crrArg = function() {
            if        (self$options$mdeW2L == "Sep") {
                list(dtaInp = self$readDataset(), fleOut = NULL, varID = self$options$id_Sep, varTme = self$options$pfxSep,
                     varLst = self$options$xfmSep, varExc = self$options$excSep, varSep = self$options$chrSep,
                     excLvl = private$.lvl2Nm())
            } else if (self$options$mdeW2L == "NSS") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,      fleOut = NULL, varID = self$options$id_NSS, 
                     varLst = rnmRes$tgtLst, varExc = self$options$excNSS, varSep = "_", excLvl = 1)
            } else if (self$options$mdeW2L == "NSA") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,      fleOut = NULL, varID =  self$options$id_NSA,
                     varLst = rnmRes$tgtLst, varExc = self$options$excNSA, varSep = "_", excLvl = 1)
            }
        },
        
        .lvl2Nm = function() {
            eval(parse(text = paste0("as.integer(c(", self$options$lvlSep, "))")))
        },

        # create data frame with index variable / conditions, target variables and frequency
        .prpRpM = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L == "Sep") {
                varID  <- ifelse(is.null(self$options$id_Sep), "ID", self$options$id_Sep)
                colOrg <- self$options$xfmSep
                colRes <- names(dtaFrm)
                selCnd <- grepl(paste0("^", self$options$pfxSep), colRes)
                selOth <- grepl(paste(paste0("^", c(varID, self$options$excSep), "$"), collapse = "|"), colRes)
                colTgt <- colRes[!(selCnd | selOth)]
                tblFrq <- as.data.frame(table(dtaFrm[, sort(which(selCnd), decreasing = TRUE)]))[, sort(seq(sum(selCnd) + 1), decreasing = TRUE)]
                varFrq <- setNames(as.data.frame(matrix(rep("", length(colOrg)), ncol = length(colTgt))), colTgt)
                if (!nzchar(self$options$lvlSep)) {
                    varFrq[, 1] <- sort(colOrg)
                } else {
                    for (i in seq_along(colTgt)) varFrq[, i] <- sort(colOrg[grepl(colTgt[i], colOrg)])
                }
                cbind(tblFrq[-1], varFrq, tblFrq[1])
            } else if (self$options$mdeW2L == "NSS") {
                tblFrq <- as.data.frame(table(dtaFrm[, self$options$idxNSS]))
                cbind(setNames(tblFrq[1], self$options$idxNSS), as.data.frame(self$options$xfmNSS, nm = self$options$tgtNSS), tblFrq[2])
            } else if (self$options$mdeW2L == "NSA") {
                varTgt <- setNames(as.data.frame(sapply(self$options$xfmNSA, "[[", "vars")), sapply(self$options$xfmNSA, "[[", "label"))
                tblFrq <- as.data.frame(table(dtaFrm[, sapply(self$options$idxNSA, "[[", "var")]))
                colFrq <- dim(tblFrq)[2]
                cbind(tblFrq[-colFrq], varTgt, tblFrq[colFrq])
            }
        },

        .rnmDta = function() {
            dtaFrm <- self$readDataset()
            if        (self$options$mdeW2L == "NSS") {
                tgtLst <- paste0(self$options$tgtNSS, private$.spfNum(length(self$options$xfmNSS)))
                selClm <- (names(dtaFrm) %in% self$options$xfmNSS)
                names(dtaFrm)[selClm] <- tgtLst
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSS, self$options$excNSS, tgtLst))], tgtLst = tgtLst)
            } else if (self$options$mdeW2L == "NSA") {
                tgtLst <- as.list(sapply(self$options$xfmNSA, "[[", "label"))
                idxNSA <- self$options$idxNSA
                for (i in seq_along(tgtLst)) {
                    for (j in seq_along(idxNSA)) {    
                        tgtLst[[i]] <- paste0(tgtLst[[i]], rep(private$.spfNum(idxNSA[[j]][["levels"]]), each = length(tgtLst[[i]])))
                    }
                    selClm <- (names(dtaFrm) %in% self$options$xfmNSA[[i]][["vars"]])
                    names(dtaFrm)[selClm] <- tgtLst[[i]]
                }
                # convert tgtLst (list) into a (character) vector
                tgtLst <- unlist(tgtLst)
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSA, self$options$excNSA, tgtLst))], tgtLst = tgtLst)
            }
        },
        
        .spfNum = function(crrNum = NA) {
             sprintf(paste0("_%0", as.character(ceiling(log10(crrNum + 1e-6))), "d"), seq(crrNum))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) {
                if (self$options$mdeW2L == "Sep") {
                    fmtSrc("jmvReadWrite::wide2long_omv", private$.crrArg()[c(-1, -2)])
                } else {
                    crrSrc <- '\n    data = data'
                    nmeOpt <- names(private$.options$options)
                    nmeOpt <- grepl(paste0(self$options$mdeW2L, "$|^mdeW2L$"), nmeOpt)
                    for (crrOpt in private$.options$options[nmeOpt]) {
                         srcOpt <- private$.sourcifyOption(crrOpt)
                         if (!base::identical(srcOpt, '')) {
                             crrSrc <- paste0(crrSrc, ',\n    ', srcOpt)
                         }
                    }
                    paste0(private$.package, '::', private$.name, '(', crrSrc, ')')
                }
            }
        }

    )
)
