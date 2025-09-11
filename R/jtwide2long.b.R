#' @importFrom jmvcore .
jtWide2LongClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .crrCmd = "jmvReadWrite::wide2long_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                # calculate the current data
                crrArg <- private$.crrArg()
                # check whether ID is unique and whether all rows are filled
                if (!all(nzchar(crrArg$dtaInp[, crrArg$varID])) || any(duplicated(crrArg$dtaInp[, crrArg$varID])))
                    jmvcore::reject(jmvcore::format(.("The values in {0} can not be empty and they need to be unique."), crrArg$varID))
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)), c(crrArg, list(fleOut = NULL)))
                private$.crrDta <- private$.adjRes(dtaFrm = private$.crrDta)
                private$.rpmDta <- private$.prpRpM(dtaFrm = private$.crrDta)
                # resize / prepare the output table (prpPvw in utils.R) for both data preview and rep. measures overview
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(), nonLtd = private$.nonLtd)
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta,                             nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        # common functions are in incFnc.R
        .run = commonFunc$private_methods$.runRpM,

        .adjRes = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L ==  "NSA") {
                selClm <- grepl(paste0("^cond[0-9]*$"), names(dtaFrm))
                dtaFrm[, selClm] <- vapply(dtaFrm[, selClm, drop = FALSE], function(x) as.integer(as.character(x)), integer(dim(dtaFrm)[1]))
                names(dtaFrm)[selClm] <- vapply(self$options$idxNSA, "[[", character(1), "var")
            } else if (self$options$mdeW2L ==  "NSS") {
                selClm <- grepl(paste0("^cond$"),       names(dtaFrm))
                dtaFrm[, selClm] <- vapply(dtaFrm[, selClm], function(x) as.integer(as.character(x)), integer(1))
                names(dtaFrm)[selClm] <- self$options$idxNSS
            }

            dtaFrm
        },

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkNSA = function() {
            xfmNSA <- self$options$xfmNSA
            resNSA <- sapply(xfmNSA, "[[", "vars")
            idxNSA <- self$options$idxNSA
            lvlNSA <- vapply(idxNSA, function(x) as.integer(c(x[["levels"]], NA))[1], integer(1))
            (is.list(xfmNSA) && length(xfmNSA) > 0 && is.matrix(resNSA) && all(dim(resNSA) >=  c(1, 1)) &&
             is.list(idxNSA) && length(idxNSA) > 0 && !any(is.na(lvlNSA)) && all(lvlNSA > 0) && prod(lvlNSA) ==  dim(resNSA)[1])
        },

        .chkSep = function() {
            xfmSep <- self$options$xfmSep
            chrSep <- self$options$chrSep
            length(xfmSep) > 0 && nzchar(self$options$pfxSep) && nzchar(chrSep) && all(grepl(chrSep, xfmSep))
        },

        .chkVar = function() {
            ((self$options$mdeW2L ==  "Sep" && private$.chkSep()) ||
             (self$options$mdeW2L ==  "NSS" && length(self$options$xfmNSS) > 0 && nzchar(self$options$idxNSS) && nzchar(self$options$tgtNSS)) ||
             (self$options$mdeW2L ==  "NSA" && private$.chkNSA()))
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            if        (self$options$mdeW2L ==  "Sep") {
                list(dtaInp = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset(),
                     varID = self$options$id_Sep,  varTme = self$options$pfxSep, varLst = self$options$xfmSep,
                     varExc = self$options$excSep, varSep = self$options$chrSep, excLvl = private$.lvl2Nm())
            } else if (self$options$mdeW2L ==  "NSS") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,       varID = self$options$id_NSS,  varLst = rnmRes$tgtLst,
                     varExc = self$options$excNSS, varSep = "_", excLvl = 1)
            } else if (self$options$mdeW2L ==  "NSA") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,       varID =  self$options$id_NSA, varLst = rnmRes$tgtLst,
                     varExc = self$options$excNSA, varTme = vapply(self$options$idxNSA, "[[", character(1), "var"),
                     varSep = "_", excLvl = 1)
            }
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .lvl2Nm = function() {
            eval(parse(text = paste0("as.integer(c(", self$options$lvlSep, "))")))
        },

        # create data frame with index variable / conditions, target variables and frequency
        .prpRpM = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L ==  "Sep") {
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
            } else if (self$options$mdeW2L ==  "NSS") {
                tblFrq <- as.data.frame(table(dtaFrm[, self$options$idxNSS]))
                cbind(setNames(tblFrq[1], self$options$idxNSS), as.data.frame(self$options$xfmNSS, nm = self$options$tgtNSS), tblFrq[2])
            } else if (self$options$mdeW2L ==  "NSA") {
                varTgt <- setNames(as.data.frame(lapply(self$options$xfmNSA, "[[", "vars")), vapply(self$options$xfmNSA, "[[", character(1), "label"))
                tblFrq <- as.data.frame(table(dtaFrm[, vapply(self$options$idxNSA, "[[", character(1), "var"), drop = FALSE]))
                colFrq <- dim(tblFrq)[2]
                cbind(tblFrq[-colFrq], varTgt, tblFrq[colFrq])
            }
        },

        .rnmDta = function() {
            dtaFrm <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            if        (self$options$mdeW2L ==  "NSS") {
                tgtLst <- paste0(self$options$tgtNSS, private$.spfNum(length(self$options$xfmNSS)))
                selClm <- (names(dtaFrm) %in% self$options$xfmNSS)
                names(dtaFrm)[selClm] <- tgtLst
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSS, self$options$excNSS, tgtLst))], tgtLst = tgtLst)
            } else if (self$options$mdeW2L ==  "NSA") {
                tgtLst <- as.list(vapply(self$options$xfmNSA, "[[", character(1), "label"))
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
                if (self$options$mdeW2L ==  "Sep") {
                    fmtSrc(private$.crrCmd, private$.crrArg()[-1])
                } else {
                    crrSrc <- "\n    data = data"
                    nmeOpt <- names(private$.options$options)
                    nmeOpt <- grepl(paste0(self$options$mdeW2L, "$|^mdeW2L$"), nmeOpt)
                    for (crrOpt in private$.options$options[nmeOpt]) {
                        srcOpt <- private$.sourcifyOption(crrOpt)
                        if (!base::identical(srcOpt, "")) {
                            crrSrc <- paste0(crrSrc, ",\n    ", srcOpt)
                        }
                    }
                    paste0(private$.package, "::", private$.name, "(", crrSrc, ")")
                }
            }
        }

    )
)
