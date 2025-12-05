#' @importFrom jmvcore .
jtWide2LongClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .crrCmd = "jmvReadWrite::wide2long_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .rpmDta = NULL,
        .sfxTtl = "Wide2Long",

        .init = function() {
            if (private$.chkVar()) {
                # check whether the ID variable is unique, if so calculate the current data
                private$.unq_ID(private$.crrArg())
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)),
                                           c(private$.crrArg(), list(fleOut = NULL)))
                private$.crrDta <- private$.adjRes(dtaFrm = private$.crrDta)
                private$.rpmDta <- private$.prpRpM(dtaFrm = private$.crrDta)
                # resize / prepare the output table (prpPvw in utils.R) for both data preview and rep. measures overview
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(),
                       nonLtd = private$.nonLtd)
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        # common functions are in incFnc.R
        .run = commonFunc$private_methods$.run,

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
                     varExc = self$options$excNSS, varSep = rnmRes$varSep, excLvl = 1)
            } else if (self$options$mdeW2L ==  "NSA") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,       varID =  self$options$id_NSA, varLst = rnmRes$tgtLst,
                     varExc = self$options$excNSA, varTme = vapply(self$options$idxNSA, "[[", character(1), "var"),
                     varSep = rnmRes$varSep, excLvl = 1)
            }
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,

        .detSep = function(varLst = c()) {
            for (varSep in c("_", ".", "-", "!", "#", "%")) {
                if (!any(vapply(varLst, function(n) grepl(varSep, n, fixed = TRUE), logical(1)))) {
                    return(varSep)
                }
            }
            jmvcore::reject(.("The Long Variables in \"Variables To Be Transformed\" are invalid, remove _, . and - from the names."))
        },

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
                varSep <- private$.detSep(self$options$tgtNSS)
                tgtLst <- paste0(self$options$tgtNSS, private$.spfNum(length(self$options$xfmNSS), varSep))
                selClm <- (names(dtaFrm) %in% self$options$xfmNSS)
                names(dtaFrm)[selClm] <- tgtLst
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSS, self$options$excNSS, tgtLst))], tgtLst = tgtLst, varSep = varSep)
            } else if (self$options$mdeW2L ==  "NSA") {
                if (any(vapply(self$options$xfmNSA, function(l) is.null(l[["label"]]) || !nzchar(l[["label"]]), logical(1)))) {
                    jmvcore::reject(.("No target Long Variables in \"Variables To Be Transformed\" can be empty."))
                }
                tgtLst <- as.list(vapply(self$options$xfmNSA, "[[", character(1), "label"))
                varSep <- private$.detSep(unlist(tgtLst))
                idxNSA <- self$options$idxNSA
                for (i in seq_along(tgtLst)) {
                    for (j in seq_along(idxNSA)) {
                        tgtLst[[i]] <- paste0(tgtLst[[i]], rep(private$.spfNum(idxNSA[[j]][["levels"]], varSep), each = length(tgtLst[[i]])))
                    }
                    selClm <- (names(dtaFrm) %in% self$options$xfmNSA[[i]][["vars"]])
                    names(dtaFrm)[selClm] <- tgtLst[[i]]
                }
                # convert tgtLst (list) into a (character) vector
                tgtLst <- unlist(tgtLst)
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSA, self$options$excNSA, tgtLst))], tgtLst = tgtLst, varSep = varSep)
            }
        },

        .spfNum = function(crrNum = NA, crrSep = "_") {
             sprintf(paste0(crrSep, "%0", as.character(ceiling(log10(crrNum + 1e-6))), "d"), seq(crrNum))
        },

        .unq_ID = function(crrArg = NULL) {
            # check whether ID is unique and whether all rows are filled
            if (!is.null(crrArg$varID) &&
                (any(duplicated(crrArg$dtaInp[, crrArg$varID])) ||
                 (is.character(crrArg$dtaInp[, crrArg$varID]) && !all(nzchar(crrArg$dtaInp[, crrArg$varID]))) ||
                 any(is.na(crrArg$dtaInp[, crrArg$varID])))) {
                jmvcore::reject(jmvcore::format(.("The values in {0} can not be empty and they need to be unique."),
                crrArg$varID))
            }
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
