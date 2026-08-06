#' @importFrom jmvcore .
jtWide2LongClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .crrCmd = "jmvReadWrite::wide2long_omv",
        .nonLtd = FALSE,
        .rpmDta = NULL,
        .sfxTtl = "long",
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
                    private$.xfmDta <- private$.adjRes(dtaFrm = private$.runXfm())
                    private$.rpmDta <- private$.prpRpM(dtaFrm = private$.xfmDta)
                }
                # resize / prepare the output table (prpPvw in utils.R) for both data preview and rep. measures overview
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

        .adjRes = function(dtaFrm = NULL) {
            crrMde <- self$options$mdeW2L
            if        (crrMde ==  "NSA") {
                selClm <- grepl(paste0("^cond[0-9]*$"), names(dtaFrm))
                dtaFrm[, selClm] <- vapply(dtaFrm[, selClm, drop = FALSE], function(x) as.integer(as.character(x)), integer(nrow(dtaFrm)))
                names(dtaFrm)[selClm] <- vapply(self$options$idxNSA, "[[", character(1), "var")
            } else if (crrMde ==  "NSS") {
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
             is.list(idxNSA) && length(idxNSA) > 0 && !any(is.na(lvlNSA)) && all(lvlNSA > 0) && prod(lvlNSA) ==  nrow(resNSA))
        },

        .chkSep = function() {
            xfmSep <- self$options$xfmSep
            chrSep <- self$options$chrSep
            length(xfmSep) > 0 && nzchar(self$options$pfxSep) && nzchar(chrSep) && all(grepl(chrSep, xfmSep, fixed = TRUE))
        },

        .chkVar = function() {
            crrMde <- self$options$mdeW2L
            ((crrMde ==  "Sep" && private$.chkSep()) ||
             (crrMde ==  "NSS" && length(self$options$xfmNSS) > 1 && nzchar(self$options$idxNSS) && nzchar(self$options$tgtNSS)) ||
             (crrMde ==  "NSA" && private$.chkNSA()))
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            crrMde <- self$options$mdeW2L
            # this case would only apply for creating syntax; for NSS and NSA no jmvReadWrite-conforming syntax is
            # produced, hence no transformation is required and the parameters can be taken as they are
            if (!getDta && crrMde %in% c("NSS", "NSA")) return(NULL)
            # adjust varID and varLst (checked in .getDta to ensure that all required columns are present) wrt. crrMde
            varID  <- switch(crrMde, Sep = self$options$id_Sep, NSS = self$options$id_NSS, NSA = self$options$id_NSA)
            varLst <- switch(crrMde,
                             Sep = c(self$options$id_Sep, self$options$xfmSep, self$options$excSep),
                             NSS = c(self$options$id_NSS, self$options$xfmNSS, self$options$excNSS),
                             NSA = c(self$options$id_NSA, unlist(lapply(self$options$xfmNSA, "[[", "vars")), self$options$excNSA))

            # obtain input data (incl. checking that all required variables are present and the ID is not invalid) 
            if (getDta) {
                dtaInp <- private$.getDta(varLst)$dtaInp
                # ensure that the ID variable is unique and that there are no missing values in that column
                if (!is.null(varID)) {
                    wrgID <- (any(duplicated(dtaInp[, varID])) ||
                              (is.character(dtaInp[, varID]) && any(!nzchar(dtaInp[, varID]))) ||
                              any(is.na(dtaInp[, varID])))
                    if (wrgID) {
                        jmvcore::reject(.("The values in '{varID}' can not be empty and they need to be unique."),
                                        varID = varID)
                    }
                }
            }

            if        (crrMde ==  "Sep") {
                list(dtaInp = if (getDta) dtaInp, varID = varID, varTme = self$options$pfxSep,
                     varLst = self$options$xfmSep, varExc = self$options$excSep, varSep = self$options$chrSep,
                     excLvl = private$.lvl2Nm())
            } else if (crrMde ==  "NSS") {
                rnmRes <- private$.rnmDta(dtaInp)
                list(dtaInp = rnmRes$dtaFrm, varID = varID, varLst = rnmRes$tgtLst, varExc = self$options$excNSS,
                     varSep = rnmRes$varSep, excLvl = 1)
            } else if (crrMde ==  "NSA") {
                rnmRes <- private$.rnmDta(dtaInp)
                list(dtaInp = rnmRes$dtaFrm, varID = varID, varLst = rnmRes$tgtLst, varExc = self$options$excNSA,
                     varTme = vapply(self$options$idxNSA, "[[", character(1), "var"), varSep = rnmRes$varSep, excLvl = 1)
            }
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,

        .detSep = function(varLst = c()) {
            for (varSep in c("_", ".", "-", "!", "#", "%")) {
                if (!any(vapply(varLst, function(n) grepl(varSep, n, fixed = TRUE), logical(1)))) {
                    return(varSep)
                }
            }
            jmvcore::reject(.("The Long Variables in 'Variables To Be Transformed' are invalid, remove _, . and - from the names."))
        },

        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .lvl2Nm = function() {
            lvlSep <- trimws(self$options$lvlSep)
            if (!nzchar(lvlSep)) return(integer(0))
            lvlNum <- suppressWarnings(as.integer(trimws(strsplit(lvlSep, ",")[[1]])))
            if (anyNA(lvlNum))
                jmvcore::reject(.("'Exclude Level' must be a comma-separated list of whole numbers."))
            lvlNum
        },

        # create data frame with index variable / conditions, target variables and frequency
        .prpRpM = function(dtaFrm = NULL) {
            crrMde <- self$options$mdeW2L
            if        (crrMde ==  "Sep") {
                varID  <- ifelse(is.null(self$options$id_Sep), "ID", self$options$id_Sep)
                colOrg <- self$options$xfmSep
                colRes <- names(dtaFrm)
                selCnd <- startsWith(colRes, self$options$pfxSep)
                colTgt <- colRes[!selCnd & !(colRes %in% c(varID, self$options$excSep))]
                tblFrq <- as.data.frame(table(dtaFrm[, sort(which(selCnd), decreasing = TRUE)]))[, sort(seq(sum(selCnd) + 1), decreasing = TRUE)]
                varFrq <- stats::setNames(as.data.frame(matrix(rep("", length(colOrg)), ncol = length(colTgt))), colTgt)
                if (!nzchar(self$options$lvlSep)) {
                    varFrq[, 1] <- sort(colOrg)
                } else {
                    for (i in seq_along(colTgt)) varFrq[, i] <- sort(colOrg[startsWith(colOrg, colTgt[i])])
                }
                cbind(tblFrq[-1], varFrq, tblFrq[1])
            } else if (crrMde ==  "NSS") {
                tblFrq <- as.data.frame(table(dtaFrm[, self$options$idxNSS]))
                cbind(stats::setNames(tblFrq[1], self$options$idxNSS),
                      as.data.frame(self$options$xfmNSS, nm = self$options$tgtNSS), tblFrq[2])
            } else if (crrMde ==  "NSA") {
                varTgt <- stats::setNames(as.data.frame(lapply(self$options$xfmNSA, "[[", "vars")),
                                          vapply(self$options$xfmNSA, "[[", character(1), "label"))
                tblFrq <- as.data.frame(table(dtaFrm[, vapply(self$options$idxNSA, "[[", character(1), "var"), drop = FALSE]))
                colFrq <- ncol(tblFrq)
                cbind(tblFrq[-colFrq], varTgt, tblFrq[colFrq])
            }
        },

        .rnmDta = function(dtaFrm) {
            crrMde <- self$options$mdeW2L
            if        (crrMde ==  "NSS") {
                varSep <- private$.detSep(self$options$tgtNSS)
                tgtLst <- paste0(self$options$tgtNSS, private$.spfNum(length(self$options$xfmNSS), varSep))
                selClm <- (names(dtaFrm) %in% self$options$xfmNSS)
                names(dtaFrm)[selClm] <- tgtLst
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[, names(dtaFrm) %in% c(self$options$id_NSS, self$options$excNSS, tgtLst)],
                     tgtLst = tgtLst, varSep = varSep)
            } else if (crrMde ==  "NSA") {
                if (any(vapply(self$options$xfmNSA, function(l) is.null(l[["label"]]) || !nzchar(l[["label"]]), logical(1)))) {
                    jmvcore::reject(.("No target Long Variables in 'Variables To Be Transformed' can be empty."))
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
                list(dtaFrm = dtaFrm[, names(dtaFrm) %in% c(self$options$id_NSA, self$options$excNSA, tgtLst)],
                     tgtLst = tgtLst, varSep = varSep)
            }
        },

        .runXfm = commonFunc$private_methods$.runXfm,

        .spfNum = function(crrNum = NA, crrSep = "_") {
             sprintf(paste0(crrSep, "%0", as.character(ceiling(log10(crrNum + 1e-6))), "d"), seq(crrNum))
        }
    ),

    public = list(

        asSource = function() {
            crrMde <- self$options$mdeW2L
            if (private$.chkVar()) {
                if (crrMde ==  "Sep") {
                    fmtSrc(private$.crrCmd, private$.crrArg(FALSE))
                } else {
                    crrSrc <- "\n    data = data"
                    nmeOpt <- names(private$.options$options)
                    nmeOpt <- grepl(paste0(crrMde, "$|^mdeW2L$"), nmeOpt)
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
