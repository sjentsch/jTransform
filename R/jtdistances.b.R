#' @importFrom jmvcore .
jtDistancesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtDistancesClass",
    inherit = jtDistancesBase,
    private = list(
        .crrCmd = "jmvReadWrite::distances_omv",
        .nonLtd = FALSE,
        .sfxTtl = "dist",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = function() {
            (all(dim(private$.crrArg(TRUE)$dtaInp) >= 2))
        },

        .chkVar = function() {
            (length(self$options$varDst) > 1)
        },

        .colFst = incFnc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            varDst <- self$options$varDst
            if (getDta) {
                dtaFrm <- private$.getDta(varDst)$dtaInp
                # update .xfmRow to the value after the transformation
                private$.xfmRow <- length(varDst)
            }
            nmeDst <- switch(self$options$dstCtg,
                             ctgCnt = paste(c(self$options$dstCnt,
                                              rep(self$options$pwrCnt, self$options$dstCnt %in% c("minkowski", "power")),
                                              rep(self$options$rt_Cnt, self$options$dstCnt %in% "power")), collapse = "_"),
                             ctgFrq = self$options$dstFrq,
                             ctgBnM = paste(c(self$options$dstBnM, self$options$p__BnM, self$options$np_BnM), collapse = "_"),
                             ctgBnC = paste(c(self$options$dstBnC, self$options$p__BnC, self$options$np_BnC), collapse = "_"),
                             ctgBnP = paste(c(self$options$dstBnP, self$options$p__BnP, self$options$np_BnP), collapse = "_"),
                             ctgBnO = paste(c(self$options$dstBnO, self$options$p__BnO, self$options$np_BnO), collapse = "_"),
                             ctgNot = "none")
            c(if (getDta) list(dtaInp = as.data.frame(lapply(dtaFrm, jmvcore::toNumeric))),
              list(varDst = varDst, clmDst = (self$options$dstCoR == "columns"),
                   stdDst = self$options$dstStd, nmeDst = nmeDst))
        },

        .crtMsg = incFnc$private_methods$.crtMsg,
        .dtaInf = incFnc$private_methods$.dtaInf,
        .dtaMsg = incFnc$private_methods$.dtaMsg,
        .getDta = incFnc$private_methods$.getDta,
        .nteRnC = incFnc$private_methods$.nteRnC,
        .runXfm = incFnc$private_methods$.runXfm

    ),

    public = list(

        asSource = incFnc$public_methods$asSource

    )
)
