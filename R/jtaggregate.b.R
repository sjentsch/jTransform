#' @importFrom jmvcore .
jtAggregateClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtAggregateClass",
    inherit = jtAggregateBase,
    private = list(
        .crrCmd = "jmvReadWrite::aggregate_omv",
        .nonLtd = FALSE,
        .sfxTtl = "agg",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = function() {
            (all(dim(private$.crrArg(TRUE)$dtaInp) >= 2))
        },

        .chkVar = function() {
            (length(self$options$varAgg) >= 1 && length(self$options$grpAgg) &&
             any(c(self$options$clcN,   self$options$clcMss, self$options$clcMn,
                   self$options$clcMdn, self$options$clcMde, self$options$clcSum,
                   self$options$clcSD,  self$options$clcVar, self$options$clcRng,
                   self$options$clcMin, self$options$clcMax, self$options$clcIQR)))
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            clcNme <- paste0("clc", jmvReadWrite:::aggStr)
            clcLst <- setNames(lapply(clcNme, function(n) self$options[[n]]), clcNme)
            varLst <- list(varAgg = self$options$varAgg, grpAgg = self$options$grpAgg)
            if (getDta) {
                dtaInp <- private$.getDta(unlist(varLst))$dtaInp
                sfxLst <- paste0("_", jmvReadWrite:::aggStr[unlist(clcLst)])
                # update .xfmCol and .xfmRow to the value after the transformation
                private$.xfmRow <- nrow(unique(dtaInp[, varLst$grpAgg, drop = FALSE]))
                private$.xfmCol <- c(varLst$grpAgg,
                                     vapply(varLst$varAgg,
                                            function(v) paste0(v, "_", jmvReadWrite:::aggStr[unlist(clcLst)]),
                                            character(sum(unlist(clcLst))), USE.NAMES = FALSE))
                c(list(dtaInp = dtaInp), varLst, clcLst, list(drpNA  = self$options$drpNA))
            } else {
                c(varLst, clcLst, list(drpNA  = self$options$drpNA))
            }
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,
        .runXfm = commonFunc$private_methods$.runXfm

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
    
)
