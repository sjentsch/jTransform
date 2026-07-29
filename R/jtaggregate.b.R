#' @importFrom jmvcore .
jtAggregateClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtAggregateClass",
    inherit = jtAggregateBase,
    private = list(
        .crrCmd = "jmvReadWrite::aggregate_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .sfxTtl = "agg",

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = function() {
            (all(dim(self$data) >= 2))
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
            c(if (getDta) private$.getDta(c(self$options$varAgg, self$options$grpAgg)),
              list(varAgg = self$options$varAgg, grpAgg = self$options$grpAgg,
                   clcN   = self$options$clcN,   clcMss = self$options$clcMss, clcMn  = self$options$clcMn,
                   clcMdn = self$options$clcMdn, clcMde = self$options$clcMde, clcSum = self$options$clcSum,
                   clcSD  = self$options$clcSD,  clcVar = self$options$clcVar, clcRng = self$options$clcRng,
                   clcMin = self$options$clcMin, clcMax = self$options$clcMax, clcIQR = self$options$clcIQR,
                   drpNA  = self$options$drpNA))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC
    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
    
)
