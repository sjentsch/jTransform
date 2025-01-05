#' @importFrom jmvcore .
jtDistancesClass <- if (requireNamespace(jmvcore, quietly = TRUE)) R6::R6Class(
    "jtDistancesClass",
    inherit = jtDistancesBase,
    private = list(
        .crrCmd = "jmvReadWrite::distances_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = function() {
            (all(dim(self$data) >=  2))
        },

        .chkVar = function() {
            (length(self$options$varDst) > 1)
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            dtaFrm <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            nmeDst <- self$options$nmeDst
            nmeDst <- paste(c(nmeDst,
                              rep(c(self$options$p__Dst, self$options$np_Dst), jmvReadWrite:::binDst(nmeDst)),
                              rep(self$options$pwrDst, grepl("^minkowski$|^power$", nmeDst)),
                              rep(self$options$rt_Dst, grepl("^power$", nmeDst))), collapse = "_")
            list(dtaInp = as.data.frame(lapply(dtaFrm, jmvcore::toNumeric)),
                 varDst = self$options$varDst, clmDst = (self$options$clmDst ==  "columns"),
                 stdDst = self$options$stdDst, nmeDst = nmeDst)
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
