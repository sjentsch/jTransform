#' @importFrom jmvcore .
jtDistancesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtDistancesClass",
    inherit = jtDistancesBase,
    private = list(
        .dstDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.dstDta <- do.call(jmvReadWrite::distances_omv, private$.crrArg())
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.dstDta, nonLtd = (private$.name == "descDistances"))
            } else {
                # reset the output table (rstPvw in utils.R) and show getting started
                # as general information (crtInf in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpDst)
            }
        },

        .run = function() {
            # check whether there are at least two variables in varOrd and that the data set has at least one row
            if (private$.chkVar()) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(jmvReadWrite::transpose_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    if (private$.name == "descDistances") {
                        crtInf(crrInf = self$results$genInf,                           hlpMsg = " ")
                    } else {
                        crtInf(crrInf = self$results$genInf, dtaFrm = private$.dstDta, hlpMsg = hlpCrt)
                    }
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.dstDta)
                }
            }
        },

        .chkVar = function() {
            (length(self$options$varDst) > 1)
        },
        
        .cncNme = function() {
            nmeDst <- self$options$nmeDst
            paste(c(nmeDst, rep(c(self$options$p__Dst, self$options$np_Dst), jmvReadWrite:::binDst(nmeDst)),
                            rep(self$options$pwrDst, grepl("^minkowski$|^power$", nmeDst)),
                            rep(self$options$rt_Dst, grepl("^power$", nmeDst))), collapse = "_")
        },

        .crrArg = function() {
            if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
            list(dtaInp = as.data.frame(lapply(dtaFrm, jmvcore::toNumeric)), fleOut = NULL, varDst = self$options$varDst,
                 clmDst = (self$options$clmDst == "columns"), stdDst = self$options$stdDst, nmeDst = private$.cncNme())
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::distances_omv", private$.crrArg()[-seq(2)])
        }

    )
)
