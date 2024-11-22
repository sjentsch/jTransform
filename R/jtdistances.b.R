#' @importFrom jmvcore .
jtDistancesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtDistancesClass",
    inherit = jtDistancesBase,
    private = list(
        .crrDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                # hide general information and show create information
                self$results$genInf$setVisible(FALSE)
                self$results$dtaInf$setVisible(private$.name != "descDistances")
                # resize / prepare the output table (prpPvw in utils.R) and
                # produce create information
                private$.crrDta <- do.call(jmvReadWrite::distances_omv, private$.crrArg())
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, nonLtd = (private$.name == "descDistances"))
            } else {
                # show getting started as general information and
                # hide data set / create information
                self$results$genInf$setVisible(self$options$shwHlp)
                self$results$dtaInf$setVisible(FALSE)
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether the required variables are present and that the data set has at least two rows
            if (private$.chkVar() && dim(self$data)[1] >= 2) {
                self$results$dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(jmvReadWrite::distances_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (fllPvw in utils.R)
                } else {
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
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
        },

        .crtMsg = function() {
            if (private$.name == "descDistances" || self$options$btnCrt) return(NULL)

            sprintf("%s <strong>%s</strong> %s", .("Pressing the"), .("\"Create\"-button opens the modified data set"),
                    .(" in a new jamovi window."))
        },

        .dtaMsg = function() {
            sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", "))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::distances_omv", private$.crrArg()[-seq(2)])
        }

    )
)
