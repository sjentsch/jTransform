#' @importFrom jmvcore .
jtDistancesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtDistancesClass",
    inherit = jtDistancesBase,
    private = list(
        .crrCmd = "jmvReadWrite::distances_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        .init = function() {
            private$.nonLtd <- (private$.name == "descDistances")
            if (private$.chkVar()) {
                # calculate the current data
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)), c(private$.crrArg(), list(fleOut = NULL)))
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(), nonLtd = private$.nonLtd)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # assemble or reset data set / create information
            private$.dtaInf()
            if (private$.chkVar()) {
                # assemble data set / create information
                self$results$dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                # if not, create a preview of the data (fllPvw in utils.R)
                } else {
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                }
            }
        },

        .chkVar = function() {
            (length(self$options$varDst) > 1 && all(dim(self$readDataset()) >= 2))
        },
        
        .cncNme = function() {
            nmeDst <- self$options$nmeDst
            paste(c(nmeDst, rep(c(self$options$p__Dst, self$options$np_Dst), jmvReadWrite:::binDst(nmeDst)),
                            rep(self$options$pwrDst, grepl("^minkowski$|^power$", nmeDst)),
                            rep(self$options$rt_Dst, grepl("^power$", nmeDst))), collapse = "_")
        },

        .colFst = function() {
            c()
        },

        .crrArg = function() {
            list(dtaInp = as.data.frame(lapply(self$readDataset(), jmvcore::toNumeric)),
                 varDst = self$options$varDst, clmDst = (self$options$clmDst == "columns"),
                 stdDst = self$options$stdDst, nmeDst = private$.cncNme())
        },

        .crtMsg = function() {
            if (private$.name == "descDistances" || self$options$btnCrt) return(NULL)

            sprintf("%s <strong>%s</strong> %s", .("Pressing the"), .("\"Create\"-button opens the modified data set"),
                    .(" in a new jamovi window."))
        },

        .dtaInf = function() {
            if (private$.chkVar()) {
                self$results$dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                self$results$dtaInf$setVisible(TRUE)
            } else {
                self$results$dtaInf$setVisible(FALSE)
            }
        },

        .dtaMsg = function() {
            sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", "))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg()[-1])
        }

    )
)
