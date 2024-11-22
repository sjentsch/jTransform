#' @importFrom jmvcore .
jtSortClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(
        .crrDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                # resize / prepare the output table (prpPvw in utils.R)
                if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = dtaFrm, colFst = self$options$varSrt)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether there are at least two variables in varOrd and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::sort_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    private$.crrDta <- do.call(jmvReadWrite::sort_omv, private$.crrArg())
                    crtInf(crrInf = self$results$genInf, infMsg = private$.crtMsg())
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, infMsg = hlpSrt)
            }
        },

        .chkVar = function() {
            (length(self$options$varSrt) >= 1)
        },

        .crrArg = function() {
            varSrt <- vapply(self$options$ordSrt, function(x) {
                                                       paste0(gsub("descend", "-", gsub("ascend", "", x[["order"]])), x[["var"]])
                                                  }, character(1))
            list(dtaInp = self$data, fleOut = NULL, varSrt = varSrt)
        },

        .crtMsg = function() {
            crtMsg <- sprintf("%s <strong>%s</strong> %s", .("Pressing the"),
                              .("\"Create\"-button opens the modified data set"), .(" in a new jamovi window."))
            if (!is.null(private$.crrDta)) {
                c(sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", ")),
                  crtMsg)
            } else {
                crtMsg
            }
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::sort_omv", private$.crrArg()[-seq(2)])
        }

    )
)
