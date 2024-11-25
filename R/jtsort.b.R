#' @importFrom jmvcore .
jtSortClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(
        .crrCmd = "jmvReadWrite::sort_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        .init = function() {
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
            (length(self$options$varSrt) >= 1 && dim(self$readDataset())[1] >= 1)
        },

        .colFst = function() {
            c()
        },

        .crrArg = function() {
            varSrt <- vapply(self$options$ordSrt, function(x) {
                                                       paste0(gsub("descend", "-", gsub("ascend", "", x[["order"]])), x[["var"]])
                                                  }, character(1))
            list(dtaInp = self$readDataset(), varSrt = varSrt)
        },

        .crtMsg = function() {
            if (self$options$btnCrt) return(NULL)

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
