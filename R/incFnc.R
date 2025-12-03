commonFunc <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "commonFunc",
    private = list(

        .init = function() {
            # Update logging flags based on current options
            set_logflags(self$options$jxfLog)
            jinfo("jTransform: init phase started")

            if (private$.chkVar()) {
                # create the current data set
                private$.crrDta <- do.call(eval(parse(text = private$.crrCmd)), c(private$.crrArg(), list(fleOut = NULL)))
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, colFst = private$.colFst(), nonLtd = private$.nonLtd)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
            jinfo("jTransform: init phase ended")
        },

        .run = function() {
            # update logging flags during the run phase
            set_logflags(self$options$jxfLog)
            jinfo("jTransform: run phase started")

            # assemble or reset data set / create information
            private$.dtaInf()
            if (private$.chkVar() && private$.chkDtF()) {
                # if “Create” was pressed, open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {                   
#                   do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                    prmCrt <- self$results$get('btnCrt')$params
                    jmvReadWrite::write_omv(dtaFrm = private$.crrDta, fleOut = prmCrt$fullPath,
                                            frcWrt = TRUE, vldExt = FALSE)
                    self$results$get('btnCrt')$setResult(list(path = prmCrt$path, ext = 'omv',
                                                              title = sprintf('jTransform - %s', private$.sfxTtl)))
                # if not, create a preview of the data (fllPvw in utils.R)
                } else {
                    fllPvw(crrTbl     = self$results$pvwDta, dtaFrm = private$.crrDta, nteRnC = private$.nteRnC())
                    if (".rpmDta" %in% names(private)) {
                        fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nteRnC = private$.nteRnC())
                    }
                    if (".mrkDff" %in% names(private)) {
                        private$.mrkDff(crrTbl = self$results$pvwDta, dtaNew = private$.crrDta, dtaOld = self$data)
                    }
                }
            }
            jinfo("jTransform: run phase ended")
        },

        # covers the most common case (data frame has at least one row)
        .chkDtF = function() {
            (!is.null(self$data) && dim(self$data)[1] >= 1)
        },

        # covers the most common case (colFst is not used)
        .colFst = function() {
            c()
        },

        .crtMsg = function() {
            if (!hasName(self$options, "btnCrt") || self$options$btnCrt) return(NULL)

            sprintf("%s <strong>%s</strong> %s", .("Pressing the"), .("\"Create\"-button opens the modified data set"),
                    .("in a new jamovi window."))
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
        },

        .nteRnC = function() {
            c(paste(.("There are %d more columns in the data set not shown here. A complete list of"),
                    .("variables can be found in \"Variables in the Output Data Set\" above this table.")),
                    .("There are %d more rows in the data set not shown here."))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg()[-1])
        }

    )
)
