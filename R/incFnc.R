commonFunc <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "commonFunc",
    private = list(

        .init = function() {
            # Update logging flags based on current options
            set_logflags(self$options$jxfLog)
            jinfo("jTransform: init phase started")

            if (private$.chkVar()) {
                # create the current data set
                private$.crrDta <- tryCatch(do.call(str2Fn(private$.crrCmd), private$.crrArg(TRUE)),
                                            error = function(e) {
                                                jmvcore::reject(.("The transformation could not be completed: {msg}"),
                                                                msg = conditionMessage(e))
                                            })
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
                # if “Create” was pressed (btnCrt ==  TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    btnCrt <- self$options$option("btnCrt")
                    # TO-DO: replace Dataset with the name of the current data set (once this is implemented)
                    crrTtl <- paste("Dataset", private$.sfxTtl, collapse = "_")
                    if (is.null(btnCrt$perform)) {
                        jmvReadWrite:::jmvOpn(dtaFrm = private$.crrDta, dtaTtl = crrTtl)
                    } else {
                        btnCrt$perform(function(action) list(data = private$.crrDta, title = crrTtl))
                    }
                } else {
                    # if not, create a preview of the data (used by all functions except jtSearch; fllPvw in utils.R)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, nteRnC = private$.nteRnC())
                    # ... fill table that shows the repeated measurement factors (used by jtLong2Wide, jtWide2Long)
                    if (utils::hasName(private, ".rpmDta")) {
                        fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nteRnC = private$.nteRnC())
                    }
                    # ... mark occurences in the preview where the values were changed / replaced (used by jtReplace)
                    if (utils::hasName(private, ".mrkDff")) {
                        private$.mrkDff(crrTbl = self$results$pvwDta, dtaNew = private$.crrDta, dtaOld = self$data)
                    }
                }
            }
            jinfo("jTransform: run phase ended")
        },

        # covers the most common case (data frame has at least one row)
        .chkDtF = function() {
            (dim(self$data)[1] >= 1)
        },

        # get the data set and check that all variables in the options in optLst are not empty (contain only NAs)
        .getDta = function(varLst = c()) {
            dtaInp <- if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset()
            for (crrVar in varLst) {
                if (all(is.na(dtaInp[, crrVar])))
                    jmvcore::reject(.("The variable '{crrVar}' contains only missing / invalid values."), crrVar = crrVar)
            }

            # return a list with the target data set as entry, the data set either contains all variables
            # (if varAll is defined) or is restricted to varLst
            list(dtaInp = dtaInp[, if (utils::hasName(self$options, "varAll")) names(dtaInp) else varLst])
        },

        # covers the most common case (colFst is not used)
        .colFst = function() {
            c()
        },

        .crtMsg = function() {
            if (!utils::hasName(self$options, "btnCrt") || self$options$btnCrt) return(NULL)

            jmvcore::format(.("Pressing the <strong>{}-button opens the modified data set</strong> in a new jamovi window."),
                            .("\"Create\""))
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
            jmvcore::format(.("<strong>Variables in the Output Data Set</strong> ({} variables in {} rows): {}"),
                            dim(private$.crrDta)[2], dim(private$.crrDta)[1],
                            paste(names(private$.crrDta), collapse = ", "))
        },

        .nteRnC = function() {
            c(paste(.("There are {} more columns in the data set not shown here."),
                    .("A complete list of variables can be found in \"Variables in the Output Data Set\" above this table.")),
              .("There are {} more rows in the data set not shown here."))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg(FALSE))
        }

    )
)
