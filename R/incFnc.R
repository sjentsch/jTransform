commonFunc <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "commonFunc",
    private = list(

        .init = function() {
            # Update logging flags based on current options
            set_logflags(self$options$jxfLog)
            jinfo(sprintf("[%s]: jTransform: init phase started", private$.name))

            if (private$.chkVar()) {
                # calculate the transformed data (if requested by .xfmFst and if .xfmDta is NULL)
                # .xfmFst marks analyses where there is no (or at least no easy) way to calculate
                # the size of the transformed data set
                if (private$.xfmFst && is.null(private$.xfmDta)) {
                    private$.xfmDta <- private$.runXfm()
                    private$.xfmCol <- names(private$.xfmDta)
                    private$.xfmRow <- nrow(private$.xfmDta)
                } else if (length(private$.xfmCol) == 0 || is.na(private$.xfmRow)) {
                    private$.crrArg(TRUE) # run .crrArg to update .xfmCol and .xfmRow
                }
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, numRow = private$.xfmRow,
                       colAll = private$.xfmCol, colFst = private$.colFst(), nonLtd = private$.nonLtd)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
            jinfo(sprintf("[%s]: jTransform: init phase ended", private$.name))
        },

        .run = function() {
            # update logging flags during the run phase
            set_logflags(self$options$jxfLog)
            jinfo(sprintf("[%s]: jTransform: run phase started", private$.name))

            # assemble or reset data set / create information
            dtaInf <- self$results$dtaInf
            if (private$.chkVar()) {
                # calculate the transformed data (if not already done - .xfmDta were not NULL in such case;
                # .xfmFst request to calculate in .init())
                if (is.null(private$.xfmDta)) {
                    private$.xfmDta <- private$.runXfm()
                }
                # generate information about the (transformed) data set
                dtaInf$setContent(paste(c(private$.dtaMsg(), private$.crtMsg()), collapse = "</p><p>"))
                dtaInf$setVisible(TRUE)
                # if “Create” was pressed (btnCrt ==  TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    btnCrt <- self$options$option("btnCrt")
                    # TO-DO: replace Dataset with the name of the current data set (once this is implemented)
                    crrTtl <- paste("Dataset", private$.sfxTtl, collapse = "_")
                    if (is.null(btnCrt$perform)) {
                        jmvReadWrite:::jmvOpn(dtaFrm = private$.xfmDta, dtaTtl = crrTtl)
                    } else {
                        btnCrt$perform(function(action) list(data = private$.xfmDta, title = crrTtl))
                    }
                } else {
                    # if not, create a preview of the data (used by all functions except jtSearch; fllPvw in utils.R)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.xfmDta, nteRnC = private$.nteRnC())
                    # ... fill table that shows the repeated measurement factors (used by jtLong2Wide, jtWide2Long)
                    if (utils::hasName(private, ".rpmDta")) {
                        fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nteRnC = private$.nteRnC())
                    }
                    # ... mark occurences in the preview where the values were changed / replaced (used by jtReplace)
                    if (utils::hasName(private, ".mrkDff")) {
                        private$.mrkDff(crrTbl = self$results$pvwDta, dtaNew = private$.xfmDta, dtaOld = self$data)
                    }
                }
            } else {
                dtaInf$setVisible(FALSE)
            }
            jinfo(sprintf("[%s]: jTransform: init phase ended", private$.name))
        },

        # covers the most common case (the input data frame has at least one row)
        .chkDtF = function() {
            (nrow(private$.crrArg(TRUE)$dtaInp) >= 1)
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

        .dtaMsg = function() {
            jmvcore::format(.("<strong>Variables in the Output Data Set</strong> ({} variables in {} rows): {}"),
                            ncol(private$.xfmDta), nrow(private$.xfmDta), paste(names(private$.xfmDta), collapse = ", "))
        },

        # get the data set and check that all variables in the options in varLst are not empty (contain only NAs)
        .getDta = function(varLst = c()) {
            dtaInp <- if (!is.null(self$data) && nrow(self$data) > 0) self$data else self$readDataset()
            for (crrVar in varLst) {
                if (all(is.na(dtaInp[, crrVar])))
                    jmvcore::reject(.("The variable '{crrVar}' contains only missing / invalid values."), crrVar = crrVar)
            }

            # return a list with the target data set as entry, the data set either contains all variables
            # (if varAll is defined) or is restricted to varLst
            private$.xfmCol <- if (utils::hasName(self$options, "varAll")) names(dtaInp) else varLst
            if (utils::hasName(private, ".xfmRow")) private$.xfmRow <- nrow(dtaInp)

            list(dtaInp = dtaInp[, private$.xfmCol])
        },

        .nteRnC = function() {
            c(paste(.("There are {} more columns in the data set not shown here."),
                    .("A complete list of variables can be found in \"Variables in the Output Data Set\" above this table.")),
              .("There are {} more rows in the data set not shown here."))
        },
        
        # covers the most common case (number of rows is the same as in the original data set)
        .numRow = function() {
        
        },

        .runXfm = function() {
            if (!private$.chkDtF()) {
                jmvcore::reject(.("It is not possible to carry out this transformation with an empty or otherwise invalid dataset."))
            }
            tryCatch(do.call(str2Fn(private$.crrCmd), private$.crrArg(TRUE)),
                     error = function(e) jmvcore::reject(.("The transformation could not be completed: {msg}"),
                                                         msg = conditionMessage(e)))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg(FALSE))
        }

    )
)
