#' @importFrom jmvcore .
jtWide2LongClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .crrDta = NULL,
        .rpmDta = NULL,

        .init = function() {
            if (private$.chkVar()) {
                private$.crrDta <- private$.adjRes(dtaFrm = do.call(jmvReadWrite::wide2long_omv, private$.crrArg()))
                private$.rpmDta <- private$.prpRpM(dtaFrm = private$.crrDta)
                # resize / prepare the output table (prpPvw in utils.R) for both data preview and rep. measures overview
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                prpPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta, nonLtd = TRUE)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether the required variables are present and that the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                    do.call(jmvReadWrite::wide2long_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information, and create
                # a preview of the data and of the repeated measurement levels (crtInf and fllPvw in utils.R)
                } else {
                    crtInf(crrInf = self$results$genInf, infMsg = private$.crtMsg())
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta)
                    fllPvw(crrTbl = self$results$pvwLvl, dtaFrm = private$.rpmDta)
                }
            } else {
                # show getting started as general information and further details as additional information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, infMsg = private$.infGen())
                crtInf(crrInf = self$results$addInf, infMsg = private$.infAdd())
            }
        },

        .adjRes = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L == "NSA") {
                selClm <- grepl("^cond[0-9]", names(dtaFrm))
                dtaFrm[, selClm] <- vapply(dtaFrm[, selClm], function(x) as.integer(as.character(x)), integer(dim(dtaFrm)[1]))
                names(dtaFrm)[selClm] <- vapply(self$options$idxNSA, "[[", character(1), "var")
            } else if (self$options$mdeW2L == "NSS") {
                selClm <- grepl("^cond$", names(dtaFrm))
                dtaFrm[, selClm] <- vapply(dtaFrm[, selClm], function(x) as.integer(as.character(x)), integer(1))
                names(dtaFrm)[selClm] <- self$options$idxNSS
            }
            dtaFrm
        },

        .chkNSA = function() {
            xfmNSA <- self$options$xfmNSA
            resNSA <- sapply(xfmNSA, "[[", "vars")
            idxNSA <- self$options$idxNSA
            lvlNSA <- vapply(idxNSA, function(x) as.integer(c(x[["levels"]], NA))[1], integer(1))
            (is.list(xfmNSA) && length(xfmNSA) > 0 && is.matrix(resNSA) && all(dim(resNSA) >= c(1, 1)) &&
             is.list(idxNSA) && length(idxNSA) > 0 && !any(is.na(lvlNSA)) && all(lvlNSA > 0) && prod(lvlNSA) == dim(resNSA)[1])
        },

        .chkSep = function() {
            xfmSep <- self$options$xfmSep
            chrSep <- self$options$chrSep
            length(xfmSep) > 0 && nzchar(self$options$pfxSep) && nzchar(chrSep) && all(grepl(chrSep, xfmSep))
        },

        .chkVar = function() {
            (self$options$mdeW2L == "Sep" && private$.chkSep()) ||
            (self$options$mdeW2L == "NSS" && length(self$options$xfmNSS) > 0 && nzchar(self$options$idxNSS) && nzchar(self$options$tgtNSS)) ||
            (self$options$mdeW2L == "NSA" && private$.chkNSA())
        },

        .crrArg = function() {
            if        (self$options$mdeW2L == "Sep") {
                if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
                list(dtaInp = dtaFrm, fleOut = NULL, varID = self$options$id_Sep, varTme = self$options$pfxSep,
                     varLst = self$options$xfmSep, varExc = self$options$excSep, varSep = self$options$chrSep,
                     excLvl = private$.lvl2Nm())
            } else if (self$options$mdeW2L == "NSS") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,      fleOut = NULL, varID = self$options$id_NSS,
                     varLst = rnmRes$tgtLst, varExc = self$options$excNSS, varSep = "_", excLvl = 1)
            } else if (self$options$mdeW2L == "NSA") {
                rnmRes <- private$.rnmDta()
                list(dtaInp = rnmRes$dtaFrm,      fleOut = NULL, varID =  self$options$id_NSA,
                     varLst = rnmRes$tgtLst, varExc = self$options$excNSA, varSep = "_", excLvl = 1)
            }
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
        },

        .infAdd = function() {
            if        (self$options$mdeW2L == "Sep") {
                c(paste0("<h2>", .("Details"), "</h2>"),
                  paste(.("\"Variables That Identify the Same Unit\" is an ID variable (e.g., a participant code)."),
                        .("This code needs to be unique (i.e., there can't be two participants, or other units,"),
                        .("with the same ID).")),
                  paste(.("\"Variables To Be Transformed\" are the so-called target variables, i.e., variables that"),
                        .("exist as many columns in the input data set and are going to be transformed, creating"),
                        .("different steps of one or more time-varying variable resulting in the output.The number"),
                        .("of variables that are created is determined by how many parts the variable name has (the"),
                        .("parts are split by the character defined as \"Separator\") and how many steps / different"),
                        .("values exist within each part. If we had a variable with 4 parts, each with two steps per"),
                        .("separated part, this would result in four columns (starting with the string defined as"),
                        .("\"Prefix\" and ending with 1, 2, 3, and 4). The number of rows would be increased by the"),
                        .("number of all possible combinations of steps (in the example above 2 * 2 * 2 * 2 = 16,"),
                        .("mulitiplied by the number of rows in the input data set, e.g., 50 rows becoming 50 * 16 ="),
                        .("800 rows).")),
                  paste(.("\"Variables NOT To Be Transformed\" are variables that \"characterize\" a participant (or"),
                        .("another unit), often also called between-subjects variables, e.g., age or sex. However,"),
                        .("they are not unique (and thus no ID variables; there may be several participants with the"),
                        .("same age or sex).")),
                  paste(.("\"Separator\" defines which character(s) should be placed between the target variable and"),
                        .("the steps of the time-varying variable / conditions when assembling the variable names"),
                        .("(e.g., VAR_COND).")),
                  paste(.("Often, an input data set contains different types of measures (e.g., whether a response"),
                        .("was correct and the reaction time) that make up a part of the variable name. Typically,"),
                        .("one wants to keep those different measures as separate columns in the output data set."),
                        .("\"Exclude Level\" permits to exclude one (or more) part (and the steps in it) from being"),
                        .("transformed from wide to long. If the kinds of measurement were the first part of the"),
                        .("variable name, 1 would have to be put into this field. If all levels are to be transformed,"),
                        .("the field needs to be blank.")),
                          " ",
                  paste(.("The principle of the transformation from long to wide can perhaps easiest be understood by"),
                        .("looking at example4jtWide2Long from the Data Library of this module. It contains results"),
                        .("from a Stroop experiment (in wide format) with fifty variables: ID (identifies the"),
                        .("participant), sex (of the participant), and afterwards 48 variables that represent a"),
                        .("combination of the measurement (first part of the variable name, rspCrr - whether the"),
                        .("response was correct - or rspTme - reaction time), the experimental condition / congruency"),
                        .("(second part; either cong[ruent], incong[ruent] or neutral), the colour the word was written"),
                        .("with (third part; BLUE, GREEN, RED or YELLOW) and which repetition of a particular"),
                        .("combination of experimental conditions the variable represents (fourth part, 1 or 2). These"),
                        .("variables have to be assigned to the following fields: ID to \"Variables That Identify the"),
                        .("Same Unit\" (it is an unique identifier of each participant); sex to \"Variables NOT To Be"),
                        .("Transformed\" (sex is a between-subjects variable that doesn't change between experimental"),
                        .("conditions; however, it is not unique and thus not suited as ID variable); and the remaining"),
                        .("variables (i.e., all variables starting with rspCrr_... or rspTme... to \"Variables To Be"),
                        .("Transformed\". Under \"Prefix\" it can be determined how the name for the different"),
                        .("conditions shall start (a number would be added if there is more than one condition).")))
            } else if (self$options$mdeW2L == "NSS") {
                c(paste0("<h2>", .("Details"), "</h2>"),
                  paste(.("\"Variables That Identify the Same Unit\" is an ID variable (e.g., a participant code)."),
                        .("This code needs to be unique (i.e., there can't be two participants, or other units, with"),
                        .("the same ID).")),
                  paste(.("\"Variables To Be Transformed\" are the so-called target variables, i.e., variables that"),
                        .("exist as many columns in the input data set and are going to be transformed, creating"),
                        .("different steps of one or more time-varying variable resulting in the output.")),
                  paste(.("\"Variables NOT To Be Transformed\" are variables that \"characterize\" a participant (or"),
                        .("another unit), often also called between-subjects variables, e.g., age or sex. However,"),
                        .("they are not unique (and thus no ID variables; there may be several participants with the"),
                        .("same age or sex).")),
                  paste(.("\"Index for Repeated Measures Levels\" permits to define the name of the index variable"),
                        .("(the index might be the different items in a questionnaire or the different experimental"),
                        .("conditions in the data set), and \"Name of the Target Variable\" permits to set the name"),
                        .("to the target variable (which is to be created from the list of \"Variables To Be"),
                        .("Transformed\").")))
            } else if (self$options$mdeW2L == "NSA") {
                c(paste0("<h2>", .("Details"), "</h2>"),
                  paste(.("\"Variables That Identify the Same Unit\" is an ID variable (e.g., a participant code)."),
                        .("This code needs to be unique (i.e., there can't be two participants, or other units, with"),
                        .("the same ID).")),
                  paste(.("\"Variables To Be Transformed\" are the so-called target variables, i.e., variables that"),
                        .("exist as many columns in the input data set and are going to be transformed, creating"),
                        .("different steps of one or more time-varying variable resulting in the output. This input"),
                        .("is organized as a combination of the name of the target variable (top line) and the"),
                        .("original variables that are to be transformed into this target variable (as a variable"),
                        .("list underneath the top line). If there are several target variables, those can be added"),
                        .("using the \"Add New Long Variable\" button. Have a look at the explanation of an example"),
                        .("data set, and how to assign variables to it in the example in the last paragraph of the"),
                        .("\"Details\" section.")),
                  paste(.("\"Variables NOT To Be Transformed\" are variables that \"characterize\" a participant"),
                        .("(or another unit), often also called between-subjects variables, e.g., age or sex."),
                        .("However, they are not unique (and thus no ID variables; there may be several participants"),
                        .("with the same age or sex).")),
                  paste(.("\"Index Variables\" are used to index how each step in the different conditions / target"),
                        .("variables matches up with an original (to be transformed) variable. If there are several"),
                        .("conditions, those can be nested. The lowest index variable goes one step up from one"),
                        .("original variable to the next. Have a look at the explanation of an example data set,"),
                        .("and how to create index variables in the next paragraph.")),
                          " ",
                  paste(.("The principle of the transformation from long to wide can perhaps easiest be understood"),
                        .("by looking at example4jtWide2Long from the Data Library of this module. It contains"),
                        .("results from a Stroop experiment (in wide format) with fifty variables: ID (identifies"),
                        .("the participant), sex (of the participant), and afterwards 48 variables that represent"),
                        .("a combination of the measurement (first part of the variable name, rspCrr - whether the"),
                        .("response was correct - or rspTme - reaction time), the experimental condition /"),
                        .("congruency (second part; either cong[ruent], incong[ruent] or neutral), the colour the"),
                        .("word was written with (third part; BLUE, GREEN, RED or YELLOW) and which repetition of"),
                        .("a particular combination of experimental conditions the variable represents (fourth part,"),
                        .("1 or 2). These variables have to be assigned to the following fields: ID to \"Variables"),
                        .("That Identify the Same Unit\" (it is an unique identifier of each participant); sex to"),
                        .("\"Variables NOT To Be Transformed\" (sex is a between-subjects variable that doesn't"),
                        .("change between experimental conditions; however, it is not unique and thus not suited"),
                        .("as ID variable); and the remaining variables (i.e., all variables starting with"),
                        .("rspCrr_... or rspTme... to \"Variables To Be Transformed\". When assigning them, you"),
                        .("first need to \"Create a new long variable\" and to name the first one (default"),
                        .("\"long_y\") \"rspCrr\", and the second one \"rspTme\". Afterwards, you assign all"),
                        .("variables starting with \"rspCrr_\" to the variable list of the first variable, and all"),
                        .("variables starting with \"rspTme_\" to the variable list of the second variable."),
                        .("Afterwards, you need to define the index variables. Check in which order your variables"),
                        .("are arranged: The index variable that changes from one variable name to the next comes"),
                        .("first, and for each of the higher index variables all steps of lower index variables"),
                        .("have to have been through a complete round, before going up a step on this variable."),
                        .("In the example data set, rspCrr_cong_GREEN_1 is followed by rspCrr_incong_GREEN_1 and"),
                        .("rspCrr_neutral_GREEN_1 (don't focus on the rspTme_... variables in between). Therefore,"),
                        .("cond is the first index variable with three steps (cong, incong, neutral), and you"),
                        .("should change \"index1\" to \"cond\" and \"0\" to \"3\". The next level is the colour"),
                        .("(rspCrr_neutral_GREEN_1 is followed by rspCrr_cong_YELLOW_1), and you should set the"),
                        .("name of the index variable to \"colour\" and \"N levels\" to \"4\" (as there are four"),
                        .("colours: GREEN, YELLOW, RED, and BLUE). The highest level is repetition"),
                        .("(rspCrr_neutral_BLUE_1 is followed by rspCrr_cong_GREEN_2), and you should thus set"),
                        .("the name of the index variable to \"rep\" and the \"N levels\" to two (1 and 2)."),
                        .("Once you did all this, and the number of variables in the variable lists for the"),
                        .("long variables under \"Variables To Be Transformed\" - 24 - matches up with the product"),
                        .("of \"N levels\" - 3 (cond) * 4 (colour) * 2 = 24 - a \"Data Preview\" and an overview"),
                        .("over the \"Repeated-Measures Levels\" is shown, and you can open the transformed data"),
                        .("set (after checking those tables) in a new jamovi window by pressing the"),
                        .("\"Create\"-button.")))
            }
        },

        .infGen = function() {
            genW2L <- paste(.("When transforming a data set from wide to long format, you first need determine how"),
                            .("the name of the variables that you would like to transform is build up: It can either"),
                            .("contain the different (e.g., experimental) conditions in the name and those conditions"),
                            .("are separated by a specific character (e.g., \"cond1_condA_conda\" with \"_\" being"),
                            .("that character) or the names either don't contain all conditions or no separating"),
                            .("character. Depending on the structure of the variable names, choose one of the tabs"),
                            .("(\"Non-sep. (simple)\" permit only one condition / index variable, \"Non-sep."),
                            .("(advanced)\" several conditions and target variables. More detailed instructions are"),
                            .("given in the next paragraph and you can check using the two output tables (the first"),
                            .("containing a data preview, the second an overview over how the repeated measurement"),
                            .("levels - i.e., the original variables - were converted)."))
            if        (self$options$mdeW2L == "Sep") {
                c(genW2L,
                  paste(.("Please assign the variables that identify participant (or another measurement unit; e.g.,"),
                        .("a number or an ID) to \"Variables that Identify the Same Unit\", and those that are"),
                        .("unique to an unit but not an identifier (e.g., gender, age group) to \"Variables NOT To"),
                        .("Be Transformed\". If no variable that identifies a participant is given, such variable"),
                        .("will be created (and named \"ID\"). Then assign all variables to be converted from wide"),
                        .("to long (columns to rows) to \"Variables To Be Transformed\". The different conditions"),
                        .("in those variable names are separated by a character that has to be assigned to"),
                        .("\"Separator\" (e.g., in cond1_condA_conda, the separator is \"_\") and the prefix for"),
                        .("the conditions can be given under \"Prefix\" (in the previous example, there would be"),
                        .("three different conditions, and hence the columns in the resulting data set would be"),
                        .("[Prefix]1, [Prefix]2 and [Prefix]3, with [Prefix] replaced by whatever was given as"),
                        .("string in \"Prefix\"). Finally, if one level contains a variable name that should be"),
                        .("kept, this level can be excluded via \"Exclude Level\" (e.g., for var1_cond1_condA_conda,"),
                        .("put \"1\" there, or use \"4\" for cond1_condA_conda_var1. For an example about a typical"),
                        .("wide-to-long-transformation, see the last paragraph in \"Details\" underneath the preview"),
                        .("table.")))
            } else if (self$options$mdeW2L == "NSS") {
                c(genW2L,
                  paste(.("Please assign the variables (columns) of the original data set that you would like to"),
                        .("convert from long (rows) to wide (columns) and assign them to \"Variables To Be"),
                        .("Transformed\". If there exists (one or more) variables that identify a participant (or"),
                        .("another measurement unit; e.g., a number or an ID) it can be assigned to \"Variables that"),
                        .("Identify the same Unit\", otherwise such variable will be created (and named \"ID\")."),
                        .("Variables that are not to be converted (e.g., personal characteristics such as gender, or"),
                        .("age group) should be assigned to \"Variables NOT To Be Transformed\". The text field"),
                        .("\"Name of the Target Variable\" defines the name of the variable (in long format) that is"),
                        .("created; the text field \"Index for Repeated Measures Levels\" defines an index that"),
                        .("contains the number of the original columns / variables after the transformation. That is,"),
                        .("for each of the variables assigned to \"Variables To Be Transformed\" (originally columns)"),
                        .("the index indicates which column the value in the target variable is coming from.")))
            } else if (self$options$mdeW2L == "NSA") {
                c(genW2L,
                  paste(.("Please create a new long variable for each of your target variables, and assign the"),
                        .("respective selection of original variables (columns) from the original data set that you"),
                        .("want to convert into different long format variables under \"Variables To Be"),
                        .("Transformed\": For each long format variable that you want to create, there is is a text"),
                        .("field for the name of that variable and a field where you can drag or assign the original"),
                        .("variables that \"belong\" (are to be converted to) this variable. Please note that all"),
                        .("(original) variable lists assigned to the target variables need to contain the same number"),
                        .("of variables. Afterwards, you need to define one or more index variables for these"),
                        .("variable lists under \"Index Variables (Can Be Nested)\". If you multiply the number of"),
                        .("levels (\"N levels\") the result needs to be equal to the length of the variable lists"),
                        .("under \"Variables To Be transformed\". The first index variable is successive (1, 2, 3,"),
                        .("...), higher index variables go to the next step once all levels on the lower index"),
                        .("variables went through a complete sequence (e.g., 1-1, 2-1, 3-1, 1-2, 2-2, 3-2, ...). You"),
                        .("can use the second output table to check whether the levels of the index variables were"),
                        .("assigned correctly. If there exists (one or more) variables that identify a participant"),
                        .("(or another measurement unit; e.g., a number or an ID) it can be assigned to \"Variables"),
                        .("that identify the same unit\", otherwise such variable will be created (and named \"ID\")."),
                        .("Variables that are not to be converted (e.g., personal characteristics such as gender, or"),
                        .("age group) should be assigned to \"Variables NOT To Be Transformed\".")))
            }
        },

        .lvl2Nm = function() {
            eval(parse(text = paste0("as.integer(c(", self$options$lvlSep, "))")))
        },

        # create data frame with index variable / conditions, target variables and frequency
        .prpRpM = function(dtaFrm = NULL) {
            if        (self$options$mdeW2L == "Sep") {
                varID  <- ifelse(is.null(self$options$id_Sep), "ID", self$options$id_Sep)
                colOrg <- self$options$xfmSep
                colRes <- names(dtaFrm)
                selCnd <- grepl(paste0("^", self$options$pfxSep), colRes)
                selOth <- grepl(paste(paste0("^", c(varID, self$options$excSep), "$"), collapse = "|"), colRes)
                colTgt <- colRes[!(selCnd | selOth)]
                tblFrq <- as.data.frame(table(dtaFrm[, sort(which(selCnd), decreasing = TRUE)]))[, sort(seq(sum(selCnd) + 1), decreasing = TRUE)]
                varFrq <- setNames(as.data.frame(matrix(rep("", length(colOrg)), ncol = length(colTgt))), colTgt)
                if (!nzchar(self$options$lvlSep)) {
                    varFrq[, 1] <- sort(colOrg)
                } else {
                    for (i in seq_along(colTgt)) varFrq[, i] <- sort(colOrg[grepl(colTgt[i], colOrg)])
                }
                cbind(tblFrq[-1], varFrq, tblFrq[1])
            } else if (self$options$mdeW2L == "NSS") {
                tblFrq <- as.data.frame(table(dtaFrm[, self$options$idxNSS]))
                cbind(setNames(tblFrq[1], self$options$idxNSS), as.data.frame(self$options$xfmNSS, nm = self$options$tgtNSS), tblFrq[2])
            } else if (self$options$mdeW2L == "NSA") {
                varTgt <- setNames(as.data.frame(lapply(self$options$xfmNSA, "[[", "vars")), vapply(self$options$xfmNSA, "[[", character(1), "label"))
                tblFrq <- as.data.frame(table(dtaFrm[, vapply(self$options$idxNSA, "[[", character(1), "var")]))
                colFrq <- dim(tblFrq)[2]
                cbind(tblFrq[-colFrq], varTgt, tblFrq[colFrq])
            }
        },

        .rnmDta = function() {
            if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
            if        (self$options$mdeW2L == "NSS") {
                tgtLst <- paste0(self$options$tgtNSS, private$.spfNum(length(self$options$xfmNSS)))
                selClm <- (names(dtaFrm) %in% self$options$xfmNSS)
                names(dtaFrm)[selClm] <- tgtLst
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSS, self$options$excNSS, tgtLst))], tgtLst = tgtLst)
            } else if (self$options$mdeW2L == "NSA") {
                tgtLst <- as.list(vapply(self$options$xfmNSA, "[[", character(1), "label"))
                idxNSA <- self$options$idxNSA
                for (i in seq_along(tgtLst)) {
                    for (j in seq_along(idxNSA)) {
                        tgtLst[[i]] <- paste0(tgtLst[[i]], rep(private$.spfNum(idxNSA[[j]][["levels"]]), each = length(tgtLst[[i]])))
                    }
                    selClm <- (names(dtaFrm) %in% self$options$xfmNSA[[i]][["vars"]])
                    names(dtaFrm)[selClm] <- tgtLst[[i]]
                }
                # convert tgtLst (list) into a (character) vector
                tgtLst <- unlist(tgtLst)
                # remove columns that are not required from dtaFrm and return it together with tgtLst
                list(dtaFrm = dtaFrm[(names(dtaFrm) %in% c(self$options$id_NSA, self$options$excNSA, tgtLst))], tgtLst = tgtLst)
            }
        },

        .spfNum = function(crrNum = NA) {
             sprintf(paste0("_%0", as.character(ceiling(log10(crrNum + 1e-6))), "d"), seq(crrNum))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) {
                if (self$options$mdeW2L == "Sep") {
                    fmtSrc("jmvReadWrite::wide2long_omv", private$.crrArg()[-seq(2)])
                } else {
                    crrSrc <- "\n    data = data"
                    nmeOpt <- names(private$.options$options)
                    nmeOpt <- grepl(paste0(self$options$mdeW2L, "$|^mdeW2L$"), nmeOpt)
                    for (crrOpt in private$.options$options[nmeOpt]) {
                        srcOpt <- private$.sourcifyOption(crrOpt)
                        if (!base::identical(srcOpt, "")) {
                            crrSrc <- paste0(crrSrc, ",\n    ", srcOpt)
                        }
                    }
                    paste0(private$.package, "::", private$.name, "(", crrSrc, ")")
                }
            }
        }

    )
)
