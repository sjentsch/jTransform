testthat::test_that("jtwide2long works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtWide2Long.omv")

    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)], excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1")
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 2400 rows): ID,\n",
                                                 "cond1, cond2, cond3, rspCrr, rspTme, sex\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "cond1", "cond2", "cond3", "rspCrr", "rspTme", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:50, 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "rspTme"])), 0.4505785, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 75)
    expect_equal(chkRes$pvwLvl$asDF[, 1], rep(c("cong", "incong", "neutral"), each = 8))
    expect_equal(chkRes$pvwLvl$asDF[, 2], rep(rep(c("BLUE", "GREEN", "RED", "YELLOW"), each = 2), 3))
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(c("1", "2"), 12))
    expect_equal(chkRes$pvwLvl$asDF[, 4], sort(names(dtaInp)[seq(3, 50, 2)]))
    expect_equal(chkRes$pvwLvl$asDF[, 5], sort(names(dtaInp)[seq(4, 50, 2)]))
    expect_equal(chkRes$pvwLvl$asDF[, 6], rep(100, 24))

    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", xfmNSS = names(dtaInp)[seq(4, 50, 2)], excNSS = "sex")
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (4 variables in 2400 rows): ID,\n",
                                                 "index, var, sex\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "index", "var", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, seq(4, 50, 2), 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "var"])), 0.4983644, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 45)
    expect_equal(chkRes$pvwLvl$asDF[, 1], as.character(seq(24)))
    expect_equal(chkRes$pvwLvl$asDF[, 2], names(dtaInp)[seq(4, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(100, 24))
    
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                xfmNSA = list(list(label = "rspCrr", vars = names(dtaInp)[seq(3, 50, 2)]), list(label = "rspTme", vars = names(dtaInp)[seq(4, 50, 2)])),
                idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)))
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 2400 rows): ID, cong,\n",
                                                 "colour, rep, rspCrr, rspTme, sex\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "cong", "colour", "rep", "rspCrr", "rspTme", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, seq(3, 50, 2), seq(4, 50, 2), 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "rspTme"])), 0.4503092, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 71)
    expect_equal(chkRes$pvwLvl$asDF[, 1], rep(c("1", "2", "3"), 8))
    expect_equal(chkRes$pvwLvl$asDF[, 2], rep(rep(c("1", "2", "3", "4"), each = 3), 2))
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(c("1", "2"), each = 12))
    expect_equal(chkRes$pvwLvl$asDF[, 4], names(dtaInp)[seq(3, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 5], names(dtaInp)[seq(4, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 6], rep(100, 24))

    # check instructions when chkVar fails (xfmSep is empty, mdeW2L = "Sep")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1")
    expect_equal(names(chkRes), c("genInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), paste("\n When transforming a data set from wide to long format, you first need\n",
                                                 "determine how the name of the variables that you would like to\n",
                                                 "transform is build up: It can either contain the different (e.g.,\n",
                                                 "experimental) conditions in the name and those conditions are\n",
                                                 "separated by a specific character (e.g., \"cond1_condA_conda\" with \"_\"\n",
                                                 "being that character) or the names either don't contain all conditions\n",
                                                 "or no separating character. Depending on the structure of the variable\n",
                                                 "names, choose one of the tabs (\"Non-sep. (simple)\" permit only one\n",
                                                 "condition / index variable, \"Non-sep. (advanced)\" several conditions\n",
                                                 "and target variables. More detailed instructions are given in the next\n",
                                                 "paragraph and you can check using the two output tables (the first\n",
                                                 "containing a data preview, the second an overview over how the\n",
                                                 "repeated measurement levels - i.e., the original variables - were\n",
                                                 "converted).\n\n",
                                                 "Please assign the variables that identify participant (or another\n",
                                                 "measurement unit; e.g., a number or an ID) to \"Variables that Identify\n",
                                                 "the Same Unit\", and those that are unique to an unit but not an\n",
                                                 "identifier (e.g., gender, age group) to \"Variables NOT To Be\n",
                                                 "Transformed\". If no variable that identifies a participant is given,\n",
                                                 "such variable will be created (and named \"ID\"). Then assign all\n",
                                                 "variables to be converted from wide to long (columns to rows) to\n",
                                                 "\"Variables To Be Transformed\". The different conditions in those\n",
                                                 "variable names are separated by a character that has to be assigned to\n",
                                                 "\"Separator\" (e.g., in cond1_condA_conda, the separator is \"_\") and the\n",
                                                 "prefix for the conditions can be given under \"Prefix\" (in the previous\n",
                                                 "example, there would be three different conditions, and hence the\n",
                                                 "columns in the resulting data set would be [Prefix]1, [Prefix]2 and\n",
                                                 "[Prefix]3, with [Prefix] replaced by whatever was given as string in\n",
                                                 "\"Prefix\"). Finally, if one level contains a variable name that should\n",
                                                 "be kept, this level can be excluded via \"Exclude Level\" (e.g., for\n",
                                                 "var1_cond1_condA_conda, put \"1\" there, or use \"4\" for\n",
                                                 "cond1_condA_conda_var1.\n\n",
                                                 "For an example about a typical wide-to-long-transformation, see the\n",
                                                 "last paragraph in \"Details\" underneath the preview table.\n"))
    expect_equal(chkRes$addInf$asString(), paste("\n Details\n\n",
                                                 "\"Variables That Identify the Same Unit\" is an ID variable (e.g., a\n",
                                                 "participant code). This code needs to be unique (i.e., there can't be\n",
                                                 "two participants, or other units, with the same ID).\n\n",
                                                 "\"Variables To Be Transformed\" are the so-called target variables,\n",
                                                 "i.e., variables that exist as many columns in the input data set and\n",
                                                 "are going to be transformed, creating different steps of one or more\n",
                                                 "time-varying variable resulting in the output.The number of variables\n",
                                                 "that are created is determined by how many parts the variable name has\n",
                                                 "(the parts are split by the character defined as \"Separator\") and how\n",
                                                 "many steps / different values exist within each part. If we had a\n",
                                                 "variable with 4 parts, each with two steps per separated part, this\n",
                                                 "would result in four columns (starting with the string defined as\n",
                                                 "\"Prefix\" and ending with 1, 2, 3, and 4). The number of rows would be\n",
                                                 "increased by the number of all possible combinations of steps (in the\n",
                                                 "example above 2 * 2 * 2 * 2 = 16, mulitiplied by the number of rows in\n",
                                                 "the input data set, e.g., 50 rows becoming 50 * 16 = 800 rows).\n\n",
                                                 "\"Variables NOT To Be Transformed\" are variables that \"characterize\" a\n",
                                                 "participant (or another unit), often also called between-subjects\n",
                                                 "variables, e.g., age or sex. However, they are not unique (and thus no\n",
                                                 "ID variables; there may be several participants with the same age or\n",
                                                 "sex).\n\n",
                                                 "\"Separator\" defines which character(s) should be placed between the\n",
                                                 "target variable and the steps of the time-varying variable /\n",
                                                 "conditions when assembling the variable names (e.g., VAR_COND).\n\n",
                                                 "Often, an input data set contains different types of measures (e.g.,\n",
                                                 "whether a response was correct and the reaction time) that make up a\n",
                                                 "part of the variable name. Typically, one wants to keep those\n",
                                                 "different measures as separate columns in the output data set.\n",
                                                 "\"Exclude Level\" permits to exclude one (or more) part (and the steps\n",
                                                 "in it) from being transformed from wide to long. If the kinds of\n",
                                                 "measurement were the first part of the variable name, 1 would have to\n",
                                                 "be put into this field. If all levels are to be transformed, the field\n",
                                                 "needs to be blank.\n\n\n\n",
                                                 "The principle of the transformation from long to wide can perhaps\n",
                                                 "easiest be understood by looking at example4jtWide2Long from the Data\n",
                                                 "Library of this module. It contains results from a Stroop experiment\n",
                                                 "(in wide format) with fifty variables: ID (identifies the\n",
                                                 "participant), sex (of the participant), and afterwards 48 variables\n",
                                                 "that represent a combination of the measurement (first part of the\n",
                                                 "variable name, rspCrr - whether the response was correct - or rspTme -\n",
                                                 "reaction time), the experimental condition / congruency (second part;\n",
                                                 "either cong[ruent], incong[ruent] or neutral), the colour the word was\n",
                                                 "written with (third part; BLUE, GREEN, RED or YELLOW) and which\n",
                                                 "repetition of a particular combination of experimental conditions the\n",
                                                 "variable represents (fourth part, 1 or 2). These variables have to be\n",
                                                 "assigned to the following fields: ID to \"Variables That Identify the\n",
                                                 "Same Unit\" (it is an unique identifier of each participant); sex to\n",
                                                 "\"Variables NOT To Be Transformed\" (sex is a between-subjects variable\n",
                                                 "that doesn't change between experimental conditions; however, it is\n",
                                                 "not unique and thus not suited as ID variable); and the remaining\n",
                                                 "variables (i.e., all variables starting with rspCrr_... or rspTme...\n",
                                                 "to \"Variables To Be Transformed\". Under \"Prefix\" it can be determined\n",
                                                 "how the name for the different conditions shall start (a number would\n",
                                                 "be added if there is more than one condition).\n"))

    # check instructions when chkVar fails (xfmNSS is empty, mdeW2L = "NSS")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", excNSS = "sex")
    expect_equal(names(chkRes), c("genInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), paste("\n When transforming a data set from wide to long format, you first need\n",
                                                 "determine how the name of the variables that you would like to\n",
                                                 "transform is build up: It can either contain the different (e.g.,\n",
                                                 "experimental) conditions in the name and those conditions are\n",
                                                 "separated by a specific character (e.g., \"cond1_condA_conda\" with \"_\"\n",
                                                 "being that character) or the names either don't contain all conditions\n",
                                                 "or no separating character. Depending on the structure of the variable\n",
                                                 "names, choose one of the tabs (\"Non-sep. (simple)\" permit only one\n",
                                                 "condition / index variable, \"Non-sep. (advanced)\" several conditions\n",
                                                 "and target variables. More detailed instructions are given in the next\n",
                                                 "paragraph and you can check using the two output tables (the first\n",
                                                 "containing a data preview, the second an overview over how the\n",
                                                 "repeated measurement levels - i.e., the original variables - were\n",
                                                 "converted).\n\n",
                                                 "Please assign the variables (columns) of the original data set that\n",
                                                 "you would like to convert from long (rows) to wide (columns) and\n",
                                                 "assign them to \"Variables To Be Transformed\". If there exists (one or\n",
                                                 "more) variables that identify a participant (or another measurement\n",
                                                 "unit; e.g., a number or an ID) it can be assigned to \"Variables that\n",
                                                 "Identify the same Unit\", otherwise such variable will be created (and\n",
                                                 "named \"ID\"). Variables that are not to be converted (e.g., personal\n",
                                                 "characteristics such as gender, or age group) should be assigned to\n",
                                                 "\"Variables NOT To Be Transformed\". The text field \"Name of the Target\n",
                                                 "Variable\" defines the name of the variable (in long format) that is\n",
                                                 "created; the text field \"Index for Repeated Measures Levels\" defines\n",
                                                 "an index that contains the number of the original columns / variables\n",
                                                 "after the transformation. That is, for each of the variables assigned\n",
                                                 "to \"Variables To Be Transformed\" (originally columns) the index\n",
                                                 "indicates which column the value in the target variable is coming\n",
                                                 "from.\n"))
    expect_equal(chkRes$addInf$asString(), paste("\n Details\n\n",
                                                 "\"Variables That Identify the Same Unit\" is an ID variable (e.g., a\n",
                                                 "participant code). This code needs to be unique (i.e., there can't be\n",
                                                 "two participants, or other units, with the same ID).\n\n",
                                                 "\"Variables To Be Transformed\" are the so-called target variables,\n",
                                                 "i.e., variables that exist as many columns in the input data set and\n",
                                                 "are going to be transformed, creating different steps of one or more\n",
                                                 "time-varying variable resulting in the output.\n\n",
                                                 "\"Variables NOT To Be Transformed\" are variables that \"characterize\" a\n",
                                                 "participant (or another unit), often also called between-subjects\n",
                                                 "variables, e.g., age or sex. However, they are not unique (and thus no\n",
                                                 "ID variables; there may be several participants with the same age or\n",
                                                 "sex).\n\n",
                                                 "\"Index for Repeated Measures Levels\" permits to define the name of the\n",
                                                 "index variable (the index might be the different items in a\n",
                                                 "questionnaire or the different experimental conditions in the data\n",
                                                 "set), and \"Name of the Target Variable\" permits to set the name to the\n",
                                                 "target variable (which is to be created from the list of \"Variables To\n",
                                                 "Be Transformed\").\n"))

    # check instructions when chkVar fails (xfmNSA is empty, mdeW2L = "NSA")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                                      idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)))
    expect_equal(names(chkRes), c("genInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), paste("\n When transforming a data set from wide to long format, you first need\n",
                                                 "determine how the name of the variables that you would like to\n",
                                                 "transform is build up: It can either contain the different (e.g.,\n",
                                                 "experimental) conditions in the name and those conditions are\n",
                                                 "separated by a specific character (e.g., \"cond1_condA_conda\" with \"_\"\n",
                                                 "being that character) or the names either don't contain all conditions\n",
                                                 "or no separating character. Depending on the structure of the variable\n",
                                                 "names, choose one of the tabs (\"Non-sep. (simple)\" permit only one\n",
                                                 "condition / index variable, \"Non-sep. (advanced)\" several conditions\n",
                                                 "and target variables. More detailed instructions are given in the next\n",
                                                 "paragraph and you can check using the two output tables (the first\n",
                                                 "containing a data preview, the second an overview over how the\n",
                                                 "repeated measurement levels - i.e., the original variables - were\n",
                                                 "converted).\n\n",
                                                 "Please create a new long variable for each of your target variables,\n",
                                                 "and assign the respective selection of original variables (columns)\n",
                                                 "from the original data set that you want to convert into different\n",
                                                 "long format variables under \"Variables To Be Transformed\" : For each\n",
                                                 "long format variable that you want to create, there is is a text field\n",
                                                 "for the name of that variable and a field where you can drag or assign\n",
                                                 "the original variables that \"belong\" (are to be converted to) this\n",
                                                 "variable. Please note that all (original) variable lists assigned to\n",
                                                 "the target variables need to contain the same number of variables.\n",
                                                 "Afterwards, you need to define one or more index variables for these\n",
                                                 "variable lists under \"Index Variables (Can Be Nested)\". If you\n",
                                                 "multiply the number of levels (\"N levels\") the result needs to be\n",
                                                 "equal to the length of the variable lists under \"Variables To Be\n",
                                                 "transformed\". The first index variable is successive (1, 2, 3, ...),\n",
                                                 "higher index variables go to the next step once all levels on the\n",
                                                 "lower index variables went through a complete sequence (e.g., 1-1,\n",
                                                 "2-1, 3-1, 1-2, 2-2, 3-2, ...). You can use the second output table to\n",
                                                 "check whether the levels of the index variables were assigned\n",
                                                 "correctly. If there exists (one or more) variables that identify a\n",
                                                 "participant (or another measurement unit; e.g., a number or an ID) it\n",
                                                 "can be assigned to \"Variables that identify the same unit\", otherwise\n",
                                                 "such variable will be created (and named \"ID\"). Variables that are not\n",
                                                 "to be converted (e.g., personal characteristics such as gender, or age\n",
                                                 "group) should be assigned to \"Variables NOT To Be Transformed\".\n"))
    expect_equal(chkRes$addInf$asString(), paste("\n Details\n\n",
                                                 "\"Variables That Identify the Same Unit\" is an ID variable (e.g., a\n",
                                                 "participant code). This code needs to be unique (i.e., there can't be\n",
                                                 "two participants, or other units, with the same ID).\n\n",
                                                 "\"Variables To Be Transformed\" are the so-called target variables,\n",
                                                 "i.e., variables that exist as many columns in the input data set and\n",
                                                 "are going to be transformed, creating different steps of one or more\n",
                                                 "time-varying variable resulting in the output. This input is organized\n",
                                                 "as a combination of the name of the target variable (top line) and the\n",
                                                 "original variables that are to be transformed into this target\n",
                                                 "variable (as a variable list underneath the top line). If there are\n",
                                                 "several target variables, those can be added using the \"Add New Long\n",
                                                 "Variable\" button. Have a look at the explanation of an example data\n",
                                                 "set, and how to assign variables to it in the example in the last\n",
                                                 "paragraph of the \"Details\" section.\n\n",
                                                 "\"Variables NOT To Be Transformed\" are variables that \"characterize\" a\n",
                                                 "participant (or another unit), often also called between-subjects\n",
                                                 "variables, e.g., age or sex. However, they are not unique (and thus no\n",
                                                 "ID variables; there may be several participants with the same age or\n",
                                                 "sex).\n\n",
                                                 "\"Index Variables\" are used to index how each step in the different\n",
                                                 "conditions / target variables matches up with an original (to be\n",
                                                 "transformed) variable. If there are several conditions, those can be\n",
                                                 "nested. The lowest index variable goes one step up from one original\n",
                                                 "variable to the next. Have a look at the explanation of an example\n",
                                                 "data set, and how to create index variables in the next paragraph.\n\n\n\n",
                                                 "The principle of the transformation from long to wide can perhaps\n",
                                                 "easiest be understood by looking at example4jtWide2Long from the Data\n",
                                                 "Library of this module. It contains results from a Stroop experiment\n",
                                                 "(in wide format) with fifty variables: ID (identifies the\n",
                                                 "participant), sex (of the participant), and afterwards 48 variables\n",
                                                 "that represent a combination of the measurement (first part of the\n",
                                                 "variable name, rspCrr - whether the response was correct - or rspTme -\n",
                                                 "reaction time), the experimental condition / congruency (second part;\n",
                                                 "either cong[ruent], incong[ruent] or neutral), the colour the word was\n",
                                                 "written with (third part; BLUE, GREEN, RED or YELLOW) and which\n",
                                                 "repetition of a particular combination of experimental conditions the\n",
                                                 "variable represents (fourth part, 1 or 2). These variables have to be\n",
                                                 "assigned to the following fields: ID to \"Variables That Identify the\n",
                                                 "Same Unit\" (it is an unique identifier of each participant); sex to\n",
                                                 "\"Variables NOT To Be Transformed\" (sex is a between-subjects variable\n",
                                                 "that doesn't change between experimental conditions; however, it is\n",
                                                 "not unique and thus not suited as ID variable); and the remaining\n",
                                                 "variables (i.e., all variables starting with rspCrr_... or rspTme...\n",
                                                 "to \"Variables To Be Transformed\". When assigning them, you first need\n",
                                                 "to \"Create a new long variable\" and to name the first one (default\n",
                                                 "\"long_y\") \"rspCrr\", and the second one \"rspTme\". Afterwards, you\n",
                                                 "assign all variables starting with \"rspCrr_\" to the variable list of\n",
                                                 "the first variable, and all variables starting with \"rspTme_\" to the\n",
                                                 "variable list of the second variable. Afterwards, you need to define\n",
                                                 "the index variables. Check in which order your variables are arranged:\n",
                                                 "The index variable that changes from one variable name to the next\n",
                                                 "comes first, and for each of the higher index variables all steps of\n",
                                                 "lower index variables have to have been through a complete round,\n",
                                                 "before going up a step on this variable. In the example data set,\n",
                                                 "rspCrr_cong_GREEN_1 is followed by rspCrr_incong_GREEN_1 and\n",
                                                 "rspCrr_neutral_GREEN_1 (don't focus on the rspTme_... variables in\n",
                                                 "between). Therefore, cond is the first index variable with three steps\n",
                                                 "(cong, incong, neutral), and you should change \"index1\" to \"cond\" and\n",
                                                 "\"0\" to \"3\". The next level is the colour (rspCrr_neutral_GREEN_1 is\n",
                                                 "followed by rspCrr_cong_YELLOW_1), and you should set the name of the\n",
                                                 "index variable to \"colour\" and \"N levels\" to \"4\" (as there are four\n",
                                                 "colours: GREEN, YELLOW, RED, and BLUE). The highest level is\n",
                                                 "repetition (rspCrr_neutral_BLUE_1 is followed by rspCrr_cong_GREEN_2),\n",
                                                 "and you should thus set the name of the index variable to \"rep\" and\n",
                                                 "the \"N levels\" to two (1 and 2). Once you did all this, and the number\n",
                                                 "of variables in the variable lists for the long variables under\n",
                                                 "\"Variables To Be Transformed\" - 24 - matches up with the product of \"N\n",
                                                 "levels\" - 3 (cond) * 4 (colour) * 2 = 24 - a \"Data Preview\" and an\n",
                                                 "overview over the \"Repeated-Measures Levels\" is shown, and you can\n",
                                                 "open the transformed data set (after checking those tables) in a new\n",
                                                 "jamovi window by pressing \"CREATE\".\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtWide2Long(mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)], excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1"),
      regexp = paste("Argument 'id_Sep' contains 'ID' which is not present in the dataset"))
})
