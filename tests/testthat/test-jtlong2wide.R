testthat::test_that("jtlong2wide works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtLong2Wide.omv")

    # no aggregation: varOrd = "times"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (50 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_GREEN_1, rspCrr_cong_GREEN_2, rspCrr_cong_YELLOW_1,\n",
                                                 "rspCrr_cong_YELLOW_2, rspCrr_cong_RED_1, rspCrr_cong_RED_2,\n",
                                                 "rspCrr_cong_BLUE_1, rspCrr_cong_BLUE_2, rspCrr_incong_GREEN_1,\n",
                                                 "rspCrr_incong_GREEN_2, rspCrr_incong_YELLOW_1, rspCrr_incong_YELLOW_2,\n",
                                                 "rspCrr_incong_RED_1, rspCrr_incong_RED_2, rspCrr_incong_BLUE_1,\n",
                                                 "rspCrr_incong_BLUE_2, rspCrr_neutral_GREEN_1, rspCrr_neutral_GREEN_2,\n",
                                                 "rspCrr_neutral_YELLOW_1, rspCrr_neutral_YELLOW_2,\n",
                                                 "rspCrr_neutral_RED_1, rspCrr_neutral_RED_2, rspCrr_neutral_BLUE_1,\n",
                                                 "rspCrr_neutral_BLUE_2, rspTme_cong_GREEN_1, rspTme_cong_GREEN_2,\n",
                                                 "rspTme_cong_YELLOW_1, rspTme_cong_YELLOW_2, rspTme_cong_RED_1,\n",
                                                 "rspTme_cong_RED_2, rspTme_cong_BLUE_1, rspTme_cong_BLUE_2,\n",
                                                 "rspTme_incong_GREEN_1, rspTme_incong_GREEN_2, rspTme_incong_YELLOW_1,\n",
                                                 "rspTme_incong_YELLOW_2, rspTme_incong_RED_1, rspTme_incong_RED_2,\n",
                                                 "rspTme_incong_BLUE_1, rspTme_incong_BLUE_2, rspTme_neutral_GREEN_1,\n",
                                                 "rspTme_neutral_GREEN_2, rspTme_neutral_YELLOW_1,\n",
                                                 "rspTme_neutral_YELLOW_2, rspTme_neutral_RED_1, rspTme_neutral_RED_2,\n",
                                                 "rspTme_neutral_BLUE_1, rspTme_neutral_BLUE_2\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_GREEN_1", "rspCrr_cong_GREEN_2", "rspCrr_cong_YELLOW_1", "rspCrr_cong_YELLOW_2",
                                                 "rspTme_cong_GREEN_1", "rspTme_cong_GREEN_2", "rspTme_cong_YELLOW_1", "rspTme_cong_YELLOW_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_GREEN_1, rspCrr_cong_GREEN_2, rspCrr_cong_YELLOW_1, rspCrr_cong_YELLOW_2,",
                                                      "rspTme_cong_GREEN_1, rspTme_cong_GREEN_2, rspTme_cong_YELLOW_1, rspTme_cong_YELLOW_2 are shown first in this",
                                                      "preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\"",
                                                      "above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 40 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 211)

    # no aggregation: varOrd = "vars"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varOrd = "vars", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (50 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_GREEN_1, rspTme_cong_GREEN_1, rspCrr_incong_GREEN_1,\n",
                                                 "rspTme_incong_GREEN_1, rspCrr_neutral_GREEN_1, rspTme_neutral_GREEN_1,\n",
                                                 "rspCrr_cong_YELLOW_1, rspTme_cong_YELLOW_1, rspCrr_incong_YELLOW_1,\n",
                                                 "rspTme_incong_YELLOW_1, rspCrr_neutral_YELLOW_1,\n",
                                                 "rspTme_neutral_YELLOW_1, rspCrr_cong_RED_1, rspTme_cong_RED_1,\n",
                                                 "rspCrr_incong_RED_1, rspTme_incong_RED_1, rspCrr_neutral_RED_1,\n",
                                                 "rspTme_neutral_RED_1, rspCrr_cong_BLUE_1, rspTme_cong_BLUE_1,\n",
                                                 "rspCrr_incong_BLUE_1, rspTme_incong_BLUE_1, rspCrr_neutral_BLUE_1,\n",
                                                 "rspTme_neutral_BLUE_1, rspCrr_cong_GREEN_2, rspTme_cong_GREEN_2,\n",
                                                 "rspCrr_incong_GREEN_2, rspTme_incong_GREEN_2, rspCrr_neutral_GREEN_2,\n",
                                                 "rspTme_neutral_GREEN_2, rspCrr_cong_YELLOW_2, rspTme_cong_YELLOW_2,\n",
                                                 "rspCrr_incong_YELLOW_2, rspTme_incong_YELLOW_2,\n",
                                                 "rspCrr_neutral_YELLOW_2, rspTme_neutral_YELLOW_2, rspCrr_cong_RED_2,\n",
                                                 "rspTme_cong_RED_2, rspCrr_incong_RED_2, rspTme_incong_RED_2,\n",
                                                 "rspCrr_neutral_RED_2, rspTme_neutral_RED_2, rspCrr_cong_BLUE_2,\n",
                                                 "rspTme_cong_BLUE_2, rspCrr_incong_BLUE_2, rspTme_incong_BLUE_2,\n",
                                                 "rspCrr_neutral_BLUE_2, rspTme_neutral_BLUE_2\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_GREEN_1", "rspCrr_incong_GREEN_1", "rspCrr_neutral_GREEN_1", "rspCrr_cong_YELLOW_1",
                                                 "rspTme_cong_GREEN_1", "rspTme_incong_GREEN_1", "rspTme_neutral_GREEN_1", "rspTme_cong_YELLOW_1"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_GREEN_1, rspCrr_incong_GREEN_1, rspCrr_neutral_GREEN_1, rspCrr_cong_YELLOW_1,",
                                                      "rspTme_cong_GREEN_1, rspTme_incong_GREEN_1, rspTme_neutral_GREEN_1, rspTme_cong_YELLOW_1 are shown first in",
                                                      "this preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\"",
                                                      "above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 40 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 219)

    # aggregation over factor "rep": varAgg = "mean"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (26 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED,\n",
                                                 "rspCrr_cong_YELLOW, rspCrr_incong_BLUE, rspCrr_incong_GREEN,\n",
                                                 "rspCrr_incong_RED, rspCrr_incong_YELLOW, rspCrr_neutral_BLUE,\n",
                                                 "rspCrr_neutral_GREEN, rspCrr_neutral_RED, rspCrr_neutral_YELLOW,\n",
                                                 "rspTme_cong_BLUE, rspTme_cong_GREEN, rspTme_cong_RED,\n",
                                                 "rspTme_cong_YELLOW, rspTme_incong_BLUE, rspTme_incong_GREEN,\n",
                                                 "rspTme_incong_RED, rspTme_incong_YELLOW, rspTme_neutral_BLUE,\n",
                                                 "rspTme_neutral_GREEN, rspTme_neutral_RED, rspTme_neutral_YELLOW\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_BLUE", "rspCrr_cong_GREEN", "rspCrr_cong_RED", "rspCrr_cong_YELLOW",
                                                 "rspTme_cong_BLUE", "rspTme_cong_GREEN", "rspTme_cong_RED", "rspTme_cong_YELLOW"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, c(-1, -2, -10)], as.numeric, numeric(9)))), c(1, 0.9444444, 1, 1, 0.4517520, 0.4518615, 0.4572181), tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED, rspCrr_cong_YELLOW, rspTme_cong_BLUE,",
                                                      "rspTme_cong_GREEN, rspTme_cong_RED, rspTme_cong_YELLOW are shown first in this preview. In the created data set,",
                                                      "the variable order is as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:4, 6:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 185)

    # aggregation over factor "rep": varAgg = "first"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "first", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (26 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED,\n",
                                                 "rspCrr_cong_YELLOW, rspCrr_incong_BLUE, rspCrr_incong_GREEN,\n",
                                                 "rspCrr_incong_RED, rspCrr_incong_YELLOW, rspCrr_neutral_BLUE,\n",
                                                 "rspCrr_neutral_GREEN, rspCrr_neutral_RED, rspCrr_neutral_YELLOW,\n",
                                                 "rspTme_cong_BLUE, rspTme_cong_GREEN, rspTme_cong_RED,\n",
                                                 "rspTme_cong_YELLOW, rspTme_incong_BLUE, rspTme_incong_GREEN,\n",
                                                 "rspTme_incong_RED, rspTme_incong_YELLOW, rspTme_neutral_BLUE,\n",
                                                 "rspTme_neutral_GREEN, rspTme_neutral_RED, rspTme_neutral_YELLOW\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_BLUE", "rspCrr_cong_GREEN", "rspCrr_cong_RED", "rspCrr_cong_YELLOW",
                                                 "rspTme_cong_BLUE", "rspTme_cong_GREEN", "rspTme_cong_RED", "rspTme_cong_YELLOW"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, c(-1, -2, -10)], as.numeric, numeric(9)))), c(1, 0.8888889, 1, 1, 0.4562013, 0.4789798, 0.4522533), tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED, rspCrr_cong_YELLOW, rspTme_cong_BLUE,",
                                                      "rspTme_cong_GREEN, rspTme_cong_RED, rspTme_cong_YELLOW are shown first in this preview. In the created data set,",
                                                      "the variable order is as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:4, 6:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 185)

    # check instructions when chkVar fails (varID is empty)
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varTme = c("cond", "colour"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "first", varSep = "_")
    expect_equal(names(chkRes), c("genInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign the variables that identify participant (or another\n",
                                                 "measurement unit; e.g., a number or an ID) to \"Variables that identify\n",
                                                 "the same unit\", and those that are unique to an unit but not an\n",
                                                 "identifier (e.g., gender, age group) to \"Variables NOT to be\n",
                                                 "Transformed\". \"Variables That Differentiate Within a Unit\" typically\n",
                                                 "contain different (e.g., experimental) conditions, and \"Variables To\n",
                                                 "Be Transformed\" are the actual measurements (e.g., responses, reaction\n",
                                                 "times, etc.).\n\n For an example about a typical long-to-wide-transformation, see the\n",
                                                 "last paragraph in \"Details\" underneath the output tables.\n"))
    expect_equal(chkRes$addInf$asString(), paste("\n Details\n\n \"Variables that identify the same unit\" is an ID variable (e.g., a\n",
                                                 "participant code). This code needs to be unique (i.e., there can't be\n",
                                                 "two participants, or other units, with the same ID).\n\n",
                                                 "\"Variables that differentiate within an unit\" are the so-called\n",
                                                 "time-varying variables, i.e., variables that are originally are\n",
                                                 "arranged in rows and whose steps shall be transformed into columns (a\n",
                                                 "new column created for the each combination of a target variable,\n",
                                                 "e.g., reaction time [RT], with each experimental condition [cond…]\n",
                                                 "leading RT_cond1, RT_cond2, …). There can be several time-varying\n",
                                                 "variables (e.g., several facors in an experiment, each represented by\n",
                                                 "a different column in the input data set).\n\n",
                                                 "\"Variables to be transformed\" are the so-called target variables,\n",
                                                 "i.e., variables that exist as one column in the input data set and are\n",
                                                 "going to be transformed / combined with each step of a time-varying\n",
                                                 "variable resulting in as many columns as there are steps (e.g., would\n",
                                                 "reaction time be one column in the input data set, resulting in as\n",
                                                 "many columns as there are different combinations of time-varying\n",
                                                 "variables in the output data set; if there are 3 steps on the first\n",
                                                 "time-varying variable and 4 steps on the second, this would result in\n",
                                                 "12 columns in the output data set).\n\n",
                                                 "\"Variables NOT to be transformed\" are variables that \"characterize\" a\n",
                                                 "participant (or another unit), often also called between-subjects\n",
                                                 "variables, e.g., age or sex. However, they are not unique (and thus no\n",
                                                 "ID variables; there may be several participant with the same age or\n",
                                                 "sex).\n\n",
                                                 "\"Variable organization in the output\" determines how the variables are\n",
                                                 "to be arranged in the output data set. This is only relevant if there\n",
                                                 "is more than one target variable (e.g., if you want to transform both\n",
                                                 "correct responses and reaction times). \"Variables are adjacent\" keeps\n",
                                                 "the target variables in adjacent columns (for the example above,\n",
                                                 "correct responses and reaction time would be adjacent, and on the\n",
                                                 "higher levels, the steps of the time-varying variable / conditions\n",
                                                 "would vary). \"Time steps are adjacent\" would produce a block where all\n",
                                                 "steps of the time-varying variable / conditions for the first target\n",
                                                 "variable are adjacent, followed by a block of all conditions of the\n",
                                                 "second variable and so on.\n\n",
                                                 "\"How to accumulate?\" determines how different rows may be accumulated.\n",
                                                 "The setting does not apply if there is only one occurence / row for\n",
                                                 "each possible combination of conditions. Let's assume that you ran an\n",
                                                 "experiment with two factors: the experimental condition and which\n",
                                                 "repetition of that condition an entry represents. For evaluation, you\n",
                                                 "want to drop the repetition (you thus only assign the variable\n",
                                                 "representing the condition to \"Variables that differentiate within an\n",
                                                 "unit\". The setting determines if the mean of these occurrences shall\n",
                                                 "be calculated (\"Calculate mean\") or if all occurences after the first\n",
                                                 "should be dropped (\"First occurence\").\n\n",
                                                 "\"Separator\" defines which character(s) should be placed between the\n",
                                                 "target variable and the steps of the time-varying variable /\n",
                                                 "conditions when assembling the variable names (e.g., VAR_COND).\n\n",
                                                 "The principle of the transformation from long to wide can perhaps\n",
                                                 "easiest be understood by looking at example4jtLong2Wide from the Data\n",
                                                 "Library of this module. It contains results from a Stroop experiment\n",
                                                 "(in long format) with seven variables: ID (identifies the\n",
                                                 "participant), sex (of the participant), cond (of the Stroop\n",
                                                 "experiment; congruent, neutral, and incongruent), colour (in which the\n",
                                                 "target word was written), rep (which repetition of a particular\n",
                                                 "cond-colour-combination this entry is), rspCrr (whether the\n",
                                                 "participant named the correct colour), and rspTme (the reaction time\n",
                                                 "for that response). These variables have to be assigned to the\n",
                                                 "following fields: ID to \"Variables that identify the same unit\" (it is\n",
                                                 "an unique identifier of each participant); cond, colour and rep to\n",
                                                 "\"Variables that differentiate within an unit\" (representing all\n",
                                                 "possible combinations of experimental conditions); rspCrr and rspTme\n",
                                                 "to \"Variables to be transformed\" (these measurements are repeated for\n",
                                                 "each combination of conditions, and in a result we want a column each\n",
                                                 "for all possible combinations of those variables and the experimental\n",
                                                 "conditions, i.e., within-subject variables); and finally sex to\n",
                                                 "\"Variables NOT to be transformed\" (since sex is a between-subjects\n",
                                                 "variable that doesn't change between experimental condition, we thus\n",
                                                 "only need one column in the output; however, it is not unique and thus\n",
                                                 "not suited as ID variable).\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtLong2Wide(varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_"),
      regexp = "Argument 'varID' contains 'ID' which is not present in the dataset")
})
