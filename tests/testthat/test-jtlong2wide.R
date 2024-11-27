testthat::test_that("jtlong2wide works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtLong2Wide.omv")

    # no aggregation: varOrd = "times"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (50 variables in 100 rows): ID, sex,\n",
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
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_GREEN_1", "rspCrr_cong_GREEN_2", "rspCrr_cong_YELLOW_1", "rspCrr_cong_YELLOW_2",
                                                 "rspTme_cong_GREEN_1", "rspTme_cong_GREEN_2", "rspTme_cong_YELLOW_1", "rspTme_cong_YELLOW_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_GREEN_1, rspCrr_cong_GREEN_2, rspCrr_cong_YELLOW_1, rspCrr_cong_YELLOW_2,",
                                                      "rspTme_cong_GREEN_1, rspTme_cong_GREEN_2, rspTme_cong_YELLOW_1, rspTme_cong_YELLOW_2 are shown first in this",
                                                      "preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\"",
                                                      "above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 40 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 211)

    # no aggregation: varOrd = "vars"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varOrd = "vars", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (50 variables in 100 rows): ID, sex,\n",
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
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "sex", "rspCrr_cong_GREEN_1", "rspCrr_incong_GREEN_1", "rspCrr_neutral_GREEN_1", "rspCrr_cong_YELLOW_1",
                                                 "rspTme_cong_GREEN_1", "rspTme_incong_GREEN_1", "rspTme_neutral_GREEN_1", "rspTme_cong_YELLOW_1"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, sex, rspCrr_cong_GREEN_1, rspCrr_incong_GREEN_1, rspCrr_neutral_GREEN_1, rspCrr_cong_YELLOW_1,",
                                                      "rspTme_cong_GREEN_1, rspTme_incong_GREEN_1, rspTme_neutral_GREEN_1, rspTme_cong_YELLOW_1 are shown first in",
                                                      "this preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\"",
                                                      "above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 40 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:7, 2)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 219)

    # aggregation over factor "rep": varAgg = "mean"
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")
    expect_equal(class(chkRes), c("jtLong2WideResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (26 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED,\n",
                                                 "rspCrr_cong_YELLOW, rspCrr_incong_BLUE, rspCrr_incong_GREEN,\n",
                                                 "rspCrr_incong_RED, rspCrr_incong_YELLOW, rspCrr_neutral_BLUE,\n",
                                                 "rspCrr_neutral_GREEN, rspCrr_neutral_RED, rspCrr_neutral_YELLOW,\n",
                                                 "rspTme_cong_BLUE, rspTme_cong_GREEN, rspTme_cong_RED,\n",
                                                 "rspTme_cong_YELLOW, rspTme_incong_BLUE, rspTme_incong_GREEN,\n",
                                                 "rspTme_incong_RED, rspTme_incong_YELLOW, rspTme_neutral_BLUE,\n",
                                                 "rspTme_neutral_GREEN, rspTme_neutral_RED, rspTme_neutral_YELLOW\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (26 variables in 100 rows): ID, sex,\n",
                                                 "rspCrr_cong_BLUE, rspCrr_cong_GREEN, rspCrr_cong_RED,\n",
                                                 "rspCrr_cong_YELLOW, rspCrr_incong_BLUE, rspCrr_incong_GREEN,\n",
                                                 "rspCrr_incong_RED, rspCrr_incong_YELLOW, rspCrr_neutral_BLUE,\n",
                                                 "rspCrr_neutral_GREEN, rspCrr_neutral_RED, rspCrr_neutral_YELLOW,\n",
                                                 "rspTme_cong_BLUE, rspTme_cong_GREEN, rspTme_cong_RED,\n",
                                                 "rspTme_cong_YELLOW, rspTme_incong_BLUE, rspTme_incong_GREEN,\n",
                                                 "rspTme_incong_RED, rspTme_incong_YELLOW, rspTme_neutral_BLUE,\n",
                                                 "rspTme_neutral_GREEN, rspTme_neutral_RED, rspTme_neutral_YELLOW\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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


    # ensure that help is shown
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour"), varTgt = c("rspCrr"), varExc = "sex", varSep = "_", shwHlp = TRUE)
    expect_equal(vapply(c("genInf", "addInf"), function(n) chkRes[[n]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE))

    # check asSource
    expect_equal(jTransform::jtLong2Wide(data = dtaInp, varID = "ID", varTme = c("cond", "colour"),
                                         varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")$parent$asSource(),
      paste0("jmvReadWrite::long2wide_omv(\n    dtaInp = data,\n    varTme = c(\"cond\", \"colour\"),\n    varTgt = c(\"rspCrr\", \"rspTme\"),",
             "\n    varExc = \"sex\",\n    varOrd = \"times\",\n    varAgg = \"mean\")"))

    # check instructions when chkVar fails (varID is empty)
    chkRes <- jTransform::jtLong2Wide(data = dtaInp, varTme = c("cond", "colour"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "first", varSep = "_")
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(dim(chkRes$pvwLvl$asDF), c(0, 1))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtLong2Wide(varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_"),
      regexp = "Argument 'varID' contains 'ID' which is not present in the dataset")
})
