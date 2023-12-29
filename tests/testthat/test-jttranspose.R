testthat::test_that("jttranspose works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtTranspose.omv")

    chkRes <- jTransform::jtTranspose(data = dtaInp, varNme = "qstItm", varOth = names(dtaInp)[-1])
    expect_equal(class(chkRes), c("jtTransposeResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (17 variables in 75 rows): ID,\n",
                                                 "Qst_01, Qst_02, Qst_03, Qst_04, Qst_05, Qst_06, Qst_07, Qst_08,\n",
                                                 "Qst_09, Qst_10, Qst_11, Qst_12, Qst_13, Qst_14, Qst_15, Qst_16\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "Qst_01", "Qst_02", "Qst_03", "Qst_04", "Qst_05", "Qst_06", "Qst_07", "Qst_08", "Qst_09"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 7 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 65 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 103)

    expect_error(jTransform::jtTranspose(varNme = "qstItm", varOth = names(dtaInp)[-1]),
      regexp = "Argument 'varNme' contains 'qstItm' which is not present in the dataset")
})
