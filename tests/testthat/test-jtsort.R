testthat::test_that("jtsort works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jtSort(data = dtaInp, varSrt = c("age", "gender"), varAll = names(dtaInp), ordSrt = list(list(var = "age", order = "ascend"), list(var = "gender", order = "ascend")))
    expect_equal(class(chkRes), c("jtSortResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): age,\n",
                                                 "gender, ID, A1, A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4,\n",
                                                 "E5, N1, N2, N3, N4, N5, O1, O2, O3, O4, O5\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "gender", "ID", "A1", "A2", "A3", "A4", "A5", "C1", "C2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(28, 27, 1:26)]))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, c(-1, -2, -10)], as.numeric, numeric(9)))), c(65181.333, 3.111, 3.777, 3.444, 4.111, 3.556, 3.778), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 79)

    chkRes <- jtSort(data = dtaInp, varSrt = c("age", "gender"), varAll = names(dtaInp),
                     ordSrt = list(list(var = "age", order = "descend"), list(var = "gender", order = "ascend")))
    expect_equal(class(chkRes), c("jtSortResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): age,\n",
                                                 "gender, ID, A1, A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4,\n",
                                                 "E5, N1, N2, N3, N4, N5, O1, O2, O3, O4, O5\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "gender", "ID", "A1", "A2", "A3", "A4", "A5", "C1", "C2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(28, 27, 1:26)]))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, c(-1, -2, -10)], as.numeric, numeric(9)))), c(64764.333, 1.222, 5.222, 4.889, 4.889, 5.111, 5), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 79)

    # check instructions when chkVar fails (varSrt is empty)
    chkRes <- jtSort(data = dtaInp, varAll = names(dtaInp), ordSrt = list(list(var = "age", order = "descend"), list(var = "gender", order = "ascend")))
    expect_equal(names(chkRes), c("genInf", "pvwDta"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign one or more variables to the variable box \"Variable(s)\n",
                                                 "to be Sorted After\". The order in which the variables appear in the\n",
                                                 "variable box determines after which variable is sorted first (one\n",
                                                 "could, e.g., first sort after gender and afterwards after age).\n",
                                                 "Variables are sorted in \"Ascending\" order (as default), but you can\n",
                                                 "change the order if desired.\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtSort(varSrt = c("age", "gender"), varAll = names(dtaInp), ordSrt = list(list(var = "age", order = "descend"), list(var = "gender", order = "ascend"))),
      regexp = paste("Argument 'varSrt' contains 'age', 'gender' which are not present in the dataset"))
})
