testthat::test_that("jtreplace works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c(), btnCrt = FALSE)
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n A2, A3, A4, A5, C1,",
                                                 "C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 3, 0, 1, 2, 1, 1, 1, 2))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)), c(64477.889, 2.5, 4.889, 4.5, 4.714, 5, 3.75, 3.5, 4.143), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 116)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c("A1", "A2", "A3", "A4", "A5"), btnCrt = FALSE)
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n A2, A3, A4, A5, C1,",
                                                 "C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 0, 0, 0, 0, 0, 1, 1, 2))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)), c(64477.889, 2, 4.889, 4.111, 3.889, 4.556, 3.75, 3.5, 4.143), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 91)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"), btnCrt = FALSE)
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n A2, A3, A4, A5, C1,",
                                                 "C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 3, 0, 1, 2, 1, 0, 0, 0))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)), c(64477.889, 2.5, 4.889, 4.5, 4.714, 5, 3.444, 3.222, 3.444), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 101)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "4", rplNew = "5")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"), btnCrt = FALSE)
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n A2, A3, A4, A5, C1,",
                                                 "C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "\"CREATE\" opens the modified data set in a new jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 0, 0, 0, 0, 0, 0, 0, 0))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
      c(64477.889, 2.111, 4.889, 4.333, 4.222, 4.667, 3.444, 3.222, 3.444), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 101)

    expect_error(jTransform::jtReplace(varAll = names(dtaInp), rplTrm = list(list(rplOld = "4", rplNew = "5")), whlTrm = TRUE, incExc = "exclude", varSel = c(), btnCrt = FALSE),
      regexp = paste("Argument 'varAll' contains 'ID', 'A1', 'A2', 'A3', 'A4', 'A5', 'C1', 'C2', 'C3', 'C4', 'C5', 'E1', 'E2', 'E3', 'E4', 'E5',",
                     "'N1', 'N2', 'N3', 'N4', 'N5', 'O1', 'O2', 'O3', 'O4', 'O5', 'gender', 'age' which are not present in the dataset"))
})
