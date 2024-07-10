testthat::test_that("jtreplace works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
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

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "34", rplNew = "")), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                 \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ID         A1     A2     A3     A4     A5     C1     C2     C3     C4      \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  64432        2      3      3      5      5      4      2      4    ... ᵃ   \n",
                                                 "  66278        1      6      5      1      5      3      2      2    ...     \n",
                                                 "  66391        1      6      5      1      3      6      6      5    ...     \n",
                                                 "  62920        2      6      6      6      6      5      5      5    ...     \n",
                                                 "  64835        1      5      6      5      6      1      1      1    ...     \n",
                                                 "  64810        4      2      1      4      1      3      2      1    ...     \n",
                                                 "  62574        2      5      4      4      6      4      3      3    ...     \n",
                                                 "  64620        3      6      3      5      4      2      5      5    ...     \n",
                                                 "  63441        2      5      4      4      5      3      3      5    ...     \n",
                                                 "  65839 ᵇ    ...    ...    ...    ...    ...    ...    ...    ...    ...     \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  Note. Replacements were made, but they are outside the scope (rows /\n",
                                                 "  columns) of this preview.\n",
                                                 "  ᵃ There are 18 more colums in the data set not shown here. A complete\n",
                                                 "  list of variables can be found in \"Variables in the Output Data Set\"\n",
                                                 "  above this table.\n",
                                                 "  ᵇ There are 240 more rows in the data set not shown here.\n\n"))
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
    expect_equal(chkRes$pvwDta$width, 76)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "123", rplNew = "")), whlTrm = TRUE, incCmp = TRUE,
                                    incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                 \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ID         A1     A2     A3     A4     A5     C1     C2     C3     C4      \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  64432        2      3      3      5      5      4      2      4    ... ᵃ   \n",
                                                 "  66278        1      6      5      1      5      3      2      2    ...     \n",
                                                 "  66391        1      6      5      1      3      6      6      5    ...     \n",
                                                 "  62920        2      6      6      6      6      5      5      5    ...     \n",
                                                 "  64835        1      5      6      5      6      1      1      1    ...     \n",
                                                 "  64810        4      2      1      4      1      3      2      1    ...     \n",
                                                 "  62574        2      5      4      4      6      4      3      3    ...     \n",
                                                 "  64620        3      6      3      5      4      2      5      5    ...     \n",
                                                 "  63441        2      5      4      4      5      3      3      5    ...     \n",
                                                 "  65839 ᵇ    ...    ...    ...    ...    ...    ...    ...    ...    ...     \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  Note. There were no replacements made (in the whole dataset).\n",
                                                 "  ᵃ There are 18 more colums in the data set not shown here. A complete\n",
                                                 "  list of variables can be found in \"Variables in the Output Data Set\"\n",
                                                 "  above this table.\n",
                                                 "  ᵇ There are 240 more rows in the data set not shown here.\n\n"))
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
    expect_equal(chkRes$pvwDta$width, 76)

    # check instructions when chkVar fails (rplTrm is empty)
    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(names(chkRes), c("genInf", "pvwDta"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please type the original value and the replacement into the entry\n",
                                                 "fields. If you want to have several pairs of original and replacment\n",
                                                 "values, use separate lines. To replace partial matches, unset the tick\n",
                                                 "box \"Whole Word\" (e.g., for orginal: 24 and replacement: 34, 241 will\n",
                                                 "be changed into 341).\n\n",
                                                 "The \"Include / Exclude\" collapse box permits to specifically select in\n",
                                                 "which column types, for which measurement type, and in which variables\n",
                                                 "to replace values. Ticking the check boxes includes that variable or\n",
                                                 "measurement type. When selecting individual variables using the\n",
                                                 "variable input, set the radio button to either only include the\n",
                                                 "selected variables or to exclude them.\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtReplace(varAll = names(dtaInp), rplTrm = list(list(rplOld = "4", rplNew = "5")), whlTrm = TRUE, incExc = "exclude", varSel = c()),
      regexp = paste("Argument 'varAll' contains 'ID', 'A1', 'A2', 'A3', 'A4', 'A5', 'C1', 'C2', 'C3', 'C4', 'C5', 'E1', 'E2', 'E3', 'E4', 'E5',",
                     "'N1', 'N2', 'N3', 'N4', 'N5', 'O1', 'O2', 'O3', 'O4', 'O5', 'gender', 'age' which are not present in the dataset"))
})
