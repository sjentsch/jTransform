testthat::test_that("jtreplace works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE,
                                    incOrd = TRUE, incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 3, 0, 1, 2, 1, 1, 1, 2))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
                 c(64477.889, 2.5, 4.889, 4.5, 4.714, 5, 3.75, 3.5, 4.143), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "exclude", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 0, 0, 0, 0, 0, 1, 1, 2))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
                 c(64477.889, 2, 4.889, 4.111, 3.889, 4.556, 3.75, 3.5, 4.143), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 3, 0, 1, 2, 1, 0, 0, 0))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
                 c(64477.889, 2.5, 4.889, 4.5, 4.714, 5, 3.444, 3.222, 3.444), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "4", rplNew = "5")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
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
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "34", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = c(64432, 66278, 66391, 62920, 64835, 64810, 62574, 64620, 63441),
                      A1 = c(2, 1, 1, 2, 1, 4, 2, 3, 2), A2 = c(3, 6, 6, 6, 5, 2, 5, 6, 5), A3 = c(3, 5, 5, 6, 6, 1, 4, 3, 4),
                      A4 = c(5, 1, 1, 6, 5, 4, 4, 5, 4), A5 = c(5, 5, 3, 6, 6, 1, 6, 4, 5), C1 = c(4, 3, 6, 5, 1, 3, 4, 2, 3),
                      C2 = c(2, 2, 6, 5, 1, 2, 3, 5, 3), C3 = c(4, 2, 5, 5, 1, 1, 3, 5, 5)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(chkRes$pvwDta$notes$diff$note,
                 "Replacements were made, but they are outside the scope (rows / columns) of this preview.")
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 0, 0, 0, 0, 0, 0, 0, 0))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
      c(64477.889, 2.111, 4.889, 4.333, 4.222, 4.667, 3.444, 3.222, 3.444), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "123", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "exclude", varSel = c())
    expect_equal(class(chkRes), c("jtReplaceResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = c(64432, 66278, 66391, 62920, 64835, 64810, 62574, 64620, 63441),
                      A1 = c(2, 1, 1, 2, 1, 4, 2, 3, 2), A2 = c(3, 6, 6, 6, 5, 2, 5, 6, 5), A3 = c(3, 5, 5, 6, 6, 1, 4, 3, 4),
                      A4 = c(5, 1, 1, 6, 5, 4, 4, 5, 4), A5 = c(5, 5, 3, 6, 6, 1, 6, 4, 5), C1 = c(4, 3, 6, 5, 1, 3, 4, 2, 3),
                      C2 = c(2, 2, 6, 5, 1, 2, 3, 5, 3), C3 = c(4, 2, 5, 5, 1, 1, 3, 5, 5)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(chkRes$pvwDta$notes$diff$note, "There were no replacements made (in the whole dataset).")
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(unname(colSums(is.na(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9))))), c(0, 0, 0, 0, 0, 0, 0, 0, 0))
    expect_equal(unname(colMeans(vapply(chkRes$pvwDta$asDF[-10, -10], as.numeric, numeric(9)), na.rm = TRUE)),
      c(64477.889, 2.111, 4.889, 4.333, 4.222, 4.667, 3.444, 3.222, 3.444), tolerance = 1e-3)
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # ensure that help is shown
    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "1", rplNew = "")),
                                    whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE,
                                    incNum = TRUE, incExc = "exclude", varSel = c(), shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), rplTrm = list(list(rplOld = "123", rplNew = "")), whlTrm = TRUE,
                                       incCmp = TRUE, incRcd = TRUE, incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE,
                                       incExc = "exclude", varSel = c())$parent$asSource(),
      paste0("jmvReadWrite::replace_omv(\n    dtaInp = data,\n    rplLst = list(\n        c(\"123\", \"\")))"))

    # check instructions when chkVar fails (rplTrm is empty)
    chkRes <- jTransform::jtReplace(data = dtaInp, varAll = names(dtaInp), whlTrm = TRUE, incCmp = TRUE, incRcd = TRUE,
                                    incID = TRUE, incNom = TRUE, incOrd = TRUE, incNum = TRUE, incExc = "include", varSel = c("A1", "A2", "A3", "A4", "A5"))
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtReplace(varAll = names(dtaInp), rplTrm = list(list(rplOld = "4", rplNew = "5")), whlTrm = TRUE, incExc = "exclude", varSel = c()),
      regexp = paste("Argument 'varAll' contains 'ID', 'A1', 'A2', 'A3', 'A4', 'A5', 'C1', 'C2', 'C3', 'C4', 'C5', 'E1', 'E2', 'E3', 'E4', 'E5',",
                     "'N1', 'N2', 'N3', 'N4', 'N5', 'O1', 'O2', 'O3', 'O4', 'O5', 'gender', 'age' which are not present in the dataset"))
})
