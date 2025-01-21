testthat::test_that("jtmergecols works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "outer")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age, A1_2, A2_2, A3_2, A4_2, A5_2,\n",
                                                 "C1_2, C2_2, C3_2, C4_2, C5_2, E1_2, E2_2, E3_2, E4_2, E5_2, N1_2,\n",
                                                 "N2_2, N3_2, N4_2, N5_2, O1_2, O2_2, O3_2, O4_2, O5_2, Grp, SES\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A1_2", "A2_2", "A3_2", "A4_2", "A5_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, A1, A2, A3, A4, A1_2, A2_2, A3_2, A4_2, A5_2 are shown",
                                                      "first in this preview. In the created data set, the variable order is",
                                                      "as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more columns in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "inner")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 225 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age, A1_2, A2_2, A3_2, A4_2, A5_2,\n",
                                                 "C1_2, C2_2, C3_2, C4_2, C5_2, E1_2, E2_2, E3_2, E4_2, E5_2, N1_2,\n",
                                                 "N2_2, N3_2, N4_2, N5_2, O1_2, O2_2, O3_2, O4_2, O5_2, Grp, SES\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A1_2", "A2_2", "A3_2", "A4_2", "A5_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, A1, A2, A3, A4, A1_2, A2_2, A3_2, A4_2, A5_2 are shown",
                                                      "first in this preview. In the created data set, the variable order is",
                                                      "as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more columns in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 215 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "left")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 250 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age, A1_2, A2_2, A3_2, A4_2, A5_2,\n",
                                                 "C1_2, C2_2, C3_2, C4_2, C5_2, E1_2, E2_2, E3_2, E4_2, E5_2, N1_2,\n",
                                                 "N2_2, N3_2, N4_2, N5_2, O1_2, O2_2, O3_2, O4_2, O5_2, Grp, SES\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A1_2", "A2_2", "A3_2", "A4_2", "A5_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, A1, A2, A3, A4, A1_2, A2_2, A3_2, A4_2, A5_2 are shown",
                                                      "first in this preview. In the created data set, the variable order is",
                                                      "as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more columns in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "right")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 225 rows): ID, A1,\n",
                                                 "A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3,\n",
                                                 "N4, N5, O1, O2, O3, O4, O5, gender, age, A1_2, A2_2, A3_2, A4_2, A5_2,\n",
                                                 "C1_2, C2_2, C3_2, C4_2, C5_2, E1_2, E2_2, E3_2, E4_2, E5_2, N1_2,\n",
                                                 "N2_2, N3_2, N4_2, N5_2, O1_2, O2_2, O3_2, O4_2, O5_2, Grp, SES\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "A1", "A2", "A3", "A4", "A1_2", "A2_2", "A3_2", "A4_2", "A5_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$notes$Note$note, paste("The columns ID, A1, A2, A3, A4, A1_2, A2_2, A3_2, A4_2, A5_2 are shown",
                                                      "first in this preview. In the created data set, the variable order is",
                                                      "as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more columns in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 215 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    # ensure that help is shown
    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv", shwHlp = TRUE)
    expect_equal(vapply(c("genInf", "addInf"), function(n) chkRes[[n]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE))

    # check asSource
    expect_match(jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp),
                                         fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "outer")$parent$asSource(),
      paste0("attr\\(data, \"fleInp\"\\) <- c\\(\n    \".*/example4jtMergeCols_2\\.omv\",\n    \".*/example4jtMergeCols_3\\.omv\"\\)\n",
             "jmvReadWrite::merge_cols_omv\\(\n    dtaInp = data,\n    varBy = \"ID\",\n    typMrg = \"outer\"\\)"))

    # check instructions when chkVar fails (varBy is empty)
    chkRes <- jTransform::jtMergeCols(data = dtaInp, varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "right")
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta", "addInf"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtMergeCols(varBy = "ID", varAll = names(dtaInp), fleInp = "", typMrg = "right"),
      regexp = "Argument 'varBy' contains 'ID' which is not present in the dataset")

    # ensure that an error is thrown if fleInp contains an invalid file format
    expect_error(jTransform::jtMergeCols(data = dtaInp, varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.not", typMrg = "right"),
      regexp = "'\\.\\./example4jtMergeCols_2\\.not' doesn't exists or has an unsupported file type\\.")

})
