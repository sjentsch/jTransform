testthat::test_that("jtmergecols works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "outer")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 250 rows): ID, A1,\n",
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
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more colums in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "inner")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 225 rows): ID, A1,\n",
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
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more colums in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 215 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "left")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 250 rows): ID, A1,\n",
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
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more colums in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    chkRes <- jTransform::jtMergeCols(data = dtaInp, varBy = "ID", varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "right")
    expect_equal(class(chkRes), c("jtMergeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (55 variables in 225 rows): ID, A1,\n",
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
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 45 more colums in the data set not shown here. A complete list of",
                                                  "variables can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 215 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 80)

    # check instructions when chkVar fails (varBy is empty)
    chkRes <- jTransform::jtMergeCols(data = dtaInp, varAll = names(dtaInp), fleInp = "../example4jtMergeCols_2.omv; ../example4jtMergeCols_3.omv", typMrg = "right")
    expect_equal(names(chkRes), c("genInf", "pvwDta", "addInf"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign one or more variables that appear in all data sets\n",
                                                 "(e.g., a participant code) to \"Variable(s) to Match the Data Sets by\".\n",
                                                 "Afterwards, either write the name of (one or more) file(s) to be\n",
                                                 "merged under \"Data Set(s) to Add\" (separate mulitiple file names with\n",
                                                 "semicolons), or use \"Browse...\" to select input file(s).\n\n",
                                                 "For a more comprehensive explanation regarding the types of merging\n",
                                                 "operations, see \"Details\" underneath the preview table.\n"))
    expect_equal(chkRes$addInf$asString(), paste("\n Details\n\n There are four different types of merging operations: The option\n",
                                                 "\"Keeps All Cases (Rows)\" keeps all cases (but if some input data sets\n",
                                                 "did not contain that value of the matching variable, there might be\n",
                                                 "missing values for variable from that data set). The second option\n",
                                                 "\"Keeps Only Cases Contained in All Merged Data Sets\" keeps only those\n",
                                                 "cases where a particular value of the matching variable was contained\n",
                                                 "in all datasets. The option \"Keeps All Cases From the Currently Opened\n",
                                                 "Data Set\" keeps all rows / cases from the active data set (whereas\n",
                                                 "cases that are only contained in the data sets defined under \"Data\n",
                                                 "Set(s) to Add\" are dropped). The option \"Keeps All Cases From the Data\n",
                                                 "Sets To Be Added\" keeps all cases from the data sets defined under\n",
                                                 "\"Data Set(s) to Add\" (whereas cases that are only contained in the\n",
                                                 "active data set are dropped).\n\n",
                                                 "If there are variables with the same name in several of the input data\n",
                                                 "sets, it will checked whether they contain the same content. If they\n",
                                                 "are the same, only the first of these identical variables is kept (and\n",
                                                 "the others are removed). If they are different, the name of the first\n",
                                                 "of these variables is kept, and a suffix with their position (_2, _3,\n",
                                                 "etc.) will be added to the name of all other variables.\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtMergeCols(varBy = "ID", varAll = names(dtaInp), fleInp = "", typMrg = "right"),
      regexp = "Argument 'varBy' contains 'ID' which is not present in the dataset")
})
