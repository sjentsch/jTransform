testthat::test_that("jtarrangecols works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtMergeCols_1.omv")

    chkRes <- jTransform::jtArrangeCols(data = dtaInp, varOrd = names(dtaInp)[c(1, 17:21, 12:16, 22:26, 2:11, 27:28)],
                                        varAll = names(dtaInp), blnAll = FALSE)
    expect_equal(class(chkRes), c("jtArrangeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, N1,\n",
                                                 "N2, N3, N4, N5, E1, E2, E3, E4, E5, O1, O2, O3, O4, O5, A1, A2, A3,\n",
                                                 "A4, A5, C1, C2, C3, C4, C5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                 \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ID         N1     N2     N3     N4     N5     E1     E2     E3     E4      \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  64432        5      4      3      2      4      4      5      4    ... ᵃ   \n",
                                                 "  66278        2      4      4      5      2      5      5      3    ...     \n",
                                                 "  66391        1      5      1      6      5      1      6      4    ...     \n",
                                                 "  62920        1      5      4      6      6      5      5      5    ...     \n",
                                                 "  64835        6      5      6      6      6      6      6      6    ...     \n",
                                                 "  64810        1      1      1      1      1      6      6      2    ...     \n",
                                                 "  62574        1      1      2      4      2      3      2      4    ...     \n",
                                                 "  64620        5      6      6      5      5      2      2      4    ...     \n",
                                                 "  63441        3      2      2      6      5      5      5      3    ...     \n",
                                                 "  65839 ᵇ    ...    ...    ...    ...    ...    ...    ...    ...    ...     \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ᵃ There are 18 more colums in the data set not shown here. A complete\n",
                                                 "  list of variables can be found in \"Variables in the Output Data Set\"\n",
                                                 "  above this table.\n",
                                                 "  ᵇ There are 240 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "N1", "N2", "N3", "N4", "N5", "E1", "E2", "E3", "E4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 76)

    chkRes <- jTransform::jtArrangeCols(data = dtaInp, varOrd = names(dtaInp)[c(1, 17:21, 12:16, 22:26)], varAll = names(dtaInp), blnAll = TRUE)
    expect_equal(class(chkRes), c("jtArrangeColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (28 variables in 250 rows): ID, N1,\n",
                                                 "N2, N3, N4, N5, E1, E2, E3, E4, E5, O1, O2, O3, O4, O5, A1, A2, A3,\n",
                                                 "A4, A5, C1, C2, C3, C4, C5, gender, age\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                 \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ID         N1     N2     N3     N4     N5     E1     E2     E3     E4      \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  64432        5      4      3      2      4      4      5      4    ... ᵃ   \n",
                                                 "  66278        2      4      4      5      2      5      5      3    ...     \n",
                                                 "  66391        1      5      1      6      5      1      6      4    ...     \n",
                                                 "  62920        1      5      4      6      6      5      5      5    ...     \n",
                                                 "  64835        6      5      6      6      6      6      6      6    ...     \n",
                                                 "  64810        1      1      1      1      1      6      6      2    ...     \n",
                                                 "  62574        1      1      2      4      2      3      2      4    ...     \n",
                                                 "  64620        5      6      6      5      5      2      2      4    ...     \n",
                                                 "  63441        3      2      2      6      5      5      5      3    ...     \n",
                                                 "  65839 ᵇ    ...    ...    ...    ...    ...    ...    ...    ...    ...     \n",
                                                 "──────────────────────────────────────────────────────────────────────────── \n",
                                                 "  ᵃ There are 18 more colums in the data set not shown here. A complete\n",
                                                 "  list of variables can be found in \"Variables in the Output Data Set\"\n",
                                                 "  above this table.\n",
                                                 "  ᵇ There are 240 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "N1", "N2", "N3", "N4", "N5", "E1", "E2", "E3", "E4"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 18 more colums in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 240 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 76)

    # check instructions when chkVar fails (varOrd is empty)
    chkRes <- jTransform::jtArrangeCols(data = dtaInp, varAll = names(dtaInp), blnAll = TRUE)
    expect_equal(names(chkRes), c("genInf", "pvwDta"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign the variables in their desired order to \"Desired Order\n",
                                                 "of Variables\". By ticking \"Add Remaining Variables at the End\",\n",
                                                 "variables that are not contained in \"Desired order of variables\") are\n",
                                                 "appended.\n"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtArrangeCols(varOrd = names(dtaInp)[c(1, 17:21, 12:16, 22:26, 2:11, 27:28)], varAll = names(dtaInp), blnAll = FALSE),
      regexp = paste("Argument 'varAll' contains 'ID', 'A1', 'A2', 'A3', 'A4', 'A5', 'C1', 'C2', 'C3', 'C4', 'C5', 'E1', 'E2', 'E3', 'E4', 'E5', 'N1', 'N2',",
                     "'N3', 'N4', 'N5', 'O1', 'O2', 'O3', 'O4', 'O5', 'gender', 'age' which are not present in the dataset"))
})
