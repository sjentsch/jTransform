testthat::test_that("jtcombinecols works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtCombineCols.omv")
    varPrs <- list(list(i1 = "E1_1", i2 = "E1_2"), list(i1 = "E2_1", i2 = "E2_2"),
                   list(i1 = "U1_1", i2 = "U1_2"), list(i1 = "U2_1", i2 = "U2_2"))

    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs[1:2])
    expect_equal(class(chkRes), c("jtCombineColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (6 variables in 200 rows): E1_1,\n",
                                                 "E2_1, U1_1, U1_2, U2_1, U2_2\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                     \n",
                                                 "──────────────────────────────────────────────── \n",
                                                 "  E1_1    E2_1    U1_1    U1_2    U2_1    U2_2   \n",
                                                 "──────────────────────────────────────────────── \n",
                                                 "  54        54              54              54   \n",
                                                 "  18        18      18              18           \n",
                                                 "  72        72      73      72      72           \n",
                                                 "  34        34      34              34           \n",
                                                 "  74        74      74              74      75   \n",
                                                 "  89        89              89              89   \n",
                                                 "  82        82              82              82   \n",
                                                 "  35        35              35              35   \n",
                                                 "  40        40      40              40           \n",
                                                 "   2 ᵃ     ...     ...     ...     ...     ...   \n",
                                                 "──────────────────────────────────────────────── \n",
                                                 "  ᵃ There are 190 more rows in the data set\n",
                                                 "  not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "E2_1", "U1_1", "U1_2", "U2_1", "U2_2"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 190 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 48)

    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs)
    expect_equal(class(chkRes), c("jtCombineColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n At least some values in the variables of the pair(s) U1_1 - U1_2, U2_1\n",
                                                 "- U2_2 are not equal.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview \n",
                                                 "──────────── \n",
                                                 "      \n",
                                                 "──────────── \n",
                                                 "      \n",
                                                 "──────────── \n\n"))
    expect_equal(names(chkRes$pvwDta$columns), "fstCol")
    expect_equal(chkRes$pvwDta$names, "1")
    expect_equal(chkRes$pvwDta$rowKeys, list(1))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 1)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 12)

    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs, mdeCmb = "none")
    expect_equal(class(chkRes), c("jtCombineColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n At least some values in the variables of the pair(s) U1_1 - U1_2, U2_1\n",
                                                 "- U2_2 are not equal.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview \n",
                                                 "──────────── \n",
                                                 "      \n",
                                                 "──────────── \n",
                                                 "      \n",
                                                 "──────────── \n\n"))
    expect_equal(names(chkRes$pvwDta$columns), "fstCol")
    expect_equal(chkRes$pvwDta$names, "1")
    expect_equal(chkRes$pvwDta$rowKeys, list(1))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 1)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 12)

    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs, mdeCmb = "first")
    expect_equal(class(chkRes), c("jtCombineColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (4 variables in 200 rows): E1_1,\n",
                                                 "E2_1, U1_1, U2_1\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                     \n",
                                                 "──────────────────────────────── \n",
                                                 "  E1_1    E2_1    U1_1    U2_1   \n",
                                                 "──────────────────────────────── \n",
                                                 "  54        54      54      54   \n",
                                                 "  18        18      18      18   \n",
                                                 "  72        72      73      72   \n",
                                                 "  34        34      34      34   \n",
                                                 "  74        74      74      74   \n",
                                                 "  89        89      89      89   \n",
                                                 "  82        82      82      82   \n",
                                                 "  35        35      35      35   \n",
                                                 "  40        40      40      40   \n",
                                                 "   2 ᵃ     ...     ...     ...   \n",
                                                 "──────────────────────────────── \n",
                                                 "  ᵃ There are 190 more rows\n",
                                                 "  in the data set not shown\n",
                                                 "  here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "E2_1", "U1_1", "U2_1"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 190 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 32)

    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs, mdeCmb = "second")
    expect_equal(class(chkRes), c("jtCombineColsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (4 variables in 200 rows): E1_1,\n",
                                                 "E2_1, U1_1, U2_1\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                     \n",
                                                 "──────────────────────────────── \n",
                                                 "  E1_1    E2_1    U1_1    U2_1   \n",
                                                 "──────────────────────────────── \n",
                                                 "  54        54      54      54   \n",
                                                 "  18        18      18      18   \n",
                                                 "  72        72      72      72   \n",
                                                 "  34        34      34      34   \n",
                                                 "  74        74      74      75   \n",
                                                 "  89        89      89      89   \n",
                                                 "  82        82      82      82   \n",
                                                 "  35        35      35      35   \n",
                                                 "  40        40      40      40   \n",
                                                 "   2 ᵃ     ...     ...     ...   \n",
                                                 "──────────────────────────────── \n",
                                                 "  ᵃ There are 190 more rows\n",
                                                 "  in the data set not shown\n",
                                                 "  here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "E2_1", "U1_1", "U2_1"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 190 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 32)

    # check instructions when chkVar fails (varPrs is empty)
    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = list(list(i1 = "E1_1", i2 = NULL)))
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # ensure that help is shown
    chkRes <- jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs[1:2], shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtCombineCols(data = dtaInp, varAll = names(dtaInp), varPrs = varPrs, mdeCmb = "first")$parent$asSource(),
      paste0("jmvReadWrite::combine_cols_omv(\n    dtaInp = data,\n    varPrs = list(\n        list(\n            \"E1_1\",\n            \"E1_2\"),",
             "\n        list(\n            \"E2_1\",\n            \"E2_2\"),\n        list(\n            \"U1_1\",\n            \"U1_2\"),",
             "\n        list(\n            \"U2_1\",\n            \"U2_2\")),\n    mdeCmb = \"first\")"))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtCombineCols(varAll = names(dtaInp), varPrs = varPrs, mdeCmb = "first"),
      regexp = paste("Argument 'varAll' contains 'E1_1', 'E1_2', 'E2_1', 'E2_2', 'U1_1', 'U1_2', 'U2_1', 'U2_2' which are not present in the dataset"))

    # additional tests for functions in utils.R
    expect_true(hmeDir() %in% Sys.getenv(c("USERPROFILE", "HOME")))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::combine_cols_omv", crrArg = list(varAll = names(dtaInp), varPrs = varPrs[1:2])),
      paste("jmvReadWrite::combine_cols_omv(\n    dtaInp = data,\n    varAll = c(\n        \"E1_1\",\n        \"E1_2\",\n",
            "       \"E2_1\",\n        \"E2_2\",\n        \"U1_1\",\n        \"U1_2\",\n        \"U2_1\",\n        \"U2_2\"),\n",
            "   varPrs = list(\n        list(\n            i1 = \"E1_1\",\n            i2 = \"E1_2\"),\n",
            "       list(\n            i1 = \"E2_1\",\n            i2 = \"E2_2\")))"))
})
