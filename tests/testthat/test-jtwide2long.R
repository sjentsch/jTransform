testthat::test_that("jtwide2long works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtWide2Long.omv")

    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)], excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1")
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 2400 rows): ID,\n",
                                                 "cond1, cond2, cond3, rspCrr, rspTme, sex\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "cond1", "cond2", "cond3", "rspCrr", "rspTme", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3:50, 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "rspTme"])), 0.4505785, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 75)
    expect_equal(chkRes$pvwLvl$asDF[, 1], rep(c("cong", "incong", "neutral"), each = 8))
    expect_equal(chkRes$pvwLvl$asDF[, 2], rep(rep(c("BLUE", "GREEN", "RED", "YELLOW"), each = 2), 3))
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(c("1", "2"), 12))
    expect_equal(chkRes$pvwLvl$asDF[, 4], sort(names(dtaInp)[seq(3, 50, 2)]))
    expect_equal(chkRes$pvwLvl$asDF[, 5], sort(names(dtaInp)[seq(4, 50, 2)]))
    expect_equal(chkRes$pvwLvl$asDF[, 6], rep(100, 24))

    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", xfmNSS = names(dtaInp)[seq(4, 50, 2)], excNSS = "sex")
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (4 variables in 2400 rows): ID,\n",
                                                 "index, var, sex\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "index", "var", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, seq(4, 50, 2), 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "var"])), 0.4983644, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 45)
    expect_equal(chkRes$pvwLvl$asDF[, 1], as.character(seq(24)))
    expect_equal(chkRes$pvwLvl$asDF[, 2], names(dtaInp)[seq(4, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(100, 24))

    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                xfmNSA = list(list(label = "rspCrr", vars = names(dtaInp)[seq(3, 50, 2)]), list(label = "rspTme", vars = names(dtaInp)[seq(4, 50, 2)])),
                idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)))
    expect_equal(class(chkRes), c("jtWide2LongResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 2400 rows): ID, cong,\n",
                                                 "colour, rep, rspCrr, rspTme, sex\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "cong", "colour", "rep", "rspCrr", "rspTme", "sex"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 2390 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, seq(3, 50, 2), seq(4, 50, 2), 2)]))
    expect_equal(mean(as.numeric(chkRes$pvwDta$asDF[-10, "rspTme"])), 0.4503092, tolerance = 1e-6)
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 71)
    expect_equal(chkRes$pvwLvl$asDF[, 1], rep(c("1", "2", "3"), 8))
    expect_equal(chkRes$pvwLvl$asDF[, 2], rep(rep(c("1", "2", "3", "4"), each = 3), 2))
    expect_equal(chkRes$pvwLvl$asDF[, 3], rep(c("1", "2"), each = 12))
    expect_equal(chkRes$pvwLvl$asDF[, 4], names(dtaInp)[seq(3, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 5], names(dtaInp)[seq(4, 50, 2)])
    expect_equal(chkRes$pvwLvl$asDF[, 6], rep(100, 24))

    # check instructions when chkVar fails (xfmSep is empty, mdeW2L = "Sep")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1")
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), )
    expect_equal(chkRes$addInf$asString(), )

    # check instructions when chkVar fails (xfmNSS is empty, mdeW2L = "NSS")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", excNSS = "sex")
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta", "pvwLvl", "addInf"))
    expect_true(chkRes$dtaInf$visible)

    # check instructions when chkVar fails (xfmNSA is empty, mdeW2L = "NSA")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                                      idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)))
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta", "pvwLvl", "addInf"))
    expect_equal(chkRes$genInf$asString(), )
    expect_equal(chkRes$addInf$asString(), )

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtWide2Long(mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)], excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1"),
      regexp = paste("Argument 'id_Sep' contains 'ID' which is not present in the dataset"))
})
