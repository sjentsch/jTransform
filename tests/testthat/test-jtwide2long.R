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
                xfmNSA = list(list(label = "rspCrr", vars = names(dtaInp)[seq(3, 50, 2)]),
                              list(label = "rspTme", vars = names(dtaInp)[seq(4, 50, 2)])),
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

    # ensure that help is shown
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)],
                                      excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1", shwHlp = TRUE)
    expect_equal(vapply(c("genSep", "addSep", "genNSS", "addNSS", "genNSA", "addNSA"), function(n) chkRes[[n]]$visible, logical(1), USE.NAMES = FALSE),
      c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", xfmNSS = names(dtaInp)[seq(4, 50, 2)], excNSS = "sex", shwHlp = TRUE)
    expect_equal(vapply(c("genSep", "addSep", "genNSS", "addNSS", "genNSA", "addNSA"), function(n) chkRes[[n]]$visible, logical(1), USE.NAMES = FALSE),
      c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                                         xfmNSA = list(list(label = "rspCrr", vars = names(dtaInp)[seq(3, 50, 2)]),
                                                       list(label = "rspTme", vars = names(dtaInp)[seq(4, 50, 2)])),
                                         idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)),
                                         shwHlp = TRUE)
    expect_equal(vapply(c("genSep", "addSep", "genNSS", "addNSS", "genNSA", "addNSA"), function(n) chkRes[[n]]$visible, logical(1), USE.NAMES = FALSE),
      c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))

    # check asSource
    expect_equal(jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)],
                                         excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1")$parent$asSource(),
      paste0("jmvReadWrite::wide2long_omv(\n    dtaInp = data,\n    varID = \"ID\",\n    varLst = c(\n        \"rspCrr_cong_GREEN_1\",\n        \"rspTme_cong_GREEN_1\",",
             "\n        \"rspCrr_incong_GREEN_1\",\n        \"rspTme_incong_GREEN_1\",\n        \"rspCrr_neutral_GREEN_1\",\n        \"rspTme_neutral_GREEN_1\",",
             "\n        \"rspCrr_cong_YELLOW_1\",\n        \"rspTme_cong_YELLOW_1\",\n        \"rspCrr_incong_YELLOW_1\",\n        \"rspTme_incong_YELLOW_1\",",
             "\n        \"rspCrr_neutral_YELLOW_1\",\n        \"rspTme_neutral_YELLOW_1\",\n        \"rspCrr_cong_RED_1\",\n        \"rspTme_cong_RED_1\",",
             "\n        \"rspCrr_incong_RED_1\",\n        \"rspTme_incong_RED_1\",\n        \"rspCrr_neutral_RED_1\",\n        \"rspTme_neutral_RED_1\",",
             "\n        \"rspCrr_cong_BLUE_1\",\n        \"rspTme_cong_BLUE_1\",\n        \"rspCrr_incong_BLUE_1\",\n        \"rspTme_incong_BLUE_1\",",
             "\n        \"rspCrr_neutral_BLUE_1\",\n        \"rspTme_neutral_BLUE_1\",\n        \"rspCrr_cong_GREEN_2\",\n        \"rspTme_cong_GREEN_2\",",
             "\n        \"rspCrr_incong_GREEN_2\",\n        \"rspTme_incong_GREEN_2\",\n        \"rspCrr_neutral_GREEN_2\",\n        \"rspTme_neutral_GREEN_2\",",
             "\n        \"rspCrr_cong_YELLOW_2\",\n        \"rspTme_cong_YELLOW_2\",\n        \"rspCrr_incong_YELLOW_2\",\n        \"rspTme_incong_YELLOW_2\",",
             "\n        \"rspCrr_neutral_YELLOW_2\",\n        \"rspTme_neutral_YELLOW_2\",\n        \"rspCrr_cong_RED_2\",\n        \"rspTme_cong_RED_2\",",
             "\n        \"rspCrr_incong_RED_2\",\n        \"rspTme_incong_RED_2\",\n        \"rspCrr_neutral_RED_2\",\n        \"rspTme_neutral_RED_2\",",
             "\n        \"rspCrr_cong_BLUE_2\",\n        \"rspTme_cong_BLUE_2\",\n        \"rspCrr_incong_BLUE_2\",\n        \"rspTme_incong_BLUE_2\",",
             "\n        \"rspCrr_neutral_BLUE_2\",\n        \"rspTme_neutral_BLUE_2\"),\n    varExc = \"sex\",\n    excLvl = 1)"))

    expect_equal(jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", xfmNSS = names(dtaInp)[seq(4, 50, 2)], excNSS = "sex")$parent$asSource(),
      paste0("jTransform::jtWide2Long(\n    data = data,\n    mdeW2L = \"NSS\",\n    id_NSS = ID,\n    xfmNSS = vars(rspTme_cong_GREEN_1, rspTme_incong_GREEN_1, ",
             "rspTme_neutral_GREEN_1, rspTme_cong_YELLOW_1, rspTme_incong_YELLOW_1, rspTme_neutral_YELLOW_1, rspTme_cong_RED_1, rspTme_incong_RED_1, ",
             "rspTme_neutral_RED_1, rspTme_cong_BLUE_1, rspTme_incong_BLUE_1, rspTme_neutral_BLUE_1, rspTme_cong_GREEN_2, rspTme_incong_GREEN_2, ",
             "rspTme_neutral_GREEN_2, rspTme_cong_YELLOW_2, rspTme_incong_YELLOW_2, rspTme_neutral_YELLOW_2, rspTme_cong_RED_2, rspTme_incong_RED_2, ",
             "rspTme_neutral_RED_2, rspTme_cong_BLUE_2, rspTme_incong_BLUE_2, rspTme_neutral_BLUE_2),\n    excNSS = sex)"))
    expect_equal(jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                                         xfmNSA = list(list(label = "rspCrr", vars = names(dtaInp)[seq(3, 50, 2)]),
                                                       list(label = "rspTme", vars = names(dtaInp)[seq(4, 50, 2)])),
                                         idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)))$parent$asSource(),
      paste0("jTransform::jtWide2Long(\n    data = data,\n    mdeW2L = \"NSA\",\n    id_NSA = ID,\n    xfmNSA = list(\n        ",
             "list(\n            label=\"rspCrr\",\n            vars=c(\n                \"rspCrr_cong_GREEN_1\",\n                \"rspCrr_incong_GREEN_1\",\n",
             "                \"rspCrr_neutral_GREEN_1\",\n                \"rspCrr_cong_YELLOW_1\",\n                \"rspCrr_incong_YELLOW_1\",\n",
             "                \"rspCrr_neutral_YELLOW_1\",\n                \"rspCrr_cong_RED_1\",\n                \"rspCrr_incong_RED_1\",\n",
             "                \"rspCrr_neutral_RED_1\",\n                \"rspCrr_cong_BLUE_1\",\n                \"rspCrr_incong_BLUE_1\",\n",
             "                \"rspCrr_neutral_BLUE_1\",\n                \"rspCrr_cong_GREEN_2\",\n                \"rspCrr_incong_GREEN_2\",\n",
             "                \"rspCrr_neutral_GREEN_2\",\n                \"rspCrr_cong_YELLOW_2\",\n                \"rspCrr_incong_YELLOW_2\",\n",
             "                \"rspCrr_neutral_YELLOW_2\",\n                \"rspCrr_cong_RED_2\",\n                \"rspCrr_incong_RED_2\",\n",
             "                \"rspCrr_neutral_RED_2\",\n                \"rspCrr_cong_BLUE_2\",\n                \"rspCrr_incong_BLUE_2\",\n",
             "                \"rspCrr_neutral_BLUE_2\")),\n        ",
             "list(\n            label=\"rspTme\",\n            vars=c(\n                \"rspTme_cong_GREEN_1\",\n                \"rspTme_incong_GREEN_1\",\n",
             "                \"rspTme_neutral_GREEN_1\",\n                \"rspTme_cong_YELLOW_1\",\n                \"rspTme_incong_YELLOW_1\",\n",
             "                \"rspTme_neutral_YELLOW_1\",\n                \"rspTme_cong_RED_1\",\n                \"rspTme_incong_RED_1\",\n",
             "                \"rspTme_neutral_RED_1\",\n                \"rspTme_cong_BLUE_1\",\n                \"rspTme_incong_BLUE_1\",\n",
             "                \"rspTme_neutral_BLUE_1\",\n                \"rspTme_cong_GREEN_2\",\n                \"rspTme_incong_GREEN_2\",\n",
             "                \"rspTme_neutral_GREEN_2\",\n                \"rspTme_cong_YELLOW_2\",\n                \"rspTme_incong_YELLOW_2\",\n",
             "                \"rspTme_neutral_YELLOW_2\",\n                \"rspTme_cong_RED_2\",\n                \"rspTme_incong_RED_2\",\n",
             "                \"rspTme_neutral_RED_2\",\n                \"rspTme_cong_BLUE_2\",\n                \"rspTme_incong_BLUE_2\",\n",
             "                \"rspTme_neutral_BLUE_2\"))),\n    excNSA = sex,\n    idxNSA = list(\n        list(\n            var=\"cong\",\n            ",
             "levels=3),\n        list(\n            var=\"colour\",\n            levels=4),\n        list(\n            var=\"rep\",\n            levels=2)))"))

    # check instructions when chkVar fails (xfmSep is empty, mdeW2L = "Sep")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "Sep", id_Sep = "ID", excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1", shwHlp = TRUE)
    expect_equal(names(chkRes), c("genSep", "genNSS", "genNSA", "dtaInf", "pvwDta", "pvwLvl", "addSep", "addNSS", "addNSA"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(dim(chkRes$pvwDta$asDF), c(1, 1))
    expect_equal(dim(chkRes$pvwLvl$asDF), c(0, 1))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE),
                 c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))

    # check instructions when chkVar fails (xfmNSS is empty, mdeW2L = "NSS")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSS", id_NSS = "ID", excNSS = "sex", shwHlp = TRUE)
    expect_equal(names(chkRes), c("genSep", "genNSS", "genNSA", "dtaInf", "pvwDta", "pvwLvl", "addSep", "addNSS", "addNSA"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(dim(chkRes$pvwDta$asDF), c(1, 1))
    expect_equal(dim(chkRes$pvwLvl$asDF), c(0, 1))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE),
                 c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))

    # check instructions when chkVar fails (xfmNSA is empty, mdeW2L = "NSA")
    chkRes <- jTransform::jtWide2Long(data = dtaInp, mdeW2L = "NSA", id_NSA = "ID", excNSA = "sex",
                                      idxNSA = list(list(var = "cong", levels = 3), list(var = "colour", levels = 4), list(var = "rep", levels = 2)),
                                      shwHlp = TRUE)
    expect_equal(names(chkRes), c("genSep", "genNSS", "genNSA", "dtaInf", "pvwDta", "pvwLvl", "addSep", "addNSS", "addNSA"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(dim(chkRes$pvwDta$asDF), c(1, 1))
    expect_equal(dim(chkRes$pvwLvl$asDF), c(0, 1))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE),
                 c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtWide2Long(mdeW2L = "Sep", id_Sep = "ID", xfmSep = names(dtaInp)[seq(3, 50)], excSep = "sex", pfxSep = "cond", chrSep = "_", lvlSep = "1"),
      regexp = paste("Argument 'id_Sep' contains 'ID' which is not present in the dataset"))
})
