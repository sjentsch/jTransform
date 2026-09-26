testthat::test_that("jtdistances works", {
    set.seed(1)
    dtaInp <- as.data.frame(matrix(rnorm(11000), nrow = 1000))

    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = c(0,           46.22462099, 45.02842697, 45.80499632, 46.11178773, 45.44652148, 46.58933478, 45.41292200, 44.99703457),
                      V2     = c(46.22462099,  0,          45.77490541, 46.10595963, 45.15215567, 44.56679206, 45.69738428, 46.57831483, 46.52352553),
                      V3     = c(45.02842697, 45.77490541,  0,          46.01084362, 46.84149533, 44.30804655, 47.04643135, 45.91865097, 45.77148823),
                      V4     = c(45.80499632, 46.10595963, 46.01084362,  0,          45.31441882, 47.08983560, 46.02390595, 44.76331205, 45.40934232),
                      V5     = c(46.11178773, 45.15215567, 46.84149533, 45.31441882,  0,          44.83821174, 45.19525852, 44.79030955, 44.95125597),
                      V6     = c(45.44652148, 44.56679206, 44.30804655, 47.08983560, 44.83821174,  0,          45.64828692, 44.81304360, 44.87033945),
                      V7     = c(46.58933478, 45.69738428, 47.04643135, 46.02390595, 45.19525852, 45.64828692,  0,          45.71271630, 45.39731490),
                      V8     = c(45.41292200, 46.57831483, 45.91865097, 44.76331205, 44.79030955, 44.81304360, 45.71271630,  0,          43.68123248),
                      V9     = c(44.99703457, 46.52352553, 45.77148823, 45.40934232, 44.95125597, 44.87033945, 45.39731490, 43.68123248, 0)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 10))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 1 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), dstStd = "range", dstCtg = "ctgCnt", dstCnt = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = c(0,          6.74276499, 6.82182384, 7.01764729, 6.97305436, 7.05043946, 6.90200943, 7.04894623, 6.59519985),
                      V2     = c(6.74276499, 0,          6.89837117, 7.02730355, 6.79042517, 6.87675297, 6.73381968, 7.19131425, 6.78068363),
                      V3     = c(6.82182384, 6.89837117, 0,          7.26217534, 7.31341661, 7.08883552, 7.19405403, 7.34566204, 6.93767244),
                      V4     = c(7.01764729, 7.02730355, 7.26217534, 0,          7.15184698, 7.61180952, 7.11494404, 7.23275939, 6.96341565),
                      V5     = c(6.97305436, 6.79042517, 7.31341661, 7.15184698, 0,          7.17745649, 6.90137027, 7.16868327, 6.80080935),
                      V6     = c(7.05043946, 6.87675297, 7.08883552, 7.61180952, 7.17745649, 0,          7.14781025, 7.34833414, 6.97077063),
                      V7     = c(6.90200943, 6.73381968, 7.19405403, 7.11494404, 6.90137027, 7.14781025, 0,          7.16006097, 6.72369552),
                      V8     = c(7.04894623, 7.19131425, 7.34566204, 7.23275939, 7.16868327, 7.34833414, 7.16006097, 0,          6.79096966),
                      V9     = c(6.59519985, 6.78068363, 6.93767244, 6.96341565, 6.80080935, 6.97077063, 6.72369552, 6.79096966, 0)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 10))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 1 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # ensure that a completely empty data column is raising an error message
    expect_error(jTransform::jtDistances(data = cbind(dtaInp, data.frame(V12 = rep(NA, 1000))), varDst = c("V1", "V12"),
                                         dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid"),
                 "The variable 'V12' contains only missing / invalid values.")

    # ensure that help is shown
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid", shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), dstStd = "range", dstCtg = "ctgCnt", dstCnt = "euclid")$parent$asSource(),
      paste0("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\n        \"V1\",\n        \"V2\",\n        \"V3\",",
             "\n        \"V4\",\n        \"V5\",\n        \"V6\",\n        \"V7\",\n        \"V8\",\n        \"V9\",\n        \"V10\",",
             "\n        \"V11\"),\n    stdDst = \"range\")"))

    # check when chkVar fails (varDst has only one variable)
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp)[1], dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid")
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check help messages
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid", shwHlp = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE, TRUE, TRUE))
    expect_true(is.character(chkRes$genInf$content))
    expect_true(nzchar(chkRes$genInf$content))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtDistances(varDst = names(dtaInp)[1:5], , dstStd = "none", dstCtg = "ctgCnt", dstCnt = "euclid"),
      regexp = paste("Argument 'varDst' contains 'V1', 'V2', 'V3', 'V4', 'V5' which are not present in the dataset"))

    # additional tests for functions in utils.R
    expect_true(hmeDir() %in% Sys.getenv(c("USERPROFILE", "HOME")))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::distances_omv", crrArg = list(varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid")),
      paste("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\n        \"V1\",\n        \"V2\",\n        \"V3\",\n",
            "       \"V4\",\n        \"V5\",\n        \"V6\",\n        \"V7\",\n        \"V8\",\n        \"V9\",\n        \"V10\",\n",
            "       \"V11\"))"))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::distances_omv", crrArg = list(varDst = names(dtaInp)[seq(3)], stdDst = "z", nmeDst = "jaccards")),
      paste("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\"V1\", \"V2\", \"V3\"),\n",
            "   stdDst = \"z\",\n    nmeDst = \"jaccards\")"))

})
