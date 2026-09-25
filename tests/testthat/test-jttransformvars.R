testthat::test_that("jttransformvars works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtTransformVars.omv")

    chkRes <- jTransform::jtTransformVars(data = dtaInp, posSqr = "mdrPos", negSqr = "mdrNeg", posLog = "strPos", negLog = "strNeg",
                                          posInv = "extPos", negInv = "extNeg")
    expect_equal(class(chkRes), c("jtTransformVarsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (12 variables in 1000 rows): mdrPos,\n",
                                                 "mdrNeg, strPos, strNeg, extPos, extNeg, mdrPos_SQR, mdrNeg_SQR,\n",
                                                 "strPos_LOG, strNeg_LOG, extPos_INV, extNeg_INV\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -c(3, 4, 10)], as.numeric),
                 list(fstCol     =  c(0.59603017, 0.48744137, 1.07953913,  0.69929821, 1.42150909,  0.43059315, 0.59364654, 0.62063576, 0.66946771),
                      mdrNeg_SQR =  c(0.79549028, 0.63444249, 0.98826110,  0.39903628, 0.87352892,  0.62920001, 0.72863691, 0.94895554, 1.08092774),
                      extPos_INV =  c(0.68486806, 0.93782944, 0.66954971,  0.98813181, 0.78454171,  0.71851658, 0.67246457, 0.75103517, 0.93060233),
                      extNeg_INV =  c(0.62994369, 0.96739613, 0.79553025,  0.34822987, 0.84254230,  0.51872773, 0.97620744, 0.42835734, 0.84790096),
                      mdrPos     =  c(0.13274893, 0.01509606, 0.94290170,  0.26651496, 1.79818506, -0.03709257, 0.12991319, 0.16268572, 0.22568398),
                      mdrNeg     = -c(0.38705516, 0.15676766, 0.73091040, -0.08651966, 0.51730316,  0.15014304, 0.28516213, 0.65476700, 0.92265515),
                      strPos     =  c(0.07789507, 0.79311404, 0.24726690,  0.20601021, 0.32099993,  0.30201235, 0.14379841, 0.12662232, 0.29869013)))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, c(3, 4)], as.numeric),
                 list(strPos_LOG =  c(0.03695995, 0.25624825, 0.09975020,  0.08527089, 0.12448288,  0.11824718, 0.06248159, 0.05597314, 0.11714687),
                      strNeg_LOG =  c(0.06493243, 0.09355705, 0.17128262,  0.30944038, 0.07966164,  0.00922484, 0.19602943, 0.05563840, 0.43540430)),
                      tol = 1e-6)
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(chkRes$pvwDta$notes$Note$note,
                 paste("The columns mdrPos_SQR, mdrNeg_SQR, strPos_LOG, strNeg_LOG, extPos_INV, extNeg_INV, mdrPos,",
                       "mdrNeg, strPos, strNeg, extPos, extNeg are shown first in this preview. In the created data set,",
                       "the variable order is as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", "mdrNeg_SQR", "strPos_LOG", "strNeg_LOG", "extPos_INV", "extNeg_INV", "mdrPos", "mdrNeg", "strPos", "strNeg" ))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 2 more columns in the data set not shown here. A complete list of variables can be found in",
                                                  "\"Variables in the Output Data Set\" above this table."),
                                                  "There are 990 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtTransformVars(data = dtaInp, posInv = c("mdrPos", "strPos", "extPos"), negInv = c("mdrNeg", "strNeg", "extNeg"))
    expect_equal(class(chkRes), c("jtTransformVarsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (12 variables in 1000 rows): mdrPos,\n",
                                                 "strPos, extPos, mdrNeg, strNeg, extNeg, mdrPos_INV, strPos_INV,\n",
                                                 "extPos_INV, mdrNeg_INV, strNeg_INV, extNeg_INV\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol     = c(0.73787017, 0.80801611, 0.46180743, 0.67158356, 0.33105040,  0.84358966, 0.73941734, 0.72192328, 0.69051855),
                      strPos_INV = c(0.91841728, 0.55430878, 0.79478524, 0.82172995, 0.75078765,  0.76164539, 0.86600104, 0.87907688, 0.76357751),
                      extPos_INV = c(0.68486806, 0.93782944, 0.66954971, 0.98813181, 0.78454171,  0.71851658, 0.67246457, 0.75103517, 0.93060233),
                      mdrNeg_INV = c(0.61244309, 0.71300370, 0.50590390, 0.86264162, 0.56719799,  0.71638746, 0.65320552, 0.52617272, 0.46116851),
                      strNeg_INV = c(0.86112773, 0.80620029, 0.67408922, 0.49041034, 0.83241204,  0.97898302, 0.63675237, 0.87975470, 0.36694054),
                      extNeg_INV = c(0.62994369, 0.96739613, 0.79553025, 0.34822987, 0.84254230,  0.51872773, 0.97620744, 0.42835734, 0.84790096),
                      mdrPos     = c(0.13274893, 0.01509606, 0.94290170, 0.26651496, 1.79818506, -0.03709257, 0.12991319, 0.16268572, 0.22568398),
                      strPos     = c(0.07789507, 0.79311404, 0.24726690, 0.20601021, 0.32099993, 0.30201235, 0.14379841, 0.12662232, 0.29869013),
                      extPos     = c(0.46024298, 0.06639969, 0.49364881, 0.01211847, 0.27473722, 0.39186407, 0.48717492, 0.33160329, 0.07468059)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(chkRes$pvwDta$notes$Note$note,
                 paste("The columns mdrPos_INV, strPos_INV, extPos_INV, mdrNeg_INV, strNeg_INV, extNeg_INV, mdrPos,",
                       "strPos, extPos, mdrNeg, strNeg, extNeg are shown first in this preview. In the created data set,",
                       "the variable order is as shown in \"Variables in the Output Data Set\" above this table."))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", "strPos_INV", "extPos_INV", "mdrNeg_INV", "strNeg_INV", "extNeg_INV", "mdrPos", "strPos", "extPos", "mdrNeg"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 2 more columns in the data set not shown here. A complete list of variables can be found in",
                                                  "\"Variables in the Output Data Set\" above this table."),
                                                  "There are 990 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3, 5, 2, 4, 6)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # ensure that a completely empty data column is raising an error message
    expect_error(jTransform::jtTransformVars(data = cbind(dtaInp, data.frame(V1 = NA)), posInv = c("V1")),
                 "The variable 'V1' contains only missing / invalid values.")
    expect_error(jTransform::jtTransformVars(data = cbind(dtaInp, data.frame(V1 = NA)), negSqr = c("V1")),
                 "The variable 'V1' contains only missing / invalid values.")

    # ensure that help is shown
    chkRes <- jTransform::jtTransformVars(data = dtaInp, posInv = c("mdrPos"), negInv = c("mdrNeg"), shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtTransformVars(data = dtaInp, posSqr = "mdrPos", negSqr = "mdrNeg", posLog = "strPos", negLog = "strNeg",
                                             posInv = "extPos", negInv = "extNeg")$parent$asSource(),
      paste0("jmvReadWrite::transform_vars_omv(\n    dtaInp = data,\n    varXfm = list(\n        posSqr = \"mdrPos\",\n        negSqr = \"mdrNeg\",",
             "\n        posLog = \"strPos\",\n        negLog = \"strNeg\",\n        posInv = \"extPos\",\n        negInv = \"extNeg\"))"))

    # check instructions when chkVar fails (transformation variable lists are empty)
    chkRes <- jTransform::jtTransformVars(data = dtaInp, varAll = names(dtaInp))
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))

    chkRes <- jTransform::jtTransformVars()
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$dtaInf$content, "")
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
})
