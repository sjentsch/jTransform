testthat::test_that("jtaggregate works", {
    set.seed(1234)
    dtaIn1 <- data.frame(ID = rep(sprintf("%03d", seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = as.factor(round(rnorm(n = 100 * 10, 3, 2 / 3))),
                         V3 = rep(NA, 1000))
    attr(dtaIn1[, "V1"], "jmv-desc") <- "Variable V1"
    attr(dtaIn1[, "V2"], "jmv-desc") <- "Variable V2"
    attr(dtaIn1[, "V3"], "jmv-desc") <- "Variable V3"
    dtaIn1[sample(41:100, 10), "V1"] <- NA
    dtaIn1[sample(41:100, 10), "V2"] <- NA
    dtaIn2 <- ToothGrowth
    dtaIn2[sample(11:60, 5), "len"] <- NA

    # N, mean, median, mode, sum, drpNA - TRUE ========================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 100 rows): ID, V1_N,\n",
                                                 "V1_Mn, V1_Mdn, V1_Mde, V1_Sum, V2_N, V2_Mn, V2_Mdn, V2_Mde, V2_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = seq(9), V1_N = c(rep(10, 4), 9, 8, 7, 10, 9),
                      V1_Mn  = c(48.92264141, 45.46337429, 41.65216863, 47.33584685, 51.68482201, 40.93342287, 24.80827316, 38.64675246, 50.52261921),
                      V1_Mdn = c(61.57870688, 28.92695625, 30.96529128, 38.03818427, 55.33335907, 40.18237625, 23.90257267, 35.24555549, 47.19097211),
                      V1_Mde = c(0.94957564,  18.67227897,  3.99959181, 18.10962083, 24.39288273,  7.37798801,  1.37499392,  1.46272557, 14.26153432),
                      V1_Sum = c(489.2264142, 454.6337429, 416.5216863, 473.3584685, 465.1633981, 327.4673830, 173.6579121, 386.4675246, 454.7035729),
                      V2_N   = c(rep(10, 4), rep(9, 3), 7, 7), V2_Mn  = c(3, 2.7, 3.1, 3, 2.88888889, 2.44444444, 3, 2.57142857, 2.71428571),
                      V2_Mdn = c(rep(3, 5), 2, rep(3, 3))))
    expect_equal(chkRes$pvwDta$asDF[, "V2_Mde"], rep("...", 10))
    expect_equal(as.character(chkRes$pvwDta$asDF[10, ]), c("010", rep("...", 9)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("V1_%s", c("N", "Mn", "Mdn", "Mde", "Sum")), sprintf("V2_%s", c("N", "Mn", "Mdn", "Mde"))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtAggregate(data = dtaIn2, varAgg = "len", grpAgg = c("supp", "dose"), drpNA = TRUE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 6 rows): supp, dose,\n",
                                                 "len_N, len_Mn, len_Mdn, len_Mde, len_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asDF,
                 data.frame(fstCol  = rep(c("OJ", "VC"), 3), dose = rep(c(0.5, 1, 2), each = 2), len_N = c(9, 10, 10, 8, 9, 9),
                            len_Mn  = c(13.622222, 7.98, 22.7, 17.2, 25.522222, 26.655556),
                            len_Mdn = c(14.5, 7.15, 23.45, 16.9, 25.5, 26.4),
                            len_Mde = c(8.2, 11.2, 14.5, 17.3, 26.4, 18.5),
                            len_Sum = c(122.6, 79.8, 227, 137.6, 229.7, 239.9),
                            row.names = c("\"1\"", "2", "3", "4", "5", "6")))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", "dose", sprintf("len_%s", c("N", "Mn", "Mdn", "Mde", "Sum"))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:6)))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, list("len", "supp", "dose"))
    expect_equal(chkRes$pvwDta$rowCount, 6)
      expect_equal(chkRes$pvwDta$rowSelected, 0)

    # N, mean, median, mode, sum, drpNA - FALSE =======================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 100 rows): ID, V1_N,\n",
                                                 "V1_Mn, V1_Mdn, V1_Mde, V1_Sum, V2_N, V2_Mn, V2_Mdn, V2_Mde, V2_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, -10], as.numeric),
                 list(fstCol = seq(9), V1_N = c(rep(10, 4), 9, 8, 7, 10, 9),
                      V1_Mn  = c(48.92264141,   45.46337428,  41.65216862,  47.33584685, rep(NA, 3),  38.64675246, NA),
                      V1_Mdn = c(61.57870688,   28.92695624,  30.96529127,  38.03818427, rep(NA, 3),  35.24555548, NA),
                      V1_Mde = c(0.94957564,    18.67227897,   3.99959181,  18.10962083, rep(NA, 3),   1.46272557, NA),
                      V1_Sum = c(489.22641415, 454.63374285, 416.52168629, 473.35846853, rep(NA, 3), 386.46752462, NA),
                      V2_N   = c(rep(10, 4), rep(9, 3), 7, 7), V2_Mn = c(3, 2.7, 3.1, 3.0, rep(NA, 5)),
                      V2_Mdn = c(rep(3,  4), rep(NA, 5))))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("V1_%s", c("N", "Mn", "Mdn", "Mde", "Sum")), sprintf("V2_%s", c("N", "Mn", "Mdn", "Mde"))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 90 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    chkRes <- jTransform::jtAggregate(data = dtaIn2, varAgg = "len", grpAgg = c("supp", "dose"), drpNA = FALSE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 6 rows): supp, dose,\n",
                                                 "len_N, len_Mn, len_Mdn, len_Mde, len_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asDF,
                 data.frame(fstCol  = rep(c("OJ", "VC"), 3), dose = rep(c(0.5, 1, 2), each = 2), len_N = c(9, 10, 10, 8, 9, 9),
                            len_Mn  = c(NA,  7.98, 22.70, rep(NA, 3)), len_Mdn = c(NA,  7.15,  23.45, rep(NA, 3)),
                            len_Mde = c(NA, 11.20, 14.50, rep(NA, 3)), len_Sum = c(NA, 79.80, 227.00, rep(NA, 3)),
                            row.names = c("\"1\"", "2", "3", "4", "5", "6")))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", "dose", sprintf("len_%s", c("N", "Mn", "Mdn", "Mde", "Sum"))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:6)))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, list("len", "supp", "dose"))
    expect_equal(chkRes$pvwDta$rowCount, 6)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # missing, SD, variance, range, drpNA - TRUE ======================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcMss = TRUE, clcSD = TRUE, clcVar = TRUE, clcRng = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (9 variables in 100 rows): ID,\n",
                                                 "V1_Mss, V1_SD, V1_Var, V1_Rng, V2_Mss, V2_SD, V2_Var, V2_Rng\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, ], as.numeric),
                 list(fstCol = seq(9), V1_Mss = c(rep(0, 4), 1, 2, 3, 0, 1),
                      V1_SD  = c(27.48822952, 27.33007338, 33.24971351, 28.70163147, 18.31530988, 27.95453472, 22.68959828, 31.82860074, 33.00632637),
                      V1_Var = c(755.6027621, 746.9329107, 1105.543449, 823.7836490, 335.4505761, 781.4560114, 514.8178699, 1013.059825, 1089.417580),
                      V1_Rng = c(85.14196272, 73.67106946, 87.46622480, 81.10542092, 52.15309602, 77.46125304, 55.08199008, 87.82091259, 78.37851332),
                      V2_Mss = c(rep(0, 4), rep(1, 3), 3, 3),
                      V2_SD  = c(0.66666667, 0.94868330, 0.56764621, 0.47140452, 0.60092521, 0.88191710, 0.70710678, 0.53452248, 0.48795004),
                      V2_Var = c(0.44444444, 0.90000000, 0.32222222, 0.22222222, 0.36111111, 0.77777778, 0.50000000, 0.28571429, 0.23809524),
                      V2_Rng = c(2, 3, rep(2, 3), 3, 2, 1, 1)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 4), rep(c("Mss", "SD", "Var", "Rng"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # missing, SD, variance, range, drpNA - FALSE =====================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcMss = TRUE, clcSD = TRUE, clcVar = TRUE, clcRng = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (9 variables in 100 rows): ID,\n",
                                                 "V1_Mss, V1_SD, V1_Var, V1_Rng, V2_Mss, V2_SD, V2_Var, V2_Rng\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, ], as.numeric),
                 list(fstCol = seq(9), V1_Mss = c(rep(0, 4), 1, 2, 3, 0, 1),
                      V1_SD  = c(27.48822952, 27.33007338, 33.24971351, 28.70163147, NA, NA, NA, 31.82860074, NA),
                      V1_Var = c(755.6027621, 746.9329107, 1105.543449, 823.7836490, NA, NA, NA, 1013.059825, NA),
                      V1_Rng = c(85.14196272, 73.67106946, 87.46622480, 81.10542092, NA, NA, NA, 87.82091259, NA),
                      V2_Mss = c(rep(0, 4), rep(1, 3), 3, 3),
                      V2_SD  = c(0.66666667, 0.94868330, 0.56764621, 0.47140452, rep(NA, 5)),
                      V2_Var = c(0.44444444, 0.90000000, 0.32222222, 0.22222222, rep(NA, 5)),
                      V2_Rng = c(2, 3, 2, 2, rep(NA, 5))))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 4), rep(c("Mss", "SD", "Var", "Rng"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # minimum, maximum, IQR, drpNA - TRUE =============================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 100 rows): ID,\n",
                                                 "V1_Min, V1_Max, V1_IQR, V2_Min, V2_Max, V2_IQR\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, ], as.numeric),
                 list(fstCol = seq(9),
                      V1_Min = c(0.94957564,  18.67227897,  3.99959181, 18.10962083, 24.39288273,  7.37798801,  1.37499392,  1.46272557, 14.26153432),
                      V1_Max = c(86.09153836, 92.34334843, 91.46581660, 99.21504175, 76.54597876, 84.83924104, 56.45698400, 89.28363817, 92.64004764),
                      V1_IQR = c(33.31021495, 38.56381968, 56.53889136, 43.61756622, 31.66359183, 38.80309443, 36.61129132, 53.33279965, 70.00111977),
                      V2_Min = c(2, 1, rep(2, 3), 1, rep(2, 3)), V2_Max = c(rep(4, 7), 3, 3), V2_IQR = c(0, 1, rep(0, 3), 1, 0, 1, 0.5)))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 3), rep(c("Min", "Max", "IQR"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # minimum, maximum, IQR, drpNA - FALSE ============================================================================
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 100 rows): ID,\n",
                                                 "V1_Min, V1_Max, V1_IQR, V2_Min, V2_Max, V2_IQR\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(lapply(chkRes$pvwDta$asDF[-10, ], as.numeric),
                 list(fstCol = seq(9),
                      V1_Min = c(0.94957564,  18.67227897,  3.99959181, 18.10962083, rep(NA, 3),  1.46272557, NA),
                      V1_Max = c(86.09153836, 92.34334843, 91.46581660, 99.21504175, rep(NA, 3), 89.28363817, NA),
                      V1_IQR = c(33.31021495, 38.56381968, 56.53889136, 43.61756622, rep(NA, 3), 53.33279965, NA),
                      V2_Min = c(2, 1, 2, 2, rep(NA, 5)), V2_Max = c(rep(4, 4), rep(NA, 5)), V2_IQR = c(0, 1, 0, 0, rep(NA, 5))))
    expect_equal(chkRes$pvwDta$title, "Data Preview")
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 3), rep(c("Min", "Max", "IQR"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)

    # ensure that a completely empty data column is raising an error message
    expect_error(jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2", "V3"), grpAgg = c("ID"), drpNA = TRUE,
                                         clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE),
                 "The variable 'V3' contains only missing / invalid values.")

    # ensure that help is shown
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), clcMin = TRUE, shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                         clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)$parent$asSource(),
      paste0("jmvReadWrite::aggregate_omv(\n    dtaInp = data,\n    varAgg = c(\"V1\", \"V2\"),\n    grpAgg = \"ID\",\n",
             "    clcN = TRUE,\n    clcMn = TRUE,\n    clcMdn = TRUE,\n    clcMde = TRUE,\n    clcSum = TRUE,\n",
             "    drpNA = FALSE)"))

    # check when chkVar fails (varAgg is empty)
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c(), grpAgg = c("ID"), clcN = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check when chkVar fails (grpAgg is empty)
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c(), clcN = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check when chkVar fails (not any clc... set to TRUE)
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"))
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check help messages
    chkRes <- jTransform::jtAggregate(data = dtaIn1, varAgg = c("V1", "V2"), grpAgg = c("ID"), clcN = TRUE, shwHlp = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE, TRUE, TRUE))
    expect_true(is.character(chkRes$genInf$content))
    expect_true(nzchar(chkRes$genInf$content))

    # additional tests for functions in utils.R
    expect_true(hmeDir() %in% Sys.getenv(c("USERPROFILE", "HOME")))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::aggregate_omv", crrArg = list(varAgg = c("V1", "V2"), grpAgg = c("ID"), clcN = TRUE)),
      "jmvReadWrite::aggregate_omv(\n    dtaInp = data,\n    varAgg = c(\"V1\", \"V2\"),\n    grpAgg = \"ID\",\n    clcN = TRUE)")
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::aggregate_omv", crrArg = list(varAgg = c("V1", "V2"), grpAgg = c("ID"), clcMn = TRUE)),
      "jmvReadWrite::aggregate_omv(\n    dtaInp = data,\n    varAgg = c(\"V1\", \"V2\"),\n    grpAgg = \"ID\",\n    clcMn = TRUE)")
})
