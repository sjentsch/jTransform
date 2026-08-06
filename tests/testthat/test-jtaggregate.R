testthat::test_that("jtaggregate works", {
    set.seed(1234)
    dtaInp <- data.frame(ID = rep(sprintf("%03d", seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = as.factor(round(rnorm(n = 100 * 10, 3, 2 / 3))),
                         V3 = rep(NA, 1000))
    attr(dtaInp[, "V1"], "jmv-desc") <- "Variable V1"
    attr(dtaInp[, "V2"], "jmv-desc") <- "Variable V2"
    attr(dtaInp[, "V3"], "jmv-desc") <- "Variable V3"
    dtaInp[sample(41:100, 10), "V1"] <- NA
    dtaInp[sample(41:100, 10), "V2"] <- NA

    # N, mean, median, mode, sum, drpNA - TRUE ========================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 100 rows): ID, V1_N,\n",
                                                 "V1_Mn, V1_Mdn, V1_Mde, V1_Sum, V2_N, V2_Mn, V2_Mdn, V2_Mde, V2_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                                                  \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_N    V1_Mn       V1_Mdn      V1_Mde        V1_Sum      V2_N    V2_Mn       V2_Mdn      V2_Mde   \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   001        10    48.92264    61.57871     0.9495756    489.2264      10    3.000000    3.000000     ... ᵃ   \n",
             "   002        10    45.46337    28.92696    18.6722790    454.6337      10    2.700000    3.000000     ...     \n",
             "   003        10    41.65217    30.96529     3.9995918    416.5217      10    3.100000    3.000000     ...     \n",
             "   004        10    47.33585    38.03818    18.1096208    473.3585      10    3.000000    3.000000     ...     \n",
             "   005         9    51.68482    55.33336    24.3928827    465.1634       9    2.888889    3.000000     ...     \n",
             "   006         8    40.93342    40.18238     7.3779880    327.4674       9    2.444444    2.000000     ...     \n",
             "   007         7    24.80827    23.90257     1.3749939    173.6579       9    3.000000    3.000000     ...     \n",
             "   008        10    38.64675    35.24556     1.4627256    386.4675       7    2.571429    3.000000     ...     \n",
             "   009         9    50.52262    47.19097    14.2615343    454.7036       7    2.714286    3.000000     ...     \n",
             "   010 ᵇ     ...         ...         ...           ...         ...     ...         ...         ...     ...     \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 1 more columns in the data set not shown here. A complete list of variables can be found in\n",
             "   \"Variables in the Output Data Set\" above this table.\n",
             "   ᵇ There are 90 more rows in the data set not shown here.\n\n"))
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
    expect_equal(chkRes$pvwDta$width, 109)

    # N, mean, median, mode, sum, drpNA - FALSE =======================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 100 rows): ID, V1_N,\n",
                                                 "V1_Mn, V1_Mdn, V1_Mde, V1_Sum, V2_N, V2_Mn, V2_Mdn, V2_Mde, V2_Sum\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                                                  \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_N    V1_Mn       V1_Mdn      V1_Mde        V1_Sum      V2_N    V2_Mn       V2_Mdn      V2_Mde   \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   001        10    48.92264    61.57871     0.9495756    489.2264      10    3.000000    3.000000     ... ᵃ   \n",
             "   002        10    45.46337    28.92696    18.6722790    454.6337      10    2.700000    3.000000     ...     \n",
             "   003        10    41.65217    30.96529     3.9995918    416.5217      10    3.100000    3.000000     ...     \n",
             "   004        10    47.33585    38.03818    18.1096208    473.3585      10    3.000000    3.000000     ...     \n",
             "   005         9                                                         9                             ...     \n",
             "   006         8                                                         9                             ...     \n",
             "   007         7                                                         9                             ...     \n",
             "   008        10    38.64675    35.24556     1.4627256    386.4675       7                             ...     \n",
             "   009         9                                                         7                             ...     \n",
             "   010 ᵇ     ...         ...         ...           ...         ...     ...         ...         ...     ...     \n",
             " ───────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 1 more columns in the data set not shown here. A complete list of variables can be found in\n",
             "   \"Variables in the Output Data Set\" above this table.\n",
             "   ᵇ There are 90 more rows in the data set not shown here.\n\n"))
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
    expect_equal(chkRes$pvwDta$width, 109)

    # missing, SD, variance, range, drpNA - TRUE ======================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcMss = TRUE, clcSD = TRUE, clcVar = TRUE, clcRng = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (9 variables in 100 rows): ID,\n",
                                                 "V1_Mss, V1_SD, V1_Var, V1_Rng, V2_Mss, V2_SD, V2_Var, V2_Rng\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                                             \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_Mss    V1_SD       V1_Var       V1_Rng      V2_Mss    V2_SD        V2_Var       V2_Rng     \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   001           0    27.48823     755.6028    85.14196         0    0.6666667    0.4444444    2.000000   \n",
             "   002           0    27.33007     746.9329    73.67107         0    0.9486833    0.9000000    3.000000   \n",
             "   003           0    33.24971    1105.5434    87.46622         0    0.5676462    0.3222222    2.000000   \n",
             "   004           0    28.70163     823.7836    81.10542         0    0.4714045    0.2222222    2.000000   \n",
             "   005           1    18.31531     335.4506    52.15310         1    0.6009252    0.3611111    2.000000   \n",
             "   006           2    27.95453     781.4560    77.46125         1    0.8819171    0.7777778    3.000000   \n",
             "   007           3    22.68960     514.8179    55.08199         1    0.7071068    0.5000000    2.000000   \n",
             "   008           0    31.82860    1013.0598    87.82091         3    0.5345225    0.2857143    1.000000   \n",
             "   009           1    33.00633    1089.4176    78.37851         3    0.4879500    0.2380952    1.000000   \n",
             "   010 ᵃ       ...         ...          ...         ...       ...          ...          ...         ...   \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 90 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 4), rep(c("Mss", "SD", "Var", "Rng"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 104)

    # missing, SD, variance, range, drpNA - FALSE =====================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcMss = TRUE, clcSD = TRUE, clcVar = TRUE, clcRng = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (9 variables in 100 rows): ID,\n",
                                                 "V1_Mss, V1_SD, V1_Var, V1_Rng, V2_Mss, V2_SD, V2_Var, V2_Rng\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                                             \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_Mss    V1_SD       V1_Var       V1_Rng      V2_Mss    V2_SD        V2_Var       V2_Rng     \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   001           0    27.48823     755.6028    85.14196         0    0.6666667    0.4444444    2.000000   \n",
             "   002           0    27.33007     746.9329    73.67107         0    0.9486833    0.9000000    3.000000   \n",
             "   003           0    33.24971    1105.5434    87.46622         0    0.5676462    0.3222222    2.000000   \n",
             "   004           0    28.70163     823.7836    81.10542         0    0.4714045    0.2222222    2.000000   \n",
             "   005           1                                              1                                         \n",
             "   006           2                                              1                                         \n",
             "   007           3                                              1                                         \n",
             "   008           0    31.82860    1013.0598    87.82091         3                                         \n",
             "   009           1                                              3                                         \n",
             "   010 ᵃ       ...         ...          ...         ...       ...          ...          ...         ...   \n",
             " ──────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 90 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 4), rep(c("Mss", "SD", "Var", "Rng"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 104)

    # minimum, maximum, IQR, drpNA - TRUE =============================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = TRUE,
                                      clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 100 rows): ID,\n",
                                                 "V1_Min, V1_Max, V1_IQR, V2_Min, V2_Max, V2_IQR\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                         \n",
             " ──────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_Min        V1_Max      V1_IQR      V2_Min      V2_Max      V2_IQR      \n",
             " ──────────────────────────────────────────────────────────────────────────────────── \n",
             "   001       0.9495756    86.09154    33.31021    2.000000    4.000000    0.0000000   \n",
             "   002      18.6722790    92.34335    38.56382    1.000000    4.000000    1.0000000   \n",
             "   003       3.9995918    91.46582    56.53889    2.000000    4.000000    0.0000000   \n",
             "   004      18.1096208    99.21504    43.61757    2.000000    4.000000    0.0000000   \n",
             "   005      24.3928827    76.54598    31.66359    2.000000    4.000000    0.0000000   \n",
             "   006       7.3779880    84.83924    38.80309    1.000000    4.000000    1.0000000   \n",
             "   007       1.3749939    56.45698    36.61129    2.000000    4.000000    0.0000000   \n",
             "   008       1.4627256    89.28364    53.33280    2.000000    3.000000    1.0000000   \n",
             "   009      14.2615343    92.64005    70.00112    2.000000    3.000000    0.5000000   \n",
             "   010 ᵃ           ...         ...         ...         ...         ...          ...   \n",
             " ──────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 90 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 3), rep(c("Min", "Max", "IQR"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 84)

    # minimum, maximum, IQR, drpNA - FALSE ============================================================================
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                      clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE)
    expect_equal(class(chkRes), c("jtAggregateResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (7 variables in 100 rows): ID,\n",
                                                 "V1_Min, V1_Max, V1_IQR, V2_Min, V2_Max, V2_IQR\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste0("\n Data Preview                                                                        \n",
             " ─────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_Min        V1_Max      V1_IQR      V2_Min      V2_Max      V2_IQR     \n",
             " ─────────────────────────────────────────────────────────────────────────────────── \n",
             "   001       0.9495756    86.09154    33.31021    2.000000    4.000000    0.000000   \n",
             "   002      18.6722790    92.34335    38.56382    1.000000    4.000000    1.000000   \n",
             "   003       3.9995918    91.46582    56.53889    2.000000    4.000000    0.000000   \n",
             "   004      18.1096208    99.21504    43.61757    2.000000    4.000000    0.000000   \n",
             "   005                                                                               \n",
             "   006                                                                               \n",
             "   007                                                                               \n",
             "   008       1.4627256    89.28364    53.33280                                       \n",
             "   009                                                                               \n",
             "   010 ᵃ           ...         ...         ...         ...         ...         ...   \n",
             " ─────────────────────────────────────────────────────────────────────────────────── \n",
             "   ᵃ There are 90 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns),
                 c("fstCol", sprintf("%s_%s", rep(c("V1", "V2"), each = 3), rep(c("Min", "Max", "IQR"), 2))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, "There are 90 more rows in the data set not shown here.")
    expect_equal(chkRes$pvwDta$options$varsRequired, list("V1", "V2", "ID"))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 83)

    # ensure that a completely empty data column is raising an error message
    expect_error(jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2", "V3"), grpAgg = c("ID"), drpNA = TRUE,
                                         clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE),
                 "The variable 'V3' contains only missing / invalid values.")

    # ensure that help is shown
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), clcMin = TRUE, shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                         clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)$parent$asSource(),
      paste0("jmvReadWrite::aggregate_omv(\n    dtaInp = data,\n    varAgg = c(\"V1\", \"V2\"),\n    grpAgg = \"ID\",\n",
             "    clcN = TRUE,\n    clcMn = TRUE,\n    clcMdn = TRUE,\n    clcMde = TRUE,\n    clcSum = TRUE,\n",
             "    drpNA = FALSE)"))

    # check when chkVar fails (varAgg is empty)
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c(), grpAgg = c("ID"), clcN = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check when chkVar fails (grpAgg is empty)
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c(), clcN = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check when chkVar fails (not any clc... set to TRUE)
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"))
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check help messages
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), clcN = TRUE, shwHlp = TRUE)
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
