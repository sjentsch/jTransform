testthat::test_that("jtaggregate works", {
    set.seed(1234)
    dtaInp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = as.factor(round(rnorm(n = 100 * 10, 3, 2/3))))
    attr(dtaInp[, "V1"], "jmv-desc") <- "Variable V1"
    attr(dtaInp[, "V2"], "jmv-desc") <- "Variable V2"
    dtaInp[sample(91:160, 10), "V1"] <- NA
    dtaInp[sample(91:160, 10), "V2"] <- NA

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
             "     1        10    48.92264    61.57871     0.9495756    489.2264      10    3.000000    3.000000     ... ᵃ   \n",
             "    10         9    36.68164    30.01991    10.5287503    330.1348      10    3.100000    3.000000     ...     \n",
             "   100        10    51.41201    55.94399     0.1308702    514.1201      10    3.300000    3.000000     ...     \n",
             "    11         8    25.49881    24.22270     3.5456727    203.9905       8    3.250000    3.000000     ...     \n",
             "    12        10    51.64579    49.62289    10.0769041    516.4579       8    2.750000    3.000000     ...     \n",
             "    13        10    59.37018    50.01107    25.1545701    593.7018       7    3.000000    3.000000     ...     \n",
             "    14         7    62.82957    63.40994    12.7433398    439.8070      10    3.000000    3.000000     ...     \n",
             "    15        10    61.41777    57.13898    32.4386027    614.1777       8    2.875000    3.000000     ...     \n",
             "    16         6    55.61882    52.04698     8.2158066    333.7129       9    3.111111    3.000000     ...     \n",
             "    17 ᵇ     ...         ...         ...           ...         ...     ...         ...         ...     ...     \n",
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
             "     1        10    48.92264    61.57871     0.9495756    489.2264      10    3.000000    3.000000     ... ᵃ   \n",
             "    10         9                                                        10    3.100000    3.000000     ...     \n",
             "   100        10    51.41201    55.94399     0.1308702    514.1201      10    3.300000    3.000000     ...     \n",
             "    11         8                                                         8                             ...     \n",
             "    12        10    51.64579    49.62289    10.0769041    516.4579       8                             ...     \n",
             "    13        10    59.37018    50.01107    25.1545701    593.7018       7                             ...     \n",
             "    14         7                                                        10    3.000000    3.000000     ...     \n",
             "    15        10    61.41777    57.13898    32.4386027    614.1777       8                             ...     \n",
             "    16         6                                                         9                             ...     \n",
             "    17 ᵇ     ...         ...         ...           ...         ...     ...         ...         ...     ...     \n",
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
             "     1           0    27.48823     755.6028    85.14196         0    0.6666667    0.4444444    2.000000   \n",
             "    10           1    28.98939     840.3846    79.51371         0    0.7378648    0.5444444    2.000000   \n",
             "   100           0    35.07185    1230.0343    98.56704         0    0.6749486    0.4555556    2.000000   \n",
             "    11           2    18.59151     345.6442    52.96194         2    0.7071068    0.5000000    2.000000   \n",
             "    12           0    37.63627    1416.4888    84.95359         2    0.4629100    0.2142857    1.000000   \n",
             "    13           0    28.42161     807.7881    74.30525         3    0.8164966    0.6666667    2.000000   \n",
             "    14           3    30.82519     950.1922    83.47894         0    0.8164966    0.6666667    2.000000   \n",
             "    15           0    22.88167     523.5706    65.92491         2    0.3535534    0.1250000    1.000000   \n",
             "    16           4    33.05198    1092.4333    90.60094         1    0.3333333    0.1111111    1.000000   \n",
             "    17 ᵃ       ...         ...          ...         ...       ...          ...          ...         ...   \n",
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
             "     1           0    27.48823     755.6028    85.14196         0    0.6666667    0.4444444    2.000000   \n",
             "    10           1                                              0    0.7378648    0.5444444    2.000000   \n",
             "   100           0    35.07185    1230.0343    98.56704         0    0.6749486    0.4555556    2.000000   \n",
             "    11           2                                              2                                         \n",
             "    12           0    37.63627    1416.4888    84.95359         2                                         \n",
             "    13           0    28.42161     807.7881    74.30525         3                                         \n",
             "    14           3                                              0    0.8164966    0.6666667    2.000000   \n",
             "    15           0    22.88167     523.5706    65.92491         2                                         \n",
             "    16           4                                              1                                         \n",
             "    17 ᵃ       ...         ...          ...         ...       ...          ...          ...         ...   \n",
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
             "     1       0.9495756    86.09154    33.31021    2.000000    4.000000    0.0000000   \n",
             "    10      10.5287503    90.04246    37.75054    2.000000    4.000000    0.7500000   \n",
             "   100       0.1308702    98.69791    52.24471    2.000000    4.000000    1.0000000   \n",
             "    11       3.5456727    56.50761    22.71871    2.000000    4.000000    1.0000000   \n",
             "    12      10.0769041    95.03049    73.65885    2.000000    3.000000    0.2500000   \n",
             "    13      25.1545701    99.45982    51.26818    2.000000    4.000000    1.0000000   \n",
             "    14      12.7433398    96.22228    44.63180    2.000000    4.000000    1.5000000   \n",
             "    15      32.4386027    98.36351    22.63509    2.000000    3.000000    0.0000000   \n",
             "    16       8.2158066    98.81674    39.67130    3.000000    4.000000    0.0000000   \n",
             "    17 ᵃ           ...         ...         ...         ...         ...          ...   \n",
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
      paste0("\n Data Preview                                                                         \n",
             " ──────────────────────────────────────────────────────────────────────────────────── \n",
             "   ID       V1_Min        V1_Max      V1_IQR      V2_Min      V2_Max      V2_IQR      \n",
             " ──────────────────────────────────────────────────────────────────────────────────── \n",
             "     1       0.9495756    86.09154    33.31021    2.000000    4.000000    0.0000000   \n",
             "    10                                            2.000000    4.000000    0.7500000   \n",
             "   100       0.1308702    98.69791    52.24471    2.000000    4.000000    1.0000000   \n",
             "    11                                                                                \n",
             "    12      10.0769041    95.03049    73.65885                                        \n",
             "    13      25.1545701    99.45982    51.26818                                        \n",
             "    14                                            2.000000    4.000000    1.5000000   \n",
             "    15      32.4386027    98.36351    22.63509                                        \n",
             "    16                                                                                \n",
             "    17 ᵃ           ...         ...         ...         ...         ...          ...   \n",
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

    # ensure that help is shown
    chkRes <- jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), clcMin = TRUE, shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtAggregate(data = dtaInp, varAgg = c("V1", "V2"), grpAgg = c("ID"), drpNA = FALSE,
                                         clcN = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE)$parent$asSource(),
      paste0("jTransform::jtAggregate(\n    data = data,\n    varAgg = vars(V1, V2),\n    grpAgg = ID,\n    clcN = TRUE,\n",
             "    clcMn = TRUE,\n    clcMdn = TRUE,\n    clcMde = TRUE,\n    clcSum = TRUE,\n    drpNA = FALSE)"))

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
