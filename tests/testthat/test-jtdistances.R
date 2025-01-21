testthat::test_that("jtdistances works", {
    set.seed(1)
    dtaInp <- as.data.frame(matrix(rnorm(11000), nrow = 1000))

    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste("\n Data Preview                                                                                                                     \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  V1             V2           V3           V4           V5           V6           V7           V8           V9           V10     \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "   0.000000      46.224621    45.028427    45.804996    46.111788    45.446521    46.589335    45.412922    44.997035    ... ᵃ   \n",
            "  46.224621       0.000000    45.774905    46.105960    45.152156    44.566792    45.697384    46.578315    46.523526    ...     \n",
            "  45.028427      45.774905     0.000000    46.010844    46.841495    44.308047    47.046431    45.918651    45.771488    ...     \n",
            "  45.804996      46.105960    46.010844     0.000000    45.314419    47.089836    46.023906    44.763312    45.409342    ...     \n",
            "  46.111788      45.152156    46.841495    45.314419     0.000000    44.838212    45.195259    44.790310    44.951256    ...     \n",
            "  45.446521      44.566792    44.308047    47.089836    44.838212     0.000000    45.648287    44.813044    44.870339    ...     \n",
            "  46.589335      45.697384    47.046431    46.023906    45.195259    45.648287     0.000000    45.712716    45.397315    ...     \n",
            "  45.412922      46.578315    45.918651    44.763312    44.790310    44.813044    45.712716     0.000000    43.681232    ...     \n",
            "  44.997035      46.523526    45.771488    45.409342    44.951256    44.870339    45.397315    43.681232     0.000000    ...     \n",
            "  44.109766 ᵇ          ...          ...          ...          ...          ...          ...          ...          ...    ...     \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  ᵃ There are 1 more columns in the data set not shown here. A complete list of variables can be found in \"Variables in the\n",
            "  Output Data Set\" above this table.\n   ᵇ There are 1 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 10))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 1 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 128)

    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "range", nmeDst = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste("\n Data Preview                                                                                                            \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  V1            V2          V3          V4          V5          V6          V7          V8          V9          V10     \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  0.000000      6.742765    6.821824    7.017647    6.973054    7.050439    6.902009    7.048946    6.595200    ... ᵃ   \n",
            "  6.742765      0.000000    6.898371    7.027304    6.790425    6.876753    6.733820    7.191314    6.780684    ...     \n",
            "  6.821824      6.898371    0.000000    7.262175    7.313417    7.088836    7.194054    7.345662    6.937672    ...     \n",
            "  7.017647      7.027304    7.262175    0.000000    7.151847    7.611810    7.114944    7.232759    6.963416    ...     \n",
            "  6.973054      6.790425    7.313417    7.151847    0.000000    7.177456    6.901370    7.168683    6.800809    ...     \n",
            "  7.050439      6.876753    7.088836    7.611810    7.177456    0.000000    7.147810    7.348334    6.970771    ...     \n",
            "  6.902009      6.733820    7.194054    7.114944    6.901370    7.147810    0.000000    7.160061    6.723696    ...     \n",
            "  7.048946      7.191314    7.345662    7.232759    7.168683    7.348334    7.160061    0.000000    6.790970    ...     \n",
            "  6.595200      6.780684    6.937672    6.963416    6.800809    6.970771    6.723696    6.790970    0.000000    ...     \n",
            "  6.724977 ᵇ         ...         ...         ...         ...         ...         ...         ...         ...    ...     \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  ᵃ There are 1 more columns in the data set not shown here. A complete list of variables can be found in\n",
            "  \"Variables in the Output Data Set\" above this table.\n",
            "  ᵇ There are 1 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 10))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 1 more columns in the data set not shown here. A complete list of variables",
                                                  "can be found in \"Variables in the Output Data Set\" above this table."),
                                                  "There are 1 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 119)

    # ensure that help is shown
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid", shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "range", nmeDst = "euclid")$parent$asSource(),
      paste0("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\n        \"V1\",\n        \"V2\",\n        \"V3\",",
             "\n        \"V4\",\n        \"V5\",\n        \"V6\",\n        \"V7\",\n        \"V8\",\n        \"V9\",\n        \"V10\",",
             "\n        \"V11\"),\n    stdDst = \"range\")"))

    # check when chkVar fails (varDst has only one variable)
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp)[1], stdDst = "none", nmeDst = "euclid")
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check help messages
    chkRes <- jTransform::jtDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid", shwHlp = TRUE)
    expect_equal(names(chkRes), c("fmtHTM", "genInf", "dtaInf", "pvwDta"))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE, TRUE, TRUE))
    expect_true(is.character(chkRes$genInf$content))
    expect_true(nzchar(chkRes$genInf$content))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::jtDistances(varDst = names(dtaInp)[1:5], , stdDst = "none", nmeDst = "euclid"),
      regexp = paste("Argument 'varDst' contains 'V1', 'V2', 'V3', 'V4', 'V5' which are not present in the dataset"))

    # additional tests for functions in utils.R
    expect_true(hmeDir() %in% Sys.getenv(c("USERPROFILE", "HOME")))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::distances_omv", crrArg = list(varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid")),
      paste("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\n        \"V1\",\n        \"V2\",\n        \"V3\",\n",
            "       \"V4\",\n        \"V5\",\n        \"V6\",\n        \"V7\",\n        \"V8\",\n        \"V9\",\n        \"V10\",\n",
            "       \"V11\"))"))
    expect_equal(fmtSrc(fcnNme = "jmvReadWrite::distances_omv", crrArg = list(varDst = names(dtaInp)[seq(3)], stdDst = "z", nmeDst = "laplace")),
      paste("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\"V1\", \"V2\", \"V3\"),\n",
            "   stdDst = \"z\",\n    nmeDst = \"laplace\")"))

})
