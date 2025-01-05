testthat::test_that("descdistances works", {
    set.seed(1)
    dtaInp <- as.data.frame(matrix(rnorm(11000), nrow = 1000))

    chkRes <- jTransform::descDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste("\n Distances / Proximities                                                                                                                         \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  V1           V2           V3           V4           V5           V6           V7           V8           V9           V10          V11         \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "   0.000000    46.224621    45.028427    45.804996    46.111788    45.446521    46.589335    45.412922    44.997035    44.109766    43.866874   \n",
            "  46.224621     0.000000    45.774905    46.105960    45.152156    44.566792    45.697384    46.578315    46.523526    44.330496    44.748969   \n",
            "  45.028427    45.774905     0.000000    46.010844    46.841495    44.308047    47.046431    45.918651    45.771488    46.069086    43.898556   \n",
            "  45.804996    46.105960    46.010844     0.000000    45.314419    47.089836    46.023906    44.763312    45.409342    45.366272    44.334528   \n",
            "  46.111788    45.152156    46.841495    45.314419     0.000000    44.838212    45.195259    44.790310    44.951256    44.048358    43.208235   \n",
            "  45.446521    44.566792    44.308047    47.089836    44.838212     0.000000    45.648287    44.813044    44.870339    44.134645    42.705530   \n",
            "  46.589335    45.697384    47.046431    46.023906    45.195259    45.648287     0.000000    45.712716    45.397315    44.862239    43.261355   \n",
            "  45.412922    46.578315    45.918651    44.763312    44.790310    44.813044    45.712716     0.000000    43.681232    43.270825    44.013232   \n",
            "  44.997035    46.523526    45.771488    45.409342    44.951256    44.870339    45.397315    43.681232     0.000000    45.376690    42.661402   \n",
            "  44.109766    44.330496    46.069086    45.366272    44.048358    44.134645    44.862239    43.270825    45.376690     0.000000    43.366342   \n",
            "  43.866874    44.748969    43.898556    44.334528    43.208235    42.705530    43.261355    44.013232    42.661402    43.366342     0.000000   \n",
            "─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 11))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:11)))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 11)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 143)

    chkRes <- jTransform::descDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "range", nmeDst = "euclid")
    expect_equal(class(chkRes), c("jtDistancesResults", "Group", "ResultsElement", "R6"))
    expect_false(chkRes$genInf$visible)
    expect_equal(chkRes$dtaInf$asString(), paste("\n Variables in the Output Data Set (11 variables in 11 rows): V1, V2,\n",
                                                 "V3, V4, V5, V6, V7, V8, V9, V10, V11\n"))
    expect_equal(chkRes$pvwDta$asString(),
      paste("\n Distances / Proximities                                                                                                              \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  V1          V2          V3          V4          V5          V6          V7          V8          V9          V10         V11        \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
            "  0.000000    6.742765    6.821824    7.017647    6.973054    7.050439    6.902009    7.048946    6.595200    6.724977    6.291876   \n",
            "  6.742765    0.000000    6.898371    7.027304    6.790425    6.876753    6.733820    7.191314    6.780684    6.721398    6.378775   \n",
            "  6.821824    6.898371    0.000000    7.262175    7.313417    7.088836    7.194054    7.345662    6.937672    7.252250    6.533298   \n",
            "  7.017647    7.027304    7.262175    0.000000    7.151847    7.611810    7.114944    7.232759    6.963416    7.218468    6.682771   \n",
            "  6.973054    6.790425    7.313417    7.151847    0.000000    7.177456    6.901370    7.168683    6.800809    6.932780    6.411317   \n",
            "  7.050439    6.876753    7.088836    7.611810    7.177456    0.000000    7.147810    7.348334    6.970771    7.126079    6.523711   \n",
            "  6.902009    6.733820    7.194054    7.114944    6.901370    7.147810    0.000000    7.160061    6.723696    6.906657    6.278587   \n",
            "  7.048946    7.191314    7.345662    7.232759    7.168683    7.348334    7.160061    0.000000    6.790970    6.984431    6.727196   \n",
            "  6.595200    6.780684    6.937672    6.963416    6.800809    6.970771    6.723696    6.790970    0.000000    6.923834    6.108917   \n",
            "  6.724977    6.721398    7.252250    7.218468    6.932780    7.126079    6.906657    6.984431    6.923834    0.000000    6.492392   \n",
            "  6.291876    6.378775    6.533298    6.682771    6.411317    6.523711    6.278587    6.727196    6.108917    6.492392    0.000000   \n",
            "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", paste0("V", seq(2, 11))))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:11)))
    expect_equal(chkRes$pvwDta$footnotes, character(0))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 11)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 132)

    # ensure that help is shown
    chkRes <- jTransform::descDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid", shwHlp = TRUE)
    expect_true(chkRes$genInf$visible)

    # check asSource
    expect_equal(jTransform::descDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "range", nmeDst = "euclid")$parent$asSource(),
      paste0("jmvReadWrite::distances_omv(\n    dtaInp = data,\n    varDst = c(\n        \"V1\",\n        \"V2\",\n        \"V3\",\n        \"V4\",",
             "\n        \"V5\",\n        \"V6\",\n        \"V7\",\n        \"V8\",\n        \"V9\",\n        \"V10\",\n        \"V11\"),",
             "\n    stdDst = \"range\")"))

    # check when chkVar fails (varDst has only one variable)
    chkRes <- jTransform::descDistances(data = dtaInp, varDst = names(dtaInp)[1], stdDst = "none", nmeDst = "euclid")
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta"))
    expect_equal(chkRes$pvwDta$asDF, data.frame(fstCol = NA, row.names = "1"))
    expect_equal(chkRes$dtaInf$content, "")

    # check help messages
    chkRes <- jTransform::descDistances(data = dtaInp, varDst = names(dtaInp), stdDst = "none", nmeDst = "euclid", shwHlp = TRUE)
    expect_equal(names(chkRes), c("genInf", "dtaInf", "pvwDta"))
    expect_equal(vapply(names(chkRes), function(N) chkRes[[N]]$visible, logical(1), USE.NAMES = FALSE), c(TRUE, TRUE, TRUE))
    expect_true(is.character(chkRes$genInf$content))
    expect_true(nzchar(chkRes$genInf$content))

    # ensure that an error is thrown if no data are submitted
    expect_error(jTransform::descDistances(varDst = names(dtaInp)[1:5], , stdDst = "none", nmeDst = "euclid"),
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
