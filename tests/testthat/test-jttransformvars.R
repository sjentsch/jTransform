testthat::test_that("jttransformvars works", {
    dtaInp <- jmvReadWrite::read_omv("../example4jtTransformVars.omv")

    chkRes <- jTransform::jtTransformVars(data = dtaInp, posSqr = "mdrPos", negSqr = "mdrNeg", posLog = "strPos", negLog = "strNeg",
                                          posInv = "extPos", negInv = "extNeg")

    expect_equal(class(chkRes), c("jtTransformVarsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (12 variables in 1000 rows): mdrPos,\n",
                                                 "mdrNeg, strPos, strNeg, extPos, extNeg, mdrPos_SQR, mdrNeg_SQR,\n",
                                                 "strPos_LOG, strNeg_LOG, extPos_INV, extNeg_INV\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                                                                                 \n",
                                                 "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  mdrPos_SQR     mdrNeg_SQR    strPos_LOG    strNeg_LOG     extPos_INV    extNeg_INV    mdrPos         mdrNeg         strPos        strNeg   \n",
                                                 "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  0.5960302       0.7954903    0.03695995    0.064932425     0.6848681     0.6299437     0.13274893    -0.38705516    0.07789507     ... ᵃ   \n",
                                                 "  0.4874414       0.6344425    0.25624825    0.093557052     0.9378294     0.9673961     0.01509606    -0.15676766    0.79311404     ...     \n",
                                                 "  1.0795391       0.9882611    0.09975020    0.171282615     0.6695497     0.7955302     0.94290170    -0.73091040    0.24726690     ...     \n",
                                                 "  0.6992982       0.3990363    0.08527089    0.309440383     0.9881318     0.3482299     0.26651496     0.08651966    0.20601021     ...     \n",
                                                 "  1.4215091       0.8735289    0.12448288    0.079661644     0.7845417     0.8425423     1.79818506    -0.51730316    0.32099993     ...     \n",
                                                 "  0.4305931       0.6292000    0.11824718    0.009224843     0.7185166     0.5187277    -0.03709257    -0.15014304    0.30201235     ...     \n",
                                                 "  0.5936465       0.7286369    0.06248159    0.196029430     0.6724646     0.9762074     0.12991319    -0.28516213    0.14379841     ...     \n",
                                                 "  0.6206358       0.9489555    0.05597314    0.055638402     0.7510352     0.4283573     0.16268572    -0.65476700    0.12662232     ...     \n",
                                                 "  0.6694677       1.0809277    0.11714687    0.435404305     0.9306023     0.8479010     0.22568398    -0.92265515    0.29869013     ...     \n",
                                                 "  0.6043332 ᵇ           ...           ...            ...           ...           ...            ...            ...           ...     ...     \n",
                                                 "──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  Note. The columns mdrPos_SQR, mdrNeg_SQR, strPos_LOG, strNeg_LOG, extPos_INV, extNeg_INV, mdrPos, mdrNeg, strPos, strNeg, extPos,\n",
                                                 "  extNeg are shown first in this preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\"\n",
                                                 "  above this table.\n",
                                                 "  ᵃ There are 2 more colums in the data set not shown here. A complete list of variables can be found in \"Variables in the Output Data\n",
                                                 "  Set\" above this table.\n   ᵇ There are 990 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "mdrNeg_SQR", "strPos_LOG", "strNeg_LOG", "extPos_INV", "extNeg_INV", "mdrPos", "mdrNeg", "strPos", "strNeg" ))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 2 more colums in the data set not shown here. A complete list of variables can be found in",
                                                  "\"Variables in the Output Data Set\" above this table."),
                                                  "There are 990 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 140)

    chkRes <- jTransform::jtTransformVars(data = dtaInp, posInv = c("mdrPos", "strPos", "extPos"), negInv = c("mdrNeg", "strNeg", "extNeg"))
    expect_equal(class(chkRes), c("jtTransformVarsResults", "Group", "ResultsElement", "R6"))
    expect_equal(chkRes$genInf$asString(), paste("\n Variables in the Output Data Set (12 variables in 1000 rows): mdrPos,\n",
                                                 "strPos, extPos, mdrNeg, strNeg, extNeg, mdrPos_INV, strPos_INV,\n",
                                                 "extPos_INV, mdrNeg_INV, strNeg_INV, extNeg_INV\n\n",
                                                 "Pressing the \"Create\"-button opens the modified data set in a new\n",
                                                 "jamovi window.\n"))
    expect_equal(chkRes$pvwDta$asString(), paste("\n Data Preview                                                                                                                               \n",
                                                 "────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  mdrPos_INV     strPos_INV    extPos_INV    mdrNeg_INV    strNeg_INV    extNeg_INV    mdrPos         strPos        extPos        mdrNeg   \n",
                                                 "────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  0.7378702       0.9184173     0.6848681     0.6124431     0.8611277     0.6299437     0.13274893    0.07789507    0.46024298     ... ᵃ   \n",
                                                 "  0.8080161       0.5543088     0.9378294     0.7130037     0.8062003     0.9673961     0.01509606    0.79311404    0.06639969     ...     \n",
                                                 "  0.4618074       0.7947852     0.6695497     0.5059039     0.6740892     0.7955302     0.94290170    0.24726690    0.49364881     ...     \n",
                                                 "  0.6715836       0.8217299     0.9881318     0.8626416     0.4904103     0.3482299     0.26651496    0.20601021    0.01211847     ...     \n",
                                                 "  0.3310504       0.7507876     0.7845417     0.5671980     0.8324120     0.8425423     1.79818506    0.32099993    0.27473722     ...     \n",
                                                 "  0.8435897       0.7616454     0.7185166     0.7163875     0.9789830     0.5187277    -0.03709257    0.30201235    0.39186407     ...     \n",
                                                 "  0.7394173       0.8660010     0.6724646     0.6532055     0.6367524     0.9762074     0.12991319    0.14379841    0.48717492     ...     \n",
                                                 "  0.7219233       0.8790769     0.7510352     0.5261727     0.8797547     0.4283573     0.16268572    0.12662232    0.33160329     ...     \n",
                                                 "  0.6905186       0.7635775     0.9306023     0.4611685     0.3669405     0.8479010     0.22568398    0.29869013    0.07468059     ...     \n",
                                                 "  0.7324834 ᵇ           ...           ...           ...           ...           ...            ...           ...           ...     ...     \n",
                                                 "────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── \n",
                                                 "  Note. The columns mdrPos_INV, strPos_INV, extPos_INV, mdrNeg_INV, strNeg_INV, extNeg_INV, mdrPos, strPos, extPos, mdrNeg, strNeg,\n",
                                                 "  extNeg are shown first in this preview. In the created data set, the variable order is as shown in \"Variables in the Output Data\n",
                                                 "  Set\" above this table.\n",
                                                 "  ᵃ There are 2 more colums in the data set not shown here. A complete list of variables can be found in \"Variables in the Output Data\n",
                                                 "  Set\" above this table.\n   ᵇ There are 990 more rows in the data set not shown here.\n\n"))
    expect_equal(names(chkRes$pvwDta$columns), c("fstCol", "strPos_INV", "extPos_INV", "mdrNeg_INV", "strNeg_INV", "extNeg_INV", "mdrPos", "strPos", "extPos", "mdrNeg"))
    expect_equal(chkRes$pvwDta$names, c("\"1\"", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    expect_equal(chkRes$pvwDta$rowKeys, c(list("1"), as.list(2:10)))
    expect_equal(chkRes$pvwDta$footnotes, c(paste("There are 2 more colums in the data set not shown here. A complete list of variables can be found in",
                                                  "\"Variables in the Output Data Set\" above this table."),
                                                  "There are 990 more rows in the data set not shown here."))
    expect_equal(chkRes$pvwDta$options$varsRequired, as.list(names(dtaInp)[c(1, 3, 5, 2, 4, 6)]))
    expect_equal(chkRes$pvwDta$rowCount, 10)
    expect_equal(chkRes$pvwDta$rowSelected, 0)
    expect_equal(chkRes$pvwDta$width, 138)

    # check instructions when chkVar fails (transformation variable lists are empty)
    chkRes <- jTransform::jtTransformVars(data = dtaInp, varAll = names(dtaInp))
    expect_equal(names(chkRes), c("genInf", "pvwDta"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign at least one variable to at least one of the variable\n",
                                                 "boxes indicating what (approximate) degree (moderate, strong, extreme\n",
                                                 "and kind (postive or negative) of skewness this variable has. For\n",
                                                 "moderately skewed variables, a square-root-transformation is used, for\n",
                                                 "strongly skewed variables, a logarithic transformation, and for\n",
                                                 "severly skewed variables an inversion. If necessary, a constant is\n",
                                                 "added (automatically) in order to avoid the transformation returning\n",
                                                 "NA-values.\n\n NB: The transformations work only for numeric variables (integer or\n",
                                                 "decimal); please adjust the measure / data type if necessary.\n"))

    chkRes <- jTransform::jtTransformVars()
    expect_equal(names(chkRes), c("genInf", "pvwDta"))
    expect_equal(chkRes$genInf$asString(), paste("\n Please assign at least one variable to at least one of the variable\n",
                                                 "boxes indicating what (approximate) degree (moderate, strong, extreme\n",
                                                 "and kind (postive or negative) of skewness this variable has. For\n",
                                                 "moderately skewed variables, a square-root-transformation is used, for\n",
                                                 "strongly skewed variables, a logarithic transformation, and for\n",
                                                 "severly skewed variables an inversion. If necessary, a constant is\n",
                                                 "added (automatically) in order to avoid the transformation returning\n",
                                                 "NA-values.\n\n NB: The transformations work only for numeric variables (integer or\n",
                                                 "decimal); please adjust the measure / data type if necessary.\n"))
})
