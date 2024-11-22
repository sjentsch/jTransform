if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("maxRow", "maxCol", "useIdx", "vldExt", "fmtVrI", "fmtAdC", "fmtAdR", "fmtFsC",
                             "hlpL2W", "hlpMrg", "hlpRpl", "hlpSrc", "hlpSrt", "hlpXfV", "hlpXps"))
}

# variable definitions
maxRow <- 10
maxCol <- 10
useIdx <- FALSE
vldExt <- c("omv", "csv", "tsv", "rdata", "rda", "rds", "sav", "zsav", "dta", "sas7bdat", "sd2", "sd7", "xpt", "stx", "stc")

# message formatting for sprintf
fmtAdC <- "There are %d more colums in the data set not shown here. A complete list of variables can be found in \"Variables in the Output Data Set\" above this table."
fmtAdR <- "There are %d more rows in the data set not shown here."
fmtFsC <- "The column%s %s %s shown first in this preview. In the created data set, the variable order is as shown in \"Variables in the Output Data Set\" above this table."

# help messages
hlpMrg <- c(paste("Please assign one or more variables that appear in all data sets (e.g., a participant code) to \"Variable(s) to Match the",
                  "Data Sets by\". Afterwards, either write the name of (one or more) file(s) to be merged under \"Data Set(s) to Add\" (separate",
                  "mulitiple file names with semicolons), or use \"Browse...\" to select input file(s)."),
                  "For a more comprehensive explanation regarding the types of merging operations, see \"Details\" underneath the preview table.")
hlpRpl <- c(paste("Please type the original value and the replacement into the entry fields. If you want to have several pairs of original and",
                  "replacment values, use separate lines. To replace partial matches, unset the tick box \"Whole Word\" (e.g., for orginal: 24 and",
                  "replacement: 34, 241 will be changed into 341)."),
            paste("The <strong>\"Include / Exclude\"</strong> collapse box permits to specifically select in which column types, for which measurement",
                  "type, and in which variables <strong>to replace values</strong>. Ticking the check boxes includes that variable or measurement type.",
                  "When selecting individual variables using the variable input, set the radio button to either only include the selected variables or",
                  "to exclude them."))
hlpSrc <- c(paste("Please type the term to be search for into the text box. If you want that partial matches (i.e., the search term appears within",
                  "values) are found, leave the tick box \"Whole Word\" unset."),
            paste("The <strong>\"Include / Exclude\"</strong> collapse box permits to specifically select in which column and measurement types the",
                  "<strong>search</strong> shall be conducted. Ticking the check boxes includes that variable or measurement type."))
hlpSrt <-   paste("Please assign one or more variables to the variable box \"Variable(s) to be Sorted After\". The order in which the",
                  "variables appear in the variable box determines after which variable is sorted first (one could, e.g., first sort",
                  "after gender and afterwards after age). Variables are sorted in \"Ascending\" order (as default), but you can change",
                  "the order if desired.")
hlpXfV <- c(paste("Please assign at least one variable to at least one of the variable boxes indicating what (approximate) degree (moderate, strong, extreme",
                  "and kind (postive or negative) of skewness this variable has. For moderately skewed variables, a square-root-transformation is used, for",
                  "strongly skewed variables, a logarithic transformation, and for severly skewed variables an inversion. If necessary, a constant is added",
                  "(automatically) in order to avoid the transformation returning NA-values."),
                  "NB: The transformations work only for numeric variables (integer or decimal); please adjust the measure / data type if necessary.")
hlpXps <-   paste("Please assign up to one variable to the variable box \"Column Names for the Output\" (this variable might contain names of trials",
                  "or questionnaire items). If you leave the box empty, generic variable names are generated (\"V_...\"). The variables to become rows",
                  "in your output data set have to be assigned to \"Variables To Be Transposed\".")
