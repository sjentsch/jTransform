# variable definitions
maxRow <- 10
maxCol <- 10

# message formatting for sprintf
fmtVrI <- "<strong>Variables in the Output Dataset:</strong> %s"
fmtAdC <- "There are %d more colums in the dataset not shown here. A complete list of variables can be found in “Variables in the Output Dataset” above this table."
fmtAdR <- "There are %d more rows in the dataset not shown here."
fmtFsC <- "The column%s %s %s shown first in this preview. In the created data set the variable order is as shown in “Variables in the Output Dataset” above this table."

# help messages
hlpArC =   paste("Please assign the variables in their desired order to “Desired order of variables”. By setting “Include all variables”,",
                 "the remaining variables (i.e., those not contained in “Desired order of variables”) are appended.")
hlpL2W = c()
hlpMrg =   paste("Please assign one or more variables that appear in all data sets (e.g., a participant code) to “Variable(s) to match",
                 "the data sets by”. Afterwards, either enter a name of a file to be merged under ”Data set(s) to add“ or use the"
                 "“Browse”-button to select input file(s). For details about file selection and the type of merging operation, see"
                 "Details underneath the preview table.")
hlpRpl = c(paste("Please type the original value and the replacement into the text box. Original and replacement should be separated by",
                 "a comma. If you want to have several pairs of original and to replacment values, use separate lines. If you want to",
                 "replace partial matches, unset the tick box “whole word” (e.g., for orginal: 24 and replacement: 34, 241 willbe changed",
                 "into 341)."),
           paste("The “Include / exclude” collapse box permits to specifically select in which column types, for which measurement type,",
                 "and in which variables the search shall be conducted."))
hlpSrc = c(paste("Please type the term to be search for into the text box. If you want that partial matches (i.e., the search term appears",
                 "within values) are found, leave the tick box “whole word” unset."),
           paste("The “Include / exclude” collapse box permits to specifically select in which column types and for which measurement",
                 "type the search shall be conducted."))
hlpSrt =   paste("Please assign one or more variables to the variable box “Variable(s) to be sorted after”. The order in which the",
                 "variables appear in the variable box determines after which variable is sorted first (one could, e.g., first sort",
                 "after gender and afterwards after age). Variables are sorted in ascending order (as default), but you can change"
                 "the order if desired.")
hlpXps =   paste("Please assign maximally one variable to the variable box “Variable with column names for the output” (this variable",
                 "might contain names of trials or questionnaire items). If you leave the box empty, generic variable names are generated",
                 "(“V_...”). The variables to be transposed (i.e., those to become rows in your output data set) have to be assigned to",
                 "“Variables to be transposed”.")
hlpW2L = c()

hlpCrt = "Once you are satisfied with with the preview, click on “CREATE” to open the modified dataset in a new jamovi window."
