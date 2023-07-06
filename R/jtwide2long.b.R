jtWide2LongClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtWide2LongClass",
    inherit = jtWide2LongBase,
    private = list(
        .run = function() {
            # if not blnOut -> exit
            if (!self$options$blnOut) {
                print("jtwide2long - no blnOut")        
                return()
            }

            print("jtwide2long")
            print(self$options$blnOut)
            print(self$options$fleOut)
            print(self$options$chcEnI)
            print(self$options$varEnI)
            print(self$options$varID)
            print(self$options$varOrd)
            print(self$options$varSep)
            print(self$options$varTme)

            # check whether there is a directory
            # check existence of the output directory
            # check existence of the output file
            # assmeble and run jmvReadWrite command
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
