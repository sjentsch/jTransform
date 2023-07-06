jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(

        .run = function() {
            # if not blnOut -> exit
            if (!self$options$blnOut) {
                print("jttranspose - no blnOut")        
                return()
            }

            print("jttranspose")
            print(self$options$blnOut)
            print(self$options$fleOut)
            print(self$options$varCol)

            # check whether there is a directory
            # check existence of the output directory
            # check existence of the output file
            # assmeble and run jmvReadWrite command
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
