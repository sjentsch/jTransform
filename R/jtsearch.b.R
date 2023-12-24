jtSearchClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSearchClass",
    inherit = jtSearchBase,
    private = list(

        .run = function() {
            # check whether all required variables are present
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # conduct the search and create an output string
                srcRes <- do.call(jmvReadWrite::search_omv, private$.crrArg())
                # initial line about whether the search term was found
                outRes <- sprintf("<p>Value \"<strong>%s</strong>\" (%s) %s</p>",
                                  trimws(self$options$srcTrm), paste0(rep("partial or ", !self$options$whlTrm), "exact match"),
                                  ifelse(length(srcRes) > 0, "<strong>found</strong> in variable(s): row(s)...", "<strong>not found</strong>"))
                # if it was found, create an output list with the variables and the rows where the value was found
                if (length(srcRes) > 0) {
                    outRes <- paste0(c(outRes, "<ul>",
                                       vapply(names(srcRes), function(x) sprintf("<li><strong>%s</strong>: %s</li>", x, paste0(srcRes[[x]], collapse = ", ")), character(1)),
                                       "</ul>", ""), collapse = "\n")
                }
                # assigng the output string
                self$results$srcRes$setContent(outRes)
            } else {
                # if the search term or the data set is empty, assign
                # a help message
                crtInf(crrInf = self$results$srcRes, hlpMsg = hlpSrc)
            }

        },
         
        .chkVar = function() {
            (nzchar(trimws(self$options$srcTrm)))
        },

        .crrArg = function() {
            c(list(dtaInp = self$data, srcTrm = trimws(self$options$srcTrm)), optSnR(self$options))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::search_omv", private$.crrArg()[-1])
        }

    )
)
