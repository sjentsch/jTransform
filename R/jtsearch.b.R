#' @importFrom jmvcore .
jtSearchClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSearchClass",
    inherit = jtSearchBase,
    private = list(
        .crrCmd = "jmvReadWrite::search_omv",

        .run = function() {
            # check whether all required variables are present
            if (private$.chkVar()) {
                # conduct the search and create an output string
                srcRes <- do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                # initial line about whether the search term was found
                outRes <- sprintf(paste0("<p>", .("Value"), " \"<strong>%s</strong>\" (%s) %s</p>"),
                                  trimws(self$options$srcTrm), paste0(rep(.("partial or "), !self$options$whlTrm), .("exact match")),
                                  ifelse(length(srcRes) > 0, .("<strong>found</strong> in variable(s): row(s)..."), .("<strong>not found</strong>")))
                # if it was found, create an output list with the variables and the rows where the value was found
                if (length(srcRes) > 0) {
                    outRes <- paste0(c(outRes, "<ul>",
                                       vapply(names(srcRes), function(x) sprintf("<li><strong>%s</strong>: %s</li>", x, paste0(srcRes[[x]], collapse = ", ")), character(1)),
                                       "</ul>", ""), collapse = "\n")
                }
                # assigng the output string
                self$results$srcRes$setContent(outRes)
            } else {
                # if the search term or the data set is empty, show a help message
                self$results$srcRes$setContent(" ")
            }
        },

        .chkVar = function() {
            (nzchar(trimws(self$options$srcTrm)) && dim(self$readDataset())[1] >= 1)
        },

        .crrArg = function() {
            c(list(dtaInp = self$readDataset(), srcTrm = trimws(self$options$srcTrm)), optSnR(self$options))
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc(private$.crrCmd, private$.crrArg()[-1])
        }

    )
)
