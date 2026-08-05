#' @importFrom jmvcore .
jtSearchClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtSearchClass",
    inherit = jtSearchBase,
    private = list(
        .crrCmd = "jmvReadWrite::search_omv",

        .run = function() {
            # check whether all required variables are present
            if (private$.chkVar() && private$.chkDtF()) {
                # conduct the search and create an output string
                srcRes <- do.call(str2Fn(private$.crrCmd), private$.crrArg(TRUE))
                # initial line about whether the search term was found
                outRes <- jmvcore::format(.("<p>Value \"<strong>{}</strong>\" ({}) {}</p>"),
                                          trimws(self$options$srcTrm),
                                          ifelse(self$options$whlTrm, .("exact match"), .("partial or exact match")),
                                          ifelse(length(srcRes) > 0,
                                                 .("<strong>found</strong> in variable(s): row(s)..."),
                                                 .("<strong>not found</strong>")))
                # if it was found, create an output list with the variables and the rows where the value was found
                if (length(srcRes) > 0) {
                    outRes <- paste0(c(outRes, "<ul>",
                                       vapply(names(srcRes),
                                              function(x) sprintf("<li><strong>%s</strong>: %s</li>", x, paste0(srcRes[[x]], collapse = ", ")),
                                              character(1)),
                                       "</ul>", ""), collapse = "\n")
                }
                # assigng the output string
                self$results$srcRes$setContent(outRes)
            } else {
                # if the search term or the data set is empty, show a help message
                self$results$srcRes$setContent(" ")
            }
        },

        # common functions are in incFnc.R
        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (nzchar(trimws(self$options$srcTrm)))
        },

        .crrArg = function(getDta = TRUE) {
            c(if (getDta) private$.getDta(),
              list(srcTrm = trimws(self$options$srcTrm)),
              optSnR(self$options))
        },

        .getDta = commonFunc$private_methods$.getDta

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
