
# This file is automatically generated, you probably don't want to edit this

jtArrangeColsOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtArrangeColsOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            varAll = NULL,
            varOrd = NULL,
            blnAll = FALSE,
            shwHlp = FALSE,
            btnCrt = FALSE,
            jxfLog = FALSE, ...) {

            super$initialize(
                package="jTransform",
                name="jtArrangeCols",
                requiresData=TRUE,
                ...)

            private$..varAll <- jmvcore::OptionVariables$new(
                "varAll",
                varAll,
                hidden=TRUE,
                permitted=list(
                    "numeric",
                    "factor",
                    "id"),
                default=NULL)
            private$..varOrd <- jmvcore::OptionVariables$new(
                "varOrd",
                varOrd,
                permitted=list(
                    "numeric",
                    "factor",
                    "id"),
                default=NULL)
            private$..blnAll <- jmvcore::OptionBool$new(
                "blnAll",
                blnAll,
                default=FALSE)
            private$..shwHlp <- jmvcore::OptionBool$new(
                "shwHlp",
                shwHlp,
                default=FALSE)
            private$..btnCrt <- jmvcore::OptionAction$new(
                "btnCrt",
                btnCrt,
                default=FALSE)
            private$..jxfLog <- jmvcore::OptionBool$new(
                "jxfLog",
                jxfLog,
                hidden=TRUE,
                default=FALSE)

            self$.addOption(private$..varAll)
            self$.addOption(private$..varOrd)
            self$.addOption(private$..blnAll)
            self$.addOption(private$..shwHlp)
            self$.addOption(private$..btnCrt)
            self$.addOption(private$..jxfLog)
        }),
    active = list(
        varAll = function() private$..varAll$value,
        varOrd = function() private$..varOrd$value,
        blnAll = function() private$..blnAll$value,
        shwHlp = function() private$..shwHlp$value,
        btnCrt = function() private$..btnCrt$value,
        jxfLog = function() private$..jxfLog$value),
    private = list(
        ..varAll = NA,
        ..varOrd = NA,
        ..blnAll = NA,
        ..shwHlp = NA,
        ..btnCrt = NA,
        ..jxfLog = NA)
)

jtArrangeColsResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtArrangeColsResults",
    inherit = jmvcore::Group,
    active = list(
        fmtHTM = function() private$.items[["fmtHTM"]],
        genInf = function() private$.items[["genInf"]],
        dtaInf = function() private$.items[["dtaInf"]],
        pvwDta = function() private$.items[["pvwDta"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Change Variable Order")
            self$add(jmvcore::Html$new(
                options=options,
                name="fmtHTM",
                clearWith=list(),
                content="<style> #fmtHTM { display: none; } table { width: 100%; border-collapse: collapse; } th { background-color: #3e6da9; color: white; text-align: left; } tbody > td { border: 1px solid #ddd; padding: 8px; } tbody > tr:nth-child(even) { background-color: #d6eaf8; } tbody > tr:nth-child(odd) { background-color: #ffffff; } tbody > tr:hover { background-color: #aed6f1; } </style>\n"))
            self$add(jmvcore::Html$new(
                options=options,
                name="genInf",
                visible="(shwHlp)",
                clearWith=list(),
                content="Please assign the variables in their desired order to \"Desired Order of Variables\". By ticking \"Add Remaining Variables at the End\", variables that are not contained in \"Desired order of variables\" are appended.\n"))
            self$add(jmvcore::Html$new(
                options=options,
                name="dtaInf",
                clearWith=list(
                    "varOrd",
                    "blnAll",
                    "btnCrt"),
                content=""))
            self$add(jmvcore::Table$new(
                options=options,
                name="pvwDta",
                title="Data Preview",
                refs=list(
                    "jTransform",
                    "jmvReadWrite"),
                clearWith=list(
                    "varOrd",
                    "blnAll"),
                rows=1,
                columns=list(
                    list(
                        `name`="fstCol", 
                        `title`=""))))}))

jtArrangeColsBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtArrangeColsBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jTransform",
                name = "jtArrangeCols",
                version = c(1,0,0),
                options = options,
                results = jtArrangeColsResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Change Variable Order
#'
#' Change the Order of Variables in a Data Set
#'
#' @examples
#' \donttest{
#' # the function is a wrapper for jmvReadWrite::arrange_cols_omv
#' # please use that function when in R (or in Rj)
#' # for more information: https://sjentsch.github.io/jmvReadWrite
#'}
#' @param data .
#' @param varAll .
#' @param varOrd .
#' @param blnAll .
#' @param shwHlp .
#' @param btnCrt .
#' @param jxfLog .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$fmtHTM} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$genInf} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$dtaInf} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$pvwDta} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$pvwDta$asDF}
#'
#' \code{as.data.frame(results$pvwDta)}
#'
#' @export
jtArrangeCols <- function(
    data,
    varAll = NULL,
    varOrd = NULL,
    blnAll = FALSE,
    shwHlp = FALSE,
    btnCrt = FALSE,
    jxfLog = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jtArrangeCols requires jmvcore to be installed (restart may be required)")

    if ( ! missing(varAll)) varAll <- jmvcore::resolveQuo(jmvcore::enquo(varAll))
    if ( ! missing(varOrd)) varOrd <- jmvcore::resolveQuo(jmvcore::enquo(varOrd))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(varAll), varAll, NULL),
            `if`( ! missing(varOrd), varOrd, NULL))


    options <- jtArrangeColsOptions$new(
        varAll = varAll,
        varOrd = varOrd,
        blnAll = blnAll,
        shwHlp = shwHlp,
        btnCrt = btnCrt,
        jxfLog = jxfLog)

    analysis <- jtArrangeColsClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

