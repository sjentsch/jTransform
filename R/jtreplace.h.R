
# This file is automatically generated, you probably don't want to edit this

jtReplaceOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtReplaceOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            varAll = NULL,
            rplTrm = list(
                list(rplOld="", rplNew="")),
            whlTrm = TRUE,
            shwHlp = FALSE,
            btnCrt = FALSE,
            incCmp = TRUE,
            incRcd = TRUE,
            incID = TRUE,
            incNom = TRUE,
            incOrd = TRUE,
            incNum = TRUE,
            incExc = "include",
            varSel = NULL,
            jxfLog = FALSE, ...) {

            super$initialize(
                package="jTransform",
                name="jtReplace",
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
            private$..rplTrm <- jmvcore::OptionArray$new(
                "rplTrm",
                rplTrm,
                template=jmvcore::OptionGroup$new(
                    "rplTrm",
                    NULL,
                    elements=list(
                        jmvcore::OptionString$new(
                            "rplOld",
                            NULL),
                        jmvcore::OptionString$new(
                            "rplNew",
                            NULL))),
                default=list(
                    list(rplOld="", rplNew="")))
            private$..whlTrm <- jmvcore::OptionBool$new(
                "whlTrm",
                whlTrm,
                default=TRUE)
            private$..shwHlp <- jmvcore::OptionBool$new(
                "shwHlp",
                shwHlp,
                default=FALSE)
            private$..btnCrt <- jmvcore::OptionAction$new(
                "btnCrt",
                btnCrt,
                default=FALSE)
            private$..incCmp <- jmvcore::OptionBool$new(
                "incCmp",
                incCmp,
                default=TRUE)
            private$..incRcd <- jmvcore::OptionBool$new(
                "incRcd",
                incRcd,
                default=TRUE)
            private$..incID <- jmvcore::OptionBool$new(
                "incID",
                incID,
                default=TRUE)
            private$..incNom <- jmvcore::OptionBool$new(
                "incNom",
                incNom,
                default=TRUE)
            private$..incOrd <- jmvcore::OptionBool$new(
                "incOrd",
                incOrd,
                default=TRUE)
            private$..incNum <- jmvcore::OptionBool$new(
                "incNum",
                incNum,
                default=TRUE)
            private$..incExc <- jmvcore::OptionList$new(
                "incExc",
                incExc,
                options=list(
                    "include",
                    "exclude"),
                default="include")
            private$..varSel <- jmvcore::OptionVariables$new(
                "varSel",
                varSel,
                permitted=list(
                    "numeric",
                    "factor",
                    "id"),
                default=NULL)
            private$..jxfLog <- jmvcore::OptionBool$new(
                "jxfLog",
                jxfLog,
                hidden=TRUE,
                default=FALSE)

            self$.addOption(private$..varAll)
            self$.addOption(private$..rplTrm)
            self$.addOption(private$..whlTrm)
            self$.addOption(private$..shwHlp)
            self$.addOption(private$..btnCrt)
            self$.addOption(private$..incCmp)
            self$.addOption(private$..incRcd)
            self$.addOption(private$..incID)
            self$.addOption(private$..incNom)
            self$.addOption(private$..incOrd)
            self$.addOption(private$..incNum)
            self$.addOption(private$..incExc)
            self$.addOption(private$..varSel)
            self$.addOption(private$..jxfLog)
        }),
    active = list(
        varAll = function() private$..varAll$value,
        rplTrm = function() private$..rplTrm$value,
        whlTrm = function() private$..whlTrm$value,
        shwHlp = function() private$..shwHlp$value,
        btnCrt = function() private$..btnCrt$value,
        incCmp = function() private$..incCmp$value,
        incRcd = function() private$..incRcd$value,
        incID = function() private$..incID$value,
        incNom = function() private$..incNom$value,
        incOrd = function() private$..incOrd$value,
        incNum = function() private$..incNum$value,
        incExc = function() private$..incExc$value,
        varSel = function() private$..varSel$value,
        jxfLog = function() private$..jxfLog$value),
    private = list(
        ..varAll = NA,
        ..rplTrm = NA,
        ..whlTrm = NA,
        ..shwHlp = NA,
        ..btnCrt = NA,
        ..incCmp = NA,
        ..incRcd = NA,
        ..incID = NA,
        ..incNom = NA,
        ..incOrd = NA,
        ..incNum = NA,
        ..incExc = NA,
        ..varSel = NA,
        ..jxfLog = NA)
)

jtReplaceResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtReplaceResults",
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
                title="Replace")
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
                content="Please type the original value and the replacement into the entry fields. If you want to have several pairs of original and replacment values, use separate lines. To replace partial matches, unset the tick box \"Whole Word\" (e.g., for orginal: 24 and replacement: 34, 241 will be changed into 341).</p> <p>The <strong>\"Include / Exclude\"</strong> collapse box permits to specifically select in which column types, for which measurement type, and in which variables <strong>to replace values</strong>. Ticking the check boxes includes that variable or measurement type. When selecting individual variables using the variable input, set the radio button to either only include the selected variables or to exclude them (i.e., to exclude values only in the remaining variables).\n"))
            self$add(jmvcore::Html$new(
                options=options,
                name="dtaInf",
                clearWith=list(
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
                    "rplTrm",
                    "whlTrm",
                    "incCmp",
                    "incRcd",
                    "incID",
                    "incNom",
                    "incOrd",
                    "incNum",
                    "incExc",
                    "varSel"),
                rows=1,
                columns=list(
                    list(
                        `name`="fstCol", 
                        `title`=""))))}))

jtReplaceBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jtReplaceBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jTransform",
                name = "jtReplace",
                version = c(1,0,0),
                options = options,
                results = jtReplaceResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Replace
#'
#' Replace Values in the Current Data Set
#'
#' @examples
#' \donttest{
#' # the function is a wrapper for jmvReadWrite::replace_omv
#' # please use that function when in R (or in Rj)
#' # for more information: https://sjentsch.github.io/jmvReadWrite
#'}
#' @param data .
#' @param varAll .
#' @param rplTrm .
#' @param whlTrm .
#' @param shwHlp .
#' @param btnCrt .
#' @param incCmp .
#' @param incRcd .
#' @param incID .
#' @param incNom .
#' @param incOrd .
#' @param incNum .
#' @param incExc .
#' @param varSel .
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
jtReplace <- function(
    data,
    varAll = NULL,
    rplTrm = list(
                list(rplOld="", rplNew="")),
    whlTrm = TRUE,
    shwHlp = FALSE,
    btnCrt = FALSE,
    incCmp = TRUE,
    incRcd = TRUE,
    incID = TRUE,
    incNom = TRUE,
    incOrd = TRUE,
    incNum = TRUE,
    incExc = "include",
    varSel = NULL,
    jxfLog = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jtReplace requires jmvcore to be installed (restart may be required)")

    if ( ! missing(varAll)) varAll <- jmvcore::resolveQuo(jmvcore::enquo(varAll))
    if ( ! missing(varSel)) varSel <- jmvcore::resolveQuo(jmvcore::enquo(varSel))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(varAll), varAll, NULL),
            `if`( ! missing(varSel), varSel, NULL))


    options <- jtReplaceOptions$new(
        varAll = varAll,
        rplTrm = rplTrm,
        whlTrm = whlTrm,
        shwHlp = shwHlp,
        btnCrt = btnCrt,
        incCmp = incCmp,
        incRcd = incRcd,
        incID = incID,
        incNom = incNom,
        incOrd = incOrd,
        incNum = incNum,
        incExc = incExc,
        varSel = varSel,
        jxfLog = jxfLog)

    analysis <- jtReplaceClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

