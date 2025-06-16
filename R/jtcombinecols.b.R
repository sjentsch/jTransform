#' @importFrom jmvcore .
jtCombineColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtCombineColsClass",
    inherit = jtCombineColsBase,
    private = list(
        .crrCmd = "jmvReadWrite::combine_cols_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkEql = function() {
            if (self$options$mdeCmb != "none") return(TRUE)
            dtaFrm <- private$.getDta()
            notEql <- vapply(self$options$varPrs,
                             function(l) any(dtaFrm[, l[[1]]] != dtaFrm[, l[[2]]], na.rm = TRUE),
                             logical(1))
            if (any(notEql)) {
                self$results$dtaInf$setContent(jmvcore::format(
                  .("At least <strong>some values</strong> in the variables of the pair(s) {pairs} are <strong>not equal</strong>."),
                  pairs = paste(vapply(self$options$varPrs[notEql], function(l) paste(l, collapse = " - "), character(1)), collapse = ", ")))
                self$results$dtaInf$setVisible(TRUE)
                FALSE
            } else {
                TRUE
            }
        },

        .chkVar = function() {
            (!is.null(self$options$varPrs) &&
             all(vapply(self$options$varPrs, function(l) !any(vapply(l, is.null, logical(1))), logical(1))) &&
             private$.chkEql())
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            list(dtaInp = private$.getDta(), varPrs = lapply(self$options$varPrs, unname), mdeCmb = self$options$mdeCmb)
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,

        .getDta = function() {
            if (!is.null(self$data) && dim(self$data)[1] > 0) {
                self$data
            } else {
                self$readDataset()
            }
        },

        .nteRnC = commonFunc$private_methods$.nteRnC

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)

