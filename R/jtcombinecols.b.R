#' @importFrom jmvcore .
jtCombineColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtCombineColsClass",
    inherit = jtCombineColsBase,
    private = list(
        .crrCmd = "jmvReadWrite::combine_cols_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,
        .sfxTtl = "cmb_cols",

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkEql = function() {
            if (self$options$mdeCmb != "none") return(TRUE)
            dtaFrm <- private$.crrArg(TRUE)$dtaInp
            varPrs <- self$options$varPrs
            notEql <- vapply(varPrs, function(l) any(dtaFrm[, l[[1]]] != dtaFrm[, l[[2]]], na.rm = TRUE), logical(1))
            if (any(notEql)) {
                jmvcore::reject(paste(.("At least some values in the variables of the pair(s) {pairs} are not equal."),
                                      .("Choose a mode of combining to resolve them.")),
                                pairs = paste(vapply(varPrs[notEql], function(l) paste(l, collapse = " - "), character(1)),
                                              collapse = ", "))
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

        .crrArg = function(getDta = TRUE) {
            c(if (getDta) private$.getDta(unique(unlist(lapply(self$options$varPrs, unname)))),
              list(varPrs = lapply(self$options$varPrs, unname), mdeCmb = self$options$mdeCmb))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,
        .runXfm = commonFunc$private_methods$.runXfm

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)

