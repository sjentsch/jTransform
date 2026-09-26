#' @importFrom jmvcore .
jtCombineColsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtCombineColsClass",
    inherit = jtCombineColsBase,
    private = list(
        .crrCmd = "jmvReadWrite::combine_cols_omv",
        .nonLtd = FALSE,
        .prsEql = NULL,
        .sfxTtl = "cmb_cols",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = incFnc$private_methods$.init,
        .run  = incFnc$private_methods$.run,

        .chkDtF = incFnc$private_methods$.chkDtF,

        .chkEql = function() {
            if (!is.null(private$.prsEql)) return(private$.prsEql)
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
                private$.prsEql <- TRUE
                TRUE
            }
        },

        .chkVar = function() {
            (!is.null(self$options$varPrs) &&
             all(vapply(self$options$varPrs, function(l) !any(vapply(l, is.null, logical(1))), logical(1))) &&
             private$.chkEql())
        },

        .colFst = incFnc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            varPrs <- lapply(self$options$varPrs, unname)
            if (getDta) {
                dtaInp <- private$.getDta(unique(unlist(varPrs)))$dtaInp
                # update target column order (.xfmCol is first filled in .getDta())
                private$.xfmCol <- c(vapply(varPrs, "[[", character(1), 1),
                                     setdiff(self$options$varAll, unique(unlist(varPrs))))
                list(dtaInp = dtaInp, varPrs = varPrs, mdeCmb = self$options$mdeCmb)
            } else {
                list(varPrs = varPrs, mdeCmb = self$options$mdeCmb)
            }
        },

        .crtMsg = incFnc$private_methods$.crtMsg,
        .dtaInf = incFnc$private_methods$.dtaInf,
        .dtaMsg = incFnc$private_methods$.dtaMsg,
        .getDta = incFnc$private_methods$.getDta,
        .nteRnC = incFnc$private_methods$.nteRnC,
        .runXfm = incFnc$private_methods$.runXfm

    ),

    public = list(

        asSource = incFnc$public_methods$asSource

    )
)
