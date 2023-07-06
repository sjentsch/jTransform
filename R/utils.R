#' @importFrom jmvReadWrite hasPkg

chkFle <- function(fleNme = "") {
    # check whether fleNme contains a directory
    # (otherwise, add HOME / USERPROFILE)
    if (dirname(fleNme) == ".") {
       fleNme <- file.path(Sys.getenv(ifelse(.Platform$OS.type == "unix", "HOME", "USERPROFILE")), fleNme)
    }

    # check existence of the output directory
    # and whether the file has the correct extension
    if (!file.exists(dirname(fleNme))) {
        jmvcore::reject("'{dir}' doesn't exists.", code = '', file = dirname(fleNme)) 
#       jmvcore::reject(.("'{dir}' doesn't exists."), code = '', file = dirname(fleNme))
    }
    if (tools::file_ext(fleNme) != "omv") {
        jmvcore::reject("'{file}' already exists. Change the output file or remove the exisiting file.", code = '', file = basename(fleNme))
#       jmvcore::reject(.("'{file}' already exists. Change the output file or remove the exisiting file."), code = '', file = basename(fleNme))
    }

    # check existence of the output file
    if (file.exists(fleNme)) {
        jmvcore::reject("'{file}' already exists. Change the name of the output file or remove the exisiting file.", code = '', file = basename(fleNme))
#       jmvcore::reject(.("'{file}' already exists. Change the name of the output file or remove the exisiting file."), code = '', file = basename(fleNme))
    }

    fleNme
}
