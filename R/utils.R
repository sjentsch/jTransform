hmeDir <- function() {
    Sys.getenv(ifelse(.Platform$OS.type == "unix", "HOME", "USERPROFILE"))
}

chkFle <- function(fleNme = "") {
    # check whether fleNme contains a directory
    # (otherwise, add HOME / USERPROFILE)
    if (dirname(fleNme) == ".") {
       fleNme <- file.path(hmeDir(), fleNme)
    }

    # check existence of the output directory
    # and whether the file has the correct extension
    if (!file.exists(dirname(fleNme))) {
#       jmvcore::reject(.("'{dir}' doesn't exists."), dir = dirname(fleNme))
        jmvcore::reject("'{dir}' doesn't exists.", dir = dirname(fleNme))
    }
    if (tools::file_ext(fleNme) != "omv") {
#       jmvcore::reject(.("'{file}' doesn't have the correct file extension (.omv)."), file = basename(fleNme))
        jmvcore::reject("'{file}' doesn't have the correct file extension (.omv).", file = basename(fleNme))
    }
    # check existence of the output file
    if (file.exists(fleNme)) {
#       jmvcore::reject(.("'{file}' already exists. Change the name of the output file or remove the exisiting file."), file = basename(fleNme))
        jmvcore::reject("'{file}' already exists. Change the name of the output file or remove the exisiting file.", file = basename(fleNme))
    }

    fleNme
}
