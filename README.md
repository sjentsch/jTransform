# jTransform: A jamovi Module  for Common Data Management Tasks and Data Set Transformations

<!---
<br clear="all">
--->

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-AGPL%20v3-green.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
[![Last commit](https://img.shields.io/github/last-commit/sjentsch/jTransform?logo=GitHub)](https://github.com/sjentsch/jTransform)
[![Register an issue](https://img.shields.io/github/issues/sjentsch/jTransform?color=%23fa251e&logo=GitHub)](https://github.com/sjentsch/jTransform/issues)
[![R-CMD-check](https://github.com/sjentsch/jTransform/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sjentsch/jTransform/actions/workflows/R-CMD-check.yaml)
[![code-coverage](https://github.com/sjentsch/jTransform/actions/workflows/codecov.yaml/badge.svg)](https://github.com/sjentsch/jTransform/actions/workflows/codecov.yaml)
[![Codecov coverage](https://codecov.io/gh/sjentsch/jTransform/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sjentsch/jTransform?branch=main)
<!-- badges: end -->

<!---
[![Documentation](https://img.shields.io/badge/documentation-is_here-blue)](https://sjentsch.github.io/jmvReadWrite/)
--->

The jamovi module jTransform carries out common data management tasks and
transformations of data sets. It includes functions to change the order of /
re-arrange variables in a data set, transforming a data set from long to wide
or from wide to long, transposing a data set (make rows into columns and
columns into rows), sorting a data set after one or more variable, and adding
columns from one or more additional data sets to the currently opened data set.
In addition, there are functions to search or replace values. Finally, the is
a function to apply transformations for skewed variables trying to make them
better conform to a normal distribution.

<!---
<center>
<img width="300" src="https://gamlj.github.io/commons/pics/ui.png" class="img-responsive" alt="">
</center>
<br>
--->


## Install in jamovi

If you haven't already installed [jamovi](https://www.jamovi.org/download.html),
install it. Open jamovi, and select the jamovi modules library and
[install](https://jamovi.readthedocs.io/en/latest/howto/howto_Install_modules.html)
jTransform from there.

<!---
<center>
<img width="600" src="https://gamlj.github.io/glm/install.png" class="img-responsive" alt="">
</center>
--->


## Install the development version (from source)

Assuming that you have [jamovi](https://www.jamovi.org/download.html) and 
[R](https://cran.r-project.org/) already installed on your machine, open
`R` and install the R-package `jmvtools` using the following command (this
is only required once before the first use):

```
install.packages('jmvtools', repos='https://repo.jamovi.org')
```


Clone this repository (or [download the ZIP file](../../archive/refs/heads/main.zip)
and extract it). In `R`, go to the directory where you cloned or extracted the
repository to, and compile the module with:

```
jmvtools::install()
```

The module then appears among your jamovi modules.


## Giving back

If you find this module helpful, please consider donating to the jamovi project (via
the Patreon-link on the left side).
If you can't give money, but would like to support us in another way, you may contribute
to translating [jamovi](https://hosted.weblate.org/engage/jamovi/), the
[jamovi documentation](https://hosted.weblate.org/engage/jamovidocs/) or the textbook
[”learning statistics with jamovi“](https://hosted.weblate.org/engage/jamovi/) into your
language.

Thank you for your support!
