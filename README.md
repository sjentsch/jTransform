# jTransform

jTransform carries out common data management tasks and transformations of data
sets. It includes functions to change the order of / re-arrange variables in a
data set, transforming a data set from long to wide or from wide to long,
transposing a data set (make rows into columns and columns into rows), sorting
a data set after one or more variable, and adding columns from one or more
additional data sets to the currently opened data set.

<!---
<center>
<img width="300" src="https://gamlj.github.io/commons/pics/ui.png" class="img-responsive" alt="">
</center>
<br>
--->


## Install in jamovi

Please install [jamovi](https://www.jamovi.org/download.html) and open it.
Select the jamovi modules library and install jTransform from there.

<!---
<center>
<img width="600" src="https://gamlj.github.io/glm/install.png" class="img-responsive" alt="">
</center>
--->


## Install the development version (from source)

Please install [jamovi](https://www.jamovi.org/download.html). Then install and
open [R](https://cloud.r-project.org/) and install the R-package `jmvtools`
using the following commands (this needs to be done only once with the first
use):

```

```


Clone this repository (or download the ZIP file and extract it). In `R`, go to
the place to where you cloned or extracted it to, and compile the module with:

```
jmvtools::install()
```
