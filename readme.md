# dnra: do not re-assign

The `dnra` package replaces ad hoc code like

```r
# complex_object <- long_computation(variables)
# saveRDS(complex_object, "complex_object.RDS")
complex_object = readRDS("complex_object.RDS")
```

with

```r
complex_object %<~% long_computation(variables)
```

avoiding breaking the flow of your script while avoiding recomputing values.

We start by loading the package and setting a save folder:

```r
library(drna)
set_save_folder("cache")
```

Working interactively, we can delete cached objects with `.delete()` and rename
them with `.rename()`.

## Installation

Can be installed with

```r
remotes::install_github("KiwiMateo/dnra")
```
