# ggvanced
An R package for creating advanced multivariable plots such as spider/radar charts and parallel plots. The visualizations are created on top of the `ggplot2` package. The beauty of the `ggplot2` package is the underlying grammar of graphics, allowing for creation of graphs by stacking multiple layers on top of one another. This powerful concept lets us create essentially any visualization, as long as we know how to code it.

The ggvanced package aims to provide a fast way to compare observations across multiple categories at once. To be precise, it contains functions for creation of spider charts and parallel charts. One might think that these can already be obtained using other packages such as fsmb and ggradar for radar charts and ggally for parallel plots. However, none of those gives the ability to simultaneously visualize the range of values for each presented variable.

Examples
ggspider()
The ggspider() function creates spider charts which either a single shared axis scaled to a [0,1] range, or a separate axis with real values displayed for every displayed category. Let’s test the function on a couple of examples. First, we have to format the data so that the first column contains the group identifier, and other columns the descriptory variables. We will use the built-in mtcars and iris datasets.

```
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.2.3
#> Warning: package 'ggplot2' was built under R version 4.2.3
#> Warning: package 'tibble' was built under R version 4.2.3
#> Warning: package 'tidyr' was built under R version 4.2.3
#> Warning: package 'readr' was built under R version 4.2.3
#> Warning: package 'purrr' was built under R version 4.2.3
#> Warning: package 'dplyr' was built under R version 4.2.3
#> Warning: package 'stringr' was built under R version 4.2.3
#> Warning: package 'forcats' was built under R version 4.2.3
#> Warning: package 'lubridate' was built under R version 4.2.3
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

mtcars_summary <- mtcars %>% 
  tibble::rownames_to_column(var = "group") %>%  
  tibble::as_tibble() %>% 
  tail(3)

iris_summary <- iris %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(across(everything(), mean))
library(ggvanced)

ggspider(mtcars_summary)

ggspider(iris_summary)

As mentioned before, we can also make traditional radar charts with a single common scaled axis by specifying the argument scaled = TRUE.

ggspider(iris_summary, scaled = TRUE)

The shape can also be changed from polygon to round by specifying polygon = FALSE.

ggspider(iris_summary, polygon = FALSE)

The other arguments are more aesthetic in nature, and cover aspects such as font size, position of the labels and so on. For mire details, refer to the function documentation.

ggparallel()
Although I prefer spider charts from an aesthetic viewpoint, parallel charts can make it easier to spot trends across variables. This is especially true when there are many variables or observations in the dataset.

ggparallel(mtcars_summary)
#> Joining with `by = join_by(group, parameter)`

ggparallel(iris_summary)
#> Joining with `by = join_by(group, parameter)`
