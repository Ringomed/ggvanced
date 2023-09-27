# ggvanced

![prettty_spider](https://github.com/Ringomed/ggvanced/assets/60142390/e4f65828-dc98-4abc-8383-925e17614fd8)


An R package for creating advanced multivariable plots such as spider/radar charts and parallel plots. The visualizations are created on top of the `ggplot2` package. The beauty of the `ggplot2` package is the underlying grammar of graphics, allowing for creation of graphs by stacking multiple layers on top of one another. This powerful concept lets us create essentially any visualization, as long as we know how to code it.

The `ggvanced` package aims to provide a fast way to compare observations across multiple categories at once. To be precise, it contains functions for creation of spider charts and parallel charts. One might think that these can already be obtained using other packages such as `fsmb` and `ggradar` for radar charts and `ggally` for parallel plots. However, none of those gives the ability to simultaneously visualize the range of values for each presented variable.

The package can be installed using the `devtools::install_github("Ringomed/ggvanced")` command in R and calling `library(ggvanced)` afterwards. There is also a [post on Medium](https://medium.com/towards-data-science/ggvanced-a-package-for-generating-advanced-data-visualizations-on-top-of-the-ggplot2-package-2a935763a4b) showcasing the package functions if you prefer that outlet.

## Examples

### `ggspider()` 

The `ggspider()` function creates spider charts which either a single shared axis scaled to a [0,1] range, or a separate axis with real values displayed for every displayed category. Let's test the function on a couple of examples. First, we have to format the data so that the first column contains the group identifier, and other columns the descriptory variables. We will use the built-in `mtcars` and `iris` datasets.

```{r}
library(tidyverse)

mtcars_summary <- mtcars %>% 
  tibble::rownames_to_column(var = "group") %>%  
  tibble::as_tibble() %>% 
  tail(3)

iris_summary <- iris %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(across(everything(), mean))
```

```{r}
library(ggvanced)

ggspider(mtcars_summary)
ggspider(iris_summary)
```

![image](https://github.com/Ringomed/ggvanced/assets/60142390/ed0938de-4f31-4870-8a5d-b6da26f5e0fe)

```{r}
ggspider(iris_summary)
```

![image](https://github.com/Ringomed/ggvanced/assets/60142390/c9b64e16-32e9-4086-8b03-520c215196a0)


As mentioned before, we can also make traditional radar charts with a single common scaled axis by specifying the argument `scaled = TRUE`.

```{r}
ggspider(iris_summary, scaled = TRUE)
```
![image](https://github.com/Ringomed/ggvanced/assets/60142390/ee90eaa7-964d-46f4-970f-3cd583926ceb)

The shape can also be changed from polygon to round by specifying `polygon = FALSE`.

```{r}
ggspider(iris_summary, polygon = FALSE)
```
![image](https://github.com/Ringomed/ggvanced/assets/60142390/96069519-c062-4e8a-876a-98e082927a15)


The other arguments are more aesthetic in nature, and cover aspects such as font size, position of the labels and so on. For mire details, refer to the function documentation.

### `ggparallel()`

Although I prefer spider charts from an aesthetic viewpoint, parallel charts can make it easier to spot trends across variables. This is especially true when there are many variables or observations in the dataset.

```{r}
ggparallel(mtcars_summary)
```
![image](https://github.com/Ringomed/ggvanced/assets/60142390/7961a8bb-6344-41d6-9d63-610d05eef0b6)


```{r}
ggparallel(iris_summary)
```
![image](https://github.com/Ringomed/ggvanced/assets/60142390/86439275-edb4-4070-bbb5-e5450ec2d690)

### Making charts prettier

The above charts are just barebone version. Of course, they can be “pimped up” just like any other ggplot2 chart. Below is an example of a ggvanced spider chart after a couple of alterations.

```{r}
library(sysfonts)
library(showtext)

sysfonts::font_add_google("Roboto Condensed")
showtext_auto()

mtcars_gr <- mtcars %>%
  tibble::rownames_to_column(var = "group") %>%
  tibble::as_tibble() %>%
  tail(3) %>%
  rename("Miles per Gallon" = mpg, "Cylinders" = cyl,
         "Displacement" = disp, "Horsepower" = hp,
         "Rear axle\n ratio" = drat, "Weight" = wt) %>%
  dplyr::select(1:7)

ggspider(mtcars_gr, axis_name_offset = 0.15, background_color = "beige", fill_opacity = 0.15) +
  labs(col = "Car name", title = "Comparing Car Properties") +
  theme(plot.title = element_text(hjust = 0.475, face = "bold"),
        legend.title = element_text(face = "bold"),
        text = element_text(family = "Roboto Condensed", face = "bold"))
```

![prettty_spider](https://github.com/Ringomed/ggvanced/assets/60142390/e4f65828-dc98-4abc-8383-925e17614fd8)

### Other examples

Spotify Top Danceability:
The code and notebook with additional context can be found at https://app.datacamp.com/workspace/w/693b78f1-5293-451e-a26e-ea5806b03b77/edit.
![image](https://github.com/Ringomed/ggvanced/assets/60142390/b1719d05-0c98-48f0-9f51-ae5ef3967bb8)



