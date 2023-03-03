# Geography of Economic Mobility and Air Pollution

This project visualizes the geographic relationship between intergenerational economic mobility
and average measures of fine particulate matter (PM 2.5) across the contiguous United States.
Check out the [full report here](https://dmolitor.com/air-pollution).

## Dependencies

### R

This project depends on [R >= 4.2.2](https://cran.r-project.org/index.html) and manages all
package dependencies via [`renv`](https://rstudio.github.io/renv/articles/renv.html).
To install required packages, clone this repository, make `air-pollution/` your working
directory, and execute the following in an interactive R session:
```r
install.packages("renv")
renv::activate()
renv::restore()
```

### Quarto

This project also depends on [Quarto >= 1.2.335](https://quarto.org/docs/get-started/).

## Replicate

To replicate the visualization and all corresponding text and supplementary materials, make
`air-pollution/` your working directory and execute the following at the command line:
```
quarto render
```
The resulting files will be rendered in the `docs` directory.
