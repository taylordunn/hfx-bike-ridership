* Had issues getting `kableExtra` to work with `renv` and Quarto
  * Found this workaround: https://community.rstudio.com/t/problem-rendering-quarto-when-using-renv-and-specific-packages/142859/3
  * `renv::install("htmltools@0.5.2")`
