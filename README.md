All.Links
================
Owen Snyder
2022-07-10

Render Function

``` r
rmarkdown::render("FinalREADME.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  params = list(dataChannel = "Lifestyle"),
                  output_options = list(
                    html_preview = FALSE, toc = TRUE, toc_depth = 2, toc_float = TRUE)
)
```
