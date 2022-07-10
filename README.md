Project 2 render&automation code
================
Owen Snyder
2022-07-10

-   [Description of Repo](#description-of-repo)
-   [Render Function](#render-function)
-   [Automation Code](#automation-code)
-   [Outputs of Analysis](#outputs-of-analysis)

## Description of Repo

The main purpose of this repo is to house all of our automated reports
for each channel of data that comes from the online news popularity data
set from UCI. This data is summarized by a set of 61 categorical and
quantitative variables that will eventually help us predict the number
of shares a *Mashable* article gets. We will only use a subset of these
variables to help predict the shares variable using a variety of
predictive models such as Linear models, a Random Forest, and a Boosted
Tree model. Before the models are fitted, we will split the data into a
training and test set (70/30 split) and there will be an exploratory
data analysis conducted. Once the models are fitted, we will use the
RMSE metric to determine which model has the lowest RMSE and thus
declared the winner for that data channel. A more in-depth description
of variables, models, etc. can be found by visiting each link below.

## Render Function

``` r
rmarkdown::render("FinalREADME.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  params = list(dataChannel = "Lifestyle"),
                  output_options = list(
                    html_preview = FALSE, toc = TRUE, toc_depth = 2, toc_float = TRUE)
)
```

## Automation Code

``` r
## create dataChannel object to store all channels
dataChannel <- c("Business", "Entertainment", "Lifestyle", "Social Media", "Tech", "World")
## use lapply()
params <- lapply(dataChannel, FUN= function(x){list(dataChannel=x)})
## creat output file names
output_file <-  paste0(dataChannel, ".md")
## put into a tibble
reports <- tibble(output_file, params)
## now apply to all
apply(reports, MARGIN = 1,
      FUN=function(x){  
  rmarkdown::render("Project2McKeonSnyder.Rmd",
                     output_format = "github_document",
                     output_file = x[[1]],
                     params = x[[2]])})
```

## Outputs of Analysis

The analysis for [Business articles is available
here](https://github.com/abmckeon/Project2McKeonSnyder/blob/main/Business.html).  
The analysis for [Entertainment articles is available
here](https://github.com/abmckeon/Project2McKeonSnyder/blob/main/Entertainment.html)
