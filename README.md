Project2McKeonSnyder
================
Owen Snyder
2022-07-05

-   [Introduction](#introduction)
    -   [Our Goal and Purpose](#our-goal-and-purpose)
    -   [Variable Selection](#variable-selection)
    -   [Methods](#methods)
    -   [Packages](#packages)
-   [Read in Data](#read-in-data)
-   [Create New Variable](#create-new-variable)

Render Function

``` r
rmarkdown::render("Project2McKeonSnyder.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                    html_preview = FALSE, toc = TRUE, toc_depth = 2, toc_float = TRUE)
)
```

# Introduction

This is a full report of Project 2 conducted by Ashlee McKeon and Owen
Snyder.

## Our Goal and Purpose

The main goal of this project is to create predictive models based on
data from an [online news popularity
site](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
The data includes about two years worth of information regarding
*Mashable* articles. This data is summarized by a set of 61 categorical
and quantitative variables that will eventually help us predict the
number of shares a *Mashable* article gets. We will only use a subset of
these variables to help predict the shares variable using a variety of
predictive models such as Random Forrest and Boosted Tree models. A main
variable of interest will be the…

## Variable Selection

Because of the large variety of variables this data set includes, we
decided to only consider a subset of the variables to work with.
However, it is important to note one specific set of variables we will
be basing our analysis on. We will be starting with the
\*data_channel_is\_\*\* variables to see if they have any relation to
the number of shares and the rest of the variables we select.

The following our are variables of interest:

-   **shares** - Number of shares (this is our target variable!)
-   **n_tokens_title** - Number of words in the title
-   **average_token_length** - Average length of the words in the
    content
-   **global_rate_positive_words** - Rate of positive words in the
    content
-   **global_rate_negative_words** - Rate of negative words in the
    content

For categorical variables, we will be using the following varibles. In
fact, we will be combining the days of the week into one variable called
**dayOfWeek** and the method of its creation will be described as it
arises.

-   **weekday_is_monday** - Was the article published on a Monday?
-   **weekday_is_tuesday** - Was the article published on a Tuesday?
-   **weekday_is_wednesday** - Was the article published on a Wednesday?
-   **weekday_is_thursday** - Was the article published on a Thursday?
-   **weekday_is_friday** - Was the article published on a Friday?
-   **weekday_is_saturday** - Was the article published on a Saturday?
-   **weekday_is_sunday** - Was the article published on a Sunday?

Finally, the six data channels we will be working with come from these
key variables. Also, note that we have created one variable to represent
each data channel named **dataChannel**. Its method of creation will
also be described below as it arises.

-   **data_channel_is_lifestyle** - Is data channel ‘Lifestyle’?
-   **data_channel_is_entertainment** - Is data channel ‘Entertainment’?
-   **data_channel_is_bus** - Is data channel ‘Business’?
-   **data_channel_is_socmed** - Is data channel ‘Social Media’?
-   **data_channel_is_tech** - Is data channel ‘Tech’?
-   **data_channel_is_world** - Is data channel ‘World’?

## Methods

Before we get started, it is important to briefly describe the methods
wee used for analysis.

-   We will first read in the data set and manipulate the necessary
    variables.

train/test model selection graphs tables etc

## Packages

need to add more + add basic descriptions of them

``` r
library(tidyverse)
library(ggplot2)
```

# Read in Data

``` r
newsData <- read_csv("/Users/owensnyder/Desktop/OnlineNewsPopularity.csv")
```

    ## Rows: 39644 Columns: 61
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_words, n_non_stop_un...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
newsData
```

    ## # A tibble: 39,644 × 61
    ##    url          timedelta n_tokens_title n_tokens_content n_unique_tokens n_non_stop_words n_non_stop_uniq…
    ##    <chr>            <dbl>          <dbl>            <dbl>           <dbl>            <dbl>            <dbl>
    ##  1 http://mash…       731             12              219           0.664             1.00            0.815
    ##  2 http://mash…       731              9              255           0.605             1.00            0.792
    ##  3 http://mash…       731              9              211           0.575             1.00            0.664
    ##  4 http://mash…       731              9              531           0.504             1.00            0.666
    ##  5 http://mash…       731             13             1072           0.416             1.00            0.541
    ##  6 http://mash…       731             10              370           0.560             1.00            0.698
    ##  7 http://mash…       731              8              960           0.418             1.00            0.550
    ##  8 http://mash…       731             12              989           0.434             1.00            0.572
    ##  9 http://mash…       731             11               97           0.670             1.00            0.837
    ## 10 http://mash…       731             10              231           0.636             1.00            0.797
    ## # … with 39,634 more rows, and 54 more variables: num_hrefs <dbl>, num_self_hrefs <dbl>, num_imgs <dbl>,
    ## #   num_videos <dbl>, average_token_length <dbl>, num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>, data_channel_is_socmed <dbl>,
    ## #   data_channel_is_tech <dbl>, data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>,
    ## #   kw_avg_min <dbl>, kw_min_max <dbl>, kw_max_max <dbl>, kw_avg_max <dbl>, kw_min_avg <dbl>,
    ## #   kw_max_avg <dbl>, kw_avg_avg <dbl>, self_reference_min_shares <dbl>, self_reference_max_shares <dbl>,
    ## #   self_reference_avg_sharess <dbl>, weekday_is_monday <dbl>, weekday_is_tuesday <dbl>, …

# Create New Variable

Because we are working with six different channels across six different
columns of data, we figured it would be easier to combine every channel
into one column. That way, we will be working with one single channel
variable that takes on unique values of Lifestyle, Entertainment,
Business, Social Media, Tech, and World. This will make automation and
analysis more clear. Note that the orginal data tells us that a data for
either channel is 1 or 0. meaning that 1 means “yes” and 0 means “no”.

``` r
## use mutate() to create a new channel variable
newsData <- newsData %>% mutate(dataChannel = case_when(
             data_channel_is_lifestyle==1 ~ "Lifestyle",
             data_channel_is_entertainment==1 ~"Entertainment",
             data_channel_is_bus==1 ~ "Business",
             data_channel_is_socmed==1 ~ "Social Media",
             data_channel_is_tech==1 ~ "Tech",
             data_channel_is_world==1 ~ "World"
))
## upon inspection, there are NA values present, this could be due to some articles not having
## one of these specific categories
##is.na(newsData$channel)

## now change the NA values to "Other"
newsData$dataChannel <-  replace_na(newsData$dataChannel, "Other")
##newsData$channel <- as.factor(newsData$channel)
## convert to a factor for analysis??

#newsData

#summary(newsData)
```
