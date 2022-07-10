Project2McKeonSnyder
================
Owen Snyder and Ashlee McKeon
2022-07-05

Render Function

``` r
rmarkdown::render("Project2McKeonSnyder.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  params = list(dataChannel = "Lifestyle"),
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
-   **num_imgs** - Number of images
-   **num_videos** - Number of videos
-   **num_hrefs** - Number of links

For categorical variables, we will be using the following variables. In
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
-   The data will be separated into a training and test set. This will
    be a 70/30 split.
-   We will include some contingency tables at each setting of our
    categorical variables to get a better idea of the data at hand. We
    will also include summary statistics for quantitative variables. The
    data for these summaries will be used via the *training* data.
-   We will include a variety of graphs to better visualize our data.
    These graphs will be described in a more subjective manner so that
    automation of each setting of this report makes more sense. We will
    still be using the *training* data.
-   Finally, we will be using machine learning methods via Random
    Forrest Boosted Tree models. These models will be explained in
    greater detail in their respective section.

## Packages

Below is a list of necessary packages that will aid our data analysis
and modeling.

-   `tidyverse` is a collection of useful packages designed for data
    science.
-   `ggplot2` is an amazing way to create visually pleasing and
    informative graphics.
-   `caret` is a set of functions that streamline the process of
    implementing machine learning methods.
-   `rmarkdown` is a useful package for creating R Markdown documents in
    a variety of formats.
-   `knitr` is a useful package to integrate computing and reporting.

``` r
library(tidyverse)
library(ggplot2)
library(caret)
library(rmarkdown)
library(knitr)
```

``` r
## just eval=FALSE-ing this so i can commit and check github pages
channel <- c("Business", "Entertainment", "Lifestyle", "Social Media", "Tech", "World")

params <- lapply(channel, FUN= function(x){list(channel=x)})

output_file <-  paste0(channel, ".md")

#params <- lapply(channel, FUN= function(x){list(channel=x)})

reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN=function(x){  
  rmarkdown::render("Project2McKeonSnyder.Rmd",
                     output_format = "github_document",
                     output_file = x[[1]],
                     params = x[[2]])})
```

# Read in Data

``` r
setwd("~/Desktop")
newsData <- read_csv("OnlineNewsPopularity.csv")
newsData
## need to change working dir
```

# Create New Variables

Because we are working with six different channels across six different
columns of data, we figured it would be easier to combine every channel
into one column. That way, we will be working with one single channel
variable that takes on unique values of Lifestyle, Entertainment,
Business, Social Media, Tech, and World. This will make automation and
analysis more clear. Note that the original data tells us that a data
for either channel is 1 or 0. meaning that 1 means “yes” and 0 means
“no”.

``` r
## use mutate() to create a new channel variable
newsData <- newsData %>% mutate(dataChannel = 
                              ifelse(data_channel_is_bus==1, "Business",
                              ifelse(data_channel_is_entertainment==1, "Entertainment",
                              ifelse(data_channel_is_lifestyle==1, "Lifestyle",
                              ifelse(data_channel_is_socmed==1, "Social Media",
                              ifelse(data_channel_is_tech==1, "Tech",
                              ifelse(data_channel_is_world==1, "World", "Other"))))))) %>%
                      mutate(dayOfWeek = 
                               ifelse(weekday_is_monday==1, "Monday",
                               ifelse(weekday_is_tuesday==1, "Tuesday",
                               ifelse(weekday_is_wednesday==1, "Wednesday",
                               ifelse(weekday_is_thursday==1, "Thursday",
                               ifelse(weekday_is_friday==1, "Friday",
                               ifelse(weekday_is_saturday==1, "Saturday",
                               ifelse(weekday_is_sunday==1, "Sunday", "NULL")))))))) %>%
                      mutate(weekdayWeekend = 
                               ifelse(weekday_is_monday==1, "Weekday",
                               ifelse(weekday_is_tuesday==1, "Weekday",
                               ifelse(weekday_is_wednesday==1, "Weekday",
                               ifelse(weekday_is_thursday==1, "Weekday",
                               ifelse(weekday_is_friday==1, "Weekday",
                               ifelse(weekday_is_saturday==1, "Weekend",
                               ifelse(weekday_is_sunday==1, "Weekend", "NULL"))))))))

## upon inspection, there are NA values present, this could be due to some articles not having
## one of these specific categories

#factor(newsData$dataChannel)

## convert to a factor for analysis??

#newsData

#summary(newsData)
```

## Filter Data by Channel

``` r
newsData.busn <- newsData %>% filter(dataChannel=="Business")
newsData.ent <- newsData %>% filter(dataChannel=="Entertainment")
newsData.life <- newsData %>% filter(dataChannel=="Lifestyle")
newsData.socmed <- newsData %>% filter(dataChannel=="Social Media")
newsData.tech <- newsData %>% filter(dataChannel=="Tech")
newsData.wrld <- newsData %>% filter(dataChannel=="World")


channel.data <- newsData %>% filter(dataChannel == params$dataChannel)
```

# Split Data: Train/Test Set

In this section we will split our newsData into a training and test set.
The training set will be used for model fitting and EDA while the test
set will be used for predictions (verify). This will be a 70/30 split.

NOTE: this will have to be re-ran due to the addition of the dayOfWeek
variable. process will be the same but will to re-run all data chunks.

NOTE: NEED TO REMOVE VARIABLES!! i.e the variables that are based on new
variables need to be deleted from data set.

``` r
set.seed(558) ## set seed for reproducibility
#trainIndex <- createDataPartition(newsData$shares, p = 0.70, list = FALSE)
#newsTrain <- newsData[trainIndex, ]
#newsTest <- newsData[-trainIndex, ]

trainIndex <- createDataPartition(channel.data$shares, p = 0.70, list = FALSE)
newsTrain <- channel.data[trainIndex, ]
newsTest <- channel.data[-trainIndex, ]
```

# Numerical Summaries

This section is dedicated to creating basic summary statistics for our
response variables, **shares**. Before performing more in depth analysis
and modeling, it is important to get a better idea of how our data is
distributed. For example, one should take note of means, median, and
standard deviations. Another aspect to note is that if the median is
greater than the mean then our data is likely skewed to the left.
Conversely, if the median is less than the mean, the data is likely
skewed to the right.

``` r
newsTrain %>% summarise(avgShares = mean(shares), medianShares = median(shares), sdShares = sd(shares))
```

# Contingency Tables

First, we may want to look at the frequency of all data channels in the
newsData dataset.

``` r
oneWayDataChannel <- table(newsData$dataChannel)
oneWayDataChannel 
```

    ## 
    ##      Business Entertainment     Lifestyle         Other  Social Media          Tech         World 
    ##          6258          7057          2099          6134          2323          7346          8427

We can see from the table that Tech and World are the most common, while
Lifestyle and Social Media are the least common. This may suggest
something meaningful exists here to look into more.

Considering different data channels may be published more commonly on
specific days of the week (e.g., business articles potentially having a
higher publication frequency Monday through Friday compared to Saturday
through Sunday). A two-way contingency table can be run to see if that
theory holds up at the summary level of analysis.

``` r
twoWayChannelDays <- table(newsData$dataChannel, newsData$dayOfWeek) 
twoWayChannelDays
```

    ##                
    ##                 Friday Monday Saturday Sunday Thursday Tuesday Wednesday
    ##   Business         832   1153      243    343     1234    1182      1271
    ##   Entertainment    972   1358      380    536     1231    1285      1295
    ##   Lifestyle        305    322      182    210      358     334       388
    ##   Other            966    900      424    548     1102    1111      1083
    ##   Social Media     332    337      180    137      463     458       416
    ##   Tech             989   1235      525    396     1310    1474      1417
    ##   World           1305   1356      519    567     1569    1546      1565

It does appear from the table that publication frequency is much higher
Monday through Friday compared to Saturday through Sunday, and that this
pattern is stable across all data channels.

# Plots

This section is dedicated to visualization by means of the `ggplot2`
package. (edit the toc headings below)

## Plot 1 - Scstterplot

We first want to visually examine if there is relationship between title
length and the amount of times an article is shared.

``` r
correlation1 <- cor(newsData$n_tokens_title, newsData$shares)

scatterplotTitle <- ggplot(newsTrain, aes(x= `n_tokens_title`, y= `shares`)) +
  geom_point() +
  labs(title= "N_Tokens_Title  vs. Shares") + 
  geom_smooth(method = 'lm') +
  geom_smooth(col = "blue") +
  geom_text(x = 12, y = 175000, size = 4, label = paste0("Correlation = ", round(correlation1, 2)))
```

We can see from the scatterplot that there does not seem to be a linear
relationship between title length and the amount of times an article is
shared, as evidenced by the extremely weak correlation of 0.01.

## Plot 2 - Scatterplot

What if now we are interested in determining if there is relationship
between content length and the amount of times an article is shared.

``` r
correlation2 <- cor(newsData$average_token_length, newsData$shares)

scatterplotLength <- ggplot(newsTrain, aes(x= `average_token_length`, y= `shares`)) +
  geom_point() +
  labs(title= "Average_Token_Length  vs. Shares") + 
  geom_smooth(method = 'lm') +
  geom_smooth(col = "pink") +
  geom_text(x = 2, y = 175000, size = 4, label = paste0("Correlation = ", round(correlation2, 2)))
```

The scatterplot shows that similar to the previous analysis using title
length and shares, there does not seem to be a linear relationship
between content length and the amount of times an article is shared, as
evidenced by the extremely weak correlation of -0.02.

## Plot 3 - Barplot

We can use a bar graph to determine if any data channels are published
more on weekdays then on weekends.

``` r
barplotDay <- ggplot(data = newsTrain, aes(y= `weekdayWeekend`, fill = weekdayWeekend))
barplotDay + geom_bar() + 
  labs(title= "Publication on Weekday vs. Weekend")
```

![](Social%20Media_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> We
can see from the plot that there is a very apparent difference between
the number of articles published on weekdays vs. weekends, with articles
published on weekdays being much higher.

## Plot 4 - Histogram

This is a histogram of the shares variable. We wanted to get a better
idea of how this response variable is distributed and figured it was
best to transform the data via a log transformation. This method is
useful when dealing with skewed data and can make the data more
interpretable. When looking at this plot, we hope to see a fairly Normal
distribution.

``` r
hist.shares <- ggplot(data = newsTrain, aes(x = log(shares)))
hist.shares + geom_histogram(bins = 45, fill = "lightblue", colour = 8) +
              ggtitle("Log Transformation of Shares")
```

![](Social%20Media_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Plot 5 - Scatterplot

Next we will plot the number of links versus the number of shares. This
will be done via a scatterplot because this will be able to give us a
better visual of how these data are represented and if they are
associated with one another. When looking at this plot, one should take
note of a positive or negative correlation. For example, if we see a
positive correlation, we can suggest that articles with a greater number
of links tend to be shared more.

``` r
## First, find correlation to plot on graph
correlation3 <- cor(newsTrain$num_hrefs, newsTrain$shares) 
sp1 <- ggplot(data = newsTrain, aes(x = num_hrefs, y = shares))
sp1 + geom_point() + geom_smooth(method = "lm", col = "purple") +
  ggtitle("Number of Links vs. Shares") +
  geom_text(x = 120, y = 150000, size = 5, label = paste0("Corr = ", round(correlation3,2)))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Social%20Media_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Plot 6 - Boxplot

Below is a boxplot that assesses the number of images based on each day
of the week. These plots will give us a better resprentation of how our
data is distributed per each day of the week. One should take note of
any outliers and/or skewness that is present and be able to identify the
median.

``` r
bp1 <- ggplot(data = newsTrain, aes(x=dayOfWeek, y=num_imgs, fill=dayOfWeek))
bp1 + geom_boxplot() + ggtitle("Boxplot: Number of Images per Day of Week")
```

![](Social%20Media_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# Modeling

The main goal of our modeling section is to create predictive models for
predicting the number of shares based on our variables of interest. We
will be using two linear regression type models and two ensemble
tree-based models. Once these models are fit, we will be able to
identify the best of the four.

First, it is important to describe in detail the methods behind each of
these model types.

**Linear Models** – Linear models are a method for predictive modeling
that allows for evaluating the linear relationship between a continuous
outcome variable of interest against one or more predictor variables.
With linear models you are evaluating one model at a time, and can
compare that model to other models of interest to determine best fit for
the data. Linear regression and multiple linear regression are both
examples of linear models. …

**Ensemble Models** – Ensemble models are a method for taking multiple
independent and different models and combining those models into one
stronger model for prediction. This method can produce stronger
prediction results when compared to linear models due to it’s enhanced
ability to reduce generalized error. Random forest and Boosting trees
are both examples of ensemble models. …

## lm Model 1 -

**Full Linear Model** using all of our variables of interest. explain in
more detail.

``` r
## set up trainControl using 5-fold cross validation
trnCntrl <- trainControl(method = "cv", number=5)
## NewtrnCntrl for ENSEMBLE METHODS
NewtrnCntrl <- trainControl(method = "cv", number=5, repeats=3)
```

    ## Warning: `repeats` has no meaning for this resampling method.

``` r
## now fit linear model for all of our chosen predictors
lmFit1 <- train(shares ~ n_tokens_title + average_token_length + global_rate_positive_words +
                         global_rate_negative_words + num_imgs + num_videos + num_hrefs,
                data = newsTrain,
                method = "lm",
                trControl = trnCntrl,
                preProcess = c("center", "scale"))
##pre process


## Now predict on the TEST data!
lmFit1.pred <- predict(lmFit1, newdata = newsTest)
```

## lm Model 2 -

**Linear Model** using all of the two- way interactions between our
chosen predictors

``` r
lmFit2 <- train(shares ~ (n_tokens_title + average_token_length + global_rate_positive_words +
                         global_rate_negative_words + num_imgs + num_videos + num_hrefs)^2,
                data = newsTrain,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trnCntrl)
## Now predict on the TEST data!
lmFit2.pred <- predict(lmFit2, newdata = newsTest)
```

## Ensemble 1 -

**Random Forest Model**. This is a random forest model using all
variables we have selected for analysis as predictors.

``` r
rfFit <- train(shares ~ n_tokens_title + average_token_length + global_rate_positive_words +
                         global_rate_negative_words + num_imgs + num_videos + num_hrefs,
                  data = newsTrain,
                  method = "rf",
                  trControl = NewtrnCntrl,
                  preProcess = c("center", "scale"),
                  tuneGrid = data.frame(mtry= 1:7))

rfFit.pred <- predict(rfFit, newdata = newsTest)
```

## Ensemble 2 -

**Boosted Tree Model**. This is a boosted tree model using all variables
we have selected for analysis as predictors.

``` r
gbmGrid <-  expand.grid(interaction.depth = c(1,2,3,4), 
                        n.trees = c(25,50,100,150,200), 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

boostFit <- train(shares ~ n_tokens_title + average_token_length + global_rate_positive_words +
                         global_rate_negative_words + num_imgs + num_videos + num_hrefs,
                  data = newsTrain,
                  method = "gbm",
                  trControl = NewtrnCntrl,
                  preProcess = c("center", "scale"),
                  tuneGrid = gbmGrid,
                  verbose = FALSE)

boostFitPred <- predict(boostFit, newdata = newsTest)
```

# Model Comparison

There are many different metrics that can be utilized to determine which
model should be used. For example, popular metrics include MAE, RMSE,
AIC/BIC, and many more.

This section will be dedicated to comparing our models via the RMSE
metric only.

We will be using the `predict()` function that we used above after
fitting each fo the models, along with base R functions like `sqrt()`
and `mean()` to calculate RMSE by hand.

``` r
## Find RMSE for linear model fit 1
lmFit1.RMSE <- sqrt(mean((lmFit1.pred-newsTest$shares)^2))

## Find RMSE for linear model fit 2
lmFit2.RMSE <- sqrt(mean((lmFit2.pred-newsTest$shares)^2))

## Find RMSE for random forest model
rfFit.RMSE <- sqrt(mean((rfFit.pred-newsTest$shares)^2))

## Find RMSE for boosted tree model 
boostFit.RMSE <- sqrt(mean((boostFitPred-newsTest$shares)^2))
```

Now we can create a table of all of the RMSE values to get a better
visual of how these values differ among model fits.

``` r
## create data frame of all RMSE values 
all.RMSE <- data.frame(lmFit1.RMSE,lmFit2.RMSE,rfFit.RMSE,boostFit.RMSE)
colnames(all.RMSE) <- c("Linear Reg #1", "Linear Reg #2", "Random Forest", "Boosted Tree")
all.RMSE

## use which.min to find the element in the data frame with the lowest RMSE
minRMSE <- which.min(all.RMSE)
minRMSE
```

    ## Random Forest 
    ##             3

``` r
## now automate 
paste0("The model with the lowest RMSE value is the ", colnames(all.RMSE)[minRMSE] , " model")
```

    ## [1] "The model with the lowest RMSE value is the Random Forest model"