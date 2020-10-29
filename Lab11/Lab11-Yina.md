Lab-11
================
Yina Liu
10/28/2020

``` r
library(data.table)
library(tidyverse)
library(dplyr)
library(plotly)
library(knitr)
opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  eval=TRUE,
  echo = TRUE,
  fig.width = 7, 
  fig.align = 'center',
  fig.asp = 0.618,
  out.width = "700px")
```

# Learning Goals

  - Read in and process the COVID dataset from the New York Times GitHub
    repository
  - Create interactive graphs of different types using `plot_ly()` and
    `ggplotly()` functions
  - Customize the hoverinfo and other plot features
  - Create a Choropleth map using `plot_geo()`
  - Create an interactive table using `DataTable`

# Lab Description

We will work with the COVID data presented in lecture. Recall the
dataset consists of COVID-19 cases and deaths in each US state during
the course of the COVID epidemic. We will explore cases, deaths, and
their population normalized values over time to identify trends.

# Steps

## I. Reading and processing the New York Times (NYT) state-level COVID-19 data

### 1\. Read in the data

  - Read in the COVID data with data.table:fread() from the NYT GitHub
    repository:
    “<https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv>”
  - Read in the state population data with data.table:fread() from the
    repository:
    “<https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv>”"
  - Merge datasets

<!-- end list -->

``` r
cv_states <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") )
state_pops <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv"))
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL
### FINISH THE CODE HERE ###
cv_states <- merge(cv_states,state_pops, by = "state")
```

### 2\. Look at the data

  - Inspect the dimensions, `head`, and `tail` of the data
  - Inspect the structure of each variables. Are they in the correct
    format?

<!-- end list -->

``` r
dim(cv_states)
```

    ## [1] 12542     9

``` r
head(cv_states)
```

    ##     state       date fips  cases deaths geo_id population pop_density abb
    ## 1 Alabama 2020-08-26    1 119254   2045      1    4887871    96.50939  AL
    ## 2 Alabama 2020-09-08    1 133606   2277      1    4887871    96.50939  AL
    ## 3 Alabama 2020-07-31    1  87723   1580      1    4887871    96.50939  AL
    ## 4 Alabama 2020-07-16    1  61088   1230      1    4887871    96.50939  AL
    ## 5 Alabama 2020-05-25    1  14986    566      1    4887871    96.50939  AL
    ## 6 Alabama 2020-08-25    1 117242   2037      1    4887871    96.50939  AL

``` r
tail(cv_states)
```

    ##         state       date fips cases deaths geo_id population pop_density abb
    ## 12537 Wyoming 2020-05-27   56   860     14     56     577737    5.950611  WY
    ## 12538 Wyoming 2020-09-10   56  4199     42     56     577737    5.950611  WY
    ## 12539 Wyoming 2020-07-18   56  2108     24     56     577737    5.950611  WY
    ## 12540 Wyoming 2020-08-12   56  3086     29     56     577737    5.950611  WY
    ## 12541 Wyoming 2020-09-09   56  4151     42     56     577737    5.950611  WY
    ## 12542 Wyoming 2020-06-10   56   980     18     56     577737    5.950611  WY

``` r
str(cv_states)
```

    ## 'data.frame':    12542 obs. of  9 variables:
    ##  $ state      : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
    ##  $ date       : chr  "2020-08-26" "2020-09-08" "2020-07-31" "2020-07-16" ...
    ##  $ fips       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ cases      : int  119254 133606 87723 61088 14986 117242 132973 14149 10164 105557 ...
    ##  $ deaths     : int  2045 2277 1580 1230 566 2037 2276 549 403 1890 ...
    ##  $ geo_id     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ population : int  4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 ...
    ##  $ pop_density: num  96.5 96.5 96.5 96.5 96.5 ...
    ##  $ abb        : chr  "AL" "AL" "AL" "AL" ...

### 3\. Format the data

  - Make date into a date variable
  - Make `state` and `abb` into a factor variable
  - Order the data first by state, second by date
  - Confirm the variables are now correctly formatted
  - Inspect the range values for each variable. What is the date range?
    The range of cases and deaths?

<!-- end list -->

``` r
# format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")
# format the state variable
state_list <- unique(cv_states$state)
cv_states$state <- factor(cv_states$state, levels = state_list)
# format the state abbreviation (abb) variable
abb_list = unique(cv_states$abb)
cv_states$abb = factor(cv_states$abb, levels = abb_list)
  
# order the data first by state, second by date
cv_states = cv_states[order(cv_states$state, cv_states$date),]
# Confirm the variables are now correctly formatted
str(cv_states)
```

    ## 'data.frame':    12542 obs. of  9 variables:
    ##  $ state      : Factor w/ 52 levels "Alabama","Alaska",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ date       : Date, format: "2020-03-13" "2020-03-14" ...
    ##  $ fips       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ cases      : int  6 12 23 29 39 51 78 106 131 157 ...
    ##  $ deaths     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ geo_id     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ population : int  4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 4887871 ...
    ##  $ pop_density: num  96.5 96.5 96.5 96.5 96.5 ...
    ##  $ abb        : Factor w/ 52 levels "AL","AK","AZ",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
head(cv_states)
```

    ##       state       date fips cases deaths geo_id population pop_density abb
    ## 183 Alabama 2020-03-13    1     6      0      1    4887871    96.50939  AL
    ## 190 Alabama 2020-03-14    1    12      0      1    4887871    96.50939  AL
    ## 230 Alabama 2020-03-15    1    23      0      1    4887871    96.50939  AL
    ## 133 Alabama 2020-03-16    1    29      0      1    4887871    96.50939  AL
    ## 66  Alabama 2020-03-17    1    39      0      1    4887871    96.50939  AL
    ## 39  Alabama 2020-03-18    1    51      0      1    4887871    96.50939  AL

``` r
tail(cv_states)
```

    ##         state       date fips cases deaths geo_id population pop_density abb
    ## 12497 Wyoming 2020-10-23   56 10545     68     56     577737    5.950611  WY
    ## 12389 Wyoming 2020-10-24   56 10805     68     56     577737    5.950611  WY
    ## 12404 Wyoming 2020-10-25   56 11041     68     56     577737    5.950611  WY
    ## 12342 Wyoming 2020-10-26   56 11477     77     56     577737    5.950611  WY
    ## 12461 Wyoming 2020-10-27   56 11806     77     56     577737    5.950611  WY
    ## 12477 Wyoming 2020-10-28   56 12146     77     56     577737    5.950611  WY

``` r
# Inspect the range values for each variable. What is the date range? The range of cases and deaths?
head(cv_states)
```

    ##       state       date fips cases deaths geo_id population pop_density abb
    ## 183 Alabama 2020-03-13    1     6      0      1    4887871    96.50939  AL
    ## 190 Alabama 2020-03-14    1    12      0      1    4887871    96.50939  AL
    ## 230 Alabama 2020-03-15    1    23      0      1    4887871    96.50939  AL
    ## 133 Alabama 2020-03-16    1    29      0      1    4887871    96.50939  AL
    ## 66  Alabama 2020-03-17    1    39      0      1    4887871    96.50939  AL
    ## 39  Alabama 2020-03-18    1    51      0      1    4887871    96.50939  AL

``` r
summary(cv_states)
```

    ##            state            date                 fips           cases       
    ##  Washington   :  282   Min.   :2020-01-21   Min.   : 1.00   Min.   :     1  
    ##  Illinois     :  279   1st Qu.:2020-05-01   1st Qu.:16.00   1st Qu.:  3323  
    ##  California   :  278   Median :2020-06-30   Median :29.00   Median : 20154  
    ##  Arizona      :  277   Mean   :2020-06-29   Mean   :29.77   Mean   : 66536  
    ##  Massachusetts:  271   3rd Qu.:2020-08-29   3rd Qu.:44.00   3rd Qu.: 76971  
    ##  Wisconsin    :  267   Max.   :2020-10-28   Max.   :72.00   Max.   :931113  
    ##  (Other)      :10888                                                        
    ##      deaths          geo_id        population        pop_density       
    ##  Min.   :    0   Min.   : 1.00   Min.   :  577737   Min.   :    1.292  
    ##  1st Qu.:   76   1st Qu.:16.00   1st Qu.: 1805832   1st Qu.:   54.956  
    ##  Median :  532   Median :29.00   Median : 4468402   Median :  107.860  
    ##  Mean   : 2291   Mean   :29.77   Mean   : 6560815   Mean   :  420.682  
    ##  3rd Qu.: 2250   3rd Qu.:44.00   3rd Qu.: 7535591   3rd Qu.:  229.511  
    ##  Max.   :33107   Max.   :72.00   Max.   :39557045   Max.   :11490.120  
    ##                                                     NA's   :230        
    ##       abb       
    ##  WA     :  282  
    ##  IL     :  279  
    ##  CA     :  278  
    ##  AZ     :  277  
    ##  MA     :  271  
    ##  WI     :  267  
    ##  (Other):10888

``` r
min(cv_states$date)
```

    ## [1] "2020-01-21"

``` r
max(cv_states$date)
```

    ## [1] "2020-10-28"

### 4\. Add `new_cases` and `new_deaths` and correct outliers

  - Add variables for new cases, `new_cases`, and new deaths,
    `new_deaths`:
    
      - Hint: `new_cases` is equal to the difference between cases on
        date i and date i-1, starting on date i=2

  - Use `plotly` for EDA: See if there are outliers or values that don’t
    make sense for `new_cases` and `new_deaths`. Which states and which
    dates have strange values?

  - Correct outliers: Set negative values for `new_cases` or
    `new_deaths` to 0

  - Recalculate `cases` and `deaths` as cumulative sum of updates
    `new_cases` and `new_deaths`

<!-- end list -->

``` r
# Add variables for new_cases and new_deaths:
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  cv_subset = cv_subset[order(cv_subset$date),]
  # add starting level for new cases and deaths
  cv_subset$new_cases = cv_subset$cases[1]
  cv_subset$new_deaths = cv_subset$deaths[1]
  #### FINISH THE CODE HERE ###
  for (j in 2:nrow(cv_subset)) {
    cv_subset$new_cases[j] =  cv_subset$cases[j] - cv_subset$cases[j-1]
    cv_subset$new_deaths[j] = cv_subset$deaths[j] - cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$new_cases[cv_states$state==state_list[i]] = cv_subset$new_cases
  cv_states$new_deaths[cv_states$state==state_list[i]] = cv_subset$new_deaths
}
# Inspect outliers in new_cases and new_deaths using plotly
### FINISH THE CODE HERE ###
p1<-ggplot(cv_states, 
           aes( x=date, y=new_cases, color=state )
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p1)
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-4-1.png" width="700px" style="display: block; margin: auto;" />

``` r
p1<-NULL 
### FINISH THE CODE HERE ###
p2<-ggplot(cv_states, 
           aes(x=date, y=new_deaths, color=state)
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-4-2.png" width="700px" style="display: block; margin: auto;" />

``` r
p2<-NULL 
```

``` r
# set negative new case or death counts to 0
cv_states$new_cases[cv_states$new_cases<0] = 0
cv_states$new_deaths[cv_states$new_deaths<0] = 0
# Recalculate `cases` and `deaths` as cumulative sum of updates `new_cases` and `new_deaths`
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  # add starting level for new cases and deaths
  cv_subset$cases = cv_subset$cases[1]
  cv_subset$deaths = cv_subset$deaths[1]
  for (j in 2:nrow(cv_subset)) {
    cv_subset$cases[j] = cv_subset$new_cases[j] + cv_subset$cases[j-1]
    cv_subset$deaths[j] = cv_subset$new_deaths[j] + cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$cases[cv_states$state==state_list[i]] = cv_subset$cases
  cv_states$deaths[cv_states$state==state_list[i]] = cv_subset$deaths
}
```

### 5\. Add additional variables

  - Add population-normalized (by 100,000) variables for each variable
    type (rounded to 1 decimal place). Make sure the variables you
    calculate are in the correct format (`numeric`). You can use the
    following variable names:
    
      - `per100k` = cases per 100,000 population
      - `newper100k`= new cases per 100,000
      - `deathsper100k` = deaths per 100,000
      - `newdeathsper100k` = new deaths per 100,000

  - Add a “naive CFR” variable representing `deaths / cases` on each
    date for each state

  - Create a dataframe representing values on the most recent date,
    `cv_states_today`, as done in lecture

<!-- end list -->

``` r
# add population normalized (by 100,000) counts for each variable
cv_states$per100k =  as.numeric(format(round(cv_states$cases/(cv_states$population/100000),1),nsmall=1))
cv_states$newper100k =  as.numeric(format(round(cv_states$new_cases/(cv_states$population/100000),1),nsmall=1))
cv_states$deathsper100k =  as.numeric(format(round(cv_states$deaths/(cv_states$population/100000),1),nsmall=1))
cv_states$newdeathsper100k =  as.numeric(format(round(cv_states$new_deaths/(cv_states$population/100000),1),nsmall=1))
# add a naive_CFR variable = deaths / cases
cv_states = cv_states %>% mutate(naive_CFR = round((deaths*100/cases),2))
# create a `cv_states_today` variable
### FINISH THE CODE HERE ###
max_date = max(cv_states$date)
cv_states_today = cv_states %>% filter(date==as.Date(max_date))
```

## II. Interactive plots

### 6\. Explore scatterplots using `plot_ly()`

  - Create a scatterplot using `plot_ly()` representing `pop_density`
    vs. various variables (e.g. `cases`, `per100k`, `deaths`,
    `deathsper100k`) for each state on most recent date
    (`cv_states_today`)
      - Use hover to identify any outliers.
      - Remove those outliers and replot.
  - Choose one plot. For this plot:
      - Add hoverinfo specifying the state name, cases per 100k, and
        deaths per 100k, similarly to how we did this in the lecture
        notes
      - Add layout information to title the chart and the axes
      - Enable `hovermode = "compare"`

<!-- end list -->

``` r
# pop_density vs. cases
### FINISH THE CODE HERE ###
cv_states_today %>% 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-7-1.png" width="700px" style="display: block; margin: auto;" />

``` r
# filter out "District of Columbia"
cv_states_today_scatter <- cv_states_today %>% filter(state!="District of Columbia")
# pop_density vs. cases after filtering
cv_states_today_scatter %>% filter(state!="District of Columbia") %>% 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-7-2.png" width="700px" style="display: block; margin: auto;" />

``` r
# pop_density vs. deathsper100k
cv_states_today_scatter %>% filter(state!="District of Columbia") %>% 
  plot_ly(x = ~pop_density, y = ~deathsper100k, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-7-3.png" width="700px" style="display: block; margin: auto;" />

``` r
# Adding hoverinfo
cv_states_today_scatter %>% 
  plot_ly(x = ~pop_density, y = ~deathsper100k,
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(state, ":", sep=""), paste(" Cases per 100k: ", per100k, sep="") , paste(" Deaths per 100k: ",
                        deathsper100k, sep=""), sep = "<br>")) %>%
  layout(title = "Population-normalized COVID-19 deaths (per 100k) vs. population density for US states",
                  yaxis = list(title = "Deaths per 100k"), xaxis = list(title = "Population Density"),
         hovermode = "compare")
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-7-4.png" width="700px" style="display: block; margin: auto;" />

### 7\. Explore scatterplot trend interactively using `ggplotly()` and `geom_smooth()`

  - For `pop_density` vs. `newdeathsper100k` create a chart with the
    same variables using `gglot_ly()`
      - What’s the `geom_*()` we need here?
  - Explore the pattern between \(x\) and \(y\) using `geom_smooth()`
      - Explain what you see. Do you think `pop_density` is a correlate
        of `newdeathsper100k`?

<!-- end list -->

``` r
### FINISH THE CODE HERE ###
p <- ggplot(cv_states_today_scatter, aes(x=pop_density, y=deathsper100k, size=population)) + geom_smooth() + geom_point()
ggplotly(p)
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-8-1.png" width="700px" style="display: block; margin: auto;" />

### 8\. Multiple line chart

  - Create a line chart of the `naive_CFR` for all states over time
    using `plot_ly()`
      - Use hoverinfo to identify states that had a “first peak”
      - Use the zoom and pan tools to inspect the `naive_CFR` for the
        states that had a “first peak” in September. How have they
        changed over time?
  - Create one more line chart, for Texas only, which shows `new_cases`
    and `new_deaths` together in one plot. Hint: use `add_lines()`
      - Use hoverinfo to “eyeball” the approximate peak of deaths and
        peak of cases. What is the time delay between the peak of cases
        and the peak of deaths?

<!-- end list -->

``` r
# Line chart for naive_CFR for all states over time using `plot_ly()`
plot_ly(cv_states, x = ~date, y = ~naive_CFR, color = ~state, type = "scatter", mode = "lines")
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-9-1.png" width="700px" style="display: block; margin: auto;" />

``` r
# Line chart for Texas showing new_cases and new_deaths together
### FINISH THE CODE HERE ###
cv_states %>% filter(state=="Texas") %>% plot_ly(x = ~date, y = ~new_cases, type = "scatter", mode = "lines") %>% add_lines(x = ~date, y = ~new_deaths, type = "scatter", mode = "lines")
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-9-2.png" width="700px" style="display: block; margin: auto;" />

### 9\. Heatmaps

Create a heatmap to visualize `new_cases` for each state on each date
greater than April 1st, 2020 - Start by mapping selected features in the
dataframe into a matrix using the **tidyr** package function
`pivot_wider()`, naming the rows and columns, as done in the lecture
notes - Use `plot_ly()` to create a heatmap out of this matrix - Create
a second heatmap in which the pattern of `new_cases` for each state over
time becomes more clear by filtering to only look at dates every two
weeks

``` r
# Map state, date, and new_cases to a matrix
library(tidyr)
cv_states_mat <- cv_states %>% select(state, date, new_cases) %>% filter(date>as.Date("2020-04-01"))
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = new_cases))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)
# Create a heatmap using plot_ly()
plot_ly(x=colnames(cv_states_mat2), y=rownames(cv_states_mat2),
             z=~cv_states_mat2,
             type="heatmap",
             showscale=T)
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-10-1.png" width="700px" style="display: block; margin: auto;" />

``` r
# Create a second heatmap after filtering to only include dates every other week
filter_dates <- seq(as.Date("2020-04-01"), as.Date("2020-10-01"), by="2 weeks")
### FINISH THE CODE HERE ### 
cv_states_mat <- cv_states %>% select(state, date, new_cases) %>% filter( date %in% filter_dates)
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = new_cases))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)
# Create a heatmap using plot_ly()
plot_ly(x=colnames(cv_states_mat2), y=rownames(cv_states_mat2),
             z=~cv_states_mat2,
             type="heatmap",
             showscale=T)
```

<img src="Lab11-Yina_files/figure-gfm/unnamed-chunk-10-2.png" width="700px" style="display: block; margin: auto;" />

### 10\. Map

  - Create a map to visualize the `naive_CFR` by state on May 1st, 2020
  - Compare with a map visualizing the `naive_CFR` by state on most
    recent date
  - Plot the two maps side by side using `subplot()`. Make sure the
    shading is for the same range of values (google is your friend for
    this)
  - Describe the difference in the pattern of the CFR.

<!-- end list -->

``` r
### For May 1 2020
# Extract the data for each state by its abbreviation
cv_CFR <- cv_states %>% filter(date=="2020-05-01") %>% select(state, abb, naive_CFR, cases, deaths) # select data
cv_CFR$state_name <- cv_CFR$state
cv_CFR$state <- cv_CFR$abb
cv_CFR$abb <- NULL
# Create hover text
cv_CFR$hover <- with(cv_CFR, paste(state_name, '<br>', "CFR: ", naive_CFR, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))
# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
# Make sure both maps are on the same color scale
shadeLimit <- 9
# Create the map
fig <- plot_geo(cv_CFR, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~naive_CFR, text = ~hover, locations = ~state,
    color = ~naive_CFR, colors = 'Purples'
  )
fig <- fig %>% colorbar(title = "CFR May 1 2020", color = c(0,shadeLimit))
fig <- fig %>% layout(
    title = paste('CFR by State as of', Sys.Date(), '<br>(Hover for value)'),
    geo = set_map_details
  )
fig_May1 <- fig
#############
### For Today
# Extract the data for each state by its abbreviation
cv_CFR <- cv_states_today %>%  select(state, abb, naive_CFR, cases, deaths) # select data
cv_CFR$state_name <- cv_CFR$state
cv_CFR$state <- cv_CFR$abb
cv_CFR$abb <- NULL
# Create hover text
cv_CFR$hover <- with(cv_CFR, paste(state_name, '<br>', "CFR: ", naive_CFR, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))
# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
# Create the map
fig <- plot_geo(cv_CFR, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~naive_CFR, text = ~hover, locations = ~state,
    color = ~naive_CFR, colors = 'Purples'
  )
fig <- fig %>% colorbar(title = "CFR May 1 2020", color = c(0,shadeLimit))
fig <- fig %>% layout(
    title = paste('CFR by State as of', Sys.Date(), '<br>(Hover for value)'),
    geo = set_map_details
  )
fig_Today <- fig
### Plot side by side 
```

### FINISH THE CODE HERE

subplot( \_\_\_ ) \`\`\`
