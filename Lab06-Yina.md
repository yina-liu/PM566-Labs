Lab 06 - Text Mining
================
Yina Liu

# Learning goals

  - Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
    ngrams from text.
  - Use dplyr and ggplot2 to analyze text data

# Lab description

For this lab we will be working with a new dataset. The dataset contains
transcription samples from <https://www.mtsamples.com/>. And is loaded
and “fairly” cleaned at
<https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv>.

This markdown document should be rendered using `github_document`
document.

# Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PM566-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/06-lab.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name that this folder has, i.e., “PM566-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

### Setup packages

You should load in `dplyr`, (or `data.table` if you want to work that
way), `ggplot2` and `tidytext`. If you don’t already have `tidytext`
then you can install with

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readr)
library(tidytext)
library(tidyr)
```

### read in Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
mt_samples <- read_csv("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv")
mt_samples <- mt_samples %>%
  select(description, medical_specialty, transcription)
head(mt_samples)
```

    ## # A tibble: 6 x 3
    ##   description                  medical_specialty   transcription                
    ##   <chr>                        <chr>               <chr>                        
    ## 1 A 23-year-old white female … Allergy / Immunolo… "SUBJECTIVE:,  This 23-year-…
    ## 2 Consult for laparoscopic ga… Bariatrics          "PAST MEDICAL HISTORY:, He h…
    ## 3 Consult for laparoscopic ga… Bariatrics          "HISTORY OF PRESENT ILLNESS:…
    ## 4 2-D M-Mode. Doppler.         Cardiovascular / P… "2-D M-MODE: , ,1.  Left atr…
    ## 5 2-D Echocardiogram           Cardiovascular / P… "1.  The left ventricular ca…
    ## 6 Morbid obesity.  Laparoscop… Bariatrics          "PREOPERATIVE DIAGNOSIS: , M…

``` r
mt_samples$transcription[1]
```

    ## [1] "SUBJECTIVE:,  This 23-year-old white female presents with complaint of allergies.  She used to have allergies when she lived in Seattle but she thinks they are worse here.  In the past, she has tried Claritin, and Zyrtec.  Both worked for short time but then seemed to lose effectiveness.  She has used Allegra also.  She used that last summer and she began using it again two weeks ago.  It does not appear to be working very well.  She has used over-the-counter sprays but no prescription nasal sprays.  She does have asthma but doest not require daily medication for this and does not think it is flaring up.,MEDICATIONS: , Her only medication currently is Ortho Tri-Cyclen and the Allegra.,ALLERGIES: , She has no known medicine allergies.,OBJECTIVE:,Vitals:  Weight was 130 pounds and blood pressure 124/78.,HEENT:  Her throat was mildly erythematous without exudate.  Nasal mucosa was erythematous and swollen.  Only clear drainage was seen.  TMs were clear.,Neck:  Supple without adenopathy.,Lungs:  Clear.,ASSESSMENT:,  Allergic rhinitis.,PLAN:,1.  She will try Zyrtec instead of Allegra again.  Another option will be to use loratadine.  She does not think she has prescription coverage so that might be cheaper.,2.  Samples of Nasonex two sprays in each nostril given for three weeks.  A prescription was written as well."

-----

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
catagories do we have? Are these catagories related? overlapping? evenly
distributed?

``` r
mt_samples %>%
  count(medical_specialty, sort = TRUE)
```

    ## # A tibble: 40 x 2
    ##    medical_specialty                 n
    ##    <chr>                         <int>
    ##  1 Surgery                        1103
    ##  2 Consult - History and Phy.      516
    ##  3 Cardiovascular / Pulmonary      372
    ##  4 Orthopedic                      355
    ##  5 Radiology                       273
    ##  6 General Medicine                259
    ##  7 Gastroenterology                230
    ##  8 Neurology                       223
    ##  9 SOAP / Chart / Progress Notes   166
    ## 10 Obstetrics / Gynecology         160
    ## # … with 30 more rows

## There are 40 different catagories and all the categories are all related in a medical facility. Nevertheless, the categories are not evenly distributed as Surgery appeared the most with 1103 appearances compared to Hospice - Palliative Care that only has a frequency of 6.

## Question 2

  - Tokenize the the words in the `transcription` column
  - Count the number of times each token appears
  - Visualize the top 20 most frequent words

Explain what we see from this result. Does it makes sense? What insights
(if any) do we get?

``` r
library(tidytext)
mt_samples %>%
  unnest_tokens(output=token, input=transcription) %>%
  count(token, sort = TRUE) %>%
  top_n(n=20, wt= n) %>%
  ggplot(aes(x=n, y=reorder(token,n))) +
  geom_col()
```

![](Lab06-Yina_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> The
results show that most of the frequent words are stop words, with the
most frequent being the word ‘the’ having a frequency of 150,000. —

## Question 3

  - Redo visualization but remove stopwords before
  - Bonus points if you remove numbers as well

What do we see know that we have removed stop words? Does it give us a
better idea of what the text is about?

``` r
mt_samples %>%
  unnest_tokens(output=token, input=transcription) %>%
  anti_join(stop_words, by = c("token"="word")) %>%
  count(token, sort = TRUE) %>%
  top_n(n=20, wt= n) %>%
  ggplot(aes(x=n, y=reorder(token,n))) +
  geom_col()
```

![](Lab06-Yina_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
mt_samples %>%
  unnest_tokens(word, transcription) %>%
  anti_join(tidytext::stop_words) %>%
  filter(!(word %in% as.character(seq(0,100)))) %>%
  count(word, sort = TRUE) %>%
  top_n(n=20, wt= n) %>%
  ggplot(aes(x=n, y=reorder(word,n))) +
  geom_col()
```

    ## Joining, by = "word"

![](Lab06-Yina_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> The most
frequent word is “patient” after removing stop words and numbers.

-----

# Question 4

repeat question 2, but this time tokenize into bi-grams. how does the
result change if you look at tri-grams?

``` r
mt_samples %>%
  unnest_ngrams(output = token, input = transcription, n = 2) %>%
  count(token, sort=TRUE) %>%
  top_n(n = 20, wt = n) %>%
  ggplot(aes(x = n, y = reorder(token, n))) +
  geom_col()
```

![](Lab06-Yina_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
mt_samples %>%
  unnest_ngrams(output = token, input = transcription, n = 3, n_min = 3) %>%
  count(token, sort=TRUE) %>%
  top_n(n = 20, wt = n) %>%
  ggplot(aes(x = n, y = reorder(token, n))) +
  geom_col()
```

![](Lab06-Yina_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

-----

# Question 5

Using the results you got from questions 4. Pick a word and count the
words that appears after and before it.

``` r
library(tidyr)
mt_bigrams <- mt_samples %>%
  unnest_ngrams(output = token, input = transcription, n = 2,
                collapse = FALSE) %>%
  separate(col = token, into =c("word1", "word2"), sep = " ") %>%
  select(word1, word2)
  
mt_bigrams %>%
  filter(word1 == "blood") %>%
  count (word2, sort = TRUE)
```

    ## # A tibble: 161 x 2
    ##    word2        n
    ##    <chr>    <int>
    ##  1 pressure  1265
    ##  2 loss       965
    ##  3 cell       130
    ##  4 in         114
    ##  5 cells      112
    ##  6 sugar       91
    ##  7 and         84
    ##  8 sugars      79
    ##  9 was         65
    ## 10 cultures    53
    ## # … with 151 more rows

``` r
mt_bigrams %>%
  filter(word2 == "blood") %>%
  count (word1, sort = TRUE)
```

    ## # A tibble: 439 x 2
    ##    word1         n
    ##    <chr>     <int>
    ##  1 estimated   754
    ##  2 white       180
    ##  3 signs       170
    ##  4 and         154
    ##  5 of          149
    ##  6 red         123
    ##  7 her         116
    ##  8 his          99
    ##  9 the          96
    ## 10 no           72
    ## # … with 429 more rows

``` r
mt_bigrams %>%
  count(word1, word2, sort = TRUE)
```

    ## # A tibble: 301,419 x 3
    ##    word1   word2       n
    ##    <chr>   <chr>   <int>
    ##  1 the     patient 20307
    ##  2 of      the     19062
    ##  3 in      the     12790
    ##  4 to      the     12374
    ##  5 was     then     6956
    ##  6 and     the      6350
    ##  7 patient was      6293
    ##  8 the     right    5509
    ##  9 on      the      5241
    ## 10 the     left     4860
    ## # … with 301,409 more rows

``` r
mt_bigrams %>%
  anti_join(
    tidytext :: stop_words %>% select(word), by =c("word1" = "word")
  ) %>%
  anti_join(
    tidytext :: stop_words %>% select(word), by =c("word2" = "word")
  ) %>%
  count(word1, word2, sort = TRUE)
```

    ## # A tibble: 128,018 x 3
    ##    word1         word2           n
    ##    <chr>         <chr>       <int>
    ##  1 0             vicryl       1802
    ##  2 blood         pressure     1265
    ##  3 medical       history      1223
    ##  4 diagnoses     1            1192
    ##  5 preoperative  diagnosis    1176
    ##  6 physical      examination  1156
    ##  7 4             0            1123
    ##  8 vital         signs        1121
    ##  9 past          medical      1113
    ## 10 postoperative diagnosis    1092
    ## # … with 128,008 more rows

-----

# Question 6

Which words are most used in each of the specialties. you can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the most
5 used words?

``` r
mt_samples %>%
  unnest_tokens(output=token, input=transcription) %>%
  anti_join(stop_words, by = c("token"="word")) %>%
  group_by(medical_specialty) %>%
  count(token, sort = TRUE) %>%
  top_n(1, n)
```

    ## # A tibble: 41 x 3
    ## # Groups:   medical_specialty [40]
    ##    medical_specialty          token       n
    ##    <chr>                      <chr>   <int>
    ##  1 Surgery                    patient  4855
    ##  2 Consult - History and Phy. patient  3046
    ##  3 Orthopedic                 patient  1711
    ##  4 Cardiovascular / Pulmonary left     1550
    ##  5 General Medicine           patient  1356
    ##  6 Gastroenterology           patient   872
    ##  7 Urology                    patient   776
    ##  8 Radiology                  left      701
    ##  9 Emergency Room Reports     patient   685
    ## 10 Discharge Summary          patient   672
    ## # … with 31 more rows

# Question 7 - extra

Find your own insight in the data:

Ideas:

  - Interesting ngrams
  - See if certain words are used more in some specialties than others
