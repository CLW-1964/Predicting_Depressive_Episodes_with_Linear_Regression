Capstone Project Exploratory Data Analysis
==========================================

Overview
--------

The goal of this exercise is to see if we can detect or predict a depressive episode once antidepressive medications have been reduced in a patient with a 8.5 year history of major depression.

The dataset contains the results of a series of questions that were asked of the patient several times a day, daily and weekly over the course of 239 days. The study was broken down into five phases:

-   Phase 1 - Baseline period lasting four weeks where the medication dosage was kept constant 150 mg.

-   Phase 2 - The researchers called this a double-blind phase. The medication dosage was not modified and lasted for two weeks.

-   Phase 3 - A double-blind period lasting eight weeks where the medication dosage was gradually decreased on a schedule randomly chosen by a pharmacist.

-   Phase 4 - A post-assessment period lasting eight weeks in which the patient was completely off the medication.

-   Phase 5 - A twelve week follow-up period.

Load libraries and clean data file
----------------------------------

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.4.2     v dplyr   0.7.4
    ## v tidyr   0.8.0     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(quantreg)
```

    ## Loading required package: SparseM

    ## 
    ## Attaching package: 'SparseM'

    ## The following object is masked from 'package:base':
    ## 
    ##     backsolve

    ## 
    ## Attaching package: 'quantreg'

    ## The following object is masked from 'package:Hmisc':
    ## 
    ##     latex

    ## The following object is masked from 'package:survival':
    ## 
    ##     untangle.specials

``` r
library(RColorBrewer)
library(GGally)
```

    ## 
    ## Attaching package: 'GGally'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     nasa

``` r
library(ggvis)
```

    ## 
    ## Attaching package: 'ggvis'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     resolution

``` r
library(corrgram)       
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
DatDisc <- read_csv("ESMdata_clean2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   date = col_date(format = ""),
    ##   beeptime = col_time(format = ""),
    ##   resptime_s = col_time(format = ""),
    ##   resptime_e = col_time(format = ""),
    ##   dep = col_double(),
    ##   datetime = col_datetime(format = "")
    ## )

    ## See spec(...) for full column specifications.

``` r
DatDisc%>% glimpse()
```

    ## Observations: 1,476
    ## Variables: 89
    ## $ X1                   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13...
    ## $ date                 <date> 2012-08-13, 2012-08-14, 2012-08-14, 2012...
    ## $ phase                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ concentrat           <int> 150, 150, 150, 150, 150, 150, 150, 150, 1...
    ## $ dayno                <int> 226, 227, 227, 227, 227, 227, 228, 228, 2...
    ## $ beepno               <int> 1, 5, 6, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9...
    ## $ beeptime             <time> 08:58:00, 14:32:00, 16:17:00, 18:04:00, ...
    ## $ resptime_s           <time> 08:58:56, 14:32:09, 16:17:13, 18:04:10, ...
    ## $ resptime_e           <time> 09:00:15, 14:33:25, 16:23:16, 18:06:29, ...
    ## $ resp_abort           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ mood_relaxed         <int> 5, 4, 4, 4, 4, 5, 5, 3, 3, 3, 2, 4, 4, 3,...
    ## $ mood_down            <int> -1, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, ...
    ## $ mood_irritat         <int> 1, 3, 2, 1, 2, 1, 1, 1, 3, 2, 4, 3, 2, 3,...
    ## $ mood_satisfi         <int> 5, 3, 3, 4, 4, 4, 3, 3, 3, 4, 2, 3, 4, 3,...
    ## $ mood_lonely          <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ mood_anxious         <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, ...
    ## $ mood_enthus          <int> 4, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3,...
    ## $ mood_suspic          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1,...
    ## $ mood_cheerf          <int> 5, 4, 4, 4, 4, 3, 4, 3, 3, 3, 3, 3, 4, 3,...
    ## $ mood_guilty          <int> -1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1...
    ## $ mood_doubt           <int> 1, 1, 2, 1, 2, 2, 3, 4, 3, 3, 2, 2, 2, 3,...
    ## $ mood_strong          <int> 5, 4, 4, 4, 3, 3, 4, 3, 4, 4, 3, 3, 3, 3,...
    ## $ pat_restl            <int> 1, 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 2, 3, 2,...
    ## $ pat_agitate          <int> 1, 2, 1, 1, 1, 1, 1, 1, 3, 2, 3, 3, 2, 3,...
    ## $ pat_worry            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ pat_concent          <int> 5, 4, 3, 4, 4, 3, 4, 4, 4, 3, 4, 4, 4, 3,...
    ## $ se_selflike          <int> 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,...
    ## $ se_ashamed           <int> 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,...
    ## $ se_selfdoub          <int> 1, 2, 1, 1, 3, 2, 2, 2, 3, 2, 3, 1, 2, 3,...
    ## $ se_handle            <int> 5, 4, 4, 4, 4, 3, 4, 3, 4, 4, 4, 3, 3, 3,...
    ## $ soc_who1             <int> 10, 0, 19, 10, 10, 10, 0, 0, 0, 0, 0, 0, ...
    ## $ soc_enjoy_alone      <int> NA, 5, NA, NA, NA, NA, 4, 4, 4, 4, 4, 4, ...
    ## $ soc_prefcomp         <int> NA, 1, NA, NA, NA, NA, 1, 2, 1, 2, 1, 3, ...
    ## $ soc_who2             <int> 0, NA, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA...
    ## $ soc_who3             <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ soc_belong           <int> 1, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA...
    ## $ soc_pleasant         <int> 5, NA, 5, 6, 6, 5, NA, NA, NA, NA, NA, NA...
    ## $ soc_prefalone        <int> 1, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA...
    ## $ soc_together         <int> 4, NA, 5, 4, 6, 3, NA, NA, NA, NA, NA, NA...
    ## $ phy_hungry           <int> 1, 1, 1, 2, 4, 1, 1, 1, 3, 4, 2, 2, 2, 4,...
    ## $ phy_tired            <int> 1, 2, 1, 2, 3, 3, 2, 3, 2, 2, 2, 3, 2, 4,...
    ## $ phy_pain             <int> 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1,...
    ## $ phy_dizzy            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ phy_drymouth         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ phy_nauseous         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ phy_headache         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,...
    ## $ phy_sleepy           <int> 1, 1, 1, 1, 3, 2, 1, 3, 1, 1, 1, 1, 1, 3,...
    ## $ act_what1            <int> 88, 10, 45, 45, 60, 45, 45, 0, 10, 45, 10...
    ## $ act_what2            <int> 45, 0, 0, 0, 51, 0, 0, NA, 0, 60, 0, 0, 6...
    ## $ act_difficul         <int> 1, 4, 2, 1, 2, 2, 1, 3, 4, 3, 5, 5, 2, 3,...
    ## $ act_well             <int> 5, 4, 5, 5, 5, 4, 5, 4, 4, 5, 4, 4, 5, 5,...
    ## $ act_enjoy            <int> 5, 3, 4, 5, 5, 4, 3, 3, 3, 4, 2, 3, 5, 4,...
    ## $ phy_physact          <int> 3, 4, 1, 4, 4, 1, 3, 2, 2, 1, 1, 2, 2, 2,...
    ## $ event_pleas          <int> 1, 0, 0, 0, 3, 1, 0, 0, -1, -1, -2, -2, -...
    ## $ event_import         <int> 2, 0, 0, 1, 2, 1, 0, 0, 0, 1, 0, 0, 0, 0,...
    ## $ event_cause          <int> 1, 5, 5, 2, 1, 3, 4, 4, 3, 2, 3, 3, 3, 3,...
    ## $ event_concern        <int> 2, 6, 6, 1, 1, NA, NA, NA, NA, 4, NA, NA,...
    ## $ event_freq           <int> NA, NA, NA, NA, NA, 2, NA, NA, 1, NA, 1, ...
    ## $ event_pertain        <int> NA, NA, NA, NA, NA, NA, 2, 2, NA, NA, NA,...
    ## $ event_disturb        <int> 3, 6, 4, 4, 6, 6, 5, 5, 7, 7, 7, 7, 6, 6,...
    ## $ evn_ordinary         <int> 3, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3,...
    ## $ evn_niceday          <int> 4, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3,...
    ## $ evn_inflmood         <int> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,...
    ## $ evn_pager            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ evn_work             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ evn_med              <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ mor_asleep           <int> 3, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA,...
    ## $ mor_nrwakeup         <int> 1, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA,...
    ## $ mor_lieawake         <int> 2, 3, 3, 3, 3, 3, NA, NA, NA, NA, NA, NA,...
    ## $ mor_qualsleep        <int> 5, 6, 6, 6, 6, 6, NA, NA, NA, NA, NA, NA,...
    ## $ mor_feellike         <int> 5, 5, 5, 5, 5, 5, NA, NA, NA, NA, NA, NA,...
    ## $ mor_med              <int> 1, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.14          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.20          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.22          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.5           <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.29          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.26          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.15          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.30          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.31          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.32          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.54          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.71          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ SCL.90.R.79          <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ dep                  <dbl> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ survey_time_to_take  <int> 79, 76, 363, 139, 115, 110, 96, 105, 83, ...
    ## $ datetime             <dttm> 2012-08-13 13:58:00, 2012-08-14 19:32:00...
    ## $ survey_time_to_start <int> 56, 9, 13, 10, 83, 15, 13, 24, 69, 46, 56...

Because there are questions that are answered at different intervals there are a number of empty fields or NA's in the data.

``` r
DatDisc %>%
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(col_name, count_NAs) %>%
  print(n = Inf) 
```

    ## # A tibble: 89 x 2
    ##    col_name             count_NAs
    ##    <chr>                    <int>
    ##  1 X1                           0
    ##  2 date                         0
    ##  3 phase                        0
    ##  4 concentrat                   0
    ##  5 dayno                        0
    ##  6 beepno                       0
    ##  7 beeptime                     0
    ##  8 resptime_s                   0
    ##  9 resptime_e                   2
    ## 10 resp_abort                   2
    ## 11 mood_relaxed                 0
    ## 12 mood_down                    2
    ## 13 mood_irritat                 3
    ## 14 mood_satisfi                 3
    ## 15 mood_lonely                  2
    ## 16 mood_anxious                 3
    ## 17 mood_enthus                  3
    ## 18 mood_suspic                  3
    ## 19 mood_cheerf                  3
    ## 20 mood_guilty                  3
    ## 21 mood_doubt                   3
    ## 22 mood_strong                  3
    ## 23 pat_restl                    3
    ## 24 pat_agitate                  3
    ## 25 pat_worry                    3
    ## 26 pat_concent                  3
    ## 27 se_selflike                  3
    ## 28 se_ashamed                   4
    ## 29 se_selfdoub                  4
    ## 30 se_handle                    4
    ## 31 soc_who1                     4
    ## 32 soc_enjoy_alone            678
    ## 33 soc_prefcomp               678
    ## 34 soc_who2                   802
    ## 35 soc_who3                  1234
    ## 36 soc_belong                 802
    ## 37 soc_pleasant               802
    ## 38 soc_prefalone              802
    ## 39 soc_together               802
    ## 40 phy_hungry                   5
    ## 41 phy_tired                    5
    ## 42 phy_pain                     5
    ## 43 phy_dizzy                    5
    ## 44 phy_drymouth                 5
    ## 45 phy_nauseous                 5
    ## 46 phy_headache                 5
    ## 47 phy_sleepy                   5
    ## 48 act_what1                    5
    ## 49 act_what2                   77
    ## 50 act_difficul                 5
    ## 51 act_well                     5
    ## 52 act_enjoy                    5
    ## 53 phy_physact                  5
    ## 54 event_pleas                  5
    ## 55 event_import                 5
    ## 56 event_cause                  5
    ## 57 event_concern              550
    ## 58 event_freq                1034
    ## 59 event_pertain             1371
    ## 60 event_disturb                7
    ## 61 evn_ordinary               262
    ## 62 evn_niceday                262
    ## 63 evn_inflmood               262
    ## 64 evn_pager                  262
    ## 65 evn_work                   262
    ## 66 evn_med                    262
    ## 67 mor_asleep                 584
    ## 68 mor_nrwakeup               584
    ## 69 mor_lieawake               584
    ## 70 mor_qualsleep              584
    ## 71 mor_feellike               584
    ## 72 mor_med                    591
    ## 73 SCL.90.R.14               1289
    ## 74 SCL.90.R.20               1289
    ## 75 SCL.90.R.22               1289
    ## 76 SCL.90.R.5                1289
    ## 77 SCL.90.R.29               1289
    ## 78 SCL.90.R.26               1289
    ## 79 SCL.90.R.15               1289
    ## 80 SCL.90.R.30               1289
    ## 81 SCL.90.R.31               1289
    ## 82 SCL.90.R.32               1289
    ## 83 SCL.90.R.54               1289
    ## 84 SCL.90.R.71               1289
    ## 85 SCL.90.R.79               1289
    ## 86 dep                       1289
    ## 87 survey_time_to_take          2
    ## 88 datetime                     0
    ## 89 survey_time_to_start         0

For initial data discovery I decided not to replace these NA's and work with the data as is.

Data Discovery
--------------

Since the goal is to determine if we can predict depressive episodes natural to use the dep variable for comparison purposes. The dep variable is the average score of the 13 questions from the depression subscale of the SCL-90-R. However, I am not a fan of averages in general and decided to create a new field which is the sum of the values for comparison purposes.

``` r
Grouped_DatDisc <- DatDisc %>%
  mutate(n_Sum_dep = SCL.90.R.14+SCL.90.R.20+SCL.90.R.22+SCL.90.R.5+
                      SCL.90.R.29+SCL.90.R.26+SCL.90.R.15+SCL.90.R.30+
                      SCL.90.R.31+SCL.90.R.31+SCL.90.R.32+SCL.90.R.54+
                      SCL.90.R.71+SCL.90.R.79) 
```

Traditionally, the initial phase of data discovery involves creating scatter plots, histograms, and bar charts to look for patterns in the data and possible variable interactions.

In this dataset there are 86 variables. In order to simplify the first pass at the data I grouped the variables that were asked multiple times per day into three new categories.

-   Positive Mood and Self Esteem Indicators - Questions such as, "I feel cheerful", and "I like myself"

-   Negative Mood and Self Esteem Indicators - Questions such as, "I am anxious", and "I feel worried"

-   Physical Indicators - Questions such as, "I am tired", and "I am in pain"

All questions in these groupings were measured using one of two scales. A Likert scale ranging from –3 to 3 or a 7-point Likert scale ranging from 1 to 7. Because I chose to group into these three catagories by summing the values of their individual scores, I chose to create a new variables and transform the -3 to 3 Likert scale questions to the 7-point Likert scale.

``` r
Grouped_DatDisc <- Grouped_DatDisc %>%

# recode variables on a -3 to 3 scale to a 1 to 7 scale 
#in order to sum with other Negative Indicators
  mutate("n_mood_down" = mood_down + 4,
         "n_mood_lonely" = mood_lonely + 4,
         "n_mood_anxious" = mood_anxious + 4,
         "n_mood_guilty" = mood_guilty + 4
         ) %>%
  

# Create Groupong variables for Positive Mood and Self Esteem Indicators, 
# Negative Mood and Self Esteem Indicators, and Physical Indicators
  
  mutate(n_Neg_Ind = n_mood_down+mood_irritat+n_mood_lonely+
                      n_mood_anxious+mood_suspic+n_mood_guilty+
                      mood_doubt+pat_restl+pat_agitate+pat_worry+
                      se_ashamed+se_selfdoub) %>%

 
  mutate( n_Pos_Ind = mood_relaxed+mood_satisfi+mood_enthus+
                      mood_cheerf+mood_strong+pat_concent+
                      se_selflike+se_handle) %>%
    
  mutate(n_Physical_Ind = phy_hungry+phy_tired+phy_pain+
                          phy_dizzy+phy_drymouth+phy_nauseous+
                          phy_headache+phy_sleepy) 
```

Again, these transformations and groupings were executed purely to take a "first look" at the data.

Now let's take a peek at the distribution of our newly grouped variables.

``` r
#Looking at the overall distribution of our new grouped variables
# par(mfrow = c(2,1)) tried this but it didnt do anything
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Sum_dep), fill = "red")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1289 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/GroupedHist-1.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Neg_Ind), fill = "red")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 4 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/GroupedHist-2.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Pos_Ind), fill = "red")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 4 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/GroupedHist-3.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Physical_Ind), fill = "red")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 5 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/GroupedHist-4.png)

Nothing too unusual here. n\_Sum\_dep could possibly be bimodal, but otherwise spread relatively evenly. The n\_Neg\_Ind values appear normal-ish, possibly right skewed. n\_Pos\_Ind values again appear bimodal. And interestingly enough the n\_Physical\_Ind values give the appearance of a Poisson distribution.

These variations in the distributions do not concern me. Because we are really looking at a five phase experiment we need to look at the distributions with respect to the different phases.

To review the phases for a moment, the researchers indicate that we have five phases. But to me it seems like we really have three. Phases one and two are baseline, with the medication being constant at 150 mg. And, phase two was only two weeks long so I'm not sure of its value. Phase 3 is obviously distinct because this is where the medication dosage is reduced. For the same reasons given above for phases one and two, I believe we need to consider phases four and five to be the same. In both of these phases the dosage of the medication is zero. For now we will keep the phases intact and observe the differences.

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Sum_dep), fill = "blue") +
facet_grid(phase ~ .)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1289 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/nSumDep_HistoFacet-1.png)

The faceted view of n\_Sum\_dep seems to show a generalized increase in the depression score as the patient progresses through the phases of the experiment. But of note also is the decreased number of observations in phases one and two.

To review the depression score questions were only answered once per week. Phase two lasted for two weeks, so only one questionnaire was answered to represent the entire period. This does not adequately or accurately reflect the patient's depression for that period of time.

As stated above it may become necessary to combine phases one and two, because the depression medication is constant at 150 mg. Along with phases four and five because the medication has been eliminated.

Immediately though, I feel we need to "up-fill" all the SLC.90.R question values and recalculate the sum of the values

``` r
UpFill_Grouped <- Grouped_DatDisc %>%
  fill(SCL.90.R.14 : dep, .direction = c("up")) %>%
  mutate(ufill_Sum_dep =                                                      SCL.90.R.14+SCL.90.R.20+SCL.90.R.22+SCL.90.R.5+
           SCL.90.R.29+SCL.90.R.26+SCL.90.R.15+SCL.90.R.30+
           SCL.90.R.31+SCL.90.R.31+SCL.90.R.32+SCL.90.R.54+
           SCL.90.R.71+SCL.90.R.79)
ggplot(UpFill_Grouped) + geom_histogram(aes(x = ufill_Sum_dep),           fill = "blue") +
       facet_grid(phase ~ .)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Data_Discovery_files/figure-markdown_github/Upfill_SumDep_HistoFacet-1.png)

``` r
ggplot(UpFill_Grouped) + geom_histogram(aes(x = n_Neg_Ind), fill = "blue") +
facet_grid(phase ~ .)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 4 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/nNegInd_HistoFacet-1.png)

The negative mood indicators, n\_Neg\_Ind, is interesting. Rembering these questions were valued from 1 to 7, with 1 being not and 7 being very. I would have expected more of a shift in the distributions of phases four and five when the patient was completely off the medication. The only thing I see is a heavier tail on the right of the phase five distribution.

``` r
ggplot(UpFill_Grouped) + geom_histogram(aes(x = n_Pos_Ind), fill = "blue") +
facet_grid(phase ~ .)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 4 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/nPosInd_HistoFacet-1.png)

The positive mood indicators, n\_Pos\_Ind, could be expected. I might have expected more of a shift in the distributions to the left in phases four and five. But the left skew and heavier tail on the left tells me something is going on.

``` r
ggplot(UpFill_Grouped) + geom_histogram(aes(x = n_Physical_Ind), fill = "blue") +
facet_grid(phase ~ .)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 5 rows containing non-finite values (stat_bin).

![](Data_Discovery_files/figure-markdown_github/nPhysicalInd_Histofacet-1.png)

The physical indicator grouping, n\_Physical\_Ind, at first glance may be what you expect. It would be reasonable to expect for feelings such as pain, dizziness, and headaches might increase after being taken off of the medication. However in phase five the Poisson distribution really begins to take shape. My suspicion is that this is the result of the side effects of the medication no longer being present and may also have no relation to depression or predicting depressive episodes.

Time Series Scatter Plots
-------------------------

``` r
#Time series scatterplots
ggplot(UpFill_Grouped, aes(x = datetime, y = ufill_Sum_dep, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

![](Data_Discovery_files/figure-markdown_github/GroupedVsTime_Scatter-1.png)

``` r
ggplot(UpFill_Grouped, aes(x = datetime, y = n_Neg_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/GroupedVsTime_Scatter-2.png)

``` r
ggplot(UpFill_Grouped, aes(x = datetime, y = n_Pos_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/GroupedVsTime_Scatter-3.png)

``` r
ggplot(UpFill_Grouped, aes(x = datetime, y = n_Physical_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/GroupedVsTime_Scatter-4.png)

As you can see I have colored the points by phase. Nothing new is revealed here that was not discovered by examing the distributions of the variables.

Since the initial plan is to use the depression score, ufill\_Sum\_dep, to predict or detect a depressive episode, I think we need to examine scatterplots of ufill\_Sum\_dep vs the other grouping factors. Additionally, facets need to be added to the graphs to see the true impact of the experiment phase.

``` r
ggplot(UpFill_Grouped, aes(x = n_Neg_Ind, y = ufill_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/SumDepVsNegInd_byPhase-1.png)

Okay! Now we are starting to see some things. Because of the shortness of phase two there was only one weekly depression survey completed. 'Upfilling' the data did bring in an additional survey's results. We may need to go in another direction with the analysis, but let's move on and see what else we can find.

Phases three and four are interesting in that they show two clusters. In other words, for the same n\_Neg\_Ind value the the value of n\_Sum\_dep can vary by 10 or more points. Phase five is the tightest group and also contains several outliers. Also in phase five, the trendline seems to indicate that as the n\_Neg\_Ind score increases the depression score decreases slightly.

``` r
ggplot(UpFill_Grouped, aes(x = n_Pos_Ind, y = ufill_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/SumDepVsPosInd_byPhase-1.png)

Here again we see the same clustering pattern in phases three and four. While the depression scores of phase five are elevated more than the other four phases it shows another counterintuitive trend. As the positive feeling scores increase, so does the depression score

``` r
ggplot(UpFill_Grouped, aes(x = n_Physical_Ind, y = ufill_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](Data_Discovery_files/figure-markdown_github/SumDepVsPhysInd_byPhase-1.png)

Same clustering pattern noted previously for phases three and four noted. Again, the counterintuitive trend in phase five continues. As the adverse physical symptoms increase, the depression score decreases.

Next Steps
==========

After haven taken a higher level look at the data it's time to take a deeper dive and do a little feature engineering. Here I will use regression techniques (linear and logistic) as well as analysis of variance to see if I can eliminate unnecessary variables, or recode others in order to build a predictive model for depressive episode identification.
