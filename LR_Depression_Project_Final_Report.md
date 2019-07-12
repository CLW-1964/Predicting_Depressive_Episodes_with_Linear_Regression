Capstone Project Final Report
=============================

*Predicting or Detecting a Depressive Episode in a Patient with a History of Chronic Depression*
================================================================================================

Overview
--------

It is estimated that 25 million Americans suffer from depression each year and over 50% of all people who die by suicide suffer from major depression. Additionally, depression is among the most treatable of psychiatric illnesses with 80% to 90% responding positively to treatment.

The goal of this preliminary study was to determine if they could identify a depressive episode in a patient who wished to be taken off of antidepressant medication after an 8.5-year history of chronic depression.

This study is unusual in that there is only one subject. Furthermore, the idea for the study was presented by this subject. Which means the results of this study are only germane to this particular patient. But it can serve as the framework for larger future experiments.

Mental health, primary healthcare providers and patients would all from using this information. using this information. The ability to identify a new depressive episode and intervene appropriately could save untold numbers of lives.

The Data
--------

The dataset contains the results of a series of questions that were asked of the patient several times a day, daily and weekly over the course of 239 days. The study was broken down into five phases:

-   Phase 1 - Baseline period lasting four weeks where the medication dosage was kept constant 150 mg
-   Phase 2 - The researchers called this a double-blind phase. The medication dosage was not modified and lasted for two weeks
-   Phase 3 - A double-blind period lasting eight weeks where the medication dosage was gradually decreased on a schedule randomly chosen by a pharmacist
-   Phase 4 - A post-assessment period lasting eight weeks in which the patient was completely off the medication
-   Phase 5 - A twelve-week follow-up period.

With over 80 variables the potential for multicollinearity is high so reasonable effort will be focused on determining the best variables for testing using linear regression.

Data Wrangling
--------------

``` r
library(tidyverse)
library(Hmisc)
library(quantreg)
library(RColorBrewer)
library(GGally)
library(ggvis)
library(corrgram)       
library(corrplot)

ESMdata <- read_csv("ESMdata.csv")

ESMdata %>% glimpse()
```

    ## Observations: 1,476
    ## Variables: 86
    ## $ X1              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,...
    ## $ date            <chr> "13/08/12", "14/08/12", "14/08/12", "14/08/12"...
    ## $ phase           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ concentrat      <int> 150, 150, 150, 150, 150, 150, 150, 150, 150, 1...
    ## $ dayno           <int> 226, 227, 227, 227, 227, 227, 228, 228, 228, 2...
    ## $ beepno          <int> 1, 5, 6, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 10,...
    ## $ beeptime        <time> 08:58:00, 14:32:00, 16:17:00, 18:04:00, 20:57...
    ## $ resptime_s      <time> 08:58:56, 14:32:09, 16:17:13, 18:04:10, 20:58...
    ## $ resptime_e      <time> 09:00:15, 14:33:25, 16:23:16, 18:06:29, 21:00...
    ## $ resp_abort      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ mood_relaxed    <int> 5, 4, 4, 4, 4, 5, 5, 3, 3, 3, 2, 4, 4, 3, 4, 5...
    ## $ mood_down       <int> -1, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, -1, 0...
    ## $ mood_irritat    <int> 1, 3, 2, 1, 2, 1, 1, 1, 3, 2, 4, 3, 2, 3, 2, 2...
    ## $ mood_satisfi    <int> 5, 3, 3, 4, 4, 4, 3, 3, 3, 4, 2, 3, 4, 3, 1, 4...
    ## $ mood_lonely     <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ mood_anxious    <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0,...
    ## $ mood_enthus     <int> 4, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3...
    ## $ mood_suspic     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1...
    ## $ mood_cheerf     <int> 5, 4, 4, 4, 4, 3, 4, 3, 3, 3, 3, 3, 4, 3, 4, 3...
    ## $ mood_guilty     <int> -1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, ...
    ## $ mood_doubt      <int> 1, 1, 2, 1, 2, 2, 3, 4, 3, 3, 2, 2, 2, 3, 1, 3...
    ## $ mood_strong     <int> 5, 4, 4, 4, 3, 3, 4, 3, 4, 4, 3, 3, 3, 3, 4, 3...
    ## $ pat_restl       <int> 1, 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 2, 3, 2, 1, 2...
    ## $ pat_agitate     <int> 1, 2, 1, 1, 1, 1, 1, 1, 3, 2, 3, 3, 2, 3, 1, 2...
    ## $ pat_worry       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ pat_concent     <int> 5, 4, 3, 4, 4, 3, 4, 4, 4, 3, 4, 4, 4, 3, 3, 4...
    ## $ se_selflike     <int> 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4...
    ## $ se_ashamed      <int> 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ se_selfdoub     <int> 1, 2, 1, 1, 3, 2, 2, 2, 3, 2, 3, 1, 2, 3, 1, 2...
    ## $ se_handle       <int> 5, 4, 4, 4, 4, 3, 4, 3, 4, 4, 4, 3, 3, 3, 4, 3...
    ## $ soc_who1        <int> 10, 0, 19, 10, 10, 10, 0, 0, 0, 0, 0, 0, 10, 1...
    ## $ soc_enjoy_alone <int> NA, 5, NA, NA, NA, NA, 4, 4, 4, 4, 4, 4, NA, N...
    ## $ soc_prefcomp    <int> NA, 1, NA, NA, NA, NA, 1, 2, 1, 2, 1, 3, NA, N...
    ## $ soc_who2        <int> 0, NA, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, 0, ...
    ## $ soc_who3        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ soc_belong      <int> 1, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, 1, ...
    ## $ soc_pleasant    <int> 5, NA, 5, 6, 6, 5, NA, NA, NA, NA, NA, NA, 5, ...
    ## $ soc_prefalone   <int> 1, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, 2, ...
    ## $ soc_together    <int> 4, NA, 5, 4, 6, 3, NA, NA, NA, NA, NA, NA, 3, ...
    ## $ phy_hungry      <int> 1, 1, 1, 2, 4, 1, 1, 1, 3, 4, 2, 2, 2, 4, 1, 3...
    ## $ phy_tired       <int> 1, 2, 1, 2, 3, 3, 2, 3, 2, 2, 2, 3, 2, 4, 2, 2...
    ## $ phy_pain        <int> 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2...
    ## $ phy_dizzy       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ phy_drymouth    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ phy_nauseous    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ phy_headache    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2...
    ## $ phy_sleepy      <int> 1, 1, 1, 1, 3, 2, 1, 3, 1, 1, 1, 1, 1, 3, 1, 3...
    ## $ act_what1       <int> 88, 10, 45, 45, 60, 45, 45, 0, 10, 45, 10, 10,...
    ## $ act_what2       <int> 45, 0, 0, 0, 51, 0, 0, NA, 0, 60, 0, 0, 60, 51...
    ## $ act_difficul    <int> 1, 4, 2, 1, 2, 2, 1, 3, 4, 3, 5, 5, 2, 3, 1, 1...
    ## $ act_well        <int> 5, 4, 5, 5, 5, 4, 5, 4, 4, 5, 4, 4, 5, 5, 5, 4...
    ## $ act_enjoy       <int> 5, 3, 4, 5, 5, 4, 3, 3, 3, 4, 2, 3, 5, 4, 4, 4...
    ## $ phy_physact     <int> 3, 4, 1, 4, 4, 1, 3, 2, 2, 1, 1, 2, 2, 2, 1, 2...
    ## $ event_pleas     <int> 1, 0, 0, 0, 3, 1, 0, 0, -1, -1, -2, -2, -1, -1...
    ## $ event_import    <int> 2, 0, 0, 1, 2, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1...
    ## $ event_cause     <int> 1, 5, 5, 2, 1, 3, 4, 4, 3, 2, 3, 3, 3, 3, 3, 4...
    ## $ event_concern   <int> 2, 6, 6, 1, 1, NA, NA, NA, NA, 4, NA, NA, NA, ...
    ## $ event_freq      <int> NA, NA, NA, NA, NA, 2, NA, NA, 1, NA, 1, 1, 1,...
    ## $ event_pertain   <int> NA, NA, NA, NA, NA, NA, 2, 2, NA, NA, NA, NA, ...
    ## $ event_disturb   <int> 3, 6, 4, 4, 6, 6, 5, 5, 7, 7, 7, 7, 6, 6, 7, 5...
    ## $ evn_ordinary    <int> 3, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4...
    ## $ evn_niceday     <int> 4, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5...
    ## $ evn_inflmood    <int> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3...
    ## $ evn_pager       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ evn_work        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1...
    ## $ evn_med         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ mor_asleep      <int> 3, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ mor_nrwakeup    <int> 1, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ mor_lieawake    <int> 2, 3, 3, 3, 3, 3, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ mor_qualsleep   <int> 5, 6, 6, 6, 6, 6, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ mor_feellike    <int> 5, 5, 5, 5, 5, 5, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ mor_med         <int> 1, 1, 1, 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ SCL.90.R.14     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.20     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.22     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.5      <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.29     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.26     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.15     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.30     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.31     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.32     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.54     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.71     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ SCL.90.R.79     <int> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ dep             <dbl> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...

In general, the dataset was relatively clean.

-   The date variable was reformatted from a character to date format
-   New variable created for calculating the amount of time it took the patient to begin taking the survey
-   A Date/Time field was created for performing time series analysis
-   New variable created for calculating how long it took him to take the survey

``` r
ESMdata <- ESMdata %>%
  mutate(date = as.Date(date, "%d/%m/%y"),
         survey_time_to_take = resptime_e - resptime_s,
         datetime = as.POSIXct(paste(ESMdata$date, ESMdata$beeptime), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
         survey_time_to_start = resptime_s - beeptime)
```

There were however a large number of “NA’s” contained in the dataset.

``` r
ESMdata %>% 
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
    ## 88 datetime                  1476
    ## 89 survey_time_to_start         0

This is understandable because the dataset is comprised of responses to questions that are answered at different intervals:

-   Several times a day
-   Once a day
-   Once a week

Additionally, there are questions that will only be answered based upon the choice of a previous question. At this phase of the project I left the NA’s intact.

Data Discovery
--------------

``` r
DatDisc <- read_csv("ESMdata_clean3.csv")
```

Since the goal is to determine if we can predict depressive episodes, it may seem natural to use the dep variable for comparison purposes. The dep variable is the average score of the 13 questions from the depression subscale of the SCL-90-R. However, I am not a fan of averages in general and decided to create a new field which is the sum of the values for comparison purposes.

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

All questions in these groupings were measured using one of two scales. A Likert scale ranging from –3 to 3 or a 7-point Likert scale ranging from 1 to 7. Because I chose to group into these three categories by summing the values of their individual scores, I chose to create some new variables and transform the -3 to 3 Likert scale questions to the 7-point Likert scale.

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

First, we examine the distributions of the newly created grouped variables

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Sum_dep), fill = "red")
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedHistFinal-1.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Neg_Ind), fill = "red")
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedHistFinal-2.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Pos_Ind), fill = "red")
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedHistFinal-3.png)

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Physical_Ind), fill = "red")
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedHistFinal-4.png)

Nothing too unusual here. n\_Sum\_dep could possibly be bimodal, but otherwise spread relatively evenly. The n\_Neg\_Ind values appear normal-ish, possibly right skewed. n\_Pos\_Ind values again appear bimodal. And interestingly enough the n\_Physical\_Ind values give the appearance of a Poisson distribution.

These variations in the distributions are not concerning. Because we are really looking at a five-phase experiment we need to look at the distributions with respect to the different phases.

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Sum_dep), fill = "blue") +
facet_grid(phase ~ .)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/nSumDep_HistoFacetFinal-1.png)

The faceted view of n\_Sum\_dep seems to show a generalized increase in the depression score as the patient progresses through the phases of the experiment.

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Neg_Ind), fill = "blue") + labs(x = "Negative Mood Indicator Score",  title = "Negative Mood Score Distribution by Experiment Phase") +
  facet_grid(phase ~ .)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/nNegInd_HistoFacetFinal-1.png)

The negative mood indicators, n\_Neg\_Ind, is interesting. These questions were valued from 1 to 7, with 1 being not and 7 being very. I would have expected more of a shift in the distributions of phases four and five when the patient was completely off the medication. The only thing I see is a heavier tail on the right of the phase five distribution.

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Pos_Ind), fill = "blue") + labs(x = "Positive Mood Indicator Score", title = "Positive Mood Score Distribution by Experiment Phase") +
  facet_grid(phase ~ .)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/nPosgInd_HistoFacetFinal-1.png)

For the positive mood indicators, n\_Pos\_Ind, I might have expected more of a shift in the distributions to the left in phases four and five. But the left skew and heavier tail on the left tells me something is going on.

``` r
ggplot(Grouped_DatDisc) + geom_histogram(aes(x = n_Physical_Ind), fill = "blue") + labs(x = "Physical Symptoms Indicator Score",  title = "Physical Symptoms Score Distribution by Experiment Phase") +
  facet_grid(phase ~ .)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/nPhysicalInd_HistofacetFinal-1.png)

The physical indicator grouping, n\_Physical\_Ind, at first glance may be what you expect. It would be reasonable to expect for feelings such as pain, dizziness, and headaches might increase after being taken off of the medication. However, in phase five the Poisson distribution really begins to take shape. My suspicion is that this is the result of the side effects of the medication no longer being present and may also have no relation to depression or predicting depressive episodes.

``` r
#Time series scatterplots
ggplot(Grouped_DatDisc, aes(x = datetime, y = n_Sum_dep, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  labs(x = "Date", y = "Depression Score",  title = "Depression Score vs Time", colour = "Phase") +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedVsTime_ScatterFinal-1.png)

``` r
ggplot(Grouped_DatDisc, aes(x = datetime, y = n_Neg_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  labs(x = "Date", y = "Negative Mood Indicator Score",  title = "Negative Mood Score vs Time", colour = "Phase") +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedVsTime_ScatterFinal-2.png)

``` r
ggplot(Grouped_DatDisc, aes(x = datetime, y = n_Pos_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  labs(x = "Date", y = "Positive Mood Indicator Score",  title = "Positive Mood Score vs Time", colour = "Phase") +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedVsTime_ScatterFinal-3.png)

``` r
ggplot(Grouped_DatDisc, aes(x = datetime, y = n_Physical_Ind, color = factor(phase))) +
  geom_point()+
  geom_jitter() +
  labs(x = "Date", y = "Physical Symptoms Indicator Score",  title = "Physical Symptoms Score vs Time", colour = "Phase") +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/GroupedVsTime_ScatterFinal-4.png)

Since the initial plan was to use the grouped depression score, n\_Sum\_dep, to predict or detect a depressive episode, we will next examine scatterplots of n\_Sum\_dep vs the other grouping factors. Additionally, facets need to be added to the graphs to see the true impact of the experiment phase.

``` r
ggplot(Grouped_DatDisc, aes(x = n_Neg_Ind, y = n_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/SumDepVsNegInd_byPhaseFinal-1.png)

Now we are starting to see some things. Because of the shortness of phase two there was only one weekly depression survey completed. Phases three and four are interesting in that they show two clusters. In other words, for the same n\_Neg\_Ind value the value of n\_Sum\_dep can vary by 10 or more points. Phase five is the tightest group and also contains several outliers. Also, in phase five, the trendline seems to indicate that as the n\_Neg\_Ind score increases the depression score decreases slightly.

``` r
ggplot(Grouped_DatDisc, aes(x = n_Pos_Ind, y = n_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/SumDepVsPosInd_byPhaseFinal-1.png)

Here again we see the same clustering pattern in phases three and four. While the depression scores of phase five are elevated more than the other four phases it shows another counterintuitive trend. As the positive feeling scores increase, so does the depression score.

``` r
ggplot(Grouped_DatDisc, aes(x = n_Physical_Ind, y = n_Sum_dep, color = factor(phase))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ phase)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/SumDepVsPhysInd_byPhaseFinal-1.png)

Same clustering pattern noted previously for phases three and four noted. Again, the counterintuitive trend in phase five continues. As the adverse physical symptoms increase, the depression score decreases.

After haven taken a higher-level look at the data it's time to take a deeper dive and do a little feature engineering. Here I will use linear regression and analysis of variance to see if I can eliminate unnecessary variables or recode others in order to build a predictive model for depressive episode identification.

ANOVA by Phase of the Experiment
--------------------------------

Previously I noted that the values of the grouping variables changed during the different phases of the experiment. Now let's take a look at the primary variable of interest, the depression index, and see if it differs significantly over the five phases of the experiment

``` r
u_fit <- aov(n_Sum_dep ~ factor(phase), Grouped_DatDisc)
anova(u_fit)
```

    ## Analysis of Variance Table
    ## 
    ## Response: n_Sum_dep
    ##                Df Sum Sq Mean Sq F value    Pr(>F)    
    ## factor(phase)   4 4531.2  1132.8  63.295 < 2.2e-16 ***
    ## Residuals     182 3257.3    17.9                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(u_fit)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = n_Sum_dep ~ factor(phase), data = Grouped_DatDisc)
    ## 
    ## $`factor(phase)`
    ##           diff       lwr       upr     p adj
    ## 2-1 -3.2727273 -8.365783  1.820329 0.3939790
    ## 3-1 -0.3707665 -4.245836  3.504304 0.9989296
    ## 4-1  5.5157343  1.647281  9.384188 0.0011349
    ## 5-1 10.7590188  6.949989 14.568049 0.0000000
    ## 3-2  2.9019608 -1.129338  6.933259 0.2782292
    ## 4-2  8.7884615  4.763522 12.813401 0.0000001
    ## 5-2 14.0317460 10.063886 17.999606 0.0000000
    ## 4-3  5.8865008  3.589310  8.183691 0.0000000
    ## 5-3 11.1297852  8.934140 13.325431 0.0000000
    ## 5-4  5.2432845  3.059337  7.427232 0.0000000

Looking at the Analysis of Variance Table we see that the overall model has an extremely low Pr(&gt;F) value.

To dive deeper we next perform Tukey multiple comparisons of the means. Here we see phases one, two and three are not statistically significantly different. However, phases four and five are significantly different than all other levels.

For visual reference, let’s take a look at the 95% CI for the differences in means between phases and boxplots

``` r
## Visual conformation
plot(TukeyHSD(u_fit))
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/BoxplotDepVsPhaseFinal-1.png)

``` r
ggplot(Grouped_DatDisc, aes( factor(phase) , n_Sum_dep)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 2) +
  geom_jitter(width = 0.1) 
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/BoxplotDepVsPhaseFinal-2.png)

The boxplots confirm what we saw in the faceted scatterplots. Phases three and four each have two clusters of data points, and phases four and five show elevated depression scores.

Dealing with the Missing Values
-------------------------------

Missing values, NA's, were not a big problem in the initial data discovery phase because we were essentially looking for patterns in the data to help determine the next steps for analysis. Here we can see the overwhelming majority of our grouped depression scores are missing.

``` r
Grouped_DatDisc %>% count(n_Sum_dep)
```

    ## # A tibble: 18 x 2
    ##    n_Sum_dep     n
    ##        <int> <int>
    ##  1        14    13
    ##  2        15    27
    ##  3        16     6
    ##  4        17     6
    ##  5        18    13
    ##  6        19     7
    ##  7        20     7
    ##  8        23     8
    ##  9        24     8
    ## 10        25     9
    ## 11        26     6
    ## 12        27    12
    ## 13        28    21
    ## 14        29    13
    ## 15        30    14
    ## 16        31     8
    ## 17        36     9
    ## 18        NA  1289

Briefly, the SCL.90.R questions which were used to calculate n\_Sum\_dep relate to how the patient felt over the *previous* seven day period.

Instead of upfilling the depression scores to match the grouped scores of the previous week, I will create a new field for the week in question, then create a new data frame that groups by the new week.

I will use the means of the independent variables I intend to regress upon the dependent depression score variable.

``` r
# first create the  two day offset in the day_since_start field to ensure 
# the depression scores are looking backward
Grouped_DatDisc <- Grouped_DatDisc %>%
  mutate(day_since_start = date - date[2],
         week_group = floor(day_since_start/7) + 1) 
```

We now have a group\_by variable, week\_group. Now create a new data frame to look at the average of the grouped mood and physical indicators that were created earlier as a first look.

If any of the grouped variables show a strong correlation they will be broken down into their individual components to see if a stronger model can be built.

``` r
# New DF for the grouping by week of the grouped mood indicators
WeeklyGroupedInd <- Grouped_DatDisc %>% 
  group_by(week_group) %>% 
  summarise(mean_negmood_ind = mean(n_Neg_Ind, na.rm = TRUE),
            mean_posmood_ind = mean(n_Pos_Ind, na.rm = TRUE),
            mean_physym_ind = mean(n_Physical_Ind, na.rm = TRUE),
            mean_dep_score = mean(n_Sum_dep, na.rm = TRUE)) %>% 
  arrange(week_group) %>%
  print(n = Inf)
```

    ## # A tibble: 35 x 5
    ##    week_group mean_negmood_ind mean_posmood_ind mean_physym_ind
    ##    <time>                <dbl>            <dbl>           <dbl>
    ##  1 0                      20               39              8   
    ##  2 1                      28.6             30.7           11.4 
    ##  3 2                      28.2             32.3           11.3 
    ##  4 3                      27.4             33.5           11.0 
    ##  5 4                      27.2             34.2           10.7 
    ##  6 5                      27.6             34.4           11.0 
    ##  7 6                      27.7             33.7           11.3 
    ##  8 7                      27.4             33.1           11.3 
    ##  9 8                      31.2             31.8           11.7 
    ## 10 9                      26.9             36.1           11.1 
    ## 11 10                     28.4             33.5           11.6 
    ## 12 11                     31.2             31.6           11.7 
    ## 13 12                     28.0             34.9           10.7 
    ## 14 13                     33.6             30.8           10.7 
    ## 15 14                     28.9             34.2           11.0 
    ## 16 15                     30.5             33.0           11.7 
    ## 17 16                     31.5             31.4           10.5 
    ## 18 17                     31.4             33.2            9.72
    ## 19 18                     26.8             36.5           10.2 
    ## 20 19                     31.1             33.7            9.57
    ## 21 20                     32.2             31.7           10.1 
    ## 22 21                     30.8             32.5           10.6 
    ## 23 22                     31.8             33.4           10.4 
    ## 24 23                     34.1             32.0           10.2 
    ## 25 24                     34.6             31.6           10.7 
    ## 26 25                     32.5             33.3           10.5 
    ## 27 26                     28.7             35.3            9.47
    ## 28 27                     30.6             34             10.3 
    ## 29 28                     29.2             34.4           10.1 
    ## 30 29                     30.0             34.4            9.5 
    ## 31 30                     36.2             30.6           10.6 
    ## 32 31                     30.1             34.4           10.2 
    ## 33 32                     30.8             33.7           11.1 
    ## 34 33                     30.7             34.6            9.93
    ## 35 34                     33.8             32.9           10.5 
    ## # ... with 1 more variable: mean_dep_score <dbl>

Multiple linear regression of the grouped variables
---------------------------------------------------

``` r
lmFitWeeklyGroupedInd <- lm(mean_dep_score ~ mean_negmood_ind + 
                              mean_posmood_ind + mean_physym_ind,
                            WeeklyGroupedInd)
summary(lmFitWeeklyGroupedInd)
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_negmood_ind + mean_posmood_ind + 
    ##     mean_physym_ind, data = WeeklyGroupedInd)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.0184 -1.2788  0.3161  2.2323  6.2969 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -7.8999    36.5514  -0.216 0.830711    
    ## mean_negmood_ind   1.9545     0.3422   5.712 6.95e-06 ***
    ## mean_posmood_ind   0.3099     0.6446   0.481 0.635067    
    ## mean_physym_ind   -3.6478     0.9657  -3.777 0.000923 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.2 on 24 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7845, Adjusted R-squared:  0.7575 
    ## F-statistic: 29.12 on 3 and 24 DF,  p-value: 3.624e-08

The mean\_negmood\_ind variable shows a strong positive correlation, mean\_physym\_ind show a strong negative correlation. The overall model Adjusted R-squared is 0.7575.

Remove mean\_posmood\_ind to see if it strengthens the model.

``` r
lmFitWeeklyGroupedInd2 <- lm(mean_dep_score ~ mean_negmood_ind + 
                            mean_physym_ind, WeeklyGroupedInd)          
summary(lmFitWeeklyGroupedInd2)
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_negmood_ind + mean_physym_ind, 
    ##     data = WeeklyGroupedInd)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4565 -1.2996  0.3418  2.2971  6.0909 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        9.1151     8.9812   1.015     0.32    
    ## mean_negmood_ind   1.8217     0.1988   9.163 1.82e-09 ***
    ## mean_physym_ind   -3.8993     0.7992  -4.879 5.10e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.151 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7824, Adjusted R-squared:  0.765 
    ## F-statistic: 44.94 on 2 and 25 DF,  p-value: 5.259e-09

Uncouple the grouped variables for the negative mood and physical symptom indicators since that pair had the highest Adjusted R-square and perform linear regression of the individual scores against the depression score

``` r
WeeklyNegPhys <- Grouped_DatDisc %>% 
  group_by(week_group) %>% 
  summarise(mean_n_mood_down = mean(n_mood_down, na.rm = TRUE),
            mean_mood_irritat = mean(mood_irritat, na.rm = TRUE),
            mean_n_mood_lonely = mean(n_mood_lonely, na.rm = TRUE),
            mean_n_mood_anxious = mean(n_mood_anxious, na.rm = TRUE),
            mean_mood_suspic = mean(mood_suspic, na.rm = TRUE),
            mean_n_mood_guilty = mean(n_mood_guilty, na.rm = TRUE),
            mean_mood_doubt = mean(mood_doubt, na.rm = TRUE),
            mean_pat_restl = mean(pat_restl, na.rm = TRUE),
            mean_pat_agitate = mean(pat_agitate, na.rm = TRUE),
            mean_pat_worry = mean(pat_worry, na.rm = TRUE),
            mean_se_ashamed = mean(se_ashamed, na.rm = TRUE),
            mean_se_selfdoub = mean(se_selfdoub, na.rm = TRUE),
            
            #Physical Indicators
            mean_phy_hungry = mean(phy_hungry, na.rm = TRUE),
            mean_phy_tired = mean(phy_tired, na.rm = TRUE),
            mean_phy_pain = mean(phy_pain, na.rm = TRUE),
            mean_phy_dizzy = mean(phy_dizzy, na.rm = TRUE),
            mean_phy_drymouth = mean(phy_drymouth, na.rm = TRUE),
            mean_phy_nauseous = mean(phy_nauseous, na.rm = TRUE),
            mean_phy_headache = mean(phy_headache, na.rm = TRUE),
            mean_phy_sleepy = mean(phy_sleepy, na.rm = TRUE),
            mean_dep_score = mean(n_Sum_dep, na.rm = TRUE),
            
            #morning questions to assess sleep
            mean_mor_asleep = mean(mor_asleep, na.rm = TRUE),
            mean_mor_nrwakeup = mean(mor_nrwakeup, na.rm = TRUE),
            mean_mor_lieawake = mean(mor_lieawake, na.rm = TRUE),
            mean_mor_qualsleep = mean(mor_qualsleep, na.rm = TRUE)
              ) %>% 
  arrange(week_group) 
```

First, we will look at the negative mood indicators vs the depression score to see if we can improve upon the grouped negative mood and physical symptom indicators model Adjusted R-squared of 0.7575.

``` r
lmnp1 <- lm(mean_dep_score ~ mean_n_mood_down, WeeklyNegPhys)
summary(lmnp1) # R^2 = 0.59 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_n_mood_down, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5335 -3.8414 -0.1987  3.2596  8.0041 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -37.617      9.575  -3.929 0.000563 ***
    ## mean_n_mood_down   14.538      2.302   6.315  1.1e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.161 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.6053, Adjusted R-squared:  0.5901 
    ## F-statistic: 39.87 on 1 and 26 DF,  p-value: 1.099e-06

``` r
lmnp1 <- lm(mean_dep_score ~ mean_mood_irritat, WeeklyNegPhys)
summary(lmnp1) #R^2 = 0.3214 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_mood_irritat, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.0870  -4.0353   0.7007   4.1949   8.7166 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.610      5.489   0.476 0.638356    
    ## mean_mood_irritat    8.775      2.363   3.713 0.000983 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.354 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.3465, Adjusted R-squared:  0.3214 
    ## F-statistic: 13.79 on 1 and 26 DF,  p-value: 0.000983

``` r
lmnp1 <- lm(mean_dep_score ~ mean_n_mood_lonely, WeeklyNegPhys)
summary(lmnp1) # R^2 = 0.4824 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_n_mood_lonely, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4100 -3.8268 -0.3814  3.6462  9.3750 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -51.931     14.605  -3.556  0.00147 ** 
    ## mean_n_mood_lonely   18.852      3.685   5.116 2.48e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.676 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.5016, Adjusted R-squared:  0.4824 
    ## F-statistic: 26.17 on 1 and 26 DF,  p-value: 2.478e-05

``` r
lmnp1 <- lm(mean_dep_score ~ mean_n_mood_anxious , WeeklyNegPhys)
summary(lmnp1) # R^2 = 0.2699 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_n_mood_anxious, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.8744 -5.1066 -0.3754  4.6325 11.1246 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)          -45.861     20.699  -2.216  0.03568 * 
    ## mean_n_mood_anxious   17.684      5.337   3.314  0.00271 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.553 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.2969, Adjusted R-squared:  0.2699 
    ## F-statistic: 10.98 on 1 and 26 DF,  p-value: 0.002713

``` r
lmnp1 <- lm(mean_dep_score ~ mean_mood_suspic, WeeklyNegPhys)
summary(lmnp1) # R^2=0.6599 significant ********************************
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_mood_suspic, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1570 -2.8956 -0.2419  2.4354  8.2658 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -3.020      3.585  -0.843    0.407    
    ## mean_mood_suspic   19.925      2.727   7.306 9.27e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.79 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.6725, Adjusted R-squared:  0.6599 
    ## F-statistic: 53.38 on 1 and 26 DF,  p-value: 9.274e-08

``` r
lmnp1 <- lm(mean_dep_score ~ mean_n_mood_guilty, WeeklyNegPhys)
summary(lmnp1) #R^2 =0.3844 sig
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_n_mood_guilty, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.9520 -5.0250 -0.5038  3.8098  8.7861 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -41.828     15.286  -2.736 0.011048 *  
    ## mean_n_mood_guilty   16.195      3.832   4.226 0.000259 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.099 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.4072, Adjusted R-squared:  0.3844 
    ## F-statistic: 17.86 on 1 and 26 DF,  p-value: 0.0002587

``` r
lmnp1 <- lm(mean_dep_score ~ mean_mood_doubt, WeeklyNegPhys)
summary(lmnp1) # R^2 = 0.2424 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_mood_doubt, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5942 -4.4187 -0.6438  5.1478  8.9611 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)        3.928      6.122   0.642  0.52669   
    ## mean_mood_doubt   10.074      3.245   3.105  0.00456 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.657 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.2705, Adjusted R-squared:  0.2424 
    ## F-statistic:  9.64 on 1 and 26 DF,  p-value: 0.004556

``` r
lmnp1 <- lm(mean_dep_score ~  mean_pat_restl, WeeklyNegPhys)
summary(lmnp1) #R^2 =0.3092 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_restl, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.0066  -4.2005   0.8364   4.0046   8.5130 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       3.484      5.394   0.646  0.52394   
    ## mean_pat_restl    9.121      2.521   3.617  0.00126 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.402 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.3348, Adjusted R-squared:  0.3092 
    ## F-statistic: 13.09 on 1 and 26 DF,  p-value: 0.001257

``` r
lmnp1 <- lm(mean_dep_score ~ mean_pat_agitate, WeeklyNegPhys)
summary(lmnp1) #R^2 = 0.3239 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_agitate, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.651 -4.106  0.155  4.265  9.497 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         3.576      5.207   0.687 0.498292    
    ## mean_pat_agitate    8.780      2.352   3.733 0.000935 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.344 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.3489, Adjusted R-squared:  0.3239 
    ## F-statistic: 13.93 on 1 and 26 DF,  p-value: 0.0009351

``` r
lmnp1 <- lm(mean_dep_score ~ mean_pat_worry, WeeklyNegPhys)
summary(lmnp1) #R^2 jumps to 0.774 which is higher than the grouped mean_negmood_ind of 0.5589 and also higher than the combined   mean_negmood_ind & mean_physym_ind of 0.765
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.554 -1.871 -1.432  1.868  7.913 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -5.240      2.943  -1.781   0.0867 .  
    ## mean_pat_worry   20.624      2.133   9.667 4.27e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.09 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7823, Adjusted R-squared:  0.774 
    ## F-statistic: 93.46 on 1 and 26 DF,  p-value: 4.268e-10

``` r
lmnp1 <- lm(mean_dep_score ~  mean_se_ashamed, WeeklyNegPhys)
summary(lmnp1) #R^2 dips 0.5205 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_se_ashamed, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.2543  -3.0077  -0.5747   3.1183   9.5577 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -15.398      6.962  -2.212    0.036 *  
    ## mean_se_ashamed   31.125      5.654   5.505  8.9e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.5 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.5383, Adjusted R-squared:  0.5205 
    ## F-statistic: 30.31 on 1 and 26 DF,  p-value: 8.903e-06

``` r
lmnp1 <- lm(mean_dep_score ~  mean_se_selfdoub, WeeklyNegPhys)
summary(lmnp1) #R^2 dips to 0.2475 significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_se_selfdoub, data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5207 -4.1514 -0.9173  5.1179  8.2827 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)         3.765      6.099   0.617  0.54245   
    ## mean_se_selfdoub    9.384      2.985   3.144  0.00414 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.638 on 26 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.2754, Adjusted R-squared:  0.2475 
    ## F-statistic: 9.882 on 1 and 26 DF,  p-value: 0.004142

Here we see the mean\_pat\_worry variable by itself is significant and has a higher Adjusted R-square, 0.774, than the grouped negative mood indicators (0.5589) or the combined grouped negative mood and physical symptom indicators (0.765).

``` r
ggplot(WeeklyNegPhys, aes(x = mean_pat_worry, y = mean_dep_score)) +
  geom_point( color = "black") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVWorryFinal-1.png)

Looking at the depression score vs mean\_pat\_ worry we see the linear model performs reasonably well.

Next, add the individual physical symptom indicators to mean\_pat\_worry to see if the model can be improved.

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_hungry, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_hungry, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.612 -1.936 -1.435  2.046  7.683 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -3.292      5.801  -0.568    0.575    
    ## mean_pat_worry    20.666      2.172   9.516 8.63e-10 ***
    ## mean_phy_hungry   -1.377      3.515  -0.392    0.698    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.141 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7837, Adjusted R-squared:  0.7664 
    ## F-statistic: 45.28 on 2 and 25 DF,  p-value: 4.884e-09

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_tired, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_tired, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.572 -1.994 -1.413  2.033  7.699 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -2.335      5.157  -0.453    0.655    
    ## mean_pat_worry   20.361      2.189   9.301 1.35e-09 ***
    ## mean_phy_tired   -1.271      1.844  -0.689    0.497    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.121 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7864, Adjusted R-squared:  0.7693 
    ## F-statistic: 46.02 on 2 and 25 DF,  p-value: 4.167e-09

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_pain, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_pain, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.536 -2.006 -1.206  2.039  7.803 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -7.262      5.645  -1.287    0.210    
    ## mean_pat_worry   20.451      2.206   9.269 1.45e-09 ***
    ## mean_phy_pain     1.688      3.996   0.422    0.676    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.14 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7839, Adjusted R-squared:  0.7666 
    ## F-statistic: 45.34 on 2 and 25 DF,  p-value: 4.824e-09

``` r
lmnp2 <- lm(mean_dep_score ~  mean_pat_worry + mean_phy_dizzy, WeeklyNegPhys)
summary(lmnp2)   ###  AdjR^2 0.8139, both terms significant********************
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_dizzy, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1302 -1.5230 -0.9932  1.9141  5.9181 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -86.180     31.672  -2.721   0.0117 *  
    ## mean_pat_worry   19.983      1.952  10.238 1.99e-10 ***
    ## mean_phy_dizzy   81.196     31.659   2.565   0.0167 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.804 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.8277, Adjusted R-squared:  0.8139 
    ## F-statistic: 60.04 on 2 and 25 DF,  p-value: 2.845e-10

``` r
lmnp2 <- lm(mean_dep_score ~  mean_pat_worry + mean_phy_drymouth, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_drymouth, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.485 -1.771 -1.248  1.585  8.082 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -151.055    112.252  -1.346    0.190    
    ## mean_pat_worry      20.767      2.109   9.849 4.36e-10 ***
    ## mean_phy_drymouth  145.478    111.955   1.299    0.206    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.05 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7961, Adjusted R-squared:  0.7798 
    ## F-statistic: 48.81 on 2 and 25 DF,  p-value: 2.329e-09

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_nauseous, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_nauseous, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.734 -1.589 -1.146  1.748  6.531 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -70.948     52.835  -1.343    0.191    
    ## mean_pat_worry      20.507      2.113   9.704 5.86e-10 ***
    ## mean_phy_nauseous   65.549     52.627   1.246    0.224    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.058 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7951, Adjusted R-squared:  0.7787 
    ## F-statistic:  48.5 on 2 and 25 DF,  p-value: 2.484e-09

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_headache, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_headache, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.868 -2.178 -0.921  1.808  7.165 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -1.652      4.837  -0.341    0.736    
    ## mean_pat_worry      20.355      2.158   9.433 1.03e-09 ***
    ## mean_phy_headache   -2.235      2.388  -0.936    0.358    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.097 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7897, Adjusted R-squared:  0.7729 
    ## F-statistic: 46.94 on 2 and 25 DF,  p-value: 3.428e-09

``` r
lmnp2 <- lm(mean_dep_score ~ mean_pat_worry + mean_phy_sleepy, WeeklyNegPhys)
summary(lmnp2) #no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_phy_sleepy, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.603 -2.015 -1.228  2.240  7.847 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -2.881      5.081  -0.567    0.576    
    ## mean_pat_worry    20.202      2.284   8.845 3.59e-09 ***
    ## mean_phy_sleepy   -1.272      2.218  -0.573    0.571    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.13 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.7852, Adjusted R-squared:  0.768 
    ## F-statistic: 45.69 on 2 and 25 DF,  p-value: 4.478e-09

Here we see that adding mean\_phy\_dizzy to the model increase the Adjusted R^2 from 0.774 to 0.8139!

Looking at the depression score vs mean\_phy\_dizzy we have an issue!

``` r
ggplot(WeeklyNegPhys, aes(x = mean_phy_dizzy, y = mean_dep_score)) +
  geom_point( color = "red") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVDizzyFinal-1.png)

With the exception of 6 weeks, the average weekly dizziness score was 1. While the graph clearly shows, with the exception of one of those weeks, that if the patient was experiencing dizziness his depression was higher than usual, I do not consider dizziness to be an appropriate variable to include in the model.

And finally, I want to see if the patients sleep patterns and quality affect the depression score.

``` r
lmnp3 <- lm(mean_dep_score ~  mean_pat_worry + mean_mor_asleep, WeeklyNegPhys)
summary(lmnp3) #AdjR^2 improved to 0.798! all terms significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_mor_asleep, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.574 -1.850 -0.978  1.626  7.910 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -6.0614     2.8114  -2.156   0.0409 *  
    ## mean_pat_worry   17.9962     2.3991   7.501 7.44e-08 ***
    ## mean_mor_asleep   1.3083     0.6467   2.023   0.0539 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.921 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.813,  Adjusted R-squared:  0.798 
    ## F-statistic: 54.33 on 2 and 25 DF,  p-value: 7.925e-10

``` r
lmnp3 <- lm(mean_dep_score ~  mean_pat_worry + mean_mor_nrwakeup, WeeklyNegPhys)
summary(lmnp3) #AdjR^2 improved to 0.8104! all terms significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_mor_nrwakeup, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.097 -1.783 -1.018  2.034  6.672 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -3.6803     2.7695  -1.329   0.1959    
    ## mean_pat_worry     17.0002     2.4515   6.935 2.87e-07 ***
    ## mean_mor_nrwakeup   1.5828     0.6465   2.448   0.0217 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.83 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.8244, Adjusted R-squared:  0.8104 
    ## F-statistic:  58.7 on 2 and 25 DF,  p-value: 3.592e-10

``` r
lmnp3 <- lm(mean_dep_score ~  mean_pat_worry + mean_mor_lieawake, WeeklyNegPhys)
summary(lmnp3) #AdjR^2 improved to 0.8287 all terms significant
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_mor_lieawake, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2518 -1.8244 -0.7064  1.0102  6.9433 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -6.8317     2.6146  -2.613  0.01498 *  
    ## mean_pat_worry     15.5605     2.4912   6.246 1.56e-06 ***
    ## mean_mor_lieawake   2.5722     0.8433   3.050  0.00535 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.69 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.8414, Adjusted R-squared:  0.8287 
    ## F-statistic:  66.3 on 2 and 25 DF,  p-value: 1.011e-10

``` r
lmnp3 <- lm(mean_dep_score ~  mean_pat_worry + mean_mor_qualsleep, WeeklyNegPhys)
summary(lmnp3) # no improvement
```

    ## 
    ## Call:
    ## lm(formula = mean_dep_score ~ mean_pat_worry + mean_mor_qualsleep, 
    ##     data = WeeklyNegPhys)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.512 -1.838 -1.163  1.534  7.535 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          6.0772     6.1700   0.985   0.3341    
    ## mean_pat_worry      17.6757     2.4721   7.150 1.71e-07 ***
    ## mean_mor_qualsleep  -1.5935     0.7759  -2.054   0.0506 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.915 on 25 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.8138, Adjusted R-squared:  0.7989 
    ## F-statistic: 54.62 on 2 and 25 DF,  p-value: 7.51e-10

As you can see all of the sleep variables, when added to the previous model improve the strength of the model. The Adjusted R-square for all terms is over 0.79!

``` r
ggplot(WeeklyNegPhys, aes(x = mean_mor_asleep, y = mean_dep_score)) +
  geom_point( color = "black") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVSleepFinal-1.png)

``` r
ggplot(WeeklyNegPhys, aes(x = mean_mor_nrwakeup, y = mean_dep_score)) +
  geom_point( color = "black") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVSleepFinal-2.png)

``` r
ggplot(WeeklyNegPhys, aes(x = mean_mor_lieawake, y = mean_dep_score)) +
  geom_point( color = "blue") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVSleepFinal-3.png)

``` r
ggplot(WeeklyNegPhys, aes(x = mean_mor_qualsleep, y = mean_dep_score)) +
  geom_point( color = "red") +
  geom_smooth(method = "lm", se = TRUE)
```

![](Capstone_Project_Final_Report_files/figure-markdown_github/DepVSleepFinal-4.png)

The chart for depression vs quality of sleep is negatively correlated and appears to be influenced by an outlier.

The first three variables added to the model, mean\_mor\_asleep, mean\_mor\_nrwakeup and mean\_mor\_lieawake all improve the Adjusted R-square of the model with only mean\_pat\_worry. The chart with mean\_mor\_lieawake is the most evenly distributed and has the highest Adjusted R-square and was added to the model.....0.8287

Linear Model Discussion
-----------------------

This is the regression equation I chose:

mean\_dep\_score = -6.8317 + 15.5605(mean\_pat\_worry) + 2.5722(mean\_mor\_lieawake)

This means that, holding mean\_mor\_lieawake constant, for every 1 point increase in the average weekly pat\_worry question ("I worry") rated on a scale from 1 (not) to 7 (very), the patients average weekly depression score increases 15.5605 points.

Holding mean\_pat\_worry constant, every 1 point increase in the average weekly mor\_lieawake question ("How long did I lie awake this morning before getting out of bed") rated on a scale from 1 (0 to 5 min) to 8 (240 min +), the patients average weekly depression score increases 2.5722 points.

In other words, for this patient, his depression was elevated when he expressed feelings of worry, took longer on average to get out of bed. With the two variable linear model explaining 82.9% of the variation.

In this equation, all coefficients including the y-intercept are statistically significant

| Coefficients      | Pr(&gt;abs(t)) |
|-------------------|----------------|
| (Intercept)       | 0.01498        |
| mean\_pat\_worry  | 1.56e-06       |
| mean\_mor\_asleep | 0.00535        |

Ideas for Further Research
--------------------------

It cannot be stressed enough the exploratory nature of this study. All of my findings are only valid for this individual, at this point in time. Larger and more diverse populations of patients would have to be included in order to make more concrete observations and potentially use the data for predictive purposes.

It would be a massive undertaking; however, I believe this type of research is desperately needed. The impact of depression and its cost to our society are very high and I do not see them decreasing without intervention. Much like the Framingham Heart Study helped us understand the epidemiology of cardiovascular disease, the same commitment should be made to understand the epidemiology of depression and other mental disorders.

Recommendations
---------------

The goal of this preliminary study was to determine if they could identify a depressive episode in a patient who wished to be taken off of antidepressant medication after an 8.5-year history of chronic depression. Within reason I believe I was successful with a two-factor model that explained ~83% of the variation. So, for this patient, at this point in time the following recommendations are made for the clinician and patient:

1.  Be aware of the patients reports of feeling worried, and not just expression of the word “*worry*”. Worry in itself is more an expression of anxiety over things that may happen in the future. It is the chronic state of this anxiety that manifests into depression. Giving the patient tools to recognize this pattern, then switching their focus from the future (worry) they created in their mind to the present moment can be an affective tool in breaking this pattern.

2.  Pay attention to the patients sleep patterns. All of the sleep variables increased the strength of the model as one would expect. I chose the variable that measured how long it took the patient to get out of bed upon waking for the model because it affected the Adjusted R-squared the most and was the most statistically significant. It is clear better sleep corresponds with less depression. Here, increased physical activity, relaxation techniques, and meditation would all help.

3.  Finally, even though I left the dizziness variable out of the model because I felt it was being driven by outliers, I think it still deserves consideration. It could also be a function of the side effects experienced when withdrawing from long-term antidepressant medication usage, or some other medical condition. If it is a withdrawal symptom of the medication it should subside after a couple of weeks, otherwise some other intervention may be required.
