Cleaning Script
================

# Package Loading

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Data Loading

``` r
stakeholder_survey <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/survey-data.csv")
```

# Cleaning

We will need to first select out and rename the variables that wil be
necessary for our analysis, with:

- `date` corresponding to second the response was recorded
- `completion` corresponding to the proportion of the survey that was
  responded to or viewed
- `channel` corresponding to the means through which the survey was
  accessed
- `consent` corresponding to the terms of participation
- `involvement` corresponding to the participants’ self-reported role in
  deciding dining policies and practices
- `involvement_other` corresponding to offered clarifications on
  participants’ reported roles in deciding dining policies and practices
- `stakeholder_type` corresponding to the type of professional role
  participants reported as serving
- `stakeholder_type_other` corresponding to offered clarifications on
  the type of professional role participants reported as serving
- `title` corresponding to participants’ disclosed job titles
- `role_duration` corresponding to the number of years participants have
  served in that position
- `dietary_health_ranking` corresponding to the level of priority given
  to the healthiness of food offerings
- `dietary_sustainability_ranking` corresponding to the level of
  priority given to the sustainability of guest food choices
- `institutional_sustainability_ranking` corresponding to the level of
  priority given to the sustainability of dining operations
- `food_pricing_ranking` corresponding to the level of priority given to
  the campus food prices
- `operational_costs_ranking` corresponding to the level of priority
  given to cost of funding dining operations
- `guest_satisfaction_ranking` corresponding to the level of priority
  given to the dining experiences of students
- `worker_satisfaction_ranking` corresponding to the level of priority
  given to the labor experiences of dining staff
- `campus_culture_ranking` corresponding to the level of priority given
  to campus sustainability culture and the monitoring of spillover
  effects
- `other_ranking` corresponding to the level of priority given to other
  relevant indicators of university-based dietary intervention
  performance not considered in the provided list
- `other_ranking_other` corresponding to offered indicators

``` r
stakeholder_survey <- stakeholder_survey %>%
  select(RecordedDate,Progress,DistributionChannel,Q1,Q2,Q2_4_TEXT,Q6,Q6_10_TEXT,Q3,Q5_1,Q2_1,Q2_2,Q2_3,Q2_4,Q2_5,Q2_6,Q2_7,Q2_8,Q2_9,Q2_9_TEXT) %>%
  rename(date=RecordedDate,completion=Progress,channel=DistributionChannel,consent=Q1,involvement=Q2,involvement_other=Q2_4_TEXT,stakeholder_type=Q6,stakeholder_type_other=Q6_10_TEXT,title=Q3,role_duration=Q5_1,dietary_health_ranking=Q2_1,dietary_sustainability_ranking=Q2_2,institutional_sustainability_ranking=Q2_3,food_pricing_ranking=Q2_4,operational_costs_ranking=Q2_5,guest_satisfaction_ranking=Q2_6,worker_satisfaction_ranking=Q2_7,campus_culture_ranking=Q2_8,other_ranking=Q2_9,other_ranking_other=Q2_9_TEXT) 
```

Subsequently, we will need to (1) omit the first three rows, which
contain information about how the survey variables are coded and (2)
reclass the `dietary_health_ranking`, `dietary_sustainability_ranking`,
`institutional_sustainability_ranking`, `food_price_ranking`,
`operational_costs_ranking`, `guest_satisfaction_ranking`,
`worker_satisfaction_ranking`, `campus_culure_ranking`, and
`other_ranking` variables from characteric strings to numeric vectors.

``` r
stakeholder_survey <- stakeholder_survey %>%
  slice(3:n()) %>%
  mutate(dietary_health_ranking=as.numeric(dietary_health_ranking)) %>%
  mutate(dietary_sustainability_ranking=as.numeric(dietary_sustainability_ranking)) %>%
  mutate(institutional_sustainability_ranking=as.numeric(institutional_sustainability_ranking)) %>%
  mutate(food_pricing_ranking=as.numeric(food_pricing_ranking)) %>%
  mutate(operational_costs_ranking=as.numeric(operational_costs_ranking)) %>%
  mutate(guest_satisfaction_ranking=as.numeric(guest_satisfaction_ranking)) %>%
  mutate(worker_satisfaction_ranking=as.numeric(worker_satisfaction_ranking)) %>%
  mutate(campus_culture_ranking=as.numeric(campus_culture_ranking)) %>%
  mutate(other_ranking=as.numeric(other_ranking)) 
```

Only one respondent added,

``` r
stakeholder_survey <- stakeholder_survey %>%
  mutate(dietary_health_score=case_when(dietary_health_ranking == 1 ~ 9,
                                        dietary_health_ranking == 2 ~ 8,
                                        dietary_health_ranking == 3 ~ 7,
                                        dietary_health_ranking == 4 ~ 6,
                                        dietary_health_ranking == 5 ~ 5,
                                        dietary_health_ranking == 6 ~ 4,
                                        dietary_health_ranking == 7 ~ 3,
                                        dietary_health_ranking == 8 ~ 2,
                                        dietary_health_ranking == 9 ~ 1)) %>%
  mutate(dietary_sustainability_score=case_when(dietary_sustainability_ranking == 1 ~ 9,
                                        dietary_sustainability_ranking == 2 ~ 8,
                                        dietary_sustainability_ranking == 3 ~ 7,
                                        dietary_sustainability_ranking == 4 ~ 6,
                                        dietary_sustainability_ranking == 5 ~ 5,
                                        dietary_sustainability_ranking == 6 ~ 4,
                                        dietary_sustainability_ranking == 7 ~ 3,
                                        dietary_sustainability_ranking == 8 ~ 2,
                                        dietary_sustainability_ranking == 9 ~ 1)) %>%
  mutate(institutional_sustainability_score=case_when(institutional_sustainability_ranking == 1 ~ 9,
                                        institutional_sustainability_ranking == 2 ~ 8,
                                        institutional_sustainability_ranking == 3 ~ 7,
                                        institutional_sustainability_ranking == 4 ~ 6,
                                        institutional_sustainability_ranking == 5 ~ 5,
                                        institutional_sustainability_ranking == 6 ~ 4,
                                        institutional_sustainability_ranking == 7 ~ 3,
                                        institutional_sustainability_ranking == 8 ~ 2,
                                        institutional_sustainability_ranking == 9 ~ 1)) %>%
  mutate(food_pricing_score=case_when(food_pricing_ranking == 1 ~ 9,
                                        food_pricing_ranking == 2 ~ 8,
                                        food_pricing_ranking == 3 ~ 7,
                                        food_pricing_ranking == 4 ~ 6,
                                        food_pricing_ranking == 5 ~ 5,
                                        food_pricing_ranking == 6 ~ 4,
                                        food_pricing_ranking == 7 ~ 3,
                                        food_pricing_ranking == 8 ~ 2,
                                        food_pricing_ranking == 9 ~ 1)) %>%
  mutate(operational_costs_score=case_when(operational_costs_ranking == 1 ~ 9,
                                        operational_costs_ranking == 2 ~ 8,
                                        operational_costs_ranking == 3 ~ 7,
                                        operational_costs_ranking == 4 ~ 6,
                                        operational_costs_ranking == 5 ~ 5,
                                        operational_costs_ranking == 6 ~ 4,
                                        operational_costs_ranking == 7 ~ 3,
                                        operational_costs_ranking == 8 ~ 2,
                                        operational_costs_ranking == 9 ~ 1)) %>%
  mutate(guest_satisfaction_score=case_when(guest_satisfaction_ranking == 1 ~ 9,
                                        guest_satisfaction_ranking == 2 ~ 8,
                                        guest_satisfaction_ranking == 3 ~ 7,
                                        guest_satisfaction_ranking == 4 ~ 6,
                                        guest_satisfaction_ranking == 5 ~ 5,
                                        guest_satisfaction_ranking == 6 ~ 4,
                                        guest_satisfaction_ranking == 7 ~ 3,
                                        guest_satisfaction_ranking == 8 ~ 2,
                                        guest_satisfaction_ranking == 9 ~ 1)) %>%
  mutate(worker_satisfaction_score=case_when(worker_satisfaction_ranking == 1 ~ 9,
                                        worker_satisfaction_ranking == 2 ~ 8,
                                        worker_satisfaction_ranking == 3 ~ 7,
                                        worker_satisfaction_ranking == 4 ~ 6,
                                        worker_satisfaction_ranking == 5 ~ 5,
                                        worker_satisfaction_ranking == 6 ~ 4,
                                        worker_satisfaction_ranking == 7 ~ 3,
                                        worker_satisfaction_ranking == 8 ~ 2,
                                        worker_satisfaction_ranking == 9 ~ 1)) %>%
  mutate(campus_culture_score=case_when(campus_culture_ranking == 1 ~ 9,
                                        campus_culture_ranking == 2 ~ 8,
                                        campus_culture_ranking == 3 ~ 7,
                                        campus_culture_ranking == 4 ~ 6,
                                        campus_culture_ranking == 5 ~ 5,
                                        campus_culture_ranking == 6 ~ 4,
                                        campus_culture_ranking == 7 ~ 3,
                                        campus_culture_ranking == 8 ~ 2,
                                        campus_culture_ranking == 9 ~ 1)) %>%
  mutate(other_score=case_when(other_ranking == 1 ~ 9,
                                        other_ranking == 2 ~ 8,
                                        other_ranking == 3 ~ 7,
                                        other_ranking == 4 ~ 6,
                                        other_ranking == 5 ~ 5,
                                        other_ranking == 6 ~ 4,
                                        other_ranking == 7 ~ 3,
                                        other_ranking == 8 ~ 2,
                                        other_ranking == 9 ~ 1)) 
```

``` r
stakeholder_survey %>%
  group_by(involvement) %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),other_score_mean=mean(other_score),n=n())
```

    ## # A tibble: 4 × 11
    ##   involvement                      dietary_health_score…¹ dietary_sustainabili…²
    ##   <chr>                                             <dbl>                  <dbl>
    ## 1 I am a primary decision maker                      5.67                   5.17
    ## 2 I consult on best practices                        6.31                   5.56
    ## 3 I offer feedback on existing se…                   6.67                   4.33
    ## 4 Other (please specify):                            7                      6   
    ## # ℹ abbreviated names: ¹​dietary_health_score_mean,
    ## #   ²​dietary_sustainability_score_mean
    ## # ℹ 8 more variables: institutional_sustainability_score_mean <dbl>,
    ## #   food_pricing_score_mean <dbl>, operational_costs_score_mean <dbl>,
    ## #   guest_satisfaction_score_mean <dbl>, worker_satisfaction_score_mean <dbl>,
    ## #   campus_culture_score_mean <dbl>, other_score_mean <dbl>, n <int>

``` r
stakeholder_survey %>%
  group_by(stakeholder_type) %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),other_score_mean=mean(other_score),n=n())
```

    ## # A tibble: 6 × 11
    ##   stakeholder_type           dietary_health_score_mean dietary_sustainability_…¹
    ##   <chr>                                          <dbl>                     <dbl>
    ## 1 Chef                                            5                         5.25
    ## 2 Dining director                                 5.71                      5.71
    ## 3 Nutrition specialist                            7.4                       5.6 
    ## 4 Other (please specify):                         6.83                      5.67
    ## 5 Sustainability coordinator                      5.5                       5.83
    ## 6 University administrator                        6.25                      3   
    ## # ℹ abbreviated name: ¹​dietary_sustainability_score_mean
    ## # ℹ 8 more variables: institutional_sustainability_score_mean <dbl>,
    ## #   food_pricing_score_mean <dbl>, operational_costs_score_mean <dbl>,
    ## #   guest_satisfaction_score_mean <dbl>, worker_satisfaction_score_mean <dbl>,
    ## #   campus_culture_score_mean <dbl>, other_score_mean <dbl>, n <int>

``` r
stakeholder_survey %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),other_score_mean=mean(other_score),n=n())
```

    ##   dietary_health_score_mean dietary_sustainability_score_mean
    ## 1                     6.125                            5.3125
    ##   institutional_sustainability_score_mean food_pricing_score_mean
    ## 1                                  4.8125                 5.21875
    ##   operational_costs_score_mean guest_satisfaction_score_mean
    ## 1                      5.96875                        7.0625
    ##   worker_satisfaction_score_mean campus_culture_score_mean other_score_mean  n
    ## 1                         4.5625                   4.71875          1.21875 32

``` r
stakeholder_survey %>%
  summarise(dietary_health_score_sum=sum(dietary_health_score),dietary_sustainability_score_sum=sum(dietary_sustainability_score),institutional_sustainability_score_sum=sum(institutional_sustainability_score),institutional_sustainability_score_sum=sum(institutional_sustainability_score),food_pricing_score_sum=sum(food_pricing_score),operational_costs_score_sum=sum(operational_costs_score),guest_satisfaction_score_sum=sum(guest_satisfaction_score),worker_satisfaction_score_sum=sum(worker_satisfaction_score),campus_culture_score_sum=sum(campus_culture_score),other_score_sum=sum(other_score),n=n())
```

    ##   dietary_health_score_sum dietary_sustainability_score_sum
    ## 1                      196                              170
    ##   institutional_sustainability_score_sum food_pricing_score_sum
    ## 1                                    154                    167
    ##   operational_costs_score_sum guest_satisfaction_score_sum
    ## 1                         191                          226
    ##   worker_satisfaction_score_sum campus_culture_score_sum other_score_sum  n
    ## 1                           146                      151              39 32
