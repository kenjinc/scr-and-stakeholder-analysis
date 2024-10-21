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

``` r
library(ggpubr)
library(maps)
```

    ## 
    ## Attaching package: 'maps'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

# Data Loading

``` r
stakeholder_survey <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/survey-data.csv")
review_data <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/review-data.csv")
```

# Cleaning

First, we will need to select and rename the variables relevant to our
plan of analysis. We will specifically focus on the following:

- `date`, which corresponds to the date and time the response was
  recorded
- `completion`, which corresponds to the proportion of the survey that
  was completed
- `channel`, which corresponds to the means through which the survey
  materials were accessed
- `consent`, which corresponds to whether participants provided informed
  consent
- `involvement`, which corresponds to the participants’ self-reported
  role in deciding dining policies and practices
- `involvement_other`, which corresponds to to offered clarifications on
  participants’ reported roles in deciding dining policies and practices
- `stakeholder_type`, which corresponds to the type of professional role
  participants reported as serving
- `stakeholder_type_other`, which corresponds to offered clarifications
  on the type of professional role participants reported as serving
- `title`, which corresponds to participants’ disclosed job titles
- `role_duration`, which corresponds to the number of years participants
  have served in that position
- `dietary_health_ranking`, which corresponds to the level of priority
  given to the healthiness of food offerings
- `dietary_sustainability_ranking`, which corresponds to the level of
  priority given to the sustainability of guest food choices
- `institutional_sustainability_ranking`, which corresponds to the level
  of priority given to the sustainability of dining operations
- `food_pricing_ranking`, which corresponds to the level of priority
  given to the campus food prices
- `operational_costs_ranking`, which corresponds to the level of
  priority given to cost of funding dining operations
- `guest_satisfaction_ranking`, which corresponds to the level of
  priority given to the dining experiences of students
- `worker_satisfaction_ranking`, which corresponds to the level of
  priority given to the labor experiences of dining staff
- `campus_culture_ranking`, which corresponds to the level of priority
  given to campus sustainability culture and the monitoring of spillover
  effects
- `other_ranking`, which corresponds to the level of priority given to
  other relevant indicators of university-based dietary intervention
  performance not considered in the provided list
- `other_ranking_other`, which corresponds to offered indicators

``` r
stakeholder_survey <- stakeholder_survey %>%
  select(RecordedDate,Progress,DistributionChannel,Q1,Q2,Q2_4_TEXT,Q6,Q6_10_TEXT,Q3,Q5_1,Q2_1,Q2_2,Q2_3,Q2_4,Q2_5,Q2_6,Q2_7,Q2_8,Q2_9,Q2_9_TEXT) %>%
  rename(date=RecordedDate,completion=Progress,channel=DistributionChannel,consent=Q1,involvement=Q2,involvement_other=Q2_4_TEXT,stakeholder_type=Q6,stakeholder_type_other=Q6_10_TEXT,title=Q3,role_duration=Q5_1,dietary_health_ranking=Q2_1,dietary_sustainability_ranking=Q2_2,institutional_sustainability_ranking=Q2_3,food_pricing_ranking=Q2_4,operational_costs_ranking=Q2_5,guest_satisfaction_ranking=Q2_6,worker_satisfaction_ranking=Q2_7,campus_culture_ranking=Q2_8,other_ranking=Q2_9,other_ranking_other=Q2_9_TEXT) 
```

We also need to (1) omit the first three rows, which contain information
about how the survey variables are coded and (2) reclass the
`dietary_health_ranking`, `dietary_sustainability_ranking`,
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

With this complete, we can now move on to address an isolated difference
in how one subject elected to respond to the rank-order choice set. More
specifically, this participant used the provided prompt to suggest an
unlisted performance indicator in `other_ranking_other`, with all other
respondents ranking `other_ranking` last and without any additional
details. Given this, in the interest of constructing a consistent
scoring system, we will narratively note the omission of the suggested
“Cuisine type” indicator in our results while simultaneously removing
the `other_ranking` and `other_ranking_other` variables from the
variable list.

In this particular instance, because the subject identified this
previously unspecified indicator with a rank of “2,” this requires us to
subtract 1 from every ranked indicator apart from
`guest_satisfaction_ranking`, which was ranked first, ahead of “Cuisine
type.”

To accomplish this, we will first transform the data by adding variable
`id` to attach unique identifiers to each individual response. This will
help us isolate the changes in ranking to the one outlying response set.

``` r
stakeholder_survey <- stakeholder_survey %>%
  mutate(id=row_number())
```

Now, we move on to the next step, which involves recoding all rank
responses greater than “2,” such that every performance indicator apart
from `guest_satisfaction_ranking` moves one position ahead to compensate
for the joint removal of `other_ranking` and `other_ranking_other`.

``` r
stakeholder_survey <- stakeholder_survey %>%
  mutate(campus_culture_ranking=case_when(id == 22 ~ 8,
                                          TRUE ~ campus_culture_ranking)) %>%
  mutate(worker_satisfaction_ranking=case_when(id == 22 ~ 7,
                                          TRUE ~ worker_satisfaction_ranking)) %>%
  mutate(operational_costs_ranking=case_when(id == 22 ~ 2,
                                          TRUE ~ worker_satisfaction_ranking)) %>%
  mutate(food_pricing_ranking=case_when(id == 22 ~ 3,
                                          TRUE ~ food_pricing_ranking)) %>%
  mutate(institutional_sustainability_ranking=case_when(id == 22 ~ 4,
                                          TRUE ~ institutional_sustainability_ranking)) %>%
  mutate(dietary_sustainability_ranking=case_when(id == 22 ~ 5,
                                          TRUE ~ dietary_sustainability_ranking)) %>%
  mutate(dietary_health_ranking=case_when(id == 22 ~ 6,
                                          TRUE ~ dietary_health_ranking)) 
```

With

``` r
stakeholder_survey %>% select(-other_ranking,-other_ranking_other)
```

    ##                   date completion channel               consent
    ## 1  2024-10-02 12:42:10        100      qr I wish to participate
    ## 2  2024-10-02 12:42:13        100      qr I wish to participate
    ## 3  2024-10-02 12:42:20        100      qr I wish to participate
    ## 4  2024-10-02 12:42:26        100      qr I wish to participate
    ## 5  2024-10-02 12:42:29        100      qr I wish to participate
    ## 6  2024-10-02 12:42:32        100      qr I wish to participate
    ## 7  2024-10-02 12:42:37        100      qr I wish to participate
    ## 8  2024-10-02 12:42:41        100      qr I wish to participate
    ## 9  2024-10-02 12:42:51        100      qr I wish to participate
    ## 10 2024-10-02 12:42:54        100      qr I wish to participate
    ## 11 2024-10-02 12:42:57        100      qr I wish to participate
    ## 12 2024-10-02 12:42:58        100      qr I wish to participate
    ## 13 2024-10-02 12:43:05        100      qr I wish to participate
    ## 14 2024-10-02 12:43:06        100      qr I wish to participate
    ## 15 2024-10-02 12:43:09        100      qr I wish to participate
    ## 16 2024-10-02 12:43:11        100      qr I wish to participate
    ## 17 2024-10-02 12:43:16        100      qr I wish to participate
    ## 18 2024-10-02 12:43:18        100      qr I wish to participate
    ## 19 2024-10-02 12:43:20        100      qr I wish to participate
    ## 20 2024-10-02 12:43:21        100      qr I wish to participate
    ## 21 2024-10-02 12:43:30        100      qr I wish to participate
    ## 22 2024-10-02 12:43:38        100      qr I wish to participate
    ## 23 2024-10-02 12:43:52        100      qr I wish to participate
    ## 24 2024-10-02 12:44:02        100      qr I wish to participate
    ## 25 2024-10-02 12:44:04        100      qr I wish to participate
    ## 26 2024-10-02 12:44:16        100      qr I wish to participate
    ## 27 2024-10-02 12:44:22        100      qr I wish to participate
    ## 28 2024-10-02 12:44:39        100      qr I wish to participate
    ## 29 2024-10-02 12:44:43        100      qr I wish to participate
    ## 30 2024-10-02 12:44:44        100      qr I wish to participate
    ## 31 2024-10-02 14:20:00        100      qr I wish to participate
    ## 32 2024-10-04 07:14:22        100      qr I wish to participate
    ##                              involvement
    ## 1            I consult on best practices
    ## 2            I consult on best practices
    ## 3                Other (please specify):
    ## 4          I am a primary decision maker
    ## 5          I am a primary decision maker
    ## 6          I am a primary decision maker
    ## 7          I am a primary decision maker
    ## 8            I consult on best practices
    ## 9            I consult on best practices
    ## 10         I am a primary decision maker
    ## 11           I consult on best practices
    ## 12         I am a primary decision maker
    ## 13           I consult on best practices
    ## 14         I am a primary decision maker
    ## 15           I consult on best practices
    ## 16           I consult on best practices
    ## 17           I consult on best practices
    ## 18         I am a primary decision maker
    ## 19           I consult on best practices
    ## 20 I offer feedback on existing services
    ## 21         I am a primary decision maker
    ## 22         I am a primary decision maker
    ## 23           I consult on best practices
    ## 24           I consult on best practices
    ## 25 I offer feedback on existing services
    ## 26           I consult on best practices
    ## 27         I am a primary decision maker
    ## 28           I consult on best practices
    ## 29           I consult on best practices
    ## 30           I consult on best practices
    ## 31         I am a primary decision maker
    ## 32 I offer feedback on existing services
    ##                                                                   involvement_other
    ## 1                                                                                  
    ## 2                                                                                  
    ## 3  Collaboration and influence with corporate stakeholders on food loss and waste  
    ## 4                                                                                  
    ## 5                                                                                  
    ## 6                                                                                  
    ## 7                                                                                  
    ## 8                                                                                  
    ## 9                                                                                  
    ## 10                                                                                 
    ## 11                                                                                 
    ## 12                                                                                 
    ## 13                                                                                 
    ## 14                                                                                 
    ## 15                                                                                 
    ## 16                                                                                 
    ## 17                                                                                 
    ## 18                                                                                 
    ## 19                                                                                 
    ## 20                                                                                 
    ## 21                                                                                 
    ## 22                                                                                 
    ## 23                                                                                 
    ## 24                                                                                 
    ## 25                                                                                 
    ## 26                                                                                 
    ## 27                                                                                 
    ## 28                                                                                 
    ## 29                                                                                 
    ## 30                                                                                 
    ## 31                                                                                 
    ## 32                                                                                 
    ##              stakeholder_type         stakeholder_type_other
    ## 1        Nutrition specialist                               
    ## 2  Sustainability coordinator                               
    ## 3     Other (please specify):                   NGO employee
    ## 4             Dining director                               
    ## 5             Dining director                               
    ## 6                        Chef                               
    ## 7             Dining director                               
    ## 8  Sustainability coordinator                               
    ## 9        Nutrition specialist                               
    ## 10            Dining director                               
    ## 11    Other (please specify):                       Advisor 
    ## 12                       Chef                               
    ## 13       Nutrition specialist                               
    ## 14   University administrator                               
    ## 15 Sustainability coordinator                               
    ## 16                       Chef                               
    ## 17   University administrator                               
    ## 18   University administrator                               
    ## 19    Other (please specify):             Dining management 
    ## 20                       Chef                               
    ## 21            Dining director                               
    ## 22   University administrator                               
    ## 23       Nutrition specialist                               
    ## 24 Sustainability coordinator                               
    ## 25       Nutrition specialist                               
    ## 26 Sustainability coordinator                               
    ## 27 Sustainability coordinator                               
    ## 28            Dining director                               
    ## 29    Other (please specify): Manager of residential dining 
    ## 30    Other (please specify):              Dining marketing 
    ## 31            Dining director                               
    ## 32    Other (please specify):                      marketing
    ##                                                                          title
    ## 1                                                                             
    ## 2                                                      Sustainability Manager 
    ## 3                                                 Associate Program Specialist
    ## 4                                                                     Director
    ## 5                                                           Director of Dining
    ## 6                                                        Campus Executive Chef
    ## 7                                                           Director of Dining
    ## 8                                                     Sustainability Director 
    ## 9                                                                    Dietitian
    ## 10                                          Senior Director of Dining Services
    ## 11                                                                            
    ## 12 Senior associate Director culinary strategies and plant forward experience 
    ## 13                                                   Nutrition Systems Manager
    ## 14                                                   Assistant Vice President 
    ## 15                                                      Sustainability Manager
    ## 16                                                              Executive chef
    ## 17                                                                         AVP
    ## 18                                                  Associate Vice Chancellor 
    ## 19                              Director of Culinary development and nutrition
    ## 20                                                                 Sous Chef/s
    ## 21                                                  Executive Director dining 
    ## 22                                                          Asst. Vice Provost
    ## 23                                                                   Dietitian
    ## 24                                                                            
    ## 25                                                        Registered Dietitian
    ## 26                                                  Director of sustainability
    ## 27                                             Regional Sustainability Manager
    ## 28                                       Associate Director, Food and beverage
    ## 29                                                       Halls cluster manger 
    ## 30                                                          Marketing manager 
    ## 31                                                            Dining Director 
    ## 32                                                                            
    ##    role_duration dietary_health_ranking dietary_sustainability_ranking
    ## 1              2                      2                              3
    ## 2              1                      2                              3
    ## 3              2                      3                              4
    ## 4              1                      2                              5
    ## 5             11                      8                              5
    ## 6              3                      8                              3
    ## 7              1                      1                              2
    ## 8      5.5 years                      6                              5
    ## 9              3                      2                              5
    ## 10            32                      4                              2
    ## 11             1                      7                              3
    ## 12             3                      1                              6
    ## 13            12                      6                              7
    ## 14             2                      1                              8
    ## 15             3                      4                              5
    ## 16             3                      6                              4
    ## 17      20 years                      3                              6
    ## 18             4                      4                              8
    ## 19             2                      1                              4
    ## 20             1                      5                              6
    ## 21             2                      5                              2
    ## 22            16                      6                              5
    ## 23             3                      1                              2
    ## 24            .1                      6                              2
    ## 25             3                      2                              5
    ## 26            10                      3                              5
    ## 27             2                      6                              5
    ## 28             5                      5                              8
    ## 29             2                      3                              5
    ## 30             3                      2                              4
    ## 31             3                      5                              6
    ## 32                                    3                              6
    ##    institutional_sustainability_ranking food_pricing_ranking
    ## 1                                     7                    5
    ## 2                                     1                    8
    ## 3                                     8                    2
    ## 4                                     6                    4
    ## 5                                     7                    1
    ## 6                                     6                    2
    ## 7                                     3                    5
    ## 8                                     4                    8
    ## 9                                     7                    8
    ## 10                                    1                    7
    ## 11                                    8                    5
    ## 12                                    8                    4
    ## 13                                    5                    3
    ## 14                                    5                    6
    ## 15                                    1                    6
    ## 16                                    8                    2
    ## 17                                    8                    1
    ## 18                                    7                    3
    ## 19                                    8                    6
    ## 20                                    4                    8
    ## 21                                    3                    7
    ## 22                                    4                    3
    ## 23                                    6                    8
    ## 24                                    1                    5
    ## 25                                    6                    3
    ## 26                                    1                    6
    ## 27                                    4                    2
    ## 28                                    6                    1
    ## 29                                    4                    7
    ## 30                                    3                    6
    ## 31                                    7                    8
    ## 32                                    8                    2
    ##    operational_costs_ranking guest_satisfaction_ranking
    ## 1                          6                          1
    ## 2                          5                          4
    ## 3                          7                          6
    ## 4                          7                          1
    ## 5                          2                          4
    ## 6                          7                          4
    ## 7                          6                          4
    ## 8                          1                          2
    ## 9                          6                          1
    ## 10                         6                          5
    ## 11                         6                          1
    ## 12                         2                          5
    ## 13                         8                          4
    ## 14                         3                          2
    ## 15                         8                          3
    ## 16                         7                          1
    ## 17                         2                          4
    ## 18                         6                          2
    ## 19                         3                          2
    ## 20                         3                          1
    ## 21                         1                          4
    ## 22                         2                          1
    ## 23                         7                          3
    ## 24                         4                          8
    ## 25                         8                          1
    ## 26                         8                          4
    ## 27                         8                          3
    ## 28                         7                          2
    ## 29                         8                          2
    ## 30                         7                          1
    ## 31                         2                          4
    ## 32                         5                          4
    ##    worker_satisfaction_ranking campus_culture_ranking id
    ## 1                            6                      8  1
    ## 2                            5                      6  2
    ## 3                            7                      1  3
    ## 4                            7                      8  4
    ## 5                            2                      6  5
    ## 6                            7                      5  6
    ## 7                            6                      7  7
    ## 8                            1                      7  8
    ## 9                            6                      3  9
    ## 10                           6                      8 10
    ## 11                           6                      4 11
    ## 12                           2                      3 12
    ## 13                           8                      1 13
    ## 14                           3                      4 14
    ## 15                           8                      7 15
    ## 16                           7                      5 16
    ## 17                           2                      5 17
    ## 18                           6                      1 18
    ## 19                           3                      7 19
    ## 20                           3                      2 20
    ## 21                           1                      8 21
    ## 22                           7                      8 22
    ## 23                           7                      5 23
    ## 24                           4                      7 24
    ## 25                           8                      7 25
    ## 26                           8                      2 26
    ## 27                           8                      7 27
    ## 28                           7                      4 28
    ## 29                           8                      6 29
    ## 30                           7                      8 30
    ## 31                           2                      1 31
    ## 32                           5                      7 32

Inverted scoring system

``` r
stakeholder_survey <- stakeholder_survey %>%
  mutate(dietary_health_score=case_when(dietary_health_ranking == 1 ~ 8,
                                        dietary_health_ranking == 2 ~ 7,
                                        dietary_health_ranking == 3 ~ 6,
                                        dietary_health_ranking == 4 ~ 5,
                                        dietary_health_ranking == 5 ~ 4,
                                        dietary_health_ranking == 6 ~ 3,
                                        dietary_health_ranking == 7 ~ 2,
                                        dietary_health_ranking == 8 ~ 1)) %>%
  mutate(dietary_sustainability_score=case_when(dietary_sustainability_ranking == 1 ~ 8,
                                        dietary_sustainability_ranking == 2 ~ 7,
                                        dietary_sustainability_ranking == 3 ~ 6,
                                        dietary_sustainability_ranking == 4 ~ 5,
                                        dietary_sustainability_ranking == 5 ~ 4,
                                        dietary_sustainability_ranking == 6 ~ 3,
                                        dietary_sustainability_ranking == 7 ~ 2,
                                        dietary_sustainability_ranking == 8 ~ 1)) %>%
  mutate(institutional_sustainability_score=case_when(institutional_sustainability_ranking == 1 ~ 9,
                                        institutional_sustainability_ranking == 2 ~ 8,
                                        institutional_sustainability_ranking == 3 ~ 7,
                                        institutional_sustainability_ranking == 4 ~ 6,
                                        institutional_sustainability_ranking == 5 ~ 5,
                                        institutional_sustainability_ranking == 6 ~ 4,
                                        institutional_sustainability_ranking == 7 ~ 3,
                                        institutional_sustainability_ranking == 8 ~ 2,
                                        institutional_sustainability_ranking == 9 ~ 1)) %>%
  mutate(food_pricing_score=case_when(food_pricing_ranking == 1 ~ 8,
                                        food_pricing_ranking == 2 ~ 7,
                                        food_pricing_ranking == 3 ~ 6,
                                        food_pricing_ranking == 4 ~ 5,
                                        food_pricing_ranking == 5 ~ 4,
                                        food_pricing_ranking == 6 ~ 3,
                                        food_pricing_ranking == 7 ~ 2,
                                        food_pricing_ranking == 8 ~ 1)) %>%
  mutate(operational_costs_score=case_when(operational_costs_ranking == 1 ~ 8,
                                        operational_costs_ranking == 2 ~ 7,
                                        operational_costs_ranking == 3 ~ 6,
                                        operational_costs_ranking == 4 ~ 5,
                                        operational_costs_ranking == 5 ~ 4,
                                        operational_costs_ranking == 6 ~ 3,
                                        operational_costs_ranking == 7 ~ 2,
                                        operational_costs_ranking == 8 ~ 1)) %>%
  mutate(guest_satisfaction_score=case_when(guest_satisfaction_ranking == 1 ~ 8,
                                        guest_satisfaction_ranking == 2 ~ 7,
                                        guest_satisfaction_ranking == 3 ~ 6,
                                        guest_satisfaction_ranking == 4 ~ 5,
                                        guest_satisfaction_ranking == 5 ~ 4,
                                        guest_satisfaction_ranking == 6 ~ 3,
                                        guest_satisfaction_ranking == 7 ~ 2,
                                        guest_satisfaction_ranking == 8 ~ 1)) %>%
  mutate(worker_satisfaction_score=case_when(worker_satisfaction_ranking == 1 ~ 8,
                                        worker_satisfaction_ranking == 2 ~ 7,
                                        worker_satisfaction_ranking == 3 ~ 6,
                                        worker_satisfaction_ranking == 4 ~ 5,
                                        worker_satisfaction_ranking == 5 ~ 4,
                                        worker_satisfaction_ranking == 6 ~ 3,
                                        worker_satisfaction_ranking == 7 ~ 2,
                                        worker_satisfaction_ranking == 8 ~ 1)) %>%
  mutate(campus_culture_score=case_when(campus_culture_ranking == 1 ~ 8,
                                        campus_culture_ranking == 2 ~ 7,
                                        campus_culture_ranking == 3 ~ 6,
                                        campus_culture_ranking == 4 ~ 5,
                                        campus_culture_ranking == 5 ~ 4,
                                        campus_culture_ranking == 6 ~ 3,
                                        campus_culture_ranking == 7 ~ 2,
                                        campus_culture_ranking == 8 ~ 1)) 
```

``` r
stakeholder_survey %>%
  summarise(dietary_health_score_sum=sum(dietary_health_score),dietary_sustainability_score=sum(dietary_sustainability_score),institutional_sustainability_score_sum=sum(institutional_sustainability_score),food_pricing_score_sum=sum(food_pricing_score),operational_costs_score_sum=sum(operational_costs_score),guest_satisfaction_score_sum=sum(guest_satisfaction_score),worker_satisfaction_score_sum=sum(worker_satisfaction_score),campus_culture_score_sum=sum(worker_satisfaction_score),campus_culture_score_sum=sum(campus_culture_score)) 
```

    ##   dietary_health_score_sum dietary_sustainability_score
    ## 1                      165                          139
    ##   institutional_sustainability_score_sum food_pricing_score_sum
    ## 1                                    155                    136
    ##   operational_costs_score_sum guest_satisfaction_score_sum
    ## 1                         120                          194
    ##   worker_satisfaction_score_sum campus_culture_score_sum
    ## 1                           115                      120

``` r
stakeholder_survey %>%
  group_by(involvement) %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),n=n())
```

    ## # A tibble: 4 × 10
    ##   involvement                      dietary_health_score…¹ dietary_sustainabili…²
    ##   <chr>                                             <dbl>                  <dbl>
    ## 1 I am a primary decision maker                      4.75                   4.25
    ## 2 I consult on best practices                        5.31                   4.56
    ## 3 I offer feedback on existing se…                   5.67                   3.33
    ## 4 Other (please specify):                            6                      5   
    ## # ℹ abbreviated names: ¹​dietary_health_score_mean,
    ## #   ²​dietary_sustainability_score_mean
    ## # ℹ 7 more variables: institutional_sustainability_score_mean <dbl>,
    ## #   food_pricing_score_mean <dbl>, operational_costs_score_mean <dbl>,
    ## #   guest_satisfaction_score_mean <dbl>, worker_satisfaction_score_mean <dbl>,
    ## #   campus_culture_score_mean <dbl>, n <int>

``` r
stakeholder_survey %>%
  group_by(stakeholder_type) %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),n=n())
```

    ## # A tibble: 6 × 10
    ##   stakeholder_type           dietary_health_score_mean dietary_sustainability_…¹
    ##   <chr>                                          <dbl>                     <dbl>
    ## 1 Chef                                            4                         4.25
    ## 2 Dining director                                 4.71                      4.71
    ## 3 Nutrition specialist                            6.4                       4.6 
    ## 4 Other (please specify):                         5.83                      4.67
    ## 5 Sustainability coordinator                      4.5                       4.83
    ## 6 University administrator                        5.5                       2.25
    ## # ℹ abbreviated name: ¹​dietary_sustainability_score_mean
    ## # ℹ 7 more variables: institutional_sustainability_score_mean <dbl>,
    ## #   food_pricing_score_mean <dbl>, operational_costs_score_mean <dbl>,
    ## #   guest_satisfaction_score_mean <dbl>, worker_satisfaction_score_mean <dbl>,
    ## #   campus_culture_score_mean <dbl>, n <int>

``` r
stakeholder_survey %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),n=n())
```

    ##   dietary_health_score_mean dietary_sustainability_score_mean
    ## 1                   5.15625                           4.34375
    ##   institutional_sustainability_score_mean food_pricing_score_mean
    ## 1                                 4.84375                    4.25
    ##   operational_costs_score_mean guest_satisfaction_score_mean
    ## 1                         3.75                        6.0625
    ##   worker_satisfaction_score_mean campus_culture_score_mean  n
    ## 1                        3.59375                      3.75 32

``` r
stakeholder_survey %>%
  summarise(dietary_health_score_sum=sum(dietary_health_score),dietary_sustainability_score_sum=sum(dietary_sustainability_score),institutional_sustainability_score_sum=sum(institutional_sustainability_score),institutional_sustainability_score_sum=sum(institutional_sustainability_score),food_pricing_score_sum=sum(food_pricing_score),operational_costs_score_sum=sum(operational_costs_score),guest_satisfaction_score_sum=sum(guest_satisfaction_score),worker_satisfaction_score_sum=sum(worker_satisfaction_score),campus_culture_score_sum=sum(campus_culture_score),n=n())
```

    ##   dietary_health_score_sum dietary_sustainability_score_sum
    ## 1                      165                              139
    ##   institutional_sustainability_score_sum food_pricing_score_sum
    ## 1                                    155                    136
    ##   operational_costs_score_sum guest_satisfaction_score_sum
    ## 1                         120                          194
    ##   worker_satisfaction_score_sum campus_culture_score_sum  n
    ## 1                           115                      120 32

# Extraction Data

``` r
global_shapefile <- map_data("world")
```

``` r
global_shapefile <- global_shapefile %>% 
  rename(country=region) %>%
  mutate(country=case_when(country=="Macedonia"~"North Macedonia",
                           country=="Ivory Coast"~"Cote d'Ivoire",
                           country=="Democratic Republic of the Congo"~"Congo, Dem. Rep.",
                           country=="Republic of Congo"~"Congo, Rep.",
                           country=="UK"~"United Kingdom",
                           country=="USA"~"United States",
                           country=="Laos"~"Lao",
                           country=="Slovakia"~"Slovak Republic",
                           country=="Saint Lucia"~"St. Lucia",
                           country=="Kyrgyzstan"~"Krygyz Republic",
                           country=="Micronesia"~"Micronesia, Fed. Sts.",
                           country=="Swaziland"~"Eswatini",
                           country=="Virgin Islands"~"Virgin Islands (U.S.)",
                        TRUE~country))
island_nations <- c("Antigua","Barbuda","Nevis", 
                 "Saint Kitts","Trinidad",
                 "Tobago","Grenadines","Saint Vincent")
island_nations_match <- global_shapefile %>% 
  filter(country %in% island_nations)
island_nations_match %>% distinct(country)
```

    ##         country
    ## 1       Antigua
    ## 2       Barbuda
    ## 3         Nevis
    ## 4   Saint Kitts
    ## 5      Trinidad
    ## 6        Tobago
    ## 7    Grenadines
    ## 8 Saint Vincent

``` r
ant_bar <- c(137,138 )
kit_nev <- c(930,931)
tri_tog <- c(1425,1426)
vin_gre <- c(1575,1576,1577)
island_nation_names <- c("Antigua and Barbuda","St. Kitts and Nevis","Trinidad and Tobago","St. Vincent and the Grenadines")
island_nations_match <- island_nations_match %>% 
  mutate(country=case_when(group %in% ant_bar~"Antigua and Barbuda",
                           group %in% kit_nev~"St. Kitts and Nevis",
                           group %in% tri_tog~"Trinidad and Tobago",
                           group %in% vin_gre~"St. Vincent and the Grenadines")) %>% 
  tibble()
island_nations_match %>%
  distinct(country) 
```

    ## # A tibble: 4 × 1
    ##   country                       
    ##   <chr>                         
    ## 1 Antigua and Barbuda           
    ## 2 St. Kitts and Nevis           
    ## 3 Trinidad and Tobago           
    ## 4 St. Vincent and the Grenadines

``` r
global_shapefile <- global_shapefile %>%
  filter(!country %in% island_nation_names)
global_shapefile <- global_shapefile %>% 
  bind_rows(island_nations_match) %>%
  arrange(country) %>%
  tibble()
sra_names <- c("Hong Kong","Macao")
hk_mc <- global_shapefile %>% 
  filter(subregion %in% sra_names)
hk_mc <- hk_mc %>%
  mutate(country = case_when(subregion=="Hong Kong"~"Hong Kong, China",
                             subregion=="Macao"~"Macao, China"))
global_shapefile <- global_shapefile %>%
  filter(!subregion %in% sra_names)
global_shapefile <- global_shapefile %>% 
  bind_rows(hk_mc) %>%
  select(-subregion) %>% 
  tibble()
```

``` r
global_shapefile %>%
  distinct(country)
```

    ## # A tibble: 258 × 1
    ##    country            
    ##    <chr>              
    ##  1 Afghanistan        
    ##  2 Albania            
    ##  3 Algeria            
    ##  4 American Samoa     
    ##  5 Andorra            
    ##  6 Angola             
    ##  7 Anguilla           
    ##  8 Antarctica         
    ##  9 Antigua            
    ## 10 Antigua and Barbuda
    ## # ℹ 248 more rows

``` r
review_data <- review_data %>% 
  select(Country_2,Participating.Institutions) %>%
  rename(country=Country_2,institutions=Participating.Institutions)
```

``` r
review_data <- review_data %>%
  mutate(across(country,str_replace,"USA","United States")) %>%
  mutate(across(country,str_replace,"UK","United Kingdom")) 
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(country, str_replace, "USA", "United States")`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
review_data <- review_data %>%
  group_by(country) %>%
  summarise(count=sum(institutions))
```

``` r
review_data %>% arrange(desc(count))
```

    ## # A tibble: 18 × 2
    ##    country          count
    ##    <chr>            <int>
    ##  1 "United States"     74
    ##  2 "United Kingdom"    15
    ##  3 "Canada"            13
    ##  4 "Portugal"           7
    ##  5 "Italy"              6
    ##  6 "Germany"            4
    ##  7 "China"              3
    ##  8 "Sweden"             3
    ##  9 "Australia"          2
    ## 10 "Belgium"            2
    ## 11 "Brazil"             2
    ## 12 "Netherlands"        2
    ## 13 "France"             1
    ## 14 "India"              1
    ## 15 "Norway"             1
    ## 16 "Switzerland"        1
    ## 17 "Thailand"           1
    ## 18 ""                  NA

``` r
aggregated_data <- left_join(global_shapefile,review_data,by="country")
```

``` r
aggregated_data %>%
  ggplot(aes(x=long,y=lat,fill=count,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  theme(legend.key.width=unit(3,"lines"),legend.position="bottom",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
uk_shapefile <- map_data("world",region="UK")
```

``` r
uk_shapefile %>%
  distinct(subregion)
```

    ##          subregion
    ## 1    Isle of Wight
    ## 2            Wales
    ## 3 Northern Ireland
    ## 4         Scotland
    ## 5    Great Britain

``` r
usa_shapefile <- map_data("state") %>%
  rename(state=region)
```

``` r
usa_shapefile %>%
  distinct(state)
```

    ##                   state
    ## 1               alabama
    ## 2               arizona
    ## 3              arkansas
    ## 4            california
    ## 5              colorado
    ## 6           connecticut
    ## 7              delaware
    ## 8  district of columbia
    ## 9               florida
    ## 10              georgia
    ## 11                idaho
    ## 12             illinois
    ## 13              indiana
    ## 14                 iowa
    ## 15               kansas
    ## 16             kentucky
    ## 17            louisiana
    ## 18                maine
    ## 19             maryland
    ## 20        massachusetts
    ## 21             michigan
    ## 22            minnesota
    ## 23          mississippi
    ## 24             missouri
    ## 25              montana
    ## 26             nebraska
    ## 27               nevada
    ## 28        new hampshire
    ## 29           new jersey
    ## 30           new mexico
    ## 31             new york
    ## 32       north carolina
    ## 33         north dakota
    ## 34                 ohio
    ## 35             oklahoma
    ## 36               oregon
    ## 37         pennsylvania
    ## 38         rhode island
    ## 39       south carolina
    ## 40         south dakota
    ## 41            tennessee
    ## 42                texas
    ## 43                 utah
    ## 44              vermont
    ## 45             virginia
    ## 46           washington
    ## 47        west virginia
    ## 48            wisconsin
    ## 49              wyoming

``` r
usa_data <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/review-data.csv") %>%
  select(State..If.in.USA.) %>%
  rename(region=State..If.in.USA.) 
```

``` r
usa_data %>%
  mutate(region=as.list(region)) %>%
  mutate(across(region,str_replace_all,";",",")) %>%
  unnest(region,.sep=",") %>%
  count(region, name="count") #still need to remove grouped rows
```

    ## Warning: The `.sep` argument of `unnest()` is deprecated as of tidyr 1.0.0.
    ## ℹ Use `names_sep = ','` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 31 × 2
    ##    region                                                     count
    ##    <chr>                                                      <int>
    ##  1 ""                                                           883
    ##  2 "Arizona"                                                      1
    ##  3 "Arkansas"                                                     1
    ##  4 "California"                                                  12
    ##  5 "California, California, California"                           1
    ##  6 "California, California, Massachusetts"                        1
    ##  7 "California, New Jersey, California, Texas, Massachusetts"     1
    ##  8 "Colorado"                                                     1
    ##  9 "Florida"                                                      2
    ## 10 "Georgia"                                                      1
    ## # ℹ 21 more rows

- Alabama
- Arizona
- Arkansas
- California
- Colorado
- Florida
- Georgia
- Illinois
- Indiana
- Kentucky
- Maine
- Massachusetts
- Michigan
- Nebraska
- New Jersey
- New York
- North Carolina
- Ohio
- Oklahoma
- Pennsylvania
- Rhode Island
- South Dakota
- Tennessee
- Texas
- Utah
- Vermont
- Virginia
- Wisconsin

``` r
usa_data %>%
  str_count("Alabama") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- tibble(state="alabama",
       frequency=1)
```

``` r
usa_data %>%
  str_count("Arizona") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="arizona",frequency=1)
```

``` r
usa_data %>%
  str_count("Arkansas") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="arkansas",frequency=2)
```

``` r
usa_data %>%
  str_count("California") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 19

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="california",frequency=19)
```

``` r
usa_data %>%
  str_count("Colorado") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="colorado",frequency=1)
```

``` r
usa_data %>%
  str_count("Florida") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="florida",frequency=2)
```

``` r
usa_data %>%
  str_count("Georgia") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="georgia",frequency=1) 
```

``` r
usa_data %>%
  str_count("Illinois") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 5

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="illinois",frequency=5)
```

``` r
usa_data %>%
  str_count("Indiana") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="indiana",frequency=2)
```

``` r
usa_data %>%
  str_count("Kentucky") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="kentucky",frequency=1)
```

``` r
usa_data %>%
  str_count("Maine") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="maine",frequency=1)
```

``` r
usa_data %>%
  str_count("Massachusetts") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 3

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="massachusetts",frequency=3) 
```

``` r
usa_data %>%
  str_count("Michigan") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 4

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="michigan",frequency=4)
```

``` r
usa_data %>%
  str_count("Nebraska") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="nebraska",frequency=1)
```

``` r
usa_data %>%
  str_count("New Jersey") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="new jersey",frequency=2)
```

``` r
usa_data %>%
  str_count("New York") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 4

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="new york",frequency=4)
```

``` r
usa_data %>%
  str_count("North Carolina") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="north carolina",frequency=1)
```

``` r
usa_data %>%
  str_count("Ohio") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="ohio",frequency=2)
```

``` r
usa_data %>%
  str_count("Oklahoma") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="oklahoma",frequency=1)
```

``` r
usa_data %>%
  str_count("Pennsylvania") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 5

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="pennsylvania",frequency=5)
```

``` r
usa_data %>%
  str_count("Rhode Island") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="rhode island",frequency=1)
```

``` r
usa_data %>%
  str_count("South Dakota") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="south dakota",frequency=2)
```

``` r
usa_data %>%
  str_count("Tennessee") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="tennessee",frequency=1)
```

``` r
usa_data %>%
  str_count("Texas") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="texas",frequency=2)
```

``` r
usa_data %>%
  str_count("Utah") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 3

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="utah",frequency=3)
```

``` r
usa_data %>%
  str_count("Vermont") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="vermont",frequency=1)
```

``` r
usa_data %>%
  str_count("Virginia") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 3

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="virginia",frequency=3)
```

``` r
usa_data %>%
  str_count("Wisconsin") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 2

``` r
state_frequencies <- state_frequencies %>%
  add_row(state="wisconsin",frequency=2)
```

``` r
state_frequencies
```

    ## # A tibble: 28 × 2
    ##    state      frequency
    ##    <chr>          <dbl>
    ##  1 alabama            1
    ##  2 arizona            1
    ##  3 arkansas           2
    ##  4 california        19
    ##  5 colorado           1
    ##  6 florida            2
    ##  7 georgia            1
    ##  8 illinois           5
    ##  9 indiana            2
    ## 10 kentucky           1
    ## # ℹ 18 more rows

``` r
 state_frequencies %>%
  summarise(sum=sum(frequency))
```

    ## # A tibble: 1 × 1
    ##     sum
    ##   <dbl>
    ## 1    74

``` r
state_data <- left_join(usa_shapefile,state_frequencies,by="state")
```

``` r
state_data %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  coord_map() +
  theme(legend.key.width=unit(3,"lines"),legend.position="bottom",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

``` r
uk_data <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/review-data.csv") %>%
  select(country..if.in.UK.) %>%
  rename(region=country..if.in.UK.) 
```

``` r
uk_data %>%
  str_count("England") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 13

``` r
country_frequencies <- tibble(country="Great Britain",
       frequency=13)
```

``` r
uk_data %>%
  str_count("Scotland") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
country_frequencies <- country_frequencies %>%
  add_row(country="Scotland",frequency=1)
```

``` r
uk_data %>%
  str_count("Wales") 
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

``` r
country_frequencies <- country_frequencies %>%
  add_row(country="Wales",frequency=1)
```

``` r
uk_shapefile %>%
  distinct(subregion)
```

    ##          subregion
    ## 1    Isle of Wight
    ## 2            Wales
    ## 3 Northern Ireland
    ## 4         Scotland
    ## 5    Great Britain

``` r
uk_shapefile <- uk_shapefile %>%
  rename(country=subregion)
```

``` r
uk_shapefile
```

    ##             long      lat group order region          country
    ## 1    -1.06557596 50.69024     1     1     UK    Isle of Wight
    ## 2    -1.14936531 50.65571     1     2     UK    Isle of Wight
    ## 3    -1.17583025 50.61523     1     3     UK    Isle of Wight
    ## 4    -1.19609356 50.59922     1     4     UK    Isle of Wight
    ## 5    -1.25146472 50.58882     1     5     UK    Isle of Wight
    ## 6    -1.30629909 50.58853     1     6     UK    Isle of Wight
    ## 7    -1.51533186 50.66978     1     7     UK    Isle of Wight
    ## 8    -1.56342769 50.66611     1     8     UK    Isle of Wight
    ## 9    -1.51567400 50.70332     1     9     UK    Isle of Wight
    ## 10   -1.38583994 50.73355     1    10     UK    Isle of Wight
    ## 11   -1.31279302 50.77348     1    11     UK    Isle of Wight
    ## 12   -1.14423859 50.73472     1    12     UK    Isle of Wight
    ## 13   -1.06557596 50.69024     1    13     UK    Isle of Wight
    ## 15   -4.19677734 53.32143     2    15     UK            Wales
    ## 16   -4.15488291 53.30283     2    16     UK            Wales
    ## 17   -4.04936552 53.30576     2    17     UK            Wales
    ## 18   -4.08427763 53.26431     2    18     UK            Wales
    ## 19   -4.20039082 53.21807     2    19     UK            Wales
    ## 20   -4.27861309 53.17241     2    20     UK            Wales
    ## 21   -4.37304688 53.13418     2    21     UK            Wales
    ## 22   -4.41884756 53.17803     2    22     UK            Wales
    ## 23   -4.47197294 53.17636     2    23     UK            Wales
    ## 24   -4.55322266 53.26045     2    24     UK            Wales
    ## 25   -4.56787109 53.38647     2    25     UK            Wales
    ## 26   -4.46171856 53.41929     2    26     UK            Wales
    ## 27   -4.31508780 53.41724     2    27     UK            Wales
    ## 28   -4.19677734 53.32143     2    28     UK            Wales
    ## 30   -6.21801758 54.08872     3    30     UK Northern Ireland
    ## 31   -6.30366230 54.09487     3    31     UK Northern Ireland
    ## 32   -6.36367178 54.07710     3    32     UK Northern Ireland
    ## 33   -6.40258789 54.06064     3    33     UK Northern Ireland
    ## 34   -6.44028378 54.06363     3    34     UK Northern Ireland
    ## 35   -6.54814482 54.05728     3    35     UK Northern Ireland
    ## 36   -6.64980459 54.05864     3    36     UK Northern Ireland
    ## 37   -6.66420937 54.08477     3    37     UK Northern Ireland
    ## 38   -6.64687443 54.16343     3    38     UK Northern Ireland
    ## 39   -6.66953135 54.18472     3    39     UK Northern Ireland
    ## 40   -6.76660204 54.19561     3    40     UK Northern Ireland
    ## 41   -6.80258799 54.21436     3    41     UK Northern Ireland
    ## 42   -6.85834980 54.26865     3    42     UK Northern Ireland
    ## 43   -6.86923885 54.29404     3    43     UK Northern Ireland
    ## 44   -6.87724638 54.32910     3    44     UK Northern Ireland
    ## 45   -6.93613243 54.37432     3    45     UK Northern Ireland
    ## 46   -7.00771523 54.40669     3    46     UK Northern Ireland
    ## 47   -7.04970741 54.40825     3    47     UK Northern Ireland
    ## 48   -7.13349581 54.35537     3    48     UK Northern Ireland
    ## 49   -7.20258713 54.30181     3    49     UK Northern Ireland
    ## 50   -7.17807627 54.27490     3    50     UK Northern Ireland
    ## 51   -7.15546894 54.23950     3    51     UK Northern Ireland
    ## 52   -7.19306612 54.21411     3    52     UK Northern Ireland
    ## 53   -7.30673838 54.15601     3    53     UK Northern Ireland
    ## 54   -7.32451200 54.13345     3    54     UK Northern Ireland
    ## 55   -7.35517550 54.12124     3    55     UK Northern Ireland
    ## 56   -7.40942335 54.13731     3    56     UK Northern Ireland
    ## 57   -7.54443407 54.13359     3    57     UK Northern Ireland
    ## 58   -7.60654259 54.14385     3    58     UK Northern Ireland
    ## 59   -7.67876005 54.18667     3    59     UK Northern Ireland
    ## 60   -7.85493183 54.21528     3    60     UK Northern Ireland
    ## 61   -7.88447237 54.28379     3    61     UK Northern Ireland
    ## 62   -7.91845703 54.29658     3    62     UK Northern Ireland
    ## 63   -8.11826229 54.41426     3    63     UK Northern Ireland
    ## 64   -8.14482403 54.45351     3    64     UK Northern Ireland
    ## 65   -8.11894512 54.47696     3    65     UK Northern Ireland
    ## 66   -8.04433632 54.51245     3    66     UK Northern Ireland
    ## 67   -7.79379892 54.57124     3    67     UK Northern Ireland
    ## 68   -7.75439501 54.59492     3    68     UK Northern Ireland
    ## 69   -7.74628925 54.61582     3    69     UK Northern Ireland
    ## 70   -7.81982470 54.63970     3    70     UK Northern Ireland
    ## 71   -7.88613319 54.66607     3    71     UK Northern Ireland
    ## 72   -7.90874004 54.68335     3    72     UK Northern Ireland
    ## 73   -7.91059542 54.69834     3    73     UK Northern Ireland
    ## 74   -7.87295008 54.71787     3    74     UK Northern Ireland
    ## 75   -7.79726553 54.71928     3    75     UK Northern Ireland
    ## 76   -7.73749971 54.71045     3    76     UK Northern Ireland
    ## 77   -7.68999052 54.72803     3    77     UK Northern Ireland
    ## 78   -7.60644531 54.74570     3    78     UK Northern Ireland
    ## 79   -7.55039072 54.76797     3    79     UK Northern Ireland
    ## 80   -7.50219679 54.82544     3    80     UK Northern Ireland
    ## 81   -7.45126915 54.87710     3    81     UK Northern Ireland
    ## 82   -7.44599628 54.90513     3    82     UK Northern Ireland
    ## 83   -7.40141582 55.00332     3    83     UK Northern Ireland
    ## 84   -7.37690401 55.02769     3    84     UK Northern Ireland
    ## 85   -7.21865177 55.09199     3    85     UK Northern Ireland
    ## 86   -7.17861319 55.05688     3    86     UK Northern Ireland
    ## 87   -7.10063505 55.04829     3    87     UK Northern Ireland
    ## 88   -7.03076172 55.08062     3    88     UK Northern Ireland
    ## 89   -6.94716787 55.18252     3    89     UK Northern Ireland
    ## 90   -6.88896513 55.18892     3    90     UK Northern Ireland
    ## 91   -6.82485294 55.18066     3    91     UK Northern Ireland
    ## 92   -6.69882822 55.19346     3    92     UK Northern Ireland
    ## 93   -6.47504902 55.24102     3    93     UK Northern Ireland
    ## 94   -6.37529278 55.24180     3    94     UK Northern Ireland
    ## 95   -6.23422813 55.21684     3    95     UK Northern Ireland
    ## 96   -6.12914991 55.21738     3    96     UK Northern Ireland
    ## 97   -6.03579140 55.14453     3    97     UK Northern Ireland
    ## 98   -5.98574209 55.02969     3    98     UK Northern Ireland
    ## 99   -5.86918926 54.91621     3    99     UK Northern Ireland
    ## 100  -5.71684551 54.81748     3   100     UK Northern Ireland
    ## 101  -5.71074200 54.75708     3   101     UK Northern Ireland
    ## 102  -5.76518536 54.72466     3   102     UK Northern Ireland
    ## 103  -5.87910175 54.68438     3   103     UK Northern Ireland
    ## 104  -5.87861347 54.64131     3   104     UK Northern Ireland
    ## 105  -5.80346680 54.66304     3   105     UK Northern Ireland
    ## 106  -5.73862314 54.67305     3   106     UK Northern Ireland
    ## 107  -5.58252001 54.66343     3   107     UK Northern Ireland
    ## 108  -5.52792931 54.61963     3   108     UK Northern Ireland
    ## 109  -5.49018526 54.55405     3   109     UK Northern Ireland
    ## 110  -5.47041035 54.50020     3   110     UK Northern Ireland
    ## 111  -5.48388672 54.44165     3   111     UK Northern Ireland
    ## 112  -5.52587891 54.46021     3   112     UK Northern Ireland
    ## 113  -5.56855440 54.51260     3   113     UK Northern Ireland
    ## 114  -5.61596680 54.53672     3   114     UK Northern Ireland
    ## 115  -5.67109346 54.54976     3   115     UK Northern Ireland
    ## 116  -5.64609385 54.47788     3   116     UK Northern Ireland
    ## 117  -5.65595675 54.38174     3   117     UK Northern Ireland
    ## 118  -5.63188457 54.37266     3   118     UK Northern Ireland
    ## 119  -5.55781221 54.37100     3   119     UK Northern Ireland
    ## 120  -5.60678768 54.27256     3   120     UK Northern Ireland
    ## 121  -5.70805645 54.24585     3   121     UK Northern Ireland
    ## 122  -5.82617188 54.23584     3   122     UK Northern Ireland
    ## 123  -5.85463858 54.20098     3   123     UK Northern Ireland
    ## 124  -5.87607384 54.15606     3   124     UK Northern Ireland
    ## 125  -5.93774414 54.08907     3   125     UK Northern Ireland
    ## 126  -6.01904297 54.05127     3   126     UK Northern Ireland
    ## 127  -6.11953163 54.05889     3   127     UK Northern Ireland
    ## 128  -6.21801758 54.08872     3   128     UK Northern Ireland
    ## 130  -5.10542011 55.44883     4   130     UK         Scotland
    ## 131  -5.23149395 55.44809     4   131     UK         Scotland
    ## 132  -5.27705050 55.45674     4   132     UK         Scotland
    ## 133  -5.33149433 55.48106     4   133     UK         Scotland
    ## 134  -5.39267588 55.61836     4   134     UK         Scotland
    ## 135  -5.37080097 55.66694     4   135     UK         Scotland
    ## 136  -5.34570312 55.69072     4   136     UK         Scotland
    ## 137  -5.31811523 55.70918     4   137     UK         Scotland
    ## 138  -5.25161123 55.71694     4   138     UK         Scotland
    ## 139  -5.18544912 55.69096     4   139     UK         Scotland
    ## 140  -5.16040039 55.66680     4   140     UK         Scotland
    ## 141  -5.10498095 55.57397     4   141     UK         Scotland
    ## 142  -5.09472656 55.49434     4   142     UK         Scotland
    ## 143  -5.10542011 55.44883     4   143     UK         Scotland
    ## 145  -6.12890577 55.93057     5   145     UK         Scotland
    ## 146  -6.09282255 55.80215     5   146     UK         Scotland
    ## 147  -6.05761719 55.72251     5   147     UK         Scotland
    ## 148  -6.05532217 55.69531     5   148     UK         Scotland
    ## 149  -6.08837891 55.65752     5   149     UK         Scotland
    ## 150  -6.25317335 55.60723     5   150     UK         Scotland
    ## 151  -6.30507803 55.60693     5   151     UK         Scotland
    ## 152  -6.30722618 55.61914     5   152     UK         Scotland
    ## 153  -6.27001953 55.67031     5   153     UK         Scotland
    ## 154  -6.30205107 55.72837     5   154     UK         Scotland
    ## 155  -6.28642607 55.77251     5   155     UK         Scotland
    ## 156  -6.30175734 55.78061     5   156     UK         Scotland
    ## 157  -6.33388662 55.77436     5   157     UK         Scotland
    ## 158  -6.45195341 55.70425     5   158     UK         Scotland
    ## 159  -6.49135780 55.69732     5   159     UK         Scotland
    ## 160  -6.49565411 55.71157     5   160     UK         Scotland
    ## 161  -6.46645498 55.76899     5   161     UK         Scotland
    ## 162  -6.46284199 55.80825     5   162     UK         Scotland
    ## 163  -6.44526339 55.83237     5   163     UK         Scotland
    ## 164  -6.41318369 55.85464     5   164     UK         Scotland
    ## 165  -6.37495136 55.87134     5   165     UK         Scotland
    ## 166  -6.34414053 55.87373     5   166     UK         Scotland
    ## 167  -6.31127930 55.85649     5   167     UK         Scotland
    ## 168  -6.21567345 55.90459     5   168     UK         Scotland
    ## 169  -6.12890577 55.93057     5   169     UK         Scotland
    ## 171  -5.97006845 55.81455     6   171     UK         Scotland
    ## 172  -5.99091768 55.80381     6   172     UK         Scotland
    ## 173  -6.04155302 55.80679     6   173     UK         Scotland
    ## 174  -6.06035185 55.82290     6   174     UK         Scotland
    ## 175  -6.07070351 55.84766     6   175     UK         Scotland
    ## 176  -6.07197237 55.89312     6   176     UK         Scotland
    ## 177  -6.04130888 55.92563     6   177     UK         Scotland
    ## 178  -5.91176796 55.97475     6   178     UK         Scotland
    ## 179  -5.97031212 55.99219     6   179     UK         Scotland
    ## 180  -5.97265625 56.00444     6   180     UK         Scotland
    ## 181  -5.93906307 56.04527     6   181     UK         Scotland
    ## 182  -5.79960918 56.10879     6   182     UK         Scotland
    ## 183  -5.76225615 56.12031     6   183     UK         Scotland
    ## 184  -5.72514629 56.11856     6   184     UK         Scotland
    ## 185  -5.79721689 56.00562     6   185     UK         Scotland
    ## 186  -5.97006845 55.81455     6   186     UK         Scotland
    ## 188  -5.77788067 56.34434     7   188     UK         Scotland
    ## 189  -6.17617178 56.28872     7   189     UK         Scotland
    ## 190  -6.31342793 56.29365     7   190     UK         Scotland
    ## 191  -6.32582998 56.32095     7   191     UK         Scotland
    ## 192  -6.29848623 56.33916     7   192     UK         Scotland
    ## 193  -6.18486357 56.35688     7   193     UK         Scotland
    ## 194  -6.13886690 56.49062     7   194     UK         Scotland
    ## 195  -6.31064463 56.55215     7   195     UK         Scotland
    ## 196  -6.31967735 56.56944     7   196     UK         Scotland
    ## 197  -6.30625010 56.59878     7   197     UK         Scotland
    ## 198  -6.28632832 56.61187     7   198     UK         Scotland
    ## 199  -6.18207979 56.64297     7   199     UK         Scotland
    ## 200  -6.13828135 56.64985     7   200     UK         Scotland
    ## 201  -6.10273409 56.64566     7   201     UK         Scotland
    ## 202  -6.02959013 56.60981     7   202     UK         Scotland
    ## 203  -5.94668007 56.53452     7   203     UK         Scotland
    ## 204  -5.83603525 56.52256     7   204     UK         Scotland
    ## 205  -5.76083994 56.49067     7   205     UK         Scotland
    ## 206  -5.77788067 56.34434     7   206     UK         Scotland
    ## 208  -6.60761786 56.58501     8   208     UK         Scotland
    ## 209  -6.66445303 56.57944     8   209     UK         Scotland
    ## 210  -6.66855478 56.59361     8   210     UK         Scotland
    ## 211  -6.56992197 56.66123     8   211     UK         Scotland
    ## 212  -6.50605488 56.67236     8   212     UK         Scotland
    ## 213  -6.48369122 56.66577     8   213     UK         Scotland
    ## 214  -6.53007793 56.62661     8   214     UK         Scotland
    ## 215  -6.60761786 56.58501     8   215     UK         Scotland
    ## 217  -7.41689491 56.96543     9   217     UK         Scotland
    ## 218  -7.50478458 56.95166     9   218     UK         Scotland
    ## 219  -7.53740215 56.95972     9   219     UK         Scotland
    ## 220  -7.54296875 56.97236     9   220     UK         Scotland
    ## 221  -7.52294874 57.00679     9   221     UK         Scotland
    ## 222  -7.45546913 57.01894     9   222     UK         Scotland
    ## 223  -7.40668964 57.00029     9   223     UK         Scotland
    ## 224  -7.39892626 56.98335     9   224     UK         Scotland
    ## 225  -7.41689491 56.96543     9   225     UK         Scotland
    ## 227  -6.27905273 56.96469    10   227     UK         Scotland
    ## 228  -6.30874062 56.95181    10   228     UK         Scotland
    ## 229  -6.34624052 56.95430    10   229     UK         Scotland
    ## 230  -6.38339853 56.97090    10   230     UK         Scotland
    ## 231  -6.43261719 57.01792    10   231     UK         Scotland
    ## 232  -6.32236338 57.05054    10   232     UK         Scotland
    ## 233  -6.27822304 57.03139    10   233     UK         Scotland
    ## 234  -6.26127911 57.00952    10   234     UK         Scotland
    ## 235  -6.26054716 56.98526    10   235     UK         Scotland
    ## 236  -6.27905273 56.96469    10   236     UK         Scotland
    ## 238  -7.24985313 57.11533    11   238     UK         Scotland
    ## 239  -7.29204035 57.10977    11   239     UK         Scotland
    ## 240  -7.34741211 57.11514    11   240     UK         Scotland
    ## 241  -7.38149452 57.13066    11   241     UK         Scotland
    ## 242  -7.41591787 57.19214    11   242     UK         Scotland
    ## 243  -7.42236328 57.22935    11   243     UK         Scotland
    ## 244  -7.40703106 57.29848    11   244     UK         Scotland
    ## 245  -7.41054678 57.38110    11   245     UK         Scotland
    ## 246  -7.29638624 57.38369    11   246     UK         Scotland
    ## 247  -7.26713848 57.37178    11   247     UK         Scotland
    ## 248  -7.24755907 57.12637    11   248     UK         Scotland
    ## 249  -7.24985313 57.11533    11   249     UK         Scotland
    ## 251  -6.14472675 57.50498    12   251     UK         Scotland
    ## 252  -6.14614248 57.46079    12   252     UK         Scotland
    ## 253  -6.16376925 57.40884    12   253     UK         Scotland
    ## 254  -6.14082050 57.35366    12   254     UK         Scotland
    ## 255  -6.13554668 57.31425    12   255     UK         Scotland
    ## 256  -6.09340858 57.30171    12   256     UK         Scotland
    ## 257  -6.06762695 57.28355    12   257     UK         Scotland
    ## 258  -5.88027334 57.26323    12   258     UK         Scotland
    ## 259  -5.70600557 57.26894    12   259     UK         Scotland
    ## 260  -5.67246103 57.25269    12   260     UK         Scotland
    ## 261  -5.66865253 57.22690    12   261     UK         Scotland
    ## 262  -5.69619131 57.19844    12   262     UK         Scotland
    ## 263  -5.79541016 57.14653    12   263     UK         Scotland
    ## 264  -5.91376925 57.06265    12   264     UK         Scotland
    ## 265  -5.94907236 57.04517    12   265     UK         Scotland
    ## 266  -5.98730469 57.04443    12   266     UK         Scotland
    ## 267  -6.01474619 57.05195    12   267     UK         Scotland
    ## 268  -6.03437471 57.20122    12   268     UK         Scotland
    ## 269  -6.16274452 57.18213    12   269     UK         Scotland
    ## 270  -6.26611328 57.18433    12   270     UK         Scotland
    ## 271  -6.32270479 57.20249    12   271     UK         Scotland
    ## 272  -6.36240244 57.23750    12   272     UK         Scotland
    ## 273  -6.44243145 57.32749    12   273     UK         Scotland
    ## 274  -6.67543983 57.36289    12   274     UK         Scotland
    ## 275  -6.74130821 57.41245    12   275     UK         Scotland
    ## 276  -6.76113319 57.44238    12   276     UK         Scotland
    ## 277  -6.75273418 57.45893    12   277     UK         Scotland
    ## 278  -6.70419884 57.49575    12   278     UK         Scotland
    ## 279  -6.64345694 57.48262    12   279     UK         Scotland
    ## 280  -6.60585976 57.49067    12   280     UK         Scotland
    ## 281  -6.58300781 57.50713    12   281     UK         Scotland
    ## 282  -6.58349609 57.52066    12   282     UK         Scotland
    ## 283  -6.61528301 57.55273    12   283     UK         Scotland
    ## 284  -6.61679697 57.56270    12   284     UK         Scotland
    ## 285  -6.37851572 57.60332    12   285     UK         Scotland
    ## 286  -6.35766554 57.66679    12   286     UK         Scotland
    ## 287  -6.30595684 57.67197    12   287     UK         Scotland
    ## 288  -6.24692392 57.65122    12   288     UK         Scotland
    ## 289  -6.16606474 57.58530    12   289     UK         Scotland
    ## 290  -6.14472675 57.50498    12   290     UK         Scotland
    ## 292  -7.20556593 57.68296    13   292     UK         Scotland
    ## 293  -7.09277344 57.62666    13   293     UK         Scotland
    ## 294  -7.18261766 57.53330    13   294     UK         Scotland
    ## 295  -7.32055664 57.53374    13   295     UK         Scotland
    ## 296  -7.51474571 57.60196    13   296     UK         Scotland
    ## 297  -7.51562500 57.61587    13   297     UK         Scotland
    ## 298  -7.49941444 57.63633    13   298     UK         Scotland
    ## 299  -7.47031260 57.65254    13   299     UK         Scotland
    ## 300  -7.44003868 57.65639    13   300     UK         Scotland
    ## 301  -7.39189434 57.64521    13   301     UK         Scotland
    ## 302  -7.32485390 57.66314    13   302     UK         Scotland
    ## 303  -7.27119160 57.65747    13   303     UK         Scotland
    ## 304  -7.20556593 57.68296    13   304     UK         Scotland
    ## 306  -6.19868135 58.36329    14   306     UK         Scotland
    ## 307  -6.32582998 58.18887    14   307     UK         Scotland
    ## 308  -6.37558651 58.18457    14   308     UK         Scotland
    ## 309  -6.41928768 58.14097    14   309     UK         Scotland
    ## 310  -6.55458975 58.09287    14   310     UK         Scotland
    ## 311  -6.43652344 58.09189    14   311     UK         Scotland
    ## 312  -6.40336895 58.07587    14   312     UK         Scotland
    ## 313  -6.40244150 58.04136    14   313     UK         Scotland
    ## 314  -6.42519522 58.02129    14   314     UK         Scotland
    ## 315  -6.57812500 57.94136    14   315     UK         Scotland
    ## 316  -6.68330050 57.91104    14   316     UK         Scotland
    ## 317  -6.79658175 57.82754    14   317     UK         Scotland
    ## 318  -6.85375977 57.82651    14   318     UK         Scotland
    ## 319  -6.91035175 57.77339    14   319     UK         Scotland
    ## 320  -6.95693350 57.75005    14   320     UK         Scotland
    ## 321  -6.98310518 57.75000    14   321     UK         Scotland
    ## 322  -7.01318312 57.76177    14   322     UK         Scotland
    ## 323  -7.08344746 57.81377    14   323     UK         Scotland
    ## 324  -6.95595694 57.86489    14   324     UK         Scotland
    ## 325  -6.94414043 57.89365    14   325     UK         Scotland
    ## 326  -6.85683584 57.92353    14   326     UK         Scotland
    ## 327  -6.86416054 57.93286    14   327     UK         Scotland
    ## 328  -7.00253868 57.97491    14   328     UK         Scotland
    ## 329  -7.05707979 58.00317    14   329     UK         Scotland
    ## 330  -7.05190420 58.01797    14   330     UK         Scotland
    ## 331  -6.98530293 58.05049    14   331     UK         Scotland
    ## 332  -7.01689434 58.05478    14   332     UK         Scotland
    ## 333  -7.03823233 58.07232    14   333     UK         Scotland
    ## 334  -7.07690430 58.07900    14   334     UK         Scotland
    ## 335  -7.08847618 58.09536    14   335     UK         Scotland
    ## 336  -7.09560585 58.13828    14   336     UK         Scotland
    ## 337  -7.08525419 58.18218    14   337     UK         Scotland
    ## 338  -7.04492188 58.20156    14   338     UK         Scotland
    ## 339  -7.02841806 58.22232    14   339     UK         Scotland
    ## 340  -7.01206064 58.22871    14   340     UK         Scotland
    ## 341  -6.94956017 58.21768    14   341     UK         Scotland
    ## 342  -6.88622999 58.18257    14   342     UK         Scotland
    ## 343  -6.81230497 58.19609    14   343     UK         Scotland
    ## 344  -6.72646523 58.18941    14   344     UK         Scotland
    ## 345  -6.72470713 58.19756    14   345     UK         Scotland
    ## 346  -6.78774405 58.28389    14   346     UK         Scotland
    ## 347  -6.77646446 58.30151    14   347     UK         Scotland
    ## 348  -6.74228525 58.32163    14   348     UK         Scotland
    ## 349  -6.54418898 58.38315    14   349     UK         Scotland
    ## 350  -6.29716778 58.48662    14   350     UK         Scotland
    ## 351  -6.23745108 58.50283    14   351     UK         Scotland
    ## 352  -6.21943331 58.48872    14   352     UK         Scotland
    ## 353  -6.19423771 58.43511    14   353     UK         Scotland
    ## 354  -6.19868135 58.36329    14   354     UK         Scotland
    ## 356  -3.10966778 58.51548    15   356     UK    Great Britain
    ## 357  -3.10112309 58.43369    15   357     UK    Great Britain
    ## 358  -3.11289048 58.40889    15   358     UK    Great Britain
    ## 359  -3.13676739 58.37832    15   359     UK    Great Britain
    ## 360  -3.21235394 58.32124    15   360     UK    Great Britain
    ## 361  -3.41098619 58.23965    15   361     UK    Great Britain
    ## 362  -3.77499986 58.05210    15   362     UK    Great Britain
    ## 363  -3.99003911 57.95903    15   363     UK    Great Britain
    ## 364  -4.01962900 57.91426    15   364     UK    Great Britain
    ## 365  -4.03559542 57.85200    15   365     UK    Great Britain
    ## 366  -3.90683579 57.83965    15   366     UK    Great Britain
    ## 367  -3.85712910 57.81855    15   367     UK    Great Britain
    ## 368  -3.88793921 57.78691    15   368     UK    Great Britain
    ## 369  -4.07841778 57.67705    15   369     UK    Great Britain
    ## 370  -4.13452148 57.57774    15   370     UK    Great Britain
    ## 371  -3.98847651 57.58125    15   371     UK    Great Britain
    ## 372  -3.86816406 57.60035    15   372     UK    Great Britain
    ## 373  -3.62822247 57.66225    15   373     UK    Great Britain
    ## 374  -3.40278316 57.70826    15   374     UK    Great Britain
    ## 375  -3.29453135 57.71015    15   375     UK    Great Britain
    ## 376  -3.08393574 57.67349    15   376     UK    Great Britain
    ## 377  -3.03603506 57.67231    15   377     UK    Great Britain
    ## 378  -2.94667959 57.68926    15   378     UK    Great Britain
    ## 379  -2.85629892 57.69229    15   379     UK    Great Britain
    ## 380  -2.24414039 57.68086    15   380     UK    Great Britain
    ## 381  -2.07407236 57.70239    15   381     UK    Great Britain
    ## 382  -1.96152341 57.67666    15   382     UK    Great Britain
    ## 383  -1.86738288 57.61235    15   383     UK    Great Britain
    ## 384  -1.77792978 57.49375    15   384     UK    Great Britain
    ## 385  -1.78066409 57.47402    15   385     UK    Great Britain
    ## 386  -1.83471692 57.41997    15   386     UK    Great Britain
    ## 387  -1.93447280 57.35220    15   387     UK    Great Britain
    ## 388  -2.02031279 57.25888    15   388     UK    Great Britain
    ## 389  -2.04550815 57.20855    15   389     UK    Great Britain
    ## 390  -2.06235361 57.15347    15   390     UK    Great Britain
    ## 391  -2.08955073 57.10254    15   391     UK    Great Britain
    ## 392  -2.26025367 56.86333    15   392     UK    Great Britain
    ## 393  -2.42666006 56.73071    15   393     UK    Great Britain
    ## 394  -2.50097680 56.63657    15   394     UK    Great Britain
    ## 395  -2.59267569 56.56157    15   395     UK    Great Britain
    ## 396  -2.68095732 56.51440    15   396     UK    Great Britain
    ## 397  -2.77519560 56.48296    15   397     UK    Great Britain
    ## 398  -3.04741192 56.44936    15   398     UK    Great Britain
    ## 399  -3.12358356 56.42529    15   399     UK    Great Britain
    ## 400  -3.21445298 56.38393    15   400     UK    Great Britain
    ## 401  -3.30996108 56.36348    15   401     UK    Great Britain
    ## 402  -3.19799829 56.36607    15   402     UK    Great Britain
    ## 403  -3.08701181 56.38906    15   403     UK    Great Britain
    ## 404  -2.88515639 56.39751    15   404     UK    Great Britain
    ## 405  -2.65273452 56.31826    15   405     UK    Great Britain
    ## 406  -2.67426729 56.25342    15   406     UK    Great Britain
    ## 407  -2.76757812 56.20215    15   407     UK    Great Britain
    ## 408  -2.97978497 56.19409    15   408     UK    Great Britain
    ## 409  -3.17822266 56.08012    15   409     UK    Great Britain
    ## 410  -3.26777363 56.04507    15   410     UK    Great Britain
    ## 411  -3.36225629 56.02763    15   411     UK    Great Britain
    ## 412  -3.48042011 56.03281    15   412     UK    Great Britain
    ## 413  -3.69511700 56.06333    15   413     UK    Great Britain
    ## 414  -3.78906274 56.09521    15   414     UK    Great Britain
    ## 415  -3.70415044 56.04316    15   415     UK    Great Britain
    ## 416  -3.60781264 56.01602    15   416     UK    Great Britain
    ## 417  -3.04873037 55.95195    15   417     UK    Great Britain
    ## 418  -3.01508784 55.95859    15   418     UK    Great Britain
    ## 419  -2.83686519 56.02627    15   419     UK    Great Britain
    ## 420  -2.59931636 56.02729    15   420     UK    Great Britain
    ## 421  -2.14707065 55.90298    15   421     UK    Great Britain
    ## 422  -2.01684570 55.80796    15   422     UK    Great Britain
    ## 423  -1.83027363 55.67173    15   423     UK    Great Britain
    ## 424  -1.72875977 55.61855    15   424     UK    Great Britain
    ## 425  -1.65537143 55.57036    15   425     UK    Great Britain
    ## 426  -1.61015642 55.49809    15   426     UK    Great Britain
    ## 427  -1.52255857 55.25952    15   427     UK    Great Britain
    ## 428  -1.42265642 55.02642    15   428     UK    Great Britain
    ## 429  -1.29174817 54.77388    15   429     UK    Great Britain
    ## 430  -1.23242199 54.70371    15   430     UK    Great Britain
    ## 431  -1.15439487 54.65449    15   431     UK    Great Britain
    ## 432  -0.75932604 54.54141    15   432     UK    Great Britain
    ## 433  -0.67138678 54.50391    15   433     UK    Great Britain
    ## 434  -0.51811540 54.39512    15   434     UK    Great Britain
    ## 435  -0.37036106 54.27920    15   435     UK    Great Britain
    ## 436  -0.23286150 54.19014    15   436     UK    Great Britain
    ## 437  -0.08437490 54.11806    15   437     UK    Great Britain
    ## 438  -0.15629888 54.08062    15   438     UK    Great Britain
    ## 439  -0.20556638 54.02173    15   439     UK    Great Britain
    ## 440  -0.16874981 53.94165    15   440     UK    Great Britain
    ## 441  -0.10825178 53.86519    15   441     UK    Great Britain
    ## 442   0.00000000 53.75368    15   442     UK    Great Britain
    ## 443   0.01054701 53.74282    15   443     UK    Great Britain
    ## 444   0.11533183 53.60928    15   444     UK    Great Britain
    ## 445   0.07670873 53.62944    15   445     UK    Great Britain
    ## 446   0.03608374 53.64053    15   446     UK    Great Britain
    ## 447   0.00000000 53.63837    15   447     UK    Great Britain
    ## 448  -0.01943358 53.63721    15   448     UK    Great Britain
    ## 449  -0.07373050 53.64365    15   449     UK    Great Britain
    ## 450  -0.17382795 53.68545    15   450     UK    Great Britain
    ## 451  -0.27001956 53.73677    15   451     UK    Great Britain
    ## 452  -0.46137714 53.71616    15   452     UK    Great Britain
    ## 453  -0.56767571 53.72539    15   453     UK    Great Britain
    ## 454  -0.65991217 53.72403    15   454     UK    Great Britain
    ## 455  -0.48505864 53.69439    15   455     UK    Great Britain
    ## 456  -0.29370105 53.69233    15   456     UK    Great Britain
    ## 457   0.00000000 53.53639    15   457     UK    Great Britain
    ## 458   0.12832020 53.46826    15   458     UK    Great Britain
    ## 459   0.27099586 53.33550    15   459     UK    Great Britain
    ## 460   0.35576153 53.15996    15   460     UK    Great Britain
    ## 461   0.29804692 53.08110    15   461     UK    Great Britain
    ## 462   0.20820312 53.03003    15   462     UK    Great Britain
    ## 463   0.12441435 52.97158    15   463     UK    Great Britain
    ## 464   0.04589850 52.90561    15   464     UK    Great Britain
    ## 465   0.27978504 52.80869    15   465     UK    Great Britain
    ## 466   0.33017552 52.81162    15   466     UK    Great Britain
    ## 467   0.38193366 52.82520    15   467     UK    Great Britain
    ## 468   0.43164060 52.85815    15   468     UK    Great Britain
    ## 469   0.51552737 52.93838    15   469     UK    Great Britain
    ## 470   0.55878913 52.96694    15   470     UK    Great Britain
    ## 471   0.70449227 52.97725    15   471     UK    Great Britain
    ## 472   0.82675809 52.97110    15   472     UK    Great Britain
    ## 473   0.94853503 52.95337    15   473     UK    Great Britain
    ## 474   1.05556643 52.95898    15   474     UK    Great Britain
    ## 475   1.27128899 52.92456    15   475     UK    Great Britain
    ## 476   1.38212895 52.89351    15   476     UK    Great Britain
    ## 477   1.65673840 52.75371    15   477     UK    Great Britain
    ## 478   1.71611345 52.67725    15   478     UK    Great Britain
    ## 479   1.74335933 52.57852    15   479     UK    Great Britain
    ## 480   1.74658227 52.46899    15   480     UK    Great Britain
    ## 481   1.70039034 52.36890    15   481     UK    Great Britain
    ## 482   1.64736307 52.27852    15   482     UK    Great Britain
    ## 483   1.61464834 52.16182    15   483     UK    Great Britain
    ## 484   1.59140635 52.11978    15   484     UK    Great Britain
    ## 485   1.55898440 52.08686    15   485     UK    Great Britain
    ## 486   1.41347659 51.99477    15   486     UK    Great Britain
    ## 487   1.31679690 51.95694    15   487     UK    Great Britain
    ## 488   1.27597642 51.97354    15   488     UK    Great Britain
    ## 489   1.23242199 51.97124    15   489     UK    Great Britain
    ## 490   1.22783196 51.94912    15   490     UK    Great Britain
    ## 491   1.27382779 51.90210    15   491     UK    Great Britain
    ## 492   1.27441394 51.84536    15   492     UK    Great Britain
    ## 493   1.18847656 51.80337    15   493     UK    Great Britain
    ## 494   1.10117161 51.78545    15   494     UK    Great Britain
    ## 495   0.95507824 51.80781    15   495     UK    Great Britain
    ## 496   0.75224602 51.72959    15   496     UK    Great Britain
    ## 497   0.89804661 51.68940    15   497     UK    Great Britain
    ## 498   0.92744160 51.64663    15   498     UK    Great Britain
    ## 499   0.89091790 51.57143    15   499     UK    Great Britain
    ## 500   0.79921883 51.53789    15   500     UK    Great Britain
    ## 501   0.69755834 51.52305    15   501     UK    Great Britain
    ## 502   0.59345710 51.51948    15   502     UK    Great Britain
    ## 503   0.50722635 51.50108    15   503     UK    Great Britain
    ## 504   0.42451188 51.46563    15   504     UK    Great Britain
    ## 505   0.52832037 51.48447    15   505     UK    Great Britain
    ## 506   0.60029304 51.46797    15   506     UK    Great Britain
    ## 507   0.64550799 51.40469    15   507     UK    Great Britain
    ## 508   0.68652320 51.38657    15   508     UK    Great Britain
    ## 509   0.88935542 51.35952    15   509     UK    Great Britain
    ## 510   1.01494145 51.35972    15   510     UK    Great Britain
    ## 511   1.25712895 51.37510    15   511     UK    Great Britain
    ## 512   1.37343776 51.37471    15   512     UK    Great Britain
    ## 513   1.41494167 51.36328    15   513     UK    Great Britain
    ## 514   1.41562510 51.31084    15   514     UK    Great Britain
    ## 515   1.39755857 51.18203    15   515     UK    Great Britain
    ## 516   1.36552751 51.15547    15   516     UK    Great Britain
    ## 517   1.04443395 51.04727    15   517     UK    Great Britain
    ## 518   0.97861308 50.97168    15   518     UK    Great Britain
    ## 519   0.96015638 50.92588    15   519     UK    Great Britain
    ## 520   0.77236313 50.93398    15   520     UK    Great Britain
    ## 521   0.68437517 50.88555    15   521     UK    Great Britain
    ## 522   0.53232419 50.85342    15   522     UK    Great Britain
    ## 523   0.41474581 50.81919    15   523     UK    Great Britain
    ## 524   0.29970679 50.77598    15   524     UK    Great Britain
    ## 525   0.20507820 50.76304    15   525     UK    Great Britain
    ## 526   0.00000000 50.78877    15   526     UK    Great Britain
    ## 527  -0.20390651 50.81436    15   527     UK    Great Britain
    ## 528  -0.45078143 50.81016    15   528     UK    Great Britain
    ## 529  -0.78525412 50.76543    15   529     UK    Great Britain
    ## 530  -0.87138689 50.77280    15   530     UK    Great Britain
    ## 531  -1.00058603 50.81562    15   531     UK    Great Britain
    ## 532  -1.13286138 50.84458    15   532     UK    Great Britain
    ## 533  -1.28505838 50.85733    15   533     UK    Great Britain
    ## 534  -1.41645479 50.89688    15   534     UK    Great Britain
    ## 535  -1.33447254 50.82080    15   535     UK    Great Britain
    ## 536  -1.51674819 50.74746    15   536     UK    Great Britain
    ## 537  -1.60083032 50.73286    15   537     UK    Great Britain
    ## 538  -1.68789065 50.73516    15   538     UK    Great Britain
    ## 539  -1.86601579 50.71523    15   539     UK    Great Britain
    ## 540  -2.03105450 50.72539    15   540     UK    Great Britain
    ## 541  -2.00625014 50.67324    15   541     UK    Great Britain
    ## 542  -1.96206069 50.62778    15   542     UK    Great Britain
    ## 543  -1.99790037 50.60801    15   543     UK    Great Britain
    ## 544  -2.03583980 50.60308    15   544     UK    Great Britain
    ## 545  -2.35014629 50.63741    15   545     UK    Great Britain
    ## 546  -2.39467788 50.63091    15   546     UK    Great Britain
    ## 547  -2.43344736 50.59922    15   547     UK    Great Britain
    ## 548  -2.54775357 50.61631    15   548     UK    Great Britain
    ## 549  -2.65883803 50.66973    15   549     UK    Great Britain
    ## 550  -2.77695346 50.70557    15   550     UK    Great Britain
    ## 551  -2.90087914 50.72241    15   551     UK    Great Britain
    ## 552  -2.99941397 50.71660    15   552     UK    Great Britain
    ## 553  -3.40458965 50.63242    15   553     UK    Great Britain
    ## 554  -3.48544884 50.54795    15   554     UK    Great Britain
    ## 555  -3.52587914 50.42817    15   555     UK    Great Britain
    ## 556  -3.58437538 50.32183    15   556     UK    Great Britain
    ## 557  -3.67978525 50.23994    15   557     UK    Great Britain
    ## 558  -3.79335952 50.22925    15   558     UK    Great Britain
    ## 559  -3.90019560 50.28594    15   559     UK    Great Britain
    ## 560  -4.10341740 50.34853    15   560     UK    Great Britain
    ## 561  -4.17255878 50.39082    15   561     UK    Great Britain
    ## 562  -4.19458008 50.39331    15   562     UK    Great Britain
    ## 563  -4.21728516 50.37817    15   563     UK    Great Britain
    ## 564  -4.29697275 50.35908    15   564     UK    Great Britain
    ## 565  -4.37949228 50.35820    15   565     UK    Great Britain
    ## 566  -4.50668955 50.34135    15   566     UK    Great Britain
    ## 567  -4.72797871 50.29048    15   567     UK    Great Britain
    ## 568  -4.81738281 50.25596    15   568     UK    Great Britain
    ## 569  -5.00952101 50.16074    15   569     UK    Great Britain
    ## 570  -5.04863310 50.13437    15   570     UK    Great Britain
    ## 571  -5.11850595 50.03833    15   571     UK    Great Britain
    ## 572  -5.22524357 50.02139    15   572     UK    Great Britain
    ## 573  -5.32285166 50.08296    15   573     UK    Great Britain
    ## 574  -5.43398476 50.10444    15   574     UK    Great Britain
    ## 575  -5.55122089 50.08340    15   575     UK    Great Britain
    ## 576  -5.62211895 50.05068    15   576     UK    Great Britain
    ## 577  -5.65517616 50.07725    15   577     UK    Great Britain
    ## 578  -5.65625048 50.13189    15   578     UK    Great Britain
    ## 579  -5.57065439 50.19697    15   579     UK    Great Britain
    ## 580  -5.34228516 50.24614    15   580     UK    Great Britain
    ## 581  -5.14179659 50.37373    15   581     UK    Great Britain
    ## 582  -5.04345703 50.45152    15   582     UK    Great Britain
    ## 583  -5.00444317 50.49527    15   583     UK    Great Britain
    ## 584  -4.95639658 50.52314    15   584     UK    Great Britain
    ## 585  -4.89355469 50.53369    15   585     UK    Great Britain
    ## 586  -4.86127901 50.58203    15   586     UK    Great Britain
    ## 587  -4.58291006 50.77637    15   587     UK    Great Britain
    ## 588  -4.55996084 50.82095    15   588     UK    Great Britain
    ## 589  -4.54609346 50.90068    15   589     UK    Great Britain
    ## 590  -4.52309561 50.97744    15   590     UK    Great Britain
    ## 591  -4.29648447 51.02715    15   591     UK    Great Britain
    ## 592  -4.18818331 51.18853    15   592     UK    Great Britain
    ## 593  -4.15839815 51.20132    15   593     UK    Great Britain
    ## 594  -3.84233403 51.23091    15   594     UK    Great Britain
    ## 595  -3.60790992 51.22857    15   595     UK    Great Britain
    ## 596  -3.37509775 51.19698    15   596     UK    Great Britain
    ## 597  -3.25576162 51.19414    15   597     UK    Great Britain
    ## 598  -3.13598657 51.20503    15   598     UK    Great Britain
    ## 599  -3.04204106 51.24858    15   599     UK    Great Britain
    ## 600  -2.88124990 51.40566    15   600     UK    Great Britain
    ## 601  -2.79082012 51.47480    15   601     UK    Great Britain
    ## 602  -2.68720698 51.53726    15   602     UK    Great Britain
    ## 603  -2.59028316 51.60860    15   603     UK    Great Britain
    ## 604  -2.43305659 51.74072    15   604     UK    Great Britain
    ## 605  -2.53935528 51.69522    15   605     UK    Great Britain
    ## 606  -2.66767597 51.62300    15   606     UK    Great Britain
    ## 607  -2.74213862 51.58110    15   607     UK    Great Britain
    ## 608  -2.97851539 51.53886    15   608     UK    Great Britain
    ## 609  -3.08037114 51.49580    15   609     UK    Great Britain
    ## 610  -3.25878906 51.39848    15   610     UK    Great Britain
    ## 611  -3.29311514 51.39043    15   611     UK    Great Britain
    ## 612  -3.56235361 51.41382    15   612     UK    Great Britain
    ## 613  -3.76269531 51.53994    15   613     UK    Great Britain
    ## 614  -3.89077115 51.59165    15   614     UK    Great Britain
    ## 615  -3.94365263 51.59751    15   615     UK    Great Britain
    ## 616  -3.99833941 51.58213    15   616     UK    Great Britain
    ## 617  -4.11528349 51.56641    15   617     UK    Great Britain
    ## 618  -4.23457003 51.56909    15   618     UK    Great Britain
    ## 619  -4.17368174 51.62734    15   619     UK    Great Britain
    ## 620  -4.09101582 51.65991    15   620     UK    Great Britain
    ## 621  -4.27617168 51.68252    15   621     UK    Great Britain
    ## 622  -4.32763672 51.70024    15   622     UK    Great Britain
    ## 623  -4.38627958 51.74106    15   623     UK    Great Britain
    ## 624  -4.53149414 51.74805    15   624     UK    Great Britain
    ## 625  -4.60078144 51.73765    15   625     UK    Great Britain
    ## 626  -4.71762657 51.68369    15   626     UK    Great Britain
    ## 627  -4.90229511 51.62627    15   627     UK    Great Britain
    ## 628  -5.12475586 51.70586    15   628     UK    Great Britain
    ## 629  -5.16835928 51.74072    15   629     UK    Great Britain
    ## 630  -5.16723680 51.80806    15   630     UK    Great Britain
    ## 631  -5.20058584 51.86138    15   631     UK    Great Britain
    ## 632  -5.26230431 51.88018    15   632     UK    Great Britain
    ## 633  -5.18335009 51.94966    15   633     UK    Great Britain
    ## 634  -5.08808565 51.99590    15   634     UK    Great Britain
    ## 635  -4.87851572 52.04184    15   635     UK    Great Britain
    ## 636  -4.56113291 52.15088    15   636     UK    Great Britain
    ## 637  -4.38315392 52.19731    15   637     UK    Great Britain
    ## 638  -4.21772480 52.27744    15   638     UK    Great Britain
    ## 639  -4.14936543 52.32627    15   639     UK    Great Britain
    ## 640  -4.09975624 52.39312    15   640     UK    Great Britain
    ## 641  -4.05053711 52.47514    15   641     UK    Great Britain
    ## 642  -3.98032212 52.54175    15   642     UK    Great Britain
    ## 643  -4.04843712 52.55762    15   643     UK    Great Britain
    ## 644  -4.07890606 52.60786    15   644     UK    Great Britain
    ## 645  -4.07070303 52.65884    15   645     UK    Great Britain
    ## 646  -4.03925753 52.70405    15   646     UK    Great Britain
    ## 647  -4.06743145 52.76074    15   647     UK    Great Britain
    ## 648  -4.11752939 52.82002    15   648     UK    Great Britain
    ## 649  -4.11474609 52.86616    15   649     UK    Great Britain
    ## 650  -4.10146523 52.91548    15   650     UK    Great Britain
    ## 651  -4.22915030 52.91285    15   651     UK    Great Britain
    ## 652  -4.35644484 52.89741    15   652     UK    Great Britain
    ## 653  -4.47182608 52.86245    15   653     UK    Great Britain
    ## 654  -4.58369160 52.81494    15   654     UK    Great Britain
    ## 655  -4.68305683 52.80615    15   655     UK    Great Britain
    ## 656  -4.68144512 52.84414    15   656     UK    Great Britain
    ## 657  -4.63832998 52.89111    15   657     UK    Great Britain
    ## 658  -4.52568388 52.95820    15   658     UK    Great Britain
    ## 659  -4.40507841 53.01382    15   659     UK    Great Britain
    ## 660  -4.36220741 53.05606    15   660     UK    Great Britain
    ## 661  -4.32841778 53.10513    15   661     UK    Great Britain
    ## 662  -4.26855469 53.14453    15   662     UK    Great Britain
    ## 663  -4.11103535 53.21894    15   663     UK    Great Britain
    ## 664  -3.80927730 53.30269    15   664     UK    Great Britain
    ## 665  -3.76420903 53.30762    15   665     UK    Great Britain
    ## 666  -3.64589810 53.29790    15   666     UK    Great Britain
    ## 667  -3.52958965 53.31055    15   667     UK    Great Britain
    ## 668  -3.42773438 53.34067    15   668     UK    Great Britain
    ## 669  -3.32617188 53.34717    15   669     UK    Great Britain
    ## 670  -3.09755898 53.26030    15   670     UK    Great Britain
    ## 671  -3.16557622 53.39468    15   671     UK    Great Britain
    ## 672  -3.06474590 53.42686    15   672     UK    Great Britain
    ## 673  -2.91855478 53.30537    15   673     UK    Great Britain
    ## 674  -2.86416054 53.29258    15   674     UK    Great Britain
    ## 675  -2.74951172 53.31020    15   675     UK    Great Britain
    ## 676  -2.79375005 53.33071    15   676     UK    Great Britain
    ## 677  -2.84541059 53.33193    15   677     UK    Great Britain
    ## 678  -2.91308594 53.35025    15   678     UK    Great Britain
    ## 679  -2.96997094 53.38921    15   679     UK    Great Britain
    ## 680  -3.06459928 53.51284    15   680     UK    Great Britain
    ## 681  -3.05947256 53.58623    15   681     UK    Great Britain
    ## 682  -2.99570298 53.66255    15   682     UK    Great Britain
    ## 683  -2.92509794 53.73277    15   683     UK    Great Britain
    ## 684  -2.98432636 53.74673    15   684     UK    Great Britain
    ## 685  -3.03178740 53.77358    15   685     UK    Great Britain
    ## 686  -3.04536128 53.84385    15   686     UK    Great Britain
    ## 687  -3.02675772 53.90591    15   687     UK    Great Britain
    ## 688  -2.89985347 53.96069    15   688     UK    Great Britain
    ## 689  -2.86240244 54.04385    15   689     UK    Great Britain
    ## 690  -2.84648418 54.13530    15   690     UK    Great Britain
    ## 691  -2.86757803 54.17725    15   691     UK    Great Britain
    ## 692  -2.99350572 54.17051    15   692     UK    Great Britain
    ## 693  -3.05473661 54.15342    15   693     UK    Great Britain
    ## 694  -3.10966778 54.12632    15   694     UK    Great Britain
    ## 695  -3.16596675 54.12793    15   695     UK    Great Britain
    ## 696  -3.32153320 54.22910    15   696     UK    Great Britain
    ## 697  -3.41025400 54.30561    15   697     UK    Great Britain
    ## 698  -3.56938481 54.46758    15   698     UK    Great Britain
    ## 699  -3.59204078 54.56436    15   699     UK    Great Britain
    ## 700  -3.46459961 54.77310    15   700     UK    Great Britain
    ## 701  -3.26791978 54.90659    15   701     UK    Great Britain
    ## 702  -3.03623056 54.95308    15   702     UK    Great Britain
    ## 703  -3.08105469 54.96196    15   703     UK    Great Britain
    ## 704  -3.43408203 54.96377    15   704     UK    Great Britain
    ## 705  -3.55043960 54.94741    15   705     UK    Great Britain
    ## 706  -3.65830064 54.89287    15   706     UK    Great Britain
    ## 707  -3.71923828 54.87613    15   707     UK    Great Britain
    ## 708  -3.78325152 54.86992    15   708     UK    Great Britain
    ## 709  -3.84160185 54.84277    15   709     UK    Great Britain
    ## 710  -3.89858413 54.80508    15   710     UK    Great Britain
    ## 711  -3.95790982 54.78096    15   711     UK    Great Britain
    ## 712  -4.07578087 54.78721    15   712     UK    Great Britain
    ## 713  -4.13295889 54.77925    15   713     UK    Great Britain
    ## 714  -4.17402315 54.80107    15   714     UK    Great Britain
    ## 715  -4.20839834 54.83716    15   715     UK    Great Britain
    ## 716  -4.25341797 54.84678    15   716     UK    Great Britain
    ## 717  -4.30366230 54.83569    15   717     UK    Great Britain
    ## 718  -4.40991259 54.78706    15   718     UK    Great Britain
    ## 719  -4.51748085 54.75835    15   719     UK    Great Britain
    ## 720  -4.64755869 54.78901    15   720     UK    Great Britain
    ## 721  -4.81806612 54.84614    15   721     UK    Great Britain
    ## 722  -4.85170937 54.82529    15   722     UK    Great Britain
    ## 723  -4.88950205 54.77227    15   723     UK    Great Britain
    ## 724  -4.91123056 54.68945    15   724     UK    Great Britain
    ## 725  -5.03232479 54.76138    15   725     UK    Great Britain
    ## 726  -5.13549805 54.85752    15   726     UK    Great Britain
    ## 727  -5.17011738 54.91792    15   727     UK    Great Britain
    ## 728  -5.17270470 54.98589    15   728     UK    Great Britain
    ## 729  -5.11669922 55.01226    15   729     UK    Great Britain
    ## 730  -5.05585909 54.98814    15   730     UK    Great Britain
    ## 731  -4.96518612 55.14946    15   731     UK    Great Britain
    ## 732  -4.78481483 55.35942    15   732     UK    Great Britain
    ## 733  -4.72114229 55.42099    15   733     UK    Great Britain
    ## 734  -4.67675781 55.50132    15   734     UK    Great Britain
    ## 735  -4.68437529 55.55391    15   735     UK    Great Britain
    ## 736  -4.72416973 55.59829    15   736     UK    Great Britain
    ## 737  -4.89184570 55.69912    15   737     UK    Great Britain
    ## 738  -4.88964844 55.78120    15   738     UK    Great Britain
    ## 739  -4.87167978 55.87392    15   739     UK    Great Britain
    ## 740  -4.82607460 55.92954    15   740     UK    Great Britain
    ## 741  -4.80683565 55.94014    15   741     UK    Great Britain
    ## 742  -4.58408213 55.93867    15   742     UK    Great Britain
    ## 743  -4.67094755 55.96738    15   743     UK    Great Britain
    ## 744  -4.84409189 56.05117    15   744     UK    Great Britain
    ## 745  -4.84101582 56.08086    15   745     UK    Great Britain
    ## 746  -4.80029345 56.15835    15   746     UK    Great Britain
    ## 747  -4.81914091 56.15049    15   747     UK    Great Britain
    ## 748  -4.85624981 56.11470    15   748     UK    Great Britain
    ## 749  -4.92709970 56.02808    15   749     UK    Great Britain
    ## 750  -4.97036171 56.00786    15   750     UK    Great Britain
    ## 751  -5.09282255 55.98730    15   751     UK    Great Britain
    ## 752  -5.11499023 55.94463    15   752     UK    Great Britain
    ## 753  -5.13466787 55.93349    15   753     UK    Great Britain
    ## 754  -5.19584942 55.92866    15   754     UK    Great Britain
    ## 755  -5.21459913 55.88887    15   755     UK    Great Britain
    ## 756  -5.22822237 55.88633    15   756     UK    Great Britain
    ## 757  -5.24560595 55.92925    15   757     UK    Great Britain
    ## 758  -5.24731398 56.00039    15   758     UK    Great Britain
    ## 759  -5.22294903 56.06582    15   759     UK    Great Britain
    ## 760  -5.17641592 56.11699    15   760     UK    Great Britain
    ## 761  -4.99697304 56.23335    15   761     UK    Great Britain
    ## 762  -5.08432627 56.19746    15   762     UK    Great Britain
    ## 763  -5.28232431 56.08994    15   763     UK    Great Britain
    ## 764  -5.38344717 56.01924    15   764     UK    Great Britain
    ## 765  -5.41044903 55.99536    15   765     UK    Great Britain
    ## 766  -5.41889668 55.97524    15   766     UK    Great Britain
    ## 767  -5.41831064 55.95205    15   767     UK    Great Britain
    ## 768  -5.37290049 55.82769    15   768     UK    Great Britain
    ## 769  -5.38583994 55.77011    15   769     UK    Great Britain
    ## 770  -5.55644560 55.38960    15   770     UK    Great Britain
    ## 771  -5.58876944 55.35142    15   771     UK    Great Britain
    ## 772  -5.61845684 55.33144    15   772     UK    Great Britain
    ## 773  -5.64653349 55.32685    15   773     UK    Great Britain
    ## 774  -5.73066425 55.33413    15   774     UK    Great Britain
    ## 775  -5.76821280 55.36264    15   775     UK    Great Britain
    ## 776  -5.76787090 55.39497    15   776     UK    Great Britain
    ## 777  -5.75209951 55.44345    15   777     UK    Great Britain
    ## 778  -5.68134737 55.62397    15   778     UK    Great Britain
    ## 779  -5.65063477 55.67412    15   779     UK    Great Britain
    ## 780  -5.60502958 55.72075    15   780     UK    Great Britain
    ## 781  -5.50449228 55.80239    15   781     UK    Great Britain
    ## 782  -5.50693369 55.80771    15   782     UK    Great Britain
    ## 783  -5.57387638 55.79170    15   783     UK    Great Britain
    ## 784  -5.60239267 55.79697    15   784     UK    Great Britain
    ## 785  -5.62285137 55.81313    15   785     UK    Great Britain
    ## 786  -5.60957050 56.05527    15   786     UK    Great Britain
    ## 787  -5.55527353 56.13496    15   787     UK    Great Britain
    ## 788  -5.53496075 56.25083    15   788     UK    Great Britain
    ## 789  -5.48789072 56.35005    15   789     UK    Great Britain
    ## 790  -5.43339825 56.42231    15   790     UK    Great Britain
    ## 791  -5.39194298 56.51479    15   791     UK    Great Britain
    ## 792  -5.32944298 56.55591    15   792     UK    Great Britain
    ## 793  -5.31269550 56.61880    15   793     UK    Great Britain
    ## 794  -5.24257851 56.68686    15   794     UK    Great Britain
    ## 795  -5.18837881 56.75806    15   795     UK    Great Britain
    ## 796  -5.21757793 56.75102    15   796     UK    Great Britain
    ## 797  -5.56420946 56.56572    15   797     UK    Great Britain
    ## 798  -5.65244150 56.53198    15   798     UK    Great Britain
    ## 799  -5.77280235 56.54102    15   799     UK    Great Britain
    ## 800  -5.86484337 56.56187    15   800     UK    Great Britain
    ## 801  -5.93676758 56.60571    15   801     UK    Great Britain
    ## 802  -5.96889687 56.68990    15   802     UK    Great Britain
    ## 803  -6.05771494 56.69214    15   803     UK    Great Britain
    ## 804  -6.13369131 56.70669    15   804     UK    Great Britain
    ## 805  -6.13276386 56.71802    15   805     UK    Great Britain
    ## 806  -6.03471661 56.76392    15   806     UK    Great Britain
    ## 807  -5.87763643 56.77964    15   807     UK    Great Britain
    ## 808  -5.73061514 56.85308    15   808     UK    Great Britain
    ## 809  -5.86142588 56.90268    15   809     UK    Great Britain
    ## 810  -5.85039091 56.91841    15   810     UK    Great Britain
    ## 811  -5.73627949 56.96065    15   811     UK    Great Britain
    ## 812  -5.59130859 57.10234    15   812     UK    Great Britain
    ## 813  -5.56191397 57.23271    15   813     UK    Great Britain
    ## 814  -5.63124990 57.29395    15   814     UK    Great Britain
    ## 815  -5.65634775 57.33408    15   815     UK    Great Britain
    ## 816  -5.79492188 57.37881    15   816     UK    Great Britain
    ## 817  -5.81806612 57.43608    15   817     UK    Great Britain
    ## 818  -5.80195332 57.46802    15   818     UK    Great Britain
    ## 819  -5.75673819 57.49922    15   819     UK    Great Britain
    ## 820  -5.68862295 57.52353    15   820     UK    Great Britain
    ## 821  -5.58178663 57.54678    15   821     UK    Great Britain
    ## 822  -5.67876005 57.57168    15   822     UK    Great Britain
    ## 823  -5.71494150 57.60107    15   823     UK    Great Britain
    ## 824  -5.74238300 57.64365    15   824     UK    Great Britain
    ## 825  -5.74492168 57.66831    15   825     UK    Great Britain
    ## 826  -5.69472647 57.77822    15   826     UK    Great Britain
    ## 827  -5.66547823 57.82354    15   827     UK    Great Britain
    ## 828  -5.60834980 57.88134    15   828     UK    Great Britain
    ## 829  -5.34902334 57.87807    15   829     UK    Great Britain
    ## 830  -5.31918955 57.90361    15   830     UK    Great Britain
    ## 831  -5.28979492 57.90459    15   831     UK    Great Britain
    ## 832  -5.15722656 57.88134    15   832     UK    Great Britain
    ## 833  -5.17690468 57.90640    15   833     UK    Great Britain
    ## 834  -5.39375019 58.04360    15   834     UK    Great Britain
    ## 835  -5.41318369 58.06973    15   835     UK    Great Britain
    ## 836  -5.35136747 58.14370    15   836     UK    Great Britain
    ## 837  -5.34687471 58.17666    15   837     UK    Great Britain
    ## 838  -5.35595703 58.21191    15   838     UK    Great Britain
    ## 839  -5.33828115 58.23872    15   839     UK    Great Britain
    ## 840  -5.26953125 58.25142    15   840     UK    Great Britain
    ## 841  -5.05996132 58.25015    15   841     UK    Great Britain
    ## 842  -5.00830078 58.26265    15   842     UK    Great Britain
    ## 843  -5.03183556 58.29829    15   843     UK    Great Britain
    ## 844  -5.08061504 58.34516    15   844     UK    Great Britain
    ## 845  -5.09013700 58.38452    15   845     UK    Great Britain
    ## 846  -5.07871103 58.41928    15   846     UK    Great Britain
    ## 847  -5.07602549 58.48926    15   847     UK    Great Britain
    ## 848  -5.06650352 58.52021    15   848     UK    Great Britain
    ## 849  -5.01674795 58.56655    15   849     UK    Great Britain
    ## 850  -4.97563505 58.58032    15   850     UK    Great Britain
    ## 851  -4.92465830 58.58838    15   851     UK    Great Britain
    ## 852  -4.80961895 58.57290    15   852     UK    Great Britain
    ## 853  -4.76577139 58.55420    15   853     UK    Great Britain
    ## 854  -4.71542931 58.51001    15   854     UK    Great Britain
    ## 855  -4.67822266 58.51358    15   855     UK    Great Britain
    ## 856  -4.53496075 58.56157    15   856     UK    Great Britain
    ## 857  -4.49189425 58.56845    15   857     UK    Great Britain
    ## 858  -4.43325186 58.51284    15   858     UK    Great Britain
    ## 859  -4.18862295 58.55723    15   859     UK    Great Britain
    ## 860  -3.85952187 58.57710    15   860     UK    Great Britain
    ## 861  -3.66181636 58.60630    15   861     UK    Great Britain
    ## 862  -3.45356441 58.61689    15   862     UK    Great Britain
    ## 863  -3.25913072 58.65000    15   863     UK    Great Britain
    ## 864  -3.05307603 58.63482    15   864     UK    Great Britain
    ## 865  -3.04619145 58.61553    15   865     UK    Great Britain
    ## 866  -3.05698252 58.58877    15   866     UK    Great Britain
    ## 867  -3.10966778 58.51548    15   867     UK    Great Britain
    ## 869  -2.92939448 58.74160    16   869     UK         Scotland
    ## 870  -2.93896461 58.73862    16   870     UK         Scotland
    ## 871  -2.97539043 58.75694    16   871     UK         Scotland
    ## 872  -3.03544903 58.82265    16   872     UK         Scotland
    ## 873  -2.94121075 58.83569    16   873     UK         Scotland
    ## 874  -2.89643574 58.82759    16   874     UK         Scotland
    ## 875  -2.91308594 58.79961    16   875     UK         Scotland
    ## 876  -2.92939448 58.74160    16   876     UK         Scotland
    ## 878  -3.16494155 58.79419    17   878     UK         Scotland
    ## 879  -3.22211933 58.78096    17   879     UK         Scotland
    ## 880  -3.27880883 58.78193    17   880     UK         Scotland
    ## 881  -3.36718774 58.83974    17   881     UK         Scotland
    ## 882  -3.40083003 58.88178    17   882     UK         Scotland
    ## 883  -3.39472675 58.90962    17   883     UK         Scotland
    ## 884  -3.35742211 58.91899    17   884     UK         Scotland
    ## 885  -3.27192354 58.90527    17   885     UK         Scotland
    ## 886  -3.22763681 58.85717    17   886     UK         Scotland
    ## 887  -3.22211933 58.82588    17   887     UK         Scotland
    ## 888  -3.21162081 58.81358    17   888     UK         Scotland
    ## 889  -3.15854502 58.80122    17   889     UK         Scotland
    ## 890  -3.16494155 58.79419    17   890     UK         Scotland
    ## 892  -3.05742192 59.02964    18   892     UK         Scotland
    ## 893  -3.07070327 59.00498    18   893     UK         Scotland
    ## 894  -2.99467778 59.00557    18   894     UK         Scotland
    ## 895  -2.88457036 58.98452    18   895     UK         Scotland
    ## 896  -2.81791973 58.98189    18   896     UK         Scotland
    ## 897  -2.76245117 58.95581    18   897     UK         Scotland
    ## 898  -2.79301739 58.90693    18   898     UK         Scotland
    ## 899  -2.82622075 58.89326    18   899     UK         Scotland
    ## 900  -2.86376953 58.89053    18   900     UK         Scotland
    ## 901  -2.99482417 58.93935    18   901     UK         Scotland
    ## 902  -3.16660142 58.91909    18   902     UK         Scotland
    ## 903  -3.20078111 58.92529    18   903     UK         Scotland
    ## 904  -3.22333956 58.93877    18   904     UK         Scotland
    ## 905  -3.23261690 58.95552    18   905     UK         Scotland
    ## 906  -3.23281240 58.98965    18   906     UK         Scotland
    ## 907  -3.24213839 58.99971    18   907     UK         Scotland
    ## 908  -3.30434561 58.96743    18   908     UK         Scotland
    ## 909  -3.33164072 58.97124    18   909     UK         Scotland
    ## 910  -3.34707046 58.98672    18   910     UK         Scotland
    ## 911  -3.35371113 59.01875    18   911     UK         Scotland
    ## 912  -3.34682631 59.06499    18   912     UK         Scotland
    ## 913  -3.31035137 59.13081    18   913     UK         Scotland
    ## 914  -3.24858427 59.14395    18   914     UK         Scotland
    ## 915  -3.15649438 59.13633    18   915     UK         Scotland
    ## 916  -3.05112267 59.09903    18   916     UK         Scotland
    ## 917  -3.01923847 59.07603    18   917     UK         Scotland
    ## 918  -3.02001929 59.05767    18   918     UK         Scotland
    ## 919  -3.05742192 59.02964    18   919     UK         Scotland
    ## 921  -2.54887724 59.23135    19   921     UK         Scotland
    ## 922  -2.66206050 59.23018    19   922     UK         Scotland
    ## 923  -2.60361314 59.28931    19   923     UK         Scotland
    ## 924  -2.53564477 59.30415    19   924     UK         Scotland
    ## 925  -2.40698266 59.29756    19   925     UK         Scotland
    ## 926  -2.42983389 59.27104    19   926     UK         Scotland
    ## 927  -2.54887724 59.23135    19   927     UK         Scotland
    ## 929  -2.72939444 59.18676    20   929     UK         Scotland
    ## 930  -2.81523442 59.16192    20   930     UK         Scotland
    ## 931  -2.85185552 59.18247    20   931     UK         Scotland
    ## 932  -2.86142588 59.24682    20   932     UK         Scotland
    ## 933  -2.96376967 59.27436    20   933     UK         Scotland
    ## 934  -3.01347661 59.29146    20   934     UK         Scotland
    ## 935  -3.05205107 59.32388    20   935     UK         Scotland
    ## 936  -3.04223633 59.33384    20   936     UK         Scotland
    ## 937  -2.97553706 59.34712    20   937     UK         Scotland
    ## 938  -2.86162114 59.28833    20   938     UK         Scotland
    ## 939  -2.81503892 59.24082    20   939     UK         Scotland
    ## 940  -2.73066425 59.22676    20   940     UK         Scotland
    ## 941  -2.71992183 59.21948    20   941     UK         Scotland
    ## 942  -2.72939444 59.18676    20   942     UK         Scotland
    ## 944  -1.30810571 60.53750    21   944     UK         Scotland
    ## 945  -1.28740239 60.46704    21   945     UK         Scotland
    ## 946  -1.23574221 60.48530    21   946     UK         Scotland
    ## 947  -1.15776384 60.41772    21   947     UK         Scotland
    ## 948  -1.11796904 60.41763    21   948     UK         Scotland
    ## 949  -1.05244160 60.44448    21   949     UK         Scotland
    ## 950  -1.06567395 60.38159    21   950     UK         Scotland
    ## 951  -1.13369155 60.20699    21   951     UK         Scotland
    ## 952  -1.15278316 60.17734    21   952     UK         Scotland
    ## 953  -1.16572285 60.12427    21   953     UK         Scotland
    ## 954  -1.17924798 60.11391    21   954     UK         Scotland
    ## 955  -1.19931638 60.00659    21   955     UK         Scotland
    ## 956  -1.24531233 59.97124    21   956     UK         Scotland
    ## 957  -1.28378928 59.88691    21   957     UK         Scotland
    ## 958  -1.29946315 59.87866    21   958     UK         Scotland
    ## 959  -1.35585940 59.91113    21   959     UK         Scotland
    ## 960  -1.29951179 60.03984    21   960     UK         Scotland
    ## 961  -1.27617180 60.11465    21   961     UK         Scotland
    ## 962  -1.29091799 60.15347    21   962     UK         Scotland
    ## 963  -1.32280254 60.18838    21   963     UK         Scotland
    ## 964  -1.40903318 60.18950    21   964     UK         Scotland
    ## 965  -1.48149407 60.17339    21   965     UK         Scotland
    ## 966  -1.49687517 60.19399    21   966     UK         Scotland
    ## 967  -1.49912119 60.22178    21   967     UK         Scotland
    ## 968  -1.51660156 60.23101    21   968     UK         Scotland
    ## 969  -1.61303723 60.22910    21   969     UK         Scotland
    ## 970  -1.64135730 60.23677    21   970     UK         Scotland
    ## 971  -1.66005874 60.26225    21   971     UK         Scotland
    ## 972  -1.66376972 60.28252    21   972     UK         Scotland
    ## 973  -1.57666004 60.29839    21   973     UK         Scotland
    ## 974  -1.49443376 60.29248    21   974     UK         Scotland
    ## 975  -1.37460935 60.33291    21   975     UK         Scotland
    ## 976  -1.44956028 60.46855    21   976     UK         Scotland
    ## 977  -1.54882812 60.48130    21   977     UK         Scotland
    ## 978  -1.57177734 60.49443    21   978     UK         Scotland
    ## 979  -1.55263662 60.51743    21   979     UK         Scotland
    ## 980  -1.49814427 60.52983    21   980     UK         Scotland
    ## 981  -1.41420877 60.59873    21   981     UK         Scotland
    ## 982  -1.36396503 60.60957    21   982     UK         Scotland
    ## 983  -1.30170906 60.60766    21   983     UK         Scotland
    ## 984  -1.30810571 60.53750    21   984     UK         Scotland
    ## 986  -1.04252934 60.51386    22   986     UK         Scotland
    ## 987  -1.06787121 60.50229    22   987     UK         Scotland
    ## 988  -1.16552734 60.60390    22   988     UK         Scotland
    ## 989  -1.09331059 60.72022    22   989     UK         Scotland
    ## 990  -1.00561535 60.71650    22   990     UK         Scotland
    ## 991  -0.99165016 60.68603    22   991     UK         Scotland
    ## 992  -1.00034189 60.65801    22   992     UK         Scotland
    ## 993  -1.04501951 60.65551    22   993     UK         Scotland
    ## 994  -1.04902327 60.64692    22   994     UK         Scotland
    ## 995  -1.03510725 60.59292    22   995     UK         Scotland
    ## 996  -1.03422832 60.53017    22   996     UK         Scotland
    ## 997  -1.04252934 60.51386    22   997     UK         Scotland
    ## 999  -0.77426767 60.81196    23   999     UK         Scotland
    ## 1000 -0.77431637 60.80049    23  1000     UK         Scotland
    ## 1001 -0.82617188 60.71616    23  1001     UK         Scotland
    ## 1002 -0.82548839 60.68394    23  1002     UK         Scotland
    ## 1003 -0.90913105 60.68701    23  1003     UK         Scotland
    ## 1004 -0.92226553 60.69727    23  1004     UK         Scotland
    ## 1005 -0.93808603 60.74566    23  1005     UK         Scotland
    ## 1006 -0.92753905 60.79716    23  1006     UK         Scotland
    ## 1007 -0.91582036 60.81045    23  1007     UK         Scotland
    ## 1008 -0.89140600 60.81591    23  1008     UK         Scotland
    ## 1009 -0.86494166 60.80581    23  1009     UK         Scotland
    ## 1010 -0.82343775 60.83189    23  1010     UK         Scotland
    ## 1011 -0.80180693 60.83125    23  1011     UK         Scotland
    ## 1012 -0.77426767 60.81196    23  1012     UK         Scotland

``` r
country_data <- left_join(uk_shapefile,country_frequencies,by="country")
```

``` r
country_data
```

    ##            long      lat group order region          country frequency
    ## 1   -1.06557596 50.69024     1     1     UK    Isle of Wight        NA
    ## 2   -1.14936531 50.65571     1     2     UK    Isle of Wight        NA
    ## 3   -1.17583025 50.61523     1     3     UK    Isle of Wight        NA
    ## 4   -1.19609356 50.59922     1     4     UK    Isle of Wight        NA
    ## 5   -1.25146472 50.58882     1     5     UK    Isle of Wight        NA
    ## 6   -1.30629909 50.58853     1     6     UK    Isle of Wight        NA
    ## 7   -1.51533186 50.66978     1     7     UK    Isle of Wight        NA
    ## 8   -1.56342769 50.66611     1     8     UK    Isle of Wight        NA
    ## 9   -1.51567400 50.70332     1     9     UK    Isle of Wight        NA
    ## 10  -1.38583994 50.73355     1    10     UK    Isle of Wight        NA
    ## 11  -1.31279302 50.77348     1    11     UK    Isle of Wight        NA
    ## 12  -1.14423859 50.73472     1    12     UK    Isle of Wight        NA
    ## 13  -1.06557596 50.69024     1    13     UK    Isle of Wight        NA
    ## 14  -4.19677734 53.32143     2    15     UK            Wales         1
    ## 15  -4.15488291 53.30283     2    16     UK            Wales         1
    ## 16  -4.04936552 53.30576     2    17     UK            Wales         1
    ## 17  -4.08427763 53.26431     2    18     UK            Wales         1
    ## 18  -4.20039082 53.21807     2    19     UK            Wales         1
    ## 19  -4.27861309 53.17241     2    20     UK            Wales         1
    ## 20  -4.37304688 53.13418     2    21     UK            Wales         1
    ## 21  -4.41884756 53.17803     2    22     UK            Wales         1
    ## 22  -4.47197294 53.17636     2    23     UK            Wales         1
    ## 23  -4.55322266 53.26045     2    24     UK            Wales         1
    ## 24  -4.56787109 53.38647     2    25     UK            Wales         1
    ## 25  -4.46171856 53.41929     2    26     UK            Wales         1
    ## 26  -4.31508780 53.41724     2    27     UK            Wales         1
    ## 27  -4.19677734 53.32143     2    28     UK            Wales         1
    ## 28  -6.21801758 54.08872     3    30     UK Northern Ireland        NA
    ## 29  -6.30366230 54.09487     3    31     UK Northern Ireland        NA
    ## 30  -6.36367178 54.07710     3    32     UK Northern Ireland        NA
    ## 31  -6.40258789 54.06064     3    33     UK Northern Ireland        NA
    ## 32  -6.44028378 54.06363     3    34     UK Northern Ireland        NA
    ## 33  -6.54814482 54.05728     3    35     UK Northern Ireland        NA
    ## 34  -6.64980459 54.05864     3    36     UK Northern Ireland        NA
    ## 35  -6.66420937 54.08477     3    37     UK Northern Ireland        NA
    ## 36  -6.64687443 54.16343     3    38     UK Northern Ireland        NA
    ## 37  -6.66953135 54.18472     3    39     UK Northern Ireland        NA
    ## 38  -6.76660204 54.19561     3    40     UK Northern Ireland        NA
    ## 39  -6.80258799 54.21436     3    41     UK Northern Ireland        NA
    ## 40  -6.85834980 54.26865     3    42     UK Northern Ireland        NA
    ## 41  -6.86923885 54.29404     3    43     UK Northern Ireland        NA
    ## 42  -6.87724638 54.32910     3    44     UK Northern Ireland        NA
    ## 43  -6.93613243 54.37432     3    45     UK Northern Ireland        NA
    ## 44  -7.00771523 54.40669     3    46     UK Northern Ireland        NA
    ## 45  -7.04970741 54.40825     3    47     UK Northern Ireland        NA
    ## 46  -7.13349581 54.35537     3    48     UK Northern Ireland        NA
    ## 47  -7.20258713 54.30181     3    49     UK Northern Ireland        NA
    ## 48  -7.17807627 54.27490     3    50     UK Northern Ireland        NA
    ## 49  -7.15546894 54.23950     3    51     UK Northern Ireland        NA
    ## 50  -7.19306612 54.21411     3    52     UK Northern Ireland        NA
    ## 51  -7.30673838 54.15601     3    53     UK Northern Ireland        NA
    ## 52  -7.32451200 54.13345     3    54     UK Northern Ireland        NA
    ## 53  -7.35517550 54.12124     3    55     UK Northern Ireland        NA
    ## 54  -7.40942335 54.13731     3    56     UK Northern Ireland        NA
    ## 55  -7.54443407 54.13359     3    57     UK Northern Ireland        NA
    ## 56  -7.60654259 54.14385     3    58     UK Northern Ireland        NA
    ## 57  -7.67876005 54.18667     3    59     UK Northern Ireland        NA
    ## 58  -7.85493183 54.21528     3    60     UK Northern Ireland        NA
    ## 59  -7.88447237 54.28379     3    61     UK Northern Ireland        NA
    ## 60  -7.91845703 54.29658     3    62     UK Northern Ireland        NA
    ## 61  -8.11826229 54.41426     3    63     UK Northern Ireland        NA
    ## 62  -8.14482403 54.45351     3    64     UK Northern Ireland        NA
    ## 63  -8.11894512 54.47696     3    65     UK Northern Ireland        NA
    ## 64  -8.04433632 54.51245     3    66     UK Northern Ireland        NA
    ## 65  -7.79379892 54.57124     3    67     UK Northern Ireland        NA
    ## 66  -7.75439501 54.59492     3    68     UK Northern Ireland        NA
    ## 67  -7.74628925 54.61582     3    69     UK Northern Ireland        NA
    ## 68  -7.81982470 54.63970     3    70     UK Northern Ireland        NA
    ## 69  -7.88613319 54.66607     3    71     UK Northern Ireland        NA
    ## 70  -7.90874004 54.68335     3    72     UK Northern Ireland        NA
    ## 71  -7.91059542 54.69834     3    73     UK Northern Ireland        NA
    ## 72  -7.87295008 54.71787     3    74     UK Northern Ireland        NA
    ## 73  -7.79726553 54.71928     3    75     UK Northern Ireland        NA
    ## 74  -7.73749971 54.71045     3    76     UK Northern Ireland        NA
    ## 75  -7.68999052 54.72803     3    77     UK Northern Ireland        NA
    ## 76  -7.60644531 54.74570     3    78     UK Northern Ireland        NA
    ## 77  -7.55039072 54.76797     3    79     UK Northern Ireland        NA
    ## 78  -7.50219679 54.82544     3    80     UK Northern Ireland        NA
    ## 79  -7.45126915 54.87710     3    81     UK Northern Ireland        NA
    ## 80  -7.44599628 54.90513     3    82     UK Northern Ireland        NA
    ## 81  -7.40141582 55.00332     3    83     UK Northern Ireland        NA
    ## 82  -7.37690401 55.02769     3    84     UK Northern Ireland        NA
    ## 83  -7.21865177 55.09199     3    85     UK Northern Ireland        NA
    ## 84  -7.17861319 55.05688     3    86     UK Northern Ireland        NA
    ## 85  -7.10063505 55.04829     3    87     UK Northern Ireland        NA
    ## 86  -7.03076172 55.08062     3    88     UK Northern Ireland        NA
    ## 87  -6.94716787 55.18252     3    89     UK Northern Ireland        NA
    ## 88  -6.88896513 55.18892     3    90     UK Northern Ireland        NA
    ## 89  -6.82485294 55.18066     3    91     UK Northern Ireland        NA
    ## 90  -6.69882822 55.19346     3    92     UK Northern Ireland        NA
    ## 91  -6.47504902 55.24102     3    93     UK Northern Ireland        NA
    ## 92  -6.37529278 55.24180     3    94     UK Northern Ireland        NA
    ## 93  -6.23422813 55.21684     3    95     UK Northern Ireland        NA
    ## 94  -6.12914991 55.21738     3    96     UK Northern Ireland        NA
    ## 95  -6.03579140 55.14453     3    97     UK Northern Ireland        NA
    ## 96  -5.98574209 55.02969     3    98     UK Northern Ireland        NA
    ## 97  -5.86918926 54.91621     3    99     UK Northern Ireland        NA
    ## 98  -5.71684551 54.81748     3   100     UK Northern Ireland        NA
    ## 99  -5.71074200 54.75708     3   101     UK Northern Ireland        NA
    ## 100 -5.76518536 54.72466     3   102     UK Northern Ireland        NA
    ## 101 -5.87910175 54.68438     3   103     UK Northern Ireland        NA
    ## 102 -5.87861347 54.64131     3   104     UK Northern Ireland        NA
    ## 103 -5.80346680 54.66304     3   105     UK Northern Ireland        NA
    ## 104 -5.73862314 54.67305     3   106     UK Northern Ireland        NA
    ## 105 -5.58252001 54.66343     3   107     UK Northern Ireland        NA
    ## 106 -5.52792931 54.61963     3   108     UK Northern Ireland        NA
    ## 107 -5.49018526 54.55405     3   109     UK Northern Ireland        NA
    ## 108 -5.47041035 54.50020     3   110     UK Northern Ireland        NA
    ## 109 -5.48388672 54.44165     3   111     UK Northern Ireland        NA
    ## 110 -5.52587891 54.46021     3   112     UK Northern Ireland        NA
    ## 111 -5.56855440 54.51260     3   113     UK Northern Ireland        NA
    ## 112 -5.61596680 54.53672     3   114     UK Northern Ireland        NA
    ## 113 -5.67109346 54.54976     3   115     UK Northern Ireland        NA
    ## 114 -5.64609385 54.47788     3   116     UK Northern Ireland        NA
    ## 115 -5.65595675 54.38174     3   117     UK Northern Ireland        NA
    ## 116 -5.63188457 54.37266     3   118     UK Northern Ireland        NA
    ## 117 -5.55781221 54.37100     3   119     UK Northern Ireland        NA
    ## 118 -5.60678768 54.27256     3   120     UK Northern Ireland        NA
    ## 119 -5.70805645 54.24585     3   121     UK Northern Ireland        NA
    ## 120 -5.82617188 54.23584     3   122     UK Northern Ireland        NA
    ## 121 -5.85463858 54.20098     3   123     UK Northern Ireland        NA
    ## 122 -5.87607384 54.15606     3   124     UK Northern Ireland        NA
    ## 123 -5.93774414 54.08907     3   125     UK Northern Ireland        NA
    ## 124 -6.01904297 54.05127     3   126     UK Northern Ireland        NA
    ## 125 -6.11953163 54.05889     3   127     UK Northern Ireland        NA
    ## 126 -6.21801758 54.08872     3   128     UK Northern Ireland        NA
    ## 127 -5.10542011 55.44883     4   130     UK         Scotland         1
    ## 128 -5.23149395 55.44809     4   131     UK         Scotland         1
    ## 129 -5.27705050 55.45674     4   132     UK         Scotland         1
    ## 130 -5.33149433 55.48106     4   133     UK         Scotland         1
    ## 131 -5.39267588 55.61836     4   134     UK         Scotland         1
    ## 132 -5.37080097 55.66694     4   135     UK         Scotland         1
    ## 133 -5.34570312 55.69072     4   136     UK         Scotland         1
    ## 134 -5.31811523 55.70918     4   137     UK         Scotland         1
    ## 135 -5.25161123 55.71694     4   138     UK         Scotland         1
    ## 136 -5.18544912 55.69096     4   139     UK         Scotland         1
    ## 137 -5.16040039 55.66680     4   140     UK         Scotland         1
    ## 138 -5.10498095 55.57397     4   141     UK         Scotland         1
    ## 139 -5.09472656 55.49434     4   142     UK         Scotland         1
    ## 140 -5.10542011 55.44883     4   143     UK         Scotland         1
    ## 141 -6.12890577 55.93057     5   145     UK         Scotland         1
    ## 142 -6.09282255 55.80215     5   146     UK         Scotland         1
    ## 143 -6.05761719 55.72251     5   147     UK         Scotland         1
    ## 144 -6.05532217 55.69531     5   148     UK         Scotland         1
    ## 145 -6.08837891 55.65752     5   149     UK         Scotland         1
    ## 146 -6.25317335 55.60723     5   150     UK         Scotland         1
    ## 147 -6.30507803 55.60693     5   151     UK         Scotland         1
    ## 148 -6.30722618 55.61914     5   152     UK         Scotland         1
    ## 149 -6.27001953 55.67031     5   153     UK         Scotland         1
    ## 150 -6.30205107 55.72837     5   154     UK         Scotland         1
    ## 151 -6.28642607 55.77251     5   155     UK         Scotland         1
    ## 152 -6.30175734 55.78061     5   156     UK         Scotland         1
    ## 153 -6.33388662 55.77436     5   157     UK         Scotland         1
    ## 154 -6.45195341 55.70425     5   158     UK         Scotland         1
    ## 155 -6.49135780 55.69732     5   159     UK         Scotland         1
    ## 156 -6.49565411 55.71157     5   160     UK         Scotland         1
    ## 157 -6.46645498 55.76899     5   161     UK         Scotland         1
    ## 158 -6.46284199 55.80825     5   162     UK         Scotland         1
    ## 159 -6.44526339 55.83237     5   163     UK         Scotland         1
    ## 160 -6.41318369 55.85464     5   164     UK         Scotland         1
    ## 161 -6.37495136 55.87134     5   165     UK         Scotland         1
    ## 162 -6.34414053 55.87373     5   166     UK         Scotland         1
    ## 163 -6.31127930 55.85649     5   167     UK         Scotland         1
    ## 164 -6.21567345 55.90459     5   168     UK         Scotland         1
    ## 165 -6.12890577 55.93057     5   169     UK         Scotland         1
    ## 166 -5.97006845 55.81455     6   171     UK         Scotland         1
    ## 167 -5.99091768 55.80381     6   172     UK         Scotland         1
    ## 168 -6.04155302 55.80679     6   173     UK         Scotland         1
    ## 169 -6.06035185 55.82290     6   174     UK         Scotland         1
    ## 170 -6.07070351 55.84766     6   175     UK         Scotland         1
    ## 171 -6.07197237 55.89312     6   176     UK         Scotland         1
    ## 172 -6.04130888 55.92563     6   177     UK         Scotland         1
    ## 173 -5.91176796 55.97475     6   178     UK         Scotland         1
    ## 174 -5.97031212 55.99219     6   179     UK         Scotland         1
    ## 175 -5.97265625 56.00444     6   180     UK         Scotland         1
    ## 176 -5.93906307 56.04527     6   181     UK         Scotland         1
    ## 177 -5.79960918 56.10879     6   182     UK         Scotland         1
    ## 178 -5.76225615 56.12031     6   183     UK         Scotland         1
    ## 179 -5.72514629 56.11856     6   184     UK         Scotland         1
    ## 180 -5.79721689 56.00562     6   185     UK         Scotland         1
    ## 181 -5.97006845 55.81455     6   186     UK         Scotland         1
    ## 182 -5.77788067 56.34434     7   188     UK         Scotland         1
    ## 183 -6.17617178 56.28872     7   189     UK         Scotland         1
    ## 184 -6.31342793 56.29365     7   190     UK         Scotland         1
    ## 185 -6.32582998 56.32095     7   191     UK         Scotland         1
    ## 186 -6.29848623 56.33916     7   192     UK         Scotland         1
    ## 187 -6.18486357 56.35688     7   193     UK         Scotland         1
    ## 188 -6.13886690 56.49062     7   194     UK         Scotland         1
    ## 189 -6.31064463 56.55215     7   195     UK         Scotland         1
    ## 190 -6.31967735 56.56944     7   196     UK         Scotland         1
    ## 191 -6.30625010 56.59878     7   197     UK         Scotland         1
    ## 192 -6.28632832 56.61187     7   198     UK         Scotland         1
    ## 193 -6.18207979 56.64297     7   199     UK         Scotland         1
    ## 194 -6.13828135 56.64985     7   200     UK         Scotland         1
    ## 195 -6.10273409 56.64566     7   201     UK         Scotland         1
    ## 196 -6.02959013 56.60981     7   202     UK         Scotland         1
    ## 197 -5.94668007 56.53452     7   203     UK         Scotland         1
    ## 198 -5.83603525 56.52256     7   204     UK         Scotland         1
    ## 199 -5.76083994 56.49067     7   205     UK         Scotland         1
    ## 200 -5.77788067 56.34434     7   206     UK         Scotland         1
    ## 201 -6.60761786 56.58501     8   208     UK         Scotland         1
    ## 202 -6.66445303 56.57944     8   209     UK         Scotland         1
    ## 203 -6.66855478 56.59361     8   210     UK         Scotland         1
    ## 204 -6.56992197 56.66123     8   211     UK         Scotland         1
    ## 205 -6.50605488 56.67236     8   212     UK         Scotland         1
    ## 206 -6.48369122 56.66577     8   213     UK         Scotland         1
    ## 207 -6.53007793 56.62661     8   214     UK         Scotland         1
    ## 208 -6.60761786 56.58501     8   215     UK         Scotland         1
    ## 209 -7.41689491 56.96543     9   217     UK         Scotland         1
    ## 210 -7.50478458 56.95166     9   218     UK         Scotland         1
    ## 211 -7.53740215 56.95972     9   219     UK         Scotland         1
    ## 212 -7.54296875 56.97236     9   220     UK         Scotland         1
    ## 213 -7.52294874 57.00679     9   221     UK         Scotland         1
    ## 214 -7.45546913 57.01894     9   222     UK         Scotland         1
    ## 215 -7.40668964 57.00029     9   223     UK         Scotland         1
    ## 216 -7.39892626 56.98335     9   224     UK         Scotland         1
    ## 217 -7.41689491 56.96543     9   225     UK         Scotland         1
    ## 218 -6.27905273 56.96469    10   227     UK         Scotland         1
    ## 219 -6.30874062 56.95181    10   228     UK         Scotland         1
    ## 220 -6.34624052 56.95430    10   229     UK         Scotland         1
    ## 221 -6.38339853 56.97090    10   230     UK         Scotland         1
    ## 222 -6.43261719 57.01792    10   231     UK         Scotland         1
    ## 223 -6.32236338 57.05054    10   232     UK         Scotland         1
    ## 224 -6.27822304 57.03139    10   233     UK         Scotland         1
    ## 225 -6.26127911 57.00952    10   234     UK         Scotland         1
    ## 226 -6.26054716 56.98526    10   235     UK         Scotland         1
    ## 227 -6.27905273 56.96469    10   236     UK         Scotland         1
    ## 228 -7.24985313 57.11533    11   238     UK         Scotland         1
    ## 229 -7.29204035 57.10977    11   239     UK         Scotland         1
    ## 230 -7.34741211 57.11514    11   240     UK         Scotland         1
    ## 231 -7.38149452 57.13066    11   241     UK         Scotland         1
    ## 232 -7.41591787 57.19214    11   242     UK         Scotland         1
    ## 233 -7.42236328 57.22935    11   243     UK         Scotland         1
    ## 234 -7.40703106 57.29848    11   244     UK         Scotland         1
    ## 235 -7.41054678 57.38110    11   245     UK         Scotland         1
    ## 236 -7.29638624 57.38369    11   246     UK         Scotland         1
    ## 237 -7.26713848 57.37178    11   247     UK         Scotland         1
    ## 238 -7.24755907 57.12637    11   248     UK         Scotland         1
    ## 239 -7.24985313 57.11533    11   249     UK         Scotland         1
    ## 240 -6.14472675 57.50498    12   251     UK         Scotland         1
    ## 241 -6.14614248 57.46079    12   252     UK         Scotland         1
    ## 242 -6.16376925 57.40884    12   253     UK         Scotland         1
    ## 243 -6.14082050 57.35366    12   254     UK         Scotland         1
    ## 244 -6.13554668 57.31425    12   255     UK         Scotland         1
    ## 245 -6.09340858 57.30171    12   256     UK         Scotland         1
    ## 246 -6.06762695 57.28355    12   257     UK         Scotland         1
    ## 247 -5.88027334 57.26323    12   258     UK         Scotland         1
    ## 248 -5.70600557 57.26894    12   259     UK         Scotland         1
    ## 249 -5.67246103 57.25269    12   260     UK         Scotland         1
    ## 250 -5.66865253 57.22690    12   261     UK         Scotland         1
    ## 251 -5.69619131 57.19844    12   262     UK         Scotland         1
    ## 252 -5.79541016 57.14653    12   263     UK         Scotland         1
    ## 253 -5.91376925 57.06265    12   264     UK         Scotland         1
    ## 254 -5.94907236 57.04517    12   265     UK         Scotland         1
    ## 255 -5.98730469 57.04443    12   266     UK         Scotland         1
    ## 256 -6.01474619 57.05195    12   267     UK         Scotland         1
    ## 257 -6.03437471 57.20122    12   268     UK         Scotland         1
    ## 258 -6.16274452 57.18213    12   269     UK         Scotland         1
    ## 259 -6.26611328 57.18433    12   270     UK         Scotland         1
    ## 260 -6.32270479 57.20249    12   271     UK         Scotland         1
    ## 261 -6.36240244 57.23750    12   272     UK         Scotland         1
    ## 262 -6.44243145 57.32749    12   273     UK         Scotland         1
    ## 263 -6.67543983 57.36289    12   274     UK         Scotland         1
    ## 264 -6.74130821 57.41245    12   275     UK         Scotland         1
    ## 265 -6.76113319 57.44238    12   276     UK         Scotland         1
    ## 266 -6.75273418 57.45893    12   277     UK         Scotland         1
    ## 267 -6.70419884 57.49575    12   278     UK         Scotland         1
    ## 268 -6.64345694 57.48262    12   279     UK         Scotland         1
    ## 269 -6.60585976 57.49067    12   280     UK         Scotland         1
    ## 270 -6.58300781 57.50713    12   281     UK         Scotland         1
    ## 271 -6.58349609 57.52066    12   282     UK         Scotland         1
    ## 272 -6.61528301 57.55273    12   283     UK         Scotland         1
    ## 273 -6.61679697 57.56270    12   284     UK         Scotland         1
    ## 274 -6.37851572 57.60332    12   285     UK         Scotland         1
    ## 275 -6.35766554 57.66679    12   286     UK         Scotland         1
    ## 276 -6.30595684 57.67197    12   287     UK         Scotland         1
    ## 277 -6.24692392 57.65122    12   288     UK         Scotland         1
    ## 278 -6.16606474 57.58530    12   289     UK         Scotland         1
    ## 279 -6.14472675 57.50498    12   290     UK         Scotland         1
    ## 280 -7.20556593 57.68296    13   292     UK         Scotland         1
    ## 281 -7.09277344 57.62666    13   293     UK         Scotland         1
    ## 282 -7.18261766 57.53330    13   294     UK         Scotland         1
    ## 283 -7.32055664 57.53374    13   295     UK         Scotland         1
    ## 284 -7.51474571 57.60196    13   296     UK         Scotland         1
    ## 285 -7.51562500 57.61587    13   297     UK         Scotland         1
    ## 286 -7.49941444 57.63633    13   298     UK         Scotland         1
    ## 287 -7.47031260 57.65254    13   299     UK         Scotland         1
    ## 288 -7.44003868 57.65639    13   300     UK         Scotland         1
    ## 289 -7.39189434 57.64521    13   301     UK         Scotland         1
    ## 290 -7.32485390 57.66314    13   302     UK         Scotland         1
    ## 291 -7.27119160 57.65747    13   303     UK         Scotland         1
    ## 292 -7.20556593 57.68296    13   304     UK         Scotland         1
    ## 293 -6.19868135 58.36329    14   306     UK         Scotland         1
    ## 294 -6.32582998 58.18887    14   307     UK         Scotland         1
    ## 295 -6.37558651 58.18457    14   308     UK         Scotland         1
    ## 296 -6.41928768 58.14097    14   309     UK         Scotland         1
    ## 297 -6.55458975 58.09287    14   310     UK         Scotland         1
    ## 298 -6.43652344 58.09189    14   311     UK         Scotland         1
    ## 299 -6.40336895 58.07587    14   312     UK         Scotland         1
    ## 300 -6.40244150 58.04136    14   313     UK         Scotland         1
    ## 301 -6.42519522 58.02129    14   314     UK         Scotland         1
    ## 302 -6.57812500 57.94136    14   315     UK         Scotland         1
    ## 303 -6.68330050 57.91104    14   316     UK         Scotland         1
    ## 304 -6.79658175 57.82754    14   317     UK         Scotland         1
    ## 305 -6.85375977 57.82651    14   318     UK         Scotland         1
    ## 306 -6.91035175 57.77339    14   319     UK         Scotland         1
    ## 307 -6.95693350 57.75005    14   320     UK         Scotland         1
    ## 308 -6.98310518 57.75000    14   321     UK         Scotland         1
    ## 309 -7.01318312 57.76177    14   322     UK         Scotland         1
    ## 310 -7.08344746 57.81377    14   323     UK         Scotland         1
    ## 311 -6.95595694 57.86489    14   324     UK         Scotland         1
    ## 312 -6.94414043 57.89365    14   325     UK         Scotland         1
    ## 313 -6.85683584 57.92353    14   326     UK         Scotland         1
    ## 314 -6.86416054 57.93286    14   327     UK         Scotland         1
    ## 315 -7.00253868 57.97491    14   328     UK         Scotland         1
    ## 316 -7.05707979 58.00317    14   329     UK         Scotland         1
    ## 317 -7.05190420 58.01797    14   330     UK         Scotland         1
    ## 318 -6.98530293 58.05049    14   331     UK         Scotland         1
    ## 319 -7.01689434 58.05478    14   332     UK         Scotland         1
    ## 320 -7.03823233 58.07232    14   333     UK         Scotland         1
    ## 321 -7.07690430 58.07900    14   334     UK         Scotland         1
    ## 322 -7.08847618 58.09536    14   335     UK         Scotland         1
    ## 323 -7.09560585 58.13828    14   336     UK         Scotland         1
    ## 324 -7.08525419 58.18218    14   337     UK         Scotland         1
    ## 325 -7.04492188 58.20156    14   338     UK         Scotland         1
    ## 326 -7.02841806 58.22232    14   339     UK         Scotland         1
    ## 327 -7.01206064 58.22871    14   340     UK         Scotland         1
    ## 328 -6.94956017 58.21768    14   341     UK         Scotland         1
    ## 329 -6.88622999 58.18257    14   342     UK         Scotland         1
    ## 330 -6.81230497 58.19609    14   343     UK         Scotland         1
    ## 331 -6.72646523 58.18941    14   344     UK         Scotland         1
    ## 332 -6.72470713 58.19756    14   345     UK         Scotland         1
    ## 333 -6.78774405 58.28389    14   346     UK         Scotland         1
    ## 334 -6.77646446 58.30151    14   347     UK         Scotland         1
    ## 335 -6.74228525 58.32163    14   348     UK         Scotland         1
    ## 336 -6.54418898 58.38315    14   349     UK         Scotland         1
    ## 337 -6.29716778 58.48662    14   350     UK         Scotland         1
    ## 338 -6.23745108 58.50283    14   351     UK         Scotland         1
    ## 339 -6.21943331 58.48872    14   352     UK         Scotland         1
    ## 340 -6.19423771 58.43511    14   353     UK         Scotland         1
    ## 341 -6.19868135 58.36329    14   354     UK         Scotland         1
    ## 342 -3.10966778 58.51548    15   356     UK    Great Britain        13
    ## 343 -3.10112309 58.43369    15   357     UK    Great Britain        13
    ## 344 -3.11289048 58.40889    15   358     UK    Great Britain        13
    ## 345 -3.13676739 58.37832    15   359     UK    Great Britain        13
    ## 346 -3.21235394 58.32124    15   360     UK    Great Britain        13
    ## 347 -3.41098619 58.23965    15   361     UK    Great Britain        13
    ## 348 -3.77499986 58.05210    15   362     UK    Great Britain        13
    ## 349 -3.99003911 57.95903    15   363     UK    Great Britain        13
    ## 350 -4.01962900 57.91426    15   364     UK    Great Britain        13
    ## 351 -4.03559542 57.85200    15   365     UK    Great Britain        13
    ## 352 -3.90683579 57.83965    15   366     UK    Great Britain        13
    ## 353 -3.85712910 57.81855    15   367     UK    Great Britain        13
    ## 354 -3.88793921 57.78691    15   368     UK    Great Britain        13
    ## 355 -4.07841778 57.67705    15   369     UK    Great Britain        13
    ## 356 -4.13452148 57.57774    15   370     UK    Great Britain        13
    ## 357 -3.98847651 57.58125    15   371     UK    Great Britain        13
    ## 358 -3.86816406 57.60035    15   372     UK    Great Britain        13
    ## 359 -3.62822247 57.66225    15   373     UK    Great Britain        13
    ## 360 -3.40278316 57.70826    15   374     UK    Great Britain        13
    ## 361 -3.29453135 57.71015    15   375     UK    Great Britain        13
    ## 362 -3.08393574 57.67349    15   376     UK    Great Britain        13
    ## 363 -3.03603506 57.67231    15   377     UK    Great Britain        13
    ## 364 -2.94667959 57.68926    15   378     UK    Great Britain        13
    ## 365 -2.85629892 57.69229    15   379     UK    Great Britain        13
    ## 366 -2.24414039 57.68086    15   380     UK    Great Britain        13
    ## 367 -2.07407236 57.70239    15   381     UK    Great Britain        13
    ## 368 -1.96152341 57.67666    15   382     UK    Great Britain        13
    ## 369 -1.86738288 57.61235    15   383     UK    Great Britain        13
    ## 370 -1.77792978 57.49375    15   384     UK    Great Britain        13
    ## 371 -1.78066409 57.47402    15   385     UK    Great Britain        13
    ## 372 -1.83471692 57.41997    15   386     UK    Great Britain        13
    ## 373 -1.93447280 57.35220    15   387     UK    Great Britain        13
    ## 374 -2.02031279 57.25888    15   388     UK    Great Britain        13
    ## 375 -2.04550815 57.20855    15   389     UK    Great Britain        13
    ## 376 -2.06235361 57.15347    15   390     UK    Great Britain        13
    ## 377 -2.08955073 57.10254    15   391     UK    Great Britain        13
    ## 378 -2.26025367 56.86333    15   392     UK    Great Britain        13
    ## 379 -2.42666006 56.73071    15   393     UK    Great Britain        13
    ## 380 -2.50097680 56.63657    15   394     UK    Great Britain        13
    ## 381 -2.59267569 56.56157    15   395     UK    Great Britain        13
    ## 382 -2.68095732 56.51440    15   396     UK    Great Britain        13
    ## 383 -2.77519560 56.48296    15   397     UK    Great Britain        13
    ## 384 -3.04741192 56.44936    15   398     UK    Great Britain        13
    ## 385 -3.12358356 56.42529    15   399     UK    Great Britain        13
    ## 386 -3.21445298 56.38393    15   400     UK    Great Britain        13
    ## 387 -3.30996108 56.36348    15   401     UK    Great Britain        13
    ## 388 -3.19799829 56.36607    15   402     UK    Great Britain        13
    ## 389 -3.08701181 56.38906    15   403     UK    Great Britain        13
    ## 390 -2.88515639 56.39751    15   404     UK    Great Britain        13
    ## 391 -2.65273452 56.31826    15   405     UK    Great Britain        13
    ## 392 -2.67426729 56.25342    15   406     UK    Great Britain        13
    ## 393 -2.76757812 56.20215    15   407     UK    Great Britain        13
    ## 394 -2.97978497 56.19409    15   408     UK    Great Britain        13
    ## 395 -3.17822266 56.08012    15   409     UK    Great Britain        13
    ## 396 -3.26777363 56.04507    15   410     UK    Great Britain        13
    ## 397 -3.36225629 56.02763    15   411     UK    Great Britain        13
    ## 398 -3.48042011 56.03281    15   412     UK    Great Britain        13
    ## 399 -3.69511700 56.06333    15   413     UK    Great Britain        13
    ## 400 -3.78906274 56.09521    15   414     UK    Great Britain        13
    ## 401 -3.70415044 56.04316    15   415     UK    Great Britain        13
    ## 402 -3.60781264 56.01602    15   416     UK    Great Britain        13
    ## 403 -3.04873037 55.95195    15   417     UK    Great Britain        13
    ## 404 -3.01508784 55.95859    15   418     UK    Great Britain        13
    ## 405 -2.83686519 56.02627    15   419     UK    Great Britain        13
    ## 406 -2.59931636 56.02729    15   420     UK    Great Britain        13
    ## 407 -2.14707065 55.90298    15   421     UK    Great Britain        13
    ## 408 -2.01684570 55.80796    15   422     UK    Great Britain        13
    ## 409 -1.83027363 55.67173    15   423     UK    Great Britain        13
    ## 410 -1.72875977 55.61855    15   424     UK    Great Britain        13
    ## 411 -1.65537143 55.57036    15   425     UK    Great Britain        13
    ## 412 -1.61015642 55.49809    15   426     UK    Great Britain        13
    ## 413 -1.52255857 55.25952    15   427     UK    Great Britain        13
    ## 414 -1.42265642 55.02642    15   428     UK    Great Britain        13
    ## 415 -1.29174817 54.77388    15   429     UK    Great Britain        13
    ## 416 -1.23242199 54.70371    15   430     UK    Great Britain        13
    ## 417 -1.15439487 54.65449    15   431     UK    Great Britain        13
    ## 418 -0.75932604 54.54141    15   432     UK    Great Britain        13
    ## 419 -0.67138678 54.50391    15   433     UK    Great Britain        13
    ## 420 -0.51811540 54.39512    15   434     UK    Great Britain        13
    ## 421 -0.37036106 54.27920    15   435     UK    Great Britain        13
    ## 422 -0.23286150 54.19014    15   436     UK    Great Britain        13
    ## 423 -0.08437490 54.11806    15   437     UK    Great Britain        13
    ## 424 -0.15629888 54.08062    15   438     UK    Great Britain        13
    ## 425 -0.20556638 54.02173    15   439     UK    Great Britain        13
    ## 426 -0.16874981 53.94165    15   440     UK    Great Britain        13
    ## 427 -0.10825178 53.86519    15   441     UK    Great Britain        13
    ## 428  0.00000000 53.75368    15   442     UK    Great Britain        13
    ## 429  0.01054701 53.74282    15   443     UK    Great Britain        13
    ## 430  0.11533183 53.60928    15   444     UK    Great Britain        13
    ## 431  0.07670873 53.62944    15   445     UK    Great Britain        13
    ## 432  0.03608374 53.64053    15   446     UK    Great Britain        13
    ## 433  0.00000000 53.63837    15   447     UK    Great Britain        13
    ## 434 -0.01943358 53.63721    15   448     UK    Great Britain        13
    ## 435 -0.07373050 53.64365    15   449     UK    Great Britain        13
    ## 436 -0.17382795 53.68545    15   450     UK    Great Britain        13
    ## 437 -0.27001956 53.73677    15   451     UK    Great Britain        13
    ## 438 -0.46137714 53.71616    15   452     UK    Great Britain        13
    ## 439 -0.56767571 53.72539    15   453     UK    Great Britain        13
    ## 440 -0.65991217 53.72403    15   454     UK    Great Britain        13
    ## 441 -0.48505864 53.69439    15   455     UK    Great Britain        13
    ## 442 -0.29370105 53.69233    15   456     UK    Great Britain        13
    ## 443  0.00000000 53.53639    15   457     UK    Great Britain        13
    ## 444  0.12832020 53.46826    15   458     UK    Great Britain        13
    ## 445  0.27099586 53.33550    15   459     UK    Great Britain        13
    ## 446  0.35576153 53.15996    15   460     UK    Great Britain        13
    ## 447  0.29804692 53.08110    15   461     UK    Great Britain        13
    ## 448  0.20820312 53.03003    15   462     UK    Great Britain        13
    ## 449  0.12441435 52.97158    15   463     UK    Great Britain        13
    ## 450  0.04589850 52.90561    15   464     UK    Great Britain        13
    ## 451  0.27978504 52.80869    15   465     UK    Great Britain        13
    ## 452  0.33017552 52.81162    15   466     UK    Great Britain        13
    ## 453  0.38193366 52.82520    15   467     UK    Great Britain        13
    ## 454  0.43164060 52.85815    15   468     UK    Great Britain        13
    ## 455  0.51552737 52.93838    15   469     UK    Great Britain        13
    ## 456  0.55878913 52.96694    15   470     UK    Great Britain        13
    ## 457  0.70449227 52.97725    15   471     UK    Great Britain        13
    ## 458  0.82675809 52.97110    15   472     UK    Great Britain        13
    ## 459  0.94853503 52.95337    15   473     UK    Great Britain        13
    ## 460  1.05556643 52.95898    15   474     UK    Great Britain        13
    ## 461  1.27128899 52.92456    15   475     UK    Great Britain        13
    ## 462  1.38212895 52.89351    15   476     UK    Great Britain        13
    ## 463  1.65673840 52.75371    15   477     UK    Great Britain        13
    ## 464  1.71611345 52.67725    15   478     UK    Great Britain        13
    ## 465  1.74335933 52.57852    15   479     UK    Great Britain        13
    ## 466  1.74658227 52.46899    15   480     UK    Great Britain        13
    ## 467  1.70039034 52.36890    15   481     UK    Great Britain        13
    ## 468  1.64736307 52.27852    15   482     UK    Great Britain        13
    ## 469  1.61464834 52.16182    15   483     UK    Great Britain        13
    ## 470  1.59140635 52.11978    15   484     UK    Great Britain        13
    ## 471  1.55898440 52.08686    15   485     UK    Great Britain        13
    ## 472  1.41347659 51.99477    15   486     UK    Great Britain        13
    ## 473  1.31679690 51.95694    15   487     UK    Great Britain        13
    ## 474  1.27597642 51.97354    15   488     UK    Great Britain        13
    ## 475  1.23242199 51.97124    15   489     UK    Great Britain        13
    ## 476  1.22783196 51.94912    15   490     UK    Great Britain        13
    ## 477  1.27382779 51.90210    15   491     UK    Great Britain        13
    ## 478  1.27441394 51.84536    15   492     UK    Great Britain        13
    ## 479  1.18847656 51.80337    15   493     UK    Great Britain        13
    ## 480  1.10117161 51.78545    15   494     UK    Great Britain        13
    ## 481  0.95507824 51.80781    15   495     UK    Great Britain        13
    ## 482  0.75224602 51.72959    15   496     UK    Great Britain        13
    ## 483  0.89804661 51.68940    15   497     UK    Great Britain        13
    ## 484  0.92744160 51.64663    15   498     UK    Great Britain        13
    ## 485  0.89091790 51.57143    15   499     UK    Great Britain        13
    ## 486  0.79921883 51.53789    15   500     UK    Great Britain        13
    ## 487  0.69755834 51.52305    15   501     UK    Great Britain        13
    ## 488  0.59345710 51.51948    15   502     UK    Great Britain        13
    ## 489  0.50722635 51.50108    15   503     UK    Great Britain        13
    ## 490  0.42451188 51.46563    15   504     UK    Great Britain        13
    ## 491  0.52832037 51.48447    15   505     UK    Great Britain        13
    ## 492  0.60029304 51.46797    15   506     UK    Great Britain        13
    ## 493  0.64550799 51.40469    15   507     UK    Great Britain        13
    ## 494  0.68652320 51.38657    15   508     UK    Great Britain        13
    ## 495  0.88935542 51.35952    15   509     UK    Great Britain        13
    ## 496  1.01494145 51.35972    15   510     UK    Great Britain        13
    ## 497  1.25712895 51.37510    15   511     UK    Great Britain        13
    ## 498  1.37343776 51.37471    15   512     UK    Great Britain        13
    ## 499  1.41494167 51.36328    15   513     UK    Great Britain        13
    ## 500  1.41562510 51.31084    15   514     UK    Great Britain        13
    ## 501  1.39755857 51.18203    15   515     UK    Great Britain        13
    ## 502  1.36552751 51.15547    15   516     UK    Great Britain        13
    ## 503  1.04443395 51.04727    15   517     UK    Great Britain        13
    ## 504  0.97861308 50.97168    15   518     UK    Great Britain        13
    ## 505  0.96015638 50.92588    15   519     UK    Great Britain        13
    ## 506  0.77236313 50.93398    15   520     UK    Great Britain        13
    ## 507  0.68437517 50.88555    15   521     UK    Great Britain        13
    ## 508  0.53232419 50.85342    15   522     UK    Great Britain        13
    ## 509  0.41474581 50.81919    15   523     UK    Great Britain        13
    ## 510  0.29970679 50.77598    15   524     UK    Great Britain        13
    ## 511  0.20507820 50.76304    15   525     UK    Great Britain        13
    ## 512  0.00000000 50.78877    15   526     UK    Great Britain        13
    ## 513 -0.20390651 50.81436    15   527     UK    Great Britain        13
    ## 514 -0.45078143 50.81016    15   528     UK    Great Britain        13
    ## 515 -0.78525412 50.76543    15   529     UK    Great Britain        13
    ## 516 -0.87138689 50.77280    15   530     UK    Great Britain        13
    ## 517 -1.00058603 50.81562    15   531     UK    Great Britain        13
    ## 518 -1.13286138 50.84458    15   532     UK    Great Britain        13
    ## 519 -1.28505838 50.85733    15   533     UK    Great Britain        13
    ## 520 -1.41645479 50.89688    15   534     UK    Great Britain        13
    ## 521 -1.33447254 50.82080    15   535     UK    Great Britain        13
    ## 522 -1.51674819 50.74746    15   536     UK    Great Britain        13
    ## 523 -1.60083032 50.73286    15   537     UK    Great Britain        13
    ## 524 -1.68789065 50.73516    15   538     UK    Great Britain        13
    ## 525 -1.86601579 50.71523    15   539     UK    Great Britain        13
    ## 526 -2.03105450 50.72539    15   540     UK    Great Britain        13
    ## 527 -2.00625014 50.67324    15   541     UK    Great Britain        13
    ## 528 -1.96206069 50.62778    15   542     UK    Great Britain        13
    ## 529 -1.99790037 50.60801    15   543     UK    Great Britain        13
    ## 530 -2.03583980 50.60308    15   544     UK    Great Britain        13
    ## 531 -2.35014629 50.63741    15   545     UK    Great Britain        13
    ## 532 -2.39467788 50.63091    15   546     UK    Great Britain        13
    ## 533 -2.43344736 50.59922    15   547     UK    Great Britain        13
    ## 534 -2.54775357 50.61631    15   548     UK    Great Britain        13
    ## 535 -2.65883803 50.66973    15   549     UK    Great Britain        13
    ## 536 -2.77695346 50.70557    15   550     UK    Great Britain        13
    ## 537 -2.90087914 50.72241    15   551     UK    Great Britain        13
    ## 538 -2.99941397 50.71660    15   552     UK    Great Britain        13
    ## 539 -3.40458965 50.63242    15   553     UK    Great Britain        13
    ## 540 -3.48544884 50.54795    15   554     UK    Great Britain        13
    ## 541 -3.52587914 50.42817    15   555     UK    Great Britain        13
    ## 542 -3.58437538 50.32183    15   556     UK    Great Britain        13
    ## 543 -3.67978525 50.23994    15   557     UK    Great Britain        13
    ## 544 -3.79335952 50.22925    15   558     UK    Great Britain        13
    ## 545 -3.90019560 50.28594    15   559     UK    Great Britain        13
    ## 546 -4.10341740 50.34853    15   560     UK    Great Britain        13
    ## 547 -4.17255878 50.39082    15   561     UK    Great Britain        13
    ## 548 -4.19458008 50.39331    15   562     UK    Great Britain        13
    ## 549 -4.21728516 50.37817    15   563     UK    Great Britain        13
    ## 550 -4.29697275 50.35908    15   564     UK    Great Britain        13
    ## 551 -4.37949228 50.35820    15   565     UK    Great Britain        13
    ## 552 -4.50668955 50.34135    15   566     UK    Great Britain        13
    ## 553 -4.72797871 50.29048    15   567     UK    Great Britain        13
    ## 554 -4.81738281 50.25596    15   568     UK    Great Britain        13
    ## 555 -5.00952101 50.16074    15   569     UK    Great Britain        13
    ## 556 -5.04863310 50.13437    15   570     UK    Great Britain        13
    ## 557 -5.11850595 50.03833    15   571     UK    Great Britain        13
    ## 558 -5.22524357 50.02139    15   572     UK    Great Britain        13
    ## 559 -5.32285166 50.08296    15   573     UK    Great Britain        13
    ## 560 -5.43398476 50.10444    15   574     UK    Great Britain        13
    ## 561 -5.55122089 50.08340    15   575     UK    Great Britain        13
    ## 562 -5.62211895 50.05068    15   576     UK    Great Britain        13
    ## 563 -5.65517616 50.07725    15   577     UK    Great Britain        13
    ## 564 -5.65625048 50.13189    15   578     UK    Great Britain        13
    ## 565 -5.57065439 50.19697    15   579     UK    Great Britain        13
    ## 566 -5.34228516 50.24614    15   580     UK    Great Britain        13
    ## 567 -5.14179659 50.37373    15   581     UK    Great Britain        13
    ## 568 -5.04345703 50.45152    15   582     UK    Great Britain        13
    ## 569 -5.00444317 50.49527    15   583     UK    Great Britain        13
    ## 570 -4.95639658 50.52314    15   584     UK    Great Britain        13
    ## 571 -4.89355469 50.53369    15   585     UK    Great Britain        13
    ## 572 -4.86127901 50.58203    15   586     UK    Great Britain        13
    ## 573 -4.58291006 50.77637    15   587     UK    Great Britain        13
    ## 574 -4.55996084 50.82095    15   588     UK    Great Britain        13
    ## 575 -4.54609346 50.90068    15   589     UK    Great Britain        13
    ## 576 -4.52309561 50.97744    15   590     UK    Great Britain        13
    ## 577 -4.29648447 51.02715    15   591     UK    Great Britain        13
    ## 578 -4.18818331 51.18853    15   592     UK    Great Britain        13
    ## 579 -4.15839815 51.20132    15   593     UK    Great Britain        13
    ## 580 -3.84233403 51.23091    15   594     UK    Great Britain        13
    ## 581 -3.60790992 51.22857    15   595     UK    Great Britain        13
    ## 582 -3.37509775 51.19698    15   596     UK    Great Britain        13
    ## 583 -3.25576162 51.19414    15   597     UK    Great Britain        13
    ## 584 -3.13598657 51.20503    15   598     UK    Great Britain        13
    ## 585 -3.04204106 51.24858    15   599     UK    Great Britain        13
    ## 586 -2.88124990 51.40566    15   600     UK    Great Britain        13
    ## 587 -2.79082012 51.47480    15   601     UK    Great Britain        13
    ## 588 -2.68720698 51.53726    15   602     UK    Great Britain        13
    ## 589 -2.59028316 51.60860    15   603     UK    Great Britain        13
    ## 590 -2.43305659 51.74072    15   604     UK    Great Britain        13
    ## 591 -2.53935528 51.69522    15   605     UK    Great Britain        13
    ## 592 -2.66767597 51.62300    15   606     UK    Great Britain        13
    ## 593 -2.74213862 51.58110    15   607     UK    Great Britain        13
    ## 594 -2.97851539 51.53886    15   608     UK    Great Britain        13
    ## 595 -3.08037114 51.49580    15   609     UK    Great Britain        13
    ## 596 -3.25878906 51.39848    15   610     UK    Great Britain        13
    ## 597 -3.29311514 51.39043    15   611     UK    Great Britain        13
    ## 598 -3.56235361 51.41382    15   612     UK    Great Britain        13
    ## 599 -3.76269531 51.53994    15   613     UK    Great Britain        13
    ## 600 -3.89077115 51.59165    15   614     UK    Great Britain        13
    ## 601 -3.94365263 51.59751    15   615     UK    Great Britain        13
    ## 602 -3.99833941 51.58213    15   616     UK    Great Britain        13
    ## 603 -4.11528349 51.56641    15   617     UK    Great Britain        13
    ## 604 -4.23457003 51.56909    15   618     UK    Great Britain        13
    ## 605 -4.17368174 51.62734    15   619     UK    Great Britain        13
    ## 606 -4.09101582 51.65991    15   620     UK    Great Britain        13
    ## 607 -4.27617168 51.68252    15   621     UK    Great Britain        13
    ## 608 -4.32763672 51.70024    15   622     UK    Great Britain        13
    ## 609 -4.38627958 51.74106    15   623     UK    Great Britain        13
    ## 610 -4.53149414 51.74805    15   624     UK    Great Britain        13
    ## 611 -4.60078144 51.73765    15   625     UK    Great Britain        13
    ## 612 -4.71762657 51.68369    15   626     UK    Great Britain        13
    ## 613 -4.90229511 51.62627    15   627     UK    Great Britain        13
    ## 614 -5.12475586 51.70586    15   628     UK    Great Britain        13
    ## 615 -5.16835928 51.74072    15   629     UK    Great Britain        13
    ## 616 -5.16723680 51.80806    15   630     UK    Great Britain        13
    ## 617 -5.20058584 51.86138    15   631     UK    Great Britain        13
    ## 618 -5.26230431 51.88018    15   632     UK    Great Britain        13
    ## 619 -5.18335009 51.94966    15   633     UK    Great Britain        13
    ## 620 -5.08808565 51.99590    15   634     UK    Great Britain        13
    ## 621 -4.87851572 52.04184    15   635     UK    Great Britain        13
    ## 622 -4.56113291 52.15088    15   636     UK    Great Britain        13
    ## 623 -4.38315392 52.19731    15   637     UK    Great Britain        13
    ## 624 -4.21772480 52.27744    15   638     UK    Great Britain        13
    ## 625 -4.14936543 52.32627    15   639     UK    Great Britain        13
    ## 626 -4.09975624 52.39312    15   640     UK    Great Britain        13
    ## 627 -4.05053711 52.47514    15   641     UK    Great Britain        13
    ## 628 -3.98032212 52.54175    15   642     UK    Great Britain        13
    ## 629 -4.04843712 52.55762    15   643     UK    Great Britain        13
    ## 630 -4.07890606 52.60786    15   644     UK    Great Britain        13
    ## 631 -4.07070303 52.65884    15   645     UK    Great Britain        13
    ## 632 -4.03925753 52.70405    15   646     UK    Great Britain        13
    ## 633 -4.06743145 52.76074    15   647     UK    Great Britain        13
    ## 634 -4.11752939 52.82002    15   648     UK    Great Britain        13
    ## 635 -4.11474609 52.86616    15   649     UK    Great Britain        13
    ## 636 -4.10146523 52.91548    15   650     UK    Great Britain        13
    ## 637 -4.22915030 52.91285    15   651     UK    Great Britain        13
    ## 638 -4.35644484 52.89741    15   652     UK    Great Britain        13
    ## 639 -4.47182608 52.86245    15   653     UK    Great Britain        13
    ## 640 -4.58369160 52.81494    15   654     UK    Great Britain        13
    ## 641 -4.68305683 52.80615    15   655     UK    Great Britain        13
    ## 642 -4.68144512 52.84414    15   656     UK    Great Britain        13
    ## 643 -4.63832998 52.89111    15   657     UK    Great Britain        13
    ## 644 -4.52568388 52.95820    15   658     UK    Great Britain        13
    ## 645 -4.40507841 53.01382    15   659     UK    Great Britain        13
    ## 646 -4.36220741 53.05606    15   660     UK    Great Britain        13
    ## 647 -4.32841778 53.10513    15   661     UK    Great Britain        13
    ## 648 -4.26855469 53.14453    15   662     UK    Great Britain        13
    ## 649 -4.11103535 53.21894    15   663     UK    Great Britain        13
    ## 650 -3.80927730 53.30269    15   664     UK    Great Britain        13
    ## 651 -3.76420903 53.30762    15   665     UK    Great Britain        13
    ## 652 -3.64589810 53.29790    15   666     UK    Great Britain        13
    ## 653 -3.52958965 53.31055    15   667     UK    Great Britain        13
    ## 654 -3.42773438 53.34067    15   668     UK    Great Britain        13
    ## 655 -3.32617188 53.34717    15   669     UK    Great Britain        13
    ## 656 -3.09755898 53.26030    15   670     UK    Great Britain        13
    ## 657 -3.16557622 53.39468    15   671     UK    Great Britain        13
    ## 658 -3.06474590 53.42686    15   672     UK    Great Britain        13
    ## 659 -2.91855478 53.30537    15   673     UK    Great Britain        13
    ## 660 -2.86416054 53.29258    15   674     UK    Great Britain        13
    ## 661 -2.74951172 53.31020    15   675     UK    Great Britain        13
    ## 662 -2.79375005 53.33071    15   676     UK    Great Britain        13
    ## 663 -2.84541059 53.33193    15   677     UK    Great Britain        13
    ## 664 -2.91308594 53.35025    15   678     UK    Great Britain        13
    ## 665 -2.96997094 53.38921    15   679     UK    Great Britain        13
    ## 666 -3.06459928 53.51284    15   680     UK    Great Britain        13
    ## 667 -3.05947256 53.58623    15   681     UK    Great Britain        13
    ## 668 -2.99570298 53.66255    15   682     UK    Great Britain        13
    ## 669 -2.92509794 53.73277    15   683     UK    Great Britain        13
    ## 670 -2.98432636 53.74673    15   684     UK    Great Britain        13
    ## 671 -3.03178740 53.77358    15   685     UK    Great Britain        13
    ## 672 -3.04536128 53.84385    15   686     UK    Great Britain        13
    ## 673 -3.02675772 53.90591    15   687     UK    Great Britain        13
    ## 674 -2.89985347 53.96069    15   688     UK    Great Britain        13
    ## 675 -2.86240244 54.04385    15   689     UK    Great Britain        13
    ## 676 -2.84648418 54.13530    15   690     UK    Great Britain        13
    ## 677 -2.86757803 54.17725    15   691     UK    Great Britain        13
    ## 678 -2.99350572 54.17051    15   692     UK    Great Britain        13
    ## 679 -3.05473661 54.15342    15   693     UK    Great Britain        13
    ## 680 -3.10966778 54.12632    15   694     UK    Great Britain        13
    ## 681 -3.16596675 54.12793    15   695     UK    Great Britain        13
    ## 682 -3.32153320 54.22910    15   696     UK    Great Britain        13
    ## 683 -3.41025400 54.30561    15   697     UK    Great Britain        13
    ## 684 -3.56938481 54.46758    15   698     UK    Great Britain        13
    ## 685 -3.59204078 54.56436    15   699     UK    Great Britain        13
    ## 686 -3.46459961 54.77310    15   700     UK    Great Britain        13
    ## 687 -3.26791978 54.90659    15   701     UK    Great Britain        13
    ## 688 -3.03623056 54.95308    15   702     UK    Great Britain        13
    ## 689 -3.08105469 54.96196    15   703     UK    Great Britain        13
    ## 690 -3.43408203 54.96377    15   704     UK    Great Britain        13
    ## 691 -3.55043960 54.94741    15   705     UK    Great Britain        13
    ## 692 -3.65830064 54.89287    15   706     UK    Great Britain        13
    ## 693 -3.71923828 54.87613    15   707     UK    Great Britain        13
    ## 694 -3.78325152 54.86992    15   708     UK    Great Britain        13
    ## 695 -3.84160185 54.84277    15   709     UK    Great Britain        13
    ## 696 -3.89858413 54.80508    15   710     UK    Great Britain        13
    ## 697 -3.95790982 54.78096    15   711     UK    Great Britain        13
    ## 698 -4.07578087 54.78721    15   712     UK    Great Britain        13
    ## 699 -4.13295889 54.77925    15   713     UK    Great Britain        13
    ## 700 -4.17402315 54.80107    15   714     UK    Great Britain        13
    ## 701 -4.20839834 54.83716    15   715     UK    Great Britain        13
    ## 702 -4.25341797 54.84678    15   716     UK    Great Britain        13
    ## 703 -4.30366230 54.83569    15   717     UK    Great Britain        13
    ## 704 -4.40991259 54.78706    15   718     UK    Great Britain        13
    ## 705 -4.51748085 54.75835    15   719     UK    Great Britain        13
    ## 706 -4.64755869 54.78901    15   720     UK    Great Britain        13
    ## 707 -4.81806612 54.84614    15   721     UK    Great Britain        13
    ## 708 -4.85170937 54.82529    15   722     UK    Great Britain        13
    ## 709 -4.88950205 54.77227    15   723     UK    Great Britain        13
    ## 710 -4.91123056 54.68945    15   724     UK    Great Britain        13
    ## 711 -5.03232479 54.76138    15   725     UK    Great Britain        13
    ## 712 -5.13549805 54.85752    15   726     UK    Great Britain        13
    ## 713 -5.17011738 54.91792    15   727     UK    Great Britain        13
    ## 714 -5.17270470 54.98589    15   728     UK    Great Britain        13
    ## 715 -5.11669922 55.01226    15   729     UK    Great Britain        13
    ## 716 -5.05585909 54.98814    15   730     UK    Great Britain        13
    ## 717 -4.96518612 55.14946    15   731     UK    Great Britain        13
    ## 718 -4.78481483 55.35942    15   732     UK    Great Britain        13
    ## 719 -4.72114229 55.42099    15   733     UK    Great Britain        13
    ## 720 -4.67675781 55.50132    15   734     UK    Great Britain        13
    ## 721 -4.68437529 55.55391    15   735     UK    Great Britain        13
    ## 722 -4.72416973 55.59829    15   736     UK    Great Britain        13
    ## 723 -4.89184570 55.69912    15   737     UK    Great Britain        13
    ## 724 -4.88964844 55.78120    15   738     UK    Great Britain        13
    ## 725 -4.87167978 55.87392    15   739     UK    Great Britain        13
    ## 726 -4.82607460 55.92954    15   740     UK    Great Britain        13
    ## 727 -4.80683565 55.94014    15   741     UK    Great Britain        13
    ## 728 -4.58408213 55.93867    15   742     UK    Great Britain        13
    ## 729 -4.67094755 55.96738    15   743     UK    Great Britain        13
    ## 730 -4.84409189 56.05117    15   744     UK    Great Britain        13
    ## 731 -4.84101582 56.08086    15   745     UK    Great Britain        13
    ## 732 -4.80029345 56.15835    15   746     UK    Great Britain        13
    ## 733 -4.81914091 56.15049    15   747     UK    Great Britain        13
    ## 734 -4.85624981 56.11470    15   748     UK    Great Britain        13
    ## 735 -4.92709970 56.02808    15   749     UK    Great Britain        13
    ## 736 -4.97036171 56.00786    15   750     UK    Great Britain        13
    ## 737 -5.09282255 55.98730    15   751     UK    Great Britain        13
    ## 738 -5.11499023 55.94463    15   752     UK    Great Britain        13
    ## 739 -5.13466787 55.93349    15   753     UK    Great Britain        13
    ## 740 -5.19584942 55.92866    15   754     UK    Great Britain        13
    ## 741 -5.21459913 55.88887    15   755     UK    Great Britain        13
    ## 742 -5.22822237 55.88633    15   756     UK    Great Britain        13
    ## 743 -5.24560595 55.92925    15   757     UK    Great Britain        13
    ## 744 -5.24731398 56.00039    15   758     UK    Great Britain        13
    ## 745 -5.22294903 56.06582    15   759     UK    Great Britain        13
    ## 746 -5.17641592 56.11699    15   760     UK    Great Britain        13
    ## 747 -4.99697304 56.23335    15   761     UK    Great Britain        13
    ## 748 -5.08432627 56.19746    15   762     UK    Great Britain        13
    ## 749 -5.28232431 56.08994    15   763     UK    Great Britain        13
    ## 750 -5.38344717 56.01924    15   764     UK    Great Britain        13
    ## 751 -5.41044903 55.99536    15   765     UK    Great Britain        13
    ## 752 -5.41889668 55.97524    15   766     UK    Great Britain        13
    ## 753 -5.41831064 55.95205    15   767     UK    Great Britain        13
    ## 754 -5.37290049 55.82769    15   768     UK    Great Britain        13
    ## 755 -5.38583994 55.77011    15   769     UK    Great Britain        13
    ## 756 -5.55644560 55.38960    15   770     UK    Great Britain        13
    ## 757 -5.58876944 55.35142    15   771     UK    Great Britain        13
    ## 758 -5.61845684 55.33144    15   772     UK    Great Britain        13
    ## 759 -5.64653349 55.32685    15   773     UK    Great Britain        13
    ## 760 -5.73066425 55.33413    15   774     UK    Great Britain        13
    ## 761 -5.76821280 55.36264    15   775     UK    Great Britain        13
    ## 762 -5.76787090 55.39497    15   776     UK    Great Britain        13
    ## 763 -5.75209951 55.44345    15   777     UK    Great Britain        13
    ## 764 -5.68134737 55.62397    15   778     UK    Great Britain        13
    ## 765 -5.65063477 55.67412    15   779     UK    Great Britain        13
    ## 766 -5.60502958 55.72075    15   780     UK    Great Britain        13
    ## 767 -5.50449228 55.80239    15   781     UK    Great Britain        13
    ## 768 -5.50693369 55.80771    15   782     UK    Great Britain        13
    ## 769 -5.57387638 55.79170    15   783     UK    Great Britain        13
    ## 770 -5.60239267 55.79697    15   784     UK    Great Britain        13
    ## 771 -5.62285137 55.81313    15   785     UK    Great Britain        13
    ## 772 -5.60957050 56.05527    15   786     UK    Great Britain        13
    ## 773 -5.55527353 56.13496    15   787     UK    Great Britain        13
    ## 774 -5.53496075 56.25083    15   788     UK    Great Britain        13
    ## 775 -5.48789072 56.35005    15   789     UK    Great Britain        13
    ## 776 -5.43339825 56.42231    15   790     UK    Great Britain        13
    ## 777 -5.39194298 56.51479    15   791     UK    Great Britain        13
    ## 778 -5.32944298 56.55591    15   792     UK    Great Britain        13
    ## 779 -5.31269550 56.61880    15   793     UK    Great Britain        13
    ## 780 -5.24257851 56.68686    15   794     UK    Great Britain        13
    ## 781 -5.18837881 56.75806    15   795     UK    Great Britain        13
    ## 782 -5.21757793 56.75102    15   796     UK    Great Britain        13
    ## 783 -5.56420946 56.56572    15   797     UK    Great Britain        13
    ## 784 -5.65244150 56.53198    15   798     UK    Great Britain        13
    ## 785 -5.77280235 56.54102    15   799     UK    Great Britain        13
    ## 786 -5.86484337 56.56187    15   800     UK    Great Britain        13
    ## 787 -5.93676758 56.60571    15   801     UK    Great Britain        13
    ## 788 -5.96889687 56.68990    15   802     UK    Great Britain        13
    ## 789 -6.05771494 56.69214    15   803     UK    Great Britain        13
    ## 790 -6.13369131 56.70669    15   804     UK    Great Britain        13
    ## 791 -6.13276386 56.71802    15   805     UK    Great Britain        13
    ## 792 -6.03471661 56.76392    15   806     UK    Great Britain        13
    ## 793 -5.87763643 56.77964    15   807     UK    Great Britain        13
    ## 794 -5.73061514 56.85308    15   808     UK    Great Britain        13
    ## 795 -5.86142588 56.90268    15   809     UK    Great Britain        13
    ## 796 -5.85039091 56.91841    15   810     UK    Great Britain        13
    ## 797 -5.73627949 56.96065    15   811     UK    Great Britain        13
    ## 798 -5.59130859 57.10234    15   812     UK    Great Britain        13
    ## 799 -5.56191397 57.23271    15   813     UK    Great Britain        13
    ## 800 -5.63124990 57.29395    15   814     UK    Great Britain        13
    ## 801 -5.65634775 57.33408    15   815     UK    Great Britain        13
    ## 802 -5.79492188 57.37881    15   816     UK    Great Britain        13
    ## 803 -5.81806612 57.43608    15   817     UK    Great Britain        13
    ## 804 -5.80195332 57.46802    15   818     UK    Great Britain        13
    ## 805 -5.75673819 57.49922    15   819     UK    Great Britain        13
    ## 806 -5.68862295 57.52353    15   820     UK    Great Britain        13
    ## 807 -5.58178663 57.54678    15   821     UK    Great Britain        13
    ## 808 -5.67876005 57.57168    15   822     UK    Great Britain        13
    ## 809 -5.71494150 57.60107    15   823     UK    Great Britain        13
    ## 810 -5.74238300 57.64365    15   824     UK    Great Britain        13
    ## 811 -5.74492168 57.66831    15   825     UK    Great Britain        13
    ## 812 -5.69472647 57.77822    15   826     UK    Great Britain        13
    ## 813 -5.66547823 57.82354    15   827     UK    Great Britain        13
    ## 814 -5.60834980 57.88134    15   828     UK    Great Britain        13
    ## 815 -5.34902334 57.87807    15   829     UK    Great Britain        13
    ## 816 -5.31918955 57.90361    15   830     UK    Great Britain        13
    ## 817 -5.28979492 57.90459    15   831     UK    Great Britain        13
    ## 818 -5.15722656 57.88134    15   832     UK    Great Britain        13
    ## 819 -5.17690468 57.90640    15   833     UK    Great Britain        13
    ## 820 -5.39375019 58.04360    15   834     UK    Great Britain        13
    ## 821 -5.41318369 58.06973    15   835     UK    Great Britain        13
    ## 822 -5.35136747 58.14370    15   836     UK    Great Britain        13
    ## 823 -5.34687471 58.17666    15   837     UK    Great Britain        13
    ## 824 -5.35595703 58.21191    15   838     UK    Great Britain        13
    ## 825 -5.33828115 58.23872    15   839     UK    Great Britain        13
    ## 826 -5.26953125 58.25142    15   840     UK    Great Britain        13
    ## 827 -5.05996132 58.25015    15   841     UK    Great Britain        13
    ## 828 -5.00830078 58.26265    15   842     UK    Great Britain        13
    ## 829 -5.03183556 58.29829    15   843     UK    Great Britain        13
    ## 830 -5.08061504 58.34516    15   844     UK    Great Britain        13
    ## 831 -5.09013700 58.38452    15   845     UK    Great Britain        13
    ## 832 -5.07871103 58.41928    15   846     UK    Great Britain        13
    ## 833 -5.07602549 58.48926    15   847     UK    Great Britain        13
    ## 834 -5.06650352 58.52021    15   848     UK    Great Britain        13
    ## 835 -5.01674795 58.56655    15   849     UK    Great Britain        13
    ## 836 -4.97563505 58.58032    15   850     UK    Great Britain        13
    ## 837 -4.92465830 58.58838    15   851     UK    Great Britain        13
    ## 838 -4.80961895 58.57290    15   852     UK    Great Britain        13
    ## 839 -4.76577139 58.55420    15   853     UK    Great Britain        13
    ## 840 -4.71542931 58.51001    15   854     UK    Great Britain        13
    ## 841 -4.67822266 58.51358    15   855     UK    Great Britain        13
    ## 842 -4.53496075 58.56157    15   856     UK    Great Britain        13
    ## 843 -4.49189425 58.56845    15   857     UK    Great Britain        13
    ## 844 -4.43325186 58.51284    15   858     UK    Great Britain        13
    ## 845 -4.18862295 58.55723    15   859     UK    Great Britain        13
    ## 846 -3.85952187 58.57710    15   860     UK    Great Britain        13
    ## 847 -3.66181636 58.60630    15   861     UK    Great Britain        13
    ## 848 -3.45356441 58.61689    15   862     UK    Great Britain        13
    ## 849 -3.25913072 58.65000    15   863     UK    Great Britain        13
    ## 850 -3.05307603 58.63482    15   864     UK    Great Britain        13
    ## 851 -3.04619145 58.61553    15   865     UK    Great Britain        13
    ## 852 -3.05698252 58.58877    15   866     UK    Great Britain        13
    ## 853 -3.10966778 58.51548    15   867     UK    Great Britain        13
    ## 854 -2.92939448 58.74160    16   869     UK         Scotland         1
    ## 855 -2.93896461 58.73862    16   870     UK         Scotland         1
    ## 856 -2.97539043 58.75694    16   871     UK         Scotland         1
    ## 857 -3.03544903 58.82265    16   872     UK         Scotland         1
    ## 858 -2.94121075 58.83569    16   873     UK         Scotland         1
    ## 859 -2.89643574 58.82759    16   874     UK         Scotland         1
    ## 860 -2.91308594 58.79961    16   875     UK         Scotland         1
    ## 861 -2.92939448 58.74160    16   876     UK         Scotland         1
    ## 862 -3.16494155 58.79419    17   878     UK         Scotland         1
    ## 863 -3.22211933 58.78096    17   879     UK         Scotland         1
    ## 864 -3.27880883 58.78193    17   880     UK         Scotland         1
    ## 865 -3.36718774 58.83974    17   881     UK         Scotland         1
    ## 866 -3.40083003 58.88178    17   882     UK         Scotland         1
    ## 867 -3.39472675 58.90962    17   883     UK         Scotland         1
    ## 868 -3.35742211 58.91899    17   884     UK         Scotland         1
    ## 869 -3.27192354 58.90527    17   885     UK         Scotland         1
    ## 870 -3.22763681 58.85717    17   886     UK         Scotland         1
    ## 871 -3.22211933 58.82588    17   887     UK         Scotland         1
    ## 872 -3.21162081 58.81358    17   888     UK         Scotland         1
    ## 873 -3.15854502 58.80122    17   889     UK         Scotland         1
    ## 874 -3.16494155 58.79419    17   890     UK         Scotland         1
    ## 875 -3.05742192 59.02964    18   892     UK         Scotland         1
    ## 876 -3.07070327 59.00498    18   893     UK         Scotland         1
    ## 877 -2.99467778 59.00557    18   894     UK         Scotland         1
    ## 878 -2.88457036 58.98452    18   895     UK         Scotland         1
    ## 879 -2.81791973 58.98189    18   896     UK         Scotland         1
    ## 880 -2.76245117 58.95581    18   897     UK         Scotland         1
    ## 881 -2.79301739 58.90693    18   898     UK         Scotland         1
    ## 882 -2.82622075 58.89326    18   899     UK         Scotland         1
    ## 883 -2.86376953 58.89053    18   900     UK         Scotland         1
    ## 884 -2.99482417 58.93935    18   901     UK         Scotland         1
    ## 885 -3.16660142 58.91909    18   902     UK         Scotland         1
    ## 886 -3.20078111 58.92529    18   903     UK         Scotland         1
    ## 887 -3.22333956 58.93877    18   904     UK         Scotland         1
    ## 888 -3.23261690 58.95552    18   905     UK         Scotland         1
    ## 889 -3.23281240 58.98965    18   906     UK         Scotland         1
    ## 890 -3.24213839 58.99971    18   907     UK         Scotland         1
    ## 891 -3.30434561 58.96743    18   908     UK         Scotland         1
    ## 892 -3.33164072 58.97124    18   909     UK         Scotland         1
    ## 893 -3.34707046 58.98672    18   910     UK         Scotland         1
    ## 894 -3.35371113 59.01875    18   911     UK         Scotland         1
    ## 895 -3.34682631 59.06499    18   912     UK         Scotland         1
    ## 896 -3.31035137 59.13081    18   913     UK         Scotland         1
    ## 897 -3.24858427 59.14395    18   914     UK         Scotland         1
    ## 898 -3.15649438 59.13633    18   915     UK         Scotland         1
    ## 899 -3.05112267 59.09903    18   916     UK         Scotland         1
    ## 900 -3.01923847 59.07603    18   917     UK         Scotland         1
    ## 901 -3.02001929 59.05767    18   918     UK         Scotland         1
    ## 902 -3.05742192 59.02964    18   919     UK         Scotland         1
    ## 903 -2.54887724 59.23135    19   921     UK         Scotland         1
    ## 904 -2.66206050 59.23018    19   922     UK         Scotland         1
    ## 905 -2.60361314 59.28931    19   923     UK         Scotland         1
    ## 906 -2.53564477 59.30415    19   924     UK         Scotland         1
    ## 907 -2.40698266 59.29756    19   925     UK         Scotland         1
    ## 908 -2.42983389 59.27104    19   926     UK         Scotland         1
    ## 909 -2.54887724 59.23135    19   927     UK         Scotland         1
    ## 910 -2.72939444 59.18676    20   929     UK         Scotland         1
    ## 911 -2.81523442 59.16192    20   930     UK         Scotland         1
    ## 912 -2.85185552 59.18247    20   931     UK         Scotland         1
    ## 913 -2.86142588 59.24682    20   932     UK         Scotland         1
    ## 914 -2.96376967 59.27436    20   933     UK         Scotland         1
    ## 915 -3.01347661 59.29146    20   934     UK         Scotland         1
    ## 916 -3.05205107 59.32388    20   935     UK         Scotland         1
    ## 917 -3.04223633 59.33384    20   936     UK         Scotland         1
    ## 918 -2.97553706 59.34712    20   937     UK         Scotland         1
    ## 919 -2.86162114 59.28833    20   938     UK         Scotland         1
    ## 920 -2.81503892 59.24082    20   939     UK         Scotland         1
    ## 921 -2.73066425 59.22676    20   940     UK         Scotland         1
    ## 922 -2.71992183 59.21948    20   941     UK         Scotland         1
    ## 923 -2.72939444 59.18676    20   942     UK         Scotland         1
    ## 924 -1.30810571 60.53750    21   944     UK         Scotland         1
    ## 925 -1.28740239 60.46704    21   945     UK         Scotland         1
    ## 926 -1.23574221 60.48530    21   946     UK         Scotland         1
    ## 927 -1.15776384 60.41772    21   947     UK         Scotland         1
    ## 928 -1.11796904 60.41763    21   948     UK         Scotland         1
    ## 929 -1.05244160 60.44448    21   949     UK         Scotland         1
    ## 930 -1.06567395 60.38159    21   950     UK         Scotland         1
    ## 931 -1.13369155 60.20699    21   951     UK         Scotland         1
    ## 932 -1.15278316 60.17734    21   952     UK         Scotland         1
    ## 933 -1.16572285 60.12427    21   953     UK         Scotland         1
    ## 934 -1.17924798 60.11391    21   954     UK         Scotland         1
    ## 935 -1.19931638 60.00659    21   955     UK         Scotland         1
    ## 936 -1.24531233 59.97124    21   956     UK         Scotland         1
    ## 937 -1.28378928 59.88691    21   957     UK         Scotland         1
    ## 938 -1.29946315 59.87866    21   958     UK         Scotland         1
    ## 939 -1.35585940 59.91113    21   959     UK         Scotland         1
    ## 940 -1.29951179 60.03984    21   960     UK         Scotland         1
    ## 941 -1.27617180 60.11465    21   961     UK         Scotland         1
    ## 942 -1.29091799 60.15347    21   962     UK         Scotland         1
    ## 943 -1.32280254 60.18838    21   963     UK         Scotland         1
    ## 944 -1.40903318 60.18950    21   964     UK         Scotland         1
    ## 945 -1.48149407 60.17339    21   965     UK         Scotland         1
    ## 946 -1.49687517 60.19399    21   966     UK         Scotland         1
    ## 947 -1.49912119 60.22178    21   967     UK         Scotland         1
    ## 948 -1.51660156 60.23101    21   968     UK         Scotland         1
    ## 949 -1.61303723 60.22910    21   969     UK         Scotland         1
    ## 950 -1.64135730 60.23677    21   970     UK         Scotland         1
    ## 951 -1.66005874 60.26225    21   971     UK         Scotland         1
    ## 952 -1.66376972 60.28252    21   972     UK         Scotland         1
    ## 953 -1.57666004 60.29839    21   973     UK         Scotland         1
    ## 954 -1.49443376 60.29248    21   974     UK         Scotland         1
    ## 955 -1.37460935 60.33291    21   975     UK         Scotland         1
    ## 956 -1.44956028 60.46855    21   976     UK         Scotland         1
    ## 957 -1.54882812 60.48130    21   977     UK         Scotland         1
    ## 958 -1.57177734 60.49443    21   978     UK         Scotland         1
    ## 959 -1.55263662 60.51743    21   979     UK         Scotland         1
    ## 960 -1.49814427 60.52983    21   980     UK         Scotland         1
    ## 961 -1.41420877 60.59873    21   981     UK         Scotland         1
    ## 962 -1.36396503 60.60957    21   982     UK         Scotland         1
    ## 963 -1.30170906 60.60766    21   983     UK         Scotland         1
    ## 964 -1.30810571 60.53750    21   984     UK         Scotland         1
    ## 965 -1.04252934 60.51386    22   986     UK         Scotland         1
    ## 966 -1.06787121 60.50229    22   987     UK         Scotland         1
    ## 967 -1.16552734 60.60390    22   988     UK         Scotland         1
    ## 968 -1.09331059 60.72022    22   989     UK         Scotland         1
    ## 969 -1.00561535 60.71650    22   990     UK         Scotland         1
    ## 970 -0.99165016 60.68603    22   991     UK         Scotland         1
    ## 971 -1.00034189 60.65801    22   992     UK         Scotland         1
    ## 972 -1.04501951 60.65551    22   993     UK         Scotland         1
    ## 973 -1.04902327 60.64692    22   994     UK         Scotland         1
    ## 974 -1.03510725 60.59292    22   995     UK         Scotland         1
    ## 975 -1.03422832 60.53017    22   996     UK         Scotland         1
    ## 976 -1.04252934 60.51386    22   997     UK         Scotland         1
    ## 977 -0.77426767 60.81196    23   999     UK         Scotland         1
    ## 978 -0.77431637 60.80049    23  1000     UK         Scotland         1
    ## 979 -0.82617188 60.71616    23  1001     UK         Scotland         1
    ## 980 -0.82548839 60.68394    23  1002     UK         Scotland         1
    ## 981 -0.90913105 60.68701    23  1003     UK         Scotland         1
    ## 982 -0.92226553 60.69727    23  1004     UK         Scotland         1
    ## 983 -0.93808603 60.74566    23  1005     UK         Scotland         1
    ## 984 -0.92753905 60.79716    23  1006     UK         Scotland         1
    ## 985 -0.91582036 60.81045    23  1007     UK         Scotland         1
    ## 986 -0.89140600 60.81591    23  1008     UK         Scotland         1
    ## 987 -0.86494166 60.80581    23  1009     UK         Scotland         1
    ## 988 -0.82343775 60.83189    23  1010     UK         Scotland         1
    ## 989 -0.80180693 60.83125    23  1011     UK         Scotland         1
    ## 990 -0.77426767 60.81196    23  1012     UK         Scotland         1

``` r
country_data %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  coord_map() + 
  theme(legend.key.width=unit(3,"lines"),legend.position="bottom",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->
