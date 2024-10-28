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
library(ggsignif)
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
stakeholder_survey %>% mutate(dietary_sustainability_over_dietary_health=case_when(dietary_sustainability_ranking<dietary_health_ranking ~ 1,
                                                                                   dietary_sustainability_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(dietary_sustainability_over_dietary_health_sum=sum(dietary_sustainability_over_dietary_health))
```

    ##   dietary_sustainability_over_dietary_health_sum
    ## 1                                             10

``` r
stakeholder_survey %>% mutate(institutional_sustainability_over_dietary_health=case_when(institutional_sustainability_ranking<dietary_health_ranking ~ 1,
                                                                                   institutional_sustainability_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(institutional_sustainability_over_dietary_health_sum=sum(institutional_sustainability_over_dietary_health))
```

    ##   institutional_sustainability_over_dietary_health_sum
    ## 1                                                   13

``` r
stakeholder_survey %>% mutate(food_pricing_over_dietary_health=case_when(food_pricing_ranking<dietary_health_ranking ~ 1,
                                                                                   food_pricing_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(food_pricing_over_dietary_health_sum=sum(food_pricing_over_dietary_health))
```

    ##   food_pricing_over_dietary_health_sum
    ## 1                                   13

``` r
stakeholder_survey %>% mutate(operational_costs_over_dietary_health=case_when(operational_costs_ranking<dietary_health_ranking ~ 1,
                                                                                   operational_costs_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(operational_costs_over_dietary_health_sum=sum(operational_costs_over_dietary_health))
```

    ##   operational_costs_over_dietary_health_sum
    ## 1                                        10

``` r
stakeholder_survey %>% mutate(guest_satisfaction_over_dietary_health=case_when(guest_satisfaction_ranking<dietary_health_ranking ~ 1,
                                                                                   guest_satisfaction_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(guest_satisfaction_over_dietary_health_sum=sum(guest_satisfaction_over_dietary_health))
```

    ##   guest_satisfaction_over_dietary_health_sum
    ## 1                                         20

``` r
stakeholder_survey %>% mutate(worker_satisfaction_over_dietary_health=case_when(worker_satisfaction_ranking<dietary_health_ranking ~ 1,
                                                                                   worker_satisfaction_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(worker_satisfaction_over_dietary_health_sum=sum(worker_satisfaction_over_dietary_health))
```

    ##   worker_satisfaction_over_dietary_health_sum
    ## 1                                           9

``` r
stakeholder_survey %>% mutate(campus_culture_over_dietary_health=case_when(campus_culture_ranking<dietary_health_ranking ~ 1,
                                                                                   campus_culture_ranking>dietary_health_ranking ~ 0)) %>%
  summarise(campus_culture_over_dietary_health_sum=sum(campus_culture_over_dietary_health))
```

    ##   campus_culture_over_dietary_health_sum
    ## 1                                     11

``` r
stakeholder_survey %>% mutate(institutional_sustainability_over_dietary_sustainability=case_when(institutional_sustainability_ranking<dietary_sustainability_ranking ~ 1,
                                                                                   institutional_sustainability_ranking>dietary_sustainability_ranking ~ 0)) %>%
  summarise(institutional_sustainability_over_dietary_sustainability_sum=sum(institutional_sustainability_over_dietary_sustainability))
```

    ##   institutional_sustainability_over_dietary_sustainability_sum
    ## 1                                                           15

``` r
stakeholder_survey %>% mutate(food_pricing_over_dietary_sustainability=case_when(food_pricing_ranking<dietary_sustainability_ranking ~ 1,
                                                                                   food_pricing_ranking>dietary_sustainability_ranking ~ 0)) %>%
  summarise(food_pricing_over_dietary_sustainability_sum=sum(food_pricing_over_dietary_sustainability))
```

    ##   food_pricing_over_dietary_sustainability_sum
    ## 1                                           15

``` r
stakeholder_survey %>% mutate(operational_costs_over_dietary_sustainability=case_when(operational_costs_ranking<dietary_sustainability_ranking ~ 1,
                                                                                   operational_costs_ranking>dietary_sustainability_ranking ~ 0)) %>%
  summarise(operational_costs_over_dietary_sustainability_sum=sum(operational_costs_over_dietary_sustainability))
```

    ##   operational_costs_over_dietary_sustainability_sum
    ## 1                                                13

``` r
stakeholder_survey %>% mutate(guest_satisfaction_over_dietary_sustainability=case_when(guest_satisfaction_ranking<dietary_sustainability_ranking ~ 1,
                                                                                   guest_satisfaction_ranking>dietary_sustainability_ranking ~ 0)) %>%
  summarise(guest_satisfaction_over_dietary_sustainability_sum=sum(guest_satisfaction_over_dietary_sustainability))
```

    ##   guest_satisfaction_over_dietary_sustainability_sum
    ## 1                                                 24

``` r
stakeholder_survey %>% mutate(campus_culture_over_dietary_sustainability=case_when(campus_culture_ranking<dietary_sustainability_ranking ~ 1,
                                                                                   campus_culture_ranking>dietary_sustainability_ranking ~ 0)) %>%
  summarise(campus_culture_over_dietary_sustainability_sum=sum(campus_culture_over_dietary_sustainability))
```

    ##   campus_culture_over_dietary_sustainability_sum
    ## 1                                             11

``` r
stakeholder_survey %>% mutate(food_pricing_over_institutional_sustainability=case_when(food_pricing_ranking<institutional_sustainability_ranking ~ 1,
                                                                                   food_pricing_ranking>institutional_sustainability_ranking ~ 0)) %>%
  summarise(food_pricing_over_institutional_sustainability_sum=sum(food_pricing_over_institutional_sustainability))
```

    ##   food_pricing_over_institutional_sustainability_sum
    ## 1                                                 17

``` r
stakeholder_survey %>% mutate(operational_costs_over_institutional_sustainability=case_when(operational_costs_ranking<institutional_sustainability_ranking ~ 1,
                                                                                   operational_costs_ranking>institutional_sustainability_ranking ~ 0)) %>%
  summarise(operational_costs_over_institutional_sustainability_sum=sum(operational_costs_over_institutional_sustainability))
```

    ##   operational_costs_over_institutional_sustainability_sum
    ## 1                                                      17

``` r
stakeholder_survey %>% mutate(guest_satisfaction_over_institutional_sustainability=case_when(guest_satisfaction_ranking<institutional_sustainability_ranking ~ 1,
                                                                                   guest_satisfaction_ranking>institutional_sustainability_ranking ~ 0)) %>%
  summarise(guest_satisfaction_over_institutional_sustainability_sum=sum(guest_satisfaction_over_institutional_sustainability))
```

    ##   guest_satisfaction_over_institutional_sustainability_sum
    ## 1                                                       25

``` r
stakeholder_survey %>% mutate(worker_satisfaction_over_institutional_sustainability=case_when(worker_satisfaction_ranking<institutional_sustainability_ranking ~ 1,
                                                                                   worker_satisfaction_ranking>institutional_sustainability_ranking ~ 0)) %>%
  summarise(worker_satisfaction_over_institutional_sustainability_sum=sum(worker_satisfaction_over_institutional_sustainability))
```

    ##   worker_satisfaction_over_institutional_sustainability_sum
    ## 1                                                        16

``` r
stakeholder_survey %>% mutate(campus_culture_over_institutional_sustainability=case_when(campus_culture_ranking<institutional_sustainability_ranking ~ 1,
                                                                                   campus_culture_ranking>institutional_sustainability_ranking ~ 0)) %>%
  summarise(campus_culture_over_institutional_sustainability_sum=sum(campus_culture_over_institutional_sustainability))
```

    ##   campus_culture_over_institutional_sustainability_sum
    ## 1                                                   17

``` r
stakeholder_survey %>% mutate(operational_costs_over_food_pricing=case_when(operational_costs_ranking<food_pricing_ranking ~ 1,
                                                                                   operational_costs_ranking>food_pricing_ranking ~ 0)) %>%
  summarise(operational_costs_over_food_pricing_sum=sum(operational_costs_over_food_pricing))
```

    ##   operational_costs_over_food_pricing_sum
    ## 1                                      13

``` r
stakeholder_survey %>% mutate(guest_satisfaction_over_food_pricing=case_when(guest_satisfaction_ranking<food_pricing_ranking ~ 1,
                                                                                   guest_satisfaction_ranking>food_pricing_ranking ~ 0)) %>%
  summarise(guest_satisfaction_over_food_pricing_sum=sum(guest_satisfaction_over_food_pricing))
```

    ##   guest_satisfaction_over_food_pricing_sum
    ## 1                                       22

``` r
stakeholder_survey %>% mutate(worker_satisfaction_over_food_pricing=case_when(worker_satisfaction_ranking<food_pricing_ranking ~ 1,
                                                                                   worker_satisfaction_ranking>food_pricing_ranking ~ 0)) %>%
  summarise(worker_satisfaction_over_food_pricing_sum=sum(worker_satisfaction_over_food_pricing))
```

    ##   worker_satisfaction_over_food_pricing_sum
    ## 1                                        12

``` r
stakeholder_survey %>% mutate(campus_culture_over_food_pricing=case_when(campus_culture_ranking<food_pricing_ranking ~ 1,
                                                                                   campus_culture_ranking>food_pricing_ranking ~ 0)) %>%
  summarise(campus_culture_over_food_pricing_sum=sum(campus_culture_over_food_pricing))
```

    ##   campus_culture_over_food_pricing_sum
    ## 1                                   14

``` r
stakeholder_survey %>% mutate(guest_satisfaction_over_operational_costs=case_when(guest_satisfaction_ranking<operational_costs_ranking ~ 1,
                                                                                   guest_satisfaction_ranking>operational_costs_ranking ~ 0)) %>%
  summarise(guest_satisfaction_over_operational_costs_sum=sum(guest_satisfaction_over_operational_costs))
```

    ##   guest_satisfaction_over_operational_costs_sum
    ## 1                                            25

``` r
stakeholder_survey
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
    ##    worker_satisfaction_ranking campus_culture_ranking other_ranking
    ## 1                            6                      8             9
    ## 2                            5                      6             9
    ## 3                            7                      1             9
    ## 4                            7                      8             9
    ## 5                            2                      6             9
    ## 6                            7                      5             9
    ## 7                            6                      7             9
    ## 8                            1                      7             9
    ## 9                            6                      3             9
    ## 10                           6                      8             9
    ## 11                           6                      4             9
    ## 12                           2                      3             9
    ## 13                           8                      1             9
    ## 14                           3                      4             9
    ## 15                           8                      7             9
    ## 16                           7                      5             9
    ## 17                           2                      5             9
    ## 18                           6                      1             9
    ## 19                           3                      7             9
    ## 20                           3                      2             9
    ## 21                           1                      8             9
    ## 22                           7                      8             2
    ## 23                           7                      5             9
    ## 24                           4                      7             9
    ## 25                           8                      7             9
    ## 26                           8                      2             9
    ## 27                           8                      7             9
    ## 28                           7                      4             9
    ## 29                           8                      6             9
    ## 30                           7                      8             9
    ## 31                           2                      1             9
    ## 32                           5                      7             9
    ##    other_ranking_other id dietary_health_score dietary_sustainability_score
    ## 1                       1                    7                            6
    ## 2                       2                    7                            6
    ## 3                       3                    6                            5
    ## 4                       4                    7                            4
    ## 5                       5                    1                            4
    ## 6                       6                    1                            6
    ## 7                       7                    8                            7
    ## 8                       8                    3                            4
    ## 9                       9                    7                            4
    ## 10                     10                    5                            7
    ## 11                     11                    2                            6
    ## 12                     12                    8                            3
    ## 13                     13                    3                            2
    ## 14                     14                    8                            1
    ## 15                     15                    5                            4
    ## 16                     16                    3                            5
    ## 17                     17                    6                            3
    ## 18                     18                    5                            1
    ## 19                     19                    8                            5
    ## 20                     20                    4                            3
    ## 21                     21                    4                            7
    ## 22        Cuisine type 22                    3                            4
    ## 23                     23                    8                            7
    ## 24                     24                    3                            7
    ## 25                     25                    7                            4
    ## 26                     26                    6                            4
    ## 27                     27                    3                            4
    ## 28                     28                    4                            1
    ## 29                     29                    6                            4
    ## 30                     30                    7                            5
    ## 31                     31                    4                            3
    ## 32                     32                    6                            3
    ##    institutional_sustainability_score food_pricing_score
    ## 1                                   3                  4
    ## 2                                   9                  1
    ## 3                                   2                  7
    ## 4                                   4                  5
    ## 5                                   3                  8
    ## 6                                   4                  7
    ## 7                                   7                  4
    ## 8                                   6                  1
    ## 9                                   3                  1
    ## 10                                  9                  2
    ## 11                                  2                  4
    ## 12                                  2                  5
    ## 13                                  5                  6
    ## 14                                  5                  3
    ## 15                                  9                  3
    ## 16                                  2                  7
    ## 17                                  2                  8
    ## 18                                  3                  6
    ## 19                                  2                  3
    ## 20                                  6                  1
    ## 21                                  7                  2
    ## 22                                  6                  6
    ## 23                                  4                  1
    ## 24                                  9                  4
    ## 25                                  4                  6
    ## 26                                  9                  3
    ## 27                                  6                  7
    ## 28                                  4                  8
    ## 29                                  6                  2
    ## 30                                  7                  3
    ## 31                                  3                  1
    ## 32                                  2                  7
    ##    operational_costs_score guest_satisfaction_score worker_satisfaction_score
    ## 1                        3                        8                         3
    ## 2                        4                        5                         4
    ## 3                        2                        3                         2
    ## 4                        2                        8                         2
    ## 5                        7                        5                         7
    ## 6                        2                        5                         2
    ## 7                        3                        5                         3
    ## 8                        8                        7                         8
    ## 9                        3                        8                         3
    ## 10                       3                        4                         3
    ## 11                       3                        8                         3
    ## 12                       7                        4                         7
    ## 13                       1                        5                         1
    ## 14                       6                        7                         6
    ## 15                       1                        6                         1
    ## 16                       2                        8                         2
    ## 17                       7                        5                         7
    ## 18                       3                        7                         3
    ## 19                       6                        7                         6
    ## 20                       6                        8                         6
    ## 21                       8                        5                         8
    ## 22                       7                        8                         2
    ## 23                       2                        6                         2
    ## 24                       5                        1                         5
    ## 25                       1                        8                         1
    ## 26                       1                        5                         1
    ## 27                       1                        6                         1
    ## 28                       2                        7                         2
    ## 29                       1                        7                         1
    ## 30                       2                        8                         2
    ## 31                       7                        5                         7
    ## 32                       4                        5                         4
    ##    campus_culture_score
    ## 1                     1
    ## 2                     3
    ## 3                     8
    ## 4                     1
    ## 5                     3
    ## 6                     4
    ## 7                     2
    ## 8                     2
    ## 9                     6
    ## 10                    1
    ## 11                    5
    ## 12                    6
    ## 13                    8
    ## 14                    5
    ## 15                    2
    ## 16                    4
    ## 17                    4
    ## 18                    8
    ## 19                    2
    ## 20                    7
    ## 21                    1
    ## 22                    1
    ## 23                    4
    ## 24                    2
    ## 25                    2
    ## 26                    7
    ## 27                    2
    ## 28                    5
    ## 29                    3
    ## 30                    1
    ## 31                    8
    ## 32                    2

``` r
stakeholder_survey %>% 
  mutate(worker_satisfaction_over_operational_costs=case_when(worker_satisfaction_ranking<operational_costs_ranking ~ 1,
                                                                                   worker_satisfaction_ranking>operational_costs_ranking ~ 0)) %>%
  summarise(worker_satisfaction_over_operational_costs_sum=sum(worker_satisfaction_over_operational_costs,na.rm=TRUE))
```

    ##   worker_satisfaction_over_operational_costs_sum
    ## 1                                              0

%\>%
summarise(worker_satisfaction_over_operational_costs_sum=sum(worker_satisfaction_over_operational_costs))

``` r
stakeholder_survey
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
    ##    worker_satisfaction_ranking campus_culture_ranking other_ranking
    ## 1                            6                      8             9
    ## 2                            5                      6             9
    ## 3                            7                      1             9
    ## 4                            7                      8             9
    ## 5                            2                      6             9
    ## 6                            7                      5             9
    ## 7                            6                      7             9
    ## 8                            1                      7             9
    ## 9                            6                      3             9
    ## 10                           6                      8             9
    ## 11                           6                      4             9
    ## 12                           2                      3             9
    ## 13                           8                      1             9
    ## 14                           3                      4             9
    ## 15                           8                      7             9
    ## 16                           7                      5             9
    ## 17                           2                      5             9
    ## 18                           6                      1             9
    ## 19                           3                      7             9
    ## 20                           3                      2             9
    ## 21                           1                      8             9
    ## 22                           7                      8             2
    ## 23                           7                      5             9
    ## 24                           4                      7             9
    ## 25                           8                      7             9
    ## 26                           8                      2             9
    ## 27                           8                      7             9
    ## 28                           7                      4             9
    ## 29                           8                      6             9
    ## 30                           7                      8             9
    ## 31                           2                      1             9
    ## 32                           5                      7             9
    ##    other_ranking_other id dietary_health_score dietary_sustainability_score
    ## 1                       1                    7                            6
    ## 2                       2                    7                            6
    ## 3                       3                    6                            5
    ## 4                       4                    7                            4
    ## 5                       5                    1                            4
    ## 6                       6                    1                            6
    ## 7                       7                    8                            7
    ## 8                       8                    3                            4
    ## 9                       9                    7                            4
    ## 10                     10                    5                            7
    ## 11                     11                    2                            6
    ## 12                     12                    8                            3
    ## 13                     13                    3                            2
    ## 14                     14                    8                            1
    ## 15                     15                    5                            4
    ## 16                     16                    3                            5
    ## 17                     17                    6                            3
    ## 18                     18                    5                            1
    ## 19                     19                    8                            5
    ## 20                     20                    4                            3
    ## 21                     21                    4                            7
    ## 22        Cuisine type 22                    3                            4
    ## 23                     23                    8                            7
    ## 24                     24                    3                            7
    ## 25                     25                    7                            4
    ## 26                     26                    6                            4
    ## 27                     27                    3                            4
    ## 28                     28                    4                            1
    ## 29                     29                    6                            4
    ## 30                     30                    7                            5
    ## 31                     31                    4                            3
    ## 32                     32                    6                            3
    ##    institutional_sustainability_score food_pricing_score
    ## 1                                   3                  4
    ## 2                                   9                  1
    ## 3                                   2                  7
    ## 4                                   4                  5
    ## 5                                   3                  8
    ## 6                                   4                  7
    ## 7                                   7                  4
    ## 8                                   6                  1
    ## 9                                   3                  1
    ## 10                                  9                  2
    ## 11                                  2                  4
    ## 12                                  2                  5
    ## 13                                  5                  6
    ## 14                                  5                  3
    ## 15                                  9                  3
    ## 16                                  2                  7
    ## 17                                  2                  8
    ## 18                                  3                  6
    ## 19                                  2                  3
    ## 20                                  6                  1
    ## 21                                  7                  2
    ## 22                                  6                  6
    ## 23                                  4                  1
    ## 24                                  9                  4
    ## 25                                  4                  6
    ## 26                                  9                  3
    ## 27                                  6                  7
    ## 28                                  4                  8
    ## 29                                  6                  2
    ## 30                                  7                  3
    ## 31                                  3                  1
    ## 32                                  2                  7
    ##    operational_costs_score guest_satisfaction_score worker_satisfaction_score
    ## 1                        3                        8                         3
    ## 2                        4                        5                         4
    ## 3                        2                        3                         2
    ## 4                        2                        8                         2
    ## 5                        7                        5                         7
    ## 6                        2                        5                         2
    ## 7                        3                        5                         3
    ## 8                        8                        7                         8
    ## 9                        3                        8                         3
    ## 10                       3                        4                         3
    ## 11                       3                        8                         3
    ## 12                       7                        4                         7
    ## 13                       1                        5                         1
    ## 14                       6                        7                         6
    ## 15                       1                        6                         1
    ## 16                       2                        8                         2
    ## 17                       7                        5                         7
    ## 18                       3                        7                         3
    ## 19                       6                        7                         6
    ## 20                       6                        8                         6
    ## 21                       8                        5                         8
    ## 22                       7                        8                         2
    ## 23                       2                        6                         2
    ## 24                       5                        1                         5
    ## 25                       1                        8                         1
    ## 26                       1                        5                         1
    ## 27                       1                        6                         1
    ## 28                       2                        7                         2
    ## 29                       1                        7                         1
    ## 30                       2                        8                         2
    ## 31                       7                        5                         7
    ## 32                       4                        5                         4
    ##    campus_culture_score
    ## 1                     1
    ## 2                     3
    ## 3                     8
    ## 4                     1
    ## 5                     3
    ## 6                     4
    ## 7                     2
    ## 8                     2
    ## 9                     6
    ## 10                    1
    ## 11                    5
    ## 12                    6
    ## 13                    8
    ## 14                    5
    ## 15                    2
    ## 16                    4
    ## 17                    4
    ## 18                    8
    ## 19                    2
    ## 20                    7
    ## 21                    1
    ## 22                    1
    ## 23                    4
    ## 24                    2
    ## 25                    2
    ## 26                    7
    ## 27                    2
    ## 28                    5
    ## 29                    3
    ## 30                    1
    ## 31                    8
    ## 32                    2

``` r
stakeholder_survey %>% mutate(campus_culture_over_operational_costs=case_when(campus_culture_ranking<operational_costs_ranking ~ 1,
                                                                                   campus_culture_ranking>operational_costs_ranking ~ 0)) %>%
  summarise(campus_culture_over_operational_costs_sum=sum(campus_culture_over_operational_costs))
```

    ##   campus_culture_over_operational_costs_sum
    ## 1                                        16

``` r
stakeholder_survey %>% mutate(worker_satisfaction_over_guest_satisfaction=case_when(worker_satisfaction_ranking<guest_satisfaction_ranking ~ 1,
                                                                                   worker_satisfaction_ranking>guest_satisfaction_ranking ~ 0)) %>%
  summarise(worker_satisfaction_over_guest_satisfaction_sum=sum(worker_satisfaction_over_guest_satisfaction))
```

    ##   worker_satisfaction_over_guest_satisfaction_sum
    ## 1                                               7

``` r
stakeholder_survey %>% mutate(campus_culture_over_guest_satisfaction=case_when(campus_culture_ranking<guest_satisfaction_ranking ~ 1,
                                                                                   campus_culture_ranking>guest_satisfaction_ranking ~ 0)) %>%
  summarise(campus_culture_over_guest_satisfaction_sum=sum(campus_culture_over_guest_satisfaction))
```

    ##   campus_culture_over_guest_satisfaction_sum
    ## 1                                          7

``` r
stakeholder_survey %>% mutate(campus_culture_over_worker_satisfaction=case_when(campus_culture_ranking<worker_satisfaction_ranking ~ 1,
                                                                                   campus_culture_ranking>worker_satisfaction_ranking ~ 0)) %>%
  summarise(campus_culture_over_worker_satisfaction_sum=sum(campus_culture_over_worker_satisfaction))
```

    ##   campus_culture_over_worker_satisfaction_sum
    ## 1                                          16

will need to bridge some of these steps, specifically by using qnorm to
perform z-score conversions

for now, we will do our one-way anova and post-hoc tukey tests by
creating a tibble from the information provided in table 4

``` r
holdover_survey_data <- read.csv("/Users/kenjinchang/github/scr-and-stakeholder-analysis/data/holdover-survey-data.csv")
```

``` r
summary_table <- holdover_survey_data %>% 
  group_by(group) %>%
  summarise(count=n(),
            mean=mean(z_score),
            sd=sd(z_score))
summary_table
```

    ## # A tibble: 8 × 4
    ##   group                                count     mean    sd
    ##   <chr>                                <int>    <dbl> <dbl>
    ## 1 Campus Culture                           8 -0.209   0.296
    ## 2 Campus Food Prices                       8 -0.0205  0.265
    ## 3 Guest Dining Experiences                 8  0.574   0.283
    ## 4 Healthiness of Food Offerings            8  0.267   0.299
    ## 5 Institutional Sustainability             8 -0.163   0.256
    ## 6 Operational and Procurement Costs        8 -0.154   0.365
    ## 7 Sustainability of Guest Food Choices     8 -0.00599 0.386
    ## 8 Worker Satisfaction                      8 -0.297   0.289

``` r
summary_table %>%
  summarise(mean=mean(mean))
```

    ## # A tibble: 1 × 1
    ##       mean
    ##      <dbl>
    ## 1 -0.00112

``` r
holdover_survey_data %>%
  mutate(across(group,str_replace,"Healthiness of Food Offerings","Dietary Health")) %>%
  mutate(across(group,str_replace,"Sustainability of Guest Food Choices","Dietary Sustainability")) %>%
  mutate(across(group,str_replace,"Institutional Sustainability","Organizational Sustainability")) %>%
  mutate(across(group,str_replace,"Campus Food Prices","Food Pricing")) %>%
  mutate(across(group,str_replace,"Operational and Procurement Costs","Operating Costs")) %>%
  mutate(across(group,str_replace,"Guest Dining Experiences","Dining Experience")) %>%
  mutate(across(group,str_replace,"Worker Satisfaction","Staff Satisfaction")) %>%
  mutate(across(group,str_replace,"Campus Culture","Campus Culture")) %>%
  ggplot(aes(x=z_score,y=fct_reorder(group,z_score,.fun="mean"),fill=group,color=group)) + 
  geom_boxplot(outlier.shape=NA,alpha=0.5) +
  geom_vline(xintercept=-0.001115625,linetype="dashed",size=0.3) + 
  geom_jitter(width=0.33,size=2,shape=21,alpha=0.5) +
  scale_fill_brewer(palette="Paired") + 
  scale_color_brewer(palette="Paired") +
  xlab("Priority Score") + 
  ylab("") +
  stat_summary(fun.y=mean,geom="point",shape=20,size=3,color="black",fill="white") +
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(group, str_replace, "Healthiness of Food Offerings",
    ##   "Dietary Health")`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
    ## ℹ Please use the `fun` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](cleaning-script_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->
geom_signif(comparisons=list(c(“Guest Experience”,“Campus
Culture”),x_position=c(0.9,1.1)),annotation=“***”,color=”black”,size=0.3) +
geom_signif(comparisons=list(c(”Guest Experience”,”Food
Pricing”)),annotation=”***”,color=“black”,size=0.3) +

                 c("Guest Experience","Food Pricing"),
                               c("Guest Experience","Organizational Sustainability"),
                               c("Guest Experience","Operating Costs"),
                               c("Guest Experience","Dietary Sustainability"),
                               c("Guest Experience","Staff Satisfaction"),
                               c("Dietary Health","Staff Satisfaction"))

``` r
holdover_survey_data %>%
  mutate(across(group,str_replace,"Healthiness of Food Offerings","Dietary Health")) %>%
  mutate(across(group,str_replace,"Sustainability of Guest Food Choices","Dietary Sustainability")) %>%
  mutate(across(group,str_replace,"Institutional Sustainability","Organizational Sustainability")) %>%
  mutate(across(group,str_replace,"Campus Food Prices","Food Pricing")) %>%
  mutate(across(group,str_replace,"Operational and Procurement Costs","Operating Costs")) %>%
  mutate(across(group,str_replace,"Guest Dining Experiences","Dining Experience")) %>%
  mutate(across(group,str_replace,"Worker Satisfaction","Staff Satisfaction")) %>%
  mutate(across(group,str_replace,"Campus Culture","Campus Culture")) %>%
  ggplot(aes(x=z_score,y=fct_reorder(group,z_score,.fun="mean"),fill=group,color=group)) + 
  geom_violin(draw_quantiles=0.5,adjust=0.66,alpha=0.5,scale="width",trim=TRUE) + 
  geom_vline(xintercept=-0.001115625,linetype="dashed",size=0.3) + 
  geom_jitter(width=0.33,size=2,shape=21,alpha=0.5) +
  scale_fill_brewer(palette="Paired") + 
  scale_color_brewer(palette="Paired") +
  xlab("Priority Score") + 
  ylab("") +
  stat_summary(fun.y=mean,geom="point",shape=20,size=3,color="black",fill="white") +
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
holdover_survey_data %>%
  mutate(across(group,str_replace,"Guest Dining Experiences","Dining Experience")) %>%
  mutate(across(group,str_replace,"Healthiness of Food Offerings","Dietary Health")) %>%
  mutate(across(group,str_replace,"Sustainability of Guest Food Choices","Dietary Sustainability")) %>%
  mutate(across(group,str_replace,"Institutional Sustainability","Organizational Sustainability")) %>%
  mutate(across(group,str_replace,"Campus Food Prices","Food Pricing")) %>%
  mutate(across(group,str_replace,"Operational and Procurement Costs","Operating Costs")) %>%
  mutate(across(group,str_replace,"Worker Satisfaction","Staff Satisfaction")) %>%
  mutate(across(group,str_replace,"Campus Culture","Campus Culture")) %>%
  ggplot(aes(y=z_score,x=fct_reorder(group,z_score,.fun="mean"),fill=group,color=group)) + 
  geom_boxplot(outlier.shape=NA,alpha=0.5) +
  geom_jitter(width=0.33,size=2,shape=21,alpha=0.5) +
  geom_hline(yintercept=-0.001115625,linetype="dashed",size=0.3) +
  geom_signif(comparisons=list(c("Dining Experience","Staff Satisfaction")),color="black",size=0.25,annotation="***",y_position=-1.4,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Dining Experience","Campus Culture")),color="black",size=0.25,annotation="***",y_position=-1.3,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Dining Experience","Organizational Sustainability")),color="black",size=0.25,annotation="***",y_position=-1.2,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Dining Experience","Operating Costs")),color="black",size=0.25,annotation="***",y_position=-1.1,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Dining Experience","Food Pricing")),color="black",size=0.25,annotation="**",y_position=-1.0,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Dining Experience","Dietary Sustainability")),color="black",size=0.25,annotation="**",y_position=-0.9,tip_length=-0.01) +
  geom_signif(comparisons=list(c("Staff Satisfaction","Dietary Health")),color="black",size=0.25,annotation="*",y_position=0.6,tip_length=0.01) +
  scale_fill_brewer(palette="Paired") + 
  scale_color_brewer(palette="Paired") +
  xlab("") + 
  ylab("Priority Score") +
  stat_summary(fun.y=mean,geom="point",shape=20,size=3,color="black",fill="white") +
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
holdover_survey_data %>%
  mutate(across(group,str_replace,"Healthiness of Food Offerings","Dietary Health")) %>%
  mutate(across(group,str_replace,"Sustainability of Guest Food Choices","Dietary Sustainability")) %>%
  mutate(across(group,str_replace,"Institutional Sustainability","Organizational Sustainability")) %>%
  mutate(across(group,str_replace,"Campus Food Prices","Food Pricing")) %>%
  mutate(across(group,str_replace,"Operational and Procurement Costs","Operating Costs")) %>%
  mutate(across(group,str_replace,"Worker Satisfaction","Staff Satisfaction")) %>%
  mutate(across(group,str_replace,"Campus Culture","Campus Culture")) %>%
  ggplot(aes(y=z_score,x=fct_reorder(group,z_score,.fun="mean"),fill=group,color=group)) + 
  geom_violin(draw_quantiles=0.5,adjust=0.66,alpha=0.5,scale="width",trim=TRUE) + 
  geom_hline(yintercept=-0.001115625,linetype="dashed",size=0.3) +
  geom_jitter(width=0.33,size=2,shape=21,alpha=0.5) +
  scale_fill_brewer(palette="Paired") + 
  scale_color_brewer(palette="Paired") +
  stat_summary(fun.y=mean,geom="point",shape=20,size=3,color="black",fill="white") +
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10),axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
aov <- aov(z_score ~ group,data=holdover_survey_data)
summary(aov)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## group        7  4.668  0.6669   7.027 5.17e-06 ***
    ## Residuals   56  5.314  0.0949                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

multiple tukey pairwise comparisons

``` r
TukeyHSD(aov)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = z_score ~ group, data = holdover_survey_data)
    ## 
    ## $group
    ##                                                                              diff
    ## Campus Food Prices-Campus Culture                                       0.1886625
    ## Guest Dining Experiences-Campus Culture                                 0.7830000
    ## Healthiness of Food Offerings-Campus Culture                            0.4761875
    ## Institutional Sustainability-Campus Culture                             0.0465000
    ## Operational and Procurement Costs-Campus Culture                        0.0547000
    ## Sustainability of Guest Food Choices-Campus Culture                     0.2031250
    ## Worker Satisfaction-Campus Culture                                     -0.0882000
    ## Guest Dining Experiences-Campus Food Prices                             0.5943375
    ## Healthiness of Food Offerings-Campus Food Prices                        0.2875250
    ## Institutional Sustainability-Campus Food Prices                        -0.1421625
    ## Operational and Procurement Costs-Campus Food Prices                   -0.1339625
    ## Sustainability of Guest Food Choices-Campus Food Prices                 0.0144625
    ## Worker Satisfaction-Campus Food Prices                                 -0.2768625
    ## Healthiness of Food Offerings-Guest Dining Experiences                 -0.3068125
    ## Institutional Sustainability-Guest Dining Experiences                  -0.7365000
    ## Operational and Procurement Costs-Guest Dining Experiences             -0.7283000
    ## Sustainability of Guest Food Choices-Guest Dining Experiences          -0.5798750
    ## Worker Satisfaction-Guest Dining Experiences                           -0.8712000
    ## Institutional Sustainability-Healthiness of Food Offerings             -0.4296875
    ## Operational and Procurement Costs-Healthiness of Food Offerings        -0.4214875
    ## Sustainability of Guest Food Choices-Healthiness of Food Offerings     -0.2730625
    ## Worker Satisfaction-Healthiness of Food Offerings                      -0.5643875
    ## Operational and Procurement Costs-Institutional Sustainability          0.0082000
    ## Sustainability of Guest Food Choices-Institutional Sustainability       0.1566250
    ## Worker Satisfaction-Institutional Sustainability                       -0.1347000
    ## Sustainability of Guest Food Choices-Operational and Procurement Costs  0.1484250
    ## Worker Satisfaction-Operational and Procurement Costs                  -0.1429000
    ## Worker Satisfaction-Sustainability of Guest Food Choices               -0.2913250
    ##                                                                                 lwr
    ## Campus Food Prices-Campus Culture                                      -0.296269567
    ## Guest Dining Experiences-Campus Culture                                 0.298067933
    ## Healthiness of Food Offerings-Campus Culture                           -0.008744567
    ## Institutional Sustainability-Campus Culture                            -0.438432067
    ## Operational and Procurement Costs-Campus Culture                       -0.430232067
    ## Sustainability of Guest Food Choices-Campus Culture                    -0.281807067
    ## Worker Satisfaction-Campus Culture                                     -0.573132067
    ## Guest Dining Experiences-Campus Food Prices                             0.109405433
    ## Healthiness of Food Offerings-Campus Food Prices                       -0.197407067
    ## Institutional Sustainability-Campus Food Prices                        -0.627094567
    ## Operational and Procurement Costs-Campus Food Prices                   -0.618894567
    ## Sustainability of Guest Food Choices-Campus Food Prices                -0.470469567
    ## Worker Satisfaction-Campus Food Prices                                 -0.761794567
    ## Healthiness of Food Offerings-Guest Dining Experiences                 -0.791744567
    ## Institutional Sustainability-Guest Dining Experiences                  -1.221432067
    ## Operational and Procurement Costs-Guest Dining Experiences             -1.213232067
    ## Sustainability of Guest Food Choices-Guest Dining Experiences          -1.064807067
    ## Worker Satisfaction-Guest Dining Experiences                           -1.356132067
    ## Institutional Sustainability-Healthiness of Food Offerings             -0.914619567
    ## Operational and Procurement Costs-Healthiness of Food Offerings        -0.906419567
    ## Sustainability of Guest Food Choices-Healthiness of Food Offerings     -0.757994567
    ## Worker Satisfaction-Healthiness of Food Offerings                      -1.049319567
    ## Operational and Procurement Costs-Institutional Sustainability         -0.476732067
    ## Sustainability of Guest Food Choices-Institutional Sustainability      -0.328307067
    ## Worker Satisfaction-Institutional Sustainability                       -0.619632067
    ## Sustainability of Guest Food Choices-Operational and Procurement Costs -0.336507067
    ## Worker Satisfaction-Operational and Procurement Costs                  -0.627832067
    ## Worker Satisfaction-Sustainability of Guest Food Choices               -0.776257067
    ##                                                                                upr
    ## Campus Food Prices-Campus Culture                                       0.67359457
    ## Guest Dining Experiences-Campus Culture                                 1.26793207
    ## Healthiness of Food Offerings-Campus Culture                            0.96111957
    ## Institutional Sustainability-Campus Culture                             0.53143207
    ## Operational and Procurement Costs-Campus Culture                        0.53963207
    ## Sustainability of Guest Food Choices-Campus Culture                     0.68805707
    ## Worker Satisfaction-Campus Culture                                      0.39673207
    ## Guest Dining Experiences-Campus Food Prices                             1.07926957
    ## Healthiness of Food Offerings-Campus Food Prices                        0.77245707
    ## Institutional Sustainability-Campus Food Prices                         0.34276957
    ## Operational and Procurement Costs-Campus Food Prices                    0.35096957
    ## Sustainability of Guest Food Choices-Campus Food Prices                 0.49939457
    ## Worker Satisfaction-Campus Food Prices                                  0.20806957
    ## Healthiness of Food Offerings-Guest Dining Experiences                  0.17811957
    ## Institutional Sustainability-Guest Dining Experiences                  -0.25156793
    ## Operational and Procurement Costs-Guest Dining Experiences             -0.24336793
    ## Sustainability of Guest Food Choices-Guest Dining Experiences          -0.09494293
    ## Worker Satisfaction-Guest Dining Experiences                           -0.38626793
    ## Institutional Sustainability-Healthiness of Food Offerings              0.05524457
    ## Operational and Procurement Costs-Healthiness of Food Offerings         0.06344457
    ## Sustainability of Guest Food Choices-Healthiness of Food Offerings      0.21186957
    ## Worker Satisfaction-Healthiness of Food Offerings                      -0.07945543
    ## Operational and Procurement Costs-Institutional Sustainability          0.49313207
    ## Sustainability of Guest Food Choices-Institutional Sustainability       0.64155707
    ## Worker Satisfaction-Institutional Sustainability                        0.35023207
    ## Sustainability of Guest Food Choices-Operational and Procurement Costs  0.63335707
    ## Worker Satisfaction-Operational and Procurement Costs                   0.34203207
    ## Worker Satisfaction-Sustainability of Guest Food Choices                0.19360707
    ##                                                                            p adj
    ## Campus Food Prices-Campus Culture                                      0.9210132
    ## Guest Dining Experiences-Campus Culture                                0.0001151
    ## Healthiness of Food Offerings-Campus Culture                           0.0577452
    ## Institutional Sustainability-Campus Culture                            0.9999873
    ## Operational and Procurement Costs-Campus Culture                       0.9999615
    ## Sustainability of Guest Food Choices-Campus Culture                    0.8878762
    ## Worker Satisfaction-Campus Culture                                     0.9990727
    ## Guest Dining Experiences-Campus Food Prices                            0.0067500
    ## Healthiness of Food Offerings-Campus Food Prices                       0.5787489
    ## Institutional Sustainability-Campus Food Prices                        0.9825184
    ## Operational and Procurement Costs-Campus Food Prices                   0.9876030
    ## Sustainability of Guest Food Choices-Campus Food Prices                1.0000000
    ## Worker Satisfaction-Campus Food Prices                                 0.6243598
    ## Healthiness of Food Offerings-Guest Dining Experiences                 0.4963788
    ## Institutional Sustainability-Guest Dining Experiences                  0.0003308
    ## Operational and Procurement Costs-Guest Dining Experiences             0.0003974
    ## Sustainability of Guest Food Choices-Guest Dining Experiences          0.0089635
    ## Worker Satisfaction-Guest Dining Experiences                           0.0000146
    ## Institutional Sustainability-Healthiness of Food Offerings             0.1184066
    ## Operational and Procurement Costs-Healthiness of Food Offerings        0.1331980
    ## Sustainability of Guest Food Choices-Healthiness of Food Offerings     0.6404703
    ## Worker Satisfaction-Healthiness of Food Offerings                      0.0120744
    ## Operational and Procurement Costs-Institutional Sustainability         1.0000000
    ## Sustainability of Guest Food Choices-Institutional Sustainability      0.9699321
    ## Worker Satisfaction-Institutional Sustainability                       0.9871988
    ## Sustainability of Guest Food Choices-Operational and Procurement Costs 0.9776821
    ## Worker Satisfaction-Operational and Procurement Costs                  0.9819941
    ## Worker Satisfaction-Sustainability of Guest Food Choices               0.5624358

from this, we can glean that there are statistically significant
differences between guest satisfaction and six of the remaining
performance indicators: culture (p=0.0001151), food pricing
(p=0.0067500), institutional sustainability (p=0.0003308), operational
costs (p=0.0003974), dietary sustainability (p=0.0089635), and worker
satisfaction (p=0.0000146), as well as between worker satisfaction and
dietary health (p=0.0120744).

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
global_frequencies <- aggregated_data %>%
  ggplot(aes(x=long,y=lat,fill=count,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  scale_y_continuous(limits=c(-60,90)) + 
  xlab("") + 
  ylab("") +
  labs(caption="") +
  theme(legend.key.width=unit(3,"lines"),legend.position="bottom",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
global_frequencies
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

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
usa_frequencies <- state_data %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  coord_map() +
  theme(legend.key.width=unit(3,"lines"),legend.position="none",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
usa_frequencies
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-98-1.png)<!-- -->

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

great britain vs england

``` r
country_data <- left_join(uk_shapefile,country_frequencies,by="country")
```

``` r
uk_frequencies <- country_data %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125,alpha=0.75) +
  scale_fill_gradient(low="lavender",high="slateblue4",na.value="white",name="Intervention-Receiving Institutions",guide=guide_colourbar(reverse=FALSE,alpha=0.75,title.position="top",title.hjust=0.5,limits=c(1,74))) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  coord_map() + 
  theme(legend.key.width=unit(3,"lines"),legend.position="none",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
uk_frequencies
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-106-1.png)<!-- -->

``` r
subregion_frequencies <- ggarrange(usa_frequencies,uk_frequencies,
          ncol=2,
          widths=c(2,1),
          labels=c("B","C"))
subregion_frequencies
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-107-1.png)<!-- -->

``` r
region_frequencies <- ggarrange(global_frequencies,
          labels="A",
          legend="none")
region_frequencies
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-108-1.png)<!-- -->
