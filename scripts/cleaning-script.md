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

``` r
stakeholder_survey <- stakeholder_survey %>%
  select(RecordedDate,Progress,DistributionChannel,Q1,Q2,Q2_4_TEXT,Q6,Q6_10_TEXT,Q3,Q5_1,Q2_1,Q2_2,Q2_3,Q2_4,Q2_5,Q2_6,Q2_7,Q2_8,Q2_9,Q2_9_TEXT) %>%
  rename(data=RecordedDate,completion=Progress,channel=DistributionChannel,consent=Q1,involvement=Q2,involvement_other=Q2_4_TEXT,role_type=Q6,role_type_other=Q6_10_TEXT,title=Q3,role_duration=Q5_1,dietary_health_ranking=Q2_1,dietary_sustainability_ranking=Q2_2,institutional_sustainability_ranking=Q2_3,food_pricing_ranking=Q2_4,operational_costs_ranking=Q2_5,guest_satisfaction_ranking=Q2_6,worker_satisfaction_ranking=Q2_7,campus_culture_ranking=Q2_8,other_ranking=Q2_9,other_ranking_other=Q2_9_TEXT) %>%
  slice(3:n()) %>%
  mutate(dietary_health_ranking=as.numeric(dietary_health_ranking)) %>%
  mutate(dietary_sustainability_ranking=as.numeric(dietary_sustainability_ranking)) %>%
  mutate(institutional_sustainability_ranking=as.numeric(institutional_sustainability_ranking)) %>%
  mutate(food_pricing_ranking=as.numeric(food_pricing_ranking)) %>%
  mutate(operational_costs_ranking=as.numeric(operational_costs_ranking)) %>%
  mutate(guest_satisfaction_ranking=as.numeric(guest_satisfaction_ranking)) %>%
  mutate(worker_satisfaction_ranking=as.numeric(worker_satisfaction_ranking)) %>%
  mutate(campus_culture_ranking=as.numeric(campus_culture_ranking)) %>%
  mutate(other_ranking=as.numeric(other_ranking)) %>%
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
stakeholder_survey
```

    ##                   data completion channel               consent
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
    ##                     role_type                role_type_other
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
    ## 22            16                      7                              6
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
    ## 22                                    5                    4
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
    ## 1                          4                          1
    ## 2                          7                          4
    ## 3                          5                          6
    ## 4                          3                          1
    ## 5                          3                          4
    ## 6                          1                          4
    ## 7                          8                          4
    ## 8                          3                          2
    ## 9                          4                          1
    ## 10                         3                          5
    ## 11                         2                          1
    ## 12                         7                          5
    ## 13                         2                          4
    ## 14                         7                          2
    ## 15                         2                          3
    ## 16                         3                          1
    ## 17                         7                          4
    ## 18                         5                          2
    ## 19                         5                          2
    ## 20                         7                          1
    ## 21                         6                          4
    ## 22                         3                          1
    ## 23                         4                          3
    ## 24                         3                          8
    ## 25                         4                          1
    ## 26                         7                          4
    ## 27                         1                          3
    ## 28                         3                          2
    ## 29                         1                          2
    ## 30                         5                          1
    ## 31                         3                          4
    ## 32                         1                          4
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
    ## 22                           8                      9             2
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
    ##    other_ranking_other dietary_health_score dietary_sustainability_score
    ## 1                                         8                            7
    ## 2                                         8                            7
    ## 3                                         7                            6
    ## 4                                         8                            5
    ## 5                                         2                            5
    ## 6                                         2                            7
    ## 7                                         9                            8
    ## 8                                         4                            5
    ## 9                                         8                            5
    ## 10                                        6                            8
    ## 11                                        3                            7
    ## 12                                        9                            4
    ## 13                                        4                            3
    ## 14                                        9                            2
    ## 15                                        6                            5
    ## 16                                        4                            6
    ## 17                                        7                            4
    ## 18                                        6                            2
    ## 19                                        9                            6
    ## 20                                        5                            4
    ## 21                                        5                            8
    ## 22        Cuisine type                    3                            4
    ## 23                                        9                            8
    ## 24                                        4                            8
    ## 25                                        8                            5
    ## 26                                        7                            5
    ## 27                                        4                            5
    ## 28                                        5                            2
    ## 29                                        7                            5
    ## 30                                        8                            6
    ## 31                                        5                            4
    ## 32                                        7                            4
    ##    institutional_sustainability_score food_pricing_score
    ## 1                                   3                  5
    ## 2                                   9                  2
    ## 3                                   2                  8
    ## 4                                   4                  6
    ## 5                                   3                  9
    ## 6                                   4                  8
    ## 7                                   7                  5
    ## 8                                   6                  2
    ## 9                                   3                  2
    ## 10                                  9                  3
    ## 11                                  2                  5
    ## 12                                  2                  6
    ## 13                                  5                  7
    ## 14                                  5                  4
    ## 15                                  9                  4
    ## 16                                  2                  8
    ## 17                                  2                  9
    ## 18                                  3                  7
    ## 19                                  2                  4
    ## 20                                  6                  2
    ## 21                                  7                  3
    ## 22                                  5                  6
    ## 23                                  4                  2
    ## 24                                  9                  5
    ## 25                                  4                  7
    ## 26                                  9                  4
    ## 27                                  6                  8
    ## 28                                  4                  9
    ## 29                                  6                  3
    ## 30                                  7                  4
    ## 31                                  3                  2
    ## 32                                  2                  8
    ##    operational_costs_score guest_satisfaction_score worker_satisfaction_score
    ## 1                        6                        9                         4
    ## 2                        3                        6                         5
    ## 3                        5                        4                         3
    ## 4                        7                        9                         3
    ## 5                        7                        6                         8
    ## 6                        9                        6                         3
    ## 7                        2                        6                         4
    ## 8                        7                        8                         9
    ## 9                        6                        9                         4
    ## 10                       7                        5                         4
    ## 11                       8                        9                         4
    ## 12                       3                        5                         8
    ## 13                       8                        6                         2
    ## 14                       3                        8                         7
    ## 15                       8                        7                         2
    ## 16                       7                        9                         3
    ## 17                       3                        6                         8
    ## 18                       5                        8                         4
    ## 19                       5                        8                         7
    ## 20                       3                        9                         7
    ## 21                       4                        6                         9
    ## 22                       7                        9                         2
    ## 23                       6                        7                         3
    ## 24                       7                        2                         6
    ## 25                       6                        9                         2
    ## 26                       3                        6                         2
    ## 27                       9                        7                         2
    ## 28                       7                        8                         3
    ## 29                       9                        8                         2
    ## 30                       5                        9                         3
    ## 31                       7                        6                         8
    ## 32                       9                        6                         5
    ##    campus_culture_score other_score
    ## 1                     2           1
    ## 2                     4           1
    ## 3                     9           1
    ## 4                     2           1
    ## 5                     4           1
    ## 6                     5           1
    ## 7                     3           1
    ## 8                     3           1
    ## 9                     7           1
    ## 10                    2           1
    ## 11                    6           1
    ## 12                    7           1
    ## 13                    9           1
    ## 14                    6           1
    ## 15                    3           1
    ## 16                    5           1
    ## 17                    5           1
    ## 18                    9           1
    ## 19                    3           1
    ## 20                    8           1
    ## 21                    2           1
    ## 22                    1           8
    ## 23                    5           1
    ## 24                    3           1
    ## 25                    3           1
    ## 26                    8           1
    ## 27                    3           1
    ## 28                    6           1
    ## 29                    4           1
    ## 30                    2           1
    ## 31                    9           1
    ## 32                    3           1

``` r
stakeholder_survey %>%
  group_by(role_type) %>%
  summarise(dietary_health_score_mean=mean(dietary_health_score),dietary_sustainability_score_mean=mean(dietary_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),institutional_sustainability_score_mean=mean(institutional_sustainability_score),food_pricing_score_mean=mean(food_pricing_score),operational_costs_score_mean=mean(operational_costs_score),guest_satisfaction_score_mean=mean(guest_satisfaction_score),worker_satisfaction_score_mean=mean(worker_satisfaction_score),campus_culture_score_mean=mean(campus_culture_score),other_score_mean=mean(other_score),n=n())
```

    ## # A tibble: 6 × 11
    ##   role_type dietary_health_score…¹ dietary_sustainabili…² institutional_sustai…³
    ##   <chr>                      <dbl>                  <dbl>                  <dbl>
    ## 1 Chef                        5                      5.25                   3.5 
    ## 2 Dining d…                   5.71                   5.71                   5.29
    ## 3 Nutritio…                   7.4                    5.6                    3.8 
    ## 4 Other (p…                   6.83                   5.67                   3.5 
    ## 5 Sustaina…                   5.5                    5.83                   8   
    ## 6 Universi…                   6.25                   3                      3.75
    ## # ℹ abbreviated names: ¹​dietary_health_score_mean,
    ## #   ²​dietary_sustainability_score_mean,
    ## #   ³​institutional_sustainability_score_mean
    ## # ℹ 7 more variables: food_pricing_score_mean <dbl>,
    ## #   operational_costs_score_mean <dbl>, guest_satisfaction_score_mean <dbl>,
    ## #   worker_satisfaction_score_mean <dbl>, campus_culture_score_mean <dbl>,
    ## #   other_score_mean <dbl>, n <int>

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
