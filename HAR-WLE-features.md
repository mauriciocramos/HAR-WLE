HAR WLE Feature recreation
================
Maurício Collaça
December 5, 2017

This document analyzes the "\#DIV/0!" issue found in many features (statistics) of HAR WLE dataset and aims to reproduce the feature creation process.

Data loading
------------

``` r
library(dplyr)
training <- read.csv("pml-training.csv", stringsAsFactors = FALSE)
testing <- read.csv("pml-testing.csv", stringsAsFactors = FALSE)
```

Training Variables containing "\#DIV/0!"
----------------------------------------

``` r
(DIV0Cols <- names(which(colSums(sapply(training, function(x) is.character(x) & x=="#DIV/0!")) > 0)))
```

    ##  [1] "kurtosis_roll_belt"      "kurtosis_picth_belt"    
    ##  [3] "kurtosis_yaw_belt"       "skewness_roll_belt"     
    ##  [5] "skewness_roll_belt.1"    "skewness_yaw_belt"      
    ##  [7] "max_yaw_belt"            "min_yaw_belt"           
    ##  [9] "amplitude_yaw_belt"      "kurtosis_roll_arm"      
    ## [11] "kurtosis_picth_arm"      "kurtosis_yaw_arm"       
    ## [13] "skewness_roll_arm"       "skewness_pitch_arm"     
    ## [15] "skewness_yaw_arm"        "kurtosis_roll_dumbbell" 
    ## [17] "kurtosis_picth_dumbbell" "kurtosis_yaw_dumbbell"  
    ## [19] "skewness_roll_dumbbell"  "skewness_pitch_dumbbell"
    ## [21] "skewness_yaw_dumbbell"   "max_yaw_dumbbell"       
    ## [23] "min_yaw_dumbbell"        "amplitude_yaw_dumbbell" 
    ## [25] "kurtosis_roll_forearm"   "kurtosis_picth_forearm" 
    ## [27] "kurtosis_yaw_forearm"    "skewness_roll_forearm"  
    ## [29] "skewness_pitch_forearm"  "skewness_yaw_forearm"   
    ## [31] "max_yaw_forearm"         "min_yaw_forearm"        
    ## [33] "amplitude_yaw_forearm"

### Selecting the first case

``` r
training %>% filter_all(any_vars(. == "#DIV/0!")) %>%
    select(1:7, one_of(DIV0Cols)) %>%
    head(1)
```

    ##    X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
    ## 1 24  carlitos           1323084232               996313 05/12/2011 11:23
    ##   new_window num_window kurtosis_roll_belt kurtosis_picth_belt
    ## 1        yes         12           5.587755             #DIV/0!
    ##   kurtosis_yaw_belt skewness_roll_belt skewness_roll_belt.1
    ## 1           #DIV/0!           2.713152              #DIV/0!
    ##   skewness_yaw_belt max_yaw_belt min_yaw_belt amplitude_yaw_belt
    ## 1           #DIV/0!          5.6          5.6             0.0000
    ##   kurtosis_roll_arm kurtosis_picth_arm kurtosis_yaw_arm skewness_roll_arm
    ## 1          -1.05825            #DIV/0!          #DIV/0!           0.13832
    ##   skewness_pitch_arm skewness_yaw_arm kurtosis_roll_dumbbell
    ## 1            #DIV/0!          #DIV/0!                -0.6209
    ##   kurtosis_picth_dumbbell kurtosis_yaw_dumbbell skewness_roll_dumbbell
    ## 1                 -0.6149               #DIV/0!                -0.0960
    ##   skewness_pitch_dumbbell skewness_yaw_dumbbell max_yaw_dumbbell
    ## 1                  0.1049               #DIV/0!             -0.6
    ##   min_yaw_dumbbell amplitude_yaw_dumbbell kurtosis_roll_forearm
    ## 1             -0.6                   0.00               -0.3680
    ##   kurtosis_picth_forearm kurtosis_yaw_forearm skewness_roll_forearm
    ## 1                -2.0402              #DIV/0!                0.2113
    ##   skewness_pitch_forearm skewness_yaw_forearm max_yaw_forearm
    ## 1                -0.2117              #DIV/0!            -0.4
    ##   min_yaw_forearm amplitude_yaw_forearm
    ## 1            -0.4                  0.00

### Selecting the first 10 new windows from the first case and features of pitch\_belt

``` r
training %>%
    filter(user_name=="carlitos" & new_window=="yes") %>%
    select(2:3, ends_with("_pitch_belt"), ends_with("_picth_belt")) %>% slice(1:10) %>% knitr::kable()
```

| user\_name |  raw\_timestamp\_part\_1|  min\_pitch\_belt|  amplitude\_pitch\_belt|  avg\_pitch\_belt|  stddev\_pitch\_belt|  var\_pitch\_belt| kurtosis\_picth\_belt |  max\_picth\_belt|
|:-----------|------------------------:|-----------------:|-----------------------:|-----------------:|--------------------:|-----------------:|:----------------------|-----------------:|
| carlitos   |               1323084232|                 3|                       0|               8.1|                  0.0|               0.0| \#DIV/0!              |                 3|
| carlitos   |               1323084233|                 3|                       0|               7.9|                  0.2|               0.0| \#DIV/0!              |                 3|
| carlitos   |               1323084234|                 3|                       0|               7.4|                  0.1|               0.0| \#DIV/0!              |                 3|
| carlitos   |               1323084238|                 3|                       1|               7.1|                  0.1|               0.0| -1.298590             |                 4|
| carlitos   |               1323084242|                 3|                       1|               7.7|                  0.2|               0.1| 2.949202              |                 4|
| carlitos   |               1323084248|                 3|                       1|               7.2|                  0.5|               0.2| 1.216445              |                 4|
| carlitos   |               1323084251|                 3|                       1|               7.3|                  0.4|               0.2| 5.791962              |                 4|
| carlitos   |               1323084253|                 3|                       1|               7.0|                  0.3|               0.1| 47.000000             |                 4|
| carlitos   |               1323084254|                 3|                       1|               7.8|                  0.3|               0.1| 8.025296              |                 4|
| carlitos   |               1323084255|                 3|                       1|               7.7|                  0.3|               0.1| -1.153964             |                 4|

### TODO: Trying to reproduce the first 10 new windows

``` r
#TODO: get the window logic in the documentation
training %>%
    filter(user_name=="carlitos") %>%
    group_by(user_name, raw_timestamp_part_1) %>%
    summarise(min_pitch_belt = min(pitch_belt), amplitude_pitch_belt = max(pitch_belt) - min(pitch_belt),
              avg_pitch_belt = mean(pitch_belt),stddev_pitch_belt = sd(pitch_belt), var_pitch_belt = var(pitch_belt),
              kurtosis_pitch_belt = e1071::kurtosis(pitch_belt), max_pitch_belt = max(pitch_belt)) %>% slice(1:10) %>% knitr::kable()
```

| user\_name |  raw\_timestamp\_part\_1|  min\_pitch\_belt|  amplitude\_pitch\_belt|  avg\_pitch\_belt|  stddev\_pitch\_belt|  var\_pitch\_belt|  kurtosis\_pitch\_belt|  max\_pitch\_belt|
|:-----------|------------------------:|-----------------:|-----------------------:|-----------------:|--------------------:|-----------------:|----------------------:|-----------------:|
| carlitos   |               1323084231|              8.07|                    0.00|          8.070000|            0.0000000|         0.0000000|                    NaN|              8.07|
| carlitos   |               1323084232|              8.05|                    0.16|          8.122381|            0.0523359|         0.0027390|             -1.4749388|              8.21|
| carlitos   |               1323084233|              7.56|                    0.63|          7.993214|            0.2005004|         0.0402004|             -0.6910812|              8.19|
| carlitos   |               1323084234|              7.25|                    0.29|          7.355833|            0.0860696|         0.0074080|             -1.2375893|              7.54|
| carlitos   |               1323084235|              7.28|                    0.26|          7.418333|            0.0886942|         0.0078667|             -1.5016365|              7.54|
| carlitos   |               1323084236|              7.13|                    0.31|          7.325862|            0.1115001|         0.0124323|             -1.5395413|              7.44|
| carlitos   |               1323084237|              7.03|                    0.79|          7.408823|            0.2804880|         0.0786735|             -1.6219202|              7.82|
| carlitos   |               1323084238|              6.96|                    0.42|          7.125789|            0.1407249|         0.0198035|             -1.1703812|              7.38|
| carlitos   |               1323084239|              6.78|                    0.75|          7.334800|            0.1912485|         0.0365760|              2.5976850|              7.53|
| carlitos   |               1323084240|              6.44|                    0.22|          6.539048|            0.0758884|         0.0057590|             -1.4962315|              6.66|
