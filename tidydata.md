tidy data
================
Yang Gao
9/30/2021

``` r
library(tidyverse) 
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(haven)
options(tibble.print_min = 5)
```

``` r
pulse_df = 
  haven :: read_sas("./datasets/public_pulse_data.sas7bdat") %>%
  janitor :: clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## 4 10026  72.7 male            20             6            18            16
    ## 5 10035  60.4 male             4             0             1             2
    ## # … with 1,082 more rows

lets try pivot

``` r
pivot_tidy=
  pulse_df %>% 
  pivot_longer (
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  ) %>% 
  mutate(
    visit = replace(visit, visit == "bl" , "00m"),
    visit=factor(visit)
  )
pivot_tidy
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <fct> <dbl>
    ## 1 10003  48.0 male  00m       7
    ## 2 10003  48.0 male  01m       1
    ## 3 10003  48.0 male  06m       2
    ## 4 10003  48.0 male  12m       0
    ## 5 10015  72.5 male  00m       6
    ## # … with 4,343 more rows

``` r
analysis_df=
  tibble(
    groups = c("treatment", "treatment" , "control", "control"),
    time = c("a", "b", "a","b"),
    group_mean = c(4,8,3,6)
  )

analysis_df %>% 
  pivot_wider(
    names_from = "time",
    values_from = "group_mean"
    
  )
```

    ## # A tibble: 2 × 3
    ##   groups        a     b
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment     4     8
    ## 2 control       3     6

``` r
knitr :: kable(analysis_df)
```

| groups    | time | group\_mean |
|:----------|:-----|------------:|
| treatment | a    |           4 |
| treatment | b    |           8 |
| control   | a    |           3 |
| control   | b    |           6 |

alwaysuse `pivot_wider`

## bind\_rows

import the LotR movie words stuff

``` r
fellowship_df =
  read_excel("./datasets/data_import_examples/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")
```

``` r
towers_df =
  read_excel("./datasets/data_import_examples/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")
```

``` r
return_df =
  read_excel("./datasets/data_import_examples/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_of_king")
```

``` r
lotr_df=
  bind_rows(fellowship_df,  towers_df, return_df ) %>% 
  janitor :: clean_names() %>% 
  pivot_longer(
    female:male,
    names_to= "sex",
    values_to = "words"
  ) %>% 
  relocate (movie)

lotr_df
```

    ## # A tibble: 18 × 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring Elf    female  1229
    ##  2 fellowship_ring Elf    male     971
    ##  3 fellowship_ring Hobbit female    14
    ##  4 fellowship_ring Hobbit male    3644
    ##  5 fellowship_ring Man    female     0
    ##  6 fellowship_ring Man    male    1995
    ##  7 two_towers      Elf    female   331
    ##  8 two_towers      Elf    male     513
    ##  9 two_towers      Hobbit female     0
    ## 10 two_towers      Hobbit male    2463
    ## 11 two_towers      Man    female   401
    ## 12 two_towers      Man    male    3589
    ## 13 return_of_king  Elf    female   183
    ## 14 return_of_king  Elf    male     510
    ## 15 return_of_king  Hobbit female     2
    ## 16 return_of_king  Hobbit male    2673
    ## 17 return_of_king  Man    female   268
    ## 18 return_of_king  Man    male    2459

never use `rbind`, use `bind_rows()`

## joins

FAS data

``` r
litters_df=
  read_csv("./datasets/data_import_examples/FAS_litters.csv") %>% 
  janitor :: clean_names() %>% 
  separate (group, into = c("dose", "day_of_tx"),3) %>% 
  relocate (litter_number) %>% 
  mutate(dose = str_to_lower(dose))
```

    ## Rows: 49 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df
```

    ## # A tibble: 49 × 9
    ##   litter_number dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##   <chr>         <chr> <chr>          <dbl>       <dbl>       <dbl>
    ## 1 #85           con   7               19.7        34.7          20
    ## 2 #1/2/95/2     con   7               27          42            19
    ## 3 #5/5/3/83/3-3 con   7               26          41.4          19
    ## 4 #5/4/2/95/2   con   7               28.5        44.1          19
    ## 5 #4/2/95/3-3   con   7               NA          NA            20
    ## # … with 44 more rows, and 3 more variables: pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>

``` r
pups_df=
  read_csv("./datasets/data_import_examples/FAS_pups.csv") %>% 
  janitor :: clean_names() %>% 
  mutate(sex = recode(sex, `1` = "male",`2` = "female" ))
```

    ## Rows: 313 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df
```

    ## # A tibble: 313 × 6
    ##   litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##   <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ## 1 #85           male        4      13        7      11
    ## 2 #85           male        4      13        7      12
    ## 3 #1/2/95/2     male        5      13        7       9
    ## 4 #1/2/95/2     male        5      13        8      10
    ## 5 #5/5/3/83/3-3 male        5      13        8      10
    ## # … with 308 more rows

join these up

``` r
fas_df=
  left_join (pups_df, litters_df, by = "litter_number") %>% 
  relocate(litter_number, dose, day_of_tx)
fas_df
```

    ## # A tibble: 313 × 14
    ##   litter_number dose  day_of_tx sex   pd_ears pd_eyes pd_pivot pd_walk
    ##   <chr>         <chr> <chr>     <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ## 1 #85           con   7         male        4      13        7      11
    ## 2 #85           con   7         male        4      13        7      12
    ## 3 #1/2/95/2     con   7         male        5      13        7       9
    ## 4 #1/2/95/2     con   7         male        5      13        8      10
    ## 5 #5/5/3/83/3-3 con   7         male        5      13        8      10
    ## # … with 308 more rows, and 6 more variables: gd0_weight <dbl>,
    ## #   gd18_weight <dbl>, gd_of_birth <dbl>, pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>
