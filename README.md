# Functions to Summarise and Plot Survey Data

These function were made to aid in quickly tabulating survey data percentages and cross-tab summaries for surveys. The functions were also made to be used with population
weights.

These summary functions can be combined and chained with the plotting variables 
to quickly create and reiterate good-looking plots for quick exploration and export.

These functions create bar charts of various kinds

* Ordered by magnitude or unordered
* horizontal or vertical
* faceted

Number data annotations are automatically added to charts and aligned without having
to manually fiddle with limits.

### Basic Bar Plot

```r
df  %>% PctCalcOne( 'Q1', 'weight_r') %>% 
  plot_bar(xvar = Q1 , yvar = pct, 
           title="How many times has Los Angeles hosted the Summer Olympics?",
           subtitle= paste("n: ", NROW(df %>% filter(!is.na(Q1)))),
           x = "", y ="Percent") 
```

### Basic Bar Plot Horizontal

```r
df %>% PctCalcOne('Q_Age_f', 'weight_r') %>% 
  plot_bar2(xvar = Q_Age_f , yvar = pct, relsizex = 1, relsizey = 1, 
            subtitle= paste("n: ", NROW(df %>% filter(!is.na(Q_Age_f)))),
            title = "Age - Reweighted Sample",
            x = "", y ="Percent") + 
            scale_x_discrete(labels =  c("18 - 34", "35 -54", "65 +")) 
````

### Bar Plot With Order Horizontal

```r
df %>% PctCalcOne('Eth', 'weight_r') %>% 
  plot_bar_reorder2(xvar = Eth, yvar = pct, relsizex = 1, relsizey = 1, 
                    title = "Ethnicity - Reweighted Sample",
                    subtitle= paste("n: ", NROW(df %>% filter(!is.na(Eth)))),
                    x = "", y ="Percent")
```


### Cross-Tabbed Summary / Faceted Bar Plot

```r
df %>% PctCalcTwo('Q_LaCountyReside', 'Q4', 'weight_r') %>% 
  plot_bar_fill_dodge(xvar = as.factor( Q4 ) , yvar = pct, fillvar = as.factor(Q_LaCountyReside),  
                      title="How supportive or opposed are you of Los Angeles hosting the Summer Olympic Games in 2028?",
                      subtitle= paste("n: ", NROW(df %>% filter(!is.na(Q4) ))),
                      x = "", y ="Percent") + scale_fill_discrete(labels = label_la )  +                        scale_x_discrete(labels = label_oppose_support )
```

### Cross-Tabbed Summary / Faceted Bar Plot Flipped

```r
df %>% PctCalcTwo('Q_LaCountyReside', 'Q_Budget', 'weight_r') %>% 
  plot_bar_fill_dodge2(xvar = as.factor( Q_Budget ) , yvar = pct, fillvar = as.factor(Q_LaCountyReside),
    title="What is the official budget for the 2028 Summer Olympics?", flip = TRUE,
    subtitle= paste("n: ", NROW(df %>% filter(!is.na(Q_Budget) ))),
    x = "", y ="Percent")  + 
    scale_x_discrete(labels = pos_Q_Budget ) + 
    scale_fill_discrete(labels = pos_la ) 
```

### Cross-Tabbed Summary / Faceted Bar Plot Re-Ordered

```r
df %>% filter( ) %>% PctCalcTwo('Q_LaCountyReside', 'Q_Num_Host', 'weight_r') %>% 
  plot_bar_fill_dodgeReorder(xvar = Q_Num_Host , yvar = pct, fillvar =  as.factor(Q_LaCountyReside),
    title="How many times has Los Angeles Hosted the Summer Olympics?",
    subtitle= paste("n: ", NROW(df %>% filter(!is.na(Q_CityWithdraw)))),
    x = "", y ="Percent") + 
    scale_fill_discrete( labels = pos_la)

```
