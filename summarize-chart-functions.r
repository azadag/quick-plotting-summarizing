# devtools::install_github("lorenzwalthert/strcode")
# devtools::install_github("WinVector/replyr")
library(replyr)
#library(strcode)
library(scales)
library(extrafont)
library(hrbrthemes)

import_roboto_condensed()
update_geom_font_defaults(family=font_rc_light)
# font_import()
# loadfonts(device="win")

## Colors randomly cycled through for plotting! Use your own color schemes!

Palette <- c(
"#EE484A",
"#3AC9BB",
"#FFA605",
"#6D9BEB",
"#05D6EE",
"#11456D")


##  ............................................................................
##  ReplyR Parameterization                                                 ####

## Basic summarisation ONE  Group (SumVarName) WITHOUT a weighting variable
PctCalc <- function(d, SumVarName) {
  wrapr::let(
    alias=list( SumVar=SumVarName
    ),
    expr={
      # add a filter on off if else
      
      d %>% filter(!is.na(SumVar)) %>% group_by(SumVar) %>% summarise( IntermediateVal = n()) %>% 
        mutate( pct = IntermediateVal / sum(IntermediateVal))
    })
}

## Basic Summarisation ONE Group (*Group1Name*) WITH weighting variable (*SumVarName*)

PctCalcOne <- function(d, Group1Name, SumVarName) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               SumVar=SumVarName
               ),
    expr={
      # add a filter on off if else
      
      d %>% filter(!is.na(Group1)) %>%  group_by( Group1 ) %>%
        summarize(IntermediateVal =  sum(SumVar)) %>% 
        mutate( pct = IntermediateVal / sum(IntermediateVal))
    })
}


## Summarisation TWO Groups (*Group1Name*, *Group2Name*) WITHOUT weighting variable
## Last summary rollup on *Group2Name*

PctCalcTwoN1 <- function(d, Group1Name, Group2Name) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               Group2=Group2Name
               # SumVar=SumVarName
    ),
    expr={
      # filter on off if else
      
      d %>% ungroup() %>% filter( !is.na( Group2 )) %>% group_by( Group1, Group2 ) %>%
        summarize(IntermediateVal =  n() ) %>% group_by( Group2 ) %>% 
        mutate( pct = IntermediateVal / sum( IntermediateVal ))
    })
}


## Summarisation TWO Groups (*Group1Name*, *Group2Name*) WITHOUT weighting variable
## Last summary rollup on *Group1Name*

PctCalcTwoN2 <- function(d, Group1Name, Group2Name) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               Group2=Group2Name
               # SumVar=SumVarName
    ),
    expr={
      # filter on off if else
      
      d %>% ungroup() %>% filter( !is.na( Group2 )) %>% group_by( Group1, Group2 ) %>%
        summarize(IntermediateVal =  n() ) %>% group_by( Group1 ) %>% 
        mutate( pct = IntermediateVal / sum( IntermediateVal ))
    })
}


## Summarisation TWO Groups (*Group1Name*, *Group2Name*) WITH weighting variable
## Last summary rollup on *Group1Name*

PctCalcTwo <- function(d, Group1Name, Group2Name, SumVarName) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               Group2=Group2Name,
               SumVar=SumVarName
    ),
    expr={
      # filter on off if else
      
      d %>% ungroup() %>% filter( !is.na( Group2 )) %>% group_by( Group1, Group2 ) %>%
        summarize(IntermediateVal =  sum( SumVar )) %>% group_by( Group1 ) %>% 
        mutate( pct = IntermediateVal / sum( IntermediateVal ))
    })
}

## Summarisation TWO Groups (*Group1Name*, *Group2Name*) WITH weighting variable
## Last summary rollup on *Group2Name*
## Same as PctCalcTwo, and just reverse the group input order


PctCalcTwo1 <- function(d, Group1Name, Group2Name, SumVarName) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               Group2=Group2Name,
               SumVar=SumVarName
    ),
    expr={
      # filter on off if else
      
      d %>% ungroup() %>% filter( !is.na( Group2 )) %>% group_by( Group1, Group2 ) %>%
        summarize(IntermediateVal =  sum( SumVar )) %>% group_by( Group2 ) %>% 
        mutate( pct = IntermediateVal / sum( IntermediateVal ))
    })
}

## Summarisation TWO Groups (*Group1Name*, *Group2Name*) WITH weighting variable
## Last summary rollup on NO GROUP


PctCalcTwoN <- function(d, Group1Name, Group2Name, SumVarName) {
  wrapr::let(
    alias=list(Group1=Group1Name,
               Group2=Group2Name,
               SumVar=SumVarName
    ),
    expr={
      # filter on off if else
      
      d %>% ungroup() %>% filter( !is.na( Group2 )) %>% group_by( Group1, Group2 ) %>%
        summarize(IntermediateVal =  sum( SumVar )) %>% group_by( ) %>% 
        mutate( pct = IntermediateVal / sum( IntermediateVal ))
    })
}

