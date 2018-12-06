
##  ............................................................................
##  Chart Functions                                                         ####

require(ggplot2)
library(replyr)
#library(strcode)
library(scales)
library(extrafont)
library(hrbrthemes)

import_roboto_condensed()
update_geom_font_defaults(family=font_rc_light)

import_roboto_condensed()
update_geom_font_defaults(family=font_rc_light)
# font_import()
# loadfonts(device="win")



##  .................. #< e58f961d80cbb4922bb2f5ff86a5c0f0 ># ..................
##  theme_ipsum_az                                                          ####

# Theme used to create journal ready quick figures!
#
# base_size - font size
# base_family - font for everything
# lines_lwd = lines width 
# plot_grid = grid?
# font_type type of font
# title_size = title font size
# legend_size = legend font size 
# bg_col = background colour
# title_font = title font
# base_col  = font color
# horz_grid = add horizontal grid?
# bord_size = width of a rectangular border
# alpha_leg = opacity of legend. (0 = totally transparent)
# strip_bg = colour background for facets
# grid_thick = A multiplier to apply to the grid lines. 0.8 would reduce thickness by 20%
# grid_type = Grid type. Default is a solid line
# ticks_xy = Do you want ticks on the x or y axis? "x" = x-axis only, "y" = y-axis only, "xy" = both axes.
# grid_cols = Colour of the grid. 2 element vector. First element is major grid colour. If only one element, the first will be used for minor grid.




text <- theme(plot.title=element_text(size=rel(1), face="bold.italic"),
              axis.text.x = element_text(angle=60, hjust=1))


### . . . . . . . . .. #< 03031b4fb620f98ee0711059ec4ff9e9 ># . . . . . . . . ..
### Roboto Themed                                                           ####
#' A specific [ggplot2] default theme with opinionated defaults built on roboto theme from hbrmstr
#'
#' You should [import_roboto_condensed]() first and also install the fonts on your
#' system before trying to use this theme.
#'
#' @md
#' @section 
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults(family=font_rc_light)
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_ipsum_rc(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }


theme_ipsum_az <- function(base_family="Roboto Condensed", base_size = 18,
                           plot_title_family=base_family, plot_title_size = 16, #24
                           plot_title_face="bold", plot_title_margin = 6,
                           subtitle_family="Roboto Condensed", subtitle_size = 14, #20
                           subtitle_face = "plain", subtitle_margin = 10,
                           strip_text_family = base_family, strip_text_size = 18,
                           strip_text_face = "plain",
                           caption_family = "Roboto Condensed", caption_size = 14,
                           caption_face = "plain", caption_margin = 6,
                           axis_title_family = base_family, axis_title_size = 14, #19
                           axis_title_face = "plain", axis_title_just = "rt",
                           plot_margin = margin(10, 10, 10, 10),
                           grid = FALSE, axis = FALSE, ticks = TRUE, relsize_x = 1.4, relsize_y = 1.4  ) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid=element_line(color="#2b2b2bdd", size=0.10))
    ret <- ret + theme(panel.grid.major=element_line(color="#2b2b2b99", size=0.10))
    ret <- ret + theme(panel.grid.minor=element_line(color="#2b2b2b99", size=0.05))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(margin=margin(t=0), size = rel( relsize_x )))
  ret <- ret + theme(axis.text.y=element_text(margin=margin(r=0), size = rel( relsize_y )))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing.x=grid::unit(2, "lines"))
  ret <- ret + theme(panel.spacing.y=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  ret
  
}


##  ............................................................................
##  Bar Chart 1                                                             ####
plot_bar <- function(data, xvar, yvar, title = "", subtitle = "", 
                     x.cap = "", y.cap = "Percent", caption = "", 
                     vjust = -.5, position_dodge =.8, expand = c(0, 0), colr = sample(1 : 6, 1), limits = c(0, .5),
                     relsizex = 1.1, relsizey = 1.1,  sizerel = rel(5), base_size = 18 ) {
  
  data_gd <- NULL
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = as.factor( xvar ), y = yvar ),
              environment = environment() ) +
    geom_col( fill = Palette[colr] ) + 
    geom_text(aes(label=scales::percent(round(yvar,2))), 
              nudge_x = -.03, nudge_y = .02, size = sizerel) +
    theme_ipsum_az( base_size = base_size, relsize_x = relsizex, relsize_y = relsizey ) + # grid = "Y",
    scale_y_percent( expand = expand, limits = limits ) +
    coord_cartesian( clip = "off") +
    labs(title    =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption = caption) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  
  p
}

plot_bar2 <- function(data, xvar, yvar, title = "", subtitle = "", 
                     x.cap = "", y.cap = "Per cent", caption = "", 
                     vjust = -.5, position_dodge =.8, expand = c(0, 0), colr = sample(1 : 6, 1),
                     relsizex = .9, relsizey = .9,  sizerel = rel(5), base_size = 8, limits = c(0, .5),
                     grid_type = "X", order_xy = 1, nudgey = .005, nudgex = -.15, h_just_axis = 1, h_just_text = 0, v_just_axis = 0   
                     ) {
  
  data_gd <- NULL
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = as.factor( xvar ), y = yvar ),
              environment = environment() ) +
    geom_col( fill = Palette[colr] ) + 
    geom_text(aes(label=scales::percent(round(yvar,2))), 
              hjust= h_just_text, nudge_y = nudgey, nudge_x = nudgex, size = sizerel, vjust = vjust) +
    theme_ipsum_az(grid = grid_type, relsize_x = relsizex, relsize_y = relsizey ) +
    scale_y_percent( expand = expand, limits = limits ) +
    labs(title    =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption = caption) +
    theme(axis.text.x = element_text(angle=60, hjust= h_just_axis )) +
      coord_flip()
  
  p
}

plot_barF <- function(data, xvar, yvar, facet, title = "", subtitle = "", 
                     x.cap = "", y.cap = "Percent", caption = "", 
                     vjust = -.5, position_dodge =.8, expand = c(0, .02), colr = sample(1 : 6, 1), limits = c(0, .5),
                     relsizex = .8, relsizey = .8, base_size = 8 ) {
  
  data_gd <- NULL
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )
  data_gd$facet <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(facet), data = data),
    error = function(e) eval(envir = data, expr = parse(text=facet))
  )
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = as.factor( xvar ), y = yvar ),
              environment = environment() ) +
    geom_col( fill = Palette[colr] ) + 
    # geom_text(aes(label=scales::percent(round(yvar,2))), 
              # nudge_x = -.03, nudge_y = .02, size = rel(6.5)) +
    theme_ipsum_az(base_size = base_size, grid = "Y", plot_title_size = 12, subtitle_size = 10 ,relsize_x = relsizex, relsize_y = relsizey,
                   plot_margin = margin(2, 2, 2, 2), strip_text_size = 12) + 
    scale_y_percent( expand = expand, limits = limits ) +
    labs(title    =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption = caption) +
    theme(axis.text.x = element_text(angle=60, hjust=1)) + facet_wrap(c("facet"), nrow = 2)
  
  p
}


plot_bar_reorder <- function(data, xvar, yvar, title = "", subtitle = "", 
                             x.cap = "", y.cap = "Percent", caption = "", 
                             relsizex = 1.1, relsizey = 1.1,  sizerel = rel(5), base_size = 8, limits = c(0, .5),
                             grid_type = "Y", order_xy = 1, nudgey = .003, nudgex = -.17, h_just_axis = 1, h_just_text = 0, v_just_axis = 0,
                             vjust = -.5, position_dodge =.8, expand = c(0, 0), nudge_y = .02, colr = sample(1 : 6, 1)
                             ) {
  
  data_gd <- NULL
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  
  p <- ggplot(data = as.data.frame(data_gd), 
              aes(x = reorder(as.factor(xvar), -yvar) 
                  , y = yvar),
              environment = environment() ) +
    geom_col( fill = Palette[colr] ) + 
    geom_text(aes(label=scales::percent(round(yvar,2))), 
              hjust= h_just_text, nudge_y = nudgey, nudge_x = nudgex, size = sizerel, vjust = vjust) +
    theme_ipsum_az(relsize_x = relsizex, relsize_y = relsizey ) + # grid = grid_type, 
    scale_y_percent( expand = expand, limits = limits) +
    labs(title    =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption = caption) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  
  p
}


plot_bar_reorder2 <- function(data, xvar, yvar, title = "", subtitle = "", 
                     x.cap = "", y.cap = "Percent", caption = "", 
                     vjust = 0, position_dodge =.8, expand = c(0, 0),  colr = sample(1 : 6, 1),
                     relsizex = .9, relsizey = 1.2,  sizerel = rel(5), base_size = 8, limits = c(0, .5),
                     grid_type = "X", order_xy = -1, nudgey = .005, nudgex = -.25, h_just_axis = 1, h_just_text = 0, v_just_axis = 0   ){
  
  data_gd <- NULL
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))

  p <- ggplot(data = as.data.frame(data_gd), 
              aes(x = reorder(as.factor(xvar), order_xy * -yvar) 
                  , y = as.numeric(yvar)),
              environment = environment() ) +
    geom_col( fill = Palette[colr] ) + 
    geom_text(aes(label=scales::percent(round(yvar,2))), 
              hjust= h_just_text, nudge_y = nudgey, nudge_x = nudgex, size = sizerel, vjust = vjust) +
    theme_ipsum_az( relsize_x = relsizex, relsize_y = relsizey ) +  # grid = grid_type, 
    scale_y_percent( expand = expand, limits = limits ) +
    # scale_x_percent( limits =  c(.02,0)) +
    labs(title    =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption = caption) +
    theme(axis.text.x = element_text(angle=60, hjust= h_just_axis )) + # , # axis.text.y  = element_text(hjust = v_just_axis))
    coord_flip()
  p 
}
# %>%  PctCalcOne('', 'weight_r'),
# q_dfQ8_5m %>% plot_bar(xvar = Q8.5b, yvar = pct)


##  ............................................................................
##  Bar Chart 2 - Dodged/Filled                                             ####
plot_bar_fill_dodge <- function(data, fillvar, xvar, yvar, title = "", subtitle = "", 
                                x.cap = "", y.cap = "Percent", caption = "", 
                                vjust = -.5, position_dodge =.8, expand = c(0, .00), limits = c(0, .5),
                                relsizex = 1.1, relsizey = 1.1, base_size = 18, sizerel = rel(5),
                                leg_pos = "top", text_TRUE = TRUE, grid_type = "Y", order_xy = -1) {
  data_gd <- NULL
  g_text <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), vjust = -.5, position = position_dodge( width = .80), size = sizerel )"
  # g_scales <- "scale_y_percent( expand = expand, limits = limits) +"
  
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  data_gd$fillvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(fillvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=fillvar))
  )  
  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = xvar, 
                            y = yvar, fill = fillvar),
              environment = environment() ) +
    
    geom_col(position = "dodge") + 
    theme_ipsum_az( base_size = base_size, relsize_x = relsizex, relsize_y = relsizey ) + # grid = grid_type, 
    # scale_y_percent() +
    scale_y_percent( expand = expand, limits = limits) +
    labs(title =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption= caption) +
    theme(axis.text.x = element_text(angle=60, hjust= 1)) + 
    theme(legend.position = leg_pos) +labs(fill = "")
  
  if( text_TRUE == TRUE) p <- eval(parse(text = paste("p + " , g_text, sep = "")))
  p
}        

plot_bar_fill_dodge2 <- function(data, fillvar, xvar, yvar, title = "", subtitle = "", 
                                x.cap = "", y.cap = "Percent", caption = "", 
                                vjust = -.5, position_dodge =.8, expand = c(0, .00), limits = c(0, .5),
                                relsizex = 1.1, relsizey = 1.1, base_size = 18, sizerel = rel(5),
                                leg_pos = "top", text_TRUE = TRUE, grid_type = "Y", order_xy = 1, flip = FALSE) {
  data_gd <- NULL
  # g_text <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), vjust = -.5, position = position_dodge( width = .80), size = sizerel )"
  g_text <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), vjust = -.5, position = position_dodge( width = .80), size = sizerel , check_overlap = TRUE)"
  g_texth <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), hjust = 0, position = position_dodge( width = 1), size = sizerel , check_overlap = TRUE)"
  
  # g_scales <- "scale_y_percent( expand = expand, limits = limits) +"
  
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  data_gd$fillvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(fillvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=fillvar))
  )  
  
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  if(flip == TRUE) order_xy <- -1 
  if(flip == TRUE) grid_type <- "Y" 
  
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = as.factor(xvar), #reorder(as.factor(xvar), order_xy * -as.numeric(yvar)) 
                            y = as.numeric(yvar), fill = fillvar),
              environment = environment() ) +
    
    geom_col(position = "dodge") + 
    theme_ipsum_az( base_size = base_size, relsize_x = relsizex, relsize_y = relsizey ) + # grid = grid_type, 
    # scale_y_percent() +
    scale_y_percent( expand = expand, limits = limits) +
    labs(title =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption= caption) +
    theme(axis.text.x = element_text(angle=60, hjust= 1)) + 
    theme(legend.position = leg_pos) +labs(fill = "")
  
  if( text_TRUE == TRUE & flip == FALSE) p <- eval(parse(text = paste("p + " , g_text, sep = "")))
  if( flip == TRUE & text_TRUE == FALSE) p <- eval(parse(text = paste("p + coord_flip()", sep = "")))
  if( flip == TRUE & text_TRUE == TRUE) p <- eval(parse(text = paste("p + ", g_texth , "+ coord_flip()", sep = "")))
  p
}  

plot_bar_fill_dodgeReorder <- function(data, fillvar, xvar, yvar, title = "", subtitle = "", 
                                x.cap = "", y.cap = "Percent", caption = "", 
                                vjust = -.5, position_dodge =.8, expand = c(0, 0), sizerel= rel(5),
                                relsizex = 1.1, relsizey = 1, base_size = 18, limits = c(0, .5),
                                grid_type = "Y", order_xy = 1, h_just_axis = 1,
                                leg_pos = "top", text_TRUE = TRUE, flip = FALSE) {
  data_gd <- NULL
  
  data_gd$xvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(xvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=xvar))
  )
  data_gd$yvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(yvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=yvar))
  )  
  data_gd$fillvar <- tryCatch(
    expr = lazyeval::lazy_eval(substitute(fillvar), data = data),
    error = function(e) eval(envir = data, expr = parse(text=fillvar))
  )  
  # data_gd$n <- tryCatch(
    # expr = lazyeval::lazy_eval(substitute(n), data = data),
    # error = function(e) eval(envir = data, expr = parse(text=n))
  # )
  if(missing(limits))
    limits <- c(0, max(data_gd$yvar + .03))
  if(flip == TRUE) order_xy <- -1 
  if(flip == TRUE) grid_type <- "Y" 
  
  g_text <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), vjust = -.5, position = position_dodge( width = .80), size = sizerel , check_overlap = TRUE)"
  g_texth <- "geom_text(aes(label = scales::percent(round( yvar, 2 ))), hjust = 0, position = position_dodge( width = 1), size = sizerel , check_overlap = TRUE)"
  
  p <- ggplot(data = as.data.frame(data_gd), 
              mapping = aes(x = reorder(as.factor(xvar), order_xy * -as.numeric(yvar)) 
                                        , y = as.numeric(yvar), fill = fillvar),
              environment = environment() ) +
    
    geom_col(position = "dodge") + 
    theme_ipsum_az( base_size = base_size, relsize_x = relsizex, relsize_y = relsizey ) + #grid = grid_type,
    
    scale_y_percent( expand = expand, limits = limits ) +
    labs(title =  title,
         subtitle =  subtitle,
         x = x.cap, y = y.cap,
         caption= caption) +
    theme(axis.text.x = element_text(angle=60, hjust= h_just_axis)) + 
    theme(legend.position = "top") +labs(fill = "")
  
  if( text_TRUE == TRUE & flip == FALSE) p <- eval(parse(text = paste("p + " , g_text, sep = "")))
  if( flip == TRUE & text_TRUE == FALSE) p <- eval(parse(text = paste("p + coord_flip()", sep = "")))
  if( flip == TRUE & text_TRUE == TRUE) p <- eval(parse(text = paste("p + ", g_texth , "+ coord_flip()", sep = "")))
  p

  }



##  .................. #< 28ae9f36330192daa3afad917aa9af59 ># ..................
##  GGPlot2 Theme Extras                                                    ####

add_credits = function() {
  grid.text("",
            x = 0.99,
            y = 0.02,
            just = "right",
            gp = gpar(fontsize = 12, col = "#777777"))
}

