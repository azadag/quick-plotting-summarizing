
#   __________________ #< f0241a9886bba1486865e78887d3331b ># __________________
#   Function Runs                                                           ####
saf <- function(x) summary(as.factor(x))
af <-  function(x) as.factor(x)

##  ............................................................................
##  Other Functions                                                         ####
roles <- function(x) sub("[^_]*_","",x )
roles2 <- function(x) gsub("[^_]*_","",x )

moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}


multipleSelect.Xtab <- function(Questions, comparison, rnm = NULL, byCase = T, both = F){
  
  if(both == T){
    
    List <- list()
    for(i in 1:length(comparison)){
      Row <- sapply(Questions, table, comparison[,i])
      List[[i]] <- Row
    }
    
    for(i in 2:length(List)){
      List[[1]] <- cbind(List[[1]], List[[i]])
    }
    colnames(List[[1]]) <- colnames(comparison)
  }else{    
    List <- lapply(Questions,table,comparison)
    for(i in 2:length(List)){
      List[[1]]<-rbind(List[[1]],List[[i]])
    }
  }
  
  Table <- List[[1]]
  
  if(is.null(rnm)==F){
    rownames(Table) <- rnm
  }else rownames(Table) <- names(Questions)
  
  if(byCase==T){
    Table <- sweep(x = Table, MARGIN = 2, STATS = summary(as.factor(comparison)),FUN = "/")*100
  }
  
  return(Table)
}


insertRow2 <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)  
}

# insertRow <- function(existingDF, newrow, r) {
#   existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
#   existingDF[r,] <- newrow
#   existingDF
# }




copyEnv <- function(from, to, names = ls( from, all.names = TRUE)) {
  mapply( assign, names, mget( names, from), list(to),
          SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(NULL)
}

pretty_cuts <- function(cut_str) {
  
  # so we know when to not do something
  
  first_val <- as.numeric(str_extract_all(cut_str[1], "[[:digit:]\\.]+")[[1]][1])
  last_val <- as.numeric(str_extract_all(cut_str[length(cut_str)], "[[:digit:]\\.]+")[[1]][2])
  
  sapply(seq_along(cut_str), function(i) {
    
    # get cut range
    
    x <- str_extract_all(cut_str[i], "[[:digit:]\\.]+")[[1]]
    
    # see if a double vs an int & get # of places if decimal so
    # we know how much to inc/dec
    
    inc_dec <- 1
    if (str_detect(x[1], "\\.")) {
      x <- as.numeric(x)
      inc_dec <- 10^(-match(TRUE, round(x[1], 1:20) == x[1]))
    } else {
      x <- as.numeric(x)
    }
    
    # if not the edge cases inc & dec
    
    if (x[1] != first_val) { x[1] <- x[1] + inc_dec }
    if (x[2] != last_val)  { x[2] <- x[2] - inc_dec }
    
    sprintf("%s - %s", as.character(x[1]), as.character(x[2]))
    
  })
  
}

nice.cuts2 <- function(variable, cuts = 10, thousands.separator = FALSE) {
  
  # Load required packages (useful when used independently of context)
  Vectorize(require)(package = c("gsubfn", "scales"),
                     character.only = TRUE)
  
  # Destring this variable
  destring <- function(x) {
    ## convert factor to strings
    if (is.character(x)) {
      as.numeric(x)
    } else if (is.factor(x)) {
      as.numeric(levels(x))[x]
    } else if (is.numeric(x)) {
      x
    } else {
      stop("could not convert to numeric")
    }
  }
  
  # Apply function
  # variable <- destring(variable)
  
  # Check whether to disable scientific notation
  ifelse (mean(variable) > 100000, 
    options(scipen = 999),
    options(scipen = 0)
  )
  
  # Create pretty breaks
  cut_breaks <- pretty_breaks(n = cuts)(variable)
  
  # Round it two decimal places
  variable <- round(variable, digits = 2)
  
  # Develop cuts according to the provided object
  cuts_variable <- Hmisc::cut2(x = variable, cuts = cut_breaks)
  
  cuts_labels <- levels(cuts_variable)
  
  # Check if variable is total or with decimals
  if (all(cut_breaks %% 1 == 0)) {
    # Variable is integer
    cuts_labels <- gsubfn('\\[\\s*(\\d+),\\s*(\\d+)[^0-9]+',
                          ~paste0(x, '-',as.numeric(y)-1),
                          as.character(cuts_labels))
  } else {
    # Variable is not integer
    # Create clean cuts
    cuts_labels <- gsubfn('\\[\\s*([0-9]+\\.*[0-9]*),\\s*(\\d+\\.\\d+).*',
                          ~paste0(x, '-', as.numeric(y)- 0.01),
                          as.character(cuts_labels))
  }
  
  # Clean Inf
  cuts_labels <- gsub("Inf", max(variable), cuts_labels)
  
  # Clean punctuation
  cuts_labels <- sub("\\[(.*), (.*)\\]", "\\1 - \\2", cuts_labels)
  
  # Replace strings with spaces
  cuts_labels <- gsub("-"," - ",cuts_labels, fixed = TRUE)
  
  # Trim white spaces
  cuts_labels <- trimws(cuts_labels)
  
  
  if (thousands.separator == TRUE) {
    cuts_labels <- sapply(strsplit(cuts_labels, " - "),
                          function(x) paste(prettyNum(x,
                                                      big.mark = ",",
                                                      preserve.width = "none"),
                                            collapse = " - "))
  }
  
  levels(cuts_variable) <- cuts_labels
  cuts_variable
}