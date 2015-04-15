setwd("~/Coursera/Assignment03")

# This function ranks hospitals by outcome in states.  

rankhospital <- function(state, outcome, num="best") {
    
    # Read in the outcome data.  Coerce all columns to characater values.
    
    df <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    #  Check if invalid state passed to function.  Stop if yes.  Ask, what
    # are unique states?
    
    valid_state <- unique(df[,7])
    result <- is.element(state, valid_state)
    if(result==FALSE) {
        stop("invalid state")
    }
    
    # Check if invalid outcome passed to function.  Stop if yes.  
    
    result <- is.element(outcome, c("heart attack", "heart failure", "pneumonia"))
    if(result==FALSE) {
        stop("invalid outcome")
    }
    
    # Ask about condition and subset data frame to just state and appropriate 
    # condition.
    
    if(outcome=="heart attack") {
        df <- df[which(df$State == state), c(2, 11)]
    } else if(outcome=="heart failure") {
        df <- df[which(df$State == state), c(2, 17)]
    } else {
        df <- df[which(df$State == state), c(2, 23)]
    }
 
    # Change condition column from character to numeric.  Suppress warnings
    # about coercing NAs.  Removing NA values.  Sort outcome and then hospital 
    # value columns.
    
    df[, 2] <- suppressWarnings(as.numeric(df[, 2]))
    df <- df[order(df[, 2], df[, 1]), ]
    df <- na.omit(df)
    
    # If best, worst of ranking, subset data to select value from hospital
    # name column.
    
    if(num=="best") {
        df[which(df[, 2] == min(df[, 2])), 1]
    } else if(num=="worst") {
        df[which(df[, 2] == max(df[, 2])), 1]
    } else {
        df[num, 1]
    }
}

