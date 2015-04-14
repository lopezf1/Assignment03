setwd("~/Coursera/Assignment03")

# This function returns the best hosipital in a state for a given
# outcome.  

best <- function(state, outcome) {
    
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
    
    #  Return hospital name in the state with the lowest 30-day death rate.  First
    # ask about condition and subset data frame to just state and appropriate 
    # condition.

    if(outcome=="heart attack") {
        df <- df[which(df$State == state), c(2, 11)]
    } else if(outcome=="heart failure") {
        df <- df[which(df$State == state), c(2, 17)]
    } else {
        df <- df[which(df$State == state), c(2, 23)]
    }
    
    # Change condition column from character to numeric.  Suppress warnings
    # about coercing NAs.  Find min of condition column, removing NA values.
    # Sort outcome alphabetically if multiple min values.
    
    df[, 2] <- suppressWarnings(as.numeric(df[, 2]))
    sort(df[which(df[, 2] == min(df[, 2], na.rm=TRUE)), 1])
}

