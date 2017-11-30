best <- function(state, outcome) {
    ## Read outcome data
    data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(data.outcome[ , 11] <- as.numeric(data.outcome[ , 11]))
    suppressWarnings(data.outcome[ , 17] <- as.numeric(data.outcome[ , 17]))
    suppressWarnings(data.outcome[ , 23] <- as.numeric(data.outcome[ , 23]))
    ## Check that state and outcome are valid
    outcome.vec <- c("heart attack", "heart failure", "pneumonia")
    state.vec <- names(table(data.outcome[ , 7]))
    outcome.true <- outcome %in% outcome.vec
    state.true <- state %in% state.vec
    if (state.true == FALSE){
        stop("invalid state")
    } 
    if (outcome.true == FALSE){
        stop("invalid outcome")
    } 
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data.split <- split(data.outcome, data.outcome$State)
    data.state <- data.split[[state]]
    if (outcome == "heart attack"){
        data.final <- data.frame(data.state[11], data.state[2])
    } else if (outcome == "heart failure"){
        data.final <- data.frame(data.state[17], data.state[2])
    } else if (outcome == "pneumonia"){
        data.final <- data.frame(data.state[23], data.state[2])
    }
    names(data.final)<- c("o", "n")
    data.ord <- data.final[with(data.final, order(o, n)), ]
    data.ord[1, 2]
}