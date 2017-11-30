rankhospital <- function(state, outcome, num = "best") {
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
    num.r <- nrow(data.state[11])
    rank.list <- list(rank = numeric(num.r))
    if (outcome == "heart attack"){
        data.final <- data.frame(data.state[2], data.state[11], rank.list)
    } else if (outcome == "heart failure"){
        data.final <- data.frame(data.state[2], data.state[17], rank.list)
    } else if (outcome == "pneumonia"){
        data.final <- data.frame(data.state[2], data.state[23], rank.list)
    }
    names(data.final)<- c("Hospital.Name", "Rate", "Rank")
    data.ord <- data.final[with(data.final, order(Rate, Hospital.Name)), ]
    for (i in 1:num.r){
        data.ord[i, 3] <- i
    }
    state.data <<- data.ord
    if (num == "best"){
        num = "1"
    }else if (num == "worst"){
        num = as.character(num.r)
    }
    num <- as.numeric(num)
    if (num == num.r){
        num = num.r - sum(is.na(state.data[, 2]))
    }
    state.data[num, 1]
}