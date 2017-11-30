rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(data.outcome[ , 11] <- as.numeric(data.outcome[ , 11]))
    suppressWarnings(data.outcome[ , 17] <- as.numeric(data.outcome[ , 17]))
    suppressWarnings(data.outcome[ , 23] <- as.numeric(data.outcome[ , 23]))
    ## Check that state and outcome are valid
    outcome.vec <- c("heart attack", "heart failure", "pneumonia")
    state.vec <- names(table(data.outcome[ , 7]))
    n.state <- length(state.vec)
    outcome.true <- outcome %in% outcome.vec
    if (outcome.true == FALSE){
        stop("invalid outcome")
    } 
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data.split <- split(data.outcome, data.outcome$State)
    state.all.hosp <- character(n.state)
    state.all.state <- character(n.state)
    for(i in 1:n.state){
        num.s <- num
        state <- state.vec[i]
        data.state <- data.split[[state]]
        num.r <- nrow(data.state[11])
        if (outcome == "heart attack"){
            data.final <- data.frame(data.state[2], data.state[11])
        } else if (outcome == "heart failure"){
            data.final <- data.frame(data.state[2], data.state[17])
        } else if (outcome == "pneumonia"){
            data.final <- data.frame(data.state[2], data.state[23])
        }
        names(data.final)<- c("Hospital.Name", "Rate")
        data.ord <- data.final[with(data.final, order(Rate, Hospital.Name)), ]
        if (num.s == "best"){
            num.s = "1"
        }else if (num.s == "worst"){
            num.s = as.character(num.r)
        }
        num.s <- as.numeric(num.s)
        if (num.s == num.r){
            num.s = num.r - sum(is.na(data.ord[, 2]))
        }
        state.all.hosp[i] <- as.character(data.ord[num.s, 1])
        state.all.state[i] <- as.character(state)
    }
    state.all <<- data.frame(hospital = state.all.hosp, state = state.all.state)
}