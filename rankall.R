# R Programming Week 4: Simulation & Profiling
# Ranks hospitals at all states by one of 3 possible outcomes. Num is an integer which indicates what number ranking 
# hospital should be returned with two special character rankings best and worst. 
# They correspond to best and worst hospital in state
rankall<- function(outcome,num = "best"){
    outcomes<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE )
    outcomes<-outcomes[,c(2,7,11,17,23)]
    # Evaluate is outcome possible
    possibleoutcomes <- c("heart attack","heart failure","pneumonia")
    outcomenum<-which(possibleoutcomes == outcome)
    if(is.integer(outcomenum) && length(outcomenum) == 0L){
        stop("invalid outcome")
    }
    # Extracting necessary outcome
    outcomes<-subset(outcomes,select = c(1,2,outcomenum+2))
    outcomes<-outcomes[complete.cases(outcomes),]
    outcomes[,3]<-as.numeric(outcomes[,3])
    # Order hospitals by 30-day mortality then by state then by name
    orderedoutcomes<-outcomes[order(outcomes[,2],outcomes[,3],outcomes[,1]),]
    # Split hospitals by state
    statesplit<-split(orderedoutcomes,orderedoutcomes$State)
    # apply function to each list (so it applies to each state)
    lapplied <- lapply(statesplit,function(x){x[eval(parse(text=for_lapply(num))),c(1,2)]})
    # Transform data to data.frame
    data.frame(hospital = sapply(lapplied, '[[', 1),state=names(lapplied) )
}

# Special function to create function to apply command
for_lapply<-function (num){
    if(num == "worst"){
        return ("nrow(x)")
    }
    if(num == "best")
    {
        return("1")
    }
    return ("as.numeric(num)")
}