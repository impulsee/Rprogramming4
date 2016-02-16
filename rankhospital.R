# R Programming Week 4: Simulation & Profiling
# Ranks hospital in state by one of 3 possible outcomes. Num is an integer which indicates what number ranking 
# Hospital should be returned with two special character rankings best and worst.
# They correspond to best and worst hospital in state
rankhospital<- function(state,outcome,num = "best"){
    outcomes<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE )
    outcomes<-outcomes[,c(2,7,11,17,23)]
    possibleoutcomes <- c("heart attack","heart failure","pneumonia")
    #Evaluate is outcome possible
    outcomenum<-which(possibleoutcomes == outcome)
    if(is.integer(outcomenum) && length(outcomenum) == 0L){
        stop("invalid outcome")
    }
    #Evaluate is state possible
    stateexist<-as.factor(outcomes[,2]) %in% state
    if(!any(stateexist))
        stop("invalid state")
    #Extracting necessary outcome
    outcomes<-subset(outcomes,State ==state,select = c(1,2,outcomenum+2))
    outcomes<-outcomes[complete.cases(outcomes),]
    outcomes[,3]<-as.numeric(outcomes[,3])
    #Order hospitals by 30-day mortality then by name
    orderedoutcomes<-outcomes[order(outcomes[,3],outcomes[,1]),]
    #Return first hospital in alphabetical order with minimum mortality

    if (num == "best"){
        return(orderedoutcomes[1,1])
    }
    #Return worst hospital in alphabetical order by mortality
    if(num == "worst"){
        return(tail(orderedoutcomes$Hospital.Name,1))
    }
    # Returns Hospital by specific num 
    orderedoutcomes[as.numeric(num),1]
}