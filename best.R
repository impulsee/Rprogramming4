# R Programming Week 4: Simulation & Profiling
#Function used to determine best HOspital 30-day mortality for 3 outcomes: "heart attack","heart failure","pneumonia"
# Corresponding columns are 11,17,23

best<- function(state,outcome){
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
    outcomes[,3]<-as.numeric(outcomes[,3])
    
    #Determine hospitals with minium 30-day mortality
    outcomes<-outcomes[which(outcomes[,3] ==min(outcomes[,3], na.rm = TRUE)),]
    #Return first hospital in alphabetical order with minimum mortality
    head(outcomes[order(outcomes$Hospital.Name),][,1],1)
}