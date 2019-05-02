best<-function(state, disease){
  ##reading data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  need<-as.data.frame(cbind(data[,2], #name
                            data[,7], #state
                            data[,11], #heart attack
                            data[,17], #heart failure
                            data[,23]), #pneumonia
                      stringsAsFactors=FALSE)
  colnames(need)<-c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ##checking the validity of input
  if(!state %in% need[, "state"])
    stop('invalid state')
  else if(!disease %in% c("heart attack", "heart failure", "pneumonia"))
    stop('invalid disease')
  else{
    selectstate<-which(need[, "state"]==state)
    aboutselected<-need[selectstate, ]
    diseasedata<-as.numeric(aboutselected[, eval(disease)])
    minvalue<-min(diseasedata, na.rm=TRUE)
    result<-aboutselected[, "hospital"][which(diseasedata==minvalue)]
  }
  return(result)
}