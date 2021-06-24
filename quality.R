#Set Path for Folder cotaining Data
PATH <-"rprog_data_ProgAssignment3-data"



#Specifying the different path for files of data 
hospital_file <- "hospital-data.csv"
outcome_file <- "outcome-of-care-measures.csv"



#Setting the path for Hospital Data
hospital_dir <- paste(getwd(), "/",PATH,"/", hospital_file, sep="")
hospital <- read.csv(hospital_dir)



#Setting the Path for outcome data
outcome_dir <-paste(getwd(), "/",PATH,"/", outcome_file, sep="")

outcome.data <- read.csv(outcome_dir, colClasses = "character",)




#Creating a function That finds The best state in terms of least mortality way
#outcomes can be --> heart attack, heart failure, or pneumonia
best <-function(state, outcome){
  
  
  #Simplifying the data to a Samller dataframe: Only using the information we need
  rates <- cbind(outcome.data[2],outcome.data[7], outcome.data[11], outcome.data[17], outcome.data[23] )
  
  
  
  #Renaming the Columns to simplify searching
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  
  
  #Invalid input cases
  if(!state %in% rates[,"State"]){print("Invalid State")}
  if(!outcome %in% colnames(rates)){print("Invalid outcome")}
  
  
  #Getting data according to State and removing NA cases
  nrates <- rates[which(rates$State == state),]
  nrates <- nrates[which(!nrates[,outcome] == "Not Available"),]
  
  #transform the rates into numeric type to compare them
  nrates[,outcome] <- as.numeric(nrates[, outcome])
  
  
  #Order the rates from lowest to Highest
  rank <- nrates[order(nrates[,outcome]),]
  
  
  #Get the Hospital name with the least mortality Rates
  rank <- nrates[nrates[,outcome] == min(nrates[,outcome]),1]
  
  #chose according to aphabetical order if tie
  sort(rank)[1]
  }

#Similar to Best Function but we retrieve it according to the num or rank we want it to be
# sum can be: best, worst or a int
rankhospital <- function(state, outcome, num = "best") {
  
  #assigning the data we need to a new dataframe
  rates <- cbind(outcome.data[2],outcome.data[7], outcome.data[11], outcome.data[17], outcome.data[23] )
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  #Invalid input cases
  if(!state %in% rates[,"State"]){print("Invalid State")}
  if(!outcome %in% colnames(rates)){print("Invalid outcome")}
  
  nrates <- rates[which(rates$State == state),]
  nrates <- nrates[which(!nrates[,outcome] == "Not Available"),]
  nrates[,outcome] <- as.numeric(nrates[, outcome])
  
  
  
  rank <- nrates[order(nrates[, outcome], nrates[, "Hospital"]), ]
  if(num == "best"){
    rank <- rank[rank[,outcome] == min(rank[,outcome]),1]
    sort(rank)[1]
  }
  else if(num == "worst"){
    rank <- rank[rank[,outcome] == max(rank[,outcome]),1]
    rank
    }
  else{
    rank[num,1]
    
      
    }  
  }

rankall <- function(outcome, num="best"){
  rnum <- num
  out <-outcome
  
  rates <- cbind(outcome.data[2],outcome.data[7], outcome.data[11], outcome.data[17], outcome.data[23] )
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  if(!outcome %in% colnames(rates)){print("Invalid outcome")}
  
  nrates <- rates[which(!rates[,outcome] == "Not Available"),]
  nrates[,outcome] <- as.numeric(nrates[, outcome])
  
  for(state in sort(unique(nrates$State))){
    if(!outcome %in% colnames(rates)){print("Invalid outcome")}
      nrates <- rates[which(rates$State == state),]
      nrates <- nrates[which(!nrates[,outcome] == "Not Available"),]
      nrates[,outcome] <- as.numeric(nrates[, outcome])
      if(num == "best"){rnum <- 1}
      if(num == "worst"){rnum <- nrow(nrates)}
      else{rnum <- num}
      
      nrates<- nrates[order(nrates[, outcome], nrates[, "Hospital"]), ]
      nname <-nrates[rnum,1]
      rank <-rbind(rank,data.frame(hospital = nname,state = state))
      
  }
  
  rank
}

  

