
#upload the sample file
msresult <- read.csv("/Users/sonicboy66/Documents/Research/r/lipidanalyzer-master/sampleInput/Positive- Kumar_etal_journal_pone_0206606_positive_mode.csv", header=FALSE, colClasses = "character")

sampleL <- t(msresult)
# test for inversion of the row and column 
sampleL <- as.data.frame(t(msresult))

#convert the sample file data frame to character so GREP can filter it 
listB=as.character(as.vector(sampleL$V1)) #I could not get "sample name" off the list
listA <-listB[-1]
# user input
userInput=c("SA", "S", "B")
#userInput=c("SA1", "SA2", "B")
b <- length(userInput)
N_userInput = length(userInput)
userL <- rep( list(list()), N_userInput )

#for user entering multiple input separate by coma 
for(i in 1:b){
  rep
  userL[[i]]<-grep(userInput[i], listA)
}

# this step combine all number indices in userL into a list 
combineList <- unlist(userL, recursive = TRUE, use.names = FALSE)

#delete out duplicated indices, and sort the list from 1 to x 
Newlist <- Newlist <- unique(sort(combineList))

#create data class list 
ClassName <- gsub("\\d+", "", listA)
dataName <-data.frame(V1="sample")
dataClass <- data.frame(V1="class")
#Pre-process the data.frame before it get fed with data 
for(i in 1:length(Newlist)){
  n=i+1
  dataName[1,n] <- listA[Newlist[i]]
  dataClass[1,n] <- ClassName[Newlist[i]]
}
dataName<-t(dataName)
dataClass<-t(dataClass)
#Saving the data into data frame 
NewDF <- data.frame(V1=dataName, V2=dataClass)

#extracting and compile everything into a data frame for pre-processing 
#L= lipid name length, 32511
L<-length(msresult[,2])
#S= sameple name length,48
S <- length(dataName)-1
tempL <-data.frame(msresult[1])
for(i in 1:S){
  for(j in 1:L){
    K=Newlist[i]+1
    n=i+1
    tempL[j,n] <-msresult[j,K]
  }
}
#recreate a list to covert row to column
t_tempL<- as.data.frame(t(tempL))
n=length(t_tempL[1,]) #ending at 3251th column
for(i in 2:n){
  j=i+1 #starting=3
  NewDF[j] <-t_tempL[i]
}

