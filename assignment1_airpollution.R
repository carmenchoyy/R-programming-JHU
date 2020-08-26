# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

pollutantmean<-function(directory,pollutant,id=1:332){
  
  #directory
  list.filenames<-list.files(directory, pattern="*.csv", full.names=T)
  
  #create df
  df<-data.frame()

  #read selected monitors and store in df
  for (i in id) {
    temp<-read.csv(list.filenames[i],header=T)
    df<-rbind(df,temp)
  }
  # View(df)
  
  #get mean without NAs
  mean(df[,pollutant],na.rm=T)
}



# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows

complete<-function(directory,id=1:332){
  
  #directory
  list.filenames<-list.files(directory,pattern="*.csv", full.names=T)
  
  #create df
  df<-data.frame()
  
  #read selected monitors and store number of rows in variable mA
  for (i in id){
    temp<-read.csv(list.filenames[i],header=T)
    m<-is.na(temp$sulfate)|is.na(temp$nitrate)
    temp1<-temp[!m,]
    mA<-c(i,nrow(temp1))
    df<-rbind(df,mA)
  }
  
  #created df to store monitor ID and number of rows
  colnames(df)<-c("id","nobs")
  
  View(df)
}




#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

corr<-function(directory,threshold=0){
  
  #directory
  list.filenames<-list.files(directory,pattern="*.csv",full.names=T)
  
  #create vector
  a<-vector()
  
  #check each monitor against threshold and store correlation in vector a
  for (i in 1:332){
    temp<-read.csv(list.filenames[i],header=T)
    c<-complete.cases(temp$nitrate,temp$sulfate)
    
    temp<-temp[complete.cases(temp$nitrate,temp$sulfate),]
    
    if(nrow(temp)<threshold){next}
    else {
    a<-c(a,cor(temp$nitrate,temp$sulfate))}
    
  }

  #select only successful non-blank output
    a[!is.na(a)]
}






