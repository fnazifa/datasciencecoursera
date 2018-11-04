corr<-function(directory,threshold=0){
        #create list of file names
        filesD<-list.files(directory,full.names = TRUE)
        
        #create empty vector
        dat <- vector(mode = "numeric", length = 0)
        
        for(i in 1:length(filesD)){
                #read in file
                temp<- read.csv(filesD[i],header=TRUE)
                #delete NAs
                temp<-temp[complete.cases(temp),]
                #count the number of observations
                csum<-nrow(temp)
                #if the number of rows is greater than the threshold
                if(csum>threshold){
                        #for that file you find the correlation between nitrate and sulfate
                        #combine each correlation for each file in vector format using the concatenate function 
                        #since this is not a data frame we cannot use rbind or cbind
                        dat<-c(dat,cor(temp$nitrate,temp$sulfate))
                }
                
        }
        
        return(dat)
}