complete <- function (directory, id = 1:332){
        #create a list of files
        filesD<-list.files(directory,full.names = TRUE)
        #create an empty data frame
        dat <- data.frame()
        
        for(i in id){
                #read in the file
                temp<- read.csv(filesD[i],header=TRUE)
                #delete rows that do not have complete cases
                temp<-na.omit(temp)
                
                #count all of the rows with complete cases
                tNobs<-nrow(temp)
                
                #enumerate the complete cases by index
                dat<-rbind(dat,data.frame(i,tNobs))
                
        }
        return(dat)
}

corr<-function(directory,threshold=0){
        #create list of file names
        all <- complete(directory)
        
        if (sum(which(all$nobs>threshold))==0){
                
                NULL
        }
        
        else {
                thresh_index <- as.numeric(all[which(all$nobs>threshold),]$id)
                
                names <- list.files(directory)[thresh_index]
                read <- lapply(paste(directory,"/",names, sep = ""),read.csv)
                
                
                
                return(unlist(lapply(read, function(x){cor(x[,2],x[,3],use="pairwise.complete.obs")})))
        }
}