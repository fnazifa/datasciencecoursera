pollutantmean<- function(dir, pollutant, id=1:332) { 
        dir<- list.files(dir, full.names = T) #list files 
        dat<- data.frame() #make empty df 
        for (i in id) { 
                dat <- rbind(dat, read.csv(dir[i])) #rbind all files 
        } 
        mean(dat[,pollutant], na.rm = TRUE) #calculate mean of given column 
} 
