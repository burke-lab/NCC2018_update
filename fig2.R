library(tidyverse)

### PRISM data can be downloaded here: http://prism.oregonstate.edu ###

### We are using the monthly data. Each monthly observation has an associated station information file that lists all stations 
### used in calculation for that month. We use monthly mean data which are based on monthly minimum and maximum temperatures.
### PRISM data are separated into "recent" (1981-present) and "historic" (pre-1981) data sets. Here we've combined the data files into
### a single directory for all max data (recent + historic) and min data, respectively.

### The code here references data structured as follows:
    ### max/PRISM_tmax_stable_4kmM3_[YEARMO]_bil.stn
    ### min/PRISM_tmin_stable_4kmM3_[YEARMO]_bil.stn

### The code loops over year by month files for min and max and counts number of station used in processing for each month.
### We count a station as complete if it has an observation for both min and max in that month



## max data

    ## identify files
      filesmax <- list.files(paste("max/"))
      filesmax <- filesmax[grep(x = filesmax, pattern = ".stn.csv")]
      
    ## read in files
      stmax <- list()
      for (i in 1:length(filesmax)){
          stmax[[i]] <- data.frame(read_csv(paste("max/",filesmax[i], sep = ""), skip = 1))
          stmax[[i]]$year <- as.numeric(substr(filesmax[i],25,28))
          stmax[[i]]$month <- as.numeric(substr(filesmax[i],29,30))
          print(i)
        }
    
## min data
        
      ## identify files
        filesmin <- list.files(paste("min/"))
        filesmin <- filesmin[grep(x = filesmin, pattern = ".stn.csv")]
          
      ## read in files  
        stmin <- list()
        for (i in 1:length(filesmin)){
            stmin[[i]] <- data.frame(read_csv(paste("min/",filesmin[i], sep = ""), skip = 1))
            stmin[[i]]$year <- as.numeric(substr(filesmin[i],25,28))
            stmin[[i]]$month <- as.numeric(substr(filesmin[i],29,30))
            print(i)
          }

    ## rbind    
    stinfomax<- data.frame(data.table::rbindlist(stmax))
    stinfomin <- data.frame(data.table::rbindlist(stmin))

    #our analysis runs 1968-2004
    stinfomax <- filter(stinfomax, year >= 1968 & year <= 2004)
    stinfomin <- filter(stinfomin, year >= 1968 & year <= 2004)
    
    #for each month identify stations that have both min and max obs 
    stinfo <- inner_join(stinfomax, stinfomin, by = c("Station","Latitude","Longitude","Network","station_id","year","month"))
        nrow(stinfo)/nrow(stinfomax) #>95% of max stations have min obs
        nrow(stinfo)/nrow(stinfomin) #>95% of min stations have max obs
    
    #count stations for each data month
    st_count <-   stinfo %>% 
                  group_by(year, month) %>% 
                  summarise(n_stat = n())
  
    
  #plot time series  
  pdf("fig2.pdf", width = 6, height = 5.5)  
    par(mar = c(4.5,5,2,2))
    plot(st_count$year + st_count$month/12, st_count$n_stat, col = NA, axes = F, xlab = "", ylab = "",lwd=1.5, xlim = c(1965, 2005), ylim = c(5000, 10000))
      segments(x0=1965, x1 = 2005, y0 = seq(5000,10000,500), col = 'gray90', lwd = 0.5)
      lines(st_count$year + st_count$month/12, st_count$n_stat, lwd = 1)
      
      arrows(x0 = 1968, x1 = 2005, y0 = 5000, lwd = 1, code = 3, length = 0.05)
      segments(x0 = 1981, y0 = 5000, y1 = 10000)
      text(x = 1975, y = 5100, labels = "Historic dataset",cex=0.6)
      text(x = 1994, y = 5100, labels = "Recent dataset",cex=0.6)
    
      axis(1, at = seq(1965, 2005,5), cex.axis=0.9)
      axis(2, at =seq(5000,10000,1000),labels = paste(seq(5,10,1),",000", sep = ""),las=2, cex.axis=1)
      mtext(side = 1, text = "Year", line=3, cex=1.2)
      mtext(side = 2, text = "Number of stations",line=3.5, cex = 1.2) 
      mtext(side = 3, text = "Monthly count of PRISM temperature stations",cex=1.25,line=0.5)
      
  dev.off()    
    
