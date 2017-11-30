pollutantmean <- function(directory, pollutant, id = 1:332) {
	##'directory' is a character vector of length 1 indicating
	##the location of CSV files

	##'pollutant' is a character vector of length 1 indicating
	##the name of the pollutant for wich we will calculate the
	##mean; either "sulfate" or "nitrate"

	##'id' is an integer vector indicating the monitor ID numbers
	##to be used

	##Return the mean of pollutant across all monitors list 
	##in the 'id' vector (ignoring NA values)
	##NOTE: Do not round the result!

	setwd(directory)
	for (i in 1:length(id)) {
		if (id[i] < 10){
			filename <- paste("00",id[i],".csv",sep="")
			
		} else if (id[i] >= 10 & id[i] < 100){
			filename <- paste("0",id[i],".csv",sep="")
		} else {
			filename <- paste(id[i],".csv",sep="")
		}
		datai <- read.csv(filename)
      	if (i == 1){
			data <- datai
		}else{
			data <- rbind(data, datai)
		}
	}
	poll <- data[[pollutant]]
	bad <- is.na(poll)
	mean(poll[!bad])
}

complete <- function(directory, id = 1:332) {
	##'directory' is a character vector of length 1 indicating
	##the location of CSV files

	##'id' is an integer vector indicating the monitor ID numbers
	##to be used

	##Return a data frame of the form:
	##id nobs
	##1   117
	##2   1041
	##...
	##where 'id' is the monitor ID number and 'nobs' is the
	##number of complete cases

	setwd(directory)
	for (i in 1:length(id)) {
		if (id[i] < 10){
			filename <- paste("00",id[i],".csv",sep="")
			
		} else if (id[i] >= 10 & id[i] < 100){
			filename <- paste("0",id[i],".csv",sep="")
		} else {
			filename <- paste(id[i],".csv",sep="")
		}
		datai <- read.csv(filename)
		good <- complete.cases(datai)
		numcas = nrow(datai[good, ])
		datareci <- data.frame(id = id[i], nobs = numcas) 
      	if (i == 1){
			datarec <- datareci
		}else{
			datarec <- rbind(datarec, datareci)
		}
	}
      datarec
}

corr <- function(directory, threshold = 0) {
	##'directory' is a character vector of length 1 indicating
	##the location of CSV files

	##'threshold' is a numeric vector of length 1 indicating the 
	##number of completely observed observations (on all
	##variables) required to compute the correlation between
	##nitrate and sulfate; the default is 0

	##Return a numeric vector of correlations
	##NOTE: Do not round the result

	setwd(directory)
	k = 0
	l = 0
	for (j in 1:length(dir())) {
		x <- dir()
		pos = regexpr(".csv", x[j])
		if (pos > 0){
			k = k + 1	
		}
	}
	numfile = k
	sncorr <- vector("numeric")
	for (i in 1:numfile) {
		if (i < 10){
			filename <- paste("00",i,".csv",sep="")
		} else if (i >= 10 & i < 100){
			filename <- paste("0",i,".csv",sep="")
		} else {
			filename <- paste(i,".csv",sep="")
		}
		datai <- read.csv(filename)
		good <- complete.cases(datai)
		datac <- datai[good, ]
		numcas = nrow(datac)
		if (numcas >= threshold){
			sulf <- datac[["sulfate"]]
			nit  <- datac[["nitrate"]]
			sncorr[l] <- cor(sulf, nit)
			l = l + 1
		}
	}
	sncorr
}