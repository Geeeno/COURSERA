classifica <- function(serie = "a"){
    if (serie == "a"){
        url <- "http://www.corrieredellosport.it/live/classifica-serie-a.html?cookieAccept"
    }else if( serie == "b"){
        url <- "http://www.corrieredellosport.it/live/classifica-serie-b.html?cookieAccept"
    }else{
        message("Errore: Nessun campionato trovato")
    }
    doc <- htmlTreeParse(url, useInternalNodes = TRUE)
    team <- xpathSApply(doc, "//td[@class='team-name']", xmlValue)
    team <- gsub("\n", "", team)
    team <- gsub("\t", "", team)
    punti <- xpathSApply(doc, "//td[@class='a-center']", xmlValue)
    punti <- gsub("\n", "", punti)
    punti <- gsub("\t", "", punti)
    n.team <- length(team)
    f <- rep(1:9, n.team)
    classifica <- split(punti, f)
    nomi.tab <- c("Rank", "Punti", "Giornate", "Vinte", "Pareggiate", "Perse", "Gol Fatti", "Gol Subiti", "Ultime Giornate")
    names(classifica) <- nomi.tab
    classifica.data <- data.frame(classifica[1], squadra = team, classifica[2], classifica[3], classifica[4], classifica[5], classifica[6], classifica[7], classifica[8], classifica[9], stringsAsFactors = FALSE)
    classifica.data[, 1] <- as.numeric(classifica.data[, 1])
    for (i in 3:9){
        classifica.data[, i] <- as.numeric(classifica.data[, i])
    }
    classifica.data <<- classifica.data
}