ativos <- c("ALUP11", "TAEE11","GRND3","ITUB4","ETER3","ELET6","HGTX3","VALE5","HBOR3")
PRECOS <- NULL;
index <- length(ativos)
for(id in 1:index){

    
    ATIVO <- read.csv(paste("http://www.google.com/finance/historical?q=BVMF%3A",ativos[id],"&output=csv",sep=""), stringsAsFactors = FALSE)
    names(ATIVO) <- c("DATE","OPEN","HIGH","LOW","CLOSE","VOLUME")
    
    mdHigh <- median(ATIVO[1:30,3])
    mdLow <- median(ATIVO[1:30,4])
    
    if(is.null(PRECOS)){
        PRECOS <- data.frame(list(ativos[id],Sys.Date(), mean(mdHigh,mdLow)),check.names = FALSE)
        names(PRECOS) <- c("ATIVO","DATA","VALOR")    
    }else{
        ATUAL <-  data.frame(list(ativos[id],Sys.Date(), mean(mdHigh,mdLow)),check.names = FALSE)
        names(ATUAL) <- c("ATIVO","DATA","VALOR")  
        PRECOS <- rbind(PRECOS,ATUAL)
    }
    
    
}
