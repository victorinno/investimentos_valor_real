
ATIVO_ricos_rico <- c("CTIP3", "BBSE3","TRPL4","LEVE3","EQTL3","VIVT4")
PRECOS_rico <- NULL;
index_rico <- length(ATIVO_ricos_rico)
for(id in 1:index_rico){
    
    
    ATIVO_rico <- read.csv(paste("http://www.google.com/finance/historical?q=BVMF%3A",ATIVO_ricos_rico[id],"&output=csv",sep=""), stringsAsFactors = FALSE)
    names(ATIVO_rico) <- c("DATE","OPEN","HIGH","LOW","CLOSE","VOLUME")
    
    ATIVO_rico <-  mutate(ATIVO_rico, DATE = gsub("May","Mar",DATE)) %>%
        mutate(DATE = gsub("Apr","Abr",DATE)) %>%
        mutate(DATE = gsub("Feb","Fev",DATE)) %>%
        mutate(DATE = gsub("Sep","Set",DATE)) %>%
        mutate(DATE = gsub("Aug","Ago",DATE))
    
    mdHigh <- median(ATIVO_rico[1:30,3])
    mdLow <- median(ATIVO_rico[1:30,4])
    
    if(is.null(PRECOS_rico)){
        PRECOS_rico <- data.frame(list(ATIVO_ricos_rico[id],Sys.Date(), mean(mdHigh,mdLow)),check.names = FALSE)
        names(PRECOS_rico) <- c("ATIVO_rico","DATA","VALOR")    
    }else{
        ATUAL_rico <-  data.frame(list(ATIVO_ricos_rico[id],Sys.Date(), mean(mdHigh,mdLow)),check.names = FALSE)
        names(ATUAL_rico) <- c("ATIVO_rico","DATA","VALOR")  
        PRECOS_rico <- rbind(PRECOS_rico,ATUAL_rico)
    }
    
    
}