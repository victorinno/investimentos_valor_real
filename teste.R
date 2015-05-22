library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

ativos <- c("ALUP11", "TAEE11","GRND3","ITUB4","ETER3","ELET6","HGTX3","VALE5","HBOR3")
PRECOS <- NULL;


avaliacao_valor_perm <- NULL;

index <- length(ativos)
for(id in 1:index){
    avaliacao_valor <- NULL;
    
    ATIVO <- read.csv(paste("http://www.google.com/finance/historical?q=BVMF%3A",ativos[id],"&output=csv",sep=""), stringsAsFactors = FALSE)
    names(ATIVO) <- c("DATE","OPEN","HIGH","LOW","CLOSE","VOLUME")
    
    ATIVO <-  mutate(ATIVO, DATE = gsub("May","Mar",DATE)) %>%
        mutate(DATE = gsub("Apr","Abr",DATE)) %>%
        mutate(DATE = gsub("Feb","Fev",DATE)) %>%
        mutate(DATE = gsub("Sep","Set",DATE)) %>%
        mutate(DATE = gsub("Aug","Ago",DATE)) %>%
        mutate(DATE = gsub("Oct","Out",DATE)) %>%
        mutate(DATE = gsub("Dec","Dez",DATE))
    
    
    ATIVO_VALORES <- mutate(ATIVO, ATIVO = ativos[id]) %>%
                    mutate(VALOR = CLOSE) %>%
                    mutate(DIA = as.numeric(format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%d"))) %>%
                    mutate(MES = as.numeric(format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%m"))) %>%
                    mutate(ANO = as.numeric(format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%Y"))) %>%
                    mutate(DATA = as.Date(paste(c(format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%d"))
                                                ,"/",format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%m"))
                                          , "/", format(as.Date(ATIVO$DATE,"%d-%b-%y"), "%Y"))), sep=""), "%d/%m/%y")) %>%
                    mutate(PERC = 0) %>%
                    mutate(PERC_SUM = 0) %>%
                    select(ATIVO, VALOR, DIA, MES, ANO,PERC,PERC_SUM)
    
    for(i in 1:(length(ATIVO_VALORES$VALOR) - 1)){
        ATIVO_VALORES[i,6] = (ATIVO_VALORES[i,2] / ATIVO_VALORES[(i+1),2]) - 1
    }
    
    ATIVO_VALORES$PERC_SUM = cumsum(ATIVO_VALORES$PERC)
    
    ATIVO_VALORES <- arrange(ATIVO_VALORES, ANO, MES, DIA)
    
    
    
    ggplot(ATIVO_VALORES, aes(x=DATA,y=PERC_SUM)) + 
        geom_line()  + 
        scale_x_date(breaks = date_breaks("days"), labels = date_format("%d")) +
        coord_cartesian(ylim = c(-120, 120))
    
}
