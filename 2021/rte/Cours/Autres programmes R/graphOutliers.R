date_rupture <- 2000
date_fin_rupture <- 2001
date_deb <- 1990
date_fin <- 2010
frequence <- 12
ao <-function(){
    x <- ts(0,start=date_deb,end=date_fin,
            frequency = frequence)
    window(x,start=date_rupture,end=date_rupture) <- 1
    x
}
ls <-function(){
    x <- ts(0,start=date_deb,end=date_fin,
            frequency = frequence)
    x[time(x)>=date_rupture] <- 1
    x
}
tc <- function(tcrate = 0.7){
    x <- ts(1,start=date_deb,end=date_fin,
            frequency = frequence)
    t <- ts(1:length(x),start=date_deb,end=date_fin,
            frequency = frequence)
    t0 <- as.numeric(window(t, start = date_rupture, end = date_rupture))
    x <- tcrate^(t - t0) * (t >= t0)
    x
}

so <- function(){
    x <- ts(0,start=date_deb,end=date_fin,
            frequency = frequence)
    per_rupture <- as.numeric(cycle(window(x,start=date_rupture,end=date_rupture)))
    
    x[time(x)>=date_rupture] <- -1/(frequence-1)
    x[(cycle(x) == per_rupture) & (time(x)>=date_rupture)] <- 1
    x
}
so2 <- function(){
    x <- ts(0,start=date_deb,end=date_fin,
            frequency = frequence)
    per_rupture <- as.numeric(cycle(window(x,start=date_rupture,end=date_rupture)))
    
    # x[time(x)>=date_rupture] <- -1/(frequence-1)
    x[(cycle(x) == per_rupture) & (time(x)>=date_rupture)] <- 1
    x
}
ramp <- function(){
    x <- ts(-1,start=date_deb,end=date_fin,
            frequency = frequence)
    t <- ts(1:length(x),start=date_deb,end=date_fin,
                   frequency = frequence)
    t0 <- as.numeric(window(t, start = date_rupture, end = date_rupture))
    t1 <- as.numeric(window(t, start = date_fin_rupture, end = date_fin_rupture))
    x <- -1 * (t <= t0) + ((t - t0) / (t1-t0) - 1)* (t > t0)* (t < t1) 
    x

}

tls <- function(){
    x <- ts(1,start=date_deb,end=date_fin,
            frequency = frequence)
    t <- ts(1:length(x),start=date_deb,end=date_fin,
            frequency = frequence)
    t0 <- as.numeric(window(t, start = date_rupture, end = date_rupture))
    t1 <- as.numeric(window(t, start = date_fin_rupture, end = date_fin_rupture))
    x <- 1 * (t >= t0 & t <= t1) 
    x
    
}


#PSO = SCR
pso <- function(){
    x <- ts(0,start=date_deb,end=date_fin,
            frequency = frequence)
        per_rupture <- as.numeric(cycle(window(x,start=date_rupture,end=date_rupture)))
    
    x[(cycle(x) == (per_rupture+1)) & (time(x) < date_rupture)] <- 1
    x[(cycle(x) == per_rupture) & (time(x) < date_rupture)] <- -1
    x
}


iv <- function(tcrate = 0.7){
    x <- ts(1,start=date_deb,end=date_fin,
            frequency = frequence)
    t <- ts(1:length(x),start=date_deb,end=date_fin,
            frequency = frequence)
    t0 <- as.numeric(window(t, start = date_rupture, end = date_rupture))
    x <- (1 + (-tcrate)^(t - t0) ) * (t >= t0)
    x
}
plot(iv())

library(zoo) 
library(tikzDevice)
outliers <- ts.union(ao(),ls(),tc(),so(), so2(),ramp(),tls(),pso(),iv())
outliers <- window(outliers, start = 1996, end = c(2003,12))
# outliers <- window(outliers, start = 1999, end = c(2002,12))
colnames(outliers) <- c("AO","LS","TC","SO","SO2","Ramp","TLS","IV","PSO")


tikz("img/outliers.tex",width=4.8,height=2.7)
plot(zoo(outliers),
     main= "",
     xlab = NULL, oma=c(0.2,0,0,0), mar = c(0,4.1,0,1.1),
     yaxt='n', xaxt='n')
dev.off()


for(nom_col in colnames(outliers)){
    tikz(paste0("img/",nom_col,".tex"),width=1.3,height=0.6)
    par(oma=c(0,0,0,0), mar = c(0,0,0,0),mai=c(0,0,0,0))
    plot.default(x=1:length(outliers[,nom_col]),y=as.numeric(outliers[,nom_col]),
                 type="l",
                 main= "",xlab = "",ylab="",
                 yaxt='n', xaxt='n',axes = FALSE)
    
    dev.off()
}
