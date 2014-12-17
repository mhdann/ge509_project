# Code snippets from ge509_project.Rmd

# Heatmap for exploring spatial dependence of temperature in the smoothed runoff-flow space
heatmap <- function(data, n){
  library(spatial)          ## spatial stats library
  ma <- function(x,n,self=0){filter(x,c(self,rep(1,n)),sides=1)}
  df <- data.frame(x=ma(data$totrun, n),y=ma(data$z, n),t=ma(data$avsft, n))
  df <- df[complete.cases(df),]
  surf3 <- surf.ls(3,df$x,df$y,df$t)   ## 3rd-degree polynomial surface
  tr3 <- trmat(surf3,min(df$x),max(df$x), min(df$y), max(df$y),500)          ## project a 50x50 matrix
  image(tr3) ## color image map of the surface
  points(df$x,df$y,pch="+",col=df$t/10)
}
heatmap(sr,24)

vg <- variogram(surf3,300,xlim=c(0,5000),ylim=c(0,1.1*var(df$t)))

abline(h=var(df$t))                   ## asymptotic variance
cg  <- correlogram(surf3,300,xlim=c(0,5000))  ## limit the range of radii to 


```{r}
par(mfrow=c(1,2), oma=c(0,0,2,0))
hist(sub$pct_gen, main=NA, xlab="Fraction of Maximum Generation")
ts.plot(sub$pct_gen, ylab="Fraction of Maximum Generation", xlab="Month")
title("Normalized Generation at Stewart Dam",outer=T)
abline(h=c(0,1))
```

```{r,echo=F}
par(mfrow=c(1,1), oma=c(0,0,0,0))
```

plot_grand_mean<-function(data,quants){
  py1 <- paste0("Py[",1:data$nt,",",1,"]")
  
  plot(1:data$nt,data$norm_gen[,1], ylim=c(-1,2), ylab="Normalized Generation",xlab="Month",main="CI/PI")
  points(1:data$nt,data$norm_gen[,2])
  points(1:data$nt,data$norm_gen[,3])
  points(1:data$nt,data$norm_gen[,4])
  lines(1:data$nt, rep(quants[2,"mu"],data$nt), col=3, lty=1) # median
  lines(1:data$nt, rep(quants[1,"mu"],data$nt), col=3, lty=2) # CI
  lines(1:data$nt, rep(quants[3,"mu"],data$nt), col=3, lty=2) # CI
  
  lines(1:data$nt, quants[1,py1], col=2, lty=2) # PI
  lines(1:data$nt, quants[3,py1], col=2, lty=2) # PI
  legend("topright",legend = c("Data", "Median","CI","PI"), lty=c(1, 1,2,2), col = c(1,3,3,2))  
}

plot_dam <- function(data, quants, i, Eis2d=F){
  if (Eis2d==T){
    eyi <- paste0("Ey[",  1:data$nt, ",", i, "]")
  } else {
    eyi <- paste0("Ey[",  1:data$nt,  "]")
  }
  pyi <- paste0("Py[",1:data$nt,",",i,"]")
  plot(1:data$nt, data$norm_gen[,i], ylim=c(0,1), main=data$plant_names[i], xlab = "Year-month",ylab="Normalized Generation")
  lines(1:data$nt,data$norm_gen[,i])
  lines(1:data$nt, quants[2,eyi], col=3) # median
  lines(1:data$nt, quants[1,eyi], col=3, lty = 2) # CI
  lines(1:data$nt, quants[3,eyi], col=3, lty = 2) # CI
  lines(1:data$nt, quants[1,pyi], col=2, lty = 2) # Pi
  lines(1:data$nt, quants[3,pyi], col=2, lty = 2) # PI
  legend("topright",legend = c("Data", "Median","CI","PI"), lty=c(1, 1,2,2), col = c(1,3,3,2))
}