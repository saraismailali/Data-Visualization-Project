# Source: https://www.statmethods.net/graphs/pie.html

pieChart <- function(slices , lbls){
  
  pct <- round(slices/sum(slices)*100)
  
  lbls <- paste(lbls, pct) # add percents to labels 
  
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  
  colors = c("green" ,"red")
  
  pie(slices,labels = lbls, col=colors,
      main="Pie Chart of Titanic")
}

#slices <- c(100, 100, 100) 
#lbls <- c("US", "UK", "Australia")

#pieChart(slices, lbls)

