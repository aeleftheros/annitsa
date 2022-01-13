library(stringr)
library(rio)
library(writexl)

#Sys.setlocale(category = "LC_ALL", locale = "Greek")
#setwd("D:/Users/ALEX/Desktop/TEMP FOLDER")
#file <- "2017.xlsx"
#mysheets <- import_list(file)

fixdata<-function(x) {
  airport<-names(mysheets[[x]][3])
  airport<-substring(airport,11)
  year<-str_sub(names(mysheets[[x]][12]),-4)
  domint<-str_sub(names(mysheets[[x]][12]),1,-6)
  
  c(airport,year,domint)
}
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

n<-length(mysheets)
mydata<-data.frame()

{month<-data.frame()
  flight<-data.frame()
  arrival<-data.frame()
  departure<-data.frame()
  tranzit<-data.frame()
  fmonth<-data.frame()
  fflight<-data.frame()
  farrival<-data.frame()
  fdeparture<-data.frame()
  ftranzit<-data.frame()
}
k<-0
l<-0
for(i in 1:n){
  if (length(mysheets[[i]][[2]])<14) {break}
  
  if (fixdata(i)[3]=="DOMESTIC + INT/NAL"||fixdata(i)[3]=="INTER/NAL SCHED.+ NO") {next}
  l<-l+1 
  
  month<-data.frame(t(data.frame(lapply(mysheets[[i]][[2]],function(x) x))))
  flight<-data.frame(t(data.frame(lapply(mysheets[[i]][[5]],function(x) x))))
  arrival<-data.frame(t(data.frame(lapply(mysheets[[i]][[7]],function(x) x))))
  departure<-data.frame(t(data.frame(lapply(mysheets[[i]][[9]],function(x) x))))
  tranzit<-data.frame(t(data.frame(lapply(mysheets[[i]][[10]],function(x) x))))
  
  {month<-data.frame(month[-c(1,2),])
    month[month=="'"]<-0
    names(month)<-NULL
    flight<-data.frame(flight[-c(1,2),])
    flight[flight=="'"]<-0
    names(flight)<-NULL
    arrival<-data.frame(arrival[-c(1,2),])
    arrival[arrival=="'"]<-0
    names(arrival)<-NULL
    departure<-data.frame(departure[-c(1,2),])
    departure[departure=="'"]<-0
    names(departure)<-NULL
    tranzit<-data.frame(tranzit[-c(1,2),])
    tranzit[tranzit=="'"]<-0
    names(tranzit)<-NULL}
  
  {fmonth<-cbind.fill(fmonth,month)
    fflight<-cbind.fill(fflight,flight)
    farrival<-cbind.fill(farrival,arrival)
    fdeparture<-cbind.fill(fdeparture,departure)
    ftranzit<-cbind.fill(ftranzit,tranzit)}
  
  for (m in 1:12){
    k<-k+1
    mydata[k,1]<-fixdata(i)[1]
    mydata[k,2]<-fixdata(i)[2]
    mydata[k,3]<-fmonth[m,l]
    mydata[k,4]<-fixdata(i)[3]
    mydata[k,5]<-fflight[m,l]
    mydata[k,6]<-farrival[m,l]
    mydata[k,7]<-fdeparture[m,l]
    mydata[k,8]<-ftranzit[m,l]
  }
}

names(mydata)<-c("AIRPORT","YEAR","MONTH","TYPE","FLIGHTS","ARRIVALS","DEPARTURES","TRANZIT")

#write_xlsx(mydata,"D:/Users/ALEX/Desktop/TEMP FOLDER/RESULTS/results2017.xlsx")

