library(rio)
library(stringr)

Sys.setlocale(category = "LC_ALL", locale = "Greek")
setwd("C:/Users/alexe/Downloads/OneDrive_10_1-11-2022")

file <- "ΣΤΑΤΙΣΤΙΚΗ ΑΕΡΟΠΟΡΙΚΗΣ ΚΙΝΗΣΗΣ 2017_3.xlsx"

mysheets <- import_list(file)

n<-length(mysheets)

for(i in 1:n){
  if (length(mysheets[[i]][[2]])<14) {
  break
  }
mydata[i,1]<-fixdata(i)[1]
mydata[i,2]<-fixdata(i)[2]
mydata[i,3]<-""
mydata[i,4]<-fixdata(i)[3]
}

fixdata<-function(x) {
airport<-names(mysheets[[x]][3])
airport<-substring(airport,11)
year<-str_sub(names(mysheets[[x]][12]),-4)
domint<-str_sub(names(mysheets[[x]][12]),1,-6)

c(airport,year,domint)
}

lapply(mysheets,fixdata)
