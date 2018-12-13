rm(list=ls())


mathdat=read.csv("psat-math-school Quick Report.csv")
eladat=read.csv("psat-ela-school Quick Report.csv")

colnames(mathdat)=c("dist","school","grade","num_enrolled","pctested","not","partial","meeting","exceed","meetandexceed","avgscalescore")
colnames(eladat)=c("dist","school","grade","num_enrolled","pctested","not","partial","meeting","exceed","meetandexceed","avgscalescore")

data=mathdat

#fucntion to remove header, extra cells,& non/excluded data (* fields)
cleaner=function(data){
  for(c in 1:2){ #convert character columns to char
    data[,c]=as.character(data[,c])
  }
  exclude=numeric()
  for(r in 1:length(data[,1])){
    for(c in 3:length(colnames(data))){
      if(data[r,c][1]=="*" | data[r,c][1]=="**" | data[r,c][1]=="***"){
        exclude=c(exclude,r)
      } else {
        data[r,c]=as.numeric(data[r,c][1])
      }
    }
  }
    exclude=unique(exclude)
    data=data[-c(1,exclude),]
    return(data)
}
  

cleaner(mathdat)

unique(exclude)
data=mathdat
length(data[,1])

data[,1]=as.character(data[,1])
data[,1]
data[10,4]

mathdat=mathdat[-1,]
mathdat[,4]=as.numeric(mathdat[,4])

mathdat[10,][4]%%1
