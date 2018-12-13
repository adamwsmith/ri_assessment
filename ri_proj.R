rm(list=ls())


mathdat=read.csv("psat-math-school Quick Report.csv")
eladat=read.csv("psat-ela-school Quick Report.csv")

colnames(mathdat)=c("dist","school","grade","num_enrolled","pctested","not","partial","meeting","exceed","meetandexceed","avgscalescore")
colnames(eladat)=c("dist","school","grade","num_enrolled","pctested","not","partial","meeting","exceed","meetandexceed","avgscalescore")



#fucntion to remove header, extra cells,& non/excluded data (* fields)
cleaner=function(data){
  data=data[-1,]
  for(r in 3:length(colnames(mathdat))){ #ensure fields are numeric NOT factor
    mathdat[,r]=as.numeric(mathdat[,r])
  }
  # exclude=numeric()
  # for(i in 2:length(data[,1])){ #identify/remove lines with *
  #   if(is.na(data[i,][4]%%1)){
  #     exclude=c(exclude,i)
  #   }
  # }
  #data=data[-exclude,]
}

cleaner(mathdat)

data=mathdat

mathdat=mathdat[-1,]
mathdat[,4]=as.numeric(mathdat[,4])

mathdat[10,][4]%%1
