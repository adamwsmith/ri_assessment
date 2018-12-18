rm(list=ls())
# getwd()
setwd("/Users/spikesmth/Documents/GitHub/ri_assessment")

#import .csv
psatmath=read.csv("psat-math-school Quick Report.csv")
psatlang=read.csv("psat-ela-school Quick Report.csv")



#function to remove header, extra cells,& non/excluded data (* fields)
cleaner=function(data){
  for(c in 1:2){ #convert character columns to char
    data[,c]=as.character(data[,c])
  }
  exclude=numeric() #initialize exclusion vector
  for(r in 3:length(data[,1])){ #identify rows to add to exclusion vector (rows containing *,**,***)
    for(c in 3:length(colnames(data))){
      if(data[r,c]=="*" | data[r,c]=="**" | data[r,c]=="***"){
        exclude=c(exclude,r)
      } 
    }
  }
  exclude=unique(exclude) #remove duplicate exclusion terms
  data=data[-c(1,exclude),] #eliminate rows from dataframe
  for(c in 3:length(colnames(data))){ #convert factor values to char then numeric
    data[,c]=as.character(data[,c])
    data[,c]=as.numeric(data[,c])
  }
  clip=numeric() #remove extra frames at the bottom
  for(r in 1:length(data[,4])){
    if(is.na(data[r,4])){
      clip=c(clip,r)
    }
  }
  data=data[-clip,]
  colnames(data)=c("dist","school","grade","num_enrolled","pctested","not","partial","meeting","exceed","meetandexceed","avgscalescore")
  return(data)
}


mathdat=cleaner(psatmath)
eladat=cleaner(psatlang)


eshdb=read.csv("ri_ass_dat_2018_01.csv")

cleaner2=function(data){
  #converting columns from factor to char,numeric,logical
  for(c in c(1,3,5,7,11,12,14,16)){
    data[,c]=as.character(data[,c])
    data[,c]=as.numeric(data[,c])
  }
  for(c in c(2,4,6,8,9,15)){
    data[,c]=as.character(data[,c])
  }
  for(c in c(10,13)){
    data[,c]=as.logical(data[,c])
  }
  return(data)
}

eshdb_clean=cleaner2(eshdb)


ridata=mathdat
eshdata=eshdb_clean
num_char=12
#matching algo by name
namematch=function(ridata,eshdata,num_char){
  maindf=data.frame()
  for(r in 1:length(ridata[,1])){
    rowdat=list()
    name.source=ridata$school[r] #pick up school name each row
    name.source=gsub(" ","",name.source) #kill spaces
    name.source=gsub("\\.","",name.source) #kill periods
    name.source=tolower(name.source) #make lower case
    name.source=substr(name.source,1,num_char) #clip string to length of num_char input var.
    for(t in 1:length(eshdata[,1])){
      name.target=eshdata$school_name[t] #pick up school name each row
      name.target=gsub(" ","",name.target) #kill spaces
      name.target=gsub("\\.","",name.target) #kill periods
      name.target=tolower(name.target) #make lower case
      name.target=substr(name.target,1,num_char) #clip string to length of num_char input var.
      if(name.source==name.target){
        rowdat=c(ridata[r,],
                     "frl_percent"=eshdata$frl_percent[t], #percent eligible for lunch subsidy
                     "stupschool"=(eshdata$num_students[t]/eshdata$num_schools[t]), #average student/school/dist
                     "teachpschool"=(eshdata$num_teachers[t]/eshdata$num_schools[t]), #average teach/school/dist
                     "pctc2_spent"=eshdata$pct_c2_spent[t], #percent of C2 budget spent
                     "bwps"=eshdata$ia_bandwidth_per_student_kbps[t]) #bandwidth per student
        
      }
    }
    maindf=rbind(maindf,rowdat)
  }
  maindf=as.data.frame(maindf)
  return(maindf)
}

ridata[7,]
