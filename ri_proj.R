rm(list=ls())
# getwd()
setwd("/Users/spikesmth/Documents/GitHub/ri_assessment")

#import .csv
psatmath=read.csv("psat-math-school Quick Report.csv")
psatlang=read.csv("psat-ela-school Quick Report.csv")

#fucntion to remove header, extra cells,& non/excluded data (* fields)
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
  

mathdat=cleaner(mathdat)
eladat=cleaner(eladat)


bwlunch=read.csv("lunch_bwps.csv")

cleaner2=function(data){ #coherce factor column into character and rename columns
  data[,2]=as.character(data[,2])
  colnames(data)=c("dist_id","school","frlp","bwps")
  return(data)
}

bwlunch=cleaner2(bwlunch)

source=mathdat
tgt=bwlunch
bit=data.frame()
nam=data.frame()
core=data.frame()

for(r in 1:length(source[,1])){
  patt=substr(source[r,2],1,15)
  for(i in 1:length(tgt[,1])){
    if(grepl(patt,tgt[i,2],ignore.case = T)){
      nam=c(nam,source[r,2])
      core=rbind(core,source[r,3:11])
      bit=rbind(bit,tgt[i,3:4])
      break
    }
  }
}
nam=as.data.frame(nam)
complete=cbind(t(nam),core,bit)
exclude=numeric()
for(i in 1:length(complete[,1])){
  if(complete[i,12]==0){
    exclude=c(exclude,i)
  }
}
complete=complete[-exclude,]

mod=lm(log(meetandexceed)~bwps+log(frlp),data=complete)
summary(mod)

mod2=lm(avgscalescore~bwps+log(frlp),data=complete)
summary(mod2)

mod3=lm(log(meetandexceed)~bwps+log(frlp)+avgscalescore,data=complete)
summary(mod3)

mod$coefficients
mod2$coefficients
mod3$coefficients

# %meet/exceed = bw/st + %frlp + connection type (dummy) + c2 budg spending + budget/capita + demographics + geog class