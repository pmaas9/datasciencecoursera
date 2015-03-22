# numbers from: 
# http://www.tobaccofreekids.org/research/factsheets/pdf/0099.pdf

setwd("C:/Users/Paige/Documents/GitHubStuff/datasciencecoursera/Data Products Project")
data<-read.table("smoking_updated_numbers.csv", sep=",")
states<-data[,1]
cost<-data[,2]

perday<-1
years<-1
lots=perday*365*years*(1/20)*cost
min(lots)
max(lots)
mean(lots)
hist(lots)
perday*365*years*(1/20)*avecost

perweek<-6*20
years<-1
lots=perweek*(1/7)*365*years*(1/20)*cost
min(lots)
max(lots)
mean(lots)
hist(lots)
perweek*(1/7)*365*years*(1/20)*avecost

thestate="Minnesota"
dollars=round(lots[which(states==thestate)],2)

print(paste("Smoking", perday, "cigarettes per day for", years, "years in", thestate, "costs", dollars, "dollars."))


#### Working Function ####
# number = number cigarettes or packs smoked
# years = number of years smoking
# state = state you live in, with quotes
# time = specified per "day" or "week", default "day"
# pack = 0/1, did you specify number in packs?
#
howmuch<-function(number, years, state, time="day", pack=0){
thestate=state
if(number==1){
  mult1=""}else{
          mult1="s"}
if(years==1){
  mult2=""}else{
    mult2="s"}

if(time=="day"){
if(pack==1){
perday=20*number
}else{
perday=number
}
lots=perday*365*years*(1/20)*cost
dollars=round(lots[which(states==thestate)],2)
if(pack==1){
print(paste("Smoking ", number, " pack", mult1, " per day for ", years, " year", mult2 ," in ", thestate, " costs $", formatC(dollars, digits=2, big.mark=",", format="f"),"", sep=""))
}else{
print(paste("Smoking ", round(perday,1), " cigarette", mult1, " per day for ", years, " year", mult2, " in ", thestate, " costs $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
}
}
if(time=="week"){

if(pack==1){perweek=20*number}else{perweek=number}
lots=perweek*(1/7)*365*years*(1/20)*cost
dollars=round(lots[which(states==thestate)],2)
if(pack==1){
  print(paste("Smoking ", number, " pack", mult1, " per week for ", years, " year", mult2 ," in ", thestate, " costs $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
}else{
  print(paste("Smoking ", number, " cigarette", mult1, " per week for ", years, " year", mult2, " in ", thestate, " costs $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
}
}

}

### Reduction rephrasing
reduce<-function(number, years, state, time="day", pack=0){
  thestate=state
  if(number==1){
    mult1=""}else{
      mult1="s"}
  if(years==1){
    mult2=""}else{
      mult2="s"}
  
  if(time=="day"){
    if(pack==1){
      perday=20*number
    }else{
      perday=number
    }
    lots=perday*365*years*(1/20)*cost
    dollars=round(lots[which(states==thestate)],2)
    if(pack==1){
      print(paste("Smoking ", number, " fewer pack", mult1, " per day for ", years, " year", mult2 ," in ", thestate, " would save $", formatC(dollars, digits=2, big.mark=",", format="f"),"", sep=""))
    }else{
      print(paste("Smoking ", round(perday,1), " fewer cigarette", mult1, " per day for ", years, " year", mult2, " in ", thestate, " would save $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
    }
  }
  if(time=="week"){
    
    if(pack==1){perweek=20*number}else{perweek=number}
    lots=perweek*(1/7)*365*years*(1/20)*cost
    dollars=round(lots[which(states==thestate)],2)
    if(pack==1){
      print(paste("Smoking ", number, " fewer pack", mult1, " per week for ", years, " year", mult2 ," in ", thestate, " would save $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
    }else{
      print(paste("Smoking ", number, " fewer cigarette", mult1, " per week for ", years, " year", mult2, " in ", thestate, " would save $", formatC(dollars, digits=2, big.mark=",", format="f"), "", sep=""))
    }
  }
  
}







