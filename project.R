library(plotrix)
fb_teams<-read.csv("./teams.csv")
cat("\n                            ==============\n")
cat("                            R Mini Project")
cat("\n                            ==============\n")
cat("\n\n                         =====================\n")
cat("                          Football Statistics")
cat("\n                         =====================\n")
RandomNum1 <-floor(runif(1, 1, 20))
RandomNum2 <-floor(runif(1, 1, 20))
fb_home<-fb_teams[c(RandomNum1),c("TEAMS")]##read a random number
fb_away<-fb_teams[c(RandomNum2),c("TEAMS")]##read a random number
cat("\n          ---------------Upcoming Football Match---------------")
result = paste(fb_home,"VS",fb_away)##concat string

cat("\n\n                        *************************\n")
cat("\n                         ",result)
cat("\n\n                        *************************")
cat("\n\n  Previous goals of",fb_home,"\n")
cat(" -------------------------------\n")
for(x in 1:5){
  qwerty<-paste("MATCH",x,sep = "")
  y=fb_teams[c(RandomNum1),c(qwerty)]
  cat(paste(" ",y,""))
}


cat("\n\n  Previous match Status of ",fb_home,"\n")
cat(" ------------------------------------\n")
for(x in 1:5){
  qwerty<-paste("WDL",x,sep = "")
  y=fb_teams[c(RandomNum1),c(qwerty)]
  cat(paste(" ",y,""))
}

cat("\n\n\n\n  Previous goals of",fb_away,"\n")
cat(" -------------------------------\n")
for(x in 1:5){
  qwerty<-paste("MATCH",x,sep = "")
  y=fb_teams[c(RandomNum2),c(qwerty)]
  cat(paste(" ",y,""))
}


cat("\n\n  Previous match Status of ",fb_away,"\n")
cat(" ------------------------------------\n")
for(x in 1:5){
  qwerty<-paste("WDL",x,sep = "")
  y=fb_teams[c(RandomNum2),c(qwerty)]
  cat(paste(" ",y,""))
}

t1_gc<-c(fb_teams[c(RandomNum1),c("CONCEDED1")],fb_teams[c(RandomNum1),c("CONCEDED2")],fb_teams[c(RandomNum1),c("CONCEDED3")],fb_teams[c(RandomNum1),c("CONCEDED4")],fb_teams[c(RandomNum1),c("CONCEDED5")])
t1_gs<-c(fb_teams[c(RandomNum1),c("MATCH1")],fb_teams[c(RandomNum1),c("MATCH2")],fb_teams[c(RandomNum1),c("MATCH3")],fb_teams[c(RandomNum1),c("MATCH4")],fb_teams[c(RandomNum1),c("MATCH5")])

t2_gs<-c(fb_teams[c(RandomNum2),c("MATCH1")],fb_teams[c(RandomNum2),c("MATCH2")],fb_teams[c(RandomNum2),c("MATCH3")],fb_teams[c(RandomNum2),c("MATCH4")],fb_teams[c(RandomNum2),c("MATCH5")])
t2_gc<-c(fb_teams[c(RandomNum2),c("CONCEDED1")],fb_teams[c(RandomNum2),c("CONCEDED2")],fb_teams[c(RandomNum2),c("CONCEDED3")],fb_teams[c(RandomNum2),c("CONCEDED4")],fb_teams[c(RandomNum2),c("CONCEDED5")])
win_pro<-cor(t1_gs,t2_gs,method = "pearson")
cat("\n\n\n                           =========================\n")
cat("                           Predicting the next match")
cat("\n                           =========================")
if(win_pro==0){
  cat("\n\n                            Draw the match\n")
}else if(win_pro>0){
  cat("\n\n                          ",paste(fb_home,"Win The match\n\n\n"))
}else{
  cat("\n\n                          ",paste(fb_away,"Win The match\n\n\n"))
}
p=0
cat("                          ==========================")
cat("\n                            Football League Winner\n")
cat("                          ==========================\n")
for(i in 1:20){
  n=0
  for(j in 1:5){
    qwerty<-paste("WDL",j,sep = "")
    y=fb_teams[c(i),c(qwerty)]
    if(y=="W"){
      n=n+1
    }
  }
  t_w=paste("num",i)
  t_w=n
  if(t_w==3){
    p=p+1
    if(p==1){
      cat("                          ",fb_teams[c(i),c("TEAMS")],"Win the League\n\n\n")
    }
  }
}
team_rank<-fb_teams$TR
team_names<-fb_teams$TEAMS
pie(team_rank,labels = team_rank,main = "Team Rating",col = rainbow(length(team_rank)))
legend("topright", team_names,cex = 0.5, fill = rainbow(length(team_names)))


team_data<-data.frame(fb_teams$HM,fb_teams$AM)
chi<-(chisq.test(team_data))
print(chi$p.value)
if(chi$p.value<0.5){
  print("There exists co relation between Winning Home match and Winning Away match ")
}else{
  print("There is no co relation between Winning Home match and Winning Away match ")
}