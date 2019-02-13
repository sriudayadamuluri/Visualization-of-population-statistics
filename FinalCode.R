library(ggplot2)
abc <- read.csv(file= "/Users/sriudayadamuluri/Desktop/FinalData.csv", header=TRUE, sep=",")

#1 Compare Population in 1900's and 2000's
cols <- c("Pop_1993" = "#f45c42", "Pop_2000" = "#80e5ff")
ggplot(abc, aes(x=State)) + 
  geom_line(aes(y=Pop_1993, color="Pop_1993")) +
  geom_line(aes(y=Pop_2000, color="Pop_2000")) + 
  geom_point(aes(y=Pop_1993, color="Pop_1993")) +
  geom_point(aes(y=Pop_2000, color="Pop_2000")) + 
  scale_y_continuous(limits=c(0,35000),breaks=c(0,5000,10000,15000,20000,25000,30000,35000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(x="State",y="Population ",title="Population Of Each State in Multiple Years",color="Year") +
  scale_colour_manual(values = cols,labels = c("In 1900's", "In 2000's"))



#2 Compare Population and Residing in Metro 
cols <- c("Pop_2000" = "#78ccb0", "ResideInMetro" = "#c94e4e")
ggplot(abc, aes(x=State)) + 
  geom_line(aes(y=Pop_2000, color="Pop_2000",group=1)) +
  geom_line(aes(y=ResideInMetro, color="ResideInMetro",group=1)) + 
  geom_point(aes(y=Pop_2000, color="Pop_2000")) +
  geom_point(aes(y=ResideInMetro, color="ResideInMetro")) + 
  scale_y_continuous(limits=c(0,35000),breaks=c(0,5000,10000,15000,20000,25000,30000,35000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(x="State",y="Population",title="Total Population Vs Population in Metro Area")+
  scale_colour_manual(values = cols,labels = c("Total Population", "Population in Metro Areas"))


#3 State and Unemployment Rate
ggplot(abc, aes(x=State, y=abc$UnemploymentRate)) + 
  geom_point(col="tomato2", size=3) + 
  geom_segment(aes(x=State, xend=State, y=min(UnemploymentRate), yend=max(UnemploymentRate)), 
               linetype="dashed", size=0.1) +  
  labs(title="Unemployment Rate For Each State",x="State",y="Unemployment Rate") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#4 Crime and Prisoners

ggplot() +
  geom_point(data=abc,aes(x=abc$State,y=abc$Crime,colour="ViolentCrime"))+
  geom_point(data=abc,aes(x=abc$State,y=abc$FederalAndStatePrisoners,colour="FederalAndStatePrisoners")) +
  scale_y_continuous(limits=c(0,100))+
  xlab("State")+ylab("Crime") +
  labs(x="State",y="Crime",title="Violent Crime Vs State Prisoners Per 10,000 People")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + hw + coord_flip()

#5 Choropleth

library(plotly)
df <- read.csv(file= "/Users/sriudayadamuluri/Desktop/FinalData.csv", header=TRUE, sep=",")
df$hover <- with(df, paste(State))

l <- list(color = toRGB("white"), width = 2)

g <- list(scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(z = ~GrossStateIncome, text = ~hover, locations = ~code,
            color = ~GrossStateIncome, colors = 'Blues') %>%
  colorbar(title = "Billions USD") %>%
  layout(title = 'US Gross State Income in Billions',geo = g)
ggplotly(p)


#6 MicroMap

install.packages("micromapST")
library(micromapST)
library(tidyverse)
library(GGally)
library(ggplot2)
View(abc)
states1 <-abc[c(1,15,16)]
states1
colDesc <- data.frame(
  type = c('map','id','dot','dot'),
  lab1 = c('','','HEART',"CANCER"),
  lab2 = c('' ,'','DISEASE',''),
  col1 = c(NA,NA,'Heart','Cancer')
)
t(colDesc)

pdf(file = 'micromapUdaya.pdf', width = 7.5, height = 10 )
micromapST(abc, colDesc,rowNamesCol = "State", rowNames = "full",
           plotNames = "full",
           sortVar = "Heart", ascend = FALSE,
           title = c("DEATH RATE"))



#7

library(GGally)
abc <- read.csv(file= "/Users/sriudayadamuluri/Desktop/Corr.csv", header=TRUE, sep=",")
ggcorr(abc,label=T)

