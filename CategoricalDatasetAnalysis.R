
categories<-read.csv("1Total_Number_of_Persons_Killed_RoadUser_Categories_2009-2015.csv")
View(categories)

library("ggplot2")
ggplot(categories , aes(x= Pedestrians.2009)) +
  geom_histogram(binwidth = 50) +
  theme_bw() +
  labs(y= "states count" ,
       x= " no. of pedestrians died in 2009",
       title ="No. of states having less number of accidents")

plot(x = categories$Pedestrians.2009 , y= categories$Total.2009, main= "Total deaths vs pedestrians death in 2009",
     xlab= "pedestrains death" , ylab = "Total deaths")


ped <- data.frame(categories$Pedestrians.2009, categories$Total.2009)
View(ped)

plot(x = ped$categories.Pedestrians.2009 , y= ped$categories.Total.2009, col="blue", main= "Total deaths vs pedestrians death in 2009",
     xlab= "pedestrains death" , ylab = "Total deaths")
#geom_abline(h=mean(ped$categories.Pedestrians.2009), col= "orange")
##########################################################################################



plot(x = categories$Two.Wheelers.2009 , y= categories$Total.2009, col="blue", main= "Total deaths vs pedestrians death in 2010",
     xlab= "2 wheelers death" , ylab = "Total deaths")

################################################################################
###############################################################################3
######TOTAL DEATHS VS PEDESTRIANS DEATH IN 2009###

pd<- ped$categories.Pedestrians.2009 / 1000
tt<- ped$categories.Total.2009 /1000

plot(x = pd , y= tt, col="blue", main= "Total deaths vs pedestrians death in 2009",
     xlab= "pedestrains death" , ylab = "Total deaths")
lm_total <- lm(tt ~ pd)
abline(coef(lm_total), lwd=2)
ranks<- order(pd)
lines(pd[ranks], tt[ranks])


################################################################################
################################################################

hist(pd)
hist(categories$Bicycles.2009)    
hist(categories$Two.Wheelers.2009) 
hist(categories$Auto.Rickshaws.2009) 
hist(categories$Car..Taxis..Vans.and.Other.Light.and.Medium.Motor.Vehicles.2009) 
hist(categories$Trucks.2009) 
hist(categories$Buses.2009) 
hist(categories$Other.Motor.Vehicles.2009)

#############################################################################
##############################################################################
library(calibrate)

states <- categories$States.UTs

max(categories$Pedestrians...2015)
match(max(categories$Pedestrians...2015) , categories$Pedestrians...2015)
states[match(max(categories$Pedestrians...2015) , categories$Pedestrians...2015)]

plot(x = categories$Pedestrians...2015 , y= categories$Total...2015, col="blue", main= "Total deaths vs pedestrians death in 2015",
     xlab= "pedestrians death" , ylab = "Total deaths")

Ped2015.highest <- match(max(categories$Pedestrians...2015) , categories$Pedestrians...2015)
textxy(categories$Pedestrians...2015[categories$Pedestrians...2015.highest]-10,
       categories$Total...2015[categories$Pedestrians...2015.highest]-10,
       states[categories$Pedestrians...2015.highest], col= "red" , cex=0.7)

################################################################################
##################################################################################

two<- categories$Two.Wheelers.2009

plot(ped ,col.main="darkgrey" , main="two wheelers vs pedestrian deaths in 2009",
     type ="n" , xlab= "pedestrian death" , ylab="2 wheelers death")

segments(x0=pd, y0=two, x1=pd, y1=rep(mean(two)), length(two),
         col=rgb(0,0,1,0.3))
abline(h=mean(two), col="orange")
points(ped, col=rgb(0,0,1,0.5), pch=20, cex.axis=0.6)
################################################################
#########################################################################


plot(categories$States.UTs, categories$Total...2015, Title="Relative total deaths in each state(2015)", xlab="states &UTs", ylab="total deaths(2015)")

####################################################################################
###################################################################################
#Annual pedestrian death rates#########

View(categories)
ncol(categories)
nrow(categories)
pd2 <- categories[ ,2:8]
View(pd2)
names(pd2)=c(1,2,3,4,5,6,7)
View(pd2)

f<- as.numeric(pd2$`1`)
s<- as.numeric(pd2$`2`)
t<- as.numeric(pd2$`3`)
fo<- as.numeric(pd2$`4`)
fi<- as.numeric(pd2$`5`)
si<- as.numeric(pd2$`6`)
se<- as.numeric(pd2$`7`)


pd2 <-data.frame(f, s,t,fo,fi,si,se)
View(pd2)
str(pd2)

totpd =c()
totpd =vector("numeric", 7)

for(i in 1:ncol(pd2)){
  
  totpd[i]= pd2[37,i]
}

summ<-sum(totpd)
summ

output = c()
output=vector( "numeric",7)
for (i in 1:ncol(pd2)){
  output[i] = (pd2[37,i]/summ)*100
}

###create a char vector
years<- c("2009", "2010", "2011", "2012","2013" , "2014" , "2015")
names(output) <- years
output
hist(output)
plot(years, output, type = "o", col="red",
     xlab="YEARS",
     ylab ="DEATH % ",
     main= "Annual Percentage Of Pedestrian Death Rates",
     lty=2,
     pch=19,
     col.main="darkblue")

##########################################################################
######################################################################

all2015 <- categories[ ,c(8,15,22,29,36,43,50,57,64,71)]
View(all2015)
str(all2015)

names(all2015) =c(1,2,3,4,5,6,7,8,9,10)
f<- as.numeric(all2015$`1`)
s<- as.numeric(all2015$`2`)
t<- as.numeric(all2015$`3`)
fo<- as.numeric(all2015$`4`)
fi<- as.numeric(all2015$`5`)
si<- as.numeric(all2015$`6`)
se<- as.numeric(all2015$`7`)
e<- as.numeric(all2015$`8`)
n<- as.numeric(all2015$`9`)
te<- as.numeric(all2015$`10`)

all2015 <- data.frame(f, s,t,fo,fi,si,se,e,n,te)
View(all2015)

cat<-c("pedestrians","Bicycles", "2wheelers" ,"Auto.Rickshaws" ,"Car/Taxis/Vans", "Trucks", "Buses", "Other Motor Vehicles", "others")
#cat <-c("ped" , "bi" ,"2wh" ,"auto" ,"car" , "truck" ,"bus", "other","others" )
output<-c()
output<-vector("numeric" , 9)
output

for(i in 1:(ncol(all2015)-1)){
  
  output[i] = (all2015[37,i]/all2015[37,10])*100
}

names(output)<-cat
output
final<-data.frame(cat,output)
View(final)


library("ggplot2")
ggplot(final , aes(x= cat, y=output)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y= "% death rate" ,
       x= " Categories",
       title ="Categorical annual death %")

#######################################
##################################
#######Circular plot for same#########
ggplot(final, aes(x = cat, y = output ,fill = cat)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("") +
  
  #Increase ylim to avoid having a complete circle
  ylim(c(0,75)) + 
  
  #Add group labels close to the bars :
  geom_text(data = final, hjust = 1, size = 3, aes(x = cat, y = 0, label = cat)) +
  
  #Remove useless legend, y axis ticks and y axis text
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())

##########################################################################
#######3Dpiechart of same########
install.packages("plotrix")
library(plotrix)

pie3D(output,labels=cat,explode=0.1,
      main="3D Pie Chart of categorial % death rate In 2015 ")

##################################################################3
################ ANNOTED pie chart #####################################################33

value<-as.integer(output)
cat <- paste(cat, value) # add percents to labels 
cat <- paste(cat,"%",sep="") # ad % to labels 
pie(value,labels = cat, col=rainbow(length(cat)),
    main="2D Pie Chart of categorial % death rate In 2015")

###############################################################################
###############################################################################