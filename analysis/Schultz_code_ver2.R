#### Figure 2 (Map Figure) ####

# Locations of sites in Howe Sound (Figure constructed in Microsoft Powerpoint)
sites<-read.csv("HS_Sites.csv")
head(sites)
plot(sites$Lat~sites$Lon)

library(ggplot2)
p1<-ggplot(data=sites, aes(Lon,Lat)) + 
  geom_point(size = 2,aes(colour=season)) +
  geom_text(size=2,aes(label=site))
p1

#### Table 1 (Taxa and abundance) ####

# Species abundances before and after
transects<-read.csv("transectcounts.csv")
library(reshape)
longt<-melt(data=transects,id.vars=c("ssws","site","transect"),variable_name="species",value_name="count")
sites<-aggregate(longt$value,by=list(longt$ssws,longt$site,longt$species),sum)
ba.mean<-aggregate(sites$x,by=list(sites$Group.1,sites$Group.3),mean)
ba.sd<-aggregate(sites$x,by=list(sites$Group.1,sites$Group.3),sd)
ba.mean2<-cast(ba.mean,Group.2~Group.1)
colnames(ba.mean2) <-c("species","mean.after","mean.before")
ba.sd2<-cast(ba.sd,Group.2~Group.1)
colnames(ba.sd2) <-c("species","sd.after","sd.before")
ba.spp<-ba.mean2
ba.mean2$sd.after<-ba.sd2$sd.after
ba.mean2$sd.before<-ba.sd2$sd.before
ba.mean2
write.csv(ba.spp,file="beforeaftermeansd.csv")

#### Figure 3 (Change in abundance of sunflower stars, green urchins and kelp)  ####

## Organize data ##
counts<-read.csv("transectcounts.csv")

suns<-aggregate(counts$sunflower.star,by=list(counts$ssws,counts$transect),FUN=sum)
gus<-aggregate(counts$green.urchin,by=list(counts$ssws,counts$transect),FUN=sum)
# suns$x<-suns$x/3.75 #per m count
# gus$x<-suns$x/3.75

suns$species<-c("sunflower.star")
gus$species<-c("green.urchin")
colnames(suns)<-c("ssws","transect","count","species")
colnames(gus)<-c("ssws","transect","count","species")
suns$n.per.m<-suns$count/3.75
gus$n.per.m<-gus$count/3.75
head(gus)

tapply(suns$n.per.m,INDEX=suns$ssws,FUN=mean)
tapply(gus$n.per.m,INDEX=gus$ssws,FUN=mean)
head(suns)
head(gus)

tc<-rbind(suns,gus)
tail(tc)

library(Rmisc)
bardata<-summarySE(tc,measurevar="n.per.m",groupvars=c("ssws","species"))
bardata$ssws<-factor(bardata$ssws,levels=c("before","after"),ordered=TRUE)
bardata

kelp<-read.csv("kelpbytransect.csv")
kelp$mortality<-tolower(kelp$mortality) #change to lower case to match other df
kelp.a<-aggregate(kelp$percent.cover,by=list(kelp$mortality,kelp$transect),FUN=mean)
kelp.a$species<-c("kelp")
colnames(kelp.a)<-c("ssws","transect","mean.cover")
head(kelp.a)

kelps<-summarySE(kelp.a,measurevar="mean.cover",groupvars=c("ssws"))
kelps$ssws<-factor(kelps$ssws,levels=c("before","after"),ordered=TRUE)
kelps

## Create plots ##

library(ggplot2)
require(grid)

sun.plot<-ggplot(data=bardata[bardata$species=="sunflower.star",],aes(x=ssws,y=n.per.m,fill=ssws)) + 
  geom_bar(position=position_dodge(), colour="black",stat="identity",size=1) + 
  scale_fill_manual(values=c("gray45","gray90")) + 
  geom_errorbar(aes(ymin=n.per.m-se,ymax=n.per.m+se), width=0.2, position=position_dodge(0.9),size=1) + 
  xlab(NULL) + 
  ylab(expression(atop(Count~~"(per " *m^"2"*")"," "))) + 
  scale_x_discrete(breaks=c("before","after"),labels=c("Before","After")) +
  #   scale_y_continuous(breaks=c(0,0.5,1,1.5,2,2.5)) +
  theme_bw() + 
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.text.x=element_blank()) +
  theme(legend.position="none") +
  theme(axis.line=element_line(colour="black",size=1),
        axis.text.y=element_text(size=40), 
        axis.title.y=element_text(size=65), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(colour="black",size=2),
        axis.ticks.length=unit(1,"lines"),
        plot.margin=unit(c(5,5,2,3),"lines"))

gu.plot<-ggplot(data=bardata[bardata$species=="green.urchin",],aes(x=ssws,y=n.per.m,fill=ssws)) + 
  geom_bar(position=position_dodge(), colour="black",stat="identity",size=1) + 
  scale_fill_manual(values=c("gray45","gray90")) + 
  geom_errorbar(aes(ymin=n.per.m-se,ymax=n.per.m+se), width=0.2, position=position_dodge(0.9),size=1) + 
  xlab(NULL) + 
  ylab(expression(atop(Count~~"(per " *m^"2"*")"," "))) + 
  expand_limits(y=c(0,8)) +
  scale_x_discrete(breaks=c("before","after"),labels=c("Before","After")) +
  theme_bw() + 
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.text.x=element_blank()) +
  theme(legend.position="none") +
  theme(axis.line=element_line(colour="black",size=1),
        axis.text.y=element_text(size=45), 
        axis.title.y=element_text(size=65), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(colour="black",size=2),
        axis.ticks.length=unit(1,"lines"),
        plot.margin=unit(c(3,5,2,5),"lines")) #cw from top

kelp.plot<-ggplot(data=kelps,aes(x=ssws,y=mean.cover,fill=ssws)) + 
  geom_bar(position=position_dodge(), colour="black",stat="identity") + 
  scale_fill_manual(values=c("gray45","gray90")) + 
  geom_errorbar(aes(ymin=mean.cover-se,ymax=mean.cover+se), width=0.2, position=position_dodge(0.9),size=1) + 
  xlab(NULL) + 
  ylab("Mean percent cover\n") + 
  scale_x_discrete(breaks=c("before","after"),labels=c("\n Before","\n After")) +
  theme_bw() + 
  theme(panel.border=element_blank(), 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=70)) +
  theme(axis.line=element_line(colour="black",size=1),
        axis.text.y=element_text(size=50), 
        axis.title.y=element_text(size=70), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(colour="black",size=2),
        axis.ticks.length=unit(1,"lines"),
        plot.margin=unit(c(3,5,5,5),"lines"))


# Plot together #
png(file="cascade.png",width=1100,height=3000)
multiplot(sun.plot,gu.plot,kelp.plot,cols=1)
dev.off()

## Data analysis ##

Mean % change in sea star abundance
sun.change<-tapply(sun.mean$sunflower.star,list(sun.mean$site,sun.mean$ssws), mean)
sun.change
sun.change<-data.frame(sun.change)
sun.change$percent.decline<-((sun.change$After - sun.change$Before)/sun.change$Before)*100  #formula for % change

is.nan.data.frame<-function(x)  #turn NaN's into 0's
  do.call(cbind,lapply(x,is.nan))  
sun.change[is.nan(sun.change)] <-0

mean(sun.change$percent.decline)
sd(sun.change$percent.decline)
min(sun.change$percent.decline)
max(sun.change$percent.decline)

#nlme on sea star counts before and after wasting event
transects<-read.csv("transectcounts.csv")
library(reshape2)
transect<-melt(transects,id=c("ssws","site","transect")) #long form
transectss<-transect[transect$variable=="sunflower.star",] #only sunflower stars
transectss$logtx<-log(1+transectss$value) #sslme with log-transformed data
library(nlme)
attach(transectss)
log.ss<-lme(logtx~ssws, random=~1|site,data=transectss) #random effect of site on the intercept
summary(log.ss)
#Diagnostics
plot(fitted(log.ss),residuals(log.ss))
hist(residuals(log.ss))
qqnorm(residuals(log.ss))

#nlme on green urchin counts before and after wasting event
transects<-read.csv("transectcounts.csv")
library(reshape2)
transect<-melt(transects,id=c("ssws","site","transect")) #long form
transectgu<-transect[transect$variable=="green.urchin",] #only green urchins
head(transectgu)
transectgu$logtx<-log(1+transectgu$value) #sslme with log-transformed data
library(nlme)
library(nlme)
attach(transectgu)
log.gu<-lme(logtx~ssws, random=~1|site,data=transectgu) #random effect of site on the intercept
log.gu
summary(log.gu)
#Diagnostics
plot(fitted(log.gu),residuals(log.gu))
hist(residuals(log.gu))
qqnorm(residuals(log.gu))

#LME of kelp cover before and after
kelp<-read.csv("kelpbytransect.csv")
head(kelp)
library(nlme)
algae.lme<-lme(percent.cover~mortality, random=~1|site,data=kelp) #random effect of site on the intercept
summary(algae.lme)
algae.lme
tapply(kelp$percent.cover,list(kelp$mortality),mean)
tapply(kelp$percent.cover,list(kelp$mortality),sd)

#### Figure 4 (relative change in sunflower stars, green urchins and kelp at each site) ####

sites<-read.csv("abundance3.csv") 
head(sites)
sites
library(ggplot2)
library(grid)

sites$site.no.bydate<-as.factor(sites$site.no.bydate)
labs<- expression(italic("   P. helianthoides "), italic("   S.droebachiensis "), 'Kelp species ')

pts<-ggplot(sites,aes(x=site.no.bydate, y =rd,colour = species, fill=fill.col,shape=species)) +
  geom_point(size=15,
             position=position_dodge(width=.5),
             aes(colour=factor(species), 
                 shape=factor(species),
                 fill=factor(fill.col))) + #position=position_jitter(w=.2 ,h=0)
  
  scale_shape_manual(breaks=c("sunflower.star","green.urchin","kelp"),
                     labels=labs,
                     values=c(21,22,24),
                     guide="legend")+
  scale_colour_manual(breaks=c("sunflower.star","green.urchin","kelp"),
                      labels=labs,
                      values=c("chartreuse4","firebrick2","darkslateblue"),
                      guide="none") +
  
  scale_fill_manual(breaks=c("blank", "sunflower.star","green.urchin","kelp"),
                    values=c("white","chartreuse4","firebrick2","darkslateblue"),
                    guide="none") +
  guides(color=guide_legend(override.aes = list(fill=c("darkslateblue", "chartreuse4","firebrick2"),size=20))) +
  
  geom_hline(yintercept=0,linetype="solid",colour="black",size=2) +
  geom_hline(yintercept=c(-2,-1,1,2),linetype="dashed",colour="black",size=2) +
  theme(panel.background=element_rect(fill="white"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour="grey",size=2),
        panel.grid.minor.y=element_line(colour="grey",size=2),
        panel.border=element_blank(),
        axis.line=element_line(colour="black",size=2),
        axis.text=element_text(size=50), 
        axis.title=element_text(size=60), 
        axis.ticks.y=element_line(colour="black",size=2),
        axis.ticks.length=unit(1,"lines"),
        plot.margin=unit(c(5,6,5,5),"lines")) +
  scale_x_continuous(breaks=seq(1,20,1),minor_breaks=seq(1,20,1),expand = c(0.01,0.01)) +
  labs(x="Site number \n",y="\n Relative difference in abundance \n") +
  theme(legend.title=element_blank(),legend.key = element_blank(),
        legend.text=element_text(size=50)) +
  
  coord_flip() 


png(file="TrophicCascade.png",width=2700,height=3000)
pts
grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 8))
dev.off()

#### Figure 5 (REEF time series) ####

#REEF data obtained from www.reef.org, 19 Nov 14
reef<-read.csv("ech_pacnw_w014.csv")
head(reef)

#avg abundance for each date (so that there is no more than one abundance value per date)
reef1<-aggregate(reef$Abundance,list(reef$Date,reef$Species),FUN=mean,data=reef)
colnames(reef1) <-c("date","species","abundance")
reef1$date<-as.numeric(as.Date(reef1$date))
reef1<-reef1[which(reef1$date>=14610),] #starting 2010-01-01
head(reef1)
min(reef1$date)
max(reef1$date)

## SUNFLOWER STARS ##
sun<-reef1[which(reef1$species=="173"),]
sun<-sun[,c(1,3)] #rm species column
head(sun)
length(sun$date) #1568 surveys in Washington and BC

#convert into a 'regular' series of data
alldays<-seq(14610,16376,by=1) #create blank list of all dates from 2010-01-01 to 2014-11-02
full<-data.frame(Date=alldays,Abundance=with(sun,abundance[match(alldays,date)])) #merge the list of dates with the data to create a data set with NA's (instead of non-existent rows)
head(full)

# convert into a zoo ts object
library(zoo)
Sun<-na.approx(full) #convert NA's to values using a linear interpolation 
head(Sun)
sunzoo<-zoo(Sun,order.by=Sun[,1])
head(sunzoo) 
plot(sunzoo)

#using rollapply 
sunrollmean<-rollapply(sunzoo,60,mean) #60 day running average
plot(sunrollmean$Abundance~sunrollmean$Date)
head(sunrollmean)
sunrollsd<-rollapply(sunzoo,60,sd) #sd on 60 day running average
head(sunrollsd)
df<-data.frame(sunrollmean$Date,sunrollmean$Abundance,sunrollsd$Abundance)
colnames(df)<-c("date","mean","sd")
df$sd.min<-df$mean-df$sd
df$sd.max<-df$mean+df$sd
head(df)

## GREEN URCHINS ##
gu<-reef1[which(reef1$species=="175"),]
gu<-gu[,c(1,3)] #rm species column
head(gu)

#convert into a 'regular' series of data
alldays<-seq(14610,16376,by=1) #create blank list of all dates from 2010-01-01 to 2014-11-02
full2<-data.frame(Date=alldays,Abundance=with(gu,abundance[match(alldays,date)])) #merge the list of dates with the data to create a data set with NA's (instead of non-existent rows)
head(full2)

library(zoo)
GU<-na.approx(full2) #convert NA's to values using a linear interpolation 
str(GU)
head(GU)

# convert into a zoo ts object
guzoo<-zoo(GU,order.by=GU[,1])
head(guzoo) 
gurollmean<-rollapply(guzoo,60,mean) #60 day running average
gurollsd<-rollapply(guzoo,60,sd)

date<-as.Date(as.numeric(time(gurollmean$Date)))
length(date)

tsdf<-data.frame(date=date,
                 sun=sunrollmean$Abundance,
                 gu=gurollmean$Abundance,
                 sun.sd=sunrollsd$Abundance,
                 gu.sd=gurollsd$Abundance)
head(tsdf)
tsdf$sun.sd.min<-tsdf$sun-tsdf$sun.sd
tsdf$sun.sd.max<-tsdf$sun+tsdf$sun.sd
tsdf$sun.se.min<-tsdf$sun-(tsdf$sun.sd/sqrt(60))
tsdf$sun.se.max<-tsdf$sun+(tsdf$sun.sd/sqrt(60))
tsdf$sun.ci.min<-tsdf$sun-(1.96*tsdf$sun.sd/sqrt(60))
tsdf$sun.ci.max<-tsdf$sun+(1.96*tsdf$sun.sd/sqrt(60))
tsdf$gu.sd.min<-tsdf$gu-tsdf$gu.sd
tsdf$gu.sd.max<-tsdf$gu+tsdf$gu.sd
tsdf$gu.se.min<-tsdf$gu-(tsdf$gu.sd/sqrt(60))
tsdf$gu.se.max<-tsdf$gu+(tsdf$gu.sd/sqrt(60))
tsdf$gu.ci.min<-tsdf$gu-(1.96*tsdf$gu.sd/sqrt(60))
tsdf$gu.ci.max<-tsdf$gu+(1.96*tsdf$gu.sd/sqrt(60))
head(tsdf)
tail(tsdf)
write.csv(tsdf,"time.series.df2.csv")

## Start here to make figure for MS ##
tsdf<-read.csv("time.series.df2.csv") #df2 has variance data

### Plot both spp together
library(ggplot2)
library(grid)

as.numeric(as.Date("2013-06-07")) #Earliest recorded sigting of ssws = 15863

tsplot<-ggplot(data=tsdf, aes(x=date),scale=continuous) + 
  geom_line(aes(y = sun, colour = "sun", linetype="sun"),size=2) + 
  geom_line(aes(y = gu, colour = "gu", linetype="gu"),size=2) +
  geom_ribbon(aes(ymin=sun.ci.min,ymax=sun.ci.max),alpha=0.2) +
  geom_ribbon(aes(ymin=gu.ci.min,ymax=gu.ci.max),alpha=0.2) +
  geom_vline(aes(xintercept=15863), linetype=3, colour="red",size=3) +
  xlab(NULL) + 
  ylab("Abundance score") +
  theme_bw() + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black")) +
  theme(axis.text=element_text(size=70), axis.title=element_text(size=90),
        axis.line = element_line(colour = "black",size=2),
        axis.title.y=element_text(vjust=10),axis.text.x=element_text(vjust=5),
        axis.ticks=element_line(colour="black",size=2),
        axis.ticks.length=unit(3,"lines"),
        axis.ticks.margin=unit(4,"lines"),
        plot.margin=unit(c(10,30,10,10),"lines"))+  
  theme(legend.title = element_blank(),
        legend.key.size = unit(4,"cm"),
        legend.text=element_text(size=60),
        legend.position=c(1,0.9)) +
  scale_colour_manual(values=c("chartreuse4","darkslateblue"),breaks=c("gu","sun"),labels=c("Green urchins","Sunflower stars"))+
  scale_linetype_discrete(breaks=c("gu","sun"),labels=c("Green urchins","Sunflower stars")) +
  ylim(0,3) +
  xlim(as.Date("2010-01-01"),as.Date("2015-01-01"))

# add text for dotted line
my_grob<- grobTree(textGrob("Sea star wasting \n  first observed",x=0.4,y=0.5,hjust=0,gp=gpar(col="black",fontsize=65)))

png(file="Figure3 (high res wCI).png", width=3200, height=2000)
tsplot + annotation_custom(my_grob) 
dev.off()

## %SF for sunflower stars before vs. after ##
SF<-function(data,x){
  
  f<-length(which(x!="0"))
  s<-length(x)
  SF<-f/s
  
  return(SF)
}

sun$ssws<-ifelse(sun$date>=15863,"after","before")
aggregate(x=sun$abundance, by=list(sun$ssws), data=sun, FUN=SF) #98% before, 89%after
