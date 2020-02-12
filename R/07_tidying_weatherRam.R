
library(mgcv)
library(nlme)
library(ggeffects)
library(visreg)
library(scales)
library(zoo)
library(weathercan)
library(sp)
library(mapview)
library(mgcViz)
library(DHARMa)

### old data
#d<-read.csv("~/UdeS/Consultation/L-ARenaud/2018-10-31_daily_myenv.csv")
#d<-read.csv("2018-10-31_daily_myenv.csv")

### all nordegg stations
s<-stations_search("nordegg", interval = "day")
s

s<-as.data.frame(s)
coordinates(s)<-~lon+lat
proj4string(s)<-"+init=epsg:4326"

### location of RS seems approximate or rounded. One is 12m higjher than the second.
mapview(s,zcol="station_name")

### download nordegg cs
s<-stations_search("NORDEGG CS", interval = "day")
cs<-weather_dl(station_ids = s$station_id, start = "2000-01-01", end = "2020-01-01",interval="day")
cs<-as.data.frame(cs)

### download nordegg rs
s<-stations_search("NORDEGG RS", interval = "day")
rs<-weather_dl(station_ids = s$station_id, start = "1970-01-01", end = "2020-01-01",interval="day")
rs<-as.data.frame(rs)

### show both
plot(cs$date,cs$mean_temp,xlim=range(c(cs$date,rs$date)))
points(rs$date,rs$mean_temp,col="red")

### get overlapping dates
od<-intersect(cs$date,rs$date)

### show both for the overlap period
plot(cs$date,cs$mean_temp,xlim=range(od))
points(rs$date,rs$mean_temp,col="red")

### calculate temperature difference between the two and the mean difference
diff<-cs$mean_temp[cs$date%in%od]-rs$mean_temp[rs$date%in%od]
hist(diff,breaks=100)
offset<-mean(diff,na.rm=TRUE)
offset

### calculate temperature difference between the two and the mean difference
diff<-cs$total_precip[cs$date%in%od]-rs$total_precip[rs$date%in%od]
hist(diff,breaks=100)
offset<-mean(diff,na.rm=TRUE)
offset

### number of cases where rs has NA and cs has data (only 2)
table(is.na(cs$mean_temp[cs$date%in%od]) & !is.na(rs$mean_temp[rs$date%in%od]))

### merge both and keep CS over RS (do we add the offset for each variable?)
cs$station_historic<-"CS"
rs$station_historic<-"RS"
d<-rbind(cs,rs)
d<-d[order(d$date,d$station_historic),]
d<-d[!duplicated(d$date),]

### plot NA values along time
vars<-c("mean_temp","total_precip")
plot(d$date,is.na(d$mean_temp),ylim=c(0,1),yaxt="n",ylab="")
lapply(seq_along(vars),function(i){
  v<-vars[i]
  h<-seq(0,1,length.out=length(vars)+1)[i+1]
  points(d$date,ifelse(is.na(d[,v]),h,0))
  text(min(d$date),h,paste(v,"NAs"),adj=c(0,2),font=2,xpd=TRUE,col="red")
})

### lengths of runs of NA values for each variable
l<-lapply(vars,function(i){
  r<-rle(is.na(d[,i]))
  table(r[[1]][r[[2]]])  
})
names(l)<-vars
l

### add some values/columns
d$jul<-as.integer(format(d$date,"%j"))
d$day<-as.integer(d$date)
d$year<-as.integer(d$year)

#######################################################
### linear interpolation of daily temps 
plot(mean_temp~date,d)
k<-is.na(d$mean_temp)
d$mean_temp<-na.approx(d$mean_temp,na.rm=FALSE,maxgap=Inf)
plot(d$date,d$mean_temp)
points(d$date,d$mean_temp,col=ifelse(k,"red","black"),pch=ifelse(k,16,1))

########################################################
### interpolate precip using seasonal and yearly trend using a cyclic gam 
m<-gam(total_precip~s(jul,bs="cc")+year,data=d,family=tw())
g<-ggpredict(m)
plot(g,facet=TRUE,raw=TRUE)

### replace NA values
wNA<-is.na(d$total_precip)
dNA<-d[wNA,]
p<-predict(m,dNA,type="response")
d$total_precip[wNA]<-p

### and show them
par(mfrow=c(2,1))
plot(d$jul,d$total_precip,ylim=c(0,5))
points(dNA$jul,p,col="red",pch=16)
plot(d$date,d$total_precip)
lines(d$date,predict(m,d,type="response"),col="blue",lwd=2)
points(dNA$date,p,col="red",pch=16)

### check precip model with mgcViz as we use a tweedie distribution
sims<-t(simulate(m,100))
dsims<-createDHARMa(simulatedResponse = sims, observedResponse = model.frame(m)[,1],fittedPredictedResponse = predict(m),integerResponse=FALSE)
plot(dsims,quantreg=FALSE)


##############################################################
### compute monthly time-series

temp<-aggregate(mean_temp~month+year,data=d,mean)
precip<-aggregate(total_precip~month+year,data=d,sum)
dates<-as.Date(paste(temp$year,temp$month,"15",sep="-")) # assigne mid dates for visualization
temp$date<-dates 
precip$date<-dates

### show monthly time-series with naïve trends
par(mfrow=c(2,1))
plot(temp$date,temp$mean_temp,type="b",xaxt="n")
m<-lm(mean_temp~date,data=temp)
abline(m)
axis.Date(1,at=seq(min(temp$date),max(temp$date),by="quarter"),las=2,format="%b-%y")
plot(precip$date,precip$total_precip,type="b",xaxt="n")
m<-lm(total_precip~date,data=precip)
abline(m)
axis.Date(1,at=seq(min(temp$date),max(temp$date),by="quarter"),las=2,format="%b-%y")


monthlyRam<-merge(temp,precip)

write.csv(monthlyRam,"monthlyRam.csv",row.names=FALSE)





############################################################################
############################################################################
############################################################################
### below is code to extract residuals and variance of daily observations

##############################################################
### period for which we wish to summarize climate variables
period<-c("2010-11-15","2011-04-01")
g1<-grep(substr(period[1],5,10),d$date)
g2<-grep(substr(period[2],5,10),d$date)
g2<-g2[g2>min(g1)]
g1<-g1[1:length(g2)]
m<-mapply(":",g1,g2,SIMPLIFY=FALSE)
d$period<-0
for(i in seq_along(m)){
	d$period[m[[i]]]<-as.integer(substr(d$date[m[[i]]][length(m[[i]])],1,4))
}
# minimum date
mindate<-"1973-01-01"

d<-d[d$date>mindate & !is.na(d$mean_temp),]
### 

m1<-gamm(mean_temp~s(jul,bs="cc"),data=d)
m2<-gamm(mean_temp~s(jul,bs="cc"),data=d,correlation=corARMA(form=~1|year,p=1,q=0))
m3<-gamm(mean_temp~s(jul,bs="cc")+year,data=d,correlation=corARMA(form=~1|year,p=1,q=0))

year<-1980:2017
jul<-1:365
newdat<-expand.grid(jul=jul,year=year)
newdat$date<-as.Date(paste(newdat$year,newdat$jul,sep="-"),"%Y-%j")
p1<-predict(m1$gam,newdat)
plot(mean_temp~date,data=d,pch=ifelse(d$period>0,16,1),col=alpha("black",0.25))
axis.Date(1,at=pretty(d$date,100),las=2,format="%b-%Y")
lines(newdat$date,p1,col=alpha("black",0.5),lwd=5)
p2<-predict(m2$gam,newdat)
lines(newdat$date,p2,col=alpha("red",0.5),lwd=5)
p3<-predict(m3$gam,newdat)
lines(newdat$date,p3,col=alpha("blue",0.5),lwd=5)

d$mean_temp_pred<-predict(m2$gam)
d$mean_temp_resid<-resid(m2$gam)

x<-d[d$period>0,]
mean_a<-aggregate(mean_temp_resid~period,data=x,mean)
sd_a<-aggregate(mean_temp_resid~period,data=x,sd)

mid_period<-substr(as.Date(period[1])+as.integer(diff(as.Date(period))/2),6,10)

points(as.Date(paste(mean_a$period,mid_period,sep="-")),mean_a$mean_temp_resid,cex=5)
points(as.Date(paste(sd_a$period,mid_period,sep="-")),sd_a$mean_temp_resid,cex=5,pch=3)

plot(mean_a$mean_temp_resid,sd_a$mean_temp_resid)
cor(mean_a$mean_temp_resid,sd_a$mean_temp_resid)

#g<-ggpredict(m2,terms="year",condition=c(jul=30))
#plot(g,facet=TRUE,raw=TRUE)

#g<-ggpredict(m2,terms=c("jul","year"))
#plot(g,facet=TRUE,raw=TRUE)

#g<-ggpredict(m2,terms=c("jul","year [1975,2015]"))
#plot(g,facet=TRUE,raw=TRUE)






