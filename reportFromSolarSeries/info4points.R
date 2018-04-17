library(reshape)
library(lubridate)
library(ggplot2)
#library(gplots)
library(RColorBrewer)
library(scales)
## DEPENDENCIES
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if(!is.installed("ncdf"))   {install.packages("ncdf"    ,repos="http://cran.r-project.org")}
suppressPackageStartupMessages(library("ncdf"))

##ARGS
args = commandArgs(trailingOnly=TRUE)
tag=args[1]
var=args[2]
tag2=var
#########################################     DEFINE       
metrics=c("min","max","mean","somedays")
metrics=c("mean","min","max")

########################################      #clean files
numfiles=(length(args)-2)/4
files=data.frame(filenames=character(numfiles),filetypes=character(numfiles))
nrow=as.numeric(1)
filetypes=c()
filenames=c()
filevars=c()
filetags=c()
while (nrow <= numfiles){
	originalfilename=args[nrow*4-1]
	filetype=as.character(args[nrow*4])
	filevar=as.character(args[nrow*4+1])
	filetag=as.character(args[nrow*4+2])
	filetypes=c(filetypes,filetype)
	filevars=c(filevars,filevar)
	filetags=c(filetags,filetag)
	filename=as.character(paste(filetype,"-",filetag,".txt",sep=""))
	system(paste("cp \"",originalfilename,"\" tmp/",filename,sep=""))
	if (filetype =="serie" | filetype == "reanalysis"){
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
		system(paste("sed -i 's/  / /g' tmp/",filename,sep=""))
	}
	filenames=c(filenames,filename)
	nrow=nrow+1
}
files=data.frame(filenames,filetypes,filevars,filetags,stringsAsFactors= FALSE)

###################################################################
###################################################################
#################     FUNCTIONS      ##############################
###################################################################
###################################################################
########################  FUNCTION READ FILE
readFile <- function(file,type,var){
	if (type == "obs"){
		obs<-read.csv(paste("tmp/",file,sep=""),sep=" ",skip=2)
		if (var == "ghi"){
                        colnames(obs)[3]=c("ghi")
                }
                if (var == "dni"){
                        colnames(obs)[4]=c("ghi")
                }
                if (var == "dhi"){
                        colnames(obs)[5]=c("ghi")
                }
		colnames(obs)[1:2]<-c("date","time")
		obs$min<-substr(obs$time,nchar(obs$time)-1,nchar(obs$time))
		obs$hour=sapply(obs$time,function(x){if (nchar(x)>3){paste(substr(x,0,2))}else{paste(substr(x,0,1))}})
		obs$month=sapply(obs$date,function(x){paste(substr(x,5,6))})
		obs$month=as.numeric(obs$month)
		obs$min=as.numeric(obs$min)
		obs$min=as.numeric(00)
		
		obs$hour=as.numeric(obs$hour)
		obs$TIME<-as.POSIXct(paste(obs$date," ",obs$hour,":",obs$min,sep=""),format="%Y%m%d %H:%M")
#	print(obs$ghi=="999.00")
#	obs[obs$ghi<0 | obs$ghi==999,'ghi']<-NA
#In case sub hourly values
		obs$ghi=as.numeric(obs$ghi)
		obsagg=aggregate(ghi~date + hour + month,data=obs,FUN=mean,na.ommit=TRUE)
#		obsagg[is.na(obsagg$ghi),"ghi"]<-999
		obsagg$TIME<-as.POSIXct(paste(obsagg$date," ",obsagg$hour,sep=""),format="%Y%m%d %H")
		data=obsagg[,c("TIME","ghi")]
	}
	if (type =="tmy"){
		tmyheader<-read.csv(paste("tmp/",file,sep=""),sep=";",skip=12,header=FALSE,nrows=1,colClasses=rep("character",1,12))
		tmy<-read.csv(paste("tmp/",file,sep=""),sep=";",skip=14,header=FALSE)
		colnames(tmy)=c(tmyheader[1,1:11])
		if (var == "ghi"){
			colnames(tmy)[6]=c("ghi")
		}
		if (var == "dni"){
			colnames(tmy)[7]=c("ghi")
		}
		if (var == "dhi"){
			colnames(tmy)[8]=c("ghi")
		}

		colnames(tmy)[4]=c("hour")
		colnames(tmy)[2]=c("month")
		tmy$date<-paste(tmy$Year,sprintf("%02d",tmy$month),sprintf("%02d",tmy$Day),sep="")
		tmy$TIME<-as.POSIXct(paste(tmy$date," ",tmy$hour,sep=""),format="%Y%m%d %H")
		data=tmy[,c("TIME","ghi")]
#	data[data$ghi<0 | data$ghi==999 ,"ghi"]<-NA
	}
	if (type=="serie"){
		serie<-read.csv(paste("tmp/",file,sep=""),sep=" ",skip=3)
		colnames(serie)[1:2]<-c("date","time")
		if (var == "ghi"){
			colnames(serie)[3]<-c("ghi")
		}
		if (var == "dni"){
			colnames(serie)[4]<-c("ghi")
		}
		if (var == "dhi"){
			colnames(serie)[5]<-c("ghi")
		}
		if (var == "wspd"){
			colnames(serie)[7]<-c("ghi")
		}
		if (var == "T"){
			colnames(serie)[8]<-c("ghi")
		}
		serie$hour=sapply(serie$time,function(x){if (nchar(x)>3){paste(substr(x,0,2))}else{paste(substr(x,0,1))}})
		serie$min<-substr(serie$time,nchar(serie$time)-1,nchar(serie$time))
		serie$month<-substr(serie$date,nchar(serie$date)-3,nchar(serie$date)-2)
		serie$TIME<-as.POSIXct(paste(serie$date," ",serie$hour,":",serie$min,sep=""),format="%Y%m%d %H:%M")
		data=serie[,c("TIME","ghi")]
		data[data$ghi<0 | data$ghi==999,"ghi"]<-NA
	}
	if (type=="reanalysis"){
		serie<-read.csv(paste("tmp/",file,sep=""),sep=" ",skip=3)
		colnames(serie)[1:2]<-c("date","time")
		if (var == "ghi"){
			colnames(serie)[3]<-c("ghi")
		}
		if (var == "dni"){
			colnames(serie)[4]<-c("ghi")
		}
		if (var == "dhi"){
			colnames(serie)[5]<-c("ghi")
		}
#reanalysis data!!!
		if (var == "wspd"){
			colnames(serie)[3]<-c("ghi")
		}
		if (var == "T"){
			colnames(serie)[7]<-c("ghi")
		}
		serie$hour=sapply(serie$time,function(x){if (nchar(x)>3){paste(substr(x,0,2))}else{paste(substr(x,0,1))}})
		serie$min<-substr(serie$time,nchar(serie$time)-1,nchar(serie$time))
		serie$month<-substr(serie$date,nchar(serie$date)-3,nchar(serie$date)-2)
		serie$TIME<-as.POSIXct(paste(serie$date," ",serie$hour,":",serie$min,sep=""),format="%Y%m%d %H:%M")
		data=serie[,c("TIME","ghi")]
		data[data$ghi<0 | data$ghi==999,"ghi"]<-NA
	}
	if (type=="nc"){
		ncid     <- open.ncdf(paste("tmp/",file,sep=""))
		ntime <- ncid$dim$time$len ; tvar <- get.var.ncdf(ncid,"time") ; origin <- ncid$dim$time$units ; tunits <- strsplit(ncid$dim$time$units,split=" ")
		if (tunits[[1]][1] == "hours"){
			time.dta <- data.frame(TIME=strptime(paste(tunits[[1]][3],paste(tunits[[1]][4],"00",sep=":"), sep=" "),"%Y-%m-%d %H:%M:%S",tz="GMT") +  as.numeric(as.difftime(tvar,units=tunits[[1]][1]),units="secs"))
		}
		if (tunits[[1]][1] == "seconds"){
			time.dta <- data.frame(TIME=strptime(paste(tunits[[1]][3],paste(tunits[[1]][4],"00",sep=":"), sep=" "),"%Y-%m-%d %H:%M:%S",tz="GMT") +  tvar)
		}
		nvars = ncid$nvars ; var.nms <- c() ; var.lst <- list() ; var.units <- c()
		for(ivar in 1:nvars){
			var.units[ivar] <- ncid$var[[ivar]]$units
			var.nms[ivar]   <- ncid$var[[ivar]]$name
			var.lst[[ivar]] <- get.var.ncdf(ncid, var.nms[ivar])
			if (ncid$var[[ivar]]$name == var){
				selectRow=ivar
			}
		}
		var.dtf <- as.data.frame(var.lst) ; names(var.dtf) <- var.nms ; vortex.dtf <- cbind(time.dta,var.dtf)
		Var.lon <- ncid$var[[1]]$dim[[1]]$vals ; Var.lat <- ncid$var[[1]]$dim[[2]]$vals ; Var.lev <- ncid$var[[1]]$dim[[3]]$vals
		Var.time <- get.var.ncdf(ncid,"time") ; Var.tunits <- att.get.ncdf(ncid,"time",attname="units")    
		close.ncdf(ncid)
		data=vortex.dtf[,c("TIME",var)]
		colnames(data)=c("TIME","ghi")
#    return(list(dta=vortex.dtf,lev=Var.lev,latlon=c(Var.lat,Var.lon), time=Var.time, tunits=Var.tunits, var.units=var.units))
	}
	return(data)
}

########################  FUNCTION READ FILE   END
########################  FUNCTION STATS   START
calculateStats <- function(df1,df2){
	df1$year=year(df1$TIME)
	df1$month=month(df1$TIME)
	df1$day=day(df1$TIME)
	df1$hour=hour(df1$TIME)
	df2$year=year(df2$TIME)
	df2$month=month(df2$TIME)
	df2$day=day(df2$TIME)
	df2$hour=hour(df2$TIME)
	df=merge(df1,df2,by=c("hour","day","month","year"),na.rm=TRUE)
	ydfx=aggregate(value.x~year,data=df,FUN=sum,na.rm=TRUE)
	mdfx=aggregate(value.x~month+year,data=df,FUN=sum,na.rm=TRUE)
	ddfx=aggregate(value.x~day+month+year,data=df,FUN=sum,na.rm=TRUE)
	ydfy=aggregate(value.y~year,data=df,FUN=sum,na.rm=TRUE)
	mdfy=aggregate(value.y~month+year,data=df,FUN=sum,na.rm=TRUE)
	ddfy=aggregate(value.y~day+month+year,data=df,FUN=sum,na.rm=TRUE)
	hstats=gmetric(df$value.x,df$value.y)
	dstats=gmetric(ddfx$value.x,ddfy$value.y)
	mstats=gmetric(mdfx$value.x,mdfy$value.y)
	ystats=gmetric(ydfx$value.x,ydfy$value.y)
	allstats=c(hstats,dstats,mstats,ystats)
	return(allstats)
}

gmetric <- function(v1,v2){
	verror=v1-v2
	error=mean(verror)
	avg=(mean(v2)+mean(v2))/2
	rerror=100*mean(error)/avg
	rrmse=100*sqrt(mean(verror*verror))/avg
	rmae=100*mean(abs(verror))/avg
	rdos=summary(lm(v1 ~ v2,singular.ok=TRUE))$r.squared
	vect=c(error,rerror,rrmse,rmae,rdos)
	return(vect)
}

########################  FUNCTION STATS   END

###################################################################
##############  READ AND AGGREGATE   ##############################
###################################################################
#set data frame melt
listseries=list()
nfile=0
nmetric=0
nlines=c(1:length(files[,1]))
for (nline in nlines){
	nordinal=nfile+1
#each file read and aggregates
	line=as.character(files$filenames[nline])
	type=as.character(files$filetypes[nline])
	var=as.character(files$filevars[nline])
	filetag=as.character(files$filetags[nline])
	print(paste(filetag,var,line))
	tmpdata=NULL
	tmpdata=readFile(line,type,var)
	tmpdata$monthhour=paste(sprintf("%02d",as.numeric(month(tmpdata$TIME))),sprintf("%02d",as.numeric(hour(tmpdata$TIME))),sep="")
	tmpdata$jday=as.numeric(yday(tmpdata$TIME))
	tmpdata$Year=as.numeric(year(tmpdata$TIME))
	tmpdata$month=as.numeric(month(tmpdata$TIME))
	tmpdata$ghi=ifelse(!is.na(tmpdata$ghi) & tmpdata$ghi==999,NA,tmpdata$ghi)
	tmpdata$ghi=ifelse(!is.na(tmpdata$ghi) & tmpdata$ghi>1380,NA,tmpdata$ghi)
	tmpdata$ghi=ifelse(!is.na(tmpdata$ghi) & tmpdata$ghi==0,NA,tmpdata$ghi)
	tmpdata$ghi=ifelse(!is.na(tmpdata$ghi) & tmpdata$ghi<2,NA,tmpdata$ghi)
	tmpdatad=tmpdata[!is.na(tmpdata$ghi), ]
	tmpdata=tmpdatad
#FOR on year hourly data print
	serie<-tmpdata
	serie$Year=as.numeric(serie$Year)
	serie$hour<-hour(serie$TIME)
	serie$month<-month(serie$TIME)
	serie$ghi<-as.numeric(serie$ghi)
        selectedyears=unique(serie$Year)
        selectedyearsreduced=as.numeric(selectedyears)
	if (type == "tmy"){serie$Year = 2059}
	if (type == "serie" | type == "reanalysis"){# get last 10 years
		nyears=length(unique(serie$Year))
		#LIMIT NUMBER OF YEARS
#		selectedyearsreduced=as.numeric(unique(serie$Year)[(nyears-9):nyears])
		selectedyears=unique(serie$Year)
		serie<-serie[serie$Year %in%  selectedyears,]
	}
	listseries[[nordinal]]=data.frame(serie$TIME,serie$ghi)
	colnames(listseries[[nordinal]])=c("TIME","value")
	serie=data.frame(serie$Year,serie$month,serie$hour,serie$jday,serie[,2],font=paste(filetag))
#select period by year
	colnames(serie)=c("Year","month","hour","jday","value","font")

	if (nfile ==0){serieall=serie}
	else{
		seriealltmp=rbind(serieall,serie)
		serieall=seriealltmp
	}
	for (metric in metrics){
		tmp=NULL
		tmp<-aggregate(ghi~monthhour,data=tmpdata,FUN=metric,na.rm=TRUE)
		colnames(tmp)[2]=c("value")
		tmp$month=substr(tmp$monthhour,0,2)
		tmp$hour=substr(tmp$monthhour,3,4)
		dptmp=data.frame(tmp$month,tmp$hour,tmp$value,paste(filetag),paste(metric))
		colnames(dptmp)=c("month","hour","value","font","metric")
		if(nmetric==0){
			meltall=dptmp
		}
		else{
			meltalltmp=rbind(meltall,dptmp)
			meltall=meltalltmp
		}
		nmetric=nmetric+1
	}
	nfile=nfile+1
}
##clean FILES

for (file in filenames){
	system(paste("rm tmp/",file,sep=""))
}
#######################################
#############  END READ   #############
#######################################
## METRICS / STATISTICS
print("CHECK 1 ############################################################")
n=1
nn=0
nlines=length(files[,1])+1
print(files)
print(nlines)
sink(paste('output-txt/stats-',tag,'-',tag2,'.txt',sep=""),append = TRUE)
print(paste("tag;tag2;file.x;file.y;var.x;var.y;h error","h rerror","h rrmse","h rmae","h rdos","d error","d rerror","d rrmse","d rmae","d rdos","m error","m rerror","m rrmse","m rmae","m rdos","y error","y rerror","y rrmse","y rmae","y rdos",sep=";"))
while (n < nlines){
	nn=n+1
	nline=n
	line=as.character(files$filenames[nline])
        type=as.character(files$filetypes[nline])
        var=as.character(files$filevars[nline])
        filetag=as.character(files$filetags[nline])
        while (nn < nlines){
		nline=nn
	        lined=as.character(files$filenames[nline])
        	typed=as.character(files$filetypes[nline])
	        vard=as.character(files$filevars[nline])
		filetagd=as.character(files$filetags[nline])
		if (typed == "obs" || vard =="GHI_real" || vard =="DNI_real" || vard =="GHI" || vard =="DNI"){
		    	stats=calculateStats(listseries[[n]],listseries[[nn]])
			txt=paste(tag,tag2,line,lined,var,vard,sep=";")
			for (a in stats){
                		txt=paste(txt,a,sep=";")
			}
			print(paste(txt))
		}
		nn=nn+1
	}
	n=n+1
}

sink()
##clean output stats file:
system(paste("sed -i 's/\\[1\\]\\ //g'",paste('output-txt/stats-',tag,'-',tag2,'.txt',sep="")))
system(paste("sed -i 's/\"//g'",paste('output-txt/stats-',tag,'-',tag2,'.txt',sep="")))
#aggregates 

#END read files and melt and aggregate

#COMMON melted data frame clean
#hourly averaged
meltall$hour=as.integer(meltall$hour)
meltall$metric=as.character(meltall$metric)
meltall$month=as.integer(meltall$month)
meltall$value=ifelse(!is.na(meltall$value) & meltall$value==999,NA,meltall$value)
#meltall$value=ifelse(!is.na(meltall$value) & meltall$value>1380,NA,meltall$value)
meltall$value=ifelse(!is.na(meltall$value) & meltall$value>1600,NA,meltall$value)
meltall$value=ifelse(!is.na(meltall$value) & meltall$value==0,NA,meltall$value)
meltall$value=ifelse(!is.na(meltall$value) & meltall$value<0,NA,meltall$value)
#meltall$value=ifelse(!is.na(meltall$value) & meltall$value<20,NA,meltall$value)
meltalld=meltall[!is.na(meltall$value), ]
meltall=meltalld

serieall$value=ifelse(!is.na(serieall$value) & serieall$value==999,NA,serieall$value)
#serieall$value=ifelse(!is.na(serieall$value) & serieall$value>1380,NA,serieall$value)
serieall$value=ifelse(!is.na(serieall$value) & serieall$value==0,NA,serieall$value)
#serieall$value=ifelse(!is.na(serieall$value) & serieall$value<20,NA,serieall$value)
serieall$value=ifelse(!is.na(serieall$value) & serieall$value<0,NA,serieall$value)
seriealld=serieall[!is.na(serieall$value),]
serieall=seriealld
serieall$dayhour=as.numeric(paste(sprintf("%03d",serieall$jday),sprintf("%02d",serieall$hour),sep=""))

monthserie=aggregate(value~Year+font,data=serieall,FUN=sum,na.rm=TRUE)
monthserie$value=monthserie$value/1000;
dailyprofile=aggregate(value~hour+font,data=serieall,FUN=mean,na.rm=TRUE)

## TODO MAKE DIFFERENCES
##################################################
#  bind matching days
##################################################
getCommonDays <- function(){
fonts=unique(serieall$font)
listbyfonts=list(length(fonts))
for (i in 1:length(fonts)){
	listbyfonts[[i]]=data.frame(serieall[serieall$font==fonts[i],])
	if (i == 1){#init merged df
		dfmerged=listbyfonts[[i]]
		colnames(dfmerged)[5]=as.character(fonts[i])
	}
	if (i >1){
		dftmp=merge(dfmerged,listbyfonts[[i]],by=c("Year","month","hour","jday","dayhour"))
		dfmerged=dftmp
		posvaluecol=6+2*(i-1)
		colnames(dfmerged)[posvaluecol]=as.character(fonts[i])
		nowantthiscol <- c("font.y","font.x")
	}
}
nowantthiscol <- c("font")
dfmerged<-dfmerged[,! names(dfmerged) %in% nowantthiscol, drop = F]
mergedmelted=melt(dfmerged,id=c("Year","month","hour","jday","dayhour"))

mergedmelted$datetime=as.POSIXct(paste(mergedmelted$Year,mergedmelted$jday,mergedmelted$hour,sep=" "),format="%Y %j %H",TZ="UTC")
mm=mergedmelted[order(mergedmelted$datetime,decreasing=FALSE),]
mm$day=day(mergedmelted$datetime)
mergedmelted=mm

serieall$day=day(as.POSIXct(paste(serieall$Year,serieall$month,serieall$jday,serieall$hour,sep=" "),format="%Y %j %H",TZ="UTC"))
}
##################################################
#FITCHNER, compare 1 year with obs

#serieall=serieall[serieall$Year == 2015,]
#meltall=meltall[meltall$Year == 2015,]

#series plot
#scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1200,1200,900,600,300,1))

#some days
varratio=0.001*length(unique(selectedyearsreduced))/(length(files[,1]))
serieall$yearjday=paste(serieall$Year,serieall$jday,sep="")
serieall$yearjdayhour=paste(serieall$Year,serieall$jday,serieall$hour,sep="")
somedays<-ggplot(serieall[serieall$jday %in% c(1:14),]) + geom_line(aes(hour,value,color=yearjday,group=yearjday)) +theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=0,family="mono")) + ggtitle(paste("Some days series comparison for site ",tag," and var ",var))+facet_wrap(~month)
somedays<-ggplot(serieall[serieall$jday %in% c(32:40),]) + geom_line(aes(hour,value,color=font,group=font)) + ggtitle(paste("Some days series comparison for site ",tag," and var ",var))+facet_wrap(~jday)
#somedays<-ggplot(mergedmelted) + geom_line(aes(hour,value,color=jday,group=jday)) +theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=0,family="mono")) + ggtitle(paste("Series comparison for site ",tag," and var ",var))+facet_wrap(variable~month)
#+ coord_fixed(ratio = varratio)
#+facet_grid(jday~month)

#lastyear=mergedmelted$Year[length(mergedmelted$Year)]-1
#somedays<-ggplot(mergedmelted[mergedmelted$Year == lastyear & mergedmelted$month %in% c(1,3,5,7,9,11) & mergedmelted$day %in% c(1,5,10,15,20,25),])+geom_line(aes(hour,value,group=factor(variable),color=factor(variable)))+facet_grid(month~day)+ggtitle(paste("Some days for ",tag,"&",tag2,"and year ",lastyear))

#HEATMAP SERIE
varratio=1.2*length(unique(serieall$Year))*20/(length(files[,1]))
summary(serieall)
heatmap<-ggplot(serieall)+geom_raster(aes(jday,hour,fill=value)) + scale_fill_gradientn(colours = c("lightblue","blue","green","yellow","orange","red","purple"),breaks=c(50,250,450,650,850,1050,1250))+theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = varratio) +ggtitle(paste("Series comparison for site ",tag,tag2," and var ",var))+facet_grid(font~Year)
print(paste0("SERIEALL"))
head(serieall)
heatmapYears<-ggplot(serieall)+geom_raster(aes(jday,hour,fill=value)) + scale_fill_gradientn(colours = c("lightblue","blue","green","yellow","orange","red","purple"),breaks=c(50,250,450,650,850,1050,1250))+theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = varratio/5) +ggtitle(paste("Series comparison for site ",tag,tag2," and var ",var))+facet_grid(font~Year)
#OLD SCHOOL
#serieclassic<-ggplot(serieall)+geom_line(aes(hour,value,group=factor(font),color=factor(font))) +theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=0,family="mono")) +ggtitle(paste("Series comparison for site ",tag,tag2," and var ",var))+facet_wrap(~jday)
#HEATMAP "ONE YEAR SUMMARY"
varratio=0.77*length(files[,1])/2
plotc<-ggplot(meltall)+ geom_raster(aes(month,hour,fill=value)) + scale_fill_gradientn(colours = c("lightblue","blue","green","yellow","orange","red","purple"),breaks=c(50,250,450,650,850,1050,1250))+ theme(legend.position="right", axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = varratio) +ggtitle(paste("Hourly aggregateds by Month:",var,tag,tag2))+facet_grid(metric~font)+scale_x_discrete(limits=c(rep(1:12,each =1)))
#metrics line
#OLD SCHOOL:
#aggregateline<-ggplot(meltall,aes(hour,value,group=font,color=font))+geom_line()+theme(legend.position="right", axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = 0.1) +ggtitle(paste("Hourly aggregateds by Month:",var,tag))+facet_grid(metric~month)+scale_x_discrete(limits=c(5,10,15,20))
#line plot, all months saily ptofile together
#NEW SCHOOL     
numyears=length(unique(monthserie$Year))
varratio=0.003*numyears/(2*4)
a=numyears-1
#monthserie=monthserie[monthserie$value > mean(monthserie$value)*0.88,]
monthserie$font=as.factor(monthserie$font)

plotyearlyseries=ggplot(monthserie) + geom_bar(aes(x=Year,y=value,fill=font),alpha=0.8,stat="identity",position="dodge") +theme(legend.position="right", axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = varratio) +ggtitle(paste("Year sum ",tag,tag2)) 
#scale_y_continuous(limits=c(min(monthserie$value)-50,max(monthserie$value)+20), oob = rescale_none)
#scale_y_continuous(limits=c(min(monthserie$value)-50,max(monthserie$value)+20), oob = rescale_none)
print(paste("MONTHSERIE"))
print(summary(monthserie))

#DAILY PROFILES BY MONTH 
aggregateline<-ggplot(meltall[meltall$metric=="mean",],aes(hour,value,group=factor(font),color=factor(font)))+geom_line()+theme(legend.position="right", axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = 0.008) +ggtitle(paste("Hourly aggregateds by Month:",var,tag,tag2))+facet_wrap(~ month,ncol=3)
#+scale_x_discrete(limits=c(5,10,15,20))

paste0("DAILYPROFILE")
summary(dailyprofile)

plotdailyprofile<-ggplot(dailyprofile,aes(hour,value,group=font,color=font))+geom_line()+theme(legend.position="right", axis.text.x=element_text(angle=90, hjust=0,family="mono")) + coord_fixed(ratio = 0.02) +ggtitle(paste("Daily profile ",var,tag,tag2))
paste0("graphics generating...")

#save
pdf(file=paste("output-pdf/",tag,"-",tag2,".pdf",sep=""))
par(mfrow=c(1,3), oma=c(0,0,1,0))
#print("serieclassic")
#serieclassic
print("plotdailyprofile")
plotdailyprofile
print("plotyearlyseries")
plotyearlyseries
print("somedays")
somedays
print("aggregateline")
aggregateline
print("heatmap")
heatmap
#heatmapYears
print("plotc")
#plotc
#aggregateline
#	title(paste("Site",tag,"comparison for",filetags),outer=TRUE)
#dev.off(,tag2)
