#SCRIPT TO COMPARE R2 vs AVGcoefs for EDF

dirs=seq(from=7.5,to=352.5,by=15)
metmasts=read.csv(paste("metmasts.txt",sep="/"),colClasses=c("character"),header=F)
str(metmasts)
N=nrow(metmasts)

#sample for df definition
tmp=read.csv(paste("series/",metmasts[1,1],sep=""),sep=" ")
numberofrows=nrow(tmp)

#create dataframe with series DIr,M1,M2,...,Mn
dfseries=data.frame(matrix(ncol=N+1,nrow=numberofrows))
colnames(dfseries)=c(metmasts[,1],"D")

for (n in seq(1,N)){
	data=read.csv(paste("series/",metmasts[n,1],sep=""),sep=" ")
	if ( n == 1){
		dfseries[,"D"]=data[,4]
	}
	dfseries[,n]=data[,3]

}

#NOW construct means

dfmeans=data.frame(matrix(ncol=N,nrow=25))
colnames(dfmeans)=as.vector(metmasts[,1])
dfcorrs=data.frame(matrix(ncol=N,nrow=25))
colnames(dfcorrs)=as.vector(metmasts[,1])

numberrows=((N)*(N-1))/2
print(paste("Number of rows:",numberrows))
tblmeans=matrix(ncol=26,nrow=numberrows)
tblcorrs=matrix(ncol=26,nrow=numberrows)
tblslopes=matrix(ncol=26,nrow=numberrows)
frequencies=vector('numeric')

kk=1
i=1

nums=seq(from=1,to=N-1)
for (numinitiation in c(nums)){
        i=i+1
        numstarget=seq(i,N)
        for (numtarget in c(numstarget)){
		#AVERAGE
		#name
                tblmeans[kk,1]=paste(metmasts[numtarget,1],metmasts[numinitiation,1],sep="/")
		#all period
		tblmeans[kk,26]=as.numeric(mean(dfseries[,numtarget]))/as.numeric(mean(dfseries[,numinitiation]))
		timesteps=nrow(dfseries)
		frequencies[25]=1
		#each sector
		tblmeans[kk,2]=as.numeric(mean(dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numtarget]))/as.numeric(mean(dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numinitiation]))
		frequencies[1]=100*nrow(dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],])/timesteps
		for (sec in seq(1,23)){
			frequencies[sec+1]=100*nrow(dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],])/timesteps
	                tblmeans[kk,sec+2]=as.numeric(mean(dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numtarget]))/as.numeric(mean(dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numinitiation]))
		}
		#CORRELATION
		#name
                tblcorrs[kk,1]=paste(metmasts[numtarget,1],metmasts[numinitiation,1],sep="/")
		#all period
		tblcorrs[kk,26]=as.numeric(cor(dfseries[,numtarget],dfseries[,numinitiation]))
		#each sector
		tblcorrs[kk,2]=as.numeric(cor(dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numtarget],dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numinitiation]))
		for (sec in seq(1,23)){
	                tblcorrs[kk,sec+2]=as.numeric(cor(dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numtarget],dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numinitiation]))
		}
		#Slope
		#name
                tblslopes[kk,1]=paste(metmasts[numtarget,1],metmasts[numinitiation,1],sep="/")
		#all period
		fit <- lm(dfseries[,numtarget] ~ dfseries[,numinitiation])
		tblslopes[kk,26]=as.numeric(fit$coefficients[[2]])
		#each sector
		fit <- lm(dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numtarget]~dfseries[dfseries$D > dirs[24] | dfseries$D < dirs[1],numinitiation])		
		tblslopes[kk,2]=as.numeric(fit$coefficients[[2]])
		for (sec in seq(1,23)){
			fit <- lm(dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numtarget]~dfseries[dfseries$D > dirs[sec] & dfseries$D < dirs[sec+1],numinitiation])
	                tblslopes[kk,sec+2]=as.numeric(fit$coefficients[[2]])
		}
                kk=kk+1
        }
}
#TO NUMERIC
tblmeansnum=mapply(tblmeans[,2:26],FUN=as.numeric)
tblmeansnum<-matrix(data=tblmeansnum,ncol=25,nrow=36)
tblcorrsnum=mapply(tblcorrs[,2:26],FUN=as.numeric)
tblcorrsnum<-matrix(data=tblcorrsnum,ncol=25,nrow=36)
tblslopesnum=mapply(tblslopes[,2:26],FUN=as.numeric)
tblslopesnum<-matrix(data=tblslopesnum,ncol=25,nrow=36)

#GRAPHIC RASTER
require(lattice)
levelplot(tblcorrsnum)
levelplot(tblmeansnum)
png("means-Desert-claim.png")
levelplot(tblmeansnum,at=c(seq(0,2,0.1)))
dev.off()

png("slopes-Desert-claim.png")
levelplot(tblslopesnum,at=c(seq(0,2,0.1)))
dev.off()
#GRAPHIC RASTER
require(lattice)
levelplot(tblcorrsnum)
levelplot(tblmeansnum)
png("means-Desert-claim.png")
levelplot(tblmeansnum,at=c(seq(0,2,0.1)))
dev.off()

png("slopes-Desert-claim.png")
levelplot(tblslopesnum,at=c(seq(0,2,0.1)))
dev.off()
metmastsnum=unlist(sapply(metmasts$V1,function(x) as.numeric(paste(unlist(strsplit(x,"[.]"))[2]))))
pairs=unlist(sapply(seq(1,N-1), function(x) {sapply(seq(x+1,N), function(y){ paste(metmastsnum[x],"/",metmastsnum[y],sep="")})}))
rownames(tblmeans)=pairs
rownames(tblmeansnum)=pairs
rownames(tblcorrs)=pairs
rownames(tblcorrsnum)=pairs
df=data.frame(tblcorrsnum[,25],tblmeansnum[,25])
colnames(df)=c("Correlations","MeansCoef")
df$order=seq(1,36)
df$pair=rownames(df)
melted=melt(df,id.vars=c("order","pair"))
ggplot(melted)+geom_line(aes(pair,value,color=variable,group=variable))+xlab("pair")+ylab("coef")
library(reshape)
 library(ggplot2)
meltedmeans=melt(tblmeansnum)
meltedmeans$var="MeansCoef"
meltedcorrs=melt(tblcorrsnum)
meltedcorrs$var="Correlation"
meltedall=rbind(meltedcorrs,meltedmeans)
colnames(meltedall)=c("pairs","sector","value","var")
meltedall[meltedall$var == "MeansCoef","value"]=1/meltedall[meltedall$var == "MeansCoef","value"]
ggplot(meltedall)+geom_line(aes(pairs,value,color=var,group=var))+xlab("pair")+ylab("coef")+facet_wrap(~sector)
meltedall[meltedall$order == i,"freq"]=frequencies[i]
for (i in seq(1,25)){
	meltedall[meltedall$sector == i,"freq"]=frequencies[i]
}
for (i in seq(1,25)){
	melted[melted$order == i,"freq"]=frequencies[i]
}


freqlabels=data.frame(x=rep(12,25),y=rep(1.5,25),label=round(unique(meltedall$freq),1),sector=seq(1,25))
png("methods-comparison-coefinverted.png",w=1024,h=1024)
ggplot(meltedall)+geom_line(aes(pairs,value,color=var,group=var))+geom_line(aes(pairs,value,color=var,group=var))+xlab(pairs)+ylab("coef")+facet_wrap(~sector)+geom_text(data=freqlabels,aes(x=x,y=y,label=label))
dev.off()
meltedallInverted=meltedall
meltedall[meltedall$var == "MeansCoef","value"]=1/meltedall[meltedall$var == "MeansCoef","value"]
png("methods-comparison.png",w=1024,h=1024)
ggplot(meltedall)+geom_line(aes(pairs,value,color=var,group=var))+geom_line(aes(pairs,value,color=var,group=var))+xlab(pairs)+ylab("coef")+facet_wrap(~sector)+geom_text(data=freqlabels,aes(x=x,y=y,label=label))
dev.off()




stop()



