## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VALIDATEREMOD.R
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## DEPENDENCIES
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if(!is.installed("ncdf"))   {install.packages("ncdf"    ,repos="http://cran.r-project.org")}
suppressPackageStartupMessages(library("ncdf"))

## FUNCTIONS ...
RD.SERIES <-  function(ncfile){
    ncid     <- open.ncdf(ncfile)
    ntime <- ncid$dim$time$len ; tvar <- get.var.ncdf(ncid,"time") ; origin <- ncid$dim$time$units ; tunits <- strsplit(ncid$dim$time$units,split=" ")

print(paste(tunits[[1]][1]))
    if (tunits[[1]][1] == "hours"){
		print(paste("hourly"))
            time.dta <- data.frame(TIME=strptime(paste(tunits[[1]][3],paste(tunits[[1]][4],"00",sep=":"), sep=" "),"%Y-%m-%d %H:%M:%S",tz="GMT") +  as.numeric(as.difftime(tvar,units=tunits[[1]][1]),units="secs"))
    }
    if (tunits[[1]][1] == "seconds"){
            time.dta <- data.frame(TIME=strptime(paste(tunits[[1]][3],paste(tunits[[1]][4],"00",sep=":"), sep=" "),"%Y-%m-%d %H:%M:%S",tz="GMT") +  tvar)
	    print(paste(tunits[[1]][3],tunits[[1]][4]))
    }

    cat(paste("RD.DATASERIES :: Vars in ",ncfile,"\n",sep=""))
    nvars = ncid$nvars ; var.nms <- c() ; var.lst <- list() ; var.units <- c()
    for(ivar in 1:nvars){
        cat(paste("\tVar number",ivar,", named", ncid$var[[ivar]]$name, " has units", ncid$var[[ivar]]$units,"\n" ))        
        var.units[ivar] <- ncid$var[[ivar]]$units
        var.nms[ivar]   <- ncid$var[[ivar]]$name
        var.lst[[ivar]] <- get.var.ncdf(ncid, var.nms[ivar])
    }

    var.dtf <- as.data.frame(var.lst) ; names(var.dtf) <- var.nms ; vortex.dtf <- cbind(time.dta,var.dtf)
    Var.lon <- ncid$var[[1]]$dim[[1]]$vals ; Var.lat <- ncid$var[[1]]$dim[[2]]$vals ; Var.lev <- ncid$var[[1]]$dim[[3]]$vals
    Var.time <- get.var.ncdf(ncid,"time") ; Var.tunits <- att.get.ncdf(ncid,"time",attname="units")    
    close.ncdf(ncid)
    return(list(dta=vortex.dtf,lev=Var.lev,latlon=c(Var.lat,Var.lon), time=Var.time, tunits=Var.tunits, var.units=var.units))
}

## COMMAND LINE AGRUMENTS 
input1.fl <- commandArgs()[4] ## INPUT 1 
input2.fl <- commandArgs()[5] ## INPUT 2 
var1      <- commandArgs()[6] ## VAR 1
var2      <- commandArgs()[7] ## VAR 2
aggr      <- commandArgs()[8] ## [mean, sum, median, ..., fun, ...]
norm      <- commandArgs()[9] ## [units, norm, cv]


## READ DATA ...
inp1 <- RD.SERIES(input1.fl)
inp2 <- RD.SERIES(input2.fl)


## Aquestes línies resolen el problema de quan tenim el time de les dades 00:30, per passar-ho a 00:00
inp2$dta$TIME <- as.character(inp2$dta$TIME)
inp2$dta$TIME <- as.POSIXct(paste(strtrim(inp2$dta$TIME, 13),":00:00",sep=""),tz='GMT')

## Aquestes línies resolen el problema de quan tenim el time de les dades 00:30, per passar-ho a 00:00
inp1$dta$TIME <- as.character(inp1$dta$TIME)
#print(paste(inp1$dta$TIME))
inp1$dta$TIME <- as.POSIXct(paste(strtrim(inp1$dta$TIME, 13),":00:00",sep=""),tz='GMT')


#FILTER--> SHIFT one hour
#inp2$dta$TIME <- as.POSIXct(paste(strtrim(inp2$dta$TIME, 13),":00:00",sep=""),tz='GMT') -3600

#FILTER low values
#inp1$dta[inp1$dta[,var1]< 100  | is.na(inp1$dta[,var1]),var1]<-NA
#inp2$dta[inp2$dta[,var2]< 100,var2]<-NA

## MERGE DATA.FRAME :: matching coincidences ...
inp.dtf <- merge(inp1$dta,inp2$dta,by="TIME")


## CHECK THAT VAR NAMES ARE PRESENT IN THE DATA.FRAME
if (var1 == var2){
	var1=paste(var1,".x",sep="")
	var2=paste(var2,".y",sep="")
}
if(is.na(var1)|| is.na(var2)) stop("\n MISSING VARIABLES, PLEASE CHECK AVAILABLE VARS ON INPUTS AND INCLUDE AS ARGUMENTS!!\n\n\n")
ivar1 <- which(names(inp.dtf) == var1) ; ivar2 <- which(names(inp.dtf) == var2)
if(length(ivar1) == 0) stop(paste("THE REQUESTED VARIABLES:\t", var1," 1 NOT FOUND IN DATA\n",sep=""))
if(length(ivar2) == 0) stop(paste("REQUESTED VARIABLES:\t", var2," 2 NOT FOUND IN DATA\n",sep=""))

## CHECK THAT AGGREGATION IS SET 
if(is.na(aggr)) stop("\n MISSING AGGREGATION, PLEASE SET mean OR sum OR median ...!!\n\n\n")

## CHECK THAT NORMALIZATION IS SET 
if(is.na(norm)) stop("\n MISSING NORMALIZATION, PLEASE SET units OR norm OR cv ...!!\n\n\n")

## Get TIME DIFFERENCES ...
Dt <- median(inp.dtf$TIME[2:dim(inp.dtf)[1]]  - inp.dtf$TIME[1:(dim(inp.dtf)[1]-1)])
Dt.num  <- as.numeric(median(Dt)) ; Dt.uni  <- attributes(Dt)$units ## hours, days, month
cat(paste("\nSERIES TEMPORAL STEP IS:\t",Dt.num," ",Dt.uni, "\n",sep=""))


## COMPUTE HOURLY, DAILY AND MONTHLY DATA WHERE AVAILABLE/POSSIBLE
cat(paste("\nDATA AGGREGATION...\n",sep=""))

inp.lst <- list()
if(paste(Dt.num,Dt.uni,sep="") == "1hours" || paste(Dt.num,Dt.uni,sep="") == "3600secs") {    
    inp.lst[[1]] <- inp.dtf     ## Hourly data    
    ## Daily mean data
    inp.dtf.day      <- aggregate(inp.dtf[names(inp.dtf)[-1]], format(inp.dtf["TIME"],"%Y-%m-%d"), aggr, na.rm = TRUE) ## daily average
    inp.dtf.day$TIME <- as.POSIXct(paste(inp.dtf.day$TIME," 00:00:00",sep=""),tz='GMT') ; inp.lst[[2]] <- inp.dtf.day    
    ## Monthly mean data
    inp.dtf.mon      <- aggregate(inp.dtf[names(inp.dtf)[-1]], format(inp.dtf["TIME"],"%Y-%m"), aggr, na.rm = TRUE) ## daily average
    inp.dtf.mon$TIME <- as.POSIXct(paste(inp.dtf.mon$TIME,"-01 00:00:00",sep=""),tz='GMT') ; inp.lst[[3]] <- inp.dtf.mon    
} else if(paste(Dt.num,Dt.uni,sep="") == "1days"){    
    inp.lst[[1]] <- FALSE   ## Hourly data    
    inp.lst[[2]] <- inp.dtf ## Daily mean data
    ## Monthly mean data
    inp.dtf.mon      <- aggregate(inp.dtf[names(inp.dtf)[-1]], format(inp.dtf["TIME"],"%Y-%m"), aggr, na.rm = TRUE) ## daily average
    inp.dtf.mon$TIME <- as.POSIXct(paste(inp.dtf.mon$TIME,"-01 00:00:00",sep=""),tz='GMT') ; inp.lst[[3]] <- inp.dtf.mon
} else if(paste(Dt.num,Dt.uni,sep="") == "1months"){    
    inp.lst[[1]] <- FALSE   ## Hourly data    
    inp.lst[[2]] <- FALSE   ## Daily mean data    
    inp.lst[[3]] <- inp.dtf ## Monthly mean data
} else {
    stop("TEMPORAL RESOLUTION OF THE DATA NOT ALLOWED...\n")
}
names(inp.lst) <- c("hourly","daily","monthly")

## COMPUTE ERROR METRICS ...
cat(paste("\nCOMPUTE ERROR METRICS ...\n",sep=""))
AR2.lst <- R2.lst <- bias.lst <- mae.lst <- rmse.lst <- list()
for (idd in 1:length(inp.lst)){        
    if(identical(FALSE,inp.lst[[idd]])){        
        rmse.lst [[idd]]  <- FALSE ; mae.lst [[idd]]  <- FALSE ; bias.lst [[idd]]  <- FALSE
        R2.lst[[idd]]     <- FALSE ; AR2.lst[[idd]]   <- FALSE        
    } else {        
        ## RMSE
        if(norm == "units"){ ## Natural units
            rmse.lst[[idd]] <- sqrt(mean((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2])^2,na.rm=TRUE))
        } else if(norm == "norm"){ ## Normalized
            rmse.lst[[idd]] <- 100*sqrt(mean(((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2])^2)/((inp.lst[[idd]][,var2])^2),na.rm=TRUE))
        } else if(norm == "cv"){ ## Coefficient of variation            
            rmse.lst[[idd]] <- 100*sqrt(mean((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2])^2,na.rm=TRUE))/mean(inp.lst[[idd]][,var2],na.rm=TRUE)
        }
        
        ## MAE
        if(norm == "units"){ ## Natural units
            mae.lst [[idd]] <- mean(abs(  inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2]),na.rm=TRUE)
        } else if(norm == "norm"){ ## Normalized
            mae.lst[[idd]] <- 100*mean(abs((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2])/inp.lst[[idd]][,var2]),na.rm=TRUE)
        } else if(norm == "cv"){ ## Coefficient of variation            
            mae.lst[[idd]] <- 100*mean(abs(inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2]),na.rm=TRUE)/mean(inp.lst[[idd]][,var2],na.rm=TRUE)
        }
        
        ## BIAS        
        if(norm == "units"){ ## Natural units
            bias.lst[[idd]] <- mean((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2]),na.rm=TRUE)
        } else if(norm == "norm"){ ## Normalized            
            bias.lst[[idd]] <- 100*mean((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2])/inp.lst[[idd]][,var2],na.rm=TRUE)
        } else if(norm == "cv"){ ## Coefficient of variation            
            bias.lst[[idd]] <- 100*mean((inp.lst[[idd]][,var1] -inp.lst[[idd]][,var2]),na.rm=TRUE)/mean(inp.lst[[idd]][,var2],na.rm=TRUE)
        }
                
        ## CORRELATION
        lfit <- lm(paste(var1,"~",var2),data=inp.lst[[idd]])
        R2.lst[[idd]]  <- summary(lfit)$r.squared
        AR2.lst[[idd]] <- summary(lfit)$adj.r.squared
    }
}
names(AR2.lst) <- names(R2.lst) <- names(bias.lst) <- names(mae.lst) <- names(rmse.lst) <- c("hourly","daily","monthly")

## ROUND
for (idd in 1:length(inp.lst)){        
    rmse.lst[[idd]] <- round(rmse.lst[[idd]],digits=3)
    mae.lst [[idd]] <- round(mae.lst [[idd]],digits=3)
    bias.lst[[idd]] <- round(bias.lst[[idd]],digits=3)
    R2.lst  [[idd]] <- round(R2.lst  [[idd]],digits=3)
    AR2.lst [[idd]] <- round(AR2.lst [[idd]],digits=3)
}

## WRITE REPORT FILE ...
cat(paste("\nWRITE VALIDATION REPORT ...\n",sep=""))

VarID <- paste(var1,"/",var2,"[",aggr,"+",norm,"]",sep="")
sink(paste('ValRprt1_',aggr,"-",norm,".txt",sep=""))
## cat(paste(format("Aggregation")  ,";",format(aggr),"\n",sep=""))
## cat(paste(format("Normalization"),";",format(norm),"\n",sep=""))
cat(paste(format(VarID)      , ";",format("RMSE")      ,";",format("MAE")       ,";",format("BIAS")       ,";",format("R2")       ,"\n",sep=""))
cat(paste(format("hourly")   ,";",format(rmse.lst[[1]]),";",format(mae.lst[[1]]),";",format(bias.lst[[1]]),";",format(R2.lst[[1]]),"\n",sep=""))
cat(paste(format("daily" )   ,";",format(rmse.lst[[2]]),";",format(mae.lst[[2]]),";",format(bias.lst[[2]]),";",format(R2.lst[[2]]),"\n",sep=""))
cat(paste(format("monthly")  ,";",format(rmse.lst[[3]]),";",format(mae.lst[[3]]),";",format(bias.lst[[3]]),";",format(R2.lst[[3]]),"\n",sep=""))
sink()

sink(paste('ValRprt2_',aggr,"-",norm,".txt",sep=""))
vars.header.hrly <- paste(format("hourly")     ,";",format("")          ,";",format("")           ,";",format("")         ,sep="")
vars.header.day  <- paste(format("daily")      ,";",format("")          ,";",format("")           ,";",format("")         ,sep="")
vars.header.mon  <- paste(format("monthly")    ,";",format("")          ,";",format("")           ,";",format("")         ,sep="")
vars.header      <- paste(format("RMSE")       ,";",format("MAE")       ,";",format("BIAS")       ,";",format("R2")       ,sep="")
vars.hrly        <- paste(format(rmse.lst[[1]]),";",format(mae.lst[[1]]),";",format(bias.lst[[1]]),";",format(R2.lst[[1]]),sep="")
vars.day         <- paste(format(rmse.lst[[2]]),";",format(mae.lst[[2]]),";",format(bias.lst[[2]]),";",format(R2.lst[[2]]),sep="")
vars.mon         <- paste(format(rmse.lst[[3]]),";",format(mae.lst[[3]]),";",format(bias.lst[[3]]),";",format(R2.lst[[3]]),sep="")
## cat(paste(format("Aggregation")  ,";",format(aggr),"\n",sep=""))
## cat(paste(format("Normalization"),";",format(norm),"\n",sep=""))
cat(paste(      ";",vars.header.hrly,";",vars.header.day,";",vars.header.mon,"\n",sep=""))
cat(paste(VarID,";",vars.header     ,";",vars.header    ,";",vars.header    ,"\n",sep=""))
cat(paste(      ";",vars.hrly       ,";",vars.day       ,";",vars.mon       ,"\n",sep=""))
sink()

sink(paste('ValRprt3_',aggr,"-",norm,".txt",sep=""))
vars.header <- paste(format("hourly")     ,";",format("daily") ,";",format("monthly")     ,sep="")
vars.header.rmse <- paste(format("RMSE")  ,";",format("RMSE")  ,";",format("RMSE")  ,sep="")
vars.header.mae  <- paste(format("MAE" )  ,";",format("MAE")   ,";",format("MAE")   ,sep="")
vars.header.bias <- paste(format("BIAS")  ,";",format("BIAS")  ,";",format("BIAS")  ,sep="")
vars.header.R2   <- paste(format("R2"  )  ,";",format("R2")    ,";",format("R2")    ,sep="")
vars.rmse   <- paste(format(rmse.lst[[1]]),";",format(rmse.lst[[2]]),";",format(rmse.lst[[3]]),sep="")
vars.mae    <- paste(format(mae.lst [[1]]),";",format(mae.lst [[2]]),";",format(mae.lst [[3]]),sep="")
vars.bias   <- paste(format(bias.lst[[1]]),";",format(bias.lst[[2]]),";",format(bias.lst[[3]]),sep="")
vars.R2     <- paste(format(R2.lst  [[1]]),";",format(R2.lst  [[2]]),";",format(R2.lst  [[3]]),sep="")
## cat(paste(format("Aggregation")  ,";",format(aggr),"\n",sep=""))
## cat(paste(format("Normalization"),";",format(norm),"\n",sep=""))
cat(paste(      ";",vars.header     ,";;",vars.header    ,";;",vars.header     ,";;",vars.header   ,"\n",sep=""))
cat(paste(      ";",vars.header.rmse,";;",vars.header.mae,";;",vars.header.bias,";;",vars.header.R2,"\n",sep=""))
cat(paste(VarID,";",vars.rmse       ,";;",vars.mae       ,";;",vars.bias       ,";;",vars.R2       ,"\n",sep=""))
sink()


## SUCESSFULL EXECUTION
cat(paste("\nDONE !!\n",sep=""))
quit()
