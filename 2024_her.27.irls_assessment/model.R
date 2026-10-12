## Run analysis, write model results

## Before: spaly_2023.DAT (bootstrap/data), asap.zip (bootstrap/software),
##         stock.Rdata (data)
## After:  asap3.rdat, stock.mult.opts.Rdata, stock.options.Rdata,
##         stock.proj.Rdata, yrs.csv (model)

library(icesTAF)
library(FLCore)
library(FLAssess)
library(FLash)

mkdir("model")

##############################################################################################
## Run the ASAP assessment
#############################################################################################



## Get model executable
exefile <- if(os.windows()) "asap3.exe" else "asap3"
taf.unzip("bootstrap/software/asap.zip", files=exefile, exdir="model")

## Get model input file
## Input file must be called asap3.dat
cp("bootstrap/data/spaly_2024.DAT", "model/asap3.dat")

## Run model
setwd("model")
system("./asap3")
setwd("..")



##############################################################################################
## Code to run the forecast 
## Forecast uses the FLStock object
## Read in the N and F file from ASAP
#############################################################################################



load(file='data/stock.Rdata')

asap <- dget("model/asap3.rdat")
nyears<-asap$parms[[3]]
nages<-asap$parms[[4]]


## Read in the F and N from ASAP
stock@stock.n@.Data <- array(t(asap$N.age),dim=c(nages,nyears,1,1,1,1))
stock@harvest@.Data <- array(t(asap$F.age),dim=c(nages,nyears,1,1,1,1))
stock@harvest@units <- 'f'

#look ssb at spawning time - compare with stock summary table
ssb(stock)
ssb<-ssb(stock)
summary(stock)



#Define years
TaY <- dims(stock)$maxyear   #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table
yrs <- data.frame(ImY=ImY, AdY=AdY, CtY=CtY)


# use breakpoint from julios stock recruitment if ssb in forecast year -2 is greater than the breakpoint

rec<-119691
  
## use geomean stock recruit
stock.srr <- list(model="geomean",params=FLPar(rec))

#Expand stock object

stock.proj <- stf(stock,nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)
stock.proj@stock.n[ac(2:9),ac(ImY)]  <- stock@stock.n[ac(1:8),ac(TaY)] * exp(-stock@harvest[ac(1:8),ac(TaY)]-stock@m[ac(1:8),ac(TaY)])
stock.proj@stock.n[ac(9),ac(ImY)]    <- stock.proj@stock.n[ac(9),ac(ImY)] + stock@stock.n[ac(9),ac(TaY)] * exp(-stock@harvest[ac(9),ac(TaY)]-stock@m[ac(9),ac(TaY)])
stock.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- rec

# check values
stock.proj@stock.n


#Define some constants  ## TAC + Carry over estimated quota uptake + discards
#intermediate year catch 2019 - include assumptions regarding uptake and carryover
ImY.catch <- 869

#advice year catch
## TAC figure used in calculations
AdY.catch<-869 #Monitoring TAC TAC

## updated ref points WKPELA 2018

numFmsy <- 0.26
numFpa <- 0.26
numFlim <- 0.45


#Setup options
options.l <- list(#Zero catch
  "Catch(2025) = Zero"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity="catch",
                          val=c(ImY.catch,0,0))),
  "Catch(2025) = 2024 TAC"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","catch"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch,AdY.catch))),
  "Fbar(2025) = Fmsy"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFmsy,numFmsy))),
  "Fbar(2025) = Fpa"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFpa,numFpa))),
  #Intermediate year catch equal TAC, followed Fbar = Flim
  "Fbar(2025) = Flim"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFlim,numFlim))),
  #Intermediate year catch equal TAC, followed Fbar = F2024 (0.04557) # get from options summary excel table and rerun
  "Fbar(2025) = F2024"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,0.04557,0.04557))),
  #Intermediate year catch equal TAC, followed Fbar = Fmsy (0.26) * SSB2024 (21016.7) /MSY Btrigger(54000) (0.1011)
  "Fbar(2025) = Fmsy * SSB2024 /MSY Btrigger"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,0.1011,0.1011)))
  
) #End options list






#Multi-options table
fmult.targs  <- seq(0,2,by=0.025)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(ImY,AdY,CtY),
                        quantity=c("catch","f","f"),
                        rel=c(NA,ImY,AdY),
                        val=c(ImY.catch,fmult,1)))
})
names(mult.opts.l) <- sprintf("Fmult(2024) = %4.3f",fmult.targs)

## Calculate options
stock.options <- lapply(options.l,function(ctrl) {fwd(stock.proj,ctrl=ctrl,sr=stock.srr)})
stock.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(stock.proj,ctrl=ctrl,sr=stock.srr)})


write.taf(yrs, dir="model")
save(stock.proj,file='model/stock.proj.Rdata')
save(stock.options,file='model/stock.options.Rdata')
save(stock.mult.opts,file='model/stock.mult.opts.Rdata')



