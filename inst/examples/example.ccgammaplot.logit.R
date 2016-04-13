# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())


library(RGENERATEPREC)
library(RGENERATEPRECVis)

#source('~/Dropbox/R-packages/RGENERATEPRECVis/R/corplot.R', chdir = TRUE)
#source('~/Dropbox/R-packages/RGENERATEPRECVis/R/ccgammaplot.R', chdir = TRUE)

data(trentino)


set.seed(12398)

year_min <- 1961
year_max <- 1990

origin <- paste(year_min,1,1,sep="-")
end <- paste(year_max,12,31,sep="-")

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	acc <- TRUE
	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
	
}
accepted <- accepted[!(accepted %in% c("month","day","year"))]

### excluding some station 

excluded <- c("T0001","T0014","T0064")
accepted <- accepted[!(accepted %in% excluded)]

valmin <- 1.0
prec_mes <- prec_mes[,accepted]



months <- factor(prec_mes$month)
station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
#excluded <- c("T0001","T0014","T0064")
#station <- station[!(station %in% excluded)]

it <- station[2]
vect <- Tx_mes[,it]-Tn_mes[,it]


tolerance_wilks=0.005 ###.Machine$double.eps*10^11
model <- PrecipitationOccurenceModel(x=prec_mes[,it],exogen=vect,monthly.factor=months,valmin=valmin)

obs <- prec_mes[,it]>=valmin

gen <- generate(model,exogen=vect,monthly.factor=months,n=length(months))


### MultiSite Generation
###station <- station[1:2]
months <- factor(prec_mes$month)
###station <- station
prec_mes <- prec_mes[,station]
Tx_mes <- Tx_mes[,station]
Tn_mes <- Tn_mes[,station]


exogen_var <- Tx_mes-Tn_mes

exogen <- normalizeGaussian_severalstations(x=exogen_var, data=exogen_var,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)
		
names(exogen) <- paste(names(exogen),"_exog",sep="")		
model_multisite_logit <- PrecipitationOccurenceMultiSiteModel(x=prec_mes,exogen=exogen,origin=origin,p=1,valmin=valmin,tolerance_wilks=tolerance_wilks,multisite_type="logit",station=station)
## LOGIT-type Model
####model_multisite_logit <- PrecipitationOccurenceMultiSiteModel(x=prec_mes,station=station,exogen=exogen,origin=origin,multisite_type="logit",p=1)


obs_multisite <- prec_mes[,station]>=valmin

gen_multisite <- generate(model_multisite_logit,exogen=exogen,origin=origin,end=end)

obs_mn <- as.data.frame(obs_multisite*5.0)
gen_mn <- as.data.frame(gen_multisite*5.0)





wpath <- "/Users/ecor/Dropbox/R-packages/RGENERATEPRECVis/inst"

plotccgamma=TRUE
####return.values=c("nooccurence","occurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
return.values=c("nooccurence","occurence","nooccurence_occurence","occurence_nooccurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
titles <- c("Joint probabilities that station pairs are both dry","Joint probabilities that station pairs are both wet","Joint probabilities that one station is dry and the other one is wet","Joint probabilities that one station is wet and the other one is dry",
		"Continuity Ratio","Wilks Gaussian Correlation that station pairs are both dry"
		,"Binomial Correlation of Precipitetion Occurence")           

names(return.values) <- titles 

width = 480
height = width
if (plotccgamma) for (it in titles) {
		for (lag in c(0,1)) {
			
			title <- paste(it,"(Lag Y)",sep=" ")
			png <- paste(wpath,'plot/XXX_lagY_prec_logit_RMAWGEN.png',sep='/')
			
			png <- str_replace(png,"XXX",return.values[it])
			png <- str_replace(png,"Y",lag)
			
			title <- str_replace(title,"Y",lag)
			
			ccgammaplot_out <- ccgammaplot(x=obs_mn,y=gen_mn,corx=NULL,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,lag=lag,return.value=return.values[it],valmin=valmin,tolerance=tolerance_wilks)
			
			png(png,width=width,height=height)
			print(ccgammaplot_out)
			dev.off()
			
			
		}
	}


## QQPLOT 



## QQPLOT 

valmin_spell <- valmin

dw.spell.df.dry.mes <- dw.spell(obs_mn,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)
dw.spell.df.dry.gen <- dw.spell(gen_mn,origin=origin,melting.df=TRUE,extract="dry",valmin=valmin_spell)

dw.spell.df.wet.mes <- dw.spell(obs_mn,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)
dw.spell.df.wet.gen <- dw.spell(gen_mn,origin=origin,melting.df=TRUE,extract="wet",valmin=valmin_spell)


width = 480
height = width
height_unseas=height/2
height_lags=height




dw.spell.qqplot=TRUE
if (dw.spell.qqplot) {
	for (it in station) { 
		
		### dry 
		
		png <- paste(wpath,'plot/qqplot_dw_spell_dry_prec_logit_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile dry spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'plot/qqplot_dw_spell_dry_prec_logit__RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile dry spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_logit_unseas <- QuantileQuantilePlot(x=dw.spell.df.dry.mes,y=dw.spell.df.dry.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_logit_unseas)
		dev.off()	
		
		
		### WET 
		
		png <- paste(wpath,'plot/qqplot_dw_spell_wet_prec_logit_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile wet spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
		
		
		png <- paste(wpath,'plot/qqplot_dw_spell_wet_prec_logit__RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile wet spell length [days] at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_logit_unseas <- QuantileQuantilePlot(x=dw.spell.df.wet.mes,y=dw.spell.df.wet.gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_logit_unseas)
		dev.off()	
		
		
		
		
		
	}
}


#### 
nwetdays_obs <- nwetdays(obs_mn,origin=origin,valmin=valmin,station=station)
nwetdays_gen <- nwetdays(gen_mn,origin=origin,valmin=valmin,station=station)

nwetdayplot=TRUE

if (nwetdayplot==TRUE) for (it in station) {
	
	png <- paste(wpath,'plot/qqplot_nwetdays_prec_logit_RMAWGEN_XXX.png',sep='/')  
	title <- "Quantile-Quantile number of wet days per month [days] at XXX"		
	
	png <- str_replace(png,"XXX",it)
	title <- str_replace(title,"XXX",it)
	qqplot_nwet <- QuantileQuantilePlot(x=nwetdays_obs,y=nwetdays_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
	
	png(png,width=width,height=height)
	print(qqplot_nwet)
	dev.off()	
	
	
	
}

#####
###