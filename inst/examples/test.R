
library(RGENERATEPREC)

set.seed(1234)
data(trentino)

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

valmin <- 1.0
prec_mes <- prec_mes[,accepted]



Tx_mes <- Tx_mes[,accepted]
Tn_mes <- Tn_mes[,accepted]
prec_occurence_mes <- prec_mes>=valmin

station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
it <- station[2]
vect <- Tx_mes[,it]-Tn_mes[,it]
months <- factor(prec_mes$month)
model <- PrecipitationOccurenceModel(x=prec_mes[,it],exogen=vect,monthly.factor=months,valmin=valmin)

obs <- prec_mes[,it]>=valmin

gen <- generate(model,exogen=vect,monthly.factor=months,n=length(months))


### MultiSite Generation 


station <- station[1:4]
exogen <- Tx_mes-Tn_mes
months <- factor(prec_mes$month)

model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes[,station],exogen=exogen,origin=origin)

obs_multisite <- prec_mes[,station]>=valmin

gen_multisite <- generate(model_multisite,exogen=exogen,origin=origin,end=end)

obs_mn <- as.data.frame(obs_multisite*5.0)
gen_mn <- as.data.frame(gen_multisite*5.0)






plotccgamma=TRUE
return.values=c("nooccurence","occurence","nooccurence_occurence","occurence_nooccurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")

return.values=c("nooccurence","occurence","nooccurence_occurence","occurence_nooccurence","continuity_ratio","probability_continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
titles <- c("Joint probabilities that station pairs are both dry","Joint probabilities that station pairs are both wet","Joint probabilities that one station is dry and the other one is wet","Joint probabilities that one station is wet and the other one is dry",
		"Continuity Ratio","Occurrence Continuity Ratio","Wilks Gaussian Correlation that station pairs are both dry"
		,"Binomial Correlation of Precipitation Occurence")    

names(return.values) <- titles  
return.values <- return.values[return.values!="continuity_ratio"]
wpath <- "./"
width = 180 
height = 480
if (plotccgamma) for (it in names(return.values)) {
		for (lag in c(0,1)) {
			
			title <- paste(it,"(Lag Y)",sep=" ")
			png <- paste(wpath,'temp_plot_XXX_lagY_prec_RMAWGEN.png',sep='/')
			
			png <- str_replace(png,"XXX",return.values[it])
			png <- str_replace(png,"Y",lag)
			
			title <- str_replace(title,"Y",lag)
			
			ccgammaplot_out <- ccgammaplot(x=obs_mn,y=gen_mn,corx=NULL,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,lag=lag,return.value=return.values[it],valmin=valmin)
			
			
			
			png(png,width=width,height=height)
			print(ccgammaplot_out)
			dev.off()	
			
			
		}
	}

