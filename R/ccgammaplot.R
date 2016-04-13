# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL 

#'
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param corx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param return.value string indicating which matrix returned by \code{\link{CCGamma}}. See 
#' @param tolerance,valmin,interval,nearPD see \code{\link{CCGamma}}
#' @param lag lag expressed in days used for the computation in \code{\link{CCGamma}}. It must contain only one value, it must not be a vector.
#' @param xlab,ylab,title title and axis labels
#' @param season logical value. If \code{TRUE} (default) plots are separated per seasons.
#' @param origin date corresponding to the first row
#' @param use see argement entry on \code{\link{cor}}.
#' @param return.df logical value. Default is \code{FALSE}. If it is \code{TRUE} function returns a data frame object.
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' @export
#' @seealso \code{\link{CCGamma}}
#' 
#' @import ggplot2
#' @import reshape2
#' @importFrom RMAWGEN adddate
#' @importFrom RGENERATEPREC CCGamma
#' 
#' @examples
#' 
#' library(RGENERATEPREC)
#' 
#' set.seed(1234)
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
#' 
#' origin <- paste(year_min,1,1,sep="-")
#' end <- paste(year_max,12,31,sep="-")
#' 
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max
#' 
#' prec_mes <- PRECIPITATION[period,]
#' Tx_mes <- TEMPERATURE_MAX[period_temp,]
#' Tn_mes <- TEMPERATURE_MIN[period_temp,]
## removing nonworking stations (e.g. time series with NA)
#' accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#' for (it in names(prec_mes)) {
#' 	acc <- TRUE
#' 	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
#' 	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
#' 	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
#' 	
#' }
#' 
#' valmin <- 1.0
###station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' prec_mes <- prec_mes[,accepted]
#' 
#' 
#' 
#' Tx_mes <- Tx_mes[,accepted]
#' Tn_mes <- Tn_mes[,accepted]
#' prec_occurence_mes <- prec_mes>=valmin
#' 
#' station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
#' it <- station[2]
#' vect <- Tx_mes[,it]-Tn_mes[,it]
#' months <- factor(prec_mes$month)
#' model <- PrecipitationOccurenceModel(x=prec_mes[,it],exogen=vect,
#' 	monthly.factor=months,valmin=valmin)
#' 
#' obs <- prec_mes[,it]>=valmin
#' 
#' gen <- generate(model,exogen=vect,monthly.factor=months,n=length(months))
#' 
#' 
#' ### MultiSite Generation 
#' 
#' 
#' station <- station[1:4]
#' exogen <- Tx_mes-Tn_mes
#' months <- factor(prec_mes$month)
#' 
#' model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes[,station],
#' 	exogen=exogen,origin=origin)
#'
#' obs_multisite <- prec_mes[,station]>=valmin
#' 
#' gen_multisite <- generate(model_multisite,exogen=exogen,origin=origin,end=end)
#' 
#' obs_mn <- as.data.frame(obs_multisite*5.0)
#' gen_mn <- as.data.frame(gen_multisite*5.0)
#' 
#' 
#' 
#' 
#' 
#' 
#' plotccgamma=TRUE
#' return.values=c("nooccurence","occurence",
#' '    "nooccurence_occurence","occurence_nooccurence",
#' '    "continuity_ratio","nooccurence_gcorrelation",
#' '    "nooccurence_correlation")
#' 
#' return.values=c("nooccurence","occurence","nooccurence_occurence",
#' 					"occurence_nooccurence","continuity_ratio",
#' 					"probability_continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation")
#' titles <- c("Joint probabilities that station pairs are both dry",
#' 				"Joint probabilities that station pairs are both wet",
#' 				"Joint probabilities that one station is dry and the other one is wet",
#' 				"Joint probabilities that one station is wet and the other one is dry",
#' 		       "Continuity Ratio","Occurrence Continuity Ratio",
#' 			"Wilks Gaussian Correlation that station pairs are both dry"
#' 		,   "Binomial Correlation of Precipitation Occurence")    
#' 
#' names(return.values) <- titles  
#' return.values <- return.values[return.values!="continuity_ratio"]
#' wpath <- "./"
#' width = 180 
#' height = 480
#' if (plotccgamma) for (it in names(return.values)) {
#' 		for (lag in c(0,1)) {
#' 			
#' 			title <- paste(it,"(Lag Y)",sep=" ")
#' 			png <- paste(wpath,'temp_plot_XXX_lagY_prec_RMAWGEN.png',sep='/')
#' 			
#' 			png <- str_replace(png,"XXX",return.values[it])
#' 			png <- str_replace(png,"Y",lag)
#' 			
#' 			title <- str_replace(title,"Y",lag)
#' 			
#' 			ccgammaplot_out <- ccgammaplot(x=obs_mn,y=gen_mn,
#' 					corx=NULL,xlab="observed",ylab="generated",title=title,
#' 					season=FALSE,origin=origin,lag=lag,return.value=return.values[it],
#' 					valmin=valmin)
#' 			
#' 			
#' 
#' 			png(png,width=width,height=height)
#' 			print(ccgammaplot_out)
#' 			dev.off()	
#' 			
#' 			
#' 		}
#'	}
#'



ccgammaplot <- function(x,y,use = "everything",corx=NULL,
return.value=c("nooccurence","occurence","nooccurence_occurence","occurence_nooccurence","continuity_ratio","nooccurence_gcorrelation","nooccurence_correlation"),tolerance=.Machine$double.eps,
valmin = 0.5,interval = c(-1, 1),nearPD = (lag >= 0),
xlab="observed",ylab="generated",title="Spatial Correlation",season=FALSE,origin="1960-01-01",return.df=FALSE,...) {
	
	if (length(lag)>1) lag <- lag[1]
	if (length(return.value)>1) return.value <- return.value[1]
	if (is.data.frame(y)) {
		
		y <- list(y=y)
		names(y) <- ylab[1]
	}
	
	if (season) {
		
		if (is.null(corx)) x <- addseason(x,origin=origin,nodata=TRUE)
		y <- lapply(X=y,FUN=addseason,origin=origin,nodata=TRUE)
		
		
		factor <- "season"
		
	} else {
		
		factor <- "season"
	}
	
	### CCGAMMA WRAPPER
	
#	ccgamma_wrapper <- function(data,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD) {
#		
#		out <- CCGamma(data=data)
#	}
#	
	
	####
	if (is.null(corx)) {
		print("XCC")
		print(factor)
		str(x)
		print("XXX")
		corx <- fun_factor(data=x,factor=factor,fun=CCGamma,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD)
	} else if (is.matrix(corx) | is.list(corx)) {
		
		if (is.matrix(corx) | is.data.frame(corx)) {
			corx <- list(corx=corx)
			names(corx) <- factor
		}
		## corx must be a NULL object or a single correlation matrix or a list of correlation matrices 
		out <- lapply(X=corx,FUN=as.vector)
		
		cnt <- unlist(lapply(X=out,FUN=length))
		names(cnt) <- names(out)
		
		names <- rep(names(out),times=cnt)
		
		out <- unlist(out)
		names(out) <- names
		
		n_out <- data.frame(nfactor=names(out),value=out)
		names(n_out) <- c(factor,"value")
		
		corx <- n_out
		
		
	}
	cory <- lapply(X=y,FUN=fun_factor,factor=factor,fun=CCGamma,return.value=return.value,lag=lag,tolerance = tolerance,valmin = valmin,interval = interval,nearPD = nearPD)
##	stop("go on!!!")
	str(corx)
	for (i in names(cory)) {
		
		cory[[i]][,xlab] <- corx$value
		
	}
	
	str(cory)
	cory <- melt(cory,id=c(xlab,factor))
	cory <- cory[,names(cory)!="variable"]
	
	
	names(cory)[names(cory)=="L1"] <- "level"
	
#	str(cory)
	
	names(cory)[names(cory)=="value"] <- "generated"
	names(cory)[names(cory)==xlab] <- "observed"
	
#	str(cory)
#	str(names(cory))
	
	df <- cory
	df <- df[!is.na(df$observed),]
	
	df <- df[df$observed!=1 | df$generated!=1,]
	
	
	if (return.df==TRUE) {
		
		
		return(df)
	}
	## mod by ec 20140428
	
	step <- 0.1
	scale_x <- range(df$observed)
	scale_x <- trunc(scale_x/step)*step
	scale_x[2] <- scale_x[2]+step
	###n <- 3
	str(df)
	print(scale_x)
	breaks <- seq(from=scale_x[1],to=scale_x[2],by=step)
	labels <- as.character(breaks)
	nolab <- -((1:(length(breaks)/2))*2)
	labels[nolab] <- ""
	
	
	
	if (length(unique(df$level))==1) df$level <- "" ## EC 05012015
	
	
	
	############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <- qplot(observed,generated, data = df, geom = "point", group = level) +
			facet_grid(season ~ level, scales = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline()+scale_x_continuous(breaks=breaks,labels = labels) ## added on ec 20120427
#	out <- ggplot()+geom_point(mapping=aes,data=df)+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	if (season) out <- out+facet_grid(season~season,scale="fixed")
	return(out)
	
}



