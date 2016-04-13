# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#' Function.test
#' 
#' This function is dynamically written in R to test RMAWGEN or RGENERATE or RGENERATEPREC outputs!!!
#' 
#' @param x,y  x,y 
#' @param sample charchter string. Defaul is \code{"monthly"}
#' @param origin_x,origin_y origins for \code{x,y}
#' @param station weather station code
#' @param valmin minimum value admitted. Default is \code{NA}. It is used in case of precipitation amount. 
#' @param ... further arguments for tests
#' 
#' @export
#' 
#' 


function.test <- function(x,y,sample="monthly",station,origin_x=NULL,origin_y=NULL,valmin=NA,...) {
	
	out <- NULL
	if (is.null(sample)) sample <- "all"
	if (sample=="monthly") {
		
		if (!("month" %in% names(x))) {
			
	
			x <- adddate(x,origin=origin_x)
		}
		if (!("month" %in% names(y))) {
			y <- adddate(y,origin=origin_y)
		
			
		} 
		months <- sort(unique(x$month))
		
		out <- list()
		
		for (m in months) {
			
			
			xm <- x[x$month==m,] 
			ym <- y[y$month==m,] 
			
			out[[m]] <- function.test(x=xm,y=ym,sample="all",station=station,valmin=valmin,...)
		}
		
		class(out) <- "MonthlyList"
		
	} else {
		
		x_t <- as.vector(x[,names(x) %in% station])
		y_t <- as.vector(y[,names(y) %in% station])
		
		out <- list() 
		
		x_t <- x_t[!is.na(x_t)]
		y_t <- y_t[!is.na(y_t)]
		
		if (is.null(valmin)) valmin <- NA
			
		if (!is.na(valmin)) {
			print(valmin)
			
			x_t <- x_t[x_t>=valmin]
			y_t <- y_t[y_t>=valmin]
			print(sort(x_t))
			print(sort(y_t))
		}
		
		
		out$ks.test <- ks.test(x=x_t,y=y_t,...)
		out$wilcox.test <- wilcox.test(x=x_t,y=y_t,...)
		out$var.test <- var.test(x=x_t,y=y_t,...)
		out$t.test <- t.test(x=x_t,y=y_t,...)
		out$mean_x <- mean(x_t,na.rm=TRUE)
		out$mean_y <- mean(y_t,na.rm=TRUE)
		out$sd_x <- sd(x_t,na.rm=TRUE)
		out$sd_y <- sd(y_t,na.rm=TRUE)
		
	}
	
	return(out)
	
	
}