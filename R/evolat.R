#' @title Electoral volatility calculation
#'
#' @description Electoral volatility calculation:
#'     \href{http://bit.ly/2p4cyZQ}{Pedersen (1979)},
#'     \href{http://bit.ly/2FpoeBm}{Powell and Tucker (2014)} and
#'     \href{http://bit.ly/2FD83zE}{Torcal and Lago (2015)}.
#' @param esapsObject An \code{esaps} class object. Function: \code{\link{esaps_object}}.
#' @param method Method to calculate electoral volatility: \code{"Pedersen"},
#'     \code{"Powell and Tucker"} or \code{"Torcal and Lago"}.
#' @param threshold Minimum threshold for 'Type A' electoral volatility calculation
#'     (Powell and Tucker, 2014). By default is 2\%.
#' @param summary Summary of data by country, by default it is \code{FALSE}.
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @examples
#' votes <- list(data.frame(country = rep("ARG", 3),
#'                          year = c(1995, 2000, 2005),
#'                          party_A = c(40,10,20),
#'                          party_B = c(35,20,40),
#'                          party_C = c(25,70,40)),
#'               data.frame(country = rep("URY", 4),
#'                          year = c(1995, 2000, 2005, 2010),
#'                          party_A = c(30,30,20,20),
#'                          party_B = c(30,50,40, 30),
#'                          party_C = c(30,10,30, 25),
#'                          party_D = c(10,10,10,25)),
#'               data.frame(country = rep("BRA", 2),
#'                          year = c(1993, 1998),
#'                          party_A = c(30, 55),
#'                          party_B = c(70, 45)))
#'
#' votes <- esaps_object(dataset = votes, name.country = "country", name.year = "year")
#' volatility <- evolat(esapsObject = votes, method = "Pedersen", summary = TRUE)
#' @export


evolat<-function(esapsObject,
                 method,
                 threshold=2,
                 summary=FALSE,
                 digits=2){
        if (!class(esapsObject) == "esaps"){
                stop("Input is not of class 'esaps'.")
        }
        if (threshold < 0){
                stop("'threshold' cannot be negative.")
        }
        if (digits < 0){
                stop("'digits' cannot be negative.")
        }
        if (sum(sapply(esapsObject, nrow) %in% 1L)!= 0){
                loc.error <- which(sapply(esapsObject, nrow) %in% 1L)
                error <- character()
                for(i in 1:length (loc.error)){
                        error[i] <- esapsObject[[loc.error[i]]][1,1]
                }
                stop(cat("In these countries there is only one year entered:", paste(error)))
        }
        calculus <- function(dat){
                volatA <- dat
                for(i in 1:length(volatA)){
                        for(j in 1:length(volatA[[i]])){
                                for(k in 1:(ncol(volatA[[i]][[j]])-2)){
                                        volatA[[i]][[j]][2,(k+2)] <- abs(volatA[[i]][[j]][1,(k+2)] - volatA[[i]][[j]][2,(k+2)])
                                }
                                volatA[[i]][[j]]$volat_A <- round((1/2)*sum(volatA[[i]][[j]][2,3:ncol(volatA[[i]][[j]])]),digits)
                                volatA[[i]][[j]] <- volatA[[i]][[j]][2,c(1,2,ncol(volatA[[i]][[j]]))]
                        }
                        volatA[[i]]<-do.call(rbind, volatA[[i]])
                }
                volatA <- do.call(rbind, volatA)
                return(volatA)
        }
        if (method == "Powell and Tucker"){
                data1 <- esapsObject
                if(sum(colnames(data1[[1]]) %in% "M") > 0L){
                        for(i in 1:length(data1)){
                                data1[[i]] <- data1[[i]][,-c(ncol(data1[[i]]))]
                        }
                }
                for(i in 1: length(data1)){
                        data1[[i]]<-cbind(data1[[i]],party_ghostA=threshold-1,party_ghostB=threshold+1)
                }
                list_A <-list()
                list_B <-list()
                for(j in 1:length(data1)){
                        list_A[[j]]<-list()
                        list_B[[j]]<-list()
                        for(i in 1:(nrow(data1[[j]])-1)){
                                list_A[[j]][[i]] <- list()
                                list_B[[j]][[i]] <- list()
                                corte <- as.data.frame(which(data1[[j]][c(i,i+1),3:ncol(data1[[j]])]<=threshold, arr.ind=T))
                                list_A[[j]][[i]] <- data1[[j]][c(i,i+1), c(1,2,(unique(corte$col)+2))]
                                list_B[[j]][[i]] <- data1[[j]][c(i,i+1),-(corte$col+2)]
                        }
                }
                volat_PTA <- calculus(list_A)
                volat_PTB <- calculus(list_B)
                colnames(volat_PTB)[3] <- "volat_B"
                volat_PT <- merge(volat_PTA, volat_PTB)
                volat_PT <- volat_PT[order(volat_PT$country),]
                rownames(volat_PT) <- NULL
                if (summary==TRUE){
                        tab <- plyr::ddply(volat_PT, ~ country,
                                            function(x) c(min.year = min(x$year),
                                                          max.year = max(x$year),
                                                          election = length(x$country),
                                                          mean_A = round(mean(x$volat_A), digits),
                                                          mean_B = round(mean(x$volat_B), digits)))

                        out_PT <- list(volat_PT, tab)
                        return(out_PT)

                }
                return(volat_PT)
        }
        if (method == "Torcal and Lago"){
                data2 <- esapsObject
                if(sum(sapply(sapply(data2, colnames), function(x){sum(x %in% c("M"))})) != length(data2)){
                        stop("Missing data on the district magnitude.")
                }
                lista_END <- list()
                lista_EXO <- list()
                for(i in 1:length(data2)){
                        lista_END[[i]] <- list()
                        lista_EXO[[i]] <- list()
                        for(j in 1:(nrow(data2[[i]])-1)){
                                lista_END[[i]][[j]] <- data2[[i]][c(j,j+1),]
                                lista_EXO[[i]][[j]] <- data2[[i]][c(j,j+1),]
                                srow <- sort(lista_END[[i]][[j]][2,3:(ncol(lista_END[[i]][[j]])-1)], decreasing = T )
                                M1 <- lista_END[[i]][[j]][2,ncol(lista_END[[i]][[j]])]
                                no_zero <- which(lista_END[[i]][[j]][2,] %in% srow[1:M1])
                                lista_END[[i]][[j]][, -c(1,2, no_zero)] <- 0L
                                lista_EXO[[i]][[j]][, c(no_zero, ncol(lista_EXO[[i]][[j]]))] <- 0L
                        }
                }

                lista_END <- calculus(lista_END)
                lista_EXO <- calculus(lista_EXO)
                colnames(lista_END)[3] <- "endogenous"
                colnames(lista_EXO)[3] <- "exogenous"
                volat_TL <- merge(lista_EXO, lista_END)
                volat_TL <- volat_TL[order(volat_TL$country),]
                rownames(volat_TL) <- NULL

                if (summary==TRUE){
                        tab2 <- plyr::ddply(volat_TL, ~ country,
                                           function(x) c(min.year = min(x$year),
                                                         max.year = max(x$year),
                                                         election = length(x$country),
                                                         mean_end = round(mean(x$endogenous), digits),
                                                         mean_exo = round(mean(x$exogenous), digits)))

                        out_TL <- list(volat_TL, tab2)
                        return(out_TL)
                }
                return(volat_TL)

        }
        if (method == "Pedersen"){
                data3 <- esapsObject
                if(sum(colnames(data3[[1]]) %in% "M") > 0L){
                        for(i in 1:length(data3)){
                                data3[[i]] <- data3[[i]][,-c(ncol(data3[[i]]))]
                        }
                }
                lista_PED <- list()
                for(i in 1:length(data3)){
                        lista_PED[[i]] <- list()
                        for(j in 1:(nrow(data3[[i]])-1)){
                                lista_PED[[i]][[j]] <- data3[[i]][c(j,j+1),]
                        }
                }
                volat_PED <- calculus(lista_PED)
                colnames(volat_PED)[3] <- "eVolat"
                volat_PED <- volat_PED[order(volat_PED$country),]
                rownames(volat_PED) <- NULL
                if (summary==TRUE){
                        tab3<- plyr::ddply(volat_PED, ~ country,
                                           function(x) c(min.year = min(x$year),
                                                         max.year = max(x$year),
                                                         election = length(x$country),
                                                         mean_volat = round(mean(x$eVolat), digits)))

                        out_PED <- list(volat_PED, tab3)
                        return(out_PED)
                }
                return(volat_PED)
        }
}



















