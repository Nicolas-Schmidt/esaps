#' @title Electoral volatility calculation
#'
#' @description Electoral volatility calculation:
#'     \href{http://bit.ly/2p4cyZQ}{Pedersen (1979)},
#'     \href{http://bit.ly/2FpoeBm}{Powell and Tucker (2014)} and
#'     \href{http://bit.ly/2FD83zE}{Torcal and Lago (2015)}.
#' @param tidy_data data.frame that contains the following variables with these names:
#'    \itemize{
#'             \item{\code{election}: year of election.}
#'             \item{\code{unit}: the unit of analysis (province, department ...)}
#'             \item{\code{party}: name of the political parties that obtain votes.}
#'             \item{\code{votes}: votes obtained by each party.}
#'             \item{\code{M}: magnitude of the district. Only if the \code{method = 3}. It refers to the M + 1 rule (only 'M' must be loaded).}
#'            }
#'    If the data is not structured in this way you can order it with: \code{\link{convert_esaps}}.
#' @param method Method to calculate electoral volatility:
#'     \itemize{
#'             \item{\code{method = "Pedersen"} or \code{method = 1}.}
#'             \item{\code{method = "Powell and Tucker"} or \code{method = 2}.}
#'             \item{\code{method = "Torcal and Lago"} or \code{method = 3}.}
#'             }
#' @param threshold Minimum threshold for 'Type A' electoral volatility calculation
#'     (Powell and Tucker, 2014). By default is 2\%.
#' @param summary Summary of data by unit, by default it is \code{FALSE}.
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @param scale By default it is \code{100}, the indices will vary between 0 and 100.
#'     If \code{scale = 1} the variation will be between 0 and 1.
#'
#'
#'
#' @return if \code{summary = FALSE,} return data.frame.
#'
#'     if \code{summary = TRUE}, return a list with two data.frame.
#'     \itemize{
#'             \item{\code{list[[1]]}} {Indicator}
#'             \item{\code{list[[2]]}} {Summary by 'unit'}
#'                   \itemize{
#'                       \item{\code{min} variable 'election'}
#'                       \item{\code{max} variable 'election'}
#'                       \item{number of elections}
#'                       \item{\code{mean} indicator}
#'                       \item{\code{standard deviation} indicator}
#'                       }
#'      }
#'
#'
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @examples
#'
#' votes <- data.frame(election = rep(c(1995, 2000, 2005, 2010),4),
#'                     unit = "ARG",
#'                     party = rep(c("party_A","party_B","party_C","party_D"), each = 4),
#'                     votes = c(30,30,20,20,30,50,40,30,30,10,30,25,10,10,10,25))
#' evolat(votes, 1)
#' evolat(tidy_data = votes, method = 1, summary = TRUE)
#'
#' @export


evolat<-function(tidy_data,
                 method,
                 threshold = 2,
                 summary = FALSE,
                 digits = 2,
                 scale = 100){


        if(!is.data.frame(tidy_data)){stop("'tidy_data' must be a 'data.frame'.", call. = FALSE)}
        if (threshold < 0){stop("'threshold' cannot be negative.", call. = FALSE)}
        if (digits < 0){stop("'digits' cannot be negative.", call. = FALSE)}
        if(length(method) > 1){stop("you must select only one method.", call. = FALSE)}

        ch.met <- c("Pedersen", "Powell and Tucker", "Torcal and Lago")
        nu.met <- c(1, 2, 3)
        if(!any(method == ch.met | method == nu.met)){stop("the selected method does not exist.", call. = FALSE)}

        vars <- c("election", "unit", "M", "party", "votes")
        if(all(vars %in% names(tidy_data))){
                        if(ncol(tidy_data) > 5){
                                tidy_data <- tidy_data[, vars]
                        }else{
                                tidy_data <- tidy_data[, vars]
                        }
                }else{
                        vars <- vars[-3]
                        if(all(vars %in% names(tidy_data))){
                                if(ncol(tidy_data) > 4){
                                        tidy_data <- tidy_data[, vars]
                                }else{
                                        tidy_data <- tidy_data[, vars]
                                }
                        }else{
                               stop("the name of the variables are not correct.")
                        }
        }

        if(sum(is.na(tidy_data[,1:2])) != 0){stop("The variable 'election','unit','votes' or 'M' must not have NA values.", call. = FALSE)}
        if("M" %in% names(tidy_data)){
                tidy_data$M <- tidy_data$M+1
        }
        tidy_data <- unlist(lapply(split(tidy_data, tidy_data$unit), function(x){split(x, x$election)}), recursive = FALSE)
        tidy_data <- do.call(rbind, lapply(tidy_data, function(x){cbind(x[,-c(which(names(x) == "votes"))],  votes = (x$votes/sum(x$votes, na.rm=TRUE))*scale)}))
        tidy_data <- lapply(split(tidy_data, tidy_data$unit), function(x){tidyr::spread(x, "party", "votes")})

        for (i in 1:length(tidy_data)) {
                tidy_data[[i]][is.na(tidy_data[[i]])] <- 0L
        }

        if (method == "Powell and Tucker" || method == 2){
                threshold <- threshold/scale
                data1 <- tidy_data
                if("M" %in% names(data1[[1]])){
                        data1 <- lapply(data1,"[",-3)
                }
                for(i in 1: length(data1)){
                        data1[[i]] <- cbind(data1[[i]],party_ghostA=threshold-1,party_ghostB=threshold+1)
                }
                list_A <-list()
                list_B <-list()
                for(j in 1:length(data1)){
                        list_A[[j]] <- list()
                        list_B[[j]] <- list()
                        for(i in 1:(nrow(data1[[j]])-1)){
                                list_A[[j]][[i]] <- list()
                                list_B[[j]][[i]] <- list()
                                corte <- as.data.frame(which(data1[[j]][c(i,i+1),3:ncol(data1[[j]])]<=threshold, arr.ind=T))
                                list_A[[j]][[i]] <- data1[[j]][c(i,i+1), c(1,2,(unique(corte$col)+2))]
                                list_B[[j]][[i]] <- data1[[j]][c(i,i+1),-(corte$col+2)]
                        }
                }
                volat_PTA <- calculus(dat = list_A, digits = digits)
                volat_PTB <- calculus(dat = list_B, digits = digits)
                volat_PT  <- cbind.data.frame(volat_PTA, volat_B = volat_PTB[,3])
                volat_PT  <- volat_PT[order(volat_PT$unit),]
                rownames(volat_PT) <- NULL
                if (summary == TRUE){
                        tab <- plyr::ddply(volat_PT, ~ unit,
                                           function(x) c(first_elec = min(x$election),
                                                         last_elec  = max(x$election),
                                                         election   = length(x$unit),
                                                         mean_A     = round(mean(x$volat_A), digits),
                                                         sd_A       = round(stats::sd(x$volat_A), digits),
                                                         mean_B     = round(mean(x$volat_B), digits),
                                                         sd_B       = round(stats::sd(x$volat_B), digits)))
                        out_PT <- list(volat_PT, tab)
                        return(out_PT)
                }
                return(volat_PT)
        }

        if (method == "Torcal and Lago" || method == 3){
                if(names(tidy_data[[1]])[3] != "M"){stop("The magnitude is missing.")}
                data2 <- tidy_data

                lista_END <- list()
                lista_EXO <- list()
                for(i in 1:length(data2)){
                        lista_END[[i]] <- list()
                        lista_EXO[[i]] <- list()
                        for(j in 1:(nrow(data2[[i]])-1)){
                                lista_END[[i]][[j]] <- data2[[i]][c(j,j+1),]
                                lista_EXO[[i]][[j]] <- data2[[i]][c(j,j+1),]
                                srow <- sort(lista_END[[i]][[j]][2,4:ncol(lista_END[[i]][[j]])], decreasing = TRUE ) ###
                                M1 <- lista_END[[i]][[j]][2,3] ###
                                no_zero <- which(lista_END[[i]][[j]][2,] %in% srow[1:M1])
                                lista_END[[i]][[j]][, -c(1:2, no_zero)] <- 0L
                                lista_EXO[[i]][[j]][, c(no_zero, 3)] <- 0L
                        }
                }
                lista_END <- calculus(dat = lista_END, digits = digits)
                lista_EXO <- calculus(dat = lista_EXO, digits = digits)
                colnames(lista_EXO)[3] <- "exogenous"
                volat_TL <- cbind.data.frame(lista_EXO, endogenous = lista_END[,3])
                volat_TL <- volat_TL[order(volat_TL$unit),]
                rownames(volat_TL) <- NULL

                if (summary == TRUE){
                        tab2 <- plyr::ddply(volat_TL, ~ unit,
                                            function(x) c(first_elec = min(x$election),
                                                          last_elec  = max(x$election),
                                                          election   = length(x$unit),
                                                          mean_end   = round(mean(x$endogenous), digits),
                                                          sd_end     = round(stats::sd(x$endogenous), digits),
                                                          mean_exo   = round(mean(x$exogenous), digits),
                                                          sd_exo     = round(stats::sd(x$exogenous), digits)))
                        out_TL <- list(volat_TL, tab2)
                        return(out_TL)
                }
                return(volat_TL)
        }
        if (method == "Pedersen" || method == 1){
                data3 <- tidy_data
                if("M" %in% names(data3[[1]])){
                        data3<- lapply(data3,"[",-3)
                }
                lista_PED <- list()
                for(i in 1:length(data3)){
                        lista_PED[[i]] <- list()
                        for(j in 1:(nrow(data3[[i]])-1)){
                                lista_PED[[i]][[j]] <- data3[[i]][c(j,j+1),]
                        }
                }
                volat_PED <- calculus(dat = lista_PED, digits = digits)
                colnames(volat_PED)[3] <- "eVolat"
                volat_PED <- volat_PED[order(volat_PED$unit),]
                rownames(volat_PED) <- NULL

                if (summary == TRUE){
                        tab3 <- plyr::ddply(volat_PED, ~ unit,
                                           function(x) c(first_elec = min(x$election),
                                                         last_elec  = max(x$election),
                                                         election   = length(x$unit),
                                                         mean_volat = round(mean(x$eVolat), digits),
                                                         sd_volat   = round(stats::sd(x$eVolat), digits)))

                        out_PED <- list(volat_PED, tab3)
                        return(out_PED)
                }
                return(volat_PED)
        }
}






