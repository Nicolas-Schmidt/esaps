
calculus <- function(dat, digits){
        volatA <- dat
        for(i in 1:length(volatA)){
                for(j in 1:length(volatA[[i]])){
                        for(k in 1:(ncol(volatA[[i]][[j]])-2)){
                                volatA[[i]][[j]][2,(k+2)] <- abs(volatA[[i]][[j]][1,(k+2)] - volatA[[i]][[j]][2,(k+2)])
                        }
                        volatA[[i]][[j]]$volat_A <- round((1/2)*sum(volatA[[i]][[j]][2,3:ncol(volatA[[i]][[j]])]), digits)
                        volatA[[i]][[j]] <- volatA[[i]][[j]][2,c(1,2,ncol(volatA[[i]][[j]]))]
                }
                volatA[[i]]<-do.call(rbind, volatA[[i]])
        }
        volatA <- do.call(rbind, volatA)
        return(volatA)
}


#ENP  <- function(x){1/sum((x/sum(x))^2)}
ENP  <- function(x){1/sum((x/sum(x, na.rm = TRUE))^2, na.rm = TRUE)}

gini_esaps <- function(v){
        v <- ifelse(is.na(v), 0, v)
        v <- v[order(v)]
        l <- length(v)
        g <- sum(v*1L:l)
        g <- 2 * g/sum(v) - (l + 1L)
        round(1-(g/l), 3)
}


if(getRversion() >= "2.15.1"){utils::globalVariables(c('digits'))}





