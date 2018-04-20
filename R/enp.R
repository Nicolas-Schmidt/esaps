#' @title Effective Number of Parties
#'
#' @description The Effective Number of Parties (ENP) is an index developed by
#'     \href{http://bit.ly/2oYU4dV}{Laakso and Taagepera (1979)} that allows to
#'     count the relevant parties in a party system. The formula consists on
#'     dividing one over the sum of the squares of the proportions (votes or seats)
#'     that the parties obtain in an electoral instance.
#' @param esapsObject An \code{esaps} class object. Function: \code{\link{esaps_object}}.
#' @param summary Summary of the data by country, by default it is \code{FALSE}.
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
#' en_party <- enp(votes, summary = TRUE)
#' @export


enp <- function(esapsObject,
                summary=FALSE){
        if (!class(esapsObject) == "esaps"){
                stop("Input is not of class 'esaps'.")
        }
        NEP  <- function(x){1/sum((x/100)^2)}
        out2 <- lapply(esapsObject, function(x){round(apply(x[,3:ncol(x)], 1, NEP),2)})
        out3 <- list()
        for(i in 1:length(esapsObject)){
                out3[[i]] <- cbind(esapsObject[[i]][,1:2], nep=out2[[i]])
        }
        output <- do.call(rbind.data.frame, out3)
        output <- output[order(output$country),]
        rownames(output) <- NULL
        if (summary==TRUE){
                tab <- plyr::ddply(output, ~ country,
                                   function(x) c(min.year = min(x$year),
                                                 max.year = max(x$year),
                                                 election = length(unique(x$country)),
                                                 mean_nep = round(mean(x$nep), 2)))
                out_nep <- list(output, tab)
                return(out_nep)
        }
        return(output)
}




















