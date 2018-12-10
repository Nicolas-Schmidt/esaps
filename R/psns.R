#' @title Party System Nationalization Score
#'
#' @description Party System Nationalization Score
#'     \href{https://bit.ly/2w9QgtO}{Mainwaring and Jones (2003)} and
#'     \href{https://bit.ly/2oj76SY}{Chhibber and Kollman (2004)}
#'
#' @param tidy_data data.frame that contains the following variables with these names:
#'    \itemize{
#'             \item{\code{election}: year of election.}
#'             \item{\code{unit}: the unit of analysis (province, department ...)}
#'             \item{\code{party}: name of the political parties that obtain votes.}
#'             \item{\code{votes}: votes obtained by each party.}
#'             \item{\code{votes_nac}: votes at national level for each party.}
#'            }
#'    If the data is not structured in this way you can order it with: \code{\link{convert_esaps}}.
#'
#' @param method Method to calculate Party System Nationalization Score:
#'     \itemize{
#'             \item{\code{method = "Mainwaring and Jones"} or \code{method = 1}.}
#'             \item{\code{method = "Chhibber and Kollman"} or \code{method = 2}.}
#'             }
#'
#' @param pns by default it is \code{FALSE}. If \code{TRUE}, the Party Nationalization Score
#'     is calculated. In method, you must indicate: \code{method = 1}.
#'
#' @param scale By default it is \code{100}, the indices will vary between 0 and 100.
#'     If \code{scale = 1} the variation will be between 0 and 1.
#'
#'
#'
#' @return if \code{pns = FALSE,} return data.frame.
#'
#'     if \code{pns = TRUE}, return a list with two data.frame.
#'     \itemize{
#'             \item{\code{list[[1]]}} {PSNS: Party System Nationalization Score}
#'             \item{\code{list[[2]]}} {PNS: Party Nationalization Score}
#'            }
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#'
#'
#' @examples
#' votes <- data.frame(election = rep(2000,4),
#'                     unit  = rep(c("District_1", "District_2"), each = 2),
#'                     party = rep(c("party_A", "party_B"), 2),
#'                     votes = c(0.60,0.40, 0.30, 0.70),
#'                     votes_nac = rep(c(0.55,0.45),2)
#'                    )
#' psns(tidy_data = votes, method = 1)
#' psns(tidy_data = votes, method = 1, pns = TRUE)
#'
#' @export
#'



psns <- function(tidy_data,
                method,
                pns = FALSE,
                scale = 100){

        if(!is.data.frame(tidy_data)){
                stop("'tidy_data' must be a 'data.frame'.")
        }

        if(missing(method)){
                stop("You must select only one method.")
        }

        if(sum(is.na(tidy_data[, 1:2])) != 0){
                stop("The variable 'election'and 'unit' must not have NA values.")
        }

        if(length(method)>1){
                stop("you must select only one method.")
        }

        ch.met <- c("Mainwaring and Jones", "Chibber and Kollman")
        nu.met <- c(1, 2)
        if(!any(method == ch.met | method == nu.met)){
                stop("the selected method does not exist.")
        }

        if(scale != 100 && scale != 1){
                stop("The value of 'scale' is not correct.")
        }
        vscale <- unlist(lapply(split(tidy_data, tidy_data$election), function(x){split(x, x$unit)}), recursive = FALSE)
        tidy_data <- lapply(vscale, function(x){cbind.data.frame(x, t.votes = (x$votes/sum(x$votes, na.rm=TRUE))*scale,
                                                                             t.votes_nac = (x$votes_nac/sum(x$votes_nac, na.rm=TRUE))*scale )})
        tidy_data <- do.call(rbind, lapply(tidy_data, "[", -c(4:5)))
        rownames(tidy_data)<-NULL

        if(method=="Mainwaring and Jones" || method == 1){
                gini_esaps <- function(v){
                        v <- ifelse(is.na(v), 0, v)
                        v <- v[order(v)]
                        l <- length(v)
                        g <- sum(v*1L:l)
                        g <- 2 * g/sum(v) - (l + 1L)
                        round(1-(g/l), 3)
                }
                v1 <- unlist(lapply(split(tidy_data, tidy_data$election), function(x){split(x, x$party)}), recursive = FALSE)
                v2 <- lapply(v1, function(x){cbind(x, pns = apply(x[4], 2, gini_esaps))})
                pns1 <- do.call(rbind, lapply(v2, function(x){x[1,-2]}))
                psns <- lapply(split(pns1, pns1$election), function(x){cbind(x, psns = round(sum(x$t.votes_nac*x$pns),3))})
                psns <- do.call(rbind,lapply(psns, "[", 1, c(1, 6)))
                rownames(psns) <- NULL

                if(isTRUE(pns)){
                        pn <- pns1[order(pns1$party), c(2,1,5)]
                        rownames(pn) <- NULL
                        return(list(PSNS = psns, PNS = pn))
                }
                return(psns)
        }

        if(method=="Chibber and Kollman" || method == 2){
                ENP  <- function(x){1/sum((x/sum(x, na.rm=TRUE))^2, na.rm=TRUE)}
                nep_nac <- lapply(lapply(split(tidy_data, tidy_data$election),
                                         function(x){x[duplicated(x$party)==FALSE, ]}),
                                                function(x){ENP(x$t.votes_nac)})
                nep_loc <- unlist(lapply(split(tidy_data, tidy_data$election), function(x){split(x, x$unit)}), recursive = FALSE)
                nep_loc <- do.call(rbind,lapply(nep_loc, function(x){cbind(x, nepl = ENP(x$t.votes))}))
                out <- lapply(split(nep_loc, nep_loc$election), function(x){mean(x[duplicated(x$unit)==FALSE, "nepl"])})
                output <- data.frame(CH_K = round(unlist(nep_nac)-unlist(out),3))
                output

        }

}































