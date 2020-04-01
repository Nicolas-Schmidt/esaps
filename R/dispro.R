#' @title Electoral Disproportionality
#'
#' @description Electoral Disproportionality:
#'     Rae (1971),
#'     \href{https://bit.ly/2B1mIDH}{Loosemore and Hanby (1971)},
#'     Lijphart (1986),
#'     \href{https://bit.ly/2McLshB}{Lijphart (1994)},
#'     \href{https://bit.ly/2M6CPoy}{Gallagher (1991)} and
#'     \href{https://bit.ly/2M9Bttr}{Cox and Shugart (1991)}.
#'
#' @param tidy_data data.frame that contains the following variables with these names:
#'    \itemize{
#'             \item{\code{election}: year of election.}
#'             \item{\code{unit}: the unit of analysis (province, department ...)}
#'             \item{\code{party}: name of the political parties that obtain votes.}
#'             \item{\code{votes}: votes obtained by each party.}
#'             \item{\code{seats}: .}
#'            }
#'    If the data is not structured in this way you can order it with: \code{\link{convert_esaps}}.
#' @param method Method to calculate electoral volatility:
#'     \itemize{
#'             \item{\code{method = "Rae"} or \code{method = 1}.}
#'             \item{\code{method = "Loosemore and Hanby"} or \code{method = 2}.}
#'             \item{\code{method = "Lijphart_1"} or \code{method = 3}.}
#'             \item{\code{method = "Lijphart_2"} or \code{method = 4}.}
#'             \item{\code{method = "Gallagher"} or \code{method = 5}.}
#'             \item{\code{method = "Cox and Shugart"} or \code{method = 6}.}
#'             }
#' @param scale By default it is \code{100}, the indices will vary between 0 and 100.
#'     If \code{scale = 1} the variation will be between 0 and 1.
#'
#' @return data.frame.
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @examples
#' votes <- data.frame(election = rep(c(2000, 2005), each = 4),
#'                    unit  = rep(c("ARG", "URY"), each = 4),
#'                    party = c("party_A", "party_B","party_C","party_D"),
#'                    votes = c(20, 30, 40, 10, 30, 35, 25, 10),
#'                    seats = c(25, 20, 40, 15, 35, 30, 30, 5)
#'                    )
#'
#' dispro(votes, 1:6, 1)
#' dispro(votes, 3)
#' dispro(tidy_data = votes, method = 3:5)
#' dispro(tidy_data = votes, method = c(1,3,6))
#' dispro(tidy_data = votes, method = c("Rae", "Gallagher"))
#'
#'
#' @export



dispro <- function(tidy_data,
                   method,
                   scale = 1){

        if(!is.data.frame(tidy_data)){
                stop("'tidy_data' must be a 'data.frame'.")
        }
        tidy_data <- as.data.frame(tidy_data)
        if(missing(method)){
                stop("You must select only one method.")
        }
        ch.met <- c("Rae",
                    "Loosemore and Hanby",
                    "Lijphart_1",
                    "Lijphart_2",
                    "Gallagher",
                    "Cox and Shugart")

        nu.met <- 1:length(ch.met)

        if(is.numeric(method)){
                if(!all(method %in% nu.met) ){
                        stop("The selected method does not exist.")
                }
                met <- method + 2
        }
        if(is.character(method)){
                if(!all(method %in% ch.met)){
                        stop("The selected method does not exist.")
                }
                met <- which(ch.met %in% method)
                met <- met + 2
        }
        if(scale != 100 && scale != 1){
                stop("The value of 'scale' is not correct.")
        }
        vars <- c("election", "unit", "party", "votes", "seats")
        if(all(vars %in% names(tidy_data))){
               if(ncol(tidy_data)!=5){
                       tidy_data <- tidy_data[, vars]
               }
        }else{
                stop("The names of the variables should be: 'election', 'unit', 'party', 'votes' and 'seats'.")
        }

        tidy_data <- tidy_data[, vars]
        if(sum(is.na(tidy_data[, -3])) != 0){
                stop("The variable 'election','unit','votes' or 'seats' must not have NA values.")
        }
        v1 <- unlist(lapply(split(tidy_data, tidy_data$unit), function(x){split(x, x$election)}), recursive = FALSE)
        v2 <- lapply(v1, function(x){cbind.data.frame(x, t.votes = (x$votes/sum(x$votes))*scale,
                                                      t.seats = (x$seats/sum(x$seats))*scale )})
        v3 <- lapply(v2, function(x){cbind.data.frame(x, abso = abs(x$t.votes-x$t.seats))})
        v4 <- lapply(v3, function(x){cbind.data.frame(x,
                                                      Rae = round(sum(x$abso/nrow(x)),2),
                                                      LH = round(sum(x$abso/2),2),
                                                      Lijphart_1 = round(max(x$abso),2),
                                                      Lijphart_2 = round(sum(x$abso)/(1/sum((x$t.votes/sum(x$t.votes))^2)),2),
                                                      Gallagher = round(sqrt(sum(x$abso/2)),2),
                                                      Cox_Shugart = round(stats::lm(x$t.votes ~ x$t.seats)$coefficients[2],2)
                                                      )})
        eof<- do.call(rbind, lapply(v4, "[",1, -c(3:8)))
        rownames(eof) <- NULL
        out <- eof[,c(1,2, met)]
        out
}








