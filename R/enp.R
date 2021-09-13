#' @title Effective Number of Parties
#'
#' @description The Effective Number of Parties (ENP) is an index developed by
#'     Laakso and Taagepera (1979) that allows to count the relevant parties in a
#'     party system. The formula consists on dividing one over the sum of the squares
#'     of the proportions (votes or seats) that the parties obtain in an electoral instance.
#' @param tidy_data data.frame that contains the following variables with these names:
#'    \itemize{
#'             \item{\code{election}: year of election.}
#'             \item{\code{unit}: the unit of analysis (province, department ...)}
#'             \item{\code{party}: name of the political parties that obtain votes.}
#'             \item{\code{votes}: votes obtained by each party.}
#'             \item{\code{seats}: .}
#'            }
#'    If the data is not structured in this way you can order it with: \code{\link{convert_esaps}}.
#' @param enp_seats enp_seats = TRUE allows us to calculate jointly the effective
#'     number of electoral parties and in the congress.
#' @param summary Summary of the data by unit, by default it is \code{FALSE}.
#'
#'
#' @return if \code{summary = FALSE,} return data.frame.
#'
#'     if \code{summary = TRUE}, return a list with two data.frame.
#'     \itemize{
#'             \item{\code{list[[1]]}} {Indicator}
#'             \item{\code{list[[2]]}} {Summary}
#'                   \itemize{
#'                       \item{\code{min} variable 'election'}
#'                       \item{\code{max} variable 'election'}
#'                       \item{number of elections}
#'                       \item{\code{mean} indicator}
#'                       }
#'      }
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @examples
#' votes <- data.frame(election = rep(c(2000, 2005), each = 4),
#'                    unit  = rep(c("ARG", "URY"), each = 4),
#'                    party = c("party_A", "party_B","party_C","party_D"),
#'                    votes = c(20, 20, 50, 10, 30, 35, 25, 10),
#'                    seats = c(25, 25, 40, 10, 30, 30, 30, 10)
#'                    )
#'
#' enp(votes)
#' enp(votes, enp_seats = TRUE)
#' enp(votes, summary = TRUE)
#'
#' @export


enp <- function(tidy_data,
                enp_seats = FALSE,
                summary = FALSE){

        if(!is.data.frame(tidy_data)){stop("'tidy_data' must be a 'data.frame'.", call. = FALSE)}

        vars <- c("election", "unit", "party", "votes", "seats")
        if(isTRUE(enp_seats)){
                if(all(vars %in% names(tidy_data))){
                        loc <- which(names(tidy_data) %in% vars[4:5])
                        colnames(tidy_data)[loc] <- c("enp", "enp_c")
                        vars[4:5] <- c("enp", "enp_c")
                        if(ncol(tidy_data) > 5){
                                tidy_data <- tidy_data[, vars]
                        }else{
                                tidy_data <- tidy_data[, vars]
                        }
                }else{
                        stop("The names of the variables should be: 'election', 'unit', 'party', 'votes' and 'seats'.", call. = FALSE)
                }
        }else{
                vars <- vars[-5]
                if(all(vars %in% names(tidy_data))){
                        loc <- which(names(tidy_data)%in%vars[4])
                        colnames(tidy_data)[loc] <- "enp"
                        vars[4] <- "enp"
                        if(ncol(tidy_data) > 4){
                                tidy_data <- tidy_data[, vars]
                        }else{
                                tidy_data <- tidy_data[, vars]
                        }
                }else{
                        stop("The names of the variables should be: 'election', 'unit', 'party' and 'votes'.", call. = FALSE)
                }
        }
        if(sum(is.na(tidy_data[, -3])) != 0){stop("The variable 'election','unit','votes' or 'seats' must not have NA values.", call. = FALSE)}
        v1     <- unlist(lapply(split(tidy_data, tidy_data$unit), function(x){split(x, x$election)}), recursive = FALSE)
        out    <- lapply(v1, function(x){round(apply(x[-c(1:3)], 2, ENP),2)})
        output <- cbind(do.call(rbind, lapply(v1,"[",1, 1:2)), do.call(rbind, out))
        output <- output[order(output$unit),]
        rownames(output) <- NULL

        if (summary==TRUE){
                if(isTRUE(enp_seats)){
                        tab <- plyr::ddply(output, ~ unit,
                                   function(x) c(min.election = min(x$election),
                                                 max.election = max(x$election),
                                                 n.election   = length(x$election),
                                                 mean_enp     = round(mean(x$enp), 2),
                                                 sd_enp       = round(stats::sd(x$enp), 2),
                                                 mean_enp_c   = round(mean(x$enp_c), 2),
                                                 sd_enp_c     = round(stats::sd(x$enp_c), 2)))
                        out_enp <- list(output, tab)
                        return(out_enp)
                }else{
                        tab <- plyr::ddply(output, ~ unit,
                                   function(x) c(min.election = min(x$election),
                                                 max.election = max(x$election),
                                                 n.election   = length(x$election),
                                                 mean_enp     = round(mean(x$enp), 2),
                                                 sd_enp       = round(stats::sd(x$enp), 2)))
                        out_enp <- list(output, tab)
                        return(out_enp)
                }
        }
        output
}










