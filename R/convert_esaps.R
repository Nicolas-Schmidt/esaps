#' @title Converts data in table form to tidy_data
#'
#' @description Convert data in table format to tidy_data to use in the indicators of
#'     the tidy_data.
#' @param path Character vector containing one or more path names.
#' @param dataset Electoral results by party. It can be a \code{data.frame} or a \code{list}.
#' @param file.name Name of the data file with extension.
#' @param nSheets Number of countries (number of sheets).'
#'     'Country' is a generic unit of analysis (province, department, etc.)
#' @param election.name Name of the variable that contains elections.
#' @param unit.name Name of the variable that contains the unit.
#'     'unit' is a generic unit of analysis (province, department, etc.)
#' @param M.name Name of the variable that contains the district magnitude
#'     (M+1). It is for the calculation of endogenous and exogenous
#'     electoral volatility (Torcal and Lago, 2015).
#' @param votes_nac.name la la la
#' @param allSheet By default it is \code{FALSE}. Load all the sheets that are in the
#'     files selected in \code{file.name}. This argument takes precedence over \code{nSheets}.
#' @param seats By default it is \code{FALSE}. If it is \code{TRUE}, it indicates that, in addition
#'     to electoral data per party, there is allocation data per seat. In this case,
#'     one column must be loaded for each party with the electoral result and another
#'     with the number of seats it obtained. The structure must be:
#'     party_1, party_2, ..., party_n, seats_party_1, seats_party_2, ..., seats_party_n.
#'
#'
#' @return data.frame
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
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
#' votes <- convert_esaps(dataset = votes, unit.name = "country", election.name = "year")
#'
#'
#' votes2 <- data.frame(year = c(2000, 2005),
#'                      country = "URY",
#'                      votes_party1 = c(20, 30),
#'                      votes_party2 = c(30, 35),
#'                      votes_party3 = c(40, 25),
#'                      votes_party4 = c(10, 10),
#'                      seats_party1 = c(25, 35),
#'                      seats_party2 = c(20, 30),
#'                      seats_party3 = c(40, 30),
#'                      seats_party4 = c(15, 5))
#'
#' votes <- convert_esaps(dataset = votes2, unit.name = "country", election.name = "year", seats = TRUE)
#'
#'
#' \dontrun{
#' v1 <- convert_esaps(path = getwd(),
#'                    file.name = c("electionBRA.xlsx", "electionARG.xlsx"),
#'                    election.name = "elec",
#'                    unit.name = "district",
#'                    allSheet = TRUE)
#'
#' v2 <-convert_esaps(path = getwd(),
#'                    file.name = c("ARG.ods", "URY.ods", "BRA.ods"),
#'                    nCountry = c(2, 3, 1),
#'                    election.name = "elec",
#'                    unit.name = "province")
#'
#' v3 <- convert_esaps(path = here::here(),
#'                    file.name = list.files(pattern = "*.xlsx"),
#'                    election.name = "year",
#'                    unit.name = "country",
#'                    M.name = "magnitude",
#'                    seats = TRUE,
#'                    allSheet = TRUE)
#' }
#'
#' @export


convert_esaps<-function(path = NULL,
                        dataset = NULL,
                        file.name = NULL,
                        nSheets = 1,
                        election.name,
                        unit.name,
                        M.name = NULL,
                        votes_nac.name = NULL,
                        seats = FALSE,
                        allSheet = FALSE){

        if (is.null(path) && is.null(dataset)){
                stop("any of the arguments 'path' or 'dataset' must not be NULL.")
        }
        if (!is.null(path)) {
                extention <- c("xlsx", "ods")
                extention <- extention[c(all(grepl("xlsx$", file.name)==TRUE), all(grepl("ods$", file.name)==TRUE))]
                if (length(extention) != 1L){
                        stop("It only accepts extension 'xlsx' or 'ods'.")
                }
        if(length(file.name)!=length(nSheets)){
                nSheets <- rep(nSheets[1], length(file.name))
        }
        old_setwd <- getwd()
        setwd(path)
        if (extention == "ods") {
                if(isTRUE(allSheet)){
                        nSheets <- vector("numeric", length = length(file.name))
                        for(i in 1:length(file.name)){
                                nSheets[i] <- length(readODS::ods_sheets(file.name[i]))
                        }
                }
                dat <- list()
                init <- 0L
                for(i in 1:length(file.name)){
                        for (j in 1:nSheets[i]) {
                                dat[[j+init]] <- as.data.frame(readODS::read_ods(file.name[i], sheet = j))
                        }
                        init <- length(dat)
                }
        }
        if (extention == "xlsx") {
                if(isTRUE(allSheet)){
                        nSheets <- vector("numeric", length = length(file.name))
                        for(i in 1:length(file.name)){
                                nSheets[i] <- length(readxl::excel_sheets(file.name[i]))
                                }
                        }
                        dat <- list()
                        init <- 0L
                        for(i in 1:length(file.name)){
                                for (j in 1:nSheets[i]) {
                                        dat[[j+init]] <- as.data.frame(readxl::read_excel(file.name[i], sheet = j))
                                }
                                init <- length(dat)
                        }
                }
                setwd(old_setwd)
        }
        if (!is.null(dataset)) {
                if (is.matrix(dataset)) {
                        stop("'dataset' must be a list or a data.frame")
                }
                if (is.list(dataset) && !is.data.frame(dataset)) {
                        dat <- dataset
                }
                if (is.data.frame(dataset)) {
                        dat <- list(dataset)
                }
        }
        if (0L %in% sapply(dat, nrow)) {
                zero <- sapply(dat, nrow) %in% 0L
                if(length(zero)==sum(zero)){
                        stop("The datasets have 0 rows.")
                }else{
                        dat <- dat[!zero]
                }
        }
        if(sum(sapply(dat, function(x) c(unit.name, election.name) %in% names(x)))==0){
                stop("'election.name' and 'unit.name' must be the same on all sheets.")
        }
        vector1 <- numeric()
        vector2 <- numeric()
        for (i in 1:length(dat)) {
                vector1[i] <- which(colnames(dat[[i]]) == election.name)
                colnames(dat[[i]])[vector1[i]] <- "election"
                vector2[i] <- which(colnames(dat[[i]]) == unit.name)
                colnames(dat[[i]])[vector2[i]] <- "unit"
        }
        dat2 <- list()
        dat3 <- list()
        out  <- list()
        for (i in 1:length(dat)) {
                dat2[[i]] <- dat[[i]][ c(vector1[i], vector2[i])]
                dat3[[i]] <- dat[[i]][-c(vector1[i], vector2[i])]
                out[[i]] <- cbind(dat2[[i]], dat3[[i]])
                out[[i]]$unit <- as.character(out[[i]]$unit)
        }
        if (!is.null(M.name)) {

                if(!all(sapply(out, function(x) M.name %in% names(x)))){
                        stop("'M.name' must be the same on all sheets.")
                }
                m <- numeric()
                data2 <- list()
                for (i in 1:length(out)) {
                        m[i] <- which(names(out[[i]]) == M.name)
                        data2[[i]] <- out[[i]][,m[i]]
                        out[[i]] <- out[[i]][,-m[i]]
                        out[[i]] <- cbind(out[[i]][1:2], M = data2[[i]], out[[i]][3:ncol(out[[i]])])
                }
        }
        if (!is.null(votes_nac.name)) {
                if(isTRUE(seats) || !is.null(M.name)){
                        stop("if 'votes_nac.name' are different from NULL, 'M.name' or 'seats' must have the default values.")
                }
                out0<- lapply(out, function(x){tidyr::gather(x[which(x[,2]==votes_nac.name),-2], "party", "votes_nac", -"election")})
                out <- lapply(out, function(x){tidyr::gather(x[-which(x[,2]==votes_nac.name),], "party", "votes", -"election", -"unit")})
                output <- list()
                for(i in 1:length(out)){output[[i]] <- dplyr::inner_join(out[[i]],out0[[i]] , by=c("election", "party"))}
                pull <- do.call(rbind, output)
                rownames(pull)<-NULL
                return(pull)
        }
        variables <- c(election.name, unit.name, M.name) ## acá no va votes_nac_name???
        var <- length(variables)
        if(var > 2){variables <- c("election", "unit", "M")}else{variables <- c("election", "unit")}

        if(isTRUE(seats)){
                larS <- length(out)
                c_seats <- as.logical((sapply(out, ncol)-var) %% 2)

                if(length(which(c_seats==TRUE)) > 0){
                        out <- out[-c(which(c_seats==TRUE))]
                }
                if(length(out)!=larS && length(out) != 0){
                        warning("The database was deleted: ", paste(which(c_seats==TRUE), collapse = ", "), " ...must have the same number of columns of parties and seats")
                }
                if(length(out)==0L){
                        stop("The structure of the data is not correct.")
                }
                out3 <- lapply(out, function(x){x[,c(1:2,(((ncol(x)-var)/2)+(var+1)):ncol(x))]})
                out4 <- lapply(out, function(x){x[,-c((((ncol(x)-var)/2) + (var+1)): ncol(x))]})
                out3 <- lapply(out3, function(x){tidyr::gather(x, "party", "seats", -"election", -"unit")})
                out4 <- lapply(out4, function(x){tidyr::gather(x, "party", "votes", -variables)})

                for(i in 1:length(out4)){
                        out4[[i]]<-cbind(out4[[i]], seats = out3[[i]]$seats)
                }

                pull <- do.call(rbind, out4)
        }else{
                out4 <- lapply(out, function(x){tidyr::gather(x, "party", "votes", -variables)})
                pull <- do.call(rbind.data.frame, out4)
        }

        if(length(unique(as.character(pull$unit)))>1){
                pull <- pull[order(pull$unit), ]
        }else{
                pull <- pull[order(pull$election), ]
        }

        rownames(pull) <- NULL
        return(pull)
}






