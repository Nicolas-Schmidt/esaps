#' @title Create an object of class esaps
#'
#' @description Create an object of \code{esaps} class to calculate party system indicators.
#' @param path Character vector containing one or more path names.
#' @param dataset Electoral results by party. It can be a \code{matrix}, a
#'     \code{data.frame} or a \code{list}.
#' @param name.file Name of the database file.
#' @param extention Extension of the database format.
#' @param nCountry Number of countries (number of sheets).
#' @param name.year Name of the variable that contains years.
#' @param name.country Name of the variable that contains the country.
#' @param name.M Name of the variable that contains the district magnitude
#'     (M+1). It is for the calculation of endogenous and exogenous
#'     electoral volatility (Torcal and Lago, 2015).
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
#' votes <- esaps_object(dataset=votes, name.country="country", name.year="year")
#' is(votes, "esaps")    ## TRUE
#' @export


esaps_object<-function(path=NULL,
                        dataset=NULL,
                        name.file=NULL,
                        extention=NULL,
                        nCountry=NULL,
                        name.year,
                        name.country,
                        name.M=NULL){

        if (!is.null(path)){
                if (extention!= "xlsx" & extention!= "ods"){
                        stop("It only accepts extension 'xlsx' or 'ods'.")
                }
                old_setwd <- getwd()
                setwd(path)
                if (extention=="ods"){
                        ods <- name.file
                        if (length(ods) > 1){
                                dat <- list()
                                for(i in 1:length(ods)){
                                        dat[[i]] <- readODS::read_ods(ods[i], sheet=1)
                                }
                                setwd(old_setwd)
                        } else {
                                sheetI <- seq(1, nCountry, 1)
                                dat <- list()
                                for(i in 1:nCountry){
                                        dat[[i]] <- readODS::read_ods(ods, sheet=sheetI[i])
                                }
                        }
                }
                if (extention=="xlsx"){
                        excel <- name.file
                        if (length(excel) > 1){
                                dat <- list()
                                for(i in 1:length(excel)){
                                        dat[[i]] <- as.data.frame(readxl::read_excel(excel[i], sheet=1))
                                }
                                setwd(old_setwd)
                        } else {
                                sheetI <- seq(1, nCountry, 1)
                                dat   <- list()
                                for(i in 1:nCountry){
                                        dat[[i]] <- as.data.frame(readxl::read_excel(excel, sheet=sheetI[i]))
                                }
                        }
                }
        }
        if (!is.null(dataset)){
                if (is.data.frame(dataset)){
                        dat <- list(dataset)
                }
                if (is.matrix(dataset)){
                        dat <- as.data.frame(dataset)
                        dat <- list(dat)
                }
                if (is.list(dataset)){
                        dat <- dataset
                }
        }
        if (sum(sapply(dat, nrow) %in% 0L)!= 0){
                stop("Missing data on a sheet.")
        }
        if (!is.null(name.M)){
                m <- numeric()
                data2 <- list()
                for(i in 1: length(dat)){
                        m[i] <- which(colnames(dat[[i]])==name.M)
                        data2[[i]] <- dat[[i]][,m[i]]
                        dat[[i]] <- dat[[i]][,-m[i]]
                        dat[[i]] <- cbind(dat[[i]], M=data2[[i]])
                }
                v1 <- numeric()
                v2 <- numeric()
                for(i in 1:length(dat)){
                        v1[i] <- nrow(dat[[i]])
                        v2[i] <- sum(!is.na(dat[[i]][,ncol(dat[[i]])]))
                }
                if(sum(v1-v2)!=0L){
                        stop("Missing complete data in ", paste0("'",name.M,"'"))
                }
        }

        vector1 <- numeric()
        vector2 <- numeric()
        for(i in 1: length(dat)){
                vector1[i] <- which(colnames(dat[[i]])==name.year)
                colnames(dat[[i]])[vector1[i]] <-"year"
                vector2[i] <- which(colnames(dat[[i]])==name.country)
                colnames(dat[[i]])[vector2[i]] <-"country"
        }
        control_year <- numeric()
        for(i in 1:length(dat)){
                control_year[i]<- nrow(dat[[i]])!= length(unique(dat[[i]]$year))
        }
        if(sum(control_year)!= 0L){
                vcr <- which(control_year!=0)
                country_repeat <- character()
                for(i in 1:length(vcr)){
                        country_repeat[i]<-dat[[vcr[i]]][1,1]
                }
                cat("These countries have repeated years:", paste(country_repeat),"\n\n")
        }

        dat2 <- list()
        dat3 <- list()
        for(i in 1:length(dat)){
                dat2[[i]] <- dat[[i]][,,c(vector2[i],vector1[i])]
                dat2[[i]] <- dat2[[i]][, -c(3:ncol(dat2[[i]]))]
                dat3[[i]] <- dat[[i]][,,-c(vector2[i],vector1[i])]
                dat3[[i]] <- dat3[[i]][, -c(1:2)]
        }

        for(i in 1:length(dat3)){
                dat3[[i]][is.na(dat3[[i]])] <- 0L
        }
        out <- list()
        for(i in 1:length(dat3)){
                out[[i]] <- cbind(dat2[[i]],dat3[[i]])
                out[[i]]$country <- as.character(out[[i]]$country)
        }
        class(out) <- "esaps"
        return(out)

}






