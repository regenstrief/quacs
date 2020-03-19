#' @title Get URL
#' Get text from a url.
#' @param url_base the base url, can be https
#' @param url_path the url path
#' @return the text of the webpage
#' @export
get_url <- function(url_base, url_path) {
    CAFILE <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
    x <- httr::GET(url_base, path = url_path, httr::config(cainfo = CAFILE))
    y <- httr::content(x, as="text")
    return(y)
}

#' Get ACS Data Dictionary
#'
#' Get the ACS Data Dictionary for a given year
#'
#' @param year End year of the 5-Year ACS Period
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @return The data dictionary as a data frame
#' @export
get_acs_dd <- function(year="2018") {
    COL_NAMES <- c("label", "concept", "predicateType",
                   "group", "limit", "attributes", "name")
    ACS_BASE_URL <- 'https://api.census.gov/'
    ACS_URL_PATH <- sprintf('/data/%s/acs/acs5/variables.json', year)
    y <- get_url(ACS_BASE_URL, ACS_URL_PATH)
    Z <- jsonlite::fromJSON(y)$variables
    L <- purrr::compact(lapply(1:length(Z), function(k) {
        w <- as.data.frame(t(Z[[k]]))
        w$name <- names(Z)[k]
        if (setequal(names(w), COL_NAMES)) {
            return(w)
        }
        else {
            return(NULL)
        }
    }))
    L2 <- do.call("rbind", L)
    L2 %>%
        dplyr::mutate(table_subject = substr(.data$group, 2, 3)) %>%
        dplyr::mutate_all(function(X) as.character(unlist(X))) ->
        final_df
    return(final_df)
}

#' Named
#'
#' Internal function for printing data dictionaries
#'
#' @param x A data frame
#'
#' @return a data frame with column names and missingness
.named <- function(x) {
    y <- as.data.frame(x)

    z <- data.frame(NAMES=names(y),
                    CLASS=as.character(unlist(lapply(1:ncol(y), function(i) class(y[,i])[1]))),
                    NA_PCT=sapply(1:ncol(x), function(i) sum(is.na(x[,i]))/nrow(x)))
    return(z)
}

#' Get ACS5 Year Estimate Data
#'
#' The API imposes limits on the number of varaibles extracted. To extract more than 20,
#' `census_api_acs5_multi` batches up the requests and combines them
#'
#' @param var_names Character vector of varaibles to get, length >= 20
#' @param get Character vector of varaibles to get, length < 20
#' @param geo Geographic argument to ACS API
#' @param where Where argument to ACS API
#' @param key Key argument to ACS API
#' @param other Other argument to ACS API
#' @param year Ending year for the dataset
#'
#' @importFrom rlang .data
#'
#' @return Resulting table as a data frame
#' @export
census_api_acs5 <- function(get,
                            geo="for=tract:*",
                            where="in=state:18",
                            key="-",
                            other="-",
                            year="2017") {
    # construct API call
    url_base <- "https://api.census.gov/"
    api_base <- sprintf("/data/%s/acs/acs5?", year)

    args <- c(get, geo, where, other, key)
    args_2 <- args[which(args != "-")]

    call <- paste0(api_base, paste0(as.list(args_2), collapse="&"))

    print(paste("API Call:", call, sep=" "))

    # https cert
    CAFILE <- system.file("CurlSSL", "cacert.pem", package = "RCurl")

    # make API call and acquire JSON
    x <- httr::GET(url_base, path = call, httr::config(cainfo = CAFILE))
    y <- httr::content(x, as="text")

    # format as data frame and return
    Z <- jsonlite::fromJSON(y)
    data <- as.data.frame(Z[-1,], stringsAsFactors=FALSE)
    names(data) <- Z[1,]

    return(data)
}

#' @rdname census_api_acs5
#' @export
census_api_acs5_multi <- function(var_names, geo="-", where="-", key="-", other="-", year="2017") {
    N <- length(var_names)
    if (N <= 20) {
        get <- paste0("get=", paste0(var_names, collapse=","))
        res <- census_api_acs5(get, geo=geo, key=key, where=where, other=other, year=year)
        return(res)
    }
    else {
        W <- 1:N # split into groups and lapply rbind stuff
        df <- data.frame(vn=var_names, k=floor(W/20))
        df %>% dplyr::group_by(.data$k) %>% tidyr::nest -> X
        X$data %>%
            purrr::map(function(x) {
                get <- paste0("get=", paste0(x$vn, collapse=","))
                res <- census_api_acs5(get, geo=geo, key=key, where=where, other=other)
                return(res)
            }) -> Y
        join_names <- purrr::reduce(lapply(Y, names), intersect)
        Z <- purrr::reduce(Y, dplyr::left_join, by=join_names)
        return(Z)
    }
}
