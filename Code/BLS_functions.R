# Author of blsAPI and apiDF functions:
# https://github.com/mikeasilva/blsAPI
# Package 'blsAPI' was removed from the CRAN repository.
# https://cran.r-project.org/package=blsAPI
# BLS Developer R sample code page:
# https://www.bls.gov/developers/api_r.htm


# Pull Data from BLS API --------------------------------------------------
blsAPI <- function(payload=NA, api_version=1, return_data_frame=FALSE){
  if (class(payload) == "logical"){
    # Payload not defined
    message("blsAPI: No payload specified.")
  }
  else{
    # Payload specified so make the request
    api_url <- paste0("https://api.bls.gov/publicAPI/v",
                      api_version,
                      "/timeseries/data/")
    if (is.list(payload)){
      # Multiple Series or One or More Series, Specifying Years request
      payload <- toJSON(payload)
      m <- regexpr('\\"seriesid\\":\\"[a-zA-Z0-9]*\\",', payload)
      str <- regmatches(payload, m)
      if (length(str) > 0){
        # wrap single series in []
        replace <- sub(",", "],", sub(":", ":[", str))
        payload <- sub(str, replace, payload)
      }
      response <- httr::POST(url = api_url, body = payload, httr::content_type_json())
    }
    else{
      # Single Series request
      response <- httr::GET(url = paste0(api_url, payload))
    }
    
    
    # Return the results of the API call
    if (return_data_frame){
      json <- fromJSON(rawToChar(response$content))
      if (json$status != "REQUEST_SUCCEEDED") {
        stop(paste("blsAPI call failed",
                   paste(json$message, collapse = ";"),
                   sep=":"))
      }
      # Iterate over the series
      number_of_series <- length(json$Results$series)
      for (i in 1:number_of_series){
        # Set the default structure of the data frame
        df_start <- data.frame(year = character(),
                               period = character(),
                               periodName = character(),
                               value = character(),
                               stringsAsFactors = FALSE)
        # Get the data
        series_data <- json$Results$series[[i]]$data
        # Can get no data after a successful request
        if (length(series_data) > 0) {
          j <- 0
          for (d in series_data) {
            j <- j + 1
            # Remove the footnotes from the list to stop the warnings
            d$footnotes <- NULL
            d$latest <- NULL
            # Add record to the data frame
            df_start[j, ] <- unlist(d)
          }
          # Add in the series id
          df_start$seriesID <- json$Results$series[[i]]$seriesID
        }
        # Create the data frame that will be returned
        if (!exists("df_to_return")){
          # Data frame to return not defined so create it
          df_to_return <- df_start
        }
        else {
          # Append to the existing data frame
          df_to_return <- rbind(df_to_return, df_start)
        }
      }
      return(df_to_return)
    }
    else {
      # Return the JSON results
      return(rawToChar(response$content))
    }
  }
}


# Process Results ---------------------------------------------------------
apiDF <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  i <- 0
  for(d in data){
    d$footnotes <- NULL
    d$latest <- NULL
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}
