library(httr2)

get_data <- function(endpoint, limit, token = 'TokenExample1234567890', ...) {
  base_url <- "http://localhost:8080"
  full_url <- paste0(base_url, "/", endpoint)
  
  # Make the request only once
  resp <- request(full_url) |>
    req_auth_bearer_token(token) |> 
    req_url_query(limit = limit, ...) |>    
    req_perform()
  
  # Convert the result
  result <- resp |> resp_body_json()
  return(result)
}