log_search <- function(user_id, query, token = 'TokenExample1234567890') {
  base_url <- "http://localhost:8080"
  full_url <- paste0(base_url, "/log_search")
  
  resp <- request(full_url) |>
    req_auth_bearer_token(token) |>
    req_method("POST") |>
    req_url_query(user_id = user_id, query = query) |>
    req_perform()
  
  result <- resp |> resp_body_json()
  return(result)
}