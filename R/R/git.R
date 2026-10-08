#' Read default GitHub credentials from the CIDA defaults file.
#' @export
get_default_credentials <- function() {
  
  tryCatch(
    {
      # Get the defaults
      defaults <- CIDADefaults()
      # We use a list here instead of a separate class.
      tmp_creds <- list(
        host=defaults@github_host,
        protocol=defaults@github_protocol,
        username=defaults@github_username,
        password=defaults@github_password
      )
    },
    error=function(e){
      print_failure(glue::glue("Unable to read GitHub credentials from CIDA defaults: {e}"))
      return(NULL)
    }
  )
  
  if(is.null(tmp_creds$username) || is.null(tmp_creds$password)){
    print_failure("CIDA default GitHub credentials are not set.")
    return(NULL)
  }
  
  print_info(glue::glue("Retrieved CIDA default GitHub credentials for user '{tmp_creds$username}'"))
  return(tmp_creds)
}

#' Retrieve GCM credentials, if configured.
#' @export
get_gcm_credentials <- function(){
  # Retrieve the token from GCM
  raw_gcm_result <- system2(
    command="git", 
    args=c("credential-manager", "get"), 
    input="protocol=https\nhost=github.com\n\n",
    stdout=TRUE
  )
  
  if(raw_gcm_result[[1]] != "protocol=https"){
    print_failure("Unable to retrieve GCM credentials")
    return(NULL)
  }
  
  # Parse the GCM response
  split_result <- strsplit(raw_gcm_result, "=")[1:4]
  # TODO: can we make this cleaner?
  tmp_creds <- setNames(lapply(split_result, function(x){if(is.na(x[2])){NULL}else{x[2]}}), lapply(split_result, function(x){x[1]})) 
  
  if(is.null(tmp_creds$username) || is.null(tmp_creds$password)){
    print_failure("Retrieved GCM credentials, but missing username or password!")
    return(NULL)
  }
  
  print_info(glue::glue("Retrieved GCM credentials for user '{tmp_creds$username}'"))
  return(tmp_creds)
}

#' Retrive a GitHub token to be used for CIDAtools functionality.
#' 
#' This function will first attempt to retrieve a GitHub token from CIDADefaults, but will fall back to Git Credential Manager.
get_github_credentials <- function(){
  # First, check for something in CIDA defaults
  creds <- get_default_credentials()
  
  # If we get something, return it
  if(!is.null(creds) && !is.null(creds$username) && !is.null(creds$password)){
    return(creds)
  }
  
  # Next, try GCM
  creds <- get_gcm_credentials()
  # If we get something, return it
  if(!is.null(creds) && !is.null(creds$username) && !is.null(creds$password)){
    return(creds)
  }
  # Otherwise, return NULL
  print_failure("Unable to retrieve GitHub token from CIDA defaults or GCM")
  return(NULLß)
}

#' List the available CIDAtools GitHub templates
#' @export
list_github_templates <- function(display=T, include_empty=T){
  # Check if we have GitHub credentials
  gh_creds <- get_github_credentials()
  if(is.null(gh_creds)){
    print_failure("Unable to list template repositories.")
    return(NULL)
  }
  
  # Perform the search request
  req <- httr2::request(GITHUB_SEARCH_API_URL + "?q=org:CIDA-CSPH+topic:cidatools-template") |>
    httr2::req_headers(
      "User-Agent" = "CIDA-CSPH/CIDAtools",
      "Accept" = "application/vnd.github+json",
      "Authorization" = glue::glue("Bearer {gh_creds$password}")
    ) |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = function(resp) {return(FALSE)})
  resp <- httr2::req_perform(req)
  
  # Check status code
  status <- httr2::resp_status(resp)
  if(status != 200){
    err_str <- glue::glue("Unable to list template repositories (Status {status}")
    tryCatch({
      err_json <- httr2::resp_body_json(resp)
    }, error=function(e){
      print_failure(glue::glue("{err_str})", .envir = environment()))
      return(NULL)
    })
    if(!is.null(err_json$message) && !is.null(err_json$errors)){
      #TODO: test this (mock?)
      err_tmp <- paste0(
        lapply(err_json$errors, function(err){
          if(!is.null(err$message)){
            return(err$message)
          }
          return("")
        }),
        collapse = "\n")
      err_str <- glue::glue("{err_str} {err_json$message}):\n\t{err_tmp}")
    }else if (!is.null(err_json$message)){
      err_str <- glue::glue("{err_str} {err_json$message})")
    }
    else{
      err_str <- paste0(err_str,")")
    }
    print_failure(err_str)
    return(NULL)
  }
  
  # Construct the model for the response.
  body <- httr2::resp_body_json(resp)
  template_list <- list(
    total_count = resp$total_count,
    incomplete_results = resp$incomplete_results,
    items = lapply(
      body$items,
      function(item){
        list(
          id=item$id,
          name=item$name,
          full_name=item$full_name,
          description=item$description,
          url=item$url
        )
      }
    )
  )
  
  # Choose the correct item list to display
  if(include_empty){
    empty_template <- list(
      id=0, 
      name="empty",
      full_name="empty",
      description="An empty repository (no template).",
      url=NULL
    )
    items_list <- c(list(empty_template), template_list$items)
  }else{
    items_list <- template_list$items
  }
  
  # Only print if requested
  if(display){
    # Length of longest list ID
    i_len <- nchar(as.character(length(items_list)))
    # Length of longest repo name
    repo_len <- max(unlist(lapply(items_list, function(item){nchar(item$name)})))
    # Display the choices
    for(i in seq_along(items_list)){
      item <- items_list[[i]]
      message(sprintf(glue::glue("[%0{i_len}d] %-{repo_len}s - {item$description}\n", .null=""), i, item$name))
    }
  }
  # Return the list of templates
  return(items_list)
}

#' Creates a new GitHub repository. This function is intended for interactive
#' use, and wraps the more specific create_empty_github_repository() and 
#' create_github_repository_from_template() functions. If you do not require an
#' interactive component, use one of the more specific functions instead. 
#' 
#' @export
create_github_repository <- function(
    name,
    description = NULL,
    visibility = "internal",
    template_name = NULL
  ){
  
}
