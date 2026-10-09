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
  return(NULL)
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

#' Internal function which runs before GitHub repository creation
pre_create_github_repository <- function(name, visibility){
  # Check if repo name is valid
  if((!is.character(name)) || grepl("[^a-zA-Z0-9_\\.-]",name)){
    print_failure(glue::glue("{name} is not a valid repository name.", .null = ""))
    return(list(status=F, password=NULL, repo_url=NULL))
  }
  
  # Check if visibility is correct
  if(!(visibility %in% c("internal", "private", "public"))){
    print_failure(glue::glue("{visiblity} is not a valid visibility setting.", .null = ""))
    return(list(status=F, password=NULL, repo_url=NULL))
  }else if(visibility == "public"){
    print_failure("Creating a public repository is not currently supported.")
    return(list(status=F, password=NULL, repo_url=NULL))
  }
  
  # Check if we have a GitHub token set.
  gh_creds <- get_github_credentials()
  if(is.null(gh_creds)){
    print_failure("Unable to check existing of GitHub repository.")
    return(list(status=F, password=NULL, repo_url=NULL))
  }
  
  # Check if repo exists already
  exist_req <- httr2::request(fs::path_join(c(GITHUB_REPO_API_URL, name))) |>
    httr2::req_headers(
      "User-Agent" = "CIDA-CSPH/CIDAtools",
      "Accept" = "application/vnd.github+json",
      "Authorization" = glue::glue("Bearer {gh_creds$password}"),
      "X-GitHub-Api-Version"= "2026-03-10",
    ) |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = function(resp) {return(FALSE)})
  exist_resp <- httr2::req_perform(exist_req)
  
  # Check the status code on the API response to determine availability.
  exist_status <- httr2::resp_status(exist_resp)
  repo_url <- fs::path_join(c(CIDA_GITHUB_ORGANIZATION, name))
  if(exist_status == 404){
    tryCatch({
      exist_json <- httr2::resp_body_json(exist_resp)
    }, error=function(e){
      print_failure(glue::glue("Unable to create GitHub repository (Status: {exist_status}): {e}", .null = ""))
      return(list(status=F, password=NULL, repo_url=NULL))
    })
    if(exist_json$documentation_url == "https://docs.github.com/rest/repos/repos#get-a-repository"){
      print_success(glue::glue("Repository URL {repo_url} is available.", .null=""))
      return(list(status=T, password=gh_creds$password, repo_url=repo_url))
    }
  }else if(exist_status %in% c(200, 301)){
    print_failure(glue::glue("Unable to create repository, {repo_url} already exists.", .null=""))
    return(list(status=F, password=NULL, repo_url=NULL))
  }else if(exist_status == 403){
    print_failure(glue::glue("Unable to check for repository existence: (Status: {exist_status})", .null=""))
    return(list(status=F, password=NULL, repo_url=NULL))
  }else{
    print_failure(glue::glue("Unable to create GitHub repository (Status: {exist_status})", .null=""))
    return(list(status=F, password=NULL, repo_url=NULL))
  }
}

#' Create an empty GitHub repository in the CIDA organization.
create_empty_github_repository <- function(name, description=NULL, visibility="internal"){
    slist <- pre_create_github_repository(name=name, visibility=visibility)
    
    if(!slist$status){
      return(NULL)
    }
    
    json_l <- list(name=name, visibility=visibility)
    if(is.character(description)){
      json_l["description"] <- description
    }
    
    # Perform the repository creation request
    req <- httr2::request(GITHUB_REPO_CREATE_API_URL) |>
      httr2::req_headers(
        "User-Agent" = "CIDA-CSPH/CIDAtools",
        "Accept" = "application/vnd.github+json",
        "Authorization" = glue::glue("Bearer {slist$password}"),
        "X-GitHub-Api-Version"= "2026-03-10",
      ) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(data=json_l) |>
      httr2::req_error(is_error = function(resp) {return(FALSE)})
    resp <- httr2::req_perform(req)
    
    status_code <- httr2::resp_status(resp)
    
    if(status_code == 201){
      print_success(glue::glue("Successfully created a new GitHub repository at: {slist$repo_url}\n Clone this repo by running 'git clone {slist$repo_url}'."))
      return(slist$repo_url)
    }else if(status_code == 403){
      print_failure(glue::glue("Forbidden from creating a new GitHub repository: (Status: {status_code})"))
      return(NULL)
    }else{
      print_failure(glue::glue("Unable to create a new GitHub repository: (Status: {status_code})"))
      return(NULL)
    }
    
}

#' Create a GitHub repository from template in the CIDA organization
create_github_repository_from_template <- function(name, template_name, description, visibility="internal"){
  slist <- pre_create_github_repository(name=name, visibility=visibility)
  if(!slist$status){
    return(NULL)
  }
  
  json_l <- list(name=name, owner="CIDA-CSPH", private=TRUE)
  if(is.character(description)){
    json_l["description"] <- description
  }
  
  # Make the request to create the new repository
  req <- httr2::request(glue::glue(GITHUB_REPO_CREATE_FROM_TEMPLATE_API_URL)) |>
    httr2::req_headers(
      "User-Agent" = "CIDA-CSPH/CIDAtools",
      "Accept" = "application/vnd.github+json",
      "Authorization" = glue::glue("Bearer {slist$password}"),
      "X-GitHub-Api-Version"= "2026-03-10",
    ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data=json_l) |>
    httr2::req_error(is_error = function(resp) {return(FALSE)})
  resp <- httr2::req_perform(req)
  
  # Check the status code on the API response.
  status_code <- httr2::resp_status(resp)
  if(status_code == 201){
    ret_val <- slist$repo_url
  }else{
    ret_val <- NULL
  }
  
  if(!is.null(ret_val) && visibility == "internal"){
    # BAD HACK: Sometimes this request can fail if we call PATCH too quickly after the repo creation.
    #           To fix, we just wait a little bit before calling PATCH.
    # TODO: Do something better here
    Sys.sleep(2)
    # Due to GitHub API limitation, we cannot create an 'internal' repo in a single step,
    # so we must create the repo as private, then send a follow-up request to modify the
    # repository visibility.
    update_req <- httr2::request(fs::path_join(c(GITHUB_REPO_API_URL, name))) |>
      httr2::req_headers(
        "User-Agent" = "CIDA-CSPH/CIDAtools",
        "Accept" = "application/vnd.github+json",
        "Authorization" = glue::glue("Bearer {slist$password}"),
        "X-GitHub-Api-Version"= "2026-03-10",
      ) |>
      httr2::req_method("PATCH") |>
      httr2::req_body_json(json_l) |>
      httr2::req_error(is_error = function(resp) {return(FALSE)})
    update_resp <- httr2::req_perform(update_req)
    
    # Get status code from response
    update_status_code <- httr2::resp_status(update_resp)
    
    if(status_code == 200){
      print_success("Successfully update repository visibility (internal)")
    }else if(status_code == 422){
      # Get JSON body from response
      update_json <- httr2::resp_body_json(update_resp)
      # Print a warning message
      print_warning(glue::glue("Unable to set repo visibility to 'internal', visibility will remain 'private' {update_json$message}."))
    }else{
      print_warning(glue::glue("Unable to set repo visibility to 'internal', visibility will remain 'private'. (Status: {update_status_code})"))
    }
  }
  
  # Print the deferred success or failure message for the initial repository create, so that this
  # message appears last for users.
  if(is.null(ret_val)){
    print_failure(glue::glue("Unable to create a new GitHub repository: (Status: {status_code})"))
  }else{
    print_success(glue::glue("Successfully created a new GitHub repository at: {slist$repo_url}\n Clone this repo by running 'git clone {slist$repo_url}'"))
  }
  
  # Return the result
  return(ret_val)
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
  # Retrieve the available templates.
  template_list <- list_github_templates(include_empty = T, display = is.null(template_name))
  
  # If we cannot list templates, we cannot continue.
  if(is.null(template_list)){
    print_failure("Unable to list templates.")
  }
  
  # If a template name is not chosen, prompt interactively
  if(is.null(template_name)){
    # The user's selection defaults to 1 (an empty repository)
    user_i <- NULL
    while(!isTRUE(user_i %in% seq_along(template_list))){
      user_choice <- readline("Choose a template from the above list (default 1): ")
      if(user_choice == ""){
        user_i <- 1
      }else{
        suppressWarnings({
          user_i <- as.integer(user_choice)
        })
      }
    }
    # Use the user's selection to choose the template.
    use_template_name <- template_list[[user_i]]$name
  }else{
    # List all template names
    valid_template_names <- unlist(lapply(template_list, function(template){template$name}))
    # Check the the chosen name is one of the options
    if(!(template_name %in% valid_template_names)){
      valid_template_str <- paste0(valid_template_names, collapse=", ")
      print_failure(glue::glue("Template '{template_name}' is not a valid template in {valid_template_str}"))
      return(NULL)
    }
    # Assign the template name
    use_template_name <- template_name
  }
  
  # If an empty project is requested, create it.
  if(use_template_name == "empty"){
    repo_url <- create_empty_github_repository(
      name = name,
      description = description,
      visibility = visibility
    )
  }
  # Otherwise create from the selected template.
  else{
    repo_url <- create_github_repository_from_template(
      name = name,
      description = description,
      visibility = visibility,
      template_name = use_template_name
    )
  }
  
  return(repo_url)
}

#' Clone a GitHub via Git
clone_github_repository <- function(repository_url, local_path){
  
  if(fs::file_exists(local_path) && !fs::is_dir(local_path)){
    print_failure("Path exists but is not a directory!")
    return(FALSE)
  }
  
  # TODO: Check git integration status, need to know if we are using GCM or token-based auth.
  #  If token auth, we need to add the token into the URL.
  # Run the clone operation.
  tryCatch({
    ret_code <- system2(
        "git", 
        c("clone", repository_url, fs::path_abs(local_path)),
        stdout = TRUE,
        stderr = TRUE
      )
    }, error=function(e){
      print_failure(glue::glue("Unable to clone repository: {e}", .null = ""))
      return(FALSE)
    }
  )
  
  # Success is determined by status code.
  return(ret_code == 0)
}

get_git_remote_url <- function(project_root = NULL, name = "origin") {
  project_root <- fs::path_abs(ifelse(is.null(project_root), fs::path_wd(), project_root))
  
  # Use subprocess to run git.
  tryCatch({
    remote_out <- system2(
      "git",
      c("-C", project_root, "remote", "get-url", "name"),
      stdout = TRUE
    )
  }, error=function(e){
    return(NULL)
  })
  
  # Quirky way to get the status code.
  # A 'normal' status code returns NULL, not 0.
  status_code <- attr(remote_out, "status")
  
  # If we got an error (something other than NULL), return NULL
  if(!is.null(status_code)){
    return(NULL)
  }
  
  # Get the url provided by Git.
  remote_url <- remote_out
  
}
