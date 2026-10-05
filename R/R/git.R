
GithubCredentials <- S7::new_class(
  "GithubCredentials",
  properties=list(
    host=S7::new_property(S7::class_character, default = "github.com"),
    protocol=S7::new_property(S7::class_character, default="https"),
    username=S7::new_property(NULL | S7::class_character),
    password=S7::new_property(NULL | S7::class_character)
  )
)
#'
#get_default_credentials() <- G

#' Function to list the available CIDAtools GitHub templates.
list_github_templates <- function(display=T, include_empty=T){
  
}

#' Creates a new GitHub repository. This function is intended for interactive
#' use, and wraps the more specific create_empty_github_repository() and 
#' create_github_repository_from_template() functions. If you do not require an
#' interactive component, use one of the more specific functions instead. 
#' 
#' 
create_github_repository <- function(
    name,
    description = NULL,
    visibility = "internal",
    template_name = NULL
  ){
  
}
