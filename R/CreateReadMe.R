#' Create ReadMe.md files
#'
#' This function creates ReadMe.md in the CIDA templates for the standard
#' file structure
#'
#'@param template Which ReadMe.md files should be created? Default is all. Partial
#'matching does work.
#'@param path Where should they be created? Default is the working directory.
#'@return
#'This function creates the desired readme files. It will not overwrite the file
#'however if it does not exists. It does not return anything.
#'@keywords ReadMe ReadMe.md
#'@export
CreateReadMe <- function(template = c('01_Raw_data',
                                      '02_Analysis',
                                      '03_Working_files',
                                      '04_Processed',
                                      '05_Admin'), path = getwd()){
  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # create list with lines for each template
  readme <- list()

  readme$'01_Raw_data' <- c("# 01_Raw_data  ",
                    "  ",
                    "This folder contains all raw data files (e.g., metadata, Ecotone data, Pathtrack data).  ",
                    "  ",
                    "Details about the files:  ",
                    "  ",
                    "File | Description",
                    "---|---------------------------------------------------------------------",
                    "  ",
                    "")
  readme$'02_Analysis' <- c("# 02_Analysis  ",
                         "  ",
                         "This folder contains all R and RMarkdown scripts for processing raw data.  ",
                         "  ",
                         "Details about the files:  ",
                         "  ",
                         "File | Description",
                         "---|---------------------------------------------------------------------",
                         "  ")
  readme$'03_Working_files' <- c("# 03_Working_files  ",
                            "  ",
                            "This folder contains any data products created by analysis scripts, but not intended to be final data products, e.g. tidied datasets.  ",
                            "Scripts that created the files in this folder:  ",
                            "  ",
                            "File | Script | Description",
                            "---|------------------|---------------------------------------------------",
                            "  ")
  readme$'04_Processed' <- c("# 04_Processed ",
                      "  ",
                      "This folder contains all FINALIZED data products intended for dissemination to outside collaborator or repositories, e.g. Movebank.  ",
                      "Scripts that created the files in this folder:  ",
                      "  ",
                      "File | Script | Description",
                      "---|---------------------------------------------------------------------",
                      "    ",
                      "  ")

  readme$'05_Admin' <- c("# 05_Admin ",
                            "  ",
                            "This folder contains abstracts, reports, papers, and any other miscellaneous metadata associated with this project.  ",
                            "  ",
                            "Details about the files:  ",
                            "  ",
                            "File | Description",
                            "---|---------------------------------------------------------------------",
                            "  ",
                            "  ")

  # Function for creating the directory
  createDir <- function(x){
    paste0(path, '/', x)
  }

  createFiles <- function(x){
    file.path(path, paste0(x, '/ReadMe.md'))
  }

  readme <- readme[template]

  pathnames <- sapply(names(readme), createDir)
  dir_created <- lapply(pathnames, dir.create, showWarnings = F, recursive = T)
  con <- lapply(names(readme), createFiles)
  doNotOverwrite <- sapply(con, file.exists)
  readme <- readme[!doNotOverwrite]
  con <- con[!doNotOverwrite]
  files_created <- mapply(writeLines, lapply(readme, paste0, collapse = '\n'), con)
}
