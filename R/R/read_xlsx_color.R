#' Read in xlsx with fill colour - To be deprecated
#'
#' Reads in the fill colour of excel workbooks. Creates a data frame for each
#' sheet in a list if mutliple sheets are requested. Creates a colour column for
#' each colour column specified.
#'
#' @param file the path to the file you intend to read. Can be an xls or xlsx format.
#' @param colorColumns column numbers for which you want to read the colour,
#' for multiple sheets pass a list for each sheet (see details). For no colour
#' columns pass zero.
#' @param sheet NULL(default) for all sheets otherwise a vector of sheet numbers
#' or names to read.
#' @param header should the 1st row be read in as a header? defaults to T.
#' @details For \code{colourColumns} pass a list of numeric vectors for each
#' sheet. For example for 2 sheets \code{colourColumns = list(c(1,2), c(3))} for
#' columns 1 and 2 in the first sheet and 3 in the second. If the list of
#' \code{colourColumns}
#' is shorter than the sheets the remaining sheets will be assumed to have no
#' colour columns. If the list of colour columns is longer only the first n
#' elements will be used where n is the number of sheets with a warning.
#' @return A data frame for one sheet or a list of data frames for multiple sheets
#' @export
#' @keywords Excel colour color xlsx
#'
read_xlsx_color <- function(file, colorColumns, sheet = NULL, header = T){

  deprecation_warn("read_xlsx_color.read_xlsx_color")

  if(!requireNamespace("xlsx", quietly = TRUE))
    stop("package 'xlsx' is required.")
  if(!is.list(colorColumns) & is.numeric(colorColumns))
    colorColumns <- list(colorColumns)
  if(!all(sapply(colorColumns, is.numeric)))
    stop('Please pass coloured column by number')
  if(!is.null(sheet)){
    if(!is.numeric(sheet)&!is.character(sheet))
      stop('Sheets must be numbers or names')
  }
  wb <- xlsx::loadWorkbook(file)
  sheets <- xlsx::getSheets(wb)
  if(!is.null(sheet)) sheets <- sheets[sheet]
  if(length(colorColumns) < length(sheets)){
    add <- length(sheets) - length(colorColumns)
    colorColumns <- c(colorColumns, rep(list(0), add))
  }
  if(length(colorColumns) > length(sheets)){
    colorColumns <- colorColumns[seq_along(sheets)]
    warning(paste('Only the first', length(sheets),
                  'elements of colourColumns have been used.'))
  }
  createData <- function(sheet, colorColumns){
    rows <- xlsx::getRows(sheet)
    if(header) h <- -1 else h <- seq_along(rows)
    head <- xlsx::getCells(rows[-h])
    cells <- xlsx::getCells(rows[h])
    df <- data.frame(matrix(sapply(cells, xlsx::getCellValue),
                            nrow = length(rows) - header,
                            byrow = T))
    if(!is.null(head)) names(df) <- sapply(head, xlsx::getCellValue)

    cellColor <- function(style) {
      fg  <- style$getFillForegroundXSSFColor()
      rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
      rgb <- paste(rgb, collapse = "")
      return(rgb)
    }
    if(colorColumns[1] == 0) return(df)
    getColor <- function(col){
      styles <- sapply(xlsx::getCells(rows[h], col), xlsx::getCellStyle)
      colours <- sapply(styles, cellColor)
    }
    colours <- lapply(colorColumns, getColor)
    names(colours) <- paste('colour', seq_along(colours))
    df <- cbind(df, colours)
    return(df)
  }
  z <- mapply(createData, sheets, colorColumns, SIMPLIFY = F)
  if(length(sheets) == 1) return(z[[1]])
  return(z)
}

