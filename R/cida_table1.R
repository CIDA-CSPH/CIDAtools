#' Create a table one
#'
#'
#' This is a function created to provide characteristics of a study group with
#' an option to stratify by some variable (usually an exposure).
#'
#' @param data A data frame with data
#' @param includeVars A vector of variable names you wish to include in the table.
#' @param stratifyBy The group by which you wish to stratify your table
#' @param group_labels Higher level labels for you stratified groups
#' @param group_label_span The span of columns for each group in `group_labels`
#' @param caption Optional; Adds a caption to the table
#' @param footnoot Optional; Adds a footnote to the table
#' @param include_total Bool; Default \code{TRUE}. Includes a column of the
#' summation of all the stratified groups.
#' @param exclude_mean Bool; Default \code{FALSE}. Excludes the mean
#' for numerical variables.
#' @param exclude_missing Bool; Default \code{FALSE}. Excludes percentages
#' of missing data and only returns counts
#' @param compute_pval Bool; Default \code{FALSE}. Computes p-values for
#' `inludeVars` and add a p-value column in the table.
#' @param nonParametricVars Vector; of the variable names you would like
#' non-parametric testing to be conducted on for p-values returned
#' @param exportWord Bool; whether to export the table in a nice format into word
#' @param useSciNotation Bool; Use scientific notation for large/small continuous variables
#' @return  an html table with N and percentages for categorical variables, mean
#' , SD, Median, and Range for numeric variables. Returns p-values if specified.
#' @importFrom table1 table1
#' @importFrom table1 stats.apply.rounding
#' @importFrom table1 stats.default
#' @import flextable
#' @keywords table1 tableone characteristic
#' @examples
#' # Synthetic data
#' df = data.frame(
#'     `Age` = c(10,12,14,18,20,19,28,33, rep(NA, 4)),
#'     `Sex` = c(rep("Female", 4), rep("Male", 4), rep(NA, 4)),
#'     `Smoking Status` = c(rep('Former', 2), rep('Current', 2), rep('Never', 4), rep(NA, 4)),
#'     `IL-8` = rnorm(12, 35, sd = 7),
#'     `Group` = c(rep('Control', 4), rep('Heart Disease', 4), rep('Lung Disease', 4)),
#'      check.names = FALSE
#'     )
#' # Table 1 with no p-values
#' cida_table1(data = df,
#'             includeVars = c("Age", "Sex", "Smoking Status", "IL-8"),
#'             stratifyBy = "Group",
#'             group_labels = c("Group"),
#'             group_label_span = c(3),
#'             caption = "TABLE 1",
#'             footnote = "My table 1",
#'             include_total = FALSE,
#'             compute_pval = FALSE,
#'             nonParametricVars = NULL,
#'             exportWord = FALSE)
#'
#' # Table 1 with p-values, no mean, no percent missing
#' cida_table1(data = df,
#'             includeVars = c("Age", "Sex", "Smoking Status", "IL-8"),
#'             stratifyBy = "Group",
#'             group_labels = c("", "Group", ""),
#'             group_label_span = c(1, 3, 1),
#'             caption = "TABLE 1",
#'             footnote = "My table 1",
#'             exclude_mean = TRUE,
#'             exclude_missing = TRUE,
#'             include_total = TRUE,
#'             compute_pval = TRUE,
#'             nonParametricVars = NULL,
#'             exportWord = FALSE)
#'
#' # Table 1 styling the output
#' # You can also rename the variables like this (name in data = new name)
#' cida_table1(data = df,
#'             includeVars = c("Age" = "age", "Sex" = "sex", "Smoking Status" = "smoking", "IL-8"= "Interleukin 8"),
#'             stratifyBy = "Group",
#'             group_labels = c("", "Group", ""),
#'             group_label_span = c(1, 3, 1),
#'             caption = "TABLE 1",
#'             footnote = "My table 1",
#'             exclude_mean = TRUE,
#'             exclude_missing = TRUE,
#'             include_total = TRUE,
#'             compute_pval = TRUE,
#'             nonParametricVars = c("Age", "IL-8"), # Uses original name
#'             exportWord = FALSE)
#'
#' # Tables are html so you can customize them using html and css
#` # use a different color and font for smoking status
#` # Use greek letters for IL-8
#' cida_table1(data = df,
#'             includeVars = c("Age" = "age",
#'                             "Sex" = "<span style = 'background-color:pink;'>sex</span>", # Highlight pink
#'                             "Smoking Status" = "<span style='color:purple; font-family:Snell Roundhand; font-size:1.5em; font-weight:900;'>smoking status</span>",
#'                             "IL-8"= "Interleukin 8&beta;"), # &beta; is html for greek lowercase beta
#'             stratifyBy = "Group",
#'             group_labels = c("", "Group", ""),
#'             group_label_span = c(1, 3, 1),
#'             caption = "TABLE 1",
#'             footnote = "My table 1",
#'             exclude_mean = TRUE,
#'             exclude_missing = TRUE,
#'             include_total = TRUE,
#'             compute_pval = TRUE,
#'             nonParametricVars = NULL,
#'             exportWord = FALSE)
#'
#' # You can add icons if you like
#'
#' cida_table1(data = df,
#'             includeVars = c("Age" = "age", "Sex" = "sex",
#'                             "Smoking Status" = "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>
#'                             <span>Smoking Status </span>
#'                             <i class='fa fa-medkit'></i>", # Medkit icon
#'                             "IL-8"= "Interleukin 8&beta;"),
#'             stratifyBy = "Group",
#'             group_labels = c("", "Group", ""),
#'             group_label_span = c(1, 3, 1),
#'             caption = "TABLE 1",
#'             footnote = "My table 1",
#'             exclude_mean = TRUE,
#'             exclude_missing = TRUE,
#'             include_total = TRUE,
#'             compute_pval = TRUE,
#'             nonParametricVars = NULL,
#'             exportWord = FALSE)
#'
#' @export
#'



# Function to generate a table 1
cida_table1 <- function(data,
                        includeVars,
                        stratifyBy,
                        group_labels,
                        group_label_span,
                        caption = NULL,
                        footnote = NULL,
                        include_total = TRUE,
                        exclude_mean = FALSE,
                        exclude_missing = FALSE,
                        compute_pval = FALSE,
                        nonParametricVars = NULL,
                        exportWord = FALSE,
                        useSciNotation = FALSE,
                        ...) {

  # Check variables are in provided data
  if (any(!(includeVars %in% colnames(data))) &
      if (!is.null(names(includeVars))) {
        any(!(names(includeVars) %in% colnames(data)))
      } else {
        TRUE
      }) {
    stop("A selected variabled in includeVars was not found in the data provided")
  }

  sci_notation <- function(x, ...) {
    if (!is.na(x) & (x >= 1000 | x <= 0.001)){
      return(formatC(as.numeric(x), digits = 2, format = "e"))
    } else {
      return(paste(round(as.numeric(x), 3)))
    }

  }

  labels <- list(
    variables = sapply(includeVars, list, simplify = TRUE),
    groups = sapply(group_labels, list, simplify = TRUE)
  )

  if (isTRUE(include_total)) {
    strata <- c(list(Total = data), split(data, data[, stratifyBy]))
  } else {
    strata <- split(data, data[, stratifyBy])
  }

  if (isTRUE(exclude_mean) & isTRUE(useSciNotation)){
    my.render.cont <- function(x) {
      with(
        stats.apply.rounding(stats.default(x), digits = 3, rounding.fn = sci_notation),
        c("",
          "Median [Min, Max]" =
            sprintf("%s [%s, %s]", MEDIAN, MIN, MAX))
      )
    }
  }

  else if (isTRUE(exclude_mean)){
    my.render.cont <- function(x) {
      with(
        stats.apply.rounding(stats.default(x), digits = 3),
        c("",
          "Median [Min, Max]" =
            sprintf("%s [%s, %s]", MEDIAN, MIN, MAX))
      )
    }
  }

  else if (isTRUE(useSciNotation)){
    my.render.cont <- function(x) {
      with(
        stats.apply.rounding(stats.default(x), digits = 3, rounding.fn = sci_notation),
        c("",
          "Mean (SD)" = sprintf("%s (%s)", MEAN, SD),
          "Median [Min, Max]" =
            sprintf("%s [%s, %s]", MEDIAN, MIN, MAX))
      )
    }
  }

  if (isTRUE(exclude_missing)){
    my.render.miss <- function(x) {
      with(
        stats.apply.rounding(stats.default(is.na(x)), digits = 1)$Yes,
        c()
      )
    }
  }

  .with_p <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = c(footnote, p_footnote)
    )
  )

  .with_p_median_missingP <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      render.continuous =
        my.render.cont,
      render.missing = my.render.miss,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = c(footnote, p_footnote)
    )
  )

  .with_p_median <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      render.continuous = my.render.cont,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = c(footnote, p_footnote)
    )
  )

  .with_p_missingP <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      render.missing = my.render.miss,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = c(footnote, p_footnote)
    )
  )

  .with_p_wordExport <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = paste(footnote, word_p_footnote, sep = "")
    )
  )

  .with_p_median_missingP_wordExport <-
    expression(
      table1::table1(
        strata,
        labels,
        groupspan = group_label_span,
        extra.col = list(`<i>p</i>-value` =
                           pvalue),
        render.continuous =
          my.render.cont,
        render.missing = my.render.miss,
        topclass =
          "Rtable1-zebra Rtable1-shade",
        caption = caption,
        footnote = paste(footnote, word_p_footnote, sep = "")
      )
    )

  .with_p_median_wordExport <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      render.continuous =
        my.render.cont,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = paste(footnote, word_p_footnote, sep = "")
    )
  )

  .with_p_missingP_wordExport <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      extra.col = list(`<i>p</i>-value` =
                         pvalue),
      render.missing = my.render.miss,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = paste(footnote, word_p_footnote, sep = "")
    )
  )

  .no_p <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = footnote
    )
  )
  .no_p_median_missingP <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      render.continuous =
        my.render.cont,
      render.missing = my.render.miss,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = footnote
    )
  )

  .no_p_median <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      render.continuous = my.render.cont,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = footnote
    )
  )

  .no_p_missingP <- expression(
    table1::table1(
      strata,
      labels,
      groupspan = group_label_span,
      render.missing = my.render.miss,
      topclass = "Rtable1-zebra Rtable1-shade",
      caption = caption,
      footnote = footnote
    )
  )

  word_p_footnote <- "
  p-values computed as follows:
    Parametric
      Numeric data with 2 groups -- t-test
      Numeric data with more than 2 groups -- ANOVA
    Non-parametric
      Numeric data with 2 groups -- Wilcoxon-test
      Numeric data with more than 2 groups -- Kruskal-Wallis test
    Categorical data with any cell value < 5 -- Fishers exact test
    Categorical data with all cell values >= 5 -- Chi-square test of independence
  "
  p_footnote <- c(
    "
        <p style=font-size:12px;><b><i>p</i>-values computed as follows:</b></p>",
    "&ensp; Parametric",
    "&ensp; &ensp; Numeric data with 2 groups -- t-test",
    "&ensp; &ensp; Numeric data with more than 2 groups -- ANOVA",
    "&ensp; Non-parametric",
    "&ensp; &ensp; Numeric data with 2 groups -- Wilcoxon-test",
    "&ensp; &ensp; Numeric data with more than 2 groups -- Kruskal-Wallis test",
    "&ensp; Categorical data with any cell  value < 5 -- Fishers exact test",
    "&ensp; Categerical data with all cell values >= 5 -- Chi-square test of independence"
  )

  if (isTRUE(exclude_mean)|isTRUE(useSciNotation)) {

    if (isTRUE(exclude_missing)) {

      if (isTRUE(compute_pval)) {

        if (isTRUE(exportWord)) {
          eval(.with_p_median_missingP_wordExport)
        } else {
          eval(.with_p_median_missingP)
        }

      } else {
        eval(.no_p_median_missingP)
      }

    } else {
      if (isTRUE(compute_pval)) {
        if (isTRUE(exportWord)) {
          eval(.with_p_median_wordExport)
        } else {
          eval(.with_p_median)
        }

      } else {
        eval(.no_p_median)
      }
    }

  } else {
    if (isTRUE(exclude_missing)) {

      if (isTRUE(compute_pval)) {

        if (isTRUE(exportWord)) {
          eval(.with_p_missingP_wordExport)
        } else {
          eval(.with_p_missingP)
        }

      } else {
        eval(.no_p_missingP)
      }

    } else {
      if (isTRUE(compute_pval)) {

        if (isTRUE(exportWord)) {
          eval(.with_p_wordExport)
        } else {
          eval(.with_p)
        }

      } else {
        eval(.no_p)
      }
    }
  }
}
