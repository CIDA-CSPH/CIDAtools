#' Internal function for p-value calculation in cida_table1
#'
#' @param x the row_variable
#' @param name the variables name
#' @param ... arguments passed to S3 methods
#'

# Function to automatically compute p-vals
pvalue <- function(x, name, ...) {
  # Get indicator parameter from dots
  # print(x)
  # print(name)
  # args = list(...)
  # include_total = args$include_total
  # nonParametricVars = args$nonParametricVars
  include_total = mget("include_total", envir = parent.frame(7))[[1]]
  nonParametricVars = mget("nonParametricVars", envir = parent.frame(7))[[1]]
  # Construct vectors of data y, and groups (strata) g
  if (isTRUE(include_total)) {
    y <- unlist(x[-1]) # -1 to remove `total` group from computations
    g <-
      factor(rep(1:length(x[-1]), times = sapply(x[-1], length)))
  } else {
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
  }

  if (is.null(nonParametricVars)) {
    nonParametricVars = c("")
  }

  if (is.numeric(y)) {
    # Non parametric testing
    if (name %in% nonParametricVars) {
      # More than 2 group kruskal wallis
      if (length(levels(g)) > 2) {
        p <- kruskal.test(y ~ g)$p.value
      } else {
        # Otherwise mann-whitney u test
        p <- wilcox.test(y ~ g)$p.value
      }
    }# For numeric variables, perform a standard 2-sample t-test when groups == 2 ANOVA > 2
    else if (length(levels(g)) > 2) {
      p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
    } else {
      tryCatch(
        expr = {
          p <- t.test(y ~ g)$p.value
        },
        error = function(e) {
          p <<- NA
        }
      )
    }

  } else {
    # For categorical variables, fisher test if any cell < 5 else perform a chi-squared test of independence
    if (any(as.vector(table(y, g)) < 5)) {
      tryCatch(
        expr = {
          p <- fisher.test(table(y, g))[[1]]
        },
        error = function(e) {
          p <<- fisher.test(table(y, g), simulate.p.value = TRUE)[[1]]
        }
      )
    } else {
      p <- chisq.test(table(y, g), correct = TRUE)$p.value
    }
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("",
    sub(
      "<",
      "&lt;",
      format.pval(round(p, digits = 3), eps = 0.0001, scientific = FALSE)
    ))
}
