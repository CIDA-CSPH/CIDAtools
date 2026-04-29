# Create a table one

This is a function created to provide characteristics of a study group
with an option to stratify by some variable (usually an exposure).

## Usage

``` r
cida_table1(
  data,
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
  useSciNotation = FALSE
)
```

## Arguments

- data:

  A data frame with data

- includeVars:

  A vector of variable names you wish to include in the table.

- stratifyBy:

  The group by which you wish to stratify your table

- group_labels:

  Higher level labels for you stratified groups

- group_label_span:

  The span of columns for each group in \`group_labels\`

- caption:

  Optional; Adds a caption to the table

- footnote:

  Optional; Adds a footnote to the table

- include_total:

  Bool; Default `TRUE`. Includes a column of the summation of all the
  stratified groups.

- exclude_mean:

  Bool; Default `FALSE`. Excludes the mean for numerical variables.

- exclude_missing:

  Bool; Default `FALSE`. Excludes percentages of missing data and only
  returns counts

- compute_pval:

  Bool; Default `FALSE`. Computes p-values for \`includeVars\` and add a
  p-value column in the table.

- nonParametricVars:

  Vector; of the variable names you would like non-parametric testing to
  be conducted on for p-values returned

- exportWord:

  Bool; whether to export the table in a nice format into word

- useSciNotation:

  Bool; Use scientific notation for large/small continuous variables

## Value

an html table with N and percentages for categorical variables, mean ,
SD, Median, and Range for numeric variables. Returns p-values if
specified.

## Examples

``` r
# Synthetic data
df = data.frame(
    `Age` = c(10,12,14,18,20,19,28,33, rep(NA, 4)),
    `Sex` = c(rep("Female", 4), rep("Male", 4), rep(NA, 4)),
    `Smoking Status` = c(rep('Former', 2), rep('Current', 2), rep('Never', 4), rep(NA, 4)),
    `IL-8` = rnorm(12, 35, sd = 7),
    `Group` = c(rep('Control', 4), rep('Heart Disease', 4), rep('Lung Disease', 4)),
     check.names = FALSE
    )
# Table 1 with no p-values
cida_table1(data = df,
            includeVars = c("Age", "Sex", "Smoking Status", "IL-8"),
            stratifyBy = "Group",
            group_labels = c("Group"),
            group_label_span = c(3),
            caption = "TABLE 1",
            footnote = "My table 1",
            include_total = FALSE,
            compute_pval = FALSE,
            nonParametricVars = NULL,
            exportWord = FALSE)
#> <table class="Rtable1-zebra Rtable1-shade"><caption>TABLE 1</caption>
#> 
#> <thead>
#> <tr>
#> <th class="grouplabel"></th>
#> <th colspan="3" class="grouplabel"><div>Group</div></th>
#> </tr>
#> <tr>
#> <th class='rowlabel firstrow lastrow'></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Control<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Heart Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Lung Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> </tr>
#> <tfoot><tr><td colspan="4" class="Rtable1-footnote"><p>My table 1</p>
#> </td></tr></tfoot>
#> </thead>
#> <tbody>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Age</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Mean (SD)</td>
#> <td>13.5 (3.42)</td>
#> <td>25.0 (6.68)</td>
#> <td>NA (NA)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Median [Min, Max]</td>
#> <td>13.0 [10.0, 18.0]</td>
#> <td>24.0 [19.0, 33.0]</td>
#> <td>NA [NA, NA]</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Missing</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Sex</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Female</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Male</td>
#> <td>0 (0%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Missing</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Smoking Status</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Current</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Former</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Never</td>
#> <td>0 (0%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Missing</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>IL-8</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Mean (SD)</td>
#> <td>28.7 (8.81)</td>
#> <td>34.5 (9.09)</td>
#> <td>34.2 (3.59)</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>30.1 [17.9, 36.8]</td>
#> <td class='lastrow'>36.3 [22.2, 43.0]</td>
#> <td class='lastrow'>33.2 [31.1, 39.4]</td>
#> </tr>
#> </tbody>
#> </table>

# Table 1 with p-values, no mean, no percent missing
cida_table1(data = df,
            includeVars = c("Age", "Sex", "Smoking Status", "IL-8"),
            stratifyBy = "Group",
            group_labels = c("", "Group", ""),
            group_label_span = c(1, 3, 1),
            caption = "TABLE 1",
            footnote = "My table 1",
            exclude_mean = TRUE,
            exclude_missing = TRUE,
            include_total = TRUE,
            compute_pval = TRUE,
            nonParametricVars = NULL,
            exportWord = FALSE)
#> <table class="Rtable1-zebra Rtable1-shade"><caption>TABLE 1</caption>
#> 
#> <thead>
#> <tr>
#> <th class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="3" class="grouplabel"><div>Group</div></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> </tr>
#> <tr>
#> <th class='rowlabel firstrow lastrow'></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Total<br/><span class='stratn'>(N=12)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Control<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Heart Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Lung Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'><i>p</i>-value</span></th>
#> </tr>
#> <tfoot><tr><td colspan="6" class="Rtable1-footnote"><p>My table 1</p>
#> 
#> <p>
#>         <p style=font-size:12px;><b><i>p</i>-values computed as follows:</b></p></p>
#> 
#> <p>&ensp; Parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- t-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- ANOVA</p>
#> 
#> <p>&ensp; Non-parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- Wilcoxon-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- Kruskal-Wallis test</p>
#> 
#> <p>&ensp; Categorical data with any cell  value < 5 -- Fishers exact test</p>
#> 
#> <p>&ensp; Categerical data with all cell values >= 5 -- Chi-square test of independence</p>
#> </td></tr></tfoot>
#> </thead>
#> <tbody>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Age</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>18.5 [10.0, 33.0]</td>
#> <td class='lastrow'>13.0 [10.0, 18.0]</td>
#> <td class='lastrow'>24.0 [19.0, 33.0]</td>
#> <td class='lastrow'>NA [NA, NA]</td>
#> <td class='lastrow'>0.022</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Sex</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Female</td>
#> <td>4 (33.3%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Male</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Smoking Status</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Current</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Former</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Never</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>IL-8</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>33.3 [17.9, 43.0]</td>
#> <td class='lastrow'>30.1 [17.9, 36.8]</td>
#> <td class='lastrow'>36.3 [22.2, 43.0]</td>
#> <td class='lastrow'>33.2 [31.1, 39.4]</td>
#> <td class='lastrow'>0.508</td>
#> </tr>
#> </tbody>
#> </table>

# Table 1 styling the output
# You can also rename the variables like this (name in data = new name)
cida_table1(data = df,
            includeVars = c("Age" = "age",
             "Sex" = "sex",
              "Smoking Status" = "smoking",
               "IL-8"= "Interleukin 8"),
            stratifyBy = "Group",
            group_labels = c("", "Group", ""),
            group_label_span = c(1, 3, 1),
            caption = "TABLE 1",
            footnote = "My table 1",
            exclude_mean = TRUE,
            exclude_missing = TRUE,
            include_total = TRUE,
            compute_pval = TRUE,
            nonParametricVars = c("Age", "IL-8"), # Uses original name
            exportWord = FALSE)
#> <table class="Rtable1-zebra Rtable1-shade"><caption>TABLE 1</caption>
#> 
#> <thead>
#> <tr>
#> <th class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="3" class="grouplabel"><div>Group</div></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> </tr>
#> <tr>
#> <th class='rowlabel firstrow lastrow'></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Total<br/><span class='stratn'>(N=12)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Control<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Heart Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Lung Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'><i>p</i>-value</span></th>
#> </tr>
#> <tfoot><tr><td colspan="6" class="Rtable1-footnote"><p>My table 1</p>
#> 
#> <p>
#>         <p style=font-size:12px;><b><i>p</i>-values computed as follows:</b></p></p>
#> 
#> <p>&ensp; Parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- t-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- ANOVA</p>
#> 
#> <p>&ensp; Non-parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- Wilcoxon-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- Kruskal-Wallis test</p>
#> 
#> <p>&ensp; Categorical data with any cell  value < 5 -- Fishers exact test</p>
#> 
#> <p>&ensp; Categerical data with all cell values >= 5 -- Chi-square test of independence</p>
#> </td></tr></tfoot>
#> </thead>
#> <tbody>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>age</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>18.5 [10.0, 33.0]</td>
#> <td class='lastrow'>13.0 [10.0, 18.0]</td>
#> <td class='lastrow'>24.0 [19.0, 33.0]</td>
#> <td class='lastrow'>NA [NA, NA]</td>
#> <td class='lastrow'>0.021</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>sex</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Female</td>
#> <td>4 (33.3%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Male</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>smoking</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Current</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Former</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Never</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Interleukin 8</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>33.3 [17.9, 43.0]</td>
#> <td class='lastrow'>30.1 [17.9, 36.8]</td>
#> <td class='lastrow'>36.3 [22.2, 43.0]</td>
#> <td class='lastrow'>33.2 [31.1, 39.4]</td>
#> <td class='lastrow'>0.668</td>
#> </tr>
#> </tbody>
#> </table>

# Tables are html so you can customize them using html and css
cida_table1(data = df,
            includeVars = c("Age" = "age",
                            "Sex" =
                            "<span style =
                            'background-color:pink;'>sex</span>", # Highlight pink
                            "Smoking Status" =
                            "<span style='color:purple;
                             font-family:Snell Roundhand;
                             font-size:1.5em;
                             font-weight:900;'>smoking status</span>",
                            "IL-8"=
                            "Interleukin 8&beta;"), # &beta; is html for greek lowercase beta
            stratifyBy = "Group",
            group_labels = c("", "Group", ""),
            group_label_span = c(1, 3, 1),
            caption = "TABLE 1",
            footnote = "My table 1",
            exclude_mean = TRUE,
            exclude_missing = TRUE,
            include_total = TRUE,
            compute_pval = TRUE,
            nonParametricVars = NULL,
            exportWord = FALSE)
#> <table class="Rtable1-zebra Rtable1-shade"><caption>TABLE 1</caption>
#> 
#> <thead>
#> <tr>
#> <th class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="3" class="grouplabel"><div>Group</div></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> </tr>
#> <tr>
#> <th class='rowlabel firstrow lastrow'></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Total<br/><span class='stratn'>(N=12)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Control<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Heart Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Lung Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'><i>p</i>-value</span></th>
#> </tr>
#> <tfoot><tr><td colspan="6" class="Rtable1-footnote"><p>My table 1</p>
#> 
#> <p>
#>         <p style=font-size:12px;><b><i>p</i>-values computed as follows:</b></p></p>
#> 
#> <p>&ensp; Parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- t-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- ANOVA</p>
#> 
#> <p>&ensp; Non-parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- Wilcoxon-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- Kruskal-Wallis test</p>
#> 
#> <p>&ensp; Categorical data with any cell  value < 5 -- Fishers exact test</p>
#> 
#> <p>&ensp; Categerical data with all cell values >= 5 -- Chi-square test of independence</p>
#> </td></tr></tfoot>
#> </thead>
#> <tbody>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>age</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>18.5 [10.0, 33.0]</td>
#> <td class='lastrow'>13.0 [10.0, 18.0]</td>
#> <td class='lastrow'>24.0 [19.0, 33.0]</td>
#> <td class='lastrow'>NA [NA, NA]</td>
#> <td class='lastrow'>0.022</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'><span style =
#>                             'background-color:pink;'>sex</span></span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Female</td>
#> <td>4 (33.3%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Male</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'><span style='color:purple;
#>                              font-family:Snell Roundhand;
#>                              font-size:1.5em;
#>                              font-weight:900;'>smoking status</span></span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Current</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Former</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Never</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Interleukin 8&beta;</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>33.3 [17.9, 43.0]</td>
#> <td class='lastrow'>30.1 [17.9, 36.8]</td>
#> <td class='lastrow'>36.3 [22.2, 43.0]</td>
#> <td class='lastrow'>33.2 [31.1, 39.4]</td>
#> <td class='lastrow'>0.508</td>
#> </tr>
#> </tbody>
#> </table>

# You can add icons if you like

cida_table1(data = df,
            includeVars = c("Age" = "age", "Sex" = "sex",
            "Smoking Status" =
            "<link rel='stylesheet'
href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>
            <span>Smoking Status </span>
            <i class='fa fa-medkit'></i>", # Medkit icon
            "IL-8"= "Interleukin 8&beta;"),
            stratifyBy = "Group",
            group_labels = c("", "Group", ""),
            group_label_span = c(1, 3, 1),
            caption = "TABLE 1",
            footnote = "My table 1",
            exclude_mean = TRUE,
            exclude_missing = TRUE,
            include_total = TRUE,
            compute_pval = TRUE,
            nonParametricVars = NULL,
            exportWord = FALSE)
#> <table class="Rtable1-zebra Rtable1-shade"><caption>TABLE 1</caption>
#> 
#> <thead>
#> <tr>
#> <th class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="3" class="grouplabel"><div>Group</div></th>
#> <th colspan="1" class="grouplabel"></th>
#> <th colspan="1" class="grouplabel"></th>
#> </tr>
#> <tr>
#> <th class='rowlabel firstrow lastrow'></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Total<br/><span class='stratn'>(N=12)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Control<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Heart Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'>Lung Disease<br/><span class='stratn'>(N=4)</span></span></th>
#> <th class='firstrow lastrow'><span class='stratlabel'><i>p</i>-value</span></th>
#> </tr>
#> <tfoot><tr><td colspan="6" class="Rtable1-footnote"><p>My table 1</p>
#> 
#> <p>
#>         <p style=font-size:12px;><b><i>p</i>-values computed as follows:</b></p></p>
#> 
#> <p>&ensp; Parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- t-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- ANOVA</p>
#> 
#> <p>&ensp; Non-parametric</p>
#> 
#> <p>&ensp; &ensp; Numeric data with 2 groups -- Wilcoxon-test</p>
#> 
#> <p>&ensp; &ensp; Numeric data with more than 2 groups -- Kruskal-Wallis test</p>
#> 
#> <p>&ensp; Categorical data with any cell  value < 5 -- Fishers exact test</p>
#> 
#> <p>&ensp; Categerical data with all cell values >= 5 -- Chi-square test of independence</p>
#> </td></tr></tfoot>
#> </thead>
#> <tbody>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>age</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>18.5 [10.0, 33.0]</td>
#> <td class='lastrow'>13.0 [10.0, 18.0]</td>
#> <td class='lastrow'>24.0 [19.0, 33.0]</td>
#> <td class='lastrow'>NA [NA, NA]</td>
#> <td class='lastrow'>0.022</td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>sex</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Female</td>
#> <td>4 (33.3%)</td>
#> <td>4 (100%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Male</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'><link rel='stylesheet'
#> href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>
#>             <span>Smoking Status </span>
#>             <i class='fa fa-medkit'></i></span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Current</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td>0.029</td>
#> </tr>
#> <tr>
#> <td class='rowlabel'>Former</td>
#> <td>2 (16.7%)</td>
#> <td>2 (50.0%)</td>
#> <td>0 (0%)</td>
#> <td>0 (0%)</td>
#> <td></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Never</td>
#> <td class='lastrow'>4 (33.3%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'>4 (100%)</td>
#> <td class='lastrow'>0 (0%)</td>
#> <td class='lastrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel firstrow'><span class='varlabel'>Interleukin 8&beta;</span></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> <td class='firstrow'></td>
#> </tr>
#> <tr>
#> <td class='rowlabel lastrow'>Median [Min, Max]</td>
#> <td class='lastrow'>33.3 [17.9, 43.0]</td>
#> <td class='lastrow'>30.1 [17.9, 36.8]</td>
#> <td class='lastrow'>36.3 [22.2, 43.0]</td>
#> <td class='lastrow'>33.2 [31.1, 39.4]</td>
#> <td class='lastrow'>0.508</td>
#> </tr>
#> </tbody>
#> </table>
```
