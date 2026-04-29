# Vectorized power estimates

This function allows you to use power.t.test, power.prop.test, etc in
vectorized fashion and return a table of results

## Usage

``` r
vec_power(fun = stats::power.t.test, ...)
```

## Arguments

- fun:

  a power calculating function; \<function\>

- ...:

  the arguments for the power calculating function assigned to \`fun\`;

## Value

tibble of results

## Examples

``` r
# single non-vectorized output
vec_power(fun = power.t.test, n = 100, delta = 1, sd = 1, sig.level = 0.05)
#>     n delta sd sig.level     power alternative                        note
#> 1 100     1  1      0.05 0.9999998   two.sided n is number in *each* group
#>                                method
#> 1 Two-sample t test power calculation

# multiple vectorized output
vec_power(fun = power.t.test, n = 80:100, delta = 1, sd = 1, sig.level = 0.05)
#>      n delta sd sig.level     power alternative                        note
#> 1   80     1  1      0.05 0.9999924   two.sided n is number in *each* group
#> 2   81     1  1      0.05 0.9999937   two.sided n is number in *each* group
#> 3   82     1  1      0.05 0.9999947   two.sided n is number in *each* group
#> 4   83     1  1      0.05 0.9999956   two.sided n is number in *each* group
#> 5   84     1  1      0.05 0.9999963   two.sided n is number in *each* group
#> 6   85     1  1      0.05 0.9999969   two.sided n is number in *each* group
#> 7   86     1  1      0.05 0.9999974   two.sided n is number in *each* group
#> 8   87     1  1      0.05 0.9999979   two.sided n is number in *each* group
#> 9   88     1  1      0.05 0.9999982   two.sided n is number in *each* group
#> 10  89     1  1      0.05 0.9999985   two.sided n is number in *each* group
#> 11  90     1  1      0.05 0.9999988   two.sided n is number in *each* group
#> 12  91     1  1      0.05 0.9999990   two.sided n is number in *each* group
#> 13  92     1  1      0.05 0.9999992   two.sided n is number in *each* group
#> 14  93     1  1      0.05 0.9999993   two.sided n is number in *each* group
#> 15  94     1  1      0.05 0.9999994   two.sided n is number in *each* group
#> 16  95     1  1      0.05 0.9999995   two.sided n is number in *each* group
#> 17  96     1  1      0.05 0.9999996   two.sided n is number in *each* group
#> 18  97     1  1      0.05 0.9999997   two.sided n is number in *each* group
#> 19  98     1  1      0.05 0.9999997   two.sided n is number in *each* group
#> 20  99     1  1      0.05 0.9999998   two.sided n is number in *each* group
#> 21 100     1  1      0.05 0.9999998   two.sided n is number in *each* group
#>                                 method
#> 1  Two-sample t test power calculation
#> 2  Two-sample t test power calculation
#> 3  Two-sample t test power calculation
#> 4  Two-sample t test power calculation
#> 5  Two-sample t test power calculation
#> 6  Two-sample t test power calculation
#> 7  Two-sample t test power calculation
#> 8  Two-sample t test power calculation
#> 9  Two-sample t test power calculation
#> 10 Two-sample t test power calculation
#> 11 Two-sample t test power calculation
#> 12 Two-sample t test power calculation
#> 13 Two-sample t test power calculation
#> 14 Two-sample t test power calculation
#> 15 Two-sample t test power calculation
#> 16 Two-sample t test power calculation
#> 17 Two-sample t test power calculation
#> 18 Two-sample t test power calculation
#> 19 Two-sample t test power calculation
#> 20 Two-sample t test power calculation
#> 21 Two-sample t test power calculation

# every combination of arguments vectorized output
vec_power(fun = power.t.test, n = 90:100, delta = 1, sd = seq(0, 1, length=10), sig.level = 0.05)
#>       n delta        sd sig.level     power alternative
#> 1    90     1 0.0000000      0.05 1.0000000   two.sided
#> 2    91     1 0.0000000      0.05 1.0000000   two.sided
#> 3    92     1 0.0000000      0.05 1.0000000   two.sided
#> 4    93     1 0.0000000      0.05 1.0000000   two.sided
#> 5    94     1 0.0000000      0.05 1.0000000   two.sided
#> 6    95     1 0.0000000      0.05 1.0000000   two.sided
#> 7    96     1 0.0000000      0.05 1.0000000   two.sided
#> 8    97     1 0.0000000      0.05 1.0000000   two.sided
#> 9    98     1 0.0000000      0.05 1.0000000   two.sided
#> 10   99     1 0.0000000      0.05 1.0000000   two.sided
#> 11  100     1 0.0000000      0.05 1.0000000   two.sided
#> 12   90     1 0.1111111      0.05 1.0000000   two.sided
#> 13   91     1 0.1111111      0.05 1.0000000   two.sided
#> 14   92     1 0.1111111      0.05 1.0000000   two.sided
#> 15   93     1 0.1111111      0.05 1.0000000   two.sided
#> 16   94     1 0.1111111      0.05 1.0000000   two.sided
#> 17   95     1 0.1111111      0.05 1.0000000   two.sided
#> 18   96     1 0.1111111      0.05 1.0000000   two.sided
#> 19   97     1 0.1111111      0.05 1.0000000   two.sided
#> 20   98     1 0.1111111      0.05 1.0000000   two.sided
#> 21   99     1 0.1111111      0.05 1.0000000   two.sided
#> 22  100     1 0.1111111      0.05 1.0000000   two.sided
#> 23   90     1 0.2222222      0.05 1.0000000   two.sided
#> 24   91     1 0.2222222      0.05 1.0000000   two.sided
#> 25   92     1 0.2222222      0.05 1.0000000   two.sided
#> 26   93     1 0.2222222      0.05 1.0000000   two.sided
#> 27   94     1 0.2222222      0.05 1.0000000   two.sided
#> 28   95     1 0.2222222      0.05 1.0000000   two.sided
#> 29   96     1 0.2222222      0.05 1.0000000   two.sided
#> 30   97     1 0.2222222      0.05 1.0000000   two.sided
#> 31   98     1 0.2222222      0.05 1.0000000   two.sided
#> 32   99     1 0.2222222      0.05 1.0000000   two.sided
#> 33  100     1 0.2222222      0.05 1.0000000   two.sided
#> 34   90     1 0.3333333      0.05 1.0000000   two.sided
#> 35   91     1 0.3333333      0.05 1.0000000   two.sided
#> 36   92     1 0.3333333      0.05 1.0000000   two.sided
#> 37   93     1 0.3333333      0.05 1.0000000   two.sided
#> 38   94     1 0.3333333      0.05 1.0000000   two.sided
#> 39   95     1 0.3333333      0.05 1.0000000   two.sided
#> 40   96     1 0.3333333      0.05 1.0000000   two.sided
#> 41   97     1 0.3333333      0.05 1.0000000   two.sided
#> 42   98     1 0.3333333      0.05 1.0000000   two.sided
#> 43   99     1 0.3333333      0.05 1.0000000   two.sided
#> 44  100     1 0.3333333      0.05 1.0000000   two.sided
#> 45   90     1 0.4444444      0.05 1.0000000   two.sided
#> 46   91     1 0.4444444      0.05 1.0000000   two.sided
#> 47   92     1 0.4444444      0.05 1.0000000   two.sided
#> 48   93     1 0.4444444      0.05 1.0000000   two.sided
#> 49   94     1 0.4444444      0.05 1.0000000   two.sided
#> 50   95     1 0.4444444      0.05 1.0000000   two.sided
#> 51   96     1 0.4444444      0.05 1.0000000   two.sided
#> 52   97     1 0.4444444      0.05 1.0000000   two.sided
#> 53   98     1 0.4444444      0.05 1.0000000   two.sided
#> 54   99     1 0.4444444      0.05 1.0000000   two.sided
#> 55  100     1 0.4444444      0.05 1.0000000   two.sided
#> 56   90     1 0.5555556      0.05 1.0000000   two.sided
#> 57   91     1 0.5555556      0.05 1.0000000   two.sided
#> 58   92     1 0.5555556      0.05 1.0000000   two.sided
#> 59   93     1 0.5555556      0.05 1.0000000   two.sided
#> 60   94     1 0.5555556      0.05 1.0000000   two.sided
#> 61   95     1 0.5555556      0.05 1.0000000   two.sided
#> 62   96     1 0.5555556      0.05 1.0000000   two.sided
#> 63   97     1 0.5555556      0.05 1.0000000   two.sided
#> 64   98     1 0.5555556      0.05 1.0000000   two.sided
#> 65   99     1 0.5555556      0.05 1.0000000   two.sided
#> 66  100     1 0.5555556      0.05 1.0000000   two.sided
#> 67   90     1 0.6666667      0.05 1.0000000   two.sided
#> 68   91     1 0.6666667      0.05 1.0000000   two.sided
#> 69   92     1 0.6666667      0.05 1.0000000   two.sided
#> 70   93     1 0.6666667      0.05 1.0000000   two.sided
#> 71   94     1 0.6666667      0.05 1.0000000   two.sided
#> 72   95     1 0.6666667      0.05 1.0000000   two.sided
#> 73   96     1 0.6666667      0.05 1.0000000   two.sided
#> 74   97     1 0.6666667      0.05 1.0000000   two.sided
#> 75   98     1 0.6666667      0.05 1.0000000   two.sided
#> 76   99     1 0.6666667      0.05 1.0000000   two.sided
#> 77  100     1 0.6666667      0.05 1.0000000   two.sided
#> 78   90     1 0.7777778      0.05 1.0000000   two.sided
#> 79   91     1 0.7777778      0.05 1.0000000   two.sided
#> 80   92     1 0.7777778      0.05 1.0000000   two.sided
#> 81   93     1 0.7777778      0.05 1.0000000   two.sided
#> 82   94     1 0.7777778      0.05 1.0000000   two.sided
#> 83   95     1 0.7777778      0.05 1.0000000   two.sided
#> 84   96     1 0.7777778      0.05 1.0000000   two.sided
#> 85   97     1 0.7777778      0.05 1.0000000   two.sided
#> 86   98     1 0.7777778      0.05 1.0000000   two.sided
#> 87   99     1 0.7777778      0.05 1.0000000   two.sided
#> 88  100     1 0.7777778      0.05 1.0000000   two.sided
#> 89   90     1 0.8888889      0.05 1.0000000   two.sided
#> 90   91     1 0.8888889      0.05 1.0000000   two.sided
#> 91   92     1 0.8888889      0.05 1.0000000   two.sided
#> 92   93     1 0.8888889      0.05 1.0000000   two.sided
#> 93   94     1 0.8888889      0.05 1.0000000   two.sided
#> 94   95     1 0.8888889      0.05 1.0000000   two.sided
#> 95   96     1 0.8888889      0.05 1.0000000   two.sided
#> 96   97     1 0.8888889      0.05 1.0000000   two.sided
#> 97   98     1 0.8888889      0.05 1.0000000   two.sided
#> 98   99     1 0.8888889      0.05 1.0000000   two.sided
#> 99  100     1 0.8888889      0.05 1.0000000   two.sided
#> 100  90     1 1.0000000      0.05 0.9999988   two.sided
#> 101  91     1 1.0000000      0.05 0.9999990   two.sided
#> 102  92     1 1.0000000      0.05 0.9999992   two.sided
#> 103  93     1 1.0000000      0.05 0.9999993   two.sided
#> 104  94     1 1.0000000      0.05 0.9999994   two.sided
#> 105  95     1 1.0000000      0.05 0.9999995   two.sided
#> 106  96     1 1.0000000      0.05 0.9999996   two.sided
#> 107  97     1 1.0000000      0.05 0.9999997   two.sided
#> 108  98     1 1.0000000      0.05 0.9999997   two.sided
#> 109  99     1 1.0000000      0.05 0.9999998   two.sided
#> 110 100     1 1.0000000      0.05 0.9999998   two.sided
#>                            note                              method
#> 1   n is number in *each* group Two-sample t test power calculation
#> 2   n is number in *each* group Two-sample t test power calculation
#> 3   n is number in *each* group Two-sample t test power calculation
#> 4   n is number in *each* group Two-sample t test power calculation
#> 5   n is number in *each* group Two-sample t test power calculation
#> 6   n is number in *each* group Two-sample t test power calculation
#> 7   n is number in *each* group Two-sample t test power calculation
#> 8   n is number in *each* group Two-sample t test power calculation
#> 9   n is number in *each* group Two-sample t test power calculation
#> 10  n is number in *each* group Two-sample t test power calculation
#> 11  n is number in *each* group Two-sample t test power calculation
#> 12  n is number in *each* group Two-sample t test power calculation
#> 13  n is number in *each* group Two-sample t test power calculation
#> 14  n is number in *each* group Two-sample t test power calculation
#> 15  n is number in *each* group Two-sample t test power calculation
#> 16  n is number in *each* group Two-sample t test power calculation
#> 17  n is number in *each* group Two-sample t test power calculation
#> 18  n is number in *each* group Two-sample t test power calculation
#> 19  n is number in *each* group Two-sample t test power calculation
#> 20  n is number in *each* group Two-sample t test power calculation
#> 21  n is number in *each* group Two-sample t test power calculation
#> 22  n is number in *each* group Two-sample t test power calculation
#> 23  n is number in *each* group Two-sample t test power calculation
#> 24  n is number in *each* group Two-sample t test power calculation
#> 25  n is number in *each* group Two-sample t test power calculation
#> 26  n is number in *each* group Two-sample t test power calculation
#> 27  n is number in *each* group Two-sample t test power calculation
#> 28  n is number in *each* group Two-sample t test power calculation
#> 29  n is number in *each* group Two-sample t test power calculation
#> 30  n is number in *each* group Two-sample t test power calculation
#> 31  n is number in *each* group Two-sample t test power calculation
#> 32  n is number in *each* group Two-sample t test power calculation
#> 33  n is number in *each* group Two-sample t test power calculation
#> 34  n is number in *each* group Two-sample t test power calculation
#> 35  n is number in *each* group Two-sample t test power calculation
#> 36  n is number in *each* group Two-sample t test power calculation
#> 37  n is number in *each* group Two-sample t test power calculation
#> 38  n is number in *each* group Two-sample t test power calculation
#> 39  n is number in *each* group Two-sample t test power calculation
#> 40  n is number in *each* group Two-sample t test power calculation
#> 41  n is number in *each* group Two-sample t test power calculation
#> 42  n is number in *each* group Two-sample t test power calculation
#> 43  n is number in *each* group Two-sample t test power calculation
#> 44  n is number in *each* group Two-sample t test power calculation
#> 45  n is number in *each* group Two-sample t test power calculation
#> 46  n is number in *each* group Two-sample t test power calculation
#> 47  n is number in *each* group Two-sample t test power calculation
#> 48  n is number in *each* group Two-sample t test power calculation
#> 49  n is number in *each* group Two-sample t test power calculation
#> 50  n is number in *each* group Two-sample t test power calculation
#> 51  n is number in *each* group Two-sample t test power calculation
#> 52  n is number in *each* group Two-sample t test power calculation
#> 53  n is number in *each* group Two-sample t test power calculation
#> 54  n is number in *each* group Two-sample t test power calculation
#> 55  n is number in *each* group Two-sample t test power calculation
#> 56  n is number in *each* group Two-sample t test power calculation
#> 57  n is number in *each* group Two-sample t test power calculation
#> 58  n is number in *each* group Two-sample t test power calculation
#> 59  n is number in *each* group Two-sample t test power calculation
#> 60  n is number in *each* group Two-sample t test power calculation
#> 61  n is number in *each* group Two-sample t test power calculation
#> 62  n is number in *each* group Two-sample t test power calculation
#> 63  n is number in *each* group Two-sample t test power calculation
#> 64  n is number in *each* group Two-sample t test power calculation
#> 65  n is number in *each* group Two-sample t test power calculation
#> 66  n is number in *each* group Two-sample t test power calculation
#> 67  n is number in *each* group Two-sample t test power calculation
#> 68  n is number in *each* group Two-sample t test power calculation
#> 69  n is number in *each* group Two-sample t test power calculation
#> 70  n is number in *each* group Two-sample t test power calculation
#> 71  n is number in *each* group Two-sample t test power calculation
#> 72  n is number in *each* group Two-sample t test power calculation
#> 73  n is number in *each* group Two-sample t test power calculation
#> 74  n is number in *each* group Two-sample t test power calculation
#> 75  n is number in *each* group Two-sample t test power calculation
#> 76  n is number in *each* group Two-sample t test power calculation
#> 77  n is number in *each* group Two-sample t test power calculation
#> 78  n is number in *each* group Two-sample t test power calculation
#> 79  n is number in *each* group Two-sample t test power calculation
#> 80  n is number in *each* group Two-sample t test power calculation
#> 81  n is number in *each* group Two-sample t test power calculation
#> 82  n is number in *each* group Two-sample t test power calculation
#> 83  n is number in *each* group Two-sample t test power calculation
#> 84  n is number in *each* group Two-sample t test power calculation
#> 85  n is number in *each* group Two-sample t test power calculation
#> 86  n is number in *each* group Two-sample t test power calculation
#> 87  n is number in *each* group Two-sample t test power calculation
#> 88  n is number in *each* group Two-sample t test power calculation
#> 89  n is number in *each* group Two-sample t test power calculation
#> 90  n is number in *each* group Two-sample t test power calculation
#> 91  n is number in *each* group Two-sample t test power calculation
#> 92  n is number in *each* group Two-sample t test power calculation
#> 93  n is number in *each* group Two-sample t test power calculation
#> 94  n is number in *each* group Two-sample t test power calculation
#> 95  n is number in *each* group Two-sample t test power calculation
#> 96  n is number in *each* group Two-sample t test power calculation
#> 97  n is number in *each* group Two-sample t test power calculation
#> 98  n is number in *each* group Two-sample t test power calculation
#> 99  n is number in *each* group Two-sample t test power calculation
#> 100 n is number in *each* group Two-sample t test power calculation
#> 101 n is number in *each* group Two-sample t test power calculation
#> 102 n is number in *each* group Two-sample t test power calculation
#> 103 n is number in *each* group Two-sample t test power calculation
#> 104 n is number in *each* group Two-sample t test power calculation
#> 105 n is number in *each* group Two-sample t test power calculation
#> 106 n is number in *each* group Two-sample t test power calculation
#> 107 n is number in *each* group Two-sample t test power calculation
#> 108 n is number in *each* group Two-sample t test power calculation
#> 109 n is number in *each* group Two-sample t test power calculation
#> 110 n is number in *each* group Two-sample t test power calculation
```
