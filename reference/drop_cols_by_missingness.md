# Drop columns with a high percentage of missing values

Removes columns from `data` whose missingness percentage is at or above
`threshold`. To inspect what would be dropped without modifying the
data, call
[`summarize_missingness()`](https://anaboeriu14.github.io/adRutils/reference/summarize_missingness.md)
with the same `threshold` (and `na_strings`).

## Usage

``` r
drop_cols_by_missingness(
  data,
  threshold = 99,
  na_strings = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame or tibble.

- threshold:

  Numeric in `[0, 100]`. Columns at or above this missingness percentage
  are removed. Default `99`.

- na_strings:

  Optional character vector of string values to treat as missing,
  compared after trimming whitespace (so `""` also matches
  whitespace-only entries). Applied to character and factor columns
  only. When `NULL` (default), only true `NA` is counted.

- quiet:

  If `TRUE`, suppress messages. Default `FALSE`.

## Value

`data` with high-missingness columns removed.

## See also

[`summarize_missingness()`](https://anaboeriu14.github.io/adRutils/reference/summarize_missingness.md)
for inspecting missingness without removing columns.

## Examples

``` r
df <- data.frame(
  id               = 1:100,
  almost_empty     = c(rep(NA, 99), 1),
  completely_empty = rep(NA, 100),
  mostly_filled    = c(rep(NA, 20), 1:80)
)
drop_cols_by_missingness(df, threshold = 90)
#> ℹ Removing 2 columns with >= 90% missing
#>      id mostly_filled
#> 1     1            NA
#> 2     2            NA
#> 3     3            NA
#> 4     4            NA
#> 5     5            NA
#> 6     6            NA
#> 7     7            NA
#> 8     8            NA
#> 9     9            NA
#> 10   10            NA
#> 11   11            NA
#> 12   12            NA
#> 13   13            NA
#> 14   14            NA
#> 15   15            NA
#> 16   16            NA
#> 17   17            NA
#> 18   18            NA
#> 19   19            NA
#> 20   20            NA
#> 21   21             1
#> 22   22             2
#> 23   23             3
#> 24   24             4
#> 25   25             5
#> 26   26             6
#> 27   27             7
#> 28   28             8
#> 29   29             9
#> 30   30            10
#> 31   31            11
#> 32   32            12
#> 33   33            13
#> 34   34            14
#> 35   35            15
#> 36   36            16
#> 37   37            17
#> 38   38            18
#> 39   39            19
#> 40   40            20
#> 41   41            21
#> 42   42            22
#> 43   43            23
#> 44   44            24
#> 45   45            25
#> 46   46            26
#> 47   47            27
#> 48   48            28
#> 49   49            29
#> 50   50            30
#> 51   51            31
#> 52   52            32
#> 53   53            33
#> 54   54            34
#> 55   55            35
#> 56   56            36
#> 57   57            37
#> 58   58            38
#> 59   59            39
#> 60   60            40
#> 61   61            41
#> 62   62            42
#> 63   63            43
#> 64   64            44
#> 65   65            45
#> 66   66            46
#> 67   67            47
#> 68   68            48
#> 69   69            49
#> 70   70            50
#> 71   71            51
#> 72   72            52
#> 73   73            53
#> 74   74            54
#> 75   75            55
#> 76   76            56
#> 77   77            57
#> 78   78            58
#> 79   79            59
#> 80   80            60
#> 81   81            61
#> 82   82            62
#> 83   83            63
#> 84   84            64
#> 85   85            65
#> 86   86            66
#> 87   87            67
#> 88   88            68
#> 89   89            69
#> 90   90            70
#> 91   91            71
#> 92   92            72
#> 93   93            73
#> 94   94            74
#> 95   95            75
#> 96   96            76
#> 97   97            77
#> 98   98            78
#> 99   99            79
#> 100 100            80
```
