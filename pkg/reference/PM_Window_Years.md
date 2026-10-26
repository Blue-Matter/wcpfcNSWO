# Years for a Performance Metric Window

The calendar years in which the CMPs are active (from `MPStartYear`), or
a window of them: `'short'` (the first 10), `'medium'` (the years
between the first and last 10), `'long'` (the last 10), or `'terminal'`
(the last year).

## Usage

``` r
PM_Window_Years(
  object,
  window = c("all", "short", "medium", "long", "terminal")
)
```

## Arguments

- object:

  An `mse` object.

- window:

  Character. One of `'all'`, `'short'`, `'medium'`, `'long'`, or
  `'terminal'`.

## Value

Numeric vector of calendar years.
