# Names of data sets in `teal_data` object

**\[deprecated\]**

Use [`names()`](https://rdrr.io/r/base/names.html) instead of
`datanames()`.

`datanames()` is deprecated. If object should be hidden, then use a `.`
(dot) prefix for the object's name.

## Usage

``` r
datanames(x)

datanames(x) <- value

# S3 method for class 'teal_data'
names(x) <- value
```

## Arguments

- x:

  (`teal_data` or `qenv_error`) object to access or modify

- value:

  (`character`) new value for `@datanames`; all elements must be names
  of variables existing in `@.xData`

## Value

The contents of `@datanames` or `teal_data` object with updated
`@datanames`.
