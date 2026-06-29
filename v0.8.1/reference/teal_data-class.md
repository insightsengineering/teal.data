# Reproducible data

Reproducible data container class. Inherits code tracking behavior from
[`teal.code::qenv`](https://insightsengineering.github.io/teal.code/latest-tag/reference/qenv-class.html).

## Details

This class provides an isolated environment in which to store and
process data with all code being recorded. The environment, code, data
set names, and data joining keys are stored in their respective slots.
These slots should never be accessed directly, use the provided get/set
functions.

As code is evaluated in `teal_data`, messages and warnings are stored in
their respective slots. If errors are raised, a `qenv.error` object is
returned.

## Slots

- `.xData`:

  (`environment`) environment containing data sets and possibly
  auxiliary variables. Access variables with
  [`get()`](https://rdrr.io/r/base/get.html),
  [`$`](https://rdrr.io/r/base/Extract.html),
  [`teal.code::get_var()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_var.html)
  or \[`[[`\]. No setter provided. Evaluate code to add variables into
  `@.xData`.

- `code`:

  (`list` of `character`) representing code necessary to reproduce the
  contents of `qenv`. Access with
  [`teal.code::get_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_code.html).
  No setter provided. Evaluate code to append code to the slot.

- `join_keys`:

  (`join_keys`) object specifying joining keys for data sets in
  `@.xData`. Access or modify with
  [`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md).

- `verified`:

  (`logical(1)`) flag signifying that code in `@code` has been proven to
  yield contents of `@.xData`. Used internally. See
  [`verify()`](https://insightsengineering.github.io/teal.data/reference/verify.md)
  for more details.

## Code

Each code element is a character representing one call. Each element is
named with the random identifier to make sure uniqueness when joining.
Each element has possible attributes:

- `warnings` (`character`) the warnings output when evaluating the code
  element.

- `messages` (`character`) the messages output when evaluating the code
  element.

- `dependency` (`character`) names of objects that appear in this call
  and gets affected by this call, separated by `<-` (objects on LHS of
  `<-` are affected by this line, and objects on RHS are affecting this
  line).
