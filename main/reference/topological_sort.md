# Topological graph sort

Graph is a `list` which for each node contains a vector of child nodes
in the returned list, parents appear before their children.

## Usage

``` r
topological_sort(graph)
```

## Arguments

- graph:

  (`named list`) with node vector elements

## Details

Implementation of `Kahn` algorithm with a modification to maintain the
order of input elements.
