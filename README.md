
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fediverser

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `fediverser` is to offer a client for the Fediverse that R
users can customise: decide what is in front of them and why, not
necessarily the most recent posts.

At this stage, this is just a prototype. Install and `run_app()` to see
the little that works (show contents of a hashtag from an instance, but
nothing more).

It is built on modules, in a way that should eventually enable full
functionalities (e.g. reply, favorite, etc.) from the app.

Easy to toggle customisation could include:

- show just latest post by each user
- do not show again posts that I’ve already seen
- …

## Installation

You can install the development version of `fediverser` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("giocomai/fediverser")
```

## Example

Just run:

``` r
run_app()
```

And you may get an idea of where this is going, obviously with more
controls, better rendering of cards, etc.

## Notes

Names of functions are subject to change, as they will likely have a
common prefix such as `fedi_`

## Code of Conduct

Please note that the `fediverser` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
