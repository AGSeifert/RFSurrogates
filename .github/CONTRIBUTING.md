# Contributing

## Making Changes

This project follows [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) to allow for [Semantic Versioning](https://semver.org/).

- [Conventional commit types explained](https://daily-dev-tips.com/posts/git-basics-conventional-commits/)

### Adding new functionality

New functions should have test cases.

``` r
usethis::use_r("NEW_FUNCTION_NAME_HERE")
usethis::use_test("NEW_FUNCTION_NAME_HERE")
```

### Changing `DESCRIPTION`

Cleanup after making changes to `DESCRIPTION`.

``` r
usethis::use_tidy_description()
```

## Preparing your Pull Request

Make sure documentation is up-to-date and checks pass on your machine.

``` r
styler::style_pkg()
devtools::document()
devtools::check()
```
