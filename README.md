
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# featscreen

**featscreen** is an R package providing ready-to-use functions for
straightforward feature screening in both supervised and unsupervised
settings.

**featscreen** is crafted with the overarching goal of simplifying and
unifying commonly used screening techniques into a single, accessible R
package.

Explore the variety of supervised and unsupervised screening techniques
available in **featscreen** to tailor your feature selection process
according to your analysis needs.

The following table provides an overview of the supervised methods
currently supported in the package:

|   category   |                                                                    name | numerical | categorical | survival |
|:------------:|------------------------------------------------------------------------:|:---------:|:-----------:|:--------:|
| correlation  |                           Kendall’s rank correlation coefficient t-test |     x     |             |          |
| correlation  |                 Pearson’s product moment correlation coefficient t-test |     x     |             |          |
| correlation  |                          Spearman’s rank correlation coefficient t-test |     x     |             |          |
|  two-groups  |                                      two-sample Student’s pooled t-test |           |      x      |          |
|  two-groups  |                                      paired two-sample Student’s t-test |           |      x      |          |
|  two-groups  | two-sample t-test with the Welch modification to the degrees of freedom |           |      x      |          |
|  two-groups  |                             paired two-sample Wilcoxon signed-rank test |           |      x      |          |
|  two-groups  |                                          two-sample Mann-Whitney U-test |           |      x      |          |
| multi-groups |                                     one-way analysis of variance F-test |           |      x      |          |
| multi-groups |               one-way analysis of variance F-test with Welch correction |           |      x      |          |
| multi-groups |                                                       Pearson’s χ²-test |           |      x      |          |
| multi-groups |                                                   Kruskal-Wallis H-test |           |      x      |          |
|   survival   |                                    Cox PH regression coefficient z-test |           |             |    x     |
| biostatistic |                                        empirical Bayes moderated F-test |     x     |      x      |          |
| biostatistic |                                        empirical Bayes moderated t-test |     x     |      x      |          |
| biostatistic |                    significant analysis of microarrays permutation test |     x     |      x      |    x     |

Continuing our exploration of feature screening capabilities in
**featscreen**, the following table offers insights into the
unsupervised screening methods integrated into the package:

|                          name | numerical | categorical | multiresponse | survival |
|------------------------------:|:---------:|:-----------:|:-------------:|:--------:|
|  above-median frequency ratio |     x     |             |               |          |
| above-minimum frequency ratio |     x     |             |               |          |
|                  median value |     x     |             |               |          |
|           missing value ratio |     x     |      x      |       x       |    x     |
|                   variability |     x     |             |               |          |

## Installation

You can install latest development version from GitHub (requires
[devtools](https://github.com/hadley/devtools) package):

``` r
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github(
  repo = "alebarberis/featscreen", 
  dependencies = TRUE, 
  build_vignettes = FALSE
)
```

## Getting started

If you are just getting started with **featscreen**, we recommend
looking at the [Getting Started](articles/featscreen.html) section of
the site.

## Credits

**featscreen** was conceived, developed, and is maintained by Alessandro
Barberis ([@alebarberis](https://github.com/alebarberis)).

## Acknowledgements

I would like to express my sincere gratitude to [Prostate Cancer
UK](https://prostatecanceruk.org/) for their generous funding, which
made it possible for me to develop the first version of this package.
