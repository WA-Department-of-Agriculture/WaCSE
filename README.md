
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="inst/app/www/rmd/img/favicon.png" align="right" width="130"/>

# Washington Climate Smart Estimator (WaCSE)

The [Washington State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources)
developed WaCSE for the Washington State Conservation Commission to use
in the [Sustainable Farms and Fields](https://www.scc.wa.gov/sff) (SFF)
program. Intended users are the Conservation Commission, conservation
districts, growers, and anyone interested in reducing agricultural
greenhouse gas (GHG) emissions. This interactive tool estimates the
reduction of GHG emissions from different conservation practices across
Washington’s diverse counties.

This app was built using the
[Golem](https://github.com/ThinkR-open/golem) framework.

## Install and run WaCSE

You can install and run the development version of WaCSE from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("WA-Department-of-Agriculture/WaCSE")

WaCSE::run_app()
```

## Data reference

Amy Swan, Mark Easter, Adam Chambers, Kevin Brown, Stephen A. Williams,
Jeff Creque, John Wick, Keith Paustian. 2020. COMET-Planner Dataset,
Version 2.1, Build 1, and COMET-Planner Report: Carbon and Greenhouse
Gas Evaluation for NRCS Conservation Practice Planning. A Companion
report to [COMET-Planner](http://www.comet-planner.com).
[COMET-Planner_Report_Final.pdf](https://bfuels.nrel.colostate.edu/beta/COMET-Planner_Report_Final.pdf).

## Suggested Citation

When using WaCSE, we appreciate if you include a reference in your
publications. To cite the web application, please use:

> Ryan JN, Michel L, Gelardi DL. 2022. WaCSE: A shiny web app for
> comparing climate benefits of agricultural conservation practices.
> Natural Resources Assessment Section, Washington Department of
> Agriculture. <https://github.com/WA-Department-of-Agriculture/WaCSE>.

## Source Code and Feedback

To view the source code, visit the [GitHub
repository](https://github.com/WA-Department-of-Agriculture/WaCSE).

If you have feedback or would like to report a bug, please [submit an
issue](https://github.com/WA-Department-of-Agriculture/WaCSE/issues) or
contact the app developer: Jadey Ryan at
[jryan@agr.wa.gov](mailto:jryan@agr.wa.gov?subject=WaCSE).
