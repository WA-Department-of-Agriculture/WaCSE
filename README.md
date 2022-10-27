
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="inst/app/www/rmd/img/favicon.png" align="right" width="130"/>

# Washington Climate Smart Estimator (WaCSE)

The
<a href= "https://agr.wa.gov/departments/land-and-water/natural-resources" target="_blank">Washington
State Department of Agriculture</a> developed WaCSE for the Washington
State Conservation Commission to use in the
<a href="https://www.scc.wa.gov/sff" target="_blank">Sustainable Farms
and Fields</a> (SFF) program. Intended users are the Conservation
Commission, conservation districts, growers, and anyone interested in
reducing agricultural greenhouse gas (GHG) emissions. This interactive
tool estimates the reduction of GHG emissions from different
conservation practices across Washington’s diverse counties.

This app was built using the
<a href= "https://github.com/ThinkR-open/golem" target = "_blank">Golem</a>
framework.

## Install and run WaCSE

You can install and run the development version of WaCSE from
<a href= "https://github.com/" target = "_blank">GitHub</a> with:

``` r
# install.packages("devtools")
devtools::install_github("WA-Department-of-Agriculture/WaCSE")

WaCSE::run_app()
```

## Data reference

Amy Swan, Mark Easter, Adam Chambers, Kevin Brown, Stephen A. Williams,
Jeff Creque, John Wick, Keith Paustian. 2022. COMET-Planner Dataset,
Version 3.0, Build 1, and COMET-Planner Report: Carbon and Greenhouse
Gas Evaluation for NRCS Conservation Practice Planning. A Companion
report to
<a href="http://www.comet-planner.com" target = "_blank">COMET-Planner.</a>
<a href="https://storage.googleapis.com/comet_public_directory/planner50states/pdfs/COMET-PlannerReport.pdf" target = "_blank">COMET-Planner
Report.</a>

## Suggested Citation

When using WaCSE, we appreciate if you include a reference in your
publications. To cite the web application, please use:

> Ryan JN, Michel L, Gelardi DL. 2022. WaCSE: A shiny web app for
> comparing climate benefits of agricultural conservation practices.
> Natural Resources Assessment Section, Washington Department of
> Agriculture.
> <a href = "https://github.com/WA-Department-of-Agriculture/WaCSE" target = "_blank"><https://github.com/WA-Department-of-Agriculture/WaCSE>.</a>

## Source Code and Feedback

To view the source code, visit the
<a href = "https://github.com/WA-Department-of-Agriculture/WaCSE" target = "_blank">GitHub
repository.</a>

If you have feedback or would like to report a bug, please
<a href = "https://github.com/WA-Department-of-Agriculture/WaCSE/issues" target = "_blank">
submit an issue</a> or contact the app developer: Jadey Ryan at
[jryan@agr.wa.gov](mailto:jryan@agr.wa.gov?subject=WaCSE).
