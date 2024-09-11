<!-- badges: start -->
  [![R-CMD-check](https://github.com/WorldHealthOrganization/poliprep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WorldHealthOrganization/poliprep/actions/workflows/R-CMD-check.yaml)  [![CodeFactor](https://www.codefactor.io/repository/github/WorldHealthOrganization/poliprep/badge)](https://www.codefactor.io/repository/github/WorldHealthOrganization/poliprep) [![codecov](https://codecov.io/gh/WorldHealthOrganization/poliprep/graph/badge.svg?token=PCYAMB2S6Y)](https://codecov.io/gh/WorldHealthOrganization/poliprep)
<!-- badges: end -->

# poliprep

## What is poliprep?

`poliprep` is an R package developed by the polio data science team at The World Health Organisation Regional Office for Africa ([WHO AFRO](https://www.afro.who.int/)). It is designed to assist users who have access to and work with datasets collected as part of the [Global Polio Eradication Initiative](https://polioeradication.org/).

The package offers a number of functions for various tasks, including importing and exporting data, pulling data from relevant APIs, data processing and cleaning, as well as performing validations and checks. It also features the capability to generate reports or scorecards highlighting areas of concern identified during the validation process.

## :wrench: Installation

The package can be installed directly using `devtools`. The steps are as follows:

```r
# 1) Install devtools if you haven't already
install.packages("devtools")

# 2) Install the poliprep package from GitHub
devtools::install_github("WorldHealthOrganization/poliprep")
```

## :globe_with_meridians: Roadmap

We plan to add a number of functions which do the following:

- [x] Import and export different datasets (tabular, shapefiles, etc.).
- [x] Pull data from ONA using an API call.
- [x] Update data from ONA API without re-downloading.
- [x] Clean and fix names of places including geolocations.
- [x] Check and clean geo-coordinates.
- [x] Match variables naming conventions & datatypes within two dataframes.
- [x] Validate POLIS data.
- [x] Validate AFP Surveillance data.
- [ ] Validate Environmental Surveillance data.
- [ ] Validate all Lab data.
- [ ] Produce validation reports and scorecards.


## :incoming_envelope: Contacting us

For any issues or questions about `poliprep`, please contact Mo at [moyusuf\@who.int](mailto:moyusuf@who.int).

## :handshake: Guidance on making contributions

We welcome contributions via forking and pull requests. For guidance and best practices, please follow [WHO's Contribution Guidelines](https://github.com/WorldHealthOrganization/open-source-communication-channel/blob/main/CONTRIBUTING.md). Additionally, before any contribution, please ensure you are familiar with WHO's [Contributor Covenant Code of Conduct](https://github.com/WorldHealthOrganization/open-source-communication-channel/tree/main?tab=coc-ov-file).
