# DoseFinderHub

DoseFinderHub is an R Shiny app for planning, comparing and running Phase 1 dose-finding trials. It covers three designs: the continual reassessment method (CRM), Bayesian optimal interval (BOIN) and 3+3. The designs come from the [`escalation`](https://cran.r-project.org/package=escalation) package.

It is for trialists and statisticians who want to compare these designs on their own trial settings before the trial, and then use the chosen design while the trial runs.

**v0.1.0 is a preview release.** Results should be checked by a statistician before they are used. The questionnaire's recommendation is based on expert opinion, not yet on published evidence. The app may still change in later versions.

## What the app does

The app has five tabs:

- **Introduction**: an overview of the three designs and the team.
- **Questionnaire**: questions about your trial. The app recommends a design and passes your answers on to Trial Design.
- **Trial Design**: the settings for each design, such as the number of doses, the target toxicity, the cohort size and the stopping rules. The app opens with a default design, so you can run a simulation straight away. You can save the settings to a file and load them again.
- **Simulation**: simulate CRM, BOIN and 3+3 under several toxicity scenarios and compare them on accuracy, duration and the % of patients with a DLT. Download the results as an HTML report or the tables as CSV.
- **Conduct**: choose one design, enter the patients' outcomes cohort by cohort and see the recommended next dose. Download an HTML report or the patient table as CSV.

## Use it online

The app will run in your web browser at https://xlai.github.io/dosefinderhub/, without installing R. It is published with [Shinylive](https://posit-dev.github.io/r-shinylive/) and is available from the v0.1.0 release. Simulations are slower in the browser than in R on your computer.

## Run it on your computer

You need R 4.5 (the lockfile was built with R 4.5.1) and the [`renv`](https://rstudio.github.io/renv/) package.

1. Clone the repository and start R in its root folder. The project's `.Rprofile` activates renv.
2. Install the packages recorded in `renv.lock`:

   ```r
   renv::restore()
   ```

3. Start the app:

   ```r
   shiny::runApp("app")
   ```

Some packages need system libraries on Linux (for example GDAL, GEOS, PROJ and udunits, used through `ggpattern`). `.github/workflows/tests.yml` lists the ones used on Ubuntu.

## Run the tests

The tests use their own renv profile, with extra packages such as `testthat` and `shinytest2`. The browser tests need Chrome or Chromium.

```sh
RENV_PROFILE=test Rscript -e 'renv::restore(prompt = FALSE)'
RENV_PROFILE=test Rscript -e 'shiny::runTests("app")'
```

The tests also run on GitHub Actions for every pull request to `dev`.

## Project layout

- `app/`: the Shiny app (`app.R`, `pages/`, `R/`) and its tests (`tests/`).
- `webr/`: the Shinylive build, including a stand-in `trialr` package so that `escalation` loads in the browser.
- `documentation/decisions/`: the decision records.
- `RELEASE.md`: the scope and criteria for v0.1.0. `NEWS.md`: what changed in each release.

## Contributing

Work happens on the `dev` branch. Please open an issue first for bigger changes, and send pull requests to `dev`.

## License

[GNU General Public License v3.0 (GPLv3)](https://www.gnu.org/licenses/gpl-3.0.en.html). See the `LICENSE` file for details.

## Contact

For questions, feedback or support, please [open an issue on GitHub](https://github.com/xlai/dosefinderhub/issues).
