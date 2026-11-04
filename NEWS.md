# dosefinderhub 0.1.0

First tagged release. **This is a preview release.** Results should be checked by a statistician before they are used, the questionnaire's recommendation is based on expert opinion and not yet on published evidence, and the app may still change in later 0.x versions. The app shows this notice above every tab (https://github.com/xlai/dosefinderhub/pull/135).

v0.1.0 brings the work from several branches into one app with five tabs (Introduction, Questionnaire, Trial Design, Simulation and Conduct) and three designs: CRM, BOIN and 3+3, from the `escalation` package. The scope and release criteria are in `RELEASE.md`, and the step-by-step plan is in https://github.com/xlai/dosefinderhub/issues/100.

## Breaking changes

- Downloads are now HTML reports and CSV tables only. The PDF, Rmd and Excel downloads are removed, so no LaTeX is needed (https://github.com/xlai/dosefinderhub/pull/129, https://github.com/xlai/dosefinderhub/pull/131, https://github.com/xlai/dosefinderhub/pull/132).
- The app lives in `app/` (previously `app_skeleton/`) and runs from its own folder: `shiny::runApp("app")` (https://github.com/xlai/dosefinderhub/pull/105, https://github.com/xlai/dosefinderhub/pull/107, https://github.com/xlai/dosefinderhub/pull/108).

## New features

- Default design: the app opens with one signed-off default design, used everywhere a default is set, so a simulation runs without changing any input (https://github.com/xlai/dosefinderhub/pull/101).
- Simulation report: download the results as one HTML report with tables and plots, starting with the design settings (including the seed and the number of simulations) and ending with the questionnaire answers and package versions. The results tables can also be downloaded as CSV (https://github.com/xlai/dosefinderhub/pull/129).
- Conduct report: download the trial so far as an HTML report, or the patient table as CSV (https://github.com/xlai/dosefinderhub/pull/131).
- Simulation shows the seed used. Each design and scenario is run with the same seed, so the same inputs give the same results whichever other designs are selected (https://github.com/xlai/dosefinderhub/pull/120).
- Conduct says why the trial stopped (https://github.com/xlai/dosefinderhub/pull/124).
- Browser version: a Shinylive build of the app that runs in the browser through webR, with a stand-in `trialr` package so that `escalation` loads there (https://github.com/xlai/dosefinderhub/pull/113). It is published on GitHub Pages from `main`, from this release on.
- README rewritten: what the app does, who it is for, how to run it and where to find it online (https://github.com/xlai/dosefinderhub/pull/136).

## Bug fixes

Trial Design:

- Editing the CRM skeleton no longer breaks the CRM simulation. The skeleton is checked before it is used, and it is rebuilt from the target, halfwidth and prior MTD guess (https://github.com/xlai/dosefinderhub/pull/114).
- The BOIN boundary settings (p.saf and p.tox) are used, follow the target, and are checked against it, with a message instead of a crash when they don't fit (https://github.com/xlai/dosefinderhub/pull/115).
- The Trial Design inputs, such as the cohort size, are no longer reset when the app starts or after the questionnaire answers are sent (https://github.com/xlai/dosefinderhub/pull/116).
- Saving and loading the Trial Design settings works (https://github.com/xlai/dosefinderhub/pull/117).

Simulation:

- When every dose is above the target, choosing no dose counts as correct in "Accuracy". The metric labelled "Overdose" is replaced by what it measured: the % of patients with a DLT in each trial (https://github.com/xlai/dosefinderhub/pull/102).
- Plots show the right design names, whichever designs are selected (https://github.com/xlai/dosefinderhub/pull/118).
- CRM and BOIN use the cohort size you enter, and BOIN uses the stopping rule you choose. 3+3 always uses cohorts of 3, and the app says so (https://github.com/xlai/dosefinderhub/pull/119).
- "View Conduct Tab" opens the Conduct tab (https://github.com/xlai/dosefinderhub/pull/121).

Conduct:

- Conduct builds its designs the same way as Simulation, from the same settings (https://github.com/xlai/dosefinderhub/pull/122).
- The BOIN plot shows correctly before every dose has been given (https://github.com/xlai/dosefinderhub/pull/123).
- Remove Cohort keeps the first cohort, whatever the cohort size (https://github.com/xlai/dosefinderhub/pull/125).

Questionnaire:

- The Reset, Save, Upload and Previous buttons work without closing the session (https://github.com/xlai/dosefinderhub/pull/126).
- The questionnaire starts from the default design (https://github.com/xlai/dosefinderhub/pull/127).
- The scoring conditions are fixed: the recommendation chooses 3+3 when there is no statistician, and cohort sizes other than 3 and targets on a band edge now score. The rationale text no longer shows contradictory notes (https://github.com/xlai/dosefinderhub/pull/128).
- The "questionnaire complete" message no longer fails because of an invalid notification type (https://github.com/xlai/dosefinderhub/pull/130).

## Tests, CI and dependencies

- Automated tests with testthat, including a shinytest2 smoke test that starts the app and runs one simulation for each design (https://github.com/xlai/dosefinderhub/pull/111). 293 expectations at the time of release.
- The tests run on GitHub Actions for every pull request to `dev`, including changes to the app's own Markdown pages (https://github.com/xlai/dosefinderhub/pull/112, https://github.com/xlai/dosefinderhub/pull/137).
- Unused package loads (tidyverse, rmarkdown, shinydashboard) are removed (https://github.com/xlai/dosefinderhub/pull/109).
- `renv.lock` is regenerated with the latest `escalation` (0.2.3), and the test packages have their own renv profile (https://github.com/xlai/dosefinderhub/pull/110, https://github.com/xlai/dosefinderhub/pull/133).

## Earlier work

- The prototype app that was in `app/` and a stray `debug.log` are removed. The current app, previously in `app_skeleton/`, now lives in `app/`. The prototype stays browsable at the tag [`prototype-archive-2025`](https://github.com/xlai/dosefinderhub/tree/prototype-archive-2025). Thanks to Sian Dennett ([@sdennett20](https://github.com/sdennett20)), Jake Holton ([@jakeholton](https://github.com/jakeholton)) and Eduardo Viegas ([@eduardo-vie](https://github.com/eduardo-vie)), who built the prototype with Xiaoran Lai.
