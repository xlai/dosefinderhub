# dosefinderhub 0.1.0

First tagged release. **This is a preview release.** Results should be checked by a statistician before they are used, the questionnaire's recommendation is based on expert opinion and not yet on published evidence, and the app may still change in later 0.x versions. The app shows this notice above every tab (https://github.com/xlai/dosefinderhub/pull/135).

v0.1.0 brings the work from several branches into one app with five tabs (Introduction, Questionnaire, Trial Design, Simulation and Conduct) and three designs: CRM, BOIN and 3+3, from the `escalation` package. The scope and release criteria are in `RELEASE.md`, and the step-by-step plan is in https://github.com/xlai/dosefinderhub/issues/100.

## Breaking changes

- Downloads are HTML reports and CSV tables only. The PDF, Rmd and Excel downloads are removed (https://github.com/xlai/dosefinderhub/pull/129, https://github.com/xlai/dosefinderhub/pull/131, https://github.com/xlai/dosefinderhub/pull/132).
- The app lives in `app/` (previously `app_skeleton/`) and runs from its own folder (https://github.com/xlai/dosefinderhub/pull/105, https://github.com/xlai/dosefinderhub/pull/107, https://github.com/xlai/dosefinderhub/pull/108).

## New features

- The app opens with a default design (https://github.com/xlai/dosefinderhub/pull/101).
- Simulation report (HTML) and results tables (CSV) (https://github.com/xlai/dosefinderhub/pull/129).
- Conduct report (HTML) and patient table (CSV) (https://github.com/xlai/dosefinderhub/pull/131).
- Simulation shows the seed used (https://github.com/xlai/dosefinderhub/pull/120).
- Conduct says why the trial stopped (https://github.com/xlai/dosefinderhub/pull/124).
- Browser version of the app with Shinylive, published on GitHub Pages (https://github.com/xlai/dosefinderhub/pull/113).
- New README (https://github.com/xlai/dosefinderhub/pull/136).

## Tests, CI and dependencies

- Automated tests, including a smoke test that runs one simulation for each design (https://github.com/xlai/dosefinderhub/pull/111).
- Tests run on GitHub Actions for every pull request to `dev` (https://github.com/xlai/dosefinderhub/pull/112, https://github.com/xlai/dosefinderhub/pull/137).
- `renv.lock` updated to the latest `escalation`, with a separate profile for the tests (https://github.com/xlai/dosefinderhub/pull/109, https://github.com/xlai/dosefinderhub/pull/110, https://github.com/xlai/dosefinderhub/pull/133).

## Earlier work

- The prototype app that was in `app/` and a stray `debug.log` are removed. The current app, previously in `app_skeleton/`, now lives in `app/`. The prototype stays browsable at the tag [`prototype-archive-2025`](https://github.com/xlai/dosefinderhub/tree/prototype-archive-2025). Thanks to Sian Dennett ([@sdennett20](https://github.com/sdennett20)), Jake Holton ([@jakeholton](https://github.com/jakeholton)) and Eduardo Viegas ([@eduardo-vie](https://github.com/eduardo-vie)), who built the prototype with Xiaoran Lai.
