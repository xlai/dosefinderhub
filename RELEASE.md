# Release plan

## Target: v0.1.0

First tagged release. Its main goal is to bring the work spread across many unmerged branches back into one consistent `main`, resolving the conflicts between them. This is not a 1.0 stability commitment: interfaces may still change between 0.x releases.

v0.1.0 is a **preview release** as a whole. All five tabs ship, with one app-wide notice that results should be checked by a statistician and that interfaces may change. Individual tabs are not marked separately.

## Scope

v0.1.0 is limited to the dose-finding methods already implemented. No new methods are added.

- CRM
- BOIN
- 3+3

The app is the integrated bslib app now in `app_skeleton/`, with five tabs: Introduction, Questionnaire, Trial Design, Simulation and Conduct.

### Branches

`main` holds stable releases. `dev` holds release-candidate work, and every PR targets `dev`.

In v0.1.0:

- [ ] `exp` lands on `dev` as one fast-forward. `dev` is an ancestor of `exp`, so no history is rewritten.
- [ ] `main` is merged into `dev`, so `dev` has this file, `NEWS.md` and `documentation/decisions/`.
- [ ] Every fix after that goes to `dev` as its own small PR.

Left out of v0.1.0:

- `xlai/issue80` (simulation refactor, https://github.com/xlai/dosefinderhub/pull/81): merging it empties the comparative tables and brings a scenario row-index bug. Revisit after v0.1.0.
- `xlai/issue78` (basic mode, https://github.com/xlai/dosefinderhub/pull/79): unfinished. Revisit after v0.1.0.
- `doc/variablelist` (https://github.com/xlai/dosefinderhub/pull/13): kept as input for the parameter design after v0.1.0. Not used in v0.1.0.
- Fully merged, superseded or obsolete, so nothing to bring in: `a-working-simulations-tab`, `feature/readme`, `feature/trial_interface2`, `misc/add_reactive_output_to_ui`, `setting-up-the-bslib-dashboard`, `revert-47-hotfix/namespace-issue`.

### Not in v0.1.0

- No refactoring unless it is needed to fix a release-blocking problem.
- Framework choice (rhino, golem or leprechaun), and how parameters are stored, read and updated (master variable list, https://github.com/xlai/dosefinderhub/issues/12).
- One shared model builder for all tabs, and Simulation through a single `simulate_compare()` call.
- Interactive plots (ggiraph), more than 3 scenarios, and upload and download of designs.
- Evidence-based scoring for the questionnaire recommendation.

## Release criteria

v0.1.0 is ready to tag when all of these are true on `dev`, and `dev` has been merged into `main` through a release PR.

### App

- [ ] The app starts from a single self-contained app folder, with no paths into the old `app/` tree.
- [ ] The app opens with a default design loaded, replacing the random dummy data, so it is ready to simulate without any input.
- [ ] With the default design, each of CRM, BOIN and 3+3 runs in Trial Design, Simulation and Conduct without an error or a lost session.
- [ ] The app-wide preview notice is shown.

### Known bugs fixed

Simulation and Trial Design:

- [ ] Individual plots show the correct design label for every combination of selected designs.
- [ ] Cohort size from the inputs is used by all three designs.
- [ ] The BOIN stopping-rule input is used, not a hard-coded value.
- [ ] Each simulation run sets its own seed, so results repeat for the same inputs and do not depend on which other designs are selected. The seed is shown.
- [ ] BOIN boundaries are recomputed when the target changes, and a run is blocked with a message unless p.saf < target < p.tox.
- [ ] Editing the CRM skeleton does not break CRM runs. The skeleton is checked for length, increasing order and range (0, 1), and is rebuilt when the target changes.
- [ ] "Accuracy" is correct when no dose is at or below target. "Overdose" is relabelled to what it measures (mean number of DLTs).
- [ ] BOIN lambda inputs reach the simulation.
- [ ] Cohort size is not overwritten at startup.
- [ ] Input ids are namespaced, so the Trial Design configuration download and upload work.

Conduct:

- [ ] Conduct builds each design from the same inputs as Simulation (no hard-coded stopping rule, CRM uses the prior variance, no target for 3+3).
- [ ] The BOIN plot works.

Questionnaire:

- [ ] Reset and Save work without ending the session.

### Exports

- [ ] The operating characteristics (OC) report and the Conduct report download as a self-contained HTML file, with tables and plots. They need no LaTeX or pandoc.
- [ ] Both reports start with the parameter specification (design, target, doses, start dose, skeleton, stopping rules, scenarios, number of simulations, seed). Questionnaire answers and package versions are appendices.
- [ ] The OC tables and the Conduct patient table also download as CSV.
- [ ] PDF, Rmd and Excel export are removed, unless users ask for them.

### Tests and CI

- [ ] testthat tests cover the bugs fixed above, as far as they can be tested outside the browser.
- [ ] A smoke test starts the app and runs one simulation for each design.
- [ ] CI runs the tests on every PR to `dev`.
- [ ] `dev` has one `renv.lock`, regenerated in a dedicated PR.

### Deployment

- [ ] The app is exported with Shinylive and deployed to GitHub Pages from `main`, copying the dtptite-shiny setup (trialr stub, export script, workflow) and changing only what must differ.
- [ ] The deployed app loads and runs one simulation for each of CRM, BOIN and 3+3.

### History and documentation

- [ ] Before the old `app/` tree is removed, the current `main` is tagged as the prototype archive. The PR that removes it credits the earlier contributors and links the tag.
- [ ] `app_skeleton/` is moved into place with `git mv` in its own commit, so file history is kept.
- [ ] README says what the app does, how to run it locally and the link to the deployed app.
- [ ] NEWS.md lists the changes in v0.1.0 and states that it is a preview release.
- [ ] The main v0.1.0 decisions are recorded in `documentation/decisions/`.

## Decisions and assumptions

The main decisions and assumptions behind this release are recorded in `documentation/decisions/`.
