# Release plan

## Target: v0.1.0

v0.1.0 will be our first tagged release. The main aim is to bring together the work that is currently spread across many unmerged branches into one consistent `main`. It is not a 1.0 release, so interfaces may still change in later 0.x versions.

We will treat the whole of v0.1.0 as a preview. All five tabs will be included, and the app will show a single notice saying that results should be checked by a statistician and that things may still change.

The step-by-step plan, with one small pull request per step, is in https://github.com/xlai/dosefinderhub/issues/100.

## Scope

This release covers the three methods we already have: CRM, BOIN and 3+3. We won't add any new methods.

The app is the bslib version now in `app_skeleton/`, with five tabs: Introduction, Questionnaire, Trial Design, Simulation and Conduct. During this release it moves to `app/`, after the old prototype in `app/` is retired.

### Branches

From now on we keep two long-lived branches: `main` for stable releases and `dev` for work towards the next release. All pull requests go to `dev`.

- [x] Merge `exp` into `dev`. This keeps all existing history.
- [x] Merge `main` into `dev`, so that `dev` also has this file, `NEWS.md` and `documentation/decisions/`.
- [ ] Make each fix as its own small pull request to `dev`.

Some branches will not be part of this release:

- `xlai/issue80`, the simulation refactor (https://github.com/xlai/dosefinderhub/pull/81). Merging it now would empty the comparison tables and bring in a bug with scenario selection. We will come back to it after v0.1.0.
- `xlai/issue78`, basic mode (https://github.com/xlai/dosefinderhub/pull/79). It isn't finished yet, so it will wait until after v0.1.0.
- `doc/variablelist` (https://github.com/xlai/dosefinderhub/pull/13). We will keep it for when we redesign how parameters are handled, after v0.1.0.
- `a-working-simulations-tab`, `feature/readme`, `feature/trial_interface2`, `misc/add_reactive_output_to_ui`, `setting-up-the-bslib-dashboard` and `revert-47-hotfix/namespace-issue` are already merged, replaced by later work, or no longer needed.

### What waits until after v0.1.0

- Refactoring, unless something is broken and blocks the release.
- Choosing an app framework (rhino, golem or leprechaun), and deciding how parameters are stored and shared between tabs, including the master variable list (https://github.com/xlai/dosefinderhub/issues/12, https://github.com/xlai/dosefinderhub/issues/72).
- A single shared way of building the models for all tabs, and running all simulations through one `simulate_compare()` call.
- Interactive plots (with ggiraph), saving and loading designs, and a progress bar for long simulations.
- Creating the default scenarios automatically from the number of doses and the target, and allowing more than three scenarios (https://github.com/xlai/dosefinderhub/issues/95).
- Evidence for how the questionnaire should score its method recommendation (https://github.com/xlai/dosefinderhub/issues/64). In v0.1.0 the recommendation is shown, and the preview notice covers that its scoring is still based on expert opinion.

## Release criteria

We will tag v0.1.0 once everything below is done on `dev` and `dev` has been merged into `main`.

"Works" below means the app shows the expected result without an error message, a red error in place of an output, or the session closing.

### The app

- [ ] The app runs from its own folder and no longer depends on the old `app/` folder.
- [ ] The app opens with the signed-off default design (https://github.com/xlai/dosefinderhub/issues/91), so you can run a simulation straight away.
- [ ] With that default design, CRM, BOIN and 3+3 all work in Trial Design, Simulation and Conduct.
- [ ] The preview notice is shown.

### Known bugs to fix

Trial Design and Simulation:

- [ ] Plots show the right design names, whichever designs are selected.
- [ ] The cohort size you enter is used by CRM and BOIN. 3+3 always uses cohorts of 3, and the app says so next to the input.
- [ ] The BOIN stopping rule you choose is actually used.
- [ ] The same inputs always give the same results, whichever other designs are selected. The seed is shown.
- [ ] The BOIN boundary settings you enter are used, they are updated when the target changes, and the app explains the problem instead of crashing when they don't fit the target.
- [ ] Editing the CRM skeleton no longer breaks the CRM simulation. The skeleton is checked before it is used, and it updates when the target changes.
- [ ] When every dose is above the target, choosing no dose counts as a correct result in "Accuracy". "Overdose" is renamed to what it really shows: the average number of DLTs.
- [ ] The cohort size is no longer reset when the app starts.
- [ ] Saving and loading the trial design settings works.
- [ ] "View Conduct Tab" opens the Conduct tab.

Conduct:

- [ ] Conduct uses the same design settings as Simulation.
- [ ] The BOIN plot shows correctly.

Questionnaire:

- [ ] The Reset, Save and Upload buttons work without closing the session.
- [ ] The recommendation chooses 3+3 when the user has no statistician.
- [ ] The questionnaire answers still reach Trial Design.

### Reports and downloads

- [ ] You can download the simulation results and the Conduct results as a single HTML report with tables and plots. This doesn't need LaTeX.
- [ ] Each report starts with the design settings used, including the seed and the number of simulations. The questionnaire answers and package versions go at the end.
- [ ] The simulation results tables and the Conduct patient table can also be downloaded as CSV.
- [ ] The PDF, Rmd and Excel downloads are removed. HTML and CSV are the only formats.

### Tests and CI

- [ ] Each bug fixed above has an automated test. Where a bug can only be seen in the browser, the PR says how it was checked by hand.
- [ ] A quick test starts the app and runs one simulation for each design.
- [ ] Tests run automatically on every pull request to `dev`.
- [ ] `dev` has one up-to-date `renv.lock` with the latest `escalation` release, updated only in its own pull requests.

### Deployment

- [ ] The app is published on GitHub Pages using Shinylive.
- [ ] Opened in a current version of Chrome or Firefox, the published app loads and runs a simulation for each of CRM, BOIN and 3+3. This is checked by hand.

### History and documentation

- [ ] Before removing the old `app/` folder, we tag the current `main` so the earlier work stays easy to find, and we credit the people who built it.
- [ ] `app_skeleton/` is renamed to `app/` in a way that keeps its file history.
- [ ] The README explains what the app does, how to run it and where to find it online.
- [ ] NEWS.md lists what changed in v0.1.0 and says it is a preview.
- [ ] The main decisions for v0.1.0 are written up in `documentation/decisions/`.

## Decisions and assumptions

The main decisions and assumptions behind this release are recorded in `documentation/decisions/`.
