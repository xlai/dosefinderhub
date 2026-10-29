# Release plan

## Target: v0.1.0

v0.1.0 will be our first tagged release. The main aim is to bring together the work that is currently spread across many unmerged branches into one consistent `main`. It is not a 1.0 release, so interfaces may still change in later 0.x versions.

We will treat the whole of v0.1.0 as a preview. All five tabs will be included, and the app will show a single notice saying that results should be checked by a statistician and that things may still change.

## Scope

This release covers the three methods we already have: CRM, BOIN and 3+3. We won't add any new methods.

The app is the bslib version in `app_skeleton/`, with five tabs: Introduction, Questionnaire, Trial Design, Simulation and Conduct.

### Branches

From now on we keep two long-lived branches: `main` for stable releases and `dev` for work towards the next release. All pull requests go to `dev`.

To get started:

- [ ] Merge `exp` into `dev`. This keeps all existing history.
- [ ] Merge `main` into `dev`, so that `dev` also has this file, `NEWS.md` and `documentation/decisions/`.
- [ ] After that, make each fix as its own small pull request to `dev`.

Some branches will not be part of this release:

- `xlai/issue80`, the simulation refactor (https://github.com/xlai/dosefinderhub/pull/81). Merging it now would empty the comparison tables and bring in a bug with scenario selection. We will come back to it after v0.1.0.
- `xlai/issue78`, basic mode (https://github.com/xlai/dosefinderhub/pull/79). It isn't finished yet, so it will wait until after v0.1.0.
- `doc/variablelist` (https://github.com/xlai/dosefinderhub/pull/13). We will keep it for when we redesign how parameters are handled, after v0.1.0.
- `a-working-simulations-tab`, `feature/readme`, `feature/trial_interface2`, `misc/add_reactive_output_to_ui`, `setting-up-the-bslib-dashboard` and `revert-47-hotfix/namespace-issue` are already merged, replaced by later work, or no longer needed.

### What waits until after v0.1.0

- Refactoring, unless something is broken and blocks the release.
- Choosing an app framework (rhino, golem or leprechaun), and deciding how parameters are stored and updated, including the master variable list (https://github.com/xlai/dosefinderhub/issues/12).
- A single shared way of building the models for all tabs, and running all simulations through one `simulate_compare()` call.
- Interactive plots (with ggiraph), more than three scenarios, and saving and loading designs.
- Finding evidence for how the questionnaire should score its recommendation.

## Release criteria

We will tag v0.1.0 once everything below is done on `dev` and `dev` has been merged into `main`.

### The app

- [ ] The app runs from a single folder and no longer depends on the old `app/` folder.
- [ ] The app opens with a sensible default design instead of random dummy data, so you can run a simulation straight away.
- [ ] With that default design, CRM, BOIN and 3+3 all work in Trial Design, Simulation and Conduct without errors.
- [ ] The preview notice is shown.

### Known bugs to fix

Trial Design and Simulation:

- [ ] Plots show the right design names, whichever designs are selected.
- [ ] The cohort size you enter is used by all three designs.
- [ ] The BOIN stopping rule you choose is actually used.
- [ ] The same inputs always give the same results, whichever other designs are selected. The seed is shown.
- [ ] Changing the target updates the BOIN boundaries, and the app explains the problem instead of crashing when the boundaries don't fit the target.
- [ ] Editing the CRM skeleton no longer breaks the CRM simulation. The skeleton is checked before it is used, and it updates when the target changes.
- [ ] "Accuracy" is correct even when every dose is above the target, and "Overdose" is renamed to what it really shows: the average number of DLTs.
- [ ] The BOIN lambda values you enter are used in the simulation.
- [ ] The cohort size is no longer reset when the app starts.
- [ ] Saving and loading the trial design settings works.

Conduct:

- [ ] Conduct uses the same design settings as Simulation.
- [ ] The BOIN plot shows correctly.

Questionnaire:

- [ ] The Reset and Save buttons work without closing the session.

### Reports and downloads

- [ ] You can download the simulation results and the Conduct results as a single HTML report with tables and plots. This doesn't need LaTeX.
- [ ] Each report starts with the design settings used, including the seed and the number of simulations. The questionnaire answers and package versions go at the end.
- [ ] The main tables can also be downloaded as CSV.
- [ ] We drop the PDF, Rmd and Excel downloads unless someone needs them.

### Tests and CI

- [ ] Automated tests check the bugs above, as far as they can be tested outside the browser.
- [ ] A quick test starts the app and runs one simulation for each design.
- [ ] Tests run automatically on every pull request to `dev`.
- [ ] `dev` has one up-to-date `renv.lock`, updated in its own pull request.

### Deployment

- [ ] The app is published on GitHub Pages using Shinylive.
- [ ] The published app loads and runs a simulation for each of CRM, BOIN and 3+3.

### History and documentation

- [ ] Before removing the old `app/` folder, we tag the current `main` so the earlier work stays easy to find, and we credit the people who built it.
- [ ] `app_skeleton/` is renamed in a way that keeps its file history.
- [ ] The README explains what the app does, how to run it and where to find it online.
- [ ] NEWS.md lists what changed in v0.1.0 and says it is a preview.
- [ ] The main decisions for v0.1.0 are written up in `documentation/decisions/`.

## Decisions and assumptions

The main decisions and assumptions behind this release are recorded in `documentation/decisions/`.
