# 0002. Publish the app with Shinylive on GitHub Pages

- Status: accepted
- Date: 2026-10-29
- Release: v0.1.0
- Discussion: https://github.com/xlai/dosefinderhub/pull/88

## Context

The app should be easy for trialists to open without installing R. We also want it to be cheap to host and to update automatically from the repository.

The app uses the `escalation` package for CRM, BOIN and 3+3. `escalation` requires `trialr`, and `trialr` needs Stan, which cannot run in a web browser.

## Options considered

- **A Shiny server** (shinyapps.io, Posit Connect, or an institutional server). Runs any R code, but needs an account, a running server and ongoing upkeep.
- **Shinylive on GitHub Pages.** The app runs in the user's browser through webR. Hosting is free and static, and the site can be built by a GitHub Actions workflow.

## Decision

We publish the app with Shinylive on GitHub Pages, built and deployed from `main` by GitHub Actions.

To let `escalation` load in the browser, the build bundles a small stand-in `trialr` package. It provides the three functions `escalation` imports from `trialr`, and each one stops with a clear message if called. The app only uses the CRM (via `dfcrm`), BOIN and 3+3 methods, which do not use `trialr`.

## Assumptions

- The app does not need any `escalation` method that relies on `trialr`.
- The other packages the app uses are available for webR.
- Simulations run slower in the browser than in native R, but fast enough with sensible defaults for the number of simulations.

## Consequences

- No server to run or pay for.
- Each visitor runs their own copy of R, so one user's settings or random seed cannot affect another.
- The stand-in package must be kept in step with what `escalation` imports, especially when `escalation` is upgraded.
- Features that need software outside R, such as LaTeX for PDF reports, are not available. See 0003.
