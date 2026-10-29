# 0003. Export reports as HTML and tables as CSV

- Status: proposed
- Date: 2026-10-29
- Release: v0.1.0
- Discussion: https://github.com/xlai/dosefinderhub/pull/88

## Context

The Conduct tab offers report downloads as PDF, Rmd and Excel. PDF needs LaTeX, Rmd output has missing plots, and for CRM and BOIN every format currently fails. The Simulation tab has a download button that does nothing. LaTeX and pandoc are not available when the app runs in the browser (see 0002).

What users need are two reports: the operating characteristics from Simulation, and the results from Conduct.

## Options considered

- **Fix PDF, Rmd and Excel.** Keeps the current choices, but PDF cannot work in the browser, and Excel adds a dependency for little gain over CSV.
- **HTML and CSV only.** Works the same locally and in the browser, with no extra software.

## Decision

- Both reports download as a single, self-contained HTML file with tables and plots. Users who need a PDF can print the page from their browser.
- Each report starts with the design settings used, including the seed and the number of simulations. Questionnaire answers and package versions go in appendices at the end.
- The main tables also download as CSV.
- PDF, Rmd and Excel downloads are removed.

## Assumptions

- No user needs an editable Excel file or the Rmd source of a report.

## Consequences

- `rmarkdown`, `knitr`, `openxlsx` and LaTeX are no longer needed for exports, which makes the browser app smaller.
- Putting the settings in every report makes results easier to check and reproduce.
