---
title: "Dose-Finding Methods"
output: html_document
---

DoseFinderHub compares three dose-escalation methods for Phase I clinical trials. Each method has distinct advantages and is suited to different trial contexts.

### 3+3 Design

Traditional rule-based method that treats patients in cohorts of 3-6. Escalates dose when 0/3 patients experience dose-limiting toxicity (DLT), expands to 6 patients when 1/3 experience DLT, and stops escalation when ≥2 patients experience DLT.

**Advantages:**
- Simple to understand and implement
- Widely accepted by regulators
- No statistical modeling required

**Disadvantages:**
- Statistically inefficient
- Often selects suboptimal doses
- Conservative escalation

### Continual Reassessment Method (CRM)

Model-based approach that uses statistical modeling to estimate DLT probability at each dose level. Continuously updates estimates as data accrue and selects the dose closest to a pre-specified target toxicity rate.

**Advantages:**
- More accurate MTD estimation
- Efficient use of patient data
- Can incorporate prior information
- Faster dose escalation

**Disadvantages:**
- Requires statistical expertise
- Less familiar to many clinicians
- More complex implementation

### Bayesian Optimal Interval (BOIN)

Model-assisted design that combines rule-based simplicity with improved statistical performance. Uses pre-calculated escalation and de-escalation boundaries around a target toxicity rate.

**Advantages:**
- Simple implementation like 3+3
- Better accuracy than 3+3
- No prior model specification needed
- Transparent decision rules

**Disadvantages:**
- Less efficient than CRM
- Newer method with less regulatory familiarity

### Key References

*This section will include important literature references for each method to support further reading and protocol development.*

