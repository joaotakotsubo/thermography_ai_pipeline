# Contributing and reporting reproducibility issues

Please open a GitHub Issue before proposing a change that affects scientific
logic, deduplication survivors, eligibility decisions, denominators, or output
interpretation. Use pull requests for proposed code or documentation changes;
do not rewrite historical study decisions inside an implementation patch.

Include the release tag or commit SHA, R version, operating system, stage name,
expected and observed behavior, and a minimal synthetic example. Where relevant,
state whether dependencies were restored from `renv.lock` and whether the run
used the original approved human input layers or a new dataset.

Before submitting a pull request, run:

```sh
Rscript --vanilla executar_testes.R
```

Add a regression test for changed behavior. Keep identifiers stable and retain
explicit distinctions between missing, negative, uncertain, and not-applicable
values. Do not substitute automated judgments for required joint decisions.
Explain any changes to input contracts, checkpoint invariants, or dependencies.

Never attach database exports with redistribution restrictions, nonpublic full
texts, completed reviewer spreadsheets, manuscripts, participant data, secrets,
or credentials. Use synthetic records for reports and tests. Contributions are
submitted under the repository's MIT License; that license does not grant rights
to redistribute third-party study materials.
