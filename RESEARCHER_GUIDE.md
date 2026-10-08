# Researcher verification guide

## Scope and limits

This repository publishes the R implementation of a study-specific systematic
review workflow. It is not a dataset release, a pretrained AI model, or a
general-purpose automated systematic-review service. The reviewed topic is
computational models using infrared thermography in human pain and related
clinical tasks. Human eligibility decisions and methodological judgments are
inputs to the workflow, not decisions inferred by the software.

There are two different verification levels:

1. **Public code verification:** inspect the scripts and run the 12 synthetic
   controls without access to protected study materials.
2. **Numerical reproduction of the review:** provide the source records,
   independent reviewer assessments, completed extraction workbook, and
   approved joint decisions. The public repository alone cannot establish
   byte-identical reproduction of every historical output.

## Where to inspect the implementation

| Component | Location |
| --- | --- |
| Executor and ordered stages | `executar_pipeline.R` |
| Configuration and working-directory initialization | `pipeline/config.R`, `pipeline/_bootstrap.R`, and `pipeline/etapas/01_preparacao_importacao/A00_setup.R` |
| Bibliographic parsers and normalization | `pipeline/R/helpers_parsers.R`, `pipeline/R/helpers_text_normalization.R` |
| Exact deduplication and candidate queues | `pipeline/etapas/02_deduplicacao/A06_deduplicate.R`, `pipeline/R/helpers_dedup.R` |
| Reviewer disagreement and joint-decision controls | `pipeline/R/helpers_reviewer_reconciliation.R`, `pipeline/R/helpers_checkpoint.R` |
| Required human-input columns | `pipeline/R/helpers_input_contracts.R` |
| Methodological appraisal and clinical readiness | `pipeline/etapas/06_avaliacao_metodologica/`, `pipeline/etapas/07_prontidao_clinica/` |
| TEHAI, descriptive synthesis, and reporting checks | `pipeline/etapas/08_tehai/`, `pipeline/etapas/09_sintese/` |
| PRISMA and other figures | `pipeline/etapas/10_figuras/` |
| Output hashes and environment record | `pipeline/etapas/11_integridade/11_manifesto_reproducibilidade.R` |

## Public verification procedure

From the repository root, using R 4.4.3:

```sh
Rscript --vanilla executar_testes.R
```

Expected final message: `Todos os 12 testes passaram.` The tests check syntax,
stable identifiers, invalid-key rejection, DOI normalization, title similarity,
reviewer alignment, exact joint-decision matching, controlled missingness
states, exclusion of protected file formats and personal paths, the TEHAI
decision contract, and absence of tabular data in the code release.

Review the actual test assertions in `testes/testes_base.R`; do not infer test
coverage merely from a successful badge. The GitHub workflow runs these tests
on R 4.4.3. It does not execute the complete review against protected data.

## Running with external study inputs

Follow the environment restoration instructions in [README.md](README.md).
Set `AITHERMO_DATA_ROOT` to a separate local directory, not to the code tree.
The initialization creates eight working directories: `pipeline`,
`pipeline/processed`, `pipeline/screening`, `pipeline/screening/reviewer_inputs`,
`pipeline/outputs`, `pipeline/snowballingo`, `pipeline/logs`, and
`pipeline/checkpoints`. Other output parent directories are created as needed.

Input paths and environment overrides are documented in
[documentacao/ENTRADAS_EXTERNAS.md](documentacao/ENTRADAS_EXTERNAS.md). That
technical document and the code comments are in Portuguese; column names and
controlled values must be used exactly as implemented. Do not translate them
inside input files. Code, contracts, and tests are the operative specification.

`AITHERMO_STRICT` defaults to true. Some checkpoints retain historical counts
and denominators specific to the original review, including the expected
search inventory. An unrelated dataset is not a valid drop-in replacement.
Changing these assumptions requires a documented adaptation and independent
validation, not silently bypassing checks.

The executor stops when approvals or valid inputs are absent. Preserve the two
reviewers' independent files and supply documented joint decisions for every
disagreement. Do not fabricate approvals or adjudications to make stages pass.
The reconstruction validation note documents historical checks and their
limits; those historical checks are distinct from the public synthetic tests.

## Interpreting and reporting a reproduction

Record the release tag, commit SHA, R version, operating system, dependency
versions, stage interval, input provenance and hashes, and whether the human
decision layers match the original study. Distinguish reproduced outputs from
modified or re-adjudicated analyses. A SHA-256 match establishes file identity,
not scientific validity. Figure pixels can depend on fonts and system libraries.

Report discrepancies through GitHub Issues with a minimal synthetic example.
Do not upload licensed exports, nonpublic articles, completed reviewer forms,
credentials, participant information, or restricted research data. Cite this
software using `CITATION.cff`; a Zenodo identifier will be added only after a
deposit has actually been confirmed.
