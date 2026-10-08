# thermography_ai_pipeline

[![Synthetic tests](https://github.com/joaotakotsubo/thermography_ai_pipeline/actions/workflows/testes.yml/badge.svg)](https://github.com/joaotakotsubo/thermography_ai_pipeline/actions/workflows/testes.yml)

Portuguese version: [README_PT_BR.md](README_PT_BR.md)

Reproducible R workflow for a systematic review of computational models,
infrared thermography, and human pain. This repository contains code only,
including input contracts defined in R, synthetic tests, and technical
documentation.

This is a study-specific workflow, not a trained clinical prediction model or
a clinical decision-support product. Begin with the
[researcher verification guide](RESEARCHER_GUIDE.md) to understand what can be
checked from the public repository and what requires external study inputs.

## Workflow coverage

1. inventory and import of database search exports;
2. normalization and deterministic exact deduplication;
3. separate review queues for approximate matches and preprint–publication pairs;
4. independent screening and documented joint decisions;
5. full-text retrieval and eligibility assessment;
6. preparation, import, and validation of structured data extraction;
7. reconciliation of methodological appraisal;
8. clinical-readiness assessment and external adjudication;
9. TEHAI reconciliation and descriptive synthesis;
10. narrative synthesis and assessment of whether meta-analysis is appropriate;
11. preparation of display data;
12. generation of the PRISMA diagram, main figures, and supplementary figures;
13. SHA-256 manifest and execution-environment record.

Scientific decisions are not computed by the code. The two reviewer forms remain
independent. Exact agreements are carried forward, whereas every disagreement
requires an external joint decision with a stable key, rationale, decision role,
and date. Reviewer files are never overwritten.

## Data not distributed

Bibliographic exports, full-text articles, completed spreadsheets, manuscripts,
and scientific results are not included in this repository. To reproduce the
review, retrieve the records using the search strategies reported in the
supplementary material and organize them according to
[documentacao/ENTRADAS_EXTERNAS.md](documentacao/ENTRADAS_EXTERNAS.md).

## Environment

- R 4.4.3;
- dependencies locked in `renv.lock`;
- UTF-8 encoding;
- UTC execution time zone;
- random seed: `20260626`.

Clone the repository and work from its root directory:

```sh
git clone https://github.com/joaotakotsubo/thermography_ai_pipeline.git
cd thermography_ai_pipeline
Rscript --vanilla executar_testes.R
```

The synthetic tests use base R and do not require the study datasets. For the
analytical stages, restore the locked dependencies from R in this directory:

```r
install.packages("renv")
renv::restore()
```

Because the executor starts its stages with `--vanilla`, explicitly expose the
restored library to its child R processes. In a POSIX shell, after restoring:

```sh
export R_LIBS_USER="$(Rscript --vanilla -e 'cat(renv::paths$library())')"
```

R package versions are pinned, but operating-system libraries, font rendering,
and proprietary source availability are not supplied by the lockfile.

Set the external data directory before running the workflow:

```sh
export AITHERMO_DATA_ROOT="/path/to/local/data"
Rscript --vanilla executar_pipeline.R --listar
Rscript --vanilla executar_pipeline.R --de=P00 --ate=A00
```

Initialization creates the working directories beneath `AITHERMO_DATA_ROOT`.
It does not create or download source records or supply human decisions. The
original-study invariants remain enabled by default; do not disable them to
force an apparently successful reproduction. After checking and approving the
initial checkpoints, proceed through the remaining stages. A request covering
`--de=P00 --ate=MANIFESTO` is not an unattended end-to-end run: it intentionally
stops at missing inputs or unapproved checkpoints.

The workflow stops with `HOLD` whenever an approval, reviewer form, or expected
key is missing. Once the pending input has been resolved, resume from the stage
reported in the log. For example:

```sh
Rscript --vanilla executar_pipeline.R --de=B10 --ate=C13
```

## Tests

```sh
Rscript --vanilla executar_testes.R
```

The tests use synthetic data only. They check syntax, repository privacy,
identifier stability, deduplication rules, and exact matching between joint
decisions and frozen disagreement queues. Full numerical reproduction requires
the records obtained through the published search strategies and the approved
human decision layers.

There are 12 automated controls in this version. GitHub Actions runs them on
pushes and pull requests with R 4.4.3. Passing these controls does not prove
clinical validity or independently reproduce every result of the review.

The reconstruction checks, including comparisons against historical artifacts,
are documented in
[documentacao/VALIDACAO_DA_RECONSTRUCAO.md](documentacao/VALIDACAO_DA_RECONSTRUCAO.md).

## Identifiers

The `REC_PRIMARY_*`, `REC_CTGOV_*`, and `REC_OVERLAP_*` identifiers are created at
stage A06. Numbering occurs after deduplication and sorting by the durable
`record_source_id` key; it therefore does not depend on the original row order.

## License

The code is released under the MIT License. Source data remain subject to the
licenses and restrictions of their respective databases and are not redistributed
through this repository.

## Citation

Complete authorship and version metadata are provided in
[`CITATION.cff`](CITATION.cff). On GitHub, use **Cite this repository** to obtain
the software citation. Version 1.0.0 is the initial release of this public
repository. A Zenodo DOI has not yet been assigned in these metadata; no
identifier is claimed until a deposit is confirmed.

## Reporting issues and proposing changes

Use [GitHub Issues](https://github.com/joaotakotsubo/thermography_ai_pipeline/issues)
for reproducibility questions and see [CONTRIBUTING.md](CONTRIBUTING.md) for
review and contribution requirements. Do not attach licensed exports,
nonpublic full texts, completed reviewer forms, credentials, or personal data.
