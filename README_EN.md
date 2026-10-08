# thermography_ai_pipeline

The current English landing page and publication instructions are maintained
in [README.md](README.md). Portuguese version:
[README_PT_BR.md](README_PT_BR.md).

Reproducible R workflow for a systematic review of computational models,
infrared thermography, and human pain. This repository contains code only,
including input contracts defined in R, synthetic tests, and technical
documentation.

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

```r
install.packages("renv")
renv::restore()
```

Set the external data directory before running the workflow:

```sh
export AITHERMO_DATA_ROOT="/path/to/local/data"
Rscript --vanilla executar_pipeline.R --listar
Rscript --vanilla executar_pipeline.R --de=P00 --ate=MANIFESTO
```

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
the citation for the archived release.
