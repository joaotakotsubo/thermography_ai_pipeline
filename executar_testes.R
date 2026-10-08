#!/usr/bin/env Rscript

# Executa os testes com dados sintéticos em uma pasta temporária.
# Os dados da revisão e os arquivos dos revisores não são usados nesses testes.

raiz <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[[1L]], mustWork = TRUE))
Sys.setenv(AITHERMO_CODE_ROOT = raiz, AITHERMO_DATA_ROOT = file.path(tempdir(), "aithermo_test_data"), AITHERMO_STRICT = "false")
source(file.path(raiz, "testes", "testes_base.R"), chdir = TRUE)
