# Arquitetura do fluxo

## Separação entre código, dados e decisões

O repositório é a camada de código. A variável `AITHERMO_DATA_ROOT` aponta para a
camada local de dados. Dentro dela, `BUSCAS/` guarda exportações originais,
`dados_humanos/` guarda formulários e decisões, e `pipeline/` recebe somente
artefatos derivados e logs.

```text
código público
├── executar_pipeline.R
├── pipeline/R
├── pipeline/etapas
├── modelos
└── testes

dados locais, fora do repositório
├── BUSCAS
├── dados_humanos
└── pipeline
    ├── processed
    ├── screening
    ├── full_text
    ├── data_extraction
    ├── methodological_appraisal
    ├── clinical_readiness
    ├── tehai
    ├── reporting
    ├── synthesis
    ├── outputs
    └── logs
```

## Etapas com intervenção humana

- A08: conferência dos registros sentinela.
- B10–B10d: duas triagens independentes e decisão conjunta.
- C11–C12b: obtenção, elegibilidade e decisão conjunta em texto completo.
- C13: preparação da extração estruturada.
- C13b: importação da extração concluída, com validação e SHA-256.
- CP14: duas avaliações metodológicas independentes e decisão conjunta.
- B15–B15c: duas avaliações independentes de prontidão e decisão conjunta.
- TEHAI_1: duas avaliações independentes e adjudicação humana aprovada.
- VIS_DADOS: classificação humana aprovada usada nos cruzamentos interpretativos.

Cada transição confere chaves e vocabulários. Nenhuma linha discordante é resolvida
por maioria, posição ou preenchimento automático.

As etapas que dependem de trabalho humano são pontos de retomada. O fluxo gera
ou valida o contrato, interrompe a execução quando a devolutiva ainda não existe e
continua somente depois que a entrada externa correspondente é disponibilizada.

## Figuras

`VIS_DADOS` produz os CSVs de exibição a partir das tabelas finais. As classificações
que exigem interpretação são lidas do contrato externo. `FIG_1` gera o diagrama
PRISMA; `FIG_2_6` gera as Figuras 2–6; `FIG_SUP` gera S1–S8. As exportações são PDF,
PNG e TIFF a 600 dpi, com dados de exibição e reconciliação numérica.
