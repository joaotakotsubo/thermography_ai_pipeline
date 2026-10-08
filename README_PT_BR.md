# thermography_ai_pipeline

Apresentação principal e instruções atualizadas de publicação:
[README.md](README.md). Guia de verificação para pesquisadores:
[RESEARCHER_GUIDE.md](RESEARCHER_GUIDE.md).

English version: [README_EN.md](README_EN.md)

Fluxo reprodutível em R para a revisão sistemática sobre modelos computacionais,
termografia infravermelha e dor humana. O repositório contém exclusivamente
código, contratos de entrada definidos em R, testes sintéticos e documentação técnica.

## O que o fluxo cobre

1. inventário e importação das exportações de busca;
2. normalização e deduplicação exata determinística;
3. filas separadas para pares aproximados e preprint–publicação;
4. triagem independente e decisão conjunta documentada;
5. recuperação e elegibilidade em texto completo;
6. preparação, importação e validação da extração estruturada;
7. reconciliação da avaliação metodológica;
8. prontidão clínica e adjudicação externa;
9. reconciliação e síntese descritiva TEHAI;
10. síntese narrativa e verificação da possibilidade de metanálise;
11. preparação dos dados de exibição;
12. PRISMA, figuras principais e figuras suplementares;
13. manifesto SHA-256 e registro do ambiente.

As decisões científicas não são calculadas pelo código. Os dois formulários de
revisão permanecem independentes; concordâncias exatas são transportadas e cada
discordância exige uma decisão conjunta externa com chave, justificativa, papel
e data. Arquivos dos revisores nunca são sobrescritos.

## Dados não distribuídos

Exportações bibliográficas, PDFs, planilhas preenchidas, manuscritos e resultados
científicos não fazem parte deste repositório. Para reproduzir a revisão, obtenha
os registros com as estratégias publicadas no material suplementar e organize-os
na árvore descrita em [documentacao/ENTRADAS_EXTERNAS.md](documentacao/ENTRADAS_EXTERNAS.md).

## Ambiente

- R 4.4.3;
- dependências fixadas em `renv.lock`;
- codificação UTF-8;
- fuso das execuções: UTC;
- semente: `20260626`.

```r
install.packages("renv")
renv::restore()
```

Defina a pasta externa de dados antes da execução:

```sh
export AITHERMO_DATA_ROOT="/caminho/para/os/dados_locais"
Rscript --vanilla executar_pipeline.R --listar
Rscript --vanilla executar_pipeline.R --de=P00 --ate=MANIFESTO
```

O fluxo interrompe com `HOLD` sempre que falta uma aprovação, um formulário ou
uma chave esperada. Depois de resolver a pendência, retome pela etapa indicada no
log, por exemplo:

```sh
Rscript --vanilla executar_pipeline.R --de=B10 --ate=C13
```

## Testes

```sh
Rscript --vanilla executar_testes.R
```

Os testes usam apenas dados sintéticos e verificam sintaxe, privacidade da árvore,
estabilidade dos identificadores, regras de deduplicação e correspondência exata
das decisões conjuntas. A reprodução numérica integral requer os dados obtidos
pelas estratégias de busca e as decisões humanas aprovadas.

Os ensaios executados durante a reconstrução, inclusive as comparações com os
artefatos históricos, estão descritos em
[documentacao/VALIDACAO_DA_RECONSTRUCAO.md](documentacao/VALIDACAO_DA_RECONSTRUCAO.md).

## Identificadores

Os identificadores `REC_PRIMARY_*`, `REC_CTGOV_*` e `REC_OVERLAP_*` são criados
na etapa A06. A numeração é aplicada depois da deduplicação e da ordenação pela
chave durável `record_source_id`; portanto, não depende da posição original das
linhas no arquivo.

## Licença

Código distribuído sob a licença MIT. Os dados de origem obedecem às licenças e
restrições de suas respectivas bases e não são redistribuídos aqui.

## Citação

Os metadados completos de autoria e versão estão disponíveis em
[`CITATION.cff`](CITATION.cff). No GitHub, use a opção **Cite this repository**
para obter a citação da versão arquivada.
