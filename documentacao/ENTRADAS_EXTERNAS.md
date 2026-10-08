# Entradas externas

## Estrutura mínima

```text
AITHERMO_DATA_ROOT/
├── BUSCAS/
├── data_intermediate/inventory/
├── governanca/REGISTRO_APROVACOES.md
└── dados_humanos/
    ├── triagem/revisor_1 e revisor_2
    ├── texto_completo/revisor_1 e revisor_2
    ├── cp13/data_extraction_package_CP13_COMPLETE_33.xlsx
    ├── cp14/aplicabilidade/revisor_1 e revisor_2
    ├── cp14/codificacao/revisor_1 e revisor_2
    ├── cp14/decisoes_conjuntas.csv
    ├── cp15/reviewer_1 e reviewer_2
    ├── cp15/decisoes_conjuntas.csv
    ├── tehai/revisor_1 e revisor_2
    ├── tehai/decisoes_conjuntas.csv
    └── visualizacao/classificacao_unidades_modelo_tarefa.csv
```

As etapas iniciais constroem parte dessa árvore. Os contratos de colunas estão
em `pipeline/R/helpers_input_contracts.R`. Variáveis de ambiente podem substituir
os caminhos padrão:

| Variável | Finalidade |
|---|---|
| `AITHERMO_DATA_ROOT` | raiz externa dos dados |
| `AITHERMO_STRICT` | ativa os invariantes específicos do estudo |
| `AITHERMO_FULL_TEXT_R1` / `AITHERMO_FULL_TEXT_R2` | retornos independentes do texto completo |
| `AITHERMO_FULL_TEXT_CONFORMANCE` | correções operacionais autorizadas |
| `AITHERMO_CP13_COMPLETED_WORKBOOK` | planilha de extração concluída e aprovada |
| `AITHERMO_CP14_APPLICABILITY_R1_DIR` / `AITHERMO_CP14_APPLICABILITY_R2_DIR` | aplicabilidade metodológica independente |
| `AITHERMO_CP14_CODING_R1_DIR` / `AITHERMO_CP14_CODING_R2_DIR` | itens e domínios metodológicos independentes |
| `AITHERMO_CP14_JOINT_DECISIONS` | decisões conjuntas CP14 |
| `AITHERMO_CP15_REVIEWER_1_DIR` / `AITHERMO_CP15_REVIEWER_2_DIR` | formulários independentes de prontidão |
| `AITHERMO_CP15_ADJUDICATION_FILE` | decisões conjuntas de prontidão |
| `AITHERMO_TEHAI_R1_DIR` / `AITHERMO_TEHAI_R2_DIR` | formulários TEHAI independentes |
| `AITHERMO_TEHAI_JOINT_DECISIONS` | adjudicações TEHAI aprovadas |

Os arquivos brutos devem ser baixados diretamente das bases com as estratégias
publicadas. O código não inclui credenciais e não reexecuta buscas remotas.

Os diretórios CP14 devem conter exportações padronizadas e completas das duas
revisões, com as chaves previstas nos modelos. Os arquivos históricos podem ter
sido divididos em lotes operacionais diferentes; antes da reconciliação, cada
linha precisa manter seu identificador estável e o lote científico B01–B32.
