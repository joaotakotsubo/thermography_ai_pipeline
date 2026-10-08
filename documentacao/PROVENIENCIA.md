# Proveniência e rastreabilidade

Cada artefato deriva de entradas explícitas. Os scripts usam chaves estáveis para
junções e escrevem CSVs em ordem determinística. Arquivos relevantes recebem
SHA-256; `MANIFESTO` registra o conjunto final e `sessionInfo()` documenta o ambiente.

Os principais vínculos são:

| Camada | Entrada | Saída |
|---|---|---|
| importação | exportações e manifesto | registros brutos padronizados |
| deduplicação | registros normalizados | sobreviventes, auditoria e filas candidatas |
| triagem | dois formulários | acordos, discordâncias e fila de texto completo |
| elegibilidade | dois formulários e decisões | estudos incluídos e exclusões justificadas |
| extração | estudos incluídos e planilha humana concluída | cópia validada com SHA-256 preservado |
| método | dois conjuntos e decisões | itens e domínios finais |
| prontidão | dois conjuntos e decisões | unidades e estágio final |
| TEHAI | dois conjuntos e adjudicações | 435 avaliações aprovadas e distribuições |
| síntese | camadas finais | tabelas narrativas e verificação de agrupamento |
| figuras | tabelas finais e classificação aprovada | dados de exibição e imagens |

O manifesto prova identidade de arquivos; ele não substitui a documentação das
decisões humanas nem autoriza reinterpretação científica.
