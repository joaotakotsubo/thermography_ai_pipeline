# Governança científica

- Dados brutos e arquivos históricos são somente leitura.
- Revisor 1 e Revisor 2 são entradas independentes e imutáveis.
- Acordos exatos podem ser transportados sem reinterpretação.
- Discordâncias exigem uma terceira camada, nunca alteração dos revisores.
- Chaves faltantes, extras ou duplicadas interrompem a execução.
- Ausência de evidência não é convertida em evidência negativa.
- Vazio, `NA`, `no` e `not_applicable` têm significados distintos.
- A síntese TEHAI não cria escore global, peso, corte, ranking ou radar.
- A verificação de metanálise não interpreta falta de agrupamento como falta de efeito.
- Toda saída derivada recebe origem, data de execução e manifesto quando aplicável.

As contagens específicas do estudo são verificadas somente quando
`AITHERMO_STRICT=true`. Os testes públicos utilizam `false` e dados sintéticos.
