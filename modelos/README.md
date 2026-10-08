# Contratos de entrada humana

Por segurança, o repositório não inclui arquivos tabulares, nem mesmo modelos
vazios. Os nomes obrigatórios das colunas estão definidos como funções em
`pipeline/R/helpers_input_contracts.R`.

As planilhas e os CSVs preenchidos devem permanecer na raiz externa de dados.
As etapas recusam chaves faltantes, chaves extras, duplicatas e vocabulários fora
do protocolo.
