# Validação da reconstrução

## Controles executados

A reconstrução foi verificada em três camadas, sempre com cópias temporárias
das fontes locais e sem alterar os dados de origem.

1. Os 57 arquivos R foram analisados sintaticamente sem erro.
2. Os 11 testes sintéticos do repositório passaram.
3. A importação e a deduplicação foram executadas com as exportações da
   revisão. As saídas centrais de deduplicação reproduziram os SHA-256 dos
   arquivos históricos correspondentes.
4. A importação dos 12 arquivos independentes de prontidão, a formação da fila
   de 57 discordâncias e a aplicação das decisões conjuntas reproduziram, nas 39
   colunas publicáveis, a base histórica de 50 unidades. Os denominadores finais
   foram 33 estudos, 38 unidades classificáveis, 12 não classificáveis e níveis
   0/1/2/3/4 iguais a 11/24/3/0/0.
5. Os 58 formulários TEHAI independentes reproduziram a fila congelada de 86
   discordâncias. A etapa permanece corretamente em espera quando a tabela de
   decisões humanas aprovadas não é fornecida.
6. O conjunto de figuras foi regenerado com as fontes aprovadas. Quatorze
   invariantes numéricos passaram, incluindo 33 relatos, 50 unidades, 54
   resultados primários, 47 avaliações termográficas aplicáveis, 29 sistemas
   TEHAI, 435 avaliações e seis grupos candidatos à síntese.

## Limite deliberado

O repositório não distribui as fontes protegidas nem os julgamentos preenchidos.
Por isso, o teste público automatizado cobre contratos, sintaxe e invariantes
sintéticos. A reprodução numérica integral requer as exportações obtidas pelas
estratégias publicadas e as camadas humanas aprovadas.
