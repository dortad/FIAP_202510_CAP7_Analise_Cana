#Durval_RM567007_fase2_cap7_V1.R

# GRUPO COM OS SEGUINTES ALUNOS:
# Durval de Oliveira Dorta Junior - RM567007
# Murilo Ferreira Borges - RM567738


library(tibble)
library(DescTools)
library(ggplot2)
library(gridExtra)


# ==========================================================
# Atividade 3 – Decolando com Ciências de Dados (R)
# ==========================================================
#
# objetivo: Explorar dados reais do agronegócio aplicando estatística descritiva e análise em R.
# • Criar uma base de dados (mínimo 30 linhas × 4 colunas).
# • Analisar variáveis quantitativas e qualitativas no R (média, desvio, quartis, gráficos).
# • Utilizar dados de fontes como CONAB, IBGE, MAPA, EMBRAPA, INPE ou CNA.ma empresa deseja analisar como o tempo de estudo e a quantidade de horas de sono

#
# Com base nessas instruções foi criada uma base de dados fictícios de experimentos com mudas de cana-de-açúcar, com ajuda do ChatGPt, que pesquisou as fontes
#       O objetivo foi construir uma base calculando a produtividade para diferentes variedades, épocas do ano e processos de plantio, ao final entender como o processo influencia na produtividade.
#       A base contém 56 linhas e 22 colunas, o resultado da produtividade é o TCH_estimado_t_ha (Toneladas de Cana por Hectare).

# observando que são Variáveis to tipo :
# quantitativas: Area_efetiva_ha, Area_ha, E, G_final, L_to, Massa de muda (t/ha), Perc_Manobra_%, Perc_Trafego_%, TCH_estimado_t_ha, d_equiv_kg_m, d_rec_kg_m, g_to, gemas_plantadas_m, m_linhas_por_ha, massa_total_area_t, rho, s, toletes_por_ha, toletes_por_m
# qualitativas: Epoca, Processo, Variedade
# ----------------------------------------------------------
# Significado destas variaveis (colunas) na tabela abaixo:
# | Nome da coluna       | Termo                            | Unidade        | Definição prática                                                      | Observações                                                 |
# | -------------------- | -------------------------------- | -------------- | ---------------------------------------------------------------------- | ----------------------------------------------------------- |
# | Area_ha              | Área (A)                         | ha             | Área total da linha de cálculo.                                        | Usada para dimensionar a área efetiva após perdas.          |
# | Perc_Manobra_%       | Perdas por manobra/bordadura (B) | %              | Percentual de área não efetiva devido a bordaduras, curvas e manobras. | Configurar por processo/talhão conforme realidade local.    |
# | Perc_Trafego_%       | Perdas por tráfego (C)           | %              | Percentual de área não efetiva devido ao tráfego/compactação.          | Depende de umidade do solo, pneus, CTF e logística.         |
# | Variedade            | Variedade (D)                    | —              | Cultivar da cana-de-açúcar (RB…, CTC…).                                | Define parâmetros via lookup na aba Parametros.             |
# | Epoca                | Época (E)                        | —              | Condição sazonal: Chuva ou Seca.                                       | Controla G_final, tolerância de desvio e TCH de referência. |
# | Processo             | Processo (F)                     | —              | Tipo de operação: Manual ou Mecanizado.                                | Afeta a viabilidade operacional (fator por processo).       |
# | E                    | Espaçamento entre linhas (E)     | m              | Distância entre linhas de plantio.                                     | Define os metros de linha por hectare: 10.000/E.            |
# | G_final              | Meta de gemas viáveis (G_final)  | gemas/m        | Número-alvo de gemas viáveis por metro de linha.                       | Aumenta na seca; base da dose de muda.                      |
# | s                    | Gemas por tolete (s)             | gemas/tolete   | Gemas totais por tolete.                                               | Junto com Q, valida a deposição total de gemas.             |
# | g_to                 | Gemas viáveis por tolete (g_to)  | viáveis/tolete | Número de gemas viáveis por tolete.                                    | Usado para calcular Q (toletes/m) e a dose-alvo.            |
# | L_to                 | Comprimento do tolete (L_to)     | m              | Comprimento de cada tolete.                                            | Normalmente padronizado (ex.: 0,40 m).                      |
# | rho                  | Densidade linear do colmo (ρ)    | kg/m           | Massa por metro de colmo.                                              | Pode variar por variedade/classe de colmo.                  |
# | d_rec_kg_m           | Dose alvo por metro (O)          | kg/m           | Dose técnica recomendada por metro.                                    | Cálculo: (G_final/g_to) × L_to × ρ.                         |
# | m_linhas_por_ha      | Metros de linha por hectare (P)  | m/ha           | Quantidade de metros de linha em 1 ha.                                 | Cálculo: 10.000 / E.                                        |
# | toletes_por_m        | Toletes por metro (Q)            | toletes/m      | Quantidade de toletes depositados por metro.                           | Cálculo: G_final / g_to.                                    |
# | gemas_plantadas_m    | Gemas plantadas por metro (R)    | gemas/m        | Total de gemas depositadas por metro.                                  | Cálculo: Q × s.                                             |
# | toletes_por_ha       | Toletes por hectare (S)          | toletes/ha     | Total de toletes depositados por hectare.                              | Cálculo: Q × P.                                             |
# | Massa de muda (t/ha) | Massa de muda (T)                | t/ha           | Toneladas de muda utilizadas por hectare (não produtividade).          | Cálculo: (S × L_to × ρ) / 1000.                             |
# | d_equiv_kg_m         | Dose praticada por metro (U)     | kg/m           | Dose efetivamente praticada por metro.                                 | Em v13: usa fator de processo na viabilidade (opção 2).     |
# | Area_efetiva_ha      | Área efetiva (V)                 | ha             | Área útil após perdas de manobra e tráfego.                            | Cálculo: A × (1 − B% − C%).                                 |
# | massa_total_area_t   | Massa total na área (W)          | t              | Toneladas totais de muda para a área efetiva.                          | Cálculo: T × V.                                             |
# | TCH_estimado_t_ha    | TCH estimado                     | t/ha           | Produtividade estimada de colheita.                                    | Cálculo típico: TCH_ref × (1 − k × desvio).                 |


# A Seguir a base em formato Tibble e a analise das variáveis quantitativas e qualitativas no R (média, desvio, quartis, gráficos).

dados <- tibble::tribble(
  ~`Area_ha`, ~`Perc_Manobra_%`, ~`Perc_Trafego_%`, ~`Variedade`, ~`Epoca`, ~`Processo`, ~`E`, ~`G_final`, ~`s`, ~`g_to`, ~`L_to`, ~`rho`, ~`d_rec_kg_m`, ~`m_linhas_por_ha`, ~`toletes_por_m`, ~`gemas_plantadas_m`, ~`toletes_por_ha`, ~`Massa de muda (t/ha)`, ~`d_equiv_kg_m`, ~`Area_efetiva_ha`, ~`massa_total_area_t`, ~`TCH_estimado_t_ha`,
  10, 0.03, 0.02, "RB867515", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.995000000000001, 127.93600000000004, 80,
  10, 0.03, 0.02, "RB867515", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.995000000000001, 170.58133333333336, 70,
  10, 0.03, 0.02, "RB92579", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.995000000000001, 127.93600000000004, 80,
  10, 0.03, 0.02, "RB92579", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.995000000000001, 170.58133333333336, 70,
  10, 0.03, 0.02, "SP80-3280", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.12, 1.7920000000000003, 6666.666666666667, 4, 2.48, 26666.666666666668, 11.946666666666669, 1.7920000000000003, 9.995000000000001, 119.40693333333337, 80,
  10, 0.03, 0.02, "SP80-3280", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.12, 2.3893333333333335, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 15.92888888888889, 2.3893333333333335, 9.995000000000001, 159.20924444444447, 70,
  10, 0.03, 0.02, "RB966928", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.28, 2.048, 6666.666666666667, 4, 2.48, 26666.666666666668, 13.653333333333336, 2.048, 9.995000000000001, 136.4650666666667, 80,
  10, 0.03, 0.02, "RB966928", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.28, 2.7306666666666666, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 18.204444444444444, 2.7306666666666666, 9.995000000000001, 181.95342222222223, 70,
  10, 0.03, 0.02, "RB965902", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.995000000000001, 127.93600000000004, 80,
  10, 0.03, 0.02, "RB965902", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.995000000000001, 170.58133333333336, 70,
  10, 0.03, 0.02, "CTC4", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.28, 2.048, 6666.666666666667, 4, 2.48, 26666.666666666668, 13.653333333333336, 2.048, 9.995000000000001, 136.4650666666667, 80,
  10, 0.03, 0.02, "CTC4", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.28, 2.7306666666666666, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 18.204444444444444, 2.7306666666666666, 9.995000000000001, 181.95342222222223, 70,
  10, 0.03, 0.02, "CTC20", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.995000000000001, 127.93600000000004, 80,
  10, 0.03, 0.02, "CTC20", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.995000000000001, 170.58133333333336, 70,
  10, 0.05, 0.05, "RB867515", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.990000000000002, 137.00571428571433, 72.47311827956992,
  10, 0.05, 0.05, "RB867515", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.990000000000002, 182.67428571428573, 63.41397849462367,
  10, 0.05, 0.05, "RB92579", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.990000000000002, 137.00571428571433, 72.47311827956992,
  10, 0.05, 0.05, "RB92579", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.990000000000002, 182.67428571428573, 63.41397849462367,
  10, 0.05, 0.05, "SP80-3280", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.12, 1.9200000000000004, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 12.800000000000002, 2.0645161290322585, 9.990000000000002, 127.87200000000006, 72.47311827956989,
  10, 0.05, 0.05, "SP80-3280", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.12, 2.5600000000000005, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 17.06666666666667, 2.7526881720430114, 9.990000000000002, 170.49600000000007, 63.413978494623656,
  10, 0.05, 0.05, "RB966928", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.28, 2.1942857142857144, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 14.62857142857143, 2.359447004608295, 9.990000000000002, 146.13942857142862, 72.4731182795699,
  10, 0.05, 0.05, "RB966928", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.28, 2.925714285714286, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 19.504761904761907, 3.145929339477727, 9.990000000000002, 194.85257142857148, 63.413978494623656,
  10, 0.05, 0.05, "RB965902", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.990000000000002, 137.00571428571433, 72.47311827956992,
  10, 0.05, 0.05, "RB965902", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.990000000000002, 182.67428571428573, 63.41397849462367,
  10, 0.05, 0.05, "CTC4", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.28, 2.1942857142857144, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 14.62857142857143, 2.359447004608295, 9.990000000000002, 146.13942857142862, 72.4731182795699,
  10, 0.05, 0.05, "CTC4", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.28, 2.925714285714286, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 19.504761904761907, 3.145929339477727, 9.990000000000002, 194.85257142857148, 63.413978494623656,
  10, 0.05, 0.05, "CTC20", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.990000000000002, 137.00571428571433, 72.47311827956992,
  10, 0.05, 0.05, "CTC20", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.990000000000002, 182.67428571428573, 63.41397849462367,
  10, 0.0256, 0.0156, "RB867515", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.99588, 127.94726400000002, 80,
  10, 0.0256, 0.0156, "RB867515", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.99588, 170.596352, 70,
  10, 0.0256, 0.0156, "RB92579", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.99588, 127.94726400000002, 80,
  10, 0.0256, 0.0156, "RB92579", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.99588, 170.596352, 70,
  10, 0.0256, 0.0156, "SP80-3280", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.12, 1.7920000000000003, 6666.666666666667, 4, 2.48, 26666.666666666668, 11.946666666666669, 1.7920000000000003, 9.99588, 119.41744640000002, 80,
  10, 0.0256, 0.0156, "SP80-3280", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.12, 2.3893333333333335, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 15.92888888888889, 2.3893333333333335, 9.99588, 159.22326186666666, 70,
  10, 0.0256, 0.0156, "RB966928", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.28, 2.048, 6666.666666666667, 4, 2.48, 26666.666666666668, 13.653333333333336, 2.048, 9.99588, 136.47708160000002, 80,
  10, 0.0256, 0.0156, "RB966928", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.28, 2.7306666666666666, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 18.204444444444444, 2.7306666666666666, 9.99588, 181.96944213333333, 70,
  10, 0.0256, 0.0156, "RB965902", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.99588, 127.94726400000002, 80,
  10, 0.0256, 0.0156, "RB965902", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.99588, 170.596352, 70,
  10, 0.0256, 0.0156, "CTC4", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.28, 2.048, 6666.666666666667, 4, 2.48, 26666.666666666668, 13.653333333333336, 2.048, 9.99588, 136.47708160000002, 80,
  10, 0.0256, 0.0156, "CTC4", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.28, 2.7306666666666666, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 18.204444444444444, 2.7306666666666666, 9.99588, 181.96944213333333, 70,
  10, 0.0256, 0.0156, "CTC20", "Chuva", "Manual", 1.5, 12, 0.62, 3, 0.4, 1.2, 1.92, 6666.666666666667, 4, 2.48, 26666.666666666668, 12.800000000000002, 1.92, 9.99588, 127.94726400000002, 80,
  10, 0.0256, 0.0156, "CTC20", "Seca", "Manual", 1.5, 16, 0.58, 3, 0.4, 1.2, 2.56, 6666.666666666667, 5.333333333333333, 3.093333333333333, 35555.555555555555, 17.066666666666666, 2.56, 9.99588, 170.596352, 70,
  10, 0.041, 0.041, "RB867515", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.9918, 137.03040000000001, 72.47311827956992,
  10, 0.041, 0.041, "RB867515", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.9918, 182.70719999999997, 63.41397849462367,
  10, 0.041, 0.041, "RB92579", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.9918, 137.03040000000001, 72.47311827956992,
  10, 0.041, 0.041, "RB92579", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.9918, 182.70719999999997, 63.41397849462367,
  10, 0.041, 0.041, "SP80-3280", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.12, 1.9200000000000004, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 12.800000000000002, 2.0645161290322585, 9.9918, 127.89504000000002, 72.47311827956989,
  10, 0.041, 0.041, "SP80-3280", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.12, 2.5600000000000005, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 17.06666666666667, 2.7526881720430114, 9.9918, 170.52672, 63.413978494623656,
  10, 0.041, 0.041, "RB966928", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.28, 2.1942857142857144, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 14.62857142857143, 2.359447004608295, 9.9918, 146.16576, 72.4731182795699,
  10, 0.041, 0.041, "RB966928", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.28, 2.925714285714286, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 19.504761904761907, 3.145929339477727, 9.9918, 194.88768000000002, 63.413978494623656,
  10, 0.041, 0.041, "RB965902", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.9918, 137.03040000000001, 72.47311827956992,
  10, 0.041, 0.041, "RB965902", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.9918, 182.70719999999997, 63.41397849462367,
  10, 0.041, 0.041, "CTC4", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.28, 2.1942857142857144, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 14.62857142857143, 2.359447004608295, 9.9918, 146.16576, 72.4731182795699,
  10, 0.041, 0.041, "CTC4", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.28, 2.925714285714286, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 19.504761904761907, 3.145929339477727, 9.9918, 194.88768000000002, 63.413978494623656,
  10, 0.041, 0.041, "CTC20", "Chuva", "Mecanizado", 1.5, 12, 0.62, 2.8, 0.4, 1.2, 2.0571428571428574, 6666.666666666667, 4.285714285714286, 2.657142857142857, 28571.428571428572, 13.714285714285715, 2.2119815668202762, 9.9918, 137.03040000000001, 72.47311827956992,
  10, 0.041, 0.041, "CTC20", "Seca", "Mecanizado", 1.5, 16, 0.58, 2.8, 0.4, 1.2, 2.7428571428571433, 6666.666666666667, 5.714285714285714, 3.314285714285714, 38095.2380952381, 18.285714285714285, 2.9493087557603688, 9.9918, 182.70719999999997, 63.41397849462367
)

# Preview
#print(dados)
#str(dados)

# Iniciando o estudo comparando os dados
## • Analisar variáveis quantitativas e qualitativas no R (média, desvio, quartis, gráficos).

# Análise descritiva
#summary(dados)


# =======================================================================================================
# Análise da Produtividade (TCH_estimado_t_ha) pela Variável Categórica Processo
# =======================================================================================================
# Objetivo: Comparar a produtividade entre os tipos de processo (Manual vs Mecanizado)


# ----------------------------------------------------------
# ANÁLISE POR PROCESSO
# ----------------------------------------------------------
print("### ANÁLISE DA PRODUTIVIDADE POR PROCESSO ###")

# Separando dados por Processo para usar nas analsies
dados_manual <- dados$TCH_estimado_t_ha[dados$Processo == "Manual"]
dados_mecanizado <- dados$TCH_estimado_t_ha[dados$Processo == "Mecanizado"]

# Estatísticas descritivas por Processo
print("############ Tendencia Central ############")

print("=================Média por Processo:")
print(paste("Manual:", mean(dados_manual)))
print(paste("Mecanizado:", mean(dados_mecanizado)))

print("=================Mediana por Processo:")
print(paste("Manual:", median(dados_manual)))
print(paste("Mecanizado:", median(dados_mecanizado)))

# Cálculo da Moda por Processo
print("=================Moda por Processo:")
# Para Processo Manual
moda_manual <- Mode(dados_manual)[1]
print(paste("Manual:", moda_manual))

# Para Processo Mecanizado
moda_mecanizado <- Mode(dados_mecanizado)[1]
print(paste("Mecanizado:", moda_mecanizado))

print(" ")
print(" ")
print(" ")


print("############ Dispersão ############")
print("=================Valor Mínimo por Processo:")
print(paste("Manual:", min(dados_manual)))
print(paste("Mecanizado:", min(dados_mecanizado)))

print("=================Valor Máximo por Processo:")
print(paste("Manual:", max(dados_manual)))
print(paste("Mecanizado:", max(dados_mecanizado)))

print("=================Amplitude por Processo:")
print(paste("Manual:", diff(range(dados_manual))))
print(paste("Mecanizado:", diff(range(dados_mecanizado))))

print("=================Variância por Processo:")
print(paste("Manual:", var(dados_manual)))
print(paste("Mecanizado:", var(dados_mecanizado)))

print("=================Desvio Padrão por Processo:")
print(paste("Manual:", sd(dados_manual)))
print(paste("Mecanizado:", sd(dados_mecanizado)))

print("=================Coeficiente de Variação por Processo (%):")
cv_manual <- (sd(dados_manual) / mean(dados_manual)) * 100
print(paste("Manual:", cv_manual, "%"))
cv_mecanizado <- (sd(dados_mecanizado) / mean(dados_mecanizado)) * 100
print(paste("Mecanizado:", cv_mecanizado, "%"))

print(" ")
print(" ")
print(" ")

print("############ Separatrizes ############")

print("=================Quartis por Processo:")
print("Manual:")
quartis_manual <- quantile(dados_manual, probs = c(0.25, 0.50, 0.75))

print(paste("Q1 (25%):", quartis_manual[1]))
print(paste("Q2 (50% - Mediana):", quartis_manual[2]))
print(paste("Q3 (75%):", quartis_manual[3]))

print("=================")

print("Mecanizado:")
quartis_mecanizado <- quantile(dados_mecanizado, probs = c(0.25, 0.50, 0.75))

print(paste("Q1 (25%):", quartis_mecanizado[1]))
print(paste("Q2 (50% - Mediana):", quartis_mecanizado[2]))
print(paste("Q3 (75%):", quartis_mecanizado[3]))

print("=============== Decis por Processo:")
print("Manual:")
decis_manual <- quantile(dados_manual, probs = seq(0.1, 0.9, by = 0.1))
print(decis_manual)
print("Mecanizado:")
decis_mecanizado <- quantile(dados_mecanizado, probs = seq(0.1, 0.9, by = 0.1))
print(decis_mecanizado)


print("=============== Centis por Processo:")
print("Manual:")
centis_manual <- quantile(dados_manual, probs = seq(0.01, 0.99, by = 0.01)) 
print(centis_manual)
print("Mecanizado:")
centis_mecanizado <- quantile(dados_mecanizado, probs = seq(0.01, 0.99, by = 0.01))
print(centis_mecanizado)

# Resumo
resumo <- summary(dados$TCH_estimado_t_ha)
print(" ")
print(" ")
print(" ")
print("############ Resumo Estatístico Geral ############")
print(resumo)

print(" ")
print(" ")
print(" ")




print(" ")
print(" ")
print(" ")



# ----------------------------------------------------------
# GRÁFICOS COM GGPLOT2
# ----------------------------------------------------------

library(gridExtra)  # Para combinar múltiplos gráficos ggplot2

# Preparar dados para média por processo
medias_df <- data.frame(
  Processo = c("Manual", "Mecanizado"),
  Media = c(mean(dados_manual), mean(dados_mecanizado))
)

# Boxplot comparando produtividade por Processo
p1 <- ggplot(dados, aes(x = Processo, y = TCH_estimado_t_ha, fill = Processo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Manual" = "lightgreen", "Mecanizado" = "lightyellow")) +
  labs(    title = "Produtividade por Processo",    x = "Processo",    y = "Produtividade (t/ha)"  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Histograma de produtividade por Processo
p2 <- ggplot(dados, aes(x = TCH_estimado_t_ha, fill = Processo)) +
  geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Manual" = "lightgreen", "Mecanizado" = "lightyellow")) +
  labs(
    title = "Distribuição da Produtividade por Processo",
    x = "Produtividade (t/ha)",
    y = "Frequência"
  ) +
  theme_minimal()


# Gráfico de pontos com média
p3 <- ggplot(dados, aes(x = Processo, y = TCH_estimado_t_ha, color = Processo)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "red", shape = 18) +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -1, color = "red", size = 4) +
  scale_color_manual(values = c("Manual" = "darkgreen", "Mecanizado" = "darkgoldenrod")) +
  labs(
    title = "Produtividade Individual por Processo",
    subtitle = "Losango vermelho indica a média",
    x = "Processo",
    y = "Produtividade (t/ha)"
  ) +
  theme_minimal()
# Gráfico de Distribuição Normal por Processo
p4 <- ggplot(dados, aes(x = TCH_estimado_t_ha, color = Processo)) +
  geom_density(size = 1.2) +
  # Adicionar curva normal teórica para Manual
  stat_function(
    fun = dnorm,
    args = list(mean = mean(dados_manual), sd = sd(dados_manual)),
    linetype = "dashed",
    color = "darkgreen",
    size = 0.8
  ) +
  # Adicionar curva normal teórica para Mecanizado
  stat_function(
    fun = dnorm,
    args = list(mean = mean(dados_mecanizado), sd = sd(dados_mecanizado)),
    linetype = "dashed",
    color = "darkgoldenrod",
    size = 0.8
  ) +
  scale_color_manual(values = c("Manual" = "lightgreen", "Mecanizado" = "lightyellow")) +
  labs(
    title = "Distribuição Normal da Produtividade",
    subtitle = "Linha sólida: densidade observada | Linha tracejada: curva normal teórica",
    x = "Produtividade (t/ha)",
    y = "Densidade"
  ) +
  theme_minimal()

# Exibir os gráficos
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

