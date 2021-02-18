library(tidyverse)

# Número	Nome	Período	Território
# 3939	Efetivo dos rebanhos, por tipo de rebanho	1974 a 2019	BR, GR, UF, ME, MI, MU
# 73	Efetivo dos rebanhos, por tipo de rebanho (série encerrada)	1974 a 2012	BR, GR, UF, ME, MI, MU
# 95	Ovinos tosquiados	1974 a 2019	BR, GR, UF, ME, MI, MU
# 74	Produção de origem animal, por tipo de produto	1974 a 2019	BR, GR, UF, ME, MI, MU
# 94	Vacas ordenhadas	1974 a 2019	BR, GR, UF, ME, MI, MU
# 3940	Produção da aquicultura, por tipo de produto	2013 a 2019	BR, GR, UF, ME, MI, MU


load_ppm = function(aggregation_level = "municipality",year_begin = 1985 ,year_end = 2019, language = 'pt'){

## Define Basic Parameters

  if (aggregation_level == 'municipality'){geo_reg = 'City'}

  input_df = expand.grid(
    x=c(12,27,13,16,29,23,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17),
    y=as.character(seq(year_begin,year_end))
    )

  input = list(x = as.list(input_df$x),y = as.list(as.character(input_df$y)))

  ## Loading Data

  dat = input %>%
    purrr::pmap(function(x,y) sidrar::get_sidra(74,geo=geo_reg,period = y,geo.filter = list("State" = x))) %>%
    bind_rows() %>%
    janitor::clean_names() %>%
    select(-tidyselect::matches('nivel_territorial'),-ano_codigo,-variavel_codigo,
           -tipo_de_produto_de_origem_animal_codigo,-unidade_de_medida_codigo) %>%
    as_tibble()

  #################################################
  ## Specific To "74	Produção de origem animal" ##
  #################################################

  ## Production in Quantity and its value is stacked together - we undo this to make visualization compatible with styling guides
  ## There is also a very important decision to be made here: Data contain duplicates, which we can choose to either sum, take the mean or drop

  # dat = dat %>% select(-unidade_de_medida) %>% pivot_wider(id_cols = municipio_codigo:ano,
  #                                                      names_from = variavel:tipo_de_produto_de_origem_animal,
  #                                                      values_from=valor,
  #                                                      names_sep = '_x_',
  #                                                      values_fn = length)

  dat$variavel = ifelse(dat$variavel == 'Produção de origem animal','producao','valor')

  # table(dat$tipo_de_produto_de_origem_animal,dat$unidade_de_medida)

  # Valor is always in 1k R$ and production can be in 1k dozens, 1k liters or kgs. We standardize to make comparison possible
  # Dozen is used for eggs, with a mean size of 50 to 54g per unit (https://www.ovoonline.com.br/?:=sobre_ovo&tt=atd&c=8)
  # Liter is used for milk, with 1,032 g/ml https://www.agencia.cnptia.embrapa.br/Agencia8/AG01/arvore/
  #AG01_196_21720039246.html#:~:text=A%20densidade%20do%20leite%20%C3%A9,leite%20desnatado%2C%20cerca%20de%201%2C035

  dat$valor = ifelse(dat$unidade_de_medida == 'Mil dúzias', (((dat$valor/(12*1e3))*52)/1e3),dat$valor) # 1k Dozen to Kg
  dat$valor = ifelse(dat$unidade_de_medida == 'Mil litros',((dat$valor/1e3)*1.032) ,dat$valor) # 1k Liters to Kg

  dat = dat %>% select(-unidade_de_medida) %>%
    pivot_wider(id_cols = municipio_codigo:ano,
                        names_from = variavel:tipo_de_produto_de_origem_animal,
                        values_from=valor,
                        names_sep = '_x_',
                        values_fn = sum,
                        values_fill = 0) %>%
    janitor::clean_names()

  return(dat)


}
