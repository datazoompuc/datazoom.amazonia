if("BA" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
  municipio == "santa teresinha" & uf == "BA" ~ "santa terezinha",
  municipio == "muquem de sao francisco" & uf == "BA" ~ "muquem do sao francisco",
  TRUE ~ municipio
      )
    )
}

#####################################

if("RR" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "sao luiz do anuaa" & uf == "RR" ~ "sao luiz",
        TRUE ~ municipio
      )
    )
}

##########################

if("PA" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "eldorado dos carajas" & uf == "PA" ~ "eldorado do carajas",
        municipio == "santa isabel do para" & uf == "PA" ~ "santa izabel do para",
        TRUE ~ municipio
      )
    )
}

########################

if("TO" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "couto de magalhaes" & uf == "TO" ~ "couto magalhaes",
        municipio == "fortaleza do tabocao" & uf == "TO" ~ "tabocao",
        municipio == "pau d arco" & uf == "TO" ~ "pau d'arco",
        municipio == "sao valerio da natividade" & uf == "TO" ~ "sao valerio",
        TRUE ~ municipio
      )
    )
}

######################

if("MA" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "governador edson lobao" & uf == "MA" ~ "governador edison lobao",
        TRUE ~ municipio
      )
    )
}

####################
if("RN" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "assu" & uf == "RN" ~ "acu",
        municipio == "presidente juscelino" & uf == "RN" ~ "serra caiada",
        municipio == "olho-d'agua do borges" & uf == "RN" ~ "olho d'agua do borges",
        municipio == "augusto severo" & uf == "RN" ~ "campo grande",
        TRUE ~ municipio
      )
    )
}

#################

if("PB" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "campo de santana" & uf == "PB" ~ "tacima",
        municipio == "santarem" & uf == "PB" ~ "joca claudino",
        municipio == "sao domingos de pombal" & uf == "PB" ~ "sao domingos",
        TRUE ~ municipio
      )
    )
}

##################

if("PE" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "lagoa do itaenga" & uf == "PE" ~ "lagoa de itaenga",
        municipio == "iguaraci" & uf == "PE" ~ "iguaracy",
        municipio == "belem de sao francisco" & uf == "PE" ~ "belem do sao francisco",
        TRUE ~ municipio
      )
    )
}

#################

if("SE" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "amparo de sao francisco" & uf == "SE" ~ "amparo do sao francisco",
        TRUE ~ municipio
      )
    )
}

###############

if("MG" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "brasopolis" & uf == "MG" ~ "brazopolis",
        municipio == "passa-vinte" & uf == "MG" ~ "passa vinte",
        municipio == "sao thome das letras" & uf == "MG" ~ "sao tome das letras",
        municipio == "dona eusebia" & uf == "MG" ~ "dona euzebia",
        municipio == "justinopolis" & uf == "MG" ~ "ribeirao das neves",
        TRUE ~ municipio
      )
    )
}

###############

if("RJ" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "trajano de morais" & uf == "RJ" ~ "trajano de moraes",
        TRUE ~ municipio
      )
    )
}

#############

if("SP" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "embu" & uf == "SP" ~ "embu das artes",
        municipio == "moji mirim" & uf == "SP" ~ "mogi mirim",
        municipio == "florinia" & uf == "SP" ~ "florinea",
        municipio == "biritiba-mirim" & uf == "SP" ~ "biritiba mirim",
        municipio == "sao luis do paraitinga" & uf == "SP" ~ "sao luiz do paraitinga",
        TRUE ~ municipio
      )
    )
}

##############

if("SC" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "grao para" & uf == "SC" ~ "grao-para",
        municipio == "presidente castelo branco" & uf == "SC" ~ "presidente castello branco",
        municipio == "passos de torres" & uf == "SC" ~ "passo de torres",
        TRUE ~ municipio
      )
    )
}

##############

if("RS" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "santana do livramento" & uf == "RS" ~ "sant'ana do livramento",
        TRUE ~ municipio
      )
    )
}

############
if("MT" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "poxoreo" & uf == "MT" ~ "poxoreu",
        TRUE ~ municipio
      )
    )
}

##############
if("GO" %in% param$states){
  dat <- dat %>%
    dplyr::mutate(
      municipio = dplyr::case_when(
        municipio == "colinas de goiais" & uf == "GO" ~ "colinas do sul",
        TRUE ~ municipio
      )
    )
}


