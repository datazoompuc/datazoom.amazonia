library(dplyr)
library(lubridate)

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/DNAC2023.dbc"
temp <- tempfile()
download.file(url, temp, method = 'curl')
teste <- datazoom.amazonia:::read.dbc(temp)

filtro <- teste %>%
  janitor::clean_names() %>%
  #Renomeando as colunas
  rename(Origem_Dados = origem, Local_Nascimento = locnasc, Conjugal_Mãe = estcivmae, Escolaridade_Mãe = escmae, Filhos_Nascidos_Vivos = qtdfilvivo, Filhos_Nascidos_Mortos = qtdfilmort,
         Municipio_Residencia = codmunres, Consultas_PréNatal = consultas, Peso_Gramas = peso, Anomalia_Cognitiva = idanomal, DataCadastro_Sistema= dtcadastro, CodigoMalformacao = codanomal,
         NaturalidadeMae_Municipio = codmunnatu, NaturalidadeMae_UF = codufnatu, EscolaridadeMae=escmae2010, NªGestacoesAnteriores = qtdgestant, NªGestacoesVaginaisAnteriores = qtdpartnor,
         NªGestacoesCesesariasAnteriores = qtdpartces, Data_UltimaMenstruacao = dtultmenst, SemanasGestacao = semagestac, Estimar_SemanasGestacao = tpmetestim, NºConsultasPreNatal = consprenat,
         MesInicioPreNatal = mesprenat, PartoInduzido = sttrabpart, CesariaAntesParto = stcesparto, Nascimento_Asssistido = tpnascassi) %>%

  #Documentando as colunas
  mutate(
    Origem_Dados = recode(Origem_Dados, '1' = "Oracle", '2' = "FTP", '3' = "SEAD"),
    Local_Nascimento = recode(Local_Nascimento, '1' = "Hospital", '2' = "Outros Estabelecimentos de Saúde", '3' = "Domicílio", '4' = "Outros",'5' = "Aldeia Indígena", '9' = "Ignorado"),
    Conjugal_Mãe = recode(Conjugal_Mãe, '1' = "Solteira",'2' = "Casada",'3' = "Viúva",'4' = "Divorciada", '5' = "União estável",'9' = "Ignorada"),
    Escolaridade_Mãe = recode(Escolaridade_Mãe, '1' = "Nenhuma", '2' = "1 a 2 anos",'3' = "4 a 7 anos",'4' = "8 a 11 anos", '5'="12 e mais",'9' = "Ignorado"),
    gestacao = recode(gestacao, '1' = "Menos de 22 semanas", '2' = "22 a 27 semanas",'3' = "28 a 31 semanas",'4' = "32 a 36 semanas",'5' = "37 a 41 semanas",'6'= "42 semanas e mais",'9'="Ignorado"),
    gravidez = recode(gravidez, '1' = "Única",'2' = "Dupla","3" = "Tripla ou mais", '9' = "Ignorado"),
    parto = recode(parto, '1' = "Vaginal",'2' = "Cesário",'9'="Ignorado"),
    Consultas_PréNatal = recode(Consultas_PréNatal, '1' = "Nenhuma",'2'="De 1 a 3",'3'="de 4 a 6",'4'="7 e mais",'9'="Ignorado"),
    sexo = recode(sexo, '0' = "Ignorado",'1'= "Masculino",'2'="Feminino"),
    racacor = recode(racacor, '1'="Branca",'2'="Preta",'3'="Amarela",'4'="Parda",'5'="Indígena"),
    Anomalia_Cognitiva = recode(Anomalia_Cognitiva, '9' = "Ignorado", '1'="Sim",'2'="Não"),
    EscolaridadeMae = recode(EscolaridadeMae, '0' = "Sem escolaridade",'1'="Fundamental 1",'2'="Fundamental 2", '3' = "Médio (antigo 2º grau)",'4' = "Superior incompleto",'5' = "Superior completo",'9'="Ignorado"),
    dtnascmae = dmy(as.character(dtnascmae)),
    racacormae = recode(racacormae, '1'="Branca",'2'="Preta",'3'="Amarela",'4'="Parda",'5'="Indígena"),
    Data_UltimaMenstruacao = dmy(as.character(Data_UltimaMenstruacao)),
    Estimar_SemanasGestacao = recode(Estimar_SemanasGestacao, '1'="Exame físico",'2'="Outro método",'9'="Ignorado"),
    tpapresent= recode(tpapresent, '1'="Cefálica",'2'="Pélvica ou podálica",'3'="Transversa",'9'="Ignorado"),
    PartoInduzido = recode(PartoInduzido,'1'="Sim",'2'="Não",'9'="Ignorado"),
    CesariaAntesParto = recode(CesariaAntesParto, '1'="Sim",'2'="Não",'3'="Não se aplica",'9'="Ignorado"),
    Nascimento_Asssistido = recode(Nascimento_Asssistido,'1'="Medico",'2'="Enfermeira/Obstetriz",'3'="Parteira",'4'="Outros",'9'="Ignorado"),
  )

## dados melhores:

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/DNAC2023.dbc"
temp <- tempfile()
download.file(url, temp, method = 'curl')
teste <- datazoom.amazonia:::read.dbc(temp)

filtro <- teste %>%
  janitor::clean_names() %>%
  # renomeando as colunas
  rename(origem_dados = origem, local_nascimento = locnasc, conjugal_mae = estcivmae, escolaridade_mae = escmae,
         filhos_nascidos_vivos = qtdfilvivo, filhos_nascidos_mortos = qtdfilmort, municipio_residencia = codmunres,
         consultas_pre_natal = consultas, peso_gramas = peso, anomalia_cognitiva = idanomal, data_cadastro_sistema = dtcadastro,
         codigo_malformacao = codanomal, naturalidade_mae_municipio = codmunnatu, naturalidade_mae_uf = codufnatu,
         escolaridade_mae_2010 = escmae2010, gestacoes_anteriores = qtdgestant, partos_vaginais_anteriores = qtdpartnor,
         partos_cesareas_anteriores = qtdpartces, data_ultima_menstruacao = dtultmenst, semanas_gestacao = semagestac,
         estimar_semanas_gestacao = tpmetestim, num_consultas_pre_natal = consprenat, mes_inicio_pre_natal = mesprenat,
         parto_induzido = sttrabpart, cesaria_antes_parto = stcesparto, nascimento_assistido = tpnascassi) %>%

  # documentando as colunas
  mutate(
    origem_dados = recode(origem_dados, '1' = "oracle", '2' = "ftp", '3' = "sead"),
    local_nascimento = recode(local_nascimento, '1' = "hospital", '2' = "outros estabelecimentos saude", '3' = "domicilio", '4' = "outros", '5' = "aldeia indigena", '9' = "ignorado"),
    conjugal_mae = recode(conjugal_mae, '1' = "solteira", '2' = "casada", '3' = "viuva", '4' = "divorciada", '5' = "uniao estavel", '9' = "ignorado"),
    escolaridade_mae = recode(escolaridade_mae, '1' = "nenhuma", '2' = "1 a 2 anos", '3' = "4 a 7 anos", '4' = "8 a 11 anos", '5' = "12 e mais", '9' = "ignorado"),
    gestacao = recode(gestacao, '1' = "menos 22 semanas", '2' = "22 a 27 semanas", '3' = "28 a 31 semanas", '4' = "32 a 36 semanas", '5' = "37 a 41 semanas", '6'= "42 semanas mais", '9'="ignorado"),
    gravidez = recode(gravidez, '1' = "unica", '2' = "dupla", "3" = "tripla ou mais", '9' = "ignorado"),
    parto = recode(parto, '1' = "vaginal", '2' = "cesario", '9'="ignorado"),
    consultas_pre_natal = recode(consultas_pre_natal, '1' = "nenhuma", '2'="1 a 3", '3'="4 a 6", '4'="7 ou mais", '9'="ignorado"),
    sexo = recode(sexo, '0' = "ignorado", '1'= "masculino", '2'="feminino"),
    raca_cor = recode(raca_cor, '1'="branca", '2'="preta", '3'="amarela", '4'="parda", '5'="indigena"),
    anomalia_cognitiva = recode(anomalia_cognitiva, '9' = "ignorado", '1'="sim", '2'="nao"),
    escolaridade_mae_2010 = recode(escolaridade_mae_2010, '0' = "sem escolaridade", '1'="fundamental 1", '2'="fundamental 2", '3' = "medio", '4' = "superior incompleto", '5' = "superior completo", '9'="ignorado"),
    dtnascmae = dmy(as.character(dtnascmae)),
    raca_cor_mae = recode(raca_cor_mae, '1'="branca", '2'="preta", '3'="amarela", '4'="parda", '5'="indigena"),
    data_ultima_menstruacao = dmy(as.character(data_ultima_menstruacao)),
    estimar_semanas_gestacao = recode(estimar_semanas_gestacao, '1'="exame fisico", '2'="outro metodo", '9'="ignorado"),
    tipo_apresentacao = recode(tpapresent, '1'="cefalica", '2'="pelvica ou podalica", '3'="transversa", '9'="ignorado"),
    parto_induzido = recode(parto_induzido, '1'="sim", '2'="nao", '9'="ignorado"),
    cesaria_antes_parto = recode(cesaria_antes_parto, '1'="sim", '2'="nao", '3'="nao se aplica", '9'="ignorado"),
    nascimento_assistido = recode(nascimento_assistido, '1'="medico", '2'="enfermeira obstetriz", '3'="parteira", '4'="outros", '9'="ignorado")
  )
