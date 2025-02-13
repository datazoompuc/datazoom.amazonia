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
