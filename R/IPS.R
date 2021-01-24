utils::download.file(
  url = 'http://www.ipsamazonia.org.br/assets/IPS_Tabela_Completa-8bb3b841e46c8fb17b0331d8ea92bef3.xlsx',
  destfile = file.path(getwd(), 'IPS.xlsx'),
  mode = "wb"
)


raw.data <- 1:3 %>%
  purrr::map( ~.x %>%
  readxl::read_excel('IPS.xlsx', sheet = .)
  )


col.names.eng <- tibble::tribble(
  ~original_name, ~new_pt.br, ~new_eng,
  'IBGEDados', 'cod_municipio', 'city_code',
  'Município', 'municipio', 'city',
  'Estado', 'uf', 'state',
  'Índice de progresso social', 'IPS', 'SPI',
  'Necessidades Humanas Básicas', '', 'Basic human needs',
  'Fundamentos para o Bem-Estar', '', 'Well-being fundamentals',
  'Oportunidades', '', 'Opportunities',
  'Nutrição e cuidados médicos básicos', '', 'Nutrition and basic medical care',
  'Água e saneamento', '', 'Water and sanitation',
  'Moradia', '', 'Habitation',
  'Segurança pessoal', '', 'Personal safety',
  'Acesso ao conhecimento básico', '', 'Access to basic knowledge',
  'Acesso à informação e comunicação', '', 'Access to information and communication',
  'Saúde e bem-estar', '', 'Health and well-being',
  'Qualidade do meio ambiente', '', 'Environment quality',
  'Direitos individuais', '', 'Individual rights',
  'Liberdade individual e de escolha', '', 'Individual freedom of choice',
  'Tolerância e inclusão', '', 'Tolerance and inclusion',
  'Acesso à educação superior', '', 'Access to higher education',
  'Mortalidade infantil até 5 anos (Óbitos por mil nascidos vivos', '',
  'Infant mortality - until 5 years (Deaths per thousand live births',
  'Mortalidade materna (Óbitos maternos por 100 mil nascidos vivos)', '',
  'Maternal mortality (Maternal deaths per 100.000 live births',
  'Mortalidade por desnutrição (Óbitos por 100 mil habitantes)', '',
  'Malnutrition mortality (Deaths per 100.000 people)',
  'Mortalidade por doenças infecciosas (Óbitos por 100 mil habitantes)', '',
  'Infeccious diseases mortality (Deaths per 100.000 people)',
  'Subnutrição (% da população)', '', 'Malnutrition (% of population)',
  'Abastecimento de água (% da população)', '', 'Water supply (% of population)',
  "Esgotamento sanitário  (% da população)", "", 'Sewage (% of population)',
  "Saneamento rural (diferença entre a % da pop.
  Rural com acesso a água em relação à urbana)",
  "", "Rural sanitation (difference between % of rural pop. with access to water relative
  to urban population",
  "Acesso à energia elétrica (% da população)", "", "Access to electricity (% da população)",
  "Coleta de lixo (% da população)", "", "Garbage collection (% of population)",
  "Moradia adequada (% da população)", "", "Adequate habitation (% of population)",
  "Assassinatos de jovens (Óbitos por 100 mil habitantes de 15 a 24 anos.
  Pontuados em uma escala de 1-6: 1 = 0 2 = 1 - 6 3 = 6 - 10 4 = 10 - 20 5 = 20 - 40 6 > 40)",
  "", "Youth murders (Deaths per 100.000 people aged 15 to 24. Scored on a 1-6 scale:
  1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40",
  'Homicídios', '',
  'Homicides (Deaths per 100.000 people. Scored on a 1-6 scale:
  1 = 0; 2 = 1-6; 3 = 6-10; 4 = 10-10; 5 = 20-40; 6 = > 40',
  "Mortes por acidente no trânsito (Óbitos por 100 mil habitantes)", "",
  "Traffic accident deaths (Deaths per 100.000 people)",
  "Acesso ao ensino fundamental (% de frequência líquida ao ensino fundamental)",
  "", "Access to elementary school (% of net frequency in elementary school)",
  "Acesso ao ensino médio (% de frequência líquida ao ensino médio)",
  "", "Access to high school (% of net frequency in middle school)",
  'Analfabetismo (% da população de 15 anos ou mais)', "",
  "Illiteracy (% of 15+ population)",
  "Qualidade da educação Ideb (escala de 0-10)", "", "Education quality Ideb (0-10 scale)",
  "% de conexão efetuadas com sucesso. Pontuados em uma escala de 0-5.
  0 = 2% 1 = 2% - 79% 2 = 80% - 96% 3 = 96% - 98% 4 = 98% - 99% 5 = 99% - 100",
  "", "% of successful connections. Scored on a 0-5 scale. 0 = 2%; 1 = 2%-79%;
  2 = 80%-96%; 3 = 96%-98%; 4 = 98%-99%; 5 = 99%-100%",
  "Conexão de voz (% de ligações realizadas com sucesso. Pontuados em uma escala de
  1-5. 1 = 49% - 79% 2 = 80% - 96% 3 = 96% - 98% 4 = 98% - 99% 5 = 99% - 100)", "",
  "Voice connections (% of successful calls. Scored on a 1-5 scale. 1 = 49%-79%;
  2 = 80%-96%; 3 = 96%-98%; 4 = 98%-99%; 5 = 99%-100%",
  "Expectativa de vida ao nascer (número de anos)", "", "Life expectancy at birth (years)",
  "Mortalidade por doenças crônicas (Óbitos por 100 mil habitantes)", "",
  "Infeccious diseases mortality (Deaths per 100.000 people)",
  "Mortalidade por doenças respiratórias (Óbitos por 100 mil habitantes)",
  "", 'Respiratory diseases mortality (Deaths per 100.000 people)',
  "Obesidade (% da população)", "", "Obesity (% of population)",
  'Suicídio (Óbitos por 100 mil habitantes)', "",
  "Suicides (Deaths per 100.000 people)",
  "Área degradada (%)", "", "Degraded area (%)",
  'Áreas protegidas (%)', "", "Protected area (%)",
  'Desmatamento acumulado (%)', '', 'Accumulated deforestation (%)',
  "Desmatamento recente (% do desmatamento de 2015, 2016, 2017 em relação ao total)",
  "", "Recent deforestation",
  'Desperdício de água (%)', "", "Water waste (%)",
  'Diversidade partidária (%)', "", "Partisan diversity (%)",
  "Mobilidade urbana (número de ônibus por mil habitantes)",
  "", "Urban mobility (buses per thousand people)",
  "Pessoas ameaçadas (número de ameaçados de morte por 100 mil habitantes)", "",
  "Threatened people (number of people threatened with death per thousand people)",
  "Acesso a cultura, lazer e esporte (Categórica. Pontuado em:
  0 = nenhuma estrutura; 1 = uma; 2 = duas; 3 = três; 4 = todas as estruturas)",
  "", "Access to culture, leisure, and sports (Cathegorical. Scored by:
  0 = no structure; 1 = one; 2 = two; 3 = three; 4 = all structures",
  "Gravidez na infância e adolescência (% de mulheres de 15 a 17 anos que tiveram filhos)",
  "", "Child of adolescence pregnancy (% of women aged 15-17 with children)",
  "Trabalho infantil (% da população entre 10 e 14 anos de idade)", "",
  "Child labor (% of population aged 10-14)",
  "Vulnerabilidade familia (% de mães)", "", "Family vulnerability (% of mothers)",
  "Desigualdade racial na educação (% da população com 15 anos ou mais)", "",
  "Racial inequality in education (% da população com 15 anos e mais)",
  "Violência contra a mulher (casos por 100 mil mulheres)", "",
  "Violence against women (cases per 100.000 women)",
  "Violência contra indígena (casos por mil indígenas.
  Pontuados em uma escala de 1-3. 1 = 0 - 20  2 = 21 - 40  3 > 40)", "",
  "Violênce against indigenous people (cases by thousand indigenous people.
  Scored on a 1-3 scale. 1 = 0-20; 2 = 21-40; 3 = > 40)",
  "Educação feminina (% da população feminina com 15 anos ou mais)", "",
  "Female education (% of female population aged 15 or more)",
  "Frequência ao ensino superior (% da população entre 18-24 anos)", "",
  "Attendance to higher education (% of population aged 18-24)",
  "Pessoas com ensino superior (% da população com mais de 25 anos)", "",
  "People with higher education (% of population aged 25+)"
  )

colnames(raw.data[[1]]) <- col.names.eng$original_name
colnames(raw.data[[2]]) <- col.names.eng$original_name

raw.data[[1]]$ano <- 2018
raw.data[[2]]$ano <- 2014

df <- bind_rows(raw.data[[1]], raw.data[[2]])

