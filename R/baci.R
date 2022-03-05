



load_baci = function(dataset = 'HS92', raw_data, time_period,
                     language = 'pt'){



  #############################
  ## Define Basic Parameters ##
  #############################

  param=list()
  param$dataset = dataset
  param$raw_data = raw_data
  param$time_period = time_period
  param$language = language

  ## Check if year is acceptable

  year_check = datasets_baci() %>%
    dplyr::select(available_time) %>%
    unlist() %>% as.character() %>%
    stringr::str_split(pattern = '-') %>%
    unlist() %>% as.numeric()

  if (min(time_period) < year_check[1]){stop('Provided time period less than supported. Check documentation for time availability.')}
  if (max(time_period) > year_check[2]){stop('Provided time period greater than supported. Check documentation for time availability.')}


  #################
  ## Downloading ##
  #################

  if (is.null(param$dataset)){stop('Missing Dataset!')}
  if (is.null(param$raw_data)){stop('Missing TRUE/FALSE for Raw Data')}

  if(param$dataset == "HS92") {

    url = "http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202201"
  }

  url = paste0(url, ".zip")

  temp = tempfile()
  download.file(url, temp, mode = "wb")
  dat = read.table(unz(temp, paste0("BACI_HS92_Y", param$time_period, "_V202201.csv")),
                   fill = T, header = F, sep = ",")
  unlink(temp)

  dat = dat %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  dat = dat[-1,]

  colnames(dat) = c("t", "i", "j", "k", "v", "q")

  if (raw_data == TRUE){return(dat)}

  ######################
  ## Data Engineering ##
  ######################

  if(param$dataset == "HS92" & param$language == "pt") {

    dat = dat %>%
      dplyr::rename(ano = t,
                    exportador = i,
                    importador = j,
                    produto = k,
                    valor = v,
                    quantidade = q)


    dat = dat %>%
      dplyr::mutate(exportador = dplyr::case_when(exportador == 4 ~ "Afeganistao",
                                                  exportador == 8 ~ "Albania",
                                                  exportador == 12 ~ "Algeria",
                                                  exportador == 16 ~ "Samoa Americana",
                                                  exportador == 20 ~ "Andorra",
                                                  exportador == 24 ~ "Angola",
                                                  exportador == 28 ~ "Antigua e Barbuda",
                                                  exportador == 31 ~ "Azerbaijao",
                                                  exportador == 32 ~ "Argentina",
                                                  exportador == 36 ~ "Australia",
                                                  exportador == 40 ~ "Austria",
                                                  exportador == 44 ~ "Bahamas",
                                                  exportador == 48 ~ "Bahrein",
                                                  exportador == 52 ~ "Barbados",
                                                  exportador == 56 ~ "Belgica",
                                                  exportador == 60 ~ "Belgica-Luxemburgo",
                                                  exportador == 64 ~ "Butao",
                                                  exportador == 68 ~ "Bolivia",
                                                  exportador == 70 ~ "Bosnia",
                                                  exportador == 72 ~ "Botsuana",
                                                  exportador == 76 ~ "Brasil",
                                                  exportador == 84 ~ "Belize",
                                                  exportador == 86 ~ "Oceano Indico Britanico",
                                                  exportador == 90 ~ "Ilhas Salomao",
                                                  exportador == 92 ~ "Ilhas Virgens Britanicas",
                                                  exportador == 96 ~ "Brunei",
                                                  exportador == 100 ~ "Bulgaria",
                                                  exportador == 104 ~ "Mianmar",
                                                  exportador == 108 ~ "Burundi",
                                                  exportador == 112 ~ "Bielorrussia",
                                                  exportador == 116 ~ "Cambodja",
                                                  exportador == 120 ~ "Camaroes",
                                                  exportador == 124 ~ "Canada",
                                                  exportador == 132 ~ "Cabo Verde",
                                                  exportador == 136 ~ "Ilhas Cayman",
                                                  exportador == 140 ~ "Republica Centro Africana",
                                                  exportador == 144 ~ "Sri Lanka",
                                                  exportador == 148 ~ "Chade",
                                                  exportador == 152 ~ "Chile",
                                                  exportador == 156 ~ "China",
                                                  exportador == 162 ~ "Ilha Christmas",
                                                  exportador == 166 ~ "Ilhas Cocos",
                                                  exportador == 170 ~ "Colombia",
                                                  exportador == 174 ~ "Comores",
                                                  exportador == 175 ~ "Mayotte",
                                                  exportador == 178 ~ "Congo",
                                                  exportador == 180 ~ "Republica Democratica do Congo",
                                                  exportador == 184 ~ "Ilhas Cook",
                                                  exportador == 188 ~ "Costa Rica",
                                                  exportador == 191 ~ "Croacia",
                                                  exportador == 192 ~ "Cuba",
                                                  exportador == 196 ~ "Chipre",
                                                  exportador == 200 ~ "Tchecoslovaquia",
                                                  exportador == 203 ~ "Republica Tcheca",
                                                  exportador == 204 ~ "Benin",
                                                  exportador == 208 ~ "Dinamarca",
                                                  exportador == 212 ~ "Dominica",
                                                  exportador == 214 ~ "Republica Dominicana",
                                                  exportador == 218 ~ "Equador",
                                                  exportador == 222 ~ "EL Salvador",
                                                  exportador == 226 ~ "Guine Equatorial",
                                                  exportador == 231 ~ "Etiopia",
                                                  exportador == 232 ~ "Eritrea",
                                                  exportador == 233 ~ "Estonia",
                                                  exportador == 238 ~ "Ilhas Malvinas",
                                                  exportador == 242 ~ "Fiji",
                                                  exportador == 246 ~ "Finlandia",
                                                  exportador == 251 ~ "Franca",
                                                  exportador == 258 ~ "Polinesia Francesa",
                                                  exportador == 260 ~ "Terras Francesas",
                                                  exportador == 262 ~ "Djibouti",
                                                  exportador == 266 ~ "Gabao",
                                                  exportador == 268 ~ "Georgia",
                                                  exportador == 270 ~ "Gambia",
                                                  exportador == 275 ~ "Palestina",
                                                  exportador == 276 ~ "Alemanha",
                                                  exportador == 278 ~ "Antiga Alemanha Oriental",
                                                  exportador == 280 ~ "Antiga Alemanha Ocidental",
                                                  exportador == 288 ~ "Gana",
                                                  exportador == 292 ~ "Gibraltar",
                                                  exportador == 296 ~ "Quiribalti",
                                                  exportador == 300 ~ "Grecia",
                                                  exportador == 304 ~ "Groenlandia",
                                                  exportador == 308 ~ "Granada",
                                                  exportador == 316 ~ "Guam",
                                                  exportador == 320 ~ "Guatemala",
                                                  exportador == 324 ~ "Guine",
                                                  exportador == 328 ~ "Guiana",
                                                  exportador == 332 ~ "Haiti",
                                                  exportador == 340 ~ "Honduras",
                                                  exportador == 344 ~ "Hong Kong",
                                                  exportador == 348 ~ "Hungria",
                                                  exportador == 352 ~ "Islandia",
                                                  exportador == 360 ~ "Indonesia",
                                                  exportador == 364 ~ "Ira",
                                                  exportador == 368 ~ "Iraque",
                                                  exportador == 372 ~ "Irlanda",
                                                  exportador == 376 ~ "Israel",
                                                  exportador == 381 ~ "Italia",
                                                  exportador == 384 ~ "Costa do Marfim",
                                                  exportador == 388 ~ "Jamaica",
                                                  exportador == 392 ~ "Japao",
                                                  exportador == 398 ~ "Cazaquistao",
                                                  exportador == 400 ~ "Jordania",
                                                  exportador == 404 ~ "Quenia",
                                                  exportador == 408 ~ "Coreia do Norte",
                                                  exportador == 410 ~ "Coreia do Sul",
                                                  exportador == 414 ~ "Kuwait",
                                                  exportador == 417 ~ "Quirguistao",
                                                  exportador == 418 ~ "Laos",
                                                  exportador == 422 ~ "Libano",
                                                  exportador == 426 ~ "Lesoto",
                                                  exportador == 428 ~ "Latvia",
                                                  exportador == 430 ~ "Liberia",
                                                  exportador == 434 ~ "Libia",
                                                  exportador == 440 ~ "Lituania",
                                                  exportador == 442 ~ "Luxemburgo",
                                                  exportador == 446 ~ "Macau",
                                                  exportador == 450 ~ "Madagascar",
                                                  exportador == 454 ~ "Malawi",
                                                  exportador == 458 ~ "Malasia",
                                                  exportador == 462 ~ "Maldivas",
                                                  exportador == 466 ~ "Mali",
                                                  exportador == 470 ~ "Malta",
                                                  exportador == 478 ~ "Mauritania",
                                                  exportador == 480 ~ "Ilhas Mauricio",
                                                  exportador == 484 ~ "Mexico",
                                                  exportador == 490 ~ "Other Asia",
                                                  exportador == 496 ~ "Mongolia",
                                                  exportador == 498 ~ "Moldavia",
                                                  exportador == 500 ~ "Montserrat",
                                                  exportador == 504 ~ "Marrocos",
                                                  exportador == 508 ~ "Mocambique",
                                                  exportador == 512 ~ "Oma",
                                                  exportador == 516 ~ "Namibia",
                                                  exportador == 520 ~ "Nauru",
                                                  exportador == 524 ~ "Nepal",
                                                  exportador == 528 ~ "Holanda",
                                                  exportador == 530 ~ "Antilhas Holandesas",
                                                  exportador == 531 ~ "Curacao",
                                                  exportador == 533 ~ "Aruba",
                                                  exportador == 534 ~ "Paises Baixos",
                                                  exportador == 535 ~ "Paises Baixos Caribenhos",
                                                  exportador == 540 ~ "Nova Caledonia",
                                                  exportador == 548 ~ "Vanuatu",
                                                  exportador == 554 ~ "Nova Zelandia",
                                                  exportador == 558 ~ "Nicaragua",
                                                  exportador == 562 ~ "Niger",
                                                  exportador == 566 ~ "Nigeria",
                                                  exportador == 570 ~ "Niue",
                                                  exportador == 574 ~ "Ilha Norfolk",
                                                  exportador == 579 ~ "Noruega",
                                                  exportador == 580 ~ "Ilhas Marianas do Norte",
                                                  exportador == 583 ~ "Micronesia",
                                                  exportador == 584 ~ "Ilhas Marshall",
                                                  exportador == 585 ~ "Palau",
                                                  exportador == 586 ~ "Paquistao",
                                                  exportador == 591 ~ "Panama",
                                                  exportador == 598 ~ "Papua Nova Guine",
                                                  exportador == 600 ~ "Paraguai",
                                                  exportador == 604 ~ "Peru",
                                                  exportador == 608 ~ "Filipinas",
                                                  exportador == 612 ~ "Ilhas Pitcairn",
                                                  exportador == 616 ~ "Polonia",
                                                  exportador == 620 ~ "Portugal",
                                                  exportador == 624 ~ "Guine Bissau",
                                                  exportador == 626 ~ "Timor Leste",
                                                  exportador == 634 ~ "Catar",
                                                  exportador == 642 ~ "Romenia",
                                                  exportador == 643 ~ "Russia",
                                                  exportador == 646 ~ "Ruanda",
                                                  exportador == 652 ~ "Sao Bartolomeu",
                                                  exportador == 654 ~ "Santa Helena",
                                                  exportador == 659 ~ "Sao Cristovao e Nevis",
                                                  exportador == 660 ~ "Anguilla",
                                                  exportador == 662 ~ "Santa Lucia",
                                                  exportador == 666 ~ "Sao Pedro e Miquelao",
                                                  exportador == 670 ~ "Sao Vicente e Granadinas",
                                                  exportador == 674 ~ "San Marino",
                                                  exportador == 678 ~ "Sao Tome e Principe",
                                                  exportador == 682 ~ "Arabia Saudita",
                                                  exportador == 686 ~ "Senegal",
                                                  exportador == 688 ~ "Servia",
                                                  exportador == 690 ~ "Seychelles",
                                                  exportador == 694 ~ "Serra Leoa",
                                                  exportador == 697 ~ "Associacao Europeia",
                                                  exportador == 699 ~ "India",
                                                  exportador == 702 ~ "Singapura",
                                                  exportador == 703 ~ "Eslovaquia",
                                                  exportador == 704 ~ "Vietna",
                                                  exportador == 705 ~ "Eslovenia",
                                                  exportador == 706 ~ "Somalia",
                                                  exportador == 710 ~ "Africa do Sul",
                                                  exportador == 711 ~ "Uniao Aduaneira da Africa",
                                                  exportador == 716 ~ "Zimbabue",
                                                  exportador == 724 ~ "Espanha",
                                                  exportador == 728 ~ "Sudao do Sul",
                                                  exportador == 729 ~ "Sudao",
                                                  exportador == 736 ~ "Antigo Sudao",
                                                  exportador == 740 ~ "Suriname",
                                                  exportador == 748 ~ "Suazilandia",
                                                  exportador == 752 ~ "Suecia",
                                                  exportador == 757 ~ "Suica",
                                                  exportador == 760 ~ "Siria",
                                                  exportador == 762 ~ "Tajiquistao",
                                                  exportador == 764 ~ "Tailandia",
                                                  exportador == 768 ~ "Togo",
                                                  exportador == 772 ~ "Tokelau",
                                                  exportador == 776 ~ "Tonga",
                                                  exportador == 780 ~ "Trindade e Tobago",
                                                  exportador == 784 ~ "Emirados Arabes",
                                                  exportador == 788 ~ "Tunisia",
                                                  exportador == 792 ~ "Turquia",
                                                  exportador == 795 ~ "Turcomenistao",
                                                  exportador == 796 ~ "Ilhas Turcas e Caicos",
                                                  exportador == 798 ~ "Tuvalu",
                                                  exportador == 800 ~ "Uganda",
                                                  exportador == 804 ~ "Ucrania",
                                                  exportador == 807 ~ "Macedonia",
                                                  exportador == 810 ~ "Uniao Sovietica",
                                                  exportador == 818 ~ "Egito",
                                                  exportador == 826 ~ "Reino Unido",
                                                  exportador == 834 ~ "Tanzania",
                                                  exportador == 842 ~ "Estados Unidos",
                                                  exportador == 849 ~ "Ilhas Pacificas dos EUA",
                                                  exportador == 854 ~ "Burkina Faso",
                                                  exportador == 858 ~ "Uruguai",
                                                  exportador == 860 ~ "Uzbequistao",
                                                  exportador == 862 ~ "Venezuela",
                                                  exportador == 876 ~ "Wallis e Futuna",
                                                  exportador == 882 ~ "Samoa",
                                                  exportador == 887 ~ "Iemen",
                                                  exportador == 891 ~ "Servia e Montenegro",
                                                  exportador == 894 ~ "Zambia"))


    dat = dat %>%
      dplyr::mutate(importador = dplyr::case_when(importador == 4 ~ "Afeganistao",
                                                  importador == 8 ~ "Albania",
                                                  importador == 12 ~ "Algeria",
                                                  importador == 16 ~ "Samoa Americana",
                                                  importador == 20 ~ "Andorra",
                                                  importador == 24 ~ "Angola",
                                                  importador == 28 ~ "Antigua e Barbuda",
                                                  importador == 31 ~ "Azerbaijao",
                                                  importador == 32 ~ "Argentina",
                                                  importador == 36 ~ "Australia",
                                                  importador == 40 ~ "Austria",
                                                  importador == 44 ~ "Bahamas",
                                                  importador == 48 ~ "Bahrein",
                                                  importador == 52 ~ "Barbados",
                                                  importador == 56 ~ "Belgica",
                                                  importador == 60 ~ "Belgica-Luxemburgo",
                                                  importador == 64 ~ "Butao",
                                                  importador == 68 ~ "Bolivia",
                                                  importador == 70 ~ "Bosnia",
                                                  importador == 72 ~ "Botsuana",
                                                  importador == 76 ~ "Brasil",
                                                  importador == 84 ~ "Belize",
                                                  importador == 86 ~ "Oceano Indico Britanico",
                                                  importador == 90 ~ "Ilhas Salomao",
                                                  importador == 92 ~ "Ilhas Virgens Britanicas",
                                                  importador == 96 ~ "Brunei",
                                                  importador == 100 ~ "Bulgaria",
                                                  importador == 104 ~ "Mianmar",
                                                  importador == 108 ~ "Burundi",
                                                  importador == 112 ~ "Bielorrussia",
                                                  importador == 116 ~ "Cambodja",
                                                  importador == 120 ~ "Camaroes",
                                                  importador == 124 ~ "Canada",
                                                  importador == 132 ~ "Cabo Verde",
                                                  importador == 136 ~ "Ilhas Cayman",
                                                  importador == 140 ~ "Republica Centro Africana",
                                                  importador == 144 ~ "Sri Lanka",
                                                  importador == 148 ~ "Chade",
                                                  importador == 152 ~ "Chile",
                                                  importador == 156 ~ "China",
                                                  importador == 162 ~ "Ilha Christmas",
                                                  importador == 166 ~ "Ilhas Cocos",
                                                  importador == 170 ~ "Colombia",
                                                  importador == 174 ~ "Comores",
                                                  importador == 175 ~ "Mayotte",
                                                  importador == 178 ~ "Congo",
                                                  importador == 180 ~ "Republica Democratica do Congo",
                                                  importador == 184 ~ "Ilhas Cook",
                                                  importador == 188 ~ "Costa Rica",
                                                  importador == 191 ~ "Croacia",
                                                  importador == 192 ~ "Cuba",
                                                  importador == 196 ~ "Chipre",
                                                  importador == 200 ~ "Tchecoslovaquia",
                                                  importador == 203 ~ "Republica Tcheca",
                                                  importador == 204 ~ "Benin",
                                                  importador == 208 ~ "Dinamarca",
                                                  importador == 212 ~ "Dominica",
                                                  importador == 214 ~ "Republica Dominicana",
                                                  importador == 218 ~ "Equador",
                                                  importador == 222 ~ "EL Salvador",
                                                  importador == 226 ~ "Guine Equatorial",
                                                  importador == 231 ~ "Etiopia",
                                                  importador == 232 ~ "Eritrea",
                                                  importador == 233 ~ "Estonia",
                                                  importador == 238 ~ "Ilhas Malvinas",
                                                  importador == 242 ~ "Fiji",
                                                  importador == 246 ~ "Finlandia",
                                                  importador == 251 ~ "Franca",
                                                  importador == 258 ~ "Polinesia Francesa",
                                                  importador == 260 ~ "Terras Francesas",
                                                  importador == 262 ~ "Djibouti",
                                                  importador == 266 ~ "Gabao",
                                                  importador == 268 ~ "Georgia",
                                                  importador == 270 ~ "Gambia",
                                                  importador == 275 ~ "Palestina",
                                                  importador == 276 ~ "Alemanha",
                                                  importador == 278 ~ "Antiga Alemanha Oriental",
                                                  importador == 280 ~ "Antiga Alemanha Ocidental",
                                                  importador == 288 ~ "Gana",
                                                  importador == 292 ~ "Gibraltar",
                                                  importador == 296 ~ "Quiribalti",
                                                  importador == 300 ~ "Grecia",
                                                  importador == 304 ~ "Groenlandia",
                                                  importador == 308 ~ "Granada",
                                                  importador == 316 ~ "Guam",
                                                  importador == 320 ~ "Guatemala",
                                                  importador == 324 ~ "Guine",
                                                  importador == 328 ~ "Guiana",
                                                  importador == 332 ~ "Haiti",
                                                  importador == 340 ~ "Honduras",
                                                  importador == 344 ~ "Hong Kong",
                                                  importador == 348 ~ "Hungria",
                                                  importador == 352 ~ "Islandia",
                                                  importador == 360 ~ "Indonesia",
                                                  importador == 364 ~ "Ira",
                                                  importador == 368 ~ "Iraque",
                                                  importador == 372 ~ "Irlanda",
                                                  importador == 376 ~ "Israel",
                                                  importador == 381 ~ "Italia",
                                                  importador == 384 ~ "Costa do Marfim",
                                                  importador == 388 ~ "Jamaica",
                                                  importador == 392 ~ "Japao",
                                                  importador == 398 ~ "Cazaquistao",
                                                  importador == 400 ~ "Jordania",
                                                  importador == 404 ~ "Quenia",
                                                  importador == 408 ~ "Coreia do Norte",
                                                  importador == 410 ~ "Coreia do Sul",
                                                  importador == 414 ~ "Kuwait",
                                                  importador == 417 ~ "Quirguistao",
                                                  importador == 418 ~ "Laos",
                                                  importador == 422 ~ "Libano",
                                                  importador == 426 ~ "Lesoto",
                                                  importador == 428 ~ "Latvia",
                                                  importador == 430 ~ "Liberia",
                                                  importador == 434 ~ "Libia",
                                                  importador == 440 ~ "Lituania",
                                                  importador == 442 ~ "Luxemburgo",
                                                  importador == 446 ~ "Macau",
                                                  importador == 450 ~ "Madagascar",
                                                  importador == 454 ~ "Malawi",
                                                  importador == 458 ~ "Malasia",
                                                  importador == 462 ~ "Maldivas",
                                                  importador == 466 ~ "Mali",
                                                  importador == 470 ~ "Malta",
                                                  importador == 478 ~ "Mauritania",
                                                  importador == 480 ~ "Ilhas Mauricio",
                                                  importador == 484 ~ "Mexico",
                                                  importador == 490 ~ "Other Asia",
                                                  importador == 496 ~ "Mongolia",
                                                  importador == 498 ~ "Moldavia",
                                                  importador == 500 ~ "Montserrat",
                                                  importador == 504 ~ "Marrocos",
                                                  importador == 508 ~ "Mocambique",
                                                  importador == 512 ~ "Oma",
                                                  importador == 516 ~ "Namibia",
                                                  importador == 520 ~ "Nauru",
                                                  importador == 524 ~ "Nepal",
                                                  importador == 528 ~ "Holanda",
                                                  importador == 530 ~ "Antilhas Holandesas",
                                                  importador == 531 ~ "Curacao",
                                                  importador == 533 ~ "Aruba",
                                                  importador == 534 ~ "Paises Baixos",
                                                  importador == 535 ~ "Paises Baixos Caribenhos",
                                                  importador == 540 ~ "Nova Caledonia",
                                                  importador == 548 ~ "Vanuatu",
                                                  importador == 554 ~ "Nova Zelandia",
                                                  importador == 558 ~ "Nicaragua",
                                                  importador == 562 ~ "Niger",
                                                  importador == 566 ~ "Nigeria",
                                                  importador == 570 ~ "Niue",
                                                  importador == 574 ~ "Ilha Norfolk",
                                                  importador == 579 ~ "Noruega",
                                                  importador == 580 ~ "Ilhas Marianas do Norte",
                                                  importador == 583 ~ "Micronesia",
                                                  importador == 584 ~ "Ilhas Marshall",
                                                  importador == 585 ~ "Palau",
                                                  importador == 586 ~ "Paquistao",
                                                  importador == 591 ~ "Panama",
                                                  importador == 598 ~ "Papua Nova Guine",
                                                  importador == 600 ~ "Paraguai",
                                                  importador == 604 ~ "Peru",
                                                  importador == 608 ~ "Filipinas",
                                                  importador == 612 ~ "Ilhas Pitcairn",
                                                  importador == 616 ~ "Polonia",
                                                  importador == 620 ~ "Portugal",
                                                  importador == 624 ~ "Guine Bissau",
                                                  importador == 626 ~ "Timor Leste",
                                                  importador == 634 ~ "Catar",
                                                  importador == 642 ~ "Romenia",
                                                  importador == 643 ~ "Russia",
                                                  importador == 646 ~ "Ruanda",
                                                  importador == 652 ~ "Sao Bartolomeu",
                                                  importador == 654 ~ "Santa Helena",
                                                  importador == 659 ~ "Sao Cristovao e Nevis",
                                                  importador == 660 ~ "Anguilla",
                                                  importador == 662 ~ "Santa Lucia",
                                                  importador == 666 ~ "Sao Pedro e Miquelao",
                                                  importador == 670 ~ "Sao Vicente e Granadinas",
                                                  importador == 674 ~ "San Marino",
                                                  importador == 678 ~ "Sao Tome e Principe",
                                                  importador == 682 ~ "Arabia Saudita",
                                                  importador == 686 ~ "Senegal",
                                                  importador == 688 ~ "Servia",
                                                  importador == 690 ~ "Seychelles",
                                                  importador == 694 ~ "Serra Leoa",
                                                  importador == 697 ~ "Associacao Europeia",
                                                  importador == 699 ~ "India",
                                                  importador == 702 ~ "Singapura",
                                                  importador == 703 ~ "Eslovaquia",
                                                  importador == 704 ~ "Vietna",
                                                  importador == 705 ~ "Eslovenia",
                                                  importador == 706 ~ "Somalia",
                                                  importador == 710 ~ "Africa do Sul",
                                                  importador == 711 ~ "Uniao Aduaneira da Africa",
                                                  importador == 716 ~ "Zimbabue",
                                                  importador == 724 ~ "Espanha",
                                                  importador == 728 ~ "Sudao do Sul",
                                                  importador == 729 ~ "Sudao",
                                                  importador == 736 ~ "Antigo Sudao",
                                                  importador == 740 ~ "Suriname",
                                                  importador == 748 ~ "Suazilandia",
                                                  importador == 752 ~ "Suecia",
                                                  importador == 757 ~ "Suica",
                                                  importador == 760 ~ "Siria",
                                                  importador == 762 ~ "Tajiquistao",
                                                  importador == 764 ~ "Tailandia",
                                                  importador == 768 ~ "Togo",
                                                  importador == 772 ~ "Tokelau",
                                                  importador == 776 ~ "Tonga",
                                                  importador == 780 ~ "Trindade e Tobago",
                                                  importador == 784 ~ "Emirados Arabes",
                                                  importador == 788 ~ "Tunisia",
                                                  importador == 792 ~ "Turquia",
                                                  importador == 795 ~ "Turcomenistao",
                                                  importador == 796 ~ "Ilhas Turcas e Caicos",
                                                  importador == 798 ~ "Tuvalu",
                                                  importador == 800 ~ "Uganda",
                                                  importador == 804 ~ "Ucrania",
                                                  importador == 807 ~ "Macedonia",
                                                  importador == 810 ~ "Uniao Sovietica",
                                                  importador == 818 ~ "Egito",
                                                  importador == 826 ~ "Reino Unido",
                                                  importador == 834 ~ "Tanzania",
                                                  importador == 842 ~ "Estados Unidos",
                                                  importador == 849 ~ "Ilhas Pacificas dos EUA",
                                                  importador == 854 ~ "Burkina Faso",
                                                  importador == 858 ~ "Uruguai",
                                                  importador == 860 ~ "Uzbequistao",
                                                  importador == 862 ~ "Venezuela",
                                                  importador == 876 ~ "Wallis e Futuna",
                                                  importador == 882 ~ "Samoa",
                                                  importador == 887 ~ "Iemen",
                                                  importador == 891 ~ "Servia e Montenegro",
                                                  importador == 894 ~ "Zambia"))


    dic = suppressMessages(load_trade_dic(type = "hs"))

    dic = dic %>%
      dplyr::select(co_sh6, no_sh6 = no_sh6_por)

    non_dup = !duplicated(dic)

    dic = dic %>%
      dplyr::filter(non_dup)

    dat = dat %>%
      dplyr::rename(co_sh6 = produto) %>%
      dplyr::mutate(co_sh6 = formatC(co_sh6, width = 4, format = "d", flag = "0")) %>%
      dplyr::left_join(dic,by='co_sh6')


    dat = dat %>%
      dplyr::rename(cod_produto = co_sh6,
                    nome_produto = no_sh6)

    dat = dat %>%
      dplyr::mutate(valor = as.numeric(valor))

    dat = dat %>%
      dplyr::mutate(nome_produto = dplyr::case_when(cod_produto == "080130" ~
      "Nozes comestiveis: castanhas de caju, frescas ou secas, mesmo descascadas ou peladas",
      cod_produto == "844350" ~ "Maquinas de impressao: do tipo NES na posicao 8443",
      cod_produto == "854380" ~ "Maquinas e aparelhos eletricos: com funcao propria, NES na posicao 8543",
      cod_produto == "847120" ~ "Maquinas para processamento de dados: automaticas digitais, contendo no mesmo involucro pelo menos uma unidade central de processamento e uma unidade de entrada e saida, combinadas ou nao",
      cod_produto == "903081" ~ "Instrumentos e aparelhos: com dispositivo de gravacao, especialmente concebido para telecomunicacoes",
      cod_produto == "560300" ~ "Falsos tecidos: mesmo impregnados, revestidos, recobertos ou estratificados",
      cod_produto == "847193" ~ "Maquinas de processamento de dados: unidades de armazenamento, apresentadas ou nao com o resto de um sistema",
      cod_produto == "852490" ~ "Midia gravadas: NES na posicao 8524 para fenomenos de gravacao de som ou similar, incluindo matrizes e mestres para a producao de registros",
      cod_produto == "080110" ~ "Nozes comestiveis: cocos, frescos ou secos, mesmo descascados ou pelados",
      cod_produto == "080710" ~ "Frutas comestiveis: meloes (incluindo melancias), frescos"))

  }

  return(dat)
}



load_trade_dic <- function(type){

  # Bind Global Variables

  locale <-co_sh6 <-co_sh4 <-co_sh2 <-co_ncm_secrom <-no_sh6_ing <-no_sh4_ing <-no_sh2_ing <-no_sec_ing <- NULL

  #########################
  ## Download Dictionary ##
  #########################

  path = 'https://balanca.economia.gov.br/balanca/bd/'

  if (type == 'hs'){final = paste(path,'tabelas/NCM_SH.csv',sep='')} # Harmonized System


  dic = readr::read_delim(final,delim=';',locale = readr::locale(encoding='Latin1'),progress=TRUE)

  #####################
  ## Data Processing ##
  #####################

  dic = dic %>%
    dplyr::mutate_if(is.character,function(var){stringi::stri_trans_general(str=var,id="Latin-ASCII")}) %>%
    janitor::clean_names()

  return(dic)


}
