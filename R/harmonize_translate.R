

load_dictionary = function(dataset){

  if (dataset == 'pevs_forest_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,

      ## Section 1 - Food

      3402,'Alimenticios (Todos)',NA,'1K BRL','Tons',NA,
      3403,'Acai (Fruto)',NA,'1K BRL','Tons',NA,
      3404,'Castanha de Caju',NA,'1K BRL','Tons',NA,
      3405,'Castanha do Para',NA,'1K BRL','Tons',NA,
      3406,'Erva Mate',NA,'1K BRL','Tons',NA,
      3407,'Mangaba (Fruto)',NA,'1K BRL','Tons',NA,
      3408,'Palmito',NA,'1K BRL','Tons',NA,
      39409, 'Pequi (fruto)',NA,'1K BRL','Tons',NA,
      3409,'Pinhao',NA,'1K BRL','Tons',NA,
      3410,'Umbu (Fruto)',NA,'1K BRL','Tons',NA,
      11296,'Alimenticios (Outros)',NA,'1K BRL','Tons',NA,

      ## Section 2

      3411,'Aromaticos, Medicinais,Toxicos e Corantes',NA,'1K BRL','Tons',NA,
      3412,'Ipecacuanha ou poaia (raiz)',NA,'1K BRL','Tons',NA,
      3413,'Jaborandi (folha)',NA,'1K BRL','Tons',NA,
      3414,'Urucum (semente)',NA,'1K BRL','Tons',NA,
      3415,'Aromaticos, Medicinais,Toxicos e Corantes (Outros)',NA,'1K BRL','Tons',NA,

      ## Section 3 - Rubber

      3416,'Borrachas (Todos)',NA,'1K BRL','Tons',NA,
      3417,'Caucho',NA,'1K BRL','Tons',NA,
      3418,'Hevea (latex coagulado)',NA,'1K BRL','Tons',NA,
      3419,'Hevea (latex liquido)',NA,'1K BRL','Tons',NA,
      40524, 'Mangabeira',NA,'1K BRL','None',NA,

      ## Section 4 - Wax

      3420,'Ceras (Todos)',NA,'1K BRL','Tons',NA,
      3421,'Carnauba (cera)',NA,'1K BRL','Tons',NA,
      3422,'Carnauba (po)',NA,'1K BRL','Tons',NA,
      110011,'Ceras (outras)',NA,'1K BRL','Tons',NA,

      ## Section 5 - Fibers

      3423,'Fibras (Todos)',NA,'1K BRL','Tons',NA,
      3424,'Buriti',NA,'1K BRL','Tons',NA,
      3425,'Carnauba',NA,'1K BRL','Tons',NA,
      3426,'Piacava',NA,'1K BRL','Tons',NA,
      3427,'Fibras (Outros)',NA,'1K BRL','Tons',NA,

      ## Section 6

      3428,'Gomas nao elasticas (Todas)',NA,'1K BRL','Tons',NA,
      3429,'Balata',NA,'1K BRL','Tons',NA,
      3430,'Macaranduba',NA,'1K BRL','Tons',NA,
      3431,'Sorva',NA,'1K BRL','Tons',NA,

      ## Section 7 - Wood

      3433,'Carvao vegetal',NA,'1K BRL','Tons',NA,
      3434,'Lenha',NA,'1K BRL','Cubic Meters',NA,
      3435,'Madeira em tora',NA,'1K BRL','Cubic Meters',NA,

      ## Section 8 - Oilseed

      3438,'Oleaginosos (Todos)',NA,'1K BRL','Tons',NA,
      3439,'Babacu (amendoa)',NA,'1K BRL','Tons',NA,
      3440,'Copaiba (oleo)',NA,'1K BRL','Tons',NA,
      3441,'Cumaru (amendoa)',NA,'1K BRL','Tons',NA,
      3442,'Licuri (coquilho)',NA,'1K BRL','Tons',NA,
      3443,'Oiticica (semente)',NA,'1K BRL','Tons',NA,
      3444,'Pequi (amendoa)',NA,'1K BRL','Tons',NA,
      3445,'Tucum (amendoa)',NA,'1K BRL','Tons',NA,
      3446,'Oleaginosas (Outros)',NA,'1K BRL','Tons',NA,

      ## Section 9 - Pinheiro Brasileiro

      3448,'Pinheiro brasileiro (no de pinho)',NA,'1K BRL','Cubic Meters',NA,
      3449,'Pinheiro brasileiro (arvores abatidas)',NA,'1K BRL','1K Trees',NA,
      3450,'Pinheiro brasileiro (madeira em tora)',NA,'1K BRL','Cubic Meters',NA,

      ## Section 10 - Tanantes

      3451,'Tanantes (Todos)',NA,'1K BRL','Tons',NA,
      3452,'Angico (casca)',NA,'1K BRL','Tons',NA,
      3453, 'Barbatimao (casca)',NA,'1K BRL','Tons',NA,
      3454, 'Tanantes (Outros)',NA,'1K BRL','Tons',NA,

      0,'Total','Total','1K BRL','None',NA)

  }

}
