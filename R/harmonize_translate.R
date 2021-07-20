

load_dictionary = function(dataset){

  #PEVS

  if (dataset == 'pevs_forest_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      ## Section 1 - Food

      3402,'Alimenticios (Todos)','Food Products (All)','1K BRL','Tons',NA, NA,
      3403,'Acai (Fruto)','Acai (Fruit)','1K BRL','Tons',NA,NA,
      3404,'Castanha de Caju','Cashew Nut','1K BRL','Tons',NA,NA,
      3405,'Castanha do Para','Brazil Nut','1K BRL','Tons',NA,NA,
      3406,'Erva Mate','Mate Herb','1K BRL','Tons',NA,NA,
      3407,'Mangaba (Fruto)','Mangaba (Fruit)','1K BRL','Tons',NA,NA,
      3408,'Palmito','Hearts of Palm','1K BRL','Tons',NA,NA,
      39409, 'Pequi (fruto)','Pequi (Fruit)','1K BRL','Tons',NA,NA,
      3409,'Pinhao','Pine nut','1K BRL','Tons',NA,NA,
      3410,'Umbu (Fruto)','Umbu (fruit)','1K BRL','Tons',NA,NA,
      11296,'Alimenticios (Outros)','Food Products (Others)','1K BRL','Tons',NA,NA,

      ## Section 2

      3411,'Aromaticos, Medicinais,Toxicos e Corantes','Aromatic, Medicinal, Toxic and Colorant','1K BRL','Tons',NA,NA,
      3412,'Ipecacuanha ou poaia (raiz)','Ipecacuanha or Ipecac (root)','1K BRL','Tons',NA,NA,
      3413,'Jaborandi (folha)','Jaborandi (leaf)','1K BRL','Tons',NA,NA,
      3414,'Urucum (semente)','Urucum (seed)','1K BRL','Tons',NA,NA,
      3415,'Aromaticos, Medicinais,Toxicos e Corantes (Outros)','Aromatic, Medicinal, Toxic and Colorant (Others)','1K BRL','Tons',NA,NA,

      ## Section 3 - Rubber

      3416,'Borrachas (Todos)','Rubber (All)','1K BRL','Tons',NA,NA,
      3417,'Caucho','Caucho Rubber','1K BRL','Tons',NA,NA,
      3418,'Hevea (latex coagulado)','Hevea (clotted latex)','1K BRL','Tons',NA,NA,
      3419,'Hevea (latex liquido)', 'Hevea (liquid latex)','1K BRL','Tons',NA,NA,
      40524, 'Mangabeira', 'Mangabeira','1K BRL','None',NA,NA,

      ## Section 4 - Wax

      3420,'Ceras (Todos)', 'Waxes (All)','1K BRL','Tons',NA,NA,
      3421,'Carnauba (cera)', 'Carnauba (wax)','1K BRL','Tons',NA,NA,
      3422,'Carnauba (po)', 'Carnauba (dust)','1K BRL','Tons',NA,NA,
      110011,'Ceras (outras)', 'Waxes (others)','1K BRL','Tons',NA,NA,

      ## Section 5 - Fibers

      3423,'Fibras (Todos)','Fibers (All)','1K BRL','Tons',NA,NA,
      3424,'Buriti','Buriti','1K BRL','Tons',NA,NA,
      3425,'Carnauba', 'Carnauba','1K BRL','Tons',NA,NA,
      3426,'Piacava','Piave','1K BRL','Tons',NA,NA,
      3427,'Fibras (Outros)', 'Fibers (Others)','1K BRL','Tons',NA,NA,

      ## Section 6

      3428,'Gomas nao elasticas (Todas)', 'Non-elastic gums (All)','1K BRL','Tons',NA,NA,
      3429,'Balata','Balata','1K BRL','Tons',NA,NA,
      3430,'Macaranduba','Macaranduba','1K BRL','Tons',NA,NA,
      3431,'Sorva','Sip','1K BRL','Tons',NA,NA,

      ## Section 7 - Wood

      3433,'Carvao vegetal', 'Charcoal','1K BRL','Tons',NA,NA,
      3434,'Lenha', 'Firewood','1K BRL','Cubic Meters',NA,NA,
      3435,'Madeira em tora', 'Round wood','1K BRL','Cubic Meters',NA,NA,

      ## Section 8 - Oilseed

      3438,'Oleaginosos (Todos)', 'Oilseeds (All)','1K BRL','Tons',NA,NA,
      3439,'Babacu (amendoa)','Babaco (almond)','1K BRL','Tons',NA,NA,
      3440,'Copaiba (oleo)', 'Copaiba (oil)','1K BRL','Tons',NA,NA,
      3441,'Cumaru (amendoa)', 'Cumaru (almond)','1K BRL','Tons',NA,NA,
      3442,'Licuri (coquilho)', 'Licuri (coquim)','1K BRL','Tons',NA,NA,
      3443,'Oiticica (semente)', 'Oiticica (seed)','1K BRL','Tons',NA,NA,
      3444,'Pequi (amendoa)', 'Pequi (almond)','1K BRL','Tons',NA,NA,
      3445,'Tucum (amendoa)', 'Tucum (almond)','1K BRL','Tons',NA,NA,
      3446,'Oleaginosas (Outros)', 'Oilseeds (Others)','1K BRL','Tons',NA,NA,

      ## Section 9 - Pinheiro Brasileiro

      3448,'Pinheiro brasileiro (no de pinho)', 'Brazilian pine (pine knot)','1K BRL','Cubic Meters',NA,NA,
      3449,'Pinheiro brasileiro (arvores abatidas)', 'Brazilian pine ( felled trees)','1K BRL','1K Trees',NA,NA,
      3450,'Pinheiro brasileiro (madeira em tora)', 'Brazilian pine (round wood)','1K BRL','Cubic Meters',NA,NA,

      ## Section 10 - Tanantes

      3451,'Tanantes (Todos)', 'Tanants (All)' ,'1K BRL','Tons',NA,NA,
      3452,'Angico (casca)', 'Angico (shell)','1K BRL','Tons',NA,NA,
      3453, 'Barbatimao (casca)', 'Barbatimao (bark)','1K BRL','Tons',NA,NA,
      3454, 'Tanantes (Outros)', 'Tanants (Others)','1K BRL','Tons',NA,NA,

      0,'Total','Total','1K BRL','None',NA,NA)

  }


  if (dataset == 'pevs_silviculture'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      ## Section 1

      # 1.1 - Charcoal

      3455,'Carvao Vegetal',NA,'1K BRL','Tons',NA,NA,
      33247,'Carvao Vegetal de Eucalipto',NA,'1K BRL','Tons',NA,NA,
      33248,'Carvao Vegetal de Pinus',NA,'1K BRL','Tons',NA,NA,
      33249,'Carvao Vegetal de Outras Especies',NA,'1K BRL','Tons',NA,NA,

      # 1.2 - Firewood

      3456,'Lenha',NA,'1K BRL','Cubic Meters',NA,NA,
      33250,'Lenha de Eucalipto',NA,'1K BRL','Cubic Meters',NA,NA,
      33251,'Lenha de Pinus',NA,'1K BRL','Cubic Meters',NA,NA,
      33252, 'Lenha de Outras Especies',NA,'1K BRL','Cubic Meters',NA,NA,

      # 1.3 - Wood

      3457,'Madeira em Tora',NA,'1K BRL','Cubic Meters',NA,NA,
      3458,'Madeira em Tora para Papel e Celulose',NA,'1K BRL','Cubic Meters',NA,NA,
      33253,'Madeira em Tora de Eucalipto para Papel e Celulose',NA,'1K BRL','Cubic Meters',NA,NA,
      33254,'Madeira em Tora de Pinus para Papel e Celulose',NA,'1K BRL','Cubic Meters',NA,NA,
      33255,'Madeira em Tora de Outras Especies para Papel e Celulose',NA,'1K BRL','Cubic Meters',NA,NA,
      3459,'Madeira em Tora para Outras Finalidades',NA,'1K BRL','Cubic Meters',NA,NA,
      33256,'Madeira em Tora de Eucalipto para Outras Finalidades',NA,'1K BRL','Cubic Meters',NA,NA,
      33257,'Madeira em Tora de Pinus para Outras Finalidades',NA,'1K BRL','Cubic Meters',NA,NA,
      33258,'Madeira em Tora de Outras Especies para Outras Finalidades',NA,'1K BRL','Cubic Meters',NA,NA,

      ## Section 2 - Other products

      3460,'Outros Produtos',NA,'1K BRL','Tons',NA,NA,
      3461,'Acacia Negra (Casca)',NA,'1K BRL','Tons',NA,NA,
      3462,'Eucalipto (Folha)',NA,'1K BRL','Tons',NA,NA,
      3463,'Resina',NA,'1K BRL','Tons',NA,NA,


      0,'Total','Total','1K BRL','None','None',NA)

  }

  if (dataset == 'pevs_silviculture_area'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      ## Section 1 - Food

      39326,'Eucalipto',NA,NA,'Hectares',NA,NA,
      39327,'Pinus',NA,NA,'Hectares',NA,NA,
      39328,'Outras Especies',NA,NA,'Hectares',NA,NA,


      0,'Total','Total','1K BRL','None','None',NA)

  }

  #PAM

  if (dataset == 'pam_temporary_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      2688,'Abacaxi',NA,'1K BRL','Tons','Hectares',NA,
      40471,'Alfafa Fenada',NA,'1K BRL','Tons','Hectares',NA,
      2689,'Algodao Herbaceo (em Caroco)',NA,'1K BRL','Tons','Hectares',NA,
      2690,'Alho',NA,'1K BRL','Tons','Hectares',NA,
      2691,'Amendoim (em Casca)',NA,'1K BRL','Tons','Hectares',NA,
      2692,'Arroz (em Casca)',NA,'1K BRL','Tons','Hectares',NA,
      2693,'Aveia (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2694,'Batata Doce',NA,'1K BRL','Tons','Hectares',NA,
      2695,'Batata Inglesa',NA,'1K BRL','Tons','Hectares',NA,
      2696,'Cana de Acucar',NA,'1K BRL','Tons','Hectares',NA,
      40470,'Cana para Forragem',NA,'1K BRL','Tons','Hectares',NA,
      2697,'Cebola',NA,'1K BRL','Tons','Hectares',NA,
      2698,'Centeio (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2699,'Cevada (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2700,'Ervilha (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2701,'Fava (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2702,'Feijao (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2703,'Fumo (em Folha)',NA,'1K BRL','Tons','Hectares',NA,
      109179,'Girassol (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2704,'Juta (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      2705,'Linho (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      2706,'Malva (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      2707,'Mamona (Baga)',NA,'1K BRL','Tons','Hectares',NA,
      2708,'Mandioca',NA,'1K BRL','Tons','Hectares',NA,
      2709,'Melancia',NA,'1K BRL','Tons','Hectares',NA,
      2710,'Melao',NA,'1K BRL','Tons','Hectares',NA,
      2711,'Milho (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2712,'Rami (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      2713,'Soja (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2714,'Sorgo (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      2715,'Tomate',NA,'1K BRL','Tons','Hectares',NA,
      2716,'Trigo (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      109180,'Triticale (em Grao)',NA,'1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  if (dataset == 'pam_permanent_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      2717,'Abacate',NA,'1K BRL','Tons','Hectares',NA,
      2718,'Algodao Arboreo (em Caroco)',NA,'1K BRL','Tons','Hectares',NA,
      45981,'Acai',NA,'1K BRL','Tons','Hectares',NA,
      2719,'Azeitona',NA,'1K BRL','Tons','Hectares',NA,
      2720,'Banana (Cacho)',NA,'1K BRL','Tons','Hectares',NA,
      2721,'Borracha (Latex Coagulado)',NA,'1K BRL','Tons','Hectares',NA,
      40472,'Borracha (Latex Liquido)',NA,'1K BRL','Tons','Hectares',NA,
      2722,'Cacau (em Amendoa)',NA,'1K BRL','Tons','Hectares',NA,
      2723,'Cafe (em Grao) Total',NA,'1K BRL','Tons','Hectares',NA,
      31619,'Cafe (em Grao) Arabica',NA,'1K BRL','Tons','Hectares',NA,
      31620,'Cafe (em Grao) Canephora',NA,'1K BRL','Tons','Hectares',NA,
      40473,'Caju',NA,'1K BRL','Tons','Hectares',NA,
      2724,'Caqui',NA,'1K BRL','Tons','Hectares',NA,
      2725,'Castanha de Caju',NA,'1K BRL','Tons','Hectares',NA,
      2726,'Cha da India (Folha Verde)',NA,'1K BRL','Tons','Hectares',NA,
      2727,'Coco da Baia',NA,'1K BRL','Tons','Hectares',NA,
      2728,'Dende (Cacho de Coco)',NA,'1K BRL','Tons','Hectares',NA,
      2729,'Erva Mate (Folha Verde)',NA,'1K BRL','Tons','Hectares',NA,
      2730,'Figo',NA,'1K BRL','Tons','Hectares',NA,
      2731,'Goiaba',NA,'1K BRL','Tons','Hectares',NA,
      2732,'Guarana (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      2733,'Laranja',NA,'1K BRL','Tons','Hectares',NA,
      2734,'Limao',NA,'1K BRL','Tons','Hectares',NA,
      2735,'Maca',NA,'1K BRL','Tons','Hectares',NA,
      2736,'Mamao',NA,'1K BRL','Tons','Hectares',NA,
      2737,'Manga',NA,'1K BRL','Tons','Hectares',NA,
      2738,'Maracuja',NA,'1K BRL','Tons','Hectares',NA,
      2739,'Marmelo',NA,'1K BRL','Tons','Hectares',NA,
      2740,'Noz (Fruto Seco)',NA,'1K BRL','Tons','Hectares',NA,
      90001,'Palmito',NA,'1K BRL','Tons','Hectares',NA,
      2741,'Pera',NA,'1K BRL','Tons','Hectares',NA,
      2742,'Pessego',NA,'1K BRL','Tons','Hectares',NA,
      2743,'Pimenta do Reino',NA,'1K BRL','Tons','Hectares',NA,
      2744,'Sisal ou Agave (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      2745,'Tangerina',NA,'1K BRL','Tons','Hectares',NA,
      2746,'Tungue (Fruto Seco)',NA,'1K BRL','Tons','Hectares',NA,
      2747,'Urucum (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      2748,'Uva',NA,'1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  if (dataset == 'pam_all_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      40129,'Abacate',NA,'1K BRL','Tons','Hectares',NA,
      40092,'Abacaxi',NA,'1K BRL','Tons','Hectares',NA,
      45982,'Acai',NA,'1K BRL','Tons','Hectares',NA,
      40329,'Alfafa Fenada',NA,'1K BRL','Tons','Hectares',NA,
      40130,'Algodao Arboreo (em Caroco)',NA,'1K BRL','Tons','Hectares',NA,
      40099,'Algodao Herbaceo (em Caroco)',NA,'1K BRL','Tons','Hectares',NA,
      40100,'Alho',NA,'1K BRL','Tons','Hectares',NA,
      40101,'Amendoim (em Casca)',NA,'1K BRL','Tons','Hectares',NA,
      40102,'Arroz (em Casca)',NA,'1K BRL','Tons','Hectares',NA,
      40103,'Aveia (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40131,'Azeitona',NA,'1K BRL','Tons','Hectares',NA,
      40136,'Banana (Cacho)',NA,'1K BRL','Tons','Hectares',NA,
      40104,'Batata Doce',NA,'1K BRL','Tons','Hectares',NA,
      40105,'Batata Inglesa',NA,'1K BRL','Tons','Hectares',NA,
      40137,'Borracha (Latex Coagulado)',NA,'1K BRL','Tons','Hectares',NA,
      40468,'Borracha (Latex Liquido)',NA,'1K BRL','Tons','Hectares',NA,
      40138,'Cacau (em Amendoa)',NA,'1K BRL','Tons','Hectares',NA,
      40139,'Cafe (em Grao) Total',NA,'1K BRL','Tons','Hectares',NA,
      40140,'Cafe (em Grao) Arabica',NA,'1K BRL','Tons','Hectares',NA,
      40141,'Cafe (em Grao) Canephora',NA,'1K BRL','Tons','Hectares',NA,
      40330,'Caju',NA,'1K BRL','Tons','Hectares',NA,
      40106,'Cana de Acucar',NA,'1K BRL','Tons','Hectares',NA,
      40331,'Cana para Forragem',NA,'1K BRL','Tons','Hectares',NA,
      40142,'Caqui',NA,'1K BRL','Tons','Hectares',NA,
      40143,'Castanha de Caju',NA,'1K BRL','Tons','Hectares',NA,
      40107,'Cebola',NA,'1K BRL','Tons','Hectares',NA,
      40108,'Centeio (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40109,'Cevada (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40144,'Cha da India (Folha Verde)',NA,'1K BRL','Tons','Hectares',NA,
      40145,'Coco da Baia',NA,'1K BRL','Tons','Hectares',NA,
      40146,'Dende (Cacho de Coco)',NA,'1K BRL','Tons','Hectares',NA,
      40147,'Erva Mate (Folha Verde)',NA,'1K BRL','Tons','Hectares',NA,
      40110,'Ervilha (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40111,'Fava (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40112,'Feijao (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40148,'Figo',NA,'1K BRL','Tons','Hectares',NA,
      40113,'Fumo (em Folha)',NA,'1K BRL','Tons','Hectares',NA,
      40114,'Girassol (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40149,'Goiaba',NA,'1K BRL','Tons','Hectares',NA,
      40150,'Guarana (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      40115,'Juta (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      40151,'Laranja',NA,'1K BRL','Tons','Hectares',NA,
      40152,'Limao',NA,'1K BRL','Tons','Hectares',NA,
      40116,'Linho (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      40260,'Maca',NA,'1K BRL','Tons','Hectares',NA,
      40117,'Malva (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      40261,'Mamao',NA,'1K BRL','Tons','Hectares',NA,
      40118,'Mamona (Baga)',NA,'1K BRL','Tons','Hectares',NA,
      40119,'Mandioca',NA,'1K BRL','Tons','Hectares',NA,
      40262,'Manga',NA,'1K BRL','Tons','Hectares',NA,
      40263,'Maracuja',NA,'1K BRL','Tons','Hectares',NA,
      40264,'Marmelo',NA,'1K BRL','Tons','Hectares',NA,
      40120,'Melancia',NA,'1K BRL','Tons','Hectares',NA,
      40121,'Melao',NA,'1K BRL','Tons','Hectares',NA,
      40122,'Milho (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40265,'Noz (Fruto Seco)',NA,'1K BRL','Tons','Hectares',NA,
      40266,'Palmito',NA,'1K BRL','Tons','Hectares',NA,
      40267,'Pera',NA,'1K BRL','Tons','Hectares',NA,
      40268,'Pessego',NA,'1K BRL','Tons','Hectares',NA,
      40269,'Pimenta do Reino',NA,'1K BRL','Tons','Hectares',NA,
      40123,'Rami (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      40270,'Sisal ou Agave (Fibra)',NA,'1K BRL','Tons','Hectares',NA,
      40124,'Soja (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40125,'Sorgo (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40271,'Tangerina',NA,'1K BRL','Tons','Hectares',NA,
      40126,'Tomate',NA,'1K BRL','Tons','Hectares',NA,
      40127,'Trigo (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40128,'Triticale (em Grao)',NA,'1K BRL','Tons','Hectares',NA,
      40272,'Tungue (Fruto Seco)',NA,'1K BRL','Tons','Hectares',NA,
      40273,'Urucum (Semente)',NA,'1K BRL','Tons','Hectares',NA,
      40274,'Uva',NA,'1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  # Crops with more than one harvest (Corn, Potato, Peanut or Beans)

  if (dataset == 'pam_corn'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      114253,'Milho em Grao (Primeira Safra)',NA,'None','Tons','Hectares',NA,
      114254,'Milho em Grao (Segunda Safra)',NA,'None','Tons','Hectares',NA,

      31693,'Total','Total','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_potato'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117989,'Batata Inglesa (Primeira Safra)',NA,'None','Tons','Hectares',NA,
      117990,'Batata Inglesa (Segunda Safra)',NA,'None','Tons','Hectares',NA,
      117994,'Batata Inglesa (Terceiraa Safra)',NA,'None','Tons','Hectares',NA,

      31693,'Total','Total','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_peanut'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117987,'Amendoim em Casca (Primeira Safra)',NA,'None','Tons','Hectares',NA,
      117988,'Amendoim em Casca (Segunda Safra)',NA,'None','Tons','Hectares',NA,

      31693,'Total','Total','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_beans'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117991,'Feijao em Grao (Primeira Safra)',NA,'None','Tons','Hectares',NA,
      117992,'Feijao em Grao (Segunda Safra)',NA,'None','Tons','Hectares',NA,
      117993,'Feijao em Grao (Terceira Safra)',NA,'None','Tons','Hectares',NA,

      31693,'Total','Total','None','Tons','Hectares',NA)

  }
}
