load_dictionary = function(dataset){

  ##########
  ## PEVS ##
  ##########

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

      3455,'Carvao Vegetal','Charcoal','1K BRL','Tons',NA,NA,
      33247,'Carvao Vegetal de Eucalipto','Eucalyptus Charcoal','1K BRL','Tons',NA,NA,
      33248,'Carvao Vegetal de Pinus','Pine Charcoal','1K BRL','Tons',NA,NA,
      33249,'Carvao Vegetal de Outras Especies','Charcoal of Other Species','1K BRL','Tons',NA,NA,

      # 1.2 - Firewood

      3456,'Lenha','firewood','1K BRL','Cubic Meters',NA,NA,
      33250,'Lenha de Eucalipto','eucalyptus firewood','1K BRL','Cubic Meters',NA,NA,
      33251,'Lenha de Pinus','pine firewood','1K BRL','Cubic Meters',NA,NA,
      33252, 'Lenha de Outras Especies','Firewood from Other Species','1K BRL','Cubic Meters',NA,NA,

      # 1.3 - Wood

      3457,'Madeira em Tora','log wood','1K BRL','Cubic Meters',NA,NA,
      3458,'Madeira em Tora para Papel e Celulose','Round Wood for Paper and Cellulose','1K BRL','Cubic Meters',NA,NA,
      33253,'Madeira em Tora de Eucalipto para Papel e Celulose','Eucalyptus Log Wood for Pulp and Paper','1K BRL','Cubic Meters',NA,NA,
      33254,'Madeira em Tora de Pinus para Papel e Celulose','Pine Log Wood for Paper and Cellulose','1K BRL','Cubic Meters',NA,NA,
      33255,'Madeira em Tora de Outras Especies para Papel e Celulose','Round Wood from Other Species for Pulp and Paper','1K BRL','Cubic Meters',NA,NA,
      3459,'Madeira em Tora para Outras Finalidades','Round Wood for Other Purposes','1K BRL','Cubic Meters',NA,NA,
      33256,'Madeira em Tora de Eucalipto para Outras Finalidades','Eucalyptus Log Wood for Other Purposes','1K BRL','Cubic Meters',NA,NA,
      33257,'Madeira em Tora de Pinus para Outras Finalidades','Pine Round Wood for Other Purposes','1K BRL','Cubic Meters',NA,NA,
      33258,'Madeira em Tora de Outras Especies para Outras Finalidades','Round Wood of Other Species for Other Purposes','1K BRL','Cubic Meters',NA,NA,

      ## Section 2 - Other products

      3460,'Outros Produtos','Other products','1K BRL','Tons',NA,NA,
      3461,'Acacia Negra (Casca)','Black Acacia (Bark)','1K BRL','Tons',NA,NA,
      3462,'Eucalipto (Folha)','Eucalyptus (Leaf)','1K BRL','Tons',NA,NA,
      3463,'Resina','Resin','1K BRL','Tons',NA,NA,


      0,'Total','Total','1K BRL','None','None',NA)

  }

  if (dataset == 'pevs_silviculture_area'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      ## Section 1 - Food

      39326,'Eucalipto','Eucalyptus',NA,'Hectares',NA,NA,
      39327,'Pinus','pine',NA,'Hectares',NA,NA,
      39328,'Outras Especies','Other Species',NA,'Hectares',NA,NA,


      0,'Total','Total','1K BRL','None','None',NA)

  }


  #########
  ## PPM ##
  #########


  if (dataset == 'ppm_animal_origin_production'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,


      2682,'Leite', 'Milk','1L BRL','1K Liters',NA,
      2683,'Casulos do bicho-da-seda', 'Silkworm cocoons','1K BRL','Kg',NA,
      2684,'La','Wool','1L BRL','Kg',NA,
      2685,'Ovos de galinha', 'Chicken eggs','1K BRL','1K Dozens',NA,
      2686,'Ovos de codorna', 'Quail eggs','1K BRL','1K Dozens',NA,
      2687,'Mel de abelha', 'Bee Honey','1K BRL','Kg',NA,

      0,'Total','Total','1K BRL','None',NA)

  }

  if (dataset == 'ppm_livestock_inventory'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,

      2670,'Bovino', 'Bovine','None','Cattle',NA,
      2675,'Bubalino', 'Buffalo','None','Cattle',NA,
      2672,'Equino', 'Equine', 'None','Cattle',NA,
      32794,'Suino - Total', 'Swine - Total','None','Cattle',NA,
      32795,'Suino - Matrizes de Suinos', 'Swine - Breeders','None','Cattle',NA,
      2681,'Caprino', 'Goat','None','Cattle',NA,
      2677,'Ovino', 'Sheep','None','Cattle',NA,
      32796,'Galinaceos - Total', 'Galinaceous - total','None','Cattle',NA,
      32793,'Galinaceos - Galinhas', 'Galinaceos - chickens','None','Cattle',NA,
      2680,'Codornas', 'Quail','None','Cattle',NA,

      0,'Total','Total','1K BRL','None',NA)
  }

  if (dataset == 'ppm_sheep_farming'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,

      108,'Ovinos tosquiados', 'Sheared sheep','None','Cattle',NA,

      0,'Total','Total','1K BRL','None',NA)
  }

  if (dataset == 'ppm_cow_farming'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,

      107,'Vacas ordenhadas', 'Milked cows','None','Cattle',NA,

      0,'Total','Total','1K BRL','None',NA)
  }

  if (dataset == 'ppm_aquaculture'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~available_period,

      32861,'Carpa', 'Carp', '1K BRL','Kg',NA,
      32865,'Curimata, curimbata', 'curimbata, curimbata', '1K BRL', 'Kg',NA,
      32866,'Dourado', 'Dourado', '1K BRL', 'Kg',NA,
      32867,'Jatuarana, piabanha e piracanjuba', 'Jatuarana, piabanha and piracanjuba', '1K BRL', 'Kg',NA,
      32868,'Lambari', 'Lambari', '1K BRL', 'Kg',NA,
      32869,'Matrinxa', 'Matrinxa', '1K BRL', 'Kg',NA,
      32870,'Pacu e patinga', 'Pacu and skate', '1K BRL', 'Kg',NA,
      32871,'Piau, piapara, piaucu, piava', 'Piau, piapara, piaucu, chirping', '1K BRL', 'Kg',NA,
      32872,'Pintado, cachara, cachapira e pintachara, surubim', 'Pintado, cachara, cachapira and pintachara, surubim', '1K BRL', 'Kg',NA,
      32873,'Pirapitinga', 'Pirapitinga', '1K BRL', 'Kg',NA,
      32874,'Pirarucu', 'Pirarucu', '1K BRL', 'Kg',NA,
      32875,'Tambacu, tambatinga', 'tambacu, tambatinga', '1K BRL', 'Kg',NA,
      32876,'Tambaqui', 'Tambaqui', '1K BRL', 'Kg',NA,
      32877,'Tilapia', 'Tilapia', '1K BRL', 'Kg',NA,
      32878,'Traira e trairao', 'Traira e trairao', '1K BRL', 'Kg',NA,
      32879,'Truta', 'Trout', '1K BRL', 'Kg',NA,
      32880,'Tucunare', 'Peacock bass', '1K BRL', 'Kg',NA,
      32881,'Outros peixes', 'Other fish', '1K BRL', 'Kg',NA,
      32886,'Alevinos', 'Fingerlings', '1K BRL', 'Kg',NA,
      32887,'Camarao', 'Shrimp', '1K BRL', 'Kg',NA,
      32888,'Larvas e pos-larvas de camarao', 'Shrimp larvae and post-larvae', '1K BRL','1K',NA,
      32889,'Ostras, vieiras e mexilhoes', 'Oysters, scallops and mussels', '1K BRL', 'Kg',NA,
      32890,'Sementes de moluscos', 'Mollusc Seeds', '1K BRL','1K',NA,
      32891,'Outros produtos (ra, jacare, siri, caranguejo, lagosta, etc)', 'Other products (ra, alligator, crab, crab, lobster, etc.)', '1K BRL', 'None',NA,


      0,'Total','Total','1K BRL','None',NA)
  }

  #######
  # PAM #
  #######


  if (dataset == 'pam_temporary_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      2688,'Abacaxi','Pineapple','1K BRL','Tons','Hectares',NA,
      40471,'Alfafa Fenada', 'Alfafa Fenada','1K BRL','Tons','Hectares',NA,
      2689,'Algodao Herbaceo (em Caroco)', 'Herbaceous Cotton (in Caroco)','1K BRL','Tons','Hectares',NA,
      2690,'Alho', 'Garlic','1K BRL','Tons','Hectares',NA,
      2691,'Amendoim (em Casca)', 'Peanuts (in Shell)','1K BRL','Tons','Hectares',NA,
      2692,'Arroz (em Casca)', 'Rice (in husk)','1K BRL','Tons','Hectares',NA,
      2693,'Aveia (em Grao)', 'Oats (in grain)','1K BRL','Tons','Hectares',NA,
      2694,'Batata Doce','Sweet potato','1K BRL','Tons','Hectares',NA,
      2695,'Batata Inglesa','English potato','1K BRL','Tons','Hectares',NA,
      2696,'Cana de Acucar', 'Sugar cane','1K BRL','Tons','Hectares',NA,
      40470,'Cana para Forragem', 'Forage cane','1K BRL','Tons','Hectares',NA,
      2697,'Cebola','Onion','1K BRL','Tons','Hectares',NA,
      2698,'Centeio (em Grao)', 'Rye (in grain)','1K BRL','Tons','Hectares',NA,
      2699,'Cevada (em Grao)', 'Barley (in Grain)','1K BRL','Tons','Hectares',NA,
      2700,'Ervilha (em Grao)', 'Pea (in Grain)','1K BRL','Tons','Hectares',NA,
      2701,'Fava (em Grao)', 'Broad Bean (in Grain)','1K BRL','Tons','Hectares',NA,
      2702,'Feijao (em Grao)','Beans (in Grain)','1K BRL','Tons','Hectares',NA,
      2703,'Fumo (em Folha)', 'Smoke (in Sheet)','1K BRL','Tons','Hectares',NA,
      109179,'Girassol (em Grao)', 'Sunflower (in Grain)','1K BRL','Tons','Hectares',NA,
      2704,'Juta (Fibra)', 'Jute (Fiber)','1K BRL','Tons','Hectares',NA,
      2705,'Linho (Semente)', 'Linen (Seed)','1K BRL','Tons','Hectares',NA,
      2706,'Malva (Fibra)', 'Malva (Fiber)','1K BRL','Tons','Hectares',NA,
      2707,'Mamona (Baga)', 'Castor bean (Berry)','1K BRL','Tons','Hectares',NA,
      2708,'Mandioca','Cassava','1K BRL','Tons','Hectares',NA,
      2709,'Melancia','watermelon','1K BRL','Tons','Hectares',NA,
      2710,'Melao', 'Melon','1K BRL','Tons','Hectares',NA,
      2711,'Milho (em Grao)','corn (in grain)','1K BRL','Tons','Hectares',NA,
      2712,'Rami (Fibra)','Ramie (Fiber)','1K BRL','Tons','Hectares',NA,
      2713,'Soja (em Grao)','Soybean (in grain)','1K BRL','Tons','Hectares',NA,
      2714,'Sorgo (em Grao)','Sorghum (in Grain)','1K BRL','Tons','Hectares',NA,
      2715,'Tomate','Tomato','1K BRL','Tons','Hectares',NA,
      2716,'Trigo (em Grao)','Wheat in grain)','1K BRL','Tons','Hectares',NA,
      109180,'Triticale (em Grao)','Triticale (in grain)','1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  if (dataset == 'pam_permanent_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      2717,'Abacate', 'Avocado','1K BRL','Tons','Hectares',NA,
      2718,'Algodao Arboreo (em Caroco)','Arboreo cotton (in Caroco)','1K BRL','Tons','Hectares',NA,
      45981,'Acai','Acai','1K BRL','Tons','Hectares',NA,
      2719,'Azeitona','Olive','1K BRL','Tons','Hectares',NA,
      2720,'Banana (Cacho)', 'Banana (Bunch)','1K BRL','Tons','Hectares',NA,
      2721,'Borracha (Latex Coagulado)','Rubber (Coagulated Latex)','1K BRL','Tons','Hectares',NA,
      40472,'Borracha (Latex Liquido)', 'Rubber (Liquid Latex)','1K BRL','Tons','Hectares',NA,
      2722,'Cacau (em Amendoa)','Cocoa (in Almonds)','1K BRL','Tons','Hectares',NA,
      2723,'Cafe (em Grao) Total', 'Coffee (in Grain) Total','1K BRL','Tons','Hectares',NA,
      31619,'Cafe (em Grao) Arabica', 'Cafe (in Grao) Arabica','1K BRL','Tons','Hectares',NA,
      31620,'Cafe (em Grao) Canephora', 'Cafe (in Grain) Canephora','1K BRL','Tons','Hectares',NA,
      40473,'Caju','Cashew','1K BRL','Tons','Hectares',NA,
      2724,'Caqui','Khaki','1K BRL','Tons','Hectares',NA,
      2725,'Castanha de Caju','Cashew Nuts','1K BRL','Tons','Hectares',NA,
      2726,'Cha da India (Folha Verde)','India Tea (Leaf)','1K BRL','Tons','Hectares',NA,
      2727,'Coco da Baia','Coconut','1K BRL','Tons','Hectares',NA,
      2728,'Dende (Cacho de Coco)','Coconut Bunch','1K BRL','Tons','Hectares',NA,
      2729,'Erva Mate (Folha Verde)','Mate Herb (Leaf)','1K BRL','Tons','Hectares',NA,
      2730,'Figo','Fig','1K BRL','Tons','Hectares',NA,
      2731,'Goiaba','Guava','1K BRL','Tons','Hectares',NA,
      2732,'Guarana (Semente)','Guarana (Seed)','1K BRL','Tons','Hectares',NA,
      2733,'Laranja','Orange','1K BRL','Tons','Hectares',NA,
      2734,'Limao','Lemon','1K BRL','Tons','Hectares',NA,
      2735,'Maca','Apple','1K BRL','Tons','Hectares',NA,
      2736,'Mamao','Papaya','1K BRL','Tons','Hectares',NA,
      2737,'Manga','Mango','1K BRL','Tons','Hectares',NA,
      2738,'Maracuja','Passion fruit','1K BRL','Tons','Hectares',NA,
      2739,'Marmelo','Quince','1K BRL','Tons','Hectares',NA,
      2740,'Noz (Fruto Seco)','Walnut (Dry Fruit)','1K BRL','Tons','Hectares',NA,
      90001,'Palmito','Palm heart','1K BRL','Tons','Hectares',NA,
      2741,'Pera', 'Pear','1K BRL','Tons','Hectares',NA,
      2742,'Pessego', 'Peach','1K BRL','Tons','Hectares',NA,
      2743,'Pimenta do Reino','Black pepper','1K BRL','Tons','Hectares',NA,
      2744,'Sisal ou Agave (Fibra)','Sisal or Agave (Fiber)','1K BRL','Tons','Hectares',NA,
      2745,'Tangerina','Tangerine','1K BRL','Tons','Hectares',NA,
      2746,'Tungue (Fruto Seco)','Tung (Dry Fruit)','1K BRL','Tons','Hectares',NA,
      2747,'Urucum (Semente)','Annatto (Seed)','1K BRL','Tons','Hectares',NA,
      2748,'Uva','Grape','1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  if (dataset == 'pam_all_crops'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      40129,'Abacate','Avocado','1K BRL','Tons','Hectares',NA,
      40092,'Abacaxi','Pineapple','1K BRL','Tons','Hectares',NA,
      45982,'Acai','Acai','1K BRL','Tons','Hectares',NA,
      40329,'Alfafa Fenada','Fenada Alfalfa','1K BRL','Tons','Hectares',NA,
      40130,'Algodao Arboreo (em Caroco)','Arboreo cotton (in Caroco)','1K BRL','Tons','Hectares',NA,
      40099,'Algodao Herbaceo (em Caroco)','Herbaceous Cotton (in Caroco)','1K BRL','Tons','Hectares',NA,
      40100,'Alho','Garlic','1K BRL','Tons','Hectares',NA,
      40101,'Amendoim (em Casca)','Peanuts (in Shell)','1K BRL','Tons','Hectares',NA,
      40102,'Arroz (em Casca)','Rice (in husk)','1K BRL','Tons','Hectares',NA,
      40103,'Aveia (em Grao)','Oats (in grain)','1K BRL','Tons','Hectares',NA,
      40131,'Azeitona','Olive','1K BRL','Tons','Hectares',NA,
      40136,'Banana (Cacho)','Banana (Bunch)','1K BRL','Tons','Hectares',NA,
      40104,'Batata Doce','Sweet potato','1K BRL','Tons','Hectares',NA,
      40105,'Batata Inglesa','English potato','1K BRL','Tons','Hectares',NA,
      40137,'Borracha (Latex Coagulado)','Rubber (Coagulated Latex)','1K BRL','Tons','Hectares',NA,
      40468,'Borracha (Latex Liquido)','Rubber (Liquid Latex)','1K BRL','Tons','Hectares',NA,
      40138,'Cacau (em Amendoa)','Cocoa (in Almonds)','1K BRL','Tons','Hectares',NA,
      40139,'Cafe (em Grao) Total','Coffee (in Grain) Total','1K BRL','Tons','Hectares',NA,
      40140,'Cafe (em Grao) Arabica','Cafe (in Grao) Arabica','1K BRL','Tons','Hectares',NA,
      40141,'Cafe (em Grao) Canephora','Cafe (in Grain) Canephora','1K BRL','Tons','Hectares',NA,
      40330,'Caju','cashew','1K BRL','Tons','Hectares',NA,
      40106,'Cana de Acucar','Sugar cane','1K BRL','Tons','Hectares',NA,
      40331,'Cana para Forragem','Forage cane','1K BRL','Tons','Hectares',NA,
      40142,'Caqui','Khaki','1K BRL','Tons','Hectares',NA,
      40143,'Castanha de Caju','Cashew Nut','1K BRL','Tons','Hectares',NA,
      40107,'Cebola','Onion','1K BRL','Tons','Hectares',NA,
      40108,'Centeio (em Grao)','Rye (in grain)','1K BRL','Tons','Hectares',NA,
      40109,'Cevada (em Grao)','Barley (in Grain)','1K BRL','Tons','Hectares',NA,
      40144,'Cha da India (Folha Verde)','India Tea (Green Leaf)','1K BRL','Tons','Hectares',NA,
      40145,'Coco da Baia','Coconut','1K BRL','Tons','Hectares',NA,
      40146,'Dende (Cacho de Coco)','Dende (Coconut Bunch)','1K BRL','Tons','Hectares',NA,
      40147,'Erva Mate (Folha Verde)','Yerba Mate (Green Leaf)','1K BRL','Tons','Hectares',NA,
      40110,'Ervilha (em Grao)','Pea (in Grain)','1K BRL','Tons','Hectares',NA,
      40111,'Fava (em Grao)','Broad Bean (in Grain)','1K BRL','Tons','Hectares',NA,
      40112,'Feijao (em Grao)','Beans (in Grain)','1K BRL','Tons','Hectares',NA,
      40148,'Figo','Fig','1K BRL','Tons','Hectares',NA,
      40113,'Fumo (em Folha)','Smoke (in Sheet)','1K BRL','Tons','Hectares',NA,
      40114,'Girassol (em Grao)', 'Sunflower (in Grain)','1K BRL','Tons','Hectares',NA,
      40149,'Goiaba','Guava','1K BRL','Tons','Hectares',NA,
      40150,'Guarana (Semente)','Guarana (Seed)','1K BRL','Tons','Hectares',NA,
      40115,'Juta (Fibra)','Jute (Fiber)','1K BRL','Tons','Hectares',NA,
      40151,'Laranja','Orange','1K BRL','Tons','Hectares',NA,
      40152,'Limao','lemon','1K BRL','Tons','Hectares',NA,
      40116,'Linho (Semente)','Linen (Seed)','1K BRL','Tons','Hectares',NA,
      40260,'Maca','Apple','1K BRL','Tons','Hectares',NA,
      40117,'Malva (Fibra)', 'Malva (Fiber)','1K BRL','Tons','Hectares',NA,
      40261,'Mamao','Papaya','1K BRL','Tons','Hectares',NA,
      40118,'Mamona (Baga)', 'Castor bean (Berry)','1K BRL','Tons','Hectares',NA,
      40119,'Mandioca','Cassava','1K BRL','Tons','Hectares',NA,
      40262,'Manga','Mango','1K BRL','Tons','Hectares',NA,
      40263,'Maracuja','Passion fruit','1K BRL','Tons','Hectares',NA,
      40264,'Marmelo','Quince','1K BRL','Tons','Hectares',NA,
      40120,'Melancia','watermelon','1K BRL','Tons','Hectares',NA,
      40121,'Melao','Melon','1K BRL','Tons','Hectares',NA,
      40122,'Milho (em Grao)','corn (in grain)','1K BRL','Tons','Hectares',NA,
      40265,'Noz (Fruto Seco)','Walnut (Dry Fruit)','1K BRL','Tons','Hectares',NA,
      40266,'Palmito','Palm heart','1K BRL','Tons','Hectares',NA,
      40267,'Pera','Pear','1K BRL','Tons','Hectares',NA,
      40268,'Pessego','Peach','1K BRL','Tons','Hectares',NA,
      40269,'Pimenta do Reino','Black pepper','1K BRL','Tons','Hectares',NA,
      40123,'Rami (Fibra)','Ramie (Fiber)','1K BRL','Tons','Hectares',NA,
      40270,'Sisal ou Agave (Fibra)','Sisal or Agave (Fiber)','1K BRL','Tons','Hectares',NA,
      40124,'Soja (em Grao)','Soybean (in grain)','1K BRL','Tons','Hectares',NA,
      40125,'Sorgo (em Grao)','Sorghum (in Grain)','1K BRL','Tons','Hectares',NA,
      40271,'Tangerina','Tangerine','1K BRL','Tons','Hectares',NA,
      40126,'Tomate','Tomato','1K BRL','Tons','Hectares',NA,
      40127,'Trigo (em Grao)','Wheat (in grain)','1K BRL','Tons','Hectares',NA,
      40128,'Triticale (em Grao)','Triticale (in grain)','1K BRL','Tons','Hectares',NA,
      40272,'Tungue (Fruto Seco)','Tung (Dry Fruit)','1K BRL','Tons','Hectares',NA,
      40273,'Urucum (Semente)','Annatto (Seed)','1K BRL','Tons','Hectares',NA,
      40274,'Uva','Grape','1K BRL','Tons','Hectares',NA,

      0,'Total','Total','1K BRL','Tons','Hectares',NA)

  }

  # Crops with more than one harvest (Corn, Potato, Peanut or Beans)

  if (dataset == 'pam_corn'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      114253,'Milho em Grao (Primeira Safra)','Corn in Grain (First Crop)','None','Tons','Hectares',NA,
      114254,'Milho em Grao (Segunda Safra)','Corn in Grain (Second Crop)','None','Tons','Hectares',NA,

      31693,'Milho em Grao (Total)','Corn in Grain (Total)','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_potato'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117989,'Batata Inglesa (Primeira Safra)','English Potato (First Crop)','None','Tons','Hectares',NA,
      117990,'Batata Inglesa (Segunda Safra)','English Potato (Second Crop)','None','Tons','Hectares',NA,
      117994,'Batata Inglesa (Terceira Safra)', 'English Potato (Third Crop)','None','Tons','Hectares',NA,

      31693,'Batata Inglesa (Total)','English Potato (Total)','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_peanut'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117987,'Amendoim em Casca (Primeira Safra)', 'Peanuts in Shell (First Crop)','None','Tons','Hectares',NA,
      117988,'Amendoim em Casca (Segunda Safra)','Peanuts in Shell (Second Crop)','None','Tons','Hectares',NA,

      31693,'Amendoim em Casca (Total)','Peanuts in Shell (Total)','None','Tons','Hectares',NA)

  }

  if (dataset == 'pam_beans'){

    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,~unit_area,~available_period,

      117991,'Feijao em Grao (Primeira Safra)','Beans in Grain (First Crop)','None','Tons','Hectares',NA,
      117992,'Feijao em Grao (Segunda Safra)','Beans in Grain (Second Crop)','None','Tons','Hectares',NA,
      117993,'Feijao em Grao (Terceira Safra)', 'Beans in Grain (Third Crop)','None','Tons','Hectares',NA,

      31693,'Feijao em Grao (Total)','Beans in Grain (Total)','None','Tons','Hectares',NA)

  }

  ########
  # Cempre
  ########

  if (dataset == 'cempre'){

    harmonization_dat = tibble::tribble(
      ~var_code,~id_code,~var_pt,~var_eng,~unit_value,~unit_quantity,

      117897,'117897_1','Total','Total','1K','Units',
      117897,'117897_2','Total','Total', '1K','People',
      117897,'117897_3','Total','Total','1K','People',
      117897,'117897_4','Total','Total','1K BRL',NA,
      116830,'116830_1','A Agricultura, pecuaria, producao florestal, pesca e aquicultura','Agriculture, livestock, forestry, fishing and aquaculture','1K','Units',
      116830,'116830_2','A Agricultura, pecuaria, producao florestal, pesca e aquicultura','Agriculture, livestock, forestry, fishing and aquaculture','1K','People',
      116830,'116830_3','A Agricultura, pecuaria, producao florestal, pesca e aquicultura','Agriculture, livestock, forestry, fishing and aquaculture','1K','People',
      116830,'116830_4','A Agricultura, pecuaria, producao florestal, pesca e aquicultura','Agriculture, livestock, forestry, fishing and aquaculture','1K BRL',NA,
      116880,'116880_1','Industrias extrativas','Extractive industries','1K','Units',
      116880,'116880_2','Industrias extrativas','Extractive industries','1K','People',
      116880,'116880_3','Industrias extrativas','Extractive industries','1K','People',
      116880,'116880_4','Industrias extrativas','Extractive industries','1K BRL',NA,
      116910,'116910_1','Industrias de transformacao','Manufacturing industries','1K','Units',
      116910,'116910_2','Industrias de transformacao','Manufacturing industries','1K','People',
      116910,'116910_3','Industrias de transformacao','Manufacturing industries','1K','People',
      116910,'116910_4','Industrias de transformacao','Manufacturing industries','1K BRL',NA,
      117296,'117296_1','Eletricidade e gas','Electricity and gas','1K','Units',
      117296,'117296_2','Eletricidade e gas','Electricity and gas','1K','People',
      117296,'117296_3','Eletricidade e gas','Electricity and gas','1K','People',
      117296,'117296_4','Eletricidade e gas','Electricity and gas','1K BRL',NA,
      117307,'117307_1','Agua, esgoto, atividades de gestao e residuos e descontaminacao','Water, sewage, waste management and decontamination activities','1K','Units',
      117307,'117307_2','Agua, esgoto, atividades de gestao e residuos e descontaminacao','Water, sewage, waste management and decontamination activities','1K','People',
      117307,'117307_3','Agua, esgoto, atividades de gestao e residuos e descontaminacao','Water, sewage, waste management and decontamination activities','1K','People',
      117307,'117307_4','Agua, esgoto, atividades de gestao e residuos e descontaminacao','Water, sewage, waste management and decontamination activities','1K BRL',NA,
      117329,'117329_1','Construcao','Construction','1K','Units',
      117329,'117329_2','Construcao','Construction','1K','People',
      117329,'117329_3','Construcao','Construction','1K','People',
      117329,'117329_4','Construcao','Construction','1K BRL',NA,
      117363,'117363_1','Comercio: reparacao de veiculos automotores e motocicletas','Business; repair of motor vehicles and motorcycles','1K','Units',
      117363,'117363_2','Comercio: reparacao de veiculos automotores e motocicletas','Business; repair of motor vehicles and motorcycles','1K','People',
      117363,'117363_3','Comercio: reparacao de veiculos automotores e motocicletas','Business; repair of motor vehicles and motorcycles','1K','People',
      117363,'117363_4','Comercio: reparacao de veiculos automotores e motocicletas','Business; repair of motor vehicles and motorcycles','1K BRL',NA,
      117484,'117484_1','Transporte, armazenagem e correio','Transport, storage and mail','1K','Units',
      117484,'117484_2','Transporte, armazenagem e correio','Transport, storage and mail','1K','People',
      117484,'117484_3','Transporte, armazenagem e correio','Transport, storage and mail','1K','People',
      117484,'117484_4','Transporte, armazenagem e correio','Transport, storage and mail','1K BRL',NA,
      117543,'117543_1','Alojamento e alimentacao','Accommodation and food','1K','Units',
      117543,'117543_2','Alojamento e alimentacao','Accommodation and food','1K','People',
      117543,'117543_3','Alojamento e alimentacao','Accommodation and food','1K','People',
      117543,'117543_4','Alojamento e alimentacao','Accommodation and food','1K BRL',NA,
      117555,'117555_1','Informacao e comunicacao','Information and communication','1K','Units',
      117555,'117555_2','Informacao e comunicacao','Information and communication','1K','People',
      117555,'117555_3','Informacao e comunicacao','Information and communication','1K','People',
      117555,'117555_4','Informacao e comunicacao','Information and communication','1K BRL',NA,
      117608,'117608_1','Atividades financeiras, de seguros e servicos relacionados','Financial, insurance and related services activities','1K','Units',
      117608,'117608_2','Atividades financeiras, de seguros e servicos relacionados','Financial, insurance and related services activities','1K','People',
      117608,'117608_3','Atividades financeiras, de seguros e servicos relacionados','Financial, insurance and related services activities','1K','People',
      117608,'117608_4','Atividades financeiras, de seguros e servicos relacionados','Financial, insurance and related services activities','1K BRL',NA,
      117666,'117666_1','Atividades imobiliarias','Real estate activities','1K','Units',
      117666,'117666_2','Atividades imobiliarias','Real estate activities','1K','People',
      117666,'117666_3','Atividades imobiliarias','Real estate activities','1K','People',
      117666,'117666_4','Atividades imobiliarias','Real estate activities','1K BRL',NA,
      117673,'117673_1','Atividades profissionais, cientificas e tecnicas','Professional, scientific and technical activities','1K','Units',
      117673,'117673_2','Atividades profissionais, cientificas e tecnicas','Professional, scientific and technical activities','1K','People',
      117673,'117673_3','Atividades profissionais, cientificas e tecnicas','Professional, scientific and technical activities','1K','People',
      117673,'117673_4','Atividades profissionais, cientificas e tecnicas','Professional, scientific and technical activities','1K BRL',NA,
      117714,'117714_1','Atividades administrativas e servicos complementares','Administrative activities and complementary services','1K','Units',
      117714,'117714_2','Atividades administrativas e servicos complementares','Administrative activities and complementary services','1K','People',
      117714,'117714_3','Atividades administrativas e servicos complementares','Administrative activities and complementary services','1K','People',
      117714,'117714_4','Atividades administrativas e servicos complementares','Administrative activities and complementary services','1K BRL',NA,
      117774,'117774_1','Administracao publica, defesa e seguridade social','Public administration, defense and social security','1K','Units',
      117774,'117774_2','Administracao publica, defesa e seguridade social','Public administration, defense and social security','1K','People',
      117774,'117774_3','Administracao publica, defesa e seguridade social','Public administration, defense and social security','1K','People',
      117774,'117774_4','Administracao publica, defesa e seguridade social','Public administration, defense and social security','1K BRL',NA,
      117788,'117788_1','Educacao','Education','1K','Units',
      117788,'117788_2','Educacao','Education','1K','People',
      117788,'117788_3','Educacao','Education','1K','People',
      117788,'117788_4','Educacao','Education','1K BRL',NA,
      117810,'117810_1','Saude humana e servicos sociais','Human health and social services','1K','Units',
      117810,'117810_2','Saude humana e servicos sociais','Human health and social services','1K','People',
      117810,'117810_3','Saude humana e servicos sociais','Human health and social services','1K','People',
      117810,'117810_4','Saude humana e servicos sociais','Human health and social services','1K BRL',NA,
      117838,'117838_1','Artes, cultura, esporte e recreacao','Arts, culture, sports and recreation','1K','Units',
      117838,'117838_2','Artes, cultura, esporte e recreacao','Arts, culture, sports and recreation','1K','People',
      117838,'117838_3','Artes, cultura, esporte e recreacao','Arts, culture, sports and recreation','1K','People',
      117838,'117838_4','Artes, cultura, esporte e recreacao','Arts, culture, sports and recreation','1K BRL',NA,
      117861,'117861_1','Outras atividades de servicos','Other service related activities','1K','Units',
      117861,'117861_2','Outras atividades de servicos','Other service related activities','1K','People',
      117861,'117861_3','Outras atividades de servicos','Other service related activities','1K','People',
      117861,'117861_4','Outras atividades de servicos','Other service related activities','1K BRL',NA,
      117888,'117888_1','Servicos domesticos','Domestic services','1K','Units',
      117888,'117888_2','Servicos domesticos','Domestic services','1K','People',
      117888,'117888_3','Servicos domesticos','Domestic services','1K','People',
      117888,'117888_4','Servicos domesticos','Domestic services','1K BRL',NA,
      117892,'117892_1','Organismos internacionais e outras instituicoes extraterritoriais','International organizations and other extraterritorial institutions','1K','Units',
      117892,'117892_2','Organismos internacionais e outras instituicoes extraterritoriais','International organizations and other extraterritorial institutions','1K','People',
      117892,'117892_3','Organismos internacionais e outras instituicoes extraterritoriais','International organizations and other extraterritorial institutions','1K','People',
      117892,'117892_4','Organismos internacionais e outras instituicoes extraterritoriais','International organizations and other extraterritorial institutions','1K BRL',NA,


    )}

  if(dataset == "pibmunic"){
    harmonization_dat = tibble::tribble(
      ~var_code,~var_pt,~var_eng,~unit_value,~unit_quantity,

      37,'Produto Interno Bruto a precos correntes','GDP at current prices','1K BRL',NA,
      553,'Participacao do produto interno bruto a precos correntes no produto interno bruto a precos correntes da microrregiao geografica','Share of GDP at current prices in GDP at current prices of the geographic microregion',NA,'%',
      552,'Participacao do produto interno bruto a precos correntes no produto interno bruto a precos correntes da mesorregiao geografica','Share of GDP at current prices in GDP at current prices of the geographic mesoregion',NA,'%',
      497,'Participacao do produto interno bruto a precos correntes no produto interno bruto a precos correntes da unidade da federacao','Share of GDP at current prices in GDP at current prices of the federation unit',NA,'%',
      530,'Participacao do produto interno bruto a precos correntes no produto interno bruto a precos correntes da grande regiao','Share of GDP at current prices in GDP at current prices in the wider region',NA,'%',
      496,'Participacao do produto interno bruto a precos correntes no produto interno bruto a precos correntes do Brasil','Share of GDP at current prices in GDP at current prices in Brazil',NA,'%',
      543,'Impostos, liquidos de subsidios, sobre produtos a precos correntes','Taxes, net of subsidies, on products at current prices','1K BRL',NA,
      571,'Participacao dos impostos, liquidos de subsidios, sobre produtos a precos correntes nos impostos, liquidos de subsidios, sobre produtos a precos correntes da microrregiao geografica','Share of taxes, net of subsidies, on products at current prices in taxes, net of subsidies, on products at current prices of the geographic microregion',NA,'%',
      570,'Participacao dos impostos, liquidos de subsidios, sobre produtos a precos correntes nos impostos, liquidos de subsidios, sobre produtos a precos correntes da mesorregiao geografica','Share of taxes, net of subsidies, on products at current prices in taxes, net of subsidies, on products at current prices in the geographic mesoregion',NA,'%',
      545,'Participacao dos impostos, liquidos de subsidios, sobre produtos a precos correntes nos impostos, liquidos de subsidios, sobre produtos a precos correntes da unidade da federacao','Share of taxes, net of subsidies, on products at current prices in taxes, net of subsidies, on products at current prices of the federation unit',NA,'%',
      551,'Participacao dos impostos, liquidos de subsidios, sobre produtos a precos correntes nos impostos, liquidos de subsidios, sobre produtos a precos correntes da grande regiao','Share of taxes, net of subsidies, on products at current prices in taxes, net of subsidies, on products at current prices in the wider region',NA,'%',
      544,'Participacao dos impostos, liquidos de subsidios, sobre produtos a precos correntes nos impostos, liquidos de subsidios, sobre produtos a precos correntes do Brasil','Share of taxes, net of subsidies, on products at current prices in taxes, net of subsidies, on products at current prices in Brazil',NA,'%',
      498,'Valor adicionado bruto a precos correntes total','Gross value added at current prices','1K BRL',NA,
      555,'Participacao do valor adicionado bruto a precos correntes total no valor adicionado bruto a precos correntes total da microrregiao geografica','Share of gross value added at current prices in the gross value added at current prices of the geographic microregion',NA,'%',
      554,'Participacao do valor adicionado bruto a precos correntes total no valor adicionado bruto a precos correntes total da mesorregiao geografica','Share of the gross value added at current prices in the gross value added at current prices of the geographic mesoregion',NA,'%',
      500,'Participacao do valor adicionado bruto a precos correntes total no valor adicionado bruto a precos correntes total da unidade da federacao','Share of the gross value added at current prices in the gross value added at current prices of the federation unit',NA,'%',
      546,'Participacao do valor adicionado bruto a precos correntes total no valor adicionado bruto a precos correntes total da grande regiao','Share of gross value added at current prices in the gross value added at current prices in the wider region',NA,'%',
      499,'Participacao do valor adicionado bruto a precos correntes total no valor adicionado bruto a precos correntes total do Brasil','Share of gross value added at current prices in gross value added at current prices in Brazil',NA,'%',
      513,'Valor adicionado bruto a precos correntes da agropecuaria','Gross value added at current prices in farming','1K BRL',NA,
      516,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes total','Share of gross value added at current prices of the farming sector in total gross value added at current prices',NA,'%',
      557,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes da agropecuaria da microrregiao geografica','Share of gross value added at current prices of the farming sector in gross value added at current prices in farming the geographic microregion',NA,'%',
      556,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes da agropecuaria da mesorregiao geografica','Share of gross value added at current prices of the farming sector in gross value added at current prices in farming the geographic mesoregion',NA,'%',
      515,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes da agropecuaria da unidade da federacao','Share of gross value added at current prices of the farming sector in gross value added at current prices in farming in the federation unit',NA,'%',
      547,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes da agropecuaria da grande regiao','Share of gross value added at current prices of the farming sector in gross value added at current prices in farming in the wider region',NA,'%',
      514,'Participacao do valor adicionado bruto a precos correntes da agropecuaria no valor adicionado bruto a precos correntes da agropecuaria do Brasil','Share of gross value added at current prices of the farming sector in the gross value added at current prices in farming sector in Brazil',NA,'%',
      517,'Valor adicionado bruto a precos correntes da industria','Gross value added at current prices in industry','1K BRL',NA,
      520,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes total','Share of gross value added at current prices of the industrial sector in total gross value added at current prices',NA,'%',
      559,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes da industria da microrregiao geografica','Share of gross value added at current prices of the industrial sector in the gross value added at current prices of the industrial sector of the geographic microregion',NA,'%',
      558,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes da industria da mesorregiao geografica','Share of the gross value added at current prices of the industrial sector in the gross value added at current prices of the industrial sector of the geographical mesoregion',NA,'%',
      519,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes da industria da unidade da federacao','Share of the gross value added at current prices of the industrial sector in the gross value added at current prices of the industrial sector of the federation unit',NA,'%',
      548,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes da industria da grande regiao','Share of gross value added at current industry prices in gross value added at current industry prices in the wider region',NA,'%',
      518,'Participacao do valor adicionado bruto a precos correntes da industria no valor adicionado bruto a precos correntes da industria do Brasil','Share of gross value added at current prices in the industrial sector in the gross value added at current prices in the industrial sector in Brazil',NA,'%',
      6575,'Valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social','Gross value added at current prices in services, excluding administration, defense, education, public health and Social Security','1K BRL',NA,
      6574,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes total','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices',NA,'%',
      6571,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social da microrregiao geografica','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices of services sector,excluding administration, defense, education, public health and Social Security of the microregion',NA,'%',
      6570,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social da mesorregiao geografica','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices of services sector,excluding administration, defense, education, public health and Social Security of the mesoregion',NA,'%',
      6572,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social da unidade da federacao','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices of services sector,excluding administration, defense, education, public health and Social Security of the federation unit',NA,'%',
      6569,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social da grande regiao','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices of services sector,excluding administration, defense, education, public health and Social Security of the wider region',NA,'%',
      6573,'Participacao do valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publicas e seguridade social, no valor adicionado bruto a precos correntes dos servicos, exclusive administracao, defesa, educacao e saude publica e seguridade social do Brasil','Share of the gross value added at current prices of the services sector, excluding administration, defense, education, public health and Social Security, in total gross value added at current prices of services sector,excluding administration, defense, education, public health and Social Security of Brazil',NA,'%',
      525,'Valor adicionado bruto a precos correntes da administracao, defesa, educacao, saude publicas e seguridade social','Gross added value at current prices of administration, defense, education, public health and Social Security','1K BRL',NA,
      528,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes total','Share of gross value added at current prices for public administration, defense, education and health and social security in total gross value added at current prices',NA,'%',
      563,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social da microrregiao geografica','Share of gross value added at current prices of public administration, defense, education and health and social security in the gross value added at current prices of public administration, defense, education and health and social security of the microregion',NA,'%',
      562,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social da mesorregiao geografica','Share of gross value added at current prices of public administration, defense, education and health and social security in the gross value added at current prices of public administration, defense, education and health and social security of the mesoregion',NA,'%',
      527,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social da unidade da federacao','Share of gross value added at current prices of administration, defense, education and public health and social security in the gross value added at current prices of administration, defense, education and public health and social security of the federation unit',NA,'%',
      550,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social da grande regiao','Share of gross value added at current prices for public administration, defense, education and health and social security in the gross value added at current prices for public administration, defense, education and health and social security in the wider region',NA,'%',
      526,'Participacao do valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social no valor adicionado bruto a precos correntes da administracao, defesa, educacao e saude publicas e seguridade social do Brasil','Share of gross value added at current prices for public administration, defense, education and health and social security in the gross value added at current prices for public administration, defense, education and health and social security in Brazil',NA,'%',


    )}

  ############
  ## Return ##
  ############

  return(harmonization_dat)



}
