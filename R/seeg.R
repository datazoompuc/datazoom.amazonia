#' @title Greenhouse gases emission estimates (SEEG)
#'
#' Loads data of estimates of emission of greenhouse gases of Brazilian cities
#'
#' @param years A \code{vector} indicating what years will the data be loaded
#'
#' @param aggregation_level A \code{string} that defines the geographic level of the data. Defaults to National level, but can be one of "country", "region", "state", "mesoregion", "microregion" and "city"
#'
#' @param language A \code{string} that indicates in which language the data will be returned. The default is "pt", so your data will be returned in Portuguese. Currently, only Portuguese and English are supported.
#'
#' @param long A \code{boolean} that sets the format of the returned data. \code{long = TRUE} will return a list where each data frame represents a variable and products are transformed to columns so each geographical aggregation level/year will only have a single line. Default value is \code{FALSE}.
#'
#' @return A \code{data frame} or a \code{list} of data frames if \code{long} is set to \code{TRUE}.
#'
#' @author DataZoom, Department of Economics, Pontifical Catholic University of Rio de Janeiro
#'
#' @encoding UTF-8
#'
#' @export load_seeg
#'
#' @examples \dontrun{datazoom.amazonia::load_seeg(2013, language = "eng")}
#'

load_seeg <- function(language = "pt"){
  message("Function requires download of large file, so it may take time to run")

  if (language != "pt" && language != "eng"){
    warning("Language selected not supported! Proceding with Portuguese")
    language = "pt"
  }

  direc <- getwd()
  url <- "https://drive.google.com/file/d/1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd/view?usp=sharing"
  urlteste <- "https://drive.google.com/file/d/1A_neQegigQuDvGHfpngk_11wsqGyR8US/view?usp=sharing"

  df <- gsheet::gsheet2tbl(urlteste)
  df <- janitor::clean_names(df)

  df <- as.data.frame(gsub("\u00ed", "i", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f5", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e1", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e7", "c", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e3", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00c1", "A", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e9", "e", as.matrix(df)))
  df <- as.data.frame(gsub("\u00fa", "u", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f4", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00f3", "o", as.matrix(df)))
  df <- as.data.frame(gsub("\u00e2", "a", as.matrix(df)))
  df <- as.data.frame(gsub("\u00d3", "O", as.matrix(df)))


  to_english <- function(df){
    index <- df$`Tier 1` == "Agropeacuaria"
    df$`Tier 1`[index] <- "agricultura and cattle raising"
    index <- df$`Tier 1` == "Energia"
    df$`Tier 1`[index] <- "Energy"
    index <- df$`Tier 1` == "Mudança de Uso da Terra e Florestas"
    df$`Tier 1`[index] <- "Changes in land and forrest use"
    index <- df$`Tier 1` == "Processos Industriais"
    df$`Tier 1`[index] <- "industrial processes"
    index <- df$`Tier 1` == "Residuos"
    df$`Tier 1`[index] <- "residues"
    ## TIER 2##
    index <- df$`Tier 2` == "Alteracoes de Uso do Solo"
    df$`Tier 2`[index] <- "Changes in Land use"
    index <- df$`Tier 2` == "Cultivo de Arroz"
    df$`Tier 2`[index] <- "Rice growing"
    index <- df$`Tier 2` == "Efluentes Liquidos"
    df$`Tier 2`[index] <- "Liquid effluents"
    index <- df$`Tier 2` == "Emissoes de HFCs"
    df$`Tier 2`[index] <- "HFC emissions"
    index <- df$`Tier 2` == "Emissoes Fugitivas"
    df$`Tier 2`[index] <- "Fugitive emissions"
    index <- df$`Tier 2` == "Emissoes pela Queima de Combustiveis"
    df$`Tier 2`[index] <- "Emissions by fuel combustion"
    index <- df$`Tier 2` == "Fermentacao Enterica"
    df$`Tier 2`[index] <- "Enteric fermentation"
    index <- df$`Tier 2` == "Industria Quimica"
    df$`Tier 2`[index] <- "Chemical industry"
    index <- df$`Tier 2` == "Manejo de Dejetos Animais"
    df$`Tier 2`[index] <- "Animal waste handling"
    index <- df$`Tier 2` == "Producao de metais"
    df$`Tier 2`[index] <- "Metal production"
    index <- df$`Tier 2` == "Produtos minerais"
    df$`Tier 2`[index] <- "Mineral products"
    index <- df$`Tier 2` == "Queima de residuos agricolas"
    df$`Tier 2`[index] <- "agricultural residue combustion"
    index <- df$`Tier 2` == "Remocao em areas protegidas"
    df$`Tier 2`[index] <- "removal in protected areas"
    index <- df$`Tier 2` == "Remocao por Mudanaa de Uso da Terra"
    df$`Tier 2`[index] <- "removal by changes in Land use"
    index <- df$`Tier 2` == "Remocao por Vegetacao Secundaria"
    df$`Tier 2`[index] <- "Removal by secondary vegetation"
    index <- df$`Tier 2` == "Residuos Florestais"
    df$`Tier 2`[index] <- "Forrest residues"
    index <- df$`Tier 2` == "Residuos Solidos"
    df$`Tier 2`[index] <- "Solid residues"
    index <- df$`Tier 2` == "Solos Manejados"
    df$`Tier 2`[index] <- "Handled soils"
    index <- df$`Tier 2` == "Uso de SF6"
    df$`Tier 2`[index] <- "SF6 use"
    index <- df$`Tier 2` == "Uso Nao-Energetico de Combusteveis e Uso de Solventes"
    df$`Tier 2`[index] <- "Non-energetic fuel use and solvent use"
    ##TIER 3##
    index <- df$`Tier 3` == "Agropecuario"
    df$`Tier 3`[index] <- "agriculture and cattle raising "
    index <- df$`Tier 3` == "Amazonia"
    df$`Tier 3`[index] <- "Amazon"
    index <- df$`Tier 3` == "Consumo de Barrilha"
    df$`Tier 3`[index] <- "Soda ash consumption"
    index <- df$`Tier 3` == "Consumo Final Nao Energetico"
    df$`Tier 3`[index] <- "Final non-energetic consumption"
    index <- df$`Tier 3` == "Diretas"
    df$`Tier 3`[index] <- "Direct"
    index <- df$`Tier 3` == "Disposicao Final"
    df$`Tier 3`[index] <- "Final disposal"
    index <- df$`Tier 3` == "Efluentes Liquidos Domesticos"
    df$`Tier 3`[index] <- "Domestic liquid effluents"
    index <- df$`Tier 3` == "Efluentes Liquidos Industriais"
    df$`Tier 3`[index] <- "Industrial liquid effluents"
    index <- df$`Tier 3` == "Equipamentos Eletricos"
    df$`Tier 3`[index] <- "Electrical equipment"
    index <- df$`Tier 3` == "Geracao de Eletricidade (Servico Publico)"
    df$`Tier 3`[index] <- "Electricity generation (public service)"
    index <- df$`Tier 3` == "Incineracao ou queima a ceu aberto"
    df$`Tier 3`[index] <- "Incineration or open-air burning"
    index <- df$`Tier 3` == "Indiretas"
    df$`Tier 3`[index] <- "indirect"
    index <- df$`Tier 3` == "Producao de Acido Adipico"
    df$`Tier 3`[index] <- "adipic acid production"
    index <- df$`Tier 3` == "Producao de Acido Fosforico"
    df$`Tier 3`[index] <- "Fosforic acid production"
    index <- df$`Tier 3` == "Producao de Acido Nitrico"
    df$`Tier 3`[index] <- "nitric acid production"
    index <- df$`Tier 3` == "Producao de Acrilonitrila"
    df$`Tier 3`[index] <- "acrylonitrile production"
    index <- df$`Tier 3` == "Producao de Aluminio"
    df$`Tier 3`[index] <- "aluminum production"
    index <- df$`Tier 3` == "Producao de Amonia"
    df$`Tier 3`[index] <- "ammonium production"
    index <- df$`Tier 3` == "Producao de Cal"
    df$`Tier 3`[index] <- "lime production"
    index <- df$`Tier 3` == "Producao de Caprolactama"
    df$`Tier 3`[index] <- "caprolactam production"
    index <- df$`Tier 3` == "Producao de Carbureto de Calcio"
    df$`Tier 3`[index] <- "calcium carbide production"
    index <- df$`Tier 3` == "Producao de Cimento"
    df$`Tier 3`[index] <- "ciment production"
    index <- df$`Tier 3` == "Producao de Cloreto de Vinila"
    df$`Tier 3`[index] <- "Vinyl chloride production"
    index <- df$`Tier 3` == "Producao de Combustiveis"
    df$`Tier 3`[index] <- "fuel production"
    index <- df$`Tier 3` == "Producao de Coque de Petroleo Calcinado"
    df$`Tier 3`[index] <- "calcinated petroleum coke production"
    index <- df$`Tier 3` == "Producao de Eteno"
    df$`Tier 3`[index] <- "ethylene production"
    index <- df$`Tier 3` == "Producao de Ferro Gusa e Aco"
    df$`Tier 3`[index] <- "pig iron and steel production"
    index <- df$`Tier 3` == "Producao de Ferrovias"
    df$`Tier 3`[index] <- "Railroad production"
    index <- df$`Tier 3` == "Producao de Magnesio"
    df$`Tier 3`[index] <- "magnesium production"
    index <- df$`Tier 3` == "Produção de Metanol"
    df$`Tier 3`[index] <- "methanol production"
    index <- df$`Tier 3` == "Producao de Negro-de-fumo"
    df$`Tier 3`[index] <- "carbon black production"
    index <- df$`Tier 3` == "Producao de Outros Nao-Ferrosos"
    df$`Tier 3`[index] <- "other non-ferrous production"
    index <- df$`Tier 3` == "Producao de Oxido de Eteno"
    df$`Tier 3`[index] <- "ethylene oxide production"
    index <- df$`Tier 3` == "Producao de Vidro "
    df$`Tier 3`[index] <- "glass production"
    index <- df$`Tier 3` == "Publico"
    df$`Tier 3`[index] <- "public"
    index <- df$`Tier 3` == "Residencial"
    df$`Tier 3`[index] <- "residential"
    index <- df$`Tier 3` == "Transportes"
    df$`Tier 3`[index] <- "transport"
    index <- df$`Tier 3` == "Tratamento Biologico de Residuos Solidos"
    df$`Tier 3`[index] <- "Biologic treatment of solid residues"
    ## TIER 4##
    index <- df$`Tier 4` == "Aereo"
    df$`Tier 4`[index] <- "Aerial"
    index <- df$`Tier 4` == "Alimentos e bebidas"
    df$`Tier 4`[index] <- "Food and beverage"
    index <- df$`Tier 4` == "Aplicacao de residuos organicos"
    df$`Tier 4`[index] <- "organic residue application"
    index <- df$`Tier 4` == "Cal Calcitica"
    df$`Tier 4`[index] <- "calcium lime"
    index <- df$`Tier 4` == "Cal Dolomitica"
    df$`Tier 4`[index] <- "dolomitic lime"
    index <- df$`Tier 4` == "Cal Magnesiana"
    df$`Tier 4`[index] <- "magnesium lime"
    index <- df$`Tier 4` == "Ceramica"
    df$`Tier 4`[index] <- "ceramic"
    index <- df$`Tier 4` == "Cimento"
    df$`Tier 4`[index] <- "ciment"
    index <- df$`Tier 4` == "Consumo de Calcario"
    df$`Tier 4`[index] <- "limestone consumption"
    index <- df$`Tier 4` == "Consumo de Combustiveis Redutores"
    df$`Tier 4`[index] <- "fuel reducers consumption"
    index <- df$`Tier 4` == "Consumo de Dolomita"
    df$`Tier 4`[index] <- "dolomite consumption"
    index <- df$`Tier 4` == "Consumo em Outros Setores"
    df$`Tier 4`[index] <- "consumption in other sectors"
    index <- df$`Tier 4` == "Deposicao Atmosferica"
    df$`Tier 4`[index] <- "atmosferic deposition"
    index <- df$`Tier 4` == "Deposicao de dejetos em pastagem"
    df$`Tier 4`[index] <- "deposition of pasture excrements"
    index <- df$`Tier 4` == "em Area Protegida"
    df$`Tier 4`[index] <- "in protected area"
    index <- df$`Tier 4` == "Exploracao de petroleo e gas natural"
    df$`Tier 4`[index] <- "exploration of petroleum and natural gas"
    index <- df$`Tier 4` == "Ferro gusa e aco"
    df$`Tier 4`[index] <- "pig iron and steel"
    index <- df$`Tier 4` == "Ferro Ligas"
    df$`Tier 4`[index] <- "iron alloy"
    index <- df$`Tier 4` == "Ferroviario"
    df$`Tier 4`[index] <- "railway"
    index <- df$`Tier 4` == "Fertilizantes Sinteticos"
    df$`Tier 4`[index] <- "sinthetic fertilizers"
    index <- df$`Tier 4` == "fora de Area Protegida"
    df$`Tier 4`[index] <- "outside of protected area"
    index <- df$`Tier 4` == "Hidroviario"
    df$`Tier 4`[index] <- "waterway"
    index <- df$`Tier 4` == "Incineracao"
    df$`Tier 4`[index] <- "incineration"
    index <- df$`Tier 4` == "Lixiviacao"
    df$`Tier 4`[index] <- "leaching"
    index <- df$`Tier 4` == "Lodo de ETE"
    df$`Tier 4`[index] <- "ETE sludge"
    index <- df$`Tier 4` == "Mineracao e pelotizacao"
    df$`Tier 4`[index] <- "mining and pelletization"
    index <- df$`Tier 4` == "Mineralizacao de N associado a perda de C no solo"
    df$`Tier 4`[index] <- "mineralization of N Associated to the loss of C in the soil"
    index <- df$`Tier 4` == "Nao ferrosos e outros da metalurgia"
    df$`Tier 4`[index] <- "non-ferrous and others from metallurgy"
    index <- df$`Tier 4` == "Outras indústrias"
    df$`Tier 4`[index] <- "other industries"
    index <- df$`Tier 4` == "Outros"
    df$`Tier 4`[index] <- "other"
    index <- df$`Tier 4` == "Papel e celulose"
    df$`Tier 4`[index] <- "paper and cellulose"
    index <- df$`Tier 4` == "Producao de alcool"
    df$`Tier 4`[index] <- "alcohol production"
    index <- df$`Tier 4` == "Producao de Carne Avicola"
    df$`Tier 4`[index] <- "poultry meat production"
    index <- df$`Tier 4` == "Producao de Carne Bovina"
    df$`Tier 4`[index] <- "bovina meat production"
    index <- df$`Tier 4` == "Producao de Carne Suina"
    df$`Tier 4`[index] <- "swine meat production"
    index <- df$`Tier 4` == "Producao de carvao mineral e outros"
    df$`Tier 4`[index] <- "mineral coal production and other"
    index <- df$`Tier 4` == "Producao de carvao vegetal"
    df$`Tier 4`[index] <- "charcoal production"
    index <- df$`Tier 4` == "Producao de Celulose"
    df$`Tier 4`[index] <- "cellulose production"
    index <- df$`Tier 4` == "Producao de Cerveja"
    df$`Tier 4`[index] <- "beer production"
    index <- df$`Tier 4` == "Producao de Leite Cru"
    df$`Tier 4`[index] <- "raw milk production"
    index <- df$`Tier 4` == "Producao de Leite Pasteurizado"
    df$`Tier 4`[index] <- "pasteurized milk production"
    index <- df$`Tier 4` == "Producao de Queima de Residuos a Ceu Aberto"
    df$`Tier 4`[index] <- "open-air residue incineration production"
    index <- df$`Tier 4` == "Quimica"
    df$`Tier 4`[index] <- "chemistry"
    index <- df$`Tier 4` == "Refino de petroleo"
    df$`Tier 4`[index] <- "oil refinery"
    index <- df$`Tier 4` == "Residuos Agricolas"
    df$`Tier 4`[index] <- "agricultural residues"
    index <- df$`Tier 4` == "Residuos de servicos de saude"
    df$`Tier 4`[index] <- "Health services residues"
    index <- df$`Tier 4` == "Residuos Solidos Urbanos"
    df$`Tier 4`[index] <- "solid urban residues"
    index <- df$`Tier 4` == "Rodoviario"
    df$`Tier 4`[index] <- "Road traffic"
    index <- df$`Tier 4` == "Solos organicos"
    df$`Tier 4`[index] <- "organic soil"
    index <- df$`Tier 4` == "Tecnologia Prebaked Anode"
    df$`Tier 4`[index] <- "anode prebaked technology"
    index <- df$`Tier 4` == "Tecnologia Soderberg"
    df$`Tier 4`[index] <- "Soderberg technology"
    index <- df$`Tier 4` == "Textil"
    df$`Tier 4`[index] <- "textile"
    index <- df$`Tier 4` == "Transporte de gas natural"
    df$`Tier 4`[index] <- "natural gas transport"
    index <- df$`Tier 4` == "Uso de SF6"
    df$`Tier 4`[index] <- "SF6 use"
    ##TIER 5##
    index <- df$`Tier 4` == "Residuos"
    df$`Tier 4`[index] <- "residues"
    index <- df$`Tier 4` == "Residuos"
    df$`Tier 4`[index] <- "residues"





  }

  if (language == "eng"){
    colnames(df) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5", "Tier 6", "Type of emission",
                    "Gas", "Territory", "Municipality_Code", "Municipality", "Economic_activity", "Product",
                    "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007","2008","2009", "2010",
                    "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019_AREA")



  }
}
