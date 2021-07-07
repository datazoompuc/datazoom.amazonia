

# load_legal_amazon = function(){
#
#   amz = suppressMessages(geobr::read_amazon(year=2012,showProgress = FALSE))
#   munic = suppressMessages(geobr::read_municipality(year=2010,showProgress = FALSE))
#
#   f = sf::st_join(munic,amz,sf::st_within)
#
#   munic = munic %>%
#     mutate()
#
#
# }
