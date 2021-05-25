1 - pgamma (2300, 25, 1/80)
qgamma(0.5, 25, 1/80)
qchisq (0.98, 10)
qchisq (0.03, 10)
library(DBI)
con <- dbConnect(RSQLite::SQLite(), dbname = "/Users/mariaclaramano/Downloads/FinanceiroFinal.db")
dbBegin(con) #iniciou a primeira transa????o
dbExecute(con, "UPDATE NotaFiscal set valor = valor * 0.85, Imposto = Imposto * 0.9;")
dbExecute(con, "UPDATE Cliente set Estado = 'RJ' WHERE Estado = 'ES';")
dbCommit(con) #Confirmou a primeira transa????o

library(DBI)
con <- dbConnect(RSQLite::SQLite(), dbname = "/Users/mariaclaramano/Downloads/FinanceiroFinal.db")
library(RSQLite)
notaFiscal = dbGetQuery(con, "SELECT NumeroNota, Serie FROM NotaFiscal;")
write.table(notaFiscal, file = "/Users/mariaclaramano/Downloads/NotaFiscal.CSV", row.names = FALSE, sep =";");
