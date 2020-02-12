# Alternativa para quem nao tem gather() de tidyr

comm.df <- data.frame(Sites = character(0),
                      TaxCode = character(0),
                      Abundance = numeric(0))

for (i in 1:nrow(comm[, -1])) {
  for (j in 1:ncol(comm[, -1])) {
    df <- data.frame(Sites = comm[i, 1],
                     TaxCode = colnames(comm[, -1])[j],
                     Abundance = comm[i, j])
    comm.df <- rbind(comm.df, df)
  }
}
