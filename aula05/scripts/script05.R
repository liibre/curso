# Reading xlxs, against my will ####

# loading packages
library(readxl)

data.path <- list.files(path = "aula05/data",
                        pattern = ".xlsx",
                        full.names = TRUE)

# read data
# let's read all sheets from data
# comm, traits, envir, bio, coord, splist, sitelist
comm <- as.data.frame(read_excel(data.path,
                                 sheet = "comm",
                                 na = "NA"))

traits <- as.data.frame(read_excel(data.path,
                                   sheet = "traits",
                                   na = "NA"))

envir <- as.data.frame(read_excel(data.path,
                                  sheet = "envir",
                                  na = "NA"))

blo <- as.data.frame(read_excel(data.path,
                                sheet = "blo",
                                na = "NA"))

coord <- as.data.frame(read_excel(data.path,
                                  sheet = "coord",
                                  na = "NA"))


splist <- as.data.frame(read_excel(data.path,
                                   sheet = "splist",
                                   na = "NA"))

sitelist <- as.data.frame(read_excel(data.path,
                                     sheet = "sitelist",
                                     na = "NA"))

all_files <- list(comm = comm,
                  traits = traits,
                  envir = envir,
                  coord = coord,
                  splist = splist)

for (i in 1:length(all_files)) {
write.csv(all_files[[i]],
          paste0("aula05/data/", names(all_files)[i], ".csv"),
          row.names = FALSE)
}

