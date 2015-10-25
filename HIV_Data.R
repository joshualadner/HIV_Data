acg.pcg.combined <- read.csv(file.choose(), header=TRUE)
GSH.model = lmer(log(GSH) ~ Time + Region + (1|Subject) + (1|GSH..SD), data=acg.pcg.combined)
summary(GSH.model)
