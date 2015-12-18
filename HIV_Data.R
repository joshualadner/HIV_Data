acg.pcg.combined <- read.csv(file.choose(), header=TRUE)


na.exclude(acg.pcg.combined)


GSH.model = lmer(GSH ~ Time + Region + GSH..SD + (1|Subject), data=acg.pcg.combined)
summary(GSH.model)
residuals.gsh <- resid(GSH.model)
plot(GSH.model, xlab = "Concentration", ylab = "GSH Residual")

GSH.Cr.model = lmer(GSH.Cr ~ Time + Region + GSH..SD + (1|Subject), data=acg.pcg.combined)
summary(GSH.Cr.model)
residuals.gsh.cr <- resid(GSH.Cr.model)
plot(GSH.Cr.model, xlab = "Concentration", ylab = "GSH.Cr Residual")


Ins.model = lmer(Ins ~ Time + Region + Ins..SD + (1|Subject), data=acg.pcg.combined)
summary(Ins.model)
residuals.ins <- resid(Ins.model)
plot(Ins.model, xlab = "Concentration", ylab = "Ins Residual")

Ins.Cr.model = lmer(Ins.Cr ~ Time + Region + Ins..SD + (1|Subject), data=acg.pcg.combined)
summary(Ins.Cr.model)
residuals.ins.cr <- resid(Ins.Cr.model)
plot(Ins.Cr.model, xlab = "Concentration", ylab = "Ins.Cr Residual")


NAA.NAAG.model = lmer(NAA.NAAG ~ Time + Region + NAA.NAAG..SD + (1|Subject), data=acg.pcg.combined)
summary(NAA.NAAG.model)
residuals.naa.naag <- resid(NAA.NAAG.model)
plot(NAA.NAAG.model, xlab = "Concentration", ylab = "NAA.NAAG Residual")

NAA.NAAG.Cr.model = lmer(NAA.NAAG.Cr ~ Time + Region + NAA.NAAG..SD + (1|Subject), data=acg.pcg.combined)
summary(NAA.NAAG.Cr.model)
residuals.naa.naa.cr <- resid(NAA.NAAG.Cr.model)
plot(NAA.NAAG.Cr.model, xlab = "Concentration", ylab = "NAA.NAAG.Cr Residual")


GABA.model = lmer(GABA ~ Time + Region + GABA..SD + (1|Subject), data=acg.pcg.combined)
summary(GABA.model)
residuals.gaba <- resid(GABA.model)
plot(GABA.model, xlab = "Concentration", ylab = "GABA Residual")

GABA.Cr.model = lmer(GABA.Cr ~ Time + Region + GABA..SD + (1|Subject), data=acg.pcg.combined)
summary(GABA.Cr.model)
residuals.gaba.cr <- resid(GABA.Cr.model)
plot(GABA.Cr.model, xlab = "Concentration", ylab = "GABA.Cr Residual")


GPC.PCh.model = lmer(GPC.PCh ~ Time + Region + GPC.PCh..SD + (1|Subject), data=acg.pcg.combined)
summary(GPC.PCh.model)
residuals.gpc.pch <- resid(GPC.PCh.model)
plot(GPC.PCh.model, xlab = "Concentration", ylab = "GPC + PCh Residual")

GPC.PCh.Cr.model = lmer(GPC.PCh.Cr ~ Time + Region + GPC.PCh..SD + (1|Subject), data=acg.pcg.combined)
summary(GPC.PCh.Cr.model)
residuals.gpc.pch.cr <- resid(GPC.PCh.Cr.model)
plot(GPC.PCh.Cr.model, xlab = "Concentration", ylab = "GPC + PCh.Cr Residual")


Cr.PCr.model = lmer(Cr.PCr ~ Time + Region + Cr.PCr..SD + (1|Subject), data=acg.pcg.combined)
summary(Cr.PCr.model)


Glu.Gln.model = lmer(Glu.Gln ~ Time + Region + Glu.Gln..SD + (1|Subject), data=acg.pcg.combined)
summary(Glu.Gln.model)
residuals.glu.gln <- resid(Glu.Gln.model)
plot(Glu.Gln.model, xlab = "Concentration", ylab = "Glu + Gln Residual")

Glu.Gln.Cr.model = lmer(Glu.Gln.Cr ~ Time + Region + Glu.Gln..SD + (1|Subject), data=acg.pcg.combined)
summary(Glu.Gln.Cr.model)
residuals.glu.gln.cr <- resid(Glu.Gln.Cr.model)
plot(Glu.Gln.Cr.model, xlab = "Concentration", ylab = "Glu + Gln.Cr Residual")


Asp.model = lmer(Asp ~ Time + Region + Asp..SD + (1|Subject), data=acg.pcg.combined)
summary(Asp.model)
residuals.asp <- resid(Asp.model)
plot(Asp.model, xlab = "Concentration", ylab = "Asp Residual")

Asp.Cr.model = lmer(Asp.Cr ~ Time + Region + Asp..SD + (1|Subject), data=acg.pcg.combined)
summary(Asp.Cr.model)
residuals.asp.cr <- resid(Asp.Cr.model)
plot(Asp.Cr.model, xlab = "Concentration", ylab = "Asp.Cr Residual")

