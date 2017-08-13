############################################################################
# Gráficas del proyecto
#
# Script para generar algunas de las gráficas del proyecto de fin de carrera
############################################################################
#devtools::install_github("ncordon/imbalance")
library(imbalance)
library(ggplot2)
library(pROC)
data(ecoli1, package = "imbalance")

# Gráfica de desbalanceo
############################################################################
colorPalette <-  c("#009E73", "#D55E00", "#CC79A7")
ggplot(ecoli1, aes_string("Gvh", "Mcg", col = "Class")) +
      geom_point(alpha = 1) +
      scale_color_manual(values = colorPalette) +
      theme(panel.background = element_blank())


ggsave(filename="desbalanceo.png",width=7, height=5)


# Curva ROC
############################################################################
data("aSAH")
roc(aSAH$outcome, aSAH$s100b, plot = TRUE, xlab="Falsos positivos", ylab="Verdaderos positivos")
