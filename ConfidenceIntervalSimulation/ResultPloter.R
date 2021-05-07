library(ggrepel)

# A_X = c(0.1, 0.2, 0.3)
# A_Y = c(1, 2, 3)
# A_M = c("A1", "A2", "A3")
# DF_A <- data.frame(
#   Average_CI_Length = A_X, lotlll
#   Coverage = A_Y, 
#   PointName = A_M
# )  
# 
# B_X = c(0.15, 0.25, 0.35)
# B_Y = c(1.5, 2.5, 3.5)
# B_M = c("B1", "B2", "B3")
# DF_B <- data.frame(
#   Average_CI_Length = B_X, 
#   Coverage = B_Y, 
#   PointName = B_M
# )
# 
# C_X = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
# C_Y = c(1, 1.5, 2, 2.5, 3, 3.5)
# C_Name = c("RF", "BART", "RF", "BART", "RF", "BART")
# C_Color = c("RAW", "RAW", "B", "B", "K", "K")
# C_Shape = c("RAW", "RAW", "B", "B", "K", "K")
# DF_C <- data.frame(
#   Average_CI_Length = C_X, 
#   Coverage = C_Y, 
#   PointName = C_Name,
#   ColorGroup = C_Color,
#   ShapeGroup = C_Shape
# )

Ploter<- function(DF){
  
  p <- ggplot(DF, aes(x=Average_CI_Length, y=Coverage, color=ColorGroup, shape=ShapeGroup)) + geom_point() +geom_text_repel(aes(label=PointName), size=5) 
  #+ scale_x_continuous(expand = c(.025, .025)) + scale_y_continuous(expand = c(.2, .2))
  p
}
