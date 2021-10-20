library(tidyverse)

mtcars2 <- mtcars %>%
  select(-vs, -am)
head(mtcars2)

cor(mtcars2)

panel.cor_simple <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- cor(x, y) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * abs(r)) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
  pairs(mtcars2, lower.panel=panel.smooth, upper.panel=panel.cor_simple) 
library(corrplot)
M <- cor(mtcars2)
corrplot(M, method = "number", type="upper")