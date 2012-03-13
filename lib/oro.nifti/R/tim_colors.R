##
##
## Copyright (c) 2009, Brandon Whitcher and Volker Schmid
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer. 
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
## 
##

tim.colors <- function(n=64) {
  ## Tim Hoar's original 64 color definition definition:
  orig <- c("#00008F", "#00009F", "#0000AF", "#0000BF",
            "#0000CF", "#0000DF", "#0000EF", "#0000FF",
            "#0010FF", "#0020FF", "#0030FF", "#0040FF",
            "#0050FF", "#0060FF", "#0070FF", "#0080FF",
            "#008FFF", "#009FFF", "#00AFFF", "#00BFFF",
            "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF",
            "#10FFEF", "#20FFDF", "#30FFCF", "#40FFBF",
            "#50FFAF", "#60FF9F", "#70FF8F", "#80FF80",
            "#8FFF70", "#9FFF60", "#AFFF50", "#BFFF40",
            "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00",
            "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00",
            "#FFAF00", "#FF9F00", "#FF8F00", "#FF8000",
            "#FF7000", "#FF6000", "#FF5000", "#FF4000",
            "#FF3000", "#FF2000", "#FF1000", "#FF0000",
            "#EF0000", "#DF0000", "#CF0000", "#BF0000",
            "#AF0000", "#9F0000", "#8F0000", "#800000")
  if (n == 64) {
    return(orig)
  }
  rgb.tim <- t(col2rgb(orig))
  temp <- matrix(NA, ncol=3, nrow=n)
  x <- seq(0, 1, length.out=64)
  xg <- seq(0, 1, length.out=n)
  for (k in 1:3) {
    hold <- splines::interpSpline(x, rgb.tim[,k])
    hold <- predict(hold, xg)$y
    hold[hold < 0] <- 0
    hold[hold > 255] <- 255
    temp[,k] <- round(hold)
  }
  rgb(temp[,1], temp[,2], temp[,3], maxColorValue=255)
}
