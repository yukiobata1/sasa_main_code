#*********************************************************************
# R code for drawing map output variables of SEIB-DGVM ver. 3.0       
#                                                                     
# All rights are reserved by Dr. Hisashi SATO                         
# @Graduate School of Environmental Studies, Nagoya University        
#*********************************************************************

#____________________ Common procedure ____________________ 
#Set environment
   #Working directory
   setwd('./result_visualize/') 
   
   #Missing value
   Missing  =  0
   
   #Grid size (length on a side @ deg)
   GridSize   = 0.5
   
   #Pixel size of a grid
   PixSize   = 5.0
   
   #Visualizing area (designated by grid numbers)
   LatNoStart = 90
   LatNoEnd   = 98
   LonNoStart = 639
   LonNoEnd   = 651
   
#Compute valiables for coodination and image size
   
   #Row and column numbers of drawing area
   Lat      = LatNoEnd - LatNoStart + 1
   Lon      = LonNoEnd - LonNoStart + 1
   
   #Vertical and horizontal picture size @ pixel
   width_size  = Lon * PixSize
   height_size = Lat * PixSize
   
   #Latitude and longitude at the center of grid of the most south-west grid cell
   Lat1_loc =    90.0 - (LatNoEnd   - 0.5) * 0.5 #(North:+ , Sourth:-)
   Lon1_loc = - 180.0 + (LonNoStart - 0.5) * 0.5 #(West :- , East  :+)
   
   #Latitude coordinate
   y <- array(0.0, dim=c(Lat))                          #Prepare array y
   for(i in 1:Lat)   y[i] <- Lat1_loc + (i-1)*GridSize  #Input latitude from south to north
   
   #Longitude coordinate
   x <- array(0.0, dim=c(Lon))                          #Prepare attay x
   for(i in 1:Lon)   x[i] <- Lon1_loc + (i-1)*GridSize  #Input longitude from west to east
   
#Other preparation
   #Activate map library
   library(maps)
   library(RColorBrewer)
  #library(colorRamps)
   
   #Prepare data aray
   z  <- array(Missing, dim=c(Lon,Lat))

#____________________ Subroutines for color palette ____________________ 
set_color_topo <- function(num_col) {
   col <-  c(num_col)                #Prepare color variable (this must be division number - 1)
   col <- c(topo.colors(num_col))    #Give color from a palette
   col <- col[length(col):1]         #Turn upside down for color palette
   col[1]  = "white"                 #Change minimum class color
   return(col)                       }
   
set_color_topo2 <- function(num_col) {
   col <-  c(num_col)                #Prepare color variable (this must be division number - 1)
   col <- c(topo.colors(num_col))    #Give color from a palette
   col <- col[length(col):1]         #Turn upside down for color palette
   col[1]        = "white"           #Change minimum class color
   col[num_col]  = "gray "           #Change maximum class color
   return(col)                       }
   
set_color_heat <- function(num_col) {
   col <-  c(num_col)                #Prepare color variable (this must be division number - 1)
   col <- c(heat.colors(num_col))    #Give color from a palette
   col <- col[length(col):1]         #Turn upside down for color palette
   col[1]  = "white"                 #Change minimum class color
   return(col)                       }
   
set_color_terrain <- function(num_col) {
   col <-  c(num_col)                #Prepare color variable (this must be division number - 1)
   col <- c(terrain.colors(num_col)) #Give color from a palette
   col <- col[length(col):1]         #Turn upside down for color palette
   col[1]  = "antiquewhite1"         #Change minimum class color
   return(col)                       }

set_color_PlusMinus <- function(num_col) {
   col <-  c(num_col)                  #Prepare color variable (this must be division number - 1)
#  col <- c(matlab.like(num_col))      #Give color from a palette
#  col <- c(cm.colors(num_col))        #Give color from a palette
   col <- brewer.pal(num_col, "PiYG")  #Give color from a palette
   col[6] <- "white"                   #For Zero Color
#  col <- col[length(col):1]           #Turn upside down for color palette
   return(col)                       }


#____________________ Subroutines for data reading and coordination conversion ____________________ 
read_data <- function(fname) {
   d <- read.csv(fname, header=F)
   for (i in 1:Lat) {
   for (j in 1:Lon) {
      #Tuen upside down for latitude, and replace row and column
      z[j,i] <- d[Lat-i+1,j] 
   }
   }
   return(z)
}

#____________________ Subroutine for drawing color pannel ____________________
# LabelName :Label on the top of color pannel
# DivedNum  :Cell number of color pannel
# LabelNum  :Number of value below the color pannel
# IncreStep :Increment of color pannel

# draw_panel <- function(LabelName, DivedNum, LabelNum, IncreStep) {
#    x_start <-  -15
#    x_width <-    5
#    y_start <-  -47
#    y_width <-    5
#    
# draw_panel <- function(LabelName, DivedNum, LabelNum, IncreStep) {
#    x_start <-   60
#    x_width <-    5
#    y_start <-   75
#    y_width <-    5
#    
draw_panel <- function(LabelName, DivedNum, LabelNum, IncreStep) {
   x_start <-   140
   x_width <-    2
   y_start <-   74 #余白を含む
   y_width <-    3
   
   # 枠外への描画を許可
   par(xpd=T)
   
   #Write label on top of the color pannel
   text(x_start, y_start+2, pos=4, LabelName)
   
   #Draw white belt under color pannel
   x_end <-  x_start+x_width*LabelNum
   polygon( c(x_start, x_end, x_end, x_start), c(y_start, y_start, y_start-y_width, y_start-y_width), col='white') 
   
   #Draw color pannel
   if (DivedNum==0 || LabelNum==0) {return}
   i <- floor(DivedNum/ LabelNum)
   for (j in 1:DivedNum) {
      polygon( c(x_start, x_start+x_width, x_start+x_width, x_start), c(y_start, y_start, y_start-y_width, y_start-y_width), col=col[j])
      if (floor(j/i) == j/i) {text(x_start+0.5*x_width, y_start-y_width*0.7, pos=1, j*IncreStep)}
      x_start <- x_start+x_width
   }
   }



#____________________ Subroutine for drawing a distribution map 1 ____________________
draw_dist <- function(DivedNum,PannelStep,col,x,y,z) {
   PannelMax  <- DivedNum * PannelStep                #カラーパネルの最大値を算出
   br  <- c(seq(from=0, to=PannelMax, by=PannelStep)) #値の分割点の設定１(内部の仕切り)
   br[DivedNum+1]  = PannelMax*10                     #値の分割点の設定２(天井)
   par(mar = c(0,0,0,0) )                           #下・左・上・右の順で内部マージンを設定
   frame()                                            #画面のクリア
   image(x, y, z, breaks=br, col=col, xlab='', ylab='', axes = FALSE) #マッピング
   map(add=T, interior=FALSE)                                         #地図を重ね書き
   }
   
#____________________ Subroutine for drawing a distribution map 2 ____________________
draw_dist_LAI <- function(DivedNum,PannelStep,col,x,y,z) {
   PannelMax  <- DivedNum * PannelStep                #カラーパネルの最大値を算出
   br  <- c(seq(from=0, to=PannelMax, by=PannelStep)) #値の分割点の設定１(内部の仕切り)
   br[DivedNum+1]  = PannelMax*10                     #値の分割点の設定２(天井)
   br[1]           = 0.01                             #値の分割点の設定３(始点)
   par(mar = c(0,0,0,0) )                           #下・左・上・右の順で内部マージンを設定
   frame()                                            #画面のクリア
   image(x, y, z, breaks=br, col=col, xlab='', ylab='', axes = FALSE) #マッピング
   map(add=T, interior=FALSE)                                         #地図を重ね書き
   }
   
#____________________ Biome ____________________ 
   DivedNum   <- 12 #カラーパネルの分割数
   PannelStep <- 1  #カラーパネルの増分
   
   z <- read_data('out_biome.txt') #データ読みだしと、整形
   
   #色の設定を行う
   col <- set_color_topo(DivedNum) 
   col[ 0] <- "white"         # 0: - water -
   col[ 1] <- "white"         # 1: Polar desert
   col[ 2] <- "violet"        # 2: Arctic/Alpine-tundra
   col[ 3] <- "red"           # 3: Tropical evergreen forest
   col[ 4] <- "darksalmon"    # 4: Tropical deciduous forest
   col[ 5] <- "orange"        # 5: Temperate conifer forest
   col[ 6] <- "aquamarine"    # 6: Temperate broad-leaved evergreen forest
   col[ 7] <- "aquamarine2"   # 7: Temperate deciduous forest
   col[ 8] <- "aquamarine4"   # 8: Boreal evergreen forest
   col[ 9] <- "steelblue1"    # 9: Boreal deciduous forest
   col[10] <- "steelblue4"    #10: Xeric woodland / scrub
   col[11] <- "bisque"        #11: Grassland / steppe / Savanna
   col[12] <- "bisque3"       #12: Desert
   
   png('out_biome.png', width=width_size, height=height_size) #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)             #図本体の描画
   
#   #凡例の描画 ______
#   x_start <-   114
#   x_width <-    5
#   y_start <-   84 #余白を含む
#   y_width <-    4
#   
#   # 枠外への描画を許可
#   par(xpd=T)
#   
#   #Write label on top of the color pannel
#   text(x_start, y_start+2, pos=4, "Biome Type")
#   
#   #Draw color pannel
#   for (j in 1:DivedNum) {
#      polygon( c(x_start, x_start+x_width, x_start+x_width, x_start), c(y_start, y_start, y_start-y_width, y_start-y_width), col=col[j])
#      text(x_start+0.5*x_width, y_start-y_width*0.7, pos=1, j)
#      x_start <- x_start+x_width
#   }
   
   #デバイスドライバ閉じる ______
   dev.off()
   
#____________________ Fire number (n/year) ____________________ 
#   DivedNum   <- 20     #カラーパネルの分割数
#   PannelStep <- 0.025  #カラーパネルの増分
   
   DivedNum   <- 20     #カラーパネルの分割数
   PannelStep <- 0.0025  #カラーパネルの増分

   z   <- read_data('out_fire.txt')                               #データ読みだしと、整形
   col <- set_color_topo(DivedNum)                                #色の設定を行う
   
   png('out_fire.png', width=width_size, height=height_size)      #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)                 #図本体の描画
#  draw_panel('Fire Frequency (n/year)', DivedNum, 4, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                      #デバイスドライバ閉じる
   
#____________________ Biomass ____________________ 
   DivedNum   <- 20   #カラーパネルの分割数
   PannelStep <- 0.5 #カラーパネルの増分
   
   z <- read_data('out_wbiomass.txt')                            #データ読みだしと、整形
   col <- set_color_heat(DivedNum)                               #色の設定を行う
   
   png('out_wbiomass.png', width=width_size, height=height_size) #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)                #図本体の描画
#  draw_panel('Biomass (KgC/m2)', DivedNum, 4, PannelStep)       #カラーパネルを重ね書き
   dev.off()                                                     #デバイスドライバ閉じる

#____________________ GPP ____________________ 
   DivedNum   <- 12                    #カラーパネルの分割数
   PannelStep <- 0.2                   #カラーパネルの増分
   
   z <- read_data('out_gpp.txt')       #データ読みだしと、整形
   col <- set_color_topo(DivedNum)     #色の設定を行う
   
   png('out_gpp.png', width=width_size, height=height_size)   #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)             #図本体の描画
#  draw_panel('GPP (gC/m2/yr)', DivedNum, 4, PannelStep*1000) #カラーパネルを重ね書き
   dev.off()                                                  #デバイスドライバ閉じる
   
#____________________ NPP ____________________ 
   DivedNum   <- 12                #カラーパネルの分割数
   PannelStep <- 0.1               #カラーパネルの増分
   
   z <- read_data('out_npp.txt')   #データ読みだしと、整形
   col <- set_color_topo(DivedNum) #色の設定を行う
   
   png('out_npp.png', width=width_size, height=height_size)   #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)             #図本体の描画
#  draw_panel('NPP (gC/m2/yr)', DivedNum, 4, PannelStep*1000) #カラーパネルを重ね書き
   dev.off()                                                  #デバイスドライバ閉じる
   
#____________________ NEP ____________________ 
   DivedNum   <- 11               #カラーパネルの分割数 (奇数になるように設定すること)
   PannelStep <- 0.1              #カラーパネルの増分
   
   z <- read_data('out_nep.txt')         #データ読みだしと、整形
   col <- set_color_PlusMinus(DivedNum)  #色の設定を行う
   
   png('out_nep.png', width=width_size, height=height_size)   #デバイスドライバ開く
   par(xpd=T)                                                 #枠外への描画を許可
   
   PannelMin  <- -0.5*(DivedNum-1) * PannelStep - 0.5 * PannelStep #カラーパネルの最大値を算出
   PannelMax  <-  0.5*(DivedNum-1) * PannelStep + 0.5 * PannelStep #カラーパネルの最大値を算出
   
   br  <- c(seq(from=PannelMin, to=PannelMax, by=PannelStep)) #値の分割点の設定１(内部の仕切り)
   br[1]           = -PannelMax*10                            #値の分割点の設定２(ボトム)
   br[DivedNum+1]  =  PannelMax*10                            #値の分割点の設定３(天井)
   
   par(mar = c(0,0,0.0,0) )                                           #下・左・上・右の順で内部マージンを設定
   frame()                                                            #画面のクリア
   image(x, y, z, breaks=br, col=col, xlab='', ylab='', axes = FALSE) #マッピング
   map(add=T, interior=FALSE)                                         #地図を重ね書き
   
#   #カラーパネルを重ね書き
#   x_start <-   100
#   y_start <-   86 #余白部分へのはみ出しも考慮している
#   x_width <-    6
#   y_width <-    4
#   
#   LabelNum <-   5
#   IncreStep <-  PannelStep*1000
#   
#   text(x_start, y_start+2, pos=4, 'NEP (gC/m2/yr)') #Write label on top of the color pannel
#   
#   #Draw white belt under color pannel
#   x_end <-  x_start+x_width*DivedNum
#   polygon( c(x_start, x_end, x_end, x_start), c(y_start, y_start, y_start-y_width, y_start-y_width), col='white')
#   
#   #Draw color pannel
#   i <- floor(DivedNum/ LabelNum)
#   for (j in 1:DivedNum) {
#      polygon( c(x_start, x_start+x_width, x_start+x_width, x_start), c(y_start, y_start, y_start-y_width, y_start-y_width), col=col[j])
#      if (floor(j/i) == j/i) {text(x_start+0.5*x_width, y_start-y_width*0.7, pos=1, j*IncreStep-600)}
#      x_start <- x_start+x_width
#   }
   
   #デバイスドライバ閉じる
   dev.off()                                                  
   
#____________________ HR ____________________ 
   DivedNum   <- 12               #カラーパネルの分割数
   PannelStep <- 0.1              #カラーパネルの増分
   
   z <- read_data('out_hr.txt')    #データ読みだしと、整形
   col <- set_color_topo(DivedNum) #色の設定を行う
   
   png('out_hr.png', width=width_size, height=height_size)    #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)             #図本体の描画
#  draw_panel('HR (gC/m2/yr)', DivedNum, 6, PannelStep*1000)  #カラーパネルを重ね書き
   dev.off()                                                  #デバイスドライバ閉じる
   
#____________________ Maximum ALD (m) ____________________ 
   DivedNum   <- 20                #カラーパネルの分割数
   PannelStep <- 0.1               #カラーパネルの増分
   
   z <- read_data('out_ald_max.txt') #データ読みだしと、整形
   col <- set_color_topo2(DivedNum)   #色の設定を行う
   
   png('out_ald_max.png', width=width_size, height=height_size)           #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)                         #図本体の描画
#   draw_panel('Maximuum Active Layer Depth (m)', DivedNum, 5, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                              #デバイスドライバ閉じる
   
#____________________ JJA Available Water within the Soil layers 1 to 5 ____________________ 
   DivedNum   <- 20                 #カラーパネルの分割数
   PannelStep <- 0.02               #カラーパネルの増分
   
   z <- read_data('out_water_JJA.txt') #データ読みだしと、整形
   for (i in 1:Lat) {
   for (j in 1:Lon) {
      z[j,i] <- z[j,i] / 500.0   #mm -> fraction
   }
   }
   col <- set_color_topo(DivedNum)  #色の設定を行う
   
   png('out_water_JJA.png', width=width_size, height=height_size)                      #デバイスドライバ開く
   draw_dist (DivedNum, PannelStep, col, x, y, z)                                      #図本体の描画
#   draw_panel('JJA available Soil Water @ 0-50cm depth (fraction)', DivedNum, 4, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                                           #デバイスドライバ閉じる
   
#____________________ LAI ____________________ 
   DivedNum   <- 14                    #カラーパネルの分割数
   PannelStep <- 0.25                   #カラーパネルの増分
   
   col <- set_color_terrain(DivedNum)  #分割点と色の設定
   
   #描画a1
   z <- read_data('out_lai_amean.txt')                             #データ読みだしと、整形
   png('out_lai_amean.png', width=width_size, height=height_size)  #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)              #図本体の描画
#   draw_panel('Annual Mean LAI (m2/m2)', DivedNum, 7, PannelStep)  #カラーパネルを重ね書き
   dev.off()                                                       #デバイスドライバ閉じる
   
   #描画a2
   z <- read_data('out_lai_amean_t.txt')                                        #データ読みだしと、整形
   png('out_lai_amean_t.png', width=width_size, height=height_size)             #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)                           #図本体の描画
#   draw_panel('Annual Mean LAI of Woody PFTs(m2/m2)', DivedNum, 7, PannelStep)  #カラーパネルを重ね書き
   dev.off()                                                                    #デバイスドライバ閉じる
   
   #描画a3
   z <- read_data('out_lai_amean_g.txt')                                        #データ読みだしと、整形
   png('out_lai_amean_g.png', width=width_size, height=height_size)             #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)                           #図本体の描画
#   draw_panel('Annual Mean LAI of Grass PFT (m2/m2)', DivedNum, 7, PannelStep)  #カラーパネルを重ね書き
   dev.off()                                                                    #デバイスドライバ閉じる
   
   #描画b1
   z <- read_data('out_lai_max.txt')                                 #データ読みだしと、整形
   png('out_lai_max.png', width=width_size, height=height_size)      #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)                #図本体の描画
#   draw_panel('Annual Maximum LAI (m2/m2)', DivedNum, 7, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                         #デバイスドライバ閉じる
   
   #描画b2
   z <- read_data('out_lai_max_t.txt')                                            #データ読みだしと、整形
   png('out_lai_max_t.png', width=width_size, height=height_size)                 #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)                             #図本体の描画
#   draw_panel('Annual Maximum LAI of tree PFTs (m2/m2)', DivedNum, 7, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                                      #デバイスドライバ閉じる
   
   #描画b3
   z  <- read_data('out_lai_max_g.txt')
   png('out_lai_max_g.png', width=width_size, height=height_size)                  #デバイスドライバ開く
   draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)                              #図本体の描画
#   draw_panel('Annual Maximum LAI of grass PFTs (m2/m2)', DivedNum, 7, PannelStep) #カラーパネルを重ね書き
   dev.off()                                                                       #デバイスドライバ閉じる
  
##____________________ LAI_every_month ____________________ 
#   DivedNum   <- 14                    #カラーパネルの分割数
#   PannelStep <- 0.25                   #カラーパネルの増分
#   col <- set_color_terrain(DivedNum)  #分割点と色の設定
#   
#   for (month in 1:12) {
#      if (month <10) {z <- read_data(paste('out_lai_0', month, '.txt', sep=""))}
#      else           {z <- read_data(paste('out_lai_' , month, '.txt', sep=""))}
#      
#      png( paste('out_lai_month',month,'.png'), width=width_size, height=height_size)
#         draw_dist_LAI (DivedNum, PannelStep, col, x, y, z)
#      dev.off()
#   }
