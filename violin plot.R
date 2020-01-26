##-----------Installing the necessary packages if not installed
if (!require("plotly")) install.packages("plotly")
if (!require("brunnermunzel")) install.packages("brunnermunzel")
if (!require("readr")) install.packages("readr")
if (!require("processx")) install.packages("processx")

##-----------Loading the libraries required in this R code
library (plotly)
library (brunnermunzel)
library (readr)
library (processx)

##-----------Importing the 'Species pit.csv'file as sp from the 
##-----------python code output
sp <- read_csv("Species Pit.csv")

##-----------Creating a function to draw violin plots and attach
##-----------Brunner Munzel test results
violinplot <- function(test_num, grp1, grp2){
    BMtest <- brunnermunzel.test(x = sp$Pit[grp1],y = sp$Pit[grp2])
    test <- paste(test_num, '\n<i>p</i>:', formatC(BMtest$p.value, format = 'g', digits = 2),
                  'ES:', formatC(BMtest$estimate, format = 'g'), '\nCI:',
                  formatC(BMtest$conf.int[1], format = 'g'), '-', formatC(BMtest$conf.int[2], format = 'g'))
    
    p <- plot_ly(type = 'violin') %>%
      add_trace(y = sp$Pit[grp1], x0 = test, side = 'negative', width = 400, opacity = 0.85,
                pointpos = -0.4, points = 'suspectedoutliers', fillcolor = "#811f53",
                box = list(visible = T, width = 0.2), meanline = list(visible = T, color="#691a44"),
                line = list (color = "#42102a"), marker = list (color="#42102a", symbol="diamond")) %>% 
      add_trace(y = sp$Pit[grp2], x0 = test, side = 'positive', width = 400, opacity = 0.85,
                pointpos = 0.4, points = 'suspectedoutliers', fillcolor = "#d75b5a",
                box = list(visible = T, width = 0.2), meanline = list(visible = T, color="#a84848"),
                line = list (color = "#7e3130"), marker = list (color="#7e3130", symbol="diamond")) %>% 
      layout(yaxis = list(zeroline = F, color = "#42102a"), showlegend = F, width = 2800, height = 2800)
}

#..................Runing comparison tests..................#
##-----------The tests contating either of the groups with less
##-----------than 10 observations are #NOT to be executed
sub_p <- suppressWarnings(subplot(
  violinplot('Test 1', sp$code<=53, sp$code>=54),
  violinplot('Test 2', sp$code<=52, sp$code==53),
  violinplot('Test 3', sp$code<=48, sp$code<=52 & sp$code>=49),
  violinplot('Test 4', sp$code<=39, sp$code<=48 & sp$code>=40),
  violinplot('Test 5', sp$code<=32, sp$code<=39 & sp$code>=33),
  violinplot('Test 6', sp$code<=18, sp$code<=32 & sp$code>=19),
  violinplot('Test 7', sp$code<=10, sp$code<=18 & sp$code>=11),
  violinplot('Test 8', sp$code<=6, sp$code<=10 & sp$code>=7),
  #violinplot('Test 9', sp$code<=4, sp$code<=6 & sp$code>=5),
  violinplot('Test 10', sp$code==1, sp$code<=4 & sp$code>=2),
  #violinplot('Test 11', sp$code<=3 & sp$code>=2, sp$code==4),
  #violinplot('Test 12', sp$code==2, sp$code==3),
  #violinplot('Test 13', sp$code==5, sp$code==6),
  violinplot('Test 14', sp$code==7, sp$code<=10 & sp$code>=8),
  #violinplot('Test 15', sp$code<=10 & sp$code>=9, sp$code==8),
  violinplot('Test 16', sp$code==9, sp$code==10),
  violinplot('Test 17', sp$code==11, sp$code<=18 & sp$code>=12),
  violinplot('Test 18', sp$code==12, sp$code<=18 & sp$code>=13),
  violinplot('Test 19', sp$code==13, sp$code<=18 & sp$code>=14),
  violinplot('Test 20', sp$code==14, sp$code<=18 & sp$code>=15),
  violinplot('Test 21', sp$code<=16 & sp$code>=15, sp$code<=18 & sp$code>=17),
  #violinplot('Test 22', sp$code==15, sp$code==16),
  #violinplot('Test 23', sp$code==17, sp$code==18),
  #violinplot('Test 24', sp$code==19, sp$code<=32 & sp$code>=20),
  #violinplot('Test 25', sp$code<=21 & sp$code>=20, sp$code<=32 & sp$code>=22),
  #violinplot('Test 26', sp$code==20, sp$code==21),
  violinplot('Test 27', sp$code<=30 & sp$code>=22, sp$code<=32 & sp$code>=31),
  #violinplot('Test 28', sp$code==22, sp$code<=30 & sp$code>=23),
  #violinplot('Test 29', sp$code==23, sp$code<=30 & sp$code>=24),
  violinplot('Test 30', sp$code<=27 & sp$code>=24, sp$code<=30 & sp$code>=28),
  violinplot('Test 31', sp$code<=25 & sp$code>=24, sp$code<=27 & sp$code>=26),
  #violinplot('Test 32', sp$code==24, sp$code==25),
  #violinplot('Test 33', sp$code==26, sp$code==27),
  violinplot('Test 34', sp$code==28, sp$code<=30 & sp$code>=29),
  #violinplot('Test 35', sp$code==29, sp$code==30),
  #violinplot('Test 36', sp$code==31, sp$code==32),
  violinplot('Test 37', sp$code<=34 & sp$code>=33, sp$code<=39 & sp$code>=35),
  violinplot('Test 38', sp$code==33, sp$code==34),
  violinplot('Test 39', sp$code==35, sp$code<=39 & sp$code>=36),
  violinplot('Test 40', sp$code<=37 & sp$code>=36, sp$code<=39 & sp$code>=38),
  #violinplot('Test 41', sp$code==36, sp$code==37),
  #violinplot('Test 42', sp$code==38, sp$code==39),
  violinplot('Test 43', sp$code<=46 & sp$code>=40, sp$code<=48 & sp$code>=47),
  violinplot('Test 44', sp$code<=45 & sp$code>=40, sp$code==46),
  #violinplot('Test 45', sp$code<=44 & sp$code>=40, sp$code==45),
  violinplot('Test 46', sp$code<=42 & sp$code>=40, sp$code<=44 & sp$code>=43),
  #violinplot('Test 47', sp$code<=41 & sp$code>=40, sp$code==42),
  violinplot('Test 48', sp$code==40, sp$code==41),
  #violinplot('Test 49', sp$code==43, sp$code==44),
  #violinplot('Test 50', sp$code==47, sp$code==48),
  #violinplot('Test 51', sp$code<=50 & sp$code>=49, sp$code<=52 & sp$code>=51),
  violinplot('Test 52', sp$code==49, sp$code==50),
  #violinplot('Test 53', sp$code==51, sp$code==52),
  violinplot('Test 54', sp$code<=62 & sp$code>=54, sp$code>=63),
  violinplot('Test 55', sp$code<=56 & sp$code>=54, sp$code<=62 & sp$code>=57),
  violinplot('Test 56', sp$code==54, sp$code<=56 & sp$code>=55),
  #violinplot('Test 57', sp$code==55, sp$code==56),
  violinplot('Test 58', sp$code==57, sp$code<=62 & sp$code>=58),
  #violinplot('Test 59', sp$code==58, sp$code<=62 & sp$code>=59),
  #violinplot('Test 60', sp$code<=60 & sp$code>=59, sp$code<=62 & sp$code>=61),
  #violinplot('Test 61', sp$code==59, sp$code==60),
  #violinplot('Test 62', sp$code==61, sp$code==62),
  violinplot('Test 63', sp$code==63, sp$code>=64),
  violinplot('Test 64', sp$code<=69 & sp$code>=64, sp$code>=70),
  violinplot('Test 65', sp$code<=66 & sp$code>=64, sp$code<=69 & sp$code>=67),
  violinplot('Test 66', sp$code==64, sp$code<=66 & sp$code>=65),
  #violinplot('Test 67', sp$code==65, sp$code==66),
  #violinplot('Test 68', sp$code==67, sp$code<=69 & sp$code>=68),
  violinplot('Test 69', sp$code==68, sp$code==69),
  violinplot('Test 70', sp$code==70, sp$code>=71),
  violinplot('Test 71', sp$code<=77 & sp$code>=71, sp$code>=78),
  #violinplot('Test 72', sp$code==71, sp$code<=77 & sp$code>=72),
  #violinplot('Test 73', sp$code==72, sp$code<=77 & sp$code>=73),
  violinplot('Test 74', sp$code<=74 & sp$code>=73, sp$code<=77 & sp$code>=75),
  #violinplot('Test 75', sp$code==73, sp$code==74),
  #violinplot('Test 76', sp$code==75, sp$code<=77 & sp$code>=76),
  #violinplot('Test 77', sp$code==76, sp$code==77),
  violinplot('Test 78', sp$code<=81 & sp$code>=78, sp$code>=82),
  #violinplot('Test 79', sp$code<=79 & sp$code>=78, sp$code<=81 & sp$code>=80),
  #violinplot('Test 80', sp$code==78, sp$code==79),
  #violinplot('Test 81', sp$code==80, sp$code==81),
  violinplot('Test 82', sp$code<=85 & sp$code>=82, sp$code>=86),
  #violinplot('Test 83', sp$code<=83 & sp$code>=82, sp$code<=85 & sp$code>=84),
  #violinplot('Test 84', sp$code==82, sp$code==83),
  #violinplot('Test 85', sp$code==84, sp$code==85),
  #violinplot('Test 86', sp$code==86, sp$code<=88 & sp$code>=87),
  violinplot('Test 87', sp$code==87, sp$code==88), nrows = 7))%>%
  layout(yaxis = list(zeroline = F),showlegend = F)

##-----------Exporting the image with a PNG file extension
print(sub_p)
#orca(sub_p, "plot.png")
