geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for November  from 1901 to 1960 (in mm) for N.M.M.T.")
dec<-ggplot(data = first, aes(x = YEAR, y = DEC)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for December  from 1901 to 1960 (in mm) for N.M.M.T.")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
library(grid)
# Make a list from the ... arguments and plotlist
plots <- c(list(...), plotlist)
numPlots = length(plots)
# If layout is NULL, then use 'cols' to determine layout
if (is.null(layout)) {
# Make the panel
# ncol: Number of columns of plots
# nrow: Number of rows needed, calculated from # of cols
layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
ncol = cols, nrow = ceiling(numPlots/cols))
}
if (numPlots==1) {
print(plots[[1]])
} else {
# Set up the page
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
# Make each plot, in the correct location
for (i in 1:numPlots) {
# Get the i,j matrix positions of the regions that contain this subplot
matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
layout.pos.col = matchidx$col))
}
}
}
multiplot(ond,oct,nov,dec, cols = 1)
nov<-ggplot(data = first, aes(x = YEAR, y = NOV)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for November  from 1901 to 1960 (in mm) for N.M.M.T.")
multiplot(ond,oct,nov,dec, cols = 1)
ond<-ggplot(data = first, aes(x = YEAR, y = OND)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Post Monsoon  from 1901 to 1960 (in mm) for N.M.M.T.")
oct<-ggplot(data = first, aes(x = YEAR, y = OCT)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for October  from 1901 to 1960 (in mm) for N.M.M.T.")
nov<-ggplot(data = first, aes(x = YEAR, y = NOV)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for November  from 1901 to 1960 (in mm) for N.M.M.T.")
dec<-ggplot(data = first, aes(x = YEAR, y = DEC)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for December  from 1901 to 1960 (in mm) for N.M.M.T.")
multiplot(ond,oct,nov,dec, cols = 1)
library(Kendall)
Mk_annual<-MannKendall(first$ANNUAL)
(Mk_annual $ s + 1)/sqrt(Mk_annual$varS)
Mk_jf<-MannKendall(first$JF)
(Mk_jf $ s + 1)/sqrt(Mk_jf$varS)
Mk_jan<-MannKendall(first$JAN)
(Mk_jan $ s + 1)/sqrt(Mk_jan$varS)
Mk_feb<-MannKendall(first$FEB)
(Mk_feb $ s + 1)/sqrt(Mk_feb$varS)
Mk_mam<-MannKendall(first$MAM)
(Mk_mam $ s + 1)/sqrt(Mk_mam$varS)
Mk_mar<-MannKendall(first$MAR)
(Mk_mar $ s + 1)/sqrt(Mk_mar$varS)
Mk_apr<-MannKendall(first$APR)
(Mk_apr $ s + 1)/sqrt(Mk_apr$varS)
Mk_may<-MannKendall(first$MAY)
(Mk_may $ s + 1)/sqrt(Mk_may$varS)
Mk_jjas<-MannKendall(first$JJAS)
(Mk_jjas $ s + 1)/sqrt(Mk_jjas$varS)
Mk_jun<-MannKendall(first$JUN)
(Mk_jun $ s + 1)/sqrt(Mk_jun$varS)
Mk_jul<-MannKendall(first$JUL)
(Mk_jul $ s + 1)/sqrt(Mk_jul$varS)
Mk_aug<-MannKendall(first$AUG)
(Mk_aug $ s + 1)/sqrt(Mk_aug$varS)
Mk_sep<-MannKendall(first$SEP)
(Mk_sep $ s + 1)/sqrt(Mk_sep$varS)
Mk_ond<-MannKendall(first$OND)
(Mk_ond $ s + 1)/sqrt(Mk_ond$varS)
Mk_oct<-MannKendall(first$OCT)
(Mk_oct $ s + 1)/sqrt(Mk_oct$varS)
Mk_nov<-MannKendall(first$NOV)
(Mk_nov $ s + 1)/sqrt(Mk_novt$varS)
(Mk_nov $ s + 1)/sqrt(Mk_nov$varS)
Mk_dec<-MannKendall(first$DEC)
(Mk_dec $ s + 1)/sqrt(Mk_dec$varS)
library(trend)
ts_annual<-ts(first$ANNUAL,frequency = 1,start = 1901)
ts_annual
annual_sense<-sens.slope(ts_annual)
annual_sense
annual
ts_jf<-ts(first$JF,frequency = 1,start = 1901)
ts_jf
jf_sense<-sens.slope(ts_jf)
ts_jf
jf_sense
jan_sense<-sens.slope(ts_jan)
ts_jan<-ts(first$JAN,frequency = 1,start = 1901)
jan_sense<-sens.slope(ts_jan)
jan_sense
ts_feb<-ts(first$FEB,frequency = 1,start = 1901)
feb_sense<-sens.slope(ts_feb)
feb_sense
ts_mam<-ts(first$MAM,frequency = 1,start = 1901)
mam_sense<-sens.slope(ts_mam)
mam_sense
ts_mar<-ts(first$MAR,frequency = 1,start = 1901)
mar_sense<-sens.slope(ts_mar)
mar_sense
ts_apr<-ts(first$APR,frequency = 1,start = 1901)
apr_sense<-sens.slope(ts_apr)
apr_sense
ts_may<-ts(first$MAY,frequency = 1,start = 1901)
may_sense<-sens.slope(ts_may)
may_sense
ts_jjas<-ts(first$JJAS,frequency = 1,start = 1901)
jjas_sense<-sens.slope(ts_jjas)
jjas_sense
ts_jun<-ts(first$JUN,frequency = 1,start = 1901)
jun_sense<-sens.slope(ts_jun)
jun_sense
ts_jul<-ts(first$JUL,frequency = 1,start = 1901)
jul_sense<-sens.slope(ts_jul)
jul_sense
ts_aug<-ts(first$AUG,frequency = 1,start = 1901)
aug_sense<-sens.slope(ts_aug)
aug_sense
ts_sep<-ts(first$SEP,frequency = 1,start = 1901)
sep_sense<-sens.slope(ts_sep)
sep_sense
ts_ond<-ts(first$OND,frequency = 1,start = 1901)
ond_sense<-sens.slope(ts_ond)
ond_sense
ts_oct<-ts(first$OCT,frequency = 1,start = 1901)
oct_sense<-sens.slope(ts_oct)
oct_sense
ts_nov<-ts(first$NOV,frequency = 1,start = 1901)
nov_sense<-sens.slope(ts_nov)
nov_sense
ts_dec<-ts(first$DEC,frequency = 1,start = 1901)
dec_sense<-sens.slope(ts_dec)
dec_sense
library(readr)
X1961_2015a <- read_csv("~/yeardiv/1961-2015a.csv")
View(X1961_2015a)
library(readr)
X1961_2015a <- read_csv("~/yeardiv/1961-2015a.csv")
View(X1961_2015a)
View(first)
View(X1961_2015a)
remove(X1961_2015a)
remove(X1901_1960)
library(readr)
second <- read_csv("~/yeardiv/1961-2015a.csv")
View(second)
annual<-ggplot(data = second, aes(x = YEAR, y = YEAR)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Annual Rainfall  from 1961 to 2015 (in mm) for N.M.M.T.")
jf<-ggplot(data = second, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
mam<-ggplot(data = second, aes(x = YEAR, y = MAM)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Pre Monsoon(MAM) from 1961 to 2015 (in mm) for N.M.M.T.")
jjas<-ggplot(data = second, aes(x = YEAR, y = JJAS)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Monsoon(JJAS) from 1961 to 2015 (in mm) for N.M.M.T.")
ond<-ggplot(data = second, aes(x = YEAR, y = OND)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Post Monsoon(OND) from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(annual,jf,mam,jjas,ond,cols = 1)
jf<-ggplot(data = second, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
jan<-ggplot(data = second, aes(x = YEAR, y = JAN)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for January from 1961 to 2015 (in mm) for N.M.M.T.")
feb<-ggplot(data = second, aes(x = YEAR, y = FEB)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for February from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(jf,jan,feb,cols = 1)
mam<-ggplot(data = second, aes(x = YEAR, y = MAM)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Pre Monsoon(MAM) from 1961 to 2015 (in mm) for N.M.M.T.")
mar<-ggplot(data = second, aes(x = YEAR, y = MAR)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for March from 1961 to 2015 (in mm) for N.M.M.T.")
apr<-ggplot(data = second, aes(x = YEAR, y = APR)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for April from 1961 to 2015 (in mm) for N.M.M.T.")
may<-ggplot(data = second, aes(x = YEAR, y = MAY)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for May from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(mam,mar,apr,may,cols = 1)
jjas<-ggplot(data = second, aes(x = YEAR, y = JJAS)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Monsoon(JJAS) from 1961 to 2015 (in mm) for N.M.M.T.")
jun<-ggplot(data = second, aes(x = YEAR, y = JUN)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for June from 1961 to 2015 (in mm) for N.M.M.T.")
jul<-ggplot(data = second, aes(x = YEAR, y = JUL)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for July from 1961 to 2015 (in mm) for N.M.M.T.")
aug<-ggplot(data = second, aes(x = YEAR, y = AUG)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for August from 1961 to 2015 (in mm) for N.M.M.T.")
sep<-ggplot(data = second, aes(x = YEAR, y = SEP)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for September from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(jjas,jun,jul,aug,cols = 1)
ond<-ggplot(data = second, aes(x = YEAR, y = OND)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Post Monsoon(OND) from 1961 to 2015 (in mm) for N.M.M.T.")
oct<-ggplot(data = second, aes(x = YEAR, y = OCT)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for October from 1961 to 2015 (in mm) for N.M.M.T.")
nov<-ggplot(data = second, aes(x = YEAR, y = NOV)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for November from 1961 to 2015 (in mm) for N.M.M.T.")
dec<-ggplot(data = second, aes(x = YEAR, y = dec)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for December from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(ond,oct,nov,cols = 1)
multiplot(ond,oct,nov,dec,cols = 1)
dec<-ggplot(data = second, aes(x = YEAR, y = dec)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for December from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(ond,oct,nov,dec,cols = 1)
dec<-ggplot(data = second, aes(x = YEAR, y = DEC)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Monthly Rainfall for December from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(ond,oct,nov,dec,cols = 1)
Mk_annual<-MannKendall(second$ANNUAL)
(Mk_annual $ s + 1)/sqrt(Mk_annual$varS)
Mk_aft_annual<-MannKendall(second$ANNUAL)
(Mk_aft_annual $ s + 1)/sqrt(Mk_aft_annual$varS)
remove(Mk_annual)
Mk_aft_jf<-MannKendall(second$JFL)
Mk_aft_jf<-MannKendall(second$JF)
(Mk_aft_jf $ s + 1)/sqrt(Mk_aft_jf$varS)
Mk_aft_jan<-MannKendall(second$JAN)
(Mk_aft_jan $ s + 1)/sqrt(Mk_aft_jan$varS)
Mk_aft_feb<-MannKendall(second$FEB)
(Mk_aft_feb $ s + 1)/sqrt(Mk_aft_feb$varS)
Mk_aft_mam<-MannKendall(second$MAM)
(Mk_aft_mam $ s + 1)/sqrt(Mk_aft_mam$varS)
Mk_aft_mar<-MannKendall(second$MAR)
(Mk_aft_mar $ s + 1)/sqrt(Mk_aft_mar$varS)
Mk_aft_apr<-MannKendall(second$APR)
(Mk_aft_apr $ s + 1)/sqrt(Mk_aft_apr$varS)
Mk_aft_may<-MannKendall(second$MAY)
(Mk_aft_may $ s + 1)/sqrt(Mk_aft_may$varS)
Mk_aft_jjas<-MannKendall(second$JJAS)
(Mk_aft_jjas $ s + 1)/sqrt(Mk_aft_jjas$varS)
Mk_aft_jun<-MannKendall(second$JUN)
(Mk_aft_jun $ s + 1)/sqrt(Mk_aft_jun$varS)
Mk_aft_jul<-MannKendall(second$JUL)
(Mk_aft_jul $ s + 1)/sqrt(Mk_aft_jul$varS)
Mk_aft_aug<-MannKendall(second$AUG)
(Mk_aft_aug $ s + 1)/sqrt(Mk_aft_aug$varS)
Mk_aft_sep<-MannKendall(second$SEP)
(Mk_aft_sep $ s + 1)/sqrt(Mk_aft_sep$varS)
Mk_aft_ond<-MannKendall(second$OND)
(Mk_aft_ond $ s + 1)/sqrt(Mk_aft_ond$varS)
Mk_aft_oct<-MannKendall(second$OCT)
(Mk_aft_oct $ s + 1)/sqrt(Mk_aft_oct$varS)
Mk_aft_nov<-MannKendall(second$NOV)
(Mk_aft_nov $ s + 1)/sqrt(Mk_aft_nov$varS)
Mk_aft_dec<-MannKendall(second$DEC)
(Mk_aft_dec $ s + 1)/sqrt(Mk_aft_dec$varS)
Mk_annual<-MannKendall(first$ANNUAL)
(Mk_aft_annual $ s + 1)/sqrt(Mk_aft_annual$varS)
ts_aft_annual<-ts(second$ANNUAL,frequency = 1,start = 1961)
aft_annual_sense<-sens.slope(ts_aft_annual)
aft_annual_sense
ts_aft_jf<-ts(second$JF,frequency = 1,start = 1961)
aft_jf_sense<-sens.slope(ts_aft_jf)
aft_jf_sense
ts_aft_jan<-ts(second$JAN,frequency = 1,start = 1961)
aft_jan_sense<-sens.slope(ts_aft_jan)
aft_jan_sense
ts_aft_feb<-ts(second$FEB,frequency = 1,start = 1961)
aft_feb_sense<-sens.slope(ts_aft_feb)
aft_feb_sense
ts_aft_mam<-ts(second$MAM,frequency = 1,start = 1961)
aft_mam_sense<-sens.slope(ts_aft_mam)
aft_mam_sense
ts_aft_mar<-ts(second$MAR,frequency = 1,start = 1961)
aft_mar_sense<-sens.slope(ts_aft_mar)
aft_mar_sense
ts_aft_apr<-ts(second$APR,frequency = 1,start = 1961)
aft_apr_sense<-sens.slope(ts_aft_apr)
aft_apr_sense
ts_aft_may<-ts(second$MAY,frequency = 1,start = 1961)
aft_may_sense<-sens.slope(ts_aft_may)
aft_may_sense
ts_aft_jjas<-ts(second$JJAS,frequency = 1,start = 1961)
aft_jjas_sense<-sens.slope(ts_aft_jjas)
aft_jjas_sense
ts_aft_jun<-ts(second$JUN,frequency = 1,start = 1961)
aft_jun_sense<-sens.slope(ts_aft_jun)
aft_jun_sense
ts_aft_jul<-ts(second$JUL,frequency = 1,start = 1961)
aft_jul_sense<-sens.slope(ts_aft_jul)
aft_jul_sense
ts_aft_aug<-ts(second$AUG,frequency = 1,start = 1961)
aft_aug_sense<-sens.slope(ts_aft_aug)
aft_aug_sense
ts_aft_sep<-ts(second$SEP,frequency = 1,start = 1961)
aft_sep_sense<-sens.slope(ts_aft_sep)
aft_sep_sense
ts_aft_ond<-ts(second$OND,frequency = 1,start = 1961)
aft_ond_sense<-sens.slope(ts_aft_ond)
aft_ond_sense
ts_aft_oct<-ts(second$OCT,frequency = 1,start = 1961)
aft_oct_sense<-sens.slope(ts_aft_oct)
aft_oct_sense
ts_aft_nov<-ts(second$NOV,frequency = 1,start = 1961)
aft_nov_sense<-sens.slope(ts_aft_nov)
aft_nov_sense
ts_aft_dec<-ts(second$DEC,frequency = 1,start = 1961)
aft_dec_sense<-sens.slope(ts_aft_dec)
aft_dec_sense
save.image("~/yeardiv/1961-2015/history.RData")
save.image("~/yeardiv/1961-2015/env.RData")
savehistory("~/yeardiv/1961-2015/history.Rhistory")
Mk_after60<-MannKendall(second$JF)
install.packages(Kendall)
library(Kendall)
Mk_after60<-MannKendall(second$JF)
jf<-MannKendall(after60$JF)
(jf $ s + 1)/sqrt(after60$varS)
(jf $ s + 1)/sqrt(jf$varS)
ts_jf<-ts(after60$jf,frequency = 1,start = 1961)
jf<-ts(after60$jf,frequency = 1,start = 1961)
library(trend)
ts_jf<-ts(after60$jf,frequency = 1,start = 1961)
ts_jf<-ts(after60$jf,frequency = 1,start = 1961)
(jf $ s + 1)/sqrt(jf$varS)
sens.slope(after60$jf)
sens.slope(after60$JF)
ts_jf<-ts(after60$jf,frequency = 1,start = 1961)
jf<-ts(after60$jf,frequency = 1,start = 1961)
Mk_mam<-MannKendall(after60st$MAM)
Mk_mam<-MannKendall(after60$MAM)
(Mk_mam $ s + 1)/sqrt(Mk_mam$varS)
jf_sense<-sens.slope(ts_jf)
ts_jf
jf_sense
Mk_annual<-MannKendall(after60$ANNUAL)
(Mk_annual $ s + 1)/sqrt(Mk_annual$varS)
Mk_aft_mam<-MannKendall(after60$MAM)
(Mk_aft_mam $ s + 1)/sqrt(Mk_aft_mam$varS)
Mk_aft_jjas<-MannKendall(after60$JJAS)
(Mk_aft_jjas $ s + 1)/sqrt(Mk_aft_jjas$varS)
ts_aft_annual<-ts(after60$ANNUAL,frequency = 1,start = 1961)
aft_annual_sense<-sens.slope(ts_aft_annual)
aft_annual_sense
ts_aft_jf<-ts(after60$JF,frequency = 1,start = 1961)
aft_jf_sense<-sens.slope(ts_aft_jf)
aft_jf_sense
ts_annual <- ts(after60$ANNUAL, frequency = 1, start = 1961)
sens_annual <- sens.slope(ts_annual, level = 0.95)
mk_annual <- MannKendall(after60$ANNUAL)
z_annual <- (mk_annual$S + 1)/sqrt(mk_annual$varS)
z_annual
ts_jf <- ts(after60$ANNUAL, frequency = 1, start = 1961)
sens_jf <- sens.slope(ts_annual, level = 0.95)
MK_jf <- MannKendall(after60$ANNUAL)
z_jf <- (mk_annual$S + 1)/sqrt(mk_annual$varS)
z_jf
ts_mam <- ts(after60$ANNUAL, frequency = 1, start = 1961)
sens_mam <- sens.slope(ts_annual, level = 0.95)
z_mam<- (mk_annual$S + 1)/sqrt(mk_annual$varS)
z_mam
ts_jf <- ts(after60$JF, frequency = 1, start = 1961)
sens_mam <- sens.slope(ts_jf, level = 0.95)
sens_jf <- sens.slope(ts_jf, level = 0.95)
MK_jf <- MannKendall(after60$JF)
z_jf<- (mk_jf$S + 1)/sqrt(mk_annual$varS)
z_jf<- (mk_jf$S + 1)/sqrt(mk_annual$varS)
ts_jf <- ts(after60$JF, frequency = 1, start = 1961)
sens_jf <- sens.slope(ts_jf, level = 0.95)
z_jf<- (mk_jf$S + 1)/sqrt(mk_annual$varS)
ts_jf <- ts(after_1970$JF, frequency = 1, start = 1961)
ts_jf <- ts(after60$JF, frequency = 1, start = 1961)
sens_jf <- sens.slope(ts_jf, level = 0.95)
mk_jf <- MannKendall(after60$JF)
z_jf <- (mk_jf$S + 1)/sqrt(mk_jf$varS)
z_jf
ts_mam <- ts(after60$JF, frequency = 1, start = 1961)
sens_mam <- sens.slope(ts_mam, level = 0.95)
mk_mam <- MannKendall(after60$JF)
z_mam <- (mk_mam$S + 1)/sqrt(mk_mam$varS)
z_mam
multiplot
jf
jf
jf<-ggplot(data = after60, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
library(library)
library(ggplot2)
jf<-ggplot(data = after60, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
jf
mam<-ggplot(data = after60, aes(x = YEAR, y = MAM)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Pre Monsoon(MAM) from 1961 to 2015 (in mm) for N.M.M.T.")
jjas<-ggplot(data = after60, aes(x = YEAR, y = JJAS)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Monsoon(JJAS) from 1961 to 2015 (in mm) for N.M.M.T.")
jjas
ond<-ggplot(data = after60, aes(x = YEAR, y = OND)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Post Monsoon(OND) from 1961 to 2015 (in mm) for N.M.M.T.")
ond
multiplot(annual,jf,mam,jjas,ond)
library(Kendall)
Mk_after60<-MannKendall(after60$JF)
Mk_after60<-MannKendall(after60$ANNUAL)
(Mk_annual $ s + 1)/sqrt(Mk_annual$varS)
ts_aft_annual<-ts(after60$ANNUAL,frequency = 1,start = 1961)
aft_annual_sense<-sens.slope(ts_aft_annual)
jf<-ggplot(data = after60, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
annual<-ggplot(data = after60, aes(x = YEAR, y = ANNUAL)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Annual Rainfall  from 1961 to 2015 (in mm) for N.M.M.T.")
jf<-ggplot(data = after60, aes(x = YEAR, y = JF)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Winter(JF)  from 1961 to 2015 (in mm) for N.M.M.T.")
mam<-ggplot(data = after60, aes(x = YEAR, y = MAM)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Pre Monsoon(MAM) from 1961 to 2015 (in mm) for N.M.M.T.")
jjas<-ggplot(data = after60, aes(x = YEAR, y = JJAS)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Monsoon(JJAS) from 1961 to 2015 (in mm) for N.M.M.T.")
ond<-ggplot(data = after60, aes(x = YEAR, y = OND)) +
stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
geom_smooth(method = "lm", se = FALSE, color = "red") +
geom_line(color = "blue", size = 1) + labs(x = "Year", y = "Yearly Rainfall in mm") + ggtitle("Seasonal Rainfall for Post Monsoon(OND) from 1961 to 2015 (in mm) for N.M.M.T.")
multiplot(annual,jf,mam,jjas,ond)
View(nmmt)
clear
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
data_full <- read.csv("household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
setwd('C:\John\Desktop')
setwd('C:\urmila\Desktop')
f <- file.choose()
f <- file.choose()
d <- read.csv(f)
data_full <- read.csv("household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
f <- file.choose()
d <- read.csv(f)
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(power) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
subpower <- subset(power,power$Date=="1/2/2007" | power$Date =="2/2/2007")
hist(as.numeric(as.character(subpower$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")
title(main="Global Active Power")
subpower$Date <- as.Date(subpower$Date, format="%d/%m/%Y")
subpower$Time <- strptime(subpower$Time, format="%H:%M:%S")
subpower[1:1440,"Time"] <- format(subpower[1:1440,"Time"],"2007-02-01 %H:%M:%S")
subpower[1441:2880,"Time"] <- format(subpower[1441:2880,"Time"],"2007-02-02 %H:%M:%S")
plot(subpower$Time,as.numeric(as.character(subpower$Global_active_power)),type="l",xlab="",ylab="Global Active Power (kilowatts)") 
save.image("C:\\Users\\urmila\\Documents\\plot2")
