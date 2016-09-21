install.packages("data.table")
library(data.table)
library(dplyr)
cols<-c("SERIALNO","SPORDER","ST","AGEP","CIT","NWLK","SCH","SCHL","WKL","WRK","FOD1P","OCCP")
data14a<-fread("/Users/yueqizhang/Documents/w5243 ads/project1/data/2014/csv_pus/ss14pusa.csv",select=cols)
data14b<-fread("/Users/yueqizhang/Documents/w5243 ads/project1/data/2014/csv_pus/ss14pusb.csv",select=cols)
data14<-rbind(data14a,data14b)
data14<-filter(data14,FOD1P!="NA")
stem<-matrix(rep(0,dim(data14)[1]),nrow=dim(data14)[1])
stem.field<-c(1103,1104,1105,1106,1301,1401,2100,2101,2102,2105,2106,2107,2400,2401,2402,2403,2404,2407,2408,2409,2410,2411,2412,2413,2414,2415,2416,2417,2418,2419,2499,2500,2501,2502,2503,2504,2599,3600,3601,3602,3603,3604,3605,3606,3607,3608,3609,3611,3699,3700,3701,3702,3801,4002,4005,4006,5000,5001,5002,5003,5004,5005,5006,5007,5008,5102,5206,5901,6105,6202,6212)
l<-length(stem.field)
order.fod<-c()
for(i in 1:l)
{
  order.fod<-c(order.fod,which(data14$FOD1P==stem.field[i]))
}
order.fod<-sort(order.fod)
stem[order.fod]<-1
colnames(stem)<-"STEM"
data14<-cbind(data14,stem)
data14$CIT[which(data14$CIT==5)]<-0
data14$CIT[which(data14$CIT==4)]<-1
data14$CIT[which(data14$CIT==3)]<-1
data14$CIT[which(data14$CIT==2)]<-1
data14<-subset(data14,data14$SCH==1)
data14<-filter(data14,data14$SCHL>20)
data14$OCCP[which(data14$NWLK==1)]<-"NA"
data14$OCCP[which(data14$WKL==2)]<-"NA"
data14$OCCP[which(data14$WRK==2)]<-"NA"
data14$OCCP[which(data14$OCCP==9920)]<-"NA"
occp.type<-matrix(rep(0,dim(data14)[1]),nrow=dim(data14)[1])
occp.mgr<-c(0010,0020,0040,0050,0060,0100,0120,0135,0136,0137,0140,0150,0160,0205,0220,0230,0300,0310,0330,0340,0350,0360,0410,0420,0425,0430)
for(i in 1:length(occp.mgr))
{
  occp.type[which(data14$OCCP==occp.mgr[i])]<-"MGR"
}
occp.bus<-c(0500,0510,0520,0530,0540,0565,0600,0630,0640,0650,0700,0710,0725,0726,0735,0740)
for(i in 1:length(occp.bus))
{
  occp.type[which(data14$OCCP==occp.bus[i])]<-"BUS"
}
occp.fin<-c(0800,0810,0820,0830,0840,0850,0860,0900,0910,0930,0940,0950)
for(i in 1:length(occp.fin))
{
  occp.type[which(data14$OCCP==occp.fin[i])]<-"FIN"
}
occp.cmm<-c(1005,1006,1007,1010,1020,1030,1050,1060,1105,1106,1107,1200,1220,1240)
for(i in 1:length(occp.cmm))
{
  occp.type[which(data14$OCCP==occp.cmm[i])]<-"CMM"
}
occp.eng<-c(1300,1310,1320,1340,1350,1360,1400,1410,1420,1430,1440,1450,1460,1520,1530,1540,1550,1560)
for(i in 1:length(occp.eng))
{
  occp.type[which(data14$OCCP==occp.eng[i])]<-"ENG"
}
occp.sci<-c(1600,1610,1640,1650,1700,1710,1720,1740,1760,1800,1820,1840,1860,1900,1910,1920,1930,1965)
for(i in 1:length(occp.sci))
{
  occp.type[which(data14$OCCP==occp.sci[i])]<-"SCI"
}
occp.cms<-c(2000,2010,2015,2016,2025,2040,2050,2060)
for(i in 1:length(occp.cms))
{
  occp.type[which(data14$OCCP==occp.cms[i])]<-"CMS"
}
occp.lgl<-c(2100,2105,2145,2160)
for(i in 1:length(occp.lgl))
{
  occp.type[which(data14$OCCP==occp.lgl[i])]<-"LGL"
}
occp.edu<-c(2200,2300,2310,2320,2330,2340,2440,2540,2550)
for(i in 1:length(occp.edu))
{
  occp.type[which(data14$OCCP==occp.edu[i])]<-"EDU"
}
occp.ent<-c(2600,2630,2700,2710,2720,2740,2750,2760,2800,2810,2825,2830,2840,2850,2860,2900,2910,2920)
for(i in 1:length(occp.ent))
{
  occp.type[which(data14$OCCP==occp.ent[i])]<-"ENT"
}
occp.med<-c(3000,3010,3030,3040,3050,3060,3110,3120,3140,3150,3160,3200,3210,3220,3230,3245,3250,3255,3256,3258,3260,3300,3310,3320,3400,3420,3500,3510,3520,3535,3540)
for(i in 1:length(occp.med))
{
  occp.type[which(data14$OCCP==occp.med[i])]<-"MED"
}
occp.hls<-c(3600,3610,3620,3630,3640,3645,3646,3647,3648,3649,3655)
for(i in 1:length(occp.hls))
{
  occp.type[which(data14$OCCP==occp.hls[i])]<-"HLS"
}
occp.prt<-c(3700,3710,3720,3730,3740,3750,3800,3820,3840,3850,3900,3910,3930,3940,3945,3955)
for(i in 1:length(occp.prt))
{
  occp.type[which(data14$OCCP==occp.prt[i])]<-"PRT"
}
occp.eat<-c(4000,4010,4020,4030,4040,4050,4060,4110,4120,4130,4140,4150)
for(i in 1:length(occp.eat))
{
  occp.type[which(data14$OCCP==occp.eat[i])]<-"EAT"
}
occp.cln<-c(4200,4210,4220,4230,4240,4250)
for(i in 1:length(occp.cln))
{
  occp.type[which(data14$OCCP==occp.cln[i])]<-"CLN"
}
occp.prs<-c(4300,4320,4340,4350,4400,4410,4420,4430,4460,4465,4500,4510,4520,4530,4540,4600,4610,4620,4640,4650)
for(i in 1:length(occp.prs))
{
  occp.type[which(data14$OCCP==occp.prs[i])]<-"PRS"
}
occp.sal<-c(4700,4710,4720,4740,4750,4760,4800,4810,4820,4830,4840,4850,4900,4920,4930,4940,4950,4965)
for(i in 1:length(occp.sal))
{
  occp.type[which(data14$OCCP==occp.sal[i])]<-"SAL"
}
occp.off<-c(5000,5010,5020,5030,5100,5110,5120,5130,5140,5150,5160,5165,5200,5220,5230,5240,5250,5260,5300,5310,5320,5330,5340,5350,5360,5400,5410,5420,5500,5510,5520,5530,5540,5550,5560,5600,5610,5620,5630,5700,5800,5810,5820,5840,5850,5860,5900,5910,5920,5940)
for(i in 1:length(occp.off))
{
  occp.type[which(data14$OCCP==occp.off[i])]<-"OFF"
}
occp.fff<-c(6005,6010,6040,6050,6100,6120,6130)
for(i in 1:length(occp.fff))
{
  occp.type[which(data14$OCCP==occp.fff[i])]<-"FFF"
}
occp.con<-c(6200,6210,6220,6230,6240,6250,6260,6300,6320,6330,6355,6360,6400,6420,6440,6460,6515,6520,6530,6600,6660,6700,6710,6720,6730,6740,6765)
for(i in 1:length(occp.con))
{
  occp.type[which(data14$OCCP==occp.con[i])]<-"CON"
}
occp.ext<-c(6800,6820,6830,6840,6940)
for(i in 1:length(occp.ext))
{
  occp.type[which(data14$OCCP==occp.ext[i])]<-"EXT"
}
occp.rpr<-c(7000,7010,7020,7030,7040,7100,7110,7120,7130,7140,7150,7160,7200,7210,7220,7240,7260,7300,7315,7320,7330,7340,7350,7360,7410,7420,7430,7510,7540,7560,7610,7630)
for(i in 1:length(occp.rpr))
{
  occp.type[which(data14$OCCP==occp.rpr[i])]<-"RPR"
}
occp.prd<-c(7700,7710,7720,7730,7740,7750,7800,7810,7830,7840,7850,7855,7900,7920,7930,7940,7950,8030,8040,8100,8130,8140,8220,8250,8255,8256,8300,8310,8320,8330,8350,8400,8410,8420,8450,8460,8500,8510,8530,8540,8550,8600,8610,8620,8630,8640,8650,8710,8720,8730,8740,8750,8760,8800,8810,8830,8850,8910,8920,8930,8940,8950,8965)
for(i in 1:length(occp.prd))
{
  occp.type[which(data14$OCCP==occp.prd[i])]<-"PRD"
}
occp.trn<-c(9000,9030,9040,9050,9110,9120,9130,9140,9150,9200,9240,9260,9300,9310,9350,9360,9410,9415,9420,9510,9520,9560,9600,9610,9620,9630,9640,9650,9720,9750)
for(i in 1:length(occp.trn))
{
  occp.type[which(data14$OCCP==occp.trn[i])]<-"TRN"
}
occp.mil<-c(9800,9810,9820,9830,9920)
for(i in 1:length(occp.mil))
{
  occp.type[which(data14$OCCP==occp.mil[i])]<-"MIL"
}
colnames(occp.type)<-"OCCP.TYPE"
data14<-cbind(data14,occp.type)
write.csv(data14,file="data2014.csv",row.names = FALSE)
data.14<-fread("/Users/yueqizhang/Desktop/data2014.csv",select=c("ST","AGEP","CIT","SCHL","FOD1P","OCCP","STEM","OCCP.TYPE"))
write.csv(data.14,file="fndata2014.csv",row.names = FALSE)

data.14.noncit<-subset(data.14,data.14$CIT==0)
data.14.cit<-subset(data.14,data.14$CIT!=0)
write.csv(data.14.cit,file="data2014_cit.csv",row.names=FALSE)
write.csv(data.14.noncit,file="data2014_noncit.csv",row.names=FALSE)
