##############################
library(ggplot2)             #
library(maptools)            #
library(spsurvey)            #
library(sp)                  #
library(ggmap)               #
library(MASS)                #
library(xts)                 #
library(RgoogleMaps)         #
library(xtable)              #
library(rgl)                 #
library(lattice)             #
library(gstat)               #
library(geoR)                #
library(plyr)                #
library(plotrix)             #
##############################

#=============================
#データ読み込みおよび列名命名
afg<-read.csv("afg.csv",stringsAsFactors=FALSE)
str(afg)
head(afg)
colnames(afg)


afg<-afg[,c(-26:-30)]
colnames(afg)<-c("ReportKey", "DateOccurred", "Type", "Category"
	, "TrackingNumber", "Title", "Summary", "Region"
	, "AttackBy", "ComplexAttack", "ReportingUnit", "UnitName"
	, "TypeOfUnit", "FriendlyWIA", "FriendlyKIA", "HostNationWIA"
	, "HostNationKIA", "CivilianWIA", "CivilianKIA", "EnemyWIA"
	, "EnemyKIA", "EnemyDetained", "MGRS", "Latitude", "Longitude")

#===============================
#===mapデータ読み込み===

# シェイプファイルの読み込み
afgmap0 <- readShapePoly("AFG_adm/AFG_adm0.shp")
afgdbf0 <- read.dbf("AFG_adm/AFG_adm0.dbf")
#plot(afgmap0)
afgmap1 <- readShapePoly("AFG_adm/AFG_adm1.shp")
afgdbf1 <- read.dbf("AFG_adm/AFG_adm1.dbf")
#plot(afgmap1)
afgmap2 <- readShapePoly("AFG_adm/AFG_adm2.shp")
afgdbf2 <- read.dbf("AFG_adm/AFG_adm2.dbf")
#plot(afgmap2)
bo.afg<-(afgmap0@polygons[[1]]@Polygons[[1]]@coords) #アフガン国境線
max(bo.afg)

#====州名マップ====
plot(afgmap1)
ProName<-(afgdbf1$NAME_1)

xxyy<-coordinates(afgmap1)

for( i in 1:length(ProName) ){
     text(xxyy[i,1], xxyy[i,2], ProName[i],cex=0.7)
}

#dev.copy2eps(file="ProMap.eps")




#===データ選別===

#=======データ選別前Regionごとマッピング=======
#Regionごとの色付けマッピング

unique(afg$Region)
east<-which(afg$Region=="RC EAST")
south<-which(afg$Region=="RC SOUTH")
capital<-which(afg$Region=="RC CAPITAL")
west<-which(afg$Region=="RC WEST")
north<-which(afg$Region=="RC NORTH")
unknown<-which(afg$Region=="UNKNOWN")
nonsel<-which(afg$Region=="NONE SELECTED")
na<-which(afg$Region=="")
Region<-afg$Region
RegionCol<-replace(Region, capital, 1)
RegionCol<-replace(RegionCol, east, 2)
RegionCol<-replace(RegionCol, south, 3)
RegionCol<-replace(RegionCol, west, 4)
RegionCol<-replace(RegionCol, north, 5)
RegionCol<-replace(RegionCol, unknown, 6)
RegionCol<-replace(RegionCol, nonsel, 7)
RegionCol<-replace(RegionCol, na, 8)


plot(afgmap1)
points(afg$Longitude,afg$Latitude,pch=".", col=RegionCol, cex=2)

#dev.copy2eps(file="RegionMap.eps")

#======================

#=============================
#===選別前死傷者数，死傷イベント数===
FKn<-sum(na.omit(afg$FriendlyKIA))
FriKIA<-which(afg$FriendlyKIA>0)
FKe<-length(FriKIA)
FWn<-sum(na.omit(afg$FriendlyWIA))
FriWIA<-which(afg$FriendlyWIA>0)
FWe<-length(FriWIA)

HKn<-sum(na.omit(afg$HostNationKIA))
HosKIA<-which(afg$HostNationKIA>0)
HKe<-length(HosKIA)
HWn<-sum(na.omit(afg$HostNationWIA))
HosWIA<-which(afg$HostNationWIA>0)
HWe<-length(HosWIA)

CKn<-sum(na.omit(afg$CivilianKIA))
CivKIA<-which(afg$CivilianKIA>0)
CKe<-length(CivKIA)
CWn<-sum(na.omit(afg$CivilianWIA))
CivWIA<-which(afg$CivilianWIA>0)
CWe<-length(CivWIA)

EKn<-sum(na.omit(afg$EnemyKIA))
EneKIA<-which(afg$EnemyKIA>0)
EKe<-length(CivKIA)
EWn<-sum(na.omit(afg$EnemyWIA))
EneWIA<-which(afg$EnemyWIA>0)
EWe<-length(EneWIA)


PreCut<-matrix( c(FKn,FWn,HKn,HWn,CKn,CWn,EKn,EWn,
	FKe,FWe,HKe,HWe,CKe,CWe,EKe,EWe),8,2)


#=========選別===========

#分析に使用しない列の削除
afg<-afg[c("DateOccurred","Type","Category","Summary","Region","AttackBy","FriendlyWIA", "FriendlyKIA", "HostNationWIA"
	, "HostNationKIA", "CivilianWIA", "CivilianKIA", "EnemyWIA"
	, "EnemyKIA", "EnemyDetained", "Latitude", "Longitude")]
head(afg)

 #地域不明なもの削除
omit.Region<-c(which(afg$Region=="UNKNOWN"),which(afg$Region==""),which(afg$Region=="NONE SELECTED"))
afg<-afg[-omit.Region,]

#アフガン外削除
bo.afg<-(afgmap0@polygons[[1]]@Polygons[[1]]@coords) #アフガン国境線
InOrOut<-point.in.polygon(afg$Longitude,afg$Latitude, bo.afg[,1], bo.afg[,2])
In<-which(InOrOut==1)
afg<-afg[In,]

#軍事的，暴力的イベントに絞る
#犯罪、ノンコンバットは除外
CE<-which(afg$Type=="Criminal Event")
NC<-which(afg$Type=="Non-Combat Event")
NN<-which(afg$Type=="")
afg<-afg[-c(CE,NC,NN),]

which(afg$Category=="Meeting")
afg[which(afg$Category=="Meeting"),]


unique(afg$Type)
unique(afg$Category)
unique(afg$AttackBy)
unique(afg$TypeOfUnit)
unique(afg$UnitName)
unique(afg$Region)
#===========================

#=============================
#===選別後死傷者数，死傷イベント数===
FKn<-sum(na.omit(afg$FriendlyKIA))
FriKIA<-which(afg$FriendlyKIA>0)
FKe<-length(FriKIA)
FWn<-sum(na.omit(afg$FriendlyWIA))
FriWIA<-which(afg$FriendlyWIA>0)
FWe<-length(FriWIA)

HKn<-sum(na.omit(afg$HostNationKIA))
HosKIA<-which(afg$HostNationKIA>0)
HKe<-length(HosKIA)
HWn<-sum(na.omit(afg$HostNationWIA))
HosWIA<-which(afg$HostNationWIA>0)
HWe<-length(HosWIA)

CKn<-sum(na.omit(afg$CivilianKIA))
CivKIA<-which(afg$CivilianKIA>0)
CKe<-length(CivKIA)
CWn<-sum(na.omit(afg$CivilianWIA))
CivWIA<-which(afg$CivilianWIA>0)
CWe<-length(CivWIA)

EKn<-sum(na.omit(afg$EnemyKIA))
EneKIA<-which(afg$EnemyKIA>0)
EKe<-length(CivKIA)
EWn<-sum(na.omit(afg$EnemyWIA))
EneWIA<-which(afg$EnemyWIA>0)
EWe<-length(EneWIA)


PostCut<-matrix( c(FKn,FWn,HKn,HWn,CKn,CWn,EKn,EWn,
	FKe,FWe,HKe,HWe,CKe,CWe,EKe,EWe),8,2)

#===選別前後死傷者数，死傷イベント数比較===
xtable(cbind(PreCut, PostCut))



#===DateOccurredをdateオブジェクトに
#===Regionをfactorオブジェクトに
afg$DateOccurred <- as.Date(afg$DateOccurred)
is.na(afg)



#=====データ切り出し======
#############################


eve.year <- as.numeric( substr(afg$DateOccurred, 1,4) )
afg2004 <- afg[which(eve.year == 2004),]
afg2005 <- afg[which(eve.year == 2005),]
afg2006 <- afg[which(eve.year == 2006),]
afg2007 <- afg[which(eve.year == 2007),]
afg2008 <- afg[which(eve.year == 2008),]
afg2009 <- afg[which(eve.year == 2009),]

#======================
#===アフガニスタンの米軍基地===

kandahar<-data.frame(Lon=65.848, Lat=31.506)
shindand<-data.frame(Lon=62.261, Lat=33.391)
bagram<-data.frame(Lon=34.946, Lat=69.265)
leatherneck<-data.frame(Lon=64, Lat=31)
delaram<-data.frame(Lon=63.431, Lat=32.164)


#====================
#====stan=====
#AttackByの違いによるFriend死傷者数の差異
#1.敵によるイベントでの味方死傷者
#2.味方によるイベントでの味方死傷者
fri<-which(afg$AttackBy=="FRIEND")
ABFriendly<-afg[fri,]
ene<-which(afg$AttackBy=="ENEMY")
ABEnemy<-afg[ene,]

ABEnemy$FriendlyKIA[is.na(ABEnemy$FriendlyKIA)]<-0
ABEnemy$FriendlyWIA[is.na(ABEnemy$FriendlyWIA)]<-0
ABFriendly$FriendlyKIA[is.na(ABFriendly$FriendlyKIA)]<-0
ABFriendly$FriendlyWIA[is.na(ABFriendly$FriendlyWIA)]<-0

ABEn <- ABEnemy$FriendlyKIA + ABEnemy$FriendlyWIA
y1<-as.vector(ABEn)
n1<-length(y1)

ABFn <- ABFriendly$FriendlyKIA + ABFriendly$FriendlyWIA
y2<-as.vector(ABFn)
n2<-length(y2)

summary(y1)
summary(y2)

hist(y1,breaks=40)
hist(y1,breaks=40, ylim=c(0,1000)) 

hist(y2,breaks=10)
hist(y2,breaks=10, ylim=c(0,200))



#####ゼロ過剰ポアソン#####
#library(rstan)
#model <- stan_model("zip2.stan")
#fit <- sampling(model, data = list(N1=n1, N2=n2, y1=y1, y2=y2),
#                chains = 4,
#                iter = 11000, warmup = 1000, thin = 1)
#print(fit, digits = 3)
#traceplot(fit)
#save(fit, file="afgZIP2.dat")


#library(coda)
#fit.coda<-mcmc.list(lapply(1:ncol(fit),function(x) mcmc(as.array(fit)[,x,])))
#plot(fit.coda[[4]])


####################





#======3Dで可視化=======
#======死傷者数========
#===関数===

# Function to interleave the elements of two vectors
interleave <- function(v1, v2)  as.vector(rbind(v1,v2))

#======
#=======Friendly=======
head(afg)
Friendly <- afg$FriendlyWIA + afg$FriendlyKIA
Enemy <- afg$EnemyWIA + afg$EnemyKIA + afg$EnemyDetained
Host <- afg$HostNationWIA + afg$HostNationKIA
Civilian <- afg$CivilianWIA + afg$CivilianKIA 

FriHeat <- data.frame(afg$Longitude,afg$Latitude,Friendly)
colnames(FriHeat) <- c( "Lon","Lat","Friendly")
head(FriHeat)

summary(afg$Latitude)
summary(afg$Longitude)

FriHeat<-round(FriHeat,1)
FriHeat$Friendly[is.na(FriHeat$Friendly)]<-0
head(FriHeat)

FriHeat0<-na.omit(ddply(FriHeat, .(Lon,Lat), numcolwise(sum)))
head(FriHeat0)

head(bo.afg)

Lon<-round(bo.afg[,1],1)
Lat<-round(bo.afg[,2],1)
head(Lon)

Lon<-as.numeric(unique(Lon))
Lat<-as.numeric(unique(Lat))
head(Lon)

afg.grid <- expand.grid(Lon=seq( min(Lon), max(Lon), by=0.1), 
      Lat=seq( min(Lat), max(Lat), by=0.1 ))

head(afg.grid)
#アフガン外削除
bo.afg<-(afgmap0@polygons[[1]]@Polygons[[1]]@coords) #アフガン国境線
InOrOut.grid<-point.in.polygon(afg.grid$Lon,afg.grid$Lat, bo.afg[,1], bo.afg[,2])
In.grid<-which(InOrOut.grid==1)
afg.grid<-afg.grid[In.grid,]

plot(afgmap1)
points(afg.grid,pch=".", cex=0.01)
head(afg.grid)

length(Lon)
length(Lat)

FriHeat<-merge(FriHeat0, afg.grid, all=T)
FriHeat[is.na(FriHeat)] <- 0
colnames(FriHeat) <- c( "Lon","Lat","Friendly")
head(FriHeat)


plot3d(FriHeat, size=.30, type="s", lit=FALSE, col="yellowgreen")
#線を引く
segments3d(interleave(FriHeat[,1], FriHeat[,1]), 
   interleave(FriHeat[,2], FriHeat[,2]),
   interleave(FriHeat[,3], min(FriHeat[,3])),
   alpha=0.5, col="blue")

#writeWebGL(width=1000, height=1000)







#=======Enemy=======

EneHeat <- data.frame(afg$Longitude,afg$Latitude,Enemy)
colnames(EneHeat) <- c( "Lon","Lat","Enemy")
head(EneHeat)

summary(afg$Latitude)
summary(afg$Longitude)

EneHeat<-round(EneHeat,1)
EneHeat$Enemy[is.na(EneHeat$Enemy)]<-0
head(EneHeat)

EneHeat0<-na.omit(ddply(EneHeat, .(Lon,Lat), numcolwise(sum)))
head(EneHeat0)

head(bo.afg)

Lon<-round(bo.afg[,1],1)
Lat<-round(bo.afg[,2],1)
head(Lon)

Lon<-as.numeric(unique(Lon))
Lat<-as.numeric(unique(Lat))
head(Lon)

afg.grid <- expand.grid(Lon=seq( min(Lon), max(Lon), by=0.1), 
      Lat=seq( min(Lat), max(Lat), by=0.1 ))

head(afg.grid)
#アフガン外削除
bo.afg<-(afgmap0@polygons[[1]]@Polygons[[1]]@coords) #アフガン国境線
InOrOut.grid<-point.in.polygon(afg.grid$Lon,afg.grid$Lat, bo.afg[,1], bo.afg[,2])
In.grid<-which(InOrOut.grid==1)
afg.grid<-afg.grid[In.grid,]

plot(afgmap1)
points(afg.grid,pch=".", cex=0.01)
head(afg.grid)

length(Lon)
length(Lat)

EneHeat<-merge(EneHeat0, afg.grid, all=T)
EneHeat[is.na(EneHeat)] <- 0
colnames(EneHeat) <- c( "Lon","Lat","Enemy")
head(EneHeat)


plot3d(EneHeat, size=.30, type="s", lit=FALSE, col="yellowgreen")
#線を引く
segments3d(interleave(EneHeat[,1], EneHeat[,1]), 
   interleave(EneHeat[,2], EneHeat[,2]),
   interleave(EneHeat[,3], min(EneHeat[,3])),
   alpha=0.5, col="red")

#writeWebGL(width=1000, height=1000)



#############################

#======イベントヒートマップ=========
#======カーネル密度推定=========


fri<-which(afg$AttackBy=="FRIEND")
ene<-which(afg$AttackBy=="ENEMY")

afgmap11<-fortify(afgmap1)
head(afgmap11)
af11 <- ggplot(afgmap11, aes(x=long, y=lat, group=group)) + geom_path()

#===アフガニスタンの米軍基地===
kandahar<-data.frame(Lon=65.848, Lat=31.506)
shindand<-data.frame(Lon=62.261, Lat=33.391)
bagram<-data.frame(Lon=69.265, Lat=34.946)
leatherneck<-data.frame(Lon=64, Lat=31)
delaram<-data.frame(Lon=63.431, Lat=32.164)


#====AttackByFriendly====
ABFriendly<-afg[fri,]
ABFriendly<-cbind(ABFriendly$Longitude, ABFriendly$Latitude)
colnames(ABFriendly) <- c("Lon","Lat")
ABFriendly<-as.data.frame(ABFriendly)
head(ABFriendly)

mycol<-terrain.colors(40)
mycol[1]<-"#FFFFFFFF"
band_width<-c(bandwidth.nrd(ABFriendly$Lon),bandwidth.nrd(ABFriendly$Lat))
d1<-kde2d(ABFriendly$Lon, ABFriendly$Lat, band_width, n=200)
image(d1,col=mycol,xlim=c(60,75),ylim=c(26,41), xlab="Longitude", ylab="Latitude")
par(new=T)
points(afgmap11$long, afgmap11$lat,pch=".")
points(kandahar, pch=13, cex=2)
points(shindand, pch=13, cex=2)
points(bagram, pch=13, cex=2)
points(leatherneck, pch=13, cex=2)
points(delaram, pch=13, cex=2)

#dev.copy2eps(file="ABFmap.eps")




#====AttackByEnemy====
ABEnemy<-afg[ene,]
ABEnemy<-cbind(ABEnemy$Longitude, ABEnemy$Latitude)
colnames(ABEnemy) <- c("Lon","Lat")
ABEnemy<-as.data.frame(ABEnemy)
head(ABEnemy)

mycol<-terrain.colors(40)
mycol[1]<-"#FFFFFFFF"
band_width<-c(bandwidth.nrd(ABEnemy$Lon),bandwidth.nrd(ABEnemy$Lat))
d2<-kde2d(ABEnemy$Lon, ABEnemy$Lat, band_width, n=200)
image(d2,col=mycol,xlim=c(60,75),ylim=c(26,41), xlab="Longitude", ylab="Latitude")
par(new=T)
points(afgmap11$long, afgmap11$lat,pch=".")
points(kandahar, pch=13, cex=2)
points(shindand, pch=13, cex=2)
points(bagram, pch=13, cex=2)
points(leatherneck, pch=13, cex=2)
points(delaram, pch=13, cex=2)

#dev.copy2eps(file="ABEmap.eps")








#=========================
#===GoogleMap取得
bo.afg<-(afgmap0@polygons[[1]]@Polygons[[1]]@coords) #アフガン国境線
afgmapGoo = GetMap(c(34,67.5), destfile="afgmapGoo.png", zoom=6, sensor="false", hl="ja")

# 色に透明度を付け、そのまま不透明化
cr1 = "red"		# 基本となる不透明色ベクトル（任意）
alp = 0.4			# 透明度（任意）
cr2 = adjustcolor(cr1, alp)	# 半透明色ベクトル
# 半透明色の見た目そのままで、不透明化
cr3 = rgb(t(col2rgb(cr2) / 255 * alp
	+ matrix(rep((1 - alp), 3 * length(cr2)), nrow=3)))

tmp = PlotOnStaticMap(afgmapGoo, lat=afg$Latitude,
                      lon=afg$Longitude, pch=".", col=cr3,cex=0.0000000001)
#dev.copy2eps(file="afgmapGoo.eps")


#====================
#AttackByごとの色付けマッピング

unique(afg$AttackBy)
fri<-which(afg$AttackBy=="FRIEND")
neu<-which(afg$AttackBy=="NEUTRAL")
ene<-which(afg$AttackBy=="ENEMY")
unk<-which(afg$AttackBy=="UNKNOWN")
AttackBy<-afg$AttackBy
AttackByCol<-replace(AttackBy, fri, "blue")
AttackByCol<-replace(AttackByCol, neu, "green")
AttackByCol<-replace(AttackByCol, ene, "red")
AttackByCol<-replace(AttackByCol, unk, "yellow")


#all
plot(afgmap1)
points(afg$Longitude,afg$Latitude,pch=".", col=AttackByCol, cex=2)
#fri
plot(afgmap1)
points(afg$Longitude[fri],afg$Latitude[fri],pch=".", col=AttackByCol[fri], cex=2)
#ene
plot(afgmap1)
points(afg$Longitude[ene],afg$Latitude[ene],pch=".", col=AttackByCol[ene], cex=2)



#====================
#Regionごとの色付けマッピング

unique(afg$Region)
east<-which(afg$Region=="RC EAST")
south<-which(afg$Region=="RC SOUTH")
capital<-which(afg$Region=="RC CAPITAL")
west<-which(afg$Region=="RC WEST")
north<-which(afg$Region=="RC NORTH")
unknown<-which(afg$Region=="UNKNOWN")
nonsel<-which(afg$Region=="NONE SELECTED")
na<-which(afg$Region=="")
Region<-afg$Region
RegionCol<-replace(Region, capital, 1)
RegionCol<-replace(RegionCol, east, 2)
RegionCol<-replace(RegionCol, south, 3)
RegionCol<-replace(RegionCol, west, 4)
RegionCol<-replace(RegionCol, north, 5)
RegionCol<-replace(RegionCol, unknown, 6)
RegionCol<-replace(RegionCol, nonsel, 7)
RegionCol<-replace(RegionCol, na, 8)



plot(afgmap1)
points(afg$Longitude,afg$Latitude,pch=".", col=RegionCol, cex=2)
#dev.copy2eps(file="RegionMap2.eps")


plot(afgmap1)
points(afg$Longitude[capital],afg$Latitude[capital],pch=".", col=RegionCol[capital], cex=2)


#======================

#Typeごとに色付けマップ

uType<-unique(afg$Type)
FA<-which(afg$Type==uType[1])	
NCE<-which(afg$Type==uType[2])	
EA<-which(afg$Type==uType[3])	
SI<-which(afg$Type==uType[4])	
EH<-which(afg$Type==uType[5])	
AM<-which(afg$Type==uType[6])	
oth<-which(afg$Type==uType[7])	
DO<-which(afg$Type==uType[8])	
FF<-which(afg$Type==uType[9])	
CI1<-which(afg$Type==uType[10])
CI2<-which(afg$Type==uType[12])
CE<-which(afg$Type==uType[11])		
UIA<-which(afg$Type==uType[13])
na<-which(afg$Type==uType[14])
ene<-which(afg$Type==uType[15])

	
Type<-afg$Type
TypeCol<-replace(Type, FA, "blue")
TypeCol<-replace(TypeCol, NCE, "green")
TypeCol<-replace(TypeCol, EA, "red")
TypeCol<-replace(TypeCol, SI, "violet")
TypeCol<-replace(TypeCol, EH, "orange")
TypeCol<-replace(TypeCol, AM, "cyan")
TypeCol<-replace(TypeCol, oth, "black")
TypeCol<-replace(TypeCol, DO, "brown")
TypeCol<-replace(TypeCol, FF, "navy")
TypeCol<-replace(TypeCol, CI1, "yellow")
TypeCol<-replace(TypeCol, CI2, "yellow")
TypeCol<-replace(TypeCol, CE, "slateblue")
TypeCol<-replace(TypeCol, UIA, "white")
TypeCol<-replace(TypeCol, na, "white")
TypeCol<-replace(TypeCol, ene, "red")



plot(afgmap1)
points(afg$Longitude,afg$Latitude,pch=".", col=TypeCol, cex=2)


plot(afgmap1)
points(afg$Longitude[EH],afg$Latitude[EH],pch=".", col=TypeCol[EH], cex=2)

plot(afgmap1)
points(afg$Longitude[c(EA,ene)],afg$Latitude[c(EA,ene)],pch=".", col=TypeCol[c(EA,ene)], cex=2)

plot(afgmap1)
points(afg$Longitude[c(FF,FA)],afg$Latitude[c(FF,FA)],pch=".", col=TypeCol[c(FF,FA)], cex=2)



plot(afgmap1)
points(afg$Longitude[AM],afg$Latitude[AM],pch=".", col=TypeCol[AM], cex=2)


#===================
unique(afg$Category)

Assassination<-which(afg$Category=="Assassination")
afg$AttackBy[Assassination]
plot(afgmap1)
points(afg$Longitude[Assassination],afg$Latitude[Assassination],pch=".", cex=2)

DF<-which(afg$Category=="Direct Fire")
afg$AttackBy[DF]
plot(afgmap1)
points(afg$Longitude[DF],afg$Latitude[DF],pch=".", cex=2)

Accident<-which(afg$Category=="Accident")
afg$AttackBy[Accident]
afg$CivilianWIA[Accident]
which(afg$CivilianWIA >50)
plot(afgmap1)
points(afg$Longitude[Accident],afg$Latitude[Accident],pch=".", cex=2)



IED<-which(afg$Category=="IED Explosion")
afg$AttackBy[IED]
plot(afgmap1)
points(afg$Longitude[IED],afg$Latitude[IED],pch=".", cex=2)




XX<-which(afg$Category=="IED Explosion")
afg$AttackBy[XX]
plot(afgmap1)
points(afg$Longitude[XX],afg$Latitude[XX],pch=".", cex=2)





#=====================

#x軸に日付、イベントの種類で色分けしたヒストグラム
#地域ごとに

#Regionをfactorに
afg$Region<-as.factor(afg$Region)


eventRegion <- ggplot( afg, aes(x = DateOccurred) ) + geom_histogram( aes( y = ..count.., fill = AttackBy ), binwidth=30 ) + facet_wrap( ~Region ) + 
    xlab( "Date") + ylab( "Report Counts" ) + ggtitle("Monthly Event Report per Region and Target")+
    scale_fill_manual( values = c(  "darkred", "darkblue", "darkgreen", "orange" ), name = "AttackBy" ) + scale_x_date( minor_breaks = "months")
eventRegion
#dev.copy2eps(file="eventRegion.eps")



eventAllRe <-  ggplot( afg, aes(x = DateOccurred) ) + geom_histogram( aes( y = ..count.., fill = AttackBy ), binwidth=30 ) + 
    xlab( "Date") + ylab( "Report Counts" ) + ggtitle("Monthly Event Report per Target")+
    scale_fill_manual( values = c(  "darkred", "darkblue", "darkgreen", "orange" ), name = "AttackBy" ) + scale_x_date( minor_breaks = "months")
eventAllRe
#dev.copy2eps(file="eventAllRe.eps")

head(afg)



#========================
#======時系列分析======
afgTime <- afg[,c(7,8,9,10,11,12,13,14,15)]
afgTime <- as.xts(afgTime, order.by=afg$DateOccurred)
#NAを除去
afgTime <- na.omit(afgTime)
head(afgTime)

mFriKIA <- apply.monthly(afgTime$FriendlyKIA, sum)
mFriWIA <- apply.monthly(afgTime$FriendlyWIA, sum)



mFriKW<-(mFriKIA+mFriWIA)
colnames(mFriKW)<-"mFriKW"
plot(mFriKW)
#dev.copy2eps(file="mFriKW.eps")

#===季節変動分離===
#===12周期
#===月ごとの季節変動があると仮定した
mFriKWts <- ts( as.numeric( mFriKW$mFriKW ), frequency=12)
mFriKWstl<-stl(mFriKWts,s.window="per")
plot(mFriKWstl)

#dev.copy2eps(file="mFriKWstl.eps")




#=====ENEMY=======
mEneKIA <- apply.monthly(afgTime$EnemyKIA, sum)
mEneWIA <- apply.monthly(afgTime$EnemyWIA, sum)
mEneDet <- apply.monthly(afgTime$EnemyDetained, sum)



mEneKWD<-(mEneKIA+mEneWIA+mEneDet)
colnames(mEneKWD)<-"mEneKWD"
plot(mEneKWD)
#dev.copy2eps(file="mEneKW.eps")

#===enemy季節変動分離===
#===enemy12周期
#===enemy月ごとの季節変動があると仮定した
mEneKWDts <- ts( as.numeric( mEneKWD$mEneKWD ), frequency=12)
mEneKWDstl<-stl(mEneKWDts,s.window="per")
plot(mEneKWDstl)

#dev.copy2eps(file="mEneKWstl.eps")

#==重ねる
dev.off()
plot.zoo(mFriKW, ylim=c(0,1200))
par(new=T)
plot.zoo(mEneKWD, ylim=c(0,1200), col=2, ylab="")
abline(v=as.Date("2006-09-30"))
abline(v=as.Date("2007-09-30"))
abline(v=as.Date("2009-08-31"))




#敵めっちゃ死んでる月がある
hotmonth1 <- grep("2006-09", afg$DateOccurred)
hotmonth2 <- grep("2007-09", afg$DateOccurred)
hotmonth3 <- grep("2009-08", afg$DateOccurred) #アフガン大統領選挙運動カルザイ再選


hotmonth1 <- afg[hotmonth1,]
hotmonth1[which(hotmonth1$EnemyKIA > 90),]
hotmonth1[which(hotmonth1$EnemyKIA > 90),]$Summary

hotmonth2 <- afg[hotmonth2,]
hotmonth2[which(hotmonth2$EnemyKIA > 20),]
hotmonth2[which(hotmonth2$EnemyKIA),]$Summary


hotmonth3 <- afg[hotmonth3,]
hotmonth3[which(hotmonth3$EnemyKIA > 0),]
hotmonth3[which(hotmonth3$EnemyKIA > 0),]$Summary



#=====気温======
#パーラチナール -パキスタン  
#緯度: 33.87 °N / 経度: 70.08°E   高度: 1725(m)
#NAには月平均平年気温を入れた
climat <- read.csv("climat.csv",header=T)
mFriKWcli <- cbind(mFriKW,climat[,3])
colnames(mFriKWcli) <- c("Friendly", "climate")
as.matrix(mFriKWcli)





