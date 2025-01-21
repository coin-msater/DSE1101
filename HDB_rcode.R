rm(list=ls())

library(corrplot)
library(pls)
library(tree)
library(rpart)
library(ROCR)
library(caret)

# Load Data and splitting data into train/test set with ratio 7:3
# HDB = read.csv("Data/HDB_DATA.csv", head = TRUE)
load("HDB.RData")

df = HDB
set.seed(123456)
ntrain = 4200

tr = sample(1: nrow(df), ntrain)
train = df[tr,]
test = df[-tr,]

# Filtering data
time_control = c('year', 'month_Jan', 'month_Feb', 'month_Mar', 'month_Apr', 'month_May', 'month_Jun', 
                 'month_Jul', 'month_Aug', 'month_Sep', 'month_Oct', 'month_Nov', 'month_Dec')

geog_control = c('town_ANG.MO.KIO', 'town_BEDOK', 'town_BISHAN', 'town_BUKIT.BATOK', 'town_BUKIT.MERAH', 
                 'town_BUKIT.PANJANG', 'town_BUKIT.TIMAH', 'town_CENTRAL.AREA', 'town_CHOA.CHU.KANG', 'town_CLEMENTI', 
                 'town_GEYLANG', 'town_HOUGANG', 'town_JURONG.EAST', 'town_JURONG.WEST', 'town_KALLANG.WHAMPOA', 
                 'town_MARINE.PARADE', 'town_PASIR.RIS', 'town_PUNGGOL', 'town_QUEENSTOWN', 'town_SEMBAWANG', 'town_SENGKANG', 
                 'town_SERANGOON', 'town_TAMPINES', 'town_TOA.PAYOH', 'town_WOODLANDS', 'town_YISHUN')

postal_code = c('postal_2digits_05', 'postal_2digits_08', 'postal_2digits_09', 'postal_2digits_10', 'postal_2digits_12', 'postal_2digits_13', 
                'postal_2digits_14', 'postal_2digits_15', 'postal_2digits_16', 'postal_2digits_18', 'postal_2digits_19', 'postal_2digits_20', 
                'postal_2digits_21', 'postal_2digits_26', 'postal_2digits_27', 'postal_2digits_30', 'postal_2digits_31', 'postal_2digits_32', 
                'postal_2digits_33', 'postal_2digits_35', 'postal_2digits_36', 'postal_2digits_37', 'postal_2digits_38', 'postal_2digits_39', 
                'postal_2digits_40', 'postal_2digits_41', 'postal_2digits_42', 'postal_2digits_43', 'postal_2digits_44', 'postal_2digits_46', 
                'postal_2digits_47', 'postal_2digits_50', 'postal_2digits_51', 'postal_2digits_52', 'postal_2digits_53', 'postal_2digits_54', 
                'postal_2digits_55', 'postal_2digits_56', 'postal_2digits_57', 'postal_2digits_59', 'postal_2digits_60', 'postal_2digits_61', 
                'postal_2digits_64', 'postal_2digits_65', 'postal_2digits_67', 'postal_2digits_68', 'postal_2digits_73', 'postal_2digits_75', 
                'postal_2digits_76', 'postal_2digits_79', 'postal_2digits_82')

storey_range = c('storey_range_01.TO.03', 'storey_range_01.TO.05', 'storey_range_04.TO.06', 'storey_range_06.TO.10', 'storey_range_07.TO.09',
              'storey_range_10.TO.12', 'storey_range_11.TO.15', 'storey_range_13.TO.15', 'storey_range_16.TO.18', 'storey_range_16.TO.20',
              'storey_range_19.TO.21', 'storey_range_21.TO.25', 'storey_range_22.TO.24', 'storey_range_25.TO.27', 'storey_range_26.TO.30', 
              'storey_range_28.TO.30', 'storey_range_31.TO.33', 'storey_range_31.TO.35', 'storey_range_34.TO.36', 'storey_range_36.TO.40', 
              'storey_range_37.TO.39', 'storey_range_40.TO.42', 'storey_range_43.TO.45', 'storey_range_46.TO.48', 'storey_range_49.TO.51')

flat_type = c('flat_type_1.ROOM', 'flat_type_2.ROOM', 'flat_type_3.ROOM', 'flat_type_4.ROOM', 'flat_type_5.ROOM', 'flat_type_EXECUTIVE',
              'flat_type_MULTI.GENERATION', 'flat_model_2.room')

flat_model = c('flat_model_adjoined.flat', 'flat_model_apartment', 'flat_model_dbss', 
                    'flat_model_improved', 'flat_model_improved.maisonette', 'flat_model_maisonette', 'flat_model_model.a',
                    'flat_model_model.a.maisonette', 'flat_model_model.a2', 'flat_model_multi.generation', 'flat_model_new.generation', 
                    'flat_model_premium.apartment', 'flat_model_premium.apartment.loft', 'flat_model_premium.maisonette', 'flat_model_simplified', 
                    'flat_model_standard', 'flat_model_terrace', 'flat_model_type.s1', 'flat_model_type.s2')

rentals = c('X1room_sold', 'X2room_sold', 'X3room_sold', 'X4room_sold', 'X5room_sold', 'exec_sold', 'multigen_sold', 'studio_apartment_sold', 
            'X1room_rental', 'X2room_rental', 'X3room_rental', 'other_room_rental')

schools = c('Dist_nearest_primary_school', 'Nearest_primary_school_gender_BOYS..SCHOOL', 'Nearest_primary_school_gender_CO.ED.SCHOOL', 
            'Nearest_primary_school_gender_GIRLS..SCHOOL', 'no_primary_schools_1km', 'no_primary_schools_2km', 'Dist_nearest_GAI_primary_school',
            'Nearest_GAI_primary_school_gender_BOYS..SCHOOL', 'Nearest_GAI_primary_school_gender_CO.ED.SCHOOL', 'Nearest_GAI_primary_school_gender_GIRLS..SCHOOL', 
            'no_GAI_primary_schools_1km', 'no_GAI_primary_schools_2km', 'Dist_nearest_G_primary_school', 'Nearest_G_primary_school_gender_CO.ED.SCHOOL', 
            'Nearest_G_primary_school_gender_GIRLS..SCHOOL', 'no_G_primary_schools_1km', 'no_G_primary_schools_2km', 'Dist_nearest_secondary_school', 
            'Nearest_secondary_school_gender_BOYS..SCHOOL', 'Nearest_secondary_school_gender_CO.ED.SCHOOL', 'Nearest_secondary_school_gender_GIRLS..SCHOOL', 
            'Dist_nearest_GAI_secondary_school', 'Nearest_GAI_secondary_school_gender_BOYS..SCHOOL', 'Nearest_GAI_secondary_school_gender_CO.ED.SCHOOL', 
            'Nearest_GAI_secondary_school_gender_GIRLS..SCHOOL', 'Dist_nearest_G_secondary_school', 'Nearest_G_secondary_school_gender_BOYS..SCHOOL', 
            'Nearest_G_secondary_school_gender_CO.ED.SCHOOL', 'Nearest_G_secondary_school_gender_GIRLS..SCHOOL')

others = c('mature', 'NSL', 'EWL', 'NEL', 'CCL', 'DTL', 'TEL', 'LRT', 'beach_within_2km', 'waterbody_within_2km')

par(mfrow = c(1, 1))

# Correlation between flat type and floor_area_sqm: Boxplot
corr1 = train[,c("resale_price","floor_area_sqm", 'flat_type_1.ROOM', 'flat_type_2.ROOM',
                 'flat_type_3.ROOM', 'flat_type_4.ROOM', 'flat_type_5.ROOM', 
                 'flat_type_EXECUTIVE', 'flat_type_MULTI.GENERATION')]
room1 = subset(corr1, corr1$flat_type_1.ROOM == 1)
room2 = subset(corr1, corr1$flat_type_2.ROOM == 1)
room3 = subset(corr1, corr1$flat_type_3.ROOM == 1 & corr1$floor_area_sqm < 200)
room4 = subset(corr1, corr1$flat_type_4.ROOM == 1)
room5 = subset(corr1, corr1$flat_type_5.ROOM == 1)
roomE = subset(corr1, corr1$flat_type_EXECUTIVE == 1 & corr1$floor_area_sqm < 200)
roomM = subset(corr1, corr1$flat_type_MULTI.GENERATION == 1 & corr1$floor_area_sqm < 200)

label=c("1Room","2Room","3Room", "4Room", "5Room", "Multi-Gen", "Exec") 
boxplot(room1$floor_area_sqm, room2$floor_area_sqm, room3$floor_area_sqm, 
        room4$floor_area_sqm, room5$floor_area_sqm, roomM$floor_area_sqm, 
        roomE$floor_area_sqm, medcol = 'blue', xlab = "Flat Type", ylab = "Floor Area / sqm", names = label)

# Correlation between storey_range and resale_price: Boxplot
corr2 = train[,c("resale_price","floor_area_sqm", 'storey_range_01.TO.03', 'storey_range_07.TO.09', 
                 'storey_range_13.TO.15', 'storey_range_19.TO.21', 'storey_range_28.TO.30', 
                 'storey_range_34.TO.36', 'storey_range_25.TO.27', 'storey_range_43.TO.45')]
storey1 = subset(corr2, corr2$storey_range_01.TO.03 == 1)
storey7 = subset(corr2, corr2$storey_range_07.TO.09 == 1)
storey13 = subset(corr2, corr2$storey_range_13.TO.15 == 1)
storey19 = subset(corr2, corr2$storey_range_19.TO.21 == 1)
storey28 = subset(corr2, corr2$storey_range_28.TO.30 == 1)
storey34 = subset(corr2, corr2$storey_range_34.TO.36 == 1)
storey25 = subset(corr2, corr2$storey_range_25.TO.27 == 1)
storey43 = subset(corr2, corr2$storey_range_43.TO.45 == 1)

label=c("1 - 3","7 - 9","13 - 15", "19 - 21","25 - 27", "28 - 30", "34 - 36", "43 - 45") 
boxplot(I(storey1$resale_price/(storey1$floor_area_sqm * 1000)), I(storey7$resale_price/(storey7$floor_area_sqm * 1000)), 
        I(storey13$resale_price/(storey13$floor_area_sqm * 1000)), I(storey19$resale_price/(storey19$floor_area_sqm * 1000)), 
        I(storey25$resale_price/(storey25$floor_area_sqm * 1000)), I(storey28$resale_price/(storey28$floor_area_sqm * 1000)), 
        I(storey34$resale_price/(storey34$floor_area_sqm * 1000)), I(storey43$resale_price/(storey43$floor_area_sqm * 1000)), medcol="blue", xlab = "Storey Range", ylab = "Resale Price per sqm / $1000", names = label)

# Correlation between distance to MRT and mall/hawker centres: Scatter plot
set_seed(111111)
df1 = train
ntrain = 2500
tr1 = sample(1: nrow(df1), ntrain)
trainSmaller1 = df1[tr1,]

plot(x = trainSmaller1$Dist_nearest_station, y = trainSmaller1$Dist_nearest_mall,
     xlab = "Distance to Nearest MRT Station / km", ylab = "Distance to Nearest Mall / km",
     col = 'blue', pch = 20, xlim = c(0, 2.5), ylim = c(0, 2.5))
abline(a = 0, b = 1, col = "red")
abline(a = -.2, b = 1, col = "red", lty = 2)
abline(a = .2, b = 1, col = "red", lty = 2, )

# Impact of remaining_lease on things: Boxplot
lease1 = subset(train, train$Remaining_lease > 90)
lease2 = subset(train, train$Remaining_lease > 80 & train$Remaining_lease < 90)
lease3 = subset(train, train$Remaining_lease > 70 & train$Remaining_lease < 80)
lease4 = subset(train, train$Remaining_lease > 60 & train$Remaining_lease < 70)
lease5 = subset(train, train$Remaining_lease > 50 & train$Remaining_lease < 60)
lease6 = subset(train, train$Remaining_lease < 50)
label = c("<50", "50 - 60", "60 - 70", "70 - 80", "80 - 90", ">90")
boxplot(I(lease6$resale_price/(lease6$floor_area_sqm * 1000)), I(lease5$resale_price/(lease5$floor_area_sqm * 1000)), I(lease4$resale_price/(lease4$floor_area_sqm * 1000)),
        I(lease3$resale_price/(lease3$floor_area_sqm * 1000)), I(lease2$resale_price/(lease2$floor_area_sqm * 1000)), I(lease1$resale_price/(lease1$floor_area_sqm * 1000)),
        medcol = 'blue', xlab = "Remaining Lease / year", ylab = "Resale Price per sqm / $1000", names = label)

boxplot(I(lease6$resale_price/1000), I(lease5$resale_price/1000), I(lease4$resale_price/1000),
        I(lease3$resale_price/(1000)), I(lease2$resale_price/(1000)), I(lease1$resale_price/(1000)),
        medcol = 'blue', xlab = "Remaining Lease / year", ylab = "Resale Price / $1000", names = label)

# Geographical influences: Scatter plot
# 1. LRT
plot(x = train$Remaining_lease, y = train$resale_price/(train$floor_area_sqm * 1000),
     xlab = "Remaining Lease / year", ylab = "Resale Price per sqm / $1000",
     col = ifelse(train$LRT == '1', "red", "blue"),
     xlim = c(45, 98),
     pch =  20)

# 2. HDB towns
plot(x = train$Remaining_lease, y = train$resale_price/(train$floor_area_sqm * 1000),
     xlab = "Remaining Lease / year", ylab = "Resale Price per sqm / $1000",
     col = ifelse(train$town_MARINE.PARADE == '1', "red", "blue"),
     xlim = c(45, 98),
     pch =  20)

plot(x = train$Remaining_lease, y = train$resale_price/(train$floor_area_sqm * 1000),
     xlab = "Remaining Lease / year", ylab = "Resale Price per sqm / $1000",
     col = ifelse(train$town_PUNGGOL == '1', "red", "blue"),
     xlim = c(45, 98),
     pch =  20)

# 3. mature
plot(x = train$Remaining_lease, y = train$resale_price/(train$floor_area_sqm * 1000),
     xlab = "Remaining Lease / year", ylab = "Resale Price per sqm / $1000",
     col = ifelse(train$mature == '1', "red", "blue"),
     xlim = c(45, 98),
     pch =  20)

#######################################################################################

# Modelling
## MLR

# resale_price against all variables
lm1 = lm(I(resale_price/1000) ~., data = train)
summary(lm1)

pred1 = predict.lm(lm1, newdata = test) #prediction on test data
rmse1 = sqrt(mean((I(test$resale_price/1000)-pred1)^2))

# Creating new df for selected variables
keep1 = c('resale_price', 'Remaining_lease', 'floor_area_sqm', 'max_floor_lvl',
          'storey_range_01.TO.03', 'storey_range_04.TO.06', 'storey_range_07.TO.09',
          'storey_range_10.TO.12', 'storey_range_13.TO.15', 'storey_range_16.TO.18',
          'storey_range_19.TO.21', 'storey_range_22.TO.24', 'storey_range_25.TO.27',
          'storey_range_28.TO.30', 'storey_range_31.TO.33', 'storey_range_34.TO.36',
          'storey_range_37.TO.39', 'storey_range_40.TO.42', 'storey_range_43.TO.45',
          'storey_range_46.TO.48', 'storey_range_49.TO.51',
          'mature', 'Dist_nearest_GHawker', 'nearest_ghawker_no_of_cooked_food_stalls',
          'nearest_ghawker_no_of_mkt_produce_stalls', 'Dist_nearest_mall', 'no_malls_2km',
          'beach_within_2km', 'waterbody_within_2km', 'Dist_nearest_CC', 'Dist_nearest_station',
          'NSL', 'EWL', 'NEL', 'CCL', 'DTL', 'TEL', 'LRT', 'unique_no_mrt_0.5km', 'unique_no_mrt_1km',
          'Dist_nearest_polytechnic', 'Dist_nearest_university', 'Dist_nearest_hospital', 'Dist_CBD',
          'ADF_within_1km', 'total_dwelling_units', 'X1room_sold', 'X2room_sold', 'X3room_sold',
          'X4room_sold', 'X5room_sold', 'exec_sold', 'multigen_sold', 'studio_apartment_sold',
          'X1room_rental', 'X2room_rental', 'X3room_rental', 'other_room_rental')
trainSmall1 = train[,(names(train) %in% keep1)]

# resale_price against 58 selected variables
lm2 = lm((resale_price/1000) ~., data = trainSmall1)
plot(lm2, which = 1, main = 'lm((resale_price/1000) ~ .)')
summary(lm2)

pred2 = predict.lm(lm2, newdata = test) #prediction on test data
rmse2 = sqrt(mean((I(test$resale_price/1000)-pred2)^2))

# log(resale_price) against 58 selected variables
lm3 = lm(log(resale_price/1000) ~., data = trainSmall1)
plot(lm3, which = 1, main = 'lm(log(resale_price/1000) ~ .)')
summary(lm3)

pred3 = predict.lm(lm3, newdata = test) #prediction on test data
pred31 = exp(pred3)
rmse3 = sqrt(mean((I(test$resale_price/1000)-pred31)^2))

## DTree using rpart()
big.tree1 = rpart(resale_price/1000 ~ ., method = "anova", data = train, minsplit = 5, cp = .0005)
plotcp(big.tree1)
bestcp=big.tree1$cptable[which.min(big.tree1$cptable[,"xerror"]),"CP"]
length(unique(big.tree1$where))

tree1 = prune(big.tree1,cp=bestcp)
plot(tree1,uniform=TRUE)
text(tree1,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue')
length(unique(tree1$where))

treefit1=predict(tree1,newdata=test,type="vector") #prediction on test data
rmse4 = sqrt(mean((test$resale_price/1000-treefit1)^2))

## DTree using tree()
big.tree2 = tree(resale_price/1000 ~ ., data = train, mindev = 0.0005)
length(unique(big.tree2$where))

cv.tree = cv.tree(big.tree2, , prune.tree)
bestcp2 = cv.tree$size[max(which(cv.tree$dev == min(cv.tree$dev)))]

tree2 = prune.tree(big.tree2, best = bestcp2)
plot(tree2,type="uniform")
text(tree2,col="black",label=c("yval"),cex=.8)
length(unique(tree2$where))

treefit2=predict(tree2,newdata=test,type="vector") #prediction on test data
rmse5 = sqrt(mean((test$resale_price/1000-treefit2)^2))

## PCR on train
## resale_price against all variables
pcr.fit1=pcr(resale_price/1000 ~ . ,data=train, scale=FALSE, validation="CV")
summary(pcr.fit1)

validationplot(pcr.fit1, val.type="RMSEP", main="CV",legendpos = "topright", ylim = c(40, 170))
abline(v = 164, col = 'blue')

pcr.pred1=predict(pcr.fit1, newdata=test, ncomp=164)
rmse6 = sqrt(mean((test$resale_price/1000-pcr.pred1)^2)) #MSE for PCR

## log(resale_price) against all variables
pcr.fit10=pcr(log(resale_price/1000) ~ . ,data=train, scale=FALSE, validation="CV")
summary(pcr.fit10)

validationplot(pcr.fit10, val.type="RMSEP", main="CV",legendpos = "topright", ylim = c(0, 0.35))
abline(v = 164, col = 'blue')

# PCR with 164 components
pcr.pred10=predict(pcr.fit10, newdata=test, ncomp=164)
rmse7 = sqrt(mean((test$resale_price/1000-exp(pcr.pred10))^2)) #MSE for PCR

# PCR with 25 components (90% dimension reduction)
pcr.pred11=predict(pcr.fit10, newdata=test, ncomp=25)
rmse8 = sqrt(mean((test$resale_price/1000-exp(pcr.pred11))^2)) #MSE for PCR

# PCR with 58 components (comparing with lm3)
pcr.pred12=predict(pcr.fit10, newdata=test, ncomp=58)
rmse9 = sqrt(mean((test$resale_price/1000-exp(pcr.pred12))^2)) #MSE for PCR
