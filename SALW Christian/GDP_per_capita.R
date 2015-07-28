GDPc <- read.csv("SALW CHristian/Data/GDPc.csv")

GDPc<-GDPc[-(3:170),]


G<-as.matrix(GDPc)


G[1,2]<-12
  G[1,3]<-19
  G[1,4]<-54
  G[1,5]<-66
  G[1,6]<-67
  G[1,7]<-72
  G[1,8]<-90
  G[1,9]<-134
  G[1,10]<-140
  G[1,11]<-182
  G[1,12]<-183
  G[1,13]<-201
  G<-G[,-14]
  G[1,14]<-88
G[1,15]<-74
  G[1,16]<-151
  G[1,17]<-177
  G<-G[,-18]
G<-G[,-18]
  G[1,18]<-11
  G[1,19]<-135
  G[1,20]<-34
  G[1,21]<-202
  G<-G[,-22]
  G[1,22]<-3
  G[1,23]<-29
  G[1,24]<-52
  G[1,25]<-82
  G[1,26]<-150
G[1,27]<-153
  G[1,28]<-221
G<-G[,-29]
G[1,29]<-25
  G[1,30]<-47
  G[1,31]<-112
  G[1,32]<-169
  G[1,33]<-127
  G[1,34]<-164
  G[1,35]<-100
G<-G[,-36]
G[1,36]<-51
  G[1,37]<-168
  G<-G[,-38]
G[1,38]<-9
  G[1,39]<-13
  G[1,40]<-18
  G[1,41]<-63
  G[1,42]<-70
  G[1,43]<-95
  G[1,44]<-102
  G[1,45]<-104
  G[1,46]<-110
  G[1,47]<-124
  G[1,48]<-154
  G[1,49]<-186
  G[1,50]<-196
  G[1,51]<-199
  G[1,52]<-210
  G[1,53]<-176
G[1,54]<-8
  G[1,55]<-27
  G[1,56]<-38
  G[1,57]<-40
  G[1,58]<-122
  G[1,59]<-148
  G[1,60]<-209
  G[1,61]<-213
G<-G[,-62]
  G[1,62]<-24
  G[1,63]<-45
  G[1,64]<-48
  G[1,65]<-57
  G[1,66]<-58
  G[1,67]<-60
  G[1,68]<-76
  G[1,69]<-80
  G[1,70]<-81
  G[1,71]<-91
  G[1,72]<-136
  G[1,73]<-145
  G[1,74]<-147
G<-G[,-75]
  G[1,75]<-193
G<-G[,-(76:78)]
  G[1,76]<-39
  G[1,77]<-84
  G[1,78]<-85
  G[1,79]<-92
  G[1,80]<-149
G[1,81]<-99
  G[1,82]<-188
  G[1,83]<-185
  G[1,84]<-16
G<-G[,-(85:86)]
  G[1,85]<-115
  G[1,86]<-133
  G[1,87]<-142
  G[1,88]<-167
  G[1,89]<-178
G<-G[,-90]
  G[1,90]<-2
  G[1,91]<-32
G[1,92]<-103
  G[1,93]<-126
  G[1,94]<-98
  G[1,95]<-214
G<-G[,-(96:97)]
  G[1,96]<-15
  G[1,97]<-86
  G[1,98]<-87
  G[1,99]<-89
  G[1,100]<-93
  G[1,101]<-101
  G[1,102]<-105
  G[1,103]<-141
G[1,104]<-152
  G[1,105]<-162
  G[1,106]<-184
  G[1,107]<-195
  G[1,108]<-200
  G[1,109]<-218
G<-G[,-(110:112)]
  G[1,110]<-4
  G[1,111]<-6
  G[1,112]<-21
  G[1,113]<-26
  G[1,114]<-30
  G[1,115]<-31
  G[1,116]<-33
  G[1,117]<-35
  G[1,118]<-36
G[1,119]<-37
  G[1,120]<-41
  G[1,121]<-43
  G[1,122]<-46
  G[1,123]<-55
  G[1,124]<-59
  G[1,125]<-61
  G[1,126]<-62
  G[1,127]<-68
  G[1,128]<-69
  G[1,129]<-73
  G[1,130]<-77
  G[1,131]<-78
  G[1,132]<-96
  G[1,133]<-106
G[1,134]<-107
  G[1,135]<-108
  G[1,136]<-113
  G[1,137]<-114
  G[1,138]<-117
  G[1,139]<-120
  G[1,140]<-121
  G[1,141]<-128
  G[1,142]<-129
  G[1,143]<-131
  G[1,144]<-137
  G[1,145]<-138
  G[1,146]<-155
  G[1,147]<-161
  G[1,148]<-163
G[1,149]<-165
  G[1,150]<-166
  G[1,151]<-171
  G[1,152]<-173
  G[1,153]<-179
  G[1,154]<-181
  G[1,155]<-187
  G[1,156]<-190
  G[1,157]<-194
  G[1,158]<-198
  G[1,159]<-42
  G[1,160]<-222
  G[1,161]<-224
G<-G[,-(162:163)]
G<-G[,-(162:163)]
G[1,1]<-777
G<-G[-2,]

G1<-as.numeric(G)
G<-matrix(G1, 62,161)
G[is.na(G)]<-0
  
GDP<-matrix(0,224,64)
rownames(GDP)<-laenderliste$V1[1:224]
colnames(GDP)<-1950:2013

for(i in 2:161){
  d<-G[1,i]
GDP[d,1:61]<-G[2:62,i]
}
GDP[,62]<-GDP[,61]
GDP[,63]<-GDP[,61]
GDP[,64]<-GDP[,61]

rm(GDPc, G1, d,i,G)

# inlcude missing data from first
GDP2 <- read.csv("SALW Christian/Data/GDP2.csv")

# Afghansitan
GDP2<-GDP2[,-(2:4)]
for (i in 60:64){
GDP[2,i]<-GDP2[3,i-9]
}
# Andorra
for (i in 21:59 ){
  GDP[5,i]<-GDP2[2,i-9]
}
# Antigua and Barbuda
for (i in 28:64 ){
  GDP[7,i]<-GDP2[11,i-9]
}
# Bahamas
for (i in 11:64 ){
  GDP[14,i]<-GDP2[22,i-9]
}
# Barbados
for (i in 11:63 ){
  GDP[17,i]<-GDP2[29,i-9]
}
# Belize
for (i in 11:64 ){
  GDP[20,i]<-GDP2[25,i-9]
}
# Bhutan
for (i in 31:64 ){
  GDP[22,i]<-GDP2[31,i-9]
}
# Brunei
for (i in 16:64 ){
  GDP[28,i]<-GDP2[30,i-9]
}
# Cyprus
for (i in 26:64 ){
  GDP[49,i]<-GDP2[51,i-9]
}
# Dominica
for (i in 28:64 ){
  GDP[56,i]<-GDP2[55,i-9]
}
# Ethiopia
for (i in 32:64 ){
  GDP[64,i]<-GDP2[69,i-9]
}
# Fiji
for (i in 11:64 ){
  GDP[65,i]<-GDP2[73,i-9]
}
# Grenada
for (i in 28:64 ){
  GDP[75,i]<-GDP2[86,i-9]
}
# Guyana
for (i in 11:64 ){
  GDP[79,i]<-GDP2[90,i-9]
}
# Iceland
for (i in 11:64 ){
  GDP[83,i]<-GDP2[105,i-9]
}
# Kiribati
for (i in 21:64 ){
  GDP[97,i]<-GDP2[115,i-9]
}
# Liechtenstein
for (i in 21:60 ){
  GDP[109,i]<-GDP2[129,i-9]
}
# Luxembourg
for (i in 11:64 ){
  GDP[111,i]<-GDP2[135,i-9]
}
# Maldives
for (i in 31:64 ){
  GDP[116,i]<-GDP2[143,i-9]
}
# Malta
for (i in 21:64 ){
  GDP[118,i]<-GDP2[150,i-9]
}
# Marshall Islands
for (i in 32:64 ){
  GDP[119,i]<-GDP2[146,i-9]
}
# Monaco
for (i in 21:62 ){
  GDP[125,i]<-GDP2[140,i-9]
}
# Palau
for (i in 41:64 ){
  GDP[143,i]<-GDP2[180,i-9]
}
# Papua new Guinea
for (i in 11:64 ){
  GDP[146,i]<-GDP2[181,i-9]
}
# Solomon Islands
for (i in 18:20 ){
  GDP[170,i]<-GDP2[198,i-9]
}
# Solomon Islands
for (i in 22:64 ){
  GDP[170,i]<-GDP2[198,i-9]
}
# South Sudan
for (i in 59:64 ){
  GDP[175,i]<-GDP2[205,i-9]
}
# Suriname
for (i in 11:64 ){
  GDP[180,i]<-GDP2[209,i-9]
}
# Timor-Leste
for (i in 51:63 ){
  GDP[189,i]<-GDP2[223,i-9]
}
# Tonga
for (i in 26:64 ){
  GDP[191,i]<-GDP2[224,i-9]
}
# Tuvalu
for (i in 41:64 ){
  GDP[197,i]<-GDP2[228,i-9]
}
# Vanuatu
for (i in 30:64 ){
  GDP[211,i]<-GDP2[240,i-9]
}
# St Vincent and the Grenadines
for (i in 11:64 ){
  GDP[158,i]<-GDP2[236,i-9]
}
# Samoa
for (i in 33:64 ){
  GDP[159,i]<-GDP2[243,i-9]
}
# St Kitts and Nevis
for (i in 11:64 ){
  GDP[156,i]<-GDP2[116,i-9]
}
# St Lucia
for (i in 30:64 ){
  GDP[157,i]<-GDP2[125,i-9]
}
# Benin
for (i in 60:64 ){
  GDP[21,i]<-GDP2[17,i-9]
}
# Botswana
for (i in 60:64 ){
  GDP[26,i]<-GDP2[32,i-9]
}
# Burundi
for (i in 60:64 ){
  GDP[31,i]<-GDP2[15,i-9]
}
# Cape Verde
for (i in 60:64 ){
  GDP[35,i]<-GDP2[45,i-9]
}
# Central African Rep
for (i in 60:64 ){
  GDP[36,i]<-GDP2[33,i-9]
}
# Chad
for (i in 60:64 ){
  GDP[37,i]<-GDP2[218,i-9]
}
# Comoros
for (i in 60:64 ){
  GDP[41,i]<-GDP2[44,i-9]
}
# Congo
for (i in 60:64 ){
  GDP[43,i]<-GDP2[42,i-9]
}
# Djibouti
for (i in 60:64 ){
  GDP[55,i]<-GDP2[54,i-9]
}
# El Salvador
for (i in 60:64 ){
  GDP[60,i]<-GDP2[200,i-9]
}
# Equatorial Guinea
for (i in 60:64 ){
  GDP[61,i]<-GDP2[84,i-9]
}
# Gabon
for (i in 60:64 ){
  GDP[68,i]<-GDP2[77,i-9]
}
# Gambia
for (i in 60:64 ){
  GDP[69,i]<-GDP2[82,i-9]
}
# Guinea
for (i in 60:64 ){
  GDP[77,i]<-GDP2[81,i-9]
}
# Guniea Bissau
for (i in 60:64 ){
  GDP[78,i]<-GDP2[83,i-9]
}
# Haiti
for (i in 60:64 ){
  GDP[80,i]<-GDP2[96,i-9]
}
# Honduras
for (i in 60:64 ){
  GDP[81,i]<-GDP2[93,i-9]
}
# Kosovo
for (i in 51:64 ){
  GDP[100,i]<-GDP2[118,i-9]
}
# Laos
for (i in 60:64 ){
  GDP[103,i]<-GDP2[121,i-9]
}
# Lebanon
for (i in 60:64 ){
  GDP[105,i]<-GDP2[122,i-9]
}
# Lesotho
for (i in 60:64 ){
  GDP[106,i]<-GDP2[133,i-9]
}
# Liberia
for (i in 60:64 ){
  GDP[107,i]<-GDP2[123,i-9]
}
# Libya
for (i in 60:64 ){
  GDP[108,i]<-GDP2[124,i-9]
}
# Liechtenstein
GDP[109,61]<-GDP[109,60]
GDP[109,62]<-GDP[109,60]
GDP[109,63]<-GDP[109,60]
GDP[109,64]<-GDP[109,60]
# Mauretania
for (i in 60:64 ){
  GDP[120,i]<-GDP2[157,i-9]
}
# Mauritius
for (i in 60:64 ){
  GDP[121,i]<-GDP2[158,i-9]
}
# Micronesia
for (i in 37:64 ){
  GDP[123,i]<-GDP2[76,i-9]
}
# Monaco
GDP[125,63]<-GDP[125,62]
GDP[125,64]<-GDP[125,62]
# Mongolia
for (i in 60:64 ){
  GDP[126,i]<-GDP2[154,i-9]
}
# Libya
for (i in 60:64 ){
  GDP[108,i]<-GDP2[124,i-9]
}
# Namibia
for (i in 60:64 ){
  GDP[131,i]<-GDP2[162,i-9]
}
# Nepal
for (i in 60:64 ){
  GDP[133,i]<-GDP2[170,i-9]
}
# Nicaragua
for (i in 60:64 ){
  GDP[136,i]<-GDP2[166,i-9]
}
# Panama
for (i in 60:64 ){
  GDP[145,i]<-GDP2[177,i-9]
}
# Paraguay
for (i in 60:64 ){
  GDP[147,i]<-GDP2[186,i-9]
}
# Rwanda
for (i in 60:64 ){
  GDP[155,i]<-GDP2[192,i-9]
}
# Sao Tome and Principe
for (i in 60:64 ){
  GDP[161,i]<-GDP2[208,i-9]
}
# Seychelles
for (i in 60:64 ){
  GDP[165,i]<-GDP2[215,i-9]
}
# Sierra Leone
for (i in 60:64 ){
  GDP[166,i]<-GDP2[199,i-9]
}
# Somalia
for (i in 60:64 ){
  GDP[171,i]<-GDP2[202,i-9]
}
# Swaziland
for (i in 60:64 ){
  GDP[181,i]<-GDP2[213,i-9]
}
# Togo
for (i in 60:64 ){
  GDP[190,i]<-GDP2[219,i-9]
}
# Trinidad and Tobago
for (i in 60:64 ){
  GDP[193,i]<-GDP2[225,i-9]
}
GDP<-round(GDP)
