laenderliste <- read.csv("Data/laenderliste.csv", header=FALSE)

## help-function to find the row number of a given nation

natnum<-function(nation){which((laenderliste$V1)==nation)}


## help-function to find the nation for a given COW-number

cownat<- function(number){which((laenderliste$COW)==number)}

cownat2<- function(number){which((laenderliste$COW2)==number)}


# adding COW to laenderliste

laenderliste$COW<-0

laenderliste$COW[2]<-700
laenderliste$COW[3]<-339
  laenderliste$COW[4]<-615
  laenderliste$COW[5]<-232
  laenderliste$COW[6]<-540
  laenderliste$COW[7]<-58
  laenderliste$COW[8]<-160
  laenderliste$COW[9]<-371
  laenderliste$COW[11]<-900
  laenderliste$COW[12]<-305
  laenderliste$COW[13]<-373
  laenderliste$COW[14]<-31
  laenderliste$COW[15]<-692
  laenderliste$COW[16]<-771
  laenderliste$COW[17]<-53
laenderliste$COW[18]<-370
  laenderliste$COW[19]<-211
  laenderliste$COW[20]<-80
  laenderliste$COW[21]<-434
  laenderliste$COW[22]<-760
  laenderliste$COW[24]<-145
  laenderliste$COW[25]<-346
  laenderliste$COW[26]<-571
  laenderliste$COW[27]<-140
  laenderliste$COW[28]<-835
  laenderliste$COW[29]<-355
  laenderliste$COW[30]<-439
  laenderliste$COW[31]<-516
  laenderliste$COW[32]<-811
  laenderliste$COW[33]<-471
  laenderliste$COW[34]<-20
  laenderliste$COW[35]<-402
  laenderliste$COW[36]<-482
  laenderliste$COW[37]<-483
  laenderliste$COW[38]<-155
  laenderliste$COW[39]<-710
  laenderliste$COW[40]<-100
  laenderliste$COW[41]<-581
  laenderliste$COW[42]<-490
  laenderliste$COW[43]<-484
  laenderliste$COW[45]<-94
  laenderliste$COW[46]<-437
  laenderliste$COW[47]<-344
  laenderliste$COW[48]<-40
laenderliste$COW[49]<-352
  laenderliste$COW[51]<-316
  laenderliste$COW[52]<-315
  laenderliste$COW[54]<-390
  laenderliste$COW[55]<-522
  laenderliste$COW[56]<-54
  laenderliste$COW[57]<-42
  laenderliste$COW[58]<-130
  laenderliste$COW[59]<-651
  laenderliste$COW[60]<-92
  laenderliste$COW[61]<-411
  laenderliste$COW[62]<-531
  laenderliste$COW[63]<-366
  laenderliste$COW[64]<-530
  laenderliste$COW[65]<-950
  laenderliste$COW[66]<-375
  laenderliste$COW[67]<-220
  laenderliste$COW[68]<-481
  laenderliste$COW[69]<-420
  laenderliste$COW[70]<-372
  laenderliste$COW[71]<-265
  laenderliste$COW[72]<-255
  laenderliste$COW[73]<-452
  laenderliste$COW[74]<-350
  laenderliste$COW[75]<-55
  laenderliste$COW[76]<-90
  laenderliste$COW[77]<-438
  laenderliste$COW[78]<-404
  laenderliste$COW[79]<-110
  laenderliste$COW[80]<-41
  laenderliste$COW[81]<-91
  laenderliste$COW[82]<-310
  laenderliste$COW[83]<-395
  laenderliste$COW[84]<-750
  laenderliste$COW[85]<-850
  laenderliste$COW[86]<-630
  laenderliste$COW[87]<-645
  laenderliste$COW[88]<-205
  laenderliste$COW[89]<-666
  laenderliste$COW[90]<-325
  laenderliste$COW[91]<-51
  laenderliste$COW[92]<-740
  laenderliste$COW[93]<-663
  laenderliste$COW[95]<-705
  laenderliste$COW[96]<-501
  laenderliste$COW[97]<-946
  laenderliste$COW[98]<-731
  laenderliste$COW[99]<-732
  laenderliste$COW[100]<-347
  laenderliste$COW[101]<-690
  laenderliste$COW[102]<-703
  laenderliste$COW[103]<-812
  laenderliste$COW[104]<-367
  laenderliste$COW[105]<-660
  laenderliste$COW[106]<-570
  laenderliste$COW[107]<-450
  laenderliste$COW[108]<-620
  laenderliste$COW[109]<-223
  laenderliste$COW[110]<-368
  laenderliste$COW[111]<-212
  laenderliste$COW[112]<-343
  laenderliste$COW[113]<-580
  laenderliste$COW[114]<-553
  laenderliste$COW[115]<-820
  laenderliste$COW[116]<-781
  laenderliste$COW[117]<-432
  laenderliste$COW[118]<-338
  laenderliste$COW[119]<-983
  laenderliste$COW[120]<-435
  laenderliste$COW[121]<-590
  laenderliste$COW[122]<-70
  laenderliste$COW[123]<-987
  laenderliste$COW[124]<-359
  laenderliste$COW[125]<-221
  laenderliste$COW[126]<-712
laenderliste$COW[127]<-431
  laenderliste$COW[128]<-600
  laenderliste$COW[129]<-541
  laenderliste$COW[130]<-775
  laenderliste$COW[131]<-565
  laenderliste$COW[132]<-970
  laenderliste$COW[133]<-790
  laenderliste$COW[134]<-210
  laenderliste$COW[135]<-920
  laenderliste$COW[136]<-93
  laenderliste$COW[137]<-436
  laenderliste$COW[138]<-475
  laenderliste$COW[140]<-385
  laenderliste$COW[141]<-698
  laenderliste$COW[142]<-770
  laenderliste$COW[143]<-986
  laenderliste$COW[144]<-0
  laenderliste$COW[145]<-95
  laenderliste$COW[146]<-910
  laenderliste$COW[147]<-150
  laenderliste$COW[148]<-135
  laenderliste$COW[149]<-840
  laenderliste$COW[150]<-290
  laenderliste$COW[151]<-235
  laenderliste$COW[152]<-694
  laenderliste$COW[153]<-360
  laenderliste$COW[154]<-365
  laenderliste$COW[155]<-517
  laenderliste$COW[156]<-60
  laenderliste$COW[157]<-56
  laenderliste$COW[158]<-57
  laenderliste$COW[159]<-990
  laenderliste$COW[160]<-331
  laenderliste$COW[161]<-403
  laenderliste$COW[162]<-670
  laenderliste$COW[163]<-433
  laenderliste$COW[164]<-345
  laenderliste$COW[165]<-591
  laenderliste$COW[166]<-451
  laenderliste$COW[167]<-830
  laenderliste$COW[168]<-317
  laenderliste$COW[169]<-349
  laenderliste$COW[170]<-940
  laenderliste$COW[171]<-520
  laenderliste$COW[173]<-560
  laenderliste$COW[175]<-626
  laenderliste$COW[176]<-365
  laenderliste$COW[177]<-230
  laenderliste$COW[178]<-780
  laenderliste$COW[179]<-625
  laenderliste$COW[180]<-115
  laenderliste$COW[181]<-572
  laenderliste$COW[182]<-380
  laenderliste$COW[183]<-225
  laenderliste$COW[184]<-652
  laenderliste$COW[185]<-713
  laenderliste$COW[186]<-702
  laenderliste$COW[187]<-510
  laenderliste$COW[188]<-800
  laenderliste$COW[189]<-860
  laenderliste$COW[190]<-461
  laenderliste$COW[191]<-955
  laenderliste$COW[193]<-52
  laenderliste$COW[194]<-616
  laenderliste$COW[195]<-640
  laenderliste$COW[196]<-701
  laenderliste$COW[197]<-947
  laenderliste$COW[198]<-500
  laenderliste$COW[199]<-369
  laenderliste$COW[200]<-696
  laenderliste$COW[201]<-200
  laenderliste$COW[202]<-2
  laenderliste$COW[209]<-165
  laenderliste$COW[210]<-704
  laenderliste$COW[211]<-935
  laenderliste$COW[212]<-327
  laenderliste$COW[213]<-101
  laenderliste$COW[214]<-816
  laenderliste$COW[216]<-817
  laenderliste$COW[218]<-679
  laenderliste$COW[219]<-678
  laenderliste$COW[220]<-680
  laenderliste$COW[221]<-345
  laenderliste$COW[222]<-551
  laenderliste$COW[223]<-511
  laenderliste$COW[224]<-552
  View(laenderliste)

# country label in SIPRI Data

laenderliste$SIPRI<-0

laenderliste$SIPRI[1]<-'Abkhazia' 
laenderliste$SIPRI[2]<- 'Afghanistan'
laenderliste$SIPRI[3]<- 'Albania'
laenderliste$SIPRI[4]<- 'Algeria'
laenderliste$SIPRI[5]<- 'Andorra'
laenderliste$SIPRI[6]<- 'Angola'
laenderliste$SIPRI[7]<- 'Antigua and Barbuda'
laenderliste$SIPRI[8]<- 'Argentina'
laenderliste$SIPRI[9]<- 'Armenia'
laenderliste$SIPRI[10]<- 'Aruba'
laenderliste$SIPRI[11]<- 'Australia'
laenderliste$SIPRI[12]<- 'Austria'
laenderliste$SIPRI[13]<- 'Azerbaijan'
laenderliste$SIPRI[14]<- 'Bahamas'
laenderliste$SIPRI[15]<- 'Bahrain'
laenderliste$SIPRI[16]<- 'Bangladesh'
laenderliste$SIPRI[17]<- 'Barbados'
laenderliste$SIPRI[18]<- 'Belarus'
laenderliste$SIPRI[19]<- 'Belgium'
laenderliste$SIPRI[20]<- 'Belize'
laenderliste$SIPRI[21]<- 'Benin'
laenderliste$SIPRI[22]<- 'Bhutan'
laenderliste$SIPRI[23]<- 'Biafra'
laenderliste$SIPRI[24]<- 'Bolivia'
laenderliste$SIPRI[25]<- 'Bosnia and Herzegovina'
laenderliste$SIPRI[26]<- 'Botswana'
laenderliste$SIPRI[27]<- 'Brazil'
laenderliste$SIPRI[28]<- 'Brunei'
laenderliste$SIPRI[29]<- 'Bulgaria'
laenderliste$SIPRI[30]<- 'Burkina Faso'
laenderliste$SIPRI[31]<- 'Burundi'
laenderliste$SIPRI[32]<- 'Cambodia'
laenderliste$SIPRI[33]<- 'Cameroon'
laenderliste$SIPRI[34]<- 'Canada'
laenderliste$SIPRI[35]<- 'Cape Verde'
laenderliste$SIPRI[36]<- 'Central African Republic'
laenderliste$SIPRI[37]<- 'Chad'
laenderliste$SIPRI[38]<- 'Chile'
laenderliste$SIPRI[39]<- 'China'
laenderliste$SIPRI[40]<- 'Colombia'
laenderliste$SIPRI[41]<- 'Comoros'
laenderliste$SIPRI[42]<- 'DR Congo'
laenderliste$SIPRI[43]<- 'Congo'
laenderliste$SIPRI[44]<- 'Cook Islands'
laenderliste$SIPRI[45]<- 'Costa Rica'
laenderliste$SIPRI[46]<- 'Cote dIvoire'
laenderliste$SIPRI[47]<- 'Croatia'
laenderliste$SIPRI[48]<- 'Cuba'
laenderliste$SIPRI[49]<- 'Cyprus'
laenderliste$SIPRI[50]<- 'Northern Cyprus'
laenderliste$SIPRI[51]<- 'Czech Republic'
laenderliste$SIPRI[52]<- 'Czechoslovakia'
laenderliste$SIPRI[53]<- 'Darfur'
laenderliste$SIPRI[54]<- 'Denmark'
laenderliste$SIPRI[55]<- 'Djibouti'
laenderliste$SIPRI[56]<- 'Dominica'
laenderliste$SIPRI[57]<- 'Dominican Republic'
laenderliste$SIPRI[58]<- 'Ecuador'
laenderliste$SIPRI[59]<- 'Egypt'
laenderliste$SIPRI[60]<- 'El Salvador'
laenderliste$SIPRI[61]<- 'Equatorial Guinea'
laenderliste$SIPRI[62]<- 'Eritrea'
laenderliste$SIPRI[63]<- 'Estonia'
laenderliste$SIPRI[64]<- 'Ethiopia'
laenderliste$SIPRI[65]<- 'Fiji'
laenderliste$SIPRI[66]<- 'Finland'
laenderliste$SIPRI[67]<- 'France'
laenderliste$SIPRI[68]<- 'Gabon'
laenderliste$SIPRI[69]<- 'Gambia'
laenderliste$SIPRI[70]<- 'Georgia'
laenderliste$SIPRI[71]<- 'German Democratic Republic'
laenderliste$SIPRI[72]<- 'Germany (FRG)'
laenderliste$SIPRI[73]<- 'Ghana'
laenderliste$SIPRI[74]<- 'Greece'
laenderliste$SIPRI[75]<- 'Grenada'
laenderliste$SIPRI[76]<- 'Guatemala'
laenderliste$SIPRI[77]<- 'Guinea'
laenderliste$SIPRI[78]<- 'Guinea-Bissau'
laenderliste$SIPRI[79]<- 'Guyana'
laenderliste$SIPRI[80]<- 'Haiti'
laenderliste$SIPRI[81]<- 'Honduras'
laenderliste$SIPRI[82]<- 'Hungary'
laenderliste$SIPRI[83]<- 'Iceland'
laenderliste$SIPRI[84]<- 'India'
laenderliste$SIPRI[85]<- 'Indonesia'
laenderliste$SIPRI[86]<- 'Iran'
laenderliste$SIPRI[87]<- 'Iraq'
laenderliste$SIPRI[88]<- 'Ireland'
laenderliste$SIPRI[89]<- 'Israel'
laenderliste$SIPRI[90]<- 'Italy'
laenderliste$SIPRI[91]<- 'Jamaica'
laenderliste$SIPRI[92]<- 'Japan'
laenderliste$SIPRI[93]<- 'Jordan'
laenderliste$SIPRI[94]<- 'Katanga'
laenderliste$SIPRI[95]<- 'Kazakhstan'
laenderliste$SIPRI[96]<- 'Kenya'
laenderliste$SIPRI[97]<- 'Kiribati'
laenderliste$SIPRI[98]<- 'North Korea (DPRK)'
laenderliste$SIPRI[99]<- 'South Korea (ROK)'
laenderliste$SIPRI[100]<- 'Kosovo'
laenderliste$SIPRI[101]<- 'Kuwait'
laenderliste$SIPRI[102]<- 'Kyrgyzstan'
laenderliste$SIPRI[103]<- 'Laos'
laenderliste$SIPRI[104]<- 'Latvia'
laenderliste$SIPRI[105]<- 'Lebanon'
laenderliste$SIPRI[106]<- 'Lesotho'
laenderliste$SIPRI[107]<- 'Liberia'
laenderliste$SIPRI[108]<- 'Libya'
laenderliste$SIPRI[109]<- 'Liechtenstein'
laenderliste$SIPRI[110]<- 'Lithuania'
laenderliste$SIPRI[111]<- 'Luxembourg'
laenderliste$SIPRI[112]<- 'Macedonia (FYROM)'
laenderliste$SIPRI[113]<- 'Madagascar'
laenderliste$SIPRI[114]<- 'Malawi'
laenderliste$SIPRI[115]<- 'Malaysia'
laenderliste$SIPRI[116]<- 'Maldives'
laenderliste$SIPRI[117]<- 'Mali'
laenderliste$SIPRI[118]<- 'Malta'
laenderliste$SIPRI[119]<- 'Marshall Islands'
laenderliste$SIPRI[120]<- 'Mauritania'
laenderliste$SIPRI[121]<- 'Mauritius'
laenderliste$SIPRI[122]<- 'Mexico'
laenderliste$SIPRI[123]<- 'Micronesia'
laenderliste$SIPRI[124]<- 'Moldova'
laenderliste$SIPRI[125]<- 'Monaco'
laenderliste$SIPRI[126]<- 'Mongolia'
laenderliste$SIPRI[127]<- 'Montenegro'
laenderliste$SIPRI[128]<- 'Morocco'
laenderliste$SIPRI[129]<- 'Mozambique'
laenderliste$SIPRI[130]<- 'Myanmar'
laenderliste$SIPRI[131]<- 'Namibia'
laenderliste$SIPRI[132]<- 'Nauru'
laenderliste$SIPRI[133]<- 'Nepal'
laenderliste$SIPRI[134]<- 'Netherlands'
laenderliste$SIPRI[135]<- 'New Zealand'
laenderliste$SIPRI[136]<- 'Nicaragua'
laenderliste$SIPRI[137]<- 'Niger'
laenderliste$SIPRI[138]<- 'Nigeria'
laenderliste$SIPRI[139]<- 'Niue'
laenderliste$SIPRI[140]<- 'Norway'
laenderliste$SIPRI[141]<- 'Oman'
laenderliste$SIPRI[142]<- 'Pakistan'
laenderliste$SIPRI[143]<- 'Palau'
laenderliste$SIPRI[144]<- 'Palestine'
laenderliste$SIPRI[145]<- 'Panama'
laenderliste$SIPRI[146]<- 'Papua New Guinea'
laenderliste$SIPRI[147]<- 'Paraguay'
laenderliste$SIPRI[148]<- 'Peru'
laenderliste$SIPRI[149]<- 'Philippines'
laenderliste$SIPRI[150]<- 'Poland'
laenderliste$SIPRI[151]<- 'Portugal'
laenderliste$SIPRI[152]<- 'Qatar'
laenderliste$SIPRI[153]<- 'Romania'
laenderliste$SIPRI[154]<- 'Russia'
laenderliste$SIPRI[155]<- 'Rwanda'
laenderliste$SIPRI[156]<- 'Saint Kitts and Nevis'
laenderliste$SIPRI[157]<- 'Saint Lucia'
laenderliste$SIPRI[158]<- 'Saint Vincent'
laenderliste$SIPRI[159]<- 'Samoa'
laenderliste$SIPRI[160]<- 'San Marino'
laenderliste$SIPRI[161]<- 'Sao Tome and Principe'
laenderliste$SIPRI[162]<- 'Saudi Arabia'
laenderliste$SIPRI[163]<- 'Senegal'
laenderliste$SIPRI[164]<- 'Serbia'
laenderliste$SIPRI[165]<- 'Seychelles'
laenderliste$SIPRI[166]<- 'Sierra Leone'
laenderliste$SIPRI[167]<- 'Singapore'
laenderliste$SIPRI[168]<- 'Slovakia'
laenderliste$SIPRI[169]<- 'Slovenia'
laenderliste$SIPRI[170]<- 'Solomon Islands'
laenderliste$SIPRI[171]<- 'Somalia'
laenderliste$SIPRI[172]<- 'Somaliland'
laenderliste$SIPRI[173]<- 'South Africa'
laenderliste$SIPRI[174]<- 'South Ossetia'
laenderliste$SIPRI[175]<- 'South Sudan'
laenderliste$SIPRI[176]<- 'Soviet Union'
laenderliste$SIPRI[177]<- 'Spain'
laenderliste$SIPRI[178]<- 'Sri Lanka'
laenderliste$SIPRI[179]<- 'Sudan'
laenderliste$SIPRI[180]<- 'Suriname'
laenderliste$SIPRI[181]<- 'Swaziland'
laenderliste$SIPRI[182]<- 'Sweden'
laenderliste$SIPRI[183]<- 'Switzerland'
laenderliste$SIPRI[184]<- 'Syria'
laenderliste$SIPRI[185]<- 'Taiwan'
laenderliste$SIPRI[186]<- 'Tajikistan'
laenderliste$SIPRI[187]<- 'Tanzania'
laenderliste$SIPRI[188]<- 'Thailand'
laenderliste$SIPRI[189]<- 'Timor-Leste'
laenderliste$SIPRI[190]<- 'Togo'
laenderliste$SIPRI[191]<- 'Tonga'
laenderliste$SIPRI[192]<- 'Trans-Dniester'
laenderliste$SIPRI[193]<- 'Trinidad and Tobago'
laenderliste$SIPRI[194]<- 'Tunisia'
laenderliste$SIPRI[195]<- 'Turkey'
laenderliste$SIPRI[196]<- 'Turkmenistan'
laenderliste$SIPRI[197]<- 'Tuvalu'
laenderliste$SIPRI[198]<- 'Uganda'
laenderliste$SIPRI[199]<- 'Ukraine'
laenderliste$SIPRI[200]<- 'UAE'
laenderliste$SIPRI[201]<- 'United Kingdom'
laenderliste$SIPRI[202]<- 'United States'
laenderliste$SIPRI[203]<- ''
laenderliste$SIPRI[204]<- ''
laenderliste$SIPRI[205]<- ''
laenderliste$SIPRI[206]<- ''
laenderliste$SIPRI[207]<- ''
laenderliste$SIPRI[208]<- ''
laenderliste$SIPRI[209]<- 'Uruguay'
laenderliste$SIPRI[210]<- 'Uzbekistan'
laenderliste$SIPRI[211]<- 'Vanuatu'
laenderliste$SIPRI[212]<- 'Vatican (Holy See)'
laenderliste$SIPRI[213]<- 'Venezuela'
laenderliste$SIPRI[214]<- 'Viet Nam'
laenderliste$SIPRI[215]<- 'North Vietnam'
laenderliste$SIPRI[216]<- 'South Vietnam'
laenderliste$SIPRI[217]<- 'Western Sahara'
laenderliste$SIPRI[218]<- 'Yemen'
laenderliste$SIPRI[219]<- 'North Yemen'
laenderliste$SIPRI[220]<- 'South Yemen'
laenderliste$SIPRI[221]<- 'Yugoslavia'
laenderliste$SIPRI[222]<- 'Zambia'
laenderliste$SIPRI[223]<- 'Zanzibar'
laenderliste$SIPRI[224]<- 'Zimbabwe'
laenderliste$SIPRI[225]<-'African Union**'
laenderliste$SIPRI[226]<-  'Amal (Lebanon)*'
laenderliste$SIPRI[227]<-  'Armas (Guatemala)*'
laenderliste$SIPRI[228]<-  'Contras (Nicaragua)*'
laenderliste$SIPRI[229]<- 'ENLF (Ethiopia)*'
laenderliste$SIPRI[230]<- 'FMLN (El Salvador)*'
laenderliste$SIPRI[231]<-  'GUNT (Chad)*'
laenderliste$SIPRI[232]<- 'Hamas (Palestine)*'
laenderliste$SIPRI[233]<- 'Hezbollah (Lebanon)*'
laenderliste$SIPRI[234]<- 'Khmer Rouge (Cambodia)*'
laenderliste$SIPRI[235]<- 'Lebanon Palestinian rebels*'
laenderliste$SIPRI[236]<- 'LF (Lebanon)*'
laenderliste$SIPRI[237]<- 'LTTE (Sri Lanka)*'
laenderliste$SIPRI[238]<- 'MPLA (Portugal)*'
laenderliste$SIPRI[239]<- 'MTA (Myanmar)*'
laenderliste$SIPRI[240]<- 'Mujahedin (Afghanistan)*'
laenderliste$SIPRI[241]<- 'NATO**'
laenderliste$SIPRI[242]<- 'Northern Alliance (Afghanistan)*'
laenderliste$SIPRI[243]<- 'PAIGC (Portugal)*'
laenderliste$SIPRI[244]<- 'Pathet Lao (Laos)*'
laenderliste$SIPRI[245]<- 'PKK (Turkey)*'
laenderliste$SIPRI[246]<- 'PLO (Israel)*'
laenderliste$SIPRI[247]<- 'Provisional IRA (UK)*'
laenderliste$SIPRI[248]<- 'RUI (Sierra Leone)*'
laenderliste$SIPRI[249]<- 'SLA (Lebanon)*'
laenderliste$SIPRI[250]<- 'Southern rebels (Yemen)*'
laenderliste$SIPRI[251]<- 'SPLA (Sudan)*'
laenderliste$SIPRI[252]<- 'Syria rebels*'
laenderliste$SIPRI[253]<- 'UNITA (Angola)*'
laenderliste$SIPRI[254]<- 'United Nations**'
laenderliste$SIPRI[255]<- 'Viet Cong (South Vietnam)*'
laenderliste$SIPRI[256]<- 'ZAPU (Zimbabwe)*'
laenderliste$SIPRI[257]<- 'Unknown country'

laenderliste$COW2<-laenderliste$COW
laenderliste[72,4]<-260

laenderliste$cepii<-0
laenderliste$cepii[2]<-'AFG'
laenderliste$cepii[3]<-'ALB'
  laenderliste$cepii[4]<-'DZA'
  laenderliste$cepii[5]<-'AND'
  laenderliste$cepii[6]<-'AGO'
  laenderliste$cepii[7]<-'ATG'
  laenderliste$cepii[8]<-'ARG'
  laenderliste$cepii[9]<-'ARM'
  laenderliste$cepii[11]<-'AUS'
  laenderliste$cepii[12]<-'AUT'
  laenderliste$cepii[13]<-'AZE'
  laenderliste$cepii[14]<-'BHS'
  laenderliste$cepii[15]<-'BHR'
  laenderliste$cepii[16]<-'BGD'
  laenderliste$cepii[17]<-'BRB'
  laenderliste$cepii[18]<-'BLR'
  laenderliste$cepii[19]<-'BEL'
  laenderliste$cepii[20]<-'BLZ'
  laenderliste$cepii[21]<-'BEN'
  laenderliste$cepii[22]<-'BTN'
  laenderliste$cepii[24]<-  'BOL'
laenderliste$cepii[25]<- 'BIH'
  laenderliste$cepii[26]<-'BWA'
  laenderliste$cepii[27]<-'BRA'
  laenderliste$cepii[28]<-'BRN'
  laenderliste$cepii[29]<-'BGR'
  laenderliste$cepii[30]<-'BFA'
  laenderliste$cepii[31]<-'BDI'
  laenderliste$cepii[32]<-'KHM'
  laenderliste$cepii[33]<-'CMR'
  laenderliste$cepii[34]<-'CAN'
  laenderliste$cepii[35]<-'CPV'
  laenderliste$cepii[36]<-'CAF'
  laenderliste$cepii[37]<-'TCD'
  laenderliste$cepii[38]<-'CHL'
  laenderliste$cepii[39]<-'CHN'
  laenderliste$cepii[40]<-'COL'
  laenderliste$cepii[41]<-'COM'
  laenderliste$cepii[42]<-'ZAR'
  laenderliste$cepii[43]<-'COG'
  laenderliste$cepii[44]<-'COK'
  laenderliste$cepii[45]<-'CRI'
  laenderliste$cepii[46]<-'CIV'
  laenderliste$cepii[47]<-'HRV'
  laenderliste$cepii[48]<-'CUB'
  laenderliste$cepii[49]<-'CYP'
  laenderliste$cepii[50]<-'CYP'
  laenderliste$cepii[51]<-'CZE'
  laenderliste$cepii[52]<-'CZE'
  laenderliste$cepii[54]<-'DNK'
  laenderliste$cepii[55]<-'DJI'
  laenderliste$cepii[56]<-'DMA'
  laenderliste$cepii[57]<-'DOM'
  laenderliste$cepii[58]<-'ECU'
  laenderliste$cepii[59]<-'EGY'
  laenderliste$cepii[60]<-'SLV'
  laenderliste$cepii[61]<-'GNQ'
  laenderliste$cepii[62]<-'ERI'
  laenderliste$cepii[63]<-'EST'
  laenderliste$cepii[64]<-'ETH'
  laenderliste$cepii[65]<-'FJI'
  laenderliste$cepii[66]<-'FIN'
  laenderliste$cepii[67]<-'FRA'
  laenderliste$cepii[68]<-'GAB'
  laenderliste$cepii[69]<-'GMB'
  laenderliste$cepii[70]<-'GEO'
  laenderliste$cepii[71]<-'DEU'
  laenderliste$cepii[72]<-'DEU'
  laenderliste$cepii[73]<-'GHA'
  laenderliste$cepii[74]<-'GRC'
  laenderliste$cepii[75]<-'GRD'
  laenderliste$cepii[76]<-'GTM'
  laenderliste$cepii[77]<-'GIN'
  laenderliste$cepii[78]<-'GNB'
  laenderliste$cepii[79]<-'GUY'
  laenderliste$cepii[80]<-'HTI'
  laenderliste$cepii[81]<-'HND'
  laenderliste$cepii[82]<-'HUN'
  laenderliste$cepii[83]<-'ISL'
  laenderliste$cepii[84]<-'IND'
  laenderliste$cepii[85]<-'IDN'
  laenderliste$cepii[86]<-'IRN'
  laenderliste$cepii[87]<-'IRQ'
  laenderliste$cepii[88]<-'IRL'
  laenderliste$cepii[89]<-'ISR'
  laenderliste$cepii[90]<-'ITA'
  laenderliste$cepii[91]<-'JAM'
  laenderliste$cepii[92]<-'JPN'
  laenderliste$cepii[93]<-'JOR'
  laenderliste$cepii[95]<-'KAZ'
  laenderliste$cepii[96]<-'KEN'
  laenderliste$cepii[97]<-'KIR'
  laenderliste$cepii[98]<-'PRK'
  laenderliste$cepii[99]<-'KOR'
  laenderliste$cepii[101]<-'KWT'
  laenderliste$cepii[102]<-'KGZ'
  laenderliste$cepii[103]<-'LAO'
  laenderliste$cepii[104]<-'LVA'
  laenderliste$cepii[105]<-'LBN'
  laenderliste$cepii[106]<-'LSO'
  laenderliste$cepii[107]<-'LBR'
  laenderliste$cepii[108]<-'LBY'
  laenderliste$cepii[110]<-'LTU'
  laenderliste$cepii[111]<-'LUX'
  laenderliste$cepii[112]<-'MKD'
  laenderliste$cepii[113]<-'MDG'
  laenderliste$cepii[114]<-'MWI'
  laenderliste$cepii[115]<-'MYS'
  laenderliste$cepii[116]<-'MDV'
  laenderliste$cepii[117]<-'MLI'
  laenderliste$cepii[118]<-'MLT'
  laenderliste$cepii[119]<-'MHL'
  laenderliste$cepii[120]<-'MRT'
  laenderliste$cepii[121]<-'MUS'
  laenderliste$cepii[122]<-'MEX'
  laenderliste$cepii[123]<-'FSM'
  laenderliste$cepii[124]<-'MDA'
  laenderliste$cepii[126]<-'MNG'
  laenderliste$cepii[127]<-'YUG'
  laenderliste$cepii[128]<-'MAR'
  laenderliste$cepii[129]<-'MOZ'
  laenderliste$cepii[131]<-'NAM'
  laenderliste$cepii[132]<-'NRU'
  laenderliste$cepii[133]<-'NPL'
  laenderliste$cepii[134]<-'NLD'
  laenderliste$cepii[135]<-'NZL'
  laenderliste$cepii[136]<-'NIC'
  laenderliste$cepii[137]<-'NER'
  laenderliste$cepii[138]<-'NGA'
  laenderliste$cepii[139]<-'NIU'
  laenderliste$cepii[140]<-'NOR'
  laenderliste$cepii[141]<-'OMN'
  laenderliste$cepii[142]<-'PAK'
  laenderliste$cepii[143]<-'PLW'
  laenderliste$cepii[144]<-'PAL'
  laenderliste$cepii[145]<-'PAN'
  laenderliste$cepii[146]<-'PNG'
  laenderliste$cepii[147]<-'PRY'
  laenderliste$cepii[148]<-'PER'
  laenderliste$cepii[149]<-'PHL'
  laenderliste$cepii[150]<-'POL'
  laenderliste$cepii[151]<-'PRT'
  laenderliste$cepii[152]<-'QAT'
  laenderliste$cepii[153]<-'ROM'
  laenderliste$cepii[154]<-'RUS'
  laenderliste$cepii[155]<-'RWA'
  laenderliste$cepii[156]<-'KNA'
  laenderliste$cepii[157]<-'LCA'
  laenderliste$cepii[158]<-'VCT'
  laenderliste$cepii[159]<-'WSM'
  laenderliste$cepii[160]<-'SMR'
  laenderliste$cepii[161]<-'STP'
  laenderliste$cepii[162]<-'SAU'
  laenderliste$cepii[163]<-'SEN'
  laenderliste$cepii[164]<-'YUG'
  laenderliste$cepii[165]<-'SYC'
  laenderliste$cepii[166]<-'SLE'
  laenderliste$cepii[167]<-'SGP'
  laenderliste$cepii[168]<-'SVK'
  laenderliste$cepii[169]<-'SVN'
  laenderliste$cepii[170]<-'SLB'
  laenderliste$cepii[171]<-'SOM'
  laenderliste$cepii[173]<-'ZAF'
  laenderliste$cepii[176]<-'RUS'
  laenderliste$cepii[177]<-'ESP'
  laenderliste$cepii[178]<-'LKA'
  laenderliste$cepii[179]<-'SDN'
  laenderliste$cepii[180]<-'SUR'
  laenderliste$cepii[181]<-'SWZ'
  laenderliste$cepii[182]<-'SWE'
  laenderliste$cepii[183]<-'CHE'
  laenderliste$cepii[184]<-'SYR'
  laenderliste$cepii[185]<-'TWN'
  laenderliste$cepii[186]<-'TJK'
  laenderliste$cepii[187]<-'TZA'
  laenderliste$cepii[188]<-'THA'
  laenderliste$cepii[189]<-'TMP'
  laenderliste$cepii[190]<-'TGO'
  laenderliste$cepii[191]<-'TON'
  laenderliste$cepii[193]<-'TTO'
  laenderliste$cepii[194]<-'TUN'
  laenderliste$cepii[195]<-'TUR'
  laenderliste$cepii[196]<-'TKM'
  laenderliste$cepii[197]<-'TUV'
  laenderliste$cepii[198]<-'UGA'
  laenderliste$cepii[199]<-'UKR'
  laenderliste$cepii[200]<-'ARE'
  laenderliste$cepii[201]<-'GBR'
  laenderliste$cepii[202]<-'USA'
  laenderliste$cepii[209]<-'URY'
  laenderliste$cepii[210]<-'UZB'
  laenderliste$cepii[211]<-'VUT'
  laenderliste$cepii[213]<-'VEN'
  laenderliste$cepii[214]<-'VNM'
  laenderliste$cepii[215]<-'VNM'
  laenderliste$cepii[216]<-'VNM'
  laenderliste$cepii[217]<-'ESH'
  laenderliste$cepii[218]<-'YEM'
  laenderliste$cepii[219]<-'YEM'
  laenderliste$cepii[220]<-'YEM'
  laenderliste$cepii[221]<-'YUG'
  laenderliste$cepii[222]<-'ZMB'
  laenderliste$cepii[224]<-'ZWE'