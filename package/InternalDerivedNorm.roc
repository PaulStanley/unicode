## WARNING: This file is automatically generated. Do not edit it manually. ##
module [
    quickCheckNFC,
    quickCheckNFKD,
    quickCheckNFD,
    quickCheckNFKC,
    fullCompositionExclusion,
]

import InternalCP exposing [CP, toU32]

quickCheckNFD : CP -> [Yes, No, Maybe]
quickCheckNFD = \cp ->
    u32 = toU32 cp
    if (u32 >= 192 && u32 <= 197)||(u32 >= 199 && u32 <= 207)||(u32 >= 209 && u32 <= 214)||(u32 >= 217 && u32 <= 221)||(u32 >= 224 && u32 <= 229)||(u32 >= 231 && u32 <= 239)||(u32 >= 241 && u32 <= 246)||(u32 >= 249 && u32 <= 253)||(u32 >= 255 && u32 <= 271)||(u32 >= 274 && u32 <= 293)||(u32 >= 296 && u32 <= 304)||(u32 >= 308 && u32 <= 311)||(u32 >= 313 && u32 <= 318)||(u32 >= 323 && u32 <= 328)||(u32 >= 332 && u32 <= 337)||(u32 >= 340 && u32 <= 357)||(u32 >= 360 && u32 <= 382)||(u32 >= 416 && u32 <= 417)||(u32 >= 431 && u32 <= 432)||(u32 >= 461 && u32 <= 476)||(u32 >= 478 && u32 <= 483)||(u32 >= 486 && u32 <= 496)||(u32 >= 500 && u32 <= 501)||(u32 >= 504 && u32 <= 539)||(u32 >= 542 && u32 <= 543)||(u32 >= 550 && u32 <= 563)||(u32 >= 832 && u32 <= 833)||(u32 >= 835 && u32 <= 836)||(u32 == 884)||(u32 == 894)||(u32 == 901)||(u32 == 902)||(u32 == 903)||(u32 >= 904 && u32 <= 906)||(u32 == 908)||(u32 >= 910 && u32 <= 912)||(u32 >= 938 && u32 <= 944)||(u32 >= 970 && u32 <= 974)||(u32 >= 979 && u32 <= 980)||(u32 >= 1024 && u32 <= 1025)||(u32 == 1027)||(u32 == 1031)||(u32 >= 1036 && u32 <= 1038)||(u32 == 1049)||(u32 == 1081)||(u32 >= 1104 && u32 <= 1105)||(u32 == 1107)||(u32 == 1111)||(u32 >= 1116 && u32 <= 1118)||(u32 >= 1142 && u32 <= 1143)||(u32 >= 1217 && u32 <= 1218)||(u32 >= 1232 && u32 <= 1235)||(u32 >= 1238 && u32 <= 1239)||(u32 >= 1242 && u32 <= 1247)||(u32 >= 1250 && u32 <= 1255)||(u32 >= 1258 && u32 <= 1269)||(u32 >= 1272 && u32 <= 1273)||(u32 >= 1570 && u32 <= 1574)||(u32 == 1728)||(u32 == 1730)||(u32 == 1747)||(u32 == 2345)||(u32 == 2353)||(u32 == 2356)||(u32 >= 2392 && u32 <= 2399)||(u32 >= 2507 && u32 <= 2508)||(u32 >= 2524 && u32 <= 2525)||(u32 == 2527)||(u32 == 2611)||(u32 == 2614)||(u32 >= 2649 && u32 <= 2651)||(u32 == 2654)||(u32 == 2888)||(u32 >= 2891 && u32 <= 2892)||(u32 >= 2908 && u32 <= 2909)||(u32 == 2964)||(u32 >= 3018 && u32 <= 3020)||(u32 == 3144)||(u32 == 3264)||(u32 >= 3271 && u32 <= 3272)||(u32 >= 3274 && u32 <= 3275)||(u32 >= 3402 && u32 <= 3404)||(u32 == 3546)||(u32 >= 3548 && u32 <= 3550)||(u32 == 3907)||(u32 == 3917)||(u32 == 3922)||(u32 == 3927)||(u32 == 3932)||(u32 == 3945)||(u32 == 3955)||(u32 >= 3957 && u32 <= 3958)||(u32 == 3960)||(u32 == 3969)||(u32 == 3987)||(u32 == 3997)||(u32 == 4002)||(u32 == 4007)||(u32 == 4012)||(u32 == 4025)||(u32 == 4134)||(u32 == 6918)||(u32 == 6920)||(u32 == 6922)||(u32 == 6924)||(u32 == 6926)||(u32 == 6930)||(u32 == 6971)||(u32 == 6973)||(u32 >= 6976 && u32 <= 6977)||(u32 == 6979)||(u32 >= 7680 && u32 <= 7833)||(u32 == 7835)||(u32 >= 7840 && u32 <= 7929)||(u32 >= 7936 && u32 <= 7957)||(u32 >= 7960 && u32 <= 7965)||(u32 >= 7968 && u32 <= 8005)||(u32 >= 8008 && u32 <= 8013)||(u32 >= 8016 && u32 <= 8023)||(u32 == 8025)||(u32 == 8027)||(u32 == 8029)||(u32 >= 8031 && u32 <= 8061)||(u32 >= 8064 && u32 <= 8116)||(u32 >= 8118 && u32 <= 8124)||(u32 == 8126)||(u32 == 8129)||(u32 >= 8130 && u32 <= 8132)||(u32 >= 8134 && u32 <= 8140)||(u32 >= 8141 && u32 <= 8143)||(u32 >= 8144 && u32 <= 8147)||(u32 >= 8150 && u32 <= 8155)||(u32 >= 8157 && u32 <= 8159)||(u32 >= 8160 && u32 <= 8172)||(u32 >= 8173 && u32 <= 8175)||(u32 >= 8178 && u32 <= 8180)||(u32 >= 8182 && u32 <= 8188)||(u32 == 8189)||(u32 >= 8192 && u32 <= 8193)||(u32 == 8486)||(u32 >= 8490 && u32 <= 8491)||(u32 >= 8602 && u32 <= 8603)||(u32 == 8622)||(u32 == 8653)||(u32 >= 8654 && u32 <= 8655)||(u32 == 8708)||(u32 == 8713)||(u32 == 8716)||(u32 == 8740)||(u32 == 8742)||(u32 == 8769)||(u32 == 8772)||(u32 == 8775)||(u32 == 8777)||(u32 == 8800)||(u32 == 8802)||(u32 >= 8813 && u32 <= 8817)||(u32 >= 8820 && u32 <= 8821)||(u32 >= 8824 && u32 <= 8825)||(u32 >= 8832 && u32 <= 8833)||(u32 >= 8836 && u32 <= 8837)||(u32 >= 8840 && u32 <= 8841)||(u32 >= 8876 && u32 <= 8879)||(u32 >= 8928 && u32 <= 8931)||(u32 >= 8938 && u32 <= 8941)||(u32 == 9001)||(u32 == 9002)||(u32 == 10972)||(u32 == 12364)||(u32 == 12366)||(u32 == 12368)||(u32 == 12370)||(u32 == 12372)||(u32 == 12374)||(u32 == 12376)||(u32 == 12378)||(u32 == 12380)||(u32 == 12382)||(u32 == 12384)||(u32 == 12386)||(u32 == 12389)||(u32 == 12391)||(u32 == 12393)||(u32 >= 12400 && u32 <= 12401)||(u32 >= 12403 && u32 <= 12404)||(u32 >= 12406 && u32 <= 12407)||(u32 >= 12409 && u32 <= 12410)||(u32 >= 12412 && u32 <= 12413)||(u32 == 12436)||(u32 == 12446)||(u32 == 12460)||(u32 == 12462)||(u32 == 12464)||(u32 == 12466)||(u32 == 12468)||(u32 == 12470)||(u32 == 12472)||(u32 == 12474)||(u32 == 12476)||(u32 == 12478)||(u32 == 12480)||(u32 == 12482)||(u32 == 12485)||(u32 == 12487)||(u32 == 12489)||(u32 >= 12496 && u32 <= 12497)||(u32 >= 12499 && u32 <= 12500)||(u32 >= 12502 && u32 <= 12503)||(u32 >= 12505 && u32 <= 12506)||(u32 >= 12508 && u32 <= 12509)||(u32 == 12532)||(u32 >= 12535 && u32 <= 12538)||(u32 == 12542)||(u32 >= 44032 && u32 <= 55203)||(u32 >= 63744 && u32 <= 64013)||(u32 == 64016)||(u32 == 64018)||(u32 >= 64021 && u32 <= 64030)||(u32 == 64032)||(u32 == 64034)||(u32 >= 64037 && u32 <= 64038)||(u32 >= 64042 && u32 <= 64109)||(u32 >= 64112 && u32 <= 64217)||(u32 == 64285)||(u32 == 64287)||(u32 >= 64298 && u32 <= 64310)||(u32 >= 64312 && u32 <= 64316)||(u32 == 64318)||(u32 >= 64320 && u32 <= 64321)||(u32 >= 64323 && u32 <= 64324)||(u32 >= 64326 && u32 <= 64334)||(u32 == 67017)||(u32 == 67044)||(u32 == 69786)||(u32 == 69788)||(u32 == 69803)||(u32 >= 69934 && u32 <= 69935)||(u32 >= 70475 && u32 <= 70476)||(u32 == 70531)||(u32 == 70533)||(u32 == 70542)||(u32 == 70545)||(u32 == 70597)||(u32 >= 70599 && u32 <= 70600)||(u32 >= 70843 && u32 <= 70844)||(u32 == 70846)||(u32 >= 71098 && u32 <= 71099)||(u32 == 71992)||(u32 >= 90401 && u32 <= 90408)||(u32 >= 93544 && u32 <= 93546)||(u32 >= 119134 && u32 <= 119140)||(u32 >= 119227 && u32 <= 119232)||(u32 >= 194560 && u32 <= 195101) then
        No
    else
        Yes

quickCheckNFC : CP -> [Yes, No, Maybe]
quickCheckNFC = \cp ->
    u32 = toU32 cp
    if (u32 >= 832 && u32 <= 833)||(u32 >= 835 && u32 <= 836)||(u32 == 884)||(u32 == 894)||(u32 == 903)||(u32 >= 2392 && u32 <= 2399)||(u32 >= 2524 && u32 <= 2525)||(u32 == 2527)||(u32 == 2611)||(u32 == 2614)||(u32 >= 2649 && u32 <= 2651)||(u32 == 2654)||(u32 >= 2908 && u32 <= 2909)||(u32 == 3907)||(u32 == 3917)||(u32 == 3922)||(u32 == 3927)||(u32 == 3932)||(u32 == 3945)||(u32 == 3955)||(u32 >= 3957 && u32 <= 3958)||(u32 == 3960)||(u32 == 3969)||(u32 == 3987)||(u32 == 3997)||(u32 == 4002)||(u32 == 4007)||(u32 == 4012)||(u32 == 4025)||(u32 == 8049)||(u32 == 8051)||(u32 == 8053)||(u32 == 8055)||(u32 == 8057)||(u32 == 8059)||(u32 == 8061)||(u32 == 8123)||(u32 == 8126)||(u32 == 8137)||(u32 == 8139)||(u32 == 8147)||(u32 == 8155)||(u32 == 8163)||(u32 == 8171)||(u32 >= 8174 && u32 <= 8175)||(u32 == 8185)||(u32 == 8187)||(u32 == 8189)||(u32 >= 8192 && u32 <= 8193)||(u32 == 8486)||(u32 >= 8490 && u32 <= 8491)||(u32 == 9001)||(u32 == 9002)||(u32 == 10972)||(u32 >= 63744 && u32 <= 64013)||(u32 == 64016)||(u32 == 64018)||(u32 >= 64021 && u32 <= 64030)||(u32 == 64032)||(u32 == 64034)||(u32 >= 64037 && u32 <= 64038)||(u32 >= 64042 && u32 <= 64109)||(u32 >= 64112 && u32 <= 64217)||(u32 == 64285)||(u32 == 64287)||(u32 >= 64298 && u32 <= 64310)||(u32 >= 64312 && u32 <= 64316)||(u32 == 64318)||(u32 >= 64320 && u32 <= 64321)||(u32 >= 64323 && u32 <= 64324)||(u32 >= 64326 && u32 <= 64334)||(u32 >= 119134 && u32 <= 119140)||(u32 >= 119227 && u32 <= 119232)||(u32 >= 194560 && u32 <= 195101) then
        No
    else if (u32 >= 768 && u32 <= 772)||(u32 >= 774 && u32 <= 780)||(u32 == 783)||(u32 == 785)||(u32 >= 787 && u32 <= 788)||(u32 == 795)||(u32 >= 803 && u32 <= 808)||(u32 >= 813 && u32 <= 814)||(u32 >= 816 && u32 <= 817)||(u32 == 824)||(u32 == 834)||(u32 == 837)||(u32 >= 1619 && u32 <= 1621)||(u32 == 2364)||(u32 == 2494)||(u32 == 2519)||(u32 == 2878)||(u32 == 2902)||(u32 == 2903)||(u32 == 3006)||(u32 == 3031)||(u32 == 3158)||(u32 == 3266)||(u32 >= 3285 && u32 <= 3286)||(u32 == 3390)||(u32 == 3415)||(u32 == 3530)||(u32 == 3535)||(u32 == 3551)||(u32 == 4142)||(u32 >= 4449 && u32 <= 4469)||(u32 >= 4520 && u32 <= 4546)||(u32 == 6965)||(u32 >= 12441 && u32 <= 12442)||(u32 == 69818)||(u32 == 69927)||(u32 == 70462)||(u32 == 70487)||(u32 == 70584)||(u32 == 70587)||(u32 == 70594)||(u32 == 70597)||(u32 >= 70599 && u32 <= 70601)||(u32 == 70832)||(u32 == 70842)||(u32 == 70845)||(u32 == 71087)||(u32 == 71984)||(u32 >= 90398 && u32 <= 90409)||(u32 >= 93543 && u32 <= 93544) then
        Maybe
        else
        Yes

quickCheckNFKD : CP -> [Yes, No, Maybe]
quickCheckNFKD = \cp ->
    u32 = toU32 cp
    if (u32 == 160)||(u32 == 168)||(u32 == 170)||(u32 == 175)||(u32 >= 178 && u32 <= 179)||(u32 == 180)||(u32 == 181)||(u32 == 184)||(u32 == 185)||(u32 == 186)||(u32 >= 188 && u32 <= 190)||(u32 >= 192 && u32 <= 197)||(u32 >= 199 && u32 <= 207)||(u32 >= 209 && u32 <= 214)||(u32 >= 217 && u32 <= 221)||(u32 >= 224 && u32 <= 229)||(u32 >= 231 && u32 <= 239)||(u32 >= 241 && u32 <= 246)||(u32 >= 249 && u32 <= 253)||(u32 >= 255 && u32 <= 271)||(u32 >= 274 && u32 <= 293)||(u32 >= 296 && u32 <= 304)||(u32 >= 306 && u32 <= 311)||(u32 >= 313 && u32 <= 320)||(u32 >= 323 && u32 <= 329)||(u32 >= 332 && u32 <= 337)||(u32 >= 340 && u32 <= 357)||(u32 >= 360 && u32 <= 383)||(u32 >= 416 && u32 <= 417)||(u32 >= 431 && u32 <= 432)||(u32 >= 452 && u32 <= 476)||(u32 >= 478 && u32 <= 483)||(u32 >= 486 && u32 <= 501)||(u32 >= 504 && u32 <= 539)||(u32 >= 542 && u32 <= 543)||(u32 >= 550 && u32 <= 563)||(u32 >= 688 && u32 <= 696)||(u32 >= 728 && u32 <= 733)||(u32 >= 736 && u32 <= 740)||(u32 >= 832 && u32 <= 833)||(u32 >= 835 && u32 <= 836)||(u32 == 884)||(u32 == 890)||(u32 == 894)||(u32 >= 900 && u32 <= 901)||(u32 == 902)||(u32 == 903)||(u32 >= 904 && u32 <= 906)||(u32 == 908)||(u32 >= 910 && u32 <= 912)||(u32 >= 938 && u32 <= 944)||(u32 >= 970 && u32 <= 974)||(u32 >= 976 && u32 <= 982)||(u32 >= 1008 && u32 <= 1010)||(u32 >= 1012 && u32 <= 1013)||(u32 == 1017)||(u32 >= 1024 && u32 <= 1025)||(u32 == 1027)||(u32 == 1031)||(u32 >= 1036 && u32 <= 1038)||(u32 == 1049)||(u32 == 1081)||(u32 >= 1104 && u32 <= 1105)||(u32 == 1107)||(u32 == 1111)||(u32 >= 1116 && u32 <= 1118)||(u32 >= 1142 && u32 <= 1143)||(u32 >= 1217 && u32 <= 1218)||(u32 >= 1232 && u32 <= 1235)||(u32 >= 1238 && u32 <= 1239)||(u32 >= 1242 && u32 <= 1247)||(u32 >= 1250 && u32 <= 1255)||(u32 >= 1258 && u32 <= 1269)||(u32 >= 1272 && u32 <= 1273)||(u32 == 1415)||(u32 >= 1570 && u32 <= 1574)||(u32 >= 1653 && u32 <= 1656)||(u32 == 1728)||(u32 == 1730)||(u32 == 1747)||(u32 == 2345)||(u32 == 2353)||(u32 == 2356)||(u32 >= 2392 && u32 <= 2399)||(u32 >= 2507 && u32 <= 2508)||(u32 >= 2524 && u32 <= 2525)||(u32 == 2527)||(u32 == 2611)||(u32 == 2614)||(u32 >= 2649 && u32 <= 2651)||(u32 == 2654)||(u32 == 2888)||(u32 >= 2891 && u32 <= 2892)||(u32 >= 2908 && u32 <= 2909)||(u32 == 2964)||(u32 >= 3018 && u32 <= 3020)||(u32 == 3144)||(u32 == 3264)||(u32 >= 3271 && u32 <= 3272)||(u32 >= 3274 && u32 <= 3275)||(u32 >= 3402 && u32 <= 3404)||(u32 == 3546)||(u32 >= 3548 && u32 <= 3550)||(u32 == 3635)||(u32 == 3763)||(u32 >= 3804 && u32 <= 3805)||(u32 == 3852)||(u32 == 3907)||(u32 == 3917)||(u32 == 3922)||(u32 == 3927)||(u32 == 3932)||(u32 == 3945)||(u32 == 3955)||(u32 >= 3957 && u32 <= 3961)||(u32 == 3969)||(u32 == 3987)||(u32 == 3997)||(u32 == 4002)||(u32 == 4007)||(u32 == 4012)||(u32 == 4025)||(u32 == 4134)||(u32 == 4348)||(u32 == 6918)||(u32 == 6920)||(u32 == 6922)||(u32 == 6924)||(u32 == 6926)||(u32 == 6930)||(u32 == 6971)||(u32 == 6973)||(u32 >= 6976 && u32 <= 6977)||(u32 == 6979)||(u32 >= 7468 && u32 <= 7470)||(u32 >= 7472 && u32 <= 7482)||(u32 >= 7484 && u32 <= 7501)||(u32 >= 7503 && u32 <= 7530)||(u32 == 7544)||(u32 >= 7579 && u32 <= 7615)||(u32 >= 7680 && u32 <= 7835)||(u32 >= 7840 && u32 <= 7929)||(u32 >= 7936 && u32 <= 7957)||(u32 >= 7960 && u32 <= 7965)||(u32 >= 7968 && u32 <= 8005)||(u32 >= 8008 && u32 <= 8013)||(u32 >= 8016 && u32 <= 8023)||(u32 == 8025)||(u32 == 8027)||(u32 == 8029)||(u32 >= 8031 && u32 <= 8061)||(u32 >= 8064 && u32 <= 8116)||(u32 >= 8118 && u32 <= 8124)||(u32 == 8125)||(u32 == 8126)||(u32 >= 8127 && u32 <= 8129)||(u32 >= 8130 && u32 <= 8132)||(u32 >= 8134 && u32 <= 8140)||(u32 >= 8141 && u32 <= 8143)||(u32 >= 8144 && u32 <= 8147)||(u32 >= 8150 && u32 <= 8155)||(u32 >= 8157 && u32 <= 8159)||(u32 >= 8160 && u32 <= 8172)||(u32 >= 8173 && u32 <= 8175)||(u32 >= 8178 && u32 <= 8180)||(u32 >= 8182 && u32 <= 8188)||(u32 >= 8189 && u32 <= 8190)||(u32 >= 8192 && u32 <= 8202)||(u32 == 8209)||(u32 == 8215)||(u32 >= 8228 && u32 <= 8230)||(u32 == 8239)||(u32 >= 8243 && u32 <= 8244)||(u32 >= 8246 && u32 <= 8247)||(u32 == 8252)||(u32 == 8254)||(u32 >= 8263 && u32 <= 8265)||(u32 == 8279)||(u32 == 8287)||(u32 == 8304)||(u32 == 8305)||(u32 >= 8308 && u32 <= 8313)||(u32 >= 8314 && u32 <= 8316)||(u32 == 8317)||(u32 == 8318)||(u32 == 8319)||(u32 >= 8320 && u32 <= 8329)||(u32 >= 8330 && u32 <= 8332)||(u32 == 8333)||(u32 == 8334)||(u32 >= 8336 && u32 <= 8348)||(u32 == 8360)||(u32 >= 8448 && u32 <= 8449)||(u32 == 8450)||(u32 == 8451)||(u32 >= 8453 && u32 <= 8454)||(u32 == 8455)||(u32 == 8457)||(u32 >= 8458 && u32 <= 8467)||(u32 == 8469)||(u32 == 8470)||(u32 >= 8473 && u32 <= 8477)||(u32 >= 8480 && u32 <= 8482)||(u32 == 8484)||(u32 == 8486)||(u32 == 8488)||(u32 >= 8490 && u32 <= 8493)||(u32 >= 8495 && u32 <= 8497)||(u32 >= 8499 && u32 <= 8500)||(u32 >= 8501 && u32 <= 8504)||(u32 == 8505)||(u32 == 8507)||(u32 >= 8508 && u32 <= 8511)||(u32 == 8512)||(u32 >= 8517 && u32 <= 8521)||(u32 >= 8528 && u32 <= 8543)||(u32 >= 8544 && u32 <= 8575)||(u32 == 8585)||(u32 >= 8602 && u32 <= 8603)||(u32 == 8622)||(u32 == 8653)||(u32 >= 8654 && u32 <= 8655)||(u32 == 8708)||(u32 == 8713)||(u32 == 8716)||(u32 == 8740)||(u32 == 8742)||(u32 >= 8748 && u32 <= 8749)||(u32 >= 8751 && u32 <= 8752)||(u32 == 8769)||(u32 == 8772)||(u32 == 8775)||(u32 == 8777)||(u32 == 8800)||(u32 == 8802)||(u32 >= 8813 && u32 <= 8817)||(u32 >= 8820 && u32 <= 8821)||(u32 >= 8824 && u32 <= 8825)||(u32 >= 8832 && u32 <= 8833)||(u32 >= 8836 && u32 <= 8837)||(u32 >= 8840 && u32 <= 8841)||(u32 >= 8876 && u32 <= 8879)||(u32 >= 8928 && u32 <= 8931)||(u32 >= 8938 && u32 <= 8941)||(u32 == 9001)||(u32 == 9002)||(u32 >= 9312 && u32 <= 9371)||(u32 >= 9372 && u32 <= 9449)||(u32 == 9450)||(u32 == 10764)||(u32 >= 10868 && u32 <= 10870)||(u32 == 10972)||(u32 >= 11388 && u32 <= 11389)||(u32 == 11631)||(u32 == 11935)||(u32 == 12019)||(u32 >= 12032 && u32 <= 12245)||(u32 == 12288)||(u32 == 12342)||(u32 >= 12344 && u32 <= 12346)||(u32 == 12364)||(u32 == 12366)||(u32 == 12368)||(u32 == 12370)||(u32 == 12372)||(u32 == 12374)||(u32 == 12376)||(u32 == 12378)||(u32 == 12380)||(u32 == 12382)||(u32 == 12384)||(u32 == 12386)||(u32 == 12389)||(u32 == 12391)||(u32 == 12393)||(u32 >= 12400 && u32 <= 12401)||(u32 >= 12403 && u32 <= 12404)||(u32 >= 12406 && u32 <= 12407)||(u32 >= 12409 && u32 <= 12410)||(u32 >= 12412 && u32 <= 12413)||(u32 == 12436)||(u32 >= 12443 && u32 <= 12444)||(u32 == 12446)||(u32 == 12447)||(u32 == 12460)||(u32 == 12462)||(u32 == 12464)||(u32 == 12466)||(u32 == 12468)||(u32 == 12470)||(u32 == 12472)||(u32 == 12474)||(u32 == 12476)||(u32 == 12478)||(u32 == 12480)||(u32 == 12482)||(u32 == 12485)||(u32 == 12487)||(u32 == 12489)||(u32 >= 12496 && u32 <= 12497)||(u32 >= 12499 && u32 <= 12500)||(u32 >= 12502 && u32 <= 12503)||(u32 >= 12505 && u32 <= 12506)||(u32 >= 12508 && u32 <= 12509)||(u32 == 12532)||(u32 >= 12535 && u32 <= 12538)||(u32 == 12542)||(u32 == 12543)||(u32 >= 12593 && u32 <= 12686)||(u32 >= 12690 && u32 <= 12693)||(u32 >= 12694 && u32 <= 12703)||(u32 >= 12800 && u32 <= 12830)||(u32 >= 12832 && u32 <= 12841)||(u32 >= 12842 && u32 <= 12871)||(u32 == 12880)||(u32 >= 12881 && u32 <= 12895)||(u32 >= 12896 && u32 <= 12926)||(u32 >= 12928 && u32 <= 12937)||(u32 >= 12938 && u32 <= 12976)||(u32 >= 12977 && u32 <= 12991)||(u32 >= 12992 && u32 <= 13311)||(u32 >= 42652 && u32 <= 42653)||(u32 == 42864)||(u32 >= 42994 && u32 <= 42996)||(u32 >= 43000 && u32 <= 43001)||(u32 >= 43868 && u32 <= 43871)||(u32 == 43881)||(u32 >= 44032 && u32 <= 55203)||(u32 >= 63744 && u32 <= 64013)||(u32 == 64016)||(u32 == 64018)||(u32 >= 64021 && u32 <= 64030)||(u32 == 64032)||(u32 == 64034)||(u32 >= 64037 && u32 <= 64038)||(u32 >= 64042 && u32 <= 64109)||(u32 >= 64112 && u32 <= 64217)||(u32 >= 64256 && u32 <= 64262)||(u32 >= 64275 && u32 <= 64279)||(u32 == 64285)||(u32 >= 64287 && u32 <= 64296)||(u32 == 64297)||(u32 >= 64298 && u32 <= 64310)||(u32 >= 64312 && u32 <= 64316)||(u32 == 64318)||(u32 >= 64320 && u32 <= 64321)||(u32 >= 64323 && u32 <= 64324)||(u32 >= 64326 && u32 <= 64433)||(u32 >= 64467 && u32 <= 64829)||(u32 >= 64848 && u32 <= 64911)||(u32 >= 64914 && u32 <= 64967)||(u32 >= 65008 && u32 <= 65019)||(u32 == 65020)||(u32 >= 65040 && u32 <= 65046)||(u32 == 65047)||(u32 == 65048)||(u32 == 65049)||(u32 == 65072)||(u32 >= 65073 && u32 <= 65074)||(u32 >= 65075 && u32 <= 65076)||(u32 == 65077)||(u32 == 65078)||(u32 == 65079)||(u32 == 65080)||(u32 == 65081)||(u32 == 65082)||(u32 == 65083)||(u32 == 65084)||(u32 == 65085)||(u32 == 65086)||(u32 == 65087)||(u32 == 65088)||(u32 == 65089)||(u32 == 65090)||(u32 == 65091)||(u32 == 65092)||(u32 == 65095)||(u32 == 65096)||(u32 >= 65097 && u32 <= 65100)||(u32 >= 65101 && u32 <= 65103)||(u32 >= 65104 && u32 <= 65106)||(u32 >= 65108 && u32 <= 65111)||(u32 == 65112)||(u32 == 65113)||(u32 == 65114)||(u32 == 65115)||(u32 == 65116)||(u32 == 65117)||(u32 == 65118)||(u32 >= 65119 && u32 <= 65121)||(u32 == 65122)||(u32 == 65123)||(u32 >= 65124 && u32 <= 65126)||(u32 == 65128)||(u32 == 65129)||(u32 >= 65130 && u32 <= 65131)||(u32 >= 65136 && u32 <= 65138)||(u32 == 65140)||(u32 >= 65142 && u32 <= 65276)||(u32 >= 65281 && u32 <= 65283)||(u32 == 65284)||(u32 >= 65285 && u32 <= 65287)||(u32 == 65288)||(u32 == 65289)||(u32 == 65290)||(u32 == 65291)||(u32 == 65292)||(u32 == 65293)||(u32 >= 65294 && u32 <= 65295)||(u32 >= 65296 && u32 <= 65305)||(u32 >= 65306 && u32 <= 65307)||(u32 >= 65308 && u32 <= 65310)||(u32 >= 65311 && u32 <= 65312)||(u32 >= 65313 && u32 <= 65338)||(u32 == 65339)||(u32 == 65340)||(u32 == 65341)||(u32 == 65342)||(u32 == 65343)||(u32 == 65344)||(u32 >= 65345 && u32 <= 65370)||(u32 == 65371)||(u32 == 65372)||(u32 == 65373)||(u32 == 65374)||(u32 == 65375)||(u32 == 65376)||(u32 == 65377)||(u32 == 65378)||(u32 == 65379)||(u32 >= 65380 && u32 <= 65381)||(u32 >= 65382 && u32 <= 65391)||(u32 == 65392)||(u32 >= 65393 && u32 <= 65437)||(u32 >= 65438 && u32 <= 65439)||(u32 >= 65440 && u32 <= 65470)||(u32 >= 65474 && u32 <= 65479)||(u32 >= 65482 && u32 <= 65487)||(u32 >= 65490 && u32 <= 65495)||(u32 >= 65498 && u32 <= 65500)||(u32 >= 65504 && u32 <= 65505)||(u32 == 65506)||(u32 == 65507)||(u32 == 65508)||(u32 >= 65509 && u32 <= 65510)||(u32 == 65512)||(u32 >= 65513 && u32 <= 65516)||(u32 >= 65517 && u32 <= 65518)||(u32 == 67017)||(u32 == 67044)||(u32 >= 67457 && u32 <= 67461)||(u32 >= 67463 && u32 <= 67504)||(u32 >= 67506 && u32 <= 67514)||(u32 == 69786)||(u32 == 69788)||(u32 == 69803)||(u32 >= 69934 && u32 <= 69935)||(u32 >= 70475 && u32 <= 70476)||(u32 == 70531)||(u32 == 70533)||(u32 == 70542)||(u32 == 70545)||(u32 == 70597)||(u32 >= 70599 && u32 <= 70600)||(u32 >= 70843 && u32 <= 70844)||(u32 == 70846)||(u32 >= 71098 && u32 <= 71099)||(u32 == 71992)||(u32 >= 90401 && u32 <= 90408)||(u32 >= 93544 && u32 <= 93546)||(u32 >= 117974 && u32 <= 117999)||(u32 >= 118000 && u32 <= 118009)||(u32 >= 119134 && u32 <= 119140)||(u32 >= 119227 && u32 <= 119232)||(u32 >= 119808 && u32 <= 119892)||(u32 >= 119894 && u32 <= 119964)||(u32 >= 119966 && u32 <= 119967)||(u32 == 119970)||(u32 >= 119973 && u32 <= 119974)||(u32 >= 119977 && u32 <= 119980)||(u32 >= 119982 && u32 <= 119993)||(u32 == 119995)||(u32 >= 119997 && u32 <= 120003)||(u32 >= 120005 && u32 <= 120069)||(u32 >= 120071 && u32 <= 120074)||(u32 >= 120077 && u32 <= 120084)||(u32 >= 120086 && u32 <= 120092)||(u32 >= 120094 && u32 <= 120121)||(u32 >= 120123 && u32 <= 120126)||(u32 >= 120128 && u32 <= 120132)||(u32 == 120134)||(u32 >= 120138 && u32 <= 120144)||(u32 >= 120146 && u32 <= 120485)||(u32 >= 120488 && u32 <= 120512)||(u32 == 120513)||(u32 >= 120514 && u32 <= 120538)||(u32 == 120539)||(u32 >= 120540 && u32 <= 120570)||(u32 == 120571)||(u32 >= 120572 && u32 <= 120596)||(u32 == 120597)||(u32 >= 120598 && u32 <= 120628)||(u32 == 120629)||(u32 >= 120630 && u32 <= 120654)||(u32 == 120655)||(u32 >= 120656 && u32 <= 120686)||(u32 == 120687)||(u32 >= 120688 && u32 <= 120712)||(u32 == 120713)||(u32 >= 120714 && u32 <= 120744)||(u32 == 120745)||(u32 >= 120746 && u32 <= 120770)||(u32 == 120771)||(u32 >= 120772 && u32 <= 120779)||(u32 >= 120782 && u32 <= 120831)||(u32 >= 122928 && u32 <= 122989)||(u32 >= 126464 && u32 <= 126467)||(u32 >= 126469 && u32 <= 126495)||(u32 >= 126497 && u32 <= 126498)||(u32 == 126500)||(u32 == 126503)||(u32 >= 126505 && u32 <= 126514)||(u32 >= 126516 && u32 <= 126519)||(u32 == 126521)||(u32 == 126523)||(u32 == 126530)||(u32 == 126535)||(u32 == 126537)||(u32 == 126539)||(u32 >= 126541 && u32 <= 126543)||(u32 >= 126545 && u32 <= 126546)||(u32 == 126548)||(u32 == 126551)||(u32 == 126553)||(u32 == 126555)||(u32 == 126557)||(u32 == 126559)||(u32 >= 126561 && u32 <= 126562)||(u32 == 126564)||(u32 >= 126567 && u32 <= 126570)||(u32 >= 126572 && u32 <= 126578)||(u32 >= 126580 && u32 <= 126583)||(u32 >= 126585 && u32 <= 126588)||(u32 == 126590)||(u32 >= 126592 && u32 <= 126601)||(u32 >= 126603 && u32 <= 126619)||(u32 >= 126625 && u32 <= 126627)||(u32 >= 126629 && u32 <= 126633)||(u32 >= 126635 && u32 <= 126651)||(u32 >= 127232 && u32 <= 127242)||(u32 >= 127248 && u32 <= 127278)||(u32 >= 127280 && u32 <= 127311)||(u32 >= 127338 && u32 <= 127340)||(u32 == 127376)||(u32 >= 127488 && u32 <= 127490)||(u32 >= 127504 && u32 <= 127547)||(u32 >= 127552 && u32 <= 127560)||(u32 >= 127568 && u32 <= 127569)||(u32 >= 130032 && u32 <= 130041)||(u32 >= 194560 && u32 <= 195101) then
        No
    else
        Yes

quickCheckNFKC : CP -> [Yes, No, Maybe]
quickCheckNFKC = \cp ->
    u32 = toU32 cp
    if (u32 == 160)||(u32 == 168)||(u32 == 170)||(u32 == 175)||(u32 >= 178 && u32 <= 179)||(u32 == 180)||(u32 == 181)||(u32 == 184)||(u32 == 185)||(u32 == 186)||(u32 >= 188 && u32 <= 190)||(u32 >= 306 && u32 <= 307)||(u32 >= 319 && u32 <= 320)||(u32 == 329)||(u32 == 383)||(u32 >= 452 && u32 <= 460)||(u32 >= 497 && u32 <= 499)||(u32 >= 688 && u32 <= 696)||(u32 >= 728 && u32 <= 733)||(u32 >= 736 && u32 <= 740)||(u32 >= 832 && u32 <= 833)||(u32 >= 835 && u32 <= 836)||(u32 == 884)||(u32 == 890)||(u32 == 894)||(u32 >= 900 && u32 <= 901)||(u32 == 903)||(u32 >= 976 && u32 <= 982)||(u32 >= 1008 && u32 <= 1010)||(u32 >= 1012 && u32 <= 1013)||(u32 == 1017)||(u32 == 1415)||(u32 >= 1653 && u32 <= 1656)||(u32 >= 2392 && u32 <= 2399)||(u32 >= 2524 && u32 <= 2525)||(u32 == 2527)||(u32 == 2611)||(u32 == 2614)||(u32 >= 2649 && u32 <= 2651)||(u32 == 2654)||(u32 >= 2908 && u32 <= 2909)||(u32 == 3635)||(u32 == 3763)||(u32 >= 3804 && u32 <= 3805)||(u32 == 3852)||(u32 == 3907)||(u32 == 3917)||(u32 == 3922)||(u32 == 3927)||(u32 == 3932)||(u32 == 3945)||(u32 == 3955)||(u32 >= 3957 && u32 <= 3961)||(u32 == 3969)||(u32 == 3987)||(u32 == 3997)||(u32 == 4002)||(u32 == 4007)||(u32 == 4012)||(u32 == 4025)||(u32 == 4348)||(u32 >= 7468 && u32 <= 7470)||(u32 >= 7472 && u32 <= 7482)||(u32 >= 7484 && u32 <= 7501)||(u32 >= 7503 && u32 <= 7530)||(u32 == 7544)||(u32 >= 7579 && u32 <= 7615)||(u32 >= 7834 && u32 <= 7835)||(u32 == 8049)||(u32 == 8051)||(u32 == 8053)||(u32 == 8055)||(u32 == 8057)||(u32 == 8059)||(u32 == 8061)||(u32 == 8123)||(u32 == 8125)||(u32 == 8126)||(u32 >= 8127 && u32 <= 8129)||(u32 == 8137)||(u32 == 8139)||(u32 >= 8141 && u32 <= 8143)||(u32 == 8147)||(u32 == 8155)||(u32 >= 8157 && u32 <= 8159)||(u32 == 8163)||(u32 == 8171)||(u32 >= 8173 && u32 <= 8175)||(u32 == 8185)||(u32 == 8187)||(u32 >= 8189 && u32 <= 8190)||(u32 >= 8192 && u32 <= 8202)||(u32 == 8209)||(u32 == 8215)||(u32 >= 8228 && u32 <= 8230)||(u32 == 8239)||(u32 >= 8243 && u32 <= 8244)||(u32 >= 8246 && u32 <= 8247)||(u32 == 8252)||(u32 == 8254)||(u32 >= 8263 && u32 <= 8265)||(u32 == 8279)||(u32 == 8287)||(u32 == 8304)||(u32 == 8305)||(u32 >= 8308 && u32 <= 8313)||(u32 >= 8314 && u32 <= 8316)||(u32 == 8317)||(u32 == 8318)||(u32 == 8319)||(u32 >= 8320 && u32 <= 8329)||(u32 >= 8330 && u32 <= 8332)||(u32 == 8333)||(u32 == 8334)||(u32 >= 8336 && u32 <= 8348)||(u32 == 8360)||(u32 >= 8448 && u32 <= 8449)||(u32 == 8450)||(u32 == 8451)||(u32 >= 8453 && u32 <= 8454)||(u32 == 8455)||(u32 == 8457)||(u32 >= 8458 && u32 <= 8467)||(u32 == 8469)||(u32 == 8470)||(u32 >= 8473 && u32 <= 8477)||(u32 >= 8480 && u32 <= 8482)||(u32 == 8484)||(u32 == 8486)||(u32 == 8488)||(u32 >= 8490 && u32 <= 8493)||(u32 >= 8495 && u32 <= 8497)||(u32 >= 8499 && u32 <= 8500)||(u32 >= 8501 && u32 <= 8504)||(u32 == 8505)||(u32 == 8507)||(u32 >= 8508 && u32 <= 8511)||(u32 == 8512)||(u32 >= 8517 && u32 <= 8521)||(u32 >= 8528 && u32 <= 8543)||(u32 >= 8544 && u32 <= 8575)||(u32 == 8585)||(u32 >= 8748 && u32 <= 8749)||(u32 >= 8751 && u32 <= 8752)||(u32 == 9001)||(u32 == 9002)||(u32 >= 9312 && u32 <= 9371)||(u32 >= 9372 && u32 <= 9449)||(u32 == 9450)||(u32 == 10764)||(u32 >= 10868 && u32 <= 10870)||(u32 == 10972)||(u32 >= 11388 && u32 <= 11389)||(u32 == 11631)||(u32 == 11935)||(u32 == 12019)||(u32 >= 12032 && u32 <= 12245)||(u32 == 12288)||(u32 == 12342)||(u32 >= 12344 && u32 <= 12346)||(u32 >= 12443 && u32 <= 12444)||(u32 == 12447)||(u32 == 12543)||(u32 >= 12593 && u32 <= 12686)||(u32 >= 12690 && u32 <= 12693)||(u32 >= 12694 && u32 <= 12703)||(u32 >= 12800 && u32 <= 12830)||(u32 >= 12832 && u32 <= 12841)||(u32 >= 12842 && u32 <= 12871)||(u32 == 12880)||(u32 >= 12881 && u32 <= 12895)||(u32 >= 12896 && u32 <= 12926)||(u32 >= 12928 && u32 <= 12937)||(u32 >= 12938 && u32 <= 12976)||(u32 >= 12977 && u32 <= 12991)||(u32 >= 12992 && u32 <= 13311)||(u32 >= 42652 && u32 <= 42653)||(u32 == 42864)||(u32 >= 42994 && u32 <= 42996)||(u32 >= 43000 && u32 <= 43001)||(u32 >= 43868 && u32 <= 43871)||(u32 == 43881)||(u32 >= 63744 && u32 <= 64013)||(u32 == 64016)||(u32 == 64018)||(u32 >= 64021 && u32 <= 64030)||(u32 == 64032)||(u32 == 64034)||(u32 >= 64037 && u32 <= 64038)||(u32 >= 64042 && u32 <= 64109)||(u32 >= 64112 && u32 <= 64217)||(u32 >= 64256 && u32 <= 64262)||(u32 >= 64275 && u32 <= 64279)||(u32 == 64285)||(u32 >= 64287 && u32 <= 64296)||(u32 == 64297)||(u32 >= 64298 && u32 <= 64310)||(u32 >= 64312 && u32 <= 64316)||(u32 == 64318)||(u32 >= 64320 && u32 <= 64321)||(u32 >= 64323 && u32 <= 64324)||(u32 >= 64326 && u32 <= 64433)||(u32 >= 64467 && u32 <= 64829)||(u32 >= 64848 && u32 <= 64911)||(u32 >= 64914 && u32 <= 64967)||(u32 >= 65008 && u32 <= 65019)||(u32 == 65020)||(u32 >= 65040 && u32 <= 65046)||(u32 == 65047)||(u32 == 65048)||(u32 == 65049)||(u32 == 65072)||(u32 >= 65073 && u32 <= 65074)||(u32 >= 65075 && u32 <= 65076)||(u32 == 65077)||(u32 == 65078)||(u32 == 65079)||(u32 == 65080)||(u32 == 65081)||(u32 == 65082)||(u32 == 65083)||(u32 == 65084)||(u32 == 65085)||(u32 == 65086)||(u32 == 65087)||(u32 == 65088)||(u32 == 65089)||(u32 == 65090)||(u32 == 65091)||(u32 == 65092)||(u32 == 65095)||(u32 == 65096)||(u32 >= 65097 && u32 <= 65100)||(u32 >= 65101 && u32 <= 65103)||(u32 >= 65104 && u32 <= 65106)||(u32 >= 65108 && u32 <= 65111)||(u32 == 65112)||(u32 == 65113)||(u32 == 65114)||(u32 == 65115)||(u32 == 65116)||(u32 == 65117)||(u32 == 65118)||(u32 >= 65119 && u32 <= 65121)||(u32 == 65122)||(u32 == 65123)||(u32 >= 65124 && u32 <= 65126)||(u32 == 65128)||(u32 == 65129)||(u32 >= 65130 && u32 <= 65131)||(u32 >= 65136 && u32 <= 65138)||(u32 == 65140)||(u32 >= 65142 && u32 <= 65276)||(u32 >= 65281 && u32 <= 65283)||(u32 == 65284)||(u32 >= 65285 && u32 <= 65287)||(u32 == 65288)||(u32 == 65289)||(u32 == 65290)||(u32 == 65291)||(u32 == 65292)||(u32 == 65293)||(u32 >= 65294 && u32 <= 65295)||(u32 >= 65296 && u32 <= 65305)||(u32 >= 65306 && u32 <= 65307)||(u32 >= 65308 && u32 <= 65310)||(u32 >= 65311 && u32 <= 65312)||(u32 >= 65313 && u32 <= 65338)||(u32 == 65339)||(u32 == 65340)||(u32 == 65341)||(u32 == 65342)||(u32 == 65343)||(u32 == 65344)||(u32 >= 65345 && u32 <= 65370)||(u32 == 65371)||(u32 == 65372)||(u32 == 65373)||(u32 == 65374)||(u32 == 65375)||(u32 == 65376)||(u32 == 65377)||(u32 == 65378)||(u32 == 65379)||(u32 >= 65380 && u32 <= 65381)||(u32 >= 65382 && u32 <= 65391)||(u32 == 65392)||(u32 >= 65393 && u32 <= 65437)||(u32 >= 65438 && u32 <= 65439)||(u32 >= 65440 && u32 <= 65470)||(u32 >= 65474 && u32 <= 65479)||(u32 >= 65482 && u32 <= 65487)||(u32 >= 65490 && u32 <= 65495)||(u32 >= 65498 && u32 <= 65500)||(u32 >= 65504 && u32 <= 65505)||(u32 == 65506)||(u32 == 65507)||(u32 == 65508)||(u32 >= 65509 && u32 <= 65510)||(u32 == 65512)||(u32 >= 65513 && u32 <= 65516)||(u32 >= 65517 && u32 <= 65518)||(u32 >= 67457 && u32 <= 67461)||(u32 >= 67463 && u32 <= 67504)||(u32 >= 67506 && u32 <= 67514)||(u32 >= 117974 && u32 <= 117999)||(u32 >= 118000 && u32 <= 118009)||(u32 >= 119134 && u32 <= 119140)||(u32 >= 119227 && u32 <= 119232)||(u32 >= 119808 && u32 <= 119892)||(u32 >= 119894 && u32 <= 119964)||(u32 >= 119966 && u32 <= 119967)||(u32 == 119970)||(u32 >= 119973 && u32 <= 119974)||(u32 >= 119977 && u32 <= 119980)||(u32 >= 119982 && u32 <= 119993)||(u32 == 119995)||(u32 >= 119997 && u32 <= 120003)||(u32 >= 120005 && u32 <= 120069)||(u32 >= 120071 && u32 <= 120074)||(u32 >= 120077 && u32 <= 120084)||(u32 >= 120086 && u32 <= 120092)||(u32 >= 120094 && u32 <= 120121)||(u32 >= 120123 && u32 <= 120126)||(u32 >= 120128 && u32 <= 120132)||(u32 == 120134)||(u32 >= 120138 && u32 <= 120144)||(u32 >= 120146 && u32 <= 120485)||(u32 >= 120488 && u32 <= 120512)||(u32 == 120513)||(u32 >= 120514 && u32 <= 120538)||(u32 == 120539)||(u32 >= 120540 && u32 <= 120570)||(u32 == 120571)||(u32 >= 120572 && u32 <= 120596)||(u32 == 120597)||(u32 >= 120598 && u32 <= 120628)||(u32 == 120629)||(u32 >= 120630 && u32 <= 120654)||(u32 == 120655)||(u32 >= 120656 && u32 <= 120686)||(u32 == 120687)||(u32 >= 120688 && u32 <= 120712)||(u32 == 120713)||(u32 >= 120714 && u32 <= 120744)||(u32 == 120745)||(u32 >= 120746 && u32 <= 120770)||(u32 == 120771)||(u32 >= 120772 && u32 <= 120779)||(u32 >= 120782 && u32 <= 120831)||(u32 >= 122928 && u32 <= 122989)||(u32 >= 126464 && u32 <= 126467)||(u32 >= 126469 && u32 <= 126495)||(u32 >= 126497 && u32 <= 126498)||(u32 == 126500)||(u32 == 126503)||(u32 >= 126505 && u32 <= 126514)||(u32 >= 126516 && u32 <= 126519)||(u32 == 126521)||(u32 == 126523)||(u32 == 126530)||(u32 == 126535)||(u32 == 126537)||(u32 == 126539)||(u32 >= 126541 && u32 <= 126543)||(u32 >= 126545 && u32 <= 126546)||(u32 == 126548)||(u32 == 126551)||(u32 == 126553)||(u32 == 126555)||(u32 == 126557)||(u32 == 126559)||(u32 >= 126561 && u32 <= 126562)||(u32 == 126564)||(u32 >= 126567 && u32 <= 126570)||(u32 >= 126572 && u32 <= 126578)||(u32 >= 126580 && u32 <= 126583)||(u32 >= 126585 && u32 <= 126588)||(u32 == 126590)||(u32 >= 126592 && u32 <= 126601)||(u32 >= 126603 && u32 <= 126619)||(u32 >= 126625 && u32 <= 126627)||(u32 >= 126629 && u32 <= 126633)||(u32 >= 126635 && u32 <= 126651)||(u32 >= 127232 && u32 <= 127242)||(u32 >= 127248 && u32 <= 127278)||(u32 >= 127280 && u32 <= 127311)||(u32 >= 127338 && u32 <= 127340)||(u32 == 127376)||(u32 >= 127488 && u32 <= 127490)||(u32 >= 127504 && u32 <= 127547)||(u32 >= 127552 && u32 <= 127560)||(u32 >= 127568 && u32 <= 127569)||(u32 >= 130032 && u32 <= 130041)||(u32 >= 194560 && u32 <= 195101) then
        No
    else if (u32 >= 768 && u32 <= 772)||(u32 >= 774 && u32 <= 780)||(u32 == 783)||(u32 == 785)||(u32 >= 787 && u32 <= 788)||(u32 == 795)||(u32 >= 803 && u32 <= 808)||(u32 >= 813 && u32 <= 814)||(u32 >= 816 && u32 <= 817)||(u32 == 824)||(u32 == 834)||(u32 == 837)||(u32 >= 1619 && u32 <= 1621)||(u32 == 2364)||(u32 == 2494)||(u32 == 2519)||(u32 == 2878)||(u32 == 2902)||(u32 == 2903)||(u32 == 3006)||(u32 == 3031)||(u32 == 3158)||(u32 == 3266)||(u32 >= 3285 && u32 <= 3286)||(u32 == 3390)||(u32 == 3415)||(u32 == 3530)||(u32 == 3535)||(u32 == 3551)||(u32 == 4142)||(u32 >= 4449 && u32 <= 4469)||(u32 >= 4520 && u32 <= 4546)||(u32 == 6965)||(u32 >= 12441 && u32 <= 12442)||(u32 == 69818)||(u32 == 69927)||(u32 == 70462)||(u32 == 70487)||(u32 == 70584)||(u32 == 70587)||(u32 == 70594)||(u32 == 70597)||(u32 >= 70599 && u32 <= 70601)||(u32 == 70832)||(u32 == 70842)||(u32 == 70845)||(u32 == 71087)||(u32 == 71984)||(u32 >= 90398 && u32 <= 90409)||(u32 >= 93543 && u32 <= 93544) then
        Maybe
        else
        Yes

fullCompositionExclusion : CP -> Bool
fullCompositionExclusion = \cp ->
    u32 = toU32 cp
    (u32 >= 832 && u32 <= 833)||(u32 >= 835 && u32 <= 836)||(u32 == 884)||(u32 == 894)||(u32 == 903)||(u32 >= 2392 && u32 <= 2399)||(u32 >= 2524 && u32 <= 2525)||(u32 == 2527)||(u32 == 2611)||(u32 == 2614)||(u32 >= 2649 && u32 <= 2651)||(u32 == 2654)||(u32 >= 2908 && u32 <= 2909)||(u32 == 3907)||(u32 == 3917)||(u32 == 3922)||(u32 == 3927)||(u32 == 3932)||(u32 == 3945)||(u32 == 3955)||(u32 >= 3957 && u32 <= 3958)||(u32 == 3960)||(u32 == 3969)||(u32 == 3987)||(u32 == 3997)||(u32 == 4002)||(u32 == 4007)||(u32 == 4012)||(u32 == 4025)||(u32 == 8049)||(u32 == 8051)||(u32 == 8053)||(u32 == 8055)||(u32 == 8057)||(u32 == 8059)||(u32 == 8061)||(u32 == 8123)||(u32 == 8126)||(u32 == 8137)||(u32 == 8139)||(u32 == 8147)||(u32 == 8155)||(u32 == 8163)||(u32 == 8171)||(u32 >= 8174 && u32 <= 8175)||(u32 == 8185)||(u32 == 8187)||(u32 == 8189)||(u32 >= 8192 && u32 <= 8193)||(u32 == 8486)||(u32 >= 8490 && u32 <= 8491)||(u32 == 9001)||(u32 == 9002)||(u32 == 10972)||(u32 >= 63744 && u32 <= 64013)||(u32 == 64016)||(u32 == 64018)||(u32 >= 64021 && u32 <= 64030)||(u32 == 64032)||(u32 == 64034)||(u32 >= 64037 && u32 <= 64038)||(u32 >= 64042 && u32 <= 64109)||(u32 >= 64112 && u32 <= 64217)||(u32 == 64285)||(u32 == 64287)||(u32 >= 64298 && u32 <= 64310)||(u32 >= 64312 && u32 <= 64316)||(u32 == 64318)||(u32 >= 64320 && u32 <= 64321)||(u32 >= 64323 && u32 <= 64324)||(u32 >= 64326 && u32 <= 64334)||(u32 >= 119134 && u32 <= 119140)||(u32 >= 119227 && u32 <= 119232)||(u32 >= 194560 && u32 <= 195101)