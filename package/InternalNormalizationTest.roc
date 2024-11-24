## WARNING: This file is automatically generated. Do not edit it manually. ##
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }


import CodePoint exposing [CodePoint]
import pf.Stdout
import pf.Utc
import Normalization
import Helpers

convertedU32 : U32 -> CodePoint
convertedU32 = \cp ->
    when CodePoint.fromU32 cp is
    Ok x -> x
    Err _ -> crash "Invalid code point!"

converted : List U32 -> List CodePoint
converted = \cps ->
    List.map cps convertedU32

equalCodePoints : List CodePoint, List CodePoint -> Bool
equalCodePoints = \cp1, cp2 ->
    when (cp1, cp2) is
    ([], []) -> Bool.true
    ([h1, .. as t1], [h2, .. as t2]) if (CodePoint.toU32 h1) == (CodePoint.toU32 h2) ->
        equalCodePoints t1 t2
    _ -> Bool.false

#doTest : (U64, {nfc: List U32, nfd: List U32, nfkd: List U32, nfkc: List U32}) -> Task {} *
doTest = \(index, {source: sourceR, nfc: nfcR, nfd: nfdR, nfkd: nfkdR, nfkc: nfkcR}) ->
    nfc = converted nfcR
    nfd = converted nfdR
    nfkd = converted nfkdR
    nfkc = converted nfkcR
    source = converted sourceR

    start = Utc.now! {}
    nfc1 = Normalization.codePointsToNFC source
    nfc2 = Normalization.codePointsToNFC nfc
    nfc3 = Normalization.codePointsToNFC nfd
    nfc4 = Normalization.codePointsToNFC nfkd
    nfc5 = Normalization.codePointsToNFC nfkc

    nfd1 = Normalization.codePointsToNFD source
    nfd2 = Normalization.codePointsToNFD nfc
    nfd3 = Normalization.codePointsToNFD nfd
    nfd4 = Normalization.codePointsToNFD nfkd
    nfd5 = Normalization.codePointsToNFD nfkc

    nfkd1 = Normalization.codePointsToNFKD source
    nfkd2 = Normalization.codePointsToNFKD nfc
    nfkd3 = Normalization.codePointsToNFKD nfd
    nfkd4 = Normalization.codePointsToNFKD nfkd
    nfkd5 = Normalization.codePointsToNFKD nfkc
    stop = Utc.now! {}

    nfkc1 = Normalization.codePointsToNFKC source
    nfkc2 = Normalization.codePointsToNFKC nfc
    nfkc3 = Normalization.codePointsToNFKC nfd
    nfkc4 = Normalization.codePointsToNFKC nfkd
    nfkc5 = Normalization.codePointsToNFKC nfkc


    # NFC
    #      c2 ==  toNFC(c1) ==  toNFC(c2) ==  toNFC(c3)
    #      c4 ==  toNFC(c4) ==  toNFC(c5)
    nfcResult =
        equalCodePoints nfc1 nfc &&
        equalCodePoints nfc2 nfc &&
        equalCodePoints nfc3 nfc &&
        equalCodePoints nfc4 nfkc &&
        equalCodePoints nfc5 nfkc

    #    NFD
    #      c3 ==  toNFD(c1) ==  toNFD(c2) ==  toNFD(c3)
    #      c5 ==  toNFD(c4) ==  toNFD(c5)
    nfdResult =
        equalCodePoints nfd1 nfd &&
        equalCodePoints nfd2 nfd &&
        equalCodePoints nfd3 nfd &&
        equalCodePoints nfd4 nfkd &&
        equalCodePoints nfd5 nfkd

    #    NFKD
    #      c4 == toNFKD(c1) == toNFKD(c2) == toNFKD(c3) == toNFKD(c4) == toNFKD(c5)
    nfkdResult =
        equalCodePoints nfkd1 nfkd &&
        equalCodePoints nfkd2 nfkd &&
        equalCodePoints nfkd3 nfkd &&
        equalCodePoints nfkd4 nfkd &&
        equalCodePoints nfkd5 nfkd

    #    NFKC
    #      c4 == toNFKC(c1) == toNFKC(c2) == toNFKC(c3) == toNFKC(c4) == toNFKC(c5)
    #
    nfkcResult =
        equalCodePoints nfkc1 nfkc &&
        equalCodePoints nfkc2 nfkc &&
        equalCodePoints nfkc3 nfkc &&
        equalCodePoints nfkc4 nfkc &&
        equalCodePoints nfkc5 nfkc

    time = Utc.deltaAsNanos start stop |> Num.toStr
    if nfcResult && nfdResult && nfkdResult && nfkcResult then
        Stdout.line "Test $(Num.toStr index) complete in $(time)"
    else
        sourceStr = Normalization.showCodePoints source |> Inspect.toStr
        nfcStr = Normalization.showCodePoints nfc |> Inspect.toStr
        nfc1Str = Normalization.showCodePoints nfc1 |> Inspect.toStr
        nfc2Str = Normalization.showCodePoints nfc2 |> Inspect.toStr
        nfc3Str = Normalization.showCodePoints nfc3 |> Inspect.toStr
        nfc4Str = Normalization.showCodePoints nfc4 |> Inspect.toStr
        nfkcStr = Normalization.showCodePoints nfkc |> Inspect.toStr
        nfc5Str = Normalization.showCodePoints nfc5 |> Inspect.toStr

        nfdStr = Normalization.showCodePoints nfd |> Inspect.toStr
        nfd1Str = Normalization.showCodePoints nfd1 |> Inspect.toStr
        nfd2Str = Normalization.showCodePoints nfd2 |> Inspect.toStr
        nfd3Str = Normalization.showCodePoints nfd3 |> Inspect.toStr
        nfd4Str = Normalization.showCodePoints nfd4 |> Inspect.toStr
        nfkdStr = Normalization.showCodePoints nfkd |> Inspect.toStr
        nfd5Str = Normalization.showCodePoints nfd5 |> Inspect.toStr

        nfkd1Str = Normalization.showCodePoints nfkd1 |> Inspect.toStr
        nfkd2Str = Normalization.showCodePoints nfkd2 |> Inspect.toStr
        nfkd3Str = Normalization.showCodePoints nfkd3 |> Inspect.toStr
        nfkd4Str = Normalization.showCodePoints nfkd4 |> Inspect.toStr
        nfkd5Str = Normalization.showCodePoints nfkd5 |> Inspect.toStr

        nfkc1Str = Normalization.showCodePoints nfkd1 |> Inspect.toStr
        nfkc2Str = Normalization.showCodePoints nfkd2 |> Inspect.toStr
        nfkc3Str = Normalization.showCodePoints nfkd3 |> Inspect.toStr
        nfkc4Str = Normalization.showCodePoints nfkd4 |> Inspect.toStr
        nfkc5Str = Normalization.showCodePoints nfkd5 |> Inspect.toStr

        nfcResultStr =
            if nfcResult then
                "NFC test passed."
            else
                "NFC test failed.\nSource     : $(sourceStr)\nNFC        : $(nfcStr)\nSource->NFC: $(nfc1Str)\nNFC->NFC   : $(nfc2Str)\nNFD->NFC   : $(nfc3Str)\nNFKC       : $(nfkcStr)\nNFKC->NFC  : $(nfc4Str)\nNFKD->NFC  : $(nfc5Str)\n"

        nfdResultStr =
            if nfdResult then
                "NFD test passed"
            else
                "NFD test failed.\nSource     : $(sourceStr)\nNFD        : $(nfdStr)\nSource->NFD: $(nfd1Str)\nNFC->NFD   : $(nfd2Str)\nNFD->NFD   : $(nfd3Str)\nNFKD       : $(nfkdStr)\nNFKD->NFD  : $(nfd4Str)\nNFKC->NFD  : $(nfd5Str)\n"

        nfkdResultStr =
            if nfdResult then
                "NFKD test passed"
            else
                "NFKD test failed.\nSource     : $(sourceStr)\nNFKD        : $(nfkdStr)\nSource->NFKD: $(nfkd1Str)\nNFC->NFKD   : $(nfkd2Str)\nNFD->NFKD   : $(nfkd3Str)\nNFKD->NFKD  : $(nfkd4Str)\nNFKC->NFKD  : $(nfkd5Str)\n"

        nfkcResultStr =
            if nfdResult then
                "NFKC test passed"
            else
                "NFKC test failed.\nSource     : $(sourceStr)\nNFKC        : $(nfkcStr)\nSource->NFKC: $(nfkc1Str)\nNFC->NFKC   : $(nfkc2Str)\nNFD->NFKC   : $(nfkc3Str)\nNFKD->NFKC  : $(nfkc4Str)\nNFKC->NFKC  : $(nfkc5Str)\n"

        Stdout.line! "=== Test $(Num.toStr index) failed.\n$(nfcResultStr)\n$(nfdResultStr)\n$(nfkdResultStr)\n$(nfkcResultStr)"


tests = [    (44, {nfc: [7690], nfd: [68, 775], nfkc: [7690], nfkd: [68, 775], source: [7690]}),
    (45, {nfc: [7692], nfd: [68, 803], nfkc: [7692], nfkd: [68, 803], source: [7692]}),
    (46, {nfc: [7692, 775], nfd: [68, 803, 775], nfkc: [7692, 775], nfkd: [68, 803, 775], source: [7690, 803]}),
    (47, {nfc: [7692, 775], nfd: [68, 803, 775], nfkc: [7692, 775], nfkd: [68, 803, 775], source: [7692, 775]}),
    (48, {nfc: [7692, 775], nfd: [68, 803, 775], nfkc: [7692, 775], nfkd: [68, 803, 775], source: [68, 775, 803]}),
    (49, {nfc: [7692, 775], nfd: [68, 803, 775], nfkc: [7692, 775], nfkd: [68, 803, 775], source: [68, 803, 775]}),
    (50, {nfc: [7690, 795], nfd: [68, 795, 775], nfkc: [7690, 795], nfkd: [68, 795, 775], source: [7690, 795]}),
    (51, {nfc: [7692, 795], nfd: [68, 795, 803], nfkc: [7692, 795], nfkd: [68, 795, 803], source: [7692, 795]}),
    (52, {nfc: [7692, 795, 775], nfd: [68, 795, 803, 775], nfkc: [7692, 795, 775], nfkd: [68, 795, 803, 775], source: [7690, 795, 803]}),
    (53, {nfc: [7692, 795, 775], nfd: [68, 795, 803, 775], nfkc: [7692, 795, 775], nfkd: [68, 795, 803, 775], source: [7692, 795, 775]}),
    (54, {nfc: [7692, 795, 775], nfd: [68, 795, 803, 775], nfkc: [7692, 795, 775], nfkd: [68, 795, 803, 775], source: [68, 795, 775, 803]}),
    (55, {nfc: [7692, 795, 775], nfd: [68, 795, 803, 775], nfkc: [7692, 795, 775], nfkd: [68, 795, 803, 775], source: [68, 795, 803, 775]}),
    (56, {nfc: [200], nfd: [69, 768], nfkc: [200], nfkd: [69, 768], source: [200]}),
    (57, {nfc: [274], nfd: [69, 772], nfkc: [274], nfkd: [69, 772], source: [274]}),
    (58, {nfc: [200], nfd: [69, 768], nfkc: [200], nfkd: [69, 768], source: [69, 768]}),
    (59, {nfc: [274], nfd: [69, 772], nfkc: [274], nfkd: [69, 772], source: [69, 772]}),
    (60, {nfc: [7700], nfd: [69, 772, 768], nfkc: [7700], nfkd: [69, 772, 768], source: [7700]}),
    (61, {nfc: [7700], nfd: [69, 772, 768], nfkc: [7700], nfkd: [69, 772, 768], source: [274, 768]}),
    (62, {nfc: [7700, 772], nfd: [69, 772, 768, 772], nfkc: [7700, 772], nfkd: [69, 772, 768, 772], source: [7700, 772]}),
    (63, {nfc: [7700], nfd: [69, 772, 768], nfkc: [7700], nfkd: [69, 772, 768], source: [69, 772, 768]}),
    (64, {nfc: [200, 772], nfd: [69, 768, 772], nfkc: [200, 772], nfkd: [69, 768, 772], source: [69, 768, 772]}),
    (65, {nfc: [1457, 1464, 1465, 1425, 1475, 1456, 1452, 1439], nfd: [1457, 1464, 1465, 1425, 1475, 1456, 1452, 1439], nfkc: [1457, 1464, 1465, 1425, 1475, 1456, 1452, 1439], nfkd: [1457, 1464, 1465, 1425, 1475, 1456, 1452, 1439], source: [1464, 1465, 1457, 1425, 1475, 1456, 1452, 1439]}),
    (66, {nfc: [1456, 1463, 1468, 1445, 1426, 1472, 1453, 1476], nfd: [1456, 1463, 1468, 1445, 1426, 1472, 1453, 1476], nfkc: [1456, 1463, 1468, 1445, 1426, 1472, 1453, 1476], nfkd: [1456, 1463, 1468, 1445, 1426, 1472, 1453, 1476], source: [1426, 1463, 1468, 1445, 1456, 1472, 1476, 1453]}),
    (67, {nfc: [4352, 44033], nfd: [4352, 4352, 4449, 4520], nfkc: [4352, 44033], nfkd: [4352, 4352, 4449, 4520], source: [4352, 44032, 4520]}),
    (68, {nfc: [4352, 44033, 4520], nfd: [4352, 4352, 4449, 4520, 4520], nfkc: [4352, 44033, 4520], nfkd: [4352, 4352, 4449, 4520, 4520], source: [4352, 44032, 4520, 4520]}),
    (69, {nfc: [452, 803], nfd: [452, 803], nfkc: [68, 7826, 780], nfkd: [68, 90, 803, 780], source: [452, 803]}),
    (70, {nfc: [453, 803], nfd: [453, 803], nfkc: [68, 7827, 780], nfkd: [68, 122, 803, 780], source: [453, 803]}),
    (71, {nfc: [454, 803], nfd: [454, 803], nfkc: [100, 7827, 780], nfkd: [100, 122, 803, 780], source: [454, 803]}),
    (72, {nfc: [3549, 820], nfd: [3545, 3535, 820, 3530], nfkc: [3549, 820], nfkd: [3545, 3535, 820, 3530], source: [3549, 820]}),
    (73, {nfc: [13060, 820], nfd: [13060, 820], nfkc: [12452, 12491, 12531, 12464, 820], nfkd: [12452, 12491, 12531, 12463, 820, 12441], source: [13060, 820]}),
    (74, {nfc: [13063, 820], nfd: [13063, 820], nfkc: [12456, 12473, 12463, 12540, 12489, 820], nfkd: [12456, 12473, 12463, 12540, 12488, 820, 12441], source: [13063, 820]}),
    (75, {nfc: [13072, 820], nfd: [13072, 820], nfkc: [12462, 12460, 820], nfkd: [12461, 12441, 12459, 820, 12441], source: [13072, 820]}),
    (76, {nfc: [13086, 820], nfd: [13086, 820], nfkc: [12467, 12540, 12509, 820], nfkd: [12467, 12540, 12507, 820, 12442], source: [13086, 820]}),
    (77, {nfc: [13089, 820], nfd: [13089, 820], nfkc: [12471, 12522, 12531, 12464, 820], nfkd: [12471, 12522, 12531, 12463, 820, 12441], source: [13089, 820]}),
    (78, {nfc: [13106, 820], nfd: [13106, 820], nfkc: [12501, 12449, 12521, 12483, 12489, 820], nfkd: [12501, 12449, 12521, 12483, 12488, 820, 12441], source: [13106, 820]}),
    (79, {nfc: [13115, 820], nfd: [13115, 820], nfkc: [12506, 12540, 12472, 820], nfkd: [12504, 12442, 12540, 12471, 820, 12441], source: [13115, 820]}),
    (80, {nfc: [13120, 820], nfd: [13120, 820], nfkc: [12509, 12531, 12489, 820], nfkd: [12507, 12442, 12531, 12488, 820, 12441], source: [13120, 820]}),
    (81, {nfc: [13131, 820], nfd: [13131, 820], nfkc: [12513, 12460, 820], nfkd: [12513, 12459, 820, 12441], source: [13131, 820]}),
    (82, {nfc: [13134, 820], nfd: [13134, 820], nfkc: [12516, 12540, 12489, 820], nfkd: [12516, 12540, 12488, 820, 12441], source: [13134, 820]}),
    (83, {nfc: [65269, 1622], nfd: [65269, 1622], nfkc: [1604, 1570, 1622], nfkd: [1604, 1575, 1622, 1619], source: [65269, 1622]}),
    (84, {nfc: [65270, 1622], nfd: [65270, 1622], nfkc: [1604, 1570, 1622], nfkd: [1604, 1575, 1622, 1619], source: [65270, 1622]}),
    (85, {nfc: [65271, 1622], nfd: [65271, 1622], nfkc: [1604, 1571, 1622], nfkd: [1604, 1575, 1622, 1620], source: [65271, 1622]}),
    (86, {nfc: [65272, 1622], nfd: [65272, 1622], nfkc: [1604, 1571, 1622], nfkd: [1604, 1575, 1622, 1620], source: [65272, 1622]}),
    (87, {nfc: [65273, 820], nfd: [65273, 820], nfkc: [1604, 1573, 820], nfkd: [1604, 1575, 820, 1621], source: [65273, 820]}),
    (88, {nfc: [65274, 820], nfd: [65274, 820], nfkc: [1604, 1573, 820], nfkd: [1604, 1575, 820, 1621], source: [65274, 820]}),
    (89, {nfc: [160], nfd: [160], nfkc: [32], nfkd: [32], source: [160]}),
    (90, {nfc: [168], nfd: [168], nfkc: [32, 776], nfkd: [32, 776], source: [168]}),
    (91, {nfc: [170], nfd: [170], nfkc: [97], nfkd: [97], source: [170]}),
    (92, {nfc: [175], nfd: [175], nfkc: [32, 772], nfkd: [32, 772], source: [175]}),
    (93, {nfc: [178], nfd: [178], nfkc: [50], nfkd: [50], source: [178]}),
    (94, {nfc: [179], nfd: [179], nfkc: [51], nfkd: [51], source: [179]}),
    (95, {nfc: [180], nfd: [180], nfkc: [32, 769], nfkd: [32, 769], source: [180]}),
    (96, {nfc: [181], nfd: [181], nfkc: [956], nfkd: [956], source: [181]}),
    (97, {nfc: [184], nfd: [184], nfkc: [32, 807], nfkd: [32, 807], source: [184]}),
    (98, {nfc: [185], nfd: [185], nfkc: [49], nfkd: [49], source: [185]}),
    (99, {nfc: [186], nfd: [186], nfkc: [111], nfkd: [111], source: [186]}),
    (100, {nfc: [188], nfd: [188], nfkc: [49, 8260, 52], nfkd: [49, 8260, 52], source: [188]}),
    (101, {nfc: [189], nfd: [189], nfkc: [49, 8260, 50], nfkd: [49, 8260, 50], source: [189]}),
    (102, {nfc: [190], nfd: [190], nfkc: [51, 8260, 52], nfkd: [51, 8260, 52], source: [190]}),
    (103, {nfc: [192], nfd: [65, 768], nfkc: [192], nfkd: [65, 768], source: [192]}),
    (104, {nfc: [193], nfd: [65, 769], nfkc: [193], nfkd: [65, 769], source: [193]}),
    (105, {nfc: [194], nfd: [65, 770], nfkc: [194], nfkd: [65, 770], source: [194]}),
    (106, {nfc: [195], nfd: [65, 771], nfkc: [195], nfkd: [65, 771], source: [195]}),
    (107, {nfc: [196], nfd: [65, 776], nfkc: [196], nfkd: [65, 776], source: [196]}),
    (108, {nfc: [197], nfd: [65, 778], nfkc: [197], nfkd: [65, 778], source: [197]}),
    (109, {nfc: [199], nfd: [67, 807], nfkc: [199], nfkd: [67, 807], source: [199]}),
    (110, {nfc: [200], nfd: [69, 768], nfkc: [200], nfkd: [69, 768], source: [200]}),
    (111, {nfc: [201], nfd: [69, 769], nfkc: [201], nfkd: [69, 769], source: [201]}),
    (112, {nfc: [202], nfd: [69, 770], nfkc: [202], nfkd: [69, 770], source: [202]}),
    (113, {nfc: [203], nfd: [69, 776], nfkc: [203], nfkd: [69, 776], source: [203]}),
    (114, {nfc: [204], nfd: [73, 768], nfkc: [204], nfkd: [73, 768], source: [204]}),
    (115, {nfc: [205], nfd: [73, 769], nfkc: [205], nfkd: [73, 769], source: [205]}),
    (116, {nfc: [206], nfd: [73, 770], nfkc: [206], nfkd: [73, 770], source: [206]}),
    (117, {nfc: [207], nfd: [73, 776], nfkc: [207], nfkd: [73, 776], source: [207]}),
    (118, {nfc: [209], nfd: [78, 771], nfkc: [209], nfkd: [78, 771], source: [209]}),
    (119, {nfc: [210], nfd: [79, 768], nfkc: [210], nfkd: [79, 768], source: [210]}),
    (120, {nfc: [211], nfd: [79, 769], nfkc: [211], nfkd: [79, 769], source: [211]}),
    (121, {nfc: [212], nfd: [79, 770], nfkc: [212], nfkd: [79, 770], source: [212]}),
    (122, {nfc: [213], nfd: [79, 771], nfkc: [213], nfkd: [79, 771], source: [213]}),
    (123, {nfc: [214], nfd: [79, 776], nfkc: [214], nfkd: [79, 776], source: [214]}),
    (124, {nfc: [217], nfd: [85, 768], nfkc: [217], nfkd: [85, 768], source: [217]}),
    (125, {nfc: [218], nfd: [85, 769], nfkc: [218], nfkd: [85, 769], source: [218]}),
    (126, {nfc: [219], nfd: [85, 770], nfkc: [219], nfkd: [85, 770], source: [219]}),
    (127, {nfc: [220], nfd: [85, 776], nfkc: [220], nfkd: [85, 776], source: [220]}),
    (128, {nfc: [221], nfd: [89, 769], nfkc: [221], nfkd: [89, 769], source: [221]}),
    (129, {nfc: [224], nfd: [97, 768], nfkc: [224], nfkd: [97, 768], source: [224]}),
    (130, {nfc: [225], nfd: [97, 769], nfkc: [225], nfkd: [97, 769], source: [225]}),
    (131, {nfc: [226], nfd: [97, 770], nfkc: [226], nfkd: [97, 770], source: [226]}),
    (132, {nfc: [227], nfd: [97, 771], nfkc: [227], nfkd: [97, 771], source: [227]}),
    (133, {nfc: [228], nfd: [97, 776], nfkc: [228], nfkd: [97, 776], source: [228]}),
    (134, {nfc: [229], nfd: [97, 778], nfkc: [229], nfkd: [97, 778], source: [229]}),
    (135, {nfc: [231], nfd: [99, 807], nfkc: [231], nfkd: [99, 807], source: [231]}),
    (136, {nfc: [232], nfd: [101, 768], nfkc: [232], nfkd: [101, 768], source: [232]}),
    (137, {nfc: [233], nfd: [101, 769], nfkc: [233], nfkd: [101, 769], source: [233]}),
    (138, {nfc: [234], nfd: [101, 770], nfkc: [234], nfkd: [101, 770], source: [234]}),
    (139, {nfc: [235], nfd: [101, 776], nfkc: [235], nfkd: [101, 776], source: [235]}),
    (140, {nfc: [236], nfd: [105, 768], nfkc: [236], nfkd: [105, 768], source: [236]}),
    (141, {nfc: [237], nfd: [105, 769], nfkc: [237], nfkd: [105, 769], source: [237]}),
    (142, {nfc: [238], nfd: [105, 770], nfkc: [238], nfkd: [105, 770], source: [238]}),
    (143, {nfc: [239], nfd: [105, 776], nfkc: [239], nfkd: [105, 776], source: [239]}),]

main =


    Task.forEach tests doTest

