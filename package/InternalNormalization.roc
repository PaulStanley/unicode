module [
    codePointsToNFC,
    codePointsToNFD,
    codePointsToNFKD,
    codePointsToNFKC,
    showCodePoints,
]

import CodePoint exposing [CodePoint]
import InternalCP exposing [fromU32Unchecked]
import InternalDerivedNorm exposing [quickCheckNFC, quickCheckNFD, quickCheckNFKD, quickCheckNFKC]
import InternalComposition
import Helpers

CombiningClass : U8

cClass = InternalComposition.combiningClassInternal

# This is used for testing only
# makeTest : List U32 -> List CodePoint
# makeTest = \u32s -> List.map u32s fromU32Unchecked

# This is used for testing only
showCodePoints : List CodePoint -> List _
showCodePoints = \cps ->
    List.map cps \cp ->
        base =
            if cClass cp == 0 then [] else "◌" |> Str.toUtf8
        asStr = CodePoint.appendUtf8 base cp |> Str.fromUtf8 |> Result.withDefault "�"
        { codepoint: Helpers.hexStrFromU32 (CodePoint.toU32 cp), glyph: asStr, cc: cClass cp }

# Magic numbers for hangul syllables
hangulSBase = 0xac00
hangulLBase = 0x1100
hangulVBase = 0x1161
hangulTBase = 0x11a7
hangulJamoEnd = 0x11ff
# hangulBlockEnd = 0xd7a3
hangulTCount = 28
hangulNCount = 588
hangulSEnd = 0xd7af

# This identifies the hangul _composed_ syllables, which are in a block from
# U+AC00 and U+D7AF
isHangulSyllable : CodePoint -> Bool
isHangulSyllable = \cp ->
    u32 = CodePoint.toU32 cp
    (u32 >= hangulSBase && u32 <= hangulSEnd)

# The trick here is that we are interested in identifying potential
# starters -- either L, V, T or LV. These are found either in the Hangul
# Jamo block U+1100 _or_ of combined LV forms in the Hangul syllable
# block, which may combine with a T Jamo
isHangulStarter : CodePoint -> Bool
isHangulStarter = \cp ->
    u32 = CodePoint.toU32 cp
    if (u32 >= hangulLBase && u32 <= hangulJamoEnd) then
        Bool.true
    else
        isHangulLV u32

isHangulLV : U32 -> Bool
isHangulLV = \u32 ->
    # I am not sure why this is U+D788 ... but that seems to be the
    # number required for this to work properly
    if u32 >= hangulSBase && u32 <= 0xd788 then
        (u32 - hangulSBase) % hangulTCount == 0
    else
        Bool.false

expect
    result = isHangulStarter (fromU32Unchecked 0x1111)
    result == Bool.true

# A codepoint which might have composable characters after it: combining class 0
# and not a character in the Hangul LV and LVT range
isStarter : CodePoint -> Bool
isStarter = \cp ->
    cClass cp == 0 && !(isHangulSyllable cp)

# Quickcheck. Check a list of codepoints using the quick check tables
# which return Yes, No, or Maybe. If everything is "Yes" the list is
# already in normal form.
isNormal : (CodePoint -> [Yes, No, Maybe]), List CodePoint -> [Yes, No, Maybe]
isNormal = \checkerFun, cps ->
    isNormalHelp cps 0 Yes checkerFun

isNormalHelp : List CodePoint, CombiningClass, [Yes, No, Maybe], (CodePoint -> [Yes, No, Maybe]) -> [Yes, No, Maybe]
isNormalHelp = \cps, lastCc, status, checker ->
    when cps is
        [] -> status
        [first, .. as rest] if CodePoint.isSupplementary first ->
            isNormalHelp rest lastCc status checker

        [first, .. as rest] ->
            cc = cClass first
            if cc < lastCc && cc != 0 then
                No
            else
                when checker first is
                    Yes -> isNormalHelp rest cc status checker
                    No -> No
                    Maybe -> isNormalHelp rest cc Maybe checker

decomposeOneCanonical : CodePoint -> List CodePoint
decomposeOneCanonical = \cp ->
    if isHangulSyllable cp then
        hangulDecompose cp
    else
        when InternalComposition.canonicalDecompositionInternal cp is
            Ok decomp ->
                List.map decomp (decomposeOneCanonical) |> List.join

            Err NoDecomp -> [cp]

decomposeCanonical : List CodePoint -> List CodePoint
decomposeCanonical = \cps ->
    cps
    |> List.map decomposeOneCanonical
    |> List.join

decomposeOneCompatible : CodePoint -> List CodePoint
decomposeOneCompatible = \cp ->
    if isHangulSyllable cp then
        hangulDecompose cp
    else
        when InternalComposition.canonicalDecompositionInternal cp is
            Ok decomp1 ->
                List.map decomp1 (decomposeOneCompatible) |> List.join

            Err NoDecomp ->
                when InternalComposition.compatibleDecompositionInternal cp is
                    Ok decomp2 ->
                        List.map decomp2 (decomposeOneCompatible) |> List.join

                    Err NoDecomp -> [cp]

decomposeCompatible : List CodePoint -> List CodePoint
decomposeCompatible = \cps ->
    cps
    |> List.map decomposeOneCompatible
    |> List.join

hangulDecompose : CodePoint -> List CodePoint
hangulDecompose = \cp ->
    sIndex = (CodePoint.toU32 cp) - hangulSBase
    lIndex = sIndex // hangulNCount
    vIndex = (sIndex % hangulNCount) // hangulTCount
    tIndex = sIndex % hangulTCount

    when (lIndex, vIndex, tIndex) is
        (_l, _v, t) if t == 0 ->
            [hangulLBase + lIndex, hangulVBase + vIndex] |> List.keepOks CodePoint.fromU32

        (_l, _v, _t) ->
            [hangulLBase + lIndex, hangulVBase + vIndex, hangulTBase + tIndex] |> List.keepOks CodePoint.fromU32

sortCanonical : List CodePoint -> List CodePoint
sortCanonical = \cps ->
    sortCanonicalHelp cps [] []

sortCanonicalHelp : List CodePoint, List CodePoint, List CodePoint -> List CodePoint
sortCanonicalHelp = \cps, working, acc ->
    when cps is
        [] -> sortAndJoin working acc
        [cp] if cClass cp == 0 -> sortAndJoin working acc |> List.append cp
        [cp] -> sortAndJoin (List.append working cp) acc
        [cp, .. as rest] if cClass cp == 0 ->
            newAcc = (sortAndJoin working acc) |> List.append cp
            sortCanonicalHelp rest [] newAcc

        [cp, .. as rest] ->
            sortCanonicalHelp rest (List.append working cp) acc

sortAndJoin : List CodePoint, List CodePoint -> List CodePoint
sortAndJoin = \cps, acc ->
    List.concat acc (List.sortWith cps \a, b -> Num.compare (cClass a) (cClass b))

composeCanonical : List CodePoint -> List CodePoint
composeCanonical = \cps ->
    when cps is
        [] -> cps
        [_] -> cps
        [first, .. as rest] -> composeCanonicalHelp rest first [] []

composeCanonicalHelp : List CodePoint, CodePoint, List CodePoint, List CodePoint -> List CodePoint
composeCanonicalHelp = \decomped, starter, working, composed ->
    when decomped is
        [] -> starterPlusCombiners composed starter working
        [first, .. as rest] ->
            when cComposition { starter, combiner: first } is
                Err NoComp ->
                    if isStarter first then
                        composeCanonicalHelp rest first [] (starterPlusCombiners composed starter working)
                    else
                        composeCanonicalHelp rest starter (List.append working first) composed

                Err JamoNoComp ->
                    composeCanonicalHelp rest first [] (starterPlusCombiners composed starter working)

                Ok sub ->
                    composeCanonicalHelp rest sub working composed

        [first, .. as rest] if isStarter first ->
            composeCanonicalHelp rest first [] (starterPlusCombiners composed starter working)

# I drew heavily on https://github.com/dbuenzli/uunf/blob/master/src/uunf.ml for
# this, so far as it is dealing with composing hangul characters
cComposition : { starter : CodePoint, combiner : CodePoint } -> Result CodePoint [NoComp, JamoNoComp]
cComposition = \{ starter, combiner } ->
    # We deal first with the standard case, where we have to lookup in the combination
    # tables generated from unicode data.
    if Bool.not (isHangulStarter starter) then
        shouldComposeCanonical { starter, combiner }
        # The rest is dealing with Hangul syllables
    else
        starter32 = CodePoint.toU32 starter
        combiner32 = CodePoint.toU32 combiner
        # This deals with LV combinations
        if starter32 <= 0x1112 then
            if combiner32 < hangulVBase || 0x1175 < combiner32 then
                Err JamoNoComp
            else
                l = starter32 - hangulLBase
                v = combiner32 - hangulVBase
                r = hangulSBase + (l * hangulNCount) + (v * hangulTCount)
                Ok (fromU32Unchecked r)
            # And this then deals with LVT combinations
        else if combiner32 <= hangulTBase || combiner32 > 0x11c3 then
            Err JamoNoComp
            else

        r = starter32 + combiner32 - hangulTBase
        Ok (fromU32Unchecked r)

starterPlusCombiners : List CodePoint, CodePoint, List CodePoint -> List CodePoint
starterPlusCombiners = \done, starter, combiners ->
    List.concat done (List.prepend combiners starter)

shouldComposeCanonical : { starter : CodePoint, combiner : CodePoint } -> Result CodePoint [NoComp]
shouldComposeCanonical = \{ starter, combiner } ->
    when InternalComposition.canonicalCompositionInternal { starter, combiner } is
        Err NoComp -> Err NoComp
        Ok sub ->
            if
                InternalDerivedNorm.fullCompositionExclusion sub
            then
                Err NoComp
            else
                Ok sub

codePointsToNFC : List CodePoint -> List CodePoint
codePointsToNFC = \cps ->
    when isNormal quickCheckNFC cps is
        Yes -> cps
        _ ->
            cps
            |> decomposeCanonical
            |> sortCanonical
            |> composeCanonical

codePointsToNFD : List CodePoint -> List CodePoint
codePointsToNFD = \cps ->
    when isNormal quickCheckNFD cps is
        Yes -> cps
        _ ->
            cps
            |> decomposeCanonical
            |> sortCanonical

codePointsToNFKD : List CodePoint -> List CodePoint
codePointsToNFKD = \cps ->
    when isNormal quickCheckNFKD cps is
        Yes -> cps
        _ ->
            cps
            |> decomposeCompatible
            |> sortCanonical

codePointsToNFKC : List CodePoint -> List CodePoint
codePointsToNFKC = \cps ->
    when isNormal quickCheckNFKC cps is
        Yes -> cps
        _ ->
            cps
            |> decomposeCompatible
            |> sortCanonical
            |> composeCanonical
