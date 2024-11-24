module [
    showCodePoints,
    codePointsToNFC,
    codePointsToNFD,
    codePointsToNFKD,
    codePointsToNFKC,
    NormalizationForm,
]

import CodePoint exposing [CodePoint]
import InternalDerivedNorm exposing [quickCheckNFC, quickCheckNFD, quickCheckNFKD, quickCheckNFKC]
import InternalComposition
import Helpers

CombiningClass : U8

NormalizationForm: [NFD, NFC, NFKD, NFKC]

cClass = InternalComposition.combiningClassInternal

## Normalization puts strings of unicode characters into
## a predictable form and order. There are various reasons to do that. The
## most common is to enable comparison of semantically equivalent strings.
##
## Here's the basic idea. Suppose you have the word "café". In unicode
## that could be written in different ways. At the risk of oversimplification
## one way involves breaking the "é" into two characters: an "e" and a
## "combining" acute accent, which will then be over-printed on top of
## the "e":
##
## The other way involves a single codepoint which represents an "e with
## an acute accent":
##
## A user searching for the word "café" in some text may provide the
## search term in either form, and the person who prepared the text may
## have encoded it in either form. If the two forms don't match, the
## search will fail, although from a human point of view the printed
## texts would seem identical.
##
## You get similar problems if a single character has multiple accents.
## It's perfectly OK to do this, but if one version puts the accents in
## one order, and the other puts them in a different order, they will
## appear to be different--even though they would look just the same when
## printed out.
##
## Normalization aims to present "equivalent" sequences in a predictable
## form and a given order. There are four different ways this
## can be done.
##
## The details _of the algorithms_ for doing this do not matter to the
## ordinary user. But it's worth understanding what the forms imply.
##
## NFD (which stands for "normalization form D", where "D" indicates that
## characters will be _decomposed_) breaks strings up into multiple
## parts. So it would prefer to present "café" as
##
##    "c", "a", "f", "e", "´"
##
## It will arrange accents and other "combining marks" into a predictable
## order.
##
## NFC (which stands for "normalization form C" where "C" indicates that
## characters tend to be _composed_) has the opposite aim. It tends to put
## strings together using _pre-combined_ characters where possible. So it
## would prefer to present "café" as
##
##     "c", "a", "f", "é"
##
## These are the most important forms. The two other forms are "compatibility"
## normalizations. Here is a simple example. In traditional printing, the
## letters "f" and "i" are usually combined together into a ligature "fi". That
## is a single "character", and it has a unicode codepoint `U+FB01`.
##
## NFC and NFD will leave this character unchanged: NFC will not combine "f" and "i"
## in text so that it is [fi], and NFD will not break [fi] into "f" and "i". So far
## as _either_ of those forms is concerned "fi" ≠ "f" + "i". But for some purposes
## we may not want that. For instance, suppose we digitized a printed text
## which used "fi" ligatures, and kept them in the digitized version as `U+FBO1`.
## If a user searched that text for "final" for "file", they would probably hope
## the search would return a positive result. This is what the "compatibility"
## normalizations do: they try to "smooth over" typographic details of the text
## to correspond to some of the ways human beings think about printed words.
##
## NFKD (which stands for "normalization form KD" where K suggests "Kompatible" and
## D suggests "decomposition") breaks text down into constituent parts, including
## doing things like breaking ligatures open, so "fi"
## becomes "f" and "i". Some other examples:
##
## * Until around 1810, many texts used a special form of "s" (called long-s),
##   which looked a bit like an f without the crossbar. NFKD would treat that as an "s".
##
## * Text sometimes uses superscript numbers. NFKD would treat those just as
##   numbers.
##
## Doing this, of course,it  strips out meaningful detail from the string: 2 followed
## by a superscript 2 doesn't "mean" the same thing as 2 followed by a 2. But for
## some searching operations it is better to identify "false positives" rather than
## risk missing possibly interesting matches.
##
## The final form is NFKC (which stands for "normalization form KC" where "K"
## stands for "Kompatible" and "C" stands for "composed"). It first strips does the
## decompositions that NKFD does and then a _partial_ recomposition. It's not a
## full recomposition: it simply puts the result of NKFD into the form that NFC
## would produce. So accents will be recomposed
##
##     c, a, f, e, ´ -> c, a, f, é
##
## but the "fi" ligature would not be restored
##
##     <fi>, n, a, l [NKFD]-> f, i, n, a, l [NKFC] -> f, i, n, a, l
##
##
## If you've read this far, you may be asking: well, what form should I use?
##
##  * For _most_ text, use either NFC or NFD. It doesn't actually matter
##    but in practice NFC is _generally_ recommended (for example for the Web),
##    because it is likely to produce slightly shorter strings and is
##    "out of the box" compatible with Latin-1 encoding.
##
##  * Use compatibility forms for text which needs to be searched semantically
##    and you don't need to recover the original form of the text from them.
##
##  * Whichever you use, use it consistently.
##
##  * Don't forget, however, that string concatenation cannot be guaranteed to
##    preserve normalization. You will need to normalize whenever you (a)
##    accept any text that is not guaranteed to be in the normal form you
##    require or (b) concatenate any strings.
##
##  Note: do not confuse normalization with internationalization. Normalization is
##  about ensuring that particular sequences of glyphs will have a predictable
##  representation in terms of codepoints. It does not deal with those aspects
##  of text manipulation which are about things like uppercasing or collation.

# This is used for testing only
makeTest : List U32 -> List CodePoint
makeTest = \u32s -> List.map u32s CodePoint.fromU32Unsafe

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
    result = isHangulStarter (CodePoint.fromU32Unsafe 0x1111)
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
                Ok (CodePoint.fromU32Unsafe r)
            # And this then deals with LVT combinations
        else if combiner32 <= hangulTBase || combiner32 > 0x11c3 then
            Err JamoNoComp
            else

        r = starter32 + combiner32 - hangulTBase
        Ok (CodePoint.fromU32Unsafe r)

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

normalize : NormalizationForm, Str -> Str
normalize = \form, str ->
    normalizer =
        when form is
            NFD -> codePointsToNFD
            NFC -> codePointsToNFC
            NFKD -> codePointsToNFKD
            NFKC -> codePointsToNFKC
    when str |> Str.toUtf8 |> CodePoint.parseUtf8 is
        Err _ -> crash "String did not parse to valid unicode."

        Ok cps ->
            when normalizer cps |> CodePoint.toStr is
            Err _ -> crash "Failed to normalize string. This is definitely a bug in Normalization.roc in the unicode package."

            Ok normalized -> normalized

expect
    str = "Café society"
    res = normalize NFD str
    dbg Str.toUtf8 str

    dbg Str.toUtf8 res

    str == res
