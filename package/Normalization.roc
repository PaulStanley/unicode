module [
    isNormalNFC,
    showCodePoints,
]

import CodePoint exposing [CodePoint]
import InternalDerivedNorm exposing [quickCheckNFC, quickCheckNFD]
import InternalComposition

## Normalization is a way of putting strings of unicode characters into
## a predictable form and order. There are various reasons to do that. The
## most common is to enable predictable comparison of two strings.
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
## search will fail.
##
## You get similar problems if a single character has multiple accents.
## It's perfectly OK to do this, but if one version puts the accents in
## one order, and the other puts them in a different order, they will
## appear to be different--even though they would look just the same when
## printed out.
##
## Normalization guarantees that particular sequences will be presented
## in a given way and a given order. There are four different ways this
## can be done.
##
## The details _of the algorithms_ for doing this do not matter to the
## ordinary user. But it's worth understanding what the forms imply.
##
## NFD (which stands for "normalization form D", where "D" indicates that
## characters tend to be _decomposed_) tends to break strings up into multiple parts:
## so it would prefer to present "café" as
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
## in text so that it is "fi", and NFD will not break "fi" into "f" and "i". So far
## as _either_ of those forms is concerned "fi" ≠ "f" + "i". But for some purposes
## we may want to do that. For instance, suppose we digitized a printed text
## which used "fi" ligatures, and kept them in the digitized version as `U+FBO1`.
## We would probably want, if a user searched that text for "final" for "file",
## the search to return a positive result. This is what the "compatibility"
## normalizations do: they try to "smooth over" typographic details of the text
## to correspond to some of the ways human beings think about printed words.
##
## NFKD (which stands for "normalization form KD" where K suggests "Kompatible" and
## D suggests "decomposition") breaks text down into constituent parts, including
## not only accents but also doing things like breaking ligatures open, so "fi"
## becomes "f" and "i". It does other things too. For instance:
##
## * Until around 1810, many texts used a special form of "s" (called long-s),
##   which looked a bit like an f without the crossbar. NFKD would treat that as an "s".
##
## * Text sometimes uses superscript numbers. NFKD would treat those just as
##   numbers.
##
## Doing this, of course, strips out meaningful detail from the string: 2 followed
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
##     fi, n, a, l [NKFD]-> f, i, n, a, l [NKFC] -> f, i, n, a, l
##
## If you've read this far, you may be asking: well, what form should I use?
##
##  * For _most_ text, use either NFC or NFD. It doesn't actually matter
##    but in practice NFC is _generally_ recommended (for example for the Web)
##
##  * Whichever you use, use it consistently.
##
##  * Don't forget, however, that string concatenation cannot be guaranteed to
##    preserve normalization.

# This is used for testing only
makeTest : List U32 -> List CodePoint
makeTest = \u32s -> List.keepOks u32s CodePoint.fromU32

CombiningClass : U8

cClass = InternalComposition.combiningClassInternal

showCodePoints : List CodePoint -> List _
showCodePoints = \cps ->
    List.map cps \x ->
        base =
            if cClass x == 0 then [] else [' ']
        asStr = CodePoint.appendUtf8 base x |> Str.fromUtf8
        { codepoint: CodePoint.toU32 x, ccc: cClass x, string: asStr, nfc: quickCheckNFC x }

isNormalNFC : List CodePoint -> [Yes, No, Maybe]
isNormalNFC = \cps ->
    isNormalHelp cps 0 Yes quickCheckNFC

isNormalNFD : List CodePoint -> [Yes, No, Maybe]
isNormalNFD = \cps ->
    isNormalHelp cps 0 Yes quickCheckNFD

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

decomposeCanonical : CodePoint -> List CodePoint
decomposeCanonical = \cp ->
    if isHangulSyllable cp then
        hangulDecompose cp
    else
        when InternalComposition.canonicalDecompositionInternal cp is
            Ok decomp ->
                List.map decomp (decomposeCanonical) |> List.join

            Err NoDecomp -> [cp]

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

sortCanonical : List CodePoint -> List CodePoint
sortCanonical = \cps ->
    sortCanonicalHelp cps [] []

normalizeNFD : List CodePoint -> List CodePoint
normalizeNFD = \cps ->
    List.map cps decomposeCanonical |> List.join |> sortCanonical

composeCanonical : List CodePoint -> List CodePoint
composeCanonical = \cps ->
    when cps is
        [] -> cps
        [_] -> cps
        [first, .. as rest] -> composeCanonicalHelp rest first [] []

# TODO: HANGUL SYLLABLES
composeCanonicalHelp : List CodePoint, CodePoint, List CodePoint, List CodePoint -> List CodePoint
composeCanonicalHelp = \decomped, starter, working, composed ->
    when decomped is
        [] -> starterPlusCombiners composed starter working
        [first, .. as rest] if isStarter first ->
            composeCanonicalHelp rest first [] (starterPlusCombiners composed starter working)

        [first, .. as rest] ->
            when cComposition { starter, combiner: first } is
                Err NoComp ->
                    composeCanonicalHelp rest starter (List.append working first) composed

                Ok sub ->
                    composeCanonicalHelp rest sub working composed

# a character which might have composable characters after it: combining class 0
# and not a character in the Hangul V, T, or VT range
isStarter : CodePoint -> Bool
isStarter = \cp ->
    cClass cp == 0 && (Bool.not (isHangulSyllable cp)) || isHangulVT cp

# I drew heavily on https://github.com/dbuenzli/uunf/blob/master/src/uunf.ml for
# this, so far as it is dealing with composing hangul characters
cComposition : { starter : CodePoint, combiner : CodePoint } -> Result CodePoint [NoComp]
cComposition = \{ starter, combiner } ->
    if Bool.not (isHangulSyllable starter) then
        shouldComposeCanonical { starter, combiner }
    else
        starter32 = CodePoint.toU32 starter
        combiner32 = CodePoint.toU32 combiner
        if starter32 <= 0x1112 then
            if combiner32 < hangulVBase || 0x1175 < combiner32 then
                Err NoComp
            else
                l = starter32 - hangulLBase
                v = starter32 - hangulVBase
                r = hangulSBase + (l * hangulNCount) + (v * hangulTCount)
                Ok (CodePoint.fromU32Unsafe r)
        else if (starter32 - hangulSBase) % hangulTCount == 0 then
            if combiner32 <= hangulTBase || combiner32 > 0x11c3 then
                Err NoComp
            else
                r = starter32 + combiner32 - hangulTBase
                Ok (CodePoint.fromU32Unsafe r)
            else

        shouldComposeCanonical { starter, combiner }

starterPlusCombiners : List CodePoint, CodePoint, List CodePoint -> List CodePoint
starterPlusCombiners = \done, starter, combiners ->
    List.concat done (List.prepend combiners starter)

shouldComposeCanonical : { starter : CodePoint, combiner : CodePoint } -> Result CodePoint [NoComp]
shouldComposeCanonical = \{ starter, combiner } ->
    when InternalComposition.canonicalCompositionInternal { starter, combiner } is
        Err NoComp -> Err NoComp
        Ok sub ->
            if
                InternalDerivedNorm.fullCompositionExclusion starter
                || InternalDerivedNorm.fullCompositionExclusion combiner
            then
                Err NoComp
            else
                Ok sub

# Hangul syllables
hangulSBase = 0xac00
hangulLBase = 0x1100
hangulVBase = 0x1161
hangulTBase = 0x11a7
hangulBlockEnd = 0xd7a3
hangulLCount = 19
hangulVCount = 21
hangulTCount = 28
hangulNCount = 588
hangulSCount = 11172

isHangulSyllable : CodePoint -> Bool
isHangulSyllable = \cp ->
    u32 = CodePoint.toU32 cp
    u32 >= hangulSBase && u32 <= hangulBlockEnd

isHangulVT : CodePoint -> Bool
isHangulVT = \cp ->
    u32 = CodePoint.toU32 cp
    u32 >= hangulVBase && u32 <= hangulBlockEnd

hangulDecompose : CodePoint -> List CodePoint
hangulDecompose = \cp ->
    sIndex = (CodePoint.toU32 cp) - hangulSBase

    lIndex = sIndex // hangulNCount

    vIndex = (lIndex % hangulNCount) // hangulTCount

    tIndex = sIndex % hangulTCount
    dbg tIndex

    when (lIndex, vIndex, tIndex) is
        (_, v, t) if v == 0 && t == 0 -> [cp]
        (_, _, t) if t == 0 ->
            [hangulLBase + lIndex, hangulVBase + vIndex] |> List.keepOks CodePoint.fromU32

        (l, v, t) ->
            [hangulLBase + lIndex, hangulVBase + vIndex, hangulTBase + tIndex] |> List.keepOks CodePoint.fromU32

expect
    start = CodePoint.fromU32Unsafe 0xd4db
    dbg [start] |> showCodePoints

    result = hangulDecompose start
    dbg result |> showCodePoints

    1 == 2

expect
    start = [0x2126] |> makeTest
    end = normalizeNFD start
    dbg start |> showCodePoints

    dbg end |> showCodePoints

    valid = isNormalNFD end
    valid == Yes

expect
    res = [0x00c5] |> makeTest |> List.map decomposeCanonical |> List.join |> showCodePoints
    dbg res

    1 == 1

expect
    valid = [0x00c5] |> makeTest |> isNormalNFC
    valid == Yes

expect
    invalid = [0x2126] |> makeTest |> isNormalNFC
    invalid == No

expect
    invalid = [0x0041, 0x030a] |> makeTest |> isNormalNFC
    invalid == Maybe

expect
    valid = [0x0071, 0x0323, 0x0307] |> makeTest |> isNormalNFC
    valid == Maybe # not sure if this is right

expect
    invalid = [0x0071, 0x0307, 0x0323] |> makeTest |> isNormalNFC
    invalid == No

expect
    valid = [0x00c5] |> makeTest |> isNormalNFD
    valid == No

expect
    invalid = [0x2126] |> makeTest |> isNormalNFD
    invalid == No

expect
    invalid = [0x0041, 0x030a] |> makeTest |> isNormalNFD
    invalid == Yes

expect
    valid = [0x0071, 0x0323, 0x0307] |> makeTest |> isNormalNFD
    valid == Yes # not sure if this is right

expect
    invalid = [0x0071, 0x0307, 0x0323] |> makeTest |> isNormalNFD
    invalid == No

expect
    valid = [0x1e0a, 0x0323] |> makeTest |> normalizeNFD |> isNormalNFC
    dbg [0x1e0a, 0x0323] |> makeTest |> showCodePoints

    valid == Yes

expect
    valid = [0x0073, 0x0323, 0x0307] |> makeTest |> normalizeNFD |> composeCanonical
    dbg valid

    1 == 1

expect
    valid = [] |> makeTest |> composeCanonical
    dbg List.map valid isHangulSyllable

    1 == 2
