module [
    toLower,
    toUpper,
    Locale,
    makeLocale,
]

import CodePoint exposing [CodePoint]
import InternalCasemap
import InternalComposition
import InternalCP
import InternalDerivedProps
import InternalProps

Locale : [Und, En, Tr, Az, Li]

CodePointIterator : (List CodePoint, U64, U64)

makeLocale : Str -> Locale
makeLocale = \str ->
    when str is
        "Tr" -> Tr
        "Az" -> Az
        "En" -> En
        "Li" -> Li
        _ -> Und

toLower : Str, Locale -> Result Str [BadUtf8]
toLower = \str, language ->
    cps =
        when Str.toUtf8 str |> CodePoint.parseUtf8 is
            Ok x -> cpsToLower x language
            Err _ -> crash "This should not happen!"
    CodePoint.toStr cps

toUpper : Str, Locale -> Result Str [BadUtf8]
toUpper = \str, language ->
    cps =
        when Str.toUtf8 str |> CodePoint.parseUtf8 is
            Ok x -> cpsToUpper x language
            Err _ -> crash "This should not happen!"
    CodePoint.toStr cps

expect
    result = toUpper "Pijamalı hasta, yağız şoföre çabucak güvendi." Tr
    dbg result

    Bool.false

cpsToLower : List CodePoint, Locale -> List CodePoint
cpsToLower = \cps, locale ->
    when locale is
        Li -> cpsToLowerGeneral (cpIteratorInit cps) prefilterLithuanianLower
        Tr | Az -> cpsToLowerGeneral (cpIteratorInit cps) prefilterTurkishLower
        _ -> cpsToLowerGeneral (cpIteratorInit cps) prefilterFinalSigmaLower

cpsToLowerGeneral : CodePointIterator, (CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]) -> List CodePoint
cpsToLowerGeneral = \cpi, prefilter ->
    cpsToGeneralHelp cpi [] prefilter InternalCasemap.mapLower

cpsToUpper = \cps, locale ->
    when locale is
        Li -> cpsToUpperGeneral (cpIteratorInit cps) prefilterLithuanianUpper
        Tr | Az -> cpsToUpperGeneral (cpIteratorInit cps) prefilterTurkishUpper
        _ -> cpsToUpperGeneral (cpIteratorInit cps) prefilterIdentity

cpsToUpperGeneral :CodePointIterator, (CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]) -> List CodePoint
cpsToUpperGeneral = \cpi, prefilter ->
    cpsToGeneralHelp cpi [] prefilter InternalCasemap.mapUpper

cpsToGeneralHelp : CodePointIterator, List CodePoint, (CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]), (CodePoint -> Result (List CodePoint) [NoMapping]) -> List CodePoint
cpsToGeneralHelp = \cpi, acc, preFilter, checkFn ->
    when preFilter cpi acc is
        Break (next, nextAcc) ->
            cpsToGeneralHelp next nextAcc preFilter checkFn

        Complete completed -> completed

        Continue ->
            newAcc =
                when cpIteratorValue cpi |> checkFn is
                    Err NoMapping ->
                        List.append acc (cpIteratorValue cpi)

                    Ok mapping ->
                        buildCodePointList mapping acc
            when cpIteratorNext cpi is
                Err OutOfBounds -> newAcc
                Ok iterator -> cpsToGeneralHelp iterator newAcc preFilter checkFn

prefilterIdentity: CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
    prefilterIdentity = \_cpi, _cps -> Continue

# This filter deals with sigma in lowercasing: a final sigma must become 'ς', whereas any
# other sigma must become 'σ'
prefilterFinalSigmaLower : CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
prefilterFinalSigmaLower = \cpi, acc ->
    if (cpIteratorValue cpi |> InternalCP.toU32) == 0x03a3 then
        newAcc =
            if isFinalSigma cpi then
                List.append acc (InternalCP.fromU32Unchecked 0x03c2)
            else
                List.append acc (InternalCP.fromU32Unchecked 0x03c3)
        when cpIteratorNext cpi is
            Ok nextIterator -> Break (nextIterator, newAcc)
            Err OutOfBounds -> Complete newAcc
    else
        Continue

prefilterTurkishLower : CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
prefilterTurkishLower = \cpi, acc ->
    when cpIteratorValue cpi |> InternalCP.toU32 is
        # I-dot and I are pairs in Turkish and Azeri
        0x0130 -> buildListAndIterator [0x0069] acc cpi

        # When lowercasing remove dot above in the combination I plus dot above
        0x0307 if isAfterI cpi ->
            when cpIteratorNext cpi is
                Ok nextIterator -> Break (nextIterator, acc)
                Err OutOfBounds -> Complete acc
        # When lowercasing, unless an I is before a dot above, it turns into a
        # dotless I
        0x0049 if isBeforeDot cpi |> Bool.not ->
            buildListAndIterator [0x0131] acc cpi
        _ -> prefilterFinalSigmaLower cpi acc

prefilterTurkishUpper : CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
prefilterTurkishUpper = \cpi, acc ->
    when cpIteratorValue cpi |> InternalCP.toU32 is
        0x0069 -> buildListAndIterator [0x0130] acc cpi
        _ -> Continue

prefilterLithuanianUpper :CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
prefilterLithuanianUpper = \cpi, acc ->
    when cpIteratorValue cpi |> InternalCP.toU32 is
        0x0307 if isAfterSoftDotted cpi ->
            when cpIteratorNext cpi is
                Ok nextIterator -> Break (nextIterator, acc)
                Err OutOfBounds -> Complete acc
        _ -> Continue

# Introduce an explicit dot above when lowercasing capital I's and J's
# whenever there are more accents above.
# (of the accents used in Lithuanian: grave, acute, tilde above, and ogonek)

# 0049; 0069 0307; 0049; 0049; lt More_Above; # LATIN CAPITAL LETTER I
# 004A; 006A 0307; 004A; 004A; lt More_Above; # LATIN CAPITAL LETTER J
# 012E; 012F 0307; 012E; 012E; lt More_Above; # LATIN CAPITAL LETTER I WITH OGONEK
# 00CC; 0069 0307 0300; 00CC; 00CC; lt; # LATIN CAPITAL LETTER I WITH GRAVE
# 00CD; 0069 0307 0301; 00CD; 00CD; lt; # LATIN CAPITAL LETTER I WITH ACUTE
# 0128; 0069 0307 0303; 0128; 0128; lt; # LATIN CAPITAL LETTER I WITH TILDE
#
# TODO: I'm quite unclear whether this is performing as it should! I find
# the specification for more above very odd, and I can't find any actual examples
# of what this should be doing. Apparently, it's only supposed to work
# for dictionaries anyway, so probably not a major deal ...
prefilterLithuanianLower : CodePointIterator, List CodePoint -> [Continue, Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
prefilterLithuanianLower = \cpi, acc ->
    when cpIteratorValue cpi |> InternalCP.toU32 is
    0x00cc -> buildListAndIterator [0x0069, 0x0307, 0x0300] acc cpi
    0x00cd -> buildListAndIterator [0x0069, 0x0307, 0x0301] acc cpi
    0x0128 -> buildListAndIterator [0x0069, 0x0307, 0x0303] acc cpi
    0x0049 if isMoreAbove cpi -> buildListAndIterator [0x0069, 0x0307] acc cpi
    0x004a if isMoreAbove cpi -> buildListAndIterator [0x006a, 0x0307] acc cpi
    0x012e if isMoreAbove cpi -> buildListAndIterator [0x012f, 0x0307] acc cpi
    _ -> prefilterFinalSigmaLower cpi acc

buildListAndIterator : List U32, List CodePoint, CodePointIterator -> [Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
buildListAndIterator = \lst, acc, cpi ->
        newAcc =
            List.map lst InternalCP.fromU32Unchecked |> buildCodePointList acc
        when cpIteratorNext cpi is
            Ok nextIterator -> Break (nextIterator, newAcc)
            Err OutOfBounds -> Complete newAcc

cpIteratorInit : List CodePoint -> CodePointIterator
cpIteratorInit = \cps ->
    (cps, 0, List.len cps)

cpIteratorNext : CodePointIterator -> Result CodePointIterator [OutOfBounds]
cpIteratorNext = \(cps, index, maxIndex) ->
    if index >= maxIndex - 1 then
        Err OutOfBounds
    else
        Ok (cps, index + 1, maxIndex)

cpIteratorPrev : CodePointIterator -> Result CodePointIterator [OutOfBounds]
cpIteratorPrev = \(cps, index, maxIndex) ->
    if index <= 0 then
        Err OutOfBounds
    else
        Ok (cps, index - 1, maxIndex)

cpIteratorValue : CodePointIterator -> CodePoint
cpIteratorValue = \(cps, index, _) ->
    when List.get cps index is
        Ok cp -> cp
        Err _ -> crash "This should be impossible"

buildCodePointList : List CodePoint, List CodePoint -> List CodePoint
buildCodePointList = \addition, accumulator ->
    when addition is
        [] -> accumulator
        [cp] -> List.append accumulator cp
        [cp, .. as rest] -> buildCodePointList rest (List.append accumulator cp)

# defined as follows:
# There is an uppercase I before C, and there is no intervening combining character class 230
# (Above) or 0.
isAfterI : CodePointIterator -> Bool
isAfterI = \cpi ->
    when cpIteratorPrev cpi is
        Err OutOfBounds -> Bool.false
        Ok previous ->
            when cpIteratorValue previous is
                cp if cp == (InternalCP.fromU32Unchecked 0x0049) -> Bool.true
                cp if InternalComposition.combiningClassInternal cp == 230 -> Bool.false
                cp if InternalComposition.combiningClassInternal cp == 0 -> Bool.false
                _ -> isAfterI previous

#C is followed by COMBINING DOT ABOVE (U+0307). Any sequence of characters with a combining
#class that is neither 0 nor 230 may intervene between the current character and the
#combining dot above.
isBeforeDot : CodePointIterator -> Bool
isBeforeDot = \cpi ->
    when cpIteratorNext cpi is
        Err OutOfBounds -> Bool.false
        Ok next ->
            when cpIteratorValue next is
                cp if cp == (InternalCP.fromU32Unchecked 0x0307) -> Bool.true
                cp if InternalComposition.combiningClassInternal cp == 230 -> Bool.false
                cp if InternalComposition.combiningClassInternal cp == 0 -> Bool.false
                _ -> isBeforeDot next

#C is followed by a character of combining class 230 (Above) with no intervening character of
#combining class 0 or 230 (Above). I don't really understand this, however, because if there
#has been an intervening character of combining class 230 you would think that this would have
#triggered.
isMoreAbove : CodePointIterator -> Bool
isMoreAbove = \cpi ->
    when cpIteratorNext cpi is
        Err OutOfBounds -> Bool.false
        Ok next ->
            when cpIteratorValue next is
                cp if InternalComposition.combiningClassInternal cp == 230 -> Bool.true
                cp if InternalComposition.combiningClassInternal cp == 0 -> Bool.false
                _ -> isMoreAbove next

#Final sigma requires two expressions: one looking backwards and one looking forwards.
#C is preceded by a sequence consisting of a cased letter and then zero or more case-ignorable
#characters
isBeforeFinalSigma : CodePointIterator -> Bool
isBeforeFinalSigma = \cpi ->
    when cpIteratorPrev cpi is
        Err OutOfBounds -> Bool.false
        Ok prev ->
            when cpIteratorValue prev is
                cp if InternalDerivedProps.isCased cp -> Bool.true
                cp if InternalDerivedProps.isCaseIgnorable cp -> isBeforeFinalSigma prev
                _ -> Bool.false

#C is not followed by a sequence consisting of zero or more case-ignorable characters
#and then a cased letter.
isAfterFinalSigma : CodePointIterator -> Bool
isAfterFinalSigma = \cpi ->
    when cpIteratorNext cpi is
        Err OutOfBounds -> Bool.true
        Ok next ->
            when cpIteratorValue next is
                cp if InternalDerivedProps.isCased cp -> Bool.false
                cp if InternalDerivedProps.isCaseIgnorable cp -> isAfterFinalSigma next
                _ -> Bool.true

isFinalSigma : CodePointIterator -> Bool
isFinalSigma = \cpi ->
    isBeforeFinalSigma cpi && isAfterFinalSigma cpi

isAfterSoftDotted : CodePointIterator -> Bool
isAfterSoftDotted = \cpi ->
    when cpIteratorPrev cpi is
        Err OutOfBounds -> Bool.false
        Ok prev ->
            when cpIteratorValue prev is
                cp if InternalComposition.combiningClassInternal cp == 230 -> Bool.false
                cp if InternalComposition.combiningClassInternal cp == 0 -> Bool.false
                cp if InternalProps.isSoftDotted cp -> Bool.true
                _ -> isAfterSoftDotted prev
