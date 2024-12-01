## Not every language has any idea of case, and even languages that do use the
## concept do not apply it to every glyph. What is the uppercase of '+'? Or '!'?
## A few scripts or languages which use case have little quirks. In Greek, the
## lower-case letter sigma takes two forms: 'ς' at the end of a word, and
## 'σ' elsewhere. In Turkish, the capital I becomes a dotless 'ı', whereas in
## most other languages it becomes 'i'. So case is not only script-specific,
## but sometimes language specific.
##
## Things get even more complicated when it comes to so-called "title" case,
## where (for instance) English usually capitalised the first letter of every
## word *except* some common prepositions and articles: "Lord of the Rings"
## not "Lord Of The Rings". German and French would take different approaches.
##
## The so-called "basic" unicode case-mapping algorithms do *not* address all
## of the possible quirks: for instance, its idea of title-case is just one where
## the first letter of every word is capitalized. But it does address some of them
## (such as the final sigma and dotless-i points mentioned above).
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

## Note: this is just a placeholder for a properly developed idea about language
## tags and Locales. Assume that this WILL CHANGE.
Locale : [Und, En, Tr, Az, Li]

# Because case mapping often has to look forward and backwards in a string, this
# provides a type for an iterator, which is manipulated with the various cpIterator
# functions.
CodePointIterator : (List CodePoint, U64, U64)

## Temporary convenience function for languages. Assume that this WILL CHANGE.
makeLocale : Str -> Locale
makeLocale = \str ->
    when str is
        "Tr" -> Tr
        "Az" -> Az
        "En" -> En
        "Li" -> Li
        _ -> Und

## Convert a string to lowercase, given a particular language. The basic Unicode
## algorithm only offers "special" cases for Turkish (Tr), Azeri (Az), and
## Lithuanian (Li). It deals always with greek final sigma.
toLower : Str, Locale -> Result Str [BadUtf8]
toLower = \str, language ->
    cps =
        when Str.toUtf8 str |> CodePoint.parseUtf8 is
            Ok x -> cpsToLower x language
            Err _ -> crash "This should not happen!"
    CodePoint.toStr cps

## Convert a string to uppercase, given a particular language. The basic unicode
## alogrithm only offers "special" cases for Turkish (Tr), Azeri (Az), and
## Lithuanian (Li). It always makes some (controversial) shifts to iota-subscripted
## vowels. But it does not--as it arguably should--remove accents from capitals
## at the start of words, partly because that would require a knowledge of the
## semantics of the language, and in particular whether two vowels form a dipthong
## in a particular case, that it doesn't have. For good or ill, we simply aim to
## to follow the algorithm specified in the Unicode standard.
toUpper : Str, Locale -> Result Str [BadUtf8]
toUpper = \str, language ->
    cps =
        when Str.toUtf8 str |> CodePoint.parseUtf8 is
            Ok x -> cpsToUpper x language
            Err _ -> crash "This should not happen!"
    CodePoint.toStr cps

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

# This is the core function. We use the preFilter as a function that will "intervene" to handle
# special cases. We provide the basic mapping function so that this can be used both for upper
# and lowercase conversion. Because the prefilter will need to look forwards and backwards
# in the string, we do not simply consume codepoints, but do so via an iterator.
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

# A "null" prefilter. TODO: Convenient because it means we can combine upper and lower-casing
# in a single function, but might be dispensable if it turns out to be a performance problem.
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

# This simply pulls out a very common pattern that emerged in many of the prefilters
buildListAndIterator : List U32, List CodePoint, CodePointIterator -> [Break (CodePointIterator, List CodePoint), Complete (List CodePoint)]
buildListAndIterator = \lst, acc, cpi ->
        newAcc =
            List.map lst InternalCP.fromU32Unchecked |> buildCodePointList acc
        when cpIteratorNext cpi is
            Ok nextIterator -> Break (nextIterator, newAcc)
            Err OutOfBounds -> Complete newAcc

# This function initiates a codepoint iterator, which should then be manipulated
# and accessed using cpIteratorNext, cpIteratorPrev, and cpIteratorValue
cpIteratorInit : List CodePoint -> CodePointIterator
cpIteratorInit = \cps ->
    (cps, 0, List.len cps)

# Move the iterator forward, if possible
cpIteratorNext : CodePointIterator -> Result CodePointIterator [OutOfBounds]
cpIteratorNext = \(cps, index, maxIndex) ->
    if index >= maxIndex - 1 then
        Err OutOfBounds
    else
        Ok (cps, index + 1, maxIndex)

# Move the iterator backwards, if possible
cpIteratorPrev : CodePointIterator -> Result CodePointIterator [OutOfBounds]
cpIteratorPrev = \(cps, index, maxIndex) ->
    if index <= 0 then
        Err OutOfBounds
    else
        Ok (cps, index - 1, maxIndex)

# Return the value of the iterator at the "cursor"
cpIteratorValue : CodePointIterator -> CodePoint
cpIteratorValue = \(cps, index, _) ->
    when List.get cps index is
        Ok cp -> cp
        Err _ -> crash "This should be impossible"

# A convenience for adding an unknown number of codepoints to a list that is
# being accumulated. TODO: Would List.join simply be more efficient here?
buildCodePointList : List CodePoint, List CodePoint -> List CodePoint
buildCodePointList = \addition, accumulator ->
    when addition is
        [] -> accumulator
        [cp] -> List.append accumulator cp
        [cp, .. as rest] -> buildCodePointList rest (List.append accumulator cp)

# The rest of the module defines various tests which relate a given codepoint to its
# surroundings.

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
