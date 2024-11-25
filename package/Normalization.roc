## Normalization puts strings of unicode characters into
## a predictable form and order. There are various reasons to do that. The
## most common is to enable comparison of semantically equivalent strings.
##
## Full details are provided in the Unicode Standard Annex 15
## [Unicode Normalization Forms](https://unicode.org/reports/tr15/).
## Here's the basic idea. Suppose you have the word "café". In unicode
## that could be written in different ways. At the risk of oversimplification
## one way involves breaking the "é" into two characters: an "e" and a
## "combining" acute accent, which will then be over-printed on top of
## the "e":
##
## The other way involves a single codepoint which represents an "e with
## an acute accent".
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
##     <fi>, n, a, l, é [NKFD]-> f, i, n, a, l, e, ´ [NKFC] -> f, i, n, a, l, é
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
module [
    NormalizationForm,
    normalize,
]

import InternalNormalization
import CodePoint

## Normalization form
##
## There are four normalization forms:
##
## * NFC: Normalization Form C ("Canonically Composed).
##   The string is _canonically_ composed and ordered. This means
##   that (e.g.) some composed forms with accents are preferred over combining
##   accents. It is generally compatible with Latin-1, and is probably the most
##   widely used normalization form.
##
## * NFD: Normalization Form D ("Canonically Decomposed).
##   The string is _canonically_ decomposed. This means that (e.g.) accents
##   and combining marks are separated from the letters to which they relate
##   and canonically ordered.
##
## * NFKC: Normalization Form KC ("Compatibly Decomposed and Canonically Composed).
##   The string is _compatibly_ decomposed, which means that quite extensive
##   changes may be made to represent the essential _semantic_ function of the
##   characters. There is then _limited_ recomposition, which composes only
##   the same characters that are composed in NFC.
##
## * NFKD: Normalization Form KD ("Compatibly Decomposed"). The string is
##   _compatibly_ decomposed, which means that quite extensive changes may be made
##  to represent the essential _semantic_ function of the characters.
NormalizationForm : [NFC, NFD, NFKC, NFKD]

# Consistently with the suggestion in grapheme, and with discussion on Zulip
# https://roc.zulipchat.com/#narrow/channel/231634-beginners/topic/Questions.20about.20API.20design
# this _assumes_ we get valid utf8 and simply crashes if we don't have it. Still
# up for discussion though ...

## Put a string into normalized form. The form is specified as one of NFC, NFD,
## NFKC, or NFKC. For further discussion of the normalization forms see the package
## documentation.
##
## Example:
##
## ```(roc)
## asNfc = normalize NFC "café"
## asNFd = normalize NFD "café"
## expect asNfc != asNfd
normalize : NormalizationForm, Str -> Str
normalize = \form, str ->
    normalizer =
        when form is
            NFD -> InternalNormalization.codePointsToNFD
            NFC -> InternalNormalization.codePointsToNFC
            NFKD -> InternalNormalization.codePointsToNFKD
            NFKC -> InternalNormalization.codePointsToNFKC
    when str |> Str.toUtf8 |> CodePoint.parseUtf8 is
        Err _ -> crash "String did not parse to valid unicode. This should never happen."
        Ok cps ->
            when normalizer cps |> CodePoint.toStr is
                Err _ -> crash "Failed to normalize string. This is definitely a bug in Normalization.roc in the unicode package."
                Ok normalized -> normalized

expect
    str = "Café society"
    res = normalize NFD str
    str != res
