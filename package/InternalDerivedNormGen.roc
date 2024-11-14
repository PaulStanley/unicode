app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.File
import pf.Arg
import "data/DerivedNormalizationProps.txt" as data : Str
import Helpers
import pf.Stdout

# We first pick out from the mass of properties in the file those we are
# really interested in: just the Quick Check tables and the list of
# codepoints excluded from composition


propertyMapFromFile : Str, (List Str -> Result (Str, Str) [ParsingError]) -> List { cp : Helpers.CPMeta, prop : (Str, Str) }
propertyMapFromFile = \file, parsePropPart ->
    file
    |> Str.split "\n"
    |> List.keepOks Helpers.startsWithHex
    |> List.map \l ->

        when Str.split l ";" is
            [hexPart, .. as propPart] ->
                when (Helpers.parseHexPart hexPart, parsePropPart propPart) is
                    (Ok cp, Ok prop) -> { cp, prop }
                    _ -> crash "Error parsing line -- $(l)"

            _ -> crash "Error unexpected ';' on line -- $(l)"
    |> List.keepIf \{ cp: _, prop: (label, _value) } -> label == "NFD_QC" || label == "NFC_QC" || label == "NFKD_QC" || label == "NFKC_QC" || label == "Full_Composition_Exclusion"

parseAny = \lst ->
    when lst is
        [label, rest] ->
            value =
                when Str.splitFirst rest "#" is
                    Ok { before } -> Str.trim before
                    _ -> "Invalid"
            Ok (Str.trim label, value)

        [singleton] ->
            label =
                when Str.splitFirst singleton "#" is
                    Ok { before } -> Str.trim before
                    _ -> "Invalid"
            Ok (label, "true")

        _ -> Ok ("Other", "Other")

collectLabelled = \acc, { cp, prop: (label, value) }, target ->
    if label == target then
        List.append acc (cp, value)
    else
        acc

collectValued = \acc, (label, value), target ->
    if value == target then
        List.append acc label
    else
        acc

makeCheckFunction = \parsed, label, fname ->
    relevantEntries = List.walk parsed [] \s, i -> collectLabelled s i label
    nos =
        List.walk relevantEntries [] (\s, i -> collectValued s i "N")
        |> List.map Helpers.metaToExpression
        |> Str.joinWith "||"

    maybes = List.walk relevantEntries [] (\s, i -> collectValued s i "M")
        |> List.map Helpers.metaToExpression
        |> Str.joinWith "||"

    noSection =
        """
        if $(nos) then
                No
        """

    maybeSection =
        if Str.isEmpty maybes then
            """
            else
                    Yes
            """
        else
            """
            else if $(maybes) then
                    Maybe
            else
                    Yes
            """

    """
    quickCheck$(fname) : CP -> [Yes, No, Maybe]
    quickCheck$(fname) = \\cp ->
        u32 = toU32 cp
        $(noSection)
        $(maybeSection)
    """

makeExclusionFunction = \parsed ->
    relevantEntries =
        List.walk parsed [] \s, i -> collectLabelled s i "Full_Composition_Exclusion"
        |> List.map \(s, _) -> s
        |> List.map Helpers.metaToExpression
        |> Str.joinWith "||"

    """
    fullCompositionExclusion : CP -> Bool
    fullCompositionExclusion = \\cp ->
        u32 = toU32 cp
        $(relevantEntries)
    """

parsedFile = propertyMapFromFile data parseAny

fullCompositionExclusion = makeExclusionFunction parsedFile

nfdQC = makeCheckFunction parsedFile "NFD_QC" "NFD"

nfcQC = makeCheckFunction parsedFile "NFC_QC" "NFC"

nfkdQC = makeCheckFunction parsedFile "NFKD_QC" "NFKD"

template =
    """
    ## WARNING: This file is automatically generated. Do not edit it manually. ##
    module [
        quickCheckNFC,
        quickCheckNFKD,
        quickCheckNFD,
        fullCompositionExclusion,
    ]

    import InternalCP exposing [CP, toU32]

    $(nfdQC)

    $(nfcQC)

    $(nfkdQC)

    $(fullCompositionExclusion)
    """

main =


    Stdout.line! "$(template)"
