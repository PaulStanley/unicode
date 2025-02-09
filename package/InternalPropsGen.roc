app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }
import pf.File
import pf.Arg
import "data/PropList.txt" as data : Str
import Helpers

propertyMapFromFile : Str, (List Str -> Result (Str, Str) [ParsingError]) -> List { cp : Helpers.CPMeta, prop : (Str, Str) }
propertyMapFromFile = \file, parsePropPart ->
    file
    |> Str.splitOn "\n"
    |> List.keepOks Helpers.startsWithHex
    |> List.map \l ->

        when Str.splitOn l ";" is
            [hexPart, .. as propPart] ->
                when (Helpers.parseHexPart hexPart, parsePropPart propPart) is
                    (Ok cp, Ok prop) -> { cp, prop }
                    _ -> crash "Error parsing line -- $(l)"

            _ -> crash "Error unexpected ';' on line -- $(l)"
    |> List.keepIf \{ cp: _, prop: (label, _value) } -> label == "Soft_Dotted"

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

collectLabelled = \lst, desiredLabel ->
    List.keepIf lst (\{ cp: _, prop: (label, _value) } -> label == desiredLabel)
    |> List.map collectLabelledHelp
    |> List.join

collectLabelledHelp = \{ cp, prop: _prop } ->
    Helpers.metaToCodePointList cp

propertyMap = propertyMapFromFile data parseAny

softDottedTrie =
    propertyMap
    |> collectLabelled "Soft_Dotted"
    |> Helpers.toTrie
    |> Helpers.trieToSwitch 4 "Bool.false"

softDotted =
    """
    isSoftDotted : CP -> Bool
    isSoftDotted = \\cp ->
        { high: highByte, middle: middleByte, low: lowByte } = InternalCP.splitBytes cp
    $(softDottedTrie)
    """

template =
    """
    ## WARNING: This file is automatically generated. Do not edit it manually. ##
    module [
        isSoftDotted,
        ]

    import InternalCP exposing [CP]

    $(softDotted)
    """


main =

    when Arg.list! {} |> List.get 1 is
        Err _ -> Task.err (InvalidArguments "USAGE: roc run InternalPropsGen.roc -- path/to/package/")
        Ok arg ->
            File.writeUtf8 template "$(Helpers.removeTrailingSlash arg)/InternalProps.roc"
