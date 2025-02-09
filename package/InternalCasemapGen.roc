app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Arg
import pf.File
import "data/UnicodeData.txt" as data : Str
import "data/SpecialCasing.txt" as special : Str
import Helpers

isComment : Str -> Bool
isComment = \str ->
    str |> Str.trim |> Str.startsWith "#"

parseEntry : Str -> Str
parseEntry = \str ->
    if str == "" then
        "[]"
    else
        contents =
            str
            |> Str.trim
            |> Str.splitOn " "
            |> List.map Helpers.hexStrToU32
            |> List.map (\cp -> "(fromU32Unchecked $(Num.toStr cp))")
            |> Str.joinWith ", "
        "[$(contents)]"

ParsedLine : { codepoint : U32, upper : Str, lower : Str, title : Str }

# Relevant basic data is found in the last three parts of the line. In most cases
# there's nothing there and we are only interested in the times that there is
parseLine : Str -> Result ParsedLine [ParseFailure]
parseLine = \str ->
    when Str.splitOn str ";" is
        [_, _, _, _, _, _, _, _, _, _, _, _, "", "", ""] -> Err ParseFailure
        [cp, _, _, _, _, _, _, _, _, _, _, _, upper, lower, titlecase] ->
            title =
                if titlecase == "" && upper != "" then
                    upper
                else
                    titlecase
            Ok { codepoint: Helpers.hexStrToU32 cp, title: parseEntry title, lower: parseEntry lower, upper: parseEntry upper }

        _ -> Err ParseFailure

# Organised differently -- we have _lower_, _upper_ and _title_. We also want to avoid
# adding anything where there are conditions in the fifth column, because we will handle
# those cases separately, not in the table. This assumes that we have first stripped
# things down to hex lines.
parseSpecialLine : Str -> Result ParsedLine [ParseFailure]
parseSpecialLine = \str ->
    when Str.splitOn str ";" is
        [cp, lower, upper, title, comment] if isComment comment ->
            Ok { codepoint: Helpers.hexStrToU32 cp, title: parseEntry title, lower: parseEntry lower, upper: parseEntry upper }

        _ -> Err ParseFailure

toDict : Dict U32 Str, ParsedLine, [Upper, Lower, Title] -> Dict U32 Str
toDict = \dict, line, selector ->
    insertion =
        when selector is
            Upper -> "Ok $(line.upper)"
            Lower -> "Ok $(line.lower)"
            Title -> "Ok $(line.title)"
    when insertion is
        "Ok []" -> dict
        _ -> Dict.insert dict line.codepoint insertion

basic : List ParsedLine
basic =
    data
    |> Str.trim
    |> Str.splitOn "\n"
    |> List.keepOks Helpers.startsWithHex
    |> List.keepOks parseLine

specials : List ParsedLine
specials =
    special
    |> Str.trim
    |> Str.splitOn "\n"
    |> List.keepOks Helpers.startsWithHex
    |> List.keepOks parseSpecialLine

upperCaseTrie =
    List.walk basic (Dict.empty {}) (\d, x -> toDict d x Upper)
    |> \s -> List.walk specials s (\d, x -> toDict d x Upper)
    |> Dict.toList
    |> Helpers.toTrie
    |> Helpers.trieToSwitch 8 "Err NoMapping"

lowerCaseTrie =
    List.walk basic (Dict.empty {}) (\d, x -> toDict d x Lower)
    |> \s -> List.walk specials s (\d, x -> toDict d x Lower)
    |> Dict.toList
    |> Helpers.toTrie
    |> Helpers.trieToSwitch 8 "Err NoMapping"

titleCaseTrie =
    List.walk basic (Dict.empty {}) (\d, x -> toDict d x Title)
    |> \s -> List.walk specials s (\d, x -> toDict d x Title)
    |> Dict.toList
    |> Helpers.toTrie
    |> Helpers.trieToSwitch 8 "Err NoMapping"

template =
    """
    ## WARNING: This is an autogenerated file. Do not edit manually ##
    module [
        mapUpper,
        mapLower,
        mapTitle,
    ]

    import InternalCP exposing [CP, fromU32Unchecked]

    mapLower : CP -> Result (List CP) [NoMapping]
    mapLower = \\cp ->
        { high: highByte, middle: middleByte, low: lowByte } = InternalCP.splitBytes cp
        $(lowerCaseTrie)

    mapTitle : CP -> Result (List CP) [NoMapping]
    mapTitle = \\cp ->
        { high: highByte, middle: middleByte, low: lowByte } = InternalCP.splitBytes cp
        $(titleCaseTrie)

    mapUpper : CP -> Result (List CP) [NoMapping]
    mapUpper = \\cp ->
        { high: highByte, middle: middleByte, low: lowByte } = InternalCP.splitBytes cp
        $(upperCaseTrie)

    """

main =
    when Arg.list! {} |> List.get 1 is
        Err _ -> Task.err (InvalidArguments "USAGE: roc run InternalCasemapGen.roc -- path/to/package/")
        Ok arg -> File.writeUtf8 template "$(Helpers.removeTrailingSlash arg)/InternalCasemap.roc"
