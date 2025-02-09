## The purpose of this file is to generate the InternalEmoji.roc file.
##
## This file will read the test data from `data/emoji-data.txt`
## parse it and then generate the implementation for each of the Emoji properties.
app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.2/1usTzOOACTpnkarBX0ED3gFESzR4ROdAlt1Llf4WFzo.tar.br",
}

import pf.Arg
import pf.File
import "data/emoji-data.txt" as file : Str
import Helpers exposing [CPMeta, PropertyMap]

main =
    when Arg.list! {} |> List.get 1 is
        Err _ -> Task.err (InvalidArguments "USAGE: roc run InternalEmoji.roc -- path/to/package/")
        Ok arg -> File.writeUtf8 "$(Helpers.removeTrailingSlash arg)/InternalEmoji.roc" template

EMOJIProp : [Emoji, Presentation, Modifier, Base, Component, Pictographic]
EMOJIMeta : { fromBytes : List U8, property : EMOJIProp, toStr : Str }

listMeta : List EMOJIMeta
listMeta =
    # NOTE ordering matters here to match on longest first
    [
        { fromBytes: Str.toUtf8 "Extended_Pictographic", property: Pictographic, toStr: "Pictographic" },
        { fromBytes: Str.toUtf8 "Emoji_Modifier_Base", property: Base, toStr: "Base" },
        { fromBytes: Str.toUtf8 "Emoji_Modifier", property: Modifier, toStr: "Modifier" },
        { fromBytes: Str.toUtf8 "Emoji_Presentation", property: Presentation, toStr: "Presentation" },
        { fromBytes: Str.toUtf8 "Emoji_Component", property: Component, toStr: "Component" },
        { fromBytes: Str.toUtf8 "Emoji", property: Emoji, toStr: "Emoji" },
    ]

template =
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    module [EMOJI, fromCP, isPictographic]
    import InternalCP exposing [CP, toU32]

    $(propDefTemplate)
    $(isFuncTemplate)

    $(fromCPTemplate)
    """

propDefTemplate : Str
propDefTemplate =

    propStrs =
        listMeta
        |> List.map .toStr
        |> List.map \str -> "$(str)"
        |> Str.joinWith ", "

    """
    EMOJI : [$(propStrs)]
    """

isFuncTemplate : Str
isFuncTemplate =

    help : Str, EMOJIProp -> Str
    help = \name, current ->
        exp =
            cpsForProperty current
            |> List.map Helpers.metaToExpression
            |> Str.joinWith " || "

        """

        $(name) : U32 -> Bool
        $(name) = \\u32 -> $(exp)
        """

    # For each EMOJIProp define a function that returns true if the given code point has that property
    listMeta
    |> List.keepOks \{ property } ->
        when property is
            Emoji -> help "isEmoji" Emoji |> Ok
            Presentation -> help "isPresentation" Presentation |> Ok
            Modifier -> help "isModifier" Modifier |> Ok
            Base -> help "isBase" Base |> Ok
            Component -> help "isComponent" Component |> Ok
            Pictographic -> help "isPictographic" Pictographic |> Ok
    |> Str.joinWith "\n"

fromCPTemplate : Str
fromCPTemplate =
    """
    fromCP : CP -> Result EMOJI [NonEmojiCodePoint]
    fromCP = \\cp ->

        u32 = toU32 cp

        $(isXtemp listMeta "")
    """

# HELPERS

# Parse the file to map between code points and properties
fileMap : List (PropertyMap EMOJIProp)
fileMap = Helpers.propertyMapFromFile file parsePropPart

# Make a helper that returns a list of code points for the given property
cpsForProperty : EMOJIProp -> List CPMeta
cpsForProperty = \current ->
    Helpers.filterPropertyMap
        fileMap
        \{ cp, prop } -> if prop == current then Ok cp else Err NotNeeded

parsePropPart : Str -> Result EMOJIProp [ParsingError]
parsePropPart = \str ->
    when Str.splitOn str "#" is
        [propStr, ..] -> emojiPropParser (Str.trim propStr)
        _ -> Err ParsingError

expect parsePropPart " Emoji                " == Ok Emoji

emojiPropParser : Str -> Result EMOJIProp [ParsingError]
emojiPropParser = \input ->

    startsWithProp : EMOJIMeta -> Result EMOJIProp [NonPropSequence]
    startsWithProp = \prop ->
        if input |> Str.toUtf8 |> List.startsWith prop.fromBytes then
            Ok prop.property
        else
            Err NonPropSequence

    # see which properties match
    matches : List EMOJIProp
    matches = listMeta |> List.keepOks startsWithProp

    when matches is
        # take the longest match
        [a, ..] -> Ok a
        _ -> Err ParsingError

expect emojiPropParser "Extended_Pictographic" == Ok Pictographic
expect emojiPropParser "Emoji_Modifier_Base" == Ok Base
expect emojiPropParser "# ===" == Err ParsingError

# For each property, generate a function that returns true if the given code
# point has that property
isXtemp : List EMOJIMeta, Str -> Str
isXtemp = \props, buf ->
    when List.first props is
        Err ListWasEmpty ->
            "$(buf)\n        Err NonEmojiCodePoint\n"

        Ok prop ->
            when prop.property is
                Emoji ->
                    next = ifXStr "isEmoji" "Emoji"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

                Presentation ->
                    next = ifXStr "isPresentation" "Presentation"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

                Modifier ->
                    next = ifXStr "isModifier" "Modifier"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

                Base ->
                    next = ifXStr "isBase" "Base"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

                Component ->
                    next = ifXStr "isComponent" "Component"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

                Pictographic ->
                    next = ifXStr "isPictographic" "Pictographic"
                    isXtemp (List.dropFirst props 1) "$(buf)$(next)"

ifXStr : Str, Str -> Str
ifXStr = \funcStr, str ->
    "if $(funcStr) u32 then\n        Ok $(str)\n    else "
