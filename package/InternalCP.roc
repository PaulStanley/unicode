module [
    CP,
    fromU32Unchecked,
    toU32,
]

CP := U32 implements [Eq, Hash, Inspect { toInspector: codePointInspector }]

toU32 : CP -> U32
toU32 = \@CP u32 -> u32

fromU32Unchecked : U32 -> CP
fromU32Unchecked = @CP

codePointInspector : CP -> Inspector f where f implements InspectFormatter
codePointInspector = \@CP cp ->
    hex = hexStrFromU32 cp
    Inspect.str hex

decToHex : U8 -> U8
decToHex = \dec ->
    when dec is
    0 -> '0'
    1 -> '1'
    2 -> '2'
    3 -> '3'
    4 -> '4'
    5 -> '5'
    6 -> '6'
    7 -> '7'
    8 -> '8'
    9 -> '9'
    10 -> 'A'
    11 -> 'B'
    12 -> 'C'
    13 -> 'D'
    14 -> 'E'
    15 -> 'F'
    _ -> crash "decToHex: too big!"

hexStrFromU32 : U32 -> Str
hexStrFromU32 = \u32 ->
    result = asHexHelper u32 []
    padded =
        when List.len result is
            0 -> ['0', '0', '0', '0']
            1 -> List.concat ['0', '0', '0'] result
            2 -> List.concat ['0', '0'] result
            3 -> List.prepend result '0'
            _ -> result
    when padded |> Str.fromUtf8 is
        Ok x -> Str.concat "U+" x
        Err _ -> crash "asHexStr: something wrong!"

asHexHelper : U32, List U8 -> List U8
asHexHelper = \dec, hex ->
    if dec < 16 then
        List.prepend hex (decToHex (Num.toU8 dec))
    else
        here = dec % 16 |> Num.toU8 |> decToHex
        next = dec // 16
        asHexHelper next (List.prepend hex here)
