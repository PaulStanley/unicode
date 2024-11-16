app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import unicode.Normalization
import unicode.CodePoint
import pf.Stdout

main =

    result =
        "hello´j..." |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.withDefault [] |> Normalization.isNormalNFC

    dbg result

    Stdout.line! "Hello"
