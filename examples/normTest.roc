app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import unicode.Normalization
import pf.Stdout

main =

    raw = "Café"
    asNFD = Normalization.normalize NFD raw
    asNFC = Normalization.normalize NFC raw

    declaration =
        if asNFD == asNFC then
            "'$(asNFD)' and '$(asNFC)' look the same ... but they shouldn't be."
        else
            "'$(asNFD)' and '$(asNFC)' may look the same, but they're not."


    Stdout.line! declaration
