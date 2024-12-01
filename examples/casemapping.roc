app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import unicode.CaseMapping
import pf.Stdout



lithuanian = "LÌTHUANIAN"

sample = "THIS IS A GREEK WORD: ΣΑΒΒΑΤΟΣ. AND FOR $(lithuanian). Pijamalı hasta, yağız şoföre çabucak güvendi."

regularLower = CaseMapping.toLower sample Und |> Result.withDefault "Regular Failed"

turkishLower = CaseMapping.toLower sample (CaseMapping.makeLocale "Tr") |> Result.withDefault "Turkish failed"

lithuanianLower = CaseMapping.toLower sample (CaseMapping.makeLocale "Li") |> Result.withDefault "Lithuanian failed"

regularUpper = CaseMapping.toUpper sample Und |> Result.withDefault "Regular Failed"

turkishUpper = CaseMapping.toUpper sample Tr |> Result.withDefault "Turkish Failed"

lithuanianUpper = CaseMapping.toUpper sample Li |> Result.withDefault "Lithuanian Failed"

template =
    """
    Sample         $(sample)

    As lowercase:  $(regularLower)

    In Turkish:    $(turkishLower)

    In Lithuanian: $(lithuanianLower)

    As uppercase:  $(regularUpper)

    In Turkish:    $(turkishUpper)

    In Lithuanian: $(lithuanianUpper)
    """

main =

    Stdout.line template
