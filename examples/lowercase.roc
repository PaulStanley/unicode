app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import unicode.CaseMapping
import pf.Stdout

sample = "THIS IS A GREEK WORD: ΣΑΒΒΑΤΟΣ. AND FOR LÌTHUANIAN"

regular = CaseMapping.toLower sample (CaseMapping.makeLocale "Und") |> Result.withDefault "Regular Failed"

turkish = CaseMapping.toLower sample (CaseMapping.makeLocale "Tr") |> Result.withDefault "Turkish failed"

lithuanian = CaseMapping.toLower sample (CaseMapping.makeLocale "Li") |> Result.withDefault "Lithuanian failed"

template =
    """
    Sample        $(sample)

    As lowercase  $(regular)

    In Turkish    $(turkish)

    In Lithuanian $(lithuanian)
    """

main =

    Stdout.line template
