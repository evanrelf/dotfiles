# !/usr/bin/env roc
app [main] {
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    weaver: "https://github.com/smores56/weaver/releases/download/0.2.0/BBDPvzgGrYp-AhIDw0qmwxT0pWZIQP_7KOrUrZfp_xw.tar.br",
}

import ascii.Ascii
import pf.Arg
import pf.Stdout
import pf.Task exposing [Task]
import weaver.Cli
import weaver.Opt
import weaver.Param

main : Task {} _
main =
    when Cli.parseOrDisplayMessage cliParser Arg.list! is
        Ok options ->
            name = Result.withDefault options.name "world"

            if options.yell then
                name =
                    Ascii.fromStr name
                    |> Result.map Ascii.toUppercase
                    |> Result.map Ascii.toString
                    |> Result.withDefault name
                Stdout.line! "HELLO, $(name)!!!!"
            else
                Stdout.line! "Hello, $(name)!"

        Err error ->
            Stdout.line! error
            Task.err (Exit 1 "")

cliParser =
    Cli.weave {
        yell: <- Opt.flag { long: "yell", help: "Say hello louder" },
        name: <- Param.maybeStr { name: "name", help: "Who to say hello to" },
    }
    |> Cli.finish { name: "hello" }
    |> Cli.assertValid
