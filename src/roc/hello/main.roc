#!/usr/bin/env roc
app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    weaver: "https://github.com/smores56/weaver/releases/download/0.2.0/BBDPvzgGrYp-AhIDw0qmwxT0pWZIQP_7KOrUrZfp_xw.tar.br",
}

import pf.Arg
import pf.Stdout
import pf.Task exposing [Task]
import weaver.Cli
import weaver.Param

main : Task {} _
main =
    when Cli.parseOrDisplayMessage cliParser Arg.list! is
        Ok options ->
            name = Result.withDefault options.name "world"
            Stdout.line! "Hello, $(name)!"

        Err error ->
            Stdout.line! error
            Task.err (Exit 1 "")

cliParser =
    Cli.weave {
        name: <- Param.maybeStr { name: "name", help: "Who to say hello to" },
    }
    |> Cli.finish { name: "hello" }
    |> Cli.assertValid
