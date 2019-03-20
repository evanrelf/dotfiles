alias artifact "~/Code/evan.relf/scripts/artifact/artifact"
alias gauntlet "~/Code/evan.relf/scripts/gauntlet/gauntlet"
alias qa "~/Code/evan.relf/scripts/qa/qa"
alias vpn "~/Code/evan.relf/scripts/vpn/vpn"
alias autocompile "~/Code/evan.relf/scripts/autocompile/autocompile"
alias autotag "~/Code/evan.relf/scripts/autotag/autotag"
alias mk "git show makefile:Makefile | make -f -"

if _exists psql; and status --is-interactive
  abbr --add sql "psql -d vetpro -U postgres -h localhost -p 5432"
  abbr --add nr "npm run"
end
