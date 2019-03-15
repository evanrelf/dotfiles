alias artifact "~/Code/scripts/artifact/artifact"
alias gauntlet "~/Code/scripts/gauntlet/gauntlet"
alias qa "~/Code/scripts/qa/qa"
alias vpn "~/Code/scripts/vpn/vpn"
alias autocompile "~/Code/scripts/autocompile/autocompile"
alias autotag "~/Code/scripts/autotag/autotag"
alias mk "git show makefile:Makefile | make -f -"

if _exists psql; and status --is-interactive
  abbr --add sql "psql -d vetpro -U postgres -h localhost -p 5432"
  abbr --add nr "npm run"
end
