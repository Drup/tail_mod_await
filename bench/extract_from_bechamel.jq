[.series.[] |
(.name | capture("^(?<name>[a-z_ ]+) (?<run>[0-9.]+)$")) as $scan |
($scan.run | tonumber) as $nb |
{ name : $scan.name,
  nb: $nb,
  time: .result.estimate,
  rsquare: .result.r_square}
]