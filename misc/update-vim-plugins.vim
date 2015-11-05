PlugUpdate
only
PlugDiff

if getline(1) == 'No Updates.'
  quit
else
  PlugSnapshot ./vendor/vim/plugins
  PlugDiff
  only
endif
