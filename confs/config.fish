set -g theme_color_scheme terminal-dark
set -gx PATH $PATH ~/.cabal/bin 
set -gx PATH $PATH ~/.cargo/bin 
set -gx PATH $PATH ~/Library/Python/3.7/bin
set -gx GOPATH $HOME/Code/go-workspace # don't forget to change your path correctly!
set -gx PATH $PATH:$GOPATH/bin
direnv hook fish | source
