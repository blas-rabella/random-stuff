set -g theme_color_scheme terminal-dark
set -gx PATH $PATH ~/.cabal/bin 
set -gx PATH $PATH ~/.cargo/bin 
set -gx PATH $PATH ~/Library/Python/3.7/bin
set -gx GOPATH $HOME/Code/go-workspace # don't forget to change your path correctly!
set -gx PATH $PATH:$GOPATH/bin
set -gx PATH $PATH ~/.nix-profile/bin
set -gx PATH $PATH ~/.npm-global/bin
set -gx NIX_PATH $HOME/.nix-defexpr/channels
alias aws-google-auth='command aws-google-auth -I C01ljsruf -S 489702863414 -u your@email -p default -d 43200 -R eu-west-1'
direnv hook fish | source
# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/b.rabella/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/b.rabella/.ghcup/bin $PATH
