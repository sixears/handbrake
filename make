#!/home/martyn/bin/bash

pkgs_handbrake=$(dirname $( dirname $( realpath $(type -p HandBrakeCLI) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  perl -plE "s{__handbrake__}{$pkgs_handbrake}g" "$f" > "$t"
done
