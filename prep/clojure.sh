#!/bin/sh

echo "Installing Clojure (lein)"

curl -o /local/cse-mlton/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod 755 /local/cse-mlton/bin/lein
/local/cse-mlton/bin/lein version

