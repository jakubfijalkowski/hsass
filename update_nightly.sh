#!/bin/bash
if [ "$UPDATE_NIGHTLY" = "YES" ]; then
    SNAPSHOTS="$(curl -s "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json")"
    LATEST_VER="$(echo "$SNAPSHOTS" | grep -oP 'nightly-....-..-..')"
    sed -i.bak "s/nightly-....-..-../$LATEST_VER/" stack-nightly.yaml
    rm stack-nightly.yaml.bak
fi
