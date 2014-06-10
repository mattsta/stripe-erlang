#!/bin/bash

# Note: Free app IDs have a limit of 1,000 calls per month

APP_ID=$1

if [[ -z $APP_ID ]]; then
    echo "$0 [app_id]"
    echo "You can sign up for a free app id at https://openexchangerates.org/signup/free"
    exit 1
fi

if hash gsed 2>/dev/null; then
    sed=gsed
else
    sed=sed
fi

curl https://openexchangerates.org/api/latest.json?app_id=$APP_ID |$sed '/BTC/d; /disclaimer/d; /license/d; s/"\(.*\)"/"\L\1"/' > currencies.json
