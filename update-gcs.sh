
#!/usr/bin/env sh

set -e

CDN_AUTH=$(echo $CDN_AUTH | base64 --decode)

CDN_SPACE="gs://delaware-online/datasets"
PUBLIC_PATH="https://www.gannett-cdn.com/delaware-online/datasets"
CDN_PATH="https://$CDN_AUTH@www.gannett-cdn.com/delaware-online/datasets"

OUTPUT="./outputs/latest/"
PROJECT="coronavirus-tracker"

gsutil cp -r "$OUTPUT" "$CDN_SPACE/$PROJECT"
gsutil acl -r set public-read "$CDN_SPACE/$PROJECT"

curl -X PURGE --user "$CDN_AUTH" "$PUBLIC_PATH/$PROJECT"
