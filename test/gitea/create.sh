curl -X 'POST' \
  'http://localhost:3050/api/v1/user/repos' \
  -H 'accept: application/json' \
  -H 'authorization: Basic YWJhcGdpdDphYmFwZ2l0' \
  -H 'Content-Type: application/json' \
  -d '{
  "auto_init": true,
  "default_branch": "main",
  "description": "description",
  "license": "MIT",
  "name": "repo1",
  "private": false,
  "trust_model": "default"
}'
