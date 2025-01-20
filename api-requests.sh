echo ""
echo ""
echo "GET http://127.0.0.1:8989/rest/1/health/check"
curl -X GET http://127.0.0.1:8989/rest/1/health/check \
-H 'Accept: application/json'


echo ""
echo ""
echo "POST http://127.0.0.1:8989/rest/1/phones/register"
curl -X POST http://127.0.0.1:8989/rest/1/phones/register \
-H 'Content-Type: application/json' \
-d '
{
  "phone": "+1234567890"
}'


echo ""
echo ""
echo "POST http://127.0.0.1:8989/rest/1/phones/confirm"
curl -X POST http://127.0.0.1:8989/rest/1/phones/confirm \
-H 'Content-Type: application/json' \
-d '
{
  "phone": "+1234567890",
  "code": "567890"
}'
