START='{"first_name": "Robin", "last_name": "Schroer", "email": "sulami@peerwire.org", "phone": "+49 157 3843 2299", "cover_letter": "'
COVER="$(cat cover)"
END='", "urls": ["https://github.com/sulami", "https://sulami.github.io"]}'
ENC=$(echo "$START$COVER$END" | sed ':a;N;$!ba;s/\n/\\n/g')
curl \
  -H "Content-Type: application/json" \
  -X POST \
  -d "$ENC" \
  https://app.close.io/hackwithus/
#echo "$ENC"
