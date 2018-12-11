#@desc Checking that plough works
echo "Run: [1, 2]" >config.yaml
${PLOUGH} -C config.yaml -P 2 hello | grep -v WARNING
