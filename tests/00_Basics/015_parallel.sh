#@desc Checking that plough works
echo "Run: [1, 2]" >tmp/config.yaml
${PLOUGH} -C tmp/config.yaml -P 2 -q -z -i hello | grep -v WARNING
rm -f tmp/config.yaml
