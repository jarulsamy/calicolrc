## Testing simple console language
## there is no flow of control

# ls | grep .sh | open

switch unix # comment to end of line
echo "Test 1"
pwd
#more ./Tests.sh
echo "Test 2"
ls -l ../../../examples/images/chess/ | show
#ls -l | grep Hello | open
#exec "HelloWorld.sh"
echo "Test 3"
cd
echo "Test 4"
cd -
echo "Test 5"
x = 42
echo $x
echo "Test 6"
switch
switch dos
echo "Test 7"
dir
switch unix
echo "Test 8"
ls
echo "Test 9"
grep Hello `ls | grep .sh`
echo "Test 10"
exec "def nats():\n    count = 0\n    while True:\n        yield count\n        count += 1" python
echo `eval "nats()" python` | eval "(func (lambda (n) (+ n 1)))" scheme | eval "lambda n: n + 1" python | head -n 10
echo "Done"