The Instruction for Running HIME demo
Software is Tested in MAC OS X. An Screenshot is shown screen.png

TO RUN DEMO:

chmod 777 demo.sh
. demo.sh

TO VISUALIZE DEMO IN MATLAB:

plotVLMotif('dishwasher.txt','res.txt');
or
plotVLMotif('dishwasher.txt','res.txt’, [Motif Threshold You Want to Test]);

TO TEST OWN DATASET:

Using Default Parameter (start enumeration from length 300, motif threshold R(L)=0.02L):

java -Xmx8g -jar HIME.jar [DATASET] > tmp.log

Choosing minimum length start enumeration:

java -Xmx8g -jar HIME.jar [DATASET] [MINIMUM LENGTH] > tmp.log

Choosing PAA and minimum length:

java -Xmx8g -jar HIME.jar [DATASET] [PAA] [MINIMUM LENGTH]  > tmp.log

Choosing PAA, Alphabet Size and minimum length:

java -Xmx8g -jar HIME.jar [DATASET] [PAA] [MINIMUM LENGTH] [ALPHABET SIZE]  > tmp.log

Choosing PAA, Alphabet Size, minimum length and R(L)=xL:

java -Xmx8g -jar HIME.jar [DATASET] [PAA] [MINIMUM LENGTH] [ALPHABET SIZE] [x]  > tmp.log


TO CONVERT LOGS TO RESULT DATA:

grep -i "Rule" tmp.log | cut -d' ' -f 2- > res.txt


OUTPUT FORMS:

[First Instance Start Location][First Instance End Location][Second Instance Start Location][Second Instance End Location][Length][Distance]

SOURCE CODE is available in https://github.com/flash121123/HIME


