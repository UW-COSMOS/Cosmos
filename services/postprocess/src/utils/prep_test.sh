wget http://www.icst.pku.edu.cn/cpdp/ICDAR2017_PODCompetition/data/Test.zip Test.zip 
echo "ICDAR_POD2017_CPDP_TEST is the password"
unzip Test.zip
python pvoc.py Test VOC_test 1 1920
mv VOC_test/ImageSets/Main/train.text VOC_test/Main/test.txt
