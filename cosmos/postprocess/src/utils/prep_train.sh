wget http://www.icst.pku.edu.cn/cpdp/ICDAR2017_PODCompetition/data/Train.zip Train.zip
echo "ICDAR_POD2017_CPDP_TRAIN is the password"
unzip Train.zip
python pvoc.py Train VOC .8 1920
