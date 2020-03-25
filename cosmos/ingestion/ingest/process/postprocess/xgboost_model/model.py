from argparse import ArgumentParser
import yaml 
import matplotlib.pyplot as plt
plt.style.use('ggplot')
import pandas as pd
import joblib
from xgboost import XGBClassifier
from sklearn.metrics import accuracy_score


class PostProcessTrainer:
    def __init__(self, model, train_set_x, train_set_y, val_set_x, val_set_y, classes, log_dir ='./log_dir', model_path='pp_model_weights.pth'):
        self.train_set_x = train_set_x
        self.train_set_y = train_set_y
        self.val_set_x = val_set_x
        self.val_set_y = val_set_y
        self.model = model
        self.model_path = model_path
        self.classes = classes

    def train(self):        
        self.model.fit(self.train_set_x, self.train_set_y)
        accuracy, classification_list = self.validate()
        print("Val Accuracy: {} \n".format(accuracy*100))
        print(classification_list)
        #run_evaluate(classification_list, classes)
        joblib.dump(self.model, self.model_path)
        
    def validate(self):
        classification_list = []
        correct = 0
        total = 0
        predicted = self.model.predict(self.val_set_x)
        correct = sum(predicted == self.val_set_y)
        total = len(predicted)
        for i in range(len(predicted)):
            classification_list.append((self.classes[predicted[i]], self.classes[self.val_set_y[i]]))     
        return correct/total, classification_list
        

def run_evaluate(classification_list, classes):
    print('Classes')
    print(classes)
    class_counts = {}
    for p in classification_list:
        p_cls, t_cls = p
        if p_cls not in class_counts:
            class_counts[p_cls] = {}
        if t_cls in class_counts[p_cls]:
            class_counts[p_cls][t_cls] += 1
        else:
            class_counts[p_cls][t_cls] = 1
    class_precisions = {}
    all_tp = 0
    all_denom = 0
    for p_cls in class_counts:
        tp = 0
        fp = 0
        for t_cls in class_counts[p_cls]:
            if p_cls == t_cls:
                tp = class_counts[p_cls][t_cls]
            else:
                fp += class_counts[p_cls][t_cls]
        denom = tp + fp
        all_tp += tp
        all_denom += denom
        class_precisions[p_cls] = tp / denom if denom != 0 else 'No false positives or true positives found'
    print('All class precision')
    all_precision = all_tp / all_denom
    print(all_precision)
    print('-----------------')
    all_tp = 0
    all_denom = 0
    class_recalls = {}
    print('DEBUG')
    for p_cls in class_counts:
        #print(class_counts)
        tp = class_counts[p_cls][p_cls] if p_cls in class_counts[p_cls] else 0
        fn = 0
        for p2_cls in class_counts:
            if p2_cls == p_cls:
                continue
            if p_cls in class_counts[p2_cls]:
                fn += class_counts[p2_cls][p_cls]
        denom = tp + fn
        all_tp += tp
        all_denom += denom
        class_recalls[p_cls] = tp / denom if denom != 0 else 'No false negatives or true positives found'

    print('All class recall')
    all_recall = all_tp / all_denom
    print(all_recall)
    print('--------------')

    print('All class F1')
    all_f1 = 2 * all_precision * all_recall / (all_precision + all_recall)
    print(all_f1)
    print('--------------')

    print('Class recalls')
    print(class_recalls)
    print('------------')
    print('Class precisions')
    print(class_precisions)
    print('------------')
    class_f1 = {}
    for cl in class_recalls:
        rec = class_recalls[cl]
        prec = class_precisions[cl]
        if type(rec) == str:
            print(f'Class: {cl}')
            print(rec)
            continue
        if rec + prec == 0:
            class_f1[cl] = 0
            continue
        class_f1[cl] = 2 * rec * prec / (rec + prec)
    print('Class F1s')
    print(class_f1)
    print('-------------')
    print('Class counts')
    print(class_counts)
    '''
    df = pd.DataFrame(class_counts)
    df = df.fillna(value=0)
    df['Total'] = df.sum(axis=1)
    print(df[sorted(df.columns)])
    print('------------')
    '''
    class_counts = {}
    for p in classification_list:
        p_cls, t_cls = p
        if t_cls not in class_counts:
            class_counts[t_cls] = {}
        if p_cls in class_counts[t_cls]:
            class_counts[t_cls][p_cls] += 1
        else:
            class_counts[t_cls][p_cls] = 1
    print("Target Class Counts")
    print(class_counts)
