import cv2
import numpy as np
import matplotlib.pyplot as plt

COLORS = np.random.uniform(0, 255, size=(15, 3))

def visualize_prediction(detected_objs, classes):
    img_pth = detected_objs['img_path']
    image = cv2.imread(img_pth, flags=cv2.IMREAD_COLOR)
    dobjs = detected_objs['detected_objs']
    for obj in dobjs:
        bb, preds = obj
        startX, startY, endX, endY = [int(x) for x in bb]
        idx = classes.index(preds[0][1])
        pred = f'{preds[0][1]}: {preds[0][0]}'
        cv2.rectangle(image, (startX, startY), (endX, endY), COLORS[idx], thickness=2)
        y = startY - 15 if startY - 15 > 15 else startY + 15
        cv2.putText(image, pred, (startX, y),
                cv2.FONT_HERSHEY_SIMPLEX, 0.5, COLORS[idx], 2)
    return cv2.cvtColor(image, cv2.COLOR_BGR2RGB)


def visualize_region(img_path, bb):
    image = cv2.imread(img_path, flags=cv2.IMREAD_COLOR)
    startX, startY, endX, endY = [int(x) for x in bb]
    cv2.rectangle(image, (startX, startY), (endX, endY), COLORS[0], thickness=2)
    plt.figure(figsize=(20, 20))
    plt.axis('off')
    res = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    plt.imshow(res)
    plt.show()


def write_regions(img_path, bbs):
    image = cv2.imread(img_path, flags=cv2.IMREAD_COLOR)
    for bb in bbs:
        startX, startY, endX, endY = [int(x) for x in bb]
        cv2.rectangle(image, (startX, startY), (endX, endY), COLORS[0], thickness=2)
        cv2.putText(image, f'{startX}, {startY}, {endX}, {endY}', (startX, startY-10), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (255,0,0), 2)
    plt.figure(figsize=(20, 20))
    plt.axis('off')
    res = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    plt.imshow(res)
    plt.savefig(f'{img_path}.png')
    plt.close()







