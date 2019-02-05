from argparse import ArgumentParser
import pandas as pd
import numpy as np
from pascal_voc_writer import Writer
import yaml
from proposal_matcher.xml_utils import parse_xml

classes = yaml.load(open("classes.yaml"))["classes"]



def load_xml(xml_path):
    schema = ["class","x1","y1", "x2", "y2"]
    lst = parse_xml(xml_path)
    df = pd.DataFrame(dict(zip(schema, lst)))
    return df

def load_proposal(proposal_path):
    print(proposal_path)
    data = np.genfromtxt(proposal_path, delimiter=",", encoding='utf-8')
    N, _ = data.shape
    data_dict = {
        "x1": data[:, 0],
        "y1": data[:, 1],
        "x2": data[:, 2],
        "y2": data[:, 3]
        }
    for cls in classes:
        data_dict[cls] = np.zeros(N)
    df = pd.DataFrame(data_dict)
    return df

def get_ious(pred_df, box):
    X = 0
    Y = 1
    X2 = 2
    Y2 = 3
    xml_box = pred_df[["x1", "y1", "x2", "y2"]].values
    N, _ = xml_box.shape
    i_boxes = np.zeros((N,4))
    i_boxes[:, X] = np.maximum(xml_box[:, X].reshape(-1), box[X].reshape(-1))
    i_boxes[:, Y] = np.maximum(xml_box[:, Y].reshape(-1), box[ Y].reshape(-1))
    i_boxes[:, X2] = np.minimum(xml_box[:, X2].reshape(-1), box[X2].reshape(-1))
    i_boxes[:, Y2] = np.minimum(xml_box[:, Y2].reshape(-1), box[Y2].reshape(-1))
    i_area = (i_boxes[:, X2] - i_boxes[:, X]) * (i_boxes[:, Y2] - i_boxes[:, Y])
    i_area[i_boxes[:, X2] < i_boxes[:, X]] = 0
    i_area[i_boxes[:, Y2] < i_boxes[:, Y]] = 0
    xml_area = (xml_box[:, X2] - xml_box[:, X]) * (xml_box[:, Y2] - xml_box[:, Y])
    box_area = (box[X2] - box[X]) * (box[Y2] - box[Y])
    iou = i_area/(xml_area+ box_area - i_area)
    iou_df = pd.DataFrame({
        "class": pred_df["class"],
        "iou": iou
        })
    iou_df = iou_df.groupby("class").sum()
    # print(iou_df)
    return iou_df



def get_votes(pred_df, proposal_df):
    N, _ = pred_df.shape
    M, _ = proposal_df.shape
    for i,row in proposal_df.iterrows():
        box = np.array([row.x1, row.y1, row.x2, row.y2])
        iou_df = get_ious(pred_df, box)
        for key in iou_df.index:
            proposal_df.at[i, key] = iou_df.at[key,"iou"]
    return proposal_df


def get_winner(row):
    # select coords we care about
    select = row[4:]
    # print(select)
    winner = select.idxmax()
    if select[winner] == 0:
        return "Body Text"
    return winner

def write_matches(vote_df, out_path):
    vote_df["winner"] = vote_df.apply(get_winner, axis=1)
    # TODO if this matters we need to fill it in
    writer = Writer("n/a", 1920, 1920)
    for row in vote_df.itertuples():
        writer.addObject(row.winner, int(row.x1),
                int(row.y1),
                int(row.x2),
                int(row.y2))
    writer.save(out_path)


def process_doc(prediction_path, proposal_path, out_path):
    data_xml = load_xml(prediction_path)
    data_proposal = load_proposal(proposal_path)
    vote_df = get_votes(data_xml, data_proposal)
    write_matches(vote_df,out_path)

if __name__ == "__main__":
    parser = ArgumentParser("snap Mask RCNN predictions to connected components bounding boxes")
    parser.add_argument("prediction_path", type=str)
    parser.add_argument("proposal_path", type=str)
    parser.add_argument("out_path", type=str)
    args = parser.parse_args()
    process_doc(args.prediction_path, args.proposal_path, args.out_path)




