import torch
X = 0
Y = 1
H = 2
W = 3
X2 = 2
Y2 = 3


class BoxFormatError(Exception):
    pass

class BBoxes:
    def __init__(self, tensor,fmt):
        self.data = tensor.float()
        assert len(self.data.shape) == 2
        self.fmt = fmt

    def change_format(self, fmt):
        if fmt == self.fmt:
            return
        if fmt == "xyxy":
            self.fmt = "xyxy"
            bboxes = self.data.clone()
            bboxes[:, X] = (self.data[:, X] - self.data[:,W]/2.0)
            bboxes[:, Y] = (self.data[:, Y] - self.data[:, H]/ 2.0)
            bboxes[:, X2] = (bboxes[:, X] + self.data[:, W])
            bboxes[:, Y2] = (bboxes[:, Y] + self.data[:, H])
            self.data = bboxes
        elif fmt == "xyhw":
            self.fmt = "xyhw"
            bboxes = self.data.clone()
            bboxes[:, X] = (self.data[:, X] + self.data[:, X2]) / 2.0
            bboxes[:, Y] = (self.data[:, Y] + self.data[:, Y2]) / 2.0
            bboxes[:, W] = (self.data[:, X2] - self.data[:, X])
            bboxes[:, H] = (self.data[:, Y2] - self.data[:, Y])
            self.data = bboxes

    def __getattr__(self, item):
        if item == "change_format" or item == "data" or item == "fmt":
            return self.__dict__[item]
        else:
            return self.__dict__["data"].__getattribute__(item)

    def __setattr__(self, key, value):
        if key == "data" or key == "fmt":
            self.__dict__[key] = value
        else:
            self.data.__setattr__(key, value)

    def __getitem__(self, item):
        return self.data[item]

    def __str__(self):
        return str(self.data)

    def __add__(self, other):
        """
        coerces to format of self
        :param other:
        :return:
        """
        other.change_format(self.fmt)
        data_new = other.data + self.data
        return BBoxes(data_new, self.fmt)

    def __eq__(self, other):
        arg1 = self.fmt == other.fmt
        arg2 = self.data == other.data
        return arg1 and arg2.all()

def stack_bboxes(boxes):
    """
    stack a list of bboxes
    :param boxes:
    :return:
    """
    fmt = boxes[0].fmt
    data = []
    for box in boxes:
        if not fmt == box.fmt:
            raise BoxFormatError("not all boxes were the same format")
        data.append(box.data)
    data_final = torch.cat(data)
    return BBoxes(data_final, fmt)
