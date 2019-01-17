"""
read a given xml VOC annotation file
and return a list of object annotations
"""
import xml.etree.ElementTree as ET

ICDAR_convert = {
    'Figure Note': 'Figure',
    'Figure Caption': 'Figure',
    'Figure': 'Figure',
    'Table Note': 'Table',
    'Table Caption': 'Table',
    'Table': 'Table',
    'Body Text': 'Body Text',
    'Page Footer': 'Body Text',
    'Page Header': 'Body Text',
    'Equation': 'Equation',
    'Equation label': 'Equation',
    'Section Header': 'Body Text',
    'Abstract': 'Body Text',
    'Reference text': 'Body Text',
    'Other': 'Body Text'
}

similar_class_sets = [set(['Figure Note', 'Figure Caption', 'Table Note', 'Table Caption', 'Body Text', 'Page Footer', 'Page Header', 'Equation label', 'Section Header', 'Abstract', 'Reference text']), set(['Figure']), set(['Table']), set(['Equation']), set(['Other'])]

def load_from_file(path):
    """
    entry point for the application
    :param path: path to the xml file
    :return:
    """
    tree = ET.parse(path)
    root = tree.getroot()
    size_el = root.find("size")
    #width = size_el.find("width")
    #width = int(width.text)
    #height = size_el.find("height")
    #height = int(height.text)
    object_els = root.findall("object")
    objs = []
    for obj in object_els:
        bnd = obj.find("bndbox")
        coords = ["xmin", "ymin", "xmax", "ymax"]
        objs.append((obj.find("name").text ,[int(float(bnd.find(coord).text)) for coord in coords]))
    return Annotation(objs)


class Annotation:
    def __init__(self, size, objects):
        self.size = size
        # list of tuples (class, [xmin, ymin, xmax,  ymax])
        self.objects = objects

    def __init__(self, objects):
        self.size = None
        self.objects = objects

    def __str__(self):
        f = "Annnotation:\n" \
            "- Image size: {}\n".format(self.size)

        for name, coords in self.objects:
            f += "- {} at position {}\n".format(name, coords)
        return f

    def collapse_classes_icdar(self):
        new_objs = []
        for obj in self.objects:
            name, coords = obj
            new_name = ICDAR_convert[name]
            new_objs.append((new_name, coords))
        self.objects = new_objs


if __name__ == "__main__":
    annotation = load_from_file("data/voc_annotation.xml")
    print(annotation)
