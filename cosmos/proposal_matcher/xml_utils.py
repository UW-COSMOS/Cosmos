from xml.etree import ElementTree as ET

def read_int(obj, name):
    obj = obj.find("bndbox")
    coord = obj.find(name).text
    return int(coord)

def parse_xml(xml_path):
    classes = []
    x1 = []
    y1 = []
    x2 = []
    y2 = []
    tree = ET.parse(xml_path)
    root = tree.getroot()
    for obj in root.findall("object"):
        classes.append(obj.find("name").text)
        x1.append(read_int(obj, "xmin"))
        y1.append(read_int(obj, "ymin"))
        y2.append(read_int(obj, "ymax"))
        x2.append(read_int(obj, "xmax"))
    return classes, x1,y1,x2, y2
