from torchvision.transforms import Normalize

class NormalizeWrapper:
    """
    Custom class to normalize images from the 
    XML Loader class
    """
    def __init__(self, mean=[0.485, 0.456, 0.406],std=[0.229, 0.224, 0.225]):
        self.normalizer = Normalize(mean, std)

    def __call__(self, data):
        return self.normalizer(data)

