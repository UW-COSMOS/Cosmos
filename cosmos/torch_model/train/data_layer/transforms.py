from torchvision.transforms import Normalize

class NormalizeWrapper:
    """
    Custom class to normalize images from the 
    XML Loader class
    """
    def __init__(self, mean, std):
        self.normalizer = Normalize(mean, std)

    def __call__(self, data):
        return self.normalizer(data)

