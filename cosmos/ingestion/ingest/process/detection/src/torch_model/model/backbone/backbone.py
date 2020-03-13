"""
Model
"""
import torch
import torch.nn as nn
import torchvision.models as models


def get_backbone(m='resnet50', pretrained=False):
    if m == 'resnet50':
        return get_resnet(50)
    if m == 'resnet101':
        return get_resnet(101)
    if m == 'pyramid':
        return FeaturePyramidResNet()
    else:
        raise Exception('Invalid backbone model passed.')


def conv3x3(in_planes, out_planes, stride=1):
    """3x3 convolution with padding"""
    return nn.Conv2d(in_planes, out_planes, kernel_size=3, stride=stride,
                     padding=1, bias=False)


def conv1x1(in_planes, out_planes, stride=1):
    """1x1 convolution"""
    return nn.Conv2d(in_planes, out_planes, kernel_size=1, stride=stride, bias=False)


class FeaturePyramidResNet(nn.Module):

    def __init__(self, resnet='resnet50'):
        super(FeaturePyramidResNet, self).__init__()
        self.resnet = get_backbone(m=resnet)

    def forward(self, x, feature_dimension=256):
        i = 1
        # Collection of last stage results
        cs = []
        # Collection of feature dimensions
        inlayers = []
        for layer in self.resnet.children():
            if isinstance(layer, nn.Sequential):
                # Skip the first stage
                if i != 1:
                    for ind, res_layer in layer.children():
                        x = res_layer(x)
                        if ind == len(layer.children()) - 1:
                            inlayers.append(feature_dimension)
                            cs.append(x)
                            feature_dimension = feature_dimension * res_layer.expansion
                else:
                    x = layer(x)
                i += 1
            else:
                x = layer(x)

        ps = []
        inlayers = reversed(inlayers)
        for ind, c in enumerate(reversed(cs)):
            if len(ps) == 0:
                conv = conv1x1(inlayers[ind], feature_dimension)
                result = conv(c)
                ps.append(result)
            else:
                last_p_layer = ps[-1]
                # Upsample using NN
                upsample = nn.modules.upsampling.UpsamplingNearest2d(scale_factor=2)
                p_upsample = upsample(last_p_layer)
                conv = conv1x1(inlayers[ind], feature_dimension)
                result = conv(c)
                ps.append(result + p_upsample)
        final_result = torch.Tensor(ps)
        return final_result


def get_resnet(nlayers=50):
    if nlayers == 50:
        base = models.resnet50(pretrained=False)
    elif nlayers == 101:
        base = models.resnet101(pretrained=False)
    else:
        raise NotImplementedError()
    # now build from the base a sequential object with the subpieces of the network
    backbone = nn.Sequential(*list(base.children())[:-3])
    backbone.requires_grad = False
    backbone = nn.Sequential(backbone, *list(base.children())[-3:-2])
    return backbone

if __name__ == '__main__':
    pass

