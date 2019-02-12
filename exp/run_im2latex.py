from latex_ocr.img2latex import img2latex_api

if __name__ == '__main__':

    # Download weights from https://drive.google.com/file/d/1nad6PhxSkGRiWM9WHdKjOrBAqu89xT4b/view?usp=sharing
    # --weight_dir [abs - path - to - the - weights - downloaded - above(should be ended with im2latex_weights_prod /)]
    weight_dir = '/home/paulluh/im2latex_weights_prod/'

    # Configuration steps
    # 1. Open vocab.json from the weight folder and change the value of path_vocab to [absolute-path-to-latex_ocr]/vocab.txt
    # 2. pip install latex_ocr/requirements


    # --img_path[abs-path-to-the-img-of-equation]
    img_path = '/home/paulluh/test.png'

    downsample_image_ratio = 2
    cropping = True
    padding = True
    gray_scale = True

    tex = img2latex_api(weight_dir, img_path, downsample_image_ratio, cropping, padding, gray_scale)
    print(tex)
