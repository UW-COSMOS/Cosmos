### Setup (with python3.6)
1. Clone this repo 
2. Download weights from https://drive.google.com/file/d/1nad6PhxSkGRiWM9WHdKjOrBAqu89xT4b/view?usp=sharing
3. Open `vocab.json` from the weight folder and change the value of `path_vocab` to [absolute-path-to-this-repo]/vocab.txt
4. pip (python3.6) install -r requirements.txt
   

### Usage Example

```
python3 [abs-path-to-this-repo]/im2latex.py --weight_dir [abs-path-to-the-weights-downloaded-above (should be ended with im2latex_weights_prod/)] --img_path [abs-path-to-the-img-of-equation]
```

### Usage Descriptions

```
Usage: im2latex.py [OPTIONS]

  Program that takes as input an image of equation and outputs a Latex code

Options:
  --downsample_image_ratio INTEGER
                                  Ratio to down sampling
  --cropping TEXT                 Crops the source image
  --padding TEXT                  Pads the source image
  --gray_scale TEXT               Gray scales the source image
  --weight_dir TEXT               Path to configuration folder under which
                                  there're vocab.json model.json model.weights
                                  [required]
  --img_path TEXT                 Path to source img  [required]
  --help                          Show this message and exit.
```
 
