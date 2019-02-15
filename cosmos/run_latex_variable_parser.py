from latex_parser.variable_extractor import get_latex_variables

if __name__ == '__main__':
    # Setup
    # 1. install dependencies in latex_parser/requirements.txt
    print(get_latex_variables('\\frac{-b\pm\sqrt{b^2-4ac}}{2a}'))
