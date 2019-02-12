from variable_extractor import get_latex_variables_api

if __name__ == '__main__':
    # Setup
    # 1. install dependencies in latex_parser/requirements.txt
    print(get_latex_variables_api('\\frac{-b\pm\sqrt{b^2-4ac}}{2a}'))
