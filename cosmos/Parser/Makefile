#contains only the html source files and nothing else
input_folder = data/html/files/

#intermediate folder location (will be auto-generated)
output_html = out/html/
merge_folder = data/html/merged/
output_words = out/words/
output_equation = out/equations/

all_inputs = $(shell ls $(merge_folder))
db_connect_str = postgres://postgres:password@localhost:5432/cosmos8

.SECONDARY: preprocess.stamp parse.stamp link.stamp
.PHONY: install-linux install-mac

# 4. run the link file to insert coordinate information into fonduer based on the information from the json output folder (aka. hocr)
link.stamp: parse.stamp link.py
	python link.py --words_location $(output_words) --database $(db_connect_str)
	@touch link.stamp

# 3. run the fonduer parser on the generated html file. This will fill in the postgres dabase with everything
# fonduer can understand except the coordinate information.
parse.stamp: preprocess.stamp parse.py
	python parse.py --html_location $(output_html) --database $(db_connect_str)
	@touch parse.stamp

# 2. preprocess the input html and store intermediate json and html in the output folder declared above.
preprocess.stamp: parse_preprocess.py merge.stamp
	rm -r -f $(output_html)
	rm -r -f $(output_words)
	rm -r -f $(output_equation)
	mkdir -p $(output_html)
	mkdir -p $(output_words)
	mkdir -p $(output_equation)
	@$(foreach file,$(all_inputs),\
	python parse_preprocess.py --input $(merge_folder)$(file) --output_words $(output_words)$(file).json --output_html $(output_html)$(file) --output_equation $(output_equation)$(file).json;)
	@touch preprocess.stamp

# 1. group files by file name
merge.stamp: pagemerger.py
	rm -r -f $(merge_folder)
	mkdir -p $(merge_folder)
	python pagemerger.py --rawfolder $(input_folder) --outputfolder $(merge_folder)
	@touch merge.stamp


install-linux:
	sudo apt update
	sudo apt install libxml2-dev libxslt-dev python3-dev
	sudo apt build-dep python-matplotlib
	sudo apt install poppler-utils
	sudo apt install postgresql
	pip install -r requirements.txt

install-mac:
	brew install poppler
	brew install postgresql
	brew install libpng freetype pkg-config
	pip install -r requirements.txt

clean:
	rm -f merge.stamp
	rm -f preprocess.stamp
	rm -f parse.stamp
	rm -f link.stamp
	rm -r -f $(output_html)
	rm -r -f $(output_words)
	rm -r -f $(merge_folder)


