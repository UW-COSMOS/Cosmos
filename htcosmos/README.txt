--- submitting a uwcosmos document set as a HTCondor job ---

Files in this directory demonstrate how to submit a collection of PDF files to
HTCondor to be processed using the cosmos model producing PARQUET files and images.

There are two example submit files.

	parquet.sub
		process a directory PDF files from ./input or directory supplied on submit command line.
		Results are written to ./output and intermediate pickle files are written to ./pages
		The input, output and pages directory must be created before you submit the HTCondor
		job using the command

			condor_submit parquet.sub

		You can supply some options with the submit commmand on the command line:

			from=<full-path-to-input-directory>  # default is ./input
			propose=true                         # defaults to undefined/false
			disk=<request_disk-amount>           # defaults to 500MB
			memory=<request_memory-amount>       # defaults to 6GB
			cpus=<request_cpus-amount>           # defaults to 1

		To process all of the files in a given directory you can use the command

			condor_submit from=/path/to/input parquet.sub

		You can also split the processing up into two phases, a CPU only propose phase
		and a GPU processing and post-processing phase.  To do that, either use propose.sub
		or supply the propose=true option when you submit parquet.sub.
		When you pass propose=true, then the PDF files are just pre-processed, with PNG and pickle files
		being returned in the ./pages directory.  A job of this sort will be run without a GPU.  Once a
		propose=true job has finised,  you can submit parquet.sub again with the same inputs and it will
		transfer the PNG and pickle files to a machine with a GPU and finish the processing, placing
		the parquet files into ./output

	propose.sub
		The equivalent of 'condor_submit propose=true parquet.sub', but the ./pages and ./output directories
		do not need to be created before using this submit file.  This is the preferred way to do the
		page printing, pdf parsing and proposals.

	make_parquet.py
		A script intended to be run as part of a HTCondor job inside a uwcosmos/cosmos-ingestion docker container.
		It can process a pdf and make parquet files, or it can stop after the propose step. When the inputs
		include the pages and proposals, it will use those and go right to the GPU inference processing step.
		The above submit files use this script.  Some intermediate data can be returned in json format
		since this is more human-readable. stdout and stderr are used for progress logs. The stdout log
		lines have timestamps.

		Usage: python make_parquet.py <pdf-dir> <page_info_dir> <output_dir>
			where:
				<pdf-dir>       is the input directory, it should contain the PDF files to process
				<page_info_dir> is the pages directory, page PNGs and pickles are read and written here
				<output_dir>    is the output directory, parquet files are written here

			This script reads options from the environment
				MODEL_CONFIG=<path within the docker container>
				WEIGHTS_PTH=<path within the docker container>
				PP_WEIGHTS_PTH=<path within the docker container>
				AGGREGATIONS=<list of aggregations>
				KEEP_INFO=[pickle,page,pad,json] # intermediate files not in this list will not be returned
				JUST_PROPOSE=[True] # when present, only the page printing, pdf parse and propose steps are done




