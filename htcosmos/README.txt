--- submitting a uwcosmos document set as a HTCondor job ---

Files in this directory demonstrate how to submit a collection of PDF files to
HTCondor to be processed using the COSMOS model producing PARQUET files and images.

There are four example submit files, but the typical workflow will use only two of them. The files are

	parquet.sub
		process a directory PDF files from ./input or directory supplied on submit command line.
		Results are written to ./output and intermediate pickle files are written to ./pages
		The input, output and pages directory must be created before you submit the HTCondor
		job using the command

			condor_submit parquet.sub

		You can supply some options with the submit commmand on the command line:

			from=<full-path-to-input-directory>  # default is ./input
			propose=true                         # defaults to undefined/false
			infer=true                           # defaults to undefined/false, ignored if propose=true
			post=true                            # defaults to undefined/false, ignored if propose=true or infer=true
			disk=<request_disk-amount>           # defaults to 500MB
			memory=<request_memory-amount>       # defaults to 6GB
			cpus=<request_cpus-amount>           # defaults to 1

		To process all of the files in a given directory you can use the command

			condor_submit from=/path/to/input parquet.sub

		You can also split the processing up into two or three phases. Two phase is a CPU only propose phase
		and a GPU processing and post-processing phase. This is the recommended workflow.  For this workflow,
		either use propose.sub or pass propose=true on the command line when you submit parquet.sub.
		When you pass propose=true or use propose.sub, the PDF files are just pre-processed, with PNG and pickle files
		being returned in the ./pages directory.  A job of this sort will be run without a GPU.  Once a
		propose=true job has finised,  you can submit parquet.sub again with the same inputs and it will
		transfer the PNG and pickle files to a machine with a GPU and finish the processing, placing
		the parquet files into ./output

	propose.sub
		The equivalent of 'condor_submit propose=true parquet.sub', but the ./pages and ./output directories
		do not need to be created before using this submit file.  This is the preferred way to do the
		page printing, pdf parsing and proposals.

	infer.sub
		The equivalent of 'condor_submit infer=true parquet.sub'. Use this as the second (GPU inference model)
		phase of a 3 phase workflow.  Note that the 2 phase workflow is probably better...

	post.sub
		The equivalent of 'condor_submit post=true parquet.sub'. Use this as the third (aggregation) phase of a
		3 phase workflow.  Note that the 2 phase workflow is probably better...

	make_parquet.py
		A script intended to be run as part of a HTCondor job inside a uwcosmos/cosmos-ingestion docker container.
		It can process a pdf and make parquet files, or it can stop after the propose step or after the infer step.
		When the inputs include the pages and proposals, it will use those and go right to the GPU inference processing step.
		When the inputs include 'detected_objs'.  it will skip GPU inferencing and just do the post-processing steps.

		The above submit files use this script.  Some intermediate data can optionally be returned in json format
		since this is more human-readable. stdout and stderr are used for progress logs. The stdout log
		lines have timestamps.

		Usage: python make_parquet.py <pdf-dir> <page_info_dir> <output_dir>
			where:
				<pdf-dir>       is the input directory, it should contain the PDF files to process
				<page_info_dir> is the pages directory, page PNGs and pickles and progress files are read and written here
				<output_dir>    is the output directory, parquet files are written here

			This script reads options from the environment
				MODEL_CONFIG=<path within the docker container>
				WEIGHTS_PTH=<path within the docker container>
				PP_WEIGHTS_PTH=<path within the docker container>
				AGGREGATIONS=<list of aggregations>
				KEEP_INFO=[pickle,page,pad,json] # intermediate files not in this list will not be returned
				JUST_PROPOSE=[True] # when present, only the page printing, pdf parse and propose steps are done
				SKIP_AGGREGATION=[True] # when present, only do GPU inference model step
				JUST_AGGREGATION=[True]	# when present, only do the post-processing aggregation and parquet creation step

			Only one of the JUST_PROPOSE, SKIP_AGREGATION, and JUST_AGGREGATION options can be used for a single job.

	times
		A quick and dirty shell script to scan the stdout of make_parquet.py (i.e. the <clusterid>.out of the HTCondor job)
		and sum up the times represented by similar lines. Uses the processing_time.pl perl script

	processing_time.pl
		A helper perl script used by the times shell script

If the size if the input PDF files is more than 25MB,  you should increase the requested disk for the HTCondor jobs
by adding disk=<value> to the submit command line. Disk usage is typically 30x the size of the inputs for intermediate files.
The final parquet files and images are roughly the same size as the input PDF files.

The recommended workflow is to create a directory of PDF files to process (/path/to/input)
then run the two job workflow.

	condor_submit from=/path/to/input propose.sub && condor_wait -echo propose.log
	condor_submit from=/path/to/input parquet.sub && condor_wait -echo parquet.log

There is an alternative workflow using a single job.  This workflow does all of the work
on a machine with a GPU, it is overall faster, but is wasteful of GPU resources.

	mkdir pages
	mkdir output
	condor_submit from=/path/to/input parquet.sub && condor_wait -echo parquet.log

There is another alternative workflow using three jobs, this workflow uses a GPU only
for the second job, the first and last are CPU only.  This is the slowest workflow
but makes the most efficient use of GPU resources.

	condor_submit from=/path/to/input propose.sub && condor_wait -echo propose.log
	condor_submit from=/path/to/input infer.sub && condor_wait -echo infer.log
	condor_submit from=/path/to/input post.sub && condor_wait -echo post.log



