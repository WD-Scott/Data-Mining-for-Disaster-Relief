.PHONY: report setup clean

## Render the analysis notebook to Report.pdf
report:
	Rscript -e 'rmarkdown::render("scripts/analysis.Rmd", output_file = "../Report.pdf")'

## Install all dependencies via renv
setup:
	Rscript -e 'renv::restore()'

## Remove rendered output and cached files
clean:
	rm -f Report.pdf
	rm -rf scripts/analysis_cache scripts/analysis_files
