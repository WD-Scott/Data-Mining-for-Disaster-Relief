.PHONY: report setup clean

## Render the analysis notebook to Report.html
report:
	Rscript -e 'rmarkdown::render("scripts/analysis.Rmd", output_file = "../Report.html")'

## Install all dependencies via renv
setup:
	Rscript -e 'renv::restore()'

## Remove rendered output and cached files
clean:
	rm -f Report.html
	rm -rf scripts/analysis_cache scripts/analysis_files
