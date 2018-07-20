.PHONY: data

data:
	mkdir -p data
	(cd data && wget https://icfpcontest2018.github.io/assets/problemsL.zip)
	(cd data && wget https://icfpcontest2018.github.io/assets/dfltTracesL.zip)
	(cd data && mkdir -p problems && unzip -d problems problemsL.zip)
	(cd data && mkdir -p default-traces && unzip -d default-traces dfltTracesL.zip)
