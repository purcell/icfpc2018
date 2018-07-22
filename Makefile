.PHONY: data

data:
	mkdir -p data
	(cd data && wget https://icfpcontest2018.github.io/assets/problemsL.zip)
	(cd data && wget https://icfpcontest2018.github.io/assets/dfltTracesL.zip)
	(cd data && mkdir -p problemsL && unzip -d problemsL problemsL.zip)
	(cd data && mkdir -p dfltTracesL && unzip -d dfltTracesL dfltTracesL.zip)
	(cd data && wget https://icfpcontest2018.github.io/assets/problemsF.zip)
	(cd data && wget https://icfpcontest2018.github.io/assets/dfltTracesF.zip)
	(cd data && mkdir -p problemsF && unzip -d problemsF problemsF.zip)
	(cd data && mkdir -p dfltTracesF && unzip -d dfltTracesF dfltTracesF.zip)




