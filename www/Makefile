bootstrap := bootstrap-3.1.1-dist

files := index.html tutorial.html

all: $(files)

%.html: %.md template.html
	pandoc $< -o $@ --smart --template template.html \
	  --css $(bootstrap)/css/bootstrap.css \
	  --css bootstrap-custom.css \
	  --self-contained --toc --toc-depth 2

clean:
	-rm -f $(files)
