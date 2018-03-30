all:

update:
	$(MAKE) -C ../src/web
	cp ../src/web/index.html ../src/web/hyper.css ../src/web/hyper.js .

ci:
	git ci . -m "Website."
	git push
