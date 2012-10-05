BASENAME := index
DESTDIR := html/

MAIN  := Main.lhs
DEPS  := HelloWorld.lhs RouteFilters.lhs Templates.lhs RqData.lhs FileServing.lhs Reform.markdown.lhs WebRoutes.lhs AcidState.lhs TemplateHaskell.lhs
HELLOWORLD_DEMOS   := HelloWorld.lhs
ROUTE_FILTER_DEMOS := MonadPlus.lhs Dir.lhs Dir2.lhs Dirs.lhs Path.lhs FromReqURI.lhs MethodM.lhs MatchMethod.lhs
RQDATA_DEPS        := RqDataLimiting.lhs RqDataParsing.lhs Cookies.lhs
RQDATA_DEMOS       := HelloRqData.lhs RqDataPost.lhs RqDataError.lhs RqDataUpload.lhs RqDataRead.lhs RqDataCheck.lhs RqDataCheckOther.lhs  RqDataOptional.lhs
COOKIE_DEPS        := CookieLife.lhs CookieIssues.lhs CookieFeatures.lhs
COOKIE_DEMOS       := CookieCounter.lhs
FILESERVING_DEMOS  := FileServingDirectory.lhs FileServingSingle.lhs
REFORM_DEMOS       :=
ACIDSTATE_DEMOS    := AcidStateCounter.lhs IxSet.lhs IxSetDataLens.lhs AcidStateAdvanced.lhs
WEBROUTES_DEMOS    := WebRoutesDemo.lhs WebRoutesBoomerang.lhs WebRoutesHSP.lhs
TEMPLATES_DEPS     :=
TEMPLATES_DEMOS    := HelloBlaze.lhs TemplatesHeist.lhs TemplatesHSP.lhs JMacro.lhs TemplatesHSPI18n.lhs
DEMOS := $(RQDATA_DEMOS) $(ROUTE_FILTER_DEMOS) $(HELLOWORLD_DEMOS) $(TEMPLATES_DEPS) $(TEMPLATES_DEMOS) $(COOKIE_DEMOS) $(FILESERVING_DEMOS) $(REFORM_DEMOS) $(ACIDSTATE_DEMOS) $(WEBROUTES_DEMOS)
EXTRA_DEPS         := Makefile
SRC := $(MAIN) $(DEPS) $(ROUTE_FILTER_DEPS) $(DEMOS)
CSS := blog.css hscolour.css

all: $(DESTDIR) $(subst .lhs,.hs,$(addprefix $(DESTDIR),$(DEMOS))) $(addprefix $(DESTDIR),$(CSS)) $(subst .lhs,.html,$(subst .markdown.lhs,.html,$(addprefix $(DESTDIR),$(DEPS)))) $(DESTDIR)$(BASENAME).html html/theme/theme.css html/theme/code-background.jpg html/factorial.tpl $(DESTDIR)Reform.html $(DESTDIR)Reform.hs Reform.markdown.lhs


RouteFilters.html: $(ROUTE_FILTER_DEPS)
RqData.html: $(RQDATA_DEPS) $(RQDATA_DEMOS) $(COOKIE_DEPS) $(COOKIE_DEMOS)
Templates.html : $(TEMPLATES_DEPS) $(TEMPLATES_DEMOS) factorial.tpl-inc html/messages.zip JMacro.lhs
FileServing.html : FileServing.lhs $(FILESERVING_DEMOS) FileServingAdvanced.lhs
WebRoutes.html : $(WEBROUTES_DEMOS)
AcidState.html : AcidState.lhs $(ACIDSTATE_DEMOS)
Reform.html    : Reform.markdown.lhs
TemplateHaskell.html : TemplateHaskell.lhs

html/messages.zip: messages/standard/en.msg messages/standard/en-GB.msg messages/standard/jbo.msg messages/thing/en.msg
	zip $@  $^

%.html : %.markdown.lhs header.tpl footer.tpl
	cat header.tpl > $@
	cpphs --noline -DHsColour $< | sed 's/^] /> /' | ./HsColour -css -partial -lit | markdown --html4tags >> $@
	cat footer.tpl >> $@
	validate -w --verbose --emacs $@

%.lhs : %.markdown.lhs
	cpphs --noline -DHsColour $< | sed 's/^] /> /' | ./HsColour -css -partial -lit | markdown --html4tags > $@

html/theme/theme.css: theme.css
	mkdir -p html/theme
	cp $^ $@

html/theme/code-background.jpg: code-background.jpg
	mkdir -p html/theme
	cp $^ $@

html/factorial.tpl: factorial.tpl
	mkdir -p html
	cp $^ $@

%.tpl-inc : %.tpl escape-html.hs
	runhaskell escape-html.hs $< $@

toc.html: $(subst .lhs,.html,$(subst .markdown.lhs,.html,$(DEPS))) gen-toc.hs
	runhaskell gen-toc.hs  $^ > toc.html

$(DESTDIR):
	mkdir -p $(DESTDIR)

$(DESTDIR)$(BASENAME).html: Main.html
	#tidy -q -o $@ Main.html
	cp $< $@
	@echo ""
	@echo "********************************************************************************************"
	@echo "Open the following file in your browser, or run 'make open' to automatically open in firefox"
	@(echo file://$$(realpath $@))
	@echo "********************************************************************************************"

$(DESTDIR)%: %
	cp $^ $@

# open the .html file in firefox
# probably will not work if path contains spaces, etc
open: $(BASENAME).html
	(firefox -new-tab "file://$$(realpath $<)")

Main.cpphs : $(SRC) $(EXTRA_DEPS) toc.html

EXTRA := template/App.cabal template/Setup.hs

template/% : extra/%
	cp $^ $@

TEMPLATE_FILES := $(subst .lhs,.hs,$(addprefix template/,$(SRC))) $(EXTRA)
template: $(TEMPLATE_FILES)

template/%.hs: %.lhs
	(mkdir -p `dirname $@`)
	cpp -DExtract -traditional-cpp -P $^ | grep -v '{-# OPTIONS_GHC -cpp #-}' | sed -n 's/^> \?//p' > $@

template/%: %
	cp $^ $@

test:
	runhaskell -cpp -ipages Main.lhs

%.html: %.cpphs
	./HsColour -css -partial $< -o$@ -lit
	validate -w --verbose --emacs $@


%.cpphs: %.lhs
#	cpp -traditional-cpp -DHsColour -P $< $@
	cpphs --noline -DHsColour $< -O$@

$(DESTDIR)%.hs: %.markdown.lhs
	cpphs --noline $^ | grep -v '{-# LANGUAGE CPP #-}' | sed -n 's/^> \?//p' > $@

# extract a demo
$(DESTDIR)%.hs: %.lhs
	cpphs --noline $^ | grep -v '{-# LANGUAGE CPP #-}' | sed -n 's/^> \?//p' > $@
#	cpp -DExtract -traditional-cpp -P $^ | grep -v '{-# OPTIONS_GHC -cpp #-}' | sed -n 's/^> \?//p' > $@



clean:
	-rm -f $(BASENAME).html Main.html
	-rm -f $(TEMPLATE_FILES)
	(if test -d template/pages ; then rmdir template/pages ; fi)
	(if test -d template ; then rmdir template ; fi)

test-site: all
	rsync -avxz --exclude '*.o' --exclude '*.hi' html/ jeremy@happstack.com:public_html/happstack-crashcourse/

%.demo : $(DESTDIR)%.hs
	ghc --make -c -o$@ $<

check-demos: $(subst .lhs,.demo,$(DEMOS))

.PHONY: test all clean check-demos
