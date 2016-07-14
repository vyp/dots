# Not a Makefile for everything here, but only for certain programs that require
# a simple build system for their configs. :| See </themes/readme.md>.

ZATHURA=zathura
ZATHURARC=$(ZATHURA)/.config/zathura/zathurarc
CONFIGS = $(ZATHURARC)

.PHONY: all $(ZATHURA) clean

all: $(ZATHURA)

$(ZATHURA): $(ZATHURARC)

$(ZATHURARC): $(ZATHURA)/.config/zathura/rc ~/.config/zathura/colors
	cat $(ZATHURA)/.config/zathura/rc ~/.config/zathura/colors > $(ZATHURARC)

clean:
	@$(foreach rc,$(CONFIGS),rm -v $(rc);)
