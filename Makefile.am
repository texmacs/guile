# Automake requirements:
AUTOMAKE_OPTIONS = 1.7 gnu

SUBDIRS = . doc lightning opcode tests
EXTRA_DIST = config/texi2dvi config/help2man

dist_noinst_HEADERS = lightning-inst.h
nodist_noinst_HEADERS = lightning.h

uninstall-local:
	-rm -f $(DESTDIR)$(includedir)/lightning.h

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) $(srcdir)/lightning-inst.h $(DESTDIR)$(includedir)/lightning.h

aclocaldir = $(datadir)/aclocal
dist_aclocal_DATA = lightning.m4
bin_SCRIPTS = lightningize
