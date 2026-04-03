APP            := digit
VERSION        := 4.0
BUILD          := build/release
PKGROOT        := $(APP)_$(VERSION)
UNAME_M        := $(shell uname -m)
LAZBUILD       := lazbuild
TARGETOS       := $(shell fpc -iTO)
TARGETCPU      := $(shell fpc -iTP)
BUILDDIR       := $(BUILD)/$(TARGETOS)-$(TARGETCPU)

# ----------------------------
# Architecture detection (Linux)
# ----------------------------
ifeq ($(UNAME_M),x86_64)
  DEB_ARCH := amd64
endif
ifeq ($(UNAME_M),i386)
  DEB_ARCH := i386
endif
ifeq ($(UNAME_M),i686)
  DEB_ARCH := i386
endif
ifeq ($(UNAME_M),armv7l)
  DEB_ARCH := armhf
endif
ifeq ($(UNAME_M),aarch64)
  DEB_ARCH := arm64
endif
DEB_ARCH ?= unknown

# ----------------------------
# macOS DMG settings
# ----------------------------
DMG_STAGING    := $(BUILD)/dmg-staging
DMG_VOLNAME    := $(APP) $(VERSION)
APP_BUNDLE_SRC := $(BUILDDIR)/$(APP).app
DMG_OUT        := $(APP)_$(VERSION)_$(TARGETCPU).dmg

# ----------------------------
# Targets
# ----------------------------
.PHONY: all clean build build_deb build_dmg

all: build

clean:
	rm -rf $(BUILD) $(PKGROOT) *.deb *.dmg

# ----------------------------
# Linux build (native)
# ----------------------------
build:
	cd src/ && $(LAZBUILD) $(APP).lpi --build-mode=Release

# ----------------------------
# Debian package
# ----------------------------
build_deb: clean build
	@echo "Building Debian package for $(DEB_ARCH)"
	mkdir -p $(PKGROOT)/DEBIAN
	mkdir -p $(PKGROOT)/usr/bin
	mkdir -p $(PKGROOT)/usr/share/applications
	mkdir -p $(PKGROOT)/usr/share/icons/hicolor/256x256/apps
	cp $(BUILDDIR)/$(APP) $(PKGROOT)/usr/bin/
	chmod 755 $(PKGROOT)/usr/bin/$(APP)
	sed "s/@ARCH@/$(DEB_ARCH)/" debian/control > $(PKGROOT)/DEBIAN/control
	cp debian/postinst $(PKGROOT)/DEBIAN/
	chmod 755 $(PKGROOT)/DEBIAN/postinst
	cp debian/$(APP).desktop $(PKGROOT)/usr/share/applications/
	# Icons (SVG primary + PNG fallback)
	mkdir -p $(PKGROOT)/usr/share/icons/hicolor/scalable/apps
	mkdir -p $(PKGROOT)/usr/share/icons/hicolor/256x256/apps
	mkdir -p $(PKGROOT)/usr/share/icons/hicolor/128x128/apps
	mkdir -p $(PKGROOT)/usr/share/icons/hicolor/64x64/apps
	cp icons/$(APP)/$(APP).svg \
		$(PKGROOT)/usr/share/icons/hicolor/scalable/apps/$(APP).svg
	cp icons/$(APP)/$(APP)_256.png \
		$(PKGROOT)/usr/share/icons/hicolor/256x256/apps/$(APP).png
	cp icons/$(APP)/$(APP)_128.png \
		$(PKGROOT)/usr/share/icons/hicolor/128x128/apps/$(APP).png
	cp icons/$(APP)/$(APP)_64.png \
		$(PKGROOT)/usr/share/icons/hicolor/64x64/apps/$(APP).png
	fakeroot dpkg-deb --build $(PKGROOT) $(APP)_$(VERSION)_$(DEB_ARCH).deb

# ----------------------------
# macOS DMG
# ----------------------------
build_dmg: clean build
	@echo "Building macOS DMG for $(TARGETCPU)"
	@test -d "$(APP_BUNDLE_SRC)" || \
		{ echo "ERROR: .app bundle not found at $(APP_BUNDLE_SRC)"; exit 1; }
	rm -rf "$(DMG_STAGING)"
	mkdir -p "$(DMG_STAGING)"
	# Copy the .app into the staging dir, resolving every symlink to its real
	# file so the bundle is fully self-contained (-R recursive, -L dereference).
	cp -RL "$(APP_BUNDLE_SRC)" "$(DMG_STAGING)/"
	# Verify the main executable is a plain file (not a dangling link)
	@test -f "$(DMG_STAGING)/$(APP).app/Contents/MacOS/$(APP)" || \
		{ echo "ERROR: executable missing or still a symlink after copy"; exit 1; }
	# Build a compressed, internet-ready DMG
	hdiutil create \
		-volname "$(DMG_VOLNAME)" \
		-srcfolder "$(DMG_STAGING)" \
		-ov \
		-format UDZO \
		"$(DMG_OUT)"
	rm -rf "$(DMG_STAGING)"
	@echo "Done: $(DMG_OUT)"
