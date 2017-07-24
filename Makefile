emacs := /Applications/Emacs.app/Contents/MacOS/Emacs
export emacs

install:
	~/.cask/bin/cask install

update:
	~/.cask/bin/cask update
