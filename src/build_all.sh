#!/bin/sh

lazbuild --operating-system=linux --cpu=x86_64 --widgetset=gtk2 digit.lpi --build-mode=Release
lazbuild --operating-system=win32 --cpu=i386 --widgetset=win32 digit.lpi --build-mode=Release
lazbuild --operating-system=win64 --cpu=x86_64 --widgetset=win32 digit.lpi --build-mode=Release
