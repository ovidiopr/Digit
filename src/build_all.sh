#!/bin/sh

lazbuild --operating-system=linux --cpu=x86_64 --widgetset=gtk2 Digit.lpi
lazbuild --operating-system=win32 --cpu=i386 --widgetset=win32 Digit.lpi
lazbuild --operating-system=win64 --cpu=x86_64 --widgetset=win32 Digit.lpi
