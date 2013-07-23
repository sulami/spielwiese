#!/usr/bin/python
# -*- coding: utf-8 -*-
 
print '--- Der große Primzahltest ---'
zahl = int(raw_input('Bitte eine Zahl eingeben: '))
wahr = 0
for i in xrange(2, zahl, 1):
        if wahr == 0:
                if zahl % i == 0:
                        wahr = 1
                        loes = i
if wahr == 1:
        print 'Die Lösung ist:', loes, 'x', zahl / loes
else:
        print zahl, 'ist eine Primzahl'
