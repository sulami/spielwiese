#!/usr/bin/python
# -*- coding: utf-8 -*-
 
print '--- Prime ---'
 
def test(zahl):
        zahl = int(zahl)
        wahr = 0
        for i in xrange(2, zahl, 1):
                if wahr == 0:
                        if zahl % i == 0:
                                wahr = 1
                                loes = i
        if wahr == 0:
                print zahl, 'ist eine Primzahl'
 
 
ziel = int(raw_input('Alle Primzahlen berechnen bis (0 = Endlos): '))
if ziel == 0:
        i = 1
        while ziel == 0:
                test(i)
                i = i + 1
else:
        for i in xrange(1, ziel, 1):
                test(i)
