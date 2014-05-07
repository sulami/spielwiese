#!/usr/bin/env python3

"""
This takes an directory as argument and plays all music in this
directory (and subdirectories) using mplayer.

Future features:
 - automatic restart
 - keeping a sqlite-db for music
 - generating stats about music played based around various stats
 - ranking the music to play by those stats
"""

import os
import subprocess
import sys
from getch import getch

FILETYPES = ('flac', 'ogg', 'mp3', 'wav')

class Song:
    def __init__(self, path):
        self.name = os.path.split(path)[1]
        self.path = path

    def play(self):
        print('Playing %s' % self.name)
        self.process = subprocess.Popen(
            ['mplayer', '-really-quiet', self.path],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            stdin=subprocess.DEVNULL
        )

    def skip(self):
        self.process.kill()

def generateSongList(path):
    print('Generating songlist from %s...' % path)
    songlist = []
    for dirname, dirnames, filenames in os.walk(path):
        for filename in filenames:
            filetype = filename.split('.')[-1]
            if filetype in FILETYPES:
                song = Song(os.path.join(dirname, filename))
                songlist.append(song)
    print('Collected %i songs, starting playback...' % len(songlist))
    return songlist

def main():
    if len(sys.argv) > 1:
        path = sys.argv[-1]
        if not os.path.exists(path):
            path = os.getcwd()
    else:
        path = os.getcwd()
    songlist = generateSongList(path)
    for song in songlist:
        song.play()
        while True:
            inkey = getch()
            if song.process.poll() or inkey == 's':
                song.skip()
                break;
            elif inkey == 'q':
                song.skip()
                sys.exit(0)
            elif inkey == 'h':
                print('(h)elp, (s)kip, (q)uit')
    print('Everything played, exiting...')

if __name__ == '__main__':
    main()

