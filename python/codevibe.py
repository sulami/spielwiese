#!/usr/bin/env python3

"""
This takes an directory as argument and plays all music in this
directory (and subdirectories) using a mediaplayer of your choice (per
default mplayer).

Future features:
 - automatic restart
 - keeping a sqlite-db for music
 - generating stats about music played based around various stats
 - ranking the music to play by those stats

 Written by sulami (peerwire.de). Published under CC BY-SA 4.0
 http://creativecommons.org/licenses/by-sa/4.0/
"""

import os
import subprocess
import sys
from getch import getch

PLAYER = 'mplayer' # options: mplayer, cvlc, ...
FILETYPES = ('flac', 'ogg', 'mp3', 'wav')

class Player:
    def generate_song_list(self, path):
        print('Generating songlist from %s...' % path)
        self.songlist = []
        for dirname, dirnames, filenames in os.walk(path):
            for filename in filenames:
                filetype = filename.split('.')[-1]
                if filetype in FILETYPES:
                    song = Song(os.path.join(dirname, filename))
                    self.songlist.append(song)
        print('Collected %i songs, starting playback...' % len(self.songlist))

    def play(self):
        while True:
            for song in self.songlist:
                song.play()
                while song.is_running():
                    inkey = getch()
                    if inkey == 's':
                        song.skip()
                        break;
                    elif inkey == 'q':
                        song.skip()
                        sys.exit(0)
                    elif inkey == 'h':
                        print('(h)elp, (s)kip, (q)uit')

class Song:
    def __init__(self, path):
        self.name = os.path.split(path)[1]
        self.path = path

    def play(self):
        print('Playing %s' % self.name)
        self.process = subprocess.Popen(
            [PLAYER, self.path],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            stdin=subprocess.DEVNULL
        )

    def is_running(self):
        if self.process.pid is not None:
            return True
        else:
            return False

    def skip(self):
        try:
            self.process.kill()
        except:
            pass

def main():
    if len(sys.argv) > 1:
        path = sys.argv[-1]
        if not os.path.exists(path):
            path = os.getcwd()
    else:
        path = os.getcwd()
    mp = Player()
    mp.generate_song_list(path)
    mp.play()

if __name__ == '__main__':
    main()

