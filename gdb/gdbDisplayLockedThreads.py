#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright © 2017 Damian Ziobro <damian@xmementoit.com>

"""
This script shows blocking C/C++ threads in gdb based on data from core file.

Author: Damian Ziobro <damian@xmementoit.com>

Instruction: 
1) Add this to your file 'gdbcommands' (or your .gdbinit file)
'
python 
import gdbDisplayLockedThreads
end

thread apply all bt

blocked
'
2. Run this command:
gdb -c core /path/to/exec -x gdbcommands -batch

3. You should see all backtraces from your process followd by additionally info of 
which threads are waiting for mutexes holding by which other threads ex.

Thread: 27184 waits for thread: 27185  AND DEADLOCKED
Thread: 27185 waits for thread: 27184  AND DEADLOCKED

It means that thread 27184 waits for mutex holded by thread 27185 AND thread 27185 
waits for mutex holded by thread 27184 - so threas 27185 and 27184 are deadlocked

"""

import gdb

class Thread():
    def __init__(self):
        self.frames = []
        self.waitOnThread = None
        self.threadId = None

    def __getitem__(self):
        return self.waitOnThread
        

class DisplayLockedThreads(gdb.Command):
    """custom command => blocked - command show how threads blocks themselves waiting on mutexes"""
    def __init__(self):
        super (DisplayLockedThreads, self).__init__("blocked", gdb.COMMAND_SUPPORT,gdb.COMPLETE_NONE,True)
        print (self.__doc__)

    def invoke(self, arg, from_tty):
        print ("\n********************************************************************************")
        print ("Displaying blocking threads using 'blocked' command")
        threads = {}
        for process in gdb.inferiors():
            print ('process=', process)
            pt = list(process.threads())
            print ('threads=',pt)
            for thread in pt:
                print ('thread=', thread)
                trd = Thread()
                print ('ptid=',thread.ptid)
                trd.threadId = thread.ptid[1] #[1] - threadId; [0] - process pid
                print ('1....')
                thread.switch()
                print ('2....')
                frame = gdb.selected_frame()
                while frame is not None:
                    print ('3....')
                    frame.select()
                    print ('4....')
                    print("   {0}".format(frame.name()))
                    if frame is None:
                        print('Frame is None')
                        continue
                    try:
                        if frame is not None and "pthread_mutex_lock" in frame.name():
                            print ('5....')
                            print(gdb.execute("print mutex.__data.__owner", to_string=True))
                            trd.waitOnThread = int(gdb.execute("print mutex.__data.__owner", to_string=True).split()[2])
                            #print(threads[-1].waitOnThread)
                    except:
                        print('Failed at ', frame.name())
                        break
                    print ('6....')
                    trd.frames.append(frame.name())
                    print ('7....')
                    frame = frame.older()
                print ('8....')
                threads[trd.threadId] = trd
                print('Done thread')

        for (tid,thread) in threads.items():
            if thread.waitOnThread:
                deadlockedText = "" if not threads[thread.waitOnThread].waitOnThread == thread.threadId else "AND DEADLOCKED"
                print ("Thread: {0} waits for thread: {1} {2}".format(thread.threadId, thread.waitOnThread, deadlockedText))
        print ("********************************************************************************")

DisplayLockedThreads()
