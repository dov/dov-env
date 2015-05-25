define printqstring
    printf "(QString)0x%x (length=%i): \"",&$arg0,$arg0.d->size
    set $i=0
    while $i < $arg0.d->size
        set $c=$arg0.d->data[$i]
        if $c < 32 || $c > 127
                printf "\\u0x%04x", $c
        else
                printf "%c", (char)$c
        end
        set $i=$i+1             
    end
    printf "\"\n"
end

define printqdstring
    printf "(QString) (length=%i): \"",$arg0->size
    set $i=0
    while $i < $arg0->size
        set $c=$arg0->data[$i]
        if $c < 32 || $c > 127
                printf "\\u0x%04x", $c
        else
                printf "%c", (char)$c
        end
        set $i=$i+1             
    end
    printf "\"\n"
end

define pyqt5-env
    set env PATH=/usr/local/pyqt5/bin:/usr/local/bin:/usr/java/jre1.5.0_06/bin:/usr/X11R6/bin:/home/dov/scripts:/home/dov/Scripts:/home/dov/scripts:/home/dov/bin:/usr/X11R6/bin:/usr/bin:/bin:/usr/sbin:/sbin
    set env PYTHONPATH=/usr/local/pyqt5/lib/python2.7/site-packages:/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages
end

define pyqt4-env
    set env PATH=/usr/java/jre1.5.0_06/bin:/usr/X11R6/bin:/home/dov/scripts:/home/dov/Scripts:/home/dov/scripts:/home/dov/bin:/usr/X11R6/bin:/usr/bin:/bin:/usr/sbin:/sbin
    set env PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages
end

source ~/git/dov-env/gdb/pyprint.py
source ~/git/dov-env/gdb/pythreadgrep.py
source ~/git/dov-env/gdb/eigmatrixprint.py

python 
import sys, os

sys.path.insert(0, os.getenv('HOME') + '/git/dov-env/gdb')
sys.path.insert(0, os.getenv('HOME') + '/git/dov-env/gdb/eigen/printer')
from printers import register_eigen_printers
register_eigen_printers (None)
end


