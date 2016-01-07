# -*- coding: utf-8 -*-

from __future__ import print_function

import os
import sys
import shutil
import yaml

here = os.path.dirname(os.path.abspath(__file__))
CONFIG_FILE = os.path.join(here, '.dotfiles.conf')


def main():
    config = yaml.load(open(CONFIG_FILE, 'r'))
    if len(sys.argvv) > 1:
        todo_files = sys.argv[1:]
    else:
        todo_files = config.keys()

    for file_name in todo_files:
        src = os.path.join(here, file_name)
        dst = os.path.join(os.path.expanduser('~/'), '.' + file_name)
        try:
            METHODS[config[file_name]](src, dst)
        except Exception as e:
            print("处理%s时出错: %s" % (file_name, e))
        else:
            print("成功处理%s" % file_name)


def append(src, dst):
    dst_file = open(dst, 'a')
    dst_file.write('\n' + open(src, 'r').read())


def copy(src, dst):
    shutil.copy(src, dst)


METHODS = {
    'append': append,
    'copy': copy
}


if __name__ == "__main__":
    main()
