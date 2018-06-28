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
    if len(sys.argv) > 1:
        todo_files = sys.argv[1:]
    else:
        todo_files = config.keys()

    for file_name in todo_files:
        src = os.path.join(here, file_name)
        dst = os.path.join(os.path.expanduser('~/'), '.' + file_name)
        try:
            METHODS[config[file_name]](src, dst)
        except Exception as e:
            print("处理%s时出错, 跳过:\n\t%s: %s" % (file_name, e.__class__.__name__, e))
        else:
            print("成功处理%s" % file_name)


def append(src, dst):
    if not os.path.isfile(dst):
        raise Exception("In append: 文件 %s 不存在" % dst)
    dst_file = open(dst, 'a')
    dst_file.write('\n' + open(src, 'r').read())


def copy(src, dst):
    shutil.copy(src, dst)


def copytree(src, dst, symlinks=False, ignore=None):
    if not os.path.isdir(dst):
        os.makedirs(dst)
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            shutil.copytree(s, d, symlinks, ignore)
        else:
            shutil.copy2(s, d)

def create_or_append(src, dst):
    if not os.path.isfile(dst):
        os.makedirs(os.path.dirname(dst))
        shutil.copy(src, dst)
    else:
        with open(dst, 'a') as dst_file:
            dst_file.write('\n' + open(src, 'r').read())

# 可能.ssh还需要自动chmod一下...以及chattr +i 一下... 所以还需要一个pre或者post的权限设置
# 或者指定文件权限的...

METHODS = {
    'append': append,
    'copy': copy,
    'copytree': copytree,
    'create_or_append': create_or_append
}


if __name__ == "__main__":
    main()
