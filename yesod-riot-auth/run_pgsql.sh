#!/bin/sh

uname -a | grep -q Darwin
if test $? -eq 0; then
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication yesod-riot-auth'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createdb --encoding=UTF-8 --owner=yesod-riot-auth --template=template0 yesod-riot-auth'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/psql -U yesod-riot-auth yesod-riot-auth'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/dropdb yesod-riot-auth'

    sudo su postgres -c 'EDITOR=emacs /opt/local/lib/postgresql95/bin/psql -U yesod-riot-auth yesod-riot-auth'
fi


uname -a | grep -q Ubuntu
if test $? -eq 0; then
    # sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication yesod-riot-auth'
    # sudo su postgres -c 'createdb --encoding=UTF-8 --owner=yesod-riot-auth --template=template0 yesod-riot-auth'
    sudo su postgres -c 'EDITOR=emacs psql -U yesod-riot-auth yesod-riot-auth'
fi
