#!/bin/sh

cd hs-generator

if test -f ginger.tar; then
    tar -xf ginger.tar
    rm ginger.tar
fi

yesod_dir=..

tmp_file=/tmp/hs-generator.tmp

stack build hs-generator
if test $? -ne 0; then
    exit 1
fi
generator_exec=`find . -name hs-generator -type f`

exec_ginger2() {
    model_name=$1
    template_file=$2
    dest_file=$yesod_dir/$3
    echo $dest_file
    if test -n "$4"; then
        if test -n "$5"; then
	    comment_start="$4"
	    comment_end="$5"
        else
	    comment_start="$4 - start"
	    comment_end="$4 - end"
        fi
    fi
    $generator_exec $template_file $model_name > $tmp_file
    if test ! -f $dest_file; then
        touch $dest_file
    fi
    if test -n "$4"; then
        grep -q -- "$comment_start" $dest_file
        if test ! $? -eq 0; then
	    echo "$comment_start" >> $dest_file
	    echo "$comment_end" >> $dest_file
        fi
        sed -i -e "/$comment_start/,/$comment_end/{//p;d;}" $dest_file
        sed -i -e "/$comment_start/ r $tmp_file" $dest_file
    else
        cat $tmp_file > $dest_file
    fi
}

exec_ginger() {
    template_file=$1
    dest_file=$yesod_dir/$2
    echo $dest_file
    if test -n "$3"; then
        if test -n "$4"; then
	    comment_start="$3"
	    comment_end="$4"
        else
	    comment_start="$3 - start"
	    comment_end="$3 - end"
        fi
    fi
    $generator_exec $template_file > $tmp_file
    if test ! -f $dest_file; then
        touch $dest_file
    fi
    if test -n "$3"; then
        grep -q -- "$comment_start" $dest_file
        if test ! $? -eq 0; then
	    echo "$comment_start" >> $dest_file
	    echo "$comment_end" >> $dest_file
        fi
        sed -i -e "/$comment_start/,/$comment_end/{//p;d;}" $dest_file
        sed -i -e "/$comment_start/ r $tmp_file" $dest_file
    else
        cat $tmp_file > $dest_file
    fi
}

yesod_devel_pid=$(ps aux|grep 'yesod devel$'|grep -v grep|awk '{print $2}')
if test -n "$yesod_devel_pid"; then
    kill -TSTP $yesod_devel_pid
fi

exec_ginger debug_json.gtmpl context.json
exec_ginger models.gtmpl config/models_migrate '-- gen models'
exec_ginger postgresql_triggers.gtmpl postgresql_setup.sql '-- gen triggers'
exec_ginger i18n.gtmpl src/I18n.hs '-- gen i18n'

f=src/Handler/User.hs
m=user
if test -z "$1" -o "$1" = $m; then
    exec_ginger2 $m handler_data_add.gtmpl $f '-- gen data add'
    exec_ginger2 $m handler_get_add_form.gtmpl $f '-- gen get add form'
    exec_ginger2 $m handler_data_edit.gtmpl $f '-- gen data edit'
    exec_ginger2 $m handler_add_form.gtmpl $f '-- gen add form'
    exec_ginger2 $m handler_get_edit_form.gtmpl $f '-- gen get edit form'
    exec_ginger2 $m handler_edit_form.gtmpl $f '-- gen edit form'
    exec_ginger2 $m handler_get_delete_form.gtmpl $f '-- gen get delete form'
    exec_ginger2 $m handler_post_delete_form.gtmpl $f '-- gen post delete form'
    exec_ginger2 $m handler_delete_form.gtmpl $f '-- gen delete form'
fi

f=src/Handler/Config.hs
m=config
if test -z "$1" -o "$1" = $m; then
    exec_ginger2 $m handler_data_edit.gtmpl $f '-- gen data edit'
    exec_ginger2 $m handler_get_edit_form.gtmpl $f '-- gen get edit form'
    exec_ginger2 $m handler_post_edit_form.gtmpl $f '-- gen post edit form'
    exec_ginger2 $m handler_edit_form.gtmpl $f '-- gen edit form'
fi

f=src/Handler/TestMail.hs
m=testmail
if test -z "$1" -o "$1" = $m; then
    exec_ginger2 $m handler_data_add.gtmpl $f '-- gen data add'
    exec_ginger2 $m handler_get_add_form.gtmpl $f '-- gen get add form'
    exec_ginger2 $m handler_add_form.gtmpl $f '-- gen add form'
fi



if test -n "$yesod_devel_pid"; then
    sleep 1
    kill -CONT $yesod_devel_pid
fi
