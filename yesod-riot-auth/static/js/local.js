window.onerror = function (message, url, lineNo, colNo, error) {
   console.log(arguments);
   let container = document.createElement('div');
   container.style.color = 'red';
   container.style.position = 'fixed';
   container.style.background = '#eee';
   container.style.padding = '2em';
   container.style.top = '1em';
   container.style.left = '1em';
   let msg = document.createElement('pre');
   msg.innerText = [
      'Message: ' + message,
      'URL: ' + url,
      'Line: ' + lineNo,
      'Column: ' + colNo,
      'Stack: ' + (error && error.stack)
   ].join('\n');
   container.appendChild(msg);
   document.body.appendChild(container);
};

window.addEventListener('popstate', function(event) {
    if (!event.state) {
        return
    }
    dataJsonUrl = event.state['dataJsonUrl']
    $.ajax({
        url: dataJsonUrl,
        type: "GET",
        dataType: "json",
    }).done(function(data, textStatus, jqXHR) {
	riot.unmount('body-tag', true)
        bodyTag = riot.mount('body-tag')[0]
        bodyTag.data = data
        bodyTag.update()
    })
});

$(document).ready(function(){
    //Check to see if the window is top if not then display button
    $(window).scroll(function(){
        if ($(this).scrollTop() > 100) {
            $('.scrollToTop').fadeIn();
        } else {
            $('.scrollToTop').fadeOut();
        }
    });
});

function copyTextToClipboard(text) {
    var textArea = document.createElement("textarea");
    textArea.value = text;
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
    try {
	var successful = document.execCommand("copy");
	var msg = successful ? "successful" : "unsuccessful";
	console.log("Fallback: Copying text command was " + msg);
    } catch (err) {
	console.error("Fallback: Oops, unable to copy", err);
    }
    document.body.removeChild(textArea);
}

function format(str, args) {
    return str.replace(/{(\d+)}/g, function(match, number) {
        return typeof args[number] != 'undefined'
            ? args[number]
            : match
    })
}

function formatDouble(num, langs) {
    if (typeof langs == "undefined") console.log("formatDouble: languages missing (" + num + ")")
    if (typeof num == "undefined" || num === null)
        return ""
    return num.toLocaleString(langs,
                              {minimumFractionDigits: 2, maximumFractionDigits: 2});
}

function formatDoublePercent(num, langs) {
    if( typeof langs == "undefined") console.log("formatDoublePercent: languages missing (" + num + ")")
    if (typeof num == "undefined" || num === null)
        return ""
    return formatDouble(num, langs) + ' %'
}

function formatDoubleEuro(num, langs) {
    if( typeof langs == "undefined") console.log("formatDoublePercent: languages missing (" + num + ")")
    if (typeof num == "undefined" || num === null)
        return ""
    return num.toLocaleString(langs,
                              {minimumFractionDigits: 2, maximumFractionDigits: 2, style: 'currency', currency: 'EUR'});
}

function formatInt2Digits(num, langs) {
    if (typeof langs == "undefined") console.log("formatInt2Digits: languages missing (" + num + ")")
    if (typeof num == "undefined" || num === null)
        return ""
    return num.toLocaleString(langs,
                              {minimumIntegerDigits: 2, maximumIntegerDigits: 2});
}

function formatIntPercent(num) {
    if (typeof num == "undefined" || num === null)
        return ""
    return num + ' %'
}

function formatDate(date, langs) {
    if (typeof langs == "undefined") console.log("formatDate: languages missing (" + date + ")")
    return date.toLocaleDateString(langs,
                                   {day: "2-digit", month: "2-digit", year: "numeric"});
}

function formatDateStr(dateStr, langs) {
    if (typeof langs == "undefined") console.log("formatDateStr: languages missing (" + dateStr + ")")
    return formatDate(new Date(dateStr), langs);
}

function formatTimeStr(dateStr, langs) {
    if (typeof langs == "undefined") console.log("formatTimeStr: languages missing (" + dateStr + ")")
    return new Date(dateStr).toLocaleTimeString(langs);
}

function formatMonthStr(dateStr, langs) {
    if (typeof langs == "undefined") console.log("formatMonthStr: languages missing (" + dateStr + ")")
    return new Date(dateStr).toLocaleDateString(langs,
                                                {month: "2-digit", year: "numeric"});
}

function fileSize(b, langs) {
    if (typeof langs == "undefined") console.log("fileSize: languages missing (" + b + ")")
    var u = 0, s = 1024;
    while (b >= s || -b >= s) {
        b /= s;
        u++;
    }
    return (u
            ? b.toLocaleString(langs,
                               {minimumFractionDigits: 0, maximumFractionDigits: 1}) + ' '
            : formatDouble(b, langs)
           ) + ' KMGTPEZY'[u] + 'B';
}

function arrayChunks(arr, len) {
    var chunks = [],
        i = 0,
        n = arr.length;
    while (i < n) {
        chunks.push(arr.slice(i, i += len));
    }
    return chunks;
}

function classNames(classes) {
    return Object.entries(classes).reduce((acc, item) => {
	const [key, value] = item
	if (value) return [...acc, key]
	return acc
    }, []).join(' ')
}
