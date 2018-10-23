window.addEventListener('popstate', function(event) {
    //document.location.href = document.location
    if (!event.state) {
        return
    }
    dataJsonUrl = event.state['dataJsonUrl']
    $.ajax({
        url: dataJsonUrl,
        type: "GET",
        dataType: "json",
    }).done(function(data, textStatus, jqXHR) {
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
            : formatDouble(b)
           ) + ' KMGTPEZY'[u] + 'B';
}

function arrayChunks (arr, len) {
    var chunks = [],
        i = 0,
        n = arr.length;
    while (i < n) {
        chunks.push(arr.slice(i, i += len));
    }
    return chunks;
}

