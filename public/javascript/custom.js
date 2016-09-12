$(document).ready(function () {
    // menu navigation
    $('#nav li a').each(function () {
        var path = window.location.href;
        var current = path.substring(path.lastIndexOf('/') + 1);
        var url = $(this).attr('href').substr(1, $(this).attr('href').length - 1);
        if (url == current) {
            $(this).addClass('active');
        };
    });
    // multi-select "candidates" dropdown
    var config = {
        '.chosen-select': {}
    }
    for (var selector in config) {
        $(selector).chosen(config[selector]);
    }
});
