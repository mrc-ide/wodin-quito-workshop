$(function() {
    var vars = {}, hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++) {
        hash = hashes[i].split('=');
        vars[hash[0]] = hash[1];
    }

    $(function() {
        if (vars["staff"] === "true") {
            $(".show-after").each(function(index) {
                $(this).show()
            });
        }
    })
});
