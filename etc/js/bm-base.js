// This is the bookmarklet template for the bookmarklet framework.
// The js file template is in bm-base-file.js
//
// To make code for a bookmarklet  (fix-me)

// http://closure-compiler.appspot.com/home

(function(){

    ////////////////////////////////////////////////////////////////////
    /// Your values (you must use '' instead of ""):
    var myNamespace = 'NAMESPACE'; // For this bookmarklet, same as in .js file!
    var myURL='URL';
    ////////////////////////////////////////////////////////////////////

    // Has the .js file already been loaded?
    if (window[myNamespace])
        window[myNamespace]();
    else {
        var elt=document.getElementsByTagName('head')[0]
            || document.getElementsByTagName('body')[0];
        if (elt) {
            var script=document.createElement('script');
            script.type='text/javascript';
            script.src=myURL;
            elt.appendChild(script);
        } else
            alert('You must be on a html page for this to work');
        /* testing a
           comment */
    }
})();
// void(0)
