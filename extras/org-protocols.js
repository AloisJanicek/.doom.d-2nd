// ==UserScript==
// @name         org-protocols
// @namespace    http://tampermonkey.net/
// @version      0.2.0
// @description  Send links or/and selected content into your Emacs via various protocols
// @match        *://*/*
// @grant        none
// ==/UserScript==

(function() {
  'use strict';
  var keys = {};
  var openedWindow;
  var delay = 3000;

  // By pressing Ctrl + Alt + 'key':
  // stores link via `org-protocol://store-link`, which can be inserted by `org-insert-link`
  keys.link = 'K';

  // stores link + title via `org-protocol://store-link`, which can be inserted by `org-insert-link`
  keys.linkTitle = 'G';

  // captures selected text as an org markup via `org-protocol:///capture-html`, needs github.com/alphapapa/org-protocol-capture-html
  keys.html = 'H';

  // captures link + title via `org-protocol://capture`
  keys.bookmarkTitle = 'B';

  // captures link + title + selected text as a plain text via `org-protocol://capture`
  keys.bookmarkTitleSelected = 'X';

  // captures selected text inside code block in Inbox
  // emacs will ask for code block language identifier and title of the item
  keys.codeInbox = 'I';

  // captures selected text inside code block in Yankpad.
  // emacs will ask for code block language identifier, title of the item and category (heading) in Yankpad
  keys.codeYankpad = 'Y';

  // I need some form of sleep
  function wait(ms){
    var start = new Date().getTime();
    var end = start;
    while(end < start + ms) {
      end = new Date().getTime();
    }
  }

  // deal with parens
  function replace_all(str, find, replace) {
    return str.replace(new RegExp(find, 'g'), replace);
  }

  function escapeIt(text) {
    return replace_all(
      replace_all(
        replace_all(encodeURIComponent(text), "[(]", ""),
        "[)]", ""),
      "[']" , "");
  }

  // for that nasty case when location.href ends with slash
  const stripTrailingSlash = (str) => {
    return str.endsWith('/') ?
      str.slice(0, -1) :
      str;
  };

  // When you have parens in URL or Title... (Wikipedia!)
  function getLocation() {
    return escapeIt(stripTrailingSlash(location.href));
  }

  function getTitle() {
    return escapeIt(document.title)
  }
  // note: it can be hard to create association for protocol in chrome when popup is there just one second
  // for the first time and then after everytime you remove chrome's history, association needs to be confirmed
  // consider to temporarly increase valu passed to wait() function
  window.addEventListener('keyup', function() {
    if (event.ctrlKey && event.altKey && !event.shiftKey) {
      switch (String.fromCharCode(event.keyCode)) {
        case (keys.link):

          // org-protocol://store-link/URL
          //                 openedWindow = window.open('org-protocol://store-link/'
          //                                            + getLocation());

          // org-protocol://store-link?url=URL&title=TITLE
          openedWindow = window.open('org-protocol://store-link?url='
                                     + getLocation());
          wait(delay);
          openedWindow.close();
          break;
        case (keys.linkTitle):
          // org-protocol:/store-link/URL/TITLE
          //                 openedWindow = window.open('org-protocol://store-link/'
          //                                            + getLocation() + '/'
          //                                            + getTitle());

          // org-protocol://store-link?url=URL&title=TITLE
          openedWindow = window.open('org-protocol://store-link?url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle());
          wait(delay);
          openedWindow.close();
          break;
        case (keys.html):
          // org-protocol://capture/p/<url>/<title>/selection>
          //                  openedWindow = window.open('org-protocol://capture/w/'
          //                                            + getLocation() + '/'
          //                                            + getTitle() + '/'
          //                                            + encodeURIComponent(function() {

          // org-protocol://capture?template=X?url=URL?title=TITLE?body=BODY
          openedWindow = window.open('org-protocol://capture?template=w?url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle()
                                     + '&body='
                                     + encodeURIComponent(function() {
                                       var html = "";
                                       if (typeof window.getSelection != "undefined") {
                                         var sel = window.getSelection();
                                         if (sel.rangeCount) {
                                           var container = document.createElement("div");
                                           for (var i = 0, len = sel.rangeCount; i < len; ++i) {
                                             container.appendChild(sel.getRangeAt(i).cloneContents());
                                           }
                                           html = container.innerHTML;
                                         }
                                       } else if (typeof document.selection != "undefined") {
                                         if (document.selection.type == "Text") {
                                           html = document.selection.createRange().htmlText;
                                         }
                                       }
                                       var relToAbs = function(href) {
                                         var a = document.createElement("a");
                                         a.href = href;
                                         var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash;
                                         a.remove();
                                         return abs;
                                       };
                                       var elementTypes = [
                                         ['a', 'href'],
                                         ['img', 'src']
                                       ];
                                       var div = document.createElement('div');
                                       div.innerHTML = html;
                                       elementTypes.map(function(elementType) {
                                         var elements = div.getElementsByTagName(elementType[0]);
                                         for (var i = 0; i < elements.length; i++) {
                                           elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));
                                         }
                                       });
                                       return div.innerHTML;
                                     }()));
          wait(delay);
          openedWindow.close();
          break;
        case (keys.bookmarkTitle):
          // org-protocol://capture/L/<url>/<title>
          //                 openedWindow = window.open('org-protocol://capture/L/'
          //                                            + getLocation() + '/'
          //                                            + getTitle());

          // org-protocol://capture?template=X?url=URL?title=TITLE
          openedWindow = window.open('org-protocol://capture?template=L&url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle());
          wait(delay);
          openedWindow.close();
          break;
        case (keys.bookmarkTitleSelected):
          // org-protocol://capture/p/<url>/<title>/selection>
          //                 openedWindow = window.open('org-protocol://capture/p/'
          //                                            + getLocation() + '/'
          //                                            + getTitle() + '/'
          //                                            + escapeIt(window.getSelection().toString()));

          // org-protocol://capture?template=X?url=URL?title=TITLE?body=BODY
          openedWindow = window.open('org-protocol://capture?template=p&url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle()
                                     + '&body='
                                     + escapeIt(window.getSelection().toString())
                                    );
          wait(delay);
          openedWindow.close();
          break;
        case (keys.codeInbox):
          // org-protocol://capture/p/<url>/<title>/selection>
          //                 openedWindow = window.open('org-protocol://capture/p/'
          //                                            + getLocation() + '/'
          //                                            + getTitle() + '/'
          //                                            + escapeIt(window.getSelection().toString()));

          // org-protocol://capture?template=X?url=URL?title=TITLE?body=BODY
          openedWindow = window.open('org-protocol://capture?template=s&url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle()
                                     + '&body='
                                     + escapeIt(window.getSelection().toString())
                                    );
          wait(delay);
          openedWindow.close();
          break;
        case (keys.codeYankpad):
          // org-protocol://capture/p/<url>/<title>/selection>
          //                 openedWindow = window.open('org-protocol://capture/p/'
          //                                            + getLocation() + '/'
          //                                            + getTitle() + '/'
          //                                            + escapeIt(window.getSelection().toString()));

          // org-protocol://capture?template=X?url=URL?title=TITLE?body=BODY
          openedWindow = window.open('org-protocol://capture?template=y&url='
                                     + getLocation()
                                     + '&title='
                                     + getTitle()
                                     + '&body='
                                     + escapeIt(window.getSelection().toString())
                                    );
          wait(delay);
          openedWindow.close();
          break;
      }
    }
  });
})();
