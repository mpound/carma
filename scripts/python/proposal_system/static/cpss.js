   <!--
      function popup(link, windowname)
      {
         if (! window.focus) return true;
         var href;
         if (typeof(link) == 'string')
            href = link;
         else
            href = link.href;

         newwin = window.open(href, windowname, 'width=400,height=200,scrollbars=yes,toolbar=no,location=no,directories=no,status=no,menubar=no,resizable=yes,dependent=yes');
         if (window.focus) {newwin.focus();}
         return false;
      }

      function getCookie(name)
      {
         var newName = name + "=";
         var nameLength = newName.length;
         var cookieLength = document.cookie.length;
         var i = 0;
         while(i < cookieLength)
         {
            var j = i + nameLength;
            if(document.cookie.substring(i,j) == newName)
            {
               return true;
            }
            i = document.cookie.indexOf(" ",i) + 1;
            if (i == 0) break;
         }
         return false;
      }
        
      document.cookie = 'test=junk';
      if(getCookie('test'))
      {
         // delete the cookie
         document.cookie = name + '=' + '; expires=Thu, 01-Jan-70 00:00:01 GMT';
      }
      else
      {
         document.write("<div class=browser_error id=test1>If you are seeing this message then cookies are not enabled on this browser. Cookies are required for this system to work properly. Please enable cookies and click <a href='login/'>here</a> to continue.</div>");
      }
        
   // -->

