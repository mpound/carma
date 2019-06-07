<div class="login" id="login" style="visibility:hidden;width:500px;margin:0 auto 0 auto">
<center>
  Please enter your information to login. If you do not have a
  username or password, <a href="create">create one</a>. Your
  username is the e-mail address that you registered with.
  <br><br>
 
  If you have forgotten your password, type in your e-mail address
  and click password reset. A new password will then be sent to you.

  <br>
  <font color="red">%s</font>

  <form action="login" method="post">
     <table>
       <tr>
         <td>
           E-mail:
         </td>
         <td>
           <input type="text" name="user" value="%s">
         </td>
       </tr>
       <tr>
         <td>
           Password:
         </td>
         <td>
           <input type="password" name="pass">
         </td>
       </tr>
       <tr>
         <td colspan=2>
           <center>
             <input type="submit" name="submit" value="Submit">
             <input type="submit" name="forgotpw" value="Password Reset">
           </center>
         </td>
       </tr>
     </table>
  </form>
</center>
</div>

