<div id="editlist">
  <p>Justification Type</p>

  <table>
    <tr>
      <td style="width : 50%%; text-align : left;">
        Using this proposal submission tool, you have a choice of
        using the web-based tool to submit your Scientific and
        Technical Justification sections or to upload a LaTeX file
        containing this information using the template located <a
        href='static/justification.tar.gz'>here</a>.
      </td>
      <td>
        <form action='proposal/typechange/%s' method='post' name="form">
          I want to use: 
          <select name="type">
            <option value="Website Justification" %s>Website Justification
            <option value="LaTeX Template" %s>LaTeX Template
          </select>
          <input type="submit" value="Select Choice" name="submit"></input>
        </form>
