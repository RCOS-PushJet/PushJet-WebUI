window.onload = function() {

  var ws = null;

  document.getElementById("uuid_submit").onclick = function() {

    var uuid = document.getElementById("uuid").value;
    if (ws != null) {
      print("Disconnected!");
      ws.close();
    }
    ws = new WebSocket('wss://api.pushjet.io/ws');

    function print(s) {
      document.getElementById("demo").appendChild(
          document.createTextNode(s));
      document.getElementById("demo").innerHTML += "<br />"
    }

    function send(s)  {
      print("> " + s);
      ws.send(s)
    }

    ws.onmessage = function (e) { print("< " + e.data); }
    ws.onopen    = function (e) { print("Connected!"); send(uuid); }
  }

}
