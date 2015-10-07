window.onload = function () {
  var button = document.getElementById("go");
  button.onclick = sendRequest;
};

function sendRequest() {
  var seed = document.getElementById("seed").value;
  var depth = document.getElementById("depth").value;
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:3000/' + seed + '/' + depth);
  xhr.setRequestHeader("Accept", "application/json");
  xhr.onload = function() {
    if (xhr.status === 200) {
      handleSuccess(xhr.responseText);
    }
    else {
      console.log('Request failed.  Returned status of ' + xhr.status);
    }
  };
  xhr.send();
}

function handleSuccess(data) {
  if (data == "no such char found") {
    console.log('no char found you stink');
  } else {
    var parsed = JSON.parse(data),
        list = parsed.slice(-1)[0],
        tree = parsed.slice(0,-1)[0];
    document.getElementById("list").innerHTML = list;
    render(tree, "char");
  }
}

function render(tree, pos) {
  var head = tree[0],
      subtree = tree[1],
      elt,
      fst,
      snd,
      charDiv,
      subEltA,
      subEltB;
  charDiv = document.getElementById("char");
  elt = document.getElementById(pos);
  if (head === "UD" || head === "LR") {
    fst = subtree[0];
    snd = subtree[1];
    subEltA = document.createElement("span");
    subEltA.id = pos + head[0];
    setStyle(subEltA);
    subEltB = document.createElement("span");
    subEltB.id = pos + head[1];
    setStyle(subEltB);
    charDiv.appendChild(subEltA);
    charDiv.appendChild(subEltB);
    render(fst, subEltA.id);
    render(snd, subEltB.id);
  } else {
    elt.innerHTML = head;
  }
}

function setStyle(elt) {
  var scale,
      translate,
      pos,
      i,
      p;
  elt.style.position = "absolute";
  pos = elt.id.slice(4);
  for (i = 0; i < pos.length; i++) {
    console.log(pos[i]);
  }
}
