var SIZE = 400;

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
    document.getElementById("char").innerHTML = "";
    render(tree, "char");
  }
}

function render(tree, eltId, topLeft) {
  var head = tree[0],
      subtree = tree[1],
      elt,
      fst,
      snd,
      charDiv,
      subEltA,
      subEltB;
  topLeft = topLeft || [0, 0];
  charDiv = document.getElementById("char");
  charDiv.style.transform = "translate(5em, 5em)";
  elt = document.getElementById(eltId);
  if (head === "UD" || head === "LR") {
    fst = subtree[0];
    snd = subtree[1];
    subEltA = document.createElement("span");
    subEltA.id = eltId + head[0];
    setStyle(subEltA);
    subEltB = document.createElement("span");
    subEltB.id = eltId + head[1];
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
  var scale = [1,1],
      translate = [0,0],
      pos,
      i,
      p;
  elt.style.position = "absolute";
  elt.style.fontSize = SIZE + "px";
  pos = elt.id.slice(4);
  for (i = 0; i < pos.length; i++) {
    p = pos[i];
    if (p === "L") {
      scale[0] *= 0.5;
    } else if (p == "R") {
      scale[0] *= 0.5;
      translate[0] += (SIZE - translate[0])/2;
    } else if (p == "U") {
      scale[1] *= 0.5;
    } else if (p == "D") {
      scale[1] *= 0.5;
      translate[1] += (SIZE - translate[0])/2;
    }
  }
  elt.style.transform = "scale(" + scale[0] + ", " + scale[1] + ")";
  elt.style.left = translate[0] + "px";
  elt.style.top = translate[1] + "px";
}
