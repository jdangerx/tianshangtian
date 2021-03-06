var SIZE = 400;

window.onload = function () {
  var button = document.getElementById("go");
  button.onclick = sendRequest;
  var renderButton = document.getElementById("render-button");
  renderButton.onclick = renderFromInput;
};

function sendRequest() {
  var seed = document.getElementById("seed").value;
  var depth = document.getElementById("depth").value;
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:3000/' + seed + '/' + depth);
  xhr.setRequestHeader("Accept", "application/json");
  xhr.onload = function() {
    if (xhr.status === 200) {
      // handleSuccess(xhr.responseText);
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
    console.log(data);
    var parsed = JSON.parse(data),
        list = parsed.slice(-1)[0],
        tree = parsed.slice(0,-1)[0];

    // tree = ["UD",
    //         [
    //           ["折", []],
    //           ["LR", [
    //             ["LR", [["扌", []],
    //                     ["口", []]]],
    //             ["UD", [["土", []],
    //                     ["土", []]]]
    //           ]
    //           ]
    //         ]
    //        ];

    document.getElementById("list").innerHTML = list;
    document.getElementById("tree").innerHTML = JSON.stringify(tree);
    var charDiv = document.getElementById("char");
    charDiv.innerHTML = "";
    render(tree, charDiv);

    // hack to compress things with more than one big column
    if (tree[0] === "LR") {
      var cells = charDiv.getElementsByClassName("cell");
      var numCols = charDiv.childNodes.length;
      for (var i = 0; i < cells.length; i++) {
        cells[i].style.width = 1/numCols + "em";
      }
    }
    rescale(charDiv);
  }
}

function renderFromInput() {
  var charDiv = document.getElementById("char");
  charDiv.innerHTML = "";
  var tree = JSON.parse(document.getElementById("tree-in").value);
  render(tree, charDiv);

  // hack to compress things with more than one big column
  if (tree[0] === "LR") {
    var cells = charDiv.getElementsByClassName("cell");
    var numCols = charDiv.childNodes.length;
    for (var i = 0; i < cells.length; i++) {
      cells[i].style.width = 1/numCols + "em";
    }
  }
  rescale(charDiv);
}

function render(tree, elt) {
  var head = tree[0],
      subtree = tree[1],
      cell;

  if (head === "UD") {
    var upTree = subtree[0];
    var downTree = subtree[1];
    cell = document.createElement("span");
    cell.setAttribute("class", "cell");

    var upRow = document.createElement("div");
    upRow.setAttribute("class", "row");

    var downRow = document.createElement("div");
    downRow.setAttribute("class", "row");

    cell.appendChild(upRow);
    cell.appendChild(downRow);
    elt.appendChild(cell);
    render(upTree, upRow);
    render(downTree, downRow);
  } else if (head === "LR") {

    var leftTree = subtree[0];
    var rightTree = subtree[1];

    render(leftTree, elt);
    render(rightTree, elt);

  } else {
    cell = document.createElement("span");
    cell.setAttribute("class", "cell");
    cell.innerHTML = "<span class='character'>" + head + "</span>";
    elt.appendChild(cell);
  }
}


function rescale(elt) {
  var children = elt.childNodes;
  var numChildren = children.length;
  var chars;
  for (var i = 0; i < numChildren; i++) {
    var child = children[i];
    if (child.className === "row") {
      child.style.fontSize = 1/numChildren + "em";
      chars = child.getElementsByClassName("character");
      for (var j = 0; j < chars.length; j++) {
        compoundScale(chars[j], numChildren);
      }
    } else if (child.className == "cell") {
      chars = child.getElementsByClassName("character");
      for (var j = 0; j < chars.length; j++) {
        compoundScale(chars[j], 1/numChildren);
      }
    }
    rescale(child);
  }
}

function compoundScale(elt, scale) {
  var oldT = elt.getAttribute("style") || "transform: scaleX(1);",
      oldS = oldT.slice(18, -2),
      newS = oldS * scale;
  console.log(elt.innerHTML, oldS, newS);
  elt.style.transform = "scaleX(" + newS + ")";
}
