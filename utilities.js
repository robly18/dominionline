
function makeAction(a) {
	return JSON.stringify([playerno, a]);
}
function startAction() {
	return {tag:"StartGame"};
}
function nextGameAction() {
	return {tag:"NextGame"};
}
function sayAction(c) {
	return {tag:"Say", contents:c};
}


function pollAction() {
	return {tag:"Poll"};
}
function pollCardAction(c) {
	return {tag:"PollCard", contents:c};
}

function playCardAction(c) {
	return {tag:"Play", contents:c};
}
function nextPlayerAction() {
	return {tag:"EndTurn"};
}
function buyCardAction(i) {
    return {tag:"Buy", contents:i};
}
function choiceAction(c) {
	return {tag:"Choose", contents:c};
}
function cremodel(i, j) {
	return {tag:"CRemodel", contents:[i,j]};
}
function ccellar(cs) {
	return {tag:"CCellar", contents:cs};
}
function cworkshop(i) {
	return {tag:"CWorkshop", contents:i};
}
function cmilitia(cs) {
	return {tag:"CMilitia", contents:cs};
}
function cmine(i, j) {
	return {tag:"CMine", contents:[i,j]};
}
function skipchoice() {
	return {tag:"SkipChoice"};
}

function ajax(url, data, callback) {
  if (callback == null) callback = ()=>null;
  var xhttp;
  xhttp=new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      callback(this);
    }
  };
  xhttp.open("POST", url, true);
  xhttp.send(data);
}

function sendAction(a) {
    return ajax('/send/', makeAction(a), poll);
}
function poll() {
    return ajax('/send/', makeAction(pollAction()), d => renderStateData(d.responseText));
}
function pollCard(c) {
	knowncards[c] = null;
    return ajax('/send/', makeAction(pollCardAction(c)), d => knowncards[c] = JSON.parse(d.responseText).contents);
}