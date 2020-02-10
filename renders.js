
function makeJoinRender(plr, t) {
	function makePlayer(p) {
		return {id:p, deck:0, hand:0, played:0, discard:0};
	}
	return function() {
		if (t == 0) {
			currentState.players.push(makePlayer(plr));
		}
		document.getElementById("title").innerHTML = "Adding player " + plr + " (t = "+t+")";
		if (t < 10)
			return makeJoinRender(plr, t+1);
		else return null;
	}
}

function makeStartRender(plr, t) {
	return function() {
		if (t == 0) {
			currentState.playing = plr;
		}
		if (t < currentState.players.length) {
			renderp = currentState.players[t];
			addPlayerDisplay(renderp);
			updatePlayerDisplay(renderp, plr);
		}
		document.getElementById("title").innerHTML = "start render " + t;
		if (t < 10 + currentState.players.length)
			return makeStartRender(plr, t+1);
		else return null;
	}
}

let playerdisplays = {};
function addPlayerDisplay(plrdata) {

	let display = {
		player : plrdata,
		div : document.createElement("div"),
		datap : document.createElement("p")
	}
	display.div.appendChild(display.datap);

	let dp = document.getElementById("playerdata");
	dp.appendChild(display.div);

	playerdisplays[plrdata.id] = display;
}

function updatePlayerDisplay(plrdata, playingno) { //updates only money, actions and purchases!
	let display = playerdisplays[plrdata.id];

	display.datap.innerHTML = `Is it my turn? ${plrdata.id == playingno}`;
}