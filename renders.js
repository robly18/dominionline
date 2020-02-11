
function makeJoinRender(plr, t) {
	function makePlayer(p) {
		return {id:p, deck:10, hand:0, played:0, discard:0};
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
			updatePlayerCardDisplay(renderp);
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
		namep : document.createElement("p"),
		datap : document.createElement("p"),
		handp : document.createElement("p"),
		cardp : document.createElement("p")
	}
	display.div.appendChild(display.namep);
	display.div.appendChild(display.datap);
	display.div.appendChild(display.handp);
	display.div.appendChild(display.cardp);

	let dp = document.getElementById("playerdata");
	dp.appendChild(display.div);

	playerdisplays[plrdata.id] = display;
}

function updatePlayerDisplay(plrdata, playingno) { //updates only who's playing, money, actions and purchases!
	let display = playerdisplays[plrdata.id];

	display.namep.innerHTML = playingno == plrdata.id ? `>>Player ${plrdata.id}<< (Now playing)` : `Player ${plrdata.id}`;
}
function updatePlayerCardDisplay(plrdata) {
	let display = playerdisplays[plrdata.id];
	display.cardp.innerHTML = `Discard: ${"|".repeat(plrdata.played)} ${"|".repeat(plrdata.discard)}; Deck: ${"|".repeat(plrdata.deck)}`;
	display.handp.innerHTML = `Hand: ${"|".repeat(plrdata.hand)}`; 
}


function makeDrawRender(plr, t) {
	return function() {
		let renderp = currentState.players[plr];
		renderp.hand++;
		if (renderp.deck > 0) {
			renderp.deck--;
		} else {
			renderp.deck = renderp.discard - 1; //this is a bug. but it probably won't happen.
			renderp.discard = 0;
		}

		updatePlayerCardDisplay(renderp);
		return null;
	}
}

function makePlayedCardRender(card) {
	function cardDisplay(name) { //temporary
		let node = document.createElement("p");
		node.innerHTML = card;
		return node;
	}

	return function() {
		let renderp = currentState.players[currentState.playing];
		renderp.hand--;
		updatePlayerCardDisplay(renderp);
		
		document.getElementById("playedcards").appendChild(cardDisplay(card));

		return null;
	}
}

function makeBuyCardRender(plr, bought) {

}