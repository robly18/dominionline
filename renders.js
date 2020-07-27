
let currentState = {players : [], store : []};
let names = /*VERY TEMPORARY*/ ["Copper", "Silver", "Gold", "Estate", "Duchy", "Province", "Forge", "Village", "Lumberjack", "Market", "Remodel", "Cellar", "Workshop", "Moat", "Militia", "Mine"];

function setTurn(state, plr) {
	state.playing = plr;
	let p = state.players[plr];
	p.actions = 1; p.purchases = 1; p.money = 0;
}

function makeJoinRender(plr, t) {
	function makePlayer(p) {
		return {id:p, deck:10, hand:0, played:0, discard:0};
	}
	return function() {
		currentState.players.push(makePlayer(plr));
		return null;
	}
}

function makeStartRender(plr, t) {
	function sr1(plr, t) {
		return function() {
			if (t == 5) return sr2(plr, 0);
			return sr1(plr, t+1)
		}
	}
	function sr2(plr, t) {
		return function() {
			setTurn(currentState, plr);
			if (t < currentState.players.length) {
				let renderp = currentState.players[t];
				addPlayerDisplay(renderp);
				updatePlayerDisplay(renderp, plr);
				updatePlayerCardDisplay(renderp);
				return sr2(plr, t+1);
			} else {
				return sr3(plr, 0);
			}
		}
	}
	function sr3(plr, t) {
		return function() {
			if (t < 16) {
				let cardHolder = {
					amt : 10,
					name : names[t] //!!!
				}
				cardHolder.b = makeCardNode(cardHolder.name, function(){sendAction(buyCardAction(t))});
				document.getElementById("store").appendChild(cardHolder.b);
				return sr3(plr, t+1);
			} else return null;
		}
	}
	return sr1(plr, 0)
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

function updatePlayerDisplay(plrdata) { //updates only who's playing, money, actions and purchases!
	let display = playerdisplays[plrdata.id];
	if (currentState.playing != plrdata.id) {
		display.namep.innerHTML = `Player ${plrdata.id}`;
	} else {
		display.namep.innerHTML = `>>Player ${plrdata.id}<< (Now playing)<br>Money: ${plrdata.money}, Actions: ${plrdata.actions}, Purchases: ${plrdata.purchases}.`;
	}
}
function updatePlayerCardDisplay(plrdata) {
	let display = playerdisplays[plrdata.id];
	display.cardp.innerHTML = `Discard: 	${"|".repeat(plrdata.discard)}; Deck: ${"|".repeat(plrdata.deck)}`;
	display.handp.innerHTML = `Hand: ${"|".repeat(plrdata.hand)}`; 
}

let hudState = {hand : [], played : [], discard : 0, deck : 10};

function updateHudDisplay(state) {
	let hdiv = document.getElementById("hand");
	let ddiv = document.getElementById("discard");
	let pdiv = document.getElementById("played");
	let deck = document.getElementById("deck");

	hdiv.innerHTML = "";
	for (let [i, c] of state.hand.entries()) {
		hdiv.appendChild(makeCardNode(c, function(){sendAction(playCardAction(i))}));
		hdiv.appendChild(document.createElement("br"));
	}
	ddiv.innerHTML = "|".repeat(state.discard);
	pdiv.innerHTML = state.played;
	deck.innerHTML = "|".repeat(state.deck);
}

function makeDrawRender(plr, t, card) {
	return function() {
		let renderp = currentState.players[plr];
		renderp.hand++;
		if (renderp.deck > 0) {
			renderp.deck--;
		} else {
			renderp.deck = renderp.discard - 1; //this is a bug. but it probably won't happen.
												// dear past me: what?? do you mean, like, if there are no cards in the discard pile?
			renderp.discard = 0;
		}
		updatePlayerCardDisplay(renderp);
		if (plr == playerno) {
			hudState.hand.push(card);
			if (hudState.deck > 0) {
				hudState.deck--;
			} else {
				hudState.deck = hudState.discard - 1; //this is a bug. but it probably won't happen.
													// dear past me: what?? do you mean, like, if there are no cards in the discard pile?
				hudState.discard = 0;
			}
			updateHudDisplay(hudState);
		}

		return null;
	}
}

function makePlayedCardRender(plr, card, t) {
	function cardDisplay(name) { //temporary
		let node = document.createElement("p");
		node.innerHTML = name;
		return node;
	}

	return function() {
		let renderp = currentState.players[currentState.playing];
		renderp.hand--;
		renderp.played++;
		updatePlayerCardDisplay(renderp);

	
		if (plr == playerno) {
			hudState.played = hudState.played.concat(hudState.hand.splice(hudState.hand.indexOf(card), 1));
			
			updateHudDisplay(hudState);
		}
		
		document.getElementById("playedcards").prepend(cardDisplay(card));
	
		return null;
	}
}

function makePlayedCardEffectRender(plr, effect) {
	return function() {
		return null; //deprecated function
		let renderp = currentState.players[plr];
		switch (effect.tag) {
		case "Action":
			renderp.actions--;
			break;
		case "Money":
			renderp.money += effect.contents;
			break;
		case "Purchases":
			renderp.purchases += effect.contents;
			break;
		case "Actions":
			renderp.actions += effect.contents;
			break;
		}
		updatePlayerDisplay(renderp, currentState.playing);
	}
}

function makeBuyCardRender(plr, bought, t) {
	function cardBuyDisplay(name) { //temporary
		let node = document.createElement("p");
		node.innerHTML = `Bought ${name}`;
		return node;
	}

	return function() { //Todo: decrease money and purchases
		let renderp = currentState.players[currentState.playing]
		currentState.store[bought]--;
		renderp.played++;
		document.getElementById("playedcards").prepend(cardBuyDisplay(names[bought]));

		if (plr == playerno) {
			hudState.played = hudState.played.concat(names[bought])
			updateHudDisplay(hudState);
		}

		return null;
	}
}

function makeEndTurnEvent(plr, t) {
	return function() {
		let renderp = currentState.players[plr];
		renderp.discard += renderp.hand + renderp.played;
		renderp.hand = 0;
		renderp.played = 0;
		updatePlayerCardDisplay(renderp);

		document.getElementById("playedcards").innerHTML = "";

		if (plr == playerno) {
			hudState.discard += hudState.hand.length + hudState.played.length;
			hudState.hand = [];
			hudState.played = [];
			updateHudDisplay(hudState);
		}

		let nextplr = (plr+1)%currentState.players.length;
		setTurn(currentState, nextplr);
		updatePlayerDisplay(renderp, nextplr);
		updatePlayerDisplay(currentState.players[nextplr], nextplr);

		return null;
	}
}

function makeChangeRender(plr, delta) {
	return function() {
		let renderp = currentState.players[plr];
		switch (delta.tag) {
		case "MoneyDelta":
			renderp.money += delta.contents;
			break;
		case "ActionDelta":
			renderp.actions += delta.contents;
			break;
		case "PurchasesDelta":
			renderp.purchases += delta.contents;
			break;
		default:
			console.log("Unknown change type "+delta);
			break;
		}
		updatePlayerDisplay(renderp);
		return null;
	}
}