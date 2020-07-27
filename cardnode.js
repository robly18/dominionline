
let knowncards = {};
let undrawnNodes = {};
function fetchCardData(c) { //if the card is unknown, return null, flag incomplete data as true, and request data. otherwise, return card data
	let data = knowncards[c];
	if (data == null) {
		if (!(c in knowncards)) {
			pollCard(c);
		}
		incompleteData = true;
		return null;
	} else return data;
}
function cardInfoNode(c) {
	let node = document.createElement("button");
	node.className = "tooltip";
	let h = document.createElement("span");
	h.className = "tooltiptext";
	h.appendChild(cardHighlight(c));
	node.appendChild(document.createTextNode("?"));
	node.appendChild(h);
	node.style.paddingLeft = "1ex";
	node.style.paddingRight = "1ex";
	node.style.marginLeft = "0.5ex";
	node.style.marginRight = "1ex";
	return node;
}

function cardHighlight(c) {
	let data = fetchCardData(c);
	if (data == null) return document.createTextNode("unknown card");
	let [name, efflist, cost, reaction, treasure, score] = data;
	let hn = document.createElement("div");
	let t = document.createElement("h4");
	t.innerHTML = `${name}${treasure ? " - Treasure" : ""}${reaction ? " - Reaction (Immune to attacks as long as this is in your hand)" : ""}`
	t.style.margin = "0.2ex";
	hn.appendChild(t);
	for (let e of efflist) {
		hn.appendChild(document.createTextNode(showEffect(e)));
		hn.appendChild(document.createElement("br"));
	}
	hn.appendChild(document.createElement("br"));
	hn.appendChild(document.createTextNode(`Cost: ${cost}`));
	if (score != 0) {
		hn.appendChild(document.createElement("br"));
		hn.appendChild(document.createTextNode(`Worth ${score} victory point${score==1?"":"s"}`));
	}
	return hn;
}

function makeCardNode(name, callback, cssclass) {
	let div = document.createElement("div");
	let b = document.createElement("button");
	
	b.innerHTML = name;
	b.onclick = callback;
	b.className = cssclass || "cardButton";
	div.appendChild(b);
	div.appendChild(cardInfoNode(name));

	return div;
}

function showEffect(e) {
	switch (e.tag) {
	case "Action": return "Action card";
	case "Money": return `+${e.contents} money`;
	case "Actions": return `+${e.contents} actions`;
	case "Purchases": return `+${e.contents} purchases`;
	case "Draw": return `Draw ${e.contents} card${e.contents==1?"":"s"}`;
	case "PlayerChoice": case "OtherPlayerChoice": return cfTooltip(e.contents);
	default: return JSON.stringify(e);
	}
}

function cfTooltip(cf) {
	switch (cf) {
	case "CFRemodel": return "Trash a card from your hand; buy a card worth at most its cost +2";
	case "CFCellar": return "Discard any number of cards from your hand; draw an equal number of cards";
	case "CFWorkshop": return "Buy a card worth at most 4";
	case "CFMilitia": return "All other players: Discard cards until you have at most 3 in your hand";
	case "CFMine": return "Trash a treasure card from your hand; buy a treasure card worth at most its cost +3";
	default: return cf;
	}
}