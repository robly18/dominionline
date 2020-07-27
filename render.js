

let renderState = null;
let lastRenderedAction = 0;
let toRender = [];

function render() {
	if (renderState == null) {
		if (toRender.length == 0)
			return;
		renderState = makeRenderState(toRender.shift());
	}
	console.log(renderState);
	if (renderState != null)
		renderState = renderState();
}

function makeRenderState(event) {
	console.log("Making renderState:")
	console.log(event);
	switch (event.tag) {
	case "PlayerAction":
		switch (event.contents[1].tag) {
		case "Say":
			return null;
			break;
		case "StartGame":
			return makeStartRender(event.contents[0], 0);
			break;
		case "Buy":
			return makeBuyCardRender(event.contents[0], event.contents[1].contents, 0);
			break;
		case "EndTurn":
			return makeEndTurnEvent(event.contents[0], 0);
			break;
		}
		break;
	case "DrawEvent":
		return makeDrawRender(event.contents[0], 0, event.contents[1]);
		break;
	case "JoinEvent":
		return makeJoinRender(event.contents, 0);
		break;
	case "PlayedCardEvent":
		return makePlayedCardRender(event.contents[0], event.contents[1], 0);
		break;
	case "PlayedCardEffect":
		return makePlayedCardEffectRender(event.contents[0], event.contents[1]);
	}
}

let previousLogString = null;
function renderLogData(data) {
	if (data.responseText == previousLogString)
		return;
	previousLogString = data.responseText;
	let log = JSON.parse(data.responseText);
	renderChanges(log.slice(lastRenderedAction));
	lastRenderedAction = log.length;
}

function renderChanges(changes) {
	console.log("Adding changes:");
	console.log(changes);
	toRender = toRender.concat(changes);
	console.log(toRender);
}