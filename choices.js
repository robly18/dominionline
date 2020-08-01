
let flag = null;
//when it needs to make a choice (or rather, a sequence of choices),
//save something of the sort {type:choiceType, choiceFunction: a function that handles the next}
//choiceFunction works as such: choiceFunction(current_flag, choice) = next_flag
//this setup needs to be reviewed later.

function renderChoice(choice) {
	if (flag != null) {
		alert("This is not supposed to happen! Error choices01");
	}
	switch (choice) {
		case "CFRemodel": flag = makeRemodelFlag(); break;
		case "CFCellar": flag = makeCellarFlag(); break;
		case "CFWorkshop": flag = makeWorkshopFlag(); break;
		case "CFMilitia": flag = makeMilitiaFlag(); break;
		case "CFMine": flag = makeMineFlag(); break;
		default: alert("Unknown choice type! Error choices02"); break;
	}
}

function makeRemodelFlag() {
	return { makeRender : makeOwnCardChoiceRender(),
			 choiceFunction : function(f, i) {
	return { makeRender : makeStoreCardChoiceRender(),
			 choiceFunction : function(f, j) {
			 sendAction(choiceAction(cremodel(i, j)));
			 return null;
	}
	}
	}
	}
}

function makeOwnCardChoiceRender() {
	return function() {
		hudState.choice = true;
		updateHudDisplay(hudState);
	}
}
function makeClearHudChoiceRender() { //awful name tbh
	return function() {
		hudState.choice = false;
		updateHudDisplay(hudState);
	}
}

function makeStoreCardChoiceRender() {
	return function() {
		for (let t = 0; t < 16; t++) {
			storeHolders[t].b.className = "cardChoiceButton";
			storeHolders[t].b.onclick = function() {
				toRender.push({tag:"Custom", makeRender:makeClearStoreChoiceRender()});
				flag = flag.choiceFunction(flag, t);
			};
		}
		return null;
	}
}
function makeClearStoreChoiceRender() { //awful name tbh
	return function() {
		for (let t = 0; t < 16; t++) {
			storeHolders[t].b.className = "cardButton";
			storeHolders[t].onclick = function(){sendAction(buyCardAction(t));};
		}
		return null;
	}
}

/*
function makeRemodelFlag() {
	return { type : "ownCard",
			 choiceFunc : function(f, i) {
		return { type : "tableCard",
				 choiceFunc : function(f, j) {
					sendAction(choiceAction(cremodel(i, j)));
					return null;
				}
				}
			}
			}
}*/
function makeCellarFlag() {
	return { type : "ownCard",
			 _cards : [],
			 shortCircuit : function(f) {
				sendAction(choiceAction(ccellar(f._cards)));
				return null;
			 },
			 choiceFunc : function(f, i) {
				f._cards.push(i);
				return f;
			}
			}
}
function makeWorkshopFlag() {
	return { type : "tableCard",
			 choiceFunc : function(f, i) {
				sendAction(choiceAction(cworkshop(i)));
				return null;
			}
			}
}
function makeMilitiaFlag() {
	return { type : "ownCard",
			 _cards : [],
			 shortCircuit : function(f) {
				sendAction(choiceAction(cmilitia(f._cards)));
				return null;
			 },
			 choiceFunc : function(f, i) {
				f._cards.push(i);
				return f;
			}
			}
}
function makeMineFlag() {
	return { type : "ownCard",
			 choiceFunc : function(f, i) {
		return { type : "tableCard",
				 choiceFunc : function(f, j) {
					sendAction(choiceAction(cmine(i, j)));
					return null;
				}
				}
			}
			}
}