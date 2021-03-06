//var types = require('./types');



// Represents the interpreter state.


var state = {};

(function () {

var DEBUG_ON = false;

var setDebug = function(v) {
    DEBUG_ON = v;
}

var debug = function(s) {
    if (DEBUG_ON) {
	sys.debug(s);
    }
}

var debugF = function(f_s) {
    if (DEBUG_ON) {
	sys.debug(f_s());
    }
}


var defaultPrintHook = function(thing) { 
    sys.print(types.toWrittenString(thing) + "\n"); };

var defaultDisplayHook = function(thing) { 
    sys.print(types.toDisplayedString(thing)); };

var defaultToplevelNodeHook = function() { 
    throw new Error("There is a software configuration error by the system's maintainer: the toplevel node has not been initialized yet.");
};

var defaultOnSuccess = function(v) {};
var defaultOnFail = function(e) { throw e };



// Interpreter
var State = function() {
    this.v = [];       // value register
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack
    this.heap = {};    // map from name to closures
    this.globals = {}; // map from string to types.GlobalBucket values
    this.hooks = { printHook: defaultPrintHook,
		   displayHook: defaultPrintHook,
		   toplevelNodeHook: defaultToplevelNodeHook };

    this.invokedModules = {};

    // If the interpreter is running, this flag is set to true.
    this.running = false;


    // Internal flag: if set, then we stop evaluation immediately.
    this.breakRequested = false;
    this.breakRequestedListeners = [];

    this.onSuccess = defaultOnSuccess;
    this.onFail = defaultOnFail;


    // Internal: how many steps in the interpreter loop before bouncing.
    // We bounce every so often to allow UI events to process.
    // This parameter is dynamically adjusted.
    this.MAX_STEPS_BEFORE_BOUNCE = 5000;
};


// clearForEval: -> void
// Clear out the value register, the vstack, and the cstack.
State.prototype.clearForEval = function(attrs) {
    this.v = [];
    this.vstack = [];
    this.cstack = [];

    this.onSuccess = defaultOnSuccess;
    this.onFail = defaultOnFail;


    // FIXME: what should happen to globals here?
    if (attrs && attrs.preserveBreak) {
    } else {
	this.breakRequested = false;
	this.breakRequestedListeners = [];
    }


    if (attrs && attrs.clearGlobals) {
	this.globals = {};
    } else {
    }
};


State.prototype.save = function() {
    return { v: this.v,
	     vstack: this.vstack.slice(0),
	     cstack: this.cstack.slice(0),
	     heap: this.heap,
	     globals: copyHash(this.globals),
             hooks: this.hooks,
	     breakRequested: this.breakRequested,
	     breakRequestedListeners: copyHash(this.breakRequestedListeners),
	     invokedModules: this.invokedModules,
	     onSuccess: this.onSuccess,
	     onFail: this.onFail };
};


var copyHash = function(hash) {
    var result = {};
    for (var key in hash) {
	if (hash.hasOwnProperty(key)) {
	    result[key] = hash[key];
	}
    }
    return result;
};


State.prototype.restore = function(params) {
    this.v = params.v;
    this.vstack = params.vstack.slice(0);
    this.cstack = params.cstack.slice(0);
    this.heap = params.heap;
    this.globals = copyHash(params.globals);
    this.hooks = params.hooks;
    // DELIBERATE: don't restore breakRequested
    // this.breakRequested = params.breakRequested;
    this.breakRequestListeners = copyHash(params.breakRequestListeners);
    this.invokedModules = params.invokedModules;
    this.onSuccess = params.onSuccess;
    this.onFail = params.onFail;
};


// Request a break
//
// BreakRequestedListeners will be notified.
State.prototype.requestBreak = function() {
    this.breakRequested = true;
    for (var i = 0; i < this.breakRequestedListeners.length; i++ ) {
	try {
	    this.breakRequestedListeners[i]();
	} catch(e) {
	    helpers.reportError(e);
	}
    }
};


State.prototype.addBreakRequestedListener = function(listener) {
    this.breakRequestedListeners.push(listener);
};



State.prototype.removeBreakRequestedListener = function(listener) {
    for (var i = this.breakRequestedListeners.length - 1 ; i >= 0; i--) {
	if (this.breakRequestedListeners[i] === listener) {
	    this.breakRequestedListeners.splice(i, 1);
	}
    }
};



// Pop a value.
State.prototype.popValue = function() {
//    debugF(function(){ return "popValue" });
    if (this.vstack.length === 0) {
 	throw types.internalError("vstack empty", captureCurrentContinuationMarks(this));
    }
    return this.vstack.pop();
};

// Push n undefined values onto the stack.
State.prototype.pushn = function(n) {
//    debugF(function(){ return "PUSHN " + n } );
    for (var i = 0; i < n; i++) {
	this.vstack.push(types.UNDEFINED);
    }
};

// Pop n values from the stack.
State.prototype.popn = function(n) {
//    debugF(function(){ return "POPN " + n } );
    if (this.vstack.length < n) {
	throw types.internalError("vstack empty", captureCurrentContinuationMarks(this));
    }
    this.vstack.splice(this.vstack.length - n, n);
};


// Peek at the nth value on the stack.
State.prototype.peekn = function(depth) {
    if (this.vstack.length - 1 - (depth || 0) < 0) {
	throw types.internalError("vstack not long enough", captureCurrentContinuationMarks(this));
    }
    return this.vstack[this.vstack.length - 1 - (depth || 0)];
};

// Set the nth value on the stack.
State.prototype.setn = function(depth, v) {
    this.vstack[this.vstack.length - 1 - (depth || 0)] = v;
};




// Reference an element of a prefix on the value stack.
State.prototype.refPrefix = function(depth, pos) {
    var value = this.vstack[this.vstack.length-1 - depth].ref(pos);
    if (value instanceof types.ModuleVariableRecord) {
	if (this.invokedModules[value.resolvedModuleName]) {
	    var moduleRecord =  this.invokedModules[value.resolvedModuleName];
	    if (typeof (moduleRecord.lookup(value.variableName)) !== 'undefined') {
		return moduleRecord.lookup(value.variableName);
	    } else {
		helpers.raise(types.incompleteExn(
		    types.exnFailContractVariable,
		    "reference to an identifier before its definition: " +
			value.variableName,
		    [false,
		     value.variableName]));
	    }
	} else {
	    helpers.raise(types.incompleteExn(
		types.exnFailContractVariable,
		"reference to an identifier whose module was not invoked: " +
		    value.variableName,
		[false,
		 value.variableName]));
	}
    }
    return value;
};


// Set an element of a prefix on the value stack.
State.prototype.setPrefix = function(depth, pos, v) {
//    debug("setPrefix");
    this.vstack[this.vstack.length - 1 - depth].set(pos, v);
};




State.prototype.setPrintHook = function(printHook) {
    this.hooks['printHook'] = printHook;
};


State.prototype.getPrintHook = function() {
    return this.hooks['printHook'];
};


State.prototype.setDisplayHook = function(printHook) {
    this.hooks['displayHook'] = printHook;
};


State.prototype.getDisplayHook = function() {
    return this.hooks['displayHook'];
};


State.prototype.getToplevelNodeHook = function() {
    return this.hooks['toplevelNodeHook'];
};


State.prototype.setToplevelNodeHook = function(hook) {
    this.hooks['toplevelNodeHook'] = hook;
};




// Captures the current continuation marks in the state.
// Helper function
var captureCurrentContinuationMarks = function(state) {
    var dict = types.makeLowLevelEqHash();
    for (var i = state.cstack.length - 1; i >= 0; i--) {
	if (state.cstack[i] instanceof control.ContMarkRecordControl) {
	    var listOfPairs = state.cstack[i].listOfPairs;
	    while (listOfPairs !== types.EMPTY) {
		var nextPair = listOfPairs.first;
		dict.put(nextPair.first, dict.get(nextPair.first) || []);
		dict.get(nextPair.first).push(nextPair.rest);
		listOfPairs = listOfPairs.rest;
	    }
	}
    }
    return types.continuationMarkSet(dict);
};




var STACK_KEY = types.symbol("moby-stack-record-continuation-mark-key");


// The core of a contination-mark-set->context.
var getStackTraceFromContinuationMarks = function(contMarkSet) {
    var results = [];
    var stackTrace = contMarkSet.ref(STACK_KEY);
    stackTrace.reverse();

    // KLUDGE: the first element in the stack trace
    // can be weird print-values may introduce a duplicate
    // location.
    for (var i = stackTrace.length - 1; 
	 i >= 0; i--) {
	var callRecord = stackTrace[i];
	var procName = callRecord.ref(0);
	var id = callRecord.ref(1);
	var offset = callRecord.ref(2);
	var line = callRecord.ref(3);
	var column = callRecord.ref(4);
	var span = callRecord.ref(5);
	var newHash = {'procName' : procName,
		       'id': id, 
		       'offset': offset,
		       'line': line, 
		       'column': column,
		       'span': span};
	if (results.length === 0 ||
	    (! isEqualHash(results[results.length-1],
			   newHash))) {
	    results.push(newHash);
	}
    }
    return results;
};



var isEqualHash = function(hash1, hash2) {
    for (var key in hash1) {
	if (hash1.hasOwnProperty(key)) {
	    if (hash2.hasOwnProperty(key)) {
		if (hash1[key] !== hash2[key]) {
		    return false;
		}
	    } else {
		return false;
	    }
	}
    }
    for (var key in hash2) {
	if (hash2.hasOwnProperty(key)) {
	    if (hash1.hasOwnProperty(key)) {
		if (hash1[key] !== hash2[key]) {
		    return false;
		}
	    } else {
		return false;
	    }
	}
    }
    return true;
};



var captureContinuationClosure = function(state) {
    return new types.ContinuationClosureValue(state.vstack,
					      state.cstack);
};







state.State = State;
state.captureCurrentContinuationMarks = captureCurrentContinuationMarks;
state.captureContinuationClosure = captureContinuationClosure;
state.getStackTraceFromContinuationMarks = getStackTraceFromContinuationMarks;


})();
