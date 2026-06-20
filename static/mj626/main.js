import "./pixi-export.js"

async function newClip(s) {
    return new Promise((resolve, reject) => {
        var synth = new jfxr.Synth(s);
        synth.run(function(clip) {
            resolve(clip);
        });
    });
}

async function fetchText(s) {
    try {
        const response = await fetch(s);
        const text = await response.text();
        return text;
    } catch (error) {
        console.error('fetchText Error:', error);
    }
}

window.newClip = newClip;
window.fetchText = fetchText;

import wasm_init from "./wasm-init.js"
wasm_init({
    onWasmUnsupported: () => {
        console.log("onWasmUnsupported");
    },
    onWasmLoadSuccess: () => {
        console.log("onWasmLoadSuccess");
    },
    onWasmLoadFailed: (error) => {
        console.error('Error loading WASM:', error);
        console.error('Error stack:', error.stack);
    }
});
