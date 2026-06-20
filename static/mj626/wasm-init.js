import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

export default async ({ onWasmUnsupported, onWasmLoadSuccess, onWasmLoadFailed }) => {
    // Check for WebAssembly support
    if ("WebAssembly" in window) {
        const wasi = new WASI({
            stdout: (out) => console.log("[wasm stdout]", out),
            stderr: (out) => console.error("[wasm stderr]", out)
        });

        const jsffiExports = {};

        try {
            console.log('Creating JSFFI imports...');
            const jsffiImports = ghc_wasm_jsffi(jsffiExports);
            console.log('JSFFI imports created:', jsffiImports);

            console.log('Getting WASI import object...');
            const wasiImports = wasi.getImportObject();
            console.log('WASI imports:', Object.keys(wasiImports));

            const importObject = Object.assign(
                { ghc_wasm_jsffi: jsffiImports },
                wasiImports
            );
            console.log('Full import object:', Object.keys(importObject));

            console.log('Fetching and instantiating WASM...');
            const result = await WebAssembly.instantiateStreaming(
                fetch(`./main.wasm`),
                importObject
            );

            console.log('WASM instantiated, result:', result);
            console.log('Instance:', result.instance);

            if (!result || !result.instance) {
                throw new Error('Failed to get instance from instantiateStreaming');
            }

            const instance = result.instance;
            console.log('Instance exports:', Object.keys(instance.exports));

            // Fill in the jsffiExports with the instance exports for FFI to work
            Object.assign(jsffiExports, instance.exports);
            console.log('JSFFI exports filled:', Object.keys(jsffiExports));

            // Initialize the reactor module (instead of start)
            // wasi.initialize expects the full result object, not just the instance
            console.log('Initializing WASI...');
            wasi.initialize(result, {
                ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports)
            });
            console.log('WASI initialized');

            // Call the exported main function
            if (instance.exports.wasmMain) {
                console.log('Calling wasmMain...');
                instance.exports.wasmMain();
            } else {
                console.log('No wasmMain export found in test.wasm.');
                console.log('Available exports:', Object.keys(instance.exports));
            }
            onWasmLoadSuccess();
        } catch (error) {
            onWasmLoadFailed(error);
        }
    } else {
        onWasmUnsupported();
    }
}
