// ==UserScript==
// @name         Readability Extractor on Hotkey
// @namespace    http://tampermonkey.net/
// @version      0.2
// @description  Press Alt+R to extract readable text from the page using Readability.js
// @author       You
// @match        *://*/*
// @grant        GM_addStyle
// ==/UserScript==

(function () {
    'use strict';

    let loaded = false;

    // Inject styles for overlay once
    GM_addStyle(`
        #readability-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            overflow: auto;
            background: white;
            color: black;
            font-family: sans-serif;
            font-size: 16px;
            padding: 2em;
            z-index: 99999;
        }
    `);

    // Listen for hotkey: Alt+R
    window.addEventListener('keydown', async function (e) {
        if (e.altKey && e.key.toLowerCase() === 'r') {
            e.preventDefault();

            if (!loaded) {
                await loadReadability();
                loaded = true;
            }

            const article = new Readability(document.cloneNode(true)).parse();

            const overlay = document.createElement('div');
            overlay.id = 'readability-overlay';
            overlay.innerText = article.textContent;
            document.body.appendChild(overlay);

            overlay.addEventListener('click', () => overlay.remove()); // click anywhere to dismiss
        }
    });

    // Load readability.js dynamically
    function loadReadability() {
        return new Promise((resolve) => {
            const script = document.createElement('script');
            script.src = 'https://unpkg.com/@mozilla/readability@0.4.4/Readability.js';
            script.onload = resolve;
            document.body.appendChild(script);
        });
    }
})();
