---
id: tui
title: Terminal UI Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The terminal UI formatter is one of the key features of Sandwich. It uses the awesome [Brick](https://hackage.haskell.org/package/brick) library to make an interactive UI for running (or re-running! ) tests and inspecting their results.

Most of the hotkeys are documented at the top of the UI and should be self-explanatory (see below). This page will contain some high-level comments about how it works and information about configuring it in code.

<img alt="Terminal UI" src={useBaseUrl('img/tui.png')} />

