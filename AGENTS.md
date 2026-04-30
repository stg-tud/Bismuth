# AGENTS.md

- When writing or largely rewriting a file, keep a short note near the top of the file, after imports, saying the file is vibecoded.
- Keep such notes brief.
- Be concise in code comments and docs.

Known structures:
- `Modules/Channels/jvm/.../NioTCP.scala`: selector-based TCP transport; sniffs accepted sockets and can handle plain NioTCP and websocket connections concurrently.
- `Modules/Channels/shared/.../channels/ConnectionDetails.scala`: connection detail ADT; includes TCP, websocket, WebRTC, and local test transports.
- `Modules/Channels/shared/.../replication/overlay/HyParViewOverlay.scala`: multiplexed HyParView + Plumtree overlay node.
- `Modules/Channels/shared/.../replication/research/OverlayConnectionDirectory.scala`: OR-Map from node `Uid` to replicated sets of connected peers.
- `Modules/Channels/shared/.../replication/research/MerkleSearchTree.scala`: persistent Merkle search tree for timestamped encoded deltas.
- `Modules/Examples JVM/.../ex2026overlaydemo/OverlayDemo.scala`: central coordination server hands out topic connection details; clients/server run overlay nodes.
