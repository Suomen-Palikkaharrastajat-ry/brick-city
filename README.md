# Palikkakaupunki (Brick City)

**Palikkakaupunki (Brick City)** is an interactive 3D LEGO® microscale city builder by **Suomen Palikkaharrastajat ry**, deployed at [`https://kaupunki.palikkaharrastajat.fi/`](https://kaupunki.palikkaharrastajat.fi/).

Build individual **8×8 stud** microscale elements up to **32 plates tall** (`256` LDU), rotate and arrange them on a shared **4×4 to 8×8** city grid so roads and canals link up seamlessly, inspect the live 3D city diorama, and collaborate with friends in real time.

It runs on the shared Brick Frame engine and UI layer of `brick-studio` (git submodule `vendor/brick-studio`), bringing in family design tokens, Elm review rules, and brand assets from `master-builder` (`vendor/brick-studio/vendor/master-builder`).

## Features

- **Microscale 8×8 Lot Bounds**: Every lot enforces strict 8×8 stud footprint (`-80..80` LDU in X and Z), height limit `32` plates (`256` LDU above baseplate top), anchored `41539.dat` baseplate protection, and a translucent 32-plate guide cage in the editor.
- **Interactive 3D City Diorama**: Orbit, pan, zoom, switch to street-level `WASD` walk mode, click or double-click any lot, rotate lots 90° (`R`), clone/paste lots, resize the grid (`4×4`, `5×5`, `6×6`, `8×8`), and track live city metrics (population, buildings, green space %, road links, peak height `/32`, score).
- **3D Element Editor & Starter Templates**: Powered by `Studio.Core` (`brick-frame-studio`) with a curated microscale brick palette (plates, tiles, grille tiles, cheese slopes, bricks, trans-clear/blue windows, cones, trees, micro arches) and 9 built-in 8×8 starter elements (Straight Road, Corner Road, Intersection, Micro House, High-Rise Tower, Modern Skyscraper, Central Park, Industrial Plant, Waterfront Canal).
- **Persistence & Export**: Auto-saves to browser `localStorage`, supports JSON city export/import, multi-submodel `.mpd` LDraw export, and shareable `#b/...` element links.
- **Collaborative Rooms**: Host or join real-time shared cities at `#/city/:roomId` backed by Cloudflare signaling (`https://broadcast-signaling.palikkaharrastajat.workers.dev` or `VITE_SIGNALING_URL`).

## Quick Start

Using [devenv](https://devenv.sh):

```bash
# Enter devenv shell (or prefix commands with `devenv shell --`)
devenv shell

# Initialize git submodules and symlinks
make vendor

# Run formatter checks, elm-review, Elm unit tests, and JS tests
make test

# Start local dev server (http://localhost:5178)
make dev

# Production build (outputs to build/)
make build

# Preview production build (http://localhost:4178)
make preview
```

## Licensing & Trademarks

- Software is open source under the MIT License (see [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md)).
- LEGO® is a registered trademark of the LEGO Group of companies, which does not sponsor, authorize, or endorse this project.
- LDraw™ is a trademark owned and licensed by the Estate of James Jessiman.
