/**
 * Collaborative City Room Sync & Local Persistence for Brick City (`brick-city`).
 *
 * Uses the Brick Family Cloudflare Worker signaling service (`notes/signaling`)
 * via `Broadcaster` and `Viewer` from `brick-frame-game/runtime/share.js` to
 * synchronize 8x8 city lots, grid dimensions, city metadata, and live builder
 * focus across players in real time (`#/city/:roomId`).
 *
 * Local solo state and room backups are persisted in `localStorage`.
 */

import {
  Broadcaster,
  Viewer,
  randomRoomId,
  DEFAULT_SIGNALING_URL,
} from './vendor/packages/brick-frame-game/runtime/share.js'

export const CITY_STORAGE_KEY = 'palikkakehys.city.current'
export const CITY_ROOM_CACHE_PREFIX = 'palikkakehys.city.room.'
export const MAX_ROOM_CAPACITY = 8

export const SLOT_COLORS = [
  '#0284c7',
  '#ef4444',
  '#eab308',
  '#10b981',
  '#a855f7',
  '#f97316',
  '#06b6d4',
  '#ec4899',
]

export function slotColor(slot) {
  const index = Math.max(0, (Number(slot) || 1) - 1) % SLOT_COLORS.length
  return SLOT_COLORS[index]
}

/**
 * Parse a Brick City room code from `#/city/:roomId`, `#city=:roomId`,
 * `#room=:roomId`, or `#join=:roomId`. Returns uppercase 4..12 char code or ''.
 */
export function parseCityRoomFromHash(rawInput) {
  const str = String(rawInput ?? '').trim()
  if (!str) return ''
  const pathMatch = str.match(/#\/city\/([A-Za-z0-9]{4,12})(?:[/?#&]|$)/i)
  if (pathMatch && pathMatch[1]) {
    return pathMatch[1].toUpperCase()
  }
  const paramMatch = str.match(/[#&](?:city|room|join)=([A-Za-z0-9]{4,12})(?:[&#]|$)/i)
  if (paramMatch && paramMatch[1]) {
    return paramMatch[1].toUpperCase()
  }
  return ''
}

export function roomShareHash(roomId) {
  const clean = String(roomId ?? '').trim().toUpperCase()
  return clean ? `#/city/${clean}` : ''
}

export function readStoredCityJson(storage = typeof window !== 'undefined' ? window.localStorage : null) {
  try {
    if (!storage) return ''
    return storage.getItem(CITY_STORAGE_KEY) || ''
  } catch {
    return ''
  }
}

export function writeStoredCityJson(
  jsonText,
  storage = typeof window !== 'undefined' ? window.localStorage : null,
) {
  try {
    if (!storage) return false
    storage.setItem(CITY_STORAGE_KEY, String(jsonText ?? ''))
    return true
  } catch {
    return false
  }
}

export function readCachedRoomCity(
  roomId,
  storage = typeof window !== 'undefined' ? window.localStorage : null,
) {
  try {
    if (!storage || !roomId) return null
    const raw = storage.getItem(`${CITY_ROOM_CACHE_PREFIX}${String(roomId).toUpperCase()}`)
    return raw ? JSON.parse(raw) : null
  } catch {
    return null
  }
}

export function writeCachedRoomCity(
  roomId,
  snapshot,
  storage = typeof window !== 'undefined' ? window.localStorage : null,
) {
  try {
    if (!storage || !roomId || !snapshot) return false
    storage.setItem(
      `${CITY_ROOM_CACHE_PREFIX}${String(roomId).toUpperCase()}`,
      JSON.stringify(snapshot),
    )
    return true
  } catch {
    return false
  }
}

function lotKey(row, col) {
  return `${row},${col}`
}

function normalizeSnapshot(raw) {
  if (!raw || typeof raw !== 'object') {
    return { cityName: 'Palikkakaupunki', cols: 4, rows: 4, lots: {}, version: 0 }
  }
  const cols = Math.max(2, Math.min(8, Math.floor(Number(raw.cols) || 4)))
  const rows = Math.max(2, Math.min(8, Math.floor(Number(raw.rows) || 4)))
  const lots = {}
  if (raw.lots && typeof raw.lots === 'object' && !Array.isArray(raw.lots)) {
    for (const [key, val] of Object.entries(raw.lots)) {
      if (val && typeof val === 'object') {
        lots[key] = val
      }
    }
  } else if (Array.isArray(raw.lots)) {
    for (const entry of raw.lots) {
      if (entry && Number.isInteger(entry.row) && Number.isInteger(entry.col) && entry.element) {
        lots[lotKey(entry.row, entry.col)] = entry.element
      }
    }
  }
  return {
    cityName: typeof raw.cityName === 'string' && raw.cityName.trim() ? raw.cityName : 'Palikkakaupunki',
    cols,
    rows,
    lots,
    version: Math.max(0, Math.floor(Number(raw.version) || 0)),
  }
}

export function createCitySyncManager(ports, signalingUrl = DEFAULT_SIGNALING_URL, options = {}) {
  const BroadcasterClass = options.Broadcaster || Broadcaster
  const ViewerClass = options.Viewer || Viewer
  const storage =
    options.storage !== undefined
      ? options.storage
      : typeof window !== 'undefined'
        ? window.localStorage
        : null

  let broadcaster = null
  let viewer = null
  let role = null // 'host' | 'guest' | null
  let roomId = ''
  let status = 'disconnected' // 'disconnected' | 'connecting' | 'connected'
  let mySlot = 1
  let myName = 'Rakentaja'
  let myFocusedLot = null
  let session = 0
  let snapshot = normalizeSnapshot(null)

  const peers = new Map() // slot -> { slot, id, name, color, focusedLot }
  const viewerSlotMap = new Map() // viewerId -> slot

  function reportError(message) {
    ports?.cityRoomError?.send(String(message || 'Room connection error'))
  }

  function peerList() {
    return Array.from(peers.values()).sort((a, b) => a.slot - b.slot)
  }

  function emitRoomState() {
    ports?.cityRoomStateChanged?.send({
      roomId,
      role: role || '',
      status: roomId ? status : 'disconnected',
      mySlot,
      myName,
      peers: peerList(),
    })
  }

  function emitSnapshotToElm() {
    ports?.cityRemoteSnapshot?.send(snapshot)
  }

  function emitLotToElm(row, col, element, senderSlot) {
    ports?.cityRemoteLotUpdated?.send({
      row,
      col,
      element: element ?? null,
      senderSlot: Number(senderSlot) || 0,
    })
  }

  function persistRoomSnapshot() {
    if (roomId) {
      writeCachedRoomCity(roomId, snapshot, storage)
    }
  }

  function roomStatePayload() {
    return {
      type: 'room_state',
      roomId,
      peers: peerList(),
    }
  }

  function snapshotPayload() {
    return {
      type: 'city_snapshot',
      roomId,
      snapshot,
      peers: peerList(),
    }
  }

  function allocateSlot(viewerId) {
    const existing = viewerSlotMap.get(viewerId)
    if (existing) return existing
    for (let s = 2; s <= MAX_ROOM_CAPACITY; s++) {
      if (!peers.has(s)) return s
    }
    return null
  }

  function applyLotMutation(row, col, element) {
    if (!Number.isInteger(row) || !Number.isInteger(col)) return false
    if (row < 0 || col < 0 || row >= snapshot.rows || col >= snapshot.cols) return false
    const key = lotKey(row, col)
    const current = snapshot.lots[key] || null

    if (element === null || element === undefined) {
      delete snapshot.lots[key]
      snapshot.version += 1
      persistRoomSnapshot()
      return true
    }

    if (typeof element !== 'object') return false
    const nextUpdatedAt = Number(element.updatedAt) || 0
    const currUpdatedAt = Number(current?.updatedAt) || 0
    if (current && nextUpdatedAt > 0 && currUpdatedAt > 0 && nextUpdatedAt < currUpdatedAt) {
      return false
    }

    snapshot.lots[key] = element
    snapshot.version += 1
    persistRoomSnapshot()
    return true
  }

  function broadcastToGuests(msg) {
    if (!broadcaster) return
    try {
      broadcaster.push(msg)
    } catch {
      // Ignore transient DataChannel send errors
    }
  }

  function handleGuestMessage(viewerId, msg) {
    if (!msg || typeof msg !== 'object') return

    if (msg.type === 'join') {
      const assignedSlot = allocateSlot(viewerId)
      if (!assignedSlot) {
        broadcaster?.sendTo(viewerId, {
          type: 'room_error',
          message: `Room is full (max ${MAX_ROOM_CAPACITY} builders).`,
        })
        return
      }
      viewerSlotMap.set(viewerId, assignedSlot)
      const guestName =
        typeof msg.name === 'string' && msg.name.trim()
          ? msg.name.trim().slice(0, 24)
          : `Builder ${assignedSlot}`

      peers.set(assignedSlot, {
        slot: assignedSlot,
        id: viewerId,
        name: guestName,
        color: slotColor(assignedSlot),
        focusedLot: msg.focusedLot || null,
      })

      broadcaster?.sendTo(viewerId, {
        type: 'welcome',
        roomId,
        assignedSlot,
        snapshot,
        peers: peerList(),
      })
      broadcastToGuests(roomStatePayload())
      emitRoomState()
      return
    }

    const slot = viewerSlotMap.get(viewerId)
    if (!slot) return

    if (msg.type === 'focus_lot') {
      const peer = peers.get(slot)
      if (peer) {
        peer.focusedLot = msg.focusedLot || null
        broadcastToGuests(roomStatePayload())
        emitRoomState()
      }
      return
    }

    if (msg.type === 'lot_update') {
      const row = Number(msg.row)
      const col = Number(msg.col)
      if (applyLotMutation(row, col, msg.element ?? null)) {
        broadcaster.latestBytes = snapshotPayload()
        broadcastToGuests({
          type: 'lot_update',
          row,
          col,
          element: msg.element ?? null,
          senderSlot: slot,
          version: snapshot.version,
        })
        emitLotToElm(row, col, msg.element ?? null, slot)
      }
      return
    }

    if (msg.type === 'city_replace' && msg.snapshot) {
      snapshot = normalizeSnapshot(msg.snapshot)
      snapshot.version += 1
      persistRoomSnapshot()
      const payload = snapshotPayload()
      broadcaster.latestBytes = payload
      broadcastToGuests(payload)
      emitSnapshotToElm()
    }
  }

  function handleGuestLeave(viewerId) {
    const slot = viewerSlotMap.get(viewerId)
    if (!slot) return
    viewerSlotMap.delete(viewerId)
    peers.delete(slot)
    broadcastToGuests(roomStatePayload())
    emitRoomState()
  }

  function handleHostMessage(msg) {
    if (!msg || typeof msg !== 'object') return

    if (msg.type === 'room_error') {
      reportError(msg.message)
      return
    }

    if (msg.type === 'welcome') {
      if (Number.isInteger(msg.assignedSlot)) {
        mySlot = msg.assignedSlot
      }
      status = 'connected'
      if (Array.isArray(msg.peers)) {
        peers.clear()
        for (const p of msg.peers) {
          if (p && Number.isInteger(p.slot)) peers.set(p.slot, p)
        }
      }
      if (msg.snapshot) {
        snapshot = normalizeSnapshot(msg.snapshot)
        persistRoomSnapshot()
        emitSnapshotToElm()
      }
      emitRoomState()
      return
    }

    if (msg.type === 'city_snapshot') {
      status = 'connected'
      if (Array.isArray(msg.peers)) {
        peers.clear()
        for (const p of msg.peers) {
          if (p && Number.isInteger(p.slot)) peers.set(p.slot, p)
        }
      }
      if (msg.snapshot) {
        snapshot = normalizeSnapshot(msg.snapshot)
        persistRoomSnapshot()
        emitSnapshotToElm()
      }
      emitRoomState()
      return
    }

    if (msg.type === 'room_state') {
      if (Array.isArray(msg.peers)) {
        peers.clear()
        for (const p of msg.peers) {
          if (p && Number.isInteger(p.slot)) peers.set(p.slot, p)
        }
      }
      emitRoomState()
      return
    }

    if (msg.type === 'lot_update') {
      const row = Number(msg.row)
      const col = Number(msg.col)
      if (Number(msg.senderSlot) === mySlot) return
      if (applyLotMutation(row, col, msg.element ?? null)) {
        emitLotToElm(row, col, msg.element ?? null, msg.senderSlot)
      }
    }
  }

  function cleanup() {
    session++
    if (broadcaster) {
      try {
        broadcaster.stop()
      } catch {}
      broadcaster = null
    }
    if (viewer) {
      try {
        viewer.stop()
      } catch {}
      viewer = null
    }
    peers.clear()
    viewerSlotMap.clear()
    role = null
    roomId = ''
    status = 'disconnected'
    mySlot = 1
  }

  function createRoom({ roomId: requestedId, name, city } = {}) {
    cleanup()
    const mySession = session
    role = 'host'
    roomId = parseCityRoomFromHash(`#room=${requestedId || ''}`) || randomRoomId(6)
    mySlot = 1
    myName = typeof name === 'string' && name.trim() ? name.trim().slice(0, 24) : 'Host'
    status = 'connecting'

    const cached = !city ? readCachedRoomCity(roomId, storage) : null
    snapshot = normalizeSnapshot(city || cached)
    persistRoomSnapshot()

    peers.set(1, {
      slot: 1,
      id: 'host',
      name: myName,
      color: slotColor(1),
      focusedLot: myFocusedLot,
    })

    emitRoomState()

    broadcaster = new BroadcasterClass(signalingUrl, {
      roomId,
      onParticipantMessage: (viewerId, msg) => handleGuestMessage(viewerId, msg),
      onParticipantLeave: (viewerId) => handleGuestLeave(viewerId),
      onError: (err) => {
        if (session !== mySession) return
        reportError(err?.message || err)
      },
    })
    broadcaster.latestBytes = snapshotPayload()

    broadcaster
      .start()
      .then(() => {
        if (session !== mySession) return
        status = 'connected'
        emitRoomState()
      })
      .catch((err) => {
        if (session !== mySession) return
        status = 'disconnected'
        emitRoomState()
        reportError(err?.message || 'Could not create collaborative city room')
      })

    return roomId
  }

  function joinRoom({ roomId: targetId, name } = {}) {
    const cleanId =
      parseCityRoomFromHash(targetId) ||
      String(targetId ?? '')
        .toUpperCase()
        .replace(/[^A-Z0-9]/g, '')
        .slice(0, 12)
    if (!cleanId || cleanId.length < 4) {
      reportError('Enter a valid 4–12 character room code.')
      return ''
    }

    cleanup()
    const mySession = session
    role = 'guest'
    roomId = cleanId
    mySlot = 0
    myName = typeof name === 'string' && name.trim() ? name.trim().slice(0, 24) : 'Builder'
    status = 'connecting'
    emitRoomState()

    viewer = new ViewerClass(signalingUrl, roomId, {
      onSnapshot: (msg) => {
        if (session !== mySession) return
        handleHostMessage(msg)
      },
      onChannelOpen: () => {
        if (session !== mySession || !viewer) return
        viewer.sendMessage({
          type: 'join',
          name: myName,
          focusedLot: myFocusedLot,
        })
      },
      onStatus: (s) => {
        if (session !== mySession) return
        if (s === 'ended') {
          status = 'disconnected'
          emitRoomState()
        }
      },
      onError: (err) => {
        if (session !== mySession) return
        reportError(err?.message || err)
      },
    })

    viewer.start().catch((err) => {
      if (session !== mySession) return
      status = 'disconnected'
      emitRoomState()
      reportError(err?.message || 'Could not join city room')
    })

    return roomId
  }

  function leaveRoom() {
    cleanup()
    emitRoomState()
  }

  function sendLotUpdate(payload) {
    if (!payload || typeof payload !== 'object') return
    const row = Number(payload.row)
    const col = Number(payload.col)
    const element = payload.element ?? null

    if (role === 'host') {
      if (applyLotMutation(row, col, element)) {
        if (broadcaster) {
          broadcaster.latestBytes = snapshotPayload()
          broadcastToGuests({
            type: 'lot_update',
            row,
            col,
            element,
            senderSlot: mySlot,
            version: snapshot.version,
          })
        }
      }
    } else if (role === 'guest' && viewer) {
      applyLotMutation(row, col, element)
      viewer.sendMessage({
        type: 'lot_update',
        row,
        col,
        element,
      })
    }
  }

  function sendCitySnapshot(nextCity) {
    const normalized = normalizeSnapshot(nextCity)
    if (role === 'host') {
      normalized.version = snapshot.version + 1
      snapshot = normalized
      persistRoomSnapshot()
      if (broadcaster) {
        const payload = snapshotPayload()
        broadcaster.latestBytes = payload
        broadcastToGuests(payload)
      }
    } else if (role === 'guest' && viewer) {
      snapshot = normalized
      viewer.sendMessage({
        type: 'city_replace',
        snapshot: normalized,
      })
    }
  }

  function sendLotFocus(focusedLot) {
    myFocusedLot = focusedLot || null
    if (role === 'host') {
      const me = peers.get(1)
      if (me) {
        me.focusedLot = myFocusedLot
        broadcastToGuests(roomStatePayload())
        emitRoomState()
      }
    } else if (role === 'guest' && viewer) {
      viewer.sendMessage({
        type: 'focus_lot',
        focusedLot: myFocusedLot,
      })
    }
  }

  // Wire Elm ports
  ports?.saveCity?.subscribe((jsonText) => {
    writeStoredCityJson(jsonText, storage)
  })
  ports?.createCityRoom?.subscribe((req) => createRoom(req))
  ports?.joinCityRoom?.subscribe((req) => joinRoom(req))
  ports?.leaveCityRoom?.subscribe(() => leaveRoom())
  ports?.sendLotUpdate?.subscribe((payload) => sendLotUpdate(payload))
  ports?.sendCitySnapshot?.subscribe((city) => sendCitySnapshot(city))
  ports?.sendLotFocus?.subscribe((lot) => sendLotFocus(lot))

  return {
    createRoom,
    joinRoom,
    leaveRoom,
    sendLotUpdate,
    sendCitySnapshot,
    sendLotFocus,
    cleanup,
    get snapshot() {
      return snapshot
    },
    get roomId() {
      return roomId
    },
    get role() {
      return role
    },
  }
}
