// Loopback tests for Brick City collaborative room sync & localStorage persistence.
import assert from 'node:assert/strict'
import test from 'node:test'
import {
  CITY_SAVED_AT_KEY,
  CITY_STORAGE_KEY,
  createCitySyncManager,
  isRoomHostedLocally,
  parseCityRoomFromHash,
  readStoredCityJson,
  readStoredCitySavedAt,
  roomShareHash,
  setRoomHostedLocally,
  writeStoredCityJson,
} from './city-sync.js'

const tick = () => new Promise((resolve) => setTimeout(resolve, 0))
async function settle(rounds = 15) {
  for (let i = 0; i < rounds; i++) await tick()
}

function createMemoryStorage() {
  const data = new Map()
  return {
    getItem: (k) => (data.has(k) ? data.get(k) : null),
    setItem: (k, v) => data.set(k, String(v)),
    removeItem: (k) => data.delete(k),
  }
}

function createHub() {
  const hub = {
    broadcaster: null,
    viewers: new Map(),
    nextViewer: 1,
  }

  class FakeBroadcaster {
    constructor(_url, opts) {
      this.opts = opts
      this.latestBytes = undefined
      hub.broadcaster = this
    }
    async start() {}
    connectViewer(viewerId, fakeViewer) {
      if (this.latestBytes !== undefined) {
        this.deliver(viewerId, this.latestBytes)
      }
      fakeViewer.open()
    }
    deliver(viewerId, msg) {
      const v = hub.viewers.get(viewerId)
      if (!v) return false
      const clone = JSON.parse(JSON.stringify(msg))
      queueMicrotask(() => v.opts.onSnapshot(clone))
      return true
    }
    push(msg) {
      this.latestBytes = msg
      for (const id of hub.viewers.keys()) {
        this.deliver(id, msg)
      }
    }
    sendTo(viewerId, msg) {
      return this.deliver(viewerId, msg)
    }
    stop() {
      hub.broadcaster = null
    }
  }

  class FakeViewer {
    constructor(_url, _room, opts) {
      this.opts = opts
      this.viewerId = `v${hub.nextViewer++}`
      this.isOpen = false
    }
    async start() {
      hub.viewers.set(this.viewerId, this)
      queueMicrotask(() => hub.broadcaster?.connectViewer(this.viewerId, this))
    }
    open() {
      this.isOpen = true
      this.opts.onChannelOpen?.()
    }
    sendMessage(msg) {
      if (!this.isOpen || !hub.broadcaster) return false
      const clone = JSON.parse(JSON.stringify(msg))
      const b = hub.broadcaster
      queueMicrotask(() => b.opts.onParticipantMessage(this.viewerId, clone))
      return true
    }
    stop() {
      hub.viewers.delete(this.viewerId)
      hub.broadcaster?.opts.onParticipantLeave(this.viewerId)
    }
  }

  return { hub, FakeBroadcaster, FakeViewer }
}

function makeFakePorts() {
  const events = {
    roomStates: [],
    snapshots: [],
    lotUpdates: [],
    errors: [],
  }
  const ports = {
    cityRoomStateChanged: { send: (v) => events.roomStates.push(v) },
    cityRemoteSnapshot: { send: (v) => events.snapshots.push(v) },
    cityRemoteLotUpdated: { send: (v) => events.lotUpdates.push(v) },
    cityRoomError: { send: (v) => events.errors.push(v) },
  }
  return { events, ports }
}

test('parseCityRoomFromHash parses #/city/:roomId and legacy room query hashes', () => {
  assert.equal(parseCityRoomFromHash('#/city/ab12cd'), 'AB12CD')
  assert.equal(parseCityRoomFromHash('https://kaupunki.palikkaharrastajat.fi/#/city/k7m9p2'), 'K7M9P2')
  assert.equal(parseCityRoomFromHash('https://kaupunki.palikkaharrastajat.fi/?room=abcd'), 'ABCD')
  assert.equal(parseCityRoomFromHash('#room=city42'), 'CITY42')
  assert.equal(parseCityRoomFromHash('#join=xy99'), 'XY99')
  assert.equal(parseCityRoomFromHash('#b/abc'), '')
  assert.equal(roomShareHash('ab12cd'), '#/city/AB12CD')
})

test('localStorage helpers read and write city JSON and savedAt timestamp safely', () => {
  const storage = createMemoryStorage()
  assert.equal(readStoredCityJson(storage), '')
  assert.equal(readStoredCitySavedAt(storage), 0)
  assert.equal(writeStoredCityJson('{"cityName":"Test"}', storage), true)
  assert.equal(storage.getItem(CITY_STORAGE_KEY), '{"cityName":"Test"}')
  assert.equal(readStoredCityJson(storage), '{"cityName":"Test"}')
  assert.equal(Number(storage.getItem(CITY_SAVED_AT_KEY)) > 0, true)
  assert.equal(readStoredCitySavedAt(storage) > 0, true)
})

test('host creates room, guest receives city snapshot, and lot edits synchronize both ways', async () => {
  const { FakeBroadcaster, FakeViewer } = createHub()
  const hostPorts = makeFakePorts()
  const guestPorts = makeFakePorts()
  const hostStorage = createMemoryStorage()
  const guestStorage = createMemoryStorage()

  const host = createCitySyncManager(hostPorts.ports, 'http://signaling.invalid', {
    Broadcaster: FakeBroadcaster,
    Viewer: FakeViewer,
    storage: hostStorage,
  })
  const guest = createCitySyncManager(guestPorts.ports, 'http://signaling.invalid', {
    Broadcaster: FakeBroadcaster,
    Viewer: FakeViewer,
    storage: guestStorage,
  })

  const initialCity = {
    cityName: 'Tampere Micro',
    cols: 4,
    rows: 4,
    lots: {
      '0,0': {
        id: 'el-1',
        name: 'Central Park',
        category: 'park',
        ldrawText: '1 2 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat',
        rotation: 0,
        updatedAt: 100,
        updatedBy: 'Host',
        partCount: 1,
        heightPlates: 0,
      },
    },
  }

  const roomId = host.createRoom({ roomId: 'CITY01', name: 'Mayor', city: initialCity })
  assert.equal(roomId, 'CITY01')
  await settle()

  guest.joinRoom({ roomId: 'CITY01', name: 'Architect' })
  await settle()

  assert.equal(guestPorts.events.snapshots.length >= 1, true)
  const latestGuestSnap = guestPorts.events.snapshots.at(-1)
  assert.equal(latestGuestSnap.cityName, 'Tampere Micro')
  assert.equal(latestGuestSnap.lots['0,0'].name, 'Central Park')

  const hostLatestState = hostPorts.events.roomStates.at(-1)
  assert.equal(hostLatestState.peers.length, 2)
  assert.equal(hostLatestState.peers[1].name, 'Architect')

  // Guest updates lot (1, 2)
  guest.sendLotUpdate({
    row: 1,
    col: 2,
    element: {
      id: 'el-2',
      name: 'Skyscraper',
      category: 'commercial',
      ldrawText: '1 15 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat',
      rotation: 1,
      updatedAt: 200,
      updatedBy: 'Architect',
      partCount: 1,
      heightPlates: 28,
    },
  })
  await settle()

  assert.equal(hostPorts.events.lotUpdates.length, 1)
  assert.equal(hostPorts.events.lotUpdates[0].row, 1)
  assert.equal(hostPorts.events.lotUpdates[0].col, 2)
  assert.equal(hostPorts.events.lotUpdates[0].element.name, 'Skyscraper')
  assert.equal(host.snapshot.lots['1,2'].name, 'Skyscraper')

  // Host updates lot (2, 1)
  host.sendLotUpdate({
    row: 2,
    col: 1,
    element: {
      id: 'el-3',
      name: 'Road',
      category: 'road',
      ldrawText: '1 72 0 0 0 1 0 0 0 1 0 0 0 1 41539.dat',
      rotation: 0,
      updatedAt: 300,
      updatedBy: 'Mayor',
      partCount: 1,
      heightPlates: 1,
    },
  })
  await settle()

  assert.equal(guestPorts.events.lotUpdates.length, 1)
  assert.equal(guestPorts.events.lotUpdates[0].element.name, 'Road')
  assert.equal(guest.snapshot.lots['2,1'].name, 'Road')

  // Focus tracking syncs across peers
  guest.sendLotFocus({ row: 1, col: 2 })
  await settle()
  assert.deepEqual(hostPorts.events.roomStates.at(-1).peers[1].focusedLot, { row: 1, col: 2 })

  host.leaveRoom()
  guest.leaveRoom()
})

test('hosted room tracking persists created rooms in storage', () => {
  const storage = createMemoryStorage()
  assert.equal(isRoomHostedLocally('CITY99', storage), false)
  assert.equal(setRoomHostedLocally('CITY99', true, storage), true)
  assert.equal(isRoomHostedLocally('CITY99', storage), true)
  assert.equal(isRoomHostedLocally('city99', storage), true)
  assert.equal(setRoomHostedLocally('CITY99', false, storage), true)
  assert.equal(isRoomHostedLocally('CITY99', storage), false)
})

test('joinRoom times out cleanly when room host is offline instead of hanging indefinitely', async () => {
  class SilentViewer {
    constructor(_url, _room, opts) {
      this.opts = opts
      this.stopped = false
    }
    async start() {
      // Connects WebSocket but broadcaster is offline, so no offer/welcome ever arrives
    }
    stop() {
      this.stopped = true
    }
  }

  const { ports, events } = makeFakePorts()
  const storage = createMemoryStorage()
  const guest = createCitySyncManager(ports, 'http://signaling.invalid', {
    Viewer: SilentViewer,
    storage,
    connectTimeoutMs: 50,
  })

  guest.joinRoom({ roomId: 'VACANT1', name: 'Explorer', timeoutMs: 50 })
  assert.equal(events.roomStates.at(-1).status, 'connecting')

  // Wait for connectTimeoutMs
  await new Promise((r) => setTimeout(r, 80))
  await settle()

  assert.equal(events.roomStates.at(-1).status, 'disconnected')
  assert.equal(events.errors.length >= 1, true)
  assert.match(events.errors.at(-1), /isäntä/i)

  // Guest can now claim the vacant room as host
  const claimedId = guest.createRoom({
    roomId: 'VACANT1',
    name: 'NewMayor',
    city: { cityName: 'Claimed City', cols: 4, rows: 4, lots: {}, version: 1 },
  })
  assert.equal(claimedId, 'VACANT1')
  assert.equal(isRoomHostedLocally('VACANT1', storage), true)
  guest.leaveRoom()
})
