import './main.css'
import { initBricksRuntime } from './vendor/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { initThumbnails } from './vendor/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'
import {
  decodeHashToByteArray,
  readAudioMuted,
  readLanguage,
  setupGameRuntime,
} from './vendor/packages/brick-frame-game/runtime/game-runtime.js'
import { DEFAULT_SIGNALING_URL } from './vendor/packages/brick-frame-game/runtime/share.js'
import { createAudioEngine } from './audio.js'
import {
  createCitySyncManager,
  parseCityRoomFromHash,
  readStoredCityJson,
  roomShareHash,
} from './city-sync.js'
import { Elm } from './src/Main.elm'

const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'
const signalingUrl = (import.meta.env.VITE_SIGNALING_URL || DEFAULT_SIGNALING_URL).replace(/\/+$/, '')

document.documentElement.lang = readLanguage()

const coarsePointer =
  typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches

const initialHash = window.location.hash ?? ''
const initialRoomId = parseCityRoomFromHash(initialHash)
const savedAudioMuted = readAudioMuted('city')
const audioEngine = createAudioEngine(savedAudioMuted)

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'viewer',
  ldrawBase,
  indicesBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE || '/raw',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {
    coarsePointer,
    language: readLanguage(),
    savedAudioMuted,
    savedCityJson: readStoredCityJson(),
    initialRoomId,
    baseUrl: window.location.origin + window.location.pathname,
    predecodedInitialBytes: decodeHashToByteArray(initialHash),
  },
  initialHash,
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

const ports = runtime.app?.ports

setupGameRuntime(ports, {
  storagePrefix: 'city',
  audioEngine,
})

const citySync = createCitySyncManager(ports, signalingUrl)

if (initialRoomId) {
  setTimeout(() => {
    citySync.joinRoom({
      roomId: initialRoomId,
      name: `Rakentaja ${Math.floor(Math.random() * 900 + 100)}`,
    })
  }, 60)
}

window.addEventListener('hashchange', () => {
  const nextRoom = parseCityRoomFromHash(window.location.hash ?? '')
  if (nextRoom && nextRoom !== citySync.roomId) {
    citySync.joinRoom({
      roomId: nextRoom,
      name: `Rakentaja ${Math.floor(Math.random() * 900 + 100)}`,
    })
  }
})

ports?.storeLanguage?.subscribe((lang) => {
  document.documentElement.lang = lang
})

ports?.downloadCityFile?.subscribe(({ filename, content, mimeType }) => {
  try {
    const blob = new Blob([String(content ?? '')], {
      type: mimeType || 'text/plain;charset=utf-8',
    })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = filename || 'palikkakaupunki.mpd'
    document.body.appendChild(link)
    link.click()
    link.remove()
    URL.revokeObjectURL(url)
  } catch {
    // Download blocked or unavailable
  }
})

// Dedicated JSON city file picker (separate from initBricksRuntime's .ldr/.mpd/.io picker)
if (ports?.requestCityJsonImport) {
  const jsonInput = document.createElement('input')
  jsonInput.type = 'file'
  jsonInput.accept = '.json,application/json'
  jsonInput.style.display = 'none'
  document.body.appendChild(jsonInput)

  jsonInput.addEventListener('change', () => {
    const file = jsonInput.files?.[0]
    if (!file) return
    const reader = new FileReader()
    reader.onload = () => {
      ports?.cityJsonImported?.send(String(reader.result ?? ''))
    }
    reader.onerror = () => {
      ports?.cityRoomError?.send('Could not read selected JSON file.')
    }
    reader.readAsText(file)
  })

  ports.requestCityJsonImport.subscribe(() => {
    jsonInput.value = ''
    jsonInput.click()
  })
}

// Keep browser URL synced when hosting or leaving a room
ports?.createCityRoom?.subscribe(({ roomId }) => {
  if (roomId) {
    try {
      window.history.replaceState(null, '', roomShareHash(roomId))
    } catch {}
  }
})

ports?.leaveCityRoom?.subscribe(() => {
  try {
    if (parseCityRoomFromHash(window.location.hash)) {
      window.history.replaceState(null, '', '#')
    }
  } catch {}
})

initThumbnails(ports, ldrawBase)
