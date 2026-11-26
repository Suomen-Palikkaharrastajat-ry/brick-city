/**
 * Procedural Web Audio synthesizer for Brick City.
 * Extends BaseAudioEngine with crisp brick snaps, lot placement thuds,
 * template chimes, room join pings, and soft validation alerts.
 */

import { BaseAudioEngine } from './vendor/packages/brick-frame-game/runtime/audio-engine.js'

export function createAudioEngine(initialMuted = false) {
  const engine = new BaseAudioEngine(initialMuted)

  engine.playSfx = function (event) {
    if (engine.muted || !engine.ensureContext()) return
    const type = typeof event === 'string' ? event : event?.type

    if (type === 'snap' || type === 'place') {
      engine.playTone(920, 0.045, 0, 'triangle', 0.22)
      engine.playTone(1480, 0.035, 0.012, 'sine', 0.14)
    } else if (type === 'delete' || type === 'clear') {
      engine.playTone(340, 0.08, 0, 'triangle', 0.2)
      engine.playTone(210, 0.09, 0.03, 'sine', 0.14)
    } else if (type === 'save' || type === 'lotSaved') {
      engine.playTone(523.25, 0.12, 0, 'sine', 0.2)
      engine.playTone(659.25, 0.14, 0.07, 'sine', 0.22)
      engine.playTone(783.99, 0.22, 0.14, 'sine', 0.24)
    } else if (type === 'template') {
      engine.playTone(440, 0.09, 0, 'triangle', 0.18)
      engine.playTone(554.37, 0.09, 0.05, 'triangle', 0.18)
      engine.playTone(659.25, 0.15, 0.1, 'triangle', 0.22)
    } else if (type === 'join') {
      engine.playTone(587.33, 0.12, 0, 'sine', 0.18)
      engine.playTone(880, 0.18, 0.08, 'sine', 0.2)
    } else if (type === 'rotate') {
      engine.playTone(620, 0.05, 0, 'sine', 0.16)
    } else if (type === 'error') {
      engine.playTone(220, 0.14, 0, 'sawtooth', 0.12)
      engine.playTone(185, 0.16, 0.08, 'sawtooth', 0.1)
    }
  }

  return engine
}
