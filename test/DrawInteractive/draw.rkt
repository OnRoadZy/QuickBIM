#lang racket

(require racket/gui)

(provide paint)

;回话及绘图===============================
(define (paint cv dc)
  (send dc set-scale 3 3)
  (send cv set-canvas-background (make-object color% 0 0 0 1.0))
  (send dc set-text-foreground "white")
  (send dc draw-text "文字显示（Show text）!" 0 0)
  (send cv on-event (new mouse-event% [event-type (list 'left-down)])))