#lang racket

(require racket/gui)

(provide main-frame)

;定义主界面=======================
(define main-frame
  (new frame%
       [label "QuickBIM"]
       [width 800]
       [height 600]
       [border 5]))

;定义视图框架分割区域：========================
;定义菜单条:
(define menubar
  (new menu-bar%
       [parent main-frame]))

;总容器：
(define panel/all
  (new vertical-pane%
       [parent main-frame]
       [border 1]
       [alignment (list 'left 'top)]))

;定义工具面板区域：
(define panel/tools
  (new vertical-panel%
       [parent panel/all]
       ;[style (list 'border)]
       [alignment (list 'left 'top)]
       [min-height 30]
       [stretchable-height #f]))

;定义模型导航栏：
(define panel/nevigation
  (new horizontal-pane%
       [parent panel/tools]
       [alignment (list 'left 'top)]
       [min-height 30]
       [stretchable-height #f]))

;定义模型导航栏：
(define panel/nevigation
  (new horizontal-pane%
       [parent panel/tools]
       [alignment (list 'left 'top)]
       [min-height 30]
       [stretchable-height #f]))

;定义工具面板标签区域：
(define panel/tool-pads
  (new horizontal-pane%
       [parent panel/tools]
       [alignment (list 'left 'top)]
       [min-height 30]
       [stretchable-height #f]))

;定义工具条区域：
(define panel/tool-bars
  (new horizontal-pane%
       [parent panel/tools]
       [alignment (list 'left 'top)]
       [min-height 100]
       [stretchable-height #f]))

