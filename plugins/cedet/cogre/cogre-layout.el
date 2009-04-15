;;; cogre-layout.el --- Execute a layout engine on a cogre graph.
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cogre-layout.el,v 1.3 2009/04/07 00:36:44 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Reposition nodes in a graph using a layout engine.
;;
;; Calls out to graphviz for node position information.

(require 'cogre-convert)

;;; Code:
;;;###autoload
(defun cogre-layout ()
  "Layout the current graph.
This function depends on graphviz `dot' program."
  (interactive)
  (let ((tags nil)
	(elts nil)
	(maxy nil)
	(scalex (car cogre-dot-node-position-scale))
	(scaley (cdr cogre-dot-node-position-scale))
	)
    (save-window-excursion
      (save-excursion
	;; Convert to DOT.
	(cogre-export-dot)
	;; Pump it through DOT, extract the output.
	(set-buffer
	 (cedet-graphviz-dot-call (list (buffer-file-name))))
	;; Put the output into dot-mode
	(cogre-dot-mode)
	;; For some reason, the above mode change doesn't trigger
	;; the semantic new buffer function.  Do it here.
	(semantic-new-buffer-fcn)
	;; Fetch teh tags.
	(setq tags (semantic-fetch-tags))
	;; Now that we have the tags, switch back to our original
	;; graph, and try to apply the positional information.
	))
    ;; Get stuff in the graph.
    (setq elts (semantic-tag-get-attribute (car tags) :members))

    ;; Get the graph max size so we can invert Y
    (let* ((graphgeneric (semantic-find-first-tag-by-name "GRAPH" elts))
	   (graphsize (semantic-find-first-tag-by-name
		       "bb" (semantic-tag-get-attribute graphgeneric
							:attributes)))
	   (size (semantic-tag-get-attribute graphsize :value))
	   (ss (split-string size "," t)))
      (setq maxy (string-to-number (nth 3 ss))))
    
    ;; Loop over the tags.
    (dolist (E elts)
      (when (semantic-tag-of-class-p E 'node)
	(let* ((name (semantic-tag-name E))
	       (pos (semantic-find-first-tag-by-name
		     "pos" (semantic-tag-get-attribute E :attributes)))
	       (ss (split-string (semantic-tag-get-attribute pos :value)
				 "," t))
	       (X (string-to-number (car ss)))
	       (Y (string-to-number (car (cdr ss))))
	       (height (semantic-tag-get-attribute
			(semantic-find-first-tag-by-name
			 "height" (semantic-tag-get-attribute E :attributes))
			:value))
	       (width (semantic-tag-get-attribute
		       (semantic-find-first-tag-by-name
			"width" (semantic-tag-get-attribute E :attributes))
		       :value))
	       ;; dot reports width/height as inches, and the position
	       ;; as points, which is 72 points/inch.
	       (HH (* (/ (string-to-number height) 2) 72))
	       (HW (* (/ (string-to-number width) 2) 72))
	       ;; The node we want to modify
	       (cogrenode (cogre-find-node-by-name name))
	       )
	  ;; For this node at this position, move the COGRE graph node.
	  (if cogrenode
	      (progn
		(message "Found new pos %d,%d for matching node %s"
			 X Y name)
		(oset cogrenode :position
		      (vector (max 0 (floor (/ (- X HH) scalex)))
			      (max 0 (floor (/ (- maxy Y HH) scaley))))))
	    ;; No match?
	    (message "Could not find node for element %S" E)
	    ))))
    ;; Refresh the graph.
    (cogre-refresh)
    ))
  
(provide 'cogre-layout)
;;; cogre-layout.el ends here
