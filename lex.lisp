(defun nfa-new-graph ()
  (make-hash-table))

(defun nfa-graph-format (graph)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) graph))

(defun nfa-edges (graph id)
  (gethash id graph))

(defun nfa-new-state (graph)
  (let ((id (gensym)))
    (setf (gethash id graph) nil)
    id))

(defun nfa-new-edge (graph from to symbol)
  (setf (gethash from graph) (acons symbol to (gethash from graph))))

(defun nfa-node (graph id)
  (gethash id graph))

(defun nfa-simple-symbol (graph symbol)
  (let ((item (nfa-new-state graph)))
     (nfa-new-nodes symbol item item)))

(defun nfa-new-nodes (symbol tail head)
  (list (cons :symbol symbol) (cons :tail tail) (cons :head  head)))

(defun nfa--copy-nodes (id seen)
  (let ((edges (nfa-edges id))
        (new-id (nfa-new-state)))
    (mapcar
     #'(lambda (edge)
         )
     edges)
    ))

(defun nfa-copy-nodes (nodes &optional) copied
  (let* ((symbol (nfa-symbol nodes))
         (head (nfa-head nodes))
         (tail (nfa-tail nodes))
         (new-head (nfa-new-state))
         (new-copied (if copied
                         (acons head new-head copied)
                         (list (cons head new-head) (cons tail (nfa-new-state)))))))
  (mapcar
   #'(lambda ((symbol target))
       )
   ))

(defun nfa-tail (nodes)
  (cdr (assoc :tail nodes)))

(defun nfa-symbol (nodes)
  (cdr (assoc :symbol nodes)))

(defun nfa-head (nodes)
  (cdr (assoc :head nodes)))

(defun nfa-epsilon (graph)
  (nfa-simple-symbol graph nil))

(defun nfa-or (graph a b)
  (let ((tail (nfa-new-state graph))
        (head (nfa-new-state graph)))
    (nfa-new-edge graph tail (nfa-tail a) (nfa-symbol a))
    (nfa-new-edge graph tail (nfa-tail b) (nfa-symbol b))
    (nfa-new-edge graph (nfa-head a) head nil)
    (nfa-new-edge graph (nfa-head b) head nil)
    (nfa-new-nodes nil tail head)))


(defun nfa-dot (graph a b)
  (nfa-new-edge graph (nfa-head a) (nfa-tail b) (nfa-symbol b))
  (nfa-new-nodes (nfa-symbol a) (nfa-tail a) (nfa-head b)))


(defun nfa-star (graph a)
  (let ((head (nfa-new-state graph)))
    (nfa-new-edge graph (nfa-head a) head nil)
    (nfa-new-edge graph head (nfa-tail a) (nfa-symbol a))
    (nfa-new-nodes nil head head)))

(defun nfa-plus (graph a)
  (m))
