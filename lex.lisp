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

(defun nfa--copy-nodes (graph id seen)
  (let ((edges (nfa-edges graph id))
        (new-id (nfa-new-state graph)))
    (setf seen (acons id new-id seen))
    (mapcar
     #'(lambda (edge)
         (let ((link-to
               (or (cdr (assoc (cdr edge) seen))
                   (multiple-value-bind (new-id new-seen)
                       (nfa--copy-nodes graph (cdr edge) seen)
                     (setf seen new-seen)
                     new-id))))
           (nfa-new-edge graph new-id link-to (car edge))))
     edges)
    (values new-id seen)))

(defun nfa-copy-nodes (graph nodes)
  (let ((new-head (nfa-new-state graph)))
    (multiple-value-bind (new-tail)
        (nfa--copy-nodes graph (nfa-tail nodes) (acons (nfa-head nodes) new-head nil))
      (nfa-new-nodes (nfa-symbol nodes) new-head new-tail))))

(defun nfa-tail (nodes)
  (cdr (assoc :tail nodes)))

(defun nfa-symbol (nodes)
  (cdr (assoc :symbol nodes)))

(defun nfa-head (nodes)
  (cdr (assoc :head nodes)))

(defun nfa-epsilon (graph)
  (nfa-simple-symbol graph nil))

(defun nfa--or (graph a b)
  (let ((tail (nfa-new-state graph))
        (head (nfa-new-state graph)))
    (nfa-new-edge graph tail (nfa-tail a) (nfa-symbol a))
    (nfa-new-edge graph tail (nfa-tail b) (nfa-symbol b))
    (nfa-new-edge graph (nfa-head a) head nil)
    (nfa-new-edge graph (nfa-head b) head nil)
    (nfa-new-nodes nil tail head)))

(defun nfa-or (graph a b)
  (let ((new-a (nfa-copy-nodes graph a))
        (new-b (nfa-copy-nodes graph b)))
    (nfa--or graph new-a new-b)))


(defun nfa--dot (graph a b)
  (nfa-new-edge graph (nfa-head a) (nfa-tail b) (nfa-symbol b))
  (nfa-new-nodes (nfa-symbol a) (nfa-tail a) (nfa-head b)))

(defun nfa-dot (graph a b)
  (let ((new-a (nfa-copy-nodes graph a))
        (new-b (nfa-copy-nodes graph b)))
    (nfa--dot graph new-a new-b)))


(defun nfa-* (graph a)
  (let ((head (nfa-new-state graph))
        (new-a (nfa-copy-nodes graph a)))
    (nfa-new-edge graph (nfa-head new-a) head nil)
    (nfa-new-edge graph head (nfa-tail new-a) (nfa-symbol new-a))
    (nfa-new-nodes nil head head)))

(defun nfa-+(graph a)
  (nfa-dot graph a (nfa-star graph a)))

(defun nfa-?(graph a)
  (nfa-dot graph a (nfa-star graph a)))

(defun nfa-[] (graph &rest args)
  (let ((tail (nfa-new-state graph))
        (head (nfa-new-state graph)))
    (mapcar #'(lambda (c)
                (nfa-new-edge graph tail head c))
            args)
    (nfa-new-nodes nil tail head)))

(defun nfa-string (graph str)
  (reduce #'(lambda (a b) (nfa--dot graph a b))
          (map 'list #'(lambda (a) (nfa-simple-symbol graph a)) str)))
