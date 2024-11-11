(defun my-sort (sorted lst key test)
  (if lst
      (my-sort (merge 'list sorted (list (funcall key (car lst))) test) (cdr lst) key test)
      sorted))

(defun sort-start (lst &key (key #'identity) (test #'<))
  (when lst
    (my-sort (list (funcall key (car lst))) (cdr lst) key test)))

(defun rpropagation-reducer (&key (comparator #'<))
  (let ((best nil))  
    (lambda (left right)
      (if (null right)
          (progn (setf best left)
                 (list left))
          (if (funcall comparator left best)
              (progn (setf best left)       
                     (cons left right))
              (cons best right))))))

(defun check-function (name func expected &rest args)
  (let ((result (apply func args)))
    (format t "Result ~a: ~a~%" name result)
    (format t "~:[FAILED~;passed~]... ~a~%" (equal result expected) name)))

(defun test1 ()
  (check-function "Test1" 'sort-start '(1 2 3 4 5) '(4 3 5 1 2))
  (check-function "Test2" 'sort-start '(1 2 3 4) '(3 2 1 0) :key #'1+)
  (check-function "Test3" 'sort-start '(4 3 2 1) '(0 1 2 3) :key #'1+ :test #'>))


(defun test2 ()
  (check-function "Test1" 'reduce '(1 1 1 2 3) (rpropagation-reducer) '(3 2 1 2 3) :from-end t :initial-value nil)
  (check-function "Test2" 'reduce '(1 1 2 2) (rpropagation-reducer) '(3 1 4 2) :from-end t :initial-value nil)
  (check-function "Test3" 'reduce '(3 3 3) (rpropagation-reducer :comparator #'>) '(1 2 3) :from-end t :initial-value nil))
