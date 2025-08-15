# λ +tools - misc tools

*Author:* lispcat <187922791+lispcat@users.noreply.github.com><br>


```emacs-lisp

(defun +benchmark-lambdas (f1 f2 &optional n)
  (let* ((n (or n 10000))
         (time1 (car (benchmark-run n (funcall f1))))
         (time2 (car (benchmark-run n (funcall f2))))
         (less (if (< time1 time2) "f1" "f2"))
         (symbol (if (< time1 time2) "<" ">"))
         (difference (if (> time1 time2)
                         (- time1 time2)
                       (- time2 time1))))
    (message "f1: %.6f %s f2: %.6f (%s faster by %.6f)"
             time1
             symbol
             time2
             less
             difference)))

```

## ‣ end

```emacs-lisp

(provide '+tools)
;;; +tools.el ends here
```



---

*Last updated: August 14, 2025*
