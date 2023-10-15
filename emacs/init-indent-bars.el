(load "indent-bars.el")

(defun color-hsv-to-rgb (h s v)
  """Converts HSV to RGB.

  Args:
    h: The hue in degrees.
    s: The saturation, a value between 0 and 1.
    v: The value, a value between 0 and 1.

  Returns:
    A list containing the RGB values [r g b]."""

  (let* ((i (floor (* 6 h)))
         (f (- (* h 6) i))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         (t_ (* v (- 1.0 (* (- 1.0 f) s))))
         (i (mod i 6)))
    (cond
     ((= i 0) (list v t_ p))
     ((= i 1) (list q v p))
     ((= i 2) (list p v t_))
     ((= i 3) (list p q v))
     ((= i 4) (list t_ p v))
     ((= i 5) (list v p q))
     (t (list 0 0 0)))))

(let
    ((pals (cl-loop for s in '(0.05 1.0)
		    collect
		    (cl-loop for h from 0.0 upto 1.0 by 0.125
			     collect
			     (apply #'color-rgb-to-hex
                                    (color-hsv-to-rgb h s 1.0))))))
  (setq indent-bars-color-by-depth `(:palette ,(car pals))
        indent-bars-pattern "."
	indent-bars-highlight-current-depth `(:palette ,(cadr pals))
        indent-bars-width-frac 0.25
        indent-bars-pad-frac 0.25))


(provide 'init-indent-bars)
