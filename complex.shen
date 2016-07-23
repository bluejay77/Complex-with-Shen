\\ The complex data type as a type secure version
\\
\\ Dr AJY 2016-06-12
\\
\\ Idea by Dr Mark Tarver: Build a type secure defstruct kind of facility
\\ from the Shen tuples
\\
\\ Created an abstract datatype from the complex datatype AJY 2016-06-12
\\
\\ Usage: load Willi Riha's mathematics library
\\
\\ (load "maths-lib.shen")
\\
\\ Then load this file
\\
\\ (load "complex.shen")
\\
\\ AJY 2016-06-19 Created two complex number types, one in the
\\ (Re, Im) form, another one in the polar form
\\
\\ AJY 2016-06-21 Made a { complex --> complex } log function, plus
\\ added the constructors (c# Re Im) and (cp# Argt Modl)
\\


(datatype complex \\ The complex number in the real and im part rep
Re: number;   \\ Real part
Im: number;   \\ Imaginary part
=======================================
(@p Re Im): complex;


_______________________________________
c#: (number --> number --> complex); \\ Rect rep constructor



_______________________________________
re: (complex --> number); \\ rect rep real part

_______________________________________
im: (complex --> number); \\ rect rep imaginary part



_______________________________________
setRe: (complex --> number --> complex); \\ set rect rep real part

_______________________________________
setIm: (complex --> number --> complex); \\ set rect rep imaginary part



_______________________________________
c+: (complex --> complex --> complex); \\ complex sum

_______________________________________
c-: (complex --> complex --> complex); \\ complex difference

_______________________________________
c*: (complex --> complex --> complex); \\ complex product

_______________________________________
c/: (complex --> complex --> complex); \\ complex quotient

_______________________________________
cmul: (complex --> number --> complex); \\ mult complex w/ real 

_______________________________________
c-conjugate: (complex --> complex); \\ complex conjugate

_______________________________________
crec: (complex --> complex); \\ inverse, reciprocal

_______________________________________
csquare: (complex --> complex);

_______________________________________
c-copy: (complex --> complex);

_______________________________________
e2iu: (number --> complex);

_______________________________________
rect2polar: (complex --> complex-p);

_______________________________________
cexp: (complex --> complex);

_______________________________________
cpower: (complex --> complex --> complex);

_______________________________________
csqrt: (complex --> complex);


)



(datatype complex-p  \\ The complex number in the polar rep
Mod: number;  \\ Polar representation: Modulus (length)
Arg: number;  \\ Polar representation: Argument (angle with re axis)
=======================================
(@p Mod Arg): complex-p;


_______________________________________
cp#: (number --> number --> complex-p); \\ Polar rep constructor


_______________________________________
modl: (complex-p --> number); \\ polar rep modulus

_______________________________________
argt: (complex-p --> number); \\ polar rep argument



_______________________________________
setModl: (complex-p --> number --> complex-p); \\ set polar rep modulus

_______________________________________
setArgt: (complex-p --> number --> complex-p); \\ set polar rep argument


_______________________________________
polar2rect: (complex-p --> complex);

_______________________________________
clog-p: (complex-p --> complex);


)


\\ ------------------------------------------------------------


(define c#
    { number --> number --> complex }
    Re Im ->
        (@p Re Im))


(define cp# \\ Make a complex number in the polar representation
    { number --> number --> complex-p }
    Mod Arg ->
        (@p Mod Arg))

\\ ------------------------------------------------------------


(define re
    { complex --> number }
    (@p Re Im) -> Re)


(define im
    { complex --> number }
    (@p Re Im) -> Im)


(define modl
    { complex-p --> number }
    (@p Mod Arg) -> Mod)


(define argt
    { complex-p --> number }
    (@p Mod Arg) -> Arg)


\\ ------------------------------------------------------------


(define setRe
    { complex --> number --> complex }
    (@p Re Im) NewRe -> (@p NewRe Im))


(define setIm
    { complex --> number --> complex }
    (@p Re Im) NewIm -> (@p Re NewIm))


(define setArgt
    { complex-p --> number --> complex-p }
    (@p Arg Mod) NewArg -> (@p NewArg Mod))


(define setModl
    { complex-p --> number --> complex-p }
    (@p Arg Mod) NewMod -> (@p Arg NewMod))


\\ ------------------------------------------------------------


(define c+
    { complex --> complex --> complex }
    X Y ->
        (c#
	    (+ (re X) (re Y))
	    (+ (im X) (im Y))))

(define c-
    { complex --> complex --> complex }
    X Y ->
        (c#
	    (- (re X) (re Y))
	    (- (im X) (im Y))))


(define c*
    { complex --> complex --> complex }
    X Y ->
        (c#
	    (- (* (re X) (re Y)) (* (im X) (im Y)))
	    (+ (* (im X) (re Y)) (* (re X) (im Y)))))



(define c/
    { complex --> complex --> complex }
    X Y ->
        (let Aux (+ (* (re Y) (re Y)) (* (im Y) (im Y)))
	    (c#
	        (/ (+ (* (re X) (re Y)) (* (im X) (im Y))) Aux)
	        (/ (- (* (im X) (re Y)) (* (re X) (im Y))) Aux))))


(define cmul \\ multiply a complex with a real number
    { complex --> number --> complex }
    X R ->
        (c#
             (* R (re X))
	     (* R (im X))))


(define c-conjugate \\ complex conjugate
    { complex --> complex }
    X ->
       (c#
           (re X)
	   (- 0 (im X))))


(define crec  \\ inverse, reciprocal
    { complex --> complex }
    X ->
        (cmul (c-conjugate X) (/ 1.0 (+ (* (re X) (re X))
                                        (* (im X) (im X))))))

(define csquare
    { complex --> complex }
    X -> (c* X X))


(define e2iu
    { number --> complex }
    U -> (c# (cos U) (sin U)))


(define rect2polar
    { complex --> complex-p }
    R ->
        (let
	    Modl (sqrt (+ (* (re R) (re R))
                          (* (im R) (im R))))
	    Argm (atan2 (im R) (re R))
	    (cp#
		Modl
		Argm)))


(define polar2rect
    { complex-p --> complex }
    P ->
        (let
	    X (* (modl P) (cos (argt P)))
	    Y (* (modl P) (sin (argt P)))
	    (c#
	        X
		Y)))


(define cexp \\ (exp ComplexNumber) In the rect form!
    { complex --> complex }
    C ->
        (cmul (c# (cos (im C)) (sin (im C)))
	     (exp (re C))))


(define clog-p \\ (log ComplexNumber) In the polar form!
    { complex-p --> complex }
    C ->
        (c#
            (log (abs (modl C)))
	    (argt C)))


(define clog \\ (log ComplexNumber) \\ In the rect representation!
    { complex --> complex }
    C ->
        (clog-p (rect2polar C)))


(define cpower \\ ComplexNumber1 to the power ComplexNumber2
    { complex --> complex --> complex }
    C1 C2 ->
        (cexp (c* (clog C1) C2)))


(define csqrt
    { complex --> complex }
    C ->
        (cpower C (c# 0.5 0)))


