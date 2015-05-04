C===============IDENTIFICATION DIVISION====================

c-------------TO FIND THE INTERPOLATION INDEX----------------

      SUBROUTINE binary_search(head, tail, needle, haystack,
     | size, decreasing)

        IMPLICIT NONE

        INTEGER head
        INTEGER tail
        REAL needle
        REAL :: haystack(size)
        INTEGER size
        LOGICAL decreasing

Cf2py   intent(out) head
Cf2py   intent(out) tail
Cf2py   intent(in) needle
Cf2py   intent(in) haystack
Cf2py   intent(hide), depend(haystack) size=len(haystack)
Cf2py   intent(in) decreasing

        INTEGER range
        INTEGER middle

        head = 1
        tail = size
        range = tail - head
        middle = (tail + head) / 2

        IF (decreasing) THEN
          DO WHILE ( haystack(middle) .NE. needle .AND. range .GT. 1)
            IF (needle .LT. haystack(middle)) THEN
              head = middle
            ELSE
              tail = middle
            END IF

            range = tail - head
            middle = (tail + head) / 2
          END DO
        ELSE
          DO WHILE ( haystack(middle) .NE. needle .AND. range .GT. 1)
            IF (needle .GT. haystack(middle)) THEN
              head = middle
            ELSE
              tail = middle
            END IF

            range = tail - head
            middle = (tail + head) / 2

          END DO
        END IF

        IF (haystack(middle) .eq. needle) THEN
          head = middle
          tail = middle
        ELSE IF (haystack(head) .eq. needle) THEN
          tail = head
        ELSE IF (haystack(tail) .eq. needle) THEN
          head = tail
        END IF


      END SUBROUTINE binary_search

C-------THE FUNCTIONS DOING AN INTERPOLATION---------------

      SUBROUTINE interp_values(interp_val, x_interp, x_values,
     |     y_values, decreasing, nlines)

      REAL interp_val
      !The interpolated y-result
      REAL x_interp
      !The x-value at which interpolation happens
      REAL x_values(nlines)
      !The abscissa discrete values
      REAL y_values(nlines)
      !The array for which we interpolate
      LOGICAL decreasing
      !This boolean tells if the x-values are
     |! in decreasing (true) or incr. order
      INTEGER nlines

Cf2py intent(out) interp_val
Cf2py intent(in) x_interp
Cf2py intent(in) x_values
Cf2py intent(in) y_values
Cf2py intent(in) decreasing
Cf2py intent(in) nlines


      INTEGER head, tail
      !The integer giving an x-value just below
     |! the one to be interpolated

      CALL binary_search(head, tail, x_interp, x_values,
     |     nlines, decreasing)

      IF (head .ne. tail) THEN

        y_lo = y_values(head)
        y_up = y_values(tail)
        x_lo = x_values(head)
        x_up = x_values(tail)

        interp_val = ( (x_up-x_interp)*y_lo + (x_interp-x_lo)*y_up )
     |       / (x_up - x_lo)
      ELSE
        interp_val = y_values(head)
      END IF

      END SUBROUTINE

C--------FOR A LOGARITHMIC INTERPOLATION---------------------
c We will usually do an interpolation of the log of the values as these
c  values are rapidly varying, instead of simply doing a linear interp.
      SUBROUTINE log_interp_values(interp_val, x_interp, x_values,
     |     y_values, decreasing, nlines)

      REAL interp_val
      !The interpolated y-result
      REAL x_interp
      !The x-value at which interpolation happens
      REAL x_values(nlines)
      !The abscissa
      REAL y_values(nlines)
      !The array for which we interpolate
      LOGICAL decreasing
      !This boolean tells if the x-values are
     |! in decreasing (true) or incr. order
      INTEGER nlines

      INTEGER head, tail
      !The integer giving an x-value just below
     |! the one to be interpolated
      LOGICAL positive
      !This tells if the y_values are positive,
     |! needed when taking the log.

Cf2py intent(out) interp_val
Cf2py intent(in) x_interp
Cf2py intent(in) x_values
Cf2py intent(in) y_values
Cf2py intent(in) decreasing
Cf2py intent(in) nlines


      CALL binary_search(head, tail, x_interp, x_values,
     |     nlines, decreasing)

      IF (head .eq. tail) THEN
        interp_val = y_values(head)
      ELSE
        y_lo = y_values(head)
        y_up = y_values(tail)

        IF ( y_lo*y_up <= 0 ) THEN
c It is not possible to take the log if one of them is 0 or if they
c  have different signs, so we do a linear interpolation in this case
           CALL interp_values(interp_val, x_interp, x_values, y_values,
     |          decreasing, nlines)
        ELSE
           positive = .true.
           IF (y_lo < 0) THEN
              positive = .false.
              y_lo = -y_lo        !For negative numbers we take the log
              y_up = -y_up        ! of their opposite
           END IF

           y_lo = log(y_lo)
           y_up = log(y_up)
           x_lo = x_values(head)
           x_up = x_values(tail)

           interp_val = ( (x_up-x_interp)*y_lo + (x_interp-x_lo)*y_up )
     |          / (x_up - x_lo)

           interp_val = exp(interp_val)

           IF (.not.positive) THEN
              interp_val = -interp_val
           END IF

        END IF

      END IF

      END SUBROUTINE
