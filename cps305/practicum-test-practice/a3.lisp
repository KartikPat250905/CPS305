; DAY-TYPE (day). Takes a symbol and returns WEEKEND for SAT or SUN, WEEKDAY for MON through FRI, and INVALID for anything else. Use case
(defun DAY-TYPE (day)
    (case day
        ((SAT SUN) 'WEEKEND)
        ((MON TUE WED THU FRI) 'WEEKDAY)
        (otherwise 'INVALID)
    )
)