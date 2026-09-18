(defun CHECK-QUALIFICATION (age has_license)
    (cond
        ((and (>= age 18) has_license) 'FULL-DRIVER)
        ((and (>= age 18) (not has_license)) 'LEARNER)
        (t 'INELIGIBLE)
    )
)