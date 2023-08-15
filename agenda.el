;; Timesheets
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Agenda files
(setq org-agenda-files (quote ("~/OneDrive/Documents/org/20221209105326-agenda_org.org")))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Do not compact the block agenda view
(setq org-agenda-compact-blocks nil)

;; Show time spent in columns
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; Custom views
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-scheduled-past-days 0)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-overriding-header "Today's agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 5)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext five days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; five days, so we start counting from the day after.
                      (org-agenda-start-day "+6d")
                      (org-agenda-span 21)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+21d)\n")))
          (todo "TODO" ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled 'deadline))
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-overriding-header "\nNext\n")))
          (todo "LATER" ((org-agenda-overriding-header "\nLATER\n")))))
        ("c" "Class Schedule"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-block-separator nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-scheduled-past-days 0)
                      (org-agenda-category-filter-preset (quote ("+CLASS")))
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-overriding-header "This week's Classes\n")))))
        ("t" "Global Todo"
         ((todo "*" ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'todo '("MEETING" "CANCELLED" "DONE")))
                     (org-agenda-overriding-header "\nGLOBAL TODO\n")))))
        ))
