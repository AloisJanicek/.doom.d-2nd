;;; agenda-queries.el --- Org-ql queries and predicates -*- lexical-binding: t; -*-

;;; Commentary:
;;  Provides custom org-ql queries, sorting or search predicates
;;  reflecting either canonical GTD methodology or my personal needs

(require 'org-ql)

;; Org-ql search predicates
(org-ql-defpred habit-half-due ()
  "Search for habits which are at least half-due.

Normally habits appear in agenda on their scheduled day. I think this is
too soon for habits with ranges.
For habit with repeater of \".+2d/18d\", return non-nil only if today
is closer to maximum of the range rather then to the scheduled date.
"
  :body (when-let* ((headline (car (cdr (org-element-headline-parser (line-end-position)))))
                    (habit (string-equal "habit" (plist-get headline :STYLE)))
                    (habit-data (when habit (org-habit-parse-todo)))
                    (scheduled-date (nth 0 habit-data))
                    (scheduled-repeater (nth 1 habit-data))
                    (deadline-date (nth 2 habit-data))
                    (deadline-repeater (nth 3 habit-data))
                    (half (- (+ scheduled-date deadline-repeater)
                             (/ (+ scheduled-repeater deadline-repeater) 2))))
          (< half (org-today))))

;; Org-ql sort predicates
(defun agenda-queries-sort-by-recent-clock (a b)
  "Sort A and B by their latest clock."
  (let ((get-latest-clock (lambda (elm)
                            (org-with-point-at (org-element-property :org-marker elm)
                              (if (re-search-forward org-ql-clock-regexp (save-excursion (org-end-of-subtree)) t)
                                  (org-parse-time-string
                                   (plist-get (cadr (org-element-property :value (org-element-at-point))) :raw-value))
                                (org-parse-time-string "1971-01-01"))))))
    (time-less-p (funcall get-latest-clock b)
                 (funcall get-latest-clock a))))

(defun agenda-queries-sort-by-active-timestamp (a b)
  "Sort A and B by their active timestamp.

When item is habit, sort by average of
its date range instead of its scheduled time.

For items on the same day use hh:mm
to resole their precedence.
"
  (let ((get-time
         (lambda (elm)
           (let* ((habit (when-let* ((habit-data
                                      (org-with-point-at (org-element-property :org-marker elm)
                                        (when (org-is-habit-p)
                                          (org-habit-parse-todo))))
                                     (scheduled-date (nth 0 habit-data))
                                     (scheduled-repeater (nth 1 habit-data))
                                     (deadline-date (nth 2 habit-data))
                                     (deadline-repeater (nth 3 habit-data))
                                     (half (- (+ scheduled-date deadline-repeater)
                                              (/ (+ scheduled-repeater deadline-repeater) 2))))
                           half))
                  (timestamp
                   (or (plist-get (car (cdr (org-element-property :scheduled elm))) :raw-value)
                       (plist-get (car (cdr (org-element-property :deadline elm))) :raw-value)
                       (org-entry-get (org-element-property :org-marker elm) "TIMESTAMP")))
                  (hh-mm
                   (cl-destructuring-bind (_ minutes hours _ _ _ _ _ _)
                       (org-parse-time-string timestamp)
                     (list hours minutes)))
                  (time (or habit
                            (ignore-errors
                              (org-time-string-to-absolute
                               timestamp))
                            0)))
             (cons time hh-mm)))))
    (let* ((a (funcall get-time a))
           (b (funcall get-time b))
           (time-a (car a))
           (time-b (car b))
           (hh-mm-a (cdr a))
           (hh-mm-b (cdr b)))
      (if (equal time-a time-b)
          (time-less-p hh-mm-a hh-mm-b)
        (< time-a time-b)))))

(defun agenda-queries-sort-by-effort (a b)
  "Return non-nil if effort of the A is lower then effort of the B."
  (let ((get-effort (lambda (elm)
                      (string-to-number
                       (replace-regexp-in-string
                        "[[:punct:]]" ""
                        (or (org-element-property :EFFORT elm) "999"))))))
    (< (funcall get-effort a)
       (funcall get-effort b))))

(defun agenda-queries-sort-by-todo (a b)
  "Return non-nil if todo of A is less then todo of the B according to their order in `org-todo-keywords'."
  (let ((get-todo-keyword (lambda (elm)
                            (or (org-element-property :todo-keyword elm) "")))
        (todo-keyword-less-p (lambda (a b)
                               (> (length (cl-member a (cdar org-todo-keywords) :test #'string-match))
                                  (length (cl-member b (cdar org-todo-keywords) :test #'string-match))))))
    (funcall
     todo-keyword-less-p
     (funcall get-todo-keyword a)
     (funcall get-todo-keyword b))))

;; Queries

(defun agenda-queries--filter-tags-query ()
  "Return tags part of org-ql query when `agenda-filter-preset' is set. "
  (if (and agenda-filter-preset
           (not current-prefix-arg))
      (append '(tags)
              (seq-map
               (lambda (str)
                 (if (string-prefix-p "+" str)
                     (string-trim-left str "+")
                   str))
               agenda-filter-preset))
    '(tags)))

(defun agenda-queries--stucked-projects-query ()
  "Stucked projects query for org-ql."
  '(or (and (todo)
            (descendants (todo))
            (not (descendants (todo "NEXT")))
            (not (descendants (scheduled)))
            (not (and (or (todo "HOLD")
                          (todo "WAIT")
                          (todo "SOMEDAY")
                          (todo "MAYBE"))
                      (descendants (todo)))))
       (and (todo)
            (descendants (done))
            (not (descendants (todo))))
       (and (todo "PROJECT")
            (not (descendants (todo)))
            (not (descendants (done))))))

(defun agenda-queries--past-dues-query ()
  "Return valid org-ql query searching for past dues."
  `(or (and (ts-active :to ,(ts-now))
            (not (habit))
            (not (done)))
       (habit-half-due)))

(defun agenda-queries--habits-query ()
  "Return valid org-ql query searching for habits."
  `(and (habit) ,(agenda-queries--filter-tags-query)))

(defun agenda-queries--future-dues-query ()
  "Return valid org-ql query searching for future dues."
  (let ((up-to (if current-prefix-arg 365 2)))
    `(or (and (planning :from ,(ts-now) :to ,up-to))
         (and (habit)
              (planning :to ,up-to)
              (not (habit-half-due))))))

(defun agenda-queries--all-active-tasks-query ()
  "Return valid org-ql query searching for all active tasks.
"
  `(and (todo)
        (not (todo "SOMEDAY"))
        (not (todo "MAYBE"))
        (not (done))
        ,(agenda-queries--filter-tags-query)))

(defun agenda-queries--simple-task-query (keyword)
  "Return valid org-ql query searching for todo KEYWORD."
  (remove nil `(and (todo ,keyword)
                    (not (ancestors (done)))
                    ,(agenda-queries--filter-tags-query)
                    ,(if current-prefix-arg
                         nil
                       '(not (ancestors (todo)))))))

(defun agenda-queries--non-complete-tasks-query ()
  "Return valid org-ql query searching for non-complete tasks."
  `(and (todo)
        ,(agenda-queries--filter-tags-query)))

(defun agenda-queries--done-query ()
  "Return valid org-ql query searching completed tasks."
  `(and (done)
        ,(agenda-queries--filter-tags-query)))

(defun agenda-queries--stand-alone-task-query ()
  "Return custom org-ql queary for stand-alone tasks."
  `(and (todo "TODO")
        ,(agenda-queries--filter-tags-query)
        (not (scheduled))
        (not (descendants (todo)))
        (not (descendants (done)))
        (not (ancestors (todo)))
        (not (ancestors (done)))))

(defun agenda-queries--next-task-query ()
  "Return custom org-ql queary for NEXT task."
  `(and
    (todo "NEXT")
    ,(agenda-queries--filter-tags-query)
    (not (or (parent "WAIT")
             (parent "HOLD")
             (parent "SOMEDAY")
             (parent "MAYBE")))
    (not (ancestors (done)))
    (not (scheduled))))

(defun agenda-queries--projects-query ()
  "Return custom org-ql queary for Projects.

Projects are defined as a todo heading which isn't Someday or Maybe
and has todo childre."
  `(or (and (or (todo) (done))
            ,(agenda-queries--filter-tags-query)
            (descendants (todo))
            (not (or (todo "SOMEDAY")
                     (todo "MAYBE"))))
       (and (todo "PROJECT")
            ,(agenda-queries--filter-tags-query))))

(defun agenda-queries--project-descendants-query (h-title)
  "Return all descendants of heading matching H-TITLE."
  `(ancestors (heading ,h-title)))

(defun agenda-queries--custom-wait-task-query ()
  "Return custom org-ql queary for WAIT task."
  `(and (todo "WAIT" )
        ,(agenda-queries--filter-tags-query)
        (not (ancestors
              (or (done)
                  (todo "HOLD")
                  (todo "WAIT")
                  (todo "SOMEDAY")
                  (todo "MAYBE"))))))

(defun agenda-queries--custom-hold-task-query ()
  "Return custom org-ql queary for HOLD task."
  `(and (todo "HOLD" )
        ,(agenda-queries--filter-tags-query)
        (not (ancestors
              (or (done)
                  (todo "HOLD")
                  (todo "WAIT")
                  (todo "SOMEDAY")
                  (todo "MAYBE"))))))

(defun agenda-queries--custom-clocked-task-query ()
  "Return custom org-ql queary for relevant clock headings.

Relevant clock heading is either any heading which was clocked
andis not done or any heading with \"clock\" tag which is not done.

Use case for later is following:
User wants to define some tasks or headings for future time tracking
but they weren't clocked in yet. User marks those headings with \"clock\"
tag, either directly or by inheritance and they will appear among
results of this org-ql query.

Headings matching \"^Clock$\" are ommited from results - user can mark
this heading with \"clock\" tag and group clock headings under it.
"
  `(or (and (clocked)
            (not (done)))
       (and (tags "clock")
            (not (heading-regexp "^Clock$"))
            (not (done)))))

(defun agenda-queries--custom-ticklers-query ()
  "Return custom org-ql queary for tickler items.

Tickler is just plain reminder, calendar note,
 org-heading without task keyword but with active timestamp.
Tickler is not scheduled nor it doesn't have deadline."
  `(and (ts-active :to 365)
        (not (planning))))

(provide 'agenda-queries)
