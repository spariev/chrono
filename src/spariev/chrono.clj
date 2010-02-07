(ns #^{:doc "
       chrono.clj --- Because calling it date-utils would be boring.

       Use the date function to create dates. You can look up components
       much like you would in a map:
      
      (def my-date (date 2009 2 27 12 34 56))
      
      (my-date :year)   ;; 2009
      (my-date :month)  ;; 2
      (my-date :day)    ;; 27
      (my-date :hour)   ;; 12
      (my-date :minute) ;; 34
      (my-date :second) ;; 56
      
       You may omit the time if you like:
      
      (def my-other-date (date 2009 2 27))
      (my-other-date :hour) ;; 0
      
       To get a date relative to another date, use earlier and later:
      
      (earlier my-date 100 :minute) ;; 2009 2 27 10:54:56
      (later my-other-date 10 :day) ;; 2009 3 9
      
       For comparing dates, use earlier? and later?:
      
      (earlier? my-date my-other-date) ;; false
      (later? (later my-date 10 :day) my-date) ;; true
      
       You can see the time between two dates by calling time-between:
      
      (time-between my-other-date (date 2009 2 25) :days) ;; 2
      
      The date-seq function returns a lazy seq of dates incrementing by
      the units in its first arg starting from its second arg. The third
      arg if given dictates the end of the sequence.
      
      (date-seq :hours my-other-date my-date) ;; (a seq of twelve hours)
      (take 4 (date-seq :years my-date))
      ;; (date 2009 2 27 12 34 56) (date 2010 2 27 12 34 56)
      ;; (date 2011 2 27 12 34 56) (date 2012 2 27 12 34 56) [...]
      
       For converting between strings and dates, use format-date and
       parse-date
      
      (format-date my-date :short-date-time) ;; 2/27/09 12:34 PM
      (format-date my-other-date :long-date) ;; February 27, 2009
      (parse-date \"12/25/09\" :short-date) ;; (date 2009 12 25)
      (parse-date \"January 1, 2008 1:45:23 PM EST\" :long-date-time)
      ;; (date 2008 1 1 13 45 23)
      
      Supported date formats are:
        iso8601
        short-date
        medium-date
        long-date
        full-date
        short-date-time
      
      Both format-date and parse-date also support a string for the
      format argument, which will use the string as the format for a
      SimpleDateFormat (see the javadocs of that class for how to write
      these formats).
      
      See test_contrib/chrono.clj for more details.
      
       TODO:
      
      * Timezones
      * More support for weeks
      * Various others scattered through code
      
      "
       :author "Matt Moriarity, Phil Hagelberg, and Bradford Cross"}
  spariev.chrono
  (:import (java.util TimeZone)
           (org.joda.time DateTime DateTime$Property DateTimeZone 
                          Minutes Hours Period Interval)
	   (org.joda.time.format DateTimeFormat)))

(def #^{:doc "Number of seconds in each unit"}
     units-in-seconds
     {:year 31557600,
      :month 2592000,
      :week 604800,
      :day 86400,
      :hour 3600,
      :minute 60,
      :second 1,
      :millisecond 0.001})

(defn- make-datetime
  "Given some date values, create JodaTime DateTime object."
  ([]
     (org.joda.time.DateTime.))
  ([calendar-or-whatever]
     (org.joda.time.DateTime. calendar-or-whatever))
  ([year month day]
     (org.joda.time.DateTime. year month day 0 0 0 0))
  ([year month day hours minutes]
     (org.joda.time.DateTime. year month day hours minutes 0 0))       
  ([year month day hours minutes seconds]
    (org.joda.time.DateTime. year month day hours minutes seconds 0)))

(defn- get-dt-unit [datetime unit]
  (let [res  (cond
	      (= unit :year)  (. datetime year)
	      (= unit :month) (. datetime monthOfYear)
	      (= unit :day) (. datetime dayOfMonth)
	      (= unit :hour) (. datetime hourOfDay)
	      (= unit :minute) (. datetime minuteOfHour)
	      (= unit :second) (. datetime secondOfMinute)
	      (= unit :dayofweek) (. datetime dayOfWeek))]
    (.get res)))
    
(defmulti format-date
  "Take in a date and a format (either a keyword or a string) and
  return a string with the formatted date."
  (fn [date & frmt] (first frmt)))

(defmulti parse-date
  "Take in a string with a formatted date and a format (either a
  keyword or a string) and return a parsed date."
  (fn [source & frmt] (first frmt)))

(defn date
  "Returns a new date object. Takes year, month, and day as args as
  well as optionally hours, minutes, and seconds."
  [& args]
  (let [datetime (apply make-datetime args)]
    (proxy [clojure.lang.IFn clojure.lang.Associative] []
      (toString [] (format-date this :iso8601))
      (equals [other-date]
              (and (instance? (.getClass this) other-date)
                   (.equals datetime (other-date :datetime))))
      ;; look up :year, :month, :date, etc.
      (invoke [unit]
              (cond (= :datetime unit) datetime ;; mostly for internal use
                    true (get-dt-unit datetime unit)))
      ;; These (along with implementing Associative) allow us to use
      ;; (:month my-date), etc. Good idea? Not sure since we don't
      ;; implement all of Associative, just enough for keywords.
      (valAt [unit] (.invoke this unit))
      (equiv [o] (.equals this o)))))

;;joda time 
(defn time-zone 
  ""
  ([offset] (DateTimeZone/forOffsetHours offset)))

(defn joda-date
  ""
  ([str-d] (DateTime. str-d))
  ([y m d h min sec mill zone]
  (DateTime. y m d h min sec mill zone)))
    
(defn joda-proxy
  "joda-date object wraped in a proxy of goodness."
  [& args]
  (let [d (apply joda-date args)]
    (proxy [clojure.lang.IFn 
            clojure.lang.Associative] []
      (toString [] (str d))
      (equals [other-date]
              (and (instance? (.getClass this) other-date)
                   (.equals d (other-date :datetime))))
      (invoke [unit]
              (let [res
              (cond (= :years unit) (.year d) 
                    (= :months unit) (.monthOfYear d) 
                    (= :days unit) (.dayOfMonth d) 
                    (= :day-of-week unit) (.dayOfWeek d) 
                    (= :hours unit) (.hourOfDay d) 
                    (= :minutes unit) (.minuteOfHour d) 
                    (= :seconds unit) (.secondOfMinute d) 
                    (= :millis unit) (.millisOfSecond d) 
                    :otherwise d)]
                (if (instance? DateTime$Property res)
                  (.get res)
                  res)))
      ;; These (along with implementing Associative) allow us to use
      ;; (:month my-date), etc. Good idea? Not sure since we don't
      ;; implement all of Associative, just enough for keywords.
      (valAt [unit] (.invoke this unit))
      (equiv [o] (.equals this o)))))

(defn joda-str 
  ""
  ([d] (str `(DateTime. ~(str d)))))

(defmethod print-dup org.joda.time.DateTime [d w] (.write w (joda-str d)))
 
(defn joda-guard 
  ""
  ([d] (not (instance? org.joda.time.DateTime d))))

  ;;TODO: make this stuff monadic
(defn minutes-between 
  ""
  ([start end] 
    (if (or (joda-guard start) (joda-guard end))
      nil
      (.getMinutes (Minutes/minutesBetween start end)))))

(defn hours-between 
  ""
  ([start end] 
    (if (or (joda-guard start) (joda-guard end))
      nil
      (.getHours (Hours/hoursBetween start end)))))

(defn hours-from 
  ""
  ([d #^Integer h] (.plusHours d h)))

(defn minutes-from 
  ""
  ([d #^Integer m] (.plusMinutes d m)))

(defn hours-around 
  ""
  ([r #^Integer d] (map #(.plusHours d %) r)))

(defn before? 
  ""
  ([start end] (.isBefore start end)))

(defn valid-range? 
  "" 
  ([[start end]] (if (or (nil? start) (nil? end)) false (before? start end)))) ;ugly ifs instead of maybe monad :(

(defn is-within? 
  ""
  ([d [s e]] (.contains (Interval. s e) d)))

(defn are-overlapping? 
  ""
  ([[s e] [s1 e1]]
    (if (and (valid-range? [s e]) (valid-range? [s1 e1]))
      (letfn [(has-overlap? [start end start1 end1]
                          (not (nil? (.overlap (Interval. start end) (Interval. start1 end1)))))]
        (has-overlap? s e s1 e1))
      false)))

;;todo: find otu why this yields different resutls thatn reading the
;;other joda-str function output back in.
;;  joda-date 
;;        ~(.getYear d)
;;        ~(.getMonthOfYear d)
;;        ~(.getDayOfWeek d)
;;        ~(.getHourOfDay d)
;;        ~(.getMinuteOfHour d)
;;        ~(.getSecondOfMinute d)
;;        ~(.getMillisOfSecond d)
;;        (DateTimeZone/forID ~(str (.getZone d))))))

(defn now
  "Returns a new date object with the current date and time."
  []
  (date))

(defn today
  "Returns a new date object with only the current date. The time
  fields will be set to 0."
  []
  (let [d (now)]
    (date (d :year) (d :month) (d :day))))

;;; Relative functions

(defn later
  "Returns a date that is later than the-date by amount units.
  Amount is one if not specified."
  ([the-date amount units]
     (let [d (the-date :datetime)]
       (date (cond (= :years units) (.plusYears d amount) 
                    (= :months units) (.plusMonths d amount) 
                    (= :days units) (.plusDays d amount) 
                    (= :hours units) (.plusHours d amount) 
                    (= :minutes units) (.plusMinutes d amount) 
                    (= :seconds units) (.plusSeconds d amount) 
                    (= :millis units) (.plusMillis d amount) 
                    :otherwise d))))
  ([the-date units]
     (later the-date 1 units)))

(defn date-time 
  ""
  ([d t] (later d t :minute)))

(defn earlier
  "Returns a date that is earlier than the-date by amount units.
  Amount is one if not specified."
  ([the-date amount units]
     (later the-date (- amount) units))
  ([the-date units]
     (later the-date -1 units)))

(defn later? 
  "Is date-a later than date-b?"
  ([date-a date-b]
    (.isAfter (date-a :datetime) (date-b :datetime))))

(defn earlier? 
  "Is date-a earlier than date-b?"
  ([date-a date-b] (.isBefore (date-a :datetime) (date-b :datetime))))

(defn time-between
  "How many units between date-a and date-b? Units defaults to seconds."
  ([date-a date-b]
     (/ (java.lang.Math/abs
      (- (.getMillis (date-a :datetime))
         (.getMillis (date-b :datetime)))) 1000))
  ([date-a date-b units]
     ;; TODO: should we move plural support up to
     ;; units-in-seconds and units-to-calendar-units?
     (let [units (if (re-find #"s$" (name units)) ;; Allow plurals
                   ;; This relies on the patched subs defn below
                   ;;(keyword (subs (name units) 0 -1))
                   (keyword (apply str (drop-last (name units)))) ;; DEL 12/12/2009
                   units)]
       (/ (time-between date-a date-b)
          (units-in-seconds units)))))

(defn beginning-of
  "Return a date at the beginning of the month, year, day, etc. from the-date."
  [the-date unit]
  (let [dt (the-date :datetime)
	res (cond
	     (= unit :year) (.dayOfYear dt)
	     (= unit :month) (.dayOfMonth dt)
	     (= unit :day) (.hourOfDay dt)
	     (= unit :hour) (.minuteOfHour dt)
	     (= unit :minute) (.secondOfMinute dt)
	     (= unit :second) (.millisOfSecond dt))]
    (date (.withMinimumValue res))))

(defn end-of
  "Return a date at the end of the month, year, day, etc. from the-date."
  [the-date unit]
  ;; TODO: this is kinda ugly too?
  (earlier (later (beginning-of the-date unit) unit) :second))

(defn date-seq
  "Returns a lazy seq of dates starting with from up until to in
  increments of units. If to is omitted, returns an infinite seq."
  ([units from to]
     (lazy-seq
       (when (or (nil? to) (earlier? from to))
         (cons from (date-seq units (later from units) to)))))
  ([units from] (date-seq units from nil)))

;;; Formatting and Parsing

(defmacro def-date-format 
  ""
  ([fname [arg] & body]
    `(defmethod format-date ~(keyword (name fname)) [~arg ~'_]
       ~@body)))

(defmacro def-date-parser 
  ""
  ([fname [arg] & body]
    `(defmethod parse-date ~(keyword (name fname)) [~arg ~'_]
       ~@body)))

;;; Use the Joda Time date formats
;;; and some custom ones

(def #^{:private true}
     format-to-joda-const
     {:short-date (org.joda.time.format.DateTimeFormat/shortDate)
      :short-date-time (org.joda.time.format.DateTimeFormat/shortDateTime)
      :medium-date (org.joda.time.format.DateTimeFormat/mediumDate)
      :medium-date-time (org.joda.time.format.DateTimeFormat/mediumDateTime)
      :long-date (org.joda.time.format.DateTimeFormat/longDate)
      :long-date-time (org.joda.time.format.DateTimeFormat/longDateTime)
      :full-date (org.joda.time.format.DateTimeFormat/fullDate)
      :full-date-time (org.joda.time.format.DateTimeFormat/fullDateTime)
      :db-date-time (org.joda.time.format.DateTimeFormat/forPattern "yyyy-MM-dd hh:mm:ss")
      :iso8601 (org.joda.time.format.DateTimeFormat/forPattern "yyyy-MM-dd HH:mm:ss")
      :russian-short-date (org.joda.time.format.DateTimeFormat/forPattern "dd MMM ''yy")
      :compact-date (org.joda.time.format.DateTimeFormat/forPattern "yyyyMMdd")
      })

(defn get-locale
  [locale]
     ({:ru (java.util.Locale. "ru" "RU")
      :us (java.util.Locale/US)} locale))

(def *locale* nil) ; use default locale

(doseq [[key frmt] format-to-joda-const]
  (defmethod format-date key [a-date _]
    (.print  (.withLocale frmt (get-locale *locale*)) (org.joda.time.DateTime. (a-date :datetime))))
  (defmethod parse-date  key [source _]
    (date (.parseDateTime (.withLocale frmt (get-locale *locale*)) source))))
    

;;; Formats dates with a custom string format
(defmethod format-date :default [a-date frmt]
  (.print (.withLocale (org.joda.time.format.DateTimeFormat/forPattern frmt) (get-locale *locale*))
           (a-date :datetime)))

;;; Parse a date from a string format
(defmethod parse-date :default [source frmt]
  (date (.parseDateTime
	 (.withLocale (org.joda.time.format.DateTimeFormat/forPattern frmt) (get-locale *locale*)) source)))

(defmethod format-date nil [date]
  (format-date date :iso8601))

(defmacro def-simple-date-format [fname form]
  `(do
     (def-date-format ~fname [date#]
       (format-date date# ~form))
     (def-date-parser ~fname [source#]
       (parse-date source# ~form))))

;; Technically this should also have a single character time zone
;; indicator, but I'm not sure how to do that yet.
;(def-simple-date-format iso8601 "yyyy-MM-dd HH:mm:ss")

;; TODO: parse-date should be able to guess at the format

;; Redefine subs to allow for negative indices.
;; TODO: This should be submitted as a patch to Clojure.
(in-ns 'clojure.core)
(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([#^String s start] (subs s start (count s)))
  ([#^String s start end]
     (let [count-back #(if (< % 0) (+ (count s) %) %)]
       (.substring s (count-back start) (count-back end)))))

