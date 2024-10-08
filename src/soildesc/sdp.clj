(ns soildesc.sdp
  (:require [clojure.string :as str]))

;; Define soil types, rock types, and other categories as keywords
(def soil-types #{:clay :silt :sand :gravel :cobbles :boulders})
(def rock-types #{:conglomerate :sandstone :siltstone :mudstone :granite
                  :diorite :garbo :dolerite :rhyolite :andesite :basalt :obsidian})
(def bedding-types #{:very-thickly :thickly :medium :thinly :very-thinly
                     :thickly-laminated :narrowly :thinly-laminated :very-narrowly})
(def constituents #{:sandy :gravelly :clayey :silty :cobbly})
(def amounts #{:slightly :very})
(def co-strengths #{:very-soft :soft :soft-to-firm :firm :firm-to-stiff
                    :stiff :very-stiff :hard})
(def gran-strengths #{:very-loose :loose :medium-dense :dense :very-dense})
(def lightness #{:light :dark})
(def chroma #{:pinkish :reddish :yellowish :orangish :brownish :greenish :blueish :greyish})
(def hue #{:pink :red :yellow :orange :brown :green :blue :white :grey :black})
(def rock-strength #{:extremely-weak :very-weak :weak :medium-strong :strong
                     :very-strong :extremely-strong})
(def angularity #{:well-rounded :rounded :sub-rounded :sub-angular :angular :very-angular})
(def made-ground #{:made-ground})

;; Function to convert a string to camel case
(defn to-camel-case [s]
  (let [words (str/split s #"\s+")]
    (str/join "" (cons (str/lower-case (first words))
                       (map str/capitalize (rest words))))))

;; Helper function to check if a string contains a space
(defn contains-space? [s]
  (boolean (re-find #"\s" s)))

;; Helper to convert constituents to primary material
(defn cons-to-prim [c]
  (case c
    :sandy "sand"
    :gravelly "gravel"
    :clayey "clay"
    :silty "silt"
    :cobbly "cobbles"
    ""))

;; Cohesive soil strength to cohesion values (cu in kPa)
(defn co-strength-to-cu [strength]
  (case strength
    :very-soft 0.0
    :soft 20.0
    :soft-to-firm 40.0
    :firm 40.0
    :firm-to-stiff 75.0
    :stiff 75.0
    :very-stiff 150.0
    :hard 150.0
    0.0))

;; Granular soil strength to SPT N-values
(defn gran-strength-to-spt-range [strength]
  (case strength
    :loose [4 10]
    :medium-dense [10 30]
    :dense [30 50]
    :very-dense [50 100]
    [0 4]))

;; Struct-like map to represent soil description
(defn create-soil-description []
  {:primary-constituent nil
   :secondary-constituents []
   :strength nil
   :behavior nil
   :color nil
   :bedding nil
   :angularity nil
   :made-ground false
   :raw-description nil})

;; Function to build description from parsed tokens
(defn build-description [tokens]
  (reduce (fn [desc token]
            (cond
              (contains? soil-types token) (assoc desc :primary-constituent token)
              (contains? co-strengths token) (assoc desc :strength token)
              (contains? constituents token) (update-in desc [:secondary-constituents] conj token)
              (contains? gran-strengths token) (assoc desc :strength token)
              (contains? angularity token) (assoc desc :angularity token)
              (contains? made-ground token) (assoc desc :made-ground true)
              :else desc))
          (create-soil-description) tokens))
;; Define multi-word phrases that should be merged
(def multi-word-tokens {"soft to firm" :soft-to-firm
                        "firm to stiff" :firm-to-stiff
                        "very loose" :very-loose
                        "very dense" :very-dense
                        "made ground" :made-ground
                        ;; Add more phrases as needed
                        })

;; Function to merge multi-word tokens in the input string

(defn merge-multi-word-tokens [desc]
  (reduce (fn [acc [phrase keyword]]
            (let [lower-phrase (str/lower-case phrase)] ; Lowercase for case-insensitive match
              (if (.contains acc lower-phrase)          ; Check if phrase exists in string
                (str/replace acc lower-phrase (name keyword)) ; Replace it with the keyword
                acc))) ; If no match, return the string unchanged
          desc
          multi-word-tokens))

;; Updated preprocessor function
(defn preprocessor [bs5930]
  (let [cleaned (-> bs5930
                    (str/lower-case)
                    (str/replace #"[^\w\s]" "")  ;; Remove punctuation
                    (merge-multi-word-tokens))    ;; Merge multi-word tokens
        tokens (str/split cleaned #"\s+")]
    (map keyword tokens)))  ;; Convert tokens to keywords
;; Function to parse and preprocess soil description input

;; New function to build description class from description and raw string
(defn build-description-class [ full]
  (let [desc (preprocessor full)
        initial-output {:primary-constituent nil
                        :secondary-constituents []
                        :strength nil
                        :behavior nil
                        :color nil
                        :bedding nil
                        :angularity nil
                        :made-ground false
                        :raw-description full}]
    (reduce
      (fn [output i]
        (let [d (nth desc i nil)]
          (cond
            (contains? soil-types d)
            (let [behavior (if (contains? #{:clay :silt} d) :Cohesive :Granular)]
              (assoc output :primary-constituent d :behavior behavior))

            (contains? rock-types d)
            (assoc output :primary-constituent d :behavior :Rock)

            (and (contains? amounts d)
                 (contains? constituents (nth desc (inc i) nil)))
            (let [amount d
                  constituent (nth desc (inc i))]
              (update output :secondary-constituents conj {:amount amount :constituent constituent}))

            (contains? co-strengths d)
            (assoc output :strength d)

            (contains? angularity d)
            (assoc output :angularity d)

            (contains? made-ground d)
            (assoc output :made-ground true)

            :else output)))
      initial-output
      (range (count desc)))))

;; Test cases
(defn -main []
  ;; Test build-description-class
  (let [desc [:soft-to-firm :very :sandy :clay]
        full "Soft to firm very sandy clay"]
    (println (build-description-class full)))
(println (build-description-class "Very dense clayey Sand")) ;; TODO the case where there is no amount specifier does not currently work, neither does granular strength identifiers

  )


;; Call the main function to test
(-main)
