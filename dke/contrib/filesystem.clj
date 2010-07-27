(ns dke.contrib.filesystem
  (:use [clojure.contrib.string :only (split-lines split)]
        [clojure.contrib.io :only (reader)])
  (:import [java.io File RandomAccessFile BufferedReader]))

(defn path-filename [path]
  (last (split (re-pattern File/separator)
               *file*)))

(defn canonical-file-path [#^String path]
  (.getCanonicalPath (File. path)))

; returns a seq of File objects
(defn files [#^String path]
  (seq (. (File. path) listFiles)))

; returns a seq of File objects, each of which represents a directory
(defn directories [#^String path]
  (filter (fn [#^File f] (.isDirectory f)) (files path)))

(defn directory-names [#^String path]
  (map (fn [#^File f] (.getName f)) (directories path)))

(defn relative-directory-paths [#^String path]
  (map (fn [#^File f] (.getPath f)) (directories path)))

(defn absolute-directory-paths [#^String path]
  (map (fn [#^File f] (.getAbsolutePath f)) (directories path)))

(defn canonical-directory-paths [#^String path]
  (map (fn [#^File f] (.getCanonicalPath f)) (directories path)))

(defn head [filename line-count]
  (with-open [#^BufferedReader rdr (reader filename)]
    (loop [lines []]
      (let [line (.readLine rdr)]
        (cond
          (or (= (count lines) line-count)
              (nil? line))
            lines
          :default
            (recur (cons line lines)))))))

; porting the java code found at http://forums.sun.com/thread.jspa?messageID=9778798#9778798
; Not Unicode safe. Will only work with ASCII files.
(defn tail [#^String filename line-count]
  (let [chunk-size 1024]
    (with-open [f (RandomAccessFile. filename "r")]     ; with-open will call (.close f) when the with-open body returns
      (take-last line-count                             ; return at most the requested number of lines: line-count
        (loop [lines []
               end-pos (- (.length f) 1)
               start-pos (- end-pos chunk-size)]
          (cond
            (>= (count lines) line-count)             ; we've got enough lines, so return them
              lines
            (<= start-pos 0)                          ; we've worked backward all the way through the file, return ALL lines
              (let [ba (byte-array end-pos)]
                (.seek f 0)
                (.readFully f ba)
                (concat (split-lines (String. ba)) lines))
            :default                                  ; we're still buffering lines because we don't yet have enough
              (let [ba (byte-array chunk-size)]
                (.seek f start-pos)
                (.readFully f ba)
                (let [[first-line & last-lines] (split-lines (String. ba))
                      char-count (count first-line)]
                  ; if last-lines == nil, then we're processing a really long line where the line's length > chunk-size
                  (if (nil? last-lines)
                    (recur lines
                           end-pos                            ; don't move the end-pos pointer
                           (- start-pos chunk-size))          ; back the start-pos pointer up by another chunk
                    (let [end-pos (+ start-pos char-count)]   ; move end-pos pointer just past the end of the line stored in first-line
                      (recur (concat last-lines lines)
                             end-pos
                             (- end-pos chunk-size))))))))))))  ; set the start-pos pointer one chunk before the end-pos pointer