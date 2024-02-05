;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(defproject org.uncomplicate/clojure-cpp "0.2.0-SNAPSHOT"
  :description "Clojure native interop through JavaCPP"
  :author "Dragan Djuric"
  :url "http://github.com/uncomplicate/clojure-cpp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [uncomplicate/commons "0.14.0-SNAPSHOT"]
                 [uncomplicate/fluokitten "0.9.2-SNAPSHOT"]
                 [org.bytedeco/javacpp-platform "1.5.10"]]

  :profiles {:dev {:plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.8"]
                             [com.github.clj-kondo/lein-clj-kondo "0.2.5"]]
                   :global-vars {*warn-on-reflection* true
                                 *assert* false
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :dependencies [[midje "1.10.10"]
                                  [codox-theme-rdash "0.1.2"]]
                   :codox {:metadata {:doc/format :markdown}
                           :source-uri "http://github.com/uncomplicate/clojure-cpp/blob/master/{filepath}#L{line}"
                           :themes [:rdash]
                           :output-path "docs/codox"}}}

  ;;:repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots"]]

  :source-paths ["src/clojure" "src/device"]
  :java-source-paths ["src/java"]
  :test-paths ["test"])
