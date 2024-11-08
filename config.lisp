; Define a build called "n14", whose sources are in the "master" branch at the given Git repository
(defbuild "n14" :git "https://github.com/Vault-Overseers/nuclear-14.git" "master")

; Define an instance called "n14" using the "n14" build on port 1212
(definstance "n14" :build "n14" :port 1212)

; The mapping instance uses the same "n14" build, but runs on a different port
(definstance "n14map" :build "n14" :data "n14" :port 1401)

(defbuild "ds14" :git "https://github.com/SS14-Classic/deep-station-14.git" "master")
(definstance "ds14" :build "ds14" :port 1403)
