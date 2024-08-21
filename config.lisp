(defbuild "n14" :git "https://github.com/Vault-Overseers/nuclear-14.git" "master")
(definstance "n14" :build "n14" :port 1212)
(definstance "n14map" :build "n14" :data "n14" :port 1401)

(defbuild "ds14" :git "https://github.com/SS14-Classic/deep-station-14.git" "master")
(definstance "ds14" :build "ds14" :port 1403)
