(defbuild "n14" :git "https://github.com/Vault-Overseers/nuclear-14.git" "master")
(defbuild "ds14" :git "file:///home/kevinz/workspace/games/ds14" "ds14/master")

(definstance "n14-prod" :build "n14")
(definstance "n14-map" :build "n14" :data "n14-prod")

(definstance "ds14-prod" :build "ds14")
(definstance "ds14-map" :build "ds14")
