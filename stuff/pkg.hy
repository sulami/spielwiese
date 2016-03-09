;; Universal Package Manager Experiments

(import [subprocess [run]])

;; A backend example
(setv pacman-backend
      {:commands {:install "pacman -Syq --noconfirm {}"
                  :remove "pacman -Rsq --noconfirm {}"
                  :update "pacman -Syuq --noconfirm"}
       :pkgmap {"emacs" "emacs"}})

;; Get the formatstring from a backend to install a package
(defn install-package
  [backend pkg]
  (let ([fstr (-> backend (:commands) (:install))])
    (.format fstr pkg)))

;; Get the formatstring from a backend to remove a package
(defn remove-package
  [backend pkg]
  (let ([fstr (-> backend (:commands) (:remove))])
    (.format fstr pkg)))

;; Get the formatstring from a backend to update all packages
(defn update-packages
  [backend]
  (-> backend (:commands) (:update)))

;; Get the package name for a software from a backend's pkgmap. Returns nil if
;; not present
(defn get-package-name
  [backend software]
  (try (get (:pkgmap backend) software)
       (except [e KeyError] nil)))

;; Execute a command, automatically splits arguments into list
(defn execute-command
  [command]
  (run (.split command " ")))
