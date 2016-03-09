;; Universal Package Manager Experiments

(import [subprocess [run]])

(setv pacman-backend
      {:commands {:install "pacman -Syq --noconfirm {}"
                  :remove "pacman -Rsq --noconfirm {}"
                  :update "pacman -Syuq --noconfirm"}
       :pkgmap {"emacs" "emacs"}})

(defn install-package
  "Get the formatstring from a backend to install a package"
  [backend pkg]
  (let ([fstr (-> backend (:commands) (:install))])
    (.format fstr pkg)))

(defn remove-package
  "Get the formatstring from a backend to remove a package"
  [backend pkg]
  (let ([fstr (-> backend (:commands) (:remove))])
    (.format fstr pkg)))

(defn update-packages
  "Get the formatstring from a backend to update all packages"
  [backend]
  (-> backend (:commands) (:update)))

(defn get-package-name
  "Get the package name for a software from a backend's pkgmap. Returns nil if
  not present"
  [backend software]
  (try (get (:pkgmap backend) software)
       (except [e KeyError] nil)))

(defn execute-command
  "Execute a command, automatically splits arguments into list"
  [command]
  (run (.split command " ")))
