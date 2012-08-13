;;(rprint (select "Hardware" ("GPU" "Driver" "OpenCL") (path-table)))
;;(rprint (select "Parameters" ("width" "height") (path-table)))

(with-path "models"
  (rprint (select "models" ("faces" "BVHBuilding-time (ms)") (path-table))))


