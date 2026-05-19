source("~/Documents/Experiment_I/Focal leaf scans")

# Area of full leaves

leaf_areas <- run.ij(path.imagej = "~/Documents/Experiment_I/ImageJ.app/", set.memory = 4,set.directory = "~/Documents/Experiment_I/Focal leaf scans/full leaves",
       distance.pixel = 395, known.distance = 5)


class(leaf_areas)
str(leaf_areas)

data <- as.data.frame(leaf_areas)
write.csv(data, "data.csv")

# Area of leaf punches

leaf_punches <- run.ij(path.imagej = "~/Documents/Experiment_I/ImageJ.app/", set.memory = 4,
                     set.directory = "~/Documents/Experiment_I/Focal leaf scans/Leaf punches",
                     distance.pixel = 395, known.distance = 5)

class(leaf_punches)
str(leaf_punches)

data_punches <- as.data.frame(leaf_punches)

write.csv(data_punches, "data_punches.csv")
