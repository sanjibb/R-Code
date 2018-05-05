install.packages("h2o")
require(h2o)
h2o.init()

prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate.hex")
summary(prostate.hex)
# Set the CAPSULE column to be a factor column then build model.
prostate.hex$CAPSULE = as.factor(prostate.hex$CAPSULE)
model = h2o.deeplearning(x = setdiff(colnames(prostate.hex), c("ID","CAPSULE")), y = "CAPSULE", training_frame = prostate.hex, activation = "Tanh", hidden = c(10, 10, 10), epochs = 10000)
print(model@model$model_summary)

# Make predictions with the trained model with training data.
predictions = predict(object = model, newdata = prostate.hex)
# Export predictions from H2O Cluster as R dataframe.
predictions.R = as.data.frame(predictions)
head(predictions.R)
tail(predictions.R)

# Check performance of classification model.
performance = h2o.performance(model = model)
print(performance)


