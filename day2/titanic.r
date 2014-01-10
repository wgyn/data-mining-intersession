
# Section 0: Loading the packages and data

# install.packages('class')
require(class)

trainingData = read.csv('train.csv')
testData = read.csv('test.csv')


# Section 1: Pre-processing the raw data

trainingCopy = cbind(
	trainingData$Pclass,
	as.numeric(trainingData$Sex),
	trainingData$Age,
	trainingData$SibSp,
	trainingData$Parch,
	trainingData$Fare,
	as.numeric(trainingData$Embarked == 'C'),
	as.numeric(trainingData$Embarked == 'Q'),
	as.numeric(trainingData$Embarked == 'S')
)

testCopy = cbind(
	testData$Pclass,
	as.numeric(testData$Sex),
	testData$Age,
	testData$SibSp,
	testData$Parch,
	testData$Fare,
	as.numeric(testData$Embarked == 'C'),
	as.numeric(testData$Embarked == 'Q'),
	as.numeric(testData$Embarked == 'S')
)

colnames(trainingCopy) = colnames(testCopy) = c(
	'class', 'sex', 'age', 'sib/sp', 'par/ch', 'fare', 'cherbourg', 'queenstown', 'southampton'
)

# Section 2: Dealing with the missing data

mean(trainingCopy[, 3], na.rm = TRUE)
median(trainingCopy[, 3], na.rm = TRUE)
hist(trainingCopy[, 3])
trainingCopy[is.na(trainingCopy[, 3]) ,3] = mean(trainingCopy[, 3], na.rm = TRUE)
testCopy[is.na(testCopy[, 3]) ,3] = mean(testCopy[, 3], na.rm = TRUE)

which(is.na(trainingCopy))
which(is.na(testCopy))
testCopy[2243] = 8


# Section 3: Applying k-NN

?knn
predictionNN = knn(scale(trainingCopy), scale(testCopy), survival)
predictionNN


# Section 4: Evaluating k-NN

set.seed(259)
validationSet = sample(1:891, 300)
trainingNew = trainingCopy[-validationSet, ]
testNew = trainingCopy[validationSet, ]
survivedNew = trainingData$Survived[-validationSet]
# Use one line of code here to predict survival for the validation set
# Use one line of code here to evaluate the prediction from the previous line


# Section 5: Applying and evaluating linear regression

linearModel = lm(survivedNew ~ ., data = as.data.frame(trainingNew))
summary(linearModel)	# pause here to discuss interpretation
predictionLM = predict(linearModel, as.data.frame(testNew))
# Use one line of code here to convert the above predictions to 0s and 1s
# Use one line of code here to evaluate the prediction from the previous line


# Section 6: Applying and evaluating logistic regression

# Adapt the code from Section 5 above to run a logistic regression.
# Compare the prediction accuracy on the validation set


# Section 7: Applying and evaluating linear discriminant analysis


# Section 8: Applying and evaluating quadratic discriminant analysis

forest = randomForest(trainingNew, as.factor(survivedNew))
mean(predict(forest, testNew) == trainingData$Survived[validationSet])