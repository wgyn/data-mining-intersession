
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
prediction = knn(trainingCopy, testCopy, trainingData$Survived)
prediction


# Section 4: k-NN exercise


# Section 5: Evaluating k-NN

set.seed(259)
validationSet = sample(1:891, 300)
trainingNew = trainingCopy[-validationSet, ]
testNew = testCopy[validationSet, ]
survivedNew = trainingData$Survived[-validationSet]


# Section 6: Applying and evaluating linear regression
#  Ryan -- We may need to provide some helper code here, depending on how
#  we introduce them to linear regression in R.

# Section 7: Applying and evaluation logistic regression


