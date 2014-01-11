# Section 0: Install and load necessary packages
# Note: You may need to remove the hashtag at the beginnings of lines 4 and 5

# install.packages('tree')
# install.packages('randomForest')
require(randomForest)
require(tree)


# Section 1: Upload data into R

data = read.csv('post-operative.data', header = F)
colnames(data) = c(
	'L.CORE',
	'L.SURF',
	'L.02',
	'L.BP',
	'SURF.STBL',
	'CORE.STBL',
	'BP.STBL',
	'COMFORT',
	'decision'
)

# Attribute Information:
#	1. L.CORE (patient's internal temperature in C):
#		high (> 37), mid (>= 36 and <= 37), low (< 36)
#	2. L.SURF (patient's surface temperature in C):
#		high (> 36.5), mid (>= 36.5 and <= 35), low (< 35)
#	3. L.O2 (oxygen saturation in %):
#		excellent (>= 98), good (>= 90 and < 98),
#		fair (>= 80 and < 90), poor (< 80)
#	4. L.BP (last measurement of blood pressure):
#		high (> 130/90), mid (<= 130/90 and >= 90/70), low (< 90/70)
#	5. SURF.STBL (stability of patient's surface temperature):
#		stable, mod-stable, unstable
#	6. CORE.STBL (stability of patient's core temperature)
#		stable, mod-stable, unstable
#	7. BP.STBL (stability of patient's blood pressure)
#		stable, mod-stable, unstable
#	8. COMFORT (patient's perceived comfort at discharge, measured as
#		an integer between 0 and 20)
#	9. decision ADM-DECS (discharge decision):
#		S (patient prepared to go home),
#		A (patient sent to general hospital floor)



# Section 2: Minor data cleanup

data = data[data$COMFORT != '?', ]
data = data[-which(data$decision == 'I'), ]
data$decision[4] = 'A'
data$decision = as.character(data$decision)
data$decision = as.factor(data$decision)


# Section 3: Split data into training set and validation set

set.seed(713)
subset = sample(1:86, 30)
trainingX = data[-subset, 1:8]
trainingY = data[-subset, 9]
validationX = data[subset, 1:8]
validationY = data[subset, 9]


# Section 4: Fit decision tree, and evaluate performance on test set

tree = tree(trainingY ~ ., data = trainingX)
mean(predict(tree, validationX, type = 'class') == validationY)
plot(tree)
text(tree)
tree


# Section 5: Fit random forest, and evaluate performance on test set

forest = randomForest(trainingX, trainingY)
predictionRF = predict(forest, validationX)
mean(predictionRF == validationY)