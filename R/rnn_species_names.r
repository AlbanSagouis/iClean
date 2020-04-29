#' An implementation of sequence to sequence learning for performing addition
#'
#' Input: "535+61"
#' Output: "596"
#'
#' Padding is handled by using a repeated sentinel character (space)
#'
#' Input may optionally be reversed, shown to increase performance in many tasks in:
#' "Learning to Execute"
#' http://arxiv.org/abs/1410.4615
#' and
#' "Sequence to Sequence Learning with Neural Networks"
#' http://papers.nips.cc/paper/5346-sequence-to-sequence-learning-with-neural-networks.pdf
#' Theoretically it introduces shorter term dependencies between source and target.
#'
#' Two digits reversed:
#'  One layer LSTM (128 HN), 5k training examples = 99% train/test accuracy in 55 epochs
#'
#' Three digits reversed:
#'  One layer LSTM (128 HN), 50k training examples = 99% train/test accuracy in 100 epochs
#'
#' Four digits reversed:
#'  One layer LSTM (128 HN), 400k training examples = 99% train/test accuracy in 20 epochs
#'
#' Five digits reversed:
#'  One layer LSTM (128 HN), 550k training examples = 99% train/test accuracy in 30 epochs
#'

library(keras)
library(stringi)

dat <- read.csv("data/species names training dataset.csv")
# deleting names too long
dat<- dat[nchar(dat$messy) <= 20 & nchar(dat$clean) <= 20, ]

# Function Definitions ----------------------------------------------------

# Creates the char table and sorts them.
learn_encoding <- function(chars){
   sort(chars)
}

# Encode from a character sequence to a one hot integer representation.
# > encode("22+22", char_table)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# +    0    1    0    0    0    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
encode <- function(char, char_table){
   strsplit(char, "") %>%
      unlist() %>%
      sapply(function(x){
         as.numeric(x == char_table)
      }) %>%
      t()
}

# Decode the one hot representation/probabilities representation
# to their character output.
decode <- function(x, char_table){
   apply(x,1, function(y){
      char_table[which.max(y)]
   }) %>% paste0(collapse = "")
}



# Parameters --------------------------------------------------------------

# Parameters for the model and dataset
TRAINING_SIZE <- nrow(dat)
MAXLEN_MESSY <- max(nchar(dat$messy), na.rm=T)
MAXLEN_CLEAN <- max(nchar(dat$clean), na.rm=T)

# All the numbers, plus sign and space for padding
charset <- unique(unlist(sapply(dat$messy, function(x) {
   if(!is.na(x)) {
      unique(stringi::stri_sub(x, seq(1, stri_length(x), by=1), length=1))
   }
})))
char_table <- learn_encoding(charset)


# Data Preparation --------------------------------------------------------
# Generate Data
examples <- list(questions = dat$messy,
                 results = dat$clean)


# Vectorization
x <- array(0, dim = c(length(examples$questions), MAXLEN_MESSY, length(char_table)))
y <- array(0, dim = c(length(examples$questions), MAXLEN_CLEAN, length(char_table)))

for(i in 1:TRAINING_SIZE){
   x[i, 1:nchar(examples$questions[i]),] <- encode(examples$questions[i], char_table)
   y[i, 1:nchar(examples$results[i]),] <- encode(examples$results[i], char_table)
}

# Shuffle
indices <- sample(1:TRAINING_SIZE, size = TRAINING_SIZE)
x <- x[indices,,]
y <- y[indices,,]


# Explicitly set apart 10% for validation data that we never train over
split_at <- trunc(TRAINING_SIZE/10)
x_val <- x[1:split_at,,]
y_val <- y[1:split_at,,]
x_train <- x[(split_at + 1):TRAINING_SIZE,,]
y_train <- y[(split_at + 1):TRAINING_SIZE,,]

print('Training Data:')
print(dim(x_train))
print(dim(y_train))

print('Validation Data:')
print(dim(x_val))
print(dim(y_val))


# Training ----------------------------------------------------------------

HIDDEN_SIZE <- 128
BATCH_SIZE <- 128
LAYERS <- 3

# <50char strings 4 layers 2 epochs 11 minutes
# <50char strings 20 layers 2 epochs >200 minutes
# <20char strings 4 layers 50 epochs 1.55hours min loss 1.8 max acc 0.25
# <20char strings 10 layers 14 epochs 2,57hours min loss 2.3 max acc 0.11
# <20char strings 5 layers 10 epochs 1.95hours min loss 2.3 max acc 0.10
# <20char strings 3 layers 4 epochs 0.45hours min loss 2.3 max acc 0.10
# Initialize sequential model
model <- keras_model_sequential()

model %>%
   # "Encode" the input sequence using an RNN, producing an output of HIDDEN_SIZE.
   # Note: In a situation where your input sequences have a variable length,
   # use input_shape=(None, num_feature).
  layer_lstm(HIDDEN_SIZE, input_shape=c(MAXLEN_CLEAN, length(char_table))) %>%
   # layer_lstm(HIDDEN_SIZE, input_shape=c(None, num_feature)) %>%
   # As the decoder RNN's input, repeatedly provide with the last hidden state of
   # RNN for each time step. Repeat 'DIGITS + 1' times as that's the maximum
   # length of output, e.g., when DIGITS=3, max output is 999+999=1998.
   layer_repeat_vector(MAXLEN_CLEAN)

# The decoder RNN could be multiple layers stacked or a single layer.
# By setting return_sequences to True, return not only the last output but
# all the outputs so far in the form of (num_samples, timesteps,
# output_dim). This is necessary as TimeDistributed in the below expects
# the first dimension to be the timesteps.
for(i in 1:LAYERS)
   model %>% layer_lstm(HIDDEN_SIZE, return_sequences = TRUE)

model %>%
   # Apply a dense layer to the every temporal slice of an input. For each of step
   # of the output sequence, decide which character should be chosen.
   time_distributed(layer_dense(units = length(char_table))) %>%
   layer_activation("softmax")


# Compiling the model
model %>% compile(
   loss = "categorical_crossentropy",
   optimizer = "adam",
   metrics = "accuracy"
)

# Get the model summary
summary(model)

# Fitting loop
start_time <- Sys.time()
model %>% fit(
   x = x_train,
   y = y_train,
   batch_size = BATCH_SIZE,
   epochs = 40,
   validation_data = list(x_val, y_val)
)
Sys.time() - start_time

# Predict for a new observation
new_obs <- encode("Bufo sp1", char_table) %>%
   array(dim = c(1,5,12))
result <- predict(model, new_obs)
result <- result[1,,]
decode(result, char_table)






