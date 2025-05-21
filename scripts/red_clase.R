install.packages("ISLR2")
library(ISLR2)

require("pacman")
p_load("ISLR2","keras")

Gitters <- na.omit(Hitters)
n <- nrow(Gitters)
set.seed(13)
ntest <- trunc(n / 3)
testid <- sample(1:n, ntest)

modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
x <- model.matrix(Salary ~ . - 1, data = Gitters) %>% scale()
y <- Gitters$Salary

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error")
)

history <- modnn %>% fit(
  x[-testid, ], y[-testid], epochs = 600, batch_size = 32,
  validation_data = list(x[testid, ], y[testid])
)

npred <- predict(modnn, x)

prueba = data.frame(predicho = predict(modnn, x), observado = y)

ggplot(prueba, aes(x = observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = predicho, fill = "Predicho"), alpha = 0.2, color = NA) +
  
  labs(title = "Densidad de valores observados vs. predichos",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "blue")) +
  theme_classic()

summary(Gitters)


