### Utilisation de la librairie stepmix dans R via reticulate.

### J'appelle les packages reticulate et dplyr.
require(reticulate)
require(dplyr)

reticulate::use_python("C:/Python39/")
sm <- import("stepmix")

### On peut afficher les fonctions du module.
names(sm)

### On exporte les données du jeu iris et on le
### formate comme dans Python.
data(iris)
X <- iris[,1:4]
y <- c(0,1,2)[iris[, 5]]

### Création du premier modèle.
### Il faut noter ici que certaines données doivent
### être reformées. Par exemple, le nombre de
### composantes doit être un entier.
mod1 <- sm$StepMix(n_components = as.integer(3), n_steps = 1,
             measurement = "gaussian_full",
             random_state = as.integer(8546))
mod1$fit(X)
pr = mod1$predict(X)

xtabs(~ pr + y)

mod2 = sm$StepMix(n_components=as.integer(3), n_steps=1,
                 measurement="gaussian_diag",
                 random_state=as.integer(1235))
mod2$fit(X)
pr2 = c(1,2,0)[mod2$predict(X)+1]
# Now, there are only 8 out of 150 (5%) that are misidentified.
xtabs(~ pr2 + y)

mod2$get_params()

# Let's do the graph we did earlier by identifying those
# misidentified data points.
col = pd.Series(["#440154FF", "#21908CFF", "#FDE725FF"])
fig, ax = plt.subplots(1, 2)
ax[0].scatter(iris.iloc[pr2 == species, 0],
              iris.iloc[pr2 == species, 1], c=species[pr2 == species],
              marker='o')
ax[0].scatter(iris.iloc[pr2 != species, 0],
              iris.iloc[pr2 != species, 1], color=col[species[pr2 != species]],
              marker='x',)
ax[0].set_xlabel("Sepal Length")
ax[0].set_ylabel("Sepal Width")
ax[1].scatter(iris.iloc[:, 2], iris.iloc[:, 3], c=species)
ax[1].set_xlabel("Petal Length")
ax[1].set_ylabel("Petal Width")

# We can see that the two last species can have sepals of the same
# dimension so it is not discriminating very well. Let's try a model
# using only the petal.
model3 = StepMix(n_components=3, n_steps=1,
                 measurement="gaussian_diag", random_state=1234)
model3.fit(iris.iloc[:, 2:4])
pr3 = model3.predict(iris.iloc[:, 2:4])
pr3 = pd.Series([1, 0, 2])[pr3]
# Now, there are only 6 out of 150 (4%) that are misidentified.
pd.crosstab(pr3, species)

# Now suppose that we have two other variables that are related to the
# outcome: y1 (dichotomous) and y2 (continuous).
Y = pd.DataFrame(np.zeros([150, 2]), columns=['y1', 'y2'])
# x1 is binom(1,p = 0.25) in group 1 and 2 and binom(1, p = 0.50)
# in group 3.
p = np.zeros(150)
np.random.seed(3948)
for i in range(0, 150):
  p[i] = 0.25 + (0.25) * (species[i] == 2)
mu = np.array([np.repeat(1, 50),
               np.repeat(0.8, 50),
               np.repeat(1.5, 50)]).reshape(150)
Y.y1 = np.random.binomial(1, p, 150)
Y.y2 = np.random.normal(loc=mu, scale=np.repeat(1, 150), size=150)

# It's the final model...
model4 = StepMix(n_components=3, n_steps=3,
                 measurement="gaussian_diag",
                 structural="gaussian_unit",
                 random_state=1234)
model4.fit(X=iris.iloc[:, 2:4], Y=np.array(Y))
pr4 = model4.predict(X=iris.iloc[:, 2:4], Y=Y)
labels = pd.Series([1, 0, 2])
pr4 = labels[pr4]
pd.crosstab(pr4, species)
model4.get_parameters()


require(reticulate)
use_python("C:/Python39/")
mod = stepmix()
X = data.frame(x1 = c(0,1,0,1,1,0,0,0,1,1,1,0,0,1,0,1,1,0,0,1),
               x2 = c(0,1,0,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,1))

fit1 = fit(mod, X)
pr1 = predict(ft1, X)
ft1$predict(X)
savefit(fit1, "f1.pickle")
ft1 = loadfit("f1.pickle")
fit1

f1 = file("fit1.pickle", "wb")
reticulate::py_save_object(fit1, "fit1.pickle")
close(f1)

fit1 = reticulate::py_load_object("fit1.pickle")
fit1.pre
### predict stepmixr.fit
