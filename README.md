
# 📊 Simulation de Données Santé - Activité Physique & Score de Santé

## Objectif

Ce projet propose une fonction de simulation réaliste de données de santé en **coupe transversale**, dans le but d'étudier l'**effet causal de l'activité physique sur la santé auto-déclarée**. Il permet de tester des modèles d’inférence causale avec différentes stratégies de sélection de variables de contrôle, en incluant des **confondeurs légitimes**, des **médiateurs**, des **variables de surcontrôle**, et des **interactions réalistes**.

---

## Fonction principale

```r
donnees_activite_sante(n = 10000, n_regions = 5, B_activite = 2.5)
````

* `n` : taille de l’échantillon simulé (par défaut 10 000 individus)
* `n_regions` : nombre de régions (effets fixes)
* `B_activite` : effet causal simulé de l’activité physique sur la santé

---

## Variables simulées

* **ScoreSante** *(0–100)* : score de santé auto-évaluée (variable dépendante)
* **ActivitePhysique** *(0–10)* : niveau d'activité physique (variable d'intérêt)
* **Sexe** : Homme/Femme
* **Age** : 18–80 ans
* **Education** : Primaire, Secondaire, Supérieur
* **RevenuFamilial** *(en milliers d'euros)*
* **Tabagisme** : Fumeur / Non-fumeur
* **ConsommationAlcool** : Buveur / Non-buveur
* **StressPsychologique**, **SoutienSocial**, **QualiteSommeil**, **OccupationPhysique** *(0–10)*
* **AccesSoins**, **Alimentation** *(0–10)*
* **IMC** : Indice de masse corporelle (influencé par activité, âge, sexe, etc.)
* **EtatSanteChronique** *(0/1)* : indicateur de maladie chronique
* **MotivationSante** *(0–10)* : variable de surcontrôle (cause commune activité/santé)
* **Region**, **Milieu** : variables d'effets fixes

---

##  Hypothèses causales intégrées

* 📈 **Effet causal direct** : `ActivitePhysique → ScoreSante`
* 🧩 **Confondeurs contrôlés** : Age, Sexe, Revenu, Éducation, Tabagisme, etc.
* 🔄 **Médiateur** : `ActivitePhysique → IMC → ScoreSante` (à ne pas contrôler selon le modèle)
* 🚫 **Surcontrôle** : `MotivationSante` influence simultanément activité et santé
* ➕ **Interactions** : activité × âge, activité × sexe, tabac × âge
* ♒ **Non-linéarités** : âge quadratique, IMC en U inversé

---

## Utilisation possible

* Comparaison de modèles causaux (OLS, IPW, DAG, etc.)
* Études de biais de surcontrôle et de médiation
* Tests de robustesse sur différentes stratégies d’ajustement
* Enseignement de l'inférence causale et sélection de variables

---

## 📁 Exemple minimal en R

```r
# Génération des données simulées
set.seed(123)
df <- donnees_activite_sante(n = 5000)

# Visualisation rapide
summary(df$ScoreSante)
table(df$Sexe)
```

---

## 📝 Licence

Ce projet est mis à disposition sous licence MIT. Vous pouvez l'utiliser librement dans un cadre éducatif ou scientifique.

---

## 🙋‍♂️ Auteur

**Kiyali Coulibaly**
Data Scientist • Économiste de la santé
📧 [fas.coul@yahoo.fr](mailto:fas.coul@yahoo.fr)
🌍 [LinkedIn](https://www.linkedin.com/in/kiyali-coulibaly)

