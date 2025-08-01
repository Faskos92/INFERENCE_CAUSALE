#' Simulation de données de santé avec variables causales
#'
#' Cette fonction simule un jeu de données en coupe transversale pour étudier
#' l'impact de l'activité physique sur un score de santé auto-déclaré.
#' La simulation intègre des variables de contrôle légitimes, une variable
#' de surcontrôle (motivation pour la santé), des non-linéarités, interactions
#' réalistes, ainsi que plusieurs variables additionnelles pertinentes issues
#' de la littérature sur la santé publique.
#'
#' @details
#' ## Variables simulées
#' 
#' **Variable dépendante :**
#' - `ScoreSante` : Score de santé auto-déclaré (0-100)
#' 
#' **Variable d'intérêt causal :**
#' - `ActivitePhysique` : Score d'activité physique (0-10)
#' 
#' **Variables de contrôle légitimes :**
#' - `Age` : Âge en années (18-80)
#' - `Sexe` : Sexe (Homme/Femme)
#' - `Education` : Niveau d'éducation (Primaire/Secondaire/Superieur)
#' - `RevenuFamilial` : Revenu familial en milliers d'euros
#' - `Tabagisme` : Statut tabagique (Fumeur/Non-fumeur)
#' - `EtatSanteChronique` : Indicateur de maladie chronique (0/1)
#' - `ConsommationAlcool` : Comportement de consommation (Buveur/Non-buveur)
#' - `StressPsychologique` : Score de stress (0-10)
#' - `SoutienSocial` : Score de soutien social (0-10)
#' - `QualiteSommeil` : Score de qualité du sommeil (0-10)
#' - `OccupationPhysique` : Score d'occupation physique (0-10)
#' - `AccesSoins` : Score d'accès aux soins (0-10)
#' - `Alimentation` : Score d'alimentation (0-10)
#' 
#' **Variables problématiques :**
#' - `IMC` : Indice de masse corporelle (médiateur potentiel)
#' - `MotivationSante` : Variable de surcontrôle (0-10)
#' 
#' **Effets fixes :**
#' - `Region` : Région de résidence
#' - `Milieu` : Milieu de vie (Urbain/Rural)
#' 
#' ## Hypothèses causales
#' 
#' - **Effet principal** : Activité physique → Santé (effet causal direct)
#' - **Confondeurs légitimes** : Variables socio-démographiques et comportementales
#' - **Médiateur** : Activité physique → IMC → Santé
#' - **Surcontrôle** : MotivationSante influence activité physique ET santé
#' - **Non-linéarités** : Age (quadratique), IMC (courbe en U)
#' - **Interactions** : Activité × Age, Activité × Sexe, Tabagisme × Age
#'
#' @param n Nombre d'observations à générer (défaut: 10000)
#' @param n_regions Nombre de régions à simuler (défaut: 5)
#' @param B_activite Coefficient de l'effet causal de l'activité physique (défaut: 2.5)
#'
#' @return Un data.frame contenant toutes les variables simulées avec les relations
#'   causales spécifiées. Les variables catégorielles sont converties en facteurs.
#'
#' @examples
#' # Générer un échantillon standard
#' donnees <- donnees_activite_sante(n = 1000)
#' head(donnees)
#' 
#' # Personnaliser la simulation
#' donnees_custom <- donnees_activite_sante(
#'   n = 5000, 
#'   n_regions = 3, 
#'   B_activite = 3.0
#' )
#' 
#' # Statistiques descriptives
#' summary(donnees$ScoreSante)
#' table(donnees$Sexe, donnees$Education)
#' 
#' # Vérifier la relation causale
#' cor(donnees$ActivitePhysique, donnees$ScoreSante)
#'
#' @author Kiyali Coulibaly
#' 
#' @references
#' Cette simulation s'inspire de la littérature en santé publique sur les
#' déterminants de la santé et les défis de l'inférence causale en épidémiologie.
#'
#' @seealso 
#' \code{\link{resume_donnees}}, \code{\link{graphique_activite_sante}}
#'
#' @export
donnees_activite_sante <- function(n = 10000,
                                  n_regions = 5,
                                  B_activite = 2.5) {
  
  # Validation des paramètres
  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("n doit être un entier positif")
  }
  if (!is.numeric(n_regions) || n_regions <= 0 || n_regions != round(n_regions)) {
    stop("n_regions doit être un entier positif")
  }
  if (!is.numeric(B_activite)) {
    stop("B_activite doit être numérique")
  }
  
  set.seed(42)
  
  # Variables socio-démographiques de base
  Region <- factor(sample(paste0("region", LETTERS[1:n_regions]), n, replace = TRUE))
  Milieu <- factor(sample(c("Urbain", "Rural"), n, replace = TRUE, prob = c(0.7, 0.3)))
  
  Age <- round(c(rnorm(n*0.3, 35, 8), rnorm(n*0.7, 55, 12)))[1:n]
  Age[Age < 18] <- 18
  Age[Age > 80] <- 80
  
  Sexe <- factor(sample(c("Homme", "Femme"), n, replace = TRUE, prob = c(0.45, 0.55)))
  
  n <- length(Age)
  
  # Education selon Age et Milieu
  prob_educ <- matrix(NA, nrow = n, ncol = 3)
  prob_educ[Age < 40, ] <- matrix(rep(c(0.1, 0.4, 0.5), sum(Age < 40)), ncol = 3, byrow = TRUE)
  prob_educ[Age >= 40, ] <- matrix(rep(c(0.3, 0.5, 0.2), sum(Age >= 40)), ncol = 3, byrow = TRUE)
  idx_urbain <- which(Milieu == "Urbain")
  prob_educ[idx_urbain, ] <- prob_educ[idx_urbain, ] * c(0.8, 1.0, 1.3)
  prob_educ <- prob_educ / rowSums(prob_educ)
  Education <- factor(sapply(1:n, function(i) {
    sample(c("Primaire", "Secondaire", "Superieur"), 1, prob = prob_educ[i, ])
  }))
  
  RevenuFamilial <- round(25 + 
                            10 * (Education == "Secondaire") + 
                            25 * (Education == "Superieur") +
                            8 * (Milieu == "Urbain") +
                            rnorm(n, 0, 8), 1)
  RevenuFamilial[RevenuFamilial < 15] <- 15
  
  prob_tabac <- 0.25 - 0.15 * (Education == "Superieur") - 0.002 * RevenuFamilial + 
    0.003 * Age * (Age < 50) - 0.001 * Age * (Age >= 50)
  prob_tabac[prob_tabac < 0.05] <- 0.05
  prob_tabac[prob_tabac > 0.5] <- 0.5
  Tabagisme <- factor(ifelse(runif(n) < prob_tabac, "Fumeur", "Non-fumeur"))
  
  # Consommation d'alcool
  prob_alcool <- 0.3 + 0.1 * (Sexe == "Homme") - 0.05 * (Age < 25) + 0.01 * (RevenuFamilial / 10)
  prob_alcool[prob_alcool < 0.05] <- 0.05
  prob_alcool[prob_alcool > 0.8] <- 0.8
  ConsommationAlcool <- factor(ifelse(runif(n) < prob_alcool, "Buveur", "Non-buveur"))
  
  StressPsychologique <- pmin(pmax(
    round(5 + 
      1.2 * (Milieu == "Urbain") + 
      -0.8 * (RevenuFamilial / 50) + 
      0.5 * (Sexe == "Femme") +
      -0.6 * (Education == "Superieur") +
      rnorm(n, 0, 1.5), 1), 0), 10)
  
  SoutienSocial <- pmin(pmax(
    round(5 + 
      0.5 * (Milieu == "Rural") +
      -0.01 * Age +
      0.3 * (Sexe == "Femme") +
      rnorm(n, 0, 1.2), 1), 0), 10)
  
  # Placeholder temporaire pour ActivitePhysique (sera recalculé plus bas)
  ActivitePhysique_temp <- rep(5, n)
  
  QualiteSommeil <- pmin(pmax(
    round(6 + 
      -0.03 * Age + 
      -0.6 * StressPsychologique + 
      0.4 * ActivitePhysique_temp +
      rnorm(n, 0, 1.0), 1), 0), 10)
  
  OccupationPhysique <- pmin(pmax(
    round(3 + 
      1.5 * (Milieu == "Rural") + 
      0.5 * (Sexe == "Homme") + 
      -0.7 * (Education == "Superieur") + 
      rnorm(n, 0, 1.3), 1), 0), 10)
  
  AccesSoins <- pmin(pmax(
    round(5 + 
      0.6 * (RevenuFamilial / 50) + 
      0.3 * (Milieu == "Urbain") + 
      0.4 * (as.numeric(Region) / n_regions) + 
      rnorm(n, 0, 1.0), 1), 0), 10)
  
  Alimentation <- pmin(pmax(
    round(5 + 
      1.2 * (Education == "Superieur") + 
      0.5 * (RevenuFamilial / 50) + 
      0.3 * (Milieu == "Urbain") + 
      rnorm(n, 0, 1.2), 1), 0), 10)
  
  # Variable de surcontrôle
  MotivationSante <- round(5 + 
                             2 * (Sexe == "Femme") + 
                             1.5 * (Education == "Superieur") +
                             0.5 * (Tabagisme == "Non-fumeur") +
                             rnorm(n, 0, 1.5), 1)
  MotivationSante[MotivationSante < 0] <- 0
  MotivationSante[MotivationSante > 10] <- 10
  
  # Activité physique (incluant stress et soutien social)
  ActivitePhysique <- round(
    2 + 
      0.8 * MotivationSante +                    
      1.2 * (Sexe == "Homme") +                 
      0.02 * RevenuFamilial +                   
      0.8 * (Education == "Superieur") +        
      1.5 * (Milieu == "Urbain") +              
      (-0.03) * Age +                           
      (-1.5) * (Tabagisme == "Fumeur") +        
      (-0.8) * (StressPsychologique) +          
      0.5 * (SoutienSocial) +                    
      rnorm(n, 0, 1.2), 1)
  ActivitePhysique[ActivitePhysique < 0] <- 0
  ActivitePhysique[ActivitePhysique > 10] <- 10

  
  # Recalcul QualiteSommeil avec ActivitePhysique réel
  QualiteSommeil <- pmin(pmax(
    round(6 + 
      -0.03 * Age + 
      -0.6 * StressPsychologique + 
      0.4 * ActivitePhysique +
      rnorm(n, 0, 1.0), 1), 0), 10)
  
  IMC <- round(25 + 
                 (-0.8) * ActivitePhysique +      
                 0.08 * Age +                     
                 (-1.2) * (Sexe == "Homme") +     
                 0.5 * (Tabagisme == "Fumeur") +  
                 (-0.05) * RevenuFamilial +       
                 0.3 * (Alimentation) +            
                 rnorm(n, 0, 2.5), 1)
  IMC[IMC < 16] <- 16
  IMC[IMC > 45] <- 45
  
  prob_maladie <- plogis(-3 + 0.05 * Age + 0.7 * (Tabagisme == "Fumeur") + 0.1 * IMC - 0.01 * RevenuFamilial)
  EtatSanteChronique <- rbinom(n, 1, prob_maladie)
  
  effets_region <- rnorm(n_regions, 0, 8)
  effets_milieu <- c("Urbain" = 2, "Rural" = -2)
  
  Age_centre <- Age - mean(Age)
  AP_centre <- ActivitePhysique - mean(ActivitePhysique)
  
  epsilon <- rnorm(n, 0, 6)
  
  ScoreSante <- 50 +
    B_activite * ActivitePhysique +
    3 * (Sexe == "Femme") +
    (-6) * (Tabagisme == "Fumeur") +
    0.15 * RevenuFamilial +
    4 * (Education == "Secondaire") +
    7 * (Education == "Superieur") +
    (-0.15) * Age_centre +
    (-0.008) * Age_centre^2 +
    (-0.3) * (IMC - 23)^2 +
    1.8 * MotivationSante +
    (-5) * EtatSanteChronique +
    (-3) * (ConsommationAlcool == "Buveur") +
    (-0.5) * StressPsychologique +
    0.4 * SoutienSocial +
    0.5 * QualiteSommeil +
    0.2 * OccupationPhysique +
    0.4 * AccesSoins +
    0.3 * Alimentation +
    0.15 * AP_centre * (Sexe == "Homme") +
    (-0.008) * ActivitePhysique * Age_centre +
    (-0.5) * (Tabagisme == "Fumeur") * Age_centre +
    effets_region[as.numeric(Region)] +
    effets_milieu[as.character(Milieu)] +
    epsilon
  
  ScoreSante[ScoreSante < 0] <- 0
  ScoreSante[ScoreSante > 100] <- 100
  
  # Retourner le data.frame avec facteurs bien définis
  result <- data.frame(
    ScoreSante = round(ScoreSante, 1),
    ActivitePhysique = as.integer(ActivitePhysique),
    Sexe = Sexe,
    Tabagisme = Tabagisme,
    Age = as.integer(Age),
    Education = Education,
    RevenuFamilial = RevenuFamilial,
    IMC = IMC,
    MotivationSante = as.integer(MotivationSante),
    Region = Region,
    Milieu = Milieu,
    EtatSanteChronique = as.integer(EtatSanteChronique),
    ConsommationAlcool = ConsommationAlcool,
    StressPsychologique = as.integer(StressPsychologique),
    SoutienSocial = as.integer(SoutienSocial),
    QualiteSommeil = as.integer(QualiteSommeil),
    OccupationPhysique = as.integer(OccupationPhysique),
    AccesSoins = as.integer(AccesSoins),
    Alimentation = as.integer(Alimentation),
    stringsAsFactors = FALSE
  )
  
  return(result)
}