# Bandits-and-Models-Shinyapp

https://xochitlcardenasmtz.shinyapps.io/banditsandmodels/


-Xochitl Cárdenas
-Gabriela Facio
-Marco Negrete

Lab 25
Aprendizaje y Conducta Adaptativa III 2021-2

Referencia: Wilson, R. & Collins, A. (2019). Ten simple rules for the computational modeling of behavioral data. Elife, 8, DOI: https://doi.org/10.7554/eLife.49547

Tareas de bandidos
En muchas situaciones cotidianas las personas carecen de descripciones precisas acerca de las consecuencias de sus acciones, por lo cual, deben aprender a través de la experiencia, buscando disminuir la incertidumbre haciendo un balance entre dos conductas primordiales: exploración y explotación.
Por ejemplo, imagine que llegas a una ciudad relativamente nueva, donde conoces un buen restaurante para cenar. Sin embargo, te encuentras en el dilema de continuar cenando en este (explotación de la opción) o explorar en el resto de opciones (restaurantes) de la ciudad. El dilema existe ya que si te mantienes eligiendo el mismo restaurante, tendrás la confianza de recibir una buena comida, pero perderías la oportunidad de encontrar una mejor opción. Si cada día eliges un restaurante diferente, no te permitirías obtener la mayor recompensa del mejor lugar y probarías comidas desagradables más de una ocasión.
Un paradigma que ha permitido estudiar lo anterior desde Aprendizaje por Reforzamiento son las tareas de bandidos.
Bajo estas tareas se presentan varias opciones con diferentes distribuciones de recompensas, y el objetivo de los participantes es acumular la mayor cantidad de recompensa.
Se han propuesto diferentes modelos de cómo los participantes se pueden comportan ante estas tareas.

Modelando el comportamiento
El objetivo del modelamiento computacional es utilizar modelos matematicos precisos para hacer comprender mejor los datos del comportamiento.
Los modelos vienen en forma de ecuaciones matemáticas que vinculan las variables observables experimentalmente con el comportamiento inmediato. En este sentido, instancian diferentes "hipótesis algorítmicas" sobre cómo se genera el comportamiento.
Con estos modelos podemos generar datos "falsos" estableciendo parámetros particulares (Wilson & Collins, 2019).
