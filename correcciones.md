Grupo 11		
## Corrección		
	Tag o commit corregido:	e96ef4
		
### Entrega		70,00%
	En tiempo y con tag correcto	50,00%
	Commits frecuentes y con nombres significativos	90,00%
### Informe		95,00%
	¿Cómo es la interfaz de la clase u objeto que implementaron para el primer componente?	100,00%
	¿Utilizaron una clase o un objeto? ¿Por qué?	100,00%
	En sus palabras, ¿qué es la cohesión del código? ¿Por qué agregar nuevas clases aumenta la cohesión?	100,00%
	[Opcional] ¿Qué es la separación de responsabilidades en la programación y cómo se relaciona con la actividad que acabamos de realizar?	0,00%
	¿Tienen código similar que se repita en esta versión intermedia del laboratorio? ¿Qué estrategia van a usar para eliminar el código repetido?	100,00%
	¿Qué es la herencia en la programación orientada a objetos? ¿Consideraron necesario utilizarla para esta implementación? ¿Su respuesta cambiaría si tuvieran que dar soporte a muchos tipos distintos de feeds?	100,00%
	[Opcional] ¿Qué ventajas tiene definir una case class para extraer el contenido de un archivo json?	100,00%
	En sus propias palabras, ¿qué es el acoplamiento en el código?	100,00%
	¿Por qué utilizar literales como Strings para diferenciar comportamientos similares en la(s) clase(s) FeedRequester(s) crea acoplamiento?	100,00%
	Teniendo en cuenta esta clasificación de tipos de acoplamiento en la programación estructurada, ¿a qué tipo(s) corresponde el caso anterior?	100,00%
	¿Qué clase almacena las URLs suscriptas?	0,00%
	En sus palabras, ¿qué es la sobrecarga de métodos/funciones? ¿La utilizaron para implementar la clase SubscriptionService?	100,00%
	Si lo utilizaron, ¿Cómo funciona el polimorfismo de clases en su implementación? ¿De qué manera les permite reducir el acoplamiento?	100,00%
### Funcionalidad		100,00%
	Manejan el caso de error en la consulta HTTP devolviendo una lista vacía	100,00%
	Parsean correctamente los artículos de Reddit y de RSS	100,00%
	Filtran los artículos con pocas palabras y reemplazan las url en los artículos de Reddit	100,00%
	Realizan consultas a múltiples URLs subscriptas	100,00%
	Aceptan correctamente los distintos parámetros de las URLs	100,00%
### Modularización y diseño		100,00%
	Evitan repetir código que realiza el request HTTP y maneja los errores aprovechando la herencia u otra estrategia	100,00%
	El código es fácilmente extendible para soportar otros tipos de parsers	100,00%
	Utilizan una clase distinta de los parsers para administrar las subscripciones	100,00%
	No hay acoplamiento entre la clase que administra las subscripciones y la clase principal, es decir, solo una de ellas sabe cuántos tipos de URL distintos existen	100,00%
### Calidad de código		98,75%
	Utilizan el paradigma funcional cuando es posible.	100,00%
	Estructuras de código simples	100,00%
	Reutilizan funciones de librería, por ejemplo para serializar y deserializar Json	100,00%
	Estilo: no tienen errores ni warnings al hacer `sbt scalastyle`, indentación consistente, líneas de menos de 80 caracteres. Nombre de variable en camelCase	95,00%
### Opcionales		
	Punto estrella 1: Elección de campos	0,00%
	Punto estrella 2: Normalización de conteos	0,00%
	Punto estrella 3: StackExchange feed	0,00%
		
# Nota Final		9,53125
		
		
# Comentarios		
		
- Faltó el nombre del tag		
- Un poco vagos los nombres de los commits		
- Me parece que se confundieron, la clase que en su caso está almacenando las URLs es el `SubscriptionService`, el `FeedRequester` sólo recibe la url por parámetro		
- Algunas líneas de más de 80 caracteres		