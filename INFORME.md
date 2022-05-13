# Laboratorio 2: Programación orientada a objetos
## Respuestas
### Parte 1

**¿Cómo es la interfaz de la clase u objeto que implementaron para el primer componente?**

Para la obtención de datos se implementó la clase abstracta
`FeedRequester` la cual tiene tres métodos:

- getRequest: realiza un pedido HTTP a una URL específica.

- parserRequest: se encarga de extraer información de una URL. Cabe resaltar que como `FeedRequester` es una clase abstracta, es decir, al menos uno de sus métodos se nombra ya que las clases hijas tendrán una implementación específica.

- cleanContent: elimina los urls que se encuentran en el contenido de los *feeds* procesados.

Por otra parte, se coderon dos clases hijas `parserXML` y `parserJSON`, que heredan de `FeedRequester`. Estas subclases solo contienen un método con el mismo nombre. Entonces, `parserRequest` se encarga de extraer el contenido RSS en formato XML en la clase `parserXML` y los post de Reedit en formato JSON en la clase `parserJSON`.

También se modularizo el código en una función que se llama `cleanContent`, la cual se encarga de eliminar todas las referencias a otras URL en los contenidos que extrae de una URL específica.

**¿Utilizaron una clase o un objeto? ¿Por qué?**

Se utilizaron las clases `FeedRequester` y sus clases hijas `parserXML` y `parserJSON`. A su vez, para la detección de entidades se utilizó la clase `NERModel`.

Como un objeto es una instancia de una clase, los utilizamos al obtener los datos de una URL ya que para esto se necesita instanciar un objeto de `FeedRequest`.

Al tener métodos y atributos relacionados se solucionan problemas de código *spagetthi* ya que es mucho más fácil de entender. Además dividir tareas específicas en clases, nos brinda encapsulamiento y mejores herramientas para mantener el código a largo plazo.

**En sus palabras, ¿qué es la cohesión del código? ¿Por qué agregar nuevas clases aumenta la cohesión?**

La cohesión es la relación entre los atributos y métodos de una clase. Es decir, que poseen una temática en común, o métodos que trabajan para un mismo objetivo.

Agregar nuevas clases aumenta la cohesión, ya que mientras más se divida a las funciones de nuestro programa se van a agrupar en clases donde junto a otros métodos o atributos tengan un mismo objetivo. Por lo que cada método va estar focalizado en una temática específica junto a los demás miembros de la clase.

### Parte 2

**¿Tienen código similar que se repita en esta versión intermedia del laboratorio? ¿Qué estrategia van a usar para eliminar el código repetido?**

No teniamos mucho código que se repita. Solamente a la hora de eliminar las urls que aparecian en los contenidos de las subscripciones que procesamos. Para solucionar esto, se modularizo ese código repetido en la función, `cleanContent`.

**¿Qué es la herencia en la programación orientada a objetos? ¿Consideraron necesario utilizarla para esta implementación? ¿Su respuesta cambiaría si tuvieran que dar soporte a muchos tipos distintos de feeds?**

La herencia es el mecanismo de implementación mediante el cual elementos más específicos incorporan la estructura y comportamiento de elementos más generales. O sea que una clase puede recibir métodos y atributos de otras clases, para no tener que volver a implementarlas nuevamente.

En nuestro proyecto decidimos implementarla en las clases `parserXML` y `parserJSON` que son clases que se heredan de `FeedRequester`. Decidimos utilizarla, ya que esta última al ser una clase mas general, poodiamos definir operaciones que se repetian en las clases hijas, como el metodo `getRequest`.

Si tuviéramos que dar soporte a muchos tipos de *feeds* la herencia también nos seguiría siendo útil, porque lo único que se debería hacer es crear una nueva clase que herede de `FeedRequester` y que implemente `parserRequest` de manera que pueda obtener el contenido de una URL dependiendo del tipo particular de la *feed*. Esto también permite que el código sea escalable.

**En sus propias palabras, ¿qué es el acoplamiento en el código?**

El acoplamiento es cuando el comportamiento de un componente depende del valor de otros componentes en su implementación, generando un grado de dependencia. Pues si una clase A, necesita de otra clase B en su implementación, la clase A depende del correcto funcionamiento, y especificación de B. Si B en algún momento cambia su implementacion/especificación, la clase A posiblemente se debera modificar para un correcto funcionamiento del programa.

**¿Por qué utilizar literales como Strings para diferenciar comportamientos similares en la(s) clase(s) FeedRequester(s) crea acoplamiento?**

En el caso de nuestro código las clases `parserXML` y `parserJson` ambas poseen su propia implementación de la función `parserRequest` según se trate de rss o de reddit, por lo que genera acoplamiento al tener que definir múltiples veces la función de la clase `FeedRequester` para cada uno de los distintos casos en lugar de determinarlo mediante su propia implementación. A su vez en ambas clases se llama a la función `getRequest` definida en `FeedRequester` por lo que el funcionamiento correcto de las clases depende del funcionamiento correcto de la clase de la que estas heredan.

**Teniendo en cuenta esta clasificación de tipos de acoplamiento en la programación estructurada, ¿a qué tipo(s) corresponde el caso anterior?**

Según la clasificacion de tipos de acoplamiento en la programación estructurada, el caso anterior es de tipo acoplamiento de contenido o acoplamiento alto que se basa en que un módulo modifica el funcionamiento interno de otro módulo en el caso de la función `parserRequest` o que se apoya en la implementación de otro modulo para su correcto funcionamiento como en el caso de `getRequest`.

### Parte 3

**¿Qué clase almacena las URLs suscriptas?**

La clase `FeedRequester` la utilizamos para almacenar la lista de las urls y sus métodos.

**En sus palabras, ¿qué es la sobrecarga de operadores? ¿La utilizaron para implementar la clase SubscriptionService?**

La sobrecarga de operadores es un caso específico de polimorfismo, donde diferentes operadores tienen diferentes implementaciones dependiendo de sus argumentos. O sea es la creación de varios métodos con el mismo nombre pero con diferente lista de tipo de parámetros.

**¿Cómo funciona el polimorfismo de clases en su implementación? ¿De qué manera les permite reducir el acoplamiento?**

En nuestra clase `FeedRequester` usamos un polimorfismo al pasar como parámetro el parser el url para que decida la función a la cual suscribe. Nos permite reducir ya que tenemos una clase que se encarga de decidir cuando tipo parser usar, lo cuales son independientes uno del otro.

## Equipo

Grupo número 11 compuesto por:

* López, Leandro.
* Mansilla, Kevin Gaston.
* Ramirez, Lautaro.