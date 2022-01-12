% Marcelo Márquez Murillo - A01720588

-module(socios_tienda).
-export([suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0]).

matriz() -> 'servidor@Marcelos-MacBook-Pro'.

suscribir_socio(Socio) -> % Función para suscribir a un socio nuevo, ej: suscribir_socio(marcelo), suscribir_socio(natalia), suscribir_socio(ernesto), etc.
    io:format("Comprador ~p solicita suscripción~n", [Socio]),
    llama_tienda({suscribir, Socio}).
elimina_socio(Socio) -> % Función para eliminar a un socio ej: elimina_socio(marcelo), elimina_socio(natalia), elimina_socio(ernesto), etc.
    io:format("Comprador ~p solicita anular suscripción~n", [Socio]),
    llama_tienda({eliminar, Socio}).
crea_pedido(Socio, ListaDeProducto) -> % Función para crear un nuevo pedido, ej: crea_pedido(marcelo, {manzana, 10}), crea_pedido(marcelo, {pera, 4}), crea_pedido(marcelo, {platano, 2}), etc.
    io:format("Comprador ~p creo pedido~n", [Socio]),
    llama_tienda({pedido, Socio, ListaDeProducto}).
lista_existencias() -> % Función para recibir lista de todos los productos y sus cantidades
    io:format("Lista de existencias~n"),
    llama_tienda({lista_existencias}).

llama_tienda(Mensaje) -> % Necesita tener la información de como se llama el nodo
    Matriz = matriz(), % Lo convertimos en una variable de nombre Matriz
    monitor_node(Matriz, true), % monitor_node sirve para checar si otro nodo esta vivo, si el proceso de la tienda esta muerto entonces nunca le va a contestar
                                % Lo pusimos en true para que se conectara
    {servidor_tienda, Matriz} ! {self(), Mensaje}, % El mensaje se manda asi: Manda servidor_tienda que es donde esta corriendo y el nodo Matriz
                                                   % Lo que tambien hace es mandar el mensaje, manda el self() para que la tienda le pueda contestar
                                                   % y le manda el mensajew
    receive
        {servidor_tienda, Respuesta} -> % Si no esta caido, el banco le va a contestar y regresara una respuesta
            monitor_node(Matriz, false), % Desconectamos del nodo, solo queremos la conexion activa cuando se haga una solicitud
            Respuesta; % Lo que va a regresar es la respuesta, lo que respondio el banco
        {nodedown, Matriz} -> % Si el nodo esta muerto, responde con un no
            no
    end.