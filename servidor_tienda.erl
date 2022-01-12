% Marcelo Márquez Murillo - A01720588

-module(servidor_tienda).
-export([abre_tienda/0, tienda/0, lista_socios/0, registra_producto/2, modifica_producto/2, elimina_producto/1, productos_vendidos/0, cierra_tienda/0]).
% -export([registra_producto/2, elimina_producto/1, modifica_producto/2, abre_tienda/0, cierra_tienda/0, lista_socios/0, productos_vendidos/0, tienda/0, productos/1]).

tienda() ->
    tienda([], [], [], 0, []).

% ListaSocios = Lista de nombre de socios suscritos. [marcelo, pedro, adrian, natalia, ...]
% ListaProductos = [{chocolate, <0.38.0>}, {lechuga, <0.78.0>}, ...]
% CantidadProductos = [{chocolate, 35}, {lechuga, 30}, ...]
% Pedidos = Contador de pedidos
% Ventas = Lista de productos y cantidad vendida. [{chocolate, 4}, {lechuga, 10}, ...]

tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas) ->
    receive
        {De, {suscribir, Socio}} -> % Solicitud de suscripción a un nuevo socio
            io:format("Solicitud de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, ListaSocios) of % Revisamos si el socio existe dentro de la lista de socios
                true -> De ! {servidor_tienda, no}, % En caso de que ya exista, regresamos un mensaje diciendo no
                        tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas); % Volvemos a llamar tienda
                false -> De ! {servidor_tienda, agregando}, % En caso de que no exista, regresamos un mensaje diciendo agregando y llamamos tienda con la nueva ListaSocios
                        tienda([Socio|ListaSocios], ListaProductos, CantidadProductos, Pedidos, Ventas)
            end;

        {De, {eliminar, Socio}} -> % Solicitud de anular suscripción de un socio
            io:format("Solicitud de anular suscripción de ~p~n", [Socio]),
            case lists:member(Socio, ListaSocios) of % Revisamos si el socio existe dentro de la lista de socios
                true -> De ! {servidor_tienda, eliminando}, % En caso de que ya exista, regresamos un mensaje diciendo eliminando
                        tienda(lists:delete(Socio, ListaSocios), ListaProductos, CantidadProductos, Pedidos, Ventas); % Llamamos tienda y quitamos el socio dentro de la ListaSocios
                false -> De ! {servidor_tienda, no}, % El socio no existe
                        tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas) % Llamamos tienda sin cambios
            end;

        {De, {lista_existencias}} -> % Solicitud de existencias de todos los productos de la tienda
            io:format("Mostrando lista de existencias~n"),
            De ! {servidor_tienda, CantidadProductos}, % Regresamos la lista CantidadProductos al cliente
            tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas);

        {De, {pedido, Socio, ListaDeProducto}} -> % Solicitud de crear pedido, solo funciona con una tupla a la vez
            case lists:member(Socio, ListaSocios) of % Revisamos si el socio existe dentro de la ListaSocios
                false -> De ! {servidor_tienda, noExisteSocio}, % En caso que no exista, regresamos con un mensaje
                        tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas); % Llamamos tienda sin cambios
                        
                true -> % En caso que si exista
                    case checar_producto(CantidadProductos, [ListaDeProducto]) of % Revisamos si el producto del pedido existe
                        false -> % En caso de no serlo, regresamos mensaje y llamamos tienda sin cambios
                            De ! {servidor_tienda, errorProducto},
                            tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas);

                        true -> % En caso que si exista
                            De ! {servidor_tienda, [Pedidos+1, ListaDeProducto]}, % Regresamos al usuario el numero de pedido más el producto que pidio
                            tienda(ListaSocios, ListaProductos, modifica_venta(element(1, ListaDeProducto), element(2, ListaDeProducto), CantidadProductos), Pedidos+1, [{Pedidos+1, ListaDeProducto} | Ventas])
                            % Llamamos tienda y reducimos la cantidad del producto especifico, sumamos +1 al numero de pedido y agregamos el pedido a la lista de ventas
                    end
            end;

        {De, {registra, Producto, Cantidad}} -> % Registro de nuevo producto y su cantidad
            De ! {servidor_tienda, aceptado},
            tienda(ListaSocios, [{Producto, whereis(Producto)} | ListaProductos], [{Producto, Cantidad} | CantidadProductos], Pedidos, Ventas); % Llamamos tienda y agregamos los nuevos valores en ListaProductos y CantidadProductos

        {lista_socios} -> % Desplegamos la lista de todos los socios
            io:format("~p~n", [ListaSocios]),
            tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas); % Volvemos a llamar tienda sin ningun cambio

        {modifica_lista, Producto, Cantidad} -> % Modificación de un producto
            case lists:member(Producto, registered()) of % Revisamos si el producto esta registrado
                true ->
                    tienda(ListaSocios, ListaProductos, modifica_cantidad(Producto, Cantidad, CantidadProductos), Pedidos, Ventas); % En caso de serlo, corremos la función modifica_cantidad para cambiar la cantidad del producto
                false -> io:format("Error, no puedes modificar un producto que no existe~n"), % En caso de que no exista avisamos el error
                    tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas)
            end;

        {elimina_producto, Producto} -> % elimina producto especifico utilizando lists:keydelete
            exit(whereis(Producto), kill), % Matamos el proceso del Producto a la fuerza
            unregister(Producto), % Deslazamos Alias del PID
            tienda(ListaSocios, lists:keydelete(Producto, 1, ListaProductos), lists:keydelete(Producto, 1, CantidadProductos), Pedidos, Ventas); % Llamamos tienda pero quitamos el producto seleccionado de ListaProducto y CantidadProducto

        {vendidos} ->
            mostrar_ventas(Ventas),
            tienda(ListaSocios, ListaProductos, CantidadProductos, Pedidos, Ventas);

        {cierre} ->
            io:format("Cerrando tienda~n"),
            tirar(ListaProductos),
            exit(servidor_tienda, kill)
            
    end.

% **** INTERFAZ ****

abre_tienda() -> % Función para crear el register de la tienda y crear su PID
    register(servidor_tienda,
        spawn(?MODULE, tienda, [])).

lista_socios() -> % Función para desplegar una lista de todos los socios suscritos
    servidor_tienda ! {lista_socios}.

registra_producto(Producto, Cantidad) -> % Función para registrar productos
    case lists:member(Producto, registered()) of % Primero revisamos si el producto que queremos registrar ya existe
        true -> io:format("Producto ~p ya existe!", [Producto]); % Si sale verdadero, avisamos que este producto ya existe
        false -> % Sale falso
            if
                Cantidad > 0 -> % Primero revisamos la cantidad que se quiere ingresar, si es mayor de 0, proseguimos
                    register(Producto, spawn(?MODULE, tienda, [])), % Hacemos register del Producto dandole su propio PID
                    servidor_tienda ! {whereis(Producto), {registra, Producto, Cantidad}}; % Llamamos tienda con registra, enviamos Producto y su Cantidad
                true -> % En caso de que la Cantidad sea menor de 0, damos error
                    io:format("ERROR, Cantidad es menor a 0~n")
            end
    end.

elimina_producto(Producto) -> % Función para eliminar producto
    servidor_tienda ! {elimina_producto, Producto}. % Llamamos servidor_tienda con la solicitud

modifica_producto(Producto, Cantidad) -> % Función para modificar producto
    servidor_tienda ! {modifica_lista, Producto, Cantidad}. % Llamamos servidor_tienda con la solicitud

cierra_tienda() ->
    servidor_tienda ! {cierre}.

productos_vendidos() ->
    servidor_tienda ! {vendidos}.

% ******************


%  **** FUNCIONES QUE NO SON LLAMADOS POR MANAGER TIENDA ****

checar_producto(CantidadProductos, ListaDeProducto) -> % Función para revisar si el producto del pedido existe dentro de la lista de productos disponibles
  lists:all(
    fun({Key, _}) ->
      lists:keymember(Key, 1, CantidadProductos)
    end, ListaDeProducto).

modifica_cantidad(Producto, X, [{Producto, Total}|T]) -> % Función para modificar la cantidad de un producto ya sea en + o -
    [{Producto, Total + X} | T];
modifica_cantidad(Producto, X, [H|T]) ->
    [H | modifica_cantidad(Producto, X, T)];
modifica_cantidad(_, _, []) ->
    indefinido.

modifica_venta(Producto, X, [{Producto, Total}|T]) -> % Función para modificar la cantidad de un producto cuando se crea un pedido (identico a modifica_cantidad pero a fuerzas es una sustracción)
    [{Producto, Total - X} | T];
modifica_venta(Producto, X, [H|T]) ->
    [H | modifica_cantidad(Producto, X, T)];
modifica_venta(_, _, []) ->
    indefinido.

mostrar_ventas([]) -> % Funcion para desplegar todas las ventas
    done;
mostrar_ventas([H | T]) ->
    io:format("~p~n", [H]),
    mostrar_ventas(T).

tirar([]) ->
    fin;
tirar([H|T]) ->
    exit(element(1, whereis(H)), kill),
    tirar(T).


%  **********************************************************