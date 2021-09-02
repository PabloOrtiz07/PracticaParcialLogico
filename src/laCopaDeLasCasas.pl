%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parte 2 - La copa de las casas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esBuenAlumno(Mago):-
    realizoUnaAccion(Mago),
    not(esNegativa(Mago)).


hizo(harry,fueraDeCama).
hizo(harry,irA(tercerPiso)).
hizo(harry,irA(bosque)).
hizo(hermione,irA(tercerPiso)).
hizo(hermione,irA(seccionRestringidaBibloteca)).
hizo(draco,irA(masmorras)).
hizo(ron,buenaAccion(jugarAjedrez,50)).
hizo(hermione,buenaAccion(salvarAmigos,50)).
hizo(harry,buenaAccion(ganarVoldermort,60)).
hizo(hermione,contestarPregunta(dondeEstaBezoar,20,snape)).
hizo(hermione,contestarPregunta(levitarPluma,25,flitwick)).



puntajeGenerado(fueraDeCama, -50).
puntajeGenerado(irA(Lugar),Puntaje):-
    lugarProhibido(Lugar,Puntaje).
puntajeGenerado(buenaAccion(_,Puntaje),Puntaje).
puntajeGenerado(contestarPregunta(_,Puntaje,_),Puntaje).


lugarProhibido(bosque,-50).
lugarProhibido(seccionRestringidaBibloteca,-10).
lugarProhibido(tercerPiso,-75).

realizoUnaAccion(Mago):-
    hizo(Mago,_).

esNegativa(Mago):-
    hizo(Mago,HechoRealizado),
    puntajeGenerado(HechoRealizado,Puntaje),
    Puntaje < 0.

accionesPosibles(Accion):-
    hizo(_,Accion).
accionesPosibles(Accion):-
    hizo(_,buenaAccion(Accion,_)).
accionesPosibles(Accion):-
    hizo(_,irA(Accion)).

accionRecurrente(Accion):-
    hizo(Mago,Accion),
    hizo(OtroMago,Accion),
    Mago\=OtroMago.

esDe(hermione, gryffindor).

esDe(ron, gryffindor).

esDe(harry, gryffindor).

esDe(draco, slytherin).

esDe(luna, ravenclaw).

puntajeGeneradoPor(Persona,Puntaje):-
    hizo(Persona,Accion),
    puntajeGenerado(Accion,Puntaje).

sumarPuntosDeCasa(Casa,PuntosTotales):-
    esDe(_,Casa),
    findall(Puntaje,(esDe(Persona,Casa),puntajeGeneradoPor(Persona,Puntaje)), ListaPuntajes),
    sum_list(ListaPuntajes,PuntosTotales).
sumarPuntosDeCasa(casaPerri,100).
    
casaGanadora(Casa):-
    sumarPuntosDeCasa(Casa,PuntosTotalesUno),
    forall((sumarPuntosDeCasa(CasaDos,PuntosTotalesDos),Casa\=CasaDos),PuntosTotalesUno > PuntosTotalesDos).


    