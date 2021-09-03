%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parte 1 - Sombrero Seleccionador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

usuario(ana,youtube,3000000).
usuario(ana,instagram,2700000).
usuario(ana,tiktok,1000000).
usuario(ana,twitch,2).
usuario(beto,twitch,120000).
usuario(beto,youtube,6000000).
usuario(beto,instagram,1100000).
usuario(cami,tiktok,2000).
usuario(dani,youtube,1000000).
usuario(evelyn,instagram,1).

queTipoDeRed(video,youtube).
queTipoDeRed(video,tiktok).
queTipoDeRed(foto,instagram).
queTipoDeRed(stream,twitch).

redesExiste(Red):-
    usuario(_,Red,_).

influencer(Persona):-
    usuario(Persona,_,_),
    findall(Seguidores,usuario(Persona,_,Seguidores),ListaSeguidores),
    sum_list(ListaSeguidores, SumaSeguidores),
    SumaSeguidores > 10000.

omnipresente(Persona):-
    usuario(Persona,_,_),
    forall(redesExiste(Red),usuario(Persona,Red,_)).

exclusivo(Persona):-
    usuario(Persona,UnicaRed,_),
    not((usuario(Persona,OtraRed,_),
    OtraRed \= UnicaRed)).

contenidos(ana,tiktok,video([beto,evelyn],1)).
contenidos(ana,tiktok,video([ana],1)).
contenidos(ana,instagram,foto([ana])).
contenidos(beto,instagram,foto([])).
contenidos(evelyn,instagram,foto([evelyn,cami])).
contenidos(cami,twitch,stream(lol)).
contenidos(cami,youtube,video([cami,5])).



aparecerContenido(Persona,OtraPersona):-
    contenidos(Persona,_,video(ListaPersona,1)),
    member(OtraPersona,ListaPersona).
aparecerContenido(Persona,OtraPersona):-
    contenidos(Persona,_,foto([ListaPersona])),
    member(OtraPersona,ListaPersona).
aparecerContenido(Persona,OtraPersona):-
    contenidos(Persona,_,stream(_)),
    OtraPersona=Persona.

colaboran(Persona,OtraPersona):-
    aparecerContenido(Persona,OtraPersona),
    Persona\=OtraPersona.

tematicas(juego,lol).
tematicas(juego,minecraft).
tematicas(juego,aoe).


adictiva(Red):-
    contenidos(_,Red,_),
    forall(contenidos(_,Red,_),contenidosAdictivo(Red)).

contenidoAdictivo(Red):-
    contenidos(_,Red,video(_,Duracion)),
    Duracion < 3.
contenidosAdictivo(Red):-
    contenidos(_,Red,stream(Tematica)),
    tematicas(juego,Tematica).
contenidosAdictivo(Red):-
    contenidos(_,Red,foto(ListaPersonas)),
    length(ListaPersonas,TotalPersonas),
    TotalPersonas < 4.

caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,PersonaDos),
    influencer(PersonaDos).
caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,PersonaDos),
    caminoALaFama(PersonaDos).

    

    






    





    












