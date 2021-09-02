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

contenidos(ana,tiktok,([beto,evelyn],1)).
contenidos(ana,tiktok,([ana],1)).
contenidos(ana,instagram,([ana])).
contenidos(beto,instagram,([])).
contenidos(evelyn,instagram,([evelyn,cami])).
contenidos(cami,twitch,(lol)).



tematicas(juego,lol).
tematicas(juego,minecraft).
tematicas(juego,aoe).

adictiva(Red):-
    queTipoDeRed(video,Red),
    videoAdictivo(Red).
adictiva(Red):-
    queTipoDeRed(stream,Red),
    stream(Red).
adictiva(Red):-
    queTipoDeRed(foto,Red),
    foto(Red).

videoAdictivo(Red):-
    contenidos(_,Red,(_,TiempoVideo)),
    TiempoVideo < 3.

stream(Red):-
    contenidos(_,Red,Tematica),
    tematicas(juego,Tematica).

foto(Red):-
    contenidos(_,Red,ListaFotos),
    length(ListaFotos, CuantasPersonas),
    CuantasPersonas < 4.

colaboran(Persona,PersonaDos):-
    queTipoDeRed(video,_),
    contenidos(Persona,_,(ListaPersona,_)),
    member(PersonaDos,ListaPersona).
colaboran(Persona,PersonaDos):-
    queTipoDeRed(foto,_),
    contenidos(Persona,_,(ListaPersona)),
    member(PersonaDos,ListaPersona).
colaboran(Persona,PersonaDos):-
    contenidos(Persona,_,_),
    queTipoDeRed(stream,_),
    PersonaDos==Persona.
colaboran(Persona,PersonaDos):-
    colaboran(PersonaDos,Persona).

caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,PersonaDos),
    influencer(PersonaDos).
caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,PersonaDos),
    caminoALaFama(PersonaDos).

    

    






    





    












