000100990504     H* ACTGRP=*CALLER
000200051109     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000300081201     d kpjba         e ds
000400081201     d param           DS
000500090205     d  aut1                        100
000600090205     d  aut2                        100
000700030225      *
000800081201     C* Inizializzo variabili
000900081201     C                   movel     *blanks       wrkEml          253
001000081201     C                   movel     *blanks       wrkMsg         5000
001100081201     C                   movel     *blanks       wrkOgg           44
001200081201     c     *entry        plist
001300081201     c                   parm                    param
001400051109      *
001500051109     C* Valorizzo i campi della e-m@ail
001600090205      * L'invio a Ced@bartolini.it non è necessario è già di Default
001700060927     C                   eval      wrkEml= 'Michele.Malipiero@bartolini.it'
001800051129      *
001900081201     C                   eval      wrkOgg='Errato aggiornamento autista'
002000081201     C                   eval      wrkMsg='Attenzione è stato fatto un ' +
002100081201     C                             'tentativo di aggiornamento errato '+':/N'+
002200090205     C                             aut1  + ' '+':/N'+
002300090205     C*
002400090205     C                             aut2  + ' '+':/N'+
002500090205     C                             'da parte dell''utente' + ' ' + knmus
002600051109     C*
002700051109     C                   call(e)   'TIS701C'
002800051109     C                   parm                    wrkEml
002900051109     C                   parm                    wrkOgg
003000051109     C                   parm                    wrkMsg
003100051109
003200990504     c                   seton                                        lr
