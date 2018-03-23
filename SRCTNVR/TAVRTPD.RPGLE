000100140603     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200020409      *------------------------------------------------------------------------*
000300140529      * crea dettaglio tariffario TPD quando non c'è                           *
000400020409      *------------------------------------------------------------------------*
000500020409     ftntam01l  ip   e           k disk
000600140529     ftitpt01l  if   e           k disk
000700140603     ftitpd02l  uf a e           k disk
000900020409
001000140529     d kftc            s                   like(tptftc)
001800020409
001900020409     d dsta01        e ds
002500020409
002600020409     c                   movel     tamflo        dsta01
003000020411
003001140529      * File titpt
003002140529      *------------
003003140529     c     ktpt          setll     titpt01l
003004140529     c     Ktpt          reade     titpt01l
003005140603    1c                   dow       not %eof(titpt01l)
003006140529
003007140603    2c                   if        tptatb =  ' '
003100140529      * File titpd
003200020411      *------------
003300140603     c     ktpd          chain     titpd02l
003400140603    3c                   if        not %found(titpd02l)
003401140529      * scrivo il nuovo record
003402140530     c                   clear                   titpd000
003403140530     c                   eval      tpdksc = tptksc
003404140530     c                   eval      tpdctr = tptctr
003405140530     c                   eval      tpdprg = tptprg
003406140530     c                   eval      tpdftc = tptftc
003407140603     c                   eval      tpddtr = *date
003408140603      * imposto flag trasmissione="A" x identificare che sono state create in maniera automatica
003409140603     c                   eval      tpdftr = 'A'
003410140603      * applicazione  inoltro  x le sole tariffe K Località disagiate e I isole va impostata a S
003411140603     c                   if        tptftc = 'K' or tptftc = 'I'
003412140603     c                   eval      tpdain = 'S'
003413140603     c                   endif
003414140603      * imposto lo scaglione in base al tipo tariffa del TITPT
003415140603     c                   if        tpttpg <> '2'
003416140603     c                   z-add     9999999       tpdsgl
003417140603     c                   else
003418140603     c                   z-add     1             tpdsgl
003419140603     c                   endif
003420140603      * imposto il codice tassazione in base a Tariffa Italia o Estero TASMFIE del TNTAM
003421140603     c                   Select
003422140603      * italia e dpd
003423140603     c                   when      tamfie = 'I'
003424140603     c                   eval      tpdcts = 'IT'
003425140603     C                   move      'A 9'         tpdorp
003426140603     c                   write     titpd000
003427140603      * estero no FedEx
003428140603     c                   when      tamfie = 'E' and  §TAFED = ' '
003429140603     c                   eval      tpdcts = 'EE'
003430140603     C                   move      '999'         tpdorp
003431140603     c                   write     titpd000
003432140603      * estero FedEx
003433140603     c                   when      tamfie = 'E' and  §TAFED = 'S'
003434140603     c                   eval      tpdcts = 'XA'
003435140603     C                   move      'X19'         tpdorp
003436140603     c                   write     titpd000
003437140603     c                   eval      tpdcts = 'XB'
003438140603     C                   move      'X29'         tpdorp
003439140603     c                   write     titpd000
003440140603     c                   eval      tpdcts = 'XC'
003441140603     C                   move      'X39'         tpdorp
003442140603     c                   write     titpd000
003443140603     c                   eval      tpdcts = 'XD'
003444140603     C                   move      'X49'         tpdorp
003445140603     c                   write     titpd000
003446140603     c                   eval      tpdcts = 'XE'
003447140603     C                   move      'X59'         tpdorp
003448140603     c                   write     titpd000
003449140603     c                   eval      tpdcts = 'XF'
003450140603     C                   move      'X69'         tpdorp
003451140603     c                   write     titpd000
003452140603     c                   eval      tpdcts = 'XG'
003453140603     C                   move      'X79'         tpdorp
003454140603     c                   write     titpd000
003455140603
003456140603     c                   Endsl
003457140603      *
003458140603     c                   Endif
003459140603     c                   Endif
003460140603      *
003461140603     c     Ktpt          reade     titpt01l
007500020409     c                   enddo
007700020411
007800020411      *---------------------------------------------------------------*
007900020411      *   ROUTINE INIZIALE
008000020411      *---------------------------------------------------------------*
008100020411     c     *inzsr        begsr
008101140603
010900020411     c     ktpd          klist
011000020411     c                   kfld                    tamksc
011100020411     c                   kfld                    tamctr
011200020411     c                   kfld                    tamprg
011300140603     c                   kfld                    tptftc
011400020411
011401140603
011402140603     c     ktpt          klist
011403140603     c                   kfld                    tamksc
011404140603     c                   kfld                    tamctr
011405140603     c                   kfld                    tamprg
011500020411     c                   endsr
011600020411
