000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200161220     H* FNLV84R *----------------------------------------------------*
000300170421     H* - Driver di Verifica abilitaz. per "Avviso merce partita"
000301110610     h*
000400000000     H*--------------------------------------------------------------*
000500161220     FFiblr01l  IF   E           K DISK
000501170426     FAzorg01l  IF   E           K DISK
001200030722     F**
001600070313     d
001601161220     d w_dat           s                   like(ilv84drf)
001602161220     d dataiso_cor     s               d   datfmt(*iso)
001603170426     d wabi            s                   like(§blrabi)
001700140205      * ds parametri input/output
001800161220     D fnlv84ds      E DS
001801170421     D dblrflo       E DS
001802170426
001803170426     D og143         E DS
002000070313     d
002004140206
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000161220     C                   PARM                    fnlv84ds
004300940223     C*---------------------------------------------------------------*
004400161220     C* TIPO LANCIO, ilv84TLA "C" -           CHIUSO CON LR
004500161220     C* TIPO LANCIO, ilv84TLA "L" - ELABORO E CHIUDO CON LR
004600161220     C* TIPO LANCIO, ilv84TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800161220    1C     ilv84TLA      IFNE      'C'
004900161220     c                   clear                   OLV84esito
004902170421     c                   clear                   OLV84flo
004903161220
004904161220     c* Data: assumo la data odierna o la data ricevuta se > della
004905161220     c* data odierna (serve per le bolle in sospensione)
004906161220     c                   clear                   w_dat
004907161220     c                   eval      dataiso_cor=(%date())
004908161220     c                   eval      w_dat=%dec(dataiso_cor)
004909161220     c                   if        ilv84drf>w_dat
004910161220     c                   eval      w_dat=ilv84drf
004911161220     c                   endif
004912170426     c* Network della linea di arrivo
004913170426        clear og143;
004914170426        chain ilv84lna  azorg01l    ;
004915170426        if %found(azorg01l) ;
004916170426           og143=orgde3 ;
004917170426        endif   ;
004918170426
004919170426     c                   clear                   wabi
004920161220
004921161220     c     w_dat         setgt     fiblr01l
004922161220     c                   readp     fiblr01l
004923161220    1c                   dow       not %eof(fiblr01l) and
004924170426     c                             wabi=*blanks
004925161220     c* scarto se scaduto
004926161220    2c                   if        w_dat<=blrdsc
004927161220     c* verifico i dati
004928161220     c* Tipo servizio: se passato scarto il record se diverso
004929161220    3c                   if        ilv84tsp<>*blanks
004930161220     c                             and blrtsp<>*blanks
004931161220     c                             and ilv84tsp<>blrtsp
004932161220     c                   readp     fiblr01l
004933161220     c                   iter
004934161220    3c                   endif
004935161220     c* Partenza
004936161220    3c                   if        blrtip<>*blanks and blrpar<>999
004937161220     c                   select
004938161220     c                   when      blrtip='F'
004939161220     c                             and ilv84lnp<>blrpar
004940161220     c                   readp     fiblr01l
004941161220     c                   iter
004942161220     c                   when      blrtip='T'
004943161220     c                             and ilv84tfp<>blrpar
004944161220     c                   readp     fiblr01l
004945161220     c                   iter
004946161220     c                   endsl
004947161220    3c                   endif
004948161220     c* Arrivo
004949161220    3c                   if        blrtia<>*blanks and blrarr<>999
004950161220     c                   select
004951161220     c                   when      blrtia='F'
004952161220     c                             and ilv84lna<>blrarr
004953161220     c                   readp     fiblr01l
004954161220     c                   iter
004955161220     c                   when      blrtia='T'
004956161220     c                             and ilv84tfa<>blrarr
004957161220     c                   readp     fiblr01l
004958161220     c                   iter
004959161220     c                   endsl
004960161220    3c                   endif
004961170421     c                   eval      dblrflo=blrflo
004962170426     c                   eval      wabi=§blrabi
004963170426     c                   if        §blrabi='B'
004964170426     c                   eval      olv84esito='1'
004965170426     c                   endif
004966161220    2c                   endif
004967161220     c
004968161220     c                   readp     fiblr01l
004969161220     c                   enddo
004970170426     c* se non trovati record validi e spedizione export eex
004971170426     c* significa che bisogna bloccare
004972170426     c                   if        wabi=*blanks and §ogntw='EEX'
004973170426     c                   eval      olv84esito='1'
004974170426     c                   endif
005400070412     c
005500051123     c
012001140205    1c                   endif
012100051123     c
012200161220    1C     Ilv84tla      IFNE      ' '
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
