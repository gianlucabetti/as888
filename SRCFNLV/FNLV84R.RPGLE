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
001604170428     d w_tfp           s                   like(ilv84tfp)
001605170428     d w_tfa           s                   like(ilv84tfa)
001700140205      * ds parametri input/output
001701170428     D fnlv55ds      E DS
001800161220     D fnlv84ds      E DS
001802170421     D dblrflo       E DS
001803170426
001804170426     D og143         E DS
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
004903170428     c                   clear                   w_tfp
004904170428     c                   clear                   w_tfa
004905161220
004906161220     c* Data: assumo la data odierna o la data ricevuta se > della
004907161220     c* data odierna (serve per le bolle in sospensione)
004908161220     c                   clear                   w_dat
004909161220     c                   eval      dataiso_cor=(%date())
004910161220     c                   eval      w_dat=%dec(dataiso_cor)
004911161220     c                   if        ilv84drf>w_dat
004912161220     c                   eval      w_dat=ilv84drf
004913161220     c                   endif
004914170428     c* TFP: se non ricevuto lo determino
004915170428     c                   eval      w_tfp=ilv84tfp
004916170428     c                   eval      w_tfa=ilv84tfa
004917170428     c                   if        ilv84tfp=0
004918170428     c                   clear                   fnlv55ds
004919170428     c                   eval      d55tpt='P'
004920170428     c                   eval      d55lin=ilv84lnp
004921170428     c                   eval      d55drf=w_dat
004922170428     c                   call      'FNLV55R'
004923170502     C                   PARM                    fnlv55ds
004924170428     C     D55ERR        IFEQ      ' '
004925170428     C                   MOVEL     D55TFP        w_tfp
004926170428     C                   ELSE
004927170428     C                   eval      w_tfp=ilv84lnp
004928170428     C                   ENDIF
004929170428     c                   endif
004930170428     c* TFA: se non ricevuto lo determino
004931170428     c                   if        ilv84tfa=0
004932170428     c                   clear                   fnlv55ds
004933170428     c                   eval      d55tpt='A'
004934170428     c                   eval      d55lin=ilv84lna
004935170428     c                   eval      d55lnp=ilv84lnp
004936170428     c                   eval      d55drf=w_dat
004937170428     c                   call      'FNLV55R'
004938170502     C                   PARM                    fnlv55ds
004939170428     C     D55ERR        IFEQ      ' '
004940170428     C                   MOVEL     D55TFA        w_tfa
004941170428     C                   ELSE
004942170428     C                   eval      w_tfa=ilv84lna
004943170428     C                   ENDIF
004944170428     c                   endif
004945170426     c* Network della linea di arrivo
004946170426        clear og143;
004947170426        chain ilv84lna  azorg01l    ;
004948170426        if %found(azorg01l) ;
004949170426           og143=orgde3 ;
004950170426        endif   ;
004951170426
004952170426     c                   clear                   wabi
004953161220
004954161220     c     w_dat         setgt     fiblr01l
004955161220     c                   readp     fiblr01l
004956161220    1c                   dow       not %eof(fiblr01l) and
004957170426     c                             wabi=*blanks
004958161220     c* scarto se scaduto
004959161220    2c                   if        w_dat<=blrdsc
004960161220     c* verifico i dati
004961161220     c* Tipo servizio: se passato scarto il record se diverso
004962161220    3c                   if        ilv84tsp<>*blanks
004963161220     c                             and blrtsp<>*blanks
004964161220     c                             and ilv84tsp<>blrtsp
004965161220     c                   readp     fiblr01l
004966161220     c                   iter
004967161220    3c                   endif
004968161220     c* Partenza
004969161220    3c                   if        blrtip<>*blanks and blrpar<>999
004970161220     c                   select
004971161220     c                   when      blrtip='F'
004972161220     c                             and ilv84lnp<>blrpar
004973161220     c                   readp     fiblr01l
004974161220     c                   iter
004975161220     c                   when      blrtip='T'
004976170428     c                             and w_tfp<>blrpar
004977161220     c                   readp     fiblr01l
004978161220     c                   iter
004979161220     c                   endsl
004980161220    3c                   endif
004981161220     c* Arrivo
004982161220    3c                   if        blrtia<>*blanks and blrarr<>999
004983161220     c                   select
004984161220     c                   when      blrtia='F'
004985161220     c                             and ilv84lna<>blrarr
004986161220     c                   readp     fiblr01l
004987161220     c                   iter
004988161220     c                   when      blrtia='T'
004989170428     c                             and w_tfa<>blrarr
004990161220     c                   readp     fiblr01l
004991161220     c                   iter
004992161220     c                   endsl
004993170428    3c                   endif
004994170421     c                   eval      dblrflo=blrflo
004995170426     c                   eval      wabi=§blrabi
004996170426     c                   if        §blrabi='B'
004997170426     c                   eval      olv84esito='1'
004998170426     c                   endif
004999161220    2c                   endif
005000161220     c
005001161220     c                   readp     fiblr01l
005002161220     c                   enddo
005003170426     c* se non trovati record validi e spedizione export eex
005004170426     c* significa che bisogna bloccare
005005170426     c                   if        wabi=*blanks and §ogntw='EEX'
005006170426     c                   eval      olv84esito='1'
005007170426     c                   endif
005400070412     c
005500051123     c
012001140205    1c                   endif
012100051123     c
012200161220    1C     Ilv84tla      IFNE      ' '
012201170428     C                   CLEAR                   fnlv55ds
012202170428     C                   MOVEL     'C'           D55TLA
012203170428     C                   CALL      'FNLV55R'
012204170428     C                   PARM                    fnlv55ds
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700140205    1C                   ENDIF
