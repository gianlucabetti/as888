000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300130430     H* TRTCT97R_T       ---------------------------------------------------*
000400110519      **?__________________________________________________________________ */
000500130419     H*         TESTER per provare il TRTCT99C/R                            *
000600050927      **?__________________________________________________________________ */
000700130415     FfnORM01L  IF   E           K DISK
000800110519      **?__________________________________________________________________ */
000900130415      **  Elenco DS di tutti i Segmenti da tradurre
001000130415      **?------------------------------------------------------------------ */
001100130430???? D trtct97ds     E DS
001200130430     D KPJBA         E DS
001300130430     D     svkpjbu     s                   like(kpjbu)
001400130419      **?************************************************************************
001500130430     c     *entry        plist
001600130430     C                   PARM                    kpjba
001700130430     C                   clear                   trtct97DS
001800130430     C                   z-add     1             conta             5 0
001900130416      *
002000130430     c                   clear                   trtct97ds
002100130430     c                   move      '3600001'     T97CLIENTE
002200130430     c                   move      '360'         T97LINEA
002300130430     c                   eval      T97FUNZION  ='OPEN'
002400130430     c                   eval      T97ESITO    = *blank
002500130430     C                   eval      kpjbu = trtct97DS
002600130430     c                   call      'TRTCT97C'
002700130430     c                   parm                    kpjba
002800130430     C                   movel     kpjbu         trtct97DS
002900130430     c                   if        t97esito <> *blank
003000130430     c                   goto      FINE
003100130430     c                   end
003200130430      *
003300130415     c* Aggancia l'ORM per scrivere il messaggio
003400130419     c     *loval        setll     fnORM01l
003500130419     c                   read      fnORM01l
003600130415     c*
003700130419     C                   dow       not %EoF(FnORM01L)
003800130430     c                   eval      T97FUNZION  ='EXEC'
003900130430     c                   move      '3600001'     T97CLIENTE
004000130430     c                   move      '360'         T97LINEA
004100130430     c                   eval      T97ORMPOE   = ormpoe
004200130430     c                   eval      T97ORMNSR   = ormnsr
004300130430     c                   eval      T97ORMNOR   = ormnor
004400130430     c                   eval      T97ORMNRV   = ormnrv
004500130430     c                   eval      T97ESITO    = *blank
004600130430     C                   eval      kpjbu = trtct97DS
004700130430     c                   call      'TRTCT97C'
004800130419     c                   parm                    kpjba
004900130430     C                   movel     kpjbu         trtct97DS
005000130430     c                   if        t97esito <> *blank
005100130419     c                   leave
005200130419     c                   end
005300130430      *
005400130430     C                   add       1             conta
005500130430     c                   if        conta = 15
005600130430     c                   leave
005700130430     c                   end
005800130419     c                   read      fnORM01l
005900130416     c                   end
006000130416      *
006100130430      *
006200130430     c                   clear                   trtct97ds
006300130430     c                   move      '3600001'     T97CLIENTE
006400130430     c                   move      '360'         T97LINEA
006500130430     c                   eval      T97FUNZION  ='CLOSE'
006600130430     c                   eval      T97ESITO    = *blank
006700130430     C                   eval      kpjbu = trtct97DS
006800130430     c                   call      'TRTCT97C'
006900130430     c                   parm                    kpjba
007000130430     C                   movel     kpjbu         trtct97DS
007100130430     c                   if        t97esito <> *blank
007200130430     c                   goto      FINE
007300130430     c                   end
007400130430      *
007500130430     c     FINE          tag
007600050705     c                   seton                                        lr
007700130415      **?------------------------------------------------------------------ */
