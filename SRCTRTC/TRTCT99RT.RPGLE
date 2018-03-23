000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300130419     H* TRTCT99R_T       ---------------------------------------------------*
000400110519      **?__________________________________________________________________ */
000500130419     H*         TESTER per provare il TRTCT99C/R                            *
000600050927      **?__________________________________________________________________ */
000700130415     FfnORM01L  IF   E           K DISK
000800130502     FEDtab01L  IF   E           K DISK
000900110519      **?__________________________________________________________________ */
001000130415      **  Elenco DS di tutti i Segmenti da tradurre
001100130415      **?------------------------------------------------------------------ */
001200130502???? D  edidsPT      E DS
001300130502???? D trtct99ds     E DS
001400130419???? D kpjba         E DS
001500130502     D SAV_Cliente     S                   like(T99Cliente)
001600130419      **?************************************************************************
001700130415     c     *entry        plist
001800130415     C                   PARM                    kpjba
001900130416     c*
002000130502     C                   z-add     1             conta             5 0
002100130416      *
002200130415     c* Aggancia l'ORM per scrivere il messaggio
002300130419     c     *loval        setll     fnORM01l
002400130419     c                   read      fnORM01l
002500130415     c*
002600130419     C                   dow       not %EoF(FnORM01L)
002700130502     c                   if        ormpor >300
002800130502     C                   clear                   trtct99DS
002900130502      *
003000130502      * trova il cliente sulla PT
003100130502     c                   exsr      CARICA_TABELLE
003200130502      *
003300130502     C                   if        T99CLIENTE <> SAV_Cliente
003400130502     c                   if        SAV_Cliente  <> *zeros
003500130502     c                   eval      T99FUNZION  ='CLOSE'
003600130502     c                   eval      T99ESITO    = *blank
003700130502     C                   eval      kpjbu = trtct99DS
003800130502     c                   call      'TRTCT99C'
003900130502     c                   parm                    kpjba
004000130502     C                   movel     kpjbu         trtct99DS
004100130502     c                   if        t99esito <> *blank
004200130502     c                   LEAVE
004300130502     c                   end
004400130502     c                   end
004500130502     C                   z-add     T99CLIENTE    SAV_Cliente
004600130502     c                   eval      T99FUNZION  ='OPEN'
004700130502     c                   eval      T99ESITO    = *blank
004800130502     C                   eval      kpjbu = trtct99DS
004900130502     c                   call      'TRTCT99C'
005000130502     c                   parm                    kpjba
005100130502     C                   movel     kpjbu         trtct99DS
005200130502     c                   if        t99esito <> *blank
005300130502     c                   LEAVE
005400130502     c                   end
005500130502     c                   end
005600130502      *
005700130502     c                   eval      T99FUNZION  ='EXEC'
005800130419     c                   eval      T99ORMPOE   = ormpoe
005900130419     c                   eval      T99ORMNSR   = ormnsr
006000130419     c                   eval      T99ORMNOR   = ormnor
006100130419     c                   eval      T99ORMNRV   = ormnrv
006200130419     c                   eval      T99ESITO    = *blank
006300130419     C                   eval      kpjbu = trtct99DS
006400130419     c                   call      'TRTCT99C'
006500130419     c                   parm                    kpjba
006600130419     C                   movel     kpjbu         trtct99DS
006700130419     c                   if        t99esito <> *blank
006800130419     c                   leave
006900130419     c                   end
007000130502      *
007100130502     C                   add       1             conta
007200130502     c                   if        conta = 20
007300130502     c                   leave
007400130502     c                   end
007500130502      *
007600130502     c                   end
007700130419     c                   read      fnORM01l
007800130416     c                   end
007900130502      *
008000130502     c                   eval      T99FUNZION  ='CLOSE'
008100130502     c                   eval      T99ESITO    = *blank
008200130502     C                   eval      kpjbu = trtct99DS
008300130502     c                   call      'TRTCT99C'
008400130502     c                   parm                    kpjba
008500130502     C                   movel     kpjbu         trtct99DS
008600130502     c                   if        t99esito <> *blank
008700130502     c                   goto      FINE
008800130502     c                   end
008900130502      *
009000130502     c     FINE          tag
009100050705     c                   seton                                        lr
009200130415      **?------------------------------------------------------------------ */
009300130502      * ?------------------------------------------------------------------ */
009400130502      *?    CARICA TABELLE SU SCHIERE
009500130502      * ?------------------------------------------------------------------ */
009600130502     C     CARICA_tabelleBEGSR
009700130502      *
009800130502      * Caricamento Tabella Partner esteri
009900130502     C                   MOVEL     'PT'          TABCOD
010000130502     C     TABCOD        CHAIN     EDTAB01L                           30
010100130502     C     *IN30         DOWEQ     '0'
010200130502      *
010300130502      * carica solo quello che interessa uno .... due ....tre record al max.
010400130502     C                   IF        TABFLG = *BLANKS
010500130502     C                   MOVEL     TABUNI        edidsPT
010600130502      *
010700130502     C                   if          §PTLNP = ormPOR
010800130502     c                   z-add     §ptKSC        T99CLIENTE
010900130502     c                   z-add     §ptLNP        T99LINEA
011000130502     c                   LEAVE
011100130502     c                   end
011200130502      *
011300130502     C                   END
011400130502     C     TABCOD        READE     EDTAB01L                               30
011500130502     C                   END
011600130502      *
011700130502     C                   ENDSR
011800130502     c*==================================================================*
