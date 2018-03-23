000100151019      * ?------------------------------------------------------------------ *
000200151019      * ?-  Riceve un DEPOT restituisce l'indirizzo originale preso dal     *
000300151019      * ?-  Cappario DPD.                                                   *
000400151019      * ?------------------------------------------------------------------ *
000500990504     H* ACTGRP=*CALLER
000600990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000700151019      * ?__________________________________________________                 *
000800150723     fdpcdp01l  if   e           k disk
000900151020     ftabel00f  if   e           k disk
001000151019      * ?__________________________________________________                 *
001100151020      * Tabella codici nazioni
001200151020     D NAZ             S              3    DIM(500)
001300151020     D ISO             S              3    DIM(500)
001400151020     D LAD             S              3s 0 DIM(500)
001500151020     D ds15          e ds
001600151020      * ?__________________________________________________                 */
001700151020     d tisie10ds     e ds
001800151019      * ?------------------------------------------------------------------ *
001900150723      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
002000151019      * ?------------------------------------------------------------------ *
002100150723     D TxtInOut        S           2048
002200150723     D ElencoChar      S            256
002300150723     D TipoElenco      S              1
002400150723     D CharSost        S              1
002500150723     D UpperCase       S              1
002600150723     D ChkNull         S              1
002700150723     D CharNull        S              1
002800150723     D Esito_check     S              1
002900150723     D  Dati           s           1500
003000150723      * ?------------------------------------------------------------------ */
003100150723      * ?controllo x CEDINTESA
003200150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
003300150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
003400150723     d bart_it         C                   CONST('@Bartolini.it;')
003500150723     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
003600150723     D   Digits        C                   '0123456789'
003700150723     D*------------------
003800150723      * ?set caratteri Maiuscoli/Minuscoli
003900150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004000150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004100060628      * ?__________________________________________________                 */
004200150723     c                   seton                                        lr
004300150723      * primi 4 caratteri del Parcel contengono il Depot di origine
004400150723      * del Parcel.
004500151020     c                   movel     i10DEP        Origin_Depot      4
004600150723      *
004700150723      *  Reperisce l'ultima versione del Cappario
004800150723     c     *hival        setll     dpcdp01l
004900150723     c                   readp     dpcdp01l
005000150723      *
005100150723     c                   if        not %EoF(dpcdp01l)
005200150723     c                   z-add     CDPVER        Versione          5 0
005300150723     c     K_CAPPARIO    CHAIN     dpcdp01l
005400151020     c                   if        not %Found(dpcdp01l)
005500151020     c                   eval      O10ERR     = 'E'
005600151020     c                   else
005700150723      **
005800151020     c                   eval      O10CDPPTC  = CDPPTC
005900150723      *
006000150723      *   Sposta il record a tracciato libero su un campo di lavoro       =
006100150723     c                   clear                   dati
006200150723     C     minu:maiu     XLATE     CDPNAM1       dati
006300150723     c                   exsr      Syntax
006400151020     c                   eval      O10CDPNAM1 = dati
006500150723      **
006600150723     c                   clear                   dati
006700150723     C     minu:maiu     XLATE     CDPNAM2       dati
006800150723     c                   exsr      Syntax
006900151020     c                   eval      O10CDPNAM2 = dati
007000150723      **
007100150723     c                   clear                   dati
007200150723     C     minu:maiu     XLATE     CDPADR1       dati
007300150723     c                   exsr      Syntax
007400151020     c                   eval      O10CDPADR1 = dati
007500150723      **
007600150723     c                   clear                   dati
007700150723     C     minu:maiu     XLATE     CDPADR2       dati
007800150723     c                   exsr      Syntax
007900151020     c                   eval      O10CDPADR2 = dati
008000150723      **
008100150723     c                   clear                   dati
008200150723     C     minu:maiu     XLATE     CDPCTY        dati
008300150723     c                   exsr      Syntax
008400151020     c                   eval      O10CDPCTY  = dati
008500150723      *
008600151020      *
008700151020     c                   z-add     1             X
008800151020     c                   movel     CDPISO2       CDPISO2_3         3
008900151020     c     CDPISO2_3     lookup    ISO(x)                                 21
009000151020     c   21              eval      O10CDPNAZ = NAZ(x)
009100151020      *
009200150723     c                   end
009300150723     c                   end
009400150723      *
009500150723     c                   RETURN
009600060628      * ?___________________________________________________                */
009700051202     c     *inzsr        begsr
009800060628      * ?___________________________________________________                */
009900051202     c     *entry        plist
010000151020     c                   parm                    tisie10ds
010100150723      *   campi di output
010200151020     c                   clear                   O10CDPNAM1
010300151020     c                   clear                   O10CDPNAM2
010400151020     c                   clear                   O10CDPADR1
010500151020     c                   clear                   O10CDPADR2
010600151020     c                   clear                   O10CDPPTC
010700151020     c                   clear                   O10CDPCTY
010800151020     c                   clear                   O10CDPNAZ
010900151020     c                   clear                   O10ERR
011000060628      *
011100150723     c     K_CAPPARIO    klist
011200150723     C                   KFLD                    VERSIONE
011300150723     C                   KFLD                    ORIGIN_DEPOT
011400151020      *
011500151020     C     KTAB          KLIST
011600151020     C                   KFLD                    TBLKUT
011700151020     C                   KFLD                    TBLCOD
011800150723     C*------------------
011900151020      *
012000151020     C*------------------
012100151020     C* Caricamento Tabella 15
012200151020     C                   z-add     1             tblKUT
012300151020     C                   move      '15'          tblCOD
012400151020     C                   Z-ADD     0             X                 3 0
012500151020     C     KTAB          setll     TABEL00F
012600151020     C     KTAB          reade     TABEL00F
012700151020     C                   DOW       not %Eof(TABEL00F)
012800151020     C     TBLFLG        IFEQ      *BLANKS
012900151020     C                   ADD       1             X
013000151020     C                   MOVEL     TBLKEY        NAZ(X)
013100151020     C                   MOVEL     TBLUNI        DS15
013200151020     C                   MOVEL     §15COD        ISO(X)
013300151020     C                   MOVEL     §15LAD        LAD(X)
013400151020     C                   END
013500151020     C     KTAB          READE     TABEL00F
013600151020     C                   END
013700151020     C*
013800151020     C*------------------
013900060628     c                   endsr
014000060627      * ?_____________________________________________________________      */
014100150723      * *------------------------*
014200150723     C     Syntax        begSR
014300150723      * *------------------------*
014400150723      *
014500150723     c                   movel     dati          TxtInOut
014600150723     c                   eval      ElencoChar = SETA
014700150723     c                   eval      TipoElenco = *blank
014800150723     c                   eval      CharSost   = *blank
014900150723     c                   eval      UpperCase  = '1'
015000150723     c                   eval      ChkNull    = *blank
015100150723     c                   eval      CharNull   = *blank
015200150723     c                   eval      Esito_check= *blank
015300150723      *
015400150723     c                   call      'XCHKCHAR'
015500150723     c                   parm                    TxtInOut
015600150723     c                   parm                    ElencoChar
015700150723     c                   parm                    TipoElenco
015800150723     c                   parm                    CharSost
015900150723     c                   parm                    UpperCase
016000150723     c                   parm                    ChkNull
016100150723     c                   parm                    CharNull
016200150723     c                   parm                    Esito_check
016300150723      *
016400150723      *   se è stato modificato qualcosa nella riga di FLAT
016500150723     c                   if        Esito_check = '1'
016600150723     c                   movel     TxtInOut      dati
016700150723     c                   end
016800150723      *
016900150723     C                   ENDSR
017000150723      *-------------------------------------------------------------------------
