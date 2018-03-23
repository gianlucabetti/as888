000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300060628      * ?__________________________________________________                 */
000400150723     fdpcdp01l  if   e           k disk
000500150723     ftabel00f  if   e           k disk
000600151015     ftntbe01l  if   e           k disk
000700150723      * ?__________________________________________________                 */
000800150723      * Tabella codici nazioni
000900150723     D NAZ             S              3    DIM(500)
001000150723     D ISO             S              3    DIM(500)
001100150723     D LAD             S              3s 0 DIM(500)
001200150723      * ?__________________________________________________                 */
001300150723     d fnlg27ds      e ds
001400150723     D ds15          e ds
001500151015     D dLPD          e ds
001600150723      * ?------------------------------------------------------------------ */
001700150723      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
001800150723      * ?------------------------------------------------------------------ */
001900150723     D TxtInOut        S           2048
002000150723     D ElencoChar      S            256
002100150723     D TipoElenco      S              1
002200150723     D CharSost        S              1
002300150723     D UpperCase       S              1
002400150723     D ChkNull         S              1
002500150723     D CharNull        S              1
002600150723     D Esito_check     S              1
002700150723     D  Dati           s           1500
002800150723      * ?------------------------------------------------------------------ */
002900150723      * ?controllo x CEDINTESA
003000150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
003100150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
003200150723     d bart_it         C                   CONST('@Bartolini.it;')
003300150723     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
003400150723     D   Digits        C                   '0123456789'
003500150723     D*------------------
003600150723      * ?set caratteri Maiuscoli/Minuscoli
003700150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
003800150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
003900060628      * ?__________________________________________________                 */
004000150723     c                   seton                                        lr
004100150723      * primi 4 caratteri del Parcel contengono il Depot di origine
004200150723      * del Parcel.
004300150723     c                   movel     i27NPR        Origin_Depot      4
004400150723      *
004500150723      *  Reperisce l'ultima versione del Cappario
004600150723     c     *hival        setll     dpcdp01l
004700150723     c                   readp     dpcdp01l
004800150723      *
004900150723     c                   if        not %EoF(dpcdp01l)
005000150723     c                   z-add     CDPVER        Versione          5 0
005100150723     c     K_CAPPARIO    CHAIN     dpcdp01l
005200150723     c                   if        %Found(dpcdp01l)
005300150723      **
005400150723     c                   eval      O27CDPPTC  = CDPPTC
005500150723      *
005600150723      *   Sposta il record a tracciato libero su un campo di lavoro       =
005700150723     c                   clear                   dati
005800150723     C     minu:maiu     XLATE     CDPNAM1       dati
005900150723     c                   exsr      Syntax
006000150723     c                   eval      O27CDPNAM1 = dati
006100150723      **
006200150723     c                   clear                   dati
006300150723     C     minu:maiu     XLATE     CDPNAM2       dati
006400150723     c                   exsr      Syntax
006500150723     c                   eval      O27CDPNAM2 = dati
006600150723      **
006700150723     c                   clear                   dati
006800150723     C     minu:maiu     XLATE     CDPADR1       dati
006900150723     c                   exsr      Syntax
007000150723     c                   eval      O27CDPADR1 = dati
007100150723      **
007200150723     c                   clear                   dati
007300150723     C     minu:maiu     XLATE     CDPADR2       dati
007400150723     c                   exsr      Syntax
007500150723     c                   eval      O27CDPADR2 = dati
007600150723      **
007700150723     c                   clear                   dati
007800150723     C     minu:maiu     XLATE     CDPCTY        dati
007900150723     c                   exsr      Syntax
008000150723     c                   eval      O27CDPCTY  = dati
008100150723      *
008200150723     c                   z-add     1             X
008300150723     c                   movel     CDPISO2       CDPISO2_3         3
008400150723     c     CDPISO2_3     lookup    ISO(x)                                 21
008500150723     c   21              eval       O27LNA = LAD(x)
008600150723     c   21              eval      O27CDPNAZ = NAZ(x)
008700150723      *
008800151015     C                   movel     'LPD'         TBeCOD
008900151015     C                   movel(p)  o27LNA        TBeKe1
009000151015     C                   clear                   TBeKe2
009100151015     C                   clear                   dLPD
009200151015     C     KTBE          chain     tntbe01l
009300151015     c                   if        %Found(tntbe01l)
009400151015     C                   eval        dLPD = tbeUNI
009500151015     c                   end
009600151015      *
009700150723     c                   end
009800150723     c                   end
009900150723      *
010000150723     c                   RETURN
010100060628      * ?___________________________________________________                */
010200051202     c     *inzsr        begsr
010300060628      * ?___________________________________________________                */
010400051202     c     *entry        plist
010500150723     c                   parm                    fnlg27ds
010600150723      *   campi di output
010700150723     c                   clear                   O27LNA
010800150723     c                   clear                   O27CDPNAM1
010900150723     c                   clear                   O27CDPNAM2
011000150723     c                   clear                   O27CDPADR1
011100150723     c                   clear                   O27CDPADR2
011200150723     c                   clear                   O27CDPPTC
011300150723     c                   clear                   O27CDPCTY
011400150723     c                   clear                   O27CDPNAZ
011500060628      *
011600150723     c     K_CAPPARIO    klist
011700150723     C                   KFLD                    VERSIONE
011800150723     C                   KFLD                    ORIGIN_DEPOT
011900150723      *
012000150723     C     KTAB          KLIST
012100150723     C                   KFLD                    TBLKUT
012200150723     C                   KFLD                    TBLCOD
012300151015      *
012400151015     C     KTBE          KLIST
012500151015     C                   KFLD                    TBeCOD
012600151015     C                   KFLD                    TBeKe1
012700151015     C                   KFLD                    TBeKe2
012800150723      *
012900150723     C*------------------
013000150723     C* Caricamento Tabella 15
013100150723     C                   z-add     1             tblKUT
013200150723     C                   move      '15'          tblCOD
013300150723     C                   Z-ADD     0             X                 3 0
013400150723     C     KTAB          setll     TABEL00F
013500150723     C     KTAB          reade     TABEL00F
013600150723     C                   DOW       not %Eof(TABEL00F)
013700150723     C     TBLFLG        IFEQ      *BLANKS
013800150723     C                   ADD       1             X
013900150723     C                   MOVEL     TBLKEY        NAZ(X)
014000150723     C                   MOVEL     TBLUNI        DS15
014100150723     C                   MOVEL     §15COD        ISO(X)
014200150723     C                   MOVEL     §15LAD        LAD(X)
014300150723     C                   END
014400150723     C     KTAB          READE     TABEL00F
014500150723     C                   END
014600150723     C*
014700150723     C*------------------
014800060628     c                   endsr
014900060627      * ?_____________________________________________________________      */
015000150723      * *------------------------*
015100150723     C     Syntax        begSR
015200150723      * *------------------------*
015300150723      *
015400150723     c                   movel     dati          TxtInOut
015500150723     c                   eval      ElencoChar = SETA
015600150723     c                   eval      TipoElenco = *blank
015700150723     c                   eval      CharSost   = *blank
015800150723     c                   eval      UpperCase  = '1'
015900150723     c                   eval      ChkNull    = *blank
016000150723     c                   eval      CharNull   = *blank
016100150723     c                   eval      Esito_check= *blank
016200150723      *
016300150723     c                   call      'XCHKCHAR'
016400150723     c                   parm                    TxtInOut
016500150723     c                   parm                    ElencoChar
016600150723     c                   parm                    TipoElenco
016700150723     c                   parm                    CharSost
016800150723     c                   parm                    UpperCase
016900150723     c                   parm                    ChkNull
017000150723     c                   parm                    CharNull
017100150723     c                   parm                    Esito_check
017200150723      *
017300150723      *   se è stato modificato qualcosa nella riga di FLAT
017400150723     c                   if        Esito_check = '1'
017500150723     c                   movel     TxtInOut      dati
017600150723     c                   end
017700150723      *
017800150723     C                   ENDSR
017900150723      *-------------------------------------------------------------------------
