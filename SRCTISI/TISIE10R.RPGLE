000100151019      * ?------------------------------------------------------------------ *
000200151019      * ?-  Riceve un DEPOT restituisce l'indirizzo originale preso dal     *
000300151019      * ?-  Cappario DPD.                                                   *
000400151019      * ?------------------------------------------------------------------ *
000500990504     H* ACTGRP=*CALLER
000600990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000700151019      * ?__________________________________________________                 *
000800151020     ftabel00f  if   e           k disk
000900151019      * ?__________________________________________________                 *
001000151020      * Tabella codici nazioni
001100151020     D NAZ             S              3    DIM(500)
001200151020     D ISO             S              3    DIM(500)
001300151020     D LAD             S              3s 0 DIM(500)
001400151020     D ds15          e ds
001500151020      * ?__________________________________________________                 */
001600151020     d tisie10ds     e ds
001700171117     d tisie10ds1    e ds
001800171117     d tisiedepds    e ds
001900171117     D prmCDP10F     e DS                  extname(DPCDP10F)
002000151019      * ?------------------------------------------------------------------ *
002100150723      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
002200151019      * ?------------------------------------------------------------------ *
002300150723     D TxtInOut        S           2048
002400150723     D ElencoChar      S            256
002500150723     D TipoElenco      S              1
002600150723     D CharSost        S              1
002700150723     D UpperCase       S              1
002800150723     D ChkNull         S              1
002900150723     D CharNull        S              1
003000150723     D Esito_check     S              1
003100150723     D  Dati           s           1500
003200150723      * ?------------------------------------------------------------------ */
003300150723      * ?controllo x CEDINTESA
003400150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
003500150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
003600150723     d bart_it         C                   CONST('@Bartolini.it;')
003700150723     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
003800150723     D   Digits        C                   '0123456789'
003900150723     D*------------------
004000150723      * ?set caratteri Maiuscoli/Minuscoli
004100150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004200150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004300060628      * ?__________________________________________________                 */
004400150723     c                   seton                                        lr
004500150723      * primi 4 caratteri del Parcel contengono il Depot di origine
004600150723      * del Parcel.
004700171117     c                   clear                   tisiedepds
004800171117     c                   clear                   prmCDP10F
004900171117      *-
005000171117     c                   movel     i10DEP        I_DEPOT4
005100171117      *
005200171117      *- Se presente il secondo paramentro contenente il depot lungo 7
005300171117     c                   if        %PARMS = 2
005400171117     c                   if        i10dep7<>'0000000' and i10dep7<>*blank
005500171117     c                   movel     i10DEP7       I_DEPOT7
005600171117     c                   end
005700171117     c                   end
005800150723      *
005900171117      *  aggancia con il DRIVER l'Anagrafica e
006000171117      *    reperisce con l'ultima versione del Cappario
006100171117     c                   call      'TISIEDEPR'
006200171117     c                   parm                    tisieDEPds
006300171117     c                   parm                    prmCDP10F
006400171117      ** ------
006500171117      ** -  se ritornato tutto OK
006600171117     c                   if        O_FLGVERS <>'0' or O_FLGCAPP <>'0'
006700171117     c                   eval      O10ERR     = 'E'
006800171117     c                   else
006900171117      **
007000171117      *   campi di output
007100171117     c                   clear                   O10CDPNAM1
007200171117     c                   clear                   O10CDPNAM2
007300171117     c                   clear                   O10CDPADR1
007400171117     c                   clear                   O10CDPADR2
007500171117     c                   clear                   O10CDPPTC
007600171117     c                   clear                   O10CDPCTY
007700171117     c                   clear                   O10CDPNAZ
007800171117     c                   clear                   O10ERR
007900150723      **
008000151020     c                   eval      O10CDPPTC  = CDPPTC
008100150723      *
008200150723      *   Sposta il record a tracciato libero su un campo di lavoro       =
008300150723     c                   clear                   dati
008400171117     C     minu:maiu     XLATE     CDPNAME1      dati
008500150723     c                   exsr      Syntax
008600151020     c                   eval      O10CDPNAM1 = dati
008700150723      **
008800150723     c                   clear                   dati
008900171117     C     minu:maiu     XLATE     CDPNAME2      dati
009000150723     c                   exsr      Syntax
009100151020     c                   eval      O10CDPNAM2 = dati
009200150723      **
009300150723     c                   clear                   dati
009400171117     C     minu:maiu     XLATE     CDPSTRET      dati
009500150723     c                   exsr      Syntax
009600151020     c                   eval      O10CDPADR1 = dati
009700150723      **
009800150723     c                   clear                   dati
009900171117     C     minu:maiu     XLATE     CDPADDR1      dati
010000150723     c                   exsr      Syntax
010100151020     c                   eval      O10CDPADR2 = dati
010200150723      **
010300150723     c                   clear                   dati
010400171117     C     minu:maiu     XLATE     CDPCITY       dati
010500150723     c                   exsr      Syntax
010600151020     c                   eval      O10CDPCTY  = dati
010700150723      *
010800151020     c                   z-add     1             X
010900151020     c                   movel     CDPISO2       CDPISO2_3         3
011000151020     c     CDPISO2_3     lookup    ISO(x)                                 21
011100151020     c   21              eval      O10CDPNAZ = NAZ(x)
011200151020      *
011300150723     c                   end
011400150723      *
011500150723     c                   RETURN
011600060628      * ?___________________________________________________                */
011700051202     c     *inzsr        begsr
011800060628      * ?___________________________________________________                */
011900051202     c     *entry        plist
012000151020     c                   parm                    tisie10ds
012100171117     c                   parm                    tisie10ds1
012200171117      **
012300171117      *  il secondo parametro può non esserci
012400151020      *
012500151020     C     KTAB          KLIST
012600151020     C                   KFLD                    TBLKUT
012700151020     C                   KFLD                    TBLCOD
012800150723     C*------------------
012900151020      *
013000151020     C*------------------
013100151020     C* Caricamento Tabella 15
013200151020     C                   z-add     1             tblKUT
013300151020     C                   move      '15'          tblCOD
013400151020     C                   Z-ADD     0             X                 3 0
013500151020     C     KTAB          setll     TABEL00F
013600151020     C     KTAB          reade     TABEL00F
013700151020     C                   DOW       not %Eof(TABEL00F)
013800151020     C     TBLFLG        IFEQ      *BLANKS
013900151020     C                   ADD       1             X
014000151020     C                   MOVEL     TBLKEY        NAZ(X)
014100151020     C                   MOVEL     TBLUNI        DS15
014200151020     C                   MOVEL     §15COD        ISO(X)
014300151020     C                   MOVEL     §15LAD        LAD(X)
014400151020     C                   END
014500151020     C     KTAB          READE     TABEL00F
014600151020     C                   END
014700151020     C*
014800151020     C*------------------
014900060628     c                   endsr
015000060627      * ?_____________________________________________________________      */
015100150723      * *------------------------*
015200150723     C     Syntax        begSR
015300150723      * *------------------------*
015400150723      *
015500150723     c                   movel     dati          TxtInOut
015600150723     c                   eval      ElencoChar = SETA
015700150723     c                   eval      TipoElenco = *blank
015800150723     c                   eval      CharSost   = *blank
015900150723     c                   eval      UpperCase  = '1'
016000150723     c                   eval      ChkNull    = *blank
016100150723     c                   eval      CharNull   = *blank
016200150723     c                   eval      Esito_check= *blank
016300150723      *
016400150723     c                   call      'XCHKCHAR'
016500150723     c                   parm                    TxtInOut
016600150723     c                   parm                    ElencoChar
016700150723     c                   parm                    TipoElenco
016800150723     c                   parm                    CharSost
016900150723     c                   parm                    UpperCase
017000150723     c                   parm                    ChkNull
017100150723     c                   parm                    CharNull
017200150723     c                   parm                    Esito_check
017300150723      *
017400150723      *   se è stato modificato qualcosa nella riga di FLAT
017500150723     c                   if        Esito_check = '1'
017600150723     c                   movel     TxtInOut      dati
017700150723     c                   end
017800150723      *
017900150723     C                   ENDSR
018000150723      *-------------------------------------------------------------------------
