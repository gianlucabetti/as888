000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200171117      *---
000300160120     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
000400171117      *---
000500171117     fwftisid11fo  a e             disk
000600171117      *---
000700171117     fdpcdp11i  if   e           k disk
000800151020     ftabel00f  if   e           k disk
000900171117      *---
001000151020      * Tabella codici nazioni
001100151020     D NAZ             S              3    DIM(500)
001200151020     D ISO             S              3    DIM(500)
001300151020     D LAD             S              3s 0 DIM(500)
001400160120     D RED             S              1    DIM(500)
001500160120     D kpjba         e ds
001600160120     D ds15          e ds
001700171117      *---
001800160120     D tisie10ds     e ds
001900171117      *---
002000171117     D* DS PER TISID4R - CAPPARIO DPD  - 2017
002100171117     D TISId4ds      E DS
002200171117      *---
002300160120     D* DS PER TISI95R - CONTROLLO DI CAP
002400160120     D DSSI95        E DS                  EXTNAME(TISI95DS)
002500150723     D TxtInOut        S           2048
002600150723     D ElencoChar      S            256
002700150723     D TipoElenco      S              1
002800150723     D CharSost        S              1
002900150723     D UpperCase       S              1
003000150723     D ChkNull         S              1
003100150723     D CharNull        S              1
003200150723     D Esito_check     S              1
003300160121     D o10errd         S              7
003400160121     D output          S              1
003500150723     D  Dati           s           1500
003600150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
003700150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
003800150723     D   Digits        C                   '0123456789'
003900150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004000150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004100160120     D*
004200160120     D WLBDAT          DS
004300160120     D  G02DAT                 1      8  0
004400160120     D  G02INV                 9     16  0
004500160120     D  G02ERR                17     17
004600160120     D  G02TGI                18     22  0
004700160120     D oggi            s                   like(G02INV) INZ(0)
004800060628      * ?__________________________________________________                 */
004900150723     c                   seton                                        lr
005000171117      ***
005100150723      *  Reperisce l'ultima versione del Cappario
005200171117     c     *hival        setll     dpcdp11i
005300171117     c                   readp     dpcdp11i
005400150723      *
005500171117     c                   if        not %EoF(dpcdp11i)
005600150723     c                   z-add     CDPVER        Versione          5 0
005700171117     c     KVERS         setll     dpcdp11i
005800160120     c                   do        *hival
005900171117     c     KVERS         reade     dpcdp11i
006000171117     c                   if        %eof(dpcdp11i)
006100160120     c                   leave
006200160120     c                   end
006300160120     c*
006400160120     c                   clear                   O10CDPPTC
006500160120     c                   clear                   O10CDPCTY
006600160120     c                   clear                   O10CDPNAZ
006700160120     c                   clear                   O10CDPNAM1
006800160120      *
006900160120     c                   z-add     1             X
007000160120     c                   movel     CDPISO2       CDPISO2_3         3
007100160120     c                   setoff                                       0421
007200160120     c                   if        cdpiso2 <> ' '
007300160120     c     CDPISO2_3     lookup    ISO(x)                                 21
007400160120     c   21              eval      O10CDPNAZ = NAZ(x)
007500160120     c  n21              eval      O10CDPNAZ = CDPISO2_3
007600160120     c                   if        *in21 and (lad(x) = 0 and red(x) = ' ')
007700160120     c                   seton                                        04
007800160120     c                   end
007900160120     c                   end
008000160120      *
008100151020     c                   eval      O10CDPPTC  = CDPPTC
008200150723      *
008300150723     c                   clear                   dati
008400171117     C     minu:maiu     XLATE     CDPCITY       dati
008500150723     c                   exsr      Syntax
008600151020     c                   eval      O10CDPCTY  = dati
008700160120      **
008800160120     c                   clear                   dati
008900171117     C     minu:maiu     XLATE     CDPNAME1      dati
009000160120     c                   exsr      Syntax
009100160120     c                   eval      O10CDPNAM1 = dati
009200160120     c* cappario italia
009300160120     C                   CLEAR                   DSSI95
009400160120     C                   MOVEL     O10CDPCTY     I95LOC
009500160120     C                   MOVEL     O10CDPPTC     I95CAP
009600160120     C                   MOVEL     O10CDPNAZ     I95NAR
009700160120     C                   MOVEL     '3'           I95TCN
009800160120     C                   MOVEL     oggi          I95DAT
009900160120     C                   MOVEL     'F'           I95TPO
010000160120     C                   CALL      'TISI95R'
010100160120     C                   PARM                    DSSI95
010200171117     c* cappario dpd 2017
010300171117     c                   clear                   tisid4ds
010400171117     c                   z-add     oggi          isid4dri
010500171117     C                   MOVEL     O10CDPNAZ     isid4nzd
010600171117     C                   MOVEL     O10CDPPTC     Isid4cad
010700171117     c                   movel     O10CDPCTY     isid4lod
010800171117     c                   z-add     001           isid4lnp
010900171117     c                   z-add     300           isid4sorc
011000171117     C                   CALL      'TISID4R'
011100171117     C                   PARM                    tisid4ds
011200160120     c*SE ERRORE
011300160120     c                   select
011400171117     C                   when      o95err <> ' ' and osid4err <> ' '
011500160120     c                   seton                                        02
011600160120     C                   when      o95err <> ' '
011700160120     c                   seton                                        01
011800171117     C                   when      osid4err <> ' '
011900160120     c                   seton                                        03
012000160120     c                   other
012100160120     c                   setoff                                       010203
012200160120     c                   endsl
012300160120     c                   if        *in01 or *in02 or *in03
012400160120     c                   exsr      errore
012500160120     c                   end
012600151020      *
012700160120     c                   enddo
012800150723     c                   end
012900150723      *
013000150723     c                   RETURN
013100060628      * ?___________________________________________________                */
013200051202     c     *inzsr        begsr
013300060628      * ?___________________________________________________                */
013400051202     c     *entry        plist
013500160120     c                   parm                    kpjba
013600160121     c                   movel     kpjbu         output
013700171117      *----
013800160121     c                   if        output = ' '
013900160121     C/EXEC SQL
014000171117     C+ delete from  wftisid11f
014100160121     C/END-EXEC
014200160121     c                   end
014300150723      *   campi di output
014400151020      *
014500151020     C     KTAB          KLIST
014600151020     C                   KFLD                    TBLKUT
014700151020     C                   KFLD                    TBLCOD
014800171117      *
014900171117     C     KVERS         KLIST
015000171117     C                   KFLD                    flgatb
015100171117     C                   KFLD                    VERSIONE
015200171117     C                   move      ' '           flgatb            1
015300150723     C*------------------
015400160120      *  imposta UDATE di elaborazione
015500160120     C                   move      *date         G02DAT
015600160120     C                   MOVE      *ZEROS        G02INV
015700160120     C                   MOVE      *BLANKS       G02ERR
015800160120     C                   CALL      'XSRDA8'
015900160120     C                   PARM                    WLBDAT
016000160120     C                   Z-ADD     G02INV        oggi
016100160120     c                   seton                                        of
016200171117      *--
016300151020     C* Caricamento Tabella 15
016400151020     C                   z-add     1             tblKUT
016500151020     C                   move      '15'          tblCOD
016600151020     C                   Z-ADD     0             X                 3 0
016700151020     C     KTAB          setll     TABEL00F
016800151020     C     KTAB          reade     TABEL00F
016900151020     C                   DOW       not %Eof(TABEL00F)
017000151020     C     TBLFLG        IFEQ      *BLANKS
017100151020     C                   ADD       1             X
017200151020     C                   MOVEL     TBLKEY        NAZ(X)
017300151020     C                   MOVEL     TBLUNI        DS15
017400151020     C                   MOVEL     §15COD        ISO(X)
017500151020     C                   MOVEL     §15LAD        LAD(X)
017600160120     C                   MOVEL     §15red        red(X)
017700151020     C                   END
017800151020     C     KTAB          READE     TABEL00F
017900151020     C                   END
018000151020     C*
018100060628     c                   endsr
018200150723      * *------------------------*
018300160120     C     errore        begSR
018400150723      * *------------------------*
018500160121     c                   select
018600160121     c                   when      *in01
018700160121     c                   eval      o10errd = 'ITALIA'
018800160121     c                   when      *in02
018900160121     c                   eval      o10errd = 'ITA/DPD'
019000160121     c                   when      *in03
019100160121     c                   eval      o10errd = 'DPD'
019200160121     c                   endsl
019300160121     c*
019400160121     c                   if        output = 'S'
019500171117     c*
019600160120     c                   if        *inof
019700160120     c                   except    testa
019800160120     c                   setoff                                       of
019900160120     c                   end
020000171117     c*
020100160120     c                   EXCEPT    dettagli
020200171117     c*
020300160121     c                   else
020400171117     c                   clear                   wtisid011
020500160121     c                   eval      w11ver = cdpver
020600171117     c                   eval      w11depot = cdpdptn
020700160121     c                   eval      w11err = o10errd
020800171117     c                   eval      w11nam1= o10cdpnam1
020900160121     c  n21              eval      w11nam1= 'Codice ISO non trovato'
021000160121     c                   eval      w11naz = o10cdpnaz
021100160121     c                   eval      w11cty = o10cdpcty
021200160121     c                   eval      w11ptc = o10cdpptc
021300160121     c   04              eval      w11serv= 'N'
021400171117     c                   write     wtisid011
021500160121     c                   end
021600171117     c*
021700160120     c                   endsr
021800160120      * *------------------------*
021900160120     C     Syntax        begSR
022000160120      * *------------------------*
022100150723      *
022200150723     c                   movel     dati          TxtInOut
022300150723     c                   eval      ElencoChar = SETA
022400150723     c                   eval      TipoElenco = *blank
022500150723     c                   eval      CharSost   = *blank
022600150723     c                   eval      UpperCase  = '1'
022700150723     c                   eval      ChkNull    = *blank
022800150723     c                   eval      CharNull   = *blank
022900150723     c                   eval      Esito_check= *blank
023000150723      *
023100150723     c                   call      'XCHKCHAR'
023200150723     c                   parm                    TxtInOut
023300150723     c                   parm                    ElencoChar
023400150723     c                   parm                    TipoElenco
023500150723     c                   parm                    CharSost
023600150723     c                   parm                    UpperCase
023700150723     c                   parm                    ChkNull
023800150723     c                   parm                    CharNull
023900150723     c                   parm                    Esito_check
024000150723      *
024100150723      *   se è stato modificato qualcosa nella riga di FLAT
024200150723     c                   if        Esito_check = '1'
024300150723     c                   movel     TxtInOut      dati
024400150723     c                   end
024500150723      *
024600150723     C                   ENDSR
024700171117      *------------------------------------------------------------------------
024800160120     OQSYSPRT   E            TESTa          2 02
024900160120     O                                         + 25 'LISTA DEPOT CON INDIRIZZO '
025000160120     O                                         +  0 'ERRATO VERSIONE'
025100160120     O                       cdpver        z   +  1
025200171117     O                                          113 'TISID11R'
025300160120     O                       UDATE              127 '  /  /  '
025400160120     O                       PAGE1         Z    132
025500160120     O          E            testa       1  2
025600160120     o                                         +  0 'CAP ERRATO'
025700171117     o                                         +  4 'Depot'
025800160120     o                                         +  1 'Località'
025900160120     o                                         + 53 'CAP'
026000160120     o                                         +  7 'N/I'
026100160120     o                                         +  1 'Nome/Errore'
026200160120     o                                          131 'Ser'
026300160120     O          E            DETtagli    1
026400160121     O                       o10errd              7
026500171117     O                       CDPDPTN             18
026600160120     O                       O10CDPCTY         +  2
026700160120     O                       O10CDPPTC         +  1
026800160120     O                       O10CDPNAZ         +  1
026900160120     O               21      O10CDPNAM1         127
027000160120     O              n21                         114 'Codice ISO non trovato'
027100160120     O               04                         130 'No'
