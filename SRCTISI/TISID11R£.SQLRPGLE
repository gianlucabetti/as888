000600990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000601160120     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
000800160121     fwtisie11f o  a e             disk
000801160121     fdpcdp01l  if   e           k disk
000900151020     ftabel00f  if   e           k disk
001100151020      * Tabella codici nazioni
001200151020     D NAZ             S              3    DIM(500)
001300151020     D ISO             S              3    DIM(500)
001400151020     D LAD             S              3s 0 DIM(500)
001401160120     D RED             S              1    DIM(500)
001500160120     D kpjba         e ds
001501160120     D ds15          e ds
001502160120     D tisie10ds     e ds
001503160120     D* DS PER TISIE3R - CAPPARIO DPD  - NUOVO
001504160120     D TISIe3ds      E DS
001505160120     D* DS PER TISI95R - CONTROLLO DI CAP
001506160120     D DSSI95        E DS                  EXTNAME(TISI95DS)
002100150723     D TxtInOut        S           2048
002200150723     D ElencoChar      S            256
002300150723     D TipoElenco      S              1
002400150723     D CharSost        S              1
002500150723     D UpperCase       S              1
002600150723     D ChkNull         S              1
002700150723     D CharNull        S              1
002800150723     D Esito_check     S              1
002801160121     D o10errd         S              7
002802160121     D output          S              1
002900150723     D  Dati           s           1500
003200150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
003300150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
003600150723     D   Digits        C                   '0123456789'
003900150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004000150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004001160120     D*
004002160120     D WLBDAT          DS
004003160120     D  G02DAT                 1      8  0
004004160120     D  G02INV                 9     16  0
004005160120     D  G02ERR                17     17
004006160120     D  G02TGI                18     22  0
004007160120     D oggi            s                   like(G02INV) INZ(0)
004100060628      * ?__________________________________________________                 */
004200150723     c                   seton                                        lr
004700150723      *  Reperisce l'ultima versione del Cappario
004800150723     c     *hival        setll     dpcdp01l
004900150723     c                   readp     dpcdp01l
005000150723      *
005100150723     c                   if        not %EoF(dpcdp01l)
005200150723     c                   z-add     CDPVER        Versione          5 0
005300160120     c     VERSIONE      setll     dpcdp01l
005301160120     c                   do        *hival
005302160120     c     VERSIONE      reade     dpcdp01l
005400160120     c                   if        %eof(dpcdp01l)
005500160120     c                   leave
005600160120     c                   end
005605160120     c*
005606160120     c                   clear                   O10CDPPTC
005607160120     c                   clear                   O10CDPCTY
005608160120     c                   clear                   O10CDPNAZ
005609160120     c                   clear                   O10CDPNAM1
005610160120      *
005611160120     c                   z-add     1             X
005612160120     c                   movel     CDPISO2       CDPISO2_3         3
005613160120     c                   setoff                                       0421
005615160120     c                   if        cdpiso2 <> ' '
005616160120     c     CDPISO2_3     lookup    ISO(x)                                 21
005617160120     c   21              eval      O10CDPNAZ = NAZ(x)
005618160120     c  n21              eval      O10CDPNAZ = CDPISO2_3
005619160120     c                   if        *in21 and (lad(x) = 0 and red(x) = ' ')
005620160120     c                   seton                                        04
005621160120     c                   end
005622160120     c                   end
005623160120      *
005800151020     c                   eval      O10CDPPTC  = CDPPTC
005900150723      *
008100150723     c                   clear                   dati
008200150723     C     minu:maiu     XLATE     CDPCTY        dati
008300150723     c                   exsr      Syntax
008400151020     c                   eval      O10CDPCTY  = dati
008401160120      **
008402160120     c                   clear                   dati
008403160120     C     minu:maiu     XLATE     CDPNAM1       dati
008404160120     c                   exsr      Syntax
008405160120     c                   eval      O10CDPNAM1 = dati
009001160120     c* cappario italia
009002160120     C                   CLEAR                   DSSI95
009003160120     C                   MOVEL     O10CDPCTY     I95LOC
009004160120     C                   MOVEL     O10CDPPTC     I95CAP
009005160120     C                   MOVEL     O10CDPNAZ     I95NAR
009006160120     C                   MOVEL     '3'           I95TCN
009007160120     C                   MOVEL     oggi          I95DAT
009011160120     C                   MOVEL     'F'           I95TPO
009017160120     C                   CALL      'TISI95R'
009018160120     C                   PARM                    DSSI95
009026160120     c* cappario dpd
009027160120     c                   clear                   tisie3ds
009028160120     c                   eval      isie3tla='E'
009029160120     c                   z-add     oggi          isie3dri
009030160120     C                   z-add     oggi          Isie3dsp
009031160120     c                   time                    isie3hsp
009032160120     C                   MOVEL     O10CDPNAZ     isie3nzd
009033160120     C                   MOVEL     O10CDPPTC     Isie3cad
009034160120     c                   movel     O10CDPCTY     isie3cty
009035160120     c                   z-add     001           isie3lnp
009036160120     c                   z-add     300           isie3srv
009039160120     c                   movel     'N'           isie3exc
009040160120     C                   CALL      'TISIE3R'
009041160120     C                   PARM                    tisie3ds
009042160120     c*SE ERRORE
009043160120     c                   select
009044160120     C                   when      o95err <> ' ' and osie3err <> ' '
009046160120     c                   seton                                        02
009047160120     C                   when      o95err <> ' '
009049160120     c                   seton                                        01
009050160120     C                   when      osie3err <> ' '
009051160120     c                   seton                                        03
009052160120     c                   other
009058160120     c                   setoff                                       010203
009059160120     c                   endsl
009060160120     c                   if        *in01 or *in02 or *in03
009061160120     c                   exsr      errore
009062160120     c                   end
009100151020      *
009200160120     c                   enddo
009300150723     c                   end
009400150723      *
009500150723     c                   RETURN
009600060628      * ?___________________________________________________                */
009700051202     c     *inzsr        begsr
009800060628      * ?___________________________________________________                */
009900051202     c     *entry        plist
010000160120     c                   parm                    kpjba
010001160121     c                   movel     kpjbu         output
010002160121     c                   if        output = ' '
010003160121     C/EXEC SQL
010004160121     C+ delete from  wtisie11f
010005160121     C/END-EXEC
010006160121     c                   end
010100150723      *   campi di output
011400151020      *
011500151020     C     KTAB          KLIST
011600151020     C                   KFLD                    TBLKUT
011700151020     C                   KFLD                    TBLCOD
011800150723     C*------------------
011900151020      *
011901160120      *  imposta UDATE di elaborazione
011902160120     C                   move      *date         G02DAT
011903160120     C                   MOVE      *ZEROS        G02INV
011904160120     C                   MOVE      *BLANKS       G02ERR
011905160120     C                   CALL      'XSRDA8'
011906160120     C                   PARM                    WLBDAT
011908160120     C                   Z-ADD     G02INV        oggi
011909160120     c                   seton                                        of
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
013301160120     C                   MOVEL     §15red        red(X)
013400151020     C                   END
013500151020     C     KTAB          READE     TABEL00F
013600151020     C                   END
013700151020     C*
013900060628     c                   endsr
014100150723      * *------------------------*
014200160120     C     errore        begSR
014300150723      * *------------------------*
014301160121     c                   select
014302160121     c                   when      *in01
014303160121     c                   eval      o10errd = 'ITALIA'
014304160121     c                   when      *in02
014305160121     c                   eval      o10errd = 'ITA/DPD'
014306160121     c                   when      *in03
014307160121     c                   eval      o10errd = 'DPD'
014308160121     c                   endsl
014309160121     c*
014311160121     c                   if        output = 'S'
014312160120     c                   if        *inof
014313160120     c                   except    testa
014314160120     c                   setoff                                       of
014315160120     c                   end
014316160120     c                   EXCEPT    dettagli
014317160121     c                   else
014318160121     c                   clear                   wtisie011
014319160121     c                   eval      w11ver = cdpver
014320160121     c                   eval      w11depot = cdpdpc
014321160121     c                   eval      w11err = o10errd
014322160121     c                   eval      w11nam1= o10cdpnam1
014323160121     c  n21              eval      w11nam1= 'Codice ISO non trovato'
014324160121     c                   eval      w11naz = o10cdpnaz
014325160121     c                   eval      w11cty = o10cdpcty
014326160121     c                   eval      w11ptc = o10cdpptc
014327160121     c   04              eval      w11serv= 'N'
014328160121     c                   write     wtisie011
014331160121     c                   end
014332160120     c                   endsr
014333160120      * *------------------------*
014334160120     C     Syntax        begSR
014335160120      * *------------------------*
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
017000160120     OQSYSPRT   E            TESTa          2 02
017200160120     O                                         + 25 'LISTA DEPOT CON INDIRIZZO '
017300160120     O                                         +  0 'ERRATO VERSIONE'
017301160120     O                       cdpver        z   +  1
017400160120     O                                          113 'TISIE11R'
017500160120     O                       UDATE              127 '  /  /  '
017600160120     O                       PAGE1         Z    132
017601160120     O          E            testa       1  2
017602160120     o                                         +  0 'CAP ERRATO'
017603160120     o                                         +  1 'Depot'
017604160120     o                                         +  1 'Località'
017605160120     o                                         + 53 'CAP'
017606160120     o                                         +  7 'N/I'
017607160120     o                                         +  1 'Nome/Errore'
017608160120     o                                          131 'Ser'
018300160120     O          E            DETtagli    1
018400160121     O                       o10errd              7
018500160120     O                       CDPDPC              15
018800160120     O                       O10CDPCTY         +  2
018900160120     O                       O10CDPPTC         +  1
019000160120     O                       O10CDPNAZ         +  1
019100160120     O               21      O10CDPNAM1         127
019200160120     O              n21                         114 'Codice ISO non trovato'
019300160120     O               04                         130 'No'
