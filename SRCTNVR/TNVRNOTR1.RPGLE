000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500010213      *          Pulizia TFNTC00F prima di nuova gestione note                 *
000600010213      *                                                                        *
000700010213      *                        E S E C U Z I O N E                             *
000800010213      *                                                                        *
000900010213      **************************************************************************
001000010213
001100010213     FTFNTC01L  UF   E           K DISK
001200010216     FTFNTCDLT  O    E             DISK    RENAME(TFNTC:TFNTCD)
001300010219     FTABEL00F  IF   E           K DISK
001400010219     FCNACO00F  IF   E           K DISK
001500010214     FTNTAM01L  IF   E           K DISK
001600010219     FTNOFM01L  IF   E           K DISK    RENAME(TNTAM000:TNOFM000)
001700010221     FTNCPO01L  UF   E           K DISK
001800010214     FFNAPD01L  IF   E           K DISK
001900010214     FTNVIS05L  IF   E           K DISK
002000010214     FTNVRNOTP  O    E             PRINTER OFLIND(*IN55)
002100010213
002200010213     D KPJBA         E DS
002300010213     D CNCR80        E DS
002400010213     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500010214     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002600010214     D  LIN                    1     90  0
002700010214     D                                     DIM(30)
002800010219     D DS1T          E DS
002900010216     D PARAM           DS
003000010302      * S=Simulazione
003100010302     D  VTIPOE                        1                                         S=Simulazione
003200010302      * Stampa: S=Si  N=No
003300010302     D  VSTAMP                        1                                         Stampa: S=Si  N=No
003400010213
003500010214     D L1              S              3  0 DIM(30)                              P.O. Gestiti £1
003600010213     D rnt             S             60    DIM(300)                             Salvataggio Righe
003700010213     D wapl            s                   like(ntcapl)
003800010213     D wnk1            s                   like(ntcnk1)
003900010213     D wnk2            s                   like(ntcnk2)
004000010213     D wtnt            s                   like(ntctnt)
004100010219     D kcod            S                   LIKE(tblcod)
004200010219     D kkey            S                   LIKE(tblkey)
004300010213
004400010213      *------------------------------------------------------------------------*
004500010213      *                                                                        *
004600010213      *    Significato campo "wdelet" (causale annullamento record)            *
004700010213      *                                                                        *
004800010517      *    A = Nota annullata                                                  *
004900010302      * c  C = Cliente non esistente in anagrafica (CNACO)                     *
005000010302      * c  L = Cliente non gestibile dall'AS400 (NO £1)                        *
005100010302      * c  T = Non trovata tariffa cliente (TNTAM)                             *
005200010302      * p  P = Potenziale non esistente in anagrafica (TNCPO)                  *
005300010302      * p  Q = Potenziale non gestibile dall'AS400 (NO £1)                     *
005400010302      * d  R = Padroncino non esistente in anagrafica (FNAPD)                  *
005500010302      * v  V = Visita non esistente (TNVIS)                                    *
005600010516      * v  O = Non trovata offerta cliente (TNOFM)                             *
005700010302      * x  D = Riga nota duplicata                                             *
005800010302      * x  N = Numero superiore a quello previsto in tabella (1T)              *
005900010302      * x  S = Sono in sede                                                    *
006000010213      *                                                                        *
006100010213      *------------------------------------------------------------------------*
006200010213
006300010213      *------------------------------------------------------------------------*
006400010213      *                                                                        *
006500010213      *    OPERAZIONI INIZIALI                                                 *
006600010213      *                                                                        *
006700010213      *------------------------------------------------------------------------*
006800010213
006900010213     C     *ENTRY        PLIST
007000010213     C                   PARM                    KPJBA
007100010216     C                   movel     KPJBU         PARAM
007200010213
007300010213      *    Recupero Ragione Sociale Ditta da Emettere in Stampa
007400010213
007500010213     C                   Z-ADD     1             CODUT
007600010213     C                   CALL      'X§PARUT'
007700010213     C                   PARM                    UT§DSE
007800010213     C                   MOVEL     RAGUT         RSUT             20
007900010213     C                   MOVEL     REC80         CNCR80
008000010214
008100010214      *    Carico Filiali Gestite
008200010214
008300010214     C                   CLEAR                   DSUL06
008400010214     C                   MOVE      '£1'          D06COD
008500010214     C                   MOVEL     SIMFEL        D06KEY
008600010214     C                   MOVEL     DSUL06        KPJBU
008700010214     C                   CALL      'TRUL06R'
008800010214     C                   PARM                    KPJBA
008900010214     C                   MOVEL     KPJBU         DSUL06
009000010214     C                   MOVEA     LIN           L1
009100010214
009200010214      *    Accesso a CNACO
009300010214
009400010214     C     kcnaco        KLIST
009500010214     C                   kfld                    codut
009600010214     C                   kfld                    kkcc              4 0
009700010214     C                   kfld                    kksc              7 0
009800010214
009900010219      *    Accesso a TNTAM / TNOFM
010000010214
010100010214     C     ktntam        KLIST
010200010214     C                   kfld                    kksc
010300010214     C                   kfld                    kctr              3 0
010400010219
010500010219      *    Accesso a TABEL
010600010219
010700010219     C     Ktabel        klist
010800010219     C                   kfld                    codut
010900010219     C                   kfld                    kcod
011000010219     C                   kfld                    kkey
011100010219     C                   movel     '1T'          kcod
011200010213
011300010216     C     vstamp        ifeq      'S'
011400010216     C     vtipoe        ifeq      'S'
011500010214     C                   eval      psimu = '(Simulazione)'
011600010214     C                   endif
011700010214     C                   write     testa
011800010214     C                   z-add     0             num1              9 0
011900010214     C                   z-add     0             num2              9 0
012000010216     C                   endif
012100010214
012200010213      *------------------------------------------------------------------------*
012300010213      *                                                                        *
012400010213      *    ROUTINE PRINCIPALE                                                  *
012500010213      *                                                                        *
012600010213      *------------------------------------------------------------------------*
012700010213
012800010213      *    Lettura File Note
012900010213      *    =================
013000010213
013100010213     C     *loval        setll     TFNTC01L
013200010213     C                   do        *hival
013300010213     C                   read      TFNTC01L
013400010213     C                   if        %eof(tfntc01l)
013500010213     C                   leave
013600010213     C                   endif
013700010213
013800010213      *    Sflaggo la causale di annullamento record
013900010213
014000010213     C                   clear                   wdelet            1
014100010213
014200010213      *    A cambio tipo nota faccio reset della schiera delle righe
014300010219      *    e reperisco numeratore righe da tabella 1T
014400010213
014500010213     C     ntcapl        ifne      wapl
014600010213     C     ntcnk1        orne      wnk1
014700010213     C     ntcnk2        orne      wnk2
014800010213     C     ntctnt        orne      wtnt
014900010213     C                   movea     *blanks       rnt
015000010213     C                   z-add     0             x                 3 0
015100010219     C                   movel(p)  ntctnt        kkey
015200010219     C     ktabel        chain     tabel00f
015300010219     C                   if        %found(tabel00f)
015400010219     C                   movel     tbluni        ds1t
015500010219     C                   else
015600010219     C                   z-add     998           §1trig
015700010219     C                   endif
015800010213     C                   endif
015900010213
016000010213      *    Salvo valori per determinare cambio tipo nota
016100010213
016200010213     C                   eval      wapl = ntcapl
016300010213     C                   eval      wnk1 = ntcnk1
016400010213     C                   eval      wnk2 = ntcnk2
016500010213     C                   eval      wtnt = ntctnt
016600010214
016700010214      *    Controlli
016800010214      *    =========
016900010213
017000010219      *    Controllo se sono in sede
017100010213
017200010214     C     wdelet        ifeq      ' '
017300010219     C     simfel        andeq     0
017400010219     C                   eval      wdelet = 'S'                                 Sono in sede
017500010219     C                   endif
017600010517      * note annullate le deleto
017700010517     C                   if        ntcflt = 'A'
017800010517     C                   eval      wdelet = 'A'                                 pulisco le note annu
017900010517     C                   endif
018000010219
018100010219      *    Controllo su anagrafica Clienti, L1, Tariffe
018200010219
018300010219     C     wdelet        ifeq      ' '
018400010213     C     ntcapl        andeq     'C'
018500010214     C                   movel     ntcnk1        kkcc
018600010214     C                   move      ntcnk1        kksc
018700010214     C     kcnaco        chain     cnaco00f
018800010214     C                   if        not %found(cnaco00f)
018900010214     C                   eval      wdelet = 'C'                                 Non esiste CNACO
019000010214     C                   else
019100010214     C                   movel     kksc          w03num            3 0
019200010214     C     w03num        lookup    L1                                     88
019300010214     C  N88              eval      wdelet = 'L'                                 Non gestibile L1
019400010214     C     *in88         ifeq      *on
019500010214     C     ntcnk2        andne     *blanks
019600010214     C                   movel     ntcnk2        kctr
019700010214     C     ktntam        chain     tntam01l
019800010214     C                   if        not %found(tntam01l)
019900010214     C                   eval      wdelet = 'T'                                 Non esiste TNTAM
020000010219     C                   endif
020100010214     C                   endif
020200010213     C                   endif
020300010214     C                   endif
020400010214
020500010214      *    Controllo clienti Potenziali, L1
020600010214
020700010214     C     wdelet        ifeq      ' '
020800010214     C     ntcapl        andeq     'P'
020900010214     C                   move      ntcnk1        wnum11           11 0
021000010221     C     vtipoe        ifne      'S'
021100010214     C     wnum11        chain     tncpo01l
021200010221     C                   else
021300010221     C     wnum11        chain(n)  tncpo01l
021400010221     C                   endif
021500010214     C                   if        not %found(tncpo01l)
021600010214     C                   eval      wdelet = 'P'                                 Non esiste TNCPO
021700010214     C                   else
021800010214     C     cpoflt        lookup    L1                                     88
021900010214     C  N88              eval      wdelet = 'Q'                                 Non gestibile L1
022000010214     C                   endif
022100010214     C                   endif
022200010214
022300010214      *    Controllo Padroncini
022400010214
022500010214     C     wdelet        ifeq      ' '
022600010214     C     ntcapl        andeq     'D'
022700010214     C                   move      ntcnk1        wnum07            7 0
022800010214     C     wnum07        chain     fnapd01l
022900010214     C                   if        not %found(fnapd01l)
023000010214     C                   eval      wdelet = 'R'                                 Non esiste FNAPD
023100010214     C                   endif
023200010214     C                   endif
023300010214
023400010214      *    Controllo Visita
023500010214
023600010214     C     wdelet        ifeq      ' '
023700010214     C     ntcapl        andeq     'V'
023800010214     C                   move      ntcnk1        wnum07            7 0
023900010214     C     wnum07        chain     tnvis05l
024000010214     C                   if        not %found(tnvis05l)
024100010214     C                   eval      wdelet = 'V'                                 Non esiste TNVIS
024200010516     C                   else
024300010516     C     ntcnk2        ifne      *blanks
024400010516     C                   move      ntcnk1        kksc
024500010516     C                   movel     ntcnk2        kctr
024600010516     C     ktntam        chain     tnofm01l
024700010516     C                   if        not %found(tnofm01l)
024800010516     C                   eval      wdelet = 'O'                                 Non esiste TNOFM
024900010516     C                   endif
025000010516     C                   endif
025100010214     C                   endif
025200010214     C                   endif
025300010213
025400010213      *    Controllo riga duplicata
025500010213
025600010214     C     wdelet        ifeq      ' '
025700010213     C     ntcrnt        lookup    rnt                                    90
025800010213     C     *in90         ifeq      *on
025900010213     C                   eval      wdelet = 'D'                                 Riga Duplicata
026000010213     C                   else
026100010213     C                   if        x < 300
026200010213     C                   add       1             x
026300010213     C                   move      ntcrnt        rnt(x)
026400010213     C                   endif
026500010213     C                   endif
026600010213     C                   endif
026700010219
026800010219      *    Controllo numero superiore a tabella 1T
026900010219
027000010219     C     wdelet        ifeq      ' '
027100010219     C     X             andgt     §1trig
027200010219     C                   eval      wdelet = 'N'                                 Nr. superiore a tab.
027300010219     C                   endif
027400010214
027500010214      *    Stampa note da Pulire
027600010214      *    =====================
027700010214
027800010216     C     vstamp        ifeq      'S'
027900010216     C                   add       1             num1
028000010214     C     wdelet        ifne      ' '
028100010517     C     wdelet        andne     'A'
028200010214
028300010214     C                   add       1             num2
028400010214
028500010219     C     wdelet        ifeq      'S'
028600010219     C                   eval      errore = 'Elaborazione effettuata i'
028700010219     C                             + 'n Sede       '
028800010214     C                   endif
028900010219
029000010219     C     wdelet        ifeq      'C'
029100010219     C                   eval      errore = 'Cliente non esistente in '
029200010219     C                             + 'Anagrafica   '
029300010219     C                   endif
029400010214
029500010214     C     wdelet        ifeq      'L'
029600010214     C                   eval      errore = 'Cliente non gestibile su '
029700010214     C                             + 'questo AS/400'
029800010214     C                   endif
029900010214
030000010214     C     wdelet        ifeq      'T'
030100010214     C                   eval      errore = 'Non esiste la tariffa di '
030200010214     C                             + 'riferimento  '
030300010214     C                   endif
030400010214
030500010214     C     wdelet        ifeq      'P'
030600010214     C                   eval      errore = 'Potenziale non esistente '
030700010214     C                             + 'in Anagrafica'
030800010214     C                   endif
030900010214
031000010214     C     wdelet        ifeq      'Q'
031100010214     C                   eval      errore = 'Potenz. non gestibile su '
031200010214     C                             + 'questo AS/400'
031300010214     C                   endif
031400010214
031500010214     C     wdelet        ifeq      'R'
031600010214     C                   eval      errore = 'Padroncino non esistente '
031700010214     C                             + 'in Anagrafica'
031800010214     C                   endif
031900010214
032000010214     C     wdelet        ifeq      'V'
032100010214     C                   eval      errore = 'Visita non esistente     '
032200010214     C                             + '             '
032300010214     C                   endif
032400010516
032500010516     C     wdelet        ifeq      'O'
032600010516     C                   eval      errore = 'Non esiste Offerta    di '
032700010516     C                             + 'riferimento  '
032800010516     C                   endif
032900010214
033000010214     C     wdelet        ifeq      'D'
033100010214     C                   eval      errore = 'Riga nota duplicata      '
033200010214     C                             + '             '
033300010214     C                   endif
033400010219
033500010219     C     wdelet        ifeq      'N'
033600010219     C                   move      §1trig        walfa3            3
033700010219     C                   eval      errore = 'Numero riga superiore a n'
033800010219     C                             + 'umero max ' + walfa3
033900010219     C                   endif
034000010214
034100010214     C   55              write     testa
034200010214     C   55              eval      *in55 = *off
034300010214     C                   write     dett
034400010214
034500010214     C                   endif
034600010216     C                   endif
034700010214
034800010214      *    Aggiornamento Note
034900010214      *    ==================
035000010214
035100010216     C     vtipoe        ifne      'S'
035200010606      *    ....... se è da tenere e il potenziale non è annullato
035300010606      *    ....... aggiorno e sflaggo potenziale
035400010221      *    ....... per la trasmissione per note 'P'
035500010214     C     wdelet        ifeq      ' '
035600010606      *
035700010606     c                   if        cpoatb <> 'A'
035800010606      *
035900010219     C     ntcapl        ifeq      'P'
036000010219     C                   exsr      AGGPOTE
036100010219      *                  ====      =======
036200010219     C                   endif
036300010214     C                   eval      ntcflt = ' '
036400010214     C                   clear                   ntcntr
036500010216     C                   update    tfntc                                        Da Trasmettere
036600010216      *                  ----------------
036700010606     c                   else
036800010606     C                   exsr      UNLCKPOTE
036900010606     c                   endif
037000010606      *
037100010219      *    ....... se è da cancellare: cancello e scrivo dlt
037200010221      *    ....... disalloco potenziale per note 'P'
037300010214     C                   else
037400010517      * solo se non è una nota annullata
037500010517     c     wdelet        ifne      'A'
037600010221     C     ntcapl        ifeq      'P'
037700010221     C                   exsr      UNLCKPOTE
037800010221      *                  ====      =========
037900010221     C                   endif
038000010216     C                   write     tfntcd                                       Scrivo DLT
038100010517     c                   endif
038200010216      *                  ----------------
038300010216     C                   delete    tfntc                                        Cancello
038400010216      *                  ---------------
038500010214     C                   endif
038600010214     C                   endif
038700010213
038800010213     C                   enddo
038900010213
039000010213      *    Fine Programma
039100010606      *    ==============
039200010213
039300010216     C     vstamp        ifeq      'S'
039400010214     C                   write     fine
039500010216     C                   endif
039600010214
039700010213     C                   eval      *inlr = *on
039800010219
039900010219      *------------------------------------------------------------------------*
040000010219      *                                                                        *
040100010219      *    SUBROUTINES                                                         *
040200010219      *                                                                        *
040300010219      *------------------------------------------------------------------------*
040400010219
040500010219      *------------------------------------------------------------------------*
040600010221      *    AGGPOTE - Sflaggo potenziale per la trasmissione                    *
040700010219      *------------------------------------------------------------------------*
040800010219
040900010219     C     AGGPOTE       begsr
041000010221     C                   clear                   cpoftr
041100010221     C                   update    tncpo000
041200010219     C                   endsr
041300010221
041400010221      *------------------------------------------------------------------------*
041500010221      *    UNLCKPOTE - Disalloco potenziale                                    *
041600010221      *------------------------------------------------------------------------*
041700010221
041800010221     C     UNLCKPOTE     begsr
041900010221     C                   unlock    tncpo01l
042000010221     C                   endsr
