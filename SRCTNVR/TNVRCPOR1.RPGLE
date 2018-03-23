000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500010719      *          Pulizia clienti potenziali per disallineamento                *
000600010719      *                        tra filiale e sede                              *
000700010213      *                                                                        *
000800010213      *                        E S E C U Z I O N E                             *
000900010213      *                                                                        *
001000010213      **************************************************************************
001100010213
001200010718     fazorg01l  if   e           k disk
001300010718     Ftncpo01l  uF   E           K DISK
001400010718     Ftncps11l  uF   E           K DISK
001500010718     Ftncpi01l  uF   E           K DISK
001600010719     Ftncpodlt  O    E             DISK    RENAME(tncpo000:tncpod)
001700010719     Ftncpsdlt  O    E             DISK    RENAME(tncps000:tncpsd)
001800010719     Ftncpidlt  O    E             DISK    RENAME(tncpi000:tncpid)
001900010717     FtnvrcpoP  O    E             PRINTER OFLIND(*IN55)
002000010213
002100010718     d sav_cpscpo      s                   like(cpscpo)
002200010718     d sav_cpink1      s                   like(cpink1)
002300010718     d skpoel          s              3  0 dim(200)                             po da elab.
002400010718     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
002500010718     d wokpo           s              1    inz('0')
002600010718
002700010213     D KPJBA         E DS
002800010213     D CNCR80        E DS
002900010718     D UT§DSE0F      E DS
003000010216     D PARAM           DS
003100010302      * S=Simulazione
003200010302     D  VTIPOE                        1                                         S=Simulazione
003300010302      * Stampa: S=Si  N=No
003400010302     D  VSTAMP                        1                                         Stampa: S=Si  N=No
003500010213
003600010213     D rnt             S             60    DIM(300)                             Salvataggio Righe
003700010213
003800010213      *------------------------------------------------------------------------*
003900010213      *                                                                        *
004000010213      *    Significato campo "wdelet" (causale annullamento record)            *
004100010213      *                                                                        *
004200011212      *    A = Potenziale annullato                                            *
004300010718      *    P = Potenziale non in carico ad un p.o. dell'AS                     *
004400011212      *    Z = Potenziale in carico ad un p.o. uguale a zero                   *
004500010213      *                                                                        *
004600010213      *------------------------------------------------------------------------*
004700010213
004800010213      *------------------------------------------------------------------------*
004900010213      *                                                                        *
005000010213      *    OPERAZIONI INIZIALI                                                 *
005100010213      *                                                                        *
005200010213      *------------------------------------------------------------------------*
005300010213
005400010213     C     *ENTRY        PLIST
005500010213     C                   PARM                    KPJBA
005600010216     C                   movel     KPJBU         PARAM
005700010213
005800010213      *    Recupero Ragione Sociale Ditta da Emettere in Stampa
005900010213
006000010213     C                   Z-ADD     1             CODUT
006100010213     C                   CALL      'X§PARUT'
006200010718     C                   PARM                    UT§DSE0F
006300010213     C                   MOVEL     RAGUT         RSUT             20
006400010213     C                   MOVEL     REC80         CNCR80
006500010718
006600010718      * carico i p.o. da elaborare dell'AS in un sk
006700010718     c                   eval      xa = 1
006800010718     c                   eval      wokpo = *off
006900010718     c                   do        *hival
007000010718     c                   read      azorg01l
007100010718     c                   if        %eof(azorg01l)
007200010718     c                   leave
007300010718     c                   endif
007400010718     c                   if        orgfva <> ' '
007500010718     c                   iter
007600010718     c                   endif
007700010718     c                   if        orgfag <> 'A' and orgfag <> 'F'
007800010718     c                   iter
007900010718     c                   endif
008000010718      * devo cercare quelli che hanno orgfel = simfel
008100010718     c                   if        orgfel <> simfel
008200010718     c                   iter
008300010718     c                   endif
008400010718      * carico la sk dei p.o.
008500010718     c                   do        200           xa
008600010718     c                   if        skpoel(xa) = orgfil
008700010718     c                   eval      wokpo = *on
008800010718     c                   leave
008900010718     c                   else
009000010718     c                   if        skpoel(xa) = *zeros
009100010718     c                   leave
009200010718     c                   endif
009300010718     c                   endif
009400010718     c                   enddo
009500010718     c                   if        wokpo = *off
009600010718     c                   eval      skpoel(xa) = orgfil
009700010718     c                   endif
009800010718     c                   enddo
009900010214
010000010216     C     vstamp        ifeq      'S'
010100010216     C     vtipoe        ifeq      'S'
010200010214     C                   eval      psimu = '(Simulazione)'
010300010214     C                   endif
010400010214     C                   write     testa
010500010214     C                   z-add     0             num1              9 0
010600010214     C                   z-add     0             num2              9 0
010700010216     C                   endif
010800010214
010900010213      *------------------------------------------------------------------------*
011000010213      *                                                                        *
011100010213      *    ROUTINE PRINCIPALE                                                  *
011200010213      *                                                                        *
011300010213      *------------------------------------------------------------------------*
011400010213
011500010717      *    Lettura File Potenziali
011600010717      *    =======================
011700010213
011800010717     C     *loval        setll     tncpo01l
011900010213     C                   do        *hival
012000010717     C                   read      tncpo01L
012100010717     C                   if        %eof(tncpo01l)
012200010213     C                   leave
012300010213     C                   endif
012400010213
012500010213      *    Sflaggo la causale di annullamento record
012600010213
012700010213     C                   clear                   wdelet            1
012800010214
012900010214      *    Controlli
013000010214      *    =========
013100010213
013200010718      * potenziale annullato lo deleto
013300010718     C                   if        cpoatb = 'A'
013400010718     C                   eval      wdelet = 'A'
013500010517     C                   endif
013600011212
013700011212      * potenziale in carico ad un p.o. uguale a zero
013800011212     c                   if        cpoflt = 0 and wdelet = ' '
013900011212     c                   eval      wdelet = 'Z'
014000011212     c                   endif
014100010219
014200010718      * potenziale non in carico ad un p.o. dell'AS deleto
014300011212     c                   if        wdelet = ' '
014400010718     c     cpoflt        lookup    skpoel                                 30
014500010718     c                   if        *in30 = *off
014600010718     c                   eval      wdelet = 'P'
014700011212     c                   endif
014800011212
014900010718     c                   endif
015000010214
015100010718      *    Stampa potenziali da Pulire
015200010718      *    ===========================
015300010214
015400010216     C     vstamp        ifeq      'S'
015500010216     C                   add       1             num1
015600010214     C     wdelet        ifne      ' '
015700010517     C     wdelet        andne     'A'
015800010214
015900010214     C                   add       1             num2
016000010214
016100010718     C     wdelet        ifeq      'P'
016200010718     C                   eval      errore = 'Pot. non in carico ad un '
016300010718     C                             + 'po dell''AS  '
016400010214     C                   endif
016500010214
016600011212     C     wdelet        ifeq      'Z'
016700011212     C                   eval      errore = 'Pot. in carico a nessun  '
016800011212     C                             + 'P.O.         '
016900011212     C                   endif
017000011212
017100010214     C   55              write     testa
017200010214     C   55              eval      *in55 = *off
017300010214     C                   write     dett
017400010214
017500010214     C                   endif
017600010216     C                   endif
017700010719
017800010724      *    ....... scrivo dlt
017900010719     C     wdelet        ifne      ' '
018000010719     c                   write     tncpod
018100010719     C                   endif
018200010214
018300010718      *    Aggiornamento Potenziali
018400010718      *    ========================
018500010214
018600011212     C     vtipoe        ifne      'S'                                          * se non è simulazio
018700010718      *    ....... aggiorno e sflaggo potenziale da tenere
018800010214     C     wdelet        ifeq      ' '
018900010219     C                   exsr      AGGPOTE
019000010724      *    ....... se è da cancellare: cancello
019100010214     C                   else
019200010718     C                   delete    tncpo000                                     Cancello
019300010214     C                   endif
019400010718     C                   endif
019500010213
019600010213     C                   enddo
019700010718
019800010718      *    Lettura File Storico Potenziali
019900010718      *    ===============================
020000010718
020100010718     C     *loval        setll     tncps11l
020200010718     C                   do        *hival
020300010718     C                   read      tncps11l
020400010718     C                   if        %eof(tncps11l)
020500010718     C                   leave
020600010718     C                   endif
020700010718
020800010718      * per rottura di codice
020900010718     c                   if        cpscpo <> sav_cpscpo
021000010718
021100010718      *    Sflaggo la causale di annullamento record
021200010718
021300010718     C                   clear                   wdelet            1
021400010718
021500010718      *    Controlli
021600010718      *    =========
021700010718
021800010718      * non esiste il codice su TNCPO
021900010718     c     cpscpo        chain     tncpo01l
022000010718     c                   if        not%found(tncpo01l)
022100010718     c                   eval      wdelet = 'S'
022200010718     c                   endif
022300010718      *    Stampa storico potenziali da Pulire
022400010718      *    ===================================
022500010718
022600010718     C     vstamp        ifeq      'S'
022700010718     C                   add       1             num1
022800010718     C     wdelet        ifne      ' '
022900010718
023000010718     C                   add       1             num2
023100010718
023200010718     C     wdelet        ifeq      'S'
023300010718     C                   eval      errore = 'Storico non trovato su   '
023400010718     C                             + 'TNCPO        '
023500010718     C                   endif
023600010718     c                   eval      cpocpo = cpscpo
023700010718     c                   clear                   cporag
023800010718     c                   clear                   cpoflt
023900010718
024000010718     C   55              write     testa
024100010718     C   55              eval      *in55 = *off
024200010718     C                   write     dett
024300010718
024400010718     C                   endif
024500010718     C                   endif
024600010724
024700010724      *    ....... scrivo dlt
024800010724     C     wdelet        ifne      ' '
024900010724     c                   write     tncpsd
025000010724     C                   endif
025100010718
025200010718     c                   eval      sav_cpscpo = cpscpo
025300010718     c                   endif
025400010718
025500010718      *    Aggiornamento storico Potenziali
025600010718      *    ================================
025700010718
025800010718     C     vtipoe        ifne      'S'
025900010718      *    ....... aggiorno e sflaggo potenziale da tenere
026000010718     C     wdelet        ifeq      ' '
026100010718     C                   exsr      AGGPOTES
026200010724      *    ....... se è da cancellare: cancello
026300010718     C                   else
026400010718     C                   delete    tncps000                                     Cancello
026500010718     C                   endif
026600010718     C                   endif
026700010718
026800010718     C                   enddo
026900010718
027000010718      *    Lettura File Informazioni Potenziali
027100010718      *    ====================================
027200010718
027300010718     C     *loval        setll     tncpi01l
027400010718     C                   do        *hival
027500010718     C                   read      tncpi01L
027600010718     C                   if        %eof(tncpi01l)
027700010718     C                   leave
027800010718     C                   endif
027900010718
028000010718      * per rottura di codice
028100010718     c                   if        cpink1 <> sav_cpink1
028200010718
028300010718      *    Sflaggo la causale di annullamento record
028400010718
028500010718     C                   clear                   wdelet            1
028600010718
028700010718      *    Controlli
028800010718      *    =========
028900010718
029000010718      * non esiste il codice su TNCPO
029100010718     c                   movel     cpink1        cpocpo
029200010718     c     cpocpo        chain     tncpo01l
029300010718     c                   if        not%found(tncpo01l)
029400010718     c                   eval      wdelet = 'I'
029500010718     c                   endif
029600010718      *    Stampa informazioni potenziali da Pulire
029700010718      *    ========================================
029800010718
029900010718     C     vstamp        ifeq      'S'
030000010718     C                   add       1             num1
030100010718     C     wdelet        ifne      ' '
030200010718
030300010718     C                   add       1             num2
030400010718
030500010718     C     wdelet        ifeq      'I'
030600010718     C                   eval      errore = 'Informazioni non trovate '
030700010718     C                             + 'su TNCPO     '
030800010718     C                   endif
030900010718     c                   clear                   cporag
031000010718     c                   clear                   cpoflt
031100010718
031200010718     C   55              write     testa
031300010718     C   55              eval      *in55 = *off
031400010718     C                   write     dett
031500010718
031600010718     C                   endif
031700010718     C                   endif
031800010724
031900010724      *    ....... scrivo dlt
032000010724     C     wdelet        ifne      ' '
032100010724     c                   write     tncpid
032200010724     C                   endif
032300010718
032400010718     c                   eval      sav_cpink1 = cpink1
032500010718     c                   endif
032600010718
032700010718      *    Aggiornamento Informazioni Potenziali
032800010718      *    =====================================
032900010718
033000010718     C     vtipoe        ifne      'S'
033100010718      *    ....... aggiorno e sflaggo potenziale da tenere
033200010718     C     wdelet        ifeq      ' '
033300010718     C                   exsr      AGGPOTEI
033400010724      *    ....... se è da cancellare: cancello
033500010718     C                   else
033600010718     C                   delete    tncpi000                                     Cancello
033700010718     C                   endif
033800010718     C                   endif
033900010718
034000010718     C                   enddo
034100010213
034200010213      *    Fine Programma
034300010606      *    ==============
034400010213
034500010216     C     vstamp        ifeq      'S'
034600010214     C                   write     fine
034700010216     C                   endif
034800010214
034900010213     C                   eval      *inlr = *on
035000010219
035100010219      *------------------------------------------------------------------------*
035200010219      *                                                                        *
035300010219      *    SUBROUTINES                                                         *
035400010219      *                                                                        *
035500010219      *------------------------------------------------------------------------*
035600010219
035700010219      *------------------------------------------------------------------------*
035800010221      *    AGGPOTE - Sflaggo potenziale per la trasmissione                    *
035900010219      *------------------------------------------------------------------------*
036000010219
036100010219     C     AGGPOTE       begsr
036200010221     C                   clear                   cpoftr
036300010221     C                   update    tncpo000
036400010219     C                   endsr
036500010221
036600010718      *------------------------------------------------------------------------*
036700010718      *    AGGPOTES - Sflaggo potenziale per la trasmissione                    *
036800010718      *------------------------------------------------------------------------*
036900010718
037000010718     C     AGGPOTES      begsr
037100010718     C                   clear                   cpsftr
037200010718     C                   update    tncps000
037300010718     C                   endsr
037400010718
037500010718      *------------------------------------------------------------------------*
037600010718      *    AGGPOTEI - Sflaggo potenziale per la trasmissione                    *
037700010718      *------------------------------------------------------------------------*
037800010718
037900010718     C     AGGPOTEI      begsr
038000010718     C                   clear                   cpiftr
038100010718     C                   update    tncpi000
038200010718     C                   endsr
038300010718
