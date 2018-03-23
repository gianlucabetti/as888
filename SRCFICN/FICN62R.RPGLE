000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030828     H/TITLE  Lancio pulizia conteggi autotr./coop./aff/defl
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600020603      * ficn62R                                                      *
000700951009      *     *----------------------------------------------*         *
000800031124      *      pulizia archivi conteggi                                *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500020603     Fficn62D   CF   E             WORKSTN
001600020529     Ftntbe01l  IF   E           K DISK
001700020529     D dblc          E DS
001800030213     D dblo          E DS
001900030828     D dbld          E DS
002000020702     D ut§dse0f      E DS
002100020411     D* Reperimento nome PGM
002200020411     D STATUS         SDS           333
002300020411     D  nompgm           *PROC
002400020411     D*---------------------------------------------------------------*
002500020411     D* CAMPI INTERNI
002600020411     D*---------------------------------------------------------------*
002700020411     D dataiso         s               d   datfmt(*iso)
002800020411     D dataeur         s               d   datfmt(*eur)
002900120515     D dataeurINI      s               d   datfmt(*eur)
003000020823     D com08           s                   like(v1cdti)
003100030212     D TIPO            s              1
003200951009     D KPJBA         E DS
003201160119     D paramSRE        DS
003202160119     d  pardat                        8s 0
003203160119     d  partip                        1
003300951009?     *--------------------------------------------------------------*
003400951009?     *  CICLO PRINCIPALE                                            *
003500951009?     *--------------------------------------------------------------*
003600951009      *  Loop gestione videata
003700951009     C     WFINE         DOUEQ     'S'
003800000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003900951012     C                   END
004000020418     C                   END
004100980312     C*
004200020529     C     FINE          TAG
004300951009     C                   SETON                                        LR
004400020405?     *--------------------------------------------------------------*
004500020405?     *  GESS01: Gestione prima videata                              *
004600020405?     *--------------------------------------------------------------*
004700020405     C     GESS01        BEGSR
004800020405      *
004900020419     c                   if        *in28
005000020419     c                   setoff                                       28
005100020419     C                   write     y350d01
005200020419     c                   seton                                        28
005300020419     C                   END
005400020415     C                   EXFMT     y350d01
005500020415      *  Fine Lavoro
005600020415     C     *INKC         IFEQ      '1'
005700020415     C                   MOVEL     'S'           WFINE
005800020415     c                   goto      finvd1
005900020415     C                   END
006000951009      *  Controlli
006100951009     C                   EXSR      CTR01
006200020405     C   28              GOTO      FINVD1
006300020408      *  CONFERMA
006400951013     C     *INKF         IFEQ      '1'
006500020603     C                   MOVE      v1cdti        dataeur
006600020603     C                   MOVE      dataeur       dataiso
006700020603     C                   MOVE      dataiso       v1cdti
006800020603     C                   MOVEl(p)  v1cdti        kpjbu
006900020419     c     knmus         ifeq      *all'1'
007000030828     c                   select
007100030828     c                   when      tipo = 'A'
007200020603     c                   call      'FICN63C'
007300020419     c                   parm                    kpjba
007400030828     c                   when      tipo = 'C'
007500030212     c                   call      'FICN66C'
007600030212     c                   parm                    kpjba
007700030828     c                   when      tipo = 'D'
007800030828     c                   call      'FICN73R'
007900030828     c                   parm                    kpjba
008000031124     c                   when      tipo = 'O'
008100031124     c                   call      'FICN97R'
008200031124     c                   parm                    kpjba
008300040218     c                   when      tipo = 'G'
008400040218     c                   call      'FICN98R'
008500040218     c                   parm                    kpjba
008600030828     c                   endsl
008601160119     c                   movel     v1cdti        pardat
008605160119     c                   move      tipo          partip
008606160119     c                   movel     paramSRE      kpjbu
008607160119     c                   call      'FICN666R'
008608160119     c                   parm                    kpjba
008700020419     c                   else
008800030828     c                   select
008900030828     c                   when      tipo = 'A'
009000020603     C                   MOVEL     'FN63'        KCOAZ
009100030828     c                   when      tipo = 'C'
009200030212     C                   MOVEL     'FN66'        KCOAZ
009300030828     c                   when      tipo = 'D'
009400030828     C                   MOVEL     'FN73'        KCOAZ
009500031124     c                   when      tipo = 'O'
009600031124     C                   MOVEL     'FN97'        KCOAZ
009700040218     c                   when      tipo = 'G'
009800040218     C                   MOVEL     'FN98'        KCOAZ
009900030828     c                   endsl
010000030214     C                   CALL      'BCH10'
010100030214     C                   PARM                    KPJBA
010101160119     C                   MOVEL     'F666'        KCOAZ
010102160119     c                   movel     v1cdti        pardat
010103160127     c                   move      pardat        dataiso
010104160127     c                   subdur    4:*y          dataiso
010105160127     c                   move      dataiso       pardat
010106160119     c                   move      tipo          partip
010107160119     c                   movel     paramSRE      kpjbu
010108160119     C                   CALL      'BCH10'
010109160119     c                   parm                    kpjba
010200030212     c                   end
010300020423     c                   movel     'S'           wfine
010400980911     C                   END                                                    V3CTOT = y35FAT
010500951009      *
010600951009     C     FINVD1        ENDSR
010700951009?     *--------------------------------------------------------------*
010800951009?     *  CTR01: Controlli prima videata                              *
010900951009?     *--------------------------------------------------------------*
011000951009     C     CTR01         BEGSR
011100951009      *
011200020419     c                   setoff                                       28
011300020603     c* data fattura
011400020415     c                   if        v1cdti <> 0
011500020415     c     *eur          test(d)                 v1cdti                 41
011600020415     c                   if        *in41
011700020415     c     *dmy          test(d)                 v1cdti                 41
011800121109     c  n41*dmy          move      v1cdti        dataeur
011900121109     c  n41              move      dataeur       v1cdti
012000121109     c                   else
012100121109     C                   MOVE      v1cdti        dataeur
012200121109     c                   end
012300020415     c                   else
012400020415     c                   seton                                        41
012500020415     c                   end
012600120515      **
012700120515      *  imposta sempre il primo giorno del mese da cui trovare il limite dei 5 mesi
012800120515     c                   move      *DATE         dataini           8 0
012900120515     c                   movel     '01'          dataini
013000120515     c                   move      dataini       dataeurINI
013100121109     c******
013200121109      **  I mesi da tenere a disposizione sul sistema servono per le statistiche della DLO
013300121109      **   in ogni caso si sono dovuti ridurre i tempi da 5 a 3
013400121109     c******             subdur    5:*m          dataeurINI
013500121109     c                   subdur    3:*m          dataeurINI
013600120515     c                   subdur    1:*D          dataeurINI
013700120515      **
013800120515     c  n41dataeur       comp      dataeurINI                         42
013900120515     c  n41              if        *in42
014000120515     c                   eval      msgv = 'Deve essere precedente o al massimo -
014100120515     c                             uguale al ' + %CHAR(dataeurINI)
014200120515     c                   seton                                        28
014300120515     c                   leavesr
014400120515     c                   endIF
014500120515      *
014600020823     c* messaggio informativo se la data scelta è diversa dalla data fatt.
014700020823     c* in corso
014800031124     c                   if        v1cdti <> v1cdpr and tipo <> 'O'
014900020823     c                   eval      msgv = 'ATTENZIONE: la data scelta è diversa-
015000030213     c                              dalla data di fatturazione proposta'
015100020823     c                   else
015200020823     c                   clear                   msgv
015300020823     c                   end
015400020823     c* controllo che la data di pulizia sia < della data di salvataggio
015500020823     c                   move      dataeur       dataiso
015600020823     c                   move      dataiso       com08
015700030828     c                   select
015800030828     c                   when      tipo = 'A'
015900030213     c                   if        com08 > §blcsav
016000020823     c                   seton                                        28
016100020823     c                   end
016200030828     c                   when      tipo = 'C'
016300030213     c                   if        com08 > §blosav
016400030213     c                   seton                                        28
016500030213     c                   end
016600030828     c                   when      tipo = 'D'
016700030828     c                   if        com08 > §bldsav
016800030828     c                   seton                                        28
016900030828     c                   end
017000030828     c                   endsl
017100030828     c   28              eval      msgv = 'ATTENZIONE: si vuole pulire senza av-
017200030828     c                             er prima salvato'
017300951010      *
017400020419     C                   ENDSR
017500951011?     *--------------------------------------------------------------*
017600951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
017700951011?     *--------------------------------------------------------------*
017800951011     C     *INZSR        BEGSR
017900951011      *
018000951011     C     *ENTRY        PLIST
018100951011     C                   PARM                    KPJBA
018200030212     c                   movel     kpjbu         tipo
018300030828     c                   select
018400030828     c                   when      tipo = 'A'
018500030212     c                   eval      destip = 'AUTOTRASPORTATORI'
018600030828     c                   when      tipo = 'C'
018700030212     c                   eval      destip = '   COOPERATIVE   '
018800030828     c                   when      tipo = 'D'
018900030828     c                   eval      destip = 'AFFLUEN./DEFLUEN.'
019000031124     c                   when      tipo = 'O'
019100031124     c                   eval      destip = 'PRESTAZ.OCCASION.'
019200040218     c                   when      tipo = 'G'
019300040218     c                   eval      destip = '  STIME E SALDI  '
019400030828     c                   endsl
019500030212     c*
019600020702     C                   Z-ADD     1             CODUT
019700020702     C                   CALL      'X§PARUT'
019800020702     C                   PARM                    UT§DSE0F
019900020702     c                   movel     ragut         rsut
020000020418      *  Inizializzo variabili
020100020418     C                   MOVEL     '1'           WTPVID            1
020200020418     C                   MOVEL     'N'           WFINE             1
020300020529     C     Ktbe          KLIST
020400020529     C                   KFLD                    tbecod
020500020529     C                   KFLD                    tbeke1
020600020529      *  lettura tabella BLC
020700030828     c                   movel(p)  '1'           tbeke1
020800030828     c                   select
020900030828     c                   when      tipo = 'A'
021000020529     c                   movel     'BLC'         tbecod
021100020529     c     ktbe          chain     tntbe01l
021200020529     c                   if        %found(tntbe01l)
021300020529     c                   movel     tbeuni        dblc
021400020529     c* data protocollo
021500020529     c                   move      §blcdtp       dataiso
021600030321     c                   else
021700030321     c                   clear                   dblc
021800030321     c                   end
021900030213      *  lettura tabella BLO
022000040218     c                   when      tipo = 'C' or tipo = 'G'
022100030213     c                   movel     'BLO'         tbecod
022200030213     c     ktbe          chain     tntbe01l
022300030213     c                   if        %found(tntbe01l)
022400030213     c                   movel     tbeuni        dblo
022500030213     c* data protocollo
022600030213     c                   move      §blodtp       dataiso
022700030213     c                   else
022800030213     c                   clear                   dblo
022900030213     c                   end
023000030828      *  lettura tabella BLD
023100030828     c                   when      tipo = 'D'
023200030828     c                   movel     'BLD'         tbecod
023300030828     c     ktbe          chain     tntbe01l
023400030828     c                   if        %found(tntbe01l)
023500030828     c                   movel     tbeuni        dbld
023600030828     c* data protocollo
023700030828     c                   move      §blddtp       dataiso
023800030828     c                   else
023900030828     c                   clear                   dbld
024000030828     c                   end
024100030828     c                   endsl
024200030828     c                   if        %found(tntbe01l)
024300030321     c                   move      dataiso       wdatpr
024400030321     c                   move      01            wdatpr
024500030321     c                   move      wdatpr        dataiso
024600140925     c                   subdur    2:*m          dataiso
024700030321     c                   subdur    1:*d          dataiso
024800030321     c                   move      dataiso       wdatpr            8 0
024900030321     c                   move      dataiso       dataeur
025000030321     c                   move      dataeur       v1cdpr            8 0
025100030828     c                   else
025200030828     c                   move      0             v1cdpr
025300030321     c                   end
025400020529     C                   Z-ADD     v1cdpr        v1cdti
025500120515      *
025600951012     C                   ENDSR
