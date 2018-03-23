000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020418      * FNLS19R *----------------------------------------------------*
000300020418      *         - RISTAMPA PREPAGATI                    BATCH
000400020418      *--------------------------------------------------------------*
000500010717     FFNBLP31L  IF   E           K DISK
000600990924     FFIAR601L  IF   E           K DISK
000700900517     FTABEL00F  IF   E           K DISK
000800950308     F*
000900920930     D L1              S              3  0 DIM(30)
001000020418     D L6              S              3  0 DIM(30)
001100950308     D LV2             S              3  0 DIM(30)
001200920930     D LIN             S              3  0 DIM(30)
001300030625     D CMD             S              1    DIM(130) CTDATA PERRCD(65)
001400030625     D CM1             S              1    DIM(130) CTDATA PERRCD(65)
001500030625     D CM2             S              1    DIM(130) CTDATA PERRCD(65)
001600030625     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
001700030625     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
001800020418
001900020418     d sav_t06         S              3  0 dim(30)
002000020418
002100020418     d wc              s              3  0
002200020418
002300950308     D*
002400941229     D                 DS
002500941229     D  BLPAAS                 1      4  0
002600941229     D  BLPMGS                 5      8  0
002700941229     D  BLPDSP                 1      8  0
002800030625     D* PASSAGGIO DATI A FNLSB5R   - STAMPA FATTURE -
002900030625     d Fnlsb5ds      e ds
003000030625     D*DSLSB0        E DS                  EXTNAME(FNLSB0DS)
003100030625     D*DSLS01        E DS
003200030625     d Fnls19ds      e ds
003300921222     D DS3A          E DS
003400921222     D DS3L          E DS
003500030625     D*DS5P          E DS
003600920930     D CNCR80        E DS
003700920930     D KPJBA         E DS
003800920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900950308     D* DS PER TRUL06R - CARICAMENTO £X
004000950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004100950314     D  T06                    1     90  0
004200950314     D                                     DIM(30)
004300000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
004400000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
004500920930     C*---------------------------------------------------------------*
004600920930     C*    RIEPILOGO INDICATORI
004700920930     C*---------------------------------------------------------------*
004800920930     C* 03    - IMMESSA DATA AL
004900941229     C* 30/33 - DI COMODO
005000000000     C*---------------------------------------------------------------*
005100000000     C     *ENTRY        PLIST
005200000000     C                   PARM                    KPJBA
005300030625     C**!!!              MOVEL     KPJBU         DSLS01
005400030625     c                   Movel     Kpjbu         Fnls19ds
005500950308     C*
005600990924     C                   MOVEL     '1'           KTRC              1
005700030625     C                   MOVEL     i19ADA        AASP              4 0
005800030625     C                   MOVE      i19ADA        MGSP              4 0
005900950308     C*
006000000000     C                   Z-ADD     1             CODUT
006100920930     C                   CALL      'X§PARUT'
006200920930     C                   PARM                    UT§DSE
006300920930     C                   MOVEL     REC80         CNCR80
006400900511     C*---------------------------------------------------------------*
006500010717     C* ACCESSO FNBLP31L
006600900525     C     KBLP          KLIST
006700950308     C                   KFLD                    COMFIL            3 0
006800941228     C                   KFLD                    AASP
006900941228     C                   KFLD                    MGSP
007000990924     C* ACCESSO FIAR601L
007100990924     C     KAR6          KLIST
007200941229     C                   KFLD                    BLPAAS
007300941229     C                   KFLD                    BLPLNP
007400941229     C                   KFLD                    BLPNRS
007500941229     C                   KFLD                    BLPNSP
007600990924     C                   KFLD                    KTRC
007700910807     C*---------------------------------------------------------------*
007800030625     C                   CLEAR                   Fnlsb5ds
007900920421     C*
008000900524     C* CONTROLLO SE IMMESSA DATA SPEDIZIONE AL
008100030625    1C     i19AAL        IFNE      0
008200030625     C     i19AAL        ANDNE     i19ADA
008300900524     C                   SETON                                        03
008400941229    1C                   END
008500950308      *
008600950308      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
008700030625     C                   MOVEL     'P'           i19TBO
008800030625     C                   CLEAR                   i19PGM
008900030625     C                   MOVEL     'FNLS19R'     i19PGM
009000950308     C                   EXSR      COMUNE
009100941229     C*
009200920930     C                   SETON                                        LR
009300020212     C*****************************************************************
009400020212     C*  PARTE COMUNE STAMPE GIORNALIERE
009500020212     C*
009600020212     C*  . REPERIMENTO 'SIMFEL'
009700020212     C*  . CARICAMENTO '£1'
009800020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
009900020212     C*  . CODA DI STAMPA ('£OUTQ')
010000020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
010100020212     C*  . MODULO PER LA STAMPA ('£MOD')
010200020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
010300020212     C*****************************************************************
010400020212     C     COMUNE        BEGSR
010500020212     C*
010600020212     C*****************************************************************
010700020212     C*       RIEPILOGO INDICATORI:
010800020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
010900020212     C* 11    -           STAMPA PER 1L
011000020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
011100020212     C*****************************************************************
011200020212     C                   Z-ADD     1             CODUT
011300020212     C*
011400020212     C* Reperimento parametri aziendali
011500020212     C                   CALL      'X§PARUT'
011600020212     C                   PARM                    UT§DSE
011700020212     C                   MOVEL     RAGUT         RSUT             20
011800020212     C                   MOVEL     REC80         CNCR80
011900020212     C*
012000020212     C* Chiave di accesso per il file tabelle
012100020212     C     KTAB1         KLIST
012200020212     C                   KFLD                    CODUT
012300020212     C                   KFLD                    KCOD
012400020212     C                   KFLD                    KKEY
012500020212     C     KTAB2         KLIST
012600020212     C                   KFLD                    CODUT
012700020212     C                   KFLD                    KCOD
012800020212     C*
012900020212     C     *LIKE         DEFINE    TBLCOD        KCOD
013000020212     C     *LIKE         DEFINE    TBLKEY        KKEY
013100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
013200020212     C*
013300020212     C* DETERMINO DATA E ORA DI ESECUZIONE
013400020212     C                   TIME                    WTIME            14 0
013500020212     C                   MOVE      WTIME         WDATE             8 0
013600020212     C                   MOVEL     WTIME         UTIME             6 0
013700020212     C*
013800020418     C* CARICO '£1'
013900020212     C                   CLEAR                   KPJBU
014000020212     C                   CLEAR                   DSUL06
014100020212     C                   MOVE      '£1'          D06COD
014200020212     C                   MOVEL     SIMFEL        D06KEY
014300050802     C                   MOVE      'S'           D06KEY
014400050802     C                   MOVEL     i19ada        D06drf
014500020212     C                   MOVEL     DSUL06        KPJBU
014600020212     C                   CALL      'TRUL06R'
014700020212     C                   PARM                    KPJBA
014800020212     C                   MOVEL     KPJBU         DSUL06
014900020418     c                   movea     t06           sav_t06
015000020418
015100020418      * se in arrivo carico la £6
015200030625 b1  c                   if        i19tbo = 'A'
015300020418     c                   clear                   wc
015400020418 b2  c                   do        30            wa
015500020422     c                   if        sav_t06(wa) = *zeros
015600020422     c                   leave
015700020422     c                   endif
015800020418     c                   clear                   kpjbu
015900020418     c                   clear                   dsul06
016000020418     c                   eval      d06cod = '£6'
016100020418     c                   movel     sav_t06(wa)   d06key
016200050802     C                   MOVE      'S'           D06KEY
016300020418     c                   eval      d06esc = 'S'
016400050802     C                   MOVEL     i19ada        D06drf
016500020419     c                   movel     dsul06        kpjbu
016600020418     c                   call      'TRUL06R'
016700020418     c                   parm                    kpjba
016800020418     c                   movel     kpjbu         dsul06
016900020418 b3  c                   do        30            wb
017000020418     c                   if        t06(wb) = *zeros
017100020418     c                   leave
017200020418     c                   endif
017300020418     c                   add       1             wc
017400020418     c                   z-add     t06(wb)       l6(wc)
017500020418 e3  c                   enddo
017600020418 e2  c                   enddo
017700020212     C*
017800020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
017900020212     C                   Z-ADD     *ZERO         WB
018000020418 b2  C                   DO        29            WA
018100020418     C     sav_T06(WA)   LOOKUP    L6                                     30
018200020212     C  N30              ADD       1             WB
018300020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
018400020418 e2  C                   ENDDO
018500020418 x1  C                   ELSE
018600020418     C                   MOVEA     sav_T06       L1
018700020418 e1  C                   ENDIF
018800020212     C*
018900020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
019000020212     C                   MOVEA     L1            LIN
019100020212     C                   CLEAR                   L1
019200020212     C                   Z-ADD     *ZERO         WB                3 0
019300020212     C     1             DO        30            WPT               3 0
019400020212     C     LIN(WPT)      IFNE      0
019500020212     C                   CLEAR                   FNLSC4
019600020212     C                   Z-ADD     LIN(WPT)      IC4LIN
019700020212     C                   MOVE      'B'           IC4FLG
019800020212     C                   MOVEL     FNLSC4        KPJBU
019900020212     C                   CALL      'FNLSC4R1'
020000020212     C                   PARM                    KPJBA
020100050802     C                   PARM                    i19ada
020200020212     C                   MOVEL     KPJBU         FNLSC4
020300020212     C     OC4ESE        IFEQ      'S'
020400020212     C                   ADD       1             WB
020500020212     C                   MOVE      LIN(WPT)      L1(WB)
020600020212     C                   ENDIF
020700020212     C                   ENDIF
020800020212     C                   ENDDO
020900070103     C*
021000070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
021100070103     C                   Z-ADD     *ZERO         WB                3 0
021200070103     C                   DO        29            WA                3 0
021300070103     C     L1(WA)        IFNE      *ZEROS
021400070103     C     L1(WA)        ANDNE     SIMFEL
021500070103     C                   ADD       1             WB
021600070103     C                   Z-ADD     L1(WA)        LV2(WB)
021700070103     C                   ENDIF
021800070103     C                   ENDDO
021900020212     C*
022000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
022100020212     C*
022200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
022300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
022400020212     C* la stampa e' lanciata da menu
022500020212     C                   MOVEL     '3L'          KCOD
022600030625     C                   MOVEL     i19PGM        KKEY
022700030625     C                   MOVE      i19FPG        KKEY
022800020212     C     KTAB1         CHAIN     TABEL00F                           30
022900020212     C  N30              MOVEL     TBLUNI        DS3L
023000020212     C   30              MOVEL     'F'           §3LD1L
023100020212     C   30              MOVEL     ' '           §3LD2L
023200020212     C   30              MOVEL     '*STD    '    §3LDST
023300020212     C*
023400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
023500030625     C**!!!              MOVEL     §3LDST        £MOD             10
023600020212     C                   CLEAR                   LIN
023700020212     C*
023800020212     C* Verifico chi ha lanciato la stampa
023900020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
024000020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
024100020212     C* DS1FGS<>0  --->  si proviene da menu
024200030625     C     i19FGS        IFEQ      0
024300020212     C*
024400020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
024500020212     C* lancio, voglio la stampa;  invece
024600020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
024700020212     C*
024800020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
024900020212     C* creare stampa per 1°lvl. (se stessa)
025000030625     C     i19LNP        IFEQ      SIMFEL
025100020212     C*
025200020212     C* Il flag §3LD1L ha i seguenti significati:
025300020212     C*    ' ' = tutti i dati
025400020212     C*    '1' = solo filiale di 1' livello
025500020212     C     §3LD1L        IFEQ      '1'
025600020212     C                   SETON                                        11
025700020212     C                   END
025800020212     C*
025900020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
026000020212     C* nella stampa
026100020212     C*
026200020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
026300020212     C  N11              MOVEA     L1            LIN
026400020212     C   11              MOVEA     SIMFEL        LIN(1)
026500020212     C*
026600020212     C* IMPOSTO LA STAMPANTE:
026700020212     C* STAMPANTE TABULATI
026800030625     C**!!!              MOVEL     '5P'          KCOD
026900030625     C**!!!              MOVEL     'T'           W004A             4
027000030625     C**!!!              MOVE      LIN(1)        W004A
027100030625     C**!!!              MOVEL(P)  W004A         KKEY
027200030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
027300030625     C**!!!*IN99         IFEQ      *OFF
027400030625     C**!!!              MOVEL     TBLUNI        DS5P
027500030625     C**!!!              MOVEL     §5POTQ        £OUTQ            10
027600030625     C**!!!              END
027700020212     C* STAMPANTE BOLLE E FATTURE
027800030625     C**!!!              MOVEL     'B'           KKEY
027900030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
028000030625     C**!!!*IN99         IFEQ      *OFF
028100030625     C**!!!              MOVEL     TBLUNI        DS5P
028200030625     C**!!!              MOVEL     §5POTQ        £OUTQF           10
028300030625     C**!!!              END
028400020212     C*
028500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
028600020212     C                   EXSR      LEGGI
028700020212     C*
028800020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
028900020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
029000020212     C                   ELSE
029100020212     C*
029200020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
029300020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
029400020212     C*
029500020212     C* Il significato del fla §3LD2L e' il seguente:
029600020212     C*    ' ' = tutti i dati
029700020212     C*    'F' = per ogni 2' livello
029800020212     C     §3LD2L        IFEQ      'F'
029900020212     C                   SETON                                        14
030000020212     C                   ENDIF
030100020212     C*
030200020212     C                   Z-ADD     1             WB
030300020212     C                   MOVEA     *ZEROS        LIN
030400020212     C  N14              MOVEA     L1            LIN
030500020212     C*
030600020212     C* Lancio per tutti i 2° lvl
030700030625     C     i19LNP        IFEQ      0
030800020212     C*
030900020212     C* LV2 contiene tutte le filiali di 2° lvl
031000020212     C     LV2(WB)       DOWNE     0
031100020212      *
031200020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
031300020212     C                   CLEAR                   FNLSC4
031400020212     C                   Z-ADD     LV2(WB)       IC4LIN
031500020212     C                   MOVEL     FNLSC4        KPJBU
031600020212     C                   CALL      'FNLSC4R1'
031700020212     C                   PARM                    KPJBA
031800050802     C                   PARM                    i19ada
031900020212     C                   MOVEL     KPJBU         FNLSC4
032000020212      *
032100020212     C     OC4PT1        IFEQ      'S'
032200020212     C     §3LPT         ANDEQ     'N'
032300020212     C                   GOTO      NOLEGG
032400020212     C                   ENDIF
032500020212      *
032600020212     C     OC4EXP        IFEQ      'S'
032700020212     C     §3LEXP        ANDEQ     'N'
032800020212     C                   GOTO      NOLEGG
032900020212     C                   ENDIF
033000020212      *
033100020212     C     OC4DPD        IFEQ      'S'
033200020212     C     §3LDPD        ANDEQ     'N'
033300020212     C                   GOTO      NOLEGG
033400020212     C                   ENDIF
033500020212      *
033600020212     C     OC4BAR        IFEQ      'S'
033700020212     C     §3LBAR        ANDEQ     'N'
033800020212     C                   GOTO      NOLEGG
033900020212     C                   ENDIF
034000020212      *
034100020212     C     OC4FED        IFEQ      'S'
034200020212     C     §3LFED        ANDEQ     'N'
034300020212     C                   GOTO      NOLEGG
034400020212     C                   ENDIF
034500020212      *
034600020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
034700020212     C*  dati degli altri 2° lvl
034800020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
034900020212     C   14              MOVEL     LV2(WB)       LIN(1)
035000020212     C*
035100020212     C* IMPOSTO LA STAMPANTE
035200020212     C* STAMPANTE TABULATI
035300030625     C**!!!              MOVEL     '5P'          KCOD
035400030625     C**!!!              MOVEL     'T'           W004A             4
035500030625     C**!!!              MOVE      LV2(WB)       W004A
035600030625     C**!!!              MOVEL(P)  W004A         KKEY
035700030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
035800030625     C**!!!*IN99         IFEQ      *OFF
035900030625     C**!!!              MOVEL     TBLUNI        DS5P
036000030625     C**!!!              MOVEL     §5POTQ        £OUTQ            10
036100030625     C**!!!              END
036200020212     C* STAMPANTE BOLLE E FATTURE
036300030625     C**!!!              MOVEL     'B'           KKEY
036400030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
036500030625     C**!!!*IN99         IFEQ      *OFF
036600030625     C**!!!              MOVEL     TBLUNI        DS5P
036700030625     C**!!!              MOVEL     §5POTQ        £OUTQF           10
036800030625     C**!!!              END
036900020212     C*
037000020212     C* Compongo il modulo di stampa con il codice della filiale
037100020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
037200020212     C*
037300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
037400020212     C                   EXSR      LEGGI
037500020212     C*
037600020212     C     NOLEGG        TAG
037700020212     C*
037800020212     C                   ADD       1             WB
037900020212     C                   ENDDO
038000020212     C*
038100020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
038200020212     C                   ELSE
038300020212     C*
038400020212     C* Lancio solo per la 2° lvl richiesta
038500030625     C   14              MOVEL     i19LNP        LIN(1)
038600020212     C*
038700020212     C* STAMPANTE
038800020212     C* STAMPANTE TABULATI
038900030625     C**!!!              MOVEL     '5P'          KCOD
039000030625     C**!!!              MOVEL     'T'           W004A             4
039100030625     C**!!!              MOVE      DS1LNP        W004A
039200030625     C**!!!              MOVEL(P)  W004A         KKEY
039300030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
039400030625     C**!!!*IN99         IFEQ      *OFF
039500030625     C**!!!              MOVEL     TBLUNI        DS5P
039600030625     C**!!!              MOVEL     §5POTQ        £OUTQ            10
039700030625     C**!!!              END
039800020212     C* STAMPANTE BOLLE E FATTURE
039900030625     C**!!!              MOVEL     'B'           KKEY
040000030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
040100030625     C**!!!*IN99         IFEQ      *OFF
040200030625     C**!!!              MOVEL     TBLUNI        DS5P
040300030625     C**!!!              MOVEL     §5POTQ        £OUTQF           10
040400030625     C**!!!              END
040500020212     C*
040600020212     C* MODULO STAMPA
040700020212     C******               MOVE DS1LNP    £MOD
040800020212     C*
040900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
041000020212     C                   EXSR      LEGGI
041100020212     C*
041200020212     C                   ENDIF
041300020212     C                   ENDIF
041400020212     C*
041500020212     C                   ELSE
041600020212     C*
041700020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
041800020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
041900030625     C     i19LNP        IFEQ      0
042000020212     C                   MOVEA     L1            LIN
042100020212     C* ...altrimenti elaboro solo se stessa
042200020212     C                   ELSE
042300030625     C                   MOVEL     i19LNP        LIN(1)
042400020212     C                   ENDIF
042500020212     C*
042600020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
042700020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
042800020212     C* PRENDO QUELLA PRESENTE IN KPJBA
042900030625     C**!!!i19FGS        IFEQ      SIMFEL
043000030625     C**!!!              MOVEL     KQOTD         £OUTQ
043100030625     C**!!!              MOVEL     KQOTD         £OUTQF
043200030625     C**!!!              ELSE
043300020212     C* STAMPANTE TABULATI
043400030625     C**!!!              MOVEL     '5P'          KCOD
043500030625     C**!!!              MOVEL     'T'           W004A             4
043600030625     C**!!!              MOVE      DS1FGS        W004A
043700030625     C**!!!              MOVEL(P)  W004A         KKEY
043800030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
043900030625     C**!!!*IN99         IFEQ      *OFF
044000030625     C**!!!              MOVEL     TBLUNI        DS5P
044100030625     C**!!!              MOVEL     §5POTQ        £OUTQ            10
044200030625     C**!!!              END
044300020212     C* STAMPANTE BOLLE E FATTURE
044400030625     C**!!!              MOVEL     'B'           KKEY
044500030625     C**!!!KTAB1         CHAIN     TABEL00F                           99
044600030625     C**!!!*IN99         IFEQ      *OFF
044700030625     C**!!!              MOVEL     TBLUNI        DS5P
044800030625     C**!!!              MOVEL     §5POTQ        £OUTQF           10
044900030625     C**!!!              END
045000020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
045100030625     C**!!!              ENDIF
045200020212     C*
045300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
045400020212     C                   EXSR      LEGGI
045500020212     C                   ENDIF
045600020212     C*
045700020212     C                   ENDSR
045800950308     C**************************************************************************
045900950308     C* LETTURA BOLLE PER CREAZIONE STAMPA
046000950308     C**************************************************************************
046100920930     C     LEGGI         BEGSR
046200030625
046300030625     C                   Z-ADD     110           LUNG             15 5
046400950308     C*
046500030625     C* ESEGUO OVRPRTF BOLLE - vecchio modulo
046600030625     C**!!!              MOVEA     £MOD          CMD(33)
046700030625     C**!!!              MOVEA     £OUTQF        CMD(51)
046800030625     c                   Movea     i19mdb        cmd(33)
046900030625     c                   Movea     i19prb        cmd(51)
047000030625     C                   MOVEL     *BLANKS       COMMAN          110
047100950308     C                   MOVEA     CMD           COMMAN
047200920930     C                   CALL      'QCMDEXC'                            H1
047300920930     C                   PARM                    COMMAN
047400920930     C                   PARM                    LUNG
047500991206     C*
047600030625     C* ESEGUO OVRPRTF DPD - vecchio modulo
047700030625     C**!!!              MOVEA     £OUTQF        CM1(51)
047800030625     c                   Movea     i19prb        cm1(51)
047900030625     C**!!!              Z-ADD     73            LUNG
048000991206     C                   MOVEL     *BLANKS       COMMAN
048100991206     C                   MOVEA     CM1           COMMAN
048200991206     C                   CALL      'QCMDEXC'                            H1
048300991206     C                   PARM                    COMMAN
048400991206     C                   PARM                    LUNG
048500030304     C*
048600030625     C* ESEGUO OVRPRTF FedEx - vecchio modulo
048700030625     C**!!!              MOVEA     £OUTQF        CM2(51)
048800030625     c                   Movea     i19prb        cm2(51)
048900030625     C**!!!              Z-ADD     90            LUNG
049000030304     C                   MOVEL     *BLANKS       COMMAN
049100030304     C                   MOVEA     CM2           COMMAN
049200030625     C**!!!              Eval      %Subst(Comman:75:9) = 'COPIES(2)'
049300030304     C                   CALL      'QCMDEXC'                            H1
049400030304     C                   PARM                    COMMAN
049500030304     C                   PARM                    LUNG
049600030625
049700030625      * Bolle - Nuovo modulo
049800030625      * Ovrprtf Bolle A4
049900030625     c                   Movea     i19prb4       cma4(30)
050000030625     c                   Movea     i19mdb4       cma4(52)
050100030625     c                   Clear                   Comman
050200030625     c                   Movea     cmA4          Comman
050300030625     c                   Call      'QCMDEXC'                            h1
050400030625     c                   Parm                    Comman
050500030625     c                   Parm                    Lung
050600030625      * Ovrprtf Bolle A5
050700030625     c                   Movea     i19prb5       cmA5(30)
050800030625     c                   Movea     i19mdb5       cmA5(52)
050900030625     c                   Clear                   Comman
051000030625     c                   Movea     cmA5          Comman
051100030625     c                   Call      'QCMDEXC'                            h1
051200030625     c                   Parm                    Comman
051300030625     c                   Parm                    Lung
051400920930     C*
051500950308     C                   Z-ADD     1             C                 3 0
051600920930     C*
051700941229    1C     LIN(C)        DOWNE     0
051800920930     C                   MOVEL     LIN(C)        COMFIL
051900900524     C*
052000941229     C     KBLP          SETLL     FNBLP000
052100900524     C*
052200941229     C  N03KBLP          READE     FNBLP000                               31
052300941229     C   03COMFIL        READE     FNBLP000                               31
052400920930     C*
052500941229    2C     *IN31         DOWEQ     *OFF
052600941229     C*
052700900524     C* SOLO SE DATA DAL AL CONTROLLO SE SUPERA LIMITE MAGGIORE
052800941229    3C     *IN03         IFEQ      *ON
052900030625     C     BLPDSP        ANDGT     i19AAL
053000920930     C                   SETON                                        31
053100920930     C*
053200941229   X3C                   ELSE
053300941229     C*
053400920930     C* ELABORO SOLO I FRANCHI
053500950308     C                   MOVEL     '3A'          KCOD
053600950308     C                   MOVEL(P)  BLPCBO        KKEY
053700950308     C     KTAB1         CHAIN     TABEL00F                           32
053800941229     C   32              CLEAR                   DS3A
053900941229     C*
054000920930     C  N32              MOVEL     TBLUNI        DS3A
054100941229     C                   MOVEL     §3ATB1        FLGTB1            1
054200920930     C*
054300941229    4C     FLGTB1        IFEQ      'F'
054400990924     C     KAR6          CHAIN     FIAR601L                           33
054500920930     C*
054600900525     C* SE IMMESSO NUM.FAT-ESCLUDO QUELLI CHE NON HANNO QUEL NUMERO
054700941229    5C     *IN33         IFEQ      *OFF
054800941229     C*
054900030625    6C     i19NFT        IFNE      0
055000030625    7C     i19NFT        IFEQ      AR6NFT
055100030625     C                   CLEAR                   Fnlsb5ds
055200990701     C                   MOVEL     '8'           DB0TBO
055300950112     C                   Z-ADD     BLPAAS        DB0AAS
055400950112     C                   Z-ADD     BLPLNP        DB0LNP
055500950112     C                   Z-ADD     BLPNRS        DB0NRS
055600950112     C                   Z-ADD     BLPNSP        DB0NSP
055700030625     C                   CALL      i19psl
055800030625     C                   PARM                    Fnlsb5ds
055900920930     C                   SETON                                        31
056000941229    7C                   ENDIF
056100920930     C*
056200941229   X6C                   ELSE
056300900525     C*
056400900525     C* ALTRIMENTI ESCLUDO QUELLI CHE IN GENERE NON HANNO UN NUMERO FAT
056500990924    7C     AR6NFT        IFNE      0
056600030625     C                   CLEAR                   Fnlsb5ds
056700990701     C                   MOVEL     '8'           DB0TBO
056800941229     C                   Z-ADD     BLPAAS        DB0AAS
056900941229     C                   Z-ADD     BLPLNP        DB0LNP
057000941229     C                   Z-ADD     BLPNRS        DB0NRS
057100941229     C                   Z-ADD     BLPNSP        DB0NSP
057200030625     C                   CALL      i19psl
057300030625     C                   PARM                    Fnlsb5ds
057400941229    7C                   ENDIF
057500941229    6C                   ENDIF
057600941229    5C                   ENDIF
057700941229    4C                   ENDIF
057800920930     C*
057900941229     C  N03
058000941229     CANN31KBLP          READE     FNBLP000                               31
058100941229     C   03
058200941229     CANN31COMFIL        READE     FNBLP000                               31
058300941229    3C                   ENDIF
058400941229    2C                   ENDDO
058500920930     C*
058600920930     C                   ADD       1             C
058700941229    1C                   ENDDO
058800920930     C*
058900950127     C* CALL A VUOTO PER CHIUDERE I FILES
059000950127     C                   MOVEL     'C'           DB0TLA
059100030625     C                   CALL      i19psl
059200030625     C                   PARM                    fnlsb5ds
059300950127     C*
059400920930     C                   ENDSR
059500941228     C*---------------------------------------------------------------*
059600991206**   CMD
059700030625OVRPRTF FILE(FNLSB0P) FORMTYPE('          ') OUTQ(          )
059800030625 SHARE(*YES) USRDTA('Prepagati')
059900991206**   CM1
060000030625OVRPRTF FILE(FNLSB0P1) FORMTYPE('BOLLE-dpd') OUTQ(          )
060100030625 SHARE(*YES) USRDTA('Prepagati')
060200030304**   CM2
060300030625OVRPRTF FILE(FNLSB0P2) FORMTYPE('BOLLE-Fed') OUTQ(          )
060400030625 SHARE(*YES) USRDTA('Prepagati') COPIES(2)
060500030625** cmA4
060600030625OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
060700030625 SHARE(*YES)          USRDTA('Prepag_A4')
060800030625**  cmA5
060900030625OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
061000030625 SHARE(*YES)          USRDTA('Prepag_A5')
