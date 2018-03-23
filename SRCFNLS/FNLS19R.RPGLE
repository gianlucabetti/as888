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
001300030625     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
001400030625     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
001500020418
001600020418     d sav_t06         S              3  0 dim(30)
001700020418
001800020418     d wc              s              3  0
001900020418
002000950308     D*
002100941229     D                 DS
002200941229     D  BLPAAS                 1      4  0
002300941229     D  BLPMGS                 5      8  0
002400941229     D  BLPDSP                 1      8  0
002500030625     D* PASSAGGIO DATI A FNLSB5R   - STAMPA FATTURE -
002600030625     d Fnlsb5ds      e ds
002700030625     d Fnls19ds      e ds
002800921222     D DS3A          E DS
002900921222     D DS3L          E DS
003000920930     D CNCR80        E DS
003100920930     D KPJBA         E DS
003200920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003300950308     D* DS PER TRUL06R - CARICAMENTO £X
003400950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003500950314     D  T06                    1     90  0
003600950314     D                                     DIM(30)
003700000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
003800000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
003900920930     C*---------------------------------------------------------------*
004000920930     C*    RIEPILOGO INDICATORI
004100920930     C*---------------------------------------------------------------*
004200920930     C* 03    - IMMESSA DATA AL
004300941229     C* 30/33 - DI COMODO
004400000000     C*---------------------------------------------------------------*
004500000000     C     *ENTRY        PLIST
004600000000     C                   PARM                    KPJBA
004700030625     C**!!!              MOVEL     KPJBU         DSLS01
004800030625     c                   Movel     Kpjbu         Fnls19ds
004900950308     C*
005000990924     C                   MOVEL     '1'           KTRC              1
005100030625     C                   MOVEL     i19ADA        AASP              4 0
005200030625     C                   MOVE      i19ADA        MGSP              4 0
005300950308     C*
005400000000     C                   Z-ADD     1             CODUT
005500920930     C                   CALL      'X§PARUT'
005600920930     C                   PARM                    UT§DSE
005700920930     C                   MOVEL     REC80         CNCR80
005800900511     C*---------------------------------------------------------------*
005900010717     C* ACCESSO FNBLP31L
006000900525     C     KBLP          KLIST
006100950308     C                   KFLD                    COMFIL            3 0
006200941228     C                   KFLD                    AASP
006300941228     C                   KFLD                    MGSP
006400990924     C* ACCESSO FIAR601L
006500990924     C     KAR6          KLIST
006600941229     C                   KFLD                    BLPAAS
006700941229     C                   KFLD                    BLPLNP
006800941229     C                   KFLD                    BLPNRS
006900941229     C                   KFLD                    BLPNSP
007000990924     C                   KFLD                    KTRC
007100910807     C*---------------------------------------------------------------*
007200030625     C                   CLEAR                   Fnlsb5ds
007300920421     C*
007400900524     C* CONTROLLO SE IMMESSA DATA SPEDIZIONE AL
007500030625    1C     i19AAL        IFNE      0
007600030625     C     i19AAL        ANDNE     i19ADA
007700900524     C                   SETON                                        03
007800941229    1C                   END
007900950308      *
008000950308      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
008100030625     C                   MOVEL     'P'           i19TBO
008200030625     C                   CLEAR                   i19PGM
008300030625     C                   MOVEL     'FNLS19R'     i19PGM
008400950308     C                   EXSR      COMUNE
008500941229     C*
008600920930     C                   SETON                                        LR
008700020212     C*****************************************************************
008800020212     C*  PARTE COMUNE STAMPE GIORNALIERE
008900020212     C*
009000020212     C*  . REPERIMENTO 'SIMFEL'
009100020212     C*  . CARICAMENTO '£1'
009200020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
009300020212     C*  . CODA DI STAMPA ('£OUTQ')
009400020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
009500020212     C*  . MODULO PER LA STAMPA ('£MOD')
009600020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
009700020212     C*****************************************************************
009800020212     C     COMUNE        BEGSR
009900020212     C*
010000020212     C*****************************************************************
010100020212     C*       RIEPILOGO INDICATORI:
010200020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
010300020212     C* 11    -           STAMPA PER 1L
010400020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
010500020212     C*****************************************************************
010600020212     C                   Z-ADD     1             CODUT
010700020212     C*
010800020212     C* Reperimento parametri aziendali
010900020212     C                   CALL      'X§PARUT'
011000020212     C                   PARM                    UT§DSE
011100020212     C                   MOVEL     RAGUT         RSUT             20
011200020212     C                   MOVEL     REC80         CNCR80
011300020212     C*
011400020212     C* Chiave di accesso per il file tabelle
011500020212     C     KTAB1         KLIST
011600020212     C                   KFLD                    CODUT
011700020212     C                   KFLD                    KCOD
011800020212     C                   KFLD                    KKEY
011900020212     C     KTAB2         KLIST
012000020212     C                   KFLD                    CODUT
012100020212     C                   KFLD                    KCOD
012200020212     C*
012300020212     C     *LIKE         DEFINE    TBLCOD        KCOD
012400020212     C     *LIKE         DEFINE    TBLKEY        KKEY
012500020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
012600020212     C*
012700020212     C* DETERMINO DATA E ORA DI ESECUZIONE
012800020212     C                   TIME                    WTIME            14 0
012900020212     C                   MOVE      WTIME         WDATE             8 0
013000020212     C                   MOVEL     WTIME         UTIME             6 0
013100020212     C*
013200020418     C* CARICO '£1'
013300020212     C                   CLEAR                   KPJBU
013400020212     C                   CLEAR                   DSUL06
013500020212     C                   MOVE      '£1'          D06COD
013600020212     C                   MOVEL     SIMFEL        D06KEY
013700050802     C                   MOVE      'S'           D06KEY
013800050802     C                   MOVEL     i19ada        D06drf
013900020212     C                   MOVEL     DSUL06        KPJBU
014000020212     C                   CALL      'TRUL06R'
014100020212     C                   PARM                    KPJBA
014200020212     C                   MOVEL     KPJBU         DSUL06
014300020418     c                   movea     t06           sav_t06
014400020418
014500020418      * se in arrivo carico la £6
014600030625 b1  c                   if        i19tbo = 'A'
014700020418     c                   clear                   wc
014800020418 b2  c                   do        30            wa
014900020422     c                   if        sav_t06(wa) = *zeros
015000020422     c                   leave
015100020422     c                   endif
015200020418     c                   clear                   kpjbu
015300020418     c                   clear                   dsul06
015400020418     c                   eval      d06cod = '£6'
015500020418     c                   movel     sav_t06(wa)   d06key
015600050802     C                   MOVE      'S'           D06KEY
015700020418     c                   eval      d06esc = 'S'
015800050802     C                   MOVEL     i19ada        D06drf
015900020419     c                   movel     dsul06        kpjbu
016000020418     c                   call      'TRUL06R'
016100020418     c                   parm                    kpjba
016200020418     c                   movel     kpjbu         dsul06
016300020418 b3  c                   do        30            wb
016400020418     c                   if        t06(wb) = *zeros
016500020418     c                   leave
016600020418     c                   endif
016700020418     c                   add       1             wc
016800020418     c                   z-add     t06(wb)       l6(wc)
016900020418 e3  c                   enddo
017000020418 e2  c                   enddo
017100020212     C*
017200020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
017300020212     C                   Z-ADD     *ZERO         WB
017400020418 b2  C                   DO        29            WA
017500020418     C     sav_T06(WA)   LOOKUP    L6                                     30
017600020212     C  N30              ADD       1             WB
017700020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
017800020418 e2  C                   ENDDO
017900020418 x1  C                   ELSE
018000020418     C                   MOVEA     sav_T06       L1
018100020418 e1  C                   ENDIF
018200020212     C*
018300020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
018400020212     C                   MOVEA     L1            LIN
018500020212     C                   CLEAR                   L1
018600020212     C                   Z-ADD     *ZERO         WB                3 0
018700020212     C     1             DO        30            WPT               3 0
018800020212     C     LIN(WPT)      IFNE      0
018900020212     C                   CLEAR                   FNLSC4
019000020212     C                   Z-ADD     LIN(WPT)      IC4LIN
019100020212     C                   MOVE      'B'           IC4FLG
019200020212     C                   MOVEL     FNLSC4        KPJBU
019300020212     C                   CALL      'FNLSC4R1'
019400020212     C                   PARM                    KPJBA
019500050802     C                   PARM                    i19ada
019600020212     C                   MOVEL     KPJBU         FNLSC4
019700020212     C     OC4ESE        IFEQ      'S'
019800020212     C                   ADD       1             WB
019900020212     C                   MOVE      LIN(WPT)      L1(WB)
020000020212     C                   ENDIF
020100020212     C                   ENDIF
020200020212     C                   ENDDO
020300070103     C*
020400070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
020500070103     C                   Z-ADD     *ZERO         WB                3 0
020600070103     C                   DO        29            WA                3 0
020700070103     C     L1(WA)        IFNE      *ZEROS
020800070103     C     L1(WA)        ANDNE     SIMFEL
020900070103     C                   ADD       1             WB
021000070103     C                   Z-ADD     L1(WA)        LV2(WB)
021100070103     C                   ENDIF
021200070103     C                   ENDDO
021300020212     C*
021400020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
021500020212     C*
021600020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
021700020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
021800020212     C* la stampa e' lanciata da menu
021900020212     C                   MOVEL     '3L'          KCOD
022000030625     C                   MOVEL     i19PGM        KKEY
022100030625     C                   MOVE      i19FPG        KKEY
022200020212     C     KTAB1         CHAIN     TABEL00F                           30
022300020212     C  N30              MOVEL     TBLUNI        DS3L
022400020212     C   30              MOVEL     'F'           §3LD1L
022500020212     C   30              MOVEL     ' '           §3LD2L
022600020212     C   30              MOVEL     '*STD    '    §3LDST
022700020212     C*
022800020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
022900030625     C**!!!              MOVEL     §3LDST        £MOD             10
023000020212     C                   CLEAR                   LIN
023100020212     C*
023200020212     C* Verifico chi ha lanciato la stampa
023300020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
023400020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
023500020212     C* DS1FGS<>0  --->  si proviene da menu
023600030625     C     i19FGS        IFEQ      0
023700020212     C*
023800020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
023900020212     C* lancio, voglio la stampa;  invece
024000020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
024100020212     C*
024200020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
024300020212     C* creare stampa per 1°lvl. (se stessa)
024400030625     C     i19LNP        IFEQ      SIMFEL
024500020212     C*
024600020212     C* Il flag §3LD1L ha i seguenti significati:
024700020212     C*    ' ' = tutti i dati
024800020212     C*    '1' = solo filiale di 1' livello
024900020212     C     §3LD1L        IFEQ      '1'
025000020212     C                   SETON                                        11
025100020212     C                   END
025200020212     C*
025300020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
025400020212     C* nella stampa
025500020212     C*
025600020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
025700020212     C  N11              MOVEA     L1            LIN
025800020212     C   11              MOVEA     SIMFEL        LIN(1)
025900020212     C*
026000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
026100020212     C                   EXSR      LEGGI
026200020212     C*
026300020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
026400020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
026500020212     C                   ELSE
026600020212     C*
026700020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
026800020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
026900020212     C*
027000020212     C* Il significato del fla §3LD2L e' il seguente:
027100020212     C*    ' ' = tutti i dati
027200020212     C*    'F' = per ogni 2' livello
027300020212     C     §3LD2L        IFEQ      'F'
027400020212     C                   SETON                                        14
027500020212     C                   ENDIF
027600020212     C*
027700020212     C                   Z-ADD     1             WB
027800020212     C                   MOVEA     *ZEROS        LIN
027900020212     C  N14              MOVEA     L1            LIN
028000020212     C*
028100020212     C* Lancio per tutti i 2° lvl
028200030625     C     i19LNP        IFEQ      0
028300020212     C*
028400020212     C* LV2 contiene tutte le filiali di 2° lvl
028500020212     C     LV2(WB)       DOWNE     0
028600020212      *
028700020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
028800020212     C                   CLEAR                   FNLSC4
028900020212     C                   Z-ADD     LV2(WB)       IC4LIN
029000020212     C                   MOVEL     FNLSC4        KPJBU
029100020212     C                   CALL      'FNLSC4R1'
029200020212     C                   PARM                    KPJBA
029300050802     C                   PARM                    i19ada
029400020212     C                   MOVEL     KPJBU         FNLSC4
029500020212      *
029600020212     C     OC4PT1        IFEQ      'S'
029700020212     C     §3LPT         ANDEQ     'N'
029800020212     C                   GOTO      NOLEGG
029900020212     C                   ENDIF
030000020212      *
030100020212     C     OC4EXP        IFEQ      'S'
030200020212     C     §3LEXP        ANDEQ     'N'
030300020212     C                   GOTO      NOLEGG
030400020212     C                   ENDIF
030500020212      *
030600020212     C     OC4DPD        IFEQ      'S'
030700020212     C     §3LDPD        ANDEQ     'N'
030800020212     C                   GOTO      NOLEGG
030900020212     C                   ENDIF
031000020212      *
031100020212     C     OC4BAR        IFEQ      'S'
031200020212     C     §3LBAR        ANDEQ     'N'
031300020212     C                   GOTO      NOLEGG
031400020212     C                   ENDIF
031500020212      *
031600020212     C     OC4FED        IFEQ      'S'
031700020212     C     §3LFED        ANDEQ     'N'
031800020212     C                   GOTO      NOLEGG
031900020212     C                   ENDIF
032000020212      *
032100020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
032200020212     C*  dati degli altri 2° lvl
032300020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
032400020212     C   14              MOVEL     LV2(WB)       LIN(1)
032500020212     C*
032600020212     C* Compongo il modulo di stampa con il codice della filiale
032700020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
032800020212     C*
032900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
033000020212     C                   EXSR      LEGGI
033100020212     C*
033200020212     C     NOLEGG        TAG
033300020212     C*
033400020212     C                   ADD       1             WB
033500020212     C                   ENDDO
033600020212     C*
033700020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
033800020212     C                   ELSE
033900020212     C*
034000020212     C* Lancio solo per la 2° lvl richiesta
034100030625     C   14              MOVEL     i19LNP        LIN(1)
034200020212     C*
034300020212     C* MODULO STAMPA
034400020212     C******               MOVE DS1LNP    £MOD
034500020212     C*
034600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
034700020212     C                   EXSR      LEGGI
034800020212     C*
034900020212     C                   ENDIF
035000020212     C                   ENDIF
035100020212     C*
035200020212     C                   ELSE
035300020212     C*
035400020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
035500020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
035600030625     C     i19LNP        IFEQ      0
035700020212     C                   MOVEA     L1            LIN
035800020212     C* ...altrimenti elaboro solo se stessa
035900020212     C                   ELSE
036000030625     C                   MOVEL     i19LNP        LIN(1)
036100020212     C                   ENDIF
036200020212     C*
036300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
036400020212     C                   EXSR      LEGGI
036500020212     C                   ENDIF
036600020212     C*
036700020212     C                   ENDSR
036800950308     C**************************************************************************
036900950308     C* LETTURA BOLLE PER CREAZIONE STAMPA
037000950308     C**************************************************************************
037100920930     C     LEGGI         BEGSR
037200030625
037300030625     C                   Z-ADD     110           LUNG             15 5
037400950308     C*
037500030625      * Bolle - Nuovo modulo
037600030625      * Ovrprtf Bolle A4
037700030625     c                   Movea     i19prb4       cma4(30)
037800030625     c                   Movea     i19mdb4       cma4(52)
037900080710     c                   Clear                   Comman          110
038000030625     c                   Movea     cmA4          Comman
038100030625     c                   Call      'QCMDEXC'                            h1
038200030625     c                   Parm                    Comman
038300030625     c                   Parm                    Lung
038400030625      * Ovrprtf Bolle A5
038500030625     c                   Movea     i19prb5       cmA5(30)
038600030625     c                   Movea     i19mdb5       cmA5(52)
038700030625     c                   Clear                   Comman
038800030625     c                   Movea     cmA5          Comman
038900030625     c                   Call      'QCMDEXC'                            h1
039000030625     c                   Parm                    Comman
039100030625     c                   Parm                    Lung
039200920930     C*
039300950308     C                   Z-ADD     1             C                 3 0
039400920930     C*
039500941229    1C     LIN(C)        DOWNE     0
039600920930     C                   MOVEL     LIN(C)        COMFIL
039700900524     C*
039800941229     C     KBLP          SETLL     FNBLP000
039900900524     C*
040000941229     C  N03KBLP          READE     FNBLP000                               31
040100941229     C   03COMFIL        READE     FNBLP000                               31
040200920930     C*
040300941229    2C     *IN31         DOWEQ     *OFF
040400941229     C*
040500900524     C* SOLO SE DATA DAL AL CONTROLLO SE SUPERA LIMITE MAGGIORE
040600941229    3C     *IN03         IFEQ      *ON
040700030625     C     BLPDSP        ANDGT     i19AAL
040800920930     C                   SETON                                        31
040900920930     C*
041000941229   X3C                   ELSE
041100941229     C*
041200920930     C* ELABORO SOLO I FRANCHI
041300950308     C                   MOVEL     '3A'          KCOD
041400950308     C                   MOVEL(P)  BLPCBO        KKEY
041500950308     C     KTAB1         CHAIN     TABEL00F                           32
041600941229     C   32              CLEAR                   DS3A
041700941229     C*
041800920930     C  N32              MOVEL     TBLUNI        DS3A
041900941229     C                   MOVEL     §3ATB1        FLGTB1            1
042000920930     C*
042100941229    4C     FLGTB1        IFEQ      'F'
042200990924     C     KAR6          CHAIN     FIAR601L                           33
042300920930     C*
042400900525     C* SE IMMESSO NUM.FAT-ESCLUDO QUELLI CHE NON HANNO QUEL NUMERO
042500941229    5C     *IN33         IFEQ      *OFF
042600941229     C*
042700030625    6C     i19NFT        IFNE      0
042800030625    7C     i19NFT        IFEQ      AR6NFT
042900030625     C                   CLEAR                   Fnlsb5ds
043000990701     C                   MOVEL     '8'           DB0TBO
043100950112     C                   Z-ADD     BLPAAS        DB0AAS
043200950112     C                   Z-ADD     BLPLNP        DB0LNP
043300950112     C                   Z-ADD     BLPNRS        DB0NRS
043400950112     C                   Z-ADD     BLPNSP        DB0NSP
043500030625     C                   CALL      i19psl
043600030625     C                   PARM                    Fnlsb5ds
043700920930     C                   SETON                                        31
043800941229    7C                   ENDIF
043900920930     C*
044000941229   X6C                   ELSE
044100900525     C*
044200900525     C* ALTRIMENTI ESCLUDO QUELLI CHE IN GENERE NON HANNO UN NUMERO FAT
044300990924    7C     AR6NFT        IFNE      0
044400030625     C                   CLEAR                   Fnlsb5ds
044500990701     C                   MOVEL     '8'           DB0TBO
044600941229     C                   Z-ADD     BLPAAS        DB0AAS
044700941229     C                   Z-ADD     BLPLNP        DB0LNP
044800941229     C                   Z-ADD     BLPNRS        DB0NRS
044900941229     C                   Z-ADD     BLPNSP        DB0NSP
045000030625     C                   CALL      i19psl
045100030625     C                   PARM                    Fnlsb5ds
045200941229    7C                   ENDIF
045300941229    6C                   ENDIF
045400941229    5C                   ENDIF
045500941229    4C                   ENDIF
045600920930     C*
045700941229     C  N03
045800941229     CANN31KBLP          READE     FNBLP000                               31
045900941229     C   03
046000941229     CANN31COMFIL        READE     FNBLP000                               31
046100941229    3C                   ENDIF
046200941229    2C                   ENDDO
046300920930     C*
046400920930     C                   ADD       1             C
046500941229    1C                   ENDDO
046600920930     C*
046700950127     C* CALL A VUOTO PER CHIUDERE I FILES
046800950127     C                   MOVEL     'C'           DB0TLA
046900030625     C                   CALL      i19psl
047000030625     C                   PARM                    fnlsb5ds
047100950127     C*
047200920930     C                   ENDSR
047300941228     C*---------------------------------------------------------------*
047400030625** cmA4
047500030625OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
047600030625 SHARE(*YES)          USRDTA('Prepag_A4')
047700030625**  cmA5
047800030625OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
047900030625 SHARE(*YES)          USRDTA('Prepag_A5')
