000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020418      * FNLS10R *----------------------------------------------------*
000300020418      *  - STAMPA : SPEDIZIONI 8888 / CONVENUTI / OLTRE 35 Q.LI /
000400040621      *             OLTRE MC 5 E KG 500 / CON CAP GENERICO / oltre 20 colli
000500020418      *--------------------------------------------------------------*
000600030912     fFNBLP44L  if   e           k disk
000700060216     FFiar401L  IF   E           K DISK
000800991005     FFIAR601L  IF   E           K DISK
000900991020     FFIAR701L  IF   E           K DISK
001000051115     FFiar901L  IF   E           K DISK
001100920303     FTABEL00F  IF   E           K DISK
001200920916     FAZORG01L  IF   E           K DISK
001300920917     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400920917     F                                     USROPN
001500950309     D*
001600950309     D CMD             S              1    DIM(61) CTDATA PERRCD(61)
001700040621     D SK              S             66    DIM(28) CTDATA PERRCD(1)
001800920916     D L1              S              3  0 DIM(30)
001900020418     D L6              S              3  0 DIM(30)
002000950309     D LV2             S              3  0 DIM(30)
002100920916     D LIN             S              3  0 DIM(30)
002200941223     D CAP             S              9    DIM(250)
002300030912     d $SOP            s                   like(BLPsop)  dim(50)  inz
002400030915     d                                                   descend
002500030912     d $SOPcont        s              5  0               dim(50)  inz
002600030915     d $SOPsave        s                   like(BLPsop)  dim(50)  inz
002700030915     d $SOPsaveC       s              5  0               dim(50)  inz
002800020418
002900020418     d sav_t06         S              3  0 dim(30)
003000020418
003100030915     d wa              s              3  0 inz
003200030915     d wb              s              3  0 inz
003300020418     d wc              s              3  0
003400030915     d WPT             s              3  0 inz
003500030912     d xx              s              3  0 inz
003600030912     d XaltriSOP       s              5  0 inz
003700020418
003800950309     D*
003900900517     D                 DS
004000941223     D  BLPAAS                 1      4  0
004100941223     D  BLPMGS                 5      8  0
004200941223     D  BLPDSP                 1      8  0
004300941223     D*
004400941223     D* PARAMETRI PASSATI DAI PGM RICHIAMANTI
004500950309     D DSLS01        E DS
004600941227     D                 DS
004700941227     D  AA                     1      4  0
004800941227     D  MM                     5      6  0
004900941227     D  GG                     7      8  0
005000941227     D  DATA                   1      8  0
005100090604     d ds5e          e ds
005200900613     D DS3A          E DS
005300921001     D DS3I          E DS
005400991020     D DS$2          E DS
005500920916     D DS3L          E DS
005600970707     D DS5P          E DS
005700900613     D KPJBA         E DS
005800910807     D CNCR80        E DS
005900970708     D* CONTROLLO DI UN CAP
006000970708     D DSSI95        E DS                  EXTNAME(TISI95DS)
006100970708     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006200950309     D* DS PER TRUL06R - CARICAMENTO £X
006300950309     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006400950314     D  T06                    1     90  0
006500950314     D                                     DIM(30)
006600000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
006700000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
006800000000     C*---------------------------------------------------------------*
006900920916     C*    RIEPILOGO INDICATORI
007000920916     C*---------------------------------------------------------------*
007100920916     C* 03    - IMMESSA DATA AL
007200920924     C* 05    - ESISTE SECONDA BOLLA
007300920917     C* 06    - VAL MERCE
007400920917     C* 07    - CONTRASSEGNO
007500920924     C* 08    - ESISTE NUMERO E DATA FATTURA
007600920917     C* 09    - ESEGUITA LA STAMPA DI ALMENO UNA BOLLA
007700941227     C* 19/23 - ESISTONO DISPOSIZIONI IN BOLLA
007800931206     C* 22    - PFIL >< 0   E   LNP >< 0
007900040621     c* 24    - Spedizioni oltre 20 colli
008000920917     C* 25    - SPEDIZIONI 8888
008100920917     C* 26    - SPEDIZIONI CON CONVENUTO
008200920917     C* 27    - SPEDIZIONI OLTRE 35 Q.LI
008300921001     C* 28    - SPEDIZIONI OLTRE KG 500 E MC 5
008400921002     C* 29    - CON CAP GENERICO
008500920917     C* 30/32 - DI COMODO
008600920916     C*---------------------------------------------------------------*
008700000000     C     *ENTRY        PLIST
008800000000     C                   PARM                    KPJBA
008900950309     C                   MOVEL     KPJBU         DSLS01
009000950309     C*
009100950309     C                   MOVEL     DS1ADA        AASP              4 0
009200950309     C                   MOVE      DS1ADA        MGSP              4 0
009300950309     C*
009400000000     C                   Z-ADD     1             CODUT
009500950309     C                   CALL      'X§PARUT'
009600950309     C                   PARM                    UT§DSE
009700950309     C                   MOVEL     RAGUT         RSUT             20
009800950309     C                   MOVEL     REC80         CNCR80
009900900511     C*---------------------------------------------------------------*
010000030912     C* ACCESSO FNBLP44L
010100030912     C     KBLP          KLIST
010200030912     C                   KFLD                    COMFIL            3 0
010300030912     C                   KFLD                    AASP
010400030912     C                   KFLD                    MGSP
010500051115     C* ACCESSO Fiar901L
010600991020     C     KBLP9         KLIST
010700920924     C                   KFLD                    BLPAAS
010800920924     C                   KFLD                    BLPLNP
010900920924     C                   KFLD                    BLPNRS
011000920924     C                   KFLD                    BLPNSP
011100060216     C* ACCESSO Fiar401L/FIAR601L/FIAR701L
011200941227     C     KBLP3         KLIST
011300941227     C                   KFLD                    BLPAAS
011400941227     C                   KFLD                    BLPLNP
011500941227     C                   KFLD                    BLPNRS
011600941227     C                   KFLD                    BLPNSP
011700941227     C                   KFLD                    WTRC
011800941223     C*
011900941223     C* DEFINIZIONE CAMPI
012000991020     C     *LIKE         DEFINE    AR6VA1        STA2VA
012100991020     C     *LIKE         DEFINE    AR6VA1        VARIAI
012200991020     C     *LIKE         DEFINE    AR6SV1        WSV1
012300991020     C     *LIKE         DEFINE    AR6VA1        WVA1
012400991020     C     *LIKE         DEFINE    AR6DIV        STADIV
012500991020     C     *LIKE         DEFINE    AR6SV1        STASV1
012600991020     C     *LIKE         DEFINE    AR6SV2        STASV2
012700991020     C     *LIKE         DEFINE    AR6NFT        MEMNFT
012800991020     C     *LIKE         DEFINE    AR6DFT        MEMDFT
012900991020     C     *LIKE         DEFINE    BLPDSP        STADSP
013000991005     C     *LIKE         DEFINE    AR6DFT        STADFT
013100991005     C     *LIKE         DEFINE    AR6NFT        STANFT
013200060216     C     *LIKE         DEFINE    ar4TRC        WTRC
013300060216     C     *LIKE         DEFINE    ar4NOT        STANT1
013400060216     C     *LIKE         DEFINE    ar4NOT        STANT2
013500941223     C*---------------------------------------------------------------*
013600920916     C*
013700920916     C* CONTROLLO SE IMMESSA DATA SPEDIZIONE AL
013800950309     C     DS1AAL        IFNE      0
013900920916     C                   SETON                                        03
014000920916     C                   END
014100920917     C*
014200920917     C* CONTROLLO IL TIPO DI STAMPA RICHIESTO
014300991020     C                   SELECT
014400991020     C     DS1FPG        WHENEQ    '8'
014500991020     C                   SETON                                        25
014600991020     C     DS1FPG        WHENEQ    'Q'
014700991020     C                   SETON                                        27
014800991020     C     DS1FPG        WHENEQ    'P'
014900991020     C                   SETON                                        29
015000991020     C                   OTHER
015100991020     C     DS1FPG        COMP      'C'                                    26
015200991020     C     DS1FPG        COMP      '5'                                    28
015300040621     C     DS1FPG        COMP      '2'                                    24
015400991020     C                   ENDSL
015500030912      *
015600941223     C* CARICAMENTO TABELLE
015700920928     C                   EXSR      CARTAB
015800920928     C*
015900950309      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
016000950309     C                   MOVEL     'P'           DS1TBO
016100950309     C                   CLEAR                   DS1PGM
016200950309     C                   MOVEL     'FNLS10R'     DS1PGM
016300950309     C                   EXSR      COMUNE
016400920916     C*
016500970708     C     *IN29         IFEQ      *ON
016600970714     C                   CLEAR                   DSSI95
016700970708     C                   MOVEL     'C'           I95TLA
016800970708     C                   CALL      'TISI95R'
016900970708     C                   PARM                    DSSI95
017000970708     C                   ENDIF
017100970708     C**
017200920916     C                   SETON                                        LR
017300020212     C*****************************************************************
017400020212     C*  PARTE COMUNE STAMPE GIORNALIERE
017500020212     C*
017600020212     C*  . REPERIMENTO 'SIMFEL'
017700020212     C*  . CARICAMENTO '£1'
017800020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
017900020212     C*  . CODA DI STAMPA ('£OUTQ')
018000020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
018100020212     C*  . MODULO PER LA STAMPA ('£MOD')
018200020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
018300020212     C*****************************************************************
018400020212     C     COMUNE        BEGSR
018500020212     C*
018600020212     C*****************************************************************
018700020212     C*       RIEPILOGO INDICATORI:
018800020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
018900020212     C* 11    -           STAMPA PER 1L
019000020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
019100020212     C*****************************************************************
019200020212     C                   Z-ADD     1             CODUT
019300020212     C*
019400020212     C* Reperimento parametri aziendali
019500020212     C                   CALL      'X§PARUT'
019600020212     C                   PARM                    UT§DSE
019700020212     C                   MOVEL     RAGUT         RSUT             20
019800020212     C                   MOVEL     REC80         CNCR80
019900020212     C*
020000020212     C* Chiave di accesso per il file tabelle
020100020212     C     KTAB1         KLIST
020200020212     C                   KFLD                    CODUT
020300020212     C                   KFLD                    KCOD
020400020212     C                   KFLD                    KKEY
020500020212     C     KTAB2         KLIST
020600020212     C                   KFLD                    CODUT
020700020212     C                   KFLD                    KCOD
020800020212     C*
020900020212     C     *LIKE         DEFINE    TBLCOD        KCOD
021000020212     C     *LIKE         DEFINE    TBLKEY        KKEY
021100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
021200020212     C*
021300020212     C* DETERMINO DATA E ORA DI ESECUZIONE
021400020212     C                   TIME                    WTIME            14 0
021500020212     C                   MOVE      WTIME         WDATE             8 0
021600020212     C                   MOVEL     WTIME         UTIME             6 0
021700020212     C*
021800020418     C* CARICO '£1'
021900020212     C                   CLEAR                   KPJBU
022000020212     C                   CLEAR                   DSUL06
022100020212     C                   MOVE      '£1'          D06COD
022200020212     C                   MOVEL     SIMFEL        D06KEY
022300050802     C                   MOVE      'S'           D06KEY
022400050802     C                   MOVEL     ds1ada        D06drf
022500020212     C                   MOVEL     DSUL06        KPJBU
022600020212     C                   CALL      'TRUL06R'
022700020212     C                   PARM                    KPJBA
022800020212     C                   MOVEL     KPJBU         DSUL06
022900020418     c                   movea     t06           sav_t06
023000020418
023100020418      * se in arrivo carico la £6
023200020418 b1  c                   if        ds1tbo = 'A'
023300020418     c                   clear                   wc
023400020418 b2  c                   do        30            wa
023500020422     c                   if        sav_t06(wa) = *zeros
023600020422     c                   leave
023700020422     c                   endif
023800020418     c                   clear                   kpjbu
023900020418     c                   clear                   dsul06
024000020418     c                   eval      d06cod = '£6'
024100020418     c                   movel     sav_t06(wa)   d06key
024200050802     C                   MOVE      'S'           D06KEY
024300020418     c                   eval      d06esc = 'S'
024400050802     C                   MOVEL     ds1ada        D06drf
024500020419     c                   movel     dsul06        kpjbu
024600020418     c                   call      'TRUL06R'
024700020418     c                   parm                    kpjba
024800020418     c                   movel     kpjbu         dsul06
024900020418 b3  c                   do        30            wb
025000020418     c                   if        t06(wb) = *zeros
025100020418     c                   leave
025200020418     c                   endif
025300020418     c                   add       1             wc
025400020418     c                   z-add     t06(wb)       l6(wc)
025500020418 e3  c                   enddo
025600020418 e2  c                   enddo
025700020212     C*
025800020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
025900020212     C                   Z-ADD     *ZERO         WB
026000020418 b2  C                   DO        29            WA
026100020418     C     sav_T06(WA)   LOOKUP    L6                                     30
026200020212     C  N30              ADD       1             WB
026300020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
026400020418 e2  C                   ENDDO
026500020418 x1  C                   ELSE
026600020418     C                   MOVEA     sav_T06       L1
026700020418 e1  C                   ENDIF
026800020212     C*
026900020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
027000020212     C                   MOVEA     L1            LIN
027100020212     C                   CLEAR                   L1
027200030915     C                   Z-ADD     *ZERO         WB
027300030915     C     1             DO        30            WPT
027400020212     C     LIN(WPT)      IFNE      0
027500020212     C                   CLEAR                   FNLSC4
027600020212     C                   Z-ADD     LIN(WPT)      IC4LIN
027700020212     C                   MOVE      'B'           IC4FLG
027800020212     C                   MOVEL     FNLSC4        KPJBU
027900020212     C                   CALL      'FNLSC4R1'
028000020212     C                   PARM                    KPJBA
028100050802     C                   PARM                    ds1ada
028200020212     C                   MOVEL     KPJBU         FNLSC4
028300020212     C     OC4ESE        IFEQ      'S'
028400020212     C                   ADD       1             WB
028500020212     C                   MOVE      LIN(WPT)      L1(WB)
028600020212     C                   ENDIF
028700020212     C                   ENDIF
028800020212     C                   ENDDO
028900070103     C*
029000070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
029100070103     C                   Z-ADD     *ZERO         WB
029200070103     C                   DO        29            WA
029300070103     C     L1(WA)        IFNE      *ZEROS
029400070103     C     L1(WA)        ANDNE     SIMFEL
029500070103     C                   ADD       1             WB
029600070103     C                   Z-ADD     L1(WA)        LV2(WB)
029700070103     C                   ENDIF
029800070103     C                   ENDDO
029900020212     C*
030000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
030100020212     C*
030200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
030300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
030400020212     C* la stampa e' lanciata da menu
030500020212     C                   MOVEL     '3L'          KCOD
030600020212     C                   MOVEL     DS1PGM        KKEY
030700020212     C                   MOVE      DS1FPG        KKEY
030800020212     C     KTAB1         CHAIN     TABEL00F                           30
030900020212     C  N30              MOVEL     TBLUNI        DS3L
031000020212     C   30              MOVEL     'F'           §3LD1L
031100020212     C   30              MOVEL     ' '           §3LD2L
031200020212     C   30              MOVEL     '*STD    '    §3LDST
031300020212     C*
031400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
031500020212     C                   MOVEL     §3LDST        £MOD             10
031600020212     C                   CLEAR                   LIN
031700020212     C*
031800020212     C* Verifico chi ha lanciato la stampa
031900020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
032000020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
032100020212     C* DS1FGS<>0  --->  si proviene da menu
032200020212     C     DS1FGS        IFEQ      0
032300020212     C*
032400020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
032500020212     C* lancio, voglio la stampa;  invece
032600020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
032700020212     C*
032800020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
032900020212     C* creare stampa per 1°lvl. (se stessa)
033000020212     C     DS1LNP        IFEQ      SIMFEL
033100020212     C*
033200020212     C* Il flag §3LD1L ha i seguenti significati:
033300020212     C*    ' ' = tutti i dati
033400020212     C*    '1' = solo filiale di 1' livello
033500020212     C     §3LD1L        IFEQ      '1'
033600020212     C                   SETON                                        11
033700020212     C                   END
033800020212     C*
033900020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
034000020212     C* nella stampa
034100020212     C*
034200020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
034300020212     C  N11              MOVEA     L1            LIN
034400020212     C   11              MOVEA     SIMFEL        LIN(1)
034500020212     C*
034600020212     C* IMPOSTO LA STAMPANTE:
034700020212     C* STAMPANTE TABULATI
034800020212     C                   MOVEL     '5P'          KCOD
034900020212     C                   MOVEL     'T'           W004A             4
035000020212     C                   MOVE      LIN(1)        W004A
035100020212     C                   MOVEL(P)  W004A         KKEY
035200020212     C     KTAB1         CHAIN     TABEL00F                           99
035300020212     C     *IN99         IFEQ      *OFF
035400020212     C                   MOVEL     TBLUNI        DS5P
035500020212     C                   MOVEL     §5POTQ        £OUTQ            10
035600020212     C                   END
035700020212     C*
035800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
035900020212     C                   EXSR      LEGGI
036000020212     C*
036100020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
036200020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
036300020212     C                   ELSE
036400020212     C*
036500020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
036600020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
036700020212     C*
036800020212     C* Il significato del fla §3LD2L e' il seguente:
036900020212     C*    ' ' = tutti i dati
037000020212     C*    'F' = per ogni 2' livello
037100020212     C     §3LD2L        IFEQ      'F'
037200020212     C                   SETON                                        14
037300020212     C                   ENDIF
037400020212     C*
037500020212     C                   Z-ADD     1             WB
037600020212     C                   MOVEA     *ZEROS        LIN
037700020212     C  N14              MOVEA     L1            LIN
037800020212     C*
037900020212     C* Lancio per tutti i 2° lvl
038000020212     C     DS1LNP        IFEQ      0
038100020212     C*
038200020212     C* LV2 contiene tutte le filiali di 2° lvl
038300020212     C     LV2(WB)       DOWNE     0
038400020212      *
038500020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
038600020212     C                   CLEAR                   FNLSC4
038700020212     C                   Z-ADD     LV2(WB)       IC4LIN
038800020212     C                   MOVEL     FNLSC4        KPJBU
038900020212     C                   CALL      'FNLSC4R1'
039000020212     C                   PARM                    KPJBA
039100050802     C                   PARM                    ds1ada
039200020212     C                   MOVEL     KPJBU         FNLSC4
039300020212      *
039400020212     C     OC4PT1        IFEQ      'S'
039500020212     C     §3LPT         ANDEQ     'N'
039600020212     C                   GOTO      NOLEGG
039700020212     C                   ENDIF
039800020212      *
039900020212     C     OC4EXP        IFEQ      'S'
040000020212     C     §3LEXP        ANDEQ     'N'
040100020212     C                   GOTO      NOLEGG
040200020212     C                   ENDIF
040300020212      *
040400020212     C     OC4DPD        IFEQ      'S'
040500020212     C     §3LDPD        ANDEQ     'N'
040600020212     C                   GOTO      NOLEGG
040700020212     C                   ENDIF
040800020212      *
040900020212     C     OC4BAR        IFEQ      'S'
041000020212     C     §3LBAR        ANDEQ     'N'
041100020212     C                   GOTO      NOLEGG
041200020212     C                   ENDIF
041300020212      *
041400020212     C     OC4FED        IFEQ      'S'
041500020212     C     §3LFED        ANDEQ     'N'
041600020212     C                   GOTO      NOLEGG
041700020212     C                   ENDIF
041800020212      *
041900020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
042000020212     C*  dati degli altri 2° lvl
042100020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
042200020212     C   14              MOVEL     LV2(WB)       LIN(1)
042300020212     C*
042400020212     C* IMPOSTO LA STAMPANTE
042500020212     C* STAMPANTE TABULATI
042600020212     C                   MOVEL     '5P'          KCOD
042700020212     C                   MOVEL     'T'           W004A             4
042800020212     C                   MOVE      LV2(WB)       W004A
042900020212     C                   MOVEL(P)  W004A         KKEY
043000020212     C     KTAB1         CHAIN     TABEL00F                           99
043100020212     C     *IN99         IFEQ      *OFF
043200020212     C                   MOVEL     TBLUNI        DS5P
043300020212     C                   MOVEL     §5POTQ        £OUTQ            10
043400020212     C                   END
043500020212     C*
043600020212     C* Compongo il modulo di stampa con il codice della filiale
043700020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
043800020212     C*
043900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
044000020212     C                   EXSR      LEGGI
044100020212     C*
044200020212     C     NOLEGG        TAG
044300020212     C*
044400020212     C                   ADD       1             WB
044500020212     C                   ENDDO
044600020212     C*
044700020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
044800020212     C                   ELSE
044900020212     C*
045000020212     C* Lancio solo per la 2° lvl richiesta
045100020212     C   14              MOVEL     DS1LNP        LIN(1)
045200020212     C*
045300020212     C* STAMPANTE
045400020212     C* STAMPANTE TABULATI
045500020212     C                   MOVEL     '5P'          KCOD
045600020212     C                   MOVEL     'T'           W004A             4
045700020212     C                   MOVE      DS1LNP        W004A
045800020212     C                   MOVEL(P)  W004A         KKEY
045900020212     C     KTAB1         CHAIN     TABEL00F                           99
046000020212     C     *IN99         IFEQ      *OFF
046100020212     C                   MOVEL     TBLUNI        DS5P
046200020212     C                   MOVEL     §5POTQ        £OUTQ            10
046300020212     C                   END
046400020212     C*
046500020212     C* MODULO STAMPA
046600020212     C******               MOVE DS1LNP    £MOD
046700020212     C*
046800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046900020212     C                   EXSR      LEGGI
047000020212     C*
047100020212     C                   ENDIF
047200020212     C                   ENDIF
047300020212     C*
047400020212     C                   ELSE
047500020212     C*
047600020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
047700020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
047800020212     C     DS1LNP        IFEQ      0
047900020212     C                   MOVEA     L1            LIN
048000020212     C* ...altrimenti elaboro solo se stessa
048100020212     C                   ELSE
048200020212     C                   MOVEL     DS1LNP        LIN(1)
048300020212     C                   ENDIF
048400020212     C*
048500020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
048600020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
048700020212     C* PRENDO QUELLA PRESENTE IN KPJBA
048800110705     C**+  DS1FGS        IFEQ      SIMFEL
048900020212     C                   MOVEL     KQOTD         £OUTQ
049000030627     C**!!!              MOVEL     KQOTD         £OUTQF
049100110705     C***                ELSE
049200020212     C* STAMPANTE TABULATI
049300110705     C***                MOVEL     '5P'          KCOD
049400110705     C***                MOVEL     'T'           W004A             4
049500110705     C***                MOVE      DS1FGS        W004A
049600110705     C***                MOVEL(P)  W004A         KKEY
049700110705     C***  KTAB1         CHAIN     TABEL00F                           99
049800110705     C***  *IN99         IFEQ      *OFF
049900110705     C***                MOVEL     TBLUNI        DS5P
050000110705     C***                MOVEL     §5POTQ        £OUTQ            10
050100110705     C***                END
050200020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
050300110705     C***                ENDIF
050400020212     C*
050500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
050600020212     C                   EXSR      LEGGI
050700020212     C                   ENDIF
050800020212     C*
050900020212     C                   ENDSR
051000950309     C**************************************************************************
051100950309     C* LETTURA BOLLE PER CREAZIONE STAMPA
051200950309     C**************************************************************************
051300920916     C     LEGGI         BEGSR
051400950309     C*
051500920917     C                   SETOFF                                       09
051600991020     C                   CLEAR                   PAGE
051700950309     C*
051800950309     C* IMPOSTAZIONE STAMPANTE E TIPO MODULO
051900950309     C                   MOVEA     £MOD          CMD(33)
052000950309     C                   MOVEA     £OUTQ         CMD(51)
052100950309     C*
052200920917     C* ESEGUO OVRPRTF
052300950309     C                   Z-ADD     61            LUNG             15 5
052400920917     C                   MOVEL     *BLANKS       COMMAN           80
052500950309     C                   MOVEA     CMD           COMMAN
052600920917     C                   CALL      'QCMDEXC'                            H1
052700920917     C                   PARM                    COMMAN
052800920917     C                   PARM                    LUNG
052900920917     C*
053000950309     C                   Z-ADD     1             C                 3 0
053100920916     C                   OPEN      QSYSPRT
053200920916     C*
053300030912do  1C     LIN(C)        DOWNE     0
053400920916     C                   SETON                                        OF
053500920917     C                   MOVEL     LIN(C)        COMFIL
053600920924     C     COMFIL        CHAIN     AZORG                              31
053700030912if  2C     *IN31         IFEQ      *OFF
053800991020     C                   MOVEL     ORGDES        DESFIL
053900991020     C                   MOVEL     ORGDES        DESLNP
054000030912x   2C                   ELSE
054100991020     C                   MOVEL     *BLANKS       DESFIL           20
054200991020     C                   MOVEL     *BLANKS       DESLNP           20
054300030912e   2C                   ENDIF
054400920916     C*
054500941229     C     KBLP          SETLL     FNBLP000
054600920916     C*
054700991020     C   03COMFIL        READE     FNBLP000                               30
054800991020     C  N03KBLP          READE     FNBLP000                               30
054900920916     C*
055000030912do  2C     *IN30         DOWEQ     '0'
055100920924     C                   SETOFF                                       05
055200900525     C* SE IMMESSO AL E DATA LETTA > VADO A FINE FILE
055300030912if  3C   03BLPDSP        IFGT      DS1AAL
055400920916     C                   SETON                                        30
055500030912e   3C                   END
055600920925     C* TIPO BOLLA
055700950309     C                   MOVEL     '3A'          KCOD
055800950309     C                   MOVEL     *BLANKS       KKEY
055900950309     C                   MOVEL     BLPCBO        KKEY
056000950309     C     KTAB1         CHAIN     TABEL                              32
056100920925     C  N32              MOVEL     TBLUNI        DS3A
056200920916     C*
056300941228     C* AGGANCIO FILE TASSAZIONE BOLLA
056400030912if  3C     *IN30         IFEQ      *OFF
056500991020      *
056600991020     C                   MOVEL     '1'           WTRC
056700991020     C     KBLP3         CHAIN     FIAR6000                           34
056800991020      *
056900991020      * GESTIONE CAMPI TASSAZIONE
057000991020     C                   EXSR      GESTAS
057100040621     c*
057200040621     c   24              exsr      elab2
057300941228     C*
057400991020     C   25              EXSR      ELAB8
057500920917     C*
057600991020     C   26              EXSR      ELABC
057700920917     C*
057800991020     C   27              EXSR      ELABQ
057900920916     C*
058000991020     C   28              EXSR      ELAB5
058100921001     C*
058200991020     C   29              EXSR      ELABP
058300991020      *
058400030912e   3C                   ENDIF
058500921002     C*
058600991020     C  N30
058700991020     CAN 03COMFIL        READE     FNBLP000                               30
058800991020     C  N03KBLP          READE     FNBLP000                               30
058900030912e   2C                   ENDDO
059000030912      *
059100030912      * A rottura di LNP stampo il riepilogo delle spedizioni elencate
059200030912      *   totalizzate per BLPSOP
059300030912     c                   if        *in09 = *on  and  *in29 = *on
059400030912     c                   eval      *inOF = *on
059500030912     c                   exsr      PrtRiepSOP
059600030912     c                   endif
059700920916     C*
059800920916     C                   ADD       1             C
059900030912e   1C                   END
060000920916     C*
060100991020     C                   SELECT
060200991020     C     *IN09         WHENEQ    *ON
060300991020     C     DS1FGS        WHENGT    *ZEROS
060400991020     C                   MOVEL     DS1FGS        COMFIL
060500991020     C     DS1LNP        WHENEQ    SIMFEL
060600991020     C                   MOVEL     DS1LNP        COMFIL
060700991020     C                   ENDSL
060800930209     C*
060900930209     C     COMFIL        CHAIN     AZORG                              31
061000930209     C  N31              MOVEL     ORGDES        DESFIL
061100930209     C   31              MOVEL     *BLANKS       DESFIL
061200920917     C*
061300991020     C  N09
061400991020     CANN29              EXCEPT    TESTA
061500030912     C  N09
061600030912     CAN 29              EXCEPT    TCAP
061700030912     c  n09
061800030912     can 29              except    TCAP2
061900030912      *
062000920916     C                   CLOSE     QSYSPRT
062100950309     C*
062200920916     C                   ENDSR
062300950309     C**************************************************************************
062400991020     C* GESTIONE CAMPI TASSAZIONE
062500950309     C**************************************************************************
062600991020     C     GESTAS        BEGSR
062700991020      *
062800991020     C                   CLEAR                   VARIAI
062900991020     C                   CLEAR                   STADIV
063000991020     C                   CLEAR                   STAPOR
063100991020     C                   CLEAR                   STASV1
063200991020     C                   CLEAR                   STAVA1
063300991020     C                   CLEAR                   STASV2
063400991020     C                   CLEAR                   STAVA2
063500991020     C                   CLEAR                   STAVA3
063600991020     C                   CLEAR                   STAIMV
063700991020     C                   CLEAR                   MEMNFT
063800991020     C                   CLEAR                   MEMDFT
063900991020      *
064000991020     C     *IN34         IFEQ      *OFF
064100030912     C                   Z-ADD     1             XX
064200991020     C                   MOVEL     AR6DIV        STADIV
064300991020     C                   Z-ADD     AR6POR        STAPOR            9 3
064400991020     C                   Z-ADD     AR6IMV        STAIMV           11 3
064500991020     C                   Z-ADD     AR6NFT        MEMNFT
064600991020     C                   Z-ADD     AR6DFT        MEMDFT
064700991020      *  GESTIONE VARIE
064800991020     C                   CLEAR                   STAVA3
064900991020      *
065000991020     C     AR6SV1        IFNE      *BLANKS
065100991020     C                   MOVEL     AR6SV1        WSV1
065200991020     C                   Z-ADD     AR6VA1        WVA1
065300991020     C                   EXSR      MEMVAR
065400991020     C                   ENDIF
065500991020      *
065600991020     C     AR6SV2        IFNE      *BLANKS
065700991020     C                   MOVEL     AR6SV2        WSV1
065800991020     C                   Z-ADD     AR6VA2        WVA1
065900991020     C                   EXSR      MEMVAR
066000991020     C                   ENDIF
066100991020      *
066200991020     C     AR6SV3        IFNE      *BLANKS
066300991020     C                   MOVEL     AR6SV3        WSV1
066400991020     C                   Z-ADD     AR6VA3        WVA1
066500991020     C                   EXSR      MEMVAR
066600991020      *
066700991020     C     KBLP3         CHAIN     FIAR7000                           34
066800991020     C     *IN34         DOWEQ     *OFF
066900991020     C                   MOVEL     AR7SVN        WSV1
067000991020     C                   Z-ADD     AR7VAN        WVA1
067100991020     C                   EXSR      MEMVAR
067200991020     C     KBLP3         READE     FIAR7000                               34
067300991020     C                   ENDDO
067400991020     C                   ENDIF
067500991020      *
067600991020     C                   ENDIF
067700991020      *
067800991020     C                   ENDSR
067900991020     C**************************************************************************
068000991020     C* MEMORIZZO VARIE PER STAMPA
068100991020     C**************************************************************************
068200991020     C     MEMVAR        BEGSR
068300991020      *
068400991020      * MEMORIZZO LA VARIA I - GIACENZE
068500991020     C     WSV1          IFEQ      'I'
068600991020     C                   Z-ADD     WVA1          VARIAI
068700991020     C                   ENDIF
068800991020      *
068900991020      * Non considero IVA e Bollo
069000991020     C                   SELECT
069100991020      *
069200991020     C     WSV1          WHENEQ    §$2IVA
069300991020      *
069400991020     C     WSV1          WHENEQ    §$2BOL
069500991020      *
069600991020     C     XX            WHENEQ    3
069700991020     C                   ADD       WVA1          STAVA3            9 3
069800991020      *
069900991020     C     XX            WHENEQ    2
070000991020     C                   MOVEL     WSV1          STASV2
070100991020     C                   Z-ADD     WVA1          STAVA2            9 3
070200030912     C                   ADD       1             XX
070300991020      *
070400991020     C     XX            WHENEQ    1
070500991020     C                   MOVEL     WSV1          STASV1
070600991020     C                   Z-ADD     WVA1          STAVA1            9 3
070700991020     C                   ADD       1             XX
070800991020      *
070900991020     C                   ENDSL
071000991020      *
071100991020     C                   ENDSR
071200040621     C**************************************************************************
071300040621     C* ELABORO PER STAMPA spedizioni oltre 20 colli
071400040621     C**************************************************************************
071500040621     C     ELAB2         BEGSR
071600040621     C*
071700040621     C* ESCLUDO I RECUPERI
071800040621     C   32              MOVEL     'F1'          §3ATB1
071900040621     C*
072000040621     C     §3ATB1        IFNE      'F3'
072100040621     C     §3ATB1        ANDNE     'A3'
072200040621     C* Spedizioni oltre 20 colli non bancalate
072300040621     c     blpncl        andgt     20
072400040621     c                   if        %subst(blpgva:1:1) <> 'B' and
072500040621     c                             %Subst(blpgva:1:1) <> 'O'
072600040621     C                   EXSR      STAMPA
072700040621     c                   endif
072800040621     C                   END
072900040621     C*
073000040621     C                   ENDSR
073100040621     C*
073200991020     C**************************************************************************
073300991020     C* ELABORO PER STAMPA 8888
073400991020     C**************************************************************************
073500991020     C     ELAB8         BEGSR
073600991020     C*
073700920925     C   32              MOVEL     'F1'          §3ATB1
073800930930     C   32              MOVEL     ' '           §3ARBL
073900900517     C* ESCLUDO CODICI NON 8888
074000900517     C                   MOVE      BLPKSC        COM88             4 0
074100931110    1C     COM88         IFEQ      8888
074200931110     C*
074300931110     C* ESCLUDO I TIPI BOLLA STORNO (XS)
074400931110     C                   MOVE      §3ATB1        FLGDUE            1
074500931110    2C     FLGDUE        IFNE      'S'
074600920916     C*
074700930930     C* ESCLUDO I CONTI SERVIZI (§3ARBL = C)
074800931110    3C     §3ARBL        IFNE      'C'
074900920916     C* ESCLUDO I PREPAGATI
075000991020     C     MEMNFT        ANDEQ     0
075100900517     C                   EXSR      STAMPA
075200931110    3C                   ENDIF
075300931110    2C                   ENDIF
075400931110    1C                   ENDIF
075500900525     C*
075600920916     C                   ENDSR
075700950309     C**************************************************************************
075800950309     C* ELABORO PER STAMPA CONVENUTI
075900950309     C**************************************************************************
076000920917     C     ELABC         BEGSR
076100950309     C*
076200920924     C   32              MOVEL     'A2'          §3ATB1
076300920924     C                   MOVEL     §3ATB1        FLGTB1            1
076400920925     C* ESCLUDO ASSEGNATI
076500920924     C     FLGTB1        IFEQ      'A'
076600920924     C                   GOTO      ENDELC
076700991020     C                   ENDIF
076800920924     C*
076900920924     C* ESCLUDO SE BOLLA GIACENTE
077000920924     C* (VARIA I UGUALE A TOTALE IMPONIBILE)
077100991020     C     VARIAI        IFGT      *ZEROS
077200991020     C     VARIAI        ANDEQ     STAIMV
077300991020     C                   CLEAR                   VARIAI
077400920924     C                   GOTO      ENDELC
077500991020     C                   ENDIF
077600920924     C*
077700920924     C* 05 ON - ESISTE 2° BOLLA
077800991005     C                   MOVEL     '2'           WTRC
077900991005     C     KBLP3         CHAIN     FIAR6000                           32
078000991020     C     *IN32         IFEQ      *OFF
078100991020     C                   SETON                                        05
078200040203S   2c                   Select
078300040203     c                   When      Ar6Sv1 = §$2Vrg
078400991020     C                   Z-ADD     AR6VA1        STA2VA
078500040203     c                   When      Ar6Sv2 = §$2Vrg
078600040203     c                   Z-add     Ar6Va2        Sta2va
078700040203     c                   When      Ar6Sv3 = §$2Vrg
078800040203     C                   Z-Add     Ar6Va3        Sta2va
078900040203   x2c                   Other
079000040203     c     kBlp3         Setll     Fiar701l
079100040203do  3c                   Do        *Hival
079200040203     c     kBlp3         Reade     Fiar701l
079300040203     c                   If        %Eof(Fiar701l)
079400040203     c                   Leave
079500040203     c                   EndIf
079600040203     c                   If        Ar7Svn = §$2Vrg
079700040203     c                   Z-add     Ar7Van        Sta2va
079800040203     c                   EndIf
079900040203    3c                   EndDo
080000040203    2c                   EndSl
080100991020     C                   ENDIF
080200920924     C*
080300920924     C* STAMPO SE C'E' CONVENUTO
080400991020     C     STAPOR        IFNE      *ZEROS
080500991020     C     STAVA1        ORNE      *ZEROS
080600991020     C     STAVA2        ORNE      *ZEROS
080700991020     C     STAVA3        ORNE      *ZEROS
080800991020     C     STAIMV        ORNE      *ZEROS
080900991020     C     *IN05         OREQ      *ON
081000991020     C     STA2VA        ANDNE     *ZEROS
081100920924     C                   EXSR      STAMPA
081200920924     C                   END
081300920924     C*
081400920924     C     ENDELC        ENDSR
081500950309     C**************************************************************************
081600950309     C* ELABORO PER STAMPA OLTRE 35 Q.LI
081700950309     C**************************************************************************
081800920917     C     ELABQ         BEGSR
081900950309     C*
082000920925     C* ESCLUDO I RECUPERI
082100920925     C   32              MOVEL     'F1'          §3ATB1
082200920925     C*
082300920925     C     §3ATB1        IFNE      'F3'
082400920925     C     §3ATB1        ANDNE     'A3'
082500920925     C* PESO OLTRE 35 Q.LI
082600941228     C     BLPPKF        ANDGT     3500
082700920925     C                   EXSR      STAMPA
082800920925     C                   END
082900950309     C*
083000920917     C                   ENDSR
083100950309     C**************************************************************************
083200950309     C* ELABORO PER STAMPA OLTRE MC 5 E KG 500
083300950309     C**************************************************************************
083400921001     C     ELAB5         BEGSR
083500950309     C*
083600921001     C   32              MOVEL     'F1'          §3ATB1
083700921001     C* ESCLUDO RECUPERI
083800921001     C     §3ATB1        IFNE      'F3'
083900921001     C     §3ATB1        ANDNE     'A3'
084000921001     C*
084100921001     C* CONSIDERO SOLO PESO   > 500 KG
084200921001     C*              O VOLUME > 5 MC   (DA VARIABILI IN TABELLA)
084300941228     C     BLPVLF        IFGT      §3IVLF
084400941228     C     BLPPKF        ORGT      §3IPKF
084500921001     C*
084600921001     C* 05 ON - ESISTE 2° BOLLA
084700991005     C                   MOVEL     '2'           WTRC
084800991005     C     KBLP3         CHAIN     FIAR6000                           32
084900991020     C     *IN32         IFEQ      *OFF
085000991020     C                   SETON                                        05
085100040203S   2c                   Select
085200040203     c                   When      Ar6Sv1 = §$2Vrg
085300991020     C                   Z-ADD     AR6VA1        STA2VA
085400040203     c                   When      Ar6Sv2 = §$2Vrg
085500040203     c                   Z-add     Ar6Va2        Sta2va
085600040203     c                   When      Ar6Sv3 = §$2Vrg
085700040203     C                   Z-Add     Ar6Va3        Sta2va
085800040203   x2c                   Other
085900040203     c     kBlp3         Setll     Fiar701l
086000040203do  3c                   Do        *Hival
086100040203     c     kBlp3         Reade     Fiar701l
086200040203     c                   If        %Eof(Fiar701l)
086300040203     c                   Leave
086400040203     c                   EndIf
086500040203     c                   If        Ar7Svn = §$2Vrg
086600040203     c                   Z-add     Ar7Van        Sta2va
086700040203     c                   EndIf
086800040203    3c                   EndDo
086900040203    2c                   EndSl
087000991020     C                   ENDIF
087100921001     C*
087200921001     C                   EXSR      STAMPA
087300921001     C                   END
087400921001     C                   END
087500921001     C*
087600921001     C                   ENDSR
087700950309     C**************************************************************************
087800950309     C* ELABORO PER STAMPA SPE CON CAP GENERICO
087900950309     C**************************************************************************
088000921002     C     ELABP         BEGSR
088100950309     C*
088200921002     C* ESCLUDO CODICI  8888
088300921002     C                   MOVE      BLPKSC        COM88             4 0
088400970708    1C     COM88         IFNE      8888
088500970708     C* RICHIAMO FNLV13R
088600970708     C                   CLEAR                   DSSI95
088700970708     C                   MOVEL     '3'           I95TCN
088800970708     C                   MOVEL     BLPCAD        I95CAP
088900970708     C                   MOVEL     BLPNZD        I95NAR
089000970708     C                   MOVEL     BLPPRD        I95PRV
089100970708     C                   Z-ADD     BLPMGS        I95DAT
089200970708     C                   MOVEL     BLPAAS        I95DAT
089300970708     C                   CALL      'TISI95R'
089400970708     C                   PARM                    DSSI95
089500970708     C* SE CAP GENERICO CON STRADARIO ERRORE E STAMPO
089600970708    2C     O95ERR        IFEQ      ' '
089700970708     C     O95FLG        ANDEQ     'S'
089800921002     C*
089900921002     C* SE CAMBIATO CODICE CLIENTE VADO A PAGINA NUOVA
090000970708    3C     BLPKSC        IFNE      SAVKSC
090100921002     C                   SETON                                        OF
090200921002     C                   Z-ADD     BLPKSC        SAVKSC            7 0
090300970708    3C                   END
090400921002     C* VEDO SE PARTITA LA SPEDIZIONE
090500970708    3C     BLPNFV        IFNE      0
090600921002     C                   MOVEL     'P'           PARTI             1
090700921002     C                   ELSE
090800921002     C                   MOVEL     ' '           PARTI
090900970708    3C                   END
091000921002     C*
091100921002     C                   EXSR      STAMPA
091200970708    2C                   END
091300970708    1C                   END
091400921002     C*
091500921002     C                   ENDSR
091600950309     C**************************************************************************
091700950309     C*    CARICAMENTO TABELLE
091800950309     C**************************************************************************
091900920916     C     CARTAB        BEGSR
092000950309     C*
092100931206     C* DECODIFICO SIMFEL
092200931206     C     SIMFEL        CHAIN     AZORG                              30
092300931206     C  N30              MOVEL     ORGDES        DESSIM           20
092400931206     C   30              MOVEL     *BLANKS       DESSIM
092500991020     C*
092600991020     C* CARICO DS$2 PER DECODIFICA VARIE
092700991020     C                   MOVEL     '$2'          KCOD
092800991020     C                   CLEAR                   KKEY
092900991020     C                   MOVEL     '1'           KKEY
093000991020     C     KTAB1         CHAIN     TABEL                              32
093100991020     C  N32              MOVEL     TBLUNI        DS$2
093200921001     C*
093300921001     C* CARICO DS3I PER OLTRE MC 5
093400921001     C   28              DO
093500950309     C                   MOVEL     '3I'          KCOD
093600950309     C                   MOVEL     *BLANKS       KKEY
093700950309     C                   MOVEL     '1'           KKEY
093800950309     C     KTAB1         CHAIN     TABEL                              32
093900921001     C  N32              MOVEL     TBLUNI        DS3I
094000941228     C   32              Z-ADD     500           §3IPKF
094100941228     C   32              Z-ADD     5             §3IVLF
094200921001     C*
094300941228     C                   Z-ADD     §3IPKF        STAPKF            6 0
094400921001     C                   END
094500950309     C*
094600950309     C     DS1FGS        IFNE      *ZERO
094700950309     C* NON SONO LE STAMPE GIORNALIERE ( DS1FGS<>0 )
094800950309     C     DS1LNP        IFNE      *ZERO
094900950309     C                   SETON                                        22
095000950309     C                   ENDIF
095100950309     C                   ENDIF
095200950309     C*
095300920916     C                   ENDSR
095400950309     C**************************************************************************
095500950309     C*    STAMPA BOLLA
095600950309     C**************************************************************************
095700900517     C     STAMPA        BEGSR
095800950309     C*
095900991020     C                   SETOFF                                       0708
096000991020     C                   SETOFF                                       1923
096100920917     C                   SETON                                        09
096200991020     C                   CLEAR                   TRAT1
096300991020     C                   CLEAR                   TRAT2
096400040203     C                   CLEAR                   STA2VA
096500950121     C*
096600950121     C* DECODIFICA TIPO SERVIZIO
096700950309     C                   MOVEL     '5E'          KCOD
096800950309     C                   MOVEL(P)  BLPTSP        KKEY
096900950309     C     KTAB1         CHAIN     TABEL                              32
097000991020     C     *IN32         IFEQ      *OFF
097100090604     c                   eval      ds5e = tbluni
097200090604     C                   MOVEL     §5ed08        DECTSP           13
097300090604     C                   MOVEL     §5ed03        DECTS2            4
097400991020     C                   ELSE
097500991020     C                   CLEAR                   DECTSP
097600991020     C                   CLEAR                   DECTS2
097700991020     C                   ENDIF
097800950121     C*
097900900517     C* C'E' VALORE MERCE
098000941228     C     BLPIAS        COMP      0                                  0606
098100941227     C*
098200941227     C* CONTROLLO SE C'E' CONTRASSEGNO
098300941228    1C     *IN27         IFEQ      *OFF
098400051115     C     KBLP9         CHAIN     Fiar901L                           33
098500941227     C  N33              SETON                                        07
098600941228    1C                   ENDIF
098700941227     C*
098800920924     C* GIRO DATA SPEDIZIONE
098900941227     C                   MOVE      BLPDSP        DATA
099000941227     C                   Z-ADD     MM            GGMM              4 0
099100941227     C                   MOVEL     GG            GGMM
099200941227     C                   Z-ADD     AA            STADSP
099300941227     C                   MOVEL     GGMM          STADSP
099400941229     C*
099500941229     C* CAMPI DI COMODO PER STAMPA
099600941229     C                   MOVEL     BLPRSM        STARSM           20
099700941229     C                   MOVEL     BLPINM        STAINM           20
099800030912     C                   MOVEL     BLPRSD        STARSD           20
099900941229     C                   MOVEL     BLPLOD        STALOD           16
100000030912     C                   MOVEL     BLPRSD        STARS2           19
100100030912     C                   MOVEL     BLPLOD        STALO2           19
100200941229     C                   MOVEL     BLPLOM        STALOM           20
100300921002     C*
100400941228     C* 29 OFF - NON SI TRATTA DELLA STAMPA SPED CON CAP GENERICO
100500941228    1C     *IN29         IFEQ      '0'
100600941228     C*
100700941229     C* 27 OFF - NON SI TRATTA DELLA STAMPA DELLE SPEDIZ. OLTRE 35 Q.LI
100800941229    2C     *IN27         IFEQ      *OFF
100900941229     C*
101000900517     C* CONTROLLO SE ESISTONO VARIE
101100991020    3C     STASV1        IFNE      *BLANKS
101200991020     C                   MOVEL     '-'           TRAT1             1
101300941228    3C                   END
101400991020    3C     STASV2        IFNE      *BLANKS
101500991020     C                   MOVEL     '-'           TRAT2             1
101600941228    3C                   END
101700941228     C*
101800920924     C* CONTROLLO SE ESISTA DATA FATTURA
101900941228    3C     *IN05         IFEQ      *OFF
102000991020     C     MEMDFT        ANDGT     0
102100920924     C                   SETON                                        08
102200991020     C                   MOVE      MEMDFT        DATA
102300941227     C                   Z-ADD     MM            GGMM
102400941227     C                   MOVEL     GG            GGMM
102500941227     C                   Z-ADD     AA            STADFT
102600941227     C                   MOVEL     GGMM          STADFT
102700941228     C*
102800991020     C                   MOVEL     MEMNFT        STANFT
102900941228    3C                   ENDIF
103000941228     C*
103100941229   X2C                   ELSE
103200941229     C* 27 ON  - STAMPA DELLE SPEDIZIONI OLTRE 35 Q.LI
103300941229     C*
103400920925     C* CONTROLLO SE ESISTONO NOTE
103500941227     C                   MOVEL     '8'           WTRC
103600060216     C     KBLP3         CHAIN     Fiar401L                           33
103700941228    3C     *IN33         IFEQ      *OFF
103800941227     C                   SETON                                        19
103900060216     C                   MOVEL     ar4NOT        STANT1
104000941228    3C                   ENDIF
104100941227     C*
104200941227     C                   MOVEL     '9'           WTRC
104300060216     C     KBLP3         CHAIN     Fiar401L                           33
104400941228    3C     *IN33         IFEQ      *OFF
104500941227     C                   SETON                                        23
104600060216     C                   MOVEL     ar4NOT        STANT2
104700941228    3C                   ENDIF
104800941229     C*
104900941228    2C                   ENDIF
105000920925     C*
105100920924     C* STAMPO TESTATA
105200920924     C   OF              EXCEPT    TESTA
105300920924     C   OF              SETOFF                                       OF
105400900517     C* STAMPA DETTAGLIO
105500900517     C                   EXCEPT    DET
105600941228     C**
105700941228     C**
105800941228   X1C                   ELSE
105900941228     C**
106000941228     C**
106100921002     C* 29 ON - STAMPA SPED CON CAP GENERICO
106200921002     C*
106300921002     C* STAMPO TESTATA
106400030912     c                   if        *inOF
106500030912     c                   except    TCAP
106600030912     c                   except    TCAP2
106700030912     c                   movel     *off          *inOF
106800030912     c                   endif
106900921002     C* STAMPA DETTAGLIO
107000921002     C                   EXCEPT    DCAP
107100030912      *
107200030912      * Incremento contatore delle spedizioni stampate per operatore
107300030912     c                   eval      xx       = 1
107400030912     c                   eval      *in31    = *off
107500030912     c     BLPsop        lookup    $SOP(xx)                               31
107600030912      *   Se operatore non ancora elaborato lo memorizzo
107700030912if  2c                   if        *in31    = *off
107800030912     c     *blanks       lookup    $SOP(xx)                               31
107900030912if  3c                   if        *in31    = *on
108000030912     c                   eval      $SOP(xx) = BLPsop
108100030912e   3c                   endif
108200030912e   2c                   endif
108300030912      *   Incremento il contatore delle spedizioni da lui stampate
108400030912if  2c                   if        *in31    = *on
108500030912     c                   add       1             $SOPcont(xx)
108600030912x   2c                   else
108700030912     c                   add       1             XaltriSOP
108800030912e   2c                   endif
108900030912      *
109000941228    1C                   END
109100921002     C*
109200900517     C                   ENDSR
109300030912      *****************************************************************
109400030912      * Stampa Riepilogo per SOP                                      *
109500030912      *****************************************************************
109600030912     c     PrtRiepSOP    BEGSR
109700030915      *
109800030915      * Ordino i dati per sigla operatore in modo discendente
109900030915     c                   eval      $SOPsave  = $SOP
110000030915     c                   eval      $SOPsaveC = $SOPcont
110100030915     c                   clear                   $SOPcont
110200030915     c                   sorta     $SOP
110300030915     c                   eval      wa        = 1
110400030915do  1c                   DOW       wa       <= %elem($SOP)
110500030915if  2c                   if        $SOP(wa)  = *blanks
110600030915     c                   leave
110700030915e   2c                   endif
110800030915     c                   eval      wc        = 1
110900030915     c                   eval      *inOF     = *off
111000030915     c     $SOP(wa)      lookup    $SOPsave(wc)                           31
111100030915if  2c                   if        *in31
111200030915     c                   eval      $SOPcont(wa) = $SOPsaveC(wc)
111300030915e   2c                   endif
111400030915     c                   add       1             wa
111500030915e   1c                   ENDDO
111600030912      *
111700030912      * Stampo testata
111800030912     c                   except    TCAP
111900030912     c                   eval      *inOF     = *off
112000030912      *
112100030912      * Stampo riepilogo per operatore
112200030912     c                   eval      xx        = 1
112300030912do  1c                   DOW       xx       <= %elem($SOP)
112400030912      *
112500030912if  2c                   if        $Sop(xx)  = *blanks
112600030912     c                   leave
112700030912e   2c                   endif
112800030912      *
112900030912if  2c                   if        xx        = 1
113000030912     c                   except    TXRsop
113100030912e   2c                   endif
113200030912     c                   except    RIEsop
113300030912      *
113400030912     c                   add       1             xx
113500030912e   1c                   ENDDO
113600030912      *
113700030912if  1c                   if        XaltriSOP > *zeros
113800030912     c                   except    RIEsop2
113900030912e   1c                   endif
114000030912      *
114100030912      * Azzero i contatori
114200030912     c                   clear                   $SOP
114300030912     c                   clear                   $SOPcont
114400030912     c                   clear                   XaltriSOP
114500030912      *
114600030912     c                   ENDSR
114700950309     C**************************************************************************
114800040621     O* SPED 8888 E CONVENUTI E oltre 20 colli
114900040621     OQSYSPRT   E      N27N28TESTA            02
115000920924     O               25      SK(1)               66
115100920924     O               26      SK(6)               66
115200040621     O               24      SK(28)              66
115300920925     O              N03      SK(2)              132
115400920925     O               03      SK(7)              132
115500900517     O                       RSUT                20
115600950309     O                       DS1GDA              81 '  /  /    '
115700950309     O               03      DS1GAL              95 '  /  /    '
115800950314     O                       WDATE              118 '  /  /    '
115900920917     O                       PAGE          Z    132
116000920925     O* OLTRE 35 Q.LI
116100920925     O          E       27   TESTA            02
116200920925     O                       SK(9)               66
116300920925     O              N03      SK(10)             132
116400920925     O               03      SK(11)             132
116500900524     O                       RSUT                20
116600950309     O                       DS1GDA              86 '  /  /    '
116700950309     O               03      DS1GAL             100 '  /  /    '
116800950314     O                       WDATE              118 '  /  /    '
116900920917     O                       PAGE          Z    132
117000921001     O* OLTRE 5 MC E 500 KG
117100921001     O          E       28   TESTA            02
117200921001     O                       SK(13)              66
117300921001     O              N03      SK(14)             132
117400921001     O               03      SK(15)             132
117500921001     O                       RSUT                20
117600941228     O                       STAPKF        Z     65
117700941228     O                       §3IVLF        4     88
117800950309     O                       DS1GDA             103 '  /  /    '
117900950309     O               03      DS1GAL             118 '  /  /    '
118000950314     O              N03      WDATE              118 '  /  /    '
118100921001     O                       PAGE          Z    132
118200920917     O          E            TESTA            03
118300950313     O                       KNSIF               10
118400950313     O                       KNMUS               22
118500950313     O                       SK(25)             107
118600950313     O              N11N14N22                    73 'TUTTE'
118700950313     O                     22DESLNP              88
118800950313     O                     11DESSIM              88
118900950313     O                     14DESLNP              88
119000950313     O                       UTIME              117 '0 :  :  '
119100931206     O          E            TESTA            05
119200920924     O                       SK(8)               66
119300920924     O                       COMFIL              23
119400920924     O                                           25 '-'
119500931206     O                       DESFIL              47
119600030912     O          E            TESTA            07
119700900517     O                       SK(3)               66
119800920925     O                 N27   SK(4)              132
119900950106     O                  27                       66 ' '
120000920925     O                  27   SK(12)             132
120100030912     O          E            TESTA            08
120200900517     O                       SK(5)               66
120300900517     O                       SK(5)              132
120400950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
120500900517     O          E            DET         1  1
120600920924     O                       BLPNRS        Z
120700900517     O                       BLPNSP        Z   +  1
120800900524     O                       BLPLNA            +  1
120900900517     O                       STARSM            +  1
121000900517     O                       STARSD            +  1
121100941228     O                       BLPNCL        Z   +  3
121200991020     O              N27      STADIV            +  1
121300991020     O              N27      STAPOR        4   +  2
121400991020     O              N27      STASV1            +  2
121500991020     O              N27      TRAT1             +  0
121600991020     O              N27      STAVA1        4   +  0
121700991020     O              N27      STASV2            +  2
121800991020     O              N27      TRAT2             +  0
121900991020     O              N27      STAVA2        4   +  0
122000991020     O              N27      STAVA3        4   +  2
122100991020     O              N27      STAIMV        4    132
122200991020     O               27      STAIMV        4     78
122300900517     O          E            DET            1
122400941228     O                       STADSP              10 '  /  /    '
122500920924     O                       STAINM            +  5
122600900517     O                       STALOD            +  1
122700900517     O                       BLPPRD            +  1
122800941228     O                       BLPPKF            +  1 '      , '
122900950106     O                                         +  1 'COD.BOLLA'
123000941230     O                       BLPCBO            +  1
123100950106     O              N27    06                  +  2 'IMP.DA ASS:'
123200950106     O              N27    06BLPIAS        4   +  0
123300941229     O              N27    06BLPVAS            +  1
123400950106     O              N27    07                  +  2 'C/A:'
123500051115     O              N27    07ar9CAS        4   +  0
123600051115     O              N27    07ar9VCA            +  1
123700900517     O          E            DET            1
123800950121     O                       DECTSP              13
123900950121     O                       STALOM            +  2
124000900517     O                       BLPNAS            +  1
124100941228     O                       BLPFVF            +  3
124200950106     O                       BLPVLF        4   +  3
124300950106     O                  05                     +  1 'ASSEGNATO: VARIA G-'
124400991020     O                  05   STA2VA        4   +  0
124500920924     O                  08                     +  2 'FATTURA N.'
124600920924     O                  08   STANFT        Z   +  0
124700920924     O                  08                     +  1 'DEL'
124800941229     O                  08   STADFT            +  1 '  /  /    '
124900920925     O          E       19   DET            1
125000941230     O         OR    23
125100920925     O                                         + 15 'DISPOSIZIONI:'
125200941229     O                     19STANT1            +  1
125300950106     O                     23STANT2            +  3
125400950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
125500921002     O* SPED CON CAP GENERICO
125600921002     O          E            TCAP             02
125700921002     O                       SK(16)              66
125800921002     O              N03      SK(18)             132
125900921002     O               03      SK(17)             132
126000921002     O                       RSUT                20
126100950309     O                       DS1GDA              91 '  /  /    '
126200950309     O               03      DS1GAL             105 '  /  /    '
126300950314     O                       WDATE              118 '  /  /    '
126400921002     O                       PAGE          Z    132
126500921002     O          E            TCAP             03
126600950313     O                       KNSIF               10
126700950313     O                       KNMUS               22
126800950313     O                       SK(25)             107
126900950313     O              N11N14N22                    73 'TUTTE'
127000950313     O                     22DESLNP              88
127100950313     O                     11DESSIM              88
127200950313     O                     14DESLNP              88
127300950313     O                       UTIME              117 '0 :  :  '
127400931206     O          E            TCAP             05
127500921002     O                       SK(8)               66
127600921002     O                       COMFIL              23
127700921002     O                                           25 '-'
127800931206     O                       DESFIL              47
127900030912     O          E            TCAP2            07
128000921002     O                       SK(19)              66
128100921002     O                       SK(22)             132
128200030912     O          E            TCAP2            08
128300921002     O                       SK(20)              66
128400921002     O                       SK(23)             132
128500030912     O          E            TCAP2          1 09
128600921002     O                       SK(5)               66
128700921002     O                       SK(5)              132
128800950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
128900921002     O          E            DCAP           1
129000921002     O                       BLPNRS        Z
129100921002     O                       BLPNSP        Z   +  1
129200970602     O                       STADSP            +  1 '  /  /    '
129300970602     O                       BLPLNA            +  1
129400970602     O                       BLPKSC            +  1
129500921002     O                                         +  0 '-'
129600941229     O                       STARSM            +  0
129700970602     O                       BLPRMN        Z   +  1
129800030912     O                       STARS2            +  1
129900970602     O                       STALO2            +  1
130000970602     O                       BLPPRD            +  1
130100970602     O                       BLPCAD            +  1
130200970602     O                       DECTS2            +  1
130300970602     O                       PARTI             +  1
130400030912     O                       BLPsop            +  1
130500030912     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
130600030912     O          e            TXRsop      2
130700030912     O                       SK(26)
130800030912     O          e            TXRsop      1  1
130900030912     O                       SK(27)
131000030912     O          e            RIEsop      1
131100030912     O                       $SOP(xx)            40
131200030912     O                       $SOPcont(xx)  1     62
131300030912     O          e            RIEsop2     1
131400030912     O                                           38 'Altri'
131500030912     O                       XaltriSOP     1     62
131600950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
131700931206**
131800950309OVRPRTF FILE(QSYSPRT) FORMTYPE('          ') OUTQ(          )
131900900517**
132000050802XXXXXXXXXXXXXXXXXXXX                     ** ELENCO SPEDIZIONI 8888   1
132100950313 DEL XX/XX/XXXX **            FNLS10R     XX.XX.XXXX     PAG. XXXX   2
132200950106SPEDIZIONE LNA MITTENTE             DESTINATARIO           C/P/V D   3
132300991020IV  TRASPORTO      VARIE        VARIE        VARIE        IMPONIB.   4
132400920924------------------------------------------------------------------   5
132500051115XXXXXXXXXXXXXXXXXXXX            ** ELENCO SPEDIZIONI CON CONVENUTI   6
132600941223 DAL XX/XX/XXXX AL XX/XX/XXXX ** FNLS10R  XX.XX.XXXX     PAG. XXXX   7
132700931206LINEA DI PARTENZA : XXX    XXXXXXXXXXXXXXXXXXXX                      8
132800051115XXXXXXXXXXXXXXXXXXXX        ** ELENCO SPEDIZIONI CON PESO OLTRE 35   9
132900941228 Q.LI DEL XX/XX/XXXX **  FNLS10R          XX.XX.XXXX      PAG.XXXX  10
133000941228 Q.LI DAL XX/XX/XXXX AL XX/XX/XXXX **     XX.XX.XXXX LS10 PAG.XXXX  11
133100920925IMPONIBILE   COSTO PART. DIFF.NOLO PART. DIFF.NOLO ARR. COSTO ARR.  12
133200051115XXXXXXXXXXXXXXXXXXXX        * SPEDIZIONI CON PESO OLTRE KG.XXXXXX   13
133300941228O VOL. OLTRE MC.XX,XXX DEL XX/XX/XXXX *   XX.XX.XXXX LS10 PAG.XXXX  14
133400941228O VOL. OLTRE MC.XX,XXX DAL XX/XX/XXXX  AL XX/XX/XXXX LS10 PAG.XXXX  15
133500051115XXXXXXXXXXXXXXXXXXXX     *ELENCO SPEDIZ. CON C.A.P. CHE PREVEDE LO  16
133600941229 STRADARIO DAL XX/XX/XXXX AL XX/XX/XXXX * XX.XX.XXXX LS10 PAG.XXXX  17
133700941229 STRADARIO DEL XX/XX/XXXX *               XX.XX.XXXX LS10 PAG.XXXX  18
133800970602S P E D I Z I O N E  |LIN|           M  I  T  T  E  N  T  E         19
133900970602SER NUMERO    DATA   |ARR|CODICE   RAGIONE   SOCIALE    RIFER.NUME  20
134000970602XX XXXXXXX XX/XX/XXXX XXX XXXXXXX-XXXXXXXXXXXXXXXXXXXX 12345678901  21
134100030912    |        D  E  S  T  I  N  A  T  A  R  I  O          |      |S  22
134200030912RICO|RAGIONE SOCIALE     LOCALITA'           PR CAP      |      |O  23
1343000309122345 1234567890123456789 1234567890123456789 XX 123456789 1234 P x  24
134400931206LINEE PARTENZA RICHIESTE:                                           25
134500030912RIEPILOGO PER OPERATORE:        SIGLA OPERATORE       SPEDIZIONI    26
134600030912                                ---------------       ----------    27
134700040621XXXXXXXXXXXXXXXXXXXX           ** ELENCO SPEDIZIONI OLTRE 20 COLLI  28
