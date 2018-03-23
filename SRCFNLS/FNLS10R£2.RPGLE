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
005100900613     D DS3A          E DS
005200921001     D DS3I          E DS
005300991020     D DS$2          E DS
005400920916     D DS3L          E DS
005500970707     D DS5P          E DS
005600900613     D KPJBA         E DS
005700910807     D CNCR80        E DS
005800970708     D* CONTROLLO DI UN CAP
005900970708     D DSSI95        E DS                  EXTNAME(TISI95DS)
006000970708     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100950309     D* DS PER TRUL06R - CARICAMENTO £X
006200950309     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006300950314     D  T06                    1     90  0
006400950314     D                                     DIM(30)
006500000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
006600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
006700000000     C*---------------------------------------------------------------*
006800920916     C*    RIEPILOGO INDICATORI
006900920916     C*---------------------------------------------------------------*
007000920916     C* 03    - IMMESSA DATA AL
007100920924     C* 05    - ESISTE SECONDA BOLLA
007200920917     C* 06    - VAL MERCE
007300920917     C* 07    - CONTRASSEGNO
007400920924     C* 08    - ESISTE NUMERO E DATA FATTURA
007500920917     C* 09    - ESEGUITA LA STAMPA DI ALMENO UNA BOLLA
007600941227     C* 19/23 - ESISTONO DISPOSIZIONI IN BOLLA
007700931206     C* 22    - PFIL >< 0   E   LNP >< 0
007800040621     c* 24    - Spedizioni oltre 20 colli
007900920917     C* 25    - SPEDIZIONI 8888
008000920917     C* 26    - SPEDIZIONI CON CONVENUTO
008100920917     C* 27    - SPEDIZIONI OLTRE 35 Q.LI
008200921001     C* 28    - SPEDIZIONI OLTRE KG 500 E MC 5
008300921002     C* 29    - CON CAP GENERICO
008400920917     C* 30/32 - DI COMODO
008500920916     C*---------------------------------------------------------------*
008600000000     C     *ENTRY        PLIST
008700000000     C                   PARM                    KPJBA
008800950309     C                   MOVEL     KPJBU         DSLS01
008900950309     C*
009000950309     C                   MOVEL     DS1ADA        AASP              4 0
009100950309     C                   MOVE      DS1ADA        MGSP              4 0
009200950309     C*
009300000000     C                   Z-ADD     1             CODUT
009400950309     C                   CALL      'X§PARUT'
009500950309     C                   PARM                    UT§DSE
009600950309     C                   MOVEL     RAGUT         RSUT             20
009700950309     C                   MOVEL     REC80         CNCR80
009800900511     C*---------------------------------------------------------------*
009900030912     C* ACCESSO FNBLP44L
010000030912     C     KBLP          KLIST
010100030912     C                   KFLD                    COMFIL            3 0
010200030912     C                   KFLD                    AASP
010300030912     C                   KFLD                    MGSP
010400051115     C* ACCESSO Fiar901L
010500991020     C     KBLP9         KLIST
010600920924     C                   KFLD                    BLPAAS
010700920924     C                   KFLD                    BLPLNP
010800920924     C                   KFLD                    BLPNRS
010900920924     C                   KFLD                    BLPNSP
011000060216     C* ACCESSO Fiar401L/FIAR601L/FIAR701L
011100941227     C     KBLP3         KLIST
011200941227     C                   KFLD                    BLPAAS
011300941227     C                   KFLD                    BLPLNP
011400941227     C                   KFLD                    BLPNRS
011500941227     C                   KFLD                    BLPNSP
011600941227     C                   KFLD                    WTRC
011700941223     C*
011800941223     C* DEFINIZIONE CAMPI
011900991020     C     *LIKE         DEFINE    AR6VA1        STA2VA
012000991020     C     *LIKE         DEFINE    AR6VA1        VARIAI
012100991020     C     *LIKE         DEFINE    AR6SV1        WSV1
012200991020     C     *LIKE         DEFINE    AR6VA1        WVA1
012300991020     C     *LIKE         DEFINE    AR6DIV        STADIV
012400991020     C     *LIKE         DEFINE    AR6SV1        STASV1
012500991020     C     *LIKE         DEFINE    AR6SV2        STASV2
012600991020     C     *LIKE         DEFINE    AR6NFT        MEMNFT
012700991020     C     *LIKE         DEFINE    AR6DFT        MEMDFT
012800991020     C     *LIKE         DEFINE    BLPDSP        STADSP
012900991005     C     *LIKE         DEFINE    AR6DFT        STADFT
013000991005     C     *LIKE         DEFINE    AR6NFT        STANFT
013100060216     C     *LIKE         DEFINE    ar4TRC        WTRC
013200060216     C     *LIKE         DEFINE    ar4NOT        STANT1
013300060216     C     *LIKE         DEFINE    ar4NOT        STANT2
013400941223     C*---------------------------------------------------------------*
013500920916     C*
013600920916     C* CONTROLLO SE IMMESSA DATA SPEDIZIONE AL
013700950309     C     DS1AAL        IFNE      0
013800920916     C                   SETON                                        03
013900920916     C                   END
014000920917     C*
014100920917     C* CONTROLLO IL TIPO DI STAMPA RICHIESTO
014200991020     C                   SELECT
014300991020     C     DS1FPG        WHENEQ    '8'
014400991020     C                   SETON                                        25
014500991020     C     DS1FPG        WHENEQ    'Q'
014600991020     C                   SETON                                        27
014700991020     C     DS1FPG        WHENEQ    'P'
014800991020     C                   SETON                                        29
014900991020     C                   OTHER
015000991020     C     DS1FPG        COMP      'C'                                    26
015100991020     C     DS1FPG        COMP      '5'                                    28
015200040621     C     DS1FPG        COMP      '2'                                    24
015300991020     C                   ENDSL
015400030912      *
015500941223     C* CARICAMENTO TABELLE
015600920928     C                   EXSR      CARTAB
015700920928     C*
015800950309      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
015900950309     C                   MOVEL     'P'           DS1TBO
016000950309     C                   CLEAR                   DS1PGM
016100950309     C                   MOVEL     'FNLS10R'     DS1PGM
016200950309     C                   EXSR      COMUNE
016300920916     C*
016400970708     C     *IN29         IFEQ      *ON
016500970714     C                   CLEAR                   DSSI95
016600970708     C                   MOVEL     'C'           I95TLA
016700970708     C                   CALL      'TISI95R'
016800970708     C                   PARM                    DSSI95
016900970708     C                   ENDIF
017000970708     C**
017100920916     C                   SETON                                        LR
017200020212     C*****************************************************************
017300020212     C*  PARTE COMUNE STAMPE GIORNALIERE
017400020212     C*
017500020212     C*  . REPERIMENTO 'SIMFEL'
017600020212     C*  . CARICAMENTO '£1'
017700020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
017800020212     C*  . CODA DI STAMPA ('£OUTQ')
017900020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
018000020212     C*  . MODULO PER LA STAMPA ('£MOD')
018100020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
018200020212     C*****************************************************************
018300020212     C     COMUNE        BEGSR
018400020212     C*
018500020212     C*****************************************************************
018600020212     C*       RIEPILOGO INDICATORI:
018700020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
018800020212     C* 11    -           STAMPA PER 1L
018900020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
019000020212     C*****************************************************************
019100020212     C                   Z-ADD     1             CODUT
019200020212     C*
019300020212     C* Reperimento parametri aziendali
019400020212     C                   CALL      'X§PARUT'
019500020212     C                   PARM                    UT§DSE
019600020212     C                   MOVEL     RAGUT         RSUT             20
019700020212     C                   MOVEL     REC80         CNCR80
019800020212     C*
019900020212     C* Chiave di accesso per il file tabelle
020000020212     C     KTAB1         KLIST
020100020212     C                   KFLD                    CODUT
020200020212     C                   KFLD                    KCOD
020300020212     C                   KFLD                    KKEY
020400020212     C     KTAB2         KLIST
020500020212     C                   KFLD                    CODUT
020600020212     C                   KFLD                    KCOD
020700020212     C*
020800020212     C     *LIKE         DEFINE    TBLCOD        KCOD
020900020212     C     *LIKE         DEFINE    TBLKEY        KKEY
021000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
021100020212     C*
021200020212     C* DETERMINO DATA E ORA DI ESECUZIONE
021300020212     C                   TIME                    WTIME            14 0
021400020212     C                   MOVE      WTIME         WDATE             8 0
021500020212     C                   MOVEL     WTIME         UTIME             6 0
021600020212     C*
021700020418     C* CARICO '£1'
021800020212     C                   CLEAR                   KPJBU
021900020212     C                   CLEAR                   DSUL06
022000020212     C                   MOVE      '£1'          D06COD
022100020212     C                   MOVEL     SIMFEL        D06KEY
022200050802     C                   MOVE      'S'           D06KEY
022300050802     C                   MOVEL     ds1ada        D06drf
022400020212     C                   MOVEL     DSUL06        KPJBU
022500020212     C                   CALL      'TRUL06R'
022600020212     C                   PARM                    KPJBA
022700020212     C                   MOVEL     KPJBU         DSUL06
022800020418     c                   movea     t06           sav_t06
022900020418
023000020418      * se in arrivo carico la £6
023100020418 b1  c                   if        ds1tbo = 'A'
023200020418     c                   clear                   wc
023300020418 b2  c                   do        30            wa
023400020422     c                   if        sav_t06(wa) = *zeros
023500020422     c                   leave
023600020422     c                   endif
023700020418     c                   clear                   kpjbu
023800020418     c                   clear                   dsul06
023900020418     c                   eval      d06cod = '£6'
024000020418     c                   movel     sav_t06(wa)   d06key
024100050802     C                   MOVE      'S'           D06KEY
024200020418     c                   eval      d06esc = 'S'
024300050802     C                   MOVEL     ds1ada        D06drf
024400020419     c                   movel     dsul06        kpjbu
024500020418     c                   call      'TRUL06R'
024600020418     c                   parm                    kpjba
024700020418     c                   movel     kpjbu         dsul06
024800020418 b3  c                   do        30            wb
024900020418     c                   if        t06(wb) = *zeros
025000020418     c                   leave
025100020418     c                   endif
025200020418     c                   add       1             wc
025300020418     c                   z-add     t06(wb)       l6(wc)
025400020418 e3  c                   enddo
025500020418 e2  c                   enddo
025600020212     C*
025700020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
025800020212     C                   Z-ADD     *ZERO         WB
025900020418 b2  C                   DO        29            WA
026000020418     C     sav_T06(WA)   LOOKUP    L6                                     30
026100020212     C  N30              ADD       1             WB
026200020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
026300020418 e2  C                   ENDDO
026400020418 x1  C                   ELSE
026500020418     C                   MOVEA     sav_T06       L1
026600020418 e1  C                   ENDIF
026700020212     C*
026800020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
026900020212     C                   MOVEA     L1            LIN
027000020212     C                   CLEAR                   L1
027100030915     C                   Z-ADD     *ZERO         WB
027200030915     C     1             DO        30            WPT
027300020212     C     LIN(WPT)      IFNE      0
027400020212     C                   CLEAR                   FNLSC4
027500020212     C                   Z-ADD     LIN(WPT)      IC4LIN
027600020212     C                   MOVE      'B'           IC4FLG
027700020212     C                   MOVEL     FNLSC4        KPJBU
027800020212     C                   CALL      'FNLSC4R1'
027900020212     C                   PARM                    KPJBA
028000050802     C                   PARM                    ds1ada
028100020212     C                   MOVEL     KPJBU         FNLSC4
028200020212     C     OC4ESE        IFEQ      'S'
028300020212     C                   ADD       1             WB
028400020212     C                   MOVE      LIN(WPT)      L1(WB)
028500020212     C                   ENDIF
028600020212     C                   ENDIF
028700020212     C                   ENDDO
028800070103     C*
028900070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
029000070103     C                   Z-ADD     *ZERO         WB
029100070103     C                   DO        29            WA
029200070103     C     L1(WA)        IFNE      *ZEROS
029300070103     C     L1(WA)        ANDNE     SIMFEL
029400070103     C                   ADD       1             WB
029500070103     C                   Z-ADD     L1(WA)        LV2(WB)
029600070103     C                   ENDIF
029700070103     C                   ENDDO
029800020212     C*
029900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
030000020212     C*
030100020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
030200020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
030300020212     C* la stampa e' lanciata da menu
030400020212     C                   MOVEL     '3L'          KCOD
030500020212     C                   MOVEL     DS1PGM        KKEY
030600020212     C                   MOVE      DS1FPG        KKEY
030700020212     C     KTAB1         CHAIN     TABEL00F                           30
030800020212     C  N30              MOVEL     TBLUNI        DS3L
030900020212     C   30              MOVEL     'F'           §3LD1L
031000020212     C   30              MOVEL     ' '           §3LD2L
031100020212     C   30              MOVEL     '*STD    '    §3LDST
031200020212     C*
031300020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
031400020212     C                   MOVEL     §3LDST        £MOD             10
031500020212     C                   CLEAR                   LIN
031600020212     C*
031700020212     C* Verifico chi ha lanciato la stampa
031800020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
031900020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
032000020212     C* DS1FGS<>0  --->  si proviene da menu
032100020212     C     DS1FGS        IFEQ      0
032200020212     C*
032300020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
032400020212     C* lancio, voglio la stampa;  invece
032500020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
032600020212     C*
032700020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
032800020212     C* creare stampa per 1°lvl. (se stessa)
032900020212     C     DS1LNP        IFEQ      SIMFEL
033000020212     C*
033100020212     C* Il flag §3LD1L ha i seguenti significati:
033200020212     C*    ' ' = tutti i dati
033300020212     C*    '1' = solo filiale di 1' livello
033400020212     C     §3LD1L        IFEQ      '1'
033500020212     C                   SETON                                        11
033600020212     C                   END
033700020212     C*
033800020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
033900020212     C* nella stampa
034000020212     C*
034100020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
034200020212     C  N11              MOVEA     L1            LIN
034300020212     C   11              MOVEA     SIMFEL        LIN(1)
034400020212     C*
034500020212     C* IMPOSTO LA STAMPANTE:
034600020212     C* STAMPANTE TABULATI
034700020212     C                   MOVEL     '5P'          KCOD
034800020212     C                   MOVEL     'T'           W004A             4
034900020212     C                   MOVE      LIN(1)        W004A
035000020212     C                   MOVEL(P)  W004A         KKEY
035100020212     C     KTAB1         CHAIN     TABEL00F                           99
035200020212     C     *IN99         IFEQ      *OFF
035300020212     C                   MOVEL     TBLUNI        DS5P
035400020212     C                   MOVEL     §5POTQ        £OUTQ            10
035500020212     C                   END
035600020212     C*
035700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
035800020212     C                   EXSR      LEGGI
035900020212     C*
036000020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
036100020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
036200020212     C                   ELSE
036300020212     C*
036400020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
036500020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
036600020212     C*
036700020212     C* Il significato del fla §3LD2L e' il seguente:
036800020212     C*    ' ' = tutti i dati
036900020212     C*    'F' = per ogni 2' livello
037000020212     C     §3LD2L        IFEQ      'F'
037100020212     C                   SETON                                        14
037200020212     C                   ENDIF
037300020212     C*
037400020212     C                   Z-ADD     1             WB
037500020212     C                   MOVEA     *ZEROS        LIN
037600020212     C  N14              MOVEA     L1            LIN
037700020212     C*
037800020212     C* Lancio per tutti i 2° lvl
037900020212     C     DS1LNP        IFEQ      0
038000020212     C*
038100020212     C* LV2 contiene tutte le filiali di 2° lvl
038200020212     C     LV2(WB)       DOWNE     0
038300020212      *
038400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
038500020212     C                   CLEAR                   FNLSC4
038600020212     C                   Z-ADD     LV2(WB)       IC4LIN
038700020212     C                   MOVEL     FNLSC4        KPJBU
038800020212     C                   CALL      'FNLSC4R1'
038900020212     C                   PARM                    KPJBA
039000050802     C                   PARM                    ds1ada
039100020212     C                   MOVEL     KPJBU         FNLSC4
039200020212      *
039300020212     C     OC4PT1        IFEQ      'S'
039400020212     C     §3LPT         ANDEQ     'N'
039500020212     C                   GOTO      NOLEGG
039600020212     C                   ENDIF
039700020212      *
039800020212     C     OC4EXP        IFEQ      'S'
039900020212     C     §3LEXP        ANDEQ     'N'
040000020212     C                   GOTO      NOLEGG
040100020212     C                   ENDIF
040200020212      *
040300020212     C     OC4DPD        IFEQ      'S'
040400020212     C     §3LDPD        ANDEQ     'N'
040500020212     C                   GOTO      NOLEGG
040600020212     C                   ENDIF
040700020212      *
040800020212     C     OC4BAR        IFEQ      'S'
040900020212     C     §3LBAR        ANDEQ     'N'
041000020212     C                   GOTO      NOLEGG
041100020212     C                   ENDIF
041200020212      *
041300020212     C     OC4FED        IFEQ      'S'
041400020212     C     §3LFED        ANDEQ     'N'
041500020212     C                   GOTO      NOLEGG
041600020212     C                   ENDIF
041700020212      *
041800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
041900020212     C*  dati degli altri 2° lvl
042000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
042100020212     C   14              MOVEL     LV2(WB)       LIN(1)
042200020212     C*
042300020212     C* IMPOSTO LA STAMPANTE
042400020212     C* STAMPANTE TABULATI
042500020212     C                   MOVEL     '5P'          KCOD
042600020212     C                   MOVEL     'T'           W004A             4
042700020212     C                   MOVE      LV2(WB)       W004A
042800020212     C                   MOVEL(P)  W004A         KKEY
042900020212     C     KTAB1         CHAIN     TABEL00F                           99
043000020212     C     *IN99         IFEQ      *OFF
043100020212     C                   MOVEL     TBLUNI        DS5P
043200020212     C                   MOVEL     §5POTQ        £OUTQ            10
043300020212     C                   END
043400020212     C*
043500020212     C* Compongo il modulo di stampa con il codice della filiale
043600020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
043700020212     C*
043800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
043900020212     C                   EXSR      LEGGI
044000020212     C*
044100020212     C     NOLEGG        TAG
044200020212     C*
044300020212     C                   ADD       1             WB
044400020212     C                   ENDDO
044500020212     C*
044600020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
044700020212     C                   ELSE
044800020212     C*
044900020212     C* Lancio solo per la 2° lvl richiesta
045000020212     C   14              MOVEL     DS1LNP        LIN(1)
045100020212     C*
045200020212     C* STAMPANTE
045300020212     C* STAMPANTE TABULATI
045400020212     C                   MOVEL     '5P'          KCOD
045500020212     C                   MOVEL     'T'           W004A             4
045600020212     C                   MOVE      DS1LNP        W004A
045700020212     C                   MOVEL(P)  W004A         KKEY
045800020212     C     KTAB1         CHAIN     TABEL00F                           99
045900020212     C     *IN99         IFEQ      *OFF
046000020212     C                   MOVEL     TBLUNI        DS5P
046100020212     C                   MOVEL     §5POTQ        £OUTQ            10
046200020212     C                   END
046300020212     C*
046400020212     C* MODULO STAMPA
046500020212     C******               MOVE DS1LNP    £MOD
046600020212     C*
046700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046800020212     C                   EXSR      LEGGI
046900020212     C*
047000020212     C                   ENDIF
047100020212     C                   ENDIF
047200020212     C*
047300020212     C                   ELSE
047400020212     C*
047500020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
047600020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
047700020212     C     DS1LNP        IFEQ      0
047800020212     C                   MOVEA     L1            LIN
047900020212     C* ...altrimenti elaboro solo se stessa
048000020212     C                   ELSE
048100020212     C                   MOVEL     DS1LNP        LIN(1)
048200020212     C                   ENDIF
048300020212     C*
048400020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
048500020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
048600020212     C* PRENDO QUELLA PRESENTE IN KPJBA
048700020212     C     DS1FGS        IFEQ      SIMFEL
048800020212     C                   MOVEL     KQOTD         £OUTQ
048900030627     C**!!!              MOVEL     KQOTD         £OUTQF
049000020212     C                   ELSE
049100020212     C* STAMPANTE TABULATI
049200020212     C                   MOVEL     '5P'          KCOD
049300020212     C                   MOVEL     'T'           W004A             4
049400020212     C                   MOVE      DS1FGS        W004A
049500020212     C                   MOVEL(P)  W004A         KKEY
049600020212     C     KTAB1         CHAIN     TABEL00F                           99
049700020212     C     *IN99         IFEQ      *OFF
049800020212     C                   MOVEL     TBLUNI        DS5P
049900020212     C                   MOVEL     §5POTQ        £OUTQ            10
050000020212     C                   END
050100020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
050200020212     C                   ENDIF
050300020212     C*
050400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
050500020212     C                   EXSR      LEGGI
050600020212     C                   ENDIF
050700020212     C*
050800020212     C                   ENDSR
050900950309     C**************************************************************************
051000950309     C* LETTURA BOLLE PER CREAZIONE STAMPA
051100950309     C**************************************************************************
051200920916     C     LEGGI         BEGSR
051300950309     C*
051400920917     C                   SETOFF                                       09
051500991020     C                   CLEAR                   PAGE
051600950309     C*
051700950309     C* IMPOSTAZIONE STAMPANTE E TIPO MODULO
051800950309     C                   MOVEA     £MOD          CMD(33)
051900950309     C                   MOVEA     £OUTQ         CMD(51)
052000950309     C*
052100920917     C* ESEGUO OVRPRTF
052200950309     C                   Z-ADD     61            LUNG             15 5
052300920917     C                   MOVEL     *BLANKS       COMMAN           80
052400950309     C                   MOVEA     CMD           COMMAN
052500920917     C                   CALL      'QCMDEXC'                            H1
052600920917     C                   PARM                    COMMAN
052700920917     C                   PARM                    LUNG
052800920917     C*
052900950309     C                   Z-ADD     1             C                 3 0
053000920916     C                   OPEN      QSYSPRT
053100920916     C*
053200030912do  1C     LIN(C)        DOWNE     0
053300920916     C                   SETON                                        OF
053400920917     C                   MOVEL     LIN(C)        COMFIL
053500920924     C     COMFIL        CHAIN     AZORG                              31
053600030912if  2C     *IN31         IFEQ      *OFF
053700991020     C                   MOVEL     ORGDES        DESFIL
053800991020     C                   MOVEL     ORGDES        DESLNP
053900030912x   2C                   ELSE
054000991020     C                   MOVEL     *BLANKS       DESFIL           20
054100991020     C                   MOVEL     *BLANKS       DESLNP           20
054200030912e   2C                   ENDIF
054300920916     C*
054400941229     C     KBLP          SETLL     FNBLP000
054500920916     C*
054600991020     C   03COMFIL        READE     FNBLP000                               30
054700991020     C  N03KBLP          READE     FNBLP000                               30
054800920916     C*
054900030912do  2C     *IN30         DOWEQ     '0'
055000920924     C                   SETOFF                                       05
055100900525     C* SE IMMESSO AL E DATA LETTA > VADO A FINE FILE
055200030912if  3C   03BLPDSP        IFGT      DS1AAL
055300920916     C                   SETON                                        30
055400030912e   3C                   END
055500920925     C* TIPO BOLLA
055600950309     C                   MOVEL     '3A'          KCOD
055700950309     C                   MOVEL     *BLANKS       KKEY
055800950309     C                   MOVEL     BLPCBO        KKEY
055900950309     C     KTAB1         CHAIN     TABEL                              32
056000920925     C  N32              MOVEL     TBLUNI        DS3A
056100920916     C*
056200941228     C* AGGANCIO FILE TASSAZIONE BOLLA
056300030912if  3C     *IN30         IFEQ      *OFF
056400991020      *
056500991020     C                   MOVEL     '1'           WTRC
056600991020     C     KBLP3         CHAIN     FIAR6000                           34
056700991020      *
056800991020      * GESTIONE CAMPI TASSAZIONE
056900991020     C                   EXSR      GESTAS
057000040621     c*
057100040621     c   24              exsr      elab2
057200941228     C*
057300991020     C   25              EXSR      ELAB8
057400920917     C*
057500991020     C   26              EXSR      ELABC
057600920917     C*
057700991020     C   27              EXSR      ELABQ
057800920916     C*
057900991020     C   28              EXSR      ELAB5
058000921001     C*
058100991020     C   29              EXSR      ELABP
058200991020      *
058300030912e   3C                   ENDIF
058400921002     C*
058500991020     C  N30
058600991020     CAN 03COMFIL        READE     FNBLP000                               30
058700991020     C  N03KBLP          READE     FNBLP000                               30
058800030912e   2C                   ENDDO
058900030912      *
059000030912      * A rottura di LNP stampo il riepilogo delle spedizioni elencate
059100030912      *   totalizzate per BLPSOP
059200030912     c                   if        *in09 = *on  and  *in29 = *on
059300030912     c                   eval      *inOF = *on
059400030912     c                   exsr      PrtRiepSOP
059500030912     c                   endif
059600920916     C*
059700920916     C                   ADD       1             C
059800030912e   1C                   END
059900920916     C*
060000991020     C                   SELECT
060100991020     C     *IN09         WHENEQ    *ON
060200991020     C     DS1FGS        WHENGT    *ZEROS
060300991020     C                   MOVEL     DS1FGS        COMFIL
060400991020     C     DS1LNP        WHENEQ    SIMFEL
060500991020     C                   MOVEL     DS1LNP        COMFIL
060600991020     C                   ENDSL
060700930209     C*
060800930209     C     COMFIL        CHAIN     AZORG                              31
060900930209     C  N31              MOVEL     ORGDES        DESFIL
061000930209     C   31              MOVEL     *BLANKS       DESFIL
061100920917     C*
061200991020     C  N09
061300991020     CANN29              EXCEPT    TESTA
061400030912     C  N09
061500030912     CAN 29              EXCEPT    TCAP
061600030912     c  n09
061700030912     can 29              except    TCAP2
061800030912      *
061900920916     C                   CLOSE     QSYSPRT
062000950309     C*
062100920916     C                   ENDSR
062200950309     C**************************************************************************
062300991020     C* GESTIONE CAMPI TASSAZIONE
062400950309     C**************************************************************************
062500991020     C     GESTAS        BEGSR
062600991020      *
062700991020     C                   CLEAR                   VARIAI
062800991020     C                   CLEAR                   STADIV
062900991020     C                   CLEAR                   STAPOR
063000991020     C                   CLEAR                   STASV1
063100991020     C                   CLEAR                   STAVA1
063200991020     C                   CLEAR                   STASV2
063300991020     C                   CLEAR                   STAVA2
063400991020     C                   CLEAR                   STAVA3
063500991020     C                   CLEAR                   STAIMV
063600991020     C                   CLEAR                   MEMNFT
063700991020     C                   CLEAR                   MEMDFT
063800991020      *
063900991020     C     *IN34         IFEQ      *OFF
064000030912     C                   Z-ADD     1             XX
064100991020     C                   MOVEL     AR6DIV        STADIV
064200991020     C                   Z-ADD     AR6POR        STAPOR            9 3
064300991020     C                   Z-ADD     AR6IMV        STAIMV           11 3
064400991020     C                   Z-ADD     AR6NFT        MEMNFT
064500991020     C                   Z-ADD     AR6DFT        MEMDFT
064600991020      *  GESTIONE VARIE
064700991020     C                   CLEAR                   STAVA3
064800991020      *
064900991020     C     AR6SV1        IFNE      *BLANKS
065000991020     C                   MOVEL     AR6SV1        WSV1
065100991020     C                   Z-ADD     AR6VA1        WVA1
065200991020     C                   EXSR      MEMVAR
065300991020     C                   ENDIF
065400991020      *
065500991020     C     AR6SV2        IFNE      *BLANKS
065600991020     C                   MOVEL     AR6SV2        WSV1
065700991020     C                   Z-ADD     AR6VA2        WVA1
065800991020     C                   EXSR      MEMVAR
065900991020     C                   ENDIF
066000991020      *
066100991020     C     AR6SV3        IFNE      *BLANKS
066200991020     C                   MOVEL     AR6SV3        WSV1
066300991020     C                   Z-ADD     AR6VA3        WVA1
066400991020     C                   EXSR      MEMVAR
066500991020      *
066600991020     C     KBLP3         CHAIN     FIAR7000                           34
066700991020     C     *IN34         DOWEQ     *OFF
066800991020     C                   MOVEL     AR7SVN        WSV1
066900991020     C                   Z-ADD     AR7VAN        WVA1
067000991020     C                   EXSR      MEMVAR
067100991020     C     KBLP3         READE     FIAR7000                               34
067200991020     C                   ENDDO
067300991020     C                   ENDIF
067400991020      *
067500991020     C                   ENDIF
067600991020      *
067700991020     C                   ENDSR
067800991020     C**************************************************************************
067900991020     C* MEMORIZZO VARIE PER STAMPA
068000991020     C**************************************************************************
068100991020     C     MEMVAR        BEGSR
068200991020      *
068300991020      * MEMORIZZO LA VARIA I - GIACENZE
068400991020     C     WSV1          IFEQ      'I'
068500991020     C                   Z-ADD     WVA1          VARIAI
068600991020     C                   ENDIF
068700991020      *
068800991020      * Non considero IVA e Bollo
068900991020     C                   SELECT
069000991020      *
069100991020     C     WSV1          WHENEQ    §$2IVA
069200991020      *
069300991020     C     WSV1          WHENEQ    §$2BOL
069400991020      *
069500991020     C     XX            WHENEQ    3
069600991020     C                   ADD       WVA1          STAVA3            9 3
069700991020      *
069800991020     C     XX            WHENEQ    2
069900991020     C                   MOVEL     WSV1          STASV2
070000991020     C                   Z-ADD     WVA1          STAVA2            9 3
070100030912     C                   ADD       1             XX
070200991020      *
070300991020     C     XX            WHENEQ    1
070400991020     C                   MOVEL     WSV1          STASV1
070500991020     C                   Z-ADD     WVA1          STAVA1            9 3
070600991020     C                   ADD       1             XX
070700991020      *
070800991020     C                   ENDSL
070900991020      *
071000991020     C                   ENDSR
071100040621     C**************************************************************************
071200040621     C* ELABORO PER STAMPA spedizioni oltre 20 colli
071300040621     C**************************************************************************
071400040621     C     ELAB2         BEGSR
071500040621     C*
071600040621     C* ESCLUDO I RECUPERI
071700040621     C   32              MOVEL     'F1'          §3ATB1
071800040621     C*
071900040621     C     §3ATB1        IFNE      'F3'
072000040621     C     §3ATB1        ANDNE     'A3'
072100040621     C* Spedizioni oltre 20 colli non bancalate
072200040621     c     blpncl        andgt     20
072300040621     c                   if        %subst(blpgva:1:1) <> 'B' and
072400040621     c                             %Subst(blpgva:1:1) <> 'O'
072500040621     C                   EXSR      STAMPA
072600040621     c                   endif
072700040621     C                   END
072800040621     C*
072900040621     C                   ENDSR
073000040621     C*
073100991020     C**************************************************************************
073200991020     C* ELABORO PER STAMPA 8888
073300991020     C**************************************************************************
073400991020     C     ELAB8         BEGSR
073500991020     C*
073600920925     C   32              MOVEL     'F1'          §3ATB1
073700930930     C   32              MOVEL     ' '           §3ARBL
073800900517     C* ESCLUDO CODICI NON 8888
073900900517     C                   MOVE      BLPKSC        COM88             4 0
074000931110    1C     COM88         IFEQ      8888
074100931110     C*
074200931110     C* ESCLUDO I TIPI BOLLA STORNO (XS)
074300931110     C                   MOVE      §3ATB1        FLGDUE            1
074400931110    2C     FLGDUE        IFNE      'S'
074500920916     C*
074600930930     C* ESCLUDO I CONTI SERVIZI (§3ARBL = C)
074700931110    3C     §3ARBL        IFNE      'C'
074800920916     C* ESCLUDO I PREPAGATI
074900991020     C     MEMNFT        ANDEQ     0
075000900517     C                   EXSR      STAMPA
075100931110    3C                   ENDIF
075200931110    2C                   ENDIF
075300931110    1C                   ENDIF
075400900525     C*
075500920916     C                   ENDSR
075600950309     C**************************************************************************
075700950309     C* ELABORO PER STAMPA CONVENUTI
075800950309     C**************************************************************************
075900920917     C     ELABC         BEGSR
076000950309     C*
076100920924     C   32              MOVEL     'A2'          §3ATB1
076200920924     C                   MOVEL     §3ATB1        FLGTB1            1
076300920925     C* ESCLUDO ASSEGNATI
076400920924     C     FLGTB1        IFEQ      'A'
076500920924     C                   GOTO      ENDELC
076600991020     C                   ENDIF
076700920924     C*
076800920924     C* ESCLUDO SE BOLLA GIACENTE
076900920924     C* (VARIA I UGUALE A TOTALE IMPONIBILE)
077000991020     C     VARIAI        IFGT      *ZEROS
077100991020     C     VARIAI        ANDEQ     STAIMV
077200991020     C                   CLEAR                   VARIAI
077300920924     C                   GOTO      ENDELC
077400991020     C                   ENDIF
077500920924     C*
077600920924     C* 05 ON - ESISTE 2° BOLLA
077700991005     C                   MOVEL     '2'           WTRC
077800991005     C     KBLP3         CHAIN     FIAR6000                           32
077900991020     C     *IN32         IFEQ      *OFF
078000991020     C                   SETON                                        05
078100040203S   2c                   Select
078200040203     c                   When      Ar6Sv1 = §$2Vrg
078300991020     C                   Z-ADD     AR6VA1        STA2VA
078400040203     c                   When      Ar6Sv2 = §$2Vrg
078500040203     c                   Z-add     Ar6Va2        Sta2va
078600040203     c                   When      Ar6Sv3 = §$2Vrg
078700040203     C                   Z-Add     Ar6Va3        Sta2va
078800040203   x2c                   Other
078900040203     c     kBlp3         Setll     Fiar701l
079000040203do  3c                   Do        *Hival
079100040203     c     kBlp3         Reade     Fiar701l
079200040203     c                   If        %Eof(Fiar701l)
079300040203     c                   Leave
079400040203     c                   EndIf
079500040203     c                   If        Ar7Svn = §$2Vrg
079600040203     c                   Z-add     Ar7Van        Sta2va
079700040203     c                   EndIf
079800040203    3c                   EndDo
079900040203    2c                   EndSl
080000991020     C                   ENDIF
080100920924     C*
080200920924     C* STAMPO SE C'E' CONVENUTO
080300991020     C     STAPOR        IFNE      *ZEROS
080400991020     C     STAVA1        ORNE      *ZEROS
080500991020     C     STAVA2        ORNE      *ZEROS
080600991020     C     STAVA3        ORNE      *ZEROS
080700991020     C     STAIMV        ORNE      *ZEROS
080800991020     C     *IN05         OREQ      *ON
080900991020     C     STA2VA        ANDNE     *ZEROS
081000920924     C                   EXSR      STAMPA
081100920924     C                   END
081200920924     C*
081300920924     C     ENDELC        ENDSR
081400950309     C**************************************************************************
081500950309     C* ELABORO PER STAMPA OLTRE 35 Q.LI
081600950309     C**************************************************************************
081700920917     C     ELABQ         BEGSR
081800950309     C*
081900920925     C* ESCLUDO I RECUPERI
082000920925     C   32              MOVEL     'F1'          §3ATB1
082100920925     C*
082200920925     C     §3ATB1        IFNE      'F3'
082300920925     C     §3ATB1        ANDNE     'A3'
082400920925     C* PESO OLTRE 35 Q.LI
082500941228     C     BLPPKF        ANDGT     3500
082600920925     C                   EXSR      STAMPA
082700920925     C                   END
082800950309     C*
082900920917     C                   ENDSR
083000950309     C**************************************************************************
083100950309     C* ELABORO PER STAMPA OLTRE MC 5 E KG 500
083200950309     C**************************************************************************
083300921001     C     ELAB5         BEGSR
083400950309     C*
083500921001     C   32              MOVEL     'F1'          §3ATB1
083600921001     C* ESCLUDO RECUPERI
083700921001     C     §3ATB1        IFNE      'F3'
083800921001     C     §3ATB1        ANDNE     'A3'
083900921001     C*
084000921001     C* CONSIDERO SOLO PESO   > 500 KG
084100921001     C*              O VOLUME > 5 MC   (DA VARIABILI IN TABELLA)
084200941228     C     BLPVLF        IFGT      §3IVLF
084300941228     C     BLPPKF        ORGT      §3IPKF
084400921001     C*
084500921001     C* 05 ON - ESISTE 2° BOLLA
084600991005     C                   MOVEL     '2'           WTRC
084700991005     C     KBLP3         CHAIN     FIAR6000                           32
084800991020     C     *IN32         IFEQ      *OFF
084900991020     C                   SETON                                        05
085000040203S   2c                   Select
085100040203     c                   When      Ar6Sv1 = §$2Vrg
085200991020     C                   Z-ADD     AR6VA1        STA2VA
085300040203     c                   When      Ar6Sv2 = §$2Vrg
085400040203     c                   Z-add     Ar6Va2        Sta2va
085500040203     c                   When      Ar6Sv3 = §$2Vrg
085600040203     C                   Z-Add     Ar6Va3        Sta2va
085700040203   x2c                   Other
085800040203     c     kBlp3         Setll     Fiar701l
085900040203do  3c                   Do        *Hival
086000040203     c     kBlp3         Reade     Fiar701l
086100040203     c                   If        %Eof(Fiar701l)
086200040203     c                   Leave
086300040203     c                   EndIf
086400040203     c                   If        Ar7Svn = §$2Vrg
086500040203     c                   Z-add     Ar7Van        Sta2va
086600040203     c                   EndIf
086700040203    3c                   EndDo
086800040203    2c                   EndSl
086900991020     C                   ENDIF
087000921001     C*
087100921001     C                   EXSR      STAMPA
087200921001     C                   END
087300921001     C                   END
087400921001     C*
087500921001     C                   ENDSR
087600950309     C**************************************************************************
087700950309     C* ELABORO PER STAMPA SPE CON CAP GENERICO
087800950309     C**************************************************************************
087900921002     C     ELABP         BEGSR
088000950309     C*
088100921002     C* ESCLUDO CODICI  8888
088200921002     C                   MOVE      BLPKSC        COM88             4 0
088300970708    1C     COM88         IFNE      8888
088400970708     C* RICHIAMO FNLV13R
088500970708     C                   CLEAR                   DSSI95
088600970708     C                   MOVEL     '3'           I95TCN
088700970708     C                   MOVEL     BLPCAD        I95CAP
088800970708     C                   MOVEL     BLPNZD        I95NAR
088900970708     C                   MOVEL     BLPPRD        I95PRV
089000970708     C                   Z-ADD     BLPMGS        I95DAT
089100970708     C                   MOVEL     BLPAAS        I95DAT
089200970708     C                   CALL      'TISI95R'
089300970708     C                   PARM                    DSSI95
089400970708     C* SE CAP GENERICO CON STRADARIO ERRORE E STAMPO
089500970708    2C     O95ERR        IFEQ      ' '
089600970708     C     O95FLG        ANDEQ     'S'
089700921002     C*
089800921002     C* SE CAMBIATO CODICE CLIENTE VADO A PAGINA NUOVA
089900970708    3C     BLPKSC        IFNE      SAVKSC
090000921002     C                   SETON                                        OF
090100921002     C                   Z-ADD     BLPKSC        SAVKSC            7 0
090200970708    3C                   END
090300921002     C* VEDO SE PARTITA LA SPEDIZIONE
090400970708    3C     BLPNFV        IFNE      0
090500921002     C                   MOVEL     'P'           PARTI             1
090600921002     C                   ELSE
090700921002     C                   MOVEL     ' '           PARTI
090800970708    3C                   END
090900921002     C*
091000921002     C                   EXSR      STAMPA
091100970708    2C                   END
091200970708    1C                   END
091300921002     C*
091400921002     C                   ENDSR
091500950309     C**************************************************************************
091600950309     C*    CARICAMENTO TABELLE
091700950309     C**************************************************************************
091800920916     C     CARTAB        BEGSR
091900950309     C*
092000931206     C* DECODIFICO SIMFEL
092100931206     C     SIMFEL        CHAIN     AZORG                              30
092200931206     C  N30              MOVEL     ORGDES        DESSIM           20
092300931206     C   30              MOVEL     *BLANKS       DESSIM
092400991020     C*
092500991020     C* CARICO DS$2 PER DECODIFICA VARIE
092600991020     C                   MOVEL     '$2'          KCOD
092700991020     C                   CLEAR                   KKEY
092800991020     C                   MOVEL     '1'           KKEY
092900991020     C     KTAB1         CHAIN     TABEL                              32
093000991020     C  N32              MOVEL     TBLUNI        DS$2
093100921001     C*
093200921001     C* CARICO DS3I PER OLTRE MC 5
093300921001     C   28              DO
093400950309     C                   MOVEL     '3I'          KCOD
093500950309     C                   MOVEL     *BLANKS       KKEY
093600950309     C                   MOVEL     '1'           KKEY
093700950309     C     KTAB1         CHAIN     TABEL                              32
093800921001     C  N32              MOVEL     TBLUNI        DS3I
093900941228     C   32              Z-ADD     500           §3IPKF
094000941228     C   32              Z-ADD     5             §3IVLF
094100921001     C*
094200941228     C                   Z-ADD     §3IPKF        STAPKF            6 0
094300921001     C                   END
094400950309     C*
094500950309     C     DS1FGS        IFNE      *ZERO
094600950309     C* NON SONO LE STAMPE GIORNALIERE ( DS1FGS<>0 )
094700950309     C     DS1LNP        IFNE      *ZERO
094800950309     C                   SETON                                        22
094900950309     C                   ENDIF
095000950309     C                   ENDIF
095100950309     C*
095200920916     C                   ENDSR
095300950309     C**************************************************************************
095400950309     C*    STAMPA BOLLA
095500950309     C**************************************************************************
095600900517     C     STAMPA        BEGSR
095700950309     C*
095800991020     C                   SETOFF                                       0708
095900991020     C                   SETOFF                                       1923
096000920917     C                   SETON                                        09
096100991020     C                   CLEAR                   TRAT1
096200991020     C                   CLEAR                   TRAT2
096300040203     C                   CLEAR                   STA2VA
096400950121     C*
096500950121     C* DECODIFICA TIPO SERVIZIO
096600950309     C                   MOVEL     '5E'          KCOD
096700950309     C                   MOVEL(P)  BLPTSP        KKEY
096800950309     C     KTAB1         CHAIN     TABEL                              32
096900991020     C     *IN32         IFEQ      *OFF
097000991020     C                   MOVEL     TBLUNI        DECTSP           13
097100991020     C                   MOVEL     TBLUNI        DECTS2            4
097200991020     C                   ELSE
097300991020     C                   CLEAR                   DECTSP
097400991020     C                   CLEAR                   DECTS2
097500991020     C                   ENDIF
097600950121     C*
097700900517     C* C'E' VALORE MERCE
097800941228     C     BLPIAS        COMP      0                                  0606
097900941227     C*
098000941227     C* CONTROLLO SE C'E' CONTRASSEGNO
098100941228    1C     *IN27         IFEQ      *OFF
098200051115     C     KBLP9         CHAIN     Fiar901L                           33
098300941227     C  N33              SETON                                        07
098400941228    1C                   ENDIF
098500941227     C*
098600920924     C* GIRO DATA SPEDIZIONE
098700941227     C                   MOVE      BLPDSP        DATA
098800941227     C                   Z-ADD     MM            GGMM              4 0
098900941227     C                   MOVEL     GG            GGMM
099000941227     C                   Z-ADD     AA            STADSP
099100941227     C                   MOVEL     GGMM          STADSP
099200941229     C*
099300941229     C* CAMPI DI COMODO PER STAMPA
099400941229     C                   MOVEL     BLPRSM        STARSM           20
099500941229     C                   MOVEL     BLPINM        STAINM           20
099600030912     C                   MOVEL     BLPRSD        STARSD           20
099700941229     C                   MOVEL     BLPLOD        STALOD           16
099800030912     C                   MOVEL     BLPRSD        STARS2           19
099900030912     C                   MOVEL     BLPLOD        STALO2           19
100000941229     C                   MOVEL     BLPLOM        STALOM           20
100100921002     C*
100200941228     C* 29 OFF - NON SI TRATTA DELLA STAMPA SPED CON CAP GENERICO
100300941228    1C     *IN29         IFEQ      '0'
100400941228     C*
100500941229     C* 27 OFF - NON SI TRATTA DELLA STAMPA DELLE SPEDIZ. OLTRE 35 Q.LI
100600941229    2C     *IN27         IFEQ      *OFF
100700941229     C*
100800900517     C* CONTROLLO SE ESISTONO VARIE
100900991020    3C     STASV1        IFNE      *BLANKS
101000991020     C                   MOVEL     '-'           TRAT1             1
101100941228    3C                   END
101200991020    3C     STASV2        IFNE      *BLANKS
101300991020     C                   MOVEL     '-'           TRAT2             1
101400941228    3C                   END
101500941228     C*
101600920924     C* CONTROLLO SE ESISTA DATA FATTURA
101700941228    3C     *IN05         IFEQ      *OFF
101800991020     C     MEMDFT        ANDGT     0
101900920924     C                   SETON                                        08
102000991020     C                   MOVE      MEMDFT        DATA
102100941227     C                   Z-ADD     MM            GGMM
102200941227     C                   MOVEL     GG            GGMM
102300941227     C                   Z-ADD     AA            STADFT
102400941227     C                   MOVEL     GGMM          STADFT
102500941228     C*
102600991020     C                   MOVEL     MEMNFT        STANFT
102700941228    3C                   ENDIF
102800941228     C*
102900941229   X2C                   ELSE
103000941229     C* 27 ON  - STAMPA DELLE SPEDIZIONI OLTRE 35 Q.LI
103100941229     C*
103200920925     C* CONTROLLO SE ESISTONO NOTE
103300941227     C                   MOVEL     '8'           WTRC
103400060216     C     KBLP3         CHAIN     Fiar401L                           33
103500941228    3C     *IN33         IFEQ      *OFF
103600941227     C                   SETON                                        19
103700060216     C                   MOVEL     ar4NOT        STANT1
103800941228    3C                   ENDIF
103900941227     C*
104000941227     C                   MOVEL     '9'           WTRC
104100060216     C     KBLP3         CHAIN     Fiar401L                           33
104200941228    3C     *IN33         IFEQ      *OFF
104300941227     C                   SETON                                        23
104400060216     C                   MOVEL     ar4NOT        STANT2
104500941228    3C                   ENDIF
104600941229     C*
104700941228    2C                   ENDIF
104800920925     C*
104900920924     C* STAMPO TESTATA
105000920924     C   OF              EXCEPT    TESTA
105100920924     C   OF              SETOFF                                       OF
105200900517     C* STAMPA DETTAGLIO
105300900517     C                   EXCEPT    DET
105400941228     C**
105500941228     C**
105600941228   X1C                   ELSE
105700941228     C**
105800941228     C**
105900921002     C* 29 ON - STAMPA SPED CON CAP GENERICO
106000921002     C*
106100921002     C* STAMPO TESTATA
106200030912     c                   if        *inOF
106300030912     c                   except    TCAP
106400030912     c                   except    TCAP2
106500030912     c                   movel     *off          *inOF
106600030912     c                   endif
106700921002     C* STAMPA DETTAGLIO
106800921002     C                   EXCEPT    DCAP
106900030912      *
107000030912      * Incremento contatore delle spedizioni stampate per operatore
107100030912     c                   eval      xx       = 1
107200030912     c                   eval      *in31    = *off
107300030912     c     BLPsop        lookup    $SOP(xx)                               31
107400030912      *   Se operatore non ancora elaborato lo memorizzo
107500030912if  2c                   if        *in31    = *off
107600030912     c     *blanks       lookup    $SOP(xx)                               31
107700030912if  3c                   if        *in31    = *on
107800030912     c                   eval      $SOP(xx) = BLPsop
107900030912e   3c                   endif
108000030912e   2c                   endif
108100030912      *   Incremento il contatore delle spedizioni da lui stampate
108200030912if  2c                   if        *in31    = *on
108300030912     c                   add       1             $SOPcont(xx)
108400030912x   2c                   else
108500030912     c                   add       1             XaltriSOP
108600030912e   2c                   endif
108700030912      *
108800941228    1C                   END
108900921002     C*
109000900517     C                   ENDSR
109100030912      *****************************************************************
109200030912      * Stampa Riepilogo per SOP                                      *
109300030912      *****************************************************************
109400030912     c     PrtRiepSOP    BEGSR
109500030915      *
109600030915      * Ordino i dati per sigla operatore in modo discendente
109700030915     c                   eval      $SOPsave  = $SOP
109800030915     c                   eval      $SOPsaveC = $SOPcont
109900030915     c                   clear                   $SOPcont
110000030915     c                   sorta     $SOP
110100030915     c                   eval      wa        = 1
110200030915do  1c                   DOW       wa       <= %elem($SOP)
110300030915if  2c                   if        $SOP(wa)  = *blanks
110400030915     c                   leave
110500030915e   2c                   endif
110600030915     c                   eval      wc        = 1
110700030915     c                   eval      *inOF     = *off
110800030915     c     $SOP(wa)      lookup    $SOPsave(wc)                           31
110900030915if  2c                   if        *in31
111000030915     c                   eval      $SOPcont(wa) = $SOPsaveC(wc)
111100030915e   2c                   endif
111200030915     c                   add       1             wa
111300030915e   1c                   ENDDO
111400030912      *
111500030912      * Stampo testata
111600030912     c                   except    TCAP
111700030912     c                   eval      *inOF     = *off
111800030912      *
111900030912      * Stampo riepilogo per operatore
112000030912     c                   eval      xx        = 1
112100030912do  1c                   DOW       xx       <= %elem($SOP)
112200030912      *
112300030912if  2c                   if        $Sop(xx)  = *blanks
112400030912     c                   leave
112500030912e   2c                   endif
112600030912      *
112700030912if  2c                   if        xx        = 1
112800030912     c                   except    TXRsop
112900030912e   2c                   endif
113000030912     c                   except    RIEsop
113100030912      *
113200030912     c                   add       1             xx
113300030912e   1c                   ENDDO
113400030912      *
113500030912if  1c                   if        XaltriSOP > *zeros
113600030912     c                   except    RIEsop2
113700030912e   1c                   endif
113800030912      *
113900030912      * Azzero i contatori
114000030912     c                   clear                   $SOP
114100030912     c                   clear                   $SOPcont
114200030912     c                   clear                   XaltriSOP
114300030912      *
114400030912     c                   ENDSR
114500950309     C**************************************************************************
114600040621     O* SPED 8888 E CONVENUTI E oltre 20 colli
114700040621     OQSYSPRT   E      N27N28TESTA            02
114800920924     O               25      SK(1)               66
114900920924     O               26      SK(6)               66
115000040621     O               24      SK(28)              66
115100920925     O              N03      SK(2)              132
115200920925     O               03      SK(7)              132
115300900517     O                       RSUT                20
115400950309     O                       DS1GDA              81 '  /  /    '
115500950309     O               03      DS1GAL              95 '  /  /    '
115600950314     O                       WDATE              118 '  /  /    '
115700920917     O                       PAGE          Z    132
115800920925     O* OLTRE 35 Q.LI
115900920925     O          E       27   TESTA            02
116000920925     O                       SK(9)               66
116100920925     O              N03      SK(10)             132
116200920925     O               03      SK(11)             132
116300900524     O                       RSUT                20
116400950309     O                       DS1GDA              86 '  /  /    '
116500950309     O               03      DS1GAL             100 '  /  /    '
116600950314     O                       WDATE              118 '  /  /    '
116700920917     O                       PAGE          Z    132
116800921001     O* OLTRE 5 MC E 500 KG
116900921001     O          E       28   TESTA            02
117000921001     O                       SK(13)              66
117100921001     O              N03      SK(14)             132
117200921001     O               03      SK(15)             132
117300921001     O                       RSUT                20
117400941228     O                       STAPKF        Z     65
117500941228     O                       §3IVLF        4     88
117600950309     O                       DS1GDA             103 '  /  /    '
117700950309     O               03      DS1GAL             118 '  /  /    '
117800950314     O              N03      WDATE              118 '  /  /    '
117900921001     O                       PAGE          Z    132
118000920917     O          E            TESTA            03
118100950313     O                       KNSIF               10
118200950313     O                       KNMUS               22
118300950313     O                       SK(25)             107
118400950313     O              N11N14N22                    73 'TUTTE'
118500950313     O                     22DESLNP              88
118600950313     O                     11DESSIM              88
118700950313     O                     14DESLNP              88
118800950313     O                       UTIME              117 '0 :  :  '
118900931206     O          E            TESTA            05
119000920924     O                       SK(8)               66
119100920924     O                       COMFIL              23
119200920924     O                                           25 '-'
119300931206     O                       DESFIL              47
119400030912     O          E            TESTA            07
119500900517     O                       SK(3)               66
119600920925     O                 N27   SK(4)              132
119700950106     O                  27                       66 ' '
119800920925     O                  27   SK(12)             132
119900030912     O          E            TESTA            08
120000900517     O                       SK(5)               66
120100900517     O                       SK(5)              132
120200950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
120300900517     O          E            DET         1  1
120400920924     O                       BLPNRS        Z
120500900517     O                       BLPNSP        Z   +  1
120600900524     O                       BLPLNA            +  1
120700900517     O                       STARSM            +  1
120800900517     O                       STARSD            +  1
120900941228     O                       BLPNCL        Z   +  3
121000991020     O              N27      STADIV            +  1
121100991020     O              N27      STAPOR        4   +  2
121200991020     O              N27      STASV1            +  2
121300991020     O              N27      TRAT1             +  0
121400991020     O              N27      STAVA1        4   +  0
121500991020     O              N27      STASV2            +  2
121600991020     O              N27      TRAT2             +  0
121700991020     O              N27      STAVA2        4   +  0
121800991020     O              N27      STAVA3        4   +  2
121900991020     O              N27      STAIMV        4    132
122000991020     O               27      STAIMV        4     78
122100900517     O          E            DET            1
122200941228     O                       STADSP              10 '  /  /    '
122300920924     O                       STAINM            +  5
122400900517     O                       STALOD            +  1
122500900517     O                       BLPPRD            +  1
122600941228     O                       BLPPKF            +  1 '      , '
122700950106     O                                         +  1 'COD.BOLLA'
122800941230     O                       BLPCBO            +  1
122900950106     O              N27    06                  +  2 'IMP.DA ASS:'
123000950106     O              N27    06BLPIAS        4   +  0
123100941229     O              N27    06BLPVAS            +  1
123200950106     O              N27    07                  +  2 'C/A:'
123300051115     O              N27    07ar9CAS        4   +  0
123400051115     O              N27    07ar9VCA            +  1
123500900517     O          E            DET            1
123600950121     O                       DECTSP              13
123700950121     O                       STALOM            +  2
123800900517     O                       BLPNAS            +  1
123900941228     O                       BLPFVF            +  3
124000950106     O                       BLPVLF        4   +  3
124100950106     O                  05                     +  1 'ASSEGNATO: VARIA G-'
124200991020     O                  05   STA2VA        4   +  0
124300920924     O                  08                     +  2 'FATTURA N.'
124400920924     O                  08   STANFT        Z   +  0
124500920924     O                  08                     +  1 'DEL'
124600941229     O                  08   STADFT            +  1 '  /  /    '
124700920925     O          E       19   DET            1
124800941230     O         OR    23
124900920925     O                                         + 15 'DISPOSIZIONI:'
125000941229     O                     19STANT1            +  1
125100950106     O                     23STANT2            +  3
125200950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
125300921002     O* SPED CON CAP GENERICO
125400921002     O          E            TCAP             02
125500921002     O                       SK(16)              66
125600921002     O              N03      SK(18)             132
125700921002     O               03      SK(17)             132
125800921002     O                       RSUT                20
125900950309     O                       DS1GDA              91 '  /  /    '
126000950309     O               03      DS1GAL             105 '  /  /    '
126100950314     O                       WDATE              118 '  /  /    '
126200921002     O                       PAGE          Z    132
126300921002     O          E            TCAP             03
126400950313     O                       KNSIF               10
126500950313     O                       KNMUS               22
126600950313     O                       SK(25)             107
126700950313     O              N11N14N22                    73 'TUTTE'
126800950313     O                     22DESLNP              88
126900950313     O                     11DESSIM              88
127000950313     O                     14DESLNP              88
127100950313     O                       UTIME              117 '0 :  :  '
127200931206     O          E            TCAP             05
127300921002     O                       SK(8)               66
127400921002     O                       COMFIL              23
127500921002     O                                           25 '-'
127600931206     O                       DESFIL              47
127700030912     O          E            TCAP2            07
127800921002     O                       SK(19)              66
127900921002     O                       SK(22)             132
128000030912     O          E            TCAP2            08
128100921002     O                       SK(20)              66
128200921002     O                       SK(23)             132
128300030912     O          E            TCAP2          1 09
128400921002     O                       SK(5)               66
128500921002     O                       SK(5)              132
128600950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
128700921002     O          E            DCAP           1
128800921002     O                       BLPNRS        Z
128900921002     O                       BLPNSP        Z   +  1
129000970602     O                       STADSP            +  1 '  /  /    '
129100970602     O                       BLPLNA            +  1
129200970602     O                       BLPKSC            +  1
129300921002     O                                         +  0 '-'
129400941229     O                       STARSM            +  0
129500970602     O                       BLPRMN        Z   +  1
129600030912     O                       STARS2            +  1
129700970602     O                       STALO2            +  1
129800970602     O                       BLPPRD            +  1
129900970602     O                       BLPCAD            +  1
130000970602     O                       DECTS2            +  1
130100970602     O                       PARTI             +  1
130200030912     O                       BLPsop            +  1
130300030912     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
130400030912     O          e            TXRsop      2
130500030912     O                       SK(26)
130600030912     O          e            TXRsop      1  1
130700030912     O                       SK(27)
130800030912     O          e            RIEsop      1
130900030912     O                       $SOP(xx)            40
131000030912     O                       $SOPcont(xx)  1     62
131100030912     O          e            RIEsop2     1
131200030912     O                                           38 'Altri'
131300030912     O                       XaltriSOP     1     62
131400950309     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
131500931206**
131600950309OVRPRTF FILE(QSYSPRT) FORMTYPE('          ') OUTQ(          )
131700900517**
131800050802XXXXXXXXXXXXXXXXXXXX                     ** ELENCO SPEDIZIONI 8888   1
131900950313 DEL XX/XX/XXXX **            FNLS10R     XX.XX.XXXX     PAG. XXXX   2
132000950106SPEDIZIONE LNA MITTENTE             DESTINATARIO           C/P/V D   3
132100991020IV  TRASPORTO      VARIE        VARIE        VARIE        IMPONIB.   4
132200920924------------------------------------------------------------------   5
132300051115XXXXXXXXXXXXXXXXXXXX            ** ELENCO SPEDIZIONI CON CONVENUTI   6
132400941223 DAL XX/XX/XXXX AL XX/XX/XXXX ** FNLS10R  XX.XX.XXXX     PAG. XXXX   7
132500931206LINEA DI PARTENZA : XXX    XXXXXXXXXXXXXXXXXXXX                      8
132600051115XXXXXXXXXXXXXXXXXXXX        ** ELENCO SPEDIZIONI CON PESO OLTRE 35   9
132700941228 Q.LI DEL XX/XX/XXXX **  FNLS10R          XX.XX.XXXX      PAG.XXXX  10
132800941228 Q.LI DAL XX/XX/XXXX AL XX/XX/XXXX **     XX.XX.XXXX LS10 PAG.XXXX  11
132900920925IMPONIBILE   COSTO PART. DIFF.NOLO PART. DIFF.NOLO ARR. COSTO ARR.  12
133000051115XXXXXXXXXXXXXXXXXXXX        * SPEDIZIONI CON PESO OLTRE KG.XXXXXX   13
133100941228O VOL. OLTRE MC.XX,XXX DEL XX/XX/XXXX *   XX.XX.XXXX LS10 PAG.XXXX  14
133200941228O VOL. OLTRE MC.XX,XXX DAL XX/XX/XXXX  AL XX/XX/XXXX LS10 PAG.XXXX  15
133300051115XXXXXXXXXXXXXXXXXXXX     *ELENCO SPEDIZ. CON C.A.P. CHE PREVEDE LO  16
133400941229 STRADARIO DAL XX/XX/XXXX AL XX/XX/XXXX * XX.XX.XXXX LS10 PAG.XXXX  17
133500941229 STRADARIO DEL XX/XX/XXXX *               XX.XX.XXXX LS10 PAG.XXXX  18
133600970602S P E D I Z I O N E  |LIN|           M  I  T  T  E  N  T  E         19
133700970602SER NUMERO    DATA   |ARR|CODICE   RAGIONE   SOCIALE    RIFER.NUME  20
133800970602XX XXXXXXX XX/XX/XXXX XXX XXXXXXX-XXXXXXXXXXXXXXXXXXXX 12345678901  21
133900030912    |        D  E  S  T  I  N  A  T  A  R  I  O          |      |S  22
134000030912RICO|RAGIONE SOCIALE     LOCALITA'           PR CAP      |      |O  23
1341000309122345 1234567890123456789 1234567890123456789 XX 123456789 1234 P x  24
134200931206LINEE PARTENZA RICHIESTE:                                           25
134300030912RIEPILOGO PER OPERATORE:        SIGLA OPERATORE       SPEDIZIONI    26
134400030912                                ---------------       ----------    27
134500040621XXXXXXXXXXXXXXXXXXXX           ** ELENCO SPEDIZIONI OLTRE 20 COLLI  28
