000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020418      * FNLS32R *----------------------------------------------------*
000300020418      *         - BOLLE PER OPERATORE
000400020418      *--------------------------------------------------------------*
000500020418      *  ??PASSAGGIO PARAMETRI:?
000600020418      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
000700020418      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
000800020418      *  DS1FPG   FLAG PROGRAMMA
000900020418      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001000020418      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001100020418      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001200020418      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
001300020418      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
001400020418      *--------------------------------------------------------------*
001500020610     FFNBLP70L  IF   E           K DISK
001600921001     FTABEL00F  IF   E           K DISK
001700921001     FAZORG01L  IF   E           K DISK
001800921002     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001900921002     F                                     USROPN
002000950308     F*
002100950308     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
002200921001     D SK              S             66    DIM(7) CTDATA PERRCD(1)
002300921002     D*
002400011214     D OPE             S              1    DIM(99)
002500011214     D BOL             S              9  0 DIM(99)
002600011214     D WOP             S              1    DIM(99)
002700011214     D WBO             S              9  0 DIM(99)
002800921002     D*
002900950308     D L1              S              3  0 DIM(30)
003000020418     D L6              S              3  0 DIM(30)
003100950308     D LV2             S              3  0 DIM(30)
003200950308     D LIN             S              3  0 DIM(30)
003300020418
003400020418     d sav_t06         S              3  0 dim(30)
003401110502
003402110502     D $pog            s              3    dim(250)
003500020418
003600040831     d TotPPT          s              9  0 inz
003700020418     d wc              s              3  0
003800020610     d lnp             s                   like(ds1lnp)
003801110502     d carp            s              3
003802110504     D carp_d          s                   like(§05des)
003803110503     d ii              s              3  0
003804110504     D FLD_Area        s                   like(orgcar)
003900020418
004000941227     D*
004100941227     D* PARAMETRI PASSATI DAI PGM RICHIAMANTI
004200950308     D DSLS01        E DS
004300900613     D KPJBA         E DS
004400900710     D                 DS
004500941227     D  BLPAAS                 1      4  0
004600941227     D  BLPMGS                 5      8  0
004700941227     D  BLPDSP                 1      8  0
004800921001     D DS3L          E DS
004900970707     D DS5P          E DS
004901110504     D DS05          E DS
005000910808     D CNCR80        E DS
005100910808     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005200950308     D* DS PER TRUL06R - CARICAMENTO £X
005300950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005400950314     D  T06                    1     90  0
005500950314     D                                     DIM(30)
005501110502     D TRUL31DS      E DS
005600000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
005700000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
005800040831      *
005900040831     d OG143         e ds                  inz
006000921001     C*---------------------------------------------------------------*
006100921001     C*    RIEPILOGO INDICATORI
006200921001     C*---------------------------------------------------------------*
006300921001     C* 03    - IMMESSA DATA AL
006400040831     C* 11    -           STAMPA PER 1L
006500040831     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
006600040831     C* 19    - Stampa Totale bolle Poste
006700921002     C* 30/32 - DI COMODO
006800040831     C* 69    - in lookup OPE  x ricerca operatore
006900040831     C* 99    - in chain TABEL x reperimento tab.5P
007000000000     C*---------------------------------------------------------------*
007100000000     C     *ENTRY        PLIST
007200000000     C                   PARM                    KPJBA
007300950308     C                   MOVEL     KPJBU         DSLS01
007301110504
007302110504      /FREE
007303110504       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
007304110504      /END-FREE
007305110504
007400950308     C*
007500950308     C                   MOVEL     DS1ADA        AASP              4 0
007600950308     C                   MOVE      DS1ADA        MGSP              4 0
007601110502     C                   eval      carp=%subst(ds1flo:1:3)
007700011214     C*
007800000000     C                   Z-ADD     1             CODUT
007900910808     C                   CALL      'X§PARUT'
008000910808     C                   PARM                    UT§DSE
008100000000     C                   MOVEL     RAGUT         RSUT             20
008200910808     C                   MOVEL     REC80         CNCR80
008300900511     C*---------------------------------------------------------------*
008400020610     C* ACCESSO FNBLP70L
008500900509     C     KBLP          KLIST
008600020610     c                   kfld                    lnp
008700941227     C                   KFLD                    AASP
008800941227     C                   KFLD                    MGSP
008900941227     C*---------------------------------------------------------------*
009000941227     C*
009100941227     C* 03 ON  - IMMESSA DATA SPEDIZIONE AL
009200950308    1C     DS1AAL        IFNE      0
009300950308     C     DS1AAL        ANDNE     DS1ADA
009400900524     C                   SETON                                        03
009500941227    1C                   END
009501110504     c* 05 on  - Elaborazione per area
009502110504     c                   if        carp<>*blanks
009503110504     c                   seton                                        05
009504110504     C                   MOVEL     '05'          KCOD
009505110504     C                   MOVEL(p)  carp          KKEY
009506110504     c                   clear                   ds05
009507110504     c     ktab1         chain     tabel00f
009508110504     c                   if        %found(tabel00f)
009509110504     c                   movel     tbluni        ds05
009510110504     c                   eval      carp_d=§05des
009511110504     c                   endif
009512110504     c                   endif
009600950308     C*
009700950308     C* CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
009800950308     C                   MOVEL     'P'           DS1TBO
009900950308     C                   CLEAR                   DS1PGM
010000950308     C                   MOVEL     'FNLS32R'     DS1PGM
010100950308     C                   EXSR      COMUNE
010200921001     C*
010300921001     C                   SETON                                        LR
010400020212     C*****************************************************************
010500020212     C*  PARTE COMUNE STAMPE GIORNALIERE
010600020212     C*
010700020212     C*  . REPERIMENTO 'SIMFEL'
010800020212     C*  . CARICAMENTO '£1'
010900020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
011000020212     C*  . CODA DI STAMPA ('£OUTQ')
011100020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
011200020212     C*  . MODULO PER LA STAMPA ('£MOD')
011300020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
011400020212     C*****************************************************************
011500020212     C     COMUNE        BEGSR
011600020212     C*
011700020212     C*****************************************************************
011800020212     C*       RIEPILOGO INDICATORI:
011900020212     C* 11    -           STAMPA PER 1L
012000020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
012100020212     C*****************************************************************
012200020212     C                   Z-ADD     1             CODUT
012300020212     C*
012400020212     C* Reperimento parametri aziendali
012500020212     C                   CALL      'X§PARUT'
012600020212     C                   PARM                    UT§DSE
012700020212     C                   MOVEL     RAGUT         RSUT             20
012800020212     C                   MOVEL     REC80         CNCR80
012900020212     C*
013000020212     C* Chiave di accesso per il file tabelle
013100020212     C     KTAB1         KLIST
013200020212     C                   KFLD                    CODUT
013300020212     C                   KFLD                    KCOD
013400020212     C                   KFLD                    KKEY
013500020212     C     KTAB2         KLIST
013600020212     C                   KFLD                    CODUT
013700020212     C                   KFLD                    KCOD
013800020212     C*
013900020212     C     *LIKE         DEFINE    TBLCOD        KCOD
014000020212     C     *LIKE         DEFINE    TBLKEY        KKEY
014100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
014200020212     C*
014300020212     C* DETERMINO DATA E ORA DI ESECUZIONE
014400020212     C                   TIME                    WTIME            14 0
014500020212     C                   MOVE      WTIME         WDATE             8 0
014600020212     C                   MOVEL     WTIME         UTIME             6 0
014700020212     C*
014800020418     C* CARICO '£1'
014900020212     C                   CLEAR                   KPJBU
015000020212     C                   CLEAR                   DSUL06
015100020212     C                   MOVE      '£1'          D06COD
015200020212     C                   MOVEL     SIMFEL        D06KEY
015300050802     C                   MOVE      'S'           D06KEY
015400050802     C                   MOVEL     ds1ada        D06drf
015500020212     C                   MOVEL     DSUL06        KPJBU
015600020212     C                   CALL      'TRUL06R'
015700020212     C                   PARM                    KPJBA
015800020212     C                   MOVEL     KPJBU         DSUL06
015900020418     c                   movea     t06           sav_t06
016000020418
016100020418      * se in arrivo carico la £6
016200020418 b1  c                   if        ds1tbo = 'A'
016300020418     c                   clear                   wc
016400020418 b2  c                   do        30            wa
016500020422     c                   if        sav_t06(wa) = *zeros
016600020422     c                   leave
016700020422     c                   endif
016800020418     c                   clear                   kpjbu
016900020418     c                   clear                   dsul06
017000020418     c                   eval      d06cod = '£6'
017100050802     C                   MOVEL     ds1ada        D06drf
017200020418     c                   movel     sav_t06(wa)   d06key
017300050802     C                   MOVE      'S'           D06KEY
017400020418     c                   eval      d06esc = 'S'
017500020419     c                   movel     dsul06        kpjbu
017600020418     c                   call      'TRUL06R'
017700020418     c                   parm                    kpjba
017800020418     c                   movel     kpjbu         dsul06
017900020418 b3  c                   do        30            wb
018000020418     c                   if        t06(wb) = *zeros
018100020418     c                   leave
018200020418     c                   endif
018300020418     c                   add       1             wc
018400020418     c                   z-add     t06(wb)       l6(wc)
018500020418 e3  c                   enddo
018600020418 e2  c                   enddo
018700020212     C*
018800020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
018900020212     C                   Z-ADD     *ZERO         WB
019000020418 b2  C                   DO        29            WA
019100020418     C     sav_T06(WA)   LOOKUP    L6                                     30
019200020212     C  N30              ADD       1             WB
019300020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
019400020418 e2  C                   ENDDO
019500020418 x1  C                   ELSE
019600020418     C                   MOVEA     sav_T06       L1
019700020418 e1  C                   ENDIF
019800020212     C*
019900020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
020000020212     C                   MOVEA     L1            LIN
020100020212     C                   CLEAR                   L1
020200020212     C                   Z-ADD     *ZERO         WB                3 0
020300020212     C     1             DO        30            WPT               3 0
020400020212     C     LIN(WPT)      IFNE      0
020500020212     C                   CLEAR                   FNLSC4
020600020212     C                   Z-ADD     LIN(WPT)      IC4LIN
020700020212     C                   MOVE      'B'           IC4FLG
020800020212     C                   MOVEL     FNLSC4        KPJBU
020900020212     C                   CALL      'FNLSC4R1'
021000020212     C                   PARM                    KPJBA
021100050802     C                   PARM                    ds1ada
021200020212     C                   MOVEL     KPJBU         FNLSC4
021300020212     C     OC4ESE        IFEQ      'S'
021400020212     C                   ADD       1             WB
021500020212     C                   MOVE      LIN(WPT)      L1(WB)
021600020212     C                   ENDIF
021700020212     C                   ENDIF
021800020212     C                   ENDDO
021900070103     C*
022000070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
022100070103     C                   Z-ADD     *ZERO         WB                3 0
022200070103     C                   DO        29            WA                3 0
022300070103     C     L1(WA)        IFNE      *ZEROS
022400070103     C     L1(WA)        ANDNE     SIMFEL
022500070103     C                   ADD       1             WB
022600070103     C                   Z-ADD     L1(WA)        LV2(WB)
022700070103     C                   ENDIF
022800070103     C                   ENDDO
022900020212     C*
023000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
023100020212     C*
023200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
023300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
023400020212     C* la stampa e' lanciata da menu
023500020212     C                   MOVEL     '3L'          KCOD
023600020212     C                   MOVEL     DS1PGM        KKEY
023700020212     C                   MOVE      DS1FPG        KKEY
023800020212     C     KTAB1         CHAIN     TABEL00F                           30
023900020212     C  N30              MOVEL     TBLUNI        DS3L
024000020212     C   30              MOVEL     'F'           §3LD1L
024100020212     C   30              MOVEL     ' '           §3LD2L
024200020212     C   30              MOVEL     '*STD    '    §3LDST
024300020212     C*
024400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
024500020212     C                   MOVEL     §3LDST        £MOD             10
024600020212     C                   CLEAR                   LIN
024700020212     C*
024800020212     C* Verifico chi ha lanciato la stampa
024900020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
025000020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
025100020212     C* DS1FGS<>0  --->  si proviene da menu
025200020212     C     DS1FGS        IFEQ      0
025300020212     C*
025400020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
025500020212     C* lancio, voglio la stampa;  invece
025600020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
025700020212     C*
025800020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
025900020212     C* creare stampa per 1°lvl. (se stessa)
026000020212     C     DS1LNP        IFEQ      SIMFEL
026100020212     C*
026200020212     C* Il flag §3LD1L ha i seguenti significati:
026300020212     C*    ' ' = tutti i dati
026400020212     C*    '1' = solo filiale di 1' livello
026500020212     C     §3LD1L        IFEQ      '1'
026600020212     C                   SETON                                        11
026700020212     C                   END
026800020212     C*
026900020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
027000020212     C* nella stampa
027100020212     C*
027200020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
027300020212     C  N11              MOVEA     L1            LIN
027400020212     C   11              MOVEA     SIMFEL        LIN(1)
027500020212     C*
027600020212     C* IMPOSTO LA STAMPANTE:
027700020212     C* STAMPANTE TABULATI
027800020212     C                   MOVEL     '5P'          KCOD
027900020212     C                   MOVEL     'T'           W004A             4
028000020212     C                   MOVE      LIN(1)        W004A
028100020212     C                   MOVEL(P)  W004A         KKEY
028200020212     C     KTAB1         CHAIN     TABEL00F                           99
028300020212     C     *IN99         IFEQ      *OFF
028400020212     C                   MOVEL     TBLUNI        DS5P
028500020212     C                   MOVEL     §5POTQ        £OUTQ            10
028600020212     C                   END
028700020212     C*
028800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
028900020212     C                   EXSR      LEGGI
029000020212     C*
029100020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
029200020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
029300020212     C                   ELSE
029400020212     C*
029500020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
029600020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
029700020212     C*
029800020212     C* Il significato del fla §3LD2L e' il seguente:
029900020212     C*    ' ' = tutti i dati
030000020212     C*    'F' = per ogni 2' livello
030100020212     C     §3LD2L        IFEQ      'F'
030200020212     C                   SETON                                        14
030300020212     C                   ENDIF
030400020212     C*
030500020212     C                   Z-ADD     1             WB
030600020212     C                   MOVEA     *ZEROS        LIN
030700020212     C  N14              MOVEA     L1            LIN
030800020212     C*
030900020212     C* Lancio per tutti i 2° lvl
031000020212     C     DS1LNP        IFEQ      0
031100020212     C*
031200020212     C* LV2 contiene tutte le filiali di 2° lvl
031300020212     C     LV2(WB)       DOWNE     0
031400020212      *
031500020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
031600020212     C                   CLEAR                   FNLSC4
031700020212     C                   Z-ADD     LV2(WB)       IC4LIN
031800020212     C                   MOVEL     FNLSC4        KPJBU
031900020212     C                   CALL      'FNLSC4R1'
032000020212     C                   PARM                    KPJBA
032100050802     C                   PARM                    ds1ada
032200020212     C                   MOVEL     KPJBU         FNLSC4
032300020212      *
032400020212     C     OC4PT1        IFEQ      'S'
032500020212     C     §3LPT         ANDEQ     'N'
032600020212     C                   GOTO      NOLEGG
032700020212     C                   ENDIF
032800020212      *
032900020212     C     OC4EXP        IFEQ      'S'
033000020212     C     §3LEXP        ANDEQ     'N'
033100020212     C                   GOTO      NOLEGG
033200020212     C                   ENDIF
033300020212      *
033400020212     C     OC4DPD        IFEQ      'S'
033500020212     C     §3LDPD        ANDEQ     'N'
033600020212     C                   GOTO      NOLEGG
033700020212     C                   ENDIF
033800020212      *
033900020212     C     OC4BAR        IFEQ      'S'
034000020212     C     §3LBAR        ANDEQ     'N'
034100020212     C                   GOTO      NOLEGG
034200020212     C                   ENDIF
034300020212      *
034400020212     C     OC4FED        IFEQ      'S'
034500020212     C     §3LFED        ANDEQ     'N'
034600020212     C                   GOTO      NOLEGG
034700020212     C                   ENDIF
034800020212      *
034900020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
035000020212     C*  dati degli altri 2° lvl
035100020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
035200020212     C   14              MOVEL     LV2(WB)       LIN(1)
035300020212     C*
035400020212     C* IMPOSTO LA STAMPANTE
035500020212     C* STAMPANTE TABULATI
035600020212     C                   MOVEL     '5P'          KCOD
035700020212     C                   MOVEL     'T'           W004A             4
035800020212     C                   MOVE      LV2(WB)       W004A
035900020212     C                   MOVEL(P)  W004A         KKEY
036000020212     C     KTAB1         CHAIN     TABEL00F                           99
036100020212     C     *IN99         IFEQ      *OFF
036200020212     C                   MOVEL     TBLUNI        DS5P
036300020212     C                   MOVEL     §5POTQ        £OUTQ            10
036400020212     C                   END
036500020212     C*
036600020212     C* Compongo il modulo di stampa con il codice della filiale
036700020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
036800020212     C*
036900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
037000020212     C                   EXSR      LEGGI
037100020212     C*
037200020212     C     NOLEGG        TAG
037300020212     C*
037400020212     C                   ADD       1             WB
037500020212     C                   ENDDO
037600020212     C*
037700020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
037800020212     C                   ELSE
037900020212     C*
038000020212     C* Lancio solo per la 2° lvl richiesta
038100020212     C   14              MOVEL     DS1LNP        LIN(1)
038200020212     C*
038300020212     C* STAMPANTE
038400020212     C* STAMPANTE TABULATI
038500020212     C                   MOVEL     '5P'          KCOD
038600020212     C                   MOVEL     'T'           W004A             4
038700020212     C                   MOVE      DS1LNP        W004A
038800020212     C                   MOVEL(P)  W004A         KKEY
038900020212     C     KTAB1         CHAIN     TABEL00F                           99
039000020212     C     *IN99         IFEQ      *OFF
039100020212     C                   MOVEL     TBLUNI        DS5P
039200020212     C                   MOVEL     §5POTQ        £OUTQ            10
039300020212     C                   END
039400020212     C*
039500020212     C* MODULO STAMPA
039600020212     C******               MOVE DS1LNP    £MOD
039700020212     C*
039800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
039900020212     C                   EXSR      LEGGI
040000020212     C*
040100020212     C                   ENDIF
040200020212     C                   ENDIF
040300020212     C*
040400020212     C                   ELSE
040500020212     C*
040600020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
040700020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
040800020212     C     DS1LNP        IFEQ      0
040900020212     C                   MOVEA     L1            LIN
040901110502     c* se elaborazione per area devo caricare tutte le linee dell'area
040902110502     c                   if        carp<>*blanks
040903110502     C                   exsr      car_lin
040906110502     c                   endif
041000020212     C* ...altrimenti elaboro solo se stessa
041100020212     C                   ELSE
041200020212     C                   MOVEL     DS1LNP        LIN(1)
041300020212     C                   ENDIF
041400020212     C*
041500020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
041600020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
041700020212     C* PRENDO QUELLA PRESENTE IN KPJBA
041800020212     C     DS1FGS        IFEQ      SIMFEL
041900020212     C                   MOVEL     KQOTD         £OUTQ
042000030627     C**!!!              MOVEL     KQOTD         £OUTQF
042100020212     C                   ELSE
042200020212     C* STAMPANTE TABULATI
042300020212     C                   MOVEL     '5P'          KCOD
042400020212     C                   MOVEL     'T'           W004A             4
042500020212     C                   MOVE      DS1FGS        W004A
042600020212     C                   MOVEL(P)  W004A         KKEY
042700020212     C     KTAB1         CHAIN     TABEL00F                           99
042800020212     C     *IN99         IFEQ      *OFF
042900020212     C                   MOVEL     TBLUNI        DS5P
043000020212     C                   MOVEL     §5POTQ        £OUTQ            10
043100020212     C                   END
043200020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
043300020212     C                   ENDIF
043400020212     C*
043500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
043600020212     C                   EXSR      LEGGI
043700020212     C                   ENDIF
043800020212     C*
043900020212     C                   ENDSR
043901110502     C**************************************************************************
043902110502     C* Caricamento schiera LIN con le linee dell'area
043903110502     C**************************************************************************
043904110502     C     car_lin       BEGSR
043926110504
043927110504     c                   eval      ii=1
043928110504     c                   clear                   lin
043929110504     C/EXEC SQL
043930110504     C+ DECLARE AZORG CURSOR FOR
043931110504     C+ SELECT ORGFIL
043932110504     C+ FROM AZORG00F
043933110504     C+ WHERE ORGFVA <> '*' and ORGFAG <> 'V'
043934110504     C+ and orgcar=:carp
043935110504     c+ order by orgfil
043936110504     C+ FOR READ ONLY
043937110504     C/END-EXEC
043938110504     C
043939110504     C/EXEC SQL
043940110504     C+ OPEN AZORG
043941110504     C/END-EXEC
043942110504     C                   DOU       SqlCod <> 0
043943110504      **         lettura cursore
043944110504     C/EXEC SQL
043945110504     C+ FETCH NEXT FROM AZORG INTO :FLD_Area
043946110504     C/END-EXEC
043947110504     C                   if        SqlCod = 100
043948110504     c                   leave
043949110504     C                   else
043950110504     **
043952110504     c                   move      fld_area      lin(ii)
043953110504     c                   add       1             ii
043954110504     c*
043955110504     C                   ENDif
043956110504
043957110504     C                   ENDDO
043958110504     C/EXEC SQL
043959110504     C+ Close AZORG
043960110504     C/END-EXEC
043970110504     c
043971110502     c                   endsr
044000950308     C**************************************************************************
044100950308     C* LETTURA BOLLE PER CREAZIONE STAMPA
044200950308     C**************************************************************************
044300921001     C     LEGGI         BEGSR
044400950308     C*
044500950308     C* IMPOSTAZIONE STAMPANTE E TIPO MODULO
044600950308     C                   MOVEA     £MOD          CMD(33)
044700950308     C                   MOVEA     £OUTQ         CMD(51)
044800950308     C*
044900921001     C* ESEGUO OVRPRTF
045000950308     C                   Z-ADD     82            LUNG             15 5
045100950308     C                   MOVEL     *BLANKS       COMMAN           82
045200950308     C                   MOVEA     CMD           COMMAN
045300921001     C                   CALL      'QCMDEXC'                            H1
045400921001     C                   PARM                    COMMAN
045500921001     C                   PARM                    LUNG
045600921001     C*
045700921001     C                   OPEN      QSYSPRT
045800980210     C                   SETON                                        OF
045900950308     C                   Z-ADD     1             C                 3 0
046000011220     C*
046100011220     C* ripulisco le schiere dove verranno memorizzati i dati
046200011220     C* (*HIVAL servirà all'ordinamento sequenziale dei codici tramite
046300011220     C*  SORTA)
046400011220     C                   MOVEL     *HIVAL        OPE
046500011220     C                   CLEAR                   BOL
046600040831     c                   clear                   TotPPT
046700921001     C*
046800941227    1C     LIN(C)        DOWNE     0
046900020724     c                   z-add     1             cc                4 0
047000020724     c
047001110502     c* Se lancio per area nella sav_t06 devo mettere la £1 di lin(C)
047002110502     c                   if        carp<>*blanks
047003110502     c                   exsr      car_savt06
047004110502     c                   endif
047005110502     c                   exsr      elabora
049800921002     C*
049900921002     C                   EXSR      STAMPA
050000921002     C*
050100921002     C                   ADD       1             C
050200011220     C*
050300011220     C* ripulisco le schiere dove verranno memorizzati i dati
050400011220     C                   MOVEL     *HIVAL        OPE
050500011220     C                   CLEAR                   BOL
050600040831     c                   clear                   TotPPT
050700011220     C*
050800941227    1C                   END
050900900517     C*
051000921002     C                   CLOSE     QSYSPRT
051100921002     C*
051200921002     C                   ENDSR
051201110502     C**************************************************************************
051202110502     C*    caricamento sav_t06 per lancio per area
051203110502     C**************************************************************************
051204110502     C     car_savt06    BEGSR
051205110502     C                   CLEAR                   KPJBU
051206110502     C                   CLEAR                   DSUL06
051207110502     C                   MOVE      'PP'          D06COD
051208110502     C                   MOVEL     lin(c)        D06KEY
051209110503     C                   MOVE      'S'           D06KEY
051210110502     C                   MOVEL     ds1ada        D06drf
051211110502     C                   MOVEL     DSUL06        KPJBU
051212110502     C                   CALL      'TRUL06R'
051213110502     C                   PARM                    KPJBA
051214110502     C                   MOVEL     KPJBU         DSUL06
051215110502     c                   movea     t06           sav_t06
051216110502     C                   ENDSR
051217110502     C**************************************************************************
051218110502     C*    Elaboro le bolle
051219110502     C**************************************************************************
051220110502     C     ELABORA       BEGSR
051221110502    2c                   dow       sav_t06(cc)>0
051222110502     c                   eval      lnp = sav_t06(cc)
051223110502     C*
051224110502     C* ELABORAZIONE
051225110502     C     KBLP          SETLL     FNBLP000
051226110502     C*
051227110502     C*!!03              READ      FNBLP000                               30
051228110502     c   03lnp           reade     fnblp000                               30
051229110502     C  N03KBLP          READE     FNBLP000                               30
051230110502     C*
051231110502    3C     *IN30         DOWEQ     '0'
051232110502     C* SE IMMESSO AL E DATA LETTA > VADO A FINE FILE
051233110502    4C   03BLPDSP        IFGT      DS1AAL
051234110502     C                   SETON                                        30
051235110502    4C                   END
051236110502     C*
051237110502    4C  N30BLPFLB        IFEQ      LIN(C)
051238110502     C                   EXSR      ELAB
051239110502    4C                   END
051240110502     C*
051241110502     C   03
051242110502     C*!!ANN30              READ      FNBLP000                               30
051243110502     cann30lnp           reade     fnblp000                               30
051244110502     C  N03KBLP          READE     FNBLP000                               30
051245110502    3C                   ENDdo
051246110502     c                   add       1             cc
051247110502     c                   enddo
051248110502     c                   endsr
051300950308     C**************************************************************************
051400950308     C*    CALCOLO BOLLE PER OPERATORE
051500950308     C**************************************************************************
051600900626     C     ELAB          BEGSR
051700921002     C*
051800921002     C* MEMORIZZO BOLLA DELL'OPERATORE
051900011214     C                   Z-ADD     1             I                 3 0
052000900626     C     BLPSOP        LOOKUP    OPE(I)                                 31
052100011214     C*
052200011214     C     *IN31         IFEQ      *OFF
052300011214     C     *HIVAL        LOOKUP    OPE(I)                                 31
052400011214     C                   MOVEL     BLPSOP        OPE(I)
052500011214     C                   ENDIF
052600900626     C*
052700011214     C                   ADD       1             BOL(I)
052800040831      *
052900040831      * Verifico se bolla Poste
053000040831if  1c                   if        BLPlnp <> ORGfil
053100040831     c     BLPlnp        chain     AZORG
053200040831if  2c                   if        %found(AZORG01L)
053300040831     c                   movel     ORGde3        og143
053400040831x   2c                   else
053500040831     c                   clear                   og143
053600040831e   2c                   endif
053700040831e   1c                   endif
053800040831      * Conteggio le bolle Poste
053900040831if  1c                   if        §OGntw = 'PPT'
054000040831     c                   add       1             TotPPT
054100040831e   1c                   endif
054200900626     C*
054300900626     C                   ENDSR
054400950308     C**************************************************************************
054500950308     C*    STAMPA BOLLA
054600950308     C**************************************************************************
054700900517     C     STAMPA        BEGSR
054800950308     C*
054900921002     C                   Z-ADD     LIN(C)        COMFIL            3 0
055000921002     C     COMFIL        CHAIN     AZORG                              32
055100921002     C  N32              MOVEL     ORGDES        DESFIL           20
055200921002     C   32              MOVEL     *BLANKS       DESFIL
055300921002     C*
055400921002     C** STAMPA TESTATA
055500921002     C  NOF              EXCEPT    TES3
055600921002     C  NOF              EXCEPT    TES2
055700921002     C*
055800011214DO  1C   OF              DO
055900921002     C                   EXCEPT    TESTA
056000921002     C                   EXCEPT    TES2
056100921002     C                   SETOFF                                       OF
056200011214E   1C                   ENDDO
056300011214     C*
056400921002     C** STAMPA DETTAGLIO
056500921002     C*
056600921002     C* SOMMA TOTALE
056700921002     C                   XFOOT     BOL           TOTBOL            9 0
056800921002     C*
056900011214IF  1C     TOTBOL        IFNE      0
057000011214     C*
057100011214     C                   EXSR      SORTAS
057200900710     C*
057300011214DO  2C     1             DO        99            I
057400900710     C*
057500011214IF  3C     BOL(I)        IFNE      0
057600900710     C*
057700011214DO  4C   OF              DO
057800921002     C                   EXCEPT    TESTA
057900921002     C                   EXCEPT    TES2
058000921002     C                   SETOFF                                       OF
058100011214E   4C                   ENDDO
058200921002     C*
058300900517     C                   EXCEPT    DET
058400011214E   3C                   ENDIF
058500921002     C*
058600011214E   2C                   ENDDO
058700921002     C* TOTALI FINALI
058800040831     c                   eval      *in19 = (TotPPT > *zeros)
058900011214IF  2C     TOTBOL        IFNE      0
059000900926     C                   EXCEPT    FIN
059100011214E   2C                   ENDIF
059200921002     C*
059300921002     C                   MOVEA     *ZEROS        BOL
059400921002     C                   Z-ADD     0             TOTBOL
059500040831     c                   clear                   TotPPT
059600040831     c                   clear                   ORGfil
059700011214E   1C                   ENDIF
059800921002     C*
059900900517     C                   ENDSR
060000011214     C**************************************************************************
060100011214     C*    ORDINA SCHIERE
060200011214     C**************************************************************************
060300011214     C     SORTAS        BEGSR
060400011214     C*
060500011214     C                   MOVEL     OPE           WOP
060600011214     C                   SORTA     WOP
060700011214     C                   CLEAR                   WBO
060800011214     C*
060900011214DO  1C     1             DO        99            J                 3 0
061000011214     C                   Z-ADD     1             Y                 3 0
061100011214     C                   MOVEL     *OFF          *IN69
061200011214     C     WOP(J)        LOOKUP    OPE(Y)                                 69
061300011214IF  2C     *IN69         IFEQ      *ON
061400011214     C                   Z-ADD     BOL(Y)        WBO(J)
061500011214E   2C                   ENDIF
061600011214E   1C                   ENDDO
061700011214     C*
061800011214     C* Re-imposto i dati nelle schiere da stampare
061900011214     C                   MOVEL     WOP           OPE
062000011214     C                   Z-ADD     WBO           BOL
062100011214     C*
062200011214     C* Ripulisco le posizioni con *HIVAL (non utilizzate)
062300011214     C                   Z-ADD     1             J
062400011214     C                   MOVEL     *OFF          *IN69
062500011214     C     *HIVAL        LOOKUP    OPE(J)                                 69
062600011214     C   69              MOVEA     *BLANKS       OPE(J)
062700011214     C*
062800011214     C                   ENDSR
062900950308     C**************************************************************************
063000921001     OQSYSPRT   E            TESTA            02
063100900517     O                       SK(1)               66
063200921001     O                 N03   SK(2)              132
063300921001     O                  03   SK(3)              132
063400900517     O                       RSUT                20
063500910808     O                                           21 '/'
063600910808     O                       SIMFEL              24
063700950308     O                       DS1GDA              83 '  /  /    '
063800950308     O                  03   DS1GAL              97 '  /  /    '
063900950308     O                       WDATE              121 '  /  /    '
064000921001     O                       PAGE          Z    132
064100910404     O          E            TESTA            03
064200950313     O                       KNSIF               10
064300950313     O                       KNMUS               22
064301110504     o                  05                    +  40 'Cod.AREA:'
064302110504     o                  05   carp             +   1
064303110504     o                  05                    +   0 '-'
064304110504     o                  05   carp_d           +   0
064400950308     O                       UTIME              120 '0 :  :  '
064500921002     O          E            TES3        3
064600921002     O                                          120 ' '
064700921002     O          E            TES2        2
064800921002     O                       SK(5)               66
064900960926     O                       LIN(C)              26
065000921002     O                                         +  0 '-'
065100921002     O                       DESFIL            +  0
065200921002     O          E            TES2        2
065300921001     O                       SK(4)               66
065400900517     O          E            DET         1  1
065500011214     O                       OPE(I)               8
065600900626     O                       BOL(I)        Z     26
065700900926     O          E            FIN         2
065800921002     O                                           24 '------------------------'
065900921002     O                                           29 '-----'
066000040831     O               19                       +   0 '------------------------'
066100040831     O               19                       +   0 '-----------'
066200921002     O          E            FIN         2
066300921002     O                                           15 'TOTALE :'
066400900926     O                       TOTBOL        Z     26
066500040831     O               19                       +   5 'DI CUI BOLLE POSTE :'
066600040831     O               19      TOTPPT        Z  +   1
066700921002     O          E            FIN         2
066800921002     O                                           24 '------------------------'
066900921002     O                                           29 '-----'
067000040831     O               19                       +   0 '------------------------'
067100040831     O               19                       +   0 '-----------'
067200950308     O**************************************************************************
067300950308**
067400950308OVRPRTF FILE(QSYSPRT) FORMTYPE('          ') OUTQ(          ) USRDTA('Bol.Operat
067500950308')
067600900517**
067700921002XXXXXXXXXXXXXXXXXXXX/XXX             ** BOLLE ESEGUITE PER OPERATO  1
067800950308RE IL  XX/XX/XXXX **        FNLS32R          XX.XX.XXXX   PAG.XXXX  2
067900950308RE DAL XX/XX/XXXX AL XX/XX/XXXX **  FNLS32R  XX.XX.XXXX   PAG.XXXX  3
068000921002SIGLA OPERATORE  NUMERO BOLLE                                       4
068100960926P.O. DI BOLLETTAZIONE: XXX
068200950308
068300950308
