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
003500110502
003600110502     D $pog            s              3    dim(250)
003700020418
003800040831     d TotPPT          s              9  0 inz
003900020418     d wc              s              3  0
004000020610     d lnp             s                   like(ds1lnp)
004100110502     d carp            s              3
004200110504     D carp_d          s                   like(§05des)
004300110503     d ii              s              3  0
004400110504     D FLD_Area        s                   like(orgcar)
004500020418
004600941227     D*
004700941227     D* PARAMETRI PASSATI DAI PGM RICHIAMANTI
004800950308     D DSLS01        E DS
004900900613     D KPJBA         E DS
005000900710     D                 DS
005100941227     D  BLPAAS                 1      4  0
005200941227     D  BLPMGS                 5      8  0
005300941227     D  BLPDSP                 1      8  0
005400921001     D DS3L          E DS
005500970707     D DS5P          E DS
005600110504     D DS05          E DS
005700910808     D CNCR80        E DS
005800910808     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005900950308     D* DS PER TRUL06R - CARICAMENTO £X
006000950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006100950314     D  T06                    1     90  0
006200950314     D                                     DIM(30)
006300110502     D TRUL31DS      E DS
006400000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
006500000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
006600040831      *
006700040831     d OG143         e ds                  inz
006800921001     C*---------------------------------------------------------------*
006900921001     C*    RIEPILOGO INDICATORI
007000921001     C*---------------------------------------------------------------*
007100921001     C* 03    - IMMESSA DATA AL
007200040831     C* 11    -           STAMPA PER 1L
007300040831     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
007400040831     C* 19    - Stampa Totale bolle Poste
007500921002     C* 30/32 - DI COMODO
007600040831     C* 69    - in lookup OPE  x ricerca operatore
007700040831     C* 99    - in chain TABEL x reperimento tab.5P
007800000000     C*---------------------------------------------------------------*
007900000000     C     *ENTRY        PLIST
008000000000     C                   PARM                    KPJBA
008100950308     C                   MOVEL     KPJBU         DSLS01
008200110504
008300110504      /FREE
008400110504       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008500110504      /END-FREE
008600110504
008700950308     C*
008800950308     C                   MOVEL     DS1ADA        AASP              4 0
008900950308     C                   MOVE      DS1ADA        MGSP              4 0
009000110502     C                   eval      carp=%subst(ds1flo:1:3)
009100011214     C*
009200000000     C                   Z-ADD     1             CODUT
009300910808     C                   CALL      'X§PARUT'
009400910808     C                   PARM                    UT§DSE
009500000000     C                   MOVEL     RAGUT         RSUT             20
009600910808     C                   MOVEL     REC80         CNCR80
009700900511     C*---------------------------------------------------------------*
009800020610     C* ACCESSO FNBLP70L
009900900509     C     KBLP          KLIST
010000020610     c                   kfld                    lnp
010100941227     C                   KFLD                    AASP
010200941227     C                   KFLD                    MGSP
010300941227     C*---------------------------------------------------------------*
010400941227     C*
010500941227     C* 03 ON  - IMMESSA DATA SPEDIZIONE AL
010600950308    1C     DS1AAL        IFNE      0
010700950308     C     DS1AAL        ANDNE     DS1ADA
010800900524     C                   SETON                                        03
010900941227    1C                   END
011000110504     c* 05 on  - Elaborazione per area
011100110504     c                   if        carp<>*blanks
011200110504     c                   seton                                        05
011300110504     C                   MOVEL     '05'          KCOD
011400110504     C                   MOVEL(p)  carp          KKEY
011500110504     c                   clear                   ds05
011600110504     c     ktab1         chain     tabel00f
011700110504     c                   if        %found(tabel00f)
011800110504     c                   movel     tbluni        ds05
011900110504     c                   eval      carp_d=§05des
012000110504     c                   endif
012100110504     c                   endif
012200950308     C*
012300950308     C* CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
012400950308     C                   MOVEL     'P'           DS1TBO
012500950308     C                   CLEAR                   DS1PGM
012600950308     C                   MOVEL     'FNLS32R'     DS1PGM
012700950308     C                   EXSR      COMUNE
012800921001     C*
012900921001     C                   SETON                                        LR
013000020212     C*****************************************************************
013100020212     C*  PARTE COMUNE STAMPE GIORNALIERE
013200020212     C*
013300020212     C*  . REPERIMENTO 'SIMFEL'
013400020212     C*  . CARICAMENTO '£1'
013500020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
013600020212     C*  . CODA DI STAMPA ('£OUTQ')
013700020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
013800020212     C*  . MODULO PER LA STAMPA ('£MOD')
013900020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
014000020212     C*****************************************************************
014100020212     C     COMUNE        BEGSR
014200020212     C*
014300020212     C*****************************************************************
014400020212     C*       RIEPILOGO INDICATORI:
014500020212     C* 11    -           STAMPA PER 1L
014600020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
014700020212     C*****************************************************************
014800020212     C                   Z-ADD     1             CODUT
014900020212     C*
015000020212     C* Reperimento parametri aziendali
015100020212     C                   CALL      'X§PARUT'
015200020212     C                   PARM                    UT§DSE
015300020212     C                   MOVEL     RAGUT         RSUT             20
015400020212     C                   MOVEL     REC80         CNCR80
015500020212     C*
015600020212     C* Chiave di accesso per il file tabelle
015700020212     C     KTAB1         KLIST
015800020212     C                   KFLD                    CODUT
015900020212     C                   KFLD                    KCOD
016000020212     C                   KFLD                    KKEY
016100020212     C     KTAB2         KLIST
016200020212     C                   KFLD                    CODUT
016300020212     C                   KFLD                    KCOD
016400020212     C*
016500020212     C     *LIKE         DEFINE    TBLCOD        KCOD
016600020212     C     *LIKE         DEFINE    TBLKEY        KKEY
016700020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
016800020212     C*
016900020212     C* DETERMINO DATA E ORA DI ESECUZIONE
017000020212     C                   TIME                    WTIME            14 0
017100020212     C                   MOVE      WTIME         WDATE             8 0
017200020212     C                   MOVEL     WTIME         UTIME             6 0
017300020212     C*
017400020418     C* CARICO '£1'
017500020212     C                   CLEAR                   KPJBU
017600020212     C                   CLEAR                   DSUL06
017700020212     C                   MOVE      '£1'          D06COD
017800020212     C                   MOVEL     SIMFEL        D06KEY
017900050802     C                   MOVE      'S'           D06KEY
018000050802     C                   MOVEL     ds1ada        D06drf
018100020212     C                   MOVEL     DSUL06        KPJBU
018200020212     C                   CALL      'TRUL06R'
018300020212     C                   PARM                    KPJBA
018400020212     C                   MOVEL     KPJBU         DSUL06
018500020418     c                   movea     t06           sav_t06
018600020418
018700020418      * se in arrivo carico la £6
018800020418 b1  c                   if        ds1tbo = 'A'
018900020418     c                   clear                   wc
019000020418 b2  c                   do        30            wa
019100020422     c                   if        sav_t06(wa) = *zeros
019200020422     c                   leave
019300020422     c                   endif
019400020418     c                   clear                   kpjbu
019500020418     c                   clear                   dsul06
019600020418     c                   eval      d06cod = '£6'
019700050802     C                   MOVEL     ds1ada        D06drf
019800020418     c                   movel     sav_t06(wa)   d06key
019900050802     C                   MOVE      'S'           D06KEY
020000020418     c                   eval      d06esc = 'S'
020100020419     c                   movel     dsul06        kpjbu
020200020418     c                   call      'TRUL06R'
020300020418     c                   parm                    kpjba
020400020418     c                   movel     kpjbu         dsul06
020500020418 b3  c                   do        30            wb
020600020418     c                   if        t06(wb) = *zeros
020700020418     c                   leave
020800020418     c                   endif
020900020418     c                   add       1             wc
021000020418     c                   z-add     t06(wb)       l6(wc)
021100020418 e3  c                   enddo
021200020418 e2  c                   enddo
021300020212     C*
021400020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
021500020212     C                   Z-ADD     *ZERO         WB
021600020418 b2  C                   DO        29            WA
021700020418     C     sav_T06(WA)   LOOKUP    L6                                     30
021800020212     C  N30              ADD       1             WB
021900020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
022000020418 e2  C                   ENDDO
022100020418 x1  C                   ELSE
022200020418     C                   MOVEA     sav_T06       L1
022300020418 e1  C                   ENDIF
022400020212     C*
022500020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
022600020212     C                   MOVEA     L1            LIN
022700020212     C                   CLEAR                   L1
022800020212     C                   Z-ADD     *ZERO         WB                3 0
022900020212     C     1             DO        30            WPT               3 0
023000020212     C     LIN(WPT)      IFNE      0
023100020212     C                   CLEAR                   FNLSC4
023200020212     C                   Z-ADD     LIN(WPT)      IC4LIN
023300020212     C                   MOVE      'B'           IC4FLG
023400020212     C                   MOVEL     FNLSC4        KPJBU
023500020212     C                   CALL      'FNLSC4R1'
023600020212     C                   PARM                    KPJBA
023700050802     C                   PARM                    ds1ada
023800020212     C                   MOVEL     KPJBU         FNLSC4
023900020212     C     OC4ESE        IFEQ      'S'
024000020212     C                   ADD       1             WB
024100020212     C                   MOVE      LIN(WPT)      L1(WB)
024200020212     C                   ENDIF
024300020212     C                   ENDIF
024400020212     C                   ENDDO
024500070103     C*
024600070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
024700070103     C                   Z-ADD     *ZERO         WB                3 0
024800070103     C                   DO        29            WA                3 0
024900070103     C     L1(WA)        IFNE      *ZEROS
025000070103     C     L1(WA)        ANDNE     SIMFEL
025100070103     C                   ADD       1             WB
025200070103     C                   Z-ADD     L1(WA)        LV2(WB)
025300070103     C                   ENDIF
025400070103     C                   ENDDO
025500020212     C*
025600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
025700020212     C*
025800020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
025900020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
026000020212     C* la stampa e' lanciata da menu
026100020212     C                   MOVEL     '3L'          KCOD
026200020212     C                   MOVEL     DS1PGM        KKEY
026300020212     C                   MOVE      DS1FPG        KKEY
026400020212     C     KTAB1         CHAIN     TABEL00F                           30
026500020212     C  N30              MOVEL     TBLUNI        DS3L
026600020212     C   30              MOVEL     'F'           §3LD1L
026700020212     C   30              MOVEL     ' '           §3LD2L
026800020212     C   30              MOVEL     '*STD    '    §3LDST
026900020212     C*
027000020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
027100020212     C                   MOVEL     §3LDST        £MOD             10
027200020212     C                   CLEAR                   LIN
027300020212     C*
027400020212     C* Verifico chi ha lanciato la stampa
027500020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
027600020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
027700020212     C* DS1FGS<>0  --->  si proviene da menu
027800020212     C     DS1FGS        IFEQ      0
027900020212     C*
028000020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
028100020212     C* lancio, voglio la stampa;  invece
028200020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
028300020212     C*
028400020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
028500020212     C* creare stampa per 1°lvl. (se stessa)
028600020212     C     DS1LNP        IFEQ      SIMFEL
028700020212     C*
028800020212     C* Il flag §3LD1L ha i seguenti significati:
028900020212     C*    ' ' = tutti i dati
029000020212     C*    '1' = solo filiale di 1' livello
029100020212     C     §3LD1L        IFEQ      '1'
029200020212     C                   SETON                                        11
029300020212     C                   END
029400020212     C*
029500020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
029600020212     C* nella stampa
029700020212     C*
029800020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
029900020212     C  N11              MOVEA     L1            LIN
030000020212     C   11              MOVEA     SIMFEL        LIN(1)
030100020212     C*
030200020212     C* IMPOSTO LA STAMPANTE:
030300020212     C* STAMPANTE TABULATI
030400020212     C                   MOVEL     '5P'          KCOD
030500020212     C                   MOVEL     'T'           W004A             4
030600020212     C                   MOVE      LIN(1)        W004A
030700020212     C                   MOVEL(P)  W004A         KKEY
030800020212     C     KTAB1         CHAIN     TABEL00F                           99
030900020212     C     *IN99         IFEQ      *OFF
031000020212     C                   MOVEL     TBLUNI        DS5P
031100020212     C                   MOVEL     §5POTQ        £OUTQ            10
031200020212     C                   END
031300020212     C*
031400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
031500020212     C                   EXSR      LEGGI
031600020212     C*
031700020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
031800020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
031900020212     C                   ELSE
032000020212     C*
032100020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
032200020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
032300020212     C*
032400020212     C* Il significato del fla §3LD2L e' il seguente:
032500020212     C*    ' ' = tutti i dati
032600020212     C*    'F' = per ogni 2' livello
032700020212     C     §3LD2L        IFEQ      'F'
032800020212     C                   SETON                                        14
032900020212     C                   ENDIF
033000020212     C*
033100020212     C                   Z-ADD     1             WB
033200020212     C                   MOVEA     *ZEROS        LIN
033300020212     C  N14              MOVEA     L1            LIN
033400020212     C*
033500020212     C* Lancio per tutti i 2° lvl
033600020212     C     DS1LNP        IFEQ      0
033700020212     C*
033800020212     C* LV2 contiene tutte le filiali di 2° lvl
033900020212     C     LV2(WB)       DOWNE     0
034000020212      *
034100020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
034200020212     C                   CLEAR                   FNLSC4
034300020212     C                   Z-ADD     LV2(WB)       IC4LIN
034400020212     C                   MOVEL     FNLSC4        KPJBU
034500020212     C                   CALL      'FNLSC4R1'
034600020212     C                   PARM                    KPJBA
034700050802     C                   PARM                    ds1ada
034800020212     C                   MOVEL     KPJBU         FNLSC4
034900020212      *
035000020212     C     OC4PT1        IFEQ      'S'
035100020212     C     §3LPT         ANDEQ     'N'
035200020212     C                   GOTO      NOLEGG
035300020212     C                   ENDIF
035400020212      *
035500020212     C     OC4EXP        IFEQ      'S'
035600020212     C     §3LEXP        ANDEQ     'N'
035700020212     C                   GOTO      NOLEGG
035800020212     C                   ENDIF
035900020212      *
036000020212     C     OC4DPD        IFEQ      'S'
036100020212     C     §3LDPD        ANDEQ     'N'
036200020212     C                   GOTO      NOLEGG
036300020212     C                   ENDIF
036400020212      *
036500020212     C     OC4BAR        IFEQ      'S'
036600020212     C     §3LBAR        ANDEQ     'N'
036700020212     C                   GOTO      NOLEGG
036800020212     C                   ENDIF
036900020212      *
037000020212     C     OC4FED        IFEQ      'S'
037100020212     C     §3LFED        ANDEQ     'N'
037200020212     C                   GOTO      NOLEGG
037300020212     C                   ENDIF
037400020212      *
037500020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
037600020212     C*  dati degli altri 2° lvl
037700020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
037800020212     C   14              MOVEL     LV2(WB)       LIN(1)
037900020212     C*
038000020212     C* IMPOSTO LA STAMPANTE
038100020212     C* STAMPANTE TABULATI
038200020212     C                   MOVEL     '5P'          KCOD
038300020212     C                   MOVEL     'T'           W004A             4
038400020212     C                   MOVE      LV2(WB)       W004A
038500020212     C                   MOVEL(P)  W004A         KKEY
038600020212     C     KTAB1         CHAIN     TABEL00F                           99
038700020212     C     *IN99         IFEQ      *OFF
038800020212     C                   MOVEL     TBLUNI        DS5P
038900020212     C                   MOVEL     §5POTQ        £OUTQ            10
039000020212     C                   END
039100020212     C*
039200020212     C* Compongo il modulo di stampa con il codice della filiale
039300020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
039400020212     C*
039500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
039600020212     C                   EXSR      LEGGI
039700020212     C*
039800020212     C     NOLEGG        TAG
039900020212     C*
040000020212     C                   ADD       1             WB
040100020212     C                   ENDDO
040200020212     C*
040300020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
040400020212     C                   ELSE
040500020212     C*
040600020212     C* Lancio solo per la 2° lvl richiesta
040700020212     C   14              MOVEL     DS1LNP        LIN(1)
040800020212     C*
040900020212     C* STAMPANTE
041000020212     C* STAMPANTE TABULATI
041100020212     C                   MOVEL     '5P'          KCOD
041200020212     C                   MOVEL     'T'           W004A             4
041300020212     C                   MOVE      DS1LNP        W004A
041400020212     C                   MOVEL(P)  W004A         KKEY
041500020212     C     KTAB1         CHAIN     TABEL00F                           99
041600020212     C     *IN99         IFEQ      *OFF
041700020212     C                   MOVEL     TBLUNI        DS5P
041800020212     C                   MOVEL     §5POTQ        £OUTQ            10
041900020212     C                   END
042000020212     C*
042100020212     C* MODULO STAMPA
042200020212     C******               MOVE DS1LNP    £MOD
042300020212     C*
042400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
042500020212     C                   EXSR      LEGGI
042600020212     C*
042700020212     C                   ENDIF
042800020212     C                   ENDIF
042900020212     C*
043000020212     C                   ELSE
043100020212     C*
043200020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
043300020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
043400020212     C     DS1LNP        IFEQ      0
043500020212     C                   MOVEA     L1            LIN
043600110502     c* se elaborazione per area devo caricare tutte le linee dell'area
043700110502     c                   if        carp<>*blanks
043800110502     C                   exsr      car_lin
043900110502     c                   endif
044000020212     C* ...altrimenti elaboro solo se stessa
044100020212     C                   ELSE
044200020212     C                   MOVEL     DS1LNP        LIN(1)
044300020212     C                   ENDIF
044400020212     C*
044500020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
044600020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
044700020212     C* PRENDO QUELLA PRESENTE IN KPJBA
044800110705     C***  DS1FGS        IFEQ      SIMFEL
044900020212     C                   MOVEL     KQOTD         £OUTQ
045000030627     C**!!!              MOVEL     KQOTD         £OUTQF
045100110705     C***                ELSE
045200020212     C* STAMPANTE TABULATI
045300110705     C***                MOVEL     '5P'          KCOD
045400110705     C***                MOVEL     'T'           W004A             4
045500110705     C**+                MOVE      DS1FGS        W004A
045600110705     C**+                MOVEL(P)  W004A         KKEY
045700110705     C***  KTAB1         CHAIN     TABEL00F                           99
045800110705     C***  *IN99         IFEQ      *OFF
045900110705     C***                MOVEL     TBLUNI        DS5P
046000110705     C***                MOVEL     §5POTQ        £OUTQ            10
046100110705     C***                END
046200020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
046300110705     C***                ENDIF
046400020212     C*
046500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046600020212     C                   EXSR      LEGGI
046700020212     C                   ENDIF
046800020212     C*
046900020212     C                   ENDSR
047000110502     C**************************************************************************
047100110502     C* Caricamento schiera LIN con le linee dell'area
047200110502     C**************************************************************************
047300110502     C     car_lin       BEGSR
047400110504
047500110504     c                   eval      ii=1
047600110504     c                   clear                   lin
047700110504     C/EXEC SQL
047800110504     C+ DECLARE AZORG CURSOR FOR
047900110504     C+ SELECT ORGFIL
048000110504     C+ FROM AZORG00F
048100110504     C+ WHERE ORGFVA <> '*' and ORGFAG <> 'V'
048200110504     C+ and orgcar=:carp
048300110504     c+ order by orgfil
048400110504     C+ FOR READ ONLY
048500110504     C/END-EXEC
048600110504     C
048700110504     C/EXEC SQL
048800110504     C+ OPEN AZORG
048900110504     C/END-EXEC
049000110504     C                   DOU       SqlCod <> 0
049100110504      **         lettura cursore
049200110504     C/EXEC SQL
049300110504     C+ FETCH NEXT FROM AZORG INTO :FLD_Area
049400110504     C/END-EXEC
049500110504     C                   if        SqlCod = 100
049600110504     c                   leave
049700110504     C                   else
049800110504     **
049900110504     c                   move      fld_area      lin(ii)
050000110504     c                   add       1             ii
050100110504     c*
050200110504     C                   ENDif
050300110504
050400110504     C                   ENDDO
050500110504     C/EXEC SQL
050600110504     C+ Close AZORG
050700110504     C/END-EXEC
050800110504     c
050900110502     c                   endsr
051000950308     C**************************************************************************
051100950308     C* LETTURA BOLLE PER CREAZIONE STAMPA
051200950308     C**************************************************************************
051300921001     C     LEGGI         BEGSR
051400950308     C*
051500950308     C* IMPOSTAZIONE STAMPANTE E TIPO MODULO
051600950308     C                   MOVEA     £MOD          CMD(33)
051700950308     C                   MOVEA     £OUTQ         CMD(51)
051800950308     C*
051900921001     C* ESEGUO OVRPRTF
052000950308     C                   Z-ADD     82            LUNG             15 5
052100950308     C                   MOVEL     *BLANKS       COMMAN           82
052200950308     C                   MOVEA     CMD           COMMAN
052300921001     C                   CALL      'QCMDEXC'                            H1
052400921001     C                   PARM                    COMMAN
052500921001     C                   PARM                    LUNG
052600921001     C*
052700921001     C                   OPEN      QSYSPRT
052800980210     C                   SETON                                        OF
052900950308     C                   Z-ADD     1             C                 3 0
053000011220     C*
053100011220     C* ripulisco le schiere dove verranno memorizzati i dati
053200011220     C* (*HIVAL servirà all'ordinamento sequenziale dei codici tramite
053300011220     C*  SORTA)
053400011220     C                   MOVEL     *HIVAL        OPE
053500011220     C                   CLEAR                   BOL
053600040831     c                   clear                   TotPPT
053700921001     C*
053800941227    1C     LIN(C)        DOWNE     0
053900020724     c                   z-add     1             cc                4 0
054000020724     c
054100110502     c* Se lancio per area nella sav_t06 devo mettere la £1 di lin(C)
054200110502     c                   if        carp<>*blanks
054300110502     c                   exsr      car_savt06
054400110502     c                   endif
054500110502     c                   exsr      elabora
054600921002     C*
054700921002     C                   EXSR      STAMPA
054800921002     C*
054900921002     C                   ADD       1             C
055000011220     C*
055100011220     C* ripulisco le schiere dove verranno memorizzati i dati
055200011220     C                   MOVEL     *HIVAL        OPE
055300011220     C                   CLEAR                   BOL
055400040831     c                   clear                   TotPPT
055500011220     C*
055600941227    1C                   END
055700900517     C*
055800921002     C                   CLOSE     QSYSPRT
055900921002     C*
056000921002     C                   ENDSR
056100110502     C**************************************************************************
056200110502     C*    caricamento sav_t06 per lancio per area
056300110502     C**************************************************************************
056400110502     C     car_savt06    BEGSR
056500110502     C                   CLEAR                   KPJBU
056600110502     C                   CLEAR                   DSUL06
056700110502     C                   MOVE      'PP'          D06COD
056800110502     C                   MOVEL     lin(c)        D06KEY
056900110503     C                   MOVE      'S'           D06KEY
057000110502     C                   MOVEL     ds1ada        D06drf
057100110502     C                   MOVEL     DSUL06        KPJBU
057200110502     C                   CALL      'TRUL06R'
057300110502     C                   PARM                    KPJBA
057400110502     C                   MOVEL     KPJBU         DSUL06
057500110502     c                   movea     t06           sav_t06
057600110502     C                   ENDSR
057700110502     C**************************************************************************
057800110502     C*    Elaboro le bolle
057900110502     C**************************************************************************
058000110502     C     ELABORA       BEGSR
058100110502    2c                   dow       sav_t06(cc)>0
058200110502     c                   eval      lnp = sav_t06(cc)
058300110502     C*
058400110502     C* ELABORAZIONE
058500110502     C     KBLP          SETLL     FNBLP000
058600110502     C*
058700110502     C*!!03              READ      FNBLP000                               30
058800110502     c   03lnp           reade     fnblp000                               30
058900110502     C  N03KBLP          READE     FNBLP000                               30
059000110502     C*
059100110502    3C     *IN30         DOWEQ     '0'
059200110502     C* SE IMMESSO AL E DATA LETTA > VADO A FINE FILE
059300110502    4C   03BLPDSP        IFGT      DS1AAL
059400110502     C                   SETON                                        30
059500110502    4C                   END
059600110502     C*
059700110502    4C  N30BLPFLB        IFEQ      LIN(C)
059800110502     C                   EXSR      ELAB
059900110502    4C                   END
060000110502     C*
060100110502     C   03
060200110502     C*!!ANN30              READ      FNBLP000                               30
060300110502     cann30lnp           reade     fnblp000                               30
060400110502     C  N03KBLP          READE     FNBLP000                               30
060500110502    3C                   ENDdo
060600110502     c                   add       1             cc
060700110502     c                   enddo
060800110502     c                   endsr
060900950308     C**************************************************************************
061000950308     C*    CALCOLO BOLLE PER OPERATORE
061100950308     C**************************************************************************
061200900626     C     ELAB          BEGSR
061300921002     C*
061400921002     C* MEMORIZZO BOLLA DELL'OPERATORE
061500011214     C                   Z-ADD     1             I                 3 0
061600900626     C     BLPSOP        LOOKUP    OPE(I)                                 31
061700011214     C*
061800011214     C     *IN31         IFEQ      *OFF
061900011214     C     *HIVAL        LOOKUP    OPE(I)                                 31
062000011214     C                   MOVEL     BLPSOP        OPE(I)
062100011214     C                   ENDIF
062200900626     C*
062300011214     C                   ADD       1             BOL(I)
062400040831      *
062500040831      * Verifico se bolla Poste
062600040831if  1c                   if        BLPlnp <> ORGfil
062700040831     c     BLPlnp        chain     AZORG
062800040831if  2c                   if        %found(AZORG01L)
062900040831     c                   movel     ORGde3        og143
063000040831x   2c                   else
063100040831     c                   clear                   og143
063200040831e   2c                   endif
063300040831e   1c                   endif
063400040831      * Conteggio le bolle Poste
063500040831if  1c                   if        §OGntw = 'PPT'
063600040831     c                   add       1             TotPPT
063700040831e   1c                   endif
063800900626     C*
063900900626     C                   ENDSR
064000950308     C**************************************************************************
064100950308     C*    STAMPA BOLLA
064200950308     C**************************************************************************
064300900517     C     STAMPA        BEGSR
064400950308     C*
064500921002     C                   Z-ADD     LIN(C)        COMFIL            3 0
064600921002     C     COMFIL        CHAIN     AZORG                              32
064700921002     C  N32              MOVEL     ORGDES        DESFIL           20
064800921002     C   32              MOVEL     *BLANKS       DESFIL
064900921002     C*
065000921002     C** STAMPA TESTATA
065100921002     C  NOF              EXCEPT    TES3
065200921002     C  NOF              EXCEPT    TES2
065300921002     C*
065400011214DO  1C   OF              DO
065500921002     C                   EXCEPT    TESTA
065600921002     C                   EXCEPT    TES2
065700921002     C                   SETOFF                                       OF
065800011214E   1C                   ENDDO
065900011214     C*
066000921002     C** STAMPA DETTAGLIO
066100921002     C*
066200921002     C* SOMMA TOTALE
066300921002     C                   XFOOT     BOL           TOTBOL            9 0
066400921002     C*
066500011214IF  1C     TOTBOL        IFNE      0
066600011214     C*
066700011214     C                   EXSR      SORTAS
066800900710     C*
066900011214DO  2C     1             DO        99            I
067000900710     C*
067100011214IF  3C     BOL(I)        IFNE      0
067200900710     C*
067300011214DO  4C   OF              DO
067400921002     C                   EXCEPT    TESTA
067500921002     C                   EXCEPT    TES2
067600921002     C                   SETOFF                                       OF
067700011214E   4C                   ENDDO
067800921002     C*
067900900517     C                   EXCEPT    DET
068000011214E   3C                   ENDIF
068100921002     C*
068200011214E   2C                   ENDDO
068300921002     C* TOTALI FINALI
068400040831     c                   eval      *in19 = (TotPPT > *zeros)
068500011214IF  2C     TOTBOL        IFNE      0
068600900926     C                   EXCEPT    FIN
068700011214E   2C                   ENDIF
068800921002     C*
068900921002     C                   MOVEA     *ZEROS        BOL
069000921002     C                   Z-ADD     0             TOTBOL
069100040831     c                   clear                   TotPPT
069200040831     c                   clear                   ORGfil
069300011214E   1C                   ENDIF
069400921002     C*
069500900517     C                   ENDSR
069600011214     C**************************************************************************
069700011214     C*    ORDINA SCHIERE
069800011214     C**************************************************************************
069900011214     C     SORTAS        BEGSR
070000011214     C*
070100011214     C                   MOVEL     OPE           WOP
070200011214     C                   SORTA     WOP
070300011214     C                   CLEAR                   WBO
070400011214     C*
070500011214DO  1C     1             DO        99            J                 3 0
070600011214     C                   Z-ADD     1             Y                 3 0
070700011214     C                   MOVEL     *OFF          *IN69
070800011214     C     WOP(J)        LOOKUP    OPE(Y)                                 69
070900011214IF  2C     *IN69         IFEQ      *ON
071000011214     C                   Z-ADD     BOL(Y)        WBO(J)
071100011214E   2C                   ENDIF
071200011214E   1C                   ENDDO
071300011214     C*
071400011214     C* Re-imposto i dati nelle schiere da stampare
071500011214     C                   MOVEL     WOP           OPE
071600011214     C                   Z-ADD     WBO           BOL
071700011214     C*
071800011214     C* Ripulisco le posizioni con *HIVAL (non utilizzate)
071900011214     C                   Z-ADD     1             J
072000011214     C                   MOVEL     *OFF          *IN69
072100011214     C     *HIVAL        LOOKUP    OPE(J)                                 69
072200011214     C   69              MOVEA     *BLANKS       OPE(J)
072300011214     C*
072400011214     C                   ENDSR
072500950308     C**************************************************************************
072600921001     OQSYSPRT   E            TESTA            02
072700900517     O                       SK(1)               66
072800921001     O                 N03   SK(2)              132
072900921001     O                  03   SK(3)              132
073000900517     O                       RSUT                20
073100910808     O                                           21 '/'
073200910808     O                       SIMFEL              24
073300950308     O                       DS1GDA              83 '  /  /    '
073400950308     O                  03   DS1GAL              97 '  /  /    '
073500950308     O                       WDATE              121 '  /  /    '
073600921001     O                       PAGE          Z    132
073700910404     O          E            TESTA            03
073800950313     O                       KNSIF               10
073900950313     O                       KNMUS               22
074000110504     o                  05                    +  40 'Cod.AREA:'
074100110504     o                  05   carp             +   1
074200110504     o                  05                    +   0 '-'
074300110504     o                  05   carp_d           +   0
074400950308     O                       UTIME              120 '0 :  :  '
074500921002     O          E            TES3        3
074600921002     O                                          120 ' '
074700921002     O          E            TES2        2
074800921002     O                       SK(5)               66
074900960926     O                       LIN(C)              26
075000921002     O                                         +  0 '-'
075100921002     O                       DESFIL            +  0
075200921002     O          E            TES2        2
075300921001     O                       SK(4)               66
075400900517     O          E            DET         1  1
075500011214     O                       OPE(I)               8
075600900626     O                       BOL(I)        Z     26
075700900926     O          E            FIN         2
075800921002     O                                           24 '------------------------'
075900921002     O                                           29 '-----'
076000040831     O               19                       +   0 '------------------------'
076100040831     O               19                       +   0 '-----------'
076200921002     O          E            FIN         2
076300921002     O                                           15 'TOTALE :'
076400900926     O                       TOTBOL        Z     26
076500040831     O               19                       +   5 'DI CUI BOLLE POSTE :'
076600040831     O               19      TOTPPT        Z  +   1
076700921002     O          E            FIN         2
076800921002     O                                           24 '------------------------'
076900921002     O                                           29 '-----'
077000040831     O               19                       +   0 '------------------------'
077100040831     O               19                       +   0 '-----------'
077200950308     O**************************************************************************
077300950308**
077400950308OVRPRTF FILE(QSYSPRT) FORMTYPE('          ') OUTQ(          ) USRDTA('Bol.Operat
077500950308')
077600900517**
077700921002XXXXXXXXXXXXXXXXXXXX/XXX             ** BOLLE ESEGUITE PER OPERATO  1
077800950308RE IL  XX/XX/XXXX **        FNLS32R          XX.XX.XXXX   PAG.XXXX  2
077900950308RE DAL XX/XX/XXXX AL XX/XX/XXXX **  FNLS32R  XX.XX.XXXX   PAG.XXXX  3
078000921002SIGLA OPERATORE  NUMERO BOLLE                                       4
078100960926P.O. DI BOLLETTAZIONE: XXX
078200950308
078300950308
