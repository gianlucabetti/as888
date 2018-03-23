000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200010326      * FNLSE2R *----------------------------------------------------*
000300010312      *         - LISTA PREPAGATI PARTITI
000400010312      *--------------------------------------------------------------*
000500010327      *___________________________________________________________________*
000600010327      *                                                                   *
000700010329      * ATTENZIONE!!!!!! NELL'ULTIMA POSIZIONE DELLA KPJBA (502)          *
000800010329      *                  PASSO UN FLAG PER LA STAMPA DEI PREPAGATI        *
000900010329      *                  ANCORA DA CONFERMARE                             *
001000010327      *___________________________________________________________________*
001100010312      *  ??PASSAGGIO PARAMETRI:?
001200010312      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001300010312      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001400010312      *  DS1FPG   FLAG PROGRAMMA
001500010312      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001600010312      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001700010312      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001800010312      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
001900010312      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002000010312      *--------------------------------------------------------------*
002100010717     FFNBLP31L  IF   E           K DISK
002200021203     F*FNAPD01L  IF   E           K DISK
002300021203     FFIAPD01L  IF   E           K DISK
002400010312     FFIAR601L  IF   E           K DISK
002500010327     FAZORG01L  IF   E           K DISK
002600010312     FTABEL00F  IF   E           K DISK
002700010326     FFNLSE2P   O    E             PRINTER OFLIND(*IN98)
002800010326     F                                     USROPN
002900010327     FFNLSE2P1  O    E             PRINTER OFLIND(*IN97)
003000010327     F                                     USROPN
003100010312      *
003200950308     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
003300010312      *
003400950308     D L1              S              3  0 DIM(30)
003500020418     D L6              S              3  0 DIM(30)
003600950308     D LV2             S              3  0 DIM(30)
003700950308     D LIN             S              3  0 DIM(30)
003800010326     D KEY             S              2    DIM(99)
003900020418     d sav_t06         S              3  0 dim(30)
004000020418
004100020418     d wc              s              3  0
004200020418
004300010312      *----------------------------------
004400010312      * PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
004500010312      *----------------------------------
004600010312     D WLBDAT          DS
004700010312     D  G02DAT                 1      8  0
004800010312     D  G02INV                 9     16  0
004900010312     D  G02ERR                17     17
005000010312     D  G02TGI                18     22  0
005100010312      *
005200010312      * PARAMETRI PASSATI DAI PGM RICHIAMANTI
005300950308     D DSLS01        E DS
005400900613     D KPJBA         E DS
005500010329     D  SINO                 502    502
005600900710     D                 DS
005700941227     D  BLPAAS                 1      4  0
005800941227     D  BLPMGS                 5      8  0
005900941227     D  BLPDSP                 1      8  0
006000021203
006100021203     D kpdpdr          S                   LIKE(APDpdr)
006200021203     D kpdtip          S                   LIKE(APDtip)
006300121206     d carp            s              3
006400121206     D carp_d          s                   like(§05des)
006500121206     d ii              s              3  0
006600121206     D FLD_Area        s                   like(orgcar)
006700121206
006800121206     D DS05          E DS
006900121206     D DS3L          E DS
007000970707     D DS5P          E DS
007100010326     D DS3A          E DS
007200910808     D CNCR80        E DS
007300910808     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007400010312      * DS PER TRUL06R - CARICAMENTO £X
007500950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007600950314     D  T06                    1     90  0
007700950314     D                                     DIM(30)
007800010312      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
007900010312     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
008000010312      *---------------------------------------------------------------*
008100010312      *    RIEPILOGO INDICATORI
008200010312      *---------------------------------------------------------------*
008300010312      * 03    - IMMESSA DATA AL
008400010329      * 04    - IMMESSO SI PER STAMPA PREPAGARI ANCORA DA CONFERMARE
008500121213      * 05    - Stampa per cod area
008600121213      * 06    - Prepagato annullato
008700010312      * 30/32 - DI COMODO
008800010329      * 96    - Stampa testata una volta
008900010327      * 97    - Fine pagine
009000010312      * 98    - Fine pagine
009100010312      * 99    - Stampa testata una volta
009200010312      *---------------------------------------------------------------*
009300000000     C     *ENTRY        PLIST
009400000000     C                   PARM                    KPJBA
009500950308     C                   MOVEL     KPJBU         DSLS01
009600010312      *
009700000000     C                   Z-ADD     1             CODUT
009800910808     C                   CALL      'X§PARUT'
009900910808     C                   PARM                    UT§DSE
010000010312     C                   MOVEL     RAGUT         RSUT
010100910808     C                   MOVEL     REC80         CNCR80
010200010329      * reperisco data e ora
010300010329     C                   TIME                    W0140            14 0
010400010329      * DATA
010500010329     C                   MOVE      W0140         WDTGIO
010600010329      * ORA
010700010329     C                   MOVEL     W0140         WORA
010800010329      * ANCORA DA CONFERMARE
010900010329     C     SINO          IFNE      *BLANKS
011000010329     C                   SETON                                        04
011100010329     C                   ENDIF
011200010312      *---------------------------------------------------------------*
011300010312      * ACCESSO FIAR601L
011400010312     C     KAR6          KLIST
011500010326     C                   KFLD                    BLPAAS
011600010326     C                   KFLD                    BLPLNP
011700010326     C                   KFLD                    BLPNRS
011800010326     C                   KFLD                    BLPNSP
011900010326     C                   KFLD                    KTRC
012000021203
012100021203     C     Kfapd         klist
012200021203     C                   kfld                    kpdtip
012300021203     C                   kfld                    kpdpdr
012400010326     C                   MOVEL     '1'           KTRC
012500010312      *
012600010326     C     *LIKE         DEFINE    AR6TRC        KTRC
012700010326      *
012800010326     C                   Z-ADD     0             X                 2 0
012900010326     C                   MOVEL     '3A'          KCOD
013000010326     C     KTAB2         SETLL     TABEL00F
013100010326     C                   DO        *HIVAL
013200010326     C     KTAB2         READE     TABEL00F                               30
013300010326     C     *IN30         IFEQ      *ON
013400010326     C                   LEAVE
013500010326     C                   ENDIF
013600010326     C     TBLFLG        IFNE      *BLANKS
013700010326     C                   ITER
013800010326     C                   ENDIF
013900010326     C                   ADD       1             X                 2 0
014000010326     C                   MOVEL     TBLUNI        DS3A
014100010326     C                   MOVEL     §3ATB1        EFFE              1
014200010326     C     EFFE          IFEQ      'F'
014300010326     C                   MOVEL     TBLKEY        KEY(X)
014400010326     C                   ENDIF
014500010326     C                   ENDDO
014600010312      *
014700010312     C     DS1GAL        IFEQ      *ZEROS
014800010312     C                   Z-ADD     DS1GDA        DS1GAL
014900010312     C                   Z-ADD     DS1ADA        DS1AAL
015000010312     C                   ENDIF
015100010312      *
015200010312     C                   Z-ADD     DS1GDA        STPDDA
015300010312     C                   Z-ADD     DS1GAL        STPDAL
015400121206     C                   eval      carp=%subst(ds1flo:1:3)
015500010312      *
015600010312      * 03 ON  - IMMESSA DATA AL
015700010312     C     DS1AAL        IFNE      0
015800010312     C     DS1AAL        ANDNE     DS1ADA
015900010312     C                   SETON                                        03
016000010312     C                   END
016100121206     c* 05 on  - Elaborazione per area
016200121206     c                   if        carp<>*blanks
016300121206     c                   seton                                        05
016400121206     C                   MOVEL     '05'          KCOD
016500121206     C                   MOVEL(p)  carp          KKEY
016600121206     c                   clear                   ds05
016700121206     c     ktab1         chain     tabel00f
016800121206     c                   if        %found(tabel00f)
016900121206     c                   movel     tbluni        ds05
017000121206     c                   eval      carp_d=§05des
017100121206     c                   endif
017200121206     c                   endif
017300010312      *---------------------------------------------------------------*
017400010312      *
017500010312      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
017600950308     C                   MOVEL     'P'           DS1TBO
017700950308     C                   CLEAR                   DS1PGM
017800010326     C                   MOVEL     'FNLSE2R'     DS1PGM
017900950308     C                   EXSR      COMUNE
018000010312      *
018100921001     C                   SETON                                        LR
018200020212     C*****************************************************************
018300020212     C*  PARTE COMUNE STAMPE GIORNALIERE
018400020212     C*
018500020212     C*  . REPERIMENTO 'SIMFEL'
018600020212     C*  . CARICAMENTO '£1'
018700020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
018800020212     C*  . CODA DI STAMPA ('£OUTQ')
018900020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
019000020212     C*  . MODULO PER LA STAMPA ('£MOD')
019100020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
019200020212     C*****************************************************************
019300020212     C     COMUNE        BEGSR
019400020212     C*
019500020212     C*****************************************************************
019600020212     C*       RIEPILOGO INDICATORI:
019700020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
019800020212     C* 11    -           STAMPA PER 1L
019900020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
020000020212     C*****************************************************************
020100020212     C                   Z-ADD     1             CODUT
020200020212     C*
020300020212     C* Reperimento parametri aziendali
020400020212     C                   CALL      'X§PARUT'
020500020212     C                   PARM                    UT§DSE
020600020212     C                   MOVEL     RAGUT         RSUT             20
020700020212     C                   MOVEL     REC80         CNCR80
020800020212     C*
020900020212     C* Chiave di accesso per il file tabelle
021000020212     C     KTAB1         KLIST
021100020212     C                   KFLD                    CODUT
021200020212     C                   KFLD                    KCOD
021300020212     C                   KFLD                    KKEY
021400020212     C     KTAB2         KLIST
021500020212     C                   KFLD                    CODUT
021600020212     C                   KFLD                    KCOD
021700020212     C*
021800020212     C     *LIKE         DEFINE    TBLCOD        KCOD
021900020212     C     *LIKE         DEFINE    TBLKEY        KKEY
022000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
022100020212     C*
022200020212     C* DETERMINO DATA E ORA DI ESECUZIONE
022300020212     C                   TIME                    WTIME            14 0
022400020212     C                   MOVE      WTIME         WDATE             8 0
022500020212     C                   MOVEL     WTIME         UTIME             6 0
022600020212     C*
022700020418     C* CARICO '£1'
022800020212     C                   CLEAR                   KPJBU
022900020212     C                   CLEAR                   DSUL06
023000020212     C                   MOVE      '£1'          D06COD
023100020212     C                   MOVEL     SIMFEL        D06KEY
023200050802     C                   MOVE      'S'           D06KEY
023300050802     C                   MOVEL     ds1ada        D06drf
023400020212     C                   MOVEL     DSUL06        KPJBU
023500020212     C                   CALL      'TRUL06R'
023600020212     C                   PARM                    KPJBA
023700020212     C                   MOVEL     KPJBU         DSUL06
023800020418     c                   movea     t06           sav_t06
023900020418
024000020418      * se in arrivo carico la £6
024100020418 b1  c                   if        ds1tbo = 'A'
024200020418     c                   clear                   wc
024300020418 b2  c                   do        30            wa
024400020422     c                   if        sav_t06(wa) = *zeros
024500020422     c                   leave
024600020422     c                   endif
024700020418     c                   clear                   kpjbu
024800020418     c                   clear                   dsul06
024900020418     c                   eval      d06cod = '£6'
025000020418     c                   movel     sav_t06(wa)   d06key
025100050802     C                   MOVE      'S'           D06KEY
025200020418     c                   eval      d06esc = 'S'
025300050802     C                   MOVEL     ds1ada        D06drf
025400020419     c                   movel     dsul06        kpjbu
025500020418     c                   call      'TRUL06R'
025600020418     c                   parm                    kpjba
025700020418     c                   movel     kpjbu         dsul06
025800020418 b3  c                   do        30            wb
025900020418     c                   if        t06(wb) = *zeros
026000020418     c                   leave
026100020418     c                   add       1             wc
026200020419     c                   z-add     t06(wb)       l6(wc)
026300020418     c                   endif
026400020418 e3  c                   enddo
026500020418 e2  c                   enddo
026600020418
026700020418     C* ESCLUDO DA £1 LE FILIALI DELLA £6
026800020212     C                   Z-ADD     *ZERO         WB
026900020418 b2  C                   DO        29            WA
027000020418     C     sav_T06(WA)   LOOKUP    L6                                     30
027100020212     C  N30              ADD       1             WB
027200020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
027300020418 e2  C                   ENDDO
027400020418 x1  C                   ELSE
027500020418     C                   MOVEA     sav_T06       L1
027600020418 e1  C                   ENDIF
027700020212     C*
027800020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
027900020212     C                   MOVEA     L1            LIN
028000020212     C                   CLEAR                   L1
028100020212     C                   Z-ADD     *ZERO         WB                3 0
028200020212     C     1             DO        30            WPT               3 0
028300020212     C     LIN(WPT)      IFNE      0
028400020212     C                   CLEAR                   FNLSC4
028500020212     C                   Z-ADD     LIN(WPT)      IC4LIN
028600020212     C                   MOVE      'B'           IC4FLG
028700020212     C                   MOVEL     FNLSC4        KPJBU
028800020212     C                   CALL      'FNLSC4R1'
028900020212     C                   PARM                    KPJBA
029000050802     C                   PARM                    ds1ada
029100020212     C                   MOVEL     KPJBU         FNLSC4
029200020212     C     OC4ESE        IFEQ      'S'
029300020212     C                   ADD       1             WB
029400020212     C                   MOVE      LIN(WPT)      L1(WB)
029500020212     C                   ENDIF
029600020212     C                   ENDIF
029700020212     C                   ENDDO
029800070103     C*
029900070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
030000070103     C                   Z-ADD     *ZERO         WB                3 0
030100070103     C                   DO        29            WA                3 0
030200070103     C     L1(WA)        IFNE      *ZEROS
030300070103     C     L1(WA)        ANDNE     SIMFEL
030400070103     C                   ADD       1             WB
030500070103     C                   Z-ADD     L1(WA)        LV2(WB)
030600070103     C                   ENDIF
030700070103     C                   ENDDO
030800020212     C*
030900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
031000020212     C*
031100020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
031200020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
031300020212     C* la stampa e' lanciata da menu
031400020212     C                   MOVEL     '3L'          KCOD
031500020212     C                   MOVEL     DS1PGM        KKEY
031600020212     C                   MOVE      DS1FPG        KKEY
031700020212     C     KTAB1         CHAIN     TABEL00F                           30
031800020212     C  N30              MOVEL     TBLUNI        DS3L
031900020212     C   30              MOVEL     'F'           §3LD1L
032000020212     C   30              MOVEL     ' '           §3LD2L
032100020212     C   30              MOVEL     '*STD    '    §3LDST
032200020212     C*
032300020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
032400020212     C                   MOVEL     §3LDST        £MOD             10
032500020212     C                   CLEAR                   LIN
032600020212     C*
032700020212     C* Verifico chi ha lanciato la stampa
032800020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
032900020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
033000020212     C* DS1FGS<>0  --->  si proviene da menu
033100020212     C     DS1FGS        IFEQ      0
033200020212     C*
033300020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
033400020212     C* lancio, voglio la stampa;  invece
033500020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
033600020212     C*
033700020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
033800020212     C* creare stampa per 1°lvl. (se stessa)
033900020212     C     DS1LNP        IFEQ      SIMFEL
034000020212     C*
034100020212     C* Il flag §3LD1L ha i seguenti significati:
034200020212     C*    ' ' = tutti i dati
034300020212     C*    '1' = solo filiale di 1' livello
034400020212     C     §3LD1L        IFEQ      '1'
034500020212     C                   SETON                                        11
034600020212     C                   END
034700020212     C*
034800020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
034900020212     C* nella stampa
035000020212     C*
035100020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
035200020212     C  N11              MOVEA     L1            LIN
035300020212     C   11              MOVEA     SIMFEL        LIN(1)
035400020212     C*
035500020212     C* IMPOSTO LA STAMPANTE:
035600020212     C* STAMPANTE TABULATI
035700020212     C                   MOVEL     '5P'          KCOD
035800020212     C                   MOVEL     'T'           W004A             4
035900020212     C                   MOVE      LIN(1)        W004A
036000020212     C                   MOVEL(P)  W004A         KKEY
036100020212     C     KTAB1         CHAIN     TABEL00F                           99
036200020212     C     *IN99         IFEQ      *OFF
036300020212     C                   MOVEL     TBLUNI        DS5P
036400020212     C                   MOVEL     §5POTQ        £OUTQ            10
036500020212     C                   END
036600020212     C*
036700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
036800020212     C                   EXSR      LEGGI
036900020212     C*
037000020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
037100020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
037200020212     C                   ELSE
037300020212     C*
037400020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
037500020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
037600020212     C*
037700020212     C* Il significato del fla §3LD2L e' il seguente:
037800020212     C*    ' ' = tutti i dati
037900020212     C*    'F' = per ogni 2' livello
038000020212     C     §3LD2L        IFEQ      'F'
038100020212     C                   SETON                                        14
038200020212     C                   ENDIF
038300020212     C*
038400020212     C                   Z-ADD     1             WB
038500020212     C                   MOVEA     *ZEROS        LIN
038600020212     C  N14              MOVEA     L1            LIN
038700020212     C*
038800020212     C* Lancio per tutti i 2° lvl
038900020212     C     DS1LNP        IFEQ      0
039000020212     C*
039100020212     C* LV2 contiene tutte le filiali di 2° lvl
039200020212     C     LV2(WB)       DOWNE     0
039300020212      *
039400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
039500020212     C                   CLEAR                   FNLSC4
039600020212     C                   Z-ADD     LV2(WB)       IC4LIN
039700020212     C                   MOVEL     FNLSC4        KPJBU
039800020212     C                   CALL      'FNLSC4R1'
039900020212     C                   PARM                    KPJBA
040000050802     C                   PARM                    ds1ada
040100020212     C                   MOVEL     KPJBU         FNLSC4
040200020212      *
040300020212     C     OC4PT1        IFEQ      'S'
040400020212     C     §3LPT         ANDEQ     'N'
040500020212     C                   GOTO      NOLEGG
040600020212     C                   ENDIF
040700020212      *
040800020212     C     OC4EXP        IFEQ      'S'
040900020212     C     §3LEXP        ANDEQ     'N'
041000020212     C                   GOTO      NOLEGG
041100020212     C                   ENDIF
041200020212      *
041300020212     C     OC4DPD        IFEQ      'S'
041400020212     C     §3LDPD        ANDEQ     'N'
041500020212     C                   GOTO      NOLEGG
041600020212     C                   ENDIF
041700020212      *
041800020212     C     OC4BAR        IFEQ      'S'
041900020212     C     §3LBAR        ANDEQ     'N'
042000020212     C                   GOTO      NOLEGG
042100020212     C                   ENDIF
042200020212      *
042300020212     C     OC4FED        IFEQ      'S'
042400020212     C     §3LFED        ANDEQ     'N'
042500020212     C                   GOTO      NOLEGG
042600020212     C                   ENDIF
042700020212      *
042800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
042900020212     C*  dati degli altri 2° lvl
043000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
043100020212     C   14              MOVEL     LV2(WB)       LIN(1)
043200020212     C*
043300020212     C* IMPOSTO LA STAMPANTE
043400020212     C* STAMPANTE TABULATI
043500020212     C                   MOVEL     '5P'          KCOD
043600020212     C                   MOVEL     'T'           W004A             4
043700020212     C                   MOVE      LV2(WB)       W004A
043800020212     C                   MOVEL(P)  W004A         KKEY
043900020212     C     KTAB1         CHAIN     TABEL00F                           99
044000020212     C     *IN99         IFEQ      *OFF
044100020212     C                   MOVEL     TBLUNI        DS5P
044200020212     C                   MOVEL     §5POTQ        £OUTQ            10
044300020212     C                   END
044400020212     C*
044500020212     C*
044600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
044700020212     C                   EXSR      LEGGI
044800020212     C*
044900020212     C     NOLEGG        TAG
045000020212     C*
045100020212     C                   ADD       1             WB
045200020212     C                   ENDDO
045300020212     C*
045400020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
045500020212     C                   ELSE
045600020212     C*
045700020212     C* Lancio solo per la 2° lvl richiesta
045800020212     C   14              MOVEL     DS1LNP        LIN(1)
045900020212     C*
046000020212     C* STAMPANTE
046100020212     C* STAMPANTE TABULATI
046200020212     C                   MOVEL     '5P'          KCOD
046300020212     C                   MOVEL     'T'           W004A             4
046400020212     C                   MOVE      DS1LNP        W004A
046500020212     C                   MOVEL(P)  W004A         KKEY
046600020212     C     KTAB1         CHAIN     TABEL00F                           99
046700020212     C     *IN99         IFEQ      *OFF
046800020212     C                   MOVEL     TBLUNI        DS5P
046900020212     C                   MOVEL     §5POTQ        £OUTQ            10
047000020212     C                   END
047100020212     C*
047200020212     C*
047300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
047400020212     C                   EXSR      LEGGI
047500020212     C*
047600020212     C                   ENDIF
047700020212     C                   ENDIF
047800020212     C*
047900020212     C                   ELSE
048000020212     C*
048100020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
048200020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
048300020212     C     DS1LNP        IFEQ      0
048400020212     C                   MOVEA     L1            LIN
048500121206     c* se elaborazione per area devo caricare tutte le linee dell'area
048600121206     c                   if        carp<>*blanks
048700121206     C                   exsr      car_lin
048800121206     c                   endif
048900020212     C* ...altrimenti elaboro solo se stessa
049000020212     C                   ELSE
049100020212     C                   MOVEL     DS1LNP        LIN(1)
049200020212     C                   ENDIF
049300020212     C*
049400020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
049500020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
049600020212     C* PRENDO QUELLA PRESENTE IN KPJBA
049700020212     C     DS1FGS        IFEQ      SIMFEL
049800020212     C                   MOVEL     KQOTD         £OUTQ
049900030627     C**!!!              MOVEL     KQOTD         £OUTQF
050000020212     C                   ELSE
050100020212     C* STAMPANTE TABULATI
050200020212     C                   MOVEL     '5P'          KCOD
050300020212     C                   MOVEL     'T'           W004A             4
050400020212     C                   MOVE      DS1FGS        W004A
050500020212     C                   MOVEL(P)  W004A         KKEY
050600020212     C     KTAB1         CHAIN     TABEL00F                           99
050700020212     C     *IN99         IFEQ      *OFF
050800020212     C                   MOVEL     TBLUNI        DS5P
050900020212     C                   MOVEL     §5POTQ        £OUTQ            10
051000020212     C                   END
051100020212     C                   ENDIF
051200020212     C*
051300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
051400020212     C                   EXSR      LEGGI
051500020212     C                   ENDIF
051600020212     C*
051700020212     C                   ENDSR
051800010312      **************************************************************************
051900010312      * LETTURA ORM PER CREAZIONE STAMPA
052000010312      **************************************************************************
052100921001     C     LEGGI         BEGSR
052200010329      *
052300010329     C                   MOVEL     'FNLSE2P'     FILEST            8
052400010312      *
052500010312      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
052600010329     C                   MOVEA     £MOD          CMD(34)
052700010329     C                   MOVEA     £OUTQ         CMD(52)
052800010329     C                   MOVEA     FILEST        CMD(14)
052900010329      *
053000010312      * ESEGUO OVRPRTF
053100950308     C                   Z-ADD     82            LUNG             15 5
053200950308     C                   MOVEL     *BLANKS       COMMAN           82
053300950308     C                   MOVEA     CMD           COMMAN
053400921001     C                   CALL      'QCMDEXC'                            H1
053500921001     C                   PARM                    COMMAN
053600921001     C                   PARM                    LUNG
053700010312      *
053800010326     C                   OPEN      FNLSE2P
053900010327      *
054000010329     C     *IN04         IFEQ      *ON
054100010329     C                   MOVEL     'FNLSE2P1'    FILEST            8
054200010329     C                   MOVEA     FILEST        CMD(14)
054300010327      * ESEGUO OVRPRTF
054400010327     C                   Z-ADD     82            LUNG             15 5
054500010327     C                   MOVEL     *BLANKS       COMMAN           82
054600010327     C                   MOVEA     CMD           COMMAN
054700010327     C                   CALL      'QCMDEXC'                            H1
054800010327     C                   PARM                    COMMAN
054900010327     C                   PARM                    LUNG
055000010327     C                   OPEN      FNLSE2P1
055100010327     C                   ENDIF
055200010327      *
055300010329     C                   SETOFF                                       9996
055400950308     C                   Z-ADD     1             C                 3 0
055500010312      *
055600941227    1C     LIN(C)        DOWNE     0
055700010327      * CODICE FILIALE E DESCRIZIONE
055800010327     C                   CLEAR                   FIL
055900010327     C                   CLEAR                   DESFIL
056000010327     C     LIN(C)        CHAIN     AZORG01L                           32
056100121206     c
056200121206     c* Per le stampe giornaliere metto la filiale in testata
056300121206     c* oppure se richeista una sola filiale
056400121206     c                   if        ds1fgs=0 or lin(2)=0
056500010327     C     *IN32         IFEQ      *OFF
056600010327     C                   Z-ADD     LIN(C)        FIL
056700010327     C                   MOVEL     ORGDES        DESFIL
056800010327     C                   ENDIF
056900121206     C                   ENDIF
057000010312      *
057100010312      * ELABORAZIONE
057200010717     C     LIN(C)        SETLL     FNBLP31L
057300010312     C                   DO        *HIVAL
057400010717     C     LIN(C)        READE     FNBLP31L                               30
057500010312      *
057600010312     C     *IN30         IFEQ      *ON
057700010312     C                   LEAVE
057800010312     C                   ENDIF
057900010329      * Se è partito
058000010329     C     BLPPDR        IFNE      *ZEROS
058100010329      * Escludo con data borderò inferiore data da
058200010326     C     BLPDBR        IFLT      DS1ADA
058300010312     C                   ITER
058400010312     C                   ENDIF
058500010326      * Escludo con data borderò maggiore data al (se immessa)
058600010326     C   03BLPDBR        IFGT      DS1AAL
058700010312     C                   ITER
058800010312     C                   ENDIF
058900010329     C                   ENDIF
059000010329      * Escludo se non porto franco
059100010326     C                   Z-ADD     1             X
059200010326     C     BLPCBO        LOOKUP    KEY(X)                                 30
059300010326     C     *IN30         IFEQ      *ON
059400010312      *
059500010312     C                   EXSR      STAMPA
059600010326     C                   ENDIF
059700010312      *
059800010312     C                   ENDDO
059900010312      *
060000921002     C                   ADD       1             C
060100941227    1C                   END
060200010312      *
060300010326     C                   CLOSE     FNLSE2P
060400010329      *
060500010329     C     *IN04         IFEQ      *ON
060600010329     C                   CLOSE     FNLSE2P1
060700010329     C                   ENDIF
060800010312      *
060900921002     C                   ENDSR
061000010312      **************************************************************************
061100010312      *    STAMPA LISTA
061200010312      **************************************************************************
061300900517     C     STAMPA        BEGSR
061400010326      *
061500010326     C                   CLEAR                   STPRSC
061600010326     C                   CLEAR                   STPDIV
061700010326     C                   CLEAR                   STPIFT
061800010326     C                   CLEAR                   STPFIV
061900010326     C                   CLEAR                   STPNFT
062000010326     C                   CLEAR                   STPDFT
062100010327     C                   CLEAR                   STPDSP
062200010327      *
062300010327      * STAMPA TESTATA
062400010312     C     *IN99         IFEQ      *OFF
062500010312     C                   WRITE     TESTA
062600010312     C                   SETON                                        99
062700010312     C                   ENDIF
062800010312      *
062900010312      * Ragione sociale Autotrasportatore
063000021203     C*    BLPPDR        CHAIN     FNAPD01L                           30
063100021205     C                   move      BlpPdr        kpdpdr
063200021203     C                   move      'A'           kpdtip
063300021203     C     kfapd         chain     fiapd01L                           30
063400010312     C     *IN30         IFEQ      *OFF
063500010312     C                   MOVEL     APDRSC        STPRSC
063600010312     C                   ENDIF
063700010312      *
063800010312      * Fattura
063900010312     C     KAR6          CHAIN     FIAR601L                           30
064000010312     C     *IN30         IFEQ      *OFF
064100010326     C     AR6NFT        ANDGT     0
064200010326     C                   MOVE      AR6DIV        STPDIV
064300010312     C                   Z-ADD     AR6IFT        STPIFT
064400010312     C                   Z-ADD     AR6FIV        STPFIV
064500010312     C                   Z-ADD     AR6NFT        STPNFT
064600010327      *
064700010327      * Giro data fattura
064800010312     C                   CLEAR                   WLBDAT
064900010312     C                   Z-ADD     AR6DFT        G02INV
065000010321     C                   MOVEL     '3'           G02ERR
065100010312     C                   CALL      'XSRDA8'
065200010312     C                   PARM                    WLBDAT
065300010312     C                   Z-ADD     G02DAT        STPDFT
065400010327      *
065500010327      * Giro data spedizione
065600010327     C                   CLEAR                   WLBDAT
065700010327     C                   Z-ADD     BLPDSP        G02INV
065800010327     C                   MOVEL     '3'           G02ERR
065900010327     C                   CALL      'XSRDA8'
066000010327     C                   PARM                    WLBDAT
066100010327     C                   Z-ADD     G02DAT        STPDSP
066200121213     c* Verifico se annllato
066300121213     c                   setoff                                       06
066400121213     c                   if        blpcbo='M '
066500121213     c                   seton                                        06
066600121213     c                   endif
066700010327      * Prepagati partiti
066800010327     C     BLPPDR        IFNE      *ZEROS
066900010312     C   98              WRITE     TESTA
066901131219     c                   setoff                                       98
067000010327     C                   WRITE     RIGA
067100010327     C                   ENDIF
067200010327      * Prepagati non confermati
067300010327     C     BLPPDR        IFEQ      *ZEROS
067400010329     C     *IN04         ANDEQ     *ON
067500010329      * STAMPA TESTATA
067600010329     C     *IN96         IFEQ      *OFF
067700010329     C                   WRITE     TESTA1
067800010329     C                   SETON                                        96
067900010329     C                   ENDIF
068000010327     C   97              WRITE     TESTA1
068001131219     c                   setoff                                       97
068100010327     C                   WRITE     RIGA1
068200010327     C                   ENDIF
068300010326     C                   ENDIF
068400010312      *
068500900517     C                   ENDSR
068600121206     C**************************************************************************
068700121206     C* Caricamento schiera LIN con le linee dell'area
068800121206     C**************************************************************************
068900121206     C     car_lin       BEGSR
069000121206
069100121206     c                   eval      ii=1
069200121206     c                   clear                   lin
069300121206     C/EXEC SQL
069400121206     C+ DECLARE AZORG CURSOR FOR
069500121206     C+ SELECT ORGFIL
069600121206     C+ FROM AZORG00F
069700121206     C+ WHERE ORGFVA <> '*' and ORGFAG <> 'V'
069800121206     C+ and orgcar=:carp
069900121206     c+ order by orgfil
070000121206     C+ FOR READ ONLY
070100121206     C/END-EXEC
070200121206     C
070300121206     C/EXEC SQL
070400121206     C+ OPEN AZORG
070500121206     C/END-EXEC
070600121206     C                   DOU       SqlCod <> 0
070700121206      **         lettura cursore
070800121206     C/EXEC SQL
070900121206     C+ FETCH NEXT FROM AZORG INTO :FLD_Area
071000121206     C/END-EXEC
071100121206     C                   if        SqlCod = 100
071200121206     c                   leave
071300121206     C                   else
071400121206     **
071500121206     c                   move      fld_area      lin(ii)
071600121206     c                   add       1             ii
071700121206     c*
071800121206     C                   ENDif
071900121206
072000121206     C                   ENDDO
072100121206     C*/EXEC SQL
072200121206     C*+ Close AZORG
072300121206     C*/END-EXEC
072400121206     c
072500121206     c                   endsr
072600010329**
072700010329OVRPRTF FILE(        ) FORMTYPE('          ') OUTQ(          ) USRDTA('Prep.Part
072800010329')
