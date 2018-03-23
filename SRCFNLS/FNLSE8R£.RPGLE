000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200060406      * FNLSE8R *----------------------------------------------------*
000300010312      *-----------------*                                            *
000400060406      *         - LISTA PREPAGATI non incassati
000500010312      *--------------------------------------------------------------*
000600010327      *___________________________________________________________________*
000700010327      *                                                                   *
000800010329      * ATTENZIONE!!!!!! NELL'ULTIMA POSIZIONE DELLA KPJBA (502)          *
000900010329      *                  PASSO UN FLAG PER LA STAMPA DEI PREPAGATI        *
001000010329      *                  ANCORA DA CONFERMARE                             *
001100010327      *___________________________________________________________________*
001200010312      *  ??PASSAGGIO PARAMETRI:?
001300010312      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001400010312      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001500010312      *  DS1FPG   FLAG PROGRAMMA
001600010312      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001700010312      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001800010312      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001900010312      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002000010312      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002100010312      *--------------------------------------------------------------*
002200060407     FFIAR635L  IF   E           K DISK
002300060407     FFNBLP01L  IF   E           K DISK
002400021203     FFIAPD01L  IF   E           K DISK
002500010327     FAZORG01L  IF   E           K DISK
002600010312     FTABEL00F  IF   E           K DISK
002700060407     FFNLSE8P   O    E             PRINTER OFLIND(*IN98)
002800010326     F                                     USROPN
002900060407     FFNLSE8P1  O    E             PRINTER OFLIND(*IN97)
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
004000060407     d datew           S              8s 0
004100020418
004200020418     d wc              s              3  0
004300060407     D nullPtr         S               *
004400060407     D prmAas          S
004500060407     D                                     LIKE(diz$Aas)
004600060407     D prmLnp          S
004700060407     D                                     LIKE(diz$Lnp)
004800060407     D prmNrs          S
004900060407     D                                     LIKE(diz$Nrs)
005000060407     D prmNsp          S
005100060407     D                                     LIKE(diz$Nsp)
005200060407     D prmFiv          S
005300060407     D                                     LIKE(diz$Fiv)
005400060407     D prmDft          S
005500060407     D                                     LIKE(diz$Dft)
005600060407     D prmNft          S
005700060407     D                                     LIKE(diz$Nft)
005800060407     D prmEsito        S             10I 0
005900060407     D cndizion      E DS
006000060407     D                                     BASED(nullPtr)
006100060407     D                                     PREFIX(diz)
006200020418
006300010312      *----------------------------------
006400010312      * PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
006500010312      *----------------------------------
006600010312     D WLBDAT          DS
006700010312     D  G02DAT                 1      8  0
006800010312     D  G02INV                 9     16  0
006900010312     D  G02ERR                17     17
007000010312     D  G02TGI                18     22  0
007100010312      *
007200010312      * PARAMETRI PASSATI DAI PGM RICHIAMANTI
007300950308     D DSLS01        E DS
007400900613     D KPJBA         E DS
007500010329     D  SINO                 502    502
007600900710     D                 DS
007700941227     D  BLPAAS                 1      4  0
007800941227     D  BLPMGS                 5      8  0
007900941227     D  BLPDSP                 1      8  0
008000021203
008100021203     D kpdpdr          S                   LIKE(APDpdr)
008200021203     D kpdtip          S                   LIKE(APDtip)
008300060407     D k_dft           S                   LIKE(ar6dft)
008400921001     D DS3L          E DS
008500970707     D DS5P          E DS
008600010326     D DS3A          E DS
008700910808     D CNCR80        E DS
008800910808     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008900010312      * DS PER TRUL06R - CARICAMENTO £X
009000950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009100950314     D  T06                    1     90  0
009200950314     D                                     DIM(30)
009300010312      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
009400010312     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
009500010312      *---------------------------------------------------------------*
009600010312      *    RIEPILOGO INDICATORI
009700010312      *---------------------------------------------------------------*
009800010312      * 03    - IMMESSA DATA AL
009900010329      * 04    - IMMESSO SI PER STAMPA PREPAGARI ANCORA DA CONFERMARE
010000010312      * 30/32 - DI COMODO
010100060407      * 96    -
010200010327      * 97    - Fine pagine
010300010312      * 98    - Fine pagine
010400010312      * 99    - Stampa testata una volta
010500010312      *---------------------------------------------------------------*
010600000000     C     *ENTRY        PLIST
010700000000     C                   PARM                    KPJBA
010800950308     C                   MOVEL     KPJBU         DSLS01
010900010312      *
011000000000     C                   Z-ADD     1             CODUT
011100910808     C                   CALL      'X§PARUT'
011200910808     C                   PARM                    UT§DSE
011300010312     C                   MOVEL     RAGUT         RSUT
011400910808     C                   MOVEL     REC80         CNCR80
011500010329      * reperisco data e ora
011600010329     C                   TIME                    W0140            14 0
011700010329      * DATA
011800010329     C                   MOVE      W0140         WDTGIO
011900010329      * ORA
012000010329     C                   MOVEL     W0140         WORA
012100010329      * ANCORA DA CONFERMARE
012200010329     C     SINO          IFNE      *BLANKS
012300010329     C                   SETON                                        04
012400010329     C                   ENDIF
012500010312      *---------------------------------------------------------------*
012600060407      * ACCESSO FIAR635L
012700060407     C     KAR635        KLIST
012800060407     C                   KFLD                    lin(C)
012900060407     C                   KFLD                    k_dft
013000060407     C     Kblp          KLIST
013100060407     C                   KFLD                    ar6aas
013200060407     C                   KFLD                    ar6lnp
013300060407     C                   KFLD                    ar6nrs
013400060407     C                   KFLD                    ar6nsp
013500021203
013600021203     C     Kfapd         klist
013700021203     C                   kfld                    kpdtip
013800021203     C                   kfld                    kpdpdr
013900010326      *
014000010326     C                   Z-ADD     0             X                 2 0
014100010326     C                   MOVEL     '3A'          KCOD
014200010326     C     KTAB2         SETLL     TABEL00F
014300010326     C                   DO        *HIVAL
014400010326     C     KTAB2         READE     TABEL00F                               30
014500010326     C     *IN30         IFEQ      *ON
014600010326     C                   LEAVE
014700010326     C                   ENDIF
014800010326     C     TBLFLG        IFNE      *BLANKS
014900010326     C                   ITER
015000010326     C                   ENDIF
015100010326     C                   ADD       1             X                 2 0
015200010326     C                   MOVEL     TBLUNI        DS3A
015300060407     c                   if        §3aucb <> 'N' and §3arbl <> 'C'
015400010326     C                   MOVEL     TBLKEY        KEY(X)
015500010326     C                   ENDIF
015600010326     C                   ENDDO
015700010312      *
015800010312      *---------------------------------------------------------------*
015900010312      *
016000010312      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
016100950308     C                   MOVEL     'P'           DS1TBO
016200950308     C                   CLEAR                   DS1PGM
016300060407     C                   MOVEL     'FNLSE8R'     DS1PGM
016400950308     C                   EXSR      COMUNE
016500010312      *
016600921001     C                   SETON                                        LR
016700020212     C*****************************************************************
016800020212     C*  PARTE COMUNE STAMPE GIORNALIERE
016900020212     C*
017000020212     C*  . REPERIMENTO 'SIMFEL'
017100020212     C*  . CARICAMENTO '£1'
017200020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
017300020212     C*  . CODA DI STAMPA ('£OUTQ')
017400020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
017500020212     C*  . MODULO PER LA STAMPA ('£MOD')
017600020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
017700020212     C*****************************************************************
017800020212     C     COMUNE        BEGSR
017900020212     C*
018000020212     C*****************************************************************
018100020212     C*       RIEPILOGO INDICATORI:
018200020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
018300020212     C* 11    -           STAMPA PER 1L
018400020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
018500020212     C*****************************************************************
018600020212     C                   Z-ADD     1             CODUT
018700020212     C*
018800020212     C* Reperimento parametri aziendali
018900020212     C                   CALL      'X§PARUT'
019000020212     C                   PARM                    UT§DSE
019100020212     C                   MOVEL     RAGUT         RSUT             20
019200020212     C                   MOVEL     REC80         CNCR80
019300020212     C*
019400020212     C* Chiave di accesso per il file tabelle
019500020212     C     KTAB1         KLIST
019600020212     C                   KFLD                    CODUT
019700020212     C                   KFLD                    KCOD
019800020212     C                   KFLD                    KKEY
019900020212     C     KTAB2         KLIST
020000020212     C                   KFLD                    CODUT
020100020212     C                   KFLD                    KCOD
020200020212     C*
020300020212     C     *LIKE         DEFINE    TBLCOD        KCOD
020400020212     C     *LIKE         DEFINE    TBLKEY        KKEY
020500020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
020600020212     C*
020700020212     C* DETERMINO DATA E ORA DI ESECUZIONE
020800020212     C                   TIME                    WTIME            14 0
020900020212     C                   MOVE      WTIME         WDATE             8 0
021000020212     C                   MOVEL     WTIME         UTIME             6 0
021100060407     C* UDATE IN AAAAMMGG
021200060407     C                   Z-ADD     WDate         G02DAT
021300060407     C                   MOVEL     *BLANK        G02ERR
021400060407     C                   CALL      'XSRDA8'
021500060407     C                   PARM                    WLBDAT
021600060407     C                   MOVEL     G02INV        DATEw
021700020212     C*
021800020418     C* CARICO '£1'
021900020212     C                   CLEAR                   KPJBU
022000020212     C                   CLEAR                   DSUL06
022100020212     C                   MOVE      '£1'          D06COD
022200020212     C                   MOVEL     SIMFEL        D06KEY
022300020212     C                   MOVEL     DSUL06        KPJBU
022400020212     C                   CALL      'TRUL06R'
022500020212     C                   PARM                    KPJBA
022600020212     C                   MOVEL     KPJBU         DSUL06
022700020418     c                   movea     t06           sav_t06
022800020418
022900020418      * se in arrivo carico la £6
023000020418 b1  c                   if        ds1tbo = 'A'
023100020418     c                   clear                   wc
023200020418 b2  c                   do        30            wa
023300020422     c                   if        sav_t06(wa) = *zeros
023400020422     c                   leave
023500020422     c                   endif
023600020418     c                   clear                   kpjbu
023700020418     c                   clear                   dsul06
023800020418     c                   eval      d06cod = '£6'
023900020418     c                   movel     sav_t06(wa)   d06key
024000050802     C                   MOVE      'S'           D06KEY
024100020418     c                   eval      d06esc = 'S'
024200020419     c                   movel     dsul06        kpjbu
024300020418     c                   call      'TRUL06R'
024400020418     c                   parm                    kpjba
024500020418     c                   movel     kpjbu         dsul06
024600020418 b3  c                   do        30            wb
024700020418     c                   if        t06(wb) = *zeros
024800020418     c                   leave
024900020418     c                   add       1             wc
025000020419     c                   z-add     t06(wb)       l6(wc)
025100020418     c                   endif
025200020418 e3  c                   enddo
025300020418 e2  c                   enddo
025400020418
025500020418     C* ESCLUDO DA £1 LE FILIALI DELLA £6
025600020212     C                   Z-ADD     *ZERO         WB
025700020418 b2  C                   DO        29            WA
025800020418     C     sav_T06(WA)   LOOKUP    L6                                     30
025900020212     C  N30              ADD       1             WB
026000020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
026100020418 e2  C                   ENDDO
026200020418 x1  C                   ELSE
026300020418     C                   MOVEA     sav_T06       L1
026400020418 e1  C                   ENDIF
026500020212     C*
026600020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
026700020212     C                   MOVEA     L1            LIN
026800020212     C                   CLEAR                   L1
026900020212     C                   Z-ADD     *ZERO         WB                3 0
027000020212     C     1             DO        30            WPT               3 0
027100020212     C     LIN(WPT)      IFNE      0
027200020212     C                   CLEAR                   FNLSC4
027300020212     C                   Z-ADD     LIN(WPT)      IC4LIN
027400020212     C                   MOVE      'B'           IC4FLG
027500020212     C                   MOVEL     FNLSC4        KPJBU
027600020212     C                   CALL      'FNLSC4R1'
027700020212     C                   PARM                    KPJBA
027800060407     C                   PARM                    datew
027900020212     C                   MOVEL     KPJBU         FNLSC4
028000020212     C     OC4ESE        IFEQ      'S'
028100020212     C                   ADD       1             WB
028200020212     C                   MOVE      LIN(WPT)      L1(WB)
028300020212     C                   ENDIF
028400020212     C                   ENDIF
028500020212     C                   ENDDO
028600070103     C*
028700070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
028800070103     C                   Z-ADD     *ZERO         WB                3 0
028900070103     C                   DO        29            WA                3 0
029000070103     C     L1(WA)        IFNE      *ZEROS
029100070103     C     L1(WA)        ANDNE     SIMFEL
029200070103     C                   ADD       1             WB
029300070103     C                   Z-ADD     L1(WA)        LV2(WB)
029400070103     C                   ENDIF
029500070103     C                   ENDDO
029600020212     C*
029700020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
029800020212     C*
029900020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
030000020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
030100020212     C* la stampa e' lanciata da menu
030200020212     C                   MOVEL     '3L'          KCOD
030300020212     C                   MOVEL     DS1PGM        KKEY
030400020212     C                   MOVE      DS1FPG        KKEY
030500020212     C     KTAB1         CHAIN     TABEL00F                           30
030600020212     C  N30              MOVEL     TBLUNI        DS3L
030700020212     C   30              MOVEL     'F'           §3LD1L
030800020212     C   30              MOVEL     ' '           §3LD2L
030900020212     C   30              MOVEL     '*STD    '    §3LDST
031000020212     C*
031100020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
031200020212     C                   MOVEL     §3LDST        £MOD             10
031300020212     C                   CLEAR                   LIN
031400020212     C*
031500020212     C* Verifico chi ha lanciato la stampa
031600020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
031700020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
031800020212     C* DS1FGS<>0  --->  si proviene da menu
031900020212     C     DS1FGS        IFEQ      0
032000020212     C*
032100020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
032200020212     C* lancio, voglio la stampa;  invece
032300020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
032400020212     C*
032500020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
032600020212     C* creare stampa per 1°lvl. (se stessa)
032700020212     C     DS1LNP        IFEQ      SIMFEL
032800020212     C*
032900020212     C* Il flag §3LD1L ha i seguenti significati:
033000020212     C*    ' ' = tutti i dati
033100020212     C*    '1' = solo filiale di 1' livello
033200020212     C     §3LD1L        IFEQ      '1'
033300020212     C                   SETON                                        11
033400020212     C                   END
033500020212     C*
033600020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
033700020212     C* nella stampa
033800020212     C*
033900020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
034000020212     C  N11              MOVEA     L1            LIN
034100020212     C   11              MOVEA     SIMFEL        LIN(1)
034200020212     C*
034300020212     C* IMPOSTO LA STAMPANTE:
034400020212     C* STAMPANTE TABULATI
034500020212     C                   MOVEL     '5P'          KCOD
034600020212     C                   MOVEL     'T'           W004A             4
034700020212     C                   MOVE      LIN(1)        W004A
034800020212     C                   MOVEL(P)  W004A         KKEY
034900020212     C     KTAB1         CHAIN     TABEL00F                           99
035000020212     C     *IN99         IFEQ      *OFF
035100020212     C                   MOVEL     TBLUNI        DS5P
035200020212     C                   MOVEL     §5POTQ        £OUTQ            10
035300020212     C                   END
035400020212     C*
035500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
035600020212     C                   EXSR      LEGGI
035700020212     C*
035800020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
035900020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
036000020212     C                   ELSE
036100020212     C*
036200020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
036300020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
036400020212     C*
036500020212     C* Il significato del fla §3LD2L e' il seguente:
036600020212     C*    ' ' = tutti i dati
036700020212     C*    'F' = per ogni 2' livello
036800020212     C     §3LD2L        IFEQ      'F'
036900020212     C                   SETON                                        14
037000020212     C                   ENDIF
037100020212     C*
037200020212     C                   Z-ADD     1             WB
037300020212     C                   MOVEA     *ZEROS        LIN
037400020212     C  N14              MOVEA     L1            LIN
037500020212     C*
037600020212     C* Lancio per tutti i 2° lvl
037700020212     C     DS1LNP        IFEQ      0
037800020212     C*
037900020212     C* LV2 contiene tutte le filiali di 2° lvl
038000020212     C     LV2(WB)       DOWNE     0
038100020212      *
038200020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
038300020212     C                   CLEAR                   FNLSC4
038400020212     C                   Z-ADD     LV2(WB)       IC4LIN
038500020212     C                   MOVEL     FNLSC4        KPJBU
038600020212     C                   CALL      'FNLSC4R1'
038700020212     C                   PARM                    KPJBA
038800060407     C                   PARM                    datew
038900020212     C                   MOVEL     KPJBU         FNLSC4
039000020212      *
039100020212     C     OC4PT1        IFEQ      'S'
039200020212     C     §3LPT         ANDEQ     'N'
039300020212     C                   GOTO      NOLEGG
039400020212     C                   ENDIF
039500020212      *
039600020212     C     OC4EXP        IFEQ      'S'
039700020212     C     §3LEXP        ANDEQ     'N'
039800020212     C                   GOTO      NOLEGG
039900020212     C                   ENDIF
040000020212      *
040100020212     C     OC4DPD        IFEQ      'S'
040200020212     C     §3LDPD        ANDEQ     'N'
040300020212     C                   GOTO      NOLEGG
040400020212     C                   ENDIF
040500020212      *
040600020212     C     OC4BAR        IFEQ      'S'
040700020212     C     §3LBAR        ANDEQ     'N'
040800020212     C                   GOTO      NOLEGG
040900020212     C                   ENDIF
041000020212      *
041100020212     C     OC4FED        IFEQ      'S'
041200020212     C     §3LFED        ANDEQ     'N'
041300020212     C                   GOTO      NOLEGG
041400020212     C                   ENDIF
041500020212      *
041600020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
041700020212     C*  dati degli altri 2° lvl
041800020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
041900020212     C   14              MOVEL     LV2(WB)       LIN(1)
042000020212     C*
042100020212     C* IMPOSTO LA STAMPANTE
042200020212     C* STAMPANTE TABULATI
042300020212     C                   MOVEL     '5P'          KCOD
042400020212     C                   MOVEL     'T'           W004A             4
042500020212     C                   MOVE      LV2(WB)       W004A
042600020212     C                   MOVEL(P)  W004A         KKEY
042700020212     C     KTAB1         CHAIN     TABEL00F                           99
042800020212     C     *IN99         IFEQ      *OFF
042900020212     C                   MOVEL     TBLUNI        DS5P
043000020212     C                   MOVEL     §5POTQ        £OUTQ            10
043100020212     C                   END
043200020212     C*
043300020212     C*
043400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
043500020212     C                   EXSR      LEGGI
043600020212     C*
043700020212     C     NOLEGG        TAG
043800020212     C*
043900020212     C                   ADD       1             WB
044000020212     C                   ENDDO
044100020212     C*
044200020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
044300020212     C                   ELSE
044400020212     C*
044500020212     C* Lancio solo per la 2° lvl richiesta
044600020212     C   14              MOVEL     DS1LNP        LIN(1)
044700020212     C*
044800020212     C* STAMPANTE
044900020212     C* STAMPANTE TABULATI
045000020212     C                   MOVEL     '5P'          KCOD
045100020212     C                   MOVEL     'T'           W004A             4
045200020212     C                   MOVE      DS1LNP        W004A
045300020212     C                   MOVEL(P)  W004A         KKEY
045400020212     C     KTAB1         CHAIN     TABEL00F                           99
045500020212     C     *IN99         IFEQ      *OFF
045600020212     C                   MOVEL     TBLUNI        DS5P
045700020212     C                   MOVEL     §5POTQ        £OUTQ            10
045800020212     C                   END
045900020212     C*
046000020212     C*
046100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046200020212     C                   EXSR      LEGGI
046300020212     C*
046400020212     C                   ENDIF
046500020212     C                   ENDIF
046600020212     C*
046700020212     C                   ELSE
046800020212     C*
046900020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
047000020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
047100020212     C     DS1LNP        IFEQ      0
047200020212     C                   MOVEA     L1            LIN
047300020212     C* ...altrimenti elaboro solo se stessa
047400020212     C                   ELSE
047500020212     C                   MOVEL     DS1LNP        LIN(1)
047600020212     C                   ENDIF
047700020212     C*
047800020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
047900020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
048000020212     C* PRENDO QUELLA PRESENTE IN KPJBA
048100060427     C**** DS1FGS        IFEQ      SIMFEL
048200020212     C                   MOVEL     KQOTD         £OUTQ
048300030627     C**!!!              MOVEL     KQOTD         £OUTQF
048400060427     C****               ELSE
048500020212     C* STAMPANTE TABULATI
048600060427     C***                MOVEL     '5P'          KCOD
048700060427     C****               MOVEL     'T'           W004A             4
048800060427     C***                MOVE      DS1FGS        W004A
048900060427     C***                MOVEL(P)  W004A         KKEY
049000060427     C***  KTAB1         CHAIN     TABEL00F                           99
049100060427     C***  *IN99         IFEQ      *OFF
049200060427     C***                MOVEL     TBLUNI        DS5P
049300060427     C***                MOVEL     §5POTQ        £OUTQ            10
049400060427     C***                END
049500060427     C***                ENDIF
049600020212     C*
049700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
049800020212     C                   EXSR      LEGGI
049900020212     C                   ENDIF
050000020212     C*
050100020212     C                   ENDSR
050200010312      **************************************************************************
050300010312      * LETTURA ORM PER CREAZIONE STAMPA
050400010312      **************************************************************************
050500921001     C     LEGGI         BEGSR
050600010329      *
050700060406     C                   MOVEL     'FNLSE8P'     FILEST            8
050800010312      *
050900010312      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
051000010329     C                   MOVEA     £MOD          CMD(34)
051100010329     C                   MOVEA     £OUTQ         CMD(52)
051200010329     C                   MOVEA     FILEST        CMD(14)
051300010329      *
051400010312      * ESEGUO OVRPRTF
051500950308     C                   Z-ADD     82            LUNG             15 5
051600950308     C                   MOVEL     *BLANKS       COMMAN           82
051700950308     C                   MOVEA     CMD           COMMAN
051800921001     C                   CALL      'QCMDEXC'                            H1
051900921001     C                   PARM                    COMMAN
052000921001     C                   PARM                    LUNG
052100010312      *
052200060406     C                   OPEN      FNLSE8P
052300010327      *
052400010329     C     *IN04         IFEQ      *ON
052500060406     C                   MOVEL     'FNLSE8P1'    FILEST            8
052600010329     C                   MOVEA     FILEST        CMD(14)
052700010327      * ESEGUO OVRPRTF
052800010327     C                   Z-ADD     82            LUNG             15 5
052900010327     C                   MOVEL     *BLANKS       COMMAN           82
053000010327     C                   MOVEA     CMD           COMMAN
053100010327     C                   CALL      'QCMDEXC'                            H1
053200010327     C                   PARM                    COMMAN
053300010327     C                   PARM                    LUNG
053400060406     C                   OPEN      FNLSE8P1
053500010327     C                   ENDIF
053600010327      *
053700060407     C                   SETOFF                                       99
053800060407     c
053900950308     C                   Z-ADD     1             C                 3 0
054000010312      *
054100941227    1C     LIN(C)        DOWNE     0
054200010327      * CODICE FILIALE E DESCRIZIONE
054300010327     C                   CLEAR                   FIL
054400010327     C                   CLEAR                   DESFIL
054500010327     C     LIN(C)        CHAIN     AZORG01L                           32
054600010327     C     *IN32         IFEQ      *OFF
054700010327     C                   Z-ADD     LIN(C)        FIL
054800010327     C                   MOVEL     ORGDES        DESFIL
054900010327     C                   ENDIF
055000060407      * STAMPA TESTATA
055100060407     c                   if        *in99 = *off
055200060407     C                   WRITE     TESTA
055300060407     c     *in04         ifeq      *on
055400060407     c                   write     testa1
055500060407     c                   endif
055600060407     c                   seton                                        99
055700060407     c                   endif
055800010312      *
055900060407     c                   z-add     1             k_dft
056000010312      * ELABORAZIONE
056100060407     C     kar635        SETLL     fiar635l
056200010312     C                   DO        *HIVAL
056300060407     C     LIN(C)        READE     Fiar635l                               30
056400010312      *
056500010312     C     *IN30         IFEQ      *ON
056600010312     C                   LEAVE
056700010312     C                   ENDIF
056800060407     c* se presente il cod cliente significa che sto leggendo la tassazione
056900060407     c* di un assegnato e quindi scarto
057000060407     c                   if        ar6ksc > 0
057100060407     c                   iter
057200060407     c                   endif
057300060407     c     kblp          chain     fnblp01l
057400060420     c                   if        not %found(fnblp01l)
057500060420     c                   iter
057600060420     c                   end
057700010329      * Escludo se non porto franco
057800010326     C                   Z-ADD     1             X
057900010326     C     BLPCBO        LOOKUP    KEY(X)                                 30
058000010326     C     *IN30         IFEQ      *ON
058100060407     c* verifico se incassato
058200060407     c                   z-add     ar6aas        prmaas
058300060407     c                   z-add     ar6lnp        prmlnp
058400060407     c                   z-add     ar6nrs        prmnrs
058500060407     c                   z-add     ar6nsp        prmnsp
058600060407     c                   z-add     ar6fiv        prmfiv
058700060407     c                   z-add     ar6nft        prmnft
058800060407     c                   z-add     ar6dft        prmdft
058900060407     c                   clear                   prmesito
059000060407     c                   call(E)   'YCOW0121R'
059100060407     C                   PARM                    prmaas
059200060407     C                   PARM                    prmLnp
059300060407     C                   PARM                    prmNrs
059400060407     C                   PARM                    prmNsp
059500060407     C                   PARM                    prmFiv
059600060407     C                   PARM                    prmDft
059700060407     C                   PARM                    prmNft
059800060407     C                   PARM                    prmEsito
059900060407     c* stampo solo se prepagato confermato non incassato oppure (se
060000060407     c* richiesto) se prepagato non confermato
060100060407     c                   if        (prmesito=0 and blppdr > *zeros) or
060200060407     c                             (blppdr = *zeros and *in04 = '1')
060300010312      *
060400010312     C                   EXSR      STAMPA
060500060407     c                   endif
060600010326     C                   ENDIF
060700010312      *
060800060407    2C                   ENDDO
060900010312      *
061000921002     C                   ADD       1             C
061100941227    1C                   END
061200010312      *
061300060407     c                   write     fines
061400060407     c                   if        *in04 = '1'
061500060407     c                   write     fines1
061600060407     c                   endif
061700060407     c*
061800060407     C                   CLOSE     FNLSE8P
061900010329      *
062000010329     C     *IN04         IFEQ      *ON
062100060407     C                   CLOSE     FNLSE8P1
062200010329     C                   ENDIF
062300010312      *
062400921002     C                   ENDSR
062500010312      **************************************************************************
062600010312      *    STAMPA LISTA
062700010312      **************************************************************************
062800900517     C     STAMPA        BEGSR
062900010326      *
063000010326     C                   CLEAR                   STPRSC
063100010326     C                   CLEAR                   STPDIV
063200010326     C                   CLEAR                   STPIFT
063300010326     C                   CLEAR                   STPFIV
063400010326     C                   CLEAR                   STPNFT
063500010326     C                   CLEAR                   STPDFT
063600010327     C                   CLEAR                   STPDSP
063700060407     C                   CLEAR                   p1cinc
063800010327      *
063900010312      *
064000010312      * Ragione sociale Autotrasportatore
064100021205     C                   move      BlpPdr        kpdpdr
064200021203     C                   move      'A'           kpdtip
064300021203     C     kfapd         chain     fiapd01L                           30
064400010312     C     *IN30         IFEQ      *OFF
064500010312     C                   MOVEL     APDRSC        STPRSC
064600010312     C                   ENDIF
064700060407     c* prepagato non confermato e già incassato
064800060407     c                   if        prmesito <> 0 and blppdr = *zeros
064900060407     c                             and *in04 = '1'
065000060407     c                   eval      p1cinc = 'INCASSATO'
065100060407     c                   else
065200060407     c                   clear                   p1cinc
065300060407     c                   endif
065400010312      *
065500010326     C                   MOVE      AR6DIV        STPDIV
065600010312     C                   Z-ADD     AR6IFT        STPIFT
065700010312     C                   Z-ADD     AR6FIV        STPFIV
065800010312     C                   Z-ADD     AR6NFT        STPNFT
065900010327      *
066000010327      * Giro data fattura
066100010312     C                   CLEAR                   WLBDAT
066200010312     C                   Z-ADD     AR6DFT        G02INV
066300010321     C                   MOVEL     '3'           G02ERR
066400010312     C                   CALL      'XSRDA8'
066500010312     C                   PARM                    WLBDAT
066600010312     C                   Z-ADD     G02DAT        STPDFT
066700010327      *
066800010327      * Giro data spedizione
066900010327     C                   CLEAR                   WLBDAT
067000010327     C                   Z-ADD     BLPDSP        G02INV
067100010327     C                   MOVEL     '3'           G02ERR
067200010327     C                   CALL      'XSRDA8'
067300010327     C                   PARM                    WLBDAT
067400010327     C                   Z-ADD     G02DAT        STPDSP
067500060407      * Prepagati confermati e non incassati
067600010327     C     BLPPDR        IFNE      *ZEROS
067700010312     C   98              WRITE     TESTA
067800060412     c                   setoff                                       98
067900010327     C                   WRITE     RIGA
068000010327     C                   ENDIF
068100010327      * Prepagati non confermati
068200010327     C     BLPPDR        IFEQ      *ZEROS
068300010329     C     *IN04         ANDEQ     *ON
068400010329      * STAMPA TESTATA
068500010327     C   97              WRITE     TESTA1
068600060412     c                   setoff                                       97
068700010327     C                   WRITE     RIGA1
068800010327     C                   ENDIF
068900010312      *
069000900517     C                   ENDSR
069100010329**
069200010329OVRPRTF FILE(        ) FORMTYPE('          ') OUTQ(          ) USRDTA('Prep.Part
069300010329')
