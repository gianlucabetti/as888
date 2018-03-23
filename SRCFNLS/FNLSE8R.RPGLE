000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200060406      * FNLSE8R *----------------------------------------------------*
000300060406      *         - LISTA PREPAGATI non incassati
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
002100060407     FFIAR635L  IF   E           K DISK
002200060407     FFNBLP01L  IF   E           K DISK
002300021203     FFIAPD01L  IF   E           K DISK
002400010327     FAZORG01L  IF   E           K DISK
002500010312     FTABEL00F  IF   E           K DISK
002600060407     FFNLSE8P   O    E             PRINTER OFLIND(*IN98)
002700010326     F                                     USROPN
002800060407     FFNLSE8P1  O    E             PRINTER OFLIND(*IN97)
002900010327     F                                     USROPN
003000010312      *
003100950308     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
003200010312      *
003300950308     D L1              S              3  0 DIM(30)
003400020418     D L6              S              3  0 DIM(30)
003500950308     D LV2             S              3  0 DIM(30)
003600950308     D LIN             S              3  0 DIM(30)
003700010326     D KEY             S              2    DIM(99)
003800020418     d sav_t06         S              3  0 dim(30)
003900060407     d datew           S              8s 0
004000020418
004100020418     d wc              s              3  0
004200060407     D nullPtr         S               *
004300060407     D prmAas          S
004400060407     D                                     LIKE(diz$Aas)
004500060407     D prmLnp          S
004600060407     D                                     LIKE(diz$Lnp)
004700060407     D prmNrs          S
004800060407     D                                     LIKE(diz$Nrs)
004900060407     D prmNsp          S
005000060407     D                                     LIKE(diz$Nsp)
005100060407     D prmFiv          S
005200060407     D                                     LIKE(diz$Fiv)
005300060407     D prmDft          S
005400060407     D                                     LIKE(diz$Dft)
005500060407     D prmNft          S
005600060407     D                                     LIKE(diz$Nft)
005700060407     D prmEsito        S             10I 0
005800060407     D cndizion      E DS
005900060407     D                                     BASED(nullPtr)
006000060407     D                                     PREFIX(diz)
006100020418
006200010312      *----------------------------------
006300010312      * PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
006400010312      *----------------------------------
006500010312     D WLBDAT          DS
006600010312     D  G02DAT                 1      8  0
006700010312     D  G02INV                 9     16  0
006800010312     D  G02ERR                17     17
006900010312     D  G02TGI                18     22  0
007000010312      *
007100010312      * PARAMETRI PASSATI DAI PGM RICHIAMANTI
007200950308     D DSLS01        E DS
007300900613     D KPJBA         E DS
007400010329     D  SINO                 502    502
007500900710     D                 DS
007600941227     D  BLPAAS                 1      4  0
007700941227     D  BLPMGS                 5      8  0
007800941227     D  BLPDSP                 1      8  0
007900021203
008000021203     D kpdpdr          S                   LIKE(APDpdr)
008100021203     D kpdtip          S                   LIKE(APDtip)
008200060407     D k_dft           S                   LIKE(ar6dft)
008300921001     D DS3L          E DS
008400970707     D DS5P          E DS
008500010326     D DS3A          E DS
008600910808     D CNCR80        E DS
008700910808     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008800010312      * DS PER TRUL06R - CARICAMENTO £X
008900950308     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009000950314     D  T06                    1     90  0
009100950314     D                                     DIM(30)
009200010312      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
009300010312     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
009400010312      *---------------------------------------------------------------*
009500010312      *    RIEPILOGO INDICATORI
009600010312      *---------------------------------------------------------------*
009700010312      * 03    - IMMESSA DATA AL
009800010329      * 04    - IMMESSO SI PER STAMPA PREPAGARI ANCORA DA CONFERMARE
009900010312      * 30/32 - DI COMODO
010000060407      * 96    -
010100010327      * 97    - Fine pagine
010200010312      * 98    - Fine pagine
010300010312      * 99    - Stampa testata una volta
010400010312      *---------------------------------------------------------------*
010500000000     C     *ENTRY        PLIST
010600000000     C                   PARM                    KPJBA
010700950308     C                   MOVEL     KPJBU         DSLS01
010800010312      *
010900000000     C                   Z-ADD     1             CODUT
011000910808     C                   CALL      'X§PARUT'
011100910808     C                   PARM                    UT§DSE
011200010312     C                   MOVEL     RAGUT         RSUT
011300910808     C                   MOVEL     REC80         CNCR80
011400010329      * reperisco data e ora
011500010329     C                   TIME                    W0140            14 0
011600010329      * DATA
011700010329     C                   MOVE      W0140         WDTGIO
011800010329      * ORA
011900010329     C                   MOVEL     W0140         WORA
012000010329      * ANCORA DA CONFERMARE
012100010329     C     SINO          IFNE      *BLANKS
012200010329     C                   SETON                                        04
012300010329     C                   ENDIF
012400010312      *---------------------------------------------------------------*
012500060407      * ACCESSO FIAR635L
012600060407     C     KAR635        KLIST
012700060407     C                   KFLD                    lin(C)
012800060407     C                   KFLD                    k_dft
012900060407     C     Kblp          KLIST
013000060407     C                   KFLD                    ar6aas
013100060407     C                   KFLD                    ar6lnp
013200060407     C                   KFLD                    ar6nrs
013300060407     C                   KFLD                    ar6nsp
013400021203
013500021203     C     Kfapd         klist
013600021203     C                   kfld                    kpdtip
013700021203     C                   kfld                    kpdpdr
013800010326      *
013900010326     C                   Z-ADD     0             X                 2 0
014000010326     C                   MOVEL     '3A'          KCOD
014100010326     C     KTAB2         SETLL     TABEL00F
014200010326     C                   DO        *HIVAL
014300010326     C     KTAB2         READE     TABEL00F                               30
014400010326     C     *IN30         IFEQ      *ON
014500010326     C                   LEAVE
014600010326     C                   ENDIF
014700010326     C     TBLFLG        IFNE      *BLANKS
014800010326     C                   ITER
014900010326     C                   ENDIF
015000010326     C                   ADD       1             X                 2 0
015100010326     C                   MOVEL     TBLUNI        DS3A
015200060407     c                   if        §3aucb <> 'N' and §3arbl <> 'C'
015300010326     C                   MOVEL     TBLKEY        KEY(X)
015400010326     C                   ENDIF
015500010326     C                   ENDDO
015600010312      *
015700010312      *---------------------------------------------------------------*
015800010312      *
015900010312      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
016000950308     C                   MOVEL     'P'           DS1TBO
016100950308     C                   CLEAR                   DS1PGM
016200060407     C                   MOVEL     'FNLSE8R'     DS1PGM
016300950308     C                   EXSR      COMUNE
016400010312      *
016500921001     C                   SETON                                        LR
016600020212     C*****************************************************************
016700020212     C*  PARTE COMUNE STAMPE GIORNALIERE
016800020212     C*
016900020212     C*  . REPERIMENTO 'SIMFEL'
017000020212     C*  . CARICAMENTO '£1'
017100020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
017200020212     C*  . CODA DI STAMPA ('£OUTQ')
017300020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
017400020212     C*  . MODULO PER LA STAMPA ('£MOD')
017500020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
017600020212     C*****************************************************************
017700020212     C     COMUNE        BEGSR
017800020212     C*
017900020212     C*****************************************************************
018000020212     C*       RIEPILOGO INDICATORI:
018100020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
018200020212     C* 11    -           STAMPA PER 1L
018300020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
018400020212     C*****************************************************************
018500020212     C                   Z-ADD     1             CODUT
018600020212     C*
018700020212     C* Reperimento parametri aziendali
018800020212     C                   CALL      'X§PARUT'
018900020212     C                   PARM                    UT§DSE
019000020212     C                   MOVEL     RAGUT         RSUT             20
019100020212     C                   MOVEL     REC80         CNCR80
019200020212     C*
019300020212     C* Chiave di accesso per il file tabelle
019400020212     C     KTAB1         KLIST
019500020212     C                   KFLD                    CODUT
019600020212     C                   KFLD                    KCOD
019700020212     C                   KFLD                    KKEY
019800020212     C     KTAB2         KLIST
019900020212     C                   KFLD                    CODUT
020000020212     C                   KFLD                    KCOD
020100020212     C*
020200020212     C     *LIKE         DEFINE    TBLCOD        KCOD
020300020212     C     *LIKE         DEFINE    TBLKEY        KKEY
020400020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
020500020212     C*
020600020212     C* DETERMINO DATA E ORA DI ESECUZIONE
020700020212     C                   TIME                    WTIME            14 0
020800020212     C                   MOVE      WTIME         WDATE             8 0
020900020212     C                   MOVEL     WTIME         UTIME             6 0
021000060407     C* UDATE IN AAAAMMGG
021100060407     C                   Z-ADD     WDate         G02DAT
021200060407     C                   MOVEL     *BLANK        G02ERR
021300060407     C                   CALL      'XSRDA8'
021400060407     C                   PARM                    WLBDAT
021500060407     C                   MOVEL     G02INV        DATEw
021600020212     C*
021700020418     C* CARICO '£1'
021800020212     C                   CLEAR                   KPJBU
021900020212     C                   CLEAR                   DSUL06
022000020212     C                   MOVE      '£1'          D06COD
022100020212     C                   MOVEL     SIMFEL        D06KEY
022200020212     C                   MOVEL     DSUL06        KPJBU
022300020212     C                   CALL      'TRUL06R'
022400020212     C                   PARM                    KPJBA
022500020212     C                   MOVEL     KPJBU         DSUL06
022600020418     c                   movea     t06           sav_t06
022700020418
022800020418      * se in arrivo carico la £6
022900020418 b1  c                   if        ds1tbo = 'A'
023000020418     c                   clear                   wc
023100020418 b2  c                   do        30            wa
023200020422     c                   if        sav_t06(wa) = *zeros
023300020422     c                   leave
023400020422     c                   endif
023500020418     c                   clear                   kpjbu
023600020418     c                   clear                   dsul06
023700020418     c                   eval      d06cod = '£6'
023800020418     c                   movel     sav_t06(wa)   d06key
023900050802     C                   MOVE      'S'           D06KEY
024000020418     c                   eval      d06esc = 'S'
024100020419     c                   movel     dsul06        kpjbu
024200020418     c                   call      'TRUL06R'
024300020418     c                   parm                    kpjba
024400020418     c                   movel     kpjbu         dsul06
024500020418 b3  c                   do        30            wb
024600020418     c                   if        t06(wb) = *zeros
024700020418     c                   leave
024800020418     c                   add       1             wc
024900020419     c                   z-add     t06(wb)       l6(wc)
025000020418     c                   endif
025100020418 e3  c                   enddo
025200020418 e2  c                   enddo
025300020418
025400020418     C* ESCLUDO DA £1 LE FILIALI DELLA £6
025500020212     C                   Z-ADD     *ZERO         WB
025600020418 b2  C                   DO        29            WA
025700020418     C     sav_T06(WA)   LOOKUP    L6                                     30
025800020212     C  N30              ADD       1             WB
025900020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
026000020418 e2  C                   ENDDO
026100020418 x1  C                   ELSE
026200020418     C                   MOVEA     sav_T06       L1
026300020418 e1  C                   ENDIF
026400020212     C*
026500020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
026600020212     C                   MOVEA     L1            LIN
026700020212     C                   CLEAR                   L1
026800020212     C                   Z-ADD     *ZERO         WB                3 0
026900020212     C     1             DO        30            WPT               3 0
027000020212     C     LIN(WPT)      IFNE      0
027100020212     C                   CLEAR                   FNLSC4
027200020212     C                   Z-ADD     LIN(WPT)      IC4LIN
027300020212     C                   MOVE      'B'           IC4FLG
027400020212     C                   MOVEL     FNLSC4        KPJBU
027500020212     C                   CALL      'FNLSC4R1'
027600020212     C                   PARM                    KPJBA
027700060407     C                   PARM                    datew
027800020212     C                   MOVEL     KPJBU         FNLSC4
027900020212     C     OC4ESE        IFEQ      'S'
028000020212     C                   ADD       1             WB
028100020212     C                   MOVE      LIN(WPT)      L1(WB)
028200020212     C                   ENDIF
028300020212     C                   ENDIF
028400020212     C                   ENDDO
028500070103     C*
028600070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
028700070103     C                   Z-ADD     *ZERO         WB                3 0
028800070103     C                   DO        29            WA                3 0
028900070103     C     L1(WA)        IFNE      *ZEROS
029000070103     C     L1(WA)        ANDNE     SIMFEL
029100070103     C                   ADD       1             WB
029200070103     C                   Z-ADD     L1(WA)        LV2(WB)
029300070103     C                   ENDIF
029400070103     C                   ENDDO
029500020212     C*
029600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
029700020212     C*
029800020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
029900020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
030000020212     C* la stampa e' lanciata da menu
030100020212     C                   MOVEL     '3L'          KCOD
030200020212     C                   MOVEL     DS1PGM        KKEY
030300020212     C                   MOVE      DS1FPG        KKEY
030400020212     C     KTAB1         CHAIN     TABEL00F                           30
030500020212     C  N30              MOVEL     TBLUNI        DS3L
030600020212     C   30              MOVEL     'F'           §3LD1L
030700020212     C   30              MOVEL     ' '           §3LD2L
030800020212     C   30              MOVEL     '*STD    '    §3LDST
030900020212     C*
031000020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
031100020212     C                   MOVEL     §3LDST        £MOD             10
031200020212     C                   CLEAR                   LIN
031300020212     C*
031400020212     C* Verifico chi ha lanciato la stampa
031500020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
031600020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
031700020212     C* DS1FGS<>0  --->  si proviene da menu
031800020212     C     DS1FGS        IFEQ      0
031900020212     C*
032000020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
032100020212     C* lancio, voglio la stampa;  invece
032200020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
032300020212     C*
032400020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
032500020212     C* creare stampa per 1°lvl. (se stessa)
032600020212     C     DS1LNP        IFEQ      SIMFEL
032700020212     C*
032800020212     C* Il flag §3LD1L ha i seguenti significati:
032900020212     C*    ' ' = tutti i dati
033000020212     C*    '1' = solo filiale di 1' livello
033100020212     C     §3LD1L        IFEQ      '1'
033200020212     C                   SETON                                        11
033300020212     C                   END
033400020212     C*
033500020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
033600020212     C* nella stampa
033700020212     C*
033800020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
033900020212     C  N11              MOVEA     L1            LIN
034000020212     C   11              MOVEA     SIMFEL        LIN(1)
034100020212     C*
034200020212     C* IMPOSTO LA STAMPANTE:
034300020212     C* STAMPANTE TABULATI
034400020212     C                   MOVEL     '5P'          KCOD
034500020212     C                   MOVEL     'T'           W004A             4
034600020212     C                   MOVE      LIN(1)        W004A
034700020212     C                   MOVEL(P)  W004A         KKEY
034800020212     C     KTAB1         CHAIN     TABEL00F                           99
034900020212     C     *IN99         IFEQ      *OFF
035000020212     C                   MOVEL     TBLUNI        DS5P
035100020212     C                   MOVEL     §5POTQ        £OUTQ            10
035200020212     C                   END
035300020212     C*
035400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
035500020212     C                   EXSR      LEGGI
035600020212     C*
035700020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
035800020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
035900020212     C                   ELSE
036000020212     C*
036100020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
036200020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
036300020212     C*
036400020212     C* Il significato del fla §3LD2L e' il seguente:
036500020212     C*    ' ' = tutti i dati
036600020212     C*    'F' = per ogni 2' livello
036700020212     C     §3LD2L        IFEQ      'F'
036800020212     C                   SETON                                        14
036900020212     C                   ENDIF
037000020212     C*
037100020212     C                   Z-ADD     1             WB
037200020212     C                   MOVEA     *ZEROS        LIN
037300020212     C  N14              MOVEA     L1            LIN
037400020212     C*
037500020212     C* Lancio per tutti i 2° lvl
037600020212     C     DS1LNP        IFEQ      0
037700020212     C*
037800020212     C* LV2 contiene tutte le filiali di 2° lvl
037900020212     C     LV2(WB)       DOWNE     0
038000020212      *
038100020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
038200020212     C                   CLEAR                   FNLSC4
038300020212     C                   Z-ADD     LV2(WB)       IC4LIN
038400020212     C                   MOVEL     FNLSC4        KPJBU
038500020212     C                   CALL      'FNLSC4R1'
038600020212     C                   PARM                    KPJBA
038700060407     C                   PARM                    datew
038800020212     C                   MOVEL     KPJBU         FNLSC4
038900020212      *
039000020212     C     OC4PT1        IFEQ      'S'
039100020212     C     §3LPT         ANDEQ     'N'
039200020212     C                   GOTO      NOLEGG
039300020212     C                   ENDIF
039400020212      *
039500020212     C     OC4EXP        IFEQ      'S'
039600020212     C     §3LEXP        ANDEQ     'N'
039700020212     C                   GOTO      NOLEGG
039800020212     C                   ENDIF
039900020212      *
040000020212     C     OC4DPD        IFEQ      'S'
040100020212     C     §3LDPD        ANDEQ     'N'
040200020212     C                   GOTO      NOLEGG
040300020212     C                   ENDIF
040400020212      *
040500020212     C     OC4BAR        IFEQ      'S'
040600020212     C     §3LBAR        ANDEQ     'N'
040700020212     C                   GOTO      NOLEGG
040800020212     C                   ENDIF
040900020212      *
041000020212     C     OC4FED        IFEQ      'S'
041100020212     C     §3LFED        ANDEQ     'N'
041200020212     C                   GOTO      NOLEGG
041300020212     C                   ENDIF
041400020212      *
041500020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
041600020212     C*  dati degli altri 2° lvl
041700020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
041800020212     C   14              MOVEL     LV2(WB)       LIN(1)
041900020212     C*
042000020212     C* IMPOSTO LA STAMPANTE
042100020212     C* STAMPANTE TABULATI
042200020212     C                   MOVEL     '5P'          KCOD
042300020212     C                   MOVEL     'T'           W004A             4
042400020212     C                   MOVE      LV2(WB)       W004A
042500020212     C                   MOVEL(P)  W004A         KKEY
042600020212     C     KTAB1         CHAIN     TABEL00F                           99
042700020212     C     *IN99         IFEQ      *OFF
042800020212     C                   MOVEL     TBLUNI        DS5P
042900020212     C                   MOVEL     §5POTQ        £OUTQ            10
043000020212     C                   END
043100020212     C*
043200020212     C*
043300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
043400020212     C                   EXSR      LEGGI
043500020212     C*
043600020212     C     NOLEGG        TAG
043700020212     C*
043800020212     C                   ADD       1             WB
043900020212     C                   ENDDO
044000020212     C*
044100020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
044200020212     C                   ELSE
044300020212     C*
044400020212     C* Lancio solo per la 2° lvl richiesta
044500020212     C   14              MOVEL     DS1LNP        LIN(1)
044600020212     C*
044700020212     C* STAMPANTE
044800020212     C* STAMPANTE TABULATI
044900020212     C                   MOVEL     '5P'          KCOD
045000020212     C                   MOVEL     'T'           W004A             4
045100020212     C                   MOVE      DS1LNP        W004A
045200020212     C                   MOVEL(P)  W004A         KKEY
045300020212     C     KTAB1         CHAIN     TABEL00F                           99
045400020212     C     *IN99         IFEQ      *OFF
045500020212     C                   MOVEL     TBLUNI        DS5P
045600020212     C                   MOVEL     §5POTQ        £OUTQ            10
045700020212     C                   END
045800020212     C*
045900020212     C*
046000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046100020212     C                   EXSR      LEGGI
046200020212     C*
046300020212     C                   ENDIF
046400020212     C                   ENDIF
046500020212     C*
046600020212     C                   ELSE
046700020212     C*
046800020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
046900020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
047000020212     C     DS1LNP        IFEQ      0
047100020212     C                   MOVEA     L1            LIN
047200020212     C* ...altrimenti elaboro solo se stessa
047300020212     C                   ELSE
047400020212     C                   MOVEL     DS1LNP        LIN(1)
047500020212     C                   ENDIF
047600020212     C*
047700020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
047800020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
047900020212     C* PRENDO QUELLA PRESENTE IN KPJBA
048000060427     C**** DS1FGS        IFEQ      SIMFEL
048100020212     C                   MOVEL     KQOTD         £OUTQ
048200030627     C**!!!              MOVEL     KQOTD         £OUTQF
048300060427     C****               ELSE
048400020212     C* STAMPANTE TABULATI
048500060427     C***                MOVEL     '5P'          KCOD
048600060427     C****               MOVEL     'T'           W004A             4
048700060427     C***                MOVE      DS1FGS        W004A
048800060427     C***                MOVEL(P)  W004A         KKEY
048900060427     C***  KTAB1         CHAIN     TABEL00F                           99
049000060427     C***  *IN99         IFEQ      *OFF
049100060427     C***                MOVEL     TBLUNI        DS5P
049200060427     C***                MOVEL     §5POTQ        £OUTQ            10
049300060427     C***                END
049400060427     C***                ENDIF
049500020212     C*
049600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
049700020212     C                   EXSR      LEGGI
049800020212     C                   ENDIF
049900020212     C*
050000020212     C                   ENDSR
050100010312      **************************************************************************
050200010312      * LETTURA ORM PER CREAZIONE STAMPA
050300010312      **************************************************************************
050400921001     C     LEGGI         BEGSR
050500010329      *
050600060406     C                   MOVEL     'FNLSE8P'     FILEST            8
050700010312      *
050800010312      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
050900010329     C                   MOVEA     £MOD          CMD(34)
051000010329     C                   MOVEA     £OUTQ         CMD(52)
051100010329     C                   MOVEA     FILEST        CMD(14)
051200010329      *
051300010312      * ESEGUO OVRPRTF
051400950308     C                   Z-ADD     82            LUNG             15 5
051500950308     C                   MOVEL     *BLANKS       COMMAN           82
051600950308     C                   MOVEA     CMD           COMMAN
051700921001     C                   CALL      'QCMDEXC'                            H1
051800921001     C                   PARM                    COMMAN
051900921001     C                   PARM                    LUNG
052000010312      *
052100060406     C                   OPEN      FNLSE8P
052200010327      *
052300010329     C     *IN04         IFEQ      *ON
052400060406     C                   MOVEL     'FNLSE8P1'    FILEST            8
052500010329     C                   MOVEA     FILEST        CMD(14)
052600010327      * ESEGUO OVRPRTF
052700010327     C                   Z-ADD     82            LUNG             15 5
052800010327     C                   MOVEL     *BLANKS       COMMAN           82
052900010327     C                   MOVEA     CMD           COMMAN
053000010327     C                   CALL      'QCMDEXC'                            H1
053100010327     C                   PARM                    COMMAN
053200010327     C                   PARM                    LUNG
053300060406     C                   OPEN      FNLSE8P1
053400010327     C                   ENDIF
053500010327      *
053600060407     C                   SETOFF                                       99
053700060407     c
053800950308     C                   Z-ADD     1             C                 3 0
053900010312      *
054000941227    1C     LIN(C)        DOWNE     0
054100010327      * CODICE FILIALE E DESCRIZIONE
054200010327     C                   CLEAR                   FIL
054300010327     C                   CLEAR                   DESFIL
054400010327     C     LIN(C)        CHAIN     AZORG01L                           32
054500010327     C     *IN32         IFEQ      *OFF
054600010327     C                   Z-ADD     LIN(C)        FIL
054700010327     C                   MOVEL     ORGDES        DESFIL
054800010327     C                   ENDIF
054900060407      * STAMPA TESTATA
055000060407     c                   if        *in99 = *off
055100060407     C                   WRITE     TESTA
055200060407     c     *in04         ifeq      *on
055300060407     c                   write     testa1
055400060407     c                   endif
055500060407     c                   seton                                        99
055600060407     c                   endif
055700010312      *
055800060407     c                   z-add     1             k_dft
055900010312      * ELABORAZIONE
056000060407     C     kar635        SETLL     fiar635l
056100010312     C                   DO        *HIVAL
056200060407     C     LIN(C)        READE     Fiar635l                               30
056300010312      *
056400010312     C     *IN30         IFEQ      *ON
056500010312     C                   LEAVE
056600010312     C                   ENDIF
056700060407     c* se presente il cod cliente significa che sto leggendo la tassazione
056800060407     c* di un assegnato e quindi scarto
056900060407     c                   if        ar6ksc > 0
057000060407     c                   iter
057100060407     c                   endif
057200060407     c     kblp          chain     fnblp01l
057300060420     c                   if        not %found(fnblp01l)
057400060420     c                   iter
057500060420     c                   end
057600010329      * Escludo se non porto franco
057700010326     C                   Z-ADD     1             X
057800010326     C     BLPCBO        LOOKUP    KEY(X)                                 30
057900010326     C     *IN30         IFEQ      *ON
058000060407     c* verifico se incassato
058100060407     c                   z-add     ar6aas        prmaas
058200060407     c                   z-add     ar6lnp        prmlnp
058300060407     c                   z-add     ar6nrs        prmnrs
058400060407     c                   z-add     ar6nsp        prmnsp
058500060407     c                   z-add     ar6fiv        prmfiv
058600060407     c                   z-add     ar6nft        prmnft
058700060407     c                   z-add     ar6dft        prmdft
058800060407     c                   clear                   prmesito
058900060407     c                   call(E)   'YCOW0121R'
059000060407     C                   PARM                    prmaas
059100060407     C                   PARM                    prmLnp
059200060407     C                   PARM                    prmNrs
059300060407     C                   PARM                    prmNsp
059400060407     C                   PARM                    prmFiv
059500060407     C                   PARM                    prmDft
059600060407     C                   PARM                    prmNft
059700060407     C                   PARM                    prmEsito
059800060407     c* stampo solo se prepagato confermato non incassato oppure (se
059900060407     c* richiesto) se prepagato non confermato
060000060407     c                   if        (prmesito=0 and blppdr > *zeros) or
060100060407     c                             (blppdr = *zeros and *in04 = '1')
060200010312      *
060300010312     C                   EXSR      STAMPA
060400060407     c                   endif
060500010326     C                   ENDIF
060600010312      *
060700060407    2C                   ENDDO
060800010312      *
060900921002     C                   ADD       1             C
061000941227    1C                   END
061100010312      *
061200060407     c                   write     fines
061300060407     c                   if        *in04 = '1'
061400060407     c                   write     fines1
061500060407     c                   endif
061600060407     c*
061700060407     C                   CLOSE     FNLSE8P
061800010329      *
061900010329     C     *IN04         IFEQ      *ON
062000060407     C                   CLOSE     FNLSE8P1
062100010329     C                   ENDIF
062200010312      *
062300921002     C                   ENDSR
062400010312      **************************************************************************
062500010312      *    STAMPA LISTA
062600010312      **************************************************************************
062700900517     C     STAMPA        BEGSR
062800010326      *
062900010326     C                   CLEAR                   STPRSC
063000010326     C                   CLEAR                   STPDIV
063100010326     C                   CLEAR                   STPIFT
063200010326     C                   CLEAR                   STPFIV
063300010326     C                   CLEAR                   STPNFT
063400010326     C                   CLEAR                   STPDFT
063500010327     C                   CLEAR                   STPDSP
063600060407     C                   CLEAR                   p1cinc
063700010327      *
063800010312      *
063900010312      * Ragione sociale Autotrasportatore
064000021205     C                   move      BlpPdr        kpdpdr
064100021203     C                   move      'A'           kpdtip
064200021203     C     kfapd         chain     fiapd01L                           30
064300010312     C     *IN30         IFEQ      *OFF
064400010312     C                   MOVEL     APDRSC        STPRSC
064500010312     C                   ENDIF
064600060407     c* prepagato non confermato e già incassato
064700060407     c                   if        prmesito <> 0 and blppdr = *zeros
064800060407     c                             and *in04 = '1'
064900060407     c                   eval      p1cinc = 'INCASSATO'
065000060407     c                   else
065100060407     c                   clear                   p1cinc
065200060407     c                   endif
065300010312      *
065400010326     C                   MOVE      AR6DIV        STPDIV
065500010312     C                   Z-ADD     AR6IFT        STPIFT
065600010312     C                   Z-ADD     AR6FIV        STPFIV
065700010312     C                   Z-ADD     AR6NFT        STPNFT
065800010327      *
065900010327      * Giro data fattura
066000010312     C                   CLEAR                   WLBDAT
066100010312     C                   Z-ADD     AR6DFT        G02INV
066200010321     C                   MOVEL     '3'           G02ERR
066300010312     C                   CALL      'XSRDA8'
066400010312     C                   PARM                    WLBDAT
066500010312     C                   Z-ADD     G02DAT        STPDFT
066600010327      *
066700010327      * Giro data spedizione
066800010327     C                   CLEAR                   WLBDAT
066900010327     C                   Z-ADD     BLPDSP        G02INV
067000010327     C                   MOVEL     '3'           G02ERR
067100010327     C                   CALL      'XSRDA8'
067200010327     C                   PARM                    WLBDAT
067300010327     C                   Z-ADD     G02DAT        STPDSP
067400060407      * Prepagati confermati e non incassati
067500010327     C     BLPPDR        IFNE      *ZEROS
067600010312     C   98              WRITE     TESTA
067700060412     c                   setoff                                       98
067800170320     C
067900170320     c                   add       ar6ift        ifttot
068000170320     C                   ADD       1             stconta
068100010327     C                   WRITE     RIGA
068200010327     C                   ENDIF
068300170320
068400010327      * Prepagati non confermati
068500010327     C     BLPPDR        IFEQ      *ZEROS
068600010329     C     *IN04         ANDEQ     *ON
068700010329      * STAMPA TESTATA
068800010327     C   97              WRITE     TESTA1
068900060412     c                   setoff                                       97
069000170320     c                   add       ar6ift        ifttot1
069100170320     C                   ADD       1             stconta1
069200010327     C                   WRITE     RIGA1
069300010327     C                   ENDIF
069400010312      *
069500900517     C                   ENDSR
069600010329**
069700010329OVRPRTF FILE(        ) FORMTYPE('          ') OUTQ(          ) USRDTA('Prep.Part
069800010329')
