000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020418      * FNLS59R *----------------------------------------------------*
000300020418      *         - PROSPETTO NOLI
000400020418      *--------------------------------------------------------------*
000500020418      * QUESTO PGM VIENE RICHIAMATO, PER ORA, DAL SEGUENTE FILTRO:
000600020418      * FNLS58R
000700020418      * E' ANCHE RICHIAMATO DA STAMPE GIORNALIERE.
000800020418      *--------------------------------------------------------------*
000900940921     FAZORG01L  IF   E           K DISK
001000020806     FFNBLP79L  IF   E           K DISK    USROPN
001100940921     FFNFGV02L  IF   E           K DISK
001200940921     FFNFGV01L  IF   E           K DISK
001300020806     F                                     RENAME(FNFGV000:FNFGV001)
001400000427     FFNFGW01L  IF   E           K DISK
001500940927     FFNFVD01L  IF   E           K DISK
001600960221     FFNFV201L  IF   E           K DISK
001700960221     FFNFV401L  IF   E           K DISK
001800900604     FTABEL00F  IF   E           K DISK
001900940922     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002000940922     F                                     USROPN
002100940922     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOE)
002200940922     F                                     USROPN
002300900628     F                                     FORMLEN(66)
002400900628     F                                     FORMOFL(58)
002500970421     FFNLV22P   O    F  132        PRINTER OFLIND(*INOD)
002600970421     F                                     USROPN
002700020806
002800940921     D TE              S             66    DIM(15) CTDATA PERRCD(1)
002900940921     D BO              S             66    DIM(10) CTDATA PERRCD(1)
003000900926     D VE              S             66    DIM(21) CTDATA PERRCD(1)
003100000427     D PLP             S              3    DIM(300)
003200000427     D FLS             S              4    DIM(300)
003300900625     D DES             S              1    DIM(21)
003400900625     D SKI             S              1    DIM(30)
003500900628     D SK2             S              1    DIM(53)
003600941117     D*
003700090910     D unicaTSP        S             12    DIM(10)
003800090910     D TSP             S              1    DIM(10)
003900090910     D DTS             S              8    DIM(10)
004000950127     D VOL1            S             10  3 DIM(10)
004100950127     D VOL2            S             10  3 DIM(10)
004200950127     D VOL3            S             10  3 DIM(10)
004300950127     D VOL4            S             10  3 DIM(10)
004400941117     D*
004500000620     D CMD             S              1    DIM(85) CTDATA PERRCD(80)
004600970430     D CM              S              1    DIM(160) CTDATA PERRCD(80)
004700970421     D TAR             S              3  0 DIM(30)
004800000620     D*
004900000620     D L1              S              3  0 DIM(30)
005000020418     D L6              S              3  0 DIM(30)
005100000620     D LV2             S              3  0 DIM(30)
005200000620     D LIN             S              3  0 DIM(30)
005300020418
005400020418     d sav_t06         S              3  0 dim(30)
005500020418
005600020806      *
005700020806      * VARIABILI
005800020806      *
005900020806     D SAVPGM          S                   LIKE(DS1PGM)
006000020806     D KCOD            S                   LIKE(TBLCOD)
006100020806     D KKEY            S                   LIKE(TBLKEY)
006200020806     D KLNP            S                   LIKE(FVDLNP)
006300020806     D KNFV            S                   LIKE(FVDNFV)
006400020806     D KLNA            S                   LIKE(FVDLNA)
006500020806     D KTRC            S                   LIKE(FVDTRC)
006600020806     D STARTC          S                   LIKE(FV2RTC)
006700020806     D WEPA            S                   LIKE(FV2EPA) INZ('P')
006800020806     D WLAI            S                   LIKE(FV2LAI) INZ
006900020806     D WTDH            S                   LIKE(FV2TDH) INZ('R')
007000020806     D WTAR            S                   LIKE(FGVLNA)
007100020806     D WTARS           S                   LIKE(FGVLNA)
007200020806     D WGWIPT          S                   LIKE(FGWIPT)
007300020806     D WGWVIP          S                   LIKE(FGWVIP)
007400020806     D*
007500020806     D GENIPT          S                   LIKE(TOTIPT)
007600020806     D FILIPT          S                   LIKE(TOTIPT)
007700020806     D*
007800020806     D* Campi "COLLI"
007900020806     D SAVCTT          S                   LIKE(STACLT)
008000020806     D SAVCLT          S                   LIKE(STACLT)
008100020806     D FGVCTT          S                   LIKE(STACLT)
008200020806     D TOTCLT          S                   LIKE(STACLT)
008300020806     D TOTCTT          S                   LIKE(STACLT)
008400020806     D FILCLT          S                   LIKE(STACLT)
008500020806     D FILCTT          S                   LIKE(STACLT)
008600020806     D GENCLT          S                   LIKE(STACLT)
008700020806     D GENCTT          S                   LIKE(STACLT)
008800020806     D TOTCLL          S                   LIKE(STACLT)
008900020806     D TCLLT           S                   LIKE(STACLT)
009000020806     D* Campi "PESO"
009100020806     D SAVKTT          S                   LIKE(STAKGT)
009200020806     D SAVKGT          S                   LIKE(STAKGT)
009300020806     D FGVKTT          S                   LIKE(STAKGT)
009400020806     D TOTKGT          S                   LIKE(STAKGT)
009500020806     D TOTKTT          S                   LIKE(STAKGT)
009600020806     D FILKTT          S                   LIKE(STAKGT)
009700020806     D FILKGT          S                   LIKE(STAKGT)
009800020806     D GENKGT          S                   LIKE(STAKGT)
009900020806     D GENKTT          S                   LIKE(STAKGT)
010000020806     D TOTF35          S                   LIKE(STAKGT)
010100020806     D TOTO35          S                   LIKE(STAKGT)
010200020806     D PES             S                   LIKE(STAKGT)
010300020806     D TOTPES          S                   LIKE(STAKGT)
010400020806     D TF35T           S                   LIKE(STAKGT)
010500020806     D TO35T           S                   LIKE(STAKGT)
010600020806     D PEST            S                   LIKE(STAKGT)
010700020806     D TPEST           S                   LIKE(STAKGT)
010800020806     D* Campi "VOLUME" (solo 2 decimali)
010900020806     D SAVMCT          S                   LIKE(STAMCT)
011000020806     D SAVMTT          S                   LIKE(STAMCT)
011100020806     D FGVMTT          S                   LIKE(STAMCT)
011200020806     D TOTMCT          S                   LIKE(STAMCT)
011300020806     D TOTMTT          S                   LIKE(STAMCT)
011400020806     D FILMCT          S                   LIKE(STAMCT)
011500020806     D FILMTT          S                   LIKE(STAMCT)
011600020806     D GENMCT          S                   LIKE(STAMCT)
011700020806     D GENMTT          S                   LIKE(STAMCT)
011800020806     D SVOL1           S                   LIKE(STAMCT)
011900020806     D SVOL2           S                   LIKE(STAMCT)
012000020806     D SVOL4           S                   LIKE(STAMCT)
012100020806     D*
012200020806     D SAVDFV          S                   LIKE(FGVDFV)
012201130423     D wpmb            S             48
012300020806     D*
012400020806     D COMLIN          S              3  0 INZ
012500020806     D XX              S              3  0 INZ
012600090910     D YY              S              3  0 INZ
012700020806     D XB              S              3  0 INZ
012800020806     D XC              S              3  0 INZ
012900020806     D XD              S              2  0 INZ
013000020806     D XI              S              3  0 INZ
013100020806     D II              S              3  0 INZ
013200020806     D IY              S              3  0 INZ
013300020806     D WA              S              3  0 INZ
013400020806     D WB              S              3  0 INZ
013500020806     D WC              S              3  0 INZ
013600020806     D WPT             S              3  0 INZ
013700020806     D*
013800020806     D WTIME           S             14  0
013900020806     D WDATE           S              8  0
014000020806     D UTIME           S              6  0
014100020806     D*
014200020806     D W0020           S              2  0 INZ
014300020806     D W004A           S              4    INZ
014400090910
014500090910     D dataiso         s               d   datfmt(*iso)
014600020418
014700020806      *
014800020806      * STRUTTURE DATI DEFINITE INTERNAMENTE
014900020806      *
015000000707     D* PASSAGGIO A STAMPA FV PARTENZE      -FNLS94R-
015100000707     D PARAM9          DS
015200000707     D  PA9NFV                11     15
015300000707     D* FNLS94R -->  ' 'STAMPA     'R'RSTAMPA
015400000707     D  PA9FLG                16     16
015500000707     D* SE PA2MSG PIENO --> C'E' STATO ERRORE
015600000707     D  PA9MSG                17     94
015700000707     D* PARF12= 'S'     --> PREMUTO F12
015800000707     D  PA9F12               102    102
015900000707     D  PA9FGS               103    105
016000000707     D* STAMPATE E MODULO PER STAMPA FV
016100000707     D  PA9PRT               164    173
016200000707     D  PA9MDT               174    183
016300931109     D*
016400911204     D WLBDAT          DS
016500940921     D  G02DAT                 1      8  0
016600940921     D  G02INV                 9     16  0
016700940921     D  G02ERR                17     17
016800940921     D  G02TGI                18     22  0
016900910131     D                 DS
017000910405     D  FGVFLP                 1    240
017100910131     D  FGVFL2               241    447
017200010910     D  FNWFL3               448    687
017300000427     D  FGWFL4               688    897
017400000427     D  FLP                    1    897
017500000427     D                                     DIM(299)
017600910131     D                 DS
017700910405     D  FGVFFV                 1    240
017800910131     D  FGVFF2               241    447
017900000427     D  FGWFF3               448    687
018000000427     D  FGWFF4               688    897
018100000427     D  FFV                    1    897
018200000427     D                                     DIM(299)
018300970421     D                 DS
018400970421     D  AA                     1      2  0
018500970421     D  GG                     5      6  0
018600970421     D  DATA                   1      6  0
018700970421     D                 DS
018800970421     D  TAO                    1     90  0
018900970421     D                                     DIM(30)
019000970421     D  TAOALF                 1     90
019100020806      *
019200020806      * STRUTTURE DATI DEFINITE ESTERNAMENTE
019300020806      *
019400900517     D KPJBA         E DS
019500000620     D* DS PER TRUL06R - CARICAMENTO £X
019600000620     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019700000620     D  T06                    1     90  0
019800000620     D                                     DIM(30)
019900950307     D DSLS01        E DS
020000900604     D DS3A          E DS
020100941116     D DS5E          E DS
020200970421     D DSBLB         E DS
020300000619     D CNCR80        E DS
020400900601     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
020500971113     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020600000620     D DS3L          E DS
020700000620     D DS5P          E DS
020800010910      * ds esterna per la moneta di conto
020900010910     D DGED          E DS
021000010910     D DSCV          E DS
021100010910     D TIBS02        E DS                  EXTNAME(TIBS02DS)
021200010910     D YEURCO        E DS                  EXTNAME(YEURCODS)
021300090910     d
021400090910     d dvpodeco      e ds
021500000620      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
021600000620     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
021700020806      *
021800020806      * DEFINIZIONE COSTANTI
021900020806      *
022000931112     D TFVT            C                   CONST('TUTTI')
022100931112     D TFVC            C                   CONST('CHIUSI')
022200931112     D TFVA            C                   CONST('DA CHIUDERE')
022300900625      *---------------------------------------------------------------*
022400900625      * INDICATORI USATI
022500000619      * 01    =CONDIZIONANA LA SUBROUTINE DI ELABORAZIONE "ELAB"
022600000619      * 02/08 =CONDIZIONANO IL VIDEO E LA STAMPA
022700900628      * 09    =STAMPA VERBALE DI CARICO
022800910809      * 10    =STAMPO DATA PARTENZA E ORA E DATA ARRIVO EFFETTIVA SOLO
022900910809      *        SE FV CHIUSO
023000910809      * 11    =RICHIESTO UNO SPECIFICO FV
023100910809      * 12    =ON NON STAMPO IMPOSTO PATTUITO COME RICHIESTO
023200900629      * 13    =STAMPO UINA SOLA VOLTA DATI IN SECONDA RIGA
023300910322      * 14    =STAMPO PROSPETTO NOLI
023400910930      * 16    =ESISTONO TOTALI DEI TRANSITI
023500920318      * 17    =ESEGUO UNA VOLTA UNA DETERMINATA OPERAZIONE
023600920318      * 18    =NON STAMPO LA RIGA DI TRATTINI PERCHE' GIA' STAMPATA RIG
023700941117      * 19    =CONDIZIONA ALCUNE OPERAZIONI DI UNA SUBR USATA PIU'VOLTE
023800941117      * 20/21 =DI COMODO
023900920318      * 22    =STAMPO TOTALI GENERALI
024000920318      * 23    =STAMPATO GIA' ALMENO UN TOTALE PARZIALE
024100960904      * 24    =IL FV E' CHIUSO
024200931109      * 25    =SONO IN SEDE
024300931109      * 26    =SOLO X SEDE: SONO STATE RICHIESTE TUTTE LE LINEE
024400960221      * 27    =STAMPO LA SCRITTA "BIS"
024500900625      * 30/33 = FINE FILE
024600941117      * 35    = USATO PER LOKUP
024700941117      * 40    =CONDIZIONA ALCUNE OPERAZIONI DI UNA SUBR USATA PIU'VOLTE
024800000619      * 53    =CHAIN FNFGW01L PER CONTROLLO P.O. GESTIONE
024900900625      * 99 =
025000000000     C*---------------------------------------------------------------*
025100000000     C     *ENTRY        PLIST
025200000000     C                   PARM                    KPJBA
025300950307     C                   MOVEL     KPJBU         DSLS01
025400940922     C*---------------------------------------------------------------*
025500000707     C* SE HO PREMUTO F10 NEL 58R STAMPO SOLO FV
025600000707     C     DS1FL1        IFEQ      'S'
025700000707     C                   SETON                                        88
025800000707     C                   ELSE
025900000707     C                   SETOFF                                       88
026000000707     C                   ENDIF
026100000707     C*---------------------------------------------------------------*
026200941117     C* CARICAMENTO TABELLE
026300941118     C                   EXSR      CARTAB
026400000620     C**
026500000620     C* CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
026600000620     C                   MOVEL     'P'           DS1TBO
026700000621     C                   MOVEL     DS1PGM        SAVPGM
026800000620     C                   CLEAR                   DS1PGM
026900000620     C                   MOVEL     'FNLS59R'     DS1PGM
027000000620     C                   EXSR      COMUNE
027100000620     C*
027200000620     C                   SETON                                        LR
027300020212     C*****************************************************************
027400020212     C*  PARTE COMUNE STAMPE GIORNALIERE
027500020212     C*
027600020212     C*  . REPERIMENTO 'SIMFEL'
027700020212     C*  . CARICAMENTO '£1'
027800020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
027900020212     C*  . CODA DI STAMPA ('£OUTQ')
028000020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
028100020212     C*  . MODULO PER LA STAMPA ('£MOD')
028200020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
028300020212     C*****************************************************************
028400020212     C     COMUNE        BEGSR
028500020212     C*
028600020212     C*****************************************************************
028700020212     C*       RIEPILOGO INDICATORI:
028800020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
028900020212     C* 11    -           STAMPA PER 1L
029000020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
029100020212     C*****************************************************************
029200020212     C*
029300020212     C* Reperimento parametri aziendali
029400020212     C                   CALL      'X§PARUT'
029500020212     C                   PARM                    UT§DSE
029600020212     C                   MOVEL     RAGUT         RSUT             20
029700020212     C                   MOVEL     REC80         CNCR80
029800020806     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
029900020212     C* Chiave di accesso per il file tabelle
030000020212     C     KTAB1         KLIST
030100020212     C                   KFLD                    CODUT
030200020212     C                   KFLD                    KCOD
030300020212     C                   KFLD                    KKEY
030400020212     C     KTAB2         KLIST
030500020212     C                   KFLD                    CODUT
030600020212     C                   KFLD                    KCOD
030700020806     C* ACCESSO FNFVD01L
030800020806     C     KFVD1         KLIST
030900020806     C                   KFLD                    FGVLNP
031000020806     C                   KFLD                    FGVNFV
031100020806     C     KFVD2         KLIST
031200020806     C                   KFLD                    KLNP
031300020806     C                   KFLD                    KNFV
031400020806     C                   KFLD                    KLNA
031500020806     C                   KFLD                    KTRC
031600020806     C* ACCESSO FNFGV
031700020806     C     KFGV          KLIST
031800020806     C                   KFLD                    SIMFEL
031900020806     C                   KFLD                    DS1ADA
032000020806     C     KFGV2         KLIST
032100020806     C                   KFLD                    DS1LNP
032200020806     C                   KFLD                    DS1ADA
032300020806     C* ACCESSO FNFV201L
032400020806     C     KFV2          KLIST
032500020806     C                   KFLD                    FGVLNP
032600020806     C                   KFLD                    FGVNFV
032700020806     C                   KFLD                    WEPA
032800020806     C                   KFLD                    WLAI
032900020806     C                   KFLD                    WTDH
033000020806      * File FNBLP79L
033100020806     C     K02BLP79      klist
033200020806     C                   kfld                    FGVlnp
033300020806     C                   kfld                    FGVnfv
033400020806      * File FNFGW01L
033500020806     C     K02FGW01      klist
033600020806     C                   kfld                    FGVnfv
033700020806     C                   kfld                    FGVlnp
033800020806      * File FNFGV01L
033900020806     C     K02FGV01      klist
034000020806     C                   kfld                    DS1nfv
034100020806     C                   kfld                    SIMfel
034200020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
034300020212     C*
034400020418     C* CARICO '£1'
034500020212     C                   CLEAR                   KPJBU
034600020212     C                   CLEAR                   DSUL06
034700020212     C                   MOVE      '£1'          D06COD
034800020212     C                   MOVEL     SIMFEL        D06KEY
034900050802     C                   MOVE      'S'           D06KEY
035000050802     C                   MOVEL     ds1ada        D06drf
035100020212     C                   MOVEL     DSUL06        KPJBU
035200020212     C                   CALL      'TRUL06R'
035300020212     C                   PARM                    KPJBA
035400020212     C                   MOVEL     KPJBU         DSUL06
035500020418     c                   movea     t06           sav_t06
035600020418
035700020418      * se in arrivo carico la £6
035800020418 b1  c                   if        ds1tbo = 'A'
035900020418     c                   clear                   wc
036000020418 b2  c                   do        30            wa
036100020422     c                   if        sav_t06(wa) = *zeros
036200020422     c                   leave
036300020422     c                   endif
036400020418     c                   clear                   kpjbu
036500020418     c                   clear                   dsul06
036600020418     c                   eval      d06cod = '£6'
036700020418     c                   movel     sav_t06(wa)   d06key
036800050802     C                   MOVE      'S'           D06KEY
036900020418     c                   eval      d06esc = 'S'
037000050802     C                   MOVEL     ds1ada        D06drf
037100020419     c                   movel     dsul06        kpjbu
037200020418     c                   call      'TRUL06R'
037300020418     c                   parm                    kpjba
037400020418     c                   movel     kpjbu         dsul06
037500020418 b3  c                   do        30            wb
037600020418     c                   if        t06(wb) = *zeros
037700020418     c                   leave
037800020418     c                   endif
037900020418     c                   add       1             wc
038000020418     c                   z-add     t06(wb)       l6(wc)
038100020418 e3  c                   enddo
038200020418 e2  c                   enddo
038300020212     C*
038400020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
038500020212     C                   Z-ADD     *ZERO         WB
038600020418 b2  C                   DO        29            WA
038700020418     C     sav_T06(WA)   LOOKUP    L6                                     30
038800020212     C  N30              ADD       1             WB
038900020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
039000020418 e2  C                   ENDDO
039100020418 x1  C                   ELSE
039200020418     C                   MOVEA     sav_T06       L1
039300020418 e1  C                   ENDIF
039400020212     C*
039500020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
039600020212     C                   MOVEA     L1            LIN
039700020212     C                   CLEAR                   L1
039800020806     C                   Z-ADD     *ZERO         WB
039900020806     C     1             DO        30            WPT
040000020212     C     LIN(WPT)      IFNE      0
040100020212     C                   CLEAR                   FNLSC4
040200020212     C                   Z-ADD     LIN(WPT)      IC4LIN
040300020212     C                   MOVE      'B'           IC4FLG
040400020212     C                   MOVEL     FNLSC4        KPJBU
040500020212     C                   CALL      'FNLSC4R1'
040600020212     C                   PARM                    KPJBA
040700050802     C                   PARM                    ds1ada
040800020212     C                   MOVEL     KPJBU         FNLSC4
040900020212     C     OC4ESE        IFEQ      'S'
041000020212     C                   ADD       1             WB
041100020212     C                   MOVE      LIN(WPT)      L1(WB)
041200020212     C                   ENDIF
041300020212     C                   ENDIF
041400020212     C                   ENDDO
041500070103     C*
041600070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
041700070103     C                   Z-ADD     *ZERO         WB
041800070103     C                   DO        29            WA
041900070103     C     L1(WA)        IFNE      *ZEROS
042000070103     C     L1(WA)        ANDNE     SIMFEL
042100070103     C                   ADD       1             WB
042200070103     C                   Z-ADD     L1(WA)        LV2(WB)
042300070103     C                   ENDIF
042400070103     C                   ENDDO
042500020212     C*
042600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
042700020212     C*
042800020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
042900020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
043000020212     C* la stampa e' lanciata da menu
043100020212     C                   MOVEL     '3L'          KCOD
043200020212     C                   MOVEL     DS1PGM        KKEY
043300020212     C                   MOVE      DS1FPG        KKEY
043400020212     C     KTAB1         CHAIN     TABEL00F                           30
043500020212     C  N30              MOVEL     TBLUNI        DS3L
043600020212     C   30              MOVEL     'F'           §3LD1L
043700020212     C   30              MOVEL     ' '           §3LD2L
043800020212     C   30              MOVEL     '*STD    '    §3LDST
043900020212     C*
044000020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
044100020212     C                   MOVEL     §3LDST        £MOD             10
044200020212     C                   CLEAR                   LIN
044300020212     C*
044400020212     C* Verifico chi ha lanciato la stampa
044500020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
044600020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
044700020212     C* DS1FGS<>0  --->  si proviene da menu
044800020212     C     DS1FGS        IFEQ      0
044900020212     C*
045000020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
045100020212     C* lancio, voglio la stampa;  invece
045200020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
045300020212     C*
045400020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
045500020212     C* creare stampa per 1°lvl. (se stessa)
045600020212     C     DS1LNP        IFEQ      SIMFEL
045700020212     C*
045800020212     C* Il flag §3LD1L ha i seguenti significati:
045900020212     C*    ' ' = tutti i dati
046000020212     C*    '1' = solo filiale di 1' livello
046100020212     C     §3LD1L        IFEQ      '1'
046200020212     C                   SETON                                        11
046300020212     C                   END
046400020212     C*
046500020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
046600020212     C* nella stampa
046700020212     C*
046800020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
046900020212     C  N11              MOVEA     L1            LIN
047000020212     C   11              MOVEA     SIMFEL        LIN(1)
047100020212     C*
047200020212     C* IMPOSTO LA STAMPANTE:
047300020212     C* STAMPANTE TABULATI
047400020212     C                   MOVEL     '5P'          KCOD
047500020806     C                   MOVEL     'T'           W004A
047600020212     C                   MOVE      LIN(1)        W004A
047700020212     C                   MOVEL(P)  W004A         KKEY
047800020212     C     KTAB1         CHAIN     TABEL00F                           99
047900020212     C     *IN99         IFEQ      *OFF
048000020212     C                   MOVEL     TBLUNI        DS5P
048100020212     C                   MOVEL     §5POTQ        £OUTQ            10
048200020212     C                   END
048300020212     C*
048400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
048500020212     C                   EXSR      LEGGI
048600020212     C*
048700020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
048800020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
048900020212     C                   ELSE
049000020212     C*
049100020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
049200020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
049300020212     C*
049400020212     C* Il significato del fla §3LD2L e' il seguente:
049500020212     C*    ' ' = tutti i dati
049600020212     C*    'F' = per ogni 2' livello
049700020212     C     §3LD2L        IFEQ      'F'
049800020212     C                   SETON                                        14
049900020212     C                   ENDIF
050000020212     C*
050100020212     C                   Z-ADD     1             WB
050200020212     C                   MOVEA     *ZEROS        LIN
050300020212     C  N14              MOVEA     L1            LIN
050400020212     C*
050500020212     C* Lancio per tutti i 2° lvl
050600020212     C     DS1LNP        IFEQ      0
050700020212     C*
050800020212     C* LV2 contiene tutte le filiali di 2° lvl
050900020212     C     LV2(WB)       DOWNE     0
051000020212      *
051100020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
051200020212     C                   CLEAR                   FNLSC4
051300020212     C                   Z-ADD     LV2(WB)       IC4LIN
051400020212     C                   MOVEL     FNLSC4        KPJBU
051500020212     C                   CALL      'FNLSC4R1'
051600020212     C                   PARM                    KPJBA
051700050802     C                   PARM                    ds1ada
051800020212     C                   MOVEL     KPJBU         FNLSC4
051900020212      *
052000020212     C     OC4PT1        IFEQ      'S'
052100020212     C     §3LPT         ANDEQ     'N'
052200020212     C                   GOTO      NOLEGG
052300020212     C                   ENDIF
052400020212      *
052500020212     C     OC4EXP        IFEQ      'S'
052600020212     C     §3LEXP        ANDEQ     'N'
052700020212     C                   GOTO      NOLEGG
052800020212     C                   ENDIF
052900020212      *
053000020212     C     OC4DPD        IFEQ      'S'
053100020212     C     §3LDPD        ANDEQ     'N'
053200020212     C                   GOTO      NOLEGG
053300020212     C                   ENDIF
053400020212      *
053500020212     C     OC4BAR        IFEQ      'S'
053600020212     C     §3LBAR        ANDEQ     'N'
053700020212     C                   GOTO      NOLEGG
053800020212     C                   ENDIF
053900020212      *
054000020212     C     OC4FED        IFEQ      'S'
054100020212     C     §3LFED        ANDEQ     'N'
054200020212     C                   GOTO      NOLEGG
054300020212     C                   ENDIF
054400020212      *
054500020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
054600020212     C*  dati degli altri 2° lvl
054700020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
054800020212     C   14              MOVEL     LV2(WB)       LIN(1)
054900020212     C*
055000020212     C* IMPOSTO LA STAMPANTE
055100020212     C* STAMPANTE TABULATI
055200020212     C                   MOVEL     '5P'          KCOD
055300020806     C                   MOVEL     'T'           W004A
055400020212     C                   MOVE      LV2(WB)       W004A
055500020212     C                   MOVEL(P)  W004A         KKEY
055600020212     C     KTAB1         CHAIN     TABEL00F                           99
055700020212     C     *IN99         IFEQ      *OFF
055800020212     C                   MOVEL     TBLUNI        DS5P
055900020212     C                   MOVEL     §5POTQ        £OUTQ            10
056000020212     C                   END
056100020212     C*
056200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
056300020212     C                   EXSR      LEGGI
056400020212     C*
056500020212     C     NOLEGG        TAG
056600020212     C*
056700020212     C                   ADD       1             WB
056800020212     C                   ENDDO
056900020212     C*
057000020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
057100020212     C                   ELSE
057200020212     C*
057300020212     C* Lancio solo per la 2° lvl richiesta
057400020212     C   14              MOVEL     DS1LNP        LIN(1)
057500020212     C*
057600020212     C* STAMPANTE
057700020212     C* STAMPANTE TABULATI
057800020212     C                   MOVEL     '5P'          KCOD
057900020806     C                   MOVEL     'T'           W004A
058000020212     C                   MOVE      DS1LNP        W004A
058100020212     C                   MOVEL(P)  W004A         KKEY
058200020212     C     KTAB1         CHAIN     TABEL00F                           99
058300020212     C     *IN99         IFEQ      *OFF
058400020212     C                   MOVEL     TBLUNI        DS5P
058500020212     C                   MOVEL     §5POTQ        £OUTQ            10
058600020212     C                   END
058700020212     C*
058800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
058900020212     C                   EXSR      LEGGI
059000020212     C*
059100020212     C                   ENDIF
059200020212     C                   ENDIF
059300020212     C*
059400020212     C                   ELSE
059500020212     C*
059600020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
059700020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
059800020212     C     DS1LNP        IFEQ      0
059900020212     C                   MOVEA     L1            LIN
060000020212     C* ...altrimenti elaboro solo se stessa
060100020212     C                   ELSE
060200020212     C                   MOVEL     DS1LNP        LIN(1)
060300020212     C                   ENDIF
060400020212     C*
060500020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
060600020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
060700020212     C* PRENDO QUELLA PRESENTE IN KPJBA
060800020212     C     DS1FGS        IFEQ      SIMFEL
060900020212     C                   MOVEL     KQOTD         £OUTQ
061000020212     C                   ELSE
061100020212     C* STAMPANTE TABULATI
061200020212     C                   MOVEL     '5P'          KCOD
061300020806     C                   MOVEL     'T'           W004A
061400020212     C                   MOVE      DS1FGS        W004A
061500020212     C                   MOVEL(P)  W004A         KKEY
061600020212     C     KTAB1         CHAIN     TABEL00F                           99
061700020212     C     *IN99         IFEQ      *OFF
061800020212     C                   MOVEL     TBLUNI        DS5P
061900020212     C                   MOVEL     §5POTQ        £OUTQ            10
062000020212     C                   END
062100020212     C                   ENDIF
062200020212     C*
062300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
062400020212     C                   EXSR      LEGGI
062500020212     C                   ENDIF
062600020212     C*
062700020212     C                   ENDSR
062800000620     C**************************************************************************
062900000620     C*    CICLO DI LETTURA
063000000620     C**************************************************************************
063100000620     C     LEGGI         BEGSR
063200941116     C*
063300900601     C*
063400900601     C* CONTROLLO RICHIESTE FATTE
063500900608     C                   EXSR      CONTR
063600000620     C*---------------------------------------------------------------*
063700000620     C*
063800000620      * Eseguo OVRPRTF sul file di stampa
063900000620     C                   Z-ADD     85            LUNG             15 5
064000000620     C                   MOVEL     *BLANKS       COMMAN          120
064100000620     C                   MOVEA     'PRTF198'     CMD(14)
064200000620     C                   MOVEA     'Prosp.No'    CMD(34)
064300000620     C                   MOVEA     'li'          CMD(42)
064400000620     C                   MOVEA     £OUTQ         CMD(52)
064500000620     C                   MOVEA     £MOD          CMD(74)
064600000620     C                   MOVEA     CMD           COMMAN
064700000620     C                   CALL      'QCMDEXC'
064800000620     C                   PARM                    COMMAN
064900000620     C                   PARM                    LUNG
065000000620     C                   OPEN      PRTF198
065100000620     C*
065200000620     C                   MOVEL     *BLANKS       COMMAN
065300000620     C                   MOVEA     'QSYSPRT'     CMD(14)
065400000620     C                   MOVEA     'Ver.Cari'    CMD(34)
065500000620     C                   MOVEA     'co'          CMD(42)
065600000620     C                   MOVEA     £OUTQ         CMD(52)
065700000620     C                   MOVEA     £MOD          CMD(74)
065800000620     C                   MOVEA     CMD           COMMAN
065900000620     C                   CALL      'QCMDEXC'
066000000620     C                   PARM                    COMMAN
066100000620     C                   PARM                    LUNG
066200000620     C                   OPEN      QSYSPRT
066300000620     C*
066400000620     C     DS1FL6        IFEQ      'S'
066500000620     C                   Z-ADD     120           LUNG             15 5
066600000620     C                   MOVEL     *BLANKS       COMMAN
066700000620     C                   MOVEA     'FNLV22P'     CM(14)
066800000620     C                   MOVEA     'Etich.FV'    CM(34)
066900000620     C                   MOVEA     '  '          CM(42)
067000000620     C                   MOVEA     DS1MDE        CM(91)
067100000620     C                   MOVEA     DS1PRE        CM(109)
067200000620     C                   MOVEA     CM            COMMAN
067300000620     C                   CALL      'QCMDEXC'
067400000620     C                   PARM                    COMMAN
067500000620     C                   PARM                    LUNG
067600000620     C                   OPEN      FNLV22P
067700000620     C                   END
067800000620     C*
067900000620     C                   SETON                                        OF
068000900523     C*
068100941121     C     *IN25         IFEQ      *ON
068200941121     C* RUOTINE DI LETTURA PER LA SEDE
068300941121     C                   EXSR      LETSED
068400941121     C                   ELSE
068500941121     C*
068600941121     C* ROUTINE DI LETTURA PER LA FILIALE
068700931109     C                   EXSR      LETFIL
068800941121     C                   ENDIF
068900910322     C*
069000901108     C* PER FOGLI VIAGGIO CHIUSI E P.N. SINTETICO STAMPO TOTALE NOLI
069100931118     C*
069200000707     C     *IN88         IFEQ      *OFF
069300931118     C   14
069400931118     CAN 03
069500931118     CAN 05
069600931118     CAN 23              EXCEPT    STFVTO
069700000707     C                   ENDIF
069800920318     C* TOTALI DEL GIORNO
069900920318     C                   ADD       TOTKGT        GENKGT
070000920318     C                   ADD       TOTCLT        GENCLT
070100941125     C                   ADD       TOTMCT        GENMCT
070200920318     C                   ADD       TOTKTT        GENKTT
070300920318     C                   ADD       TOTCTT        GENCTT
070400941125     C                   ADD       TOTMTT        GENMTT
070500920318     C                   ADD       TOTIPT        GENIPT
070600931115     C*
070700931118     C* SOLO PER SEDE: STAMPO TOTALE ULTIMA FILIALE LETTA
070800931227     C   25
070900931227     CAN 03              DO
071000940111     C* DECODIFICO ULTIMA FILIALE LETTA
071100940921     C     SAVLNP        CHAIN     AZORG01L                           30
071200940111     C  N30              MOVEL     ORGDES        DESLNP           20
071300940111     C   30              MOVEL     *BLANKS       DESLNP
071400940111     C*
071500931227     C   OF              DO
071600000707     C  N88              EXCEPT    TESTA
071700000707     C  N88              EXCEPT    TESFV
071800931227     C                   ENDDO
071900000707     C  N88              EXCEPT    TOTFIL
072000931227     C                   ENDDO
072100931115     C*
072200931118     C* TOTALI GENERALI: SE SONO IN SEDE ED E' STATA RICHIESTA SOLO
072300931118     C*                  UNA FILIALE NON LI STAMPO
072400931119     C   14
072500931119     CAN 03
072600931119     CAN 05*IN25         IFEQ      *OFF
072700931119     C     *IN25         OREQ      *ON
072800931119     C     *IN26         ANDEQ     *ON
072900931118     C*
073000931118     C     GENIPT        IFNE      0
073100920318     C     GENCLT        ORNE      0
073200920318     C                   SETON                                        22
073300920318     C                   Z-ADD     GENIPT        TOTIPT
073400920318     C                   Z-ADD     GENKGT        TOTKGT
073500920318     C                   Z-ADD     GENCLT        TOTCLT
073600941125     C                   Z-ADD     GENMCT        TOTMCT
073700920318     C                   Z-ADD     GENKTT        TOTKTT
073800920318     C                   Z-ADD     GENCTT        TOTCTT
073900941125     C                   Z-ADD     GENMTT        TOTMTT
074000920318     C   OF              DO
074100000707     C  N88              EXCEPT    TESTA
074200000707     C  N88              EXCEPT    TESFV
074300920318     C                   END
074400000707     C  N88              EXCEPT    STFVTO
074500920318     C                   END
074600931118     C                   ENDIF
074700931118     C*
074800970421     C     DS1FL6        IFEQ      'S'
074900970421     C                   CLOSE     FNLV22P
075000970421     C                   END
075100970421     C*
075200971113     C                   CLEAR                   DSLV55
075300971113     C                   MOVEL     'C'           D55TLA
075400971113     C                   CALL      'FNLV55R'
075500971113     C                   PARM                    DSLV55
075600000620     C                   ENDSR
075700941116     C*
075800940921     C**************************************************************************
075900941116     C*    CARICAMENTO TABELLE
076000941116     C**************************************************************************
076100941116     C     CARTAB        BEGSR
076200090910     C                   TIME                    W0140            14 0
076300090910     C                   MOVE      W0140         UDATE8            8 0
076400090910     C                   TIME                    UTIME             6 0
076500090910     c     *eur          movel     udate8        dataiso
076600090910     c                   movel     dataiso       AMGdate           8 0
076700090910
076800090910     c* carico la tabella VPODECO per la decorrenza del tipo servizio H
076900090910     c                   clear                   tibs02
077000090910     c                   clear                   dvpodeco
077100090910     c                   eval      t02mod = 'C'
077200090910     c                   eval      t02sif = knsif
077300090910     c                   eval      t02cod = 'VPO'
077400090910     c                   eval      t02ke1 = 'DECO'
077500090910     c                   call      'TIBS02R'
077600090910     c                   parm                    kpjba
077700090910     c                   parm                    tibs02
077800090910     c                   if        t02err = *blanks
077900090910     c                   eval      dvpodeco = t02uni
078000090910     c                   else
078100090910     c                   eval      §vpotsph=20091001
078200090910     c                   endif
078300090910     c
078400941116     C****  TIPI SERVIZIO E DESCRIZIONI: TSP / DTS  ****
078500941116     C* CARICO I TIPI SERVIZIO ORDINATI PER NUMERO ORDINAMENTO
078600090910     C                   Z-ADD     1             CODUT
078700020806     C                   Z-ADD     0             XX
078800000620     C                   MOVEL     '5E'          KCOD
078900941116     C     KTAB2         SETLL     TABEL
079000941116     C     KTAB2         READE     TABEL                                  30
079100941116     C*
079200941116     C     *IN30         DOWEQ     *OFF
079300020806     C     TBLFLG        IFEQ      *BLANKS
079400941116     C                   MOVEL     TBLUNI        DS5E
079500090910     c* Escludo le poste e tipo servizio H se ancora da decorrere
079600090910     c                   if        §5euso<>'P'
079700090910     c                   if        tblkey<>'H' or
079800090910     c                             amgdate>=§vpotsph
079900090910     c*  Prima carico una skiera unica per ordinare i tipi servizio
080000090910     c*  poi liletto nelle singole skiere
080100090910     c                   add       1             XX
080200090910     c                   eval      unicatsp(xx)=%editc(§5eord:'X')+
080300090910     c                             %subst(tblkey:1:1)+§5ed08
080400941116     C                   ENDIF
080500090910     C                   ENDIF
080600090910     C                   ENDIF
080700941116     C*
080800941116     C     KTAB2         READE     TABEL                                  30
080900941116     C                   ENDDO
081000090910
081100090910     c                   sorta     unicaTSP
081200090910     c                   eval      xx=1
081300090910     c                   eval      yy=1
081400090910     c                   do        10            xx
081500090910     c                   if        unicatsp(xx)<>*blanks
081600090910     C                   eval      TSP(YY)=%subst(unicaTSP(XX):3:1)
081700090910     C                   eval      dts(YY)=%subst(unicaTSP(XX):4:8)
081800090910     c                   movel     §5ed08        dts(xx)
081900090910     c                   add       1             yy
082000090910     c                   endif
082100090910     c                   enddo
082200941117     C*
082300941117     C* NELLA PRIMA POSIZIONE VUOTA CHE INCONTRO METTO UN "9" PER
082400941117     C*   INDICARE LA POSIZIONE DEL CAMPO DI TOTALE
082500941118     C* SALVO INDICE SCHIERA PER SAPERE LA POSIZIONE DEL CAMPO TOTALE
082600090910     c                   if        yy>0 and yy<=10
082700090910     C                   MOVEL     yy            WIND              3 0
082800090910     C                   MOVEL     '9'           TSP(yy)
082900090910     C                   MOVE      'TOTALE'      DTS(yy)
083000941118     C                   ENDIF
083100090910     c
083200010910      * cerco la moneta di conto per il totale.
083300010910     C                   CLEAR                   DGED
083400010910     C                   CLEAR                   TIBS02
083500010910     C                   MOVEL     'L'           T02TLA
083600010910     C                   MOVEL     'C'           T02MOD
083700010910     C                   MOVEL     KNSIF         T02SIF
083800010910     C                   MOVEL     'GED'         T02COD
083900010910     C                   MOVEL     '1'           T02KE1
084000010910     C                   CALL      'TIBS02R'
084100010910     C                   PARM                    KPJBA
084200010910     C                   PARM                    TIBS02
084300010910     C                   MOVEL     T02UNI        DGED
084400010910      * tabella CV della moneta di conto
084500010910     C                   CLEAR                   DSCV
084600010910     C                   MOVEL     'CV'          KCOD
084700010910     C                   MOVEL     §GEDCN        KKEY
084800010910     C     KTAB1         CHAIN     TABEL00F                           30
084900010910     C     *IN30         IFEQ      *OFF
085000010910     C                   MOVEL     TBLUNI        DSCV
085100010910     C                   ENDIF
085200010910      *
085300971118     C                   ENDSR
085400941116     C*
085500941116     C**************************************************************************
085600940921     C*    CONTROLLI SELEZIONI PASSATE
085700940921     C**************************************************************************
085800900524     C     CONTR         BEGSR
085900000620     C* 25 ON  - SONO IN SEDE
086000000620     C     SIMFEL        IFEQ      0
086100000620     C                   SETON                                        25
086200000620     C                   ENDIF
086300000620     C*
086400000620     C* SE SONO IN FILIALE E FGS=0 E LNP>0, IMPOSTO FGS=LNP
086500000620    1C     SIMFEL        IFGT      0
086600000620    2C     DS1FGS        IFEQ      0
086700000620     C                   Z-ADD     LIN(1)        DS1FGS
086800000620    2C                   ENDIF
086900000620    2C                   ENDIF
087000000620     C**
087100931001     C*
087200931109     C* SOLO X SEDE: 26 ON - SONO STATE RICHIESTE TUTTE LE LINEE
087300950307     C   25DS1LNP        IFEQ      0
087400931109     C                   SETON                                        26
087500931109     C                   ENDIF
087600931109     C*
087700931001     C* ELABORAZIONE PER  N U M E R O   F O G L I O
087800950307     C     DS1NFV        IFNE      0
087900900608     C                   SETON                                        11
088000931001     C*
088100900608     C                   ELSE
088200931001     C*
088300931001     C* ELABORAZIONE PER  D A T A
088400931001     C* CONTROLLO SE IMMESSA DATA AL
088500950307     C     DS1GAL        IFNE      0
088600900601     C                   SETON                                        02
088700900601     C                   END
088800900613     C* CONTROLLO TIPO FOGLIO VIAGGIO
088900950307     C     DS1TFV        IFEQ      'C'
089000900613     C                   SETON                                        05
089100900613     C                   END
089200950307     C     DS1TFV        IFEQ      'A'
089300900613     C                   SETON                                        06
089400900613     C                   END
089500900613     C                   END
089600910322     C* CONTROLLO STAMPA PROSPETTO NOLI
089700950307     C     DS1FL3        IFEQ      'S'
089800910322     C                   SETON                                        14
089900900613     C* CONTROLLO TIPO TABULATO
090000950307     C     DS1FTM        IFEQ      '1'
090100900613     C                   SETON                                        04
090200900613     C                   ELSE
090300900613     C                   SETON                                        03
090400900613     C                   END
090500910322     C*
090600910322     C                   END
090700931001     C*
090800931112     C* DECODIFICO FILIALE PRIMARIA
090900931109     C     *IN25         IFEQ      *OFF
091000940921     C     SIMFEL        CHAIN     AZORG01L                           30
091100931112     C                   ELSE
091200950307     C  N26DS1LNP        CHAIN     AZORG01L                           30
091300931112     C                   ENDIF
091400931112     C*
091500931112     C  N30              MOVEL     ORGDES        DESFIL           25
091600931112     C   30              MOVEL     *BLANKS       DESFIL
091700931112     C*
091800931112     C     *IN26         IFEQ      *ON
091900931112     C                   Z-ADD     0             COMFIL            3 0
092000931112     C                   MOVEL     *BLANKS       DESFIL
092100931112     C                   MOVEL     'TUTTE'       DESFIL
092200931112     C                   ENDIF
092300931109     C*
092400940922     C*
092500900601     C                   ENDSR
092600940921     C**************************************************************************
092700940921     C*    LETTURA PER FILIALE
092800940921     C**************************************************************************
092900931109     C     LETFIL        BEGSR
093000931109     C*
093100000620     C* 11 OFF - E L A B O R1
093200931109     C*
093300931109    1C     *IN11         IFEQ      *OFF
093400931109     C*
093500020806     C     KFGV          SETLL     FNFGV02L
093600931109     C*
093700020806     C
093800020806     C  N02KFGV          READE     FNFGV02L                               30
093900020806     C   02SIMfel        READE     FNFGV02L                               30
094000931109     C*
094100931109    2C     *IN30         DOWEQ     *OFF
094200931109     C*
094300931109     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
094400020806    3C   02FGVDFV        IFGT      DS1AAL
094500931109     C                   SETON                                        30
094600931109    3C                   END
094700931109     C*
094800931109    3C     *IN30         IFEQ      *OFF
094900931109     C                   SETOFF                                       01
095000931109     C*
095100000619     C* P.O. GESTIONE DEVE CORRISPONDERE A PARAMETRO
095200020806     C     K02FGW01      CHAIN     FNFGW01L                           53
095300011015      *
095400011015     C     *IN53         IFEQ      *ON
095500011015     C                   Z-ADD     *ZEROS        FGWIPT
095600011015     C                   CLEAR                   FGWVIP
095700011015     C                   ENDIF
095800011015      *
095900000619+--- C     *IN53         IFEQ      *OFF
096000000619|    C     FGWFGS        ANDNE     0
096100000619|    C     FGWFGS        IFNE      DS1FGS
096200000619|    C                   SETON                                        01
096300000619|    C                   ENDIF
096400000619|    C                   ELSE
096500000619|    C     FGVLNP        IFNE      DS1FGS
096600000619|    C                   SETON                                        01
096700000619|    C                   ENDIF
096800000619+--- C                   ENDIF
096900931109     C* SOLO SE STAMPO PROSPETTO
097000931109     C* 05 ON - SCELTO SOLO I FV CHIUSI
097100931109     C   14
097200931109    4CAN 05FGVATB        IFEQ      *BLANKS
097300931109     C     FGVFCF        ANDEQ     *BLANKS
097400931109     C                   SETON                                        01
097500931109    4C                   END
097600931109     C* 06 ON - SCELTI SOLO I FV APERTI
097700931109     C   14
097800931109    4CAN 06FGVFCF        IFNE      *BLANKS
097900931109     C                   SETON                                        01
098000931109    4C                   END
098100931109     C*
098200931109     C  N01              EXSR      ELAB
098300931109     C*
098400020806     C  N02KFGV          READE     FNFGV02L                               30
098500020806     C   02SIMfel        READE     FNFGV02L                               30
098600931109    3C                   ENDIF
098700931109    2C                   ENDDO
098800931109     C*
098900931109     C                   ELSE
099000931109     C*
099100020806     C* 11 ON -  E L A B O R O   I
099200020806     C     K02FGV01      CHAIN     FNFGV01L                           30
099300931109     C  N30              EXSR      ELAB
099400931109     C                   ENDIF
099500931109     C*
099600931109     C                   ENDSR
099700940921     C**************************************************************************
099800941121     C*    LETTURA PER SEDE
099900941121     C**************************************************************************
100000941121     C     LETSED        BEGSR
100100941121     C                   SETOFF                                       30
100200941121     C*
100300941121     C* 11 OFF - E L A B O R O   P E R   D A T A
100400941121     C     *IN11         IFEQ      *OFF
100500941121     C*
100600941121     C   26              EXSR      IMPO
100700941121     C*
100800941121     C     *IN30         DOWEQ     *OFF
100900941121     C*
101000941121     C     KFGV2         SETLL     FNFGV02L
101100941121     C*
101200941121     C  N02KFGV2         READE     FNFGV02L                               30
101300020806     C   02DS1LNP        READE     FNFGV02L                               30
101400941121     C*
101500941121     C     *IN30         DOWEQ     *OFF
101600941121     C*
101700941121     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
101800950307     C   02FGVDFV        IFGT      DS1AAL
101900941121     C                   SETON                                        30
102000941121     C                   ENDIF
102100941121     C*
102200941121     C     *IN30         IFEQ      *OFF
102300000619     C                   SETOFF                                       01
102400000619     C*
102500000619     C* P.O. GESTIONE DEVE CORRISPONDERE A PARAMETRO
102600020806     C     K02FGW01      CHAIN     FNFGW01L                           53
102700011015      *
102800011015     C     *IN53         IFEQ      *ON
102900011015     C                   Z-ADD     *ZEROS        FGWIPT
103000011015     C                   CLEAR                   FGWVIP
103100011015     C                   ENDIF
103200011015      *
103300000619+--- C     *IN53         IFEQ      *OFF
103400000619|    C     FGWFGS        ANDNE     0
103500000619|    C     FGWFGS        IFNE      DS1FGS
103600000619|    C                   SETON                                        01
103700000619|    C                   ENDIF
103800000619|    C                   ELSE
103900000619|    C     FGVLNP        IFNE      DS1FGS
104000000619|    C                   SETON                                        01
104100000619|    C                   ENDIF
104200000619+--- C                   ENDIF
104300941121     C*
104400000619     C  N01              EXSR      ELAB
104500941121     C*
104600941121     C  N02KFGV2         READE     FNFGV02L                               30
104700020806     C   02DS1LNP        READE     FNFGV02L                               30
104800941121     C                   ENDIF
104900941121     C                   ENDDO
105000941121     C*
105100941121     C   26              EXSR      IMPO
105200941121     C                   ENDDO
105300941121     C*
105400941121     C*
105500941121     C                   ELSE
105600941121     C*
105700941121     C* 11 ON  - E L A B O R O   I L   F O G L I O   V I A G G I O
105800941121     C*
105900020806     C     K02FGV01      chain     FNFGV01L                           30
106000020806     C  N30FGVLNP        IFEQ      DS1LNP
106100020806     C                   EXSR      ELAB
106200020806     C                   ENDIF
106300941121     C*
106400941121     C                   ENDIF
106500941121     C*
106600941121     C                   ENDSR
106700941121     C*
106800941121     C**************************************************************************
106900940921     C*    IMPOSTO LETTURE
107000940921     C**************************************************************************
107100931109     C     IMPO          BEGSR
107200931109     C*
107300950307     C     DS1LNP        SETGT     AZORG01L
107400940921     C                   READ      AZORG01L                               30
107500931112     C*
107600931112     C     *IN30         IFEQ      *OFF
107700931109     C* ESCLUDO LA SEDE
107800931112     C     ORGFIL        IFEQ      046
107900940921     C                   READ      AZORG01L                               30
108000931112     C                   ENDIF
108100931112     C*
108200950307     C                   Z-ADD     ORGFIL        DS1LNP
108300931112     C                   ENDIF
108400931109     C*
108500931109     C                   ENDSR
108600940921     C**************************************************************************
108700940921     C*    ELABORAZIONE
108800940921     C**************************************************************************
108900931001     C     ELAB          BEGSR
109000931001     C*
109100000707     C* STAMPO FOGLIO VIAGGIO SE PREMUTO F10 IN PGM FILTRO
109200000707     C     *IN88         IFEQ      *ON
109300000707      * SALVO KPJBU
109400000707     C                   MOVEL     KPJBU         KPJBU$          256
109500000707      *
109600000707     C                   CLEAR                   PA9MSG
109700000707     C                   CLEAR                   PA9F12
109800000707     C                   MOVEL     ' '           PA9FLG
109900021021     C                   MOVEL     SIMFEL        PA9FGS
110000000707     C                   MOVEL     FGVNFV        PA9NFV
110100000707     C                   MOVEL     KQOTD         PA9PRT
110200000707     C                   MOVEL(P)  '*STD'        PA9MDT
110300000707     C                   MOVEL     PARAM9        KPJBU
110400000707     C                   CALL      'FNLS94C'
110500000707     C                   PARM                    KPJBA
110600000707      * RIPRISTINO KPJBU
110700000707     C                   MOVEL     KPJBU$        KPJBU
110800000707     C                   ENDIF
110900000707     C*
111000960904     C                   SETOFF                                       24
111100960904     C* SE FOGLIO VIAGGIO APERTO --> SCRIVO BORDERIZZATO
111200960904     C* SE FOGLIO VIAGGIO CHIUSO --> SCRIVO PARTITO
111300960904     C     FGVFCF        IFNE      ' '
111400960904     C                   SETON                                        24
111500960904     C                   ENDIF
111600931001     C*
111700931001     C* CARICO DESTINAZIONI
111800931001     C                   EXSR      CARDES
111900931001     C*
112000941130     C* SOLO SE RICHIESTO  X FOGLI VIAGGIO APERTI E NON ANNULLATI
112100931001     C* STAMPO  V E R B A L E    DI   C A R I C O
112200950307     C     DS1FL2        IFEQ      'S'
112300931001     C     FGVFCF        ANDEQ     ' '
112400931001     C     FGVATB        ANDNE     '*'
112500940922     C                   EXSR      STAVER
112600931001     C                   END
112700931001     C*
112800931001     C*
112900931001     C* STAMPO    P R O S P E T T O     N O L I
113000931001     C     *IN14         IFEQ      *ON
113100931001     C*
113200971118     C* STAMPO IL PROSPETTO SOLO PER LA SEDE
113300941130     C     *IN25         IFEQ      *OFF
113400941130     C*
113500931001     C  N17              Z-ADD     FGVDFV        SAVDFV
113600931112     C  N17              Z-ADD     FGVLNP        SAVLNP
113700931001     C  N17              SETON                                        17
113800931001     C*
113900931001     C* STAMPO TESTATA PROSPETTO NOLI
114000931001     C                   EXSR      STANOL
114100931001     C*
114200931001     C* STAMPO I DETTAGLIO DEI FOGLI VIAGGIO TOTALI BORDERO
114300931001     C   04FGVATB        IFNE      '*'
114400931001     C                   EXSR      CALBOR
114500931001     C                   END
114600931001     C*
114700941130     C                   ENDIF
114800000620     C                   END
114900970421     C* SOLO SE RICHIESTO  X FOGLI VIAGGIO APERTI E NON ANNULLATI
115000970421     C* STAMPO  E T I C H E T T A   F O G L I O    V I A G G I O
115100970421     C     DS1FL6        IFEQ      'S'
115200970421     C     FGVFCF        ANDEQ     ' '
115300970421     C     FGVATB        ANDNE     '*'
115400970421     C                   EXSR      STAETI
115500970421     C                   END
115600940921     C*
115700931001     C                   ENDSR
115800940921     C**************************************************************************
115900940921     C* CARICO DESTINAZIONI
116000940921     C**************************************************************************
116100910404     C     CARDES        BEGSR
116200941130     C*
116300000427     C* CHAIN FILE DI ESTENSIONE
116400020806     C     K02FGW01      CHAIN     FNFGW01L                           32
116500000427     C     *IN32         IFEQ      *ON
116600000427     C     FGWATB        ORNE      ' '
116700000427     C                   CLEAR                   FGWFF3
116800000427     C                   CLEAR                   FGWFF4
116900000427     C                   CLEAR                   FGWFL3
117000000427     C                   CLEAR                   FGWFL4
117100000427     C                   ENDIF
117200000427     C**
117300910404     C                   SETOFF                                       21
117400931001     C                   CLEAR                   PLP
117500910404     C* CARICO DESTINAZIONI
117600910404     C* STAMPO LE LINEE DI TRANSITO UNA SOLA VOLTA SU DUE RIGHE
117700910404     C*        STAMPO LA RELATIVA PROVINCIA
117800020806     C                   Z-ADD     0             XI
117900020806     C                   Z-ADD     1             XB
118000020806     C     FLP(XB)       DOWNE     *BLANKS
118100910404     C*
118200020806     C     FLP(XB)       LOOKUP    PLP                                    20
118300910404     C*
118400020806     C  N20              ADD       1             XI
118500020806     C  N20              MOVE      FLP(XB)       PLP(XI)
118600910404     C*
118700020806     C                   ADD       1             XB
118800910404     C*
118900020806     C     XB            IFGT      299
119000910404     C                   SETON                                        21
119100020806     C                   ENDIF
119200910404     C*
119300020806     C  N21              ENDDO
119400910404     C*
119500910404     C* AGGIUNGO ANCHE LA FILIALE DI ARRIVO FINALE
119600910404     C                   MOVE      FGVLNA        COMPRO            3
119700910404     C     COMPRO        LOOKUP    PLP                                    20
119800020806     C  N20              ADD       1             XI
119900020806     C  N20              MOVE      COMPRO        PLP(XI)
120000910404     C*
120100910404     C* LINEE BORDERO' FOGLI VIAGGIO APERTI PER PROS. NOLI SINT E V.C.
120200020806     C                   Z-ADD     1             XI
120300910404     C                   SETOFF                                       21
120400910404     C                   MOVEA     *BLANKS       FLS
120500020806     C     FFV(XI)       DOWGT     *ZEROS
120600910404     C*
120700910404     C                   MOVE      '-'           COM
120800020806     C                   MOVEL     FFV(XI)       COM               4
120900910404     C*
121000020806     C                   MOVE      COM           FLS(XI)
121100020806     C                   ADD       1             XI
121200910404     C*
121300020806     C     XI            IFGT      299
121400910404     C                   SETON                                        21
121500020806     C                   ENDIF
121600910404     C*
121700020806     C  N21              ENDDO
121800910404     C*
121900910404     C* AGGIUNGO ANCHE LA FILIALE DI ARRIVO FINALE
122000910404     C                   MOVEL     FGVLNA        COM
122100020806     C                   MOVE      COM           FLS(XI)
122200940921     C*
122300910404     C                   ENDSR
122400940921     C**************************************************************************
122500940921     C*    STAMPO PROSPETTO NOLI RIGA DEL FOGLIO VIAGGIO
122600940921     C**************************************************************************
122700900601     C     STANOL        BEGSR
122800960221     C                   SETOFF                                       27
122900931112     C*
123000920318     C* SE MODIFICATA LA DATA FV STAMPO TOTALE PARZIALE
123100920318     C   03
123200920318     CAN 05FGVDFV        IFNE      SAVDFV
123300931118     C*
123400000707     C  N88              EXCEPT    STFVTO
123500931118     C*
123600940922     C                   MOVEL     FGVDFV        SAVDFV
123700920318     C* TOTALI GENERALI
123800920318     C                   ADD       TOTKGT        GENKGT
123900920318     C                   ADD       TOTCLT        GENCLT
124000941125     C                   ADD       TOTMCT        GENMCT
124100920318     C                   ADD       TOTKTT        GENKTT
124200920318     C                   ADD       TOTCTT        GENCTT
124300941125     C                   ADD       TOTMTT        GENMTT
124400920318     C                   ADD       TOTIPT        GENIPT
124500920318     C* AZZERO CAMPI TOTALE
124600920318     C                   Z-ADD     0             TOTKGT
124700920318     C                   Z-ADD     0             TOTCLT
124800920318     C                   Z-ADD     0             TOTMCT
124900920318     C                   Z-ADD     0             TOTKTT
125000920318     C                   Z-ADD     0             TOTCTT
125100920318     C                   Z-ADD     0             TOTMTT
125200920318     C                   Z-ADD     0             TOTIPT
125300920318     C                   SETON                                        1823
125400920318     C                   END
125500931112     C*
125600931112     C* SOLO PER SEDE: CALCOLO TOTALI PER FILIALE
125700931118     C     *IN25         IFEQ      *ON
125800931118     C*
125900931118     C* CALCOLO TOTALI A CAMBIO FILIALE E SOLO PER P.N. SINTETICO
126000931118     C   03FGVLNP        IFNE      SAVLNP
126100931118     C*
126200940111     C* DECODIFICO LA LINEA DI PARTENZA DEL FOGLIO VIAGGIO
126300940921     C     SAVLNP        CHAIN     AZORG01L                           30
126400940111     C  N30              MOVEL     ORGDES        DESLNP           20
126500940111     C   30              MOVEL     *BLANKS       DESLNP
126600940111     C*
126700931227     C   OF              DO
126800000707     C  N88              EXCEPT    TESTA
126900000707     C  N88              EXCEPT    TESFV
127000931227     C                   ENDDO
127100000707     C  N88              EXCEPT    TOTFIL
127200931112     C*
127300931112     C                   MOVEL     FGVLNP        SAVLNP            3 0
127400931112     C* AZZERO CAMPI TOTALE
127500931112     C                   Z-ADD     0             FILKGT
127600931112     C                   Z-ADD     0             FILCLT
127700931112     C                   Z-ADD     0             FILMCT
127800931112     C                   Z-ADD     0             FILKTT
127900931112     C                   Z-ADD     0             FILCTT
128000931112     C                   Z-ADD     0             FILMTT
128100931112     C                   Z-ADD     0             FILIPT
128200931118     C                   SETON                                        18
128300931227     C*
128400931227     C* SE CAMBIA LA FILIALE MA NON LA DATA F.V. AZZERO CAMPI PER
128500931227     C*   TOTALI PARZIALI DEL GIORNO
128600931227     C     FGVDFV        IFEQ      SAVDFV
128700931227     C* TOTALI GENERALI
128800931227     C                   ADD       TOTKGT        GENKGT
128900931227     C                   ADD       TOTCLT        GENCLT
129000941125     C                   ADD       TOTMCT        GENMCT
129100931227     C                   ADD       TOTKTT        GENKTT
129200931227     C                   ADD       TOTCTT        GENCTT
129300941125     C                   ADD       TOTMTT        GENMTT
129400931227     C                   ADD       TOTIPT        GENIPT
129500931227     C* AZZERO CAMPI TOTALE
129600931227     C                   Z-ADD     0             TOTKGT
129700931227     C                   Z-ADD     0             TOTCLT
129800931227     C                   Z-ADD     0             TOTMCT
129900931227     C                   Z-ADD     0             TOTKTT
130000931227     C                   Z-ADD     0             TOTCTT
130100931227     C                   Z-ADD     0             TOTMTT
130200931227     C                   Z-ADD     0             TOTIPT
130300931227     C                   ENDIF
130400931227     C*
130500931227     C* SALTO PAGINA A CAMBIO FILIALE
130600000707     C  N88              EXCEPT    TESTA
130700000707     C  N88              EXCEPT    TESFV
130800931112     C                   ENDIF
130900931118     C                   ENDIF
131000931112     C*
131100900606     C* DATA E ORA PARTENZA SOLO SE E' CHIUSO IL FOGLIO VIAGGIO
131200900606     C                   SETOFF                                       10
131300900606     C     FGVFCF        IFNE      *BLANKS
131400931110     C* SE SONO IN SEDE STAMPO SEMPRE DATA E ORA PARTENZA DEL FOGLIO
131500931110     C     *IN25         OREQ      *ON
131600900606     C                   SETON                                        10
131700900628     C                   END
131800931112     C*
131900931112     C* GIRO DATA FV
132000940921     C                   MOVEL     FGVDFV        G02INV
132100940921     C                   MOVEL     *ZEROS        G02DAT
132200940921     C                   MOVEL     '3'           G02ERR
132300940921     C                   CALL      'XSRDA8'
132400911204     C                   PARM                    WLBDAT
132500940921     C                   Z-ADD     G02DAT        STADFV            8 0
132600940921     C*
132700910809     C* SOLO SE RICHIESTA STAMPA DI 1 FOGLIO => DATA FV IN TESTA
132800950307     C   11              Z-ADD     G02DAT        DS1GDA
132900910809     C*
133000910809     C* D A T A                       P A R T E N Z A
133100940921     C                   MOVEL     FGVDTP        G02INV
133200940921     C                   MOVEL     *ZEROS        G02DAT
133300940921     C                   MOVEL     '3'           G02ERR
133400940921     C                   CALL      'XSRDA8'
133500911204     C                   PARM                    WLBDAT
133600020806     C                   MOVE      G02DAT        W0020
133700960221     C                   MOVEL     G02DAT        STADTP            6 0
133800960221     C                   MOVE      W0020         STADTP
133900940922     C*
134000960221     C* CAUSALE RITARDO
134100960221     C                   CLEAR                   STARTC
134200960221     C                   CLEAR                   DESRTC
134201130423     c                   eval      wtdh='R'
134300960221     C     KFV2          CHAIN     FNFV201L                           34
134400960221    1C     *IN34         IFEQ      *OFF
134500960221     C     FV2ATB        ANDEQ     ' '
134600960221     C                   MOVEL     FV2RTC        STARTC
134700960221     C*
134800960221     C* SE CAUSALE GENERICA COME DECODIFICA PRENDO LE DESCRIZIONI
134900960221    2C     FV2RTC        IFEQ      'G'
135000960221     C     KFV2          SETLL     FNFV401L
135100960221     C     KFV2          READE     FNFV401L                               34
135200960221     C     *IN34         DOWEQ     *OFF
135300960221     C     FV4ATB        IFEQ      ' '
135400960221     C                   MOVEL     FV4NOT        DESRTC
135500960221     C                   SETON                                        34
135600960221     C                   ENDIF
135700960221     C  N34KFV2          READE     FNFV401L                               34
135800960221     C                   ENDDO
135900960221     C*
136000960221   X2C                   ELSE
136100960221     C* DECODIFICA CAUSALE RITARDO
136200000620     C                   MOVEL     'TC'          KCOD
136300000620     C                   MOVEL(P)  STARTC        KKEY
136400960221     C     KTAB1         CHAIN     TABEL                              34
136500960221     C     *IN34         IFEQ      *OFF
136600960221     C     TBLFLG        ANDEQ     ' '
136700010910     C                   MOVEL     TBLUNI        DESRTC           12
136800960221     C                   ENDIF
136900960221    2C                   ENDIF
137000960221    1C                   ENDIF
137100960221     C*
137200960221     C* CATEGORIA TRAINO
137300960221     C     FGVSNP        IFEQ      'B'
137400960221     C                   SETON                                        27
137500960221     C                   ENDIF
137600960221     C*
137700940927     C* I TOTALI DEL FOGLIO VIAGGIO LI PRENDO DA FNFVD
137800940922     C                   EXSR      LETFVD
137900920318     C*
138000931110     C** S O L O   P E R   S E D E
138100931110     C* L'IMPORTO PATTUITO LO STAMPO SEMPRE
138200960904     C     *IN25         IFEQ      *ON
138300931111     C                   MOVEL     'S'           FGVSNS
138400931111     C                   ENDIF
138500931111     C*
138600920318     C* TOTALE GENERALI DEL GIORNO
138700920318     C* 16 ON - TOTALI TRANSITO ESISTENTI DEL FV
138800910930     C     FGVKTT        COMP      0                                  16
138900910930     C  N16FGVCTT        COMP      0                                  16
139000910930     C  N16FGVMTT        COMP      0                                  16
139100931112     C*
139200900621     C* SE NON RICHIESTO NON STAMPO IMPORTO PATTUITO
139300911204     C                   SETOFF                                       12
139400911204     C   04FGVSNS        COMP      'N'                                    12
139500011015      *
139600011015     C                   Z-ADD     *ZEROS        WGWIPT
139700011015     C                   CLEAR                   WGWVIP
139800931112     C*
139900920318     C* TOTALI DEL GIORNO  X P.N. SINTETICO
140000920318     C   03
140100920318     CAN 05FGVATB        IFEQ      *BLANKS
140200011015      * Se l'importo sul file dell'estensione è a 0 prendo quello
140300011015      * del file principale
140400011015     C     FGWIPT        IFEQ      *ZEROS
140500011015     C     FGVIPT        ANDNE     *ZEROS
140600011015     C                   Z-ADD     FGVIPT        WGWIPT
140700011015     C                   MOVEL     'ITL'         WGWVIP
140800011015     C                   ENDIF
140900011015      * Se l'importo sul file dell'estensione è > 0 lo prendo
141000011015     C     FGWIPT        IFNE      *ZEROS
141100011015     C                   Z-ADD     FGWIPT        WGWIPT
141200011015     C                   MOVEL     FGWVIP        WGWVIP
141300011015     C                   ENDIF
141400011015      *
141500011015     C                   Z-ADD     WGWIPT        CNVIPT           11 2
141600011015     C     WGWVIP        IFNE      §GEDCN
141700010910     C                   EXSR      CONVE
141800010910     C                   ENDIF
141900011015     C                   ADD       CNVIPT        TOTIPT           12 2
142000011015     C                   Z-ADD     WGWIPT        PIPT             10 2
142100010910      *
142200941125     C                   ADD       STAKGT        TOTKGT
142300941125     C                   ADD       STACLT        TOTCLT
142400941125     C                   ADD       STAMCT        TOTMCT
142500931112     C*
142600920318     C*         TOTALE TRANSITO GENERALI DEL GIORNO
142700941125     C                   ADD       FGVKTT        TOTKTT
142800941125     C                   ADD       FGVCTT        TOTCTT
142900941125     C                   ADD       FGVMTT        TOTMTT
143000931112     C*
143100931112     C* SOLO PER SEDE: TOTALI PER FILIALE
143200931112     C     *IN25         IFEQ      *ON
143300931112     C                   ADD       FGVIPT        FILIPT
143400931112     C                   ADD       STAKGT        FILKGT
143500941125     C                   ADD       STACLT        FILCLT
143600941125     C                   ADD       STAMCT        FILMCT
143700931112     C                   ADD       FGVKTT        FILKTT
143800941125     C                   ADD       FGVCTT        FILCTT
143900941125     C                   ADD       FGVMTT        FILMTT
144000931112     C                   ENDIF
144100931112     C*
144200920318     C                   END
144300931112     C*
144400920318     C* RIGA DIVISONE FV
144500000707     C     *IN88         IFEQ      *OFF
144600920318     C   03
144700920318     CANN18
144800920318     CANNOF              EXCEPT    STFV3
144900000707     C                   ENDIF
145000920318     C  NOF              SETOFF                                       18
145100920318     C*
145200900606     C* STAMPO SEMPRE SOLO SE STAMPA ANALITICA
145300941125     C* SE IL F.V. PRECEDENTE NON AVEVA DETTAGLIO, POSSO EVITARE
145400941125     C* DI STAMPARE NUOVAMENTE L'INTESTAZIONE F.V.
145500941125     C     WTESFV        IFEQ      *BLANKS
145600000707     C  N88
145700000707     CAN 04
145800000707     CANNOF              EXCEPT    TESFV
145900941125     C                   MOVEL     '1'           WTESFV            1
146000941125     C                   ELSE
146100000707     C  N88
146200000707     CAN 04
146300000707     CANNOF              EXCEPT    STFV3
146400941125     C                   ENDIF
146500941125     C*
146600900626     C* SE ANNULLATO STAMPO LA SCRITTA ANNULLATO
146700900626     C     FGVATB        IFEQ      *BLANKS
146701130423     c* recupero i piombi da fnfv4
146702130423     c                   exsr      piombi
146800900625     C                   EXSR      TRANS
146900900626     C                   ELSE
147000900914     C   OF              DO
147100000707     C  N88              EXCEPT    TESTA
147200000707     C  N88              EXCEPT    TESFV
147300920318     C                   SETOFF                                       OF18
147400900914     C                   END
147500000707     C  N88              EXCEPT    STFV5
147600900626     C                   END
147700900601     C*
147800900601     C                   ENDSR
147900940921     C**************************************************************************
148000940927     C*    LETTURA FNFVD PER TOTALI FOGLIO VIAGGIO
148100940921     C**************************************************************************
148200931110     C     LETFVD        BEGSR
148300931110     C*
148400980213     C     KFVD1         SETLL     FNFVD01L                               33
148500940922     C*
148600980213    1C     *IN33         IFEQ      *ON
148700940922     C* - - - - - - - - - - - - - - - - - - - - -
148800940922     C* ?  T R O V A T O     S U     F L F V D  ?
148900940922     C*
149000940927     C     KFVD1         READE     FNFVD01L                               33
149100931110     C*
149200980213    2C     *IN33         DOWEQ     *OFF
149300931110     C*
149400980213    3C     FVDTRC        IFEQ      '1'
149500940927     C* POSSO AVERE IL RECORD DELL'AREA (1) E QUELLO DEI TRANSITI (2)
149600940927      * ?  D  E  L  L'  A  R  E  A                                             ?
149700931110     C* COLLI
149800931110     C                   ADD       FVDCLA        SAVCLT
149900931110     C* PESO
150000931110     C                   ADD       FVDKFA        SAVKGT
150100931110     C                   ADD       FVDKOA        SAVKGT
150200931110     C* VOLUME
150300941125     C                   ADD(H)    FVDVL1        SAVMCT
150400941125     C                   ADD(H)    FVDVL2        SAVMCT
150500941125     C                   ADD(H)    FVDVL3        SAVMCT
150600941125     C                   ADD(H)    FVDVL4        SAVMCT
150700941125     C                   ADD(H)    FVDVL5        SAVMCT
150800941125     C                   ADD(H)    FVDVL6        SAVMCT
150900931110     C*
151000940927     C* SE HO LETTO IL RECORD DELL'AREA VEDO SE C'E' ANCHE TRANSITO
151100940927     C                   MOVEL     FVDLNP        KLNP
151200940927     C                   MOVEL     FVDNFV        KNFV
151300940927     C                   MOVEL     FVDLNA        KLNA
151400940927     C                   MOVEL     '2'           KTRC
151500980216      * ?  T  R  A  N  S  I  T  I                                              ?
151600940927     C     KFVD2         CHAIN     FNFVD01L                           34
151700980213    4C     *IN34         IFEQ      *OFF
151800980216     C                   EXSR      SAVTRA
151900980213E   4C                   ENDIF
152000980213      * ?  T  R  A  N  S  I  T  I      DA SDI                                  ?
152100980213     C* SE 34 ACCESO CONTROLLO SE C'E' UN TRANSITO LINEA PARTENZA SDI
152200980213     C                   MOVEL     '3'           KTRC
152300980213     C     KFVD2         CHAIN     FNFVD01L                           34
152400980213    4C     *IN34         IFEQ      *OFF
152500980216     C                   EXSR      SAVTRA
152600980216E   4C                   ENDIF
152700980216     C                   ELSE
152800980216      * ?  T  R  A  N  S  I  T  I                                              ?
152900980216     C                   EXSR      SAVTRA
153000980213E   3C                   ENDIF
153100940927     C*
153200931110     C*
153300940927     C     KFVD1         READE     FNFVD01L                               33
153400980213E   2C                   ENDDO
153500940922     C* - - - - - - - - - - - - - - - - - - - - -
153600940922     C                   ELSE
153700940922     C* - - - - - - - - - - - - - - - - - - - - -
153800940922     C* ?  N O N     T R O V A T O     S U     F L F V D  ?
153900960904     C* ?  CERCO DIRETTAMENTE SULLE BOLLE SE FV APERTO  ?
154000941125     C*
154100941125     C* SE SONO IN SEDE NON POSSO LEGGERE FNBLP
154200941125     C     *IN25         IFEQ      *OFF
154300960904     C     *IN24         ANDEQ     *OFF
154400941125     C*
154500020806     C     WOPEN         IFEQ      *BLANKS
154600020806     C                   OPEN      FNBLP79L
154700941125     C                   MOVEL     '1'           WOPEN             1
154800941125     C                   ENDIF
154900940922     C*
155000020806     C     K02BLP79      SETLL     FNBLP79L
155100940922     C* ANNO FOGLIO VIAGGIO
155200940922     C                   MOVEL     FGVDTP        FGVAAS
155300940922     C*
155400940922     C     LEG22         TAG
155500020806     C     K02BLP79      READE     FNBLP79L                               33
155600940922     C  N33              DO
155700940922     C*
155800940922     C* AGGANCIO IL TIPO BOLLA
155900000620     C                   MOVEL     '3A'          KCOD
156000000620     C                   MOVEL     *BLANKS       KKEY
156100000620     C                   MOVEL     BLPCBO        KKEY
156200940922     C     KTAB1         CHAIN     TABEL00F                           32
156300940922     C                   MOVEL     TBLUNI        DS3A
156400940922     C   32              CLEAR                   DS3A
156500940922     C*
156600940922     C* ECSLUDO BOLLE DI RECUPERO
156700940922     C     §3ARBL        IFNE      'R'
156800940922     C* ESCLUDO IL CODICE SOCIALE 2
156900940922     C     BLPSCL        ANDNE     '2'
157000940922     C*
157100940922     C* SE ANNO DATA PARTENZA FV NON E' QUELLO DEL BORDERO' SCARTO
157200940922     C                   MOVEL     BLPDBR        COMAAS            4 0
157300940922     C     COMAAS        IFNE      FGVAAS
157400940922     C                   GOTO      LEG22
157500940922     C                   END
157600940922     C* COLLI
157700940922     C                   ADD       BLPNCL        SAVCLT
157800940922     C* PESO
157900940922     C                   ADD       BLPPKB        SAVKGT
158000940922     C* VOLUME
158100940922     C     BLPVLB        IFEQ      0
158200950127     C                   MOVE      1             BLPVLB
158300940922     C                   END
158400941125     C                   ADD(H)    BLPVLB        SAVMCT
158500940922     C*
158600940922     C                   ENDIF
158700940922     C                   GOTO      LEG22
158800940922     C                   ENDDO
158900941125     C*
159000941125     C                   ENDIF
159100940922     C* - - - - - - - - - - - - - - - - - - - - -
159200940922     C                   ENDIF
159300940922     C*
159400941125     C                   Z-ADD     SAVCTT        FGVCTT
159500941125     C                   Z-ADD     SAVKTT        FGVKTT
159600941125     C                   Z-ADD     SAVMTT        FGVMTT
159700931110     C*
159800931111     C* ADDIZIONO L'AREA AI TRANSITI
159900931111     C                   ADD       SAVCLT        SAVCTT
160000931111     C                   ADD       SAVKGT        SAVKTT
160100941125     C                   ADD       SAVMCT        SAVMTT
160200931111     C*
160300941125     C                   Z-ADD     SAVCTT        STACLT            8 0
160400941125     C                   Z-ADD     SAVKTT        STAKGT            9 1
160500950127     C                   Z-ADD     SAVMTT        STAMCT           10 3
160600931111     C*
160700931111     C* AZZERO I CAMPI
160800931110     C                   CLEAR                   SAVCLT
160900931110     C                   CLEAR                   SAVKGT
161000931110     C                   CLEAR                   SAVMCT
161100931110     C                   CLEAR                   SAVCTT
161200931110     C                   CLEAR                   SAVKTT
161300931110     C                   CLEAR                   SAVMTT
161400931110     C*
161500931110     C                   ENDSR
161600940921     C**************************************************************************
161700940921     C*    TRASFORMO IN LETTERE PER STAMPARE LINEE DI TRANSITO
161800940921     C**************************************************************************
161900900625     C     TRANS         BEGSR
162000900625     C*
162100900629     C                   SETOFF                                       0713
162200900625     C                   MOVEA     *BLANKS       SKI
162300900625     C                   MOVEA     *BLANKS       DES
162400020806     C                   Z-ADD     0             XB
162500020806     C                   Z-ADD     1             XC
162600900625     C*
162700020806     C     PLP(XC)       DOWNE     *BLANKS
162800020806     C                   MOVEL     PLP(XC)       COMLIN
162900940921     C     COMLIN        CHAIN     AZORG01L                           31
163000900626     C                   MOVEA     *BLANKS       DES
163100900625     C                   MOVEA     ORGDES        DES
163200020806     C                   Z-ADD     21            XI
163300020806     C     DES(XI)       DOWEQ     *BLANKS
163400020806     C                   SUB       1             XI
163500900625     C                   END
163600900625     C* PRIMA POSIZIONE BIANCA DELLA SCHIERA
163700020806     C                   ADD       1             XI
163800020806     C                   MOVE      '/'           DES(XI)
163900900625     C*
164000020806     C                   DO        XI            XD
164100020806     C                   ADD       1             XB
164200020806     C                   MOVE      DES(XD)       SKI(XB)
164300900625     C*
164400020806     C     XB            IFEQ      30
164500900625     C* STAMPO TESTATA PROSPETTO NOLI
164600900625     C   OF              DO
164700000707     C  N88              EXCEPT    TESTA
164800000707     C  N88              EXCEPT    TESFV
164900920318     C                   SETOFF                                       OF18
165000900625     C                   END
165100900625     C*
165200900625     C* STAMPO 1 RIGA PROSPETTO NOLI
165300000707     C  N88
165400000707     CANN07              EXCEPT    STFV
165500900625     C* STAMPO 2 RIGA PROSPETTO NOLI
165600000707     C  N88
165700000707     CAN 07              EXCEPT    STFV2
165800900629     C   07              SETON                                        13
165900900625     C*
166000900625     C  N07              SETON                                        07
166100900625     C                   MOVEA     *BLANKS       SKI
166200020806     C                   Z-ADD     0             XB
166300900625     C                   END
166400900625     C                   END
166500900625     C*
166600020806     C                   ADD       1             XC
166700900625     C                   END
166800900625     C*
166900020806     C     XB            IFGT      0
167000900625     C   OF              DO
167100000707     C  N88              EXCEPT    TESTA
167200000707     C  N88              EXCEPT    TESFV
167300920318     C                   SETOFF                                       OF18
167400900625     C                   END
167500900625     C* STAMPO 1 RIGA PROSPETTO
167600000707     C  N88
167700000707     CANN07              EXCEPT    STFV
167800000707     C  N88
167900000707     CAN 07              EXCEPT    STFV2
168000901214     C   07              SETON                                        13
168100900625     C                   END
168200900625     C*
168300900625     C* STAMPO ANCHE 2 RIGA SE NON ANCORA STAMPATA
168400900710     C  N13              MOVEA     *BLANKS       SKI
168500000707     C  N88
168600000707     CANN13              EXCEPT    STFV2
168700900626     C**
168800900903     C*
168900900903     C* SOL PER NOLI SINTETICO STAMPO LE LINEE BORDERO DEI FOGLI APERTI
169000020806     C   03FGVFCF        IFEQ      *BLANKS
169100900626     C* STAMPO
169200020806     C                   Z-ADD     1             XI
169300900626     C*
169400020806     C     XI            DOWLE     300
169500900626     C                   MOVE      *BLANKS       STAFIL
169600020806     C                   MOVEA     FLS(XI)       STAFIL           28
169700900626     C     STAFIL        IFNE      *BLANKS
169800910322     C   OF              DO
169900000707     C  N88              EXCEPT    TESTA
170000000707     C  N88              EXCEPT    TESFV
170100920318     C                   SETOFF                                       OF18
170200910322     C                   END
170300910322     C*
170400000707     C  N88              EXCEPT    STFV4
170500900626     C                   END
170600020806     C                   ADD       7             XI
170700900626     C                   END
170800900626     C*
170900900626     C                   END
171000940921     C*
171100900625     C                   ENDSR
171200940921     C**************************************************************************
171300940921     C*    CALCOLO TOTALI PER BORDERO'
171400940921     C**************************************************************************
171500900601     C     CALBOR        BEGSR
171600930713     C*
171700930713     C** PER STAMPARE IL BORDERIZZATO :
171800930715     C**    - PER QUEL FOGLIO VIAGGIO NON DEVONO ESISTERE RECORD IN
171900940927     C**      FNFVD00F (*IN24 OFF)
172000941118    1C     *IN24         IFEQ      *OFF
172100930715     C**    - IL FOGLIO VIAGGIO DEVE ESSERE APERTO
172200020806     C     FGVFCF        ANDEQ     *BLANKS
172300930713     C*****
172400930713     C*     B   O   R   D   E   R   I   Z   Z   A   T   O
172500930713     C*****
172600000620     C                   MOVEL     '3A'          KCOD
172700051006     c
172800051006     C     WOPEN         IFEQ      *BLANKS
172900051006     C                   OPEN      FNBLP79L
173000051006     C                   MOVEL     '1'           WOPEN             1
173100051006     C                   ENDIF
173200930713     C*
173300020806     C     K02BLP79      SETLL     FNBLP79L
173400911119     C* ANNO FOGLIO VIAGGIO
173500940921     C                   MOVEL     FGVDTP        FGVAAS            4 0
173600900604     C*
173700900601     C     LEG2          TAG
173800020806     C     K02BLP79      READE     FNBLP79L                               31
173900941118    2C  N31              DO
174000940922     C*
174100940922     C* AGGANCIO IL TIPO BOLLA
174200000620     C                   MOVEL     *BLANKS       KKEY
174300000620     C                   MOVEL     BLPCBO        KKEY
174400940922     C     KTAB1         CHAIN     TABEL00F                           32
174500940922     C                   MOVEL     TBLUNI        DS3A
174600940922     C   32              CLEAR                   DS3A
174700930809     C*
174800930713     C* ECSLUDO BOLLE DI RECUPERO
174900940921     C* SE LA BOLLA NON HA DETTAGLIO COLLI NON LA CONSIDERO
175000941118    3C     §3ARBL        IFNE      'R'
175100930809     C* ESCLUDO IL CODICE SOCIALE 2
175200940922     C     BLPSCL        ANDNE     '2'
175300930713     C*
175400911119     C* SE ANNO DATA PARTENZA FV NON E' QUELLO DEL BORDERO' SCARTO
175500940921     C                   MOVEL     BLPDBR        COMAAS            4 0
175600941118    4C     COMAAS        IFNE      FGVAAS
175700910410     C                   GOTO      LEG2
175800941118    4C                   END
175900900601     C*
176000900601     C  N99              Z-ADD     BLPLNA        COMLNA            3 0
176100910410     C* STAMPO GIA' LA TESTATA DEL DETTAGLIO NOLI
176200000707     C  N99
176300000707     CANN88              EXCEPT    STFV3
176400000707     C  N99
176500000707     CANN88              EXCEPT    TESBOR
176600900601     C  N99              SETON                                        99
176700900601     C*
176800941118    4C     BLPLNA        IFNE      COMLNA
176900900601     C                   EXSR      STABOR
177000941118    4C                   END
177100930713     C*
177200930713     C** TOTALE COLLI
177300900601     C                   ADD       BLPNCL        CLL               7 0
177400941125     C                   ADD       BLPNCL        TOTCLL
177500930713     C** TOTALE PESO
177600941118    4C     BLPPKB        IFLE      3500
177700940921     C                   ADD       BLPPKB        F35               8 1
177800941125     C                   ADD       BLPPKB        TOTF35
177900941118   X4C                   ELSE
178000940921     C                   ADD       BLPPKB        O35               8 1
178100941125     C                   ADD       BLPPKB        TOTO35
178200941118    4C                   END
178300900601     C*
178400941125     C                   ADD       BLPPKB        PES
178500941125     C                   ADD       BLPPKB        TOTPES
178600930713     C** TOTALE VOLUME
178700941118    4C     BLPVLB        IFEQ      0
178800950127     C                   MOVE      1             BLPVLB
178900941118    4C                   END
179000930402     C*
179100020806     C                   Z-ADD     1             XX
179200020806     C     BLPTSP        LOOKUP    TSP(XX)                                35
179300941118    4C     *IN35         IFEQ      *ON
179400020806     C                   ADD(H)    BLPVLB        VOL1(XX)
179500020806     C                   ADD(H)    BLPVLB        VOL2(XX)
179600941117     C*
179700020806     C                   Z-ADD     WIND          XX
179800941118     C                   XFOOT     VOL1          SVOL1
179900941118     C                   XFOOT     VOL2          SVOL2
180000941118    4C                   ENDIF
180100930713     C*
180200930713     C** TOTALE SPEDIZIONI
180300940927     C                   ADD       1             SPE               8 0
180400930713     C                   ADD       1             TOTSPE            9 0
180500940927     C*
180600941118    3C                   ENDIF
180700930713     C*
180800900601     C                   GOTO      LEG2
180900941118    2C                   END
181000931215     C*
181100931215     C* STAMPO ULTIMO RAGGRUPPAMENTO PRIMA DEI TOTALI
181200941118    2C     SPE           IFGT      0
181300931215     C                   EXSR      STABOR
181400941118    2C                   END
181500930713     C*****
181600930713     C*****
181700941118   X1C                   ELSE
181800930713     C*****
181900930713     C*     P     A     R     T     I     T     O
182000930713     C*****
182100940927     C* SOLO SE SONO IN SEDE RIFACCIO SETLL SU FNFVD PERCHE' HO PERSO
182200931110     C*   IL POSIZIONAMENTO NELLA ROUTINE LETFVD
182300940927     C     KFVD1         SETLL     FNFVD01L
182400931110     C*
182500940927     C     KFVD1         READE     FNFVD01L                               31
182600930713     C*
182700941118    2C     *IN31         DOWEQ     *OFF
182800941117     C                   SETOFF                                       1940
182900930713     C*
183000931110     C                   Z-ADD     FVDLNA        COMLNA
183100930713     C* STAMPO GIA' LA TESTATA DEL DETTAGLIO NOLI
183200000707     C  N99
183300000707     CANN88              EXCEPT    STFV3
183400000707     C  N99
183500000707     CANN88              EXCEPT    TESBOR
183600930713     C  N99              SETON                                        99
183700930713     C*
183800940927      * ?  D  E  L  L'  A  R  E  A                                             ?
183900941118    3C     FVDTRC        IFEQ      '1'
184000930713     C** TOTALE COLLI
184100940927     C                   Z-ADD     FVDCLA        CLL               7 0
184200941125     C                   ADD       CLL           TOTCLL
184300930713     C** TOTALE PESO
184400940927     C                   Z-ADD     FVDKFA        F35               8 1
184500941125     C                   ADD       F35           TOTF35
184600940927     C                   Z-ADD     FVDKOA        O35               8 1
184700941125     C                   ADD       O35           TOTO35
184800941125     C     F35           ADD       O35           PES
184900941125     C                   ADD       PES           TOTPES
185000930713     C** TOTALE VOLUME
185100941117     C                   SETON                                        19
185200941117     C                   SETOFF                                       40
185300941117     C                   EXSR      CALVLA
185400930713     C** TOTALE SPEDIZIONI
185500940927     C                   Z-ADD     FVDSNA        SPE
185600940927     C                   ADD       SPE           TOTSPE
185700940927     C*
185800940927      * ?  T  R  A  N  S  I  T  I                                              ?
185900940927     C* SE HO LETTO IL RECORD DELL'AREA VEDO SE CI SONO ANCHE TRANSITI
186000940927     C                   MOVEL     FVDLNP        KLNP
186100940927     C                   MOVEL     FVDNFV        KNFV
186200940927     C                   MOVEL     FVDLNA        KLNA
186300940927     C                   MOVEL     '2'           KTRC
186400940927     C     KFVD2         CHAIN     FNFVD01L                           34
186500980216     C  N34              EXSR      CALTRA
186600980216     C* CALCOLO IL TOTALE ANCHE PER TRANSITI PARTENZA SDI
186700980216     C                   MOVEL     '3'           KTRC
186800980216     C     KFVD2         CHAIN     FNFVD01L                           34
186900980216     C  N34              EXSR      CALTRA
187000980213     C*
187100980213      * ?  T  R  A  N  S  I  T  I                                              ?
187200980216     C* CALCOLO I TOTALI PARZIALI DEI TRANSITI
187300020806     C                   Z-ADD     WIND          XX
187400020806     C                   XFOOT     VOL3          VOL3(XX)
187500980216     C* TRANSITI
187600980216     C                   ELSE
187700980216     C* CALCOLO I PARZIALI TRANSITI
187800980216     C     FVDTRC        IFEQ      '2'
187900980216     C                   EXSR      CALTRA
188000980216     C                   MOVEL     FVDLNP        KLNP
188100980216     C                   MOVEL     FVDNFV        KNFV
188200980216     C                   MOVEL     FVDLNA        KLNA
188300980216     C                   MOVEL     '3'           KTRC
188400980216     C     KFVD2         CHAIN     FNFVD01L                           34
188500980216     C                   ENDIF
188600980216     C     FVDTRC        IFEQ      '3'
188700980216     C                   EXSR      CALTRA
188800980216     C                   ENDIF
188900980216     C* CALCOLO I TOTALI PARZIALI DEI TRANSITI
189000020806     C                   Z-ADD     WIND          XX
189100020806     C                   XFOOT     VOL3          VOL3(XX)
189200980213    3C                   ENDIF
189300940927     C                   EXSR      STABOR
189400930713     C*
189500940927     C     KFVD1         READE     FNFVD01L                               31
189600941118    2C                   ENDDO
189700930713     C**
189800941118    1C                   ENDIF
189900930713     C**
190000941118     C* RIEMPO CAMPO DI TOTALE DELLA SCHIERA DEI TOTALI
190100020806     C                   Z-ADD     WIND          XX
190200020806     C                   Z-ADD     SVOL2         VOL2(XX)
190300020806     C                   Z-ADD     SVOL4         VOL4(XX)
190400900604     C*
190500941125     C* STAMPO IL TOTALE F.V. SOLO SE I CAMPI SONO PIENI
190600941118    1C     TOTSPE        IFNE      0
190700941125     C     TOTCLL        ORNE      *ZERO
190800941125     C   OF              DO
190900000707     C  N88              EXCEPT    TESTA
191000000707     C  N88              EXCEPT    TESBOR
191100941125     C                   SETOFF                                       OF
191200941125     C                   END
191300000707     C  N88              EXCEPT    TOT
191400900604     C                   SETOFF                                       99
191500941125     C                   MOVEL     *BLANKS       WTESFV
191600930714     C*
191700930714     C* STAMPO TOTALE TRANSITI
191800941125     C* SE C'E' ALMENO UNA SPEDIZIONE, O COLLO
191900941118    2C     TSPET         IFGT      0
192000941125     C     TCLLT         ORGT      *ZERO
192100941125     C   OF              DO
192200000707     C  N88              EXCEPT    TESTA
192300000707     C  N88              EXCEPT    TESBOR
192400941125     C                   SETOFF                                       OF
192500941125     C                   END
192600000707     C  N88              EXCEPT    TOTTRA
192700941118    2C                   ENDIF
192800941125     C*
192900941125     C* STAMPO UNA RIGA DI SEPARAZIONE '====='
193000000707     C  N88              EXCEPT    TOTRIG
193100930714     C*
193200900604     C* PULIZIA CAMPI TOTALE
193300900604     C                   Z-ADD     0             TOTSPE
193400900604     C                   Z-ADD     0             TOTCLL
193500900604     C                   Z-ADD     0             TOTF35
193600900604     C                   Z-ADD     0             TOTO35
193700900604     C                   Z-ADD     0             TOTPES
193800941117     C                   CLEAR                   VOL2
193900941118     C                   CLEAR                   SVOL2
194000930713     C*
194100930713     C                   Z-ADD     0             TSPET
194200930713     C                   Z-ADD     0             TCLLT
194300930713     C                   Z-ADD     0             TF35T
194400930713     C                   Z-ADD     0             TO35T
194500930713     C                   Z-ADD     0             TPEST
194600980216     C                   Z-ADD     *ZEROS        SPET
194700980216     C                   Z-ADD     0             PEST
194800941117     C                   CLEAR                   VOL4
194900941118     C                   CLEAR                   SVOL4
195000941118    1C                   END
195100940921     C*
195200900601     C                   ENDSR
195300980216     C**************************************************************************
195400980216     C*    CALCOLI PARZIALI TRANSITI
195500980216     C**************************************************************************
195600980216     C     CALTRA        BEGSR
195700980216     C*
195800980216     C* I TRANSITI VANNO SOMMATI AL "DELL'AREA" E POI DA SOLI
195900980216     C** TOTALE COLLI
196000980216     C                   ADD       FVDCLA        CLL
196100980216     C                   ADD       FVDCLA        TOTCLL
196200980216     C** TOTALE PESO
196300980216     C                   ADD       FVDKFA        F35
196400980216     C                   ADD       FVDKFA        TOTF35
196500980216     C                   ADD       FVDKOA        O35
196600980216     C                   ADD       FVDKOA        TOTO35
196700980216     C                   ADD       FVDKFA        PES
196800980216     C                   ADD       FVDKOA        PES
196900980216     C                   ADD       FVDKFA        TOTPES
197000980216     C                   ADD       FVDKOA        TOTPES
197100980216     C** TOTALE VOLUME
197200980216     C                   SETON                                        40
197300980216     C                   SETOFF                                       19
197400980216     C                   EXSR      CALVLA
197500980216     C** TOTALE SPEDIZIONI
197600980216     C                   ADD       FVDSNA        SPE
197700980216     C                   ADD       FVDSNA        TOTSPE
197800980216      * ?  T  R  A  N  S  I  T  I                                              ?
197900980216     C** TOTALE COLLI
198000980216     C                   ADD       FVDCLA        CLLT              7 0
198100980216     C                   ADD       FVDCLA        TCLLT
198200980216     C** TOTALE PESO
198300980216     C                   ADD       FVDKFA        F35T              8 1
198400980216     C                   ADD       FVDKFA        TF35T
198500980216     C                   ADD       FVDKOA        O35T              8 1
198600980216     C                   ADD       FVDKOA        TO35T
198700980216     C                   ADD       FVDKFA        PEST
198800980216     C                   ADD       FVDKOA        PEST
198900980216     C                   ADD       FVDKFA        TPEST
199000980216     C                   ADD       FVDKOA        TPEST
199100980216     C** TOTALE VOLUME
199200980216     C                   EXSR      CALVLT
199300980216     C** TOTALE SPEDIZIONI
199400980216     C                   ADD       FVDSNA        SPET              8 0
199500980216     C                   ADD       FVDSNA        TSPET             9 0
199600980216     C*
199700980216     C                   ENDSR
199800980216     C**************************************************************************
199900980216     C*    SALVO  TOTALI PARZIALI TRANSITI PER FOGLIO VIAAIGGIO
200000980216     C**************************************************************************
200100980216     C     SAVTRA        BEGSR
200200980216     C* COLLI
200300980216     C                   ADD       FVDCLA        SAVCTT
200400980216     C* PESO
200500980216     C                   ADD       FVDKFA        SAVKTT
200600980216     C                   ADD       FVDKOA        SAVKTT
200700980216     C* VOLUME
200800980216     C                   ADD(H)    FVDVL1        SAVMTT
200900980216     C                   ADD(H)    FVDVL2        SAVMTT
201000980216     C                   ADD(H)    FVDVL3        SAVMTT
201100980216     C                   ADD(H)    FVDVL4        SAVMTT
201200980216     C                   ADD(H)    FVDVL5        SAVMTT
201300980216     C                   ADD(H)    FVDVL6        SAVMTT
201400980216     C*
201500980216     C                   ENDSR
201600940921     C**************************************************************************
201700940921     C*    STAMPO TOTALI PER BORDERO'
201800940921     C**************************************************************************
201900900601     C     STABOR        BEGSR
202000941118     C* RIEMPO CAMPO DI TOTALE DELLA SCHIERA DEI TOTALI
202100020806     C                   Z-ADD     WIND          XX
202200020806     C                   Z-ADD     SVOL1         VOL1(XX)
202300940921     C*
202400900601     C* DECODIFICA LINEA ARRIVO
202500940921     C     COMLNA        CHAIN     AZORG01L                           33
202600900601     C  N33              MOVEL     ORGDES        DESLNA           25
202700900601     C   33              MOVEL     *BLANKS       DESLNA
202800920318     C*
202900900601     C* TESTATA + TESTATA BORDERO'
203000900606     C   OF              DO
203100000707     C  N88              EXCEPT    TESTA
203200000707     C  N88              EXCEPT    TESBOR
203300900606     C                   SETOFF                                       OF
203400900606     C                   END
203500900601     C*
203600000707     C  N88              EXCEPT    DET
203700930714     C*
203800930714     C* STAMPO DETTAGLIO TRANSITI
203900941125     C* SE C'E' ALMENO UNA SPEDIZIONE, O COLLO
204000940927     C     SPET          IFGT      0
204100941125     C     CLLT          ORGT      *ZERO
204200000707     C  N88              EXCEPT    DETTRA
204300940927     C                   ENDIF
204400930714     C*
204500900601     C* PULIZIA CAMPI TOTALE PARZIALI
204600930713     C  N24              Z-ADD     BLPLNA        COMLNA
204700900601     C                   Z-ADD     0             CLL
204800940927     C                   Z-ADD     0             SPE
204900900601     C                   Z-ADD     0             F35
205000900601     C                   Z-ADD     0             O35
205100900605     C                   Z-ADD     0             PES
205200941118     C                   CLEAR                   VOL1
205300941118     C                   CLEAR                   SVOL1
205400900601     C*
205500930713     C                   Z-ADD     0             CLLT
205600940927     C                   Z-ADD     0             SPET
205700930713     C                   Z-ADD     0             F35T
205800930713     C                   Z-ADD     0             O35T
205900930713     C                   Z-ADD     0             PEST
206000941118     C                   CLEAR                   VOL3
206100940921     C*
206200900601     C                   ENDSR
206300941117     C*
206400940921     C**************************************************************************
206500941117     C*    ROUTINE DI CALCOLO TOTALI VOLUME DELL'AREA
206600941117     C*    19 ON  - CALCOLO TOT.VOL. DELL'AREA
206700941117     C*    40 ON  - CALCOLO TOT.VOL. TRANSITI DA SOMMARE AL "DELL'AREA"
206800941116     C**************************************************************************
206900941117     C     CALVLA        BEGSR
207000941117     C*
207100020806     C     FVDTS1        IFNE      *BLANKS
207200020806     C                   Z-ADD     1             XX
207300020806     C     FVDTS1        LOOKUP    TSP(XX)                                35
207400941118     C     *IN35         IFEQ      *ON
207500020806     C   19              Z-ADD(H)  FVDVL1        VOL1(XX)
207600020806     C   40              ADD(H)    FVDVL1        VOL1(XX)
207700020806     C                   ADD(H)    FVDVL1        VOL2(XX)
207800941117     C                   ENDIF
207900941117     C                   ENDIF
208000941117     C*
208100020806     C     FVDTS2        IFNE      *BLANKS
208200020806     C                   Z-ADD     1             XX
208300020806     C     FVDTS2        LOOKUP    TSP(XX)                                35
208400941118     C     *IN35         IFEQ      *ON
208500020806     C   19              Z-ADD(H)  FVDVL2        VOL1(XX)
208600020806     C   40              ADD(H)    FVDVL2        VOL1(XX)
208700020806     C                   ADD(H)    FVDVL2        VOL2(XX)
208800941117     C                   ENDIF
208900941117     C                   ENDIF
209000941117     C*
209100020806     C     FVDTS3        IFNE      *BLANKS
209200020806     C                   Z-ADD     1             XX
209300020806     C     FVDTS3        LOOKUP    TSP(XX)                                35
209400941118     C     *IN35         IFEQ      *ON
209500020806     C   19              Z-ADD(H)  FVDVL3        VOL1(XX)
209600020806     C   40              ADD(H)    FVDVL3        VOL1(XX)
209700020806     C                   ADD(H)    FVDVL3        VOL2(XX)
209800941117     C                   ENDIF
209900941117     C                   ENDIF
210000941117     C*
210100020806     C     FVDTS4        IFNE      *BLANKS
210200020806     C                   Z-ADD     1             XX
210300020806     C     FVDTS4        LOOKUP    TSP(XX)                                35
210400941118     C     *IN35         IFEQ      *ON
210500020806     C   19              Z-ADD(H)  FVDVL4        VOL1(XX)
210600020806     C   40              ADD(H)    FVDVL4        VOL1(XX)
210700020806     C                   ADD(H)    FVDVL4        VOL2(XX)
210800941117     C                   ENDIF
210900941117     C                   ENDIF
211000941117     C*
211100020806     C     FVDTS5        IFNE      *BLANKS
211200020806     C                   Z-ADD     1             XX
211300020806     C     FVDTS5        LOOKUP    TSP(XX)                                35
211400941118     C     *IN35         IFEQ      *ON
211500020806     C   19              Z-ADD(H)  FVDVL5        VOL1(XX)
211600020806     C   40              ADD(H)    FVDVL5        VOL1(XX)
211700020806     C                   ADD(H)    FVDVL5        VOL2(XX)
211800941117     C                   ENDIF
211900941117     C                   ENDIF
212000941117     C*
212100020806     C     FVDTS6        IFNE      *BLANKS
212200020806     C                   Z-ADD     1             XX
212300020806     C     FVDTS6        LOOKUP    TSP(XX)                                35
212400941118     C     *IN35         IFEQ      *ON
212500020806     C   19              Z-ADD(H)  FVDVL6        VOL1(XX)
212600020806     C   40              ADD(H)    FVDVL6        VOL1(XX)
212700020806     C                   ADD(H)    FVDVL6        VOL2(XX)
212800941117     C                   ENDIF
212900941117     C                   ENDIF
213000941117     C*
213100020806     C                   Z-ADD     WIND          XX
213200941125     C   19              XFOOT     VOL1          SVOL1
213300941118     C   40              XFOOT     VOL1          SVOL1
213400941125     C                   XFOOT     VOL2          SVOL2
213500941117     C                   ENDSR
213600941117     C*
213700941117     C**************************************************************************
213800941117     C*    ROUTINE DI CALCOLO TOTALI VOLUME DEI TRANSITI
213900941117     C**************************************************************************
214000941117     C     CALVLT        BEGSR
214100941117     C*
214200020806     C     FVDTS1        IFNE      *BLANKS
214300020806     C                   Z-ADD     1             XX
214400020806     C     FVDTS1        LOOKUP    TSP(XX)                                35
214500941118     C     *IN35         IFEQ      *ON
214600020806     C                   ADD(H)    FVDVL1        VOL3(XX)
214700020806     C                   ADD(H)    FVDVL1        VOL4(XX)
214800941117     C                   ENDIF
214900941117     C                   ENDIF
215000941117     C*
215100020806     C     FVDTS2        IFNE      *BLANKS
215200020806     C                   Z-ADD     1             XX
215300020806     C     FVDTS2        LOOKUP    TSP(XX)                                35
215400941118     C     *IN35         IFEQ      *ON
215500020806     C                   ADD(H)    FVDVL2        VOL3(XX)
215600020806     C                   ADD(H)    FVDVL2        VOL4(XX)
215700941117     C                   ENDIF
215800941117     C                   ENDIF
215900941117     C*
216000020806     C     FVDTS3        IFNE      *BLANKS
216100020806     C                   Z-ADD     1             XX
216200020806     C     FVDTS3        LOOKUP    TSP(XX)                                35
216300941118     C     *IN35         IFEQ      *ON
216400020806     C                   ADD(H)    FVDVL3        VOL3(XX)
216500020806     C                   ADD(H)    FVDVL3        VOL4(XX)
216600941117     C                   ENDIF
216700941117     C                   ENDIF
216800941117     C*
216900020806     C     FVDTS4        IFNE      *BLANKS
217000020806     C                   Z-ADD     1             XX
217100020806     C     FVDTS4        LOOKUP    TSP(XX)                                35
217200941118     C     *IN35         IFEQ      *ON
217300020806     C                   ADD(H)    FVDVL4        VOL3(XX)
217400020806     C                   ADD(H)    FVDVL4        VOL4(XX)
217500941117     C                   ENDIF
217600941117     C                   ENDIF
217700941117     C*
217800020806     C     FVDTS5        IFNE      *BLANKS
217900020806     C                   Z-ADD     1             XX
218000020806     C     FVDTS5        LOOKUP    TSP(XX)                                35
218100941118     C     *IN35         IFEQ      *ON
218200020806     C                   ADD(H)    FVDVL5        VOL3(XX)
218300020806     C                   ADD(H)    FVDVL5        VOL4(XX)
218400941117     C                   ENDIF
218500941117     C                   ENDIF
218600941117     C*
218700020806     C     FVDTS6        IFNE      *BLANKS
218800020806     C                   Z-ADD     1             XX
218900020806     C     FVDTS6        LOOKUP    TSP(XX)                                35
219000941118     C     *IN35         IFEQ      *ON
219100020806     C                   ADD(H)    FVDVL6        VOL3(XX)
219200020806     C                   ADD(H)    FVDVL6        VOL4(XX)
219300941117     C                   ENDIF
219400941117     C                   ENDIF
219500941117     C*
219600941125     C                   XFOOT     VOL4          SVOL4
219700941116     C                   ENDSR
219800941117     C*
219900941116     C**************************************************************************
220000940921     C*    STAMPO VERBALE DI CARICO
220100940921     C**************************************************************************
220200900628     C     STAVER        BEGSR
220300940921     C*
220400930630     C* DATA FV
220500940921     C                   MOVEL     FGVDFV        G02INV
220600940921     C                   MOVEL     *ZEROS        G02DAT
220700940921     C                   MOVEL     '3'           G02ERR
220800940921     C                   CALL      'XSRDA8'
220900930630     C                   PARM                    WLBDAT
221000940921     C                   Z-ADD     G02DAT        STADFV
221100900628     C*
221200000707     C  N88              EXCEPT    STVC
221300900628     C*
221400900628     C                   SETOFF                                       09
221500900628     C                   MOVEA     *BLANKS       SK2
221600900628     C                   MOVEA     *BLANKS       DES
221700020806     C                   Z-ADD     0             XB
221800020806     C                   Z-ADD     1             XC
221900900628     C*
222000020806     C     PLP(XC)       DOWNE     *BLANKS
222100020806     C                   MOVEL     PLP(XC)       COMLIN
222200940921     C     COMLIN        CHAIN     AZORG01L                           31
222300900628     C                   MOVEA     *BLANKS       DES
222400900628     C                   MOVEA     ORGDES        DES
222500020806     C                   Z-ADD     21            XI
222600020806     C     DES(XI)       DOWEQ     *BLANKS
222700020806     C                   SUB       1             XI
222800900628     C                   END
222900900628     C* PRIMA POSIZIONE BIANCA DELLA SCHIERA
223000020806     C                   ADD       1             XI
223100020806     C                   MOVE      '/'           DES(XI)
223200900628     C*
223300020806     C                   DO        XI            XD
223400020806     C                   ADD       1             XB
223500020806     C                   MOVE      DES(XD)       SK2(XB)
223600900628     C*
223700020806     C     XB            IFEQ      53
223800000707     C  N88              EXCEPT    STVC2
223900900628     C                   SETON                                        09
224000900628     C                   MOVEA     *BLANKS       SK2
224100020806     C                   Z-ADD     0             XB
224200900628     C                   END
224300900628     C                   END
224400900628     C*
224500020806     C                   ADD       1             XC
224600900628     C                   END
224700900628     C*
224800900628     C* STAMPO ULTIMA RIGA MEMORIZZATA
224900020806     C     XB            IFGT      0
225000000707     C  N88              EXCEPT    STVC2
225100900628     C                   END
225200900628     C* STAMPO LI LINEE BORDERO'
225300020806     C                   Z-ADD     1             XI
225400900628     C*
225500020806     C     XI            DOWLE     300
225600900628     C                   MOVE      *BLANKS       STAFL2
225700020806     C                   MOVEA     FLS(XI)       STAFL2           52
225800900628     C     STAFL2        IFNE      *BLANKS
225900000707     C  N88              EXCEPT    STVC3
226000900628     C                   END
226100020806     C                   ADD       13            XI
226200900628     C                   END
226300900628     C* STAMPO PARTE FINALE
226400130429     C                   MOVEL     FGVpmb        STADCN           16
226500000707     C  N88              EXCEPT    STVC4
226600900628     C*
226700900628     C                   DO        *HIVAL
226800000707     C  N88              EXCEPT    STVC5
226900900628     C  NOE              END
227000900628     C*
227100000707     C  N88              EXCEPT    STVC6
227200900628     C*
227300900628     C                   ENDSR
227400970421     C**************************************************************************
227500970421     C*    STAMPO ETICHETTA FOGLIO VIAGGIO
227600970421     C**************************************************************************
227700970421     C     STAETI        BEGSR
227800970421     C* CARICO SCHIERA DEI TERMINAL DI ARRIVO
227900970421     C                   CLEAR                   TAR
228000970422     C                   CLEAR                   WTAR
228100970422     C                   CLEAR                   WTARS
228200020806     C                   Z-ADD     1             II
228300020806     C                   MOVE      *ZEROS        IY
228400971113     C**
228500000427    1C     II            DOWLE     299
228600970421     C     FFV(II)       ANDGT     *ZEROS
228700971113     C* CERCO TERMINAL DI ARRIVO
228800971113     C                   CLEAR                   DSLV55
228900971113     C                   MOVEL     'A'           D55TPT
229000971113     C                   MOVE      FFV(II)       D55LIN
229100971114     C                   MOVEL     FGVLNP        D55LNP
229200971114     C                   MOVEL     FGVDFV        D55DRF
229300971113     C                   CALL      'FNLV55R'
229400971113     C                   PARM                    DSLV55
229500971113     C*
229600971113     C     D55ERR        IFNE      ' '
229700971113     C                   MOVEL     D55LIN        D55TFA
229800971113     C                   ENDIF
229900971113     C                   MOVEL     D55TFA        WTAR
230000970422     C*
230100970422    2C     WTAR          IFNE      WTARS
230200970422     C                   MOVE      WTAR          WTARS
230300970422     C     WTAR          LOOKUP    TAR                                    33
230400970422    3C     *IN33         IFEQ      *OFF
230500970422     C                   ADD       1             IY
230600970422     C* SE SUPERATI 30 ELEMENTI ACCENDO IND.DI ERR E VADO A FINE ROUTIN
230700970422    4C     IY            IFGT      30
230800980706   X4C                   ELSE
230900970422     C                   MOVE      WTAR          TAR(IY)
231000980706    4C                   END
231100970422    3C                   END
231200970422    2C                   END
231300970421     C                   ADD       1             II
231400970421    1C                   ENDDO
231500970421     C* AGGIUNGO FILIALE FINALE DI ARRIVO
231600970422     C     FGVLNA        LOOKUP    TAR                                    33
231700970422    1C     *IN33         IFEQ      *OFF
231800970422     C                   ADD       1             IY
231900970422     C* SE SUPERATI 30 ELEMENTI ACCENDO IND.DI ERR E VADO A FINE ROUTIN
232000970422    2C     IY            IFGT      30
232100980706    2C                   ELSE
232200970422     C                   MOVE      FGVLNA        TAR(IY)
232300980706    2C                   END
232400970422    1C                   END
232500970421     C     ENDCAR        TAG
232600970421     C* PREPARAZIONE SCHIERA TAO DA PASSARE AL PGM DI STAMPA ETICHETTE
232700970421     C                   SORTA     TAR
232800970421     C                   CLEAR                   TAO
232900970421     C                   CLEAR                   IY
233000970422    1C                   DO        30            II
233100970422    2C     TAR(II)       IFGT      *ZEROS
233200970421     C                   ADD       1             IY
233300970421     C                   MOVE      TAR(II)       TAO(IY)
233400970422    2C                   END
233500970422    1C                   ENDDO
233600970421     C*
233700970421     C                   CLEAR                   DSBLB
233800970421     C*
233900970421     C                   Z-ADD     FGVNFV        §LBNFV
234000970421     C* LA DATA NELLA DS DSBLB E' IN FORMATO GG/MM/AA (6 CIFRE)
234100970421     C                   MOVE      FGVDFV        DATA
234200970421     C                   MOVE      FGVDFV        §LBDAT
234300970421     C                   MOVEL     GG            §LBDAT
234400970421     C                   MOVE      AA            §LBDAT
234500970421     C*
234600970421     C                   MOVEL     FGVDPD        §LBVET
234700130429     C                   MOVEL     FGVpmb        §LBCON
234800970421     C                   MOVE      FGVLNA        §LBLNA
234900970421     C     FGVLNA        CHAIN     AZORG01L                           32
235000970421     C  N32              MOVEL     ORGDES        §LBDLA
235100970421     C   32              CLEAR                   §LBDLA
235200970421     C                   MOVE      TAOALF        §LBTFC
235300970421     C*
235400000621     C                   CALL      SAVPGM
235500970421     C                   PARM                    DSBLB
235600970421     C*
235700970421     C                   ENDSR
235800010910      *----------------------------------------------------------------
235900010910      * CONVERTI - Routine conversione importo
236000010910      *----------------------------------------------------------------
236100010910     C     CONVE         BEGSR
236200010910      *
236300010910     C                   CLEAR                   YEURCO
236400010910     C                   MOVEL     FGWVIP        YECDVI
236500010910     C                   MOVEL     §GEDCN        YECDVO
236600011015     C                   Z-ADD     WGWIPT        YECIMI
236700010910     C                   MOVE      §CVDEC        YECDCO
236800010910     C                   CALL      'YEURCO'
236900010910     C                   PARM                    YEURCO
237000010910     C     YECESI        IFEQ      ' '
237100011015     C                   Z-ADD     YECIMO        CNVIPT
237200010910     C                   ENDIF
237300010910      *
237400130423     c                   ENDSR
237401130423      *----------------------------------------------------------------
237402130423      * CONVERTI - Routine conversione importo
237403130423      *----------------------------------------------------------------
237404130423     C     piombi        BEGSR
237405130423     c                   eval      wtdh='P'
237406130423     c                   clear                   wpmb
237407130423     C     KFV2          SETLL     FNFV401L
237408130423     C     KFV2          READE     FNFV401L
237409130423     C                   dow       not %eof(fnfv401l)
237410130423     C     FV4ATB        IFEQ      ' '
237411130423     c                   if        wpmb=*blanks
237412130423     C                   eval      wpmb=fv4not
237413130423     c                   else
237414130423     c                   eval      %subst(wpmb:36)=fv4not
237415130423     c                   endif
237416130423     C                   ENDIF
237417130423     C     KFV2          READE     FNFV401L
237418130423     C                   ENDDO
237419130423     C                   ENDSR
237500940921     C**************************************************************************
237600940921     C*         P R O S P E T T O     N O L I
237700940921     C**************************************************************************
237800020806     OPRTF198   E    02      TESTA            02
237900931112     O                       TE(1)              132
238000900601     O                       RSUT                20
238100931112     O                  03                       98 'SINTETICO'
238200931112     O                  04                       98 'ANALITICO'
238300950307     O                       DS1GDA             114 '  /  /    '
238400950307     O                       DS1GAL             129 '  /  /    '
238500940921     O                                          155 'FNLS59R'
238600960926     O                       UDATE8             171 '  /  /    '
238700931112     O                       UTIME              183 '  :  :  '
238800900601     O                                          193 'PAG.'
238900900601     O                       PAGE          Z    198
239000020806     O          E   N02      TESTA            02
239100931112     O                       TE(2)              132
239200900601     O                       RSUT                20
239300931112     O                  03                      105 'SINTETICO'
239400931112     O                  04                      105 'ANALITICO'
239500950307     O                       DS1GDA             121 '  /  /    '
239600940921     O                                          155 'FNLS59R'
239700960926     O                       UDATE8             171 '  /  /    '
239800931112     O                       UTIME              183 '  :  :  '
239900900601     O                                          193 'PAG.'
240000900601     O                       PAGE          Z    198
240100020806     O          E   N25N11   TESTA       2
240200931112     O                       TE(3)               66
240300950307     O                       DS1TFV              23
240400931118     O                     05TFVC                33
240500931118     O                     06TFVA                38
240600931118     O                 N05N06TFVT                32
240700020806     O          E    25      TESTA       2
240800960926     O                                              'PUNTO OPERATIVO DI'
240900960926     O                    N26DS1LNP              23
241000960926     O                     26COMFIL              23
241100960926     O                                           25 '-'
241200960926     O                       DESFIL              51
241300941125     O* ---- TESTATA FOGLIO VIAGGIO --------------------------------------------
241400900601     O          E            TESFV       2
241500900601     O                       TE(6)               66
241600920318     O                       TE(8)              132
241700900601     O                       TE(10)             198
241800960221     O                     24                   194 'TOTALE  P A R T I T O'
241900960221     O                    N24                   196 'TOTALE  B O R D E R I Z.'
242000900601     O          E            TESFV       1
242100900601     O                       TE(7)               66
242200900601     O                       TE(9)              132
242300900601     O                       TE(11)             198
242400920318     O          E            TESFV       1
242500920318     O                       TE(5)               66
242600920318     O                       TE(5)              132
242700920318     O                       TE(5)              198
242800940921     O* ------------ DETTAGLIO  (1' RIGA) ------------
242900900601     O          E            STFV        1
243000931112     O                       FGVLNP               3
243100960221     O                       FGVDPD              28
243200960221     O                       FGVTRM              42
243300960221     O                       FGVTMZ              45
243400960221     O                       SKI                 76
243500960221     O                       FGVNT1             107
243600960221     O                 N13   FGVNT2             138
243700960221     O                     10STADTP             147 '  /  /  '
243800960221     O                     10FGVHMP             153 '  :  '
243900960221     O                       STARTC             155
244000960221     O                       STAMCT        4    180
244100960221     O                       STAKGT        4    190
244200960221     O                       STACLT        Z    198
244300940921     O* ------------ DETTAGLIO  (2' RIGA) ------------
244400900625     O          E            STFV2       1
244500931112     O                       FGVNFV        Z      5
244600960221     O                       FGVTTR               7
244700000619     O                       STADFV              27 '  /  /    '
244800960221     O                     27                    31 'BIS'
244900960221     O                 N13   FGVTRR              42
245000960221     O                       SKI                 76
245100130423     O                       wpmb               125
245200010910     O                    N12PIPT          4    138
245300011015     O                    N12WGWVIP             142
245400960221     O                       DESRTC             155
245500960221     O                 N13 16                   169 'DI CUI TRAN:'
245600960221     O                 N13 16FGVMTT        4    180
245700960221     O                 N13 16FGVKTT        4    190
245800960221     O                 N13 16FGVCTT        Z    198
245900941130     O          E       25   STFV2       0  0
246000010910     O                    N12PIPT          4    138
246100011015     O                    N12WGWVIP             142
246200940921     O* ------------
246300900626     O          E            STFV4       1
246400960221     O                       STAFIL              74
246500941125     O* ------------
246600931118     O          E            STFV5       1
246700931115     O                       FGVLNP               3
246800900626     O          E            STFV5       1
246900900626     O                       TE(12)              66
247000900626     O                       FGVNFV        Z      5
247100960221     O                       STADFV              18 '  /  /    '
247200941125     O* ------------
247300900625     O          E            STFV3       1
247400900601     O                       TE(5)               66
247500900601     O                       TE(5)              132
247600900601     O                       TE(5)              198
247700941125     O* ------------
247800900604     O          E            TESBOR      1
247900900604     O                       BO(1)               66
248000900604     O                       BO(3)              132
248100900604     O                       BO(5)              198
248200900604     O          E            TESBOR      1
248300900604     O                       BO(2)               66
248400900604     O                       BO(4)              132
248500900604     O                       BO(6)              198
248600941118     O                       DTS(1)             126
248700941118     O                       DTS(2)             138
248800941118     O                       DTS(3)             150
248900941118     O                       DTS(4)             162
249000941125     O                       DTS(5)             174
249100941118     O                       DTS(6)             186
249200941118     O                       DTS(7)             198
249300940926     O* ------------ DETTAGLIO ANALITICO LINEA PER LINEA ------------
249400940926     O* ------------ DELL'AREA                           ------------
249500900604     O          E            DET         2
249600900604     O                       COMLNA               3
249700900605     O                                            4 '-'
249800900604     O                       DESLNA              29
249900941117     O                       SPE           Z     41
250000941117     O                       CLL           Z     54
250100941117     O                       F35           4     66
250200941117     O                       O35           4     79
250300941117     O                       PES           4     90
250400941117     O                       VOL1(1)       4    126
250500941117     O                       VOL1(2)       4    138
250600941117     O                       VOL1(3)       4    150
250700941117     O                       VOL1(4)       4    162
250800941125     O                       VOL1(5)       4    174
250900941117     O                       VOL1(6)       4    186
251000941117     O                       VOL1(7)       4    198
251100940926     O* ------------ DETTAGLIO ANALITICO LINEA PER LINEA ------------
251200940926     O* ------------ IN TRANSITO                         ------------
251300930714     O          E          24DETTRA      1
251400930713     O                                         +  4 '...... DI CUI TRANSITI:'
251500941117     O                       SPET          Z     41
251600941117     O                       CLLT          Z     54
251700941117     O                       F35T          4     66
251800941117     O                       O35T          4     79
251900941117     O                       PEST          4     90
252000941117     O                       VOL3(1)       4    126
252100941117     O                       VOL3(2)       4    138
252200941117     O                       VOL3(3)       4    150
252300941117     O                       VOL3(4)       4    162
252400941117     O                       VOL3(5)       4    174
252500941117     O                       VOL3(6)       4    186
252600941117     O                       VOL3(7)       4    198
252700940926     O* ------------
252800900604     O          E            TOT         1
252900900604     O                       BO(7)               66
253000900604     O                       BO(7)              132
253100900604     O                       BO(7)              198
253200900604     O          E            TOT         1
253300930713     O                                              'T O T A L E'
253400930714     O                     24                    27 'P A R T I T O'
253500930714     O                    N24                    32 'B O R D E R I Z Z.'
253600941117     O                       TOTSPE        Z     41
253700941117     O                       TOTCLL        Z     54
253800941117     O                       TOTF35        4     66
253900941117     O                       TOTO35        4     79
254000941117     O                       TOTPES        4     90
254100941117     O                       VOL2(1)       4    126
254200941117     O                       VOL2(2)       4    138
254300941117     O                       VOL2(3)       4    150
254400941117     O                       VOL2(4)       4    162
254500941117     O                       VOL2(5)       4    174
254600941117     O                       VOL2(6)       4    186
254700941117     O                       VOL2(7)       4    198
254800941125     O          E          24TOTTRA      1
254900930713     O                                         +  4 '...... DI CUI TRANSITI:'
255000941117     O                       TSPET         Z     41
255100941117     O                       TCLLT         Z     54
255200941117     O                       TF35T         4     66
255300941117     O                       TO35T         4     79
255400941117     O                       TPEST         4     90
255500941117     O                       VOL4(1)       4    126
255600941117     O                       VOL4(2)       4    138
255700941117     O                       VOL4(3)       4    150
255800941117     O                       VOL4(4)       4    162
255900941117     O                       VOL4(5)       4    174
256000941117     O                       VOL4(6)       4    186
256100941117     O                       VOL4(7)       4    198
256200941125     O          E            TOTRIG      1
256300930715     O                       BO(7)               66
256400930715     O                       BO(7)              132
256500930715     O                       BO(7)              198
256600940921     O* ------------ T O T A L I --------
256700920318     O          E            STFVTO      1
256800920318     O                       TE(15)              66
256900920318     O                       TE(15)             132
257000920318     O                       TE(15)             198
257100901108     O          E            STFVTO      1
257200920318     O              N22                          26 'Totali Parziali del '
257300931118     O               22                          23 'TOTALI GENERALI :'
257400940921     O              N22      STADFV              36 '  /  /    '
257500010910     O                       TOTIPT        4    138
257600010910     O                       §GEDCN             142
257700970224     O                       TOTMCT        4    180
257800970224     O                       TOTKGT        4    190
257900970224     O                       TOTCLT        Z    198
258000920318     O          E            STFVTO      1
258100920318     O                                           56 'Di cui Tot. Transiti:'
258200970224     O                       TOTMTT        4    180
258300970224     O                       TOTKTT        4    190
258400970224     O                       TOTCTT        Z    198
258500901108     O          E            STFVTO      1
258600920318     O                       TE(15)              66
258700920318     O                       TE(15)             132
258800920318     O                       TE(15)             198
258900940921     O* ------------ TOTALI FILIALE --------
259000931112     O          E            TOTFIL      1
259100931112     O                       TE(15)              66
259200931112     O                       TE(15)             132
259300931112     O                       TE(15)             198
259400931112     O          E            TOTFIL      1
259500960926     O                                           28 'TOTALI PUNTO OPERATIVO'
259600010910     O                       FILIPT        4    138
259700010910     O                       §GEDCN             142
259800970224     O                       FILMCT        4    180
259900970224     O                       FILKGT        4    190
260000970224     O                       FILCLT        Z    198
260100931112     O          E            TOTFIL      1
260200931118     O                       SAVLNP               9
260300931118     O                                           11 '-'
260400931118     O                       DESLNP              32
260500931112     O                                           56 'Di cui Tot. Transiti:'
260600970224     O                       FILMTT        4    180
260700970224     O                       FILKTT        4    190
260800970224     O                       FILCTT        Z    198
260900931112     O          E            TOTFIL      1
261000931112     O                       TE(15)              66
261100931112     O                       TE(15)             132
261200931112     O                       TE(15)             198
261300940921     O**************************************************************************
261400940921     O*      V E R B A L E     D I     C A R I C O
261500940921     O**************************************************************************
261600900628     OQSYSPRT   E            STVC             02
261700900628     O                       VE(1)               66
261800900628     O                       RSUT                20
261900960926     O                       UDATE8              66 '  /  /    '
262000900628     O          E            STVC           1 04
262100900628     O                       VE(2)               66
262200000619     O                       FGVNFV        Z     42
262300940921     O                       STADFV              61 '  /  /    '
262400900628     O          E            STVC2       1
262500900628     O                    N09VE(3)               66
262600900628     O                       SK2                 66
262700900628     O          E            STVC3       1
262800900628     O                       STAFL2              65
262900900628     O          E            STVC4       2
263000900628     O                       VE(18)              66
263100900628     O                       FGVTMZ              18
263200900628     O          E            STVC4       1
263300900628     O                       VE(19)              66
263400900628     O                       FGVTRM              27
263500910606     O                       FGVTRR              60
263600900926     O          E            STVC4       1
263700900926     O                       VE(20)              66
263800910606     O                       FGVDPD              37
263900910606     O                       STADCN              66
264000940921     O          E            STVC4       1
264100900926     O                       VE(21)              66
264200900926     O                       FGVNT1              47
264300900926     O          E            STVC4       1
264400900926     O                       FGVNT2              47
264500900628     O          E            STVC4       2
264600900628     O                       VE(6)               66
264700900628     O          E            STVC4       1
264800900628     O                       VE(7)               66
264900900628     O          E            STVC4       1
265000900628     O                       VE(8)               66
265100900628     O          E            STVC4       1
265200900628     O                       VE(9)               66
265300900628     O          E            STVC4       1
265400900628     O                       VE(10)              66
265500900628     O          E            STVC4       1
265600900628     O                       VE(11)              66
265700900628     O          E            STVC4       1
265800900628     O                       VE(12)              66
265900900628     O          E            STVC4       2
266000900628     O                       VE(13)              66
266100900628     O          E            STVC4       2
266200900628     O                       VE(14)              66
266300900628     O          E            STVC4       1
266400900628     O                       VE(15)              66
266500900628     O          E            STVC5       2
266600900628     O                       VE(16)              66
266700900628     O          E            STVC6       3
266800900628     O                       VE(17)              66
266900970421     O**************************************************************************
267000970421     OFNLV22P   T    LRNLR
267100940921     O**************************************************************************
267200000620**  -=-=-=-=-  SCHIERA  TE  -=-=-=-=-
267300940921  **  PROSPETTO  NOLI  XXXXXXXXX  DAL XX/XX/XXXX  AL XX/XX/XXXX  **   1
267400940921         **  PROSPETTO  NOLI  XXXXXXXXX  DEL XX/XX/XXXX  **           2
267500940921TIPO FOGLI VIAGGIO : 'X' - XXXXX                                      3
267600940921                                                                      4
267700940921------------------------------------------------------------------    5
267800960221LNP  TP V E T T O R E          TARGA MOTR. TIP  D E S T I N A Z I     6
267900000619NFV  TR P.O.PT    D A T A      TARGA RIMOR MEZ                        7
268000960221O N I      NOTE                                                  N    8
268100960221           PIOMBI                                              PAT    9
268200960221OLO     P A R T E N Z A                                              10
268300960221TUITO   DATA    ORA  CAU                MC CARIC PESO KG     COLLI   11
268400940921                           A   N   N   U   L   L   A   T   O         12
268500940921                                       **  ELENCO  FOGLI  DI  VIAG   13
268600960926GIO  STANDARD  **                PUNTO OPERATIVO DI XXX-XXXXXXXXXX   14
268700940921==================================================================   15
268800940921**  -=-=-=-=-  SCHIERA  BO  -=-=-=-=-
268900941118                               SPEDIZIONI        COLLI           P    1
269000941118                                                           35 Q.LI    2
269100941125      E      S      O                              V        O         3
269200941118  OLTRE 35 Q       TOTALE                                             4
269300941125   L        U        M        E                                       5
269400940926                                                                      6
269500940921==================================================================    7
269600940921XXXXX XXXXXXXXXXXXXXXXXXXX XXXXXXXXXX  X XXXXXXXXXXXXXXXXXXXXXXXXX    8
269700940921XXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX XXXXX,XXXXXXXXX,XX XX    9
269800940921X.XXX.XXX XX:XX XX/XX/XX XX:XX  XX/XX/XX XX/XX/XX  XXX               10
269900940921**  -=-=-=-=-  SCHIERA  VE  -=-=-=-=-
270000940921                       ***  VERBALE  DI  CARICO  ***      XX/XX/XX    1
270100000619FOGLIO VIAGGIO PARTENZE:  NUMERO XXX XXXXX   DATA  XX/XX/XX           2
270200940921Destinazioni:                                                         3
270300940921                                                                      4
270400940921                                                                      5
270500940921        *********************  ***************************            6
270600940921        *                   *  *                         *            7
270700940921    *****                   *  *                         *            8
270800940921  *     *                   *  *                         *            9
270900940921  *     *                   *++*                         *           10
271000940921  ***************************  ***************************           11
271100940921    00                00             00             00               12
271200940921           SEGNACOLLI CARICATI NON LETTI DALLA PISTOLA               13
271300940921      LINEA       SEGNACOLLO            LINEA        SEGNACOLLO      14
271400940921PARTENZA ARRIVO  SERIE  NUMERO     PARTENZA ARRIVO  SERIE  NUMERO    15
271500940921_______ _______  ____ ________     _______ _______  ____ ________    16
271600940921FIRMA  _______________________                                       17
271700940921Tipo Automezzo : x (Autotreno; Motrice; Rimorchio; Semirimorchio)    18
271800940921Motrice .......: xxxxxxxxxx           Rimorchio : xxxxxxxxxx         19
271900940921Vettore .......: XXXXXXXXXXXXXXXXXXXX Conducente: xxxxxxxxxxxxxxxx   20
272000940921NOTE ..........:                                                     21
272100940922** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
272200000620OVRPRTF FILE(          ) USRDTA('          ') OUTQ(XXXXXXXXXX) FORMTYPE('XXXXXXX
272300000620XXX')
272400970421** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
272500970430OVRPRTF FILE(          ) USRDTA('          ') SHARE(*YES) COPIES(2)
272600000620FORMTYPE('          ') OUTQ(XXXXXXXXXX)
