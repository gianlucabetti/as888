000100000614     H DECEDIT('0,') DATEDIT(*DMY.)
000200941116     H* FNLS75R *----------------------------------------------------*
000300941116     H*         - RISTAMPA BOLLE E SEGNACOLLI
000400000000     H*--------------------------------------------------------------*
000500941116     FFNLS75D   CF   E             WORKSTN
000600910207     FAZORG01L  IF   E           K DISK
000700021203     FFIAPD01L  IF   E           K DISK
000800941228     FFNBLP01L  IF   E           K DISK
000900990201     FTABEL00F  IF   E           K DISK
001000910205     FCNACO00F  IF   E           K DISK
001100030613
001200050329     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001300941227     D L1              S              3  0 DIM(30)
001400020628
001500020628     d wpopdr          s              3  0
001600050329     d pflg            s              1    inz
001700020628
001800941228     D PARAM           DS                  INZ
001900941227     D  V1CLNP                 1      3  0
002000941227     D  V1CLNA                 4      6  0
002100941227     D  WDSD                   7     14  0
002200941227     D  WDSA                  15     22  0
002300941227     D*
002400941227     D  V1CLPD                23     25  0
002500941227     D  V1CSED                26     27  0
002600941227     D  V1CNSD                28     34  0
002700941227     D*
002800941227     D  V1CSEA                35     36  0
002900941227     D  V1CNSA                37     43  0
003000941227     D  V1CAAS                44     47  0
003100941227     D*
003200941227     D  VSCSOP                48     48
003300941227     D  VSCLNA                49     51  0
003400941227     D  VSCFLS                52     54  0
003500941227     D  VSCSCD                55     61  0
003600941227     D  VSCSCA                62     68  0
003700941227     D  VSCKSC                69     75  0
003800950116     D*
003900941227     D  VSCCPR                76     82  0
004000950116     D  VSCFPP                83     83
004100950116     D  VSCNRT                84     84
004200970701     D  VSCBAM                85     85
004300000613     D*
004400000613     D  VSCSNP                86     86
004500050329
004600050329     d  vldvcli               87     87
004700050329
004800030613      * PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE  impostare a blanks
004900030613     D  PARFLG               156    156
005000030613     d* - parametri da TRUL90R:
005100030613     d* · pgm.   per stampa LDV
005200030613     d  wD90psl              157    166
005300030613     d* · stampante per LDV in formato "A4" (laser)
005400030613     d  wD90prb4             167    176
005500030613     d* · stampante per LDV in formato "A5" (laser)
005600030613     d  wD90prb5             177    186
005700030613     d* · modulo per stampa LDV su "A4"     (laser)
005800030613     d  wD90mdb4             187    196
005900030613     d* · modulo per stampa LDV su "A5"     (laser)
006000030613     d  wD90mdb5             197    206
006100030613     d* · pgm di stampa etichette segnacolli
006200030613     d  wD90pss              207    216
006300030613     d* · modulo per stampa etichette
006400030613     d  wD90mde              227    236
006500030613     d* · stampante per etichette
006600030613     d  wD90pre              247    256
006700030613      *
006800021203      * Ds esterna per ricerca codice padroncino
006900021203     D FNLV24DS      E DS
007000941227     D***
007100941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007200941227     D***
007300941227     D WLBDAT          DS                  INZ
007400941227     D  G02DAT                 1      8  0
007500941227     D  G02INV                 9     16  0
007600941227     D  G02ERR                17     17
007700941227     D  G02TGI                18     22  0
007800941116     D KPJBA         E DS
007900910205     D TCUDS           DS
008000910205     D  F1                     1      1
008100910205     D  F3                     3      3
008200910205     D  F2                     2      2
008300910205     D  F4                     4      4
008400910205     D  F56                    5      6
008500990201     D DS3C          E DS
008600990201     D DS3A          E DS
008700051018     D Dcli          E DS
008800990201     D CNCR80        E DS
008900941227     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009000030613     D  TCU                  398    697    DIM(50)
009100030613     D  KCU                  698    847P 0 DIM(50)   PACKEVEN
009200941227     D* DS PER TRUL06R - CARICAMENTO £X
009300941227     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009400030613     D  LIN                    1     90  0 DIM(30)
009500030613     D* DS PER TRUL90R - RICHIESTA STAMPANTI
009600030613     D Trul90DS      E DS                  inz
009700000613     D* DS PER "ORGDE3" (CAMPO DI AZORG)
009800000613     D OG143         E DS
009900050329
010000050329     d tibs02ds      e ds
010100021203
010200021203     D kpdpdr          S                   LIKE(APDpdr)
010300021203     D kpdtip          S                   LIKE(APDtip)
010400910412     C****************************************************************
010500910412     C*  RIEPILOGO INDICATORI
010600910412     C***************************************************************
010700050329     c* 01    - STAMPA LDV x CLIENTE
010800910412     C* 30    - DI COMODO
010900910903     C* 40-54 - ERRORE
011000000613     C* 64    - ERRORE S,N,P PER POSTE
011100000613     C* 72    - E' PRESENTE ALMENO 1 P.O. POSTE IN L1
011200910412     C* 90    - ERRORE GENERICO
011300910412     C*****************************************************************
011400941116     C* RICHIESTA STAMPANTI
011500030613     C                   MOVEL     'S'           d90RSE
011600030613     C                   MOVEL     'S'           d90RSB
011700030613     C                   CALL      'TRUL90R'
011800941116     C                   PARM                    KPJBA
011900030613     C                   PARM                    trul90Ds
012000941116     C*
012100030613      * Parametri x ovrprtf
012200030613      * · stampante per etichette
012300030613     c                   movel     D90pre        wD90pre
012400030613      * · modulo per stampa etichette
012500030613     c                   movel     D90mde        wD90mde
012600030613      * · pgm. di stampa etichette segnacolli
012700030613     c                   movel     D90pss        wD90pss
012800030613      * · stampante per LDV in formato "A4" - laser
012900030613     c                   movel     D90prb4       wD90prb4
013000030613      * · stampante per LDV in formato "A5" - laser
013100030613     c                   movel     D90prb5       wD90prb5
013200030613      * · modulo per stampa LDV su "A4" ... - laser
013300030613     c                   movel     D90mdb4       wD90mdb4
013400030613      * · modulo per stampa LDV su "A5" ... - laser
013500030613     c                   movel     D90mdb5       wD90mdb5
013600030613      * · pgm.   per stampa LDV ........... - ad aghi o laser
013700030613     c                   movel     D90psl        wD90psl
013800030613     C*
013900941116     C**
014000941116     C* F3 - FINE
014100030613     C     d90F3         IFEQ      '1'
014200941116     C                   GOTO      FINE
014300941116     C                   ENDIF
014400941116     C**
014500950411     C     INIZIO        TAG
014600000613     C                   MOVE      'N'           VSCSNP            1
014700000613     C                   CLEAR                   OG143
014800000613     C     POUTEN        CHAIN     AZORG01L                           30
014900000613     C     *IN30         IFEQ      *OFF
015000000613     C                   MOVEL     ORGDE3        OG143
015100020628     c                   if        §ogntw = 'PPT'
015200000613     C                   MOVE      'P'           VSCSNP
015300000613     C                   ENDIF
015400000613     C                   ENDIF
015500000613      *
015600950411     C                   CLEAR                   V1CLNA
015700950411     C                   CLEAR                   V1CLNP
015800950411     C                   MOVEL     *ZEROS        V1CDSD
015900950411     C                   MOVEL     *ZEROS        V1CDSA
016000950411     C*
016100950411     C                   CLEAR                   V1CLPD
016200950411     C                   MOVEL     *ZEROS        V1CSED
016300950411     C                   MOVEL     *ZEROS        V1CSEA
016400950411     C                   MOVEL     *ZEROS        V1CNSD
016500950411     C                   MOVEL     *ZEROS        V1CNSA
016600950411     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
016700950411     C                   TIME                    W0140            14 0
016800950411     C* UDATE IN GGMMAAAA
016900950411     C                   MOVE      W0140         V1CAAS
017000950411     C*
017100950411     C                   CLEAR                   VSCSOP
017200950411     C                   CLEAR                   VSCLNA
017300950411     C                   CLEAR                   DESLNA
017400950411     C                   CLEAR                   VSCSCD
017500950411     C                   CLEAR                   VSCSCA
017600950411     C                   CLEAR                   DESKSC
017700950411     C                   CLEAR                   VSCKSC
017800950411     C                   CLEAR                   DESPDR
017900950411     C                   CLEAR                   VSCPDR
018000950411     C                   CLEAR                   VSCFLS
018100950411     C                   CLEAR                   VSCNRT
018200950411     C                   CLEAR                   VSCFPP
018300970701     C                   CLEAR                   VSCBAM
018400990203     C                   CLEAR                   SAVKSC
018500990203     C                   CLEAR                   UNO
018600950411     C**
018700900509     C     FOR01         TAG
018800941227     C                   EXFMT     LS75D01
018900941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
019000941227     C                   SETOFF                                       902840
019100941227     C                   SETOFF                                       414243
019200941227     C                   SETOFF                                       444546
019300000613     C                   SETOFF                                       474864
019400020628     c                   setoff                                       51
019500941116     C** CMD3 - FINE LAVORO
019600900509     C   KC              GOTO      FINE
019700910205     C* CONTROLLI FORMATO
019800910205     C                   EXSR      CONTR
019900941228     C  NKF
020000941228     CANNKA
020100941228     COR 90              GOTO      FOR01
020200901214     C*
020300950116     C*
020400950116     C     VSCPDR        IFGT      *ZEROS
020500950116     C                   MOVEL     VSCPDR        VSCCPR
020600950116     C                   ENDIF
020700050329     c                   eval      vldvcli = pflg
020800910207     C* RISTAMPA BOLLE
020900900523     C   KA              DO
021000900523     C                   MOVEL     PARAM         KPJBU
021100941227     C                   MOVEL     'LS77'        KCOAZ
021200030616     C***                movel(p)  'FNLS77C'     Wpgm             10
021300030616     C                   movel(p)  'BCH10'       Wpgm             10
021400030616     C                   call      Wpgm
021500900523     C                   PARM                    KPJBA
021600900523     C                   END
021700900523     C*
021800910207     C* R I S T A M P A   S E G N A C O L L I
021900910207     C   KF              DO
022000910207     C                   MOVEL     PARAM         KPJBU
022100941227     C                   MOVEL     'LS76'        KCOAZ
022200030616     C***                movel(p)  'FNLS76C'     Wpgm             10
022300030616     C                   movel(p)  'BCH10'       Wpgm             10
022400030616     C                   call      Wpgm
022500910207     C                   PARM                    KPJBA
022600910207     C                   END
022700910207     C*
022800950411     C                   GOTO      INIZIO
022900000000     C     FINE          TAG
023000000000     C                   SETON                                        LR
023100941116     C*
023200910205     C* CONTROLLO FORMATO -------------------------------------------
023300910205     C     CONTR         BEGSR
023400910205     C                   SETOFF                                       90
023500050329
023600050329     c                   eval      *In49 = *Off
023700050329
023800941227     C                   MOVEL     *BLANKS       DESPDR
023900910208     C                   MOVEL     *BLANKS       DESLNA
024000950116     C                   CLEAR                   VSCCPR
024100941116     C* LE 3 SELEZIONI SONO IN ALTERNATIVA
024200950807     C     V1CLNP        IFNE      0
024300941116     C*
024400941116     C     V1CLNA        IFNE      0
024500941116     C     V1CLPD        ORNE      0
024600941116     C     V1CSED        ORNE      0
024700941116     C     V1CNSD        ORNE      0
024800941116     C     V1CSEA        ORNE      0
024900941116     C     V1CNSA        ORNE      0
025000941116     C                   MOVEL     MSG(1)        VIDMSG
025100941228     C                   SETON                                        409028
025200941116     C                   GOTO      ENDCTR
025300941116     C                   ENDIF
025400941116     C                   ENDIF
025500941116     C*
025600950807     C     V1CLNA        IFNE      0
025700941116     C*
025800941116     C     V1CLNP        IFNE      0
025900941116     C     V1CLPD        ORNE      0
026000941116     C     V1CSED        ORNE      0
026100941116     C     V1CNSD        ORNE      0
026200941116     C     V1CSEA        ORNE      0
026300941116     C     V1CNSA        ORNE      0
026400941116     C                   MOVEL     MSG(2)        VIDMSG
026500941228     C                   SETON                                        419028
026600941116     C                   GOTO      ENDCTR
026700941116     C                   ENDIF
026800941116     C                   ENDIF
026900941116     C*
027000950807     C     V1CLPD        IFNE      0
027100941116     C*
027200941116     C     V1CLNP        IFNE      0
027300941116     C     V1CLNA        ORNE      0
027400941116     C     V1CDSD        ORNE      0
027500941116     C     V1CDSA        ORNE      0
027600941116     C                   MOVEL     MSG(3)        VIDMSG
027700941116     C                   SETON                                        449028
027800941116     C                   GOTO      ENDCTR
027900941116     C                   ENDIF
028000941116     C                   ENDIF
028100910412     C*
028200941116     C* SE TUTTE E 3 VUOTE ERRORE
028300941116     C     V1CLPD        IFEQ      0
028400941116     C     V1CLNP        ANDEQ     0
028500941227     C     V1CLNA        ANDEQ     0
028600941116     C                   MOVEL     MSG(4)        VIDMSG
028700941116     C                   SETON                                        409028
028800941116     C                   GOTO      ENDCTR
028900941116     C                   ENDIF
029000941116     C*
029100941116     C* LINEA DI PARTENZA
029200950807     C     V1CLNP        IFNE      0
029300020701      * se p.o. di secondo livello
029400020701 b2  c                   if        (simtpp = '2' or simtpp = *blanks)
029500020701      * la lnp deve essere se stesso
029600020701 b3  c                   if        v1clnp <> simpou
029700020701     c                   eval      *in40 = *on
029800020701     c                   eval      *in28 = *on
029900020701     c                   eval      *in90 = *on
030000020701     c                   eval      vidmsg = msg(5)
030100020701     c                   goto      endctr
030200020701 e3  c                   endif
030300020701      * se p.o. di primo livello
030400020701 x2  c                   else
030500020701      * controllo che faccia parta dei p.o. del terminal di partenza
030600941116     C     V1CLNP        LOOKUP    L1                                     30
030700941116     C     *IN30         IFEQ      *OFF
030800941116     C                   MOVEL     MSG(5)        VIDMSG
030900941227     C                   SETON                                        409028
031000910207     C                   GOTO      ENDCTR
031100941116     C                   ENDIF
031200020701 b2  c                   endif
031300941116     C                   ENDIF
031400941116     C* LINEA DI ARRIVO
031500950807     C     V1CLNA        IFNE      0
031600950331     C     V1CLNA        ANDNE     999
031700941116     C     V1CLNA        CHAIN     AZORG                              30
031800941116     C     *IN30         IFEQ      *ON
031900941116     C     ORGFAG        OREQ      'V'
032000941116     C                   MOVEL     MSG(6)        VIDMSG
032100941227     C                   SETON                                        419028
032200941116     C                   GOTO      ENDCTR
032300941116     C                   ENDIF
032400941116     C                   ENDIF
032500000613     C* SI, NO, SOLO POSTE
032600000613     C     VSCSNP        IFEQ      ' '
032700000613     C                   MOVEL     MSG(16)       VIDMSG
032800000613     C                   SETON                                        649028
032900000613     C                   GOTO      ENDCTR
033000000613     C                   ENDIF
033100941116     C**
033200941116     C* DATA SPEDIZIONE : SE LNP O LNA DATA DAL OBBLIGATORIA
033300941116     C**
033400950807    1C     V1CLNP        IFNE      0
033500950807     C     V1CLNA        ORNE      0
033600941227     C                   Z-ADD     V1CDSD        G02DAT
033700941116     C                   MOVEL     *BLANK        G02ERR
033800941116     C                   CALL      'XSRDA8'
033900941116     C                   PARM                    WLBDAT
034000941116     C* ERRATA
034100941116    2C     G02ERR        IFEQ      '1'
034200941116     C                   SETON                                        429028
034300941116     C                   MOVEL     MSG(7)        VIDMSG
034400020628     c                   goto      endctr
034500941116    2C                   ENDIF
034600941116     C                   Z-ADD     G02INV        WDSD              8 0
034700941228     C                   Z-ADD     G02DAT        V1CDSD
034800941116     C* DATA AL SE C'E'
034900950807    2C     V1CDSA        IFNE      0
035000941227     C                   Z-ADD     V1CDSA        G02DAT
035100941116     C                   MOVEL     *BLANK        G02ERR
035200941116     C                   CALL      'XSRDA8'
035300941116     C                   PARM                    WLBDAT
035400941116     C* ERRATA
035500941116    3C     G02ERR        IFEQ      '1'
035600941116     C                   SETON                                        439028
035700941116     C                   MOVEL     MSG(7)        VIDMSG
035800020628     c                   goto      endctr
035900941116    3C                   ENDIF
036000941228     C                   Z-ADD     G02DAT        V1CDSA
036100941116     C                   Z-ADD     G02INV        WDSA              8 0
036200941116     C* DATA AL NON PUO' ESSERE INFERIORE A DATA AL
036300941116    3C     WDSA          IFLT      WDSD
036400941116     C                   SETON                                        439028
036500941116     C                   MOVEL     MSG(8)        VIDMSG
036600020628     c                   goto      endctr
036700941116    3C                   ENDIF
036800950418     C*
036900950418     C* PER EVITARE LUNGHE ELABORAZIONI, ACCETTARE LA DATA AL  SOLO SE
037000950418     C*  E' STATA EFFETTUATA ALMENO UNA PARZIALIZZAZIONE
037100950420    3C     V1CDSA        IFNE      V1CDSD
037200950420     C     VSCSOP        ANDEQ     ' '
037300950807     C     VSCLNA        ANDLE     0
037400950807     C     VSCSCD        ANDLE     0
037500950807     C     VSCFLS        ANDLE     0
037600950807     C     VSCKSC        ANDLE     0
037700950418     C     VSCFPP        ANDEQ     ' '
037800950418     C     VSCNRT        ANDEQ     ' '
037900950419    4C     VSCPDR        IFEQ      *BLANKS
038000950807     C     VSCPDR        ORLE      *ZEROS
038100950418     C                   SETON                                        439028
038200950418     C                   MOVEL     MSG(13)       VIDMSG
038300020628     c                   goto      endctr
038400950418    4C                   ENDIF
038500950418    3C                   ENDIF
038600950418     C*
038700941116    2C                   ENDIF
038800941116    1C                   ENDIF
038900941228     C**
039000941228     C* LINEA DI PARTENZA PER NUMERO SPEDIZIONE
039100941228     C**
039200950807     C     V1CLPD        IFNE      0
039300020701      * se p.o. di secondo livello
039400020701 b2  c                   if        (simtpp = '2' or simtpp = *blanks)
039500020701      * la lnp deve essere se stesso
039600020701 b3  c                   if        v1clpd <> simpou
039700020701     c                   eval      *in44 = *on
039800020701     c                   eval      *in28 = *on
039900020701     c                   eval      *in90 = *on
040000020701     c                   eval      vidmsg = msg(5)
040100020701     c                   goto      endctr
040200020701 e3  c                   endif
040300020701      * se p.o. di primo livello
040400020701 x2  c                   else
040500020701      * controllo che faccia parta dei p.o. del terminal di partenza
040600941228     C     V1CLPD        LOOKUP    L1                                     30
040700941228     C     *IN30         IFEQ      *OFF
040800941228     C                   MOVEL     MSG(5)        VIDMSG
040900941228     C                   SETON                                        449028
041000941228     C                   GOTO      ENDCTR
041100941228     C                   ENDIF
041200020701 b2  c                   endif
041300941228     C                   ENDIF
041400970102     C*
041500970102     C* IMPOSTO PER BENE L'ANNO
041600970102     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
041700970102     C     V1CAAS        IFLT      100
041800970102     C     V1CAAS        IFGT      60
041900970102     C                   ADD       1900          V1CAAS
042000970102     C                   ELSE
042100970102     C                   ADD       2000          V1CAAS
042200970102     C                   ENDIF
042300970102     C                   ENDIF
042400941116     C**
042500910903     C* NUMERO SPEDIZIONE AL > DAL
042600941116     C**
042700941116     C* SE IMMESSO SOLO IL NUMERO DAL CONTROLLO SE ESISTE
042800941227     C     V1CNSA        IFNE      0
042900941227     C                   MOVEL     V1CSEA        COMSPA            9 0
043000941227     C                   MOVE      V1CNSA        COMSPA
043100910903     C*
043200941227     C                   MOVEL     V1CSED        COMSPD            9 0
043300941227     C                   MOVE      V1CNSD        COMSPD
043400910903     C*
043500910903     C     COMSPA        IFLT      COMSPD
043600941227     C                   MOVEL     MSG(9)        VIDMSG
043700941227     C                   SETON                                        469028
043800910903     C                   GOTO      ENDCTR
043900910903     C                   END
044000910903     C* DAL > 0 SE IMMESSO AL
044100950807     C     V1CNSD        IFLE      *ZEROS
044200941227     C                   MOVEL     MSG(10)       VIDMSG
044300941227     C                   SETON                                        469028
044400910903     C                   GOTO      ENDCTR
044500910903     C                   END
044600950419     C*
044700950420     C* CONTROLLO CHE NON CI SIANO PIU' DI 6000 BOLLE DA STAMPARE
044800950419     C*   COMPRESE NEL RANGE DEL NUMERO SPEDIZIONE DAL/AL
044900950419     C                   CLEAR                   WNSDAL
045000950419     C                   CLEAR                   WNSAL
045100950419     C*
045200950419     C* REPERISCO IL PRIMO NUMERO SPEDIZIONE ESISTENTE NEL RANGE
045300950419     C                   Z-ADD     V1CSED        WSER
045400970102     C*
045500950419     C                   Z-ADD     V1CNSD        WNSP
045600950419     C     KBLP          SETLL     FNBLP01L
045700950419     C                   READ      FNBLP01L                               30
045800970102     C*
045900950419     C  N30              MOVEL     BLPNSP        WNSDAL
046000950419     C*
046100950419     C* REPERISCO L'ULTIMO NUMERO SPEDIZIONE ESISTENTE NEL RANGE
046200950419     C                   Z-ADD     V1CSEA        WSER
046300950419     C                   Z-ADD     V1CNSA        WNSP
046400950419     C     KBLP          SETGT     FNBLP01L
046500950419     C                   READP     FNBLP01L                               30
046600950419     C  N30              MOVEL     BLPNSP        WNSAL
046700950419     C*
046800950420     C* SE LA DIFFERENZA TROVATA E' MAGGIORE DI 6000 --> ERRORE
046900950419     C     WNSAL         SUB       WNSDAL        WCONT             7 0
047000950420     C     WCONT         IFGT      6000
047100950419     C                   MOVEL     MSG(14)       VIDMSG
047200950419     C                   SETON                                        469028
047300950419     C                   GOTO      ENDCTR
047400950419     C                   ENDIF
047500950419     C*
047600941227     C                   ELSE
047700990201     C* SE IMMESSA SERIE IMMETTERE ANCHE NUMERO
047800941227     C     V1CLPD        IFGT      0
047900950807     C     V1CNSD        ANDLE     0
048000941227     C                   MOVEL     MSG(12)       VIDMSG
048100941227     C                   SETON                                        469028
048200941227     C                   GOTO      ENDCTR
048300941227     C                   END
048400941227     C*
048500950807     C     V1CNSD        IFNE      0
048600950419     C                   Z-ADD     V1CSED        WSER
048700950419     C                   Z-ADD     V1CNSD        WNSP
048800990201     C     KBLP          CHAIN     FNBLP01L                           30
048900990201     C     *IN30         IFEQ      *ON
049000941227     C                   MOVEL     MSG(11)       VIDMSG
049100941227     C                   SETON                                        469028
049200941227     C                   GOTO      ENDCTR
049300910903     C                   END
049400990201     C**
049500990201     C* SE IL CTM NON PERMETTE LA RISTAMPA ERRORE
049600990201     c* solo se premuto F1 - ristampa bolle
049700990201    2C     *INKA         IFEQ      *ON
049800990201     C     BLPNRS        ANDGT     0
049900990201     C* BEDO SE BOLLA FRANCO O ASSEGNATO
050000990201     C                   MOVEL     '3A'          COD
050100990201     C                   MOVEL(P)  BLPCBO        KEY
050200990201     C     KTAB          CHAIN     TABEL                              30
050300990201    3C     *IN30         IFEQ      *OFF
050400990201     C                   MOVEL     TBLUNI        DS3A
050500990201     C                   ELSE
050600990201     C                   CLEAR                   DS3A
050700990201    3C                   ENDIF
050800990201     C                   MOVEL     §3ATB1        W001A             1
050900990201     C**
051000990201     C* SE BOLLA CON SERIE CERCO LA 3C DEL CLIENTE
051100990201     C                   MOVEL     '3C'          COD
051200990201     C* ASSEGNATO
051300990201    3C     W001A         IFEQ      'A'
051400990201     C                   MOVEL(P)  BLPCCM        KEY
051500990201     C                   ELSE
051600990201     C* FRANCO
051700990201     C                   MOVEL(P)  BLPKSC        KEY
051800990201    3C                   ENDIF
051900990201     C**
052000990201     C     KTAB          CHAIN     TABEL                              30
052100990201    3C     *IN30         IFEQ      *OFF
052200990201     C                   MOVEL     TBLUNI        DS3C
052300990201     C                   ELSE
052400990201     C                   CLEAR                   DS3C
052500990201    3C                   ENDIF
052600990201     C* NON RISTAMPABILE
052700990201    3C     §3CRSB        IFEQ      'N'
052800990201     C                   MOVEL     MSG(15)       VIDMSG
052900990201     C                   SETON                                        469028
053000990201     C                   GOTO      ENDCTR
053100990201    3C                   ENDIF
053200990201    2C                   ENDIF
053300990201     C**
053400941227     C                   END
053500941227     C                   END
053600910903     C*
053700910207     C* LINEA DI ARRIVO NON OBBLIGATORIA
053800941227     C     VSCLNA        IFNE      0
053900941227     C     VSCLNA        CHAIN     AZORG                              30
054000941227     C   30              SETON                                        5790
054100910207     C   30              GOTO      ENDCTR
054200910207     C                   MOVEL     ORGDES        DESLNA
054300910207     C                   END
054400941227     C*
054500941227     C* LINEA DI ARRIVO NON OBBLIGATORIA
054600941227     C     VSCFLS        IFNE      0
054700941227     C     VSCFLS        CHAIN     AZORG                              30
054800941227     C   30              SETON                                        5890
054900941227     C   30              GOTO      ENDCTR
055000941227     C                   END
055100910207     C* NUMERO SEGNACOLLO AL > DAL
055200941227     C     VSCSCA        IFNE      0
055300941227     C     VSCSCA        IFLT      VSCSCD
055400941227     C                   SETON                                        5990
055500910207     C                   GOTO      ENDCTR
055600910207     C                   END
055700910207     C* DAL > 0 SE IMMESSO AL
055800950807     C     VSCSCD        IFLE      *ZEROS
055900941227     C                   SETON                                        6090
056000910207     C                   GOTO      ENDCTR
056100910207     C                   END
056200910207     C                   END
056300910205     C*
056400910207     C* CODICE CLIENTE NON OBBLIGATORIO
056500050329
056600050329      * obbligatorio se stampa ldv x cliente
056700050329     c                   If        *In01 and vscksc = *Zeros
056800050329     c                   Eval      vidmsg = msg(19)
056900050329     c                   Eval      *In28 = *On
057000050329     c                   Eval      *In49 = *On
057100050329     c                   Eval      *In90 = *On
057200050329     c                   Goto      EndCtr
057300050329     c                   EndIf
057400050329
057500990203    1C     VSCKSC        IFNE      0
057600910205     C     KACO          CHAIN     CNACO000                           30
057700910207     C   30              SETON                                        5090
057800910205     C   30              GOTO      ENDCTR
057900910207     C                   MOVEL     ACORAG        DESKSC
058000990203     C**
058100990203     C** A CAMBIO CLIENTE RICONTROLLO
058200990203     C     VSCKSC        IFNE      SAVKSC
058300990203     C                   CLEAR                   UNO
058400990203     C                   MOVEL     VSCKSC        SAVKSC
058500990203     C                   ENDIF
058600990203     C**
058700990203     C* CONTROLLO SE PER IL CLIENTE SI POSSONO RISTAMPARE LE BOLLE
058800990203     C* ERRORE FORZABILE
058900990203    2C     UNO           IFEQ      ' '
059000990203     C                   MOVEL     '3C'          COD
059100990203     C                   MOVEL(P)  VSCKSC        KEY
059200990203     C     KTAB          CHAIN     TABEL                              30
059300990203    3C     *IN30         IFEQ      *OFF
059400990203     C                   MOVEL     TBLUNI        DS3C
059500990203     C                   ELSE
059600990203     C                   CLEAR                   DS3C
059700990203    3C                   ENDIF
059800990203     C* NON RISTAMPABILE
059900990203    3C     §3CRSB        IFEQ      'N'
060000990203     C                   SETON                                        5290
060100990203     C                   MOVEL     '1'           UNO               1
060200990203     C                   GOTO      ENDCTR
060300990203    3C                   ENDIF
060400990203    2C                   ENDIF
060500990203    1C                   END
060600050329
060700050329      * se stampa ldv x cliente il cliente deve essere in tabella CLI
060800050329     c                   If        *In01
060900050329     c                   Clear                   Tibs02ds
061000050329     c                   Eval      t02mod = 'C'
061100050329     c                   Eval      t02sif = knsif
061200050329     c                   Eval      t02cod = 'CLI'
061300050329     c                   Movel(p)  vscksc        T02ke1
061400050329     c                   Call      'TIBS02R'
061500050329     c                   Parm                    kpjba
061600050329     c                   Parm                    Tibs02ds
061700051018     c                   movel     t02uni        dcli
061800051018     c                   If        t02err <> *blanks or §clildv <> 'S'
061900050329     c                   Eval      vidmsg = msg(20)
062000050329     c                   Eval      *In28 = *On
062100050329     c                   Eval      *In49 = *On
062200050329     c                   Eval      *In90 = *On
062300050329     c                   Goto      EndCtr
062400050329     c                   EndIf
062500050329     c                   EndIf
062600990203     C**
062700941227     C* RICERCA ALFABETICA
062800941227     C     DESKSC        IFNE      *BLANKS
062900941227     C     VSCKSC        ANDEQ     0
063000941227     C* DESCRIZIONE AZIENDA
063100941227     C                   MOVEL     RSUT          PAXDUT
063200941227     C* PAXSTA=9 ESCLUDO ANNULLATI
063300941227     C                   Z-ADD     9             PAXSTA
063400941227     C* CAPOCONTO CLIENTI
063500941227     C                   Z-ADD     KCI           PAXCCC
063600941227     C                   CLEAR                   PAXDMT
063700941227     C                   MOVEL     DESKSC        PAXDMT
063800981109     C                   CLEAR                   PAXDIT
063900000907     C                   Z-ADD     1             PAXNUM
064000941227     C*
064100000907     C                   CALL      'XALFA3BR'
064200941227     C                   PARM                    PAXDUT           30
064300941227     C                   PARM                    CODUT
064400941227     C                   PARM                    PAXDMT           48
064500941227     C                   PARM                    PAXCCC            4 0
064600941227     C                   PARM                    PAXSTA            1 0
064700961016     C                   PARM                    PAXFLR           90
064800981109     C                   PARM                    PAXDIT            3
064900000907     C                   PARM                    PAXNUM            2 0
065000000907     C                   PARM                    PAXKCM           80
065100000907     C                   PARM                    PAXKSM          140
065200000907     C                   PARM                    PAXKDM           60
065300941227     C*
065400941227     C* NON TROVATO O PREMUTO CMD12
065500941227     C     PAXSTA        IFNE      -1
065600941227     C* CODICE SELEZIONATO: IMPOSTO A VIDEO
065700000907     C                   MOVEL     PAXKSM        VSCKSC
065800941227     C                   ENDIF
065900941227     C*
066000941227     C                   SETON                                        90
066100941227     C                   GOTO      ENDCTR
066200941227     C                   ENDIF
066300941227     C*
066400910207     C* CODICE PADRONCINO  NON OBBLIGATORIO
066500941227     C     '?'           SCAN      VSCPDR                                 90
066600941227     C* RICERCA
066700941227     C     *IN90         IFEQ      *ON
066800021203      *
066900021203      * imposto i campi della ds esterna fnlv24ds
067000021203     C                   clear                   fnlv24ds
067100021203     C                   eval      d24fil   = v1clnp
067200021203     C                   eval      d24tip   = 'A'
067300021203     C                   eval      d24flg   = 'R'
067400021203     C                   movel     fnlv24ds      kpjbu
067500941227     C                   CALL      'FNLV24R'
067600941227     C                   PARM                    KPJBA
067700021203      *
067800021203      * valorizzo i campi a video
067900021203     C                   movel     kpjbu         fnlv24ds
068000021203     C                   if        d24pdr   >  *zeros
068100021203     C                   move      d24pdr        VSCPDR
068200021205     C                   movel     d24rsc        DESPDR
068300021203     C                   endif
068400941227     C                   GOTO      ENDCTR
068500941227     C                   ENDIF
068600941227     C*
068700941227     C     VSCPDR        IFNE      *BLANKS
068800941227     C* ESEGUO TESTN
068900941227     C                   TESTN                   VSCPDR               30
069000941227     C                   MOVE      VSCPDR        W001A             1
069100941227     C     *IN30         IFEQ      *OFF
069200941227     C     W001A         ORLT      '0'
069300020628     c                   eval      vidmsg = msg(17)
069400020628     c                   eval      *in28 = *on
069500941227     C                   SETON                                        5190
069600941227     C                   GOTO      ENDCTR
069700941227     C                   ENDIF
069800941227     C*
069900941227     C                   MOVE      VSCPDR        W0070             7 0
070000021203     C*    W0070         CHAIN     FNAPD000                           30
070100021205     C                   move      w0070         kpdpdr
070200021203     C                   move      'A'           kpdtip
070300021203     C     kfapd         chain     fiapd01l                           30
070400941227     C     *IN30         IFEQ      *ON
070500020628     c                   eval      vidmsg = msg(17)
070600020628     c                   eval      *in28 = *on
070700941227     C                   SETON                                        5190
070800941227     C                   GOTO      ENDCTR
070900941227     C                   ENDIF
071000020628
071100020628      * deve essere gestito dal p.o.
071200020628     c                   movel     vscpdr        wpopdr
071300020701      * se p.o. di secondo livello
071400020701 b2  c                   if        simtpp = '2' or simtpp = *blanks
071500020701      * controllo che sia se stesso
071600020701 b3  c                   if        wpopdr <> simpou
071700020701     c                   eval      *in51 = *on
071800020701     c                   eval      *in28 = *on
071900020701     c                   eval      *in90 = *on
072000020701     c                   eval      vidmsg = msg(18)
072100020701     c                   goto      endctr
072200020701 e3  c                   endif
072300020701      * se p.o. di primo livello
072400020701 x2  c                   else
072500020701      * controllo che faccia parta dei p.o. del terminal di partenza
072600020628     c     wpopdr        lookup    l1                                     30
072700020701 b3  c                   if        not *in30
072800020628     c                   eval      *in51 = *on
072900020628     c                   eval      *in28 = *on
073000020628     c                   eval      *in90 = *on
073100020628     c                   eval      vidmsg = msg(18)
073200020628     c                   goto      endctr
073300020701 e3  c                   endif
073400020701 e2  c                   endif
073500020628      * se inserita linea di partenza
073600020701 b2  c                   if        v1clnp <> *zeros and
073700020628      * il p.o. dell'autotrasportatore deve essere uguale alla lnp
073800020701     c                             wpopdr <> v1clnp
073900020628     c                   eval      *in51 = *on
074000020628     c                   eval      *in28 = *on
074100020628     c                   eval      *in90 = *on
074200020628     c                   eval      vidmsg = msg(18)
074300020628     c                   goto      endctr
074400020701 e2  c                   endif
074500941227     C*
074600941227     C                   MOVEL     APDRSC        DESPDR
074700910207     C                   END
074800910207     C*
074900910205     C     ENDCTR        ENDSR
075000910207     C*
075100941116     C* OPERAZIONI INIZIALI-------------------------------------------*
075200941116     C     *INZSR        BEGSR
075300941116     C*
075400941116     C     *ENTRY        PLIST
075500941116     C                   PARM                    KPJBA
075600050329
075700050329     c                   Eval      pflg = %subst(kpjbu:1:1)
075800050329     c                   Eval      *In01 = (pflg <> *Blanks)
075900050329
076000941116     C                   Z-ADD     1             CODUT
076100941227     C                   CALL      'X§PARUT'
076200941227     C                   PARM                    UT§DSE
076300941116     C                   MOVEL     RAGUT         RSUT             20
076400941227     C                   MOVEL     REC80         CNCR80
076500000613      * IMPOSTO IL CODICE P.O. UTENTE
076600020430     C                   Z-ADD     SIMPOU        POUTEN            3 0
076700941116     C* RICERCA CAPOCONTI
076800941116     C                   DO        50            X                 2 0
076900941116     C                   MOVE      TCU(X)        TCUDS
077000941116     C     F56           CABNE     'CG'          END1
077100941116     C     F4            COMP      '1'                                    21
077200941116     C     F4            COMP      '2'                                    22
077300941116     C     F4            COMP      '3'                                    23
077400941116     C     F4            COMP      '6'                                    27
077500941116     C** 1 CLIENTI   21
077600941116     C** 2 FORNITORI 22
077700941116     C** 3 AGENTI    23
077800941116     C     F3            COMP      '0'                                242425
077900941116     C     F3            COMP      'I'                                    26
078000941116     C** 0 ITALIA   25
078100941116     C** 1 ESTERO   24
078200941116     ** I CAPO CONTO IVA
078300941116     C   21
078400941116     CAN 24              Z-ADD     KCU(X)        KCE               4 0
078500941116     C   21
078600941116     CAN 25              Z-ADD     KCU(X)        KCI               4 0
078700941116     C   22
078800941116     CAN 24              Z-ADD     KCU(X)        KFE               4 0
078900941116     C   22
079000941116     CAN 25              Z-ADD     KCU(X)        KFI               4 0
079100941116     C   23
079200941116     CAN 24              Z-ADD     KCU(X)        KAE               4 0
079300941116     C   23
079400941116     CAN 25              Z-ADD     KCU(X)        KAI               4 0
079500941116     C   26              Z-ADD     KCU(X)        KIVA              4 0
079600941116     C   27              Z-ADD     KCU(X)        KBNA              4 0
079700941116     C     END1          TAG
079800941116     C                   END
079900941116     C**
080000941116     C* KLIST
080100941116     C**
080200941116     C* ACCESSO   CNACO
080300941116     C     KACO          KLIST
080400941116     C                   KFLD                    CODUT
080500941116     C                   KFLD                    CCC               4 0
080600941227     C                   KFLD                    VSCKSC
080700950419     C* ACCESSO   FNBLP01L
080800950419     C     KBLP          KLIST
080900941227     C                   KFLD                    V1CAAS
081000941227     C                   KFLD                    V1CLPD
081100950419     C                   KFLD                    WSER
081200950419     C                   KFLD                    WNSP
081300990201     C     KTAB          KLIST
081400990201     C                   KFLD                    CODUT
081500990201     C                   KFLD                    COD
081600990201     C                   KFLD                    KEY
081700941116     C*
081800021203     C     Kfapd         klist
081900021203     C                   kfld                    kpdtip
082000021203     C                   kfld                    kpdpdr
082100021203      *
082200941116     C                   Z-ADD     KCI           CCC
082300950419     C     *LIKE         DEFINE    V1CNSD        WNSP
082400950419     C     *LIKE         DEFINE    V1CSED        WSER
082500950419     C     *LIKE         DEFINE    BLPNSP        WNSDAL
082600950419     C     *LIKE         DEFINE    BLPNSP        WNSAL
082700990201     C     *LIKE         DEFINE    TBLCOD        COD
082800990201     C     *LIKE         DEFINE    TBLKEY        KEY
082900990203     C     *LIKE         DEFINE    VSCKSC        SAVKSC
083000941116     C*
083100941227     C***
083200941227     C* CARICO TABELLA FILIALI GESTITE £1
083300941227     C***
083400941227     C                   CLEAR                   DSUL06
083500941227     C                   MOVE      '£1'          D06COD
083600941227     C                   MOVEL     'L'           D06TLA
083700941227     C                   MOVEL     SIMFEL        D06KEY
083800941227     C                   MOVEL     DSUL06        KPJBU
083900941227     C*
084000941227     C                   CALL      'TRUL06R'
084100941227     C                   PARM                    KPJBA
084200941227     C                   MOVEL     KPJBU         DSUL06
084300941227     C                   MOVEA     LIN           L1
084400000613      * VERIFICO SE VI E' ALMENO UN P.O. POSTE IN L1
084500000613     C                   SETOFF                                       72
084600000613     C                   Z-ADD     1             C$$               3 0
084700000613     C     L1(C$$)       DOWGT     0
084800000613     C     L1(C$$)       CHAIN     AZORG01L                           30
084900000613     C*
085000000613     C     *IN30         IFEQ      *OFF
085100000613     C                   MOVEL     ORGDE3        OG143
085200020628     c                   if        §ogntw = 'PPT'
085300000613     C                   Z-ADD     30            C$$
085400000613     C                   SETON                                        72
085500000613     C                   ELSE
085600000613     C                   ADD       1             C$$
085700000613     C                   ENDIF
085800000613     C                   ELSE
085900000613     C                   ADD       1             C$$
086000000613     C                   ENDIF
086100000613      *
086200000613     C                   ENDDO
086300000613     C                   ENDSR
086400000613**
086500941116Se selezione per Linea PARTENZA non immettere linea di arrivo e num.spedizione01
086600941116Se selezione per Linea ARRIVO non immettere linea di partenza e num.spedizione02
086700941116Se selezione per NUMERO SPEDIZIONE non immettere la linea di partenza e arrivo03
086800941116Selezionare per Linea di PARTENZA, linea di ARRIVO o NUMERO SPEDIZIONE
086900941116Linea di partenza errata
087000941116Linea di arrivo errata
087100941116Data spedizione errata
087200941116Data AL inferiore a data DAL
087300941227Numero spedizione DAL inferiore a numero spedizione AL
087400990201Numero spedizione DAL obbligatorio se immesso l'AL                            10
087500941227Numero spedizione inesistente
087600941227Immettere almeno numero spedizione DAL
087700990201Se immessa Data AL occorre inserire almeno una parzializzazione               13
087800990201Impossibile effettuare la Ristampa: richiesti più di 6000 numeri spedizione !!14
087900990203Impossibile ristampare bolla per questo cliente                               15
088000000613Sono validi solo i valori S,N,P
088100020628Codice Autotrasportatore inesistente                                          17
088200020628Codice Autotrasportatore in gestione ad altro p.o.                            18
088300050329Immettere il codice cliente                                                   19
088400050329Cliente non abilitato alla stampa delle bolle                                 20
