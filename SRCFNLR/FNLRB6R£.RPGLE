000100070302     H DECEDIT('0,') DATEDIT(*ymd.)
000200950113     H*---------------------------------------------------------------*
000300950113     H*      AFFIDABILITA' SPEDIZIONI - RICHIESTA PARAMETRI           *
000400930610     H*---------------------------------------------------------------*
000500930610     H* INDICATORI USATI                                              *
000600930610     H*---------------------------------------------------------------*
000700930611     H* 01    - ON-FILIALE DI II LIVELLO
000800940331     H* 02    - SELEZIONATE BOLLE IN PARTENZA
000900940331     H* 03    - SELEZIONATE BOLLE IN ARRIVO
001000940331     H* 04    - L6 CONTIENE SOLTANTO UN ELEMENTO
001100980209     H* 05    - £1 CONTIENE ALMENO 1 LINEA ESTERA E SELEZIONATE BOLLE IN ARRIVO
001200121220     H* 06    - richeiste differenze di peso/volume
001300121220     H* 07    - proteggo campo scelta tipo differenzain seconda videata
001400930611     H* 21/27 - RICERCA CAPOCONTO
001500050622     H* 28    - campo messaggi
001600050622     H* 30    - LETTURA TABELLE
001700930611     H* 31    - RICERCA FILIALE IN TABELLA L1 E L6
001800961021     H* 32/33 - DI COMODO
001900950301     H* 40/70 - ERRORE
002000950301     H* 71    - RICHIESTA L'ELABORAZIONE CON L'AFFIDABILITA'
002100020607     H* 72/81 - ERRORE
002200930611     H* 90    - GENERICO DI ERRORE
002300930610     F*---------------------------------------------------------------*
002400930610     F* DATA BASE                                                     *
002500930610     F*---------------------------------------------------------------*
002600930610     FCNACO00F  IF   E           K DISK
002700930611     FTABEL00F  IF   E           K DISK
002800121220     Ftntbe01l  IF   E           K DISK
002900940331     FAZORG01L  IF   E           K DISK
003000021203     ffiapd01l  if   e           k disk
003100950113     FFNLRB6D   CF   E             WORKSTN
003200930610     D*---------------------------------------------------------------*
003300930610     D* SCHIERE                                                       *
003400930610     D*---------------------------------------------------------------*
003500930610     D* FILIALI IN GESTIONE
003600930610     D*------------------
003700930610     D L1              S              3  0 DIM(30)
003800930610     D L6              S              3  0 DIM(30)
003900961021     D*------------------
004000961021     D* MESSAGGI ERRORE
004100961021     D*------------------
004200130115     D MSG             S             78    DIM(13) CTDATA PERRCD(1)             MESSAGGI ERRORE
004300930610     D*---------------------------------------------------------------*
004400930610     D* DS                                                            *
004500930610     D*---------------------------------------------------------------*
004600961021     D                 DS
004700961021     D  VIDKLP                 1      3  0
004800961021     D  VIDKSC                 4      7  0
004900961021     D  VIDCLI                 1      7  0
005000930610     D*------------------
005100930611     D* DS RICERCA FILIALE IN GESTIONE E CAPOCONTO
005200930610     D*------------------
005300930611     D CNCR80        E DS
005400930610     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005500930611     D  TCU                  398    697
005600930611     D                                     DIM(50)
005700930611     D  KCU                  698    847P 0
005800930611     D                                     DIM(50)
005900930611     D                                     PACKEVEN
006000930611     D*------------------
006100930611     D* DS RICERCA CAPOCONTO
006200930611     D*------------------
006300930611     D TCUDS           DS
006400930611     D  F1                     1      1
006500930611     D  F3                     3      3
006600930611     D  F2                     2      2
006700930611     D  F4                     4      4
006800930611     D  F56                    5      6
006900930610     D*------------------
007000950113     D* INVERSIONE DATA (8)
007100930610     D*------------------
007200930610     D WLBDAT          DS
007300950113     D  G02DAT                 1      8  0
007400950113     D  G02INV                 9     16  0
007500950113     D  G02ERR                17     17
007600950113     D  G02TGI                18     22  0
007700020212
007800020212      *------------------
007900020212      * Ds AZORG
008000020212      *------------------
008100020212     d og143         e ds
008200020419      *------------------
008300020419      * ds per trul06r
008400020419      *------------------
008500020419     d trul06ds      e ds
008600020419     d  lin                    1     90  0 dim(30)
008700070302     d
008800070302     D* DS PER FNLV13R - CONTROLLO DI UN CAP
008900070302     D DSLV13        E DS                  EXTNAME(FNLV13DS)
009000070302     D* DS PER TISI95R - CONTROLLO DI CAP
009100070302     D DSSI95        E DS                  EXTNAME(TISI95DS)
009200070302     d
009300121220     D tntb96ds      E DS
009400020212
009500930610     D*------------------
009600950113     D* PARAMETRI LANCIO
009700930610     D*------------------
009800950301     D PARAM           DS
009900950301     D* BOLLE PARTENZA O ARRIVO (P/A)
010000950301     D  VIDBPA                 1      1
010100950301     D* BOLLE CONSEGNATE O NON  (S/N)
010200950301     D  VIDSPC                 2      2
010300950301     D* DATA 'DAL' (GG/MM/AAAA)
010400950301     D  VIDDAL                 3      7P 0
010500950301     D* DATA 'AL'  (GG/MM/AAAA)
010600950301     D  VIDAL                  8     12P 0
010700950301     D* DATA 'DAL' (AAAA/MM/GG)
010800950301     D  ROVDAL                13     17P 0
010900950301     D* DATA 'AL'  (AAAA/MM/GG)
011000950301     D  ROVAL                 18     22P 0
011100950301     D* FILIALE DI PARTENZA
011200950301     D  VIDLNP                23     24P 0
011300950301     D* FILIALE DI ARRIVO
011400950301     D  VIDLNA                25     26P 0
011500950301     D* CODICE SOTTOCONTO
011600950301     D  PARKCI                27     29P 0
011700950301     D* BOLLE DEL CLIENTE
011800961021     D  PARCLI                30     33P 0
011900950301     D* GIORNI DI RITARDO
012000950301     D  VIDGIO                34     35P 0
012100950301     D* FILIALE ELABORATORE (SIMFEL O REMFIL)
012200950301     D  PARFEL                36     37P 0
012300050623     D* stampadifferenze peso MAggiori MInori EE-entrambe
012400050623     D  vidmag                38     39
012500121220     d*  indico se devo leggere il codice cliente o il raggruppaoemnto
012600121220     d  parragr               40     40
012700121221     d* richiesta file excel  --> l'indirizzo e-mail lo metto in un file di appoggio
012800121221     d  videxcel              41     41
012900130115     d  parprg_kpjbu          42     45P 0
013000121220     d
013100950301     D* PASSO LINEE DI PARTENZA DA ELABORARE
013200950301     D  LNP                   58    117P 0
013300940331     D                                     DIM(30)
013400950301     D* PASSO LINEE DI ARRIVO DA ELABORARE
013500950301     D  LNA                  118    177P 0
013600940331     D                                     DIM(30)
013700950301     D* TIPO SERVIZIO
013800950301     D  VIDTIP               178    178
013900950301     D* S=AFFIDABILITA' CLIENTI
014000950301     D  VIDACL               179    179
014100950301     D* S=AFFIDABILITA' CONSEGNE
014200950301     D  VIDACO               180    180
014300050623     D* STAMPA ORE RITARDO   (S/N)  O DIFFERENZE DI PESO/VOLUMI(D/P/V)
014400050621     D  PARVRA               181    181
014500950113     D* SPEDIZIONI IN RITARDO/ANTICIPO/TUTTE
014600950301     D  VIDSRA               182    182
014700950301     D* DATA SPEDIZIONE DAL (GG/MM/AAAA)
014800950301     D  VIDDSD               183    187P 0
014900950301     D* DATA SPEDIZIONE AL (GG/MM/AAAA)
015000950301     D  VIDDSA               188    192P 0
015100950301     D* DATA SPEDIZIONE DAL (AAAA/MM/GG)
015200950301     D  ROVDSD               193    197P 0
015300950301     D* DATA SPEDIZIONE AL (AAAA/MM/GG)
015400950301     D  ROVDSA               198    202P 0
015500940527     D* BOLLE IN DISTINTA O NON  (S/N)
015600950301     D  VIDBDS               203    203
015700950301     D* ELABORAZIONE: D=DATA CONS/SPED O P=PADRONCINO
015800950302     D  VIDELA               204    204
015900950301     D* STAMPA A T=TOTALE O IN D=DETTAGLIO
016000950302     D  VIDTOT               205    205
016100950220     D* COD.PADRONCINO DAL/AL
016200950302     D  VIDPD1               206    209P 0
016300950302     D  VIDPD2               210    213P 0
016400950301     D* M=MATTINA / P=POMERIGGIO (PER PADRONCINO)
016500950302     D  VIDFPP               214    214
016600950301     D* NUMERO RITIRO (PER PADRONCINO)
016700950302     D  VIDNRT               215    215
016800950301     D* DATA RITIRO (PER PADRONCINO) GG/MM/AAAA DAL
016900950302     D  VIDDR1               216    220P 0
017000950301     D* DATA RITIRO (PER PADRONCINO) GG/MM/AAAA AL
017100950302     D  VIDDR2               221    225P 0
017200070302     D* nazione detinatario
017300070302     D  VIDnzd               226    228
017400070515     D* S=AFFIDABILITA' partenza
017500070515     D  VIDACp               229    229
017600960614     D* STAMPA DEI SOLI RITARDI ("S"=SI/" "=NO)
017700960614     D  VIDRIT               236    236
017800980209     D* DATA TRASMISSIONE DAL (GG/MM/AAAA)
017900980209     D  VIDDTD               237    241P 0
018000980209     D* DATA TRASMISSIONE AL (GG/MM/AAAA)
018100980209     D  VIDDTA               242    246P 0
018200070302     D* cap destinatario
018300070302     D  VIDCAD               247    255
018400930610     D*------------------
018500950113     D* ARCHITETTURA
018600930610     D*------------------
018700930610     D KPJBA         E DS
018800121221     d
018900121221     D trul01ds      E DS
019000130115     d
019100130115     D dsemail       E DS
019200020419
019300020419     d wa              s              2  0
019400020607     d wpdc            s              3  0
019500020607     d wpopdc          s              3
019600121220     d Indx            s              3  0
019700020419
019800021203     D kpdpdr          S                   LIKE(APDpdr)
019900021203     D kpdtip          S                   LIKE(APDtip)
020000070302     D varcap          S                   LIKE(vidcad)
020100070302     D rovdtd          S              8  0
020200070302     D rovdta          S              8  0
020300070302     D rovdr1          S              8  0
020400070302     D rovdr2          S              8  0
020500070302     D AAA             S              1
020600070302     D AAB             S              1
020700070302     D AAD             S              1
020800070302     D wfor1           S              1
020900121220     D kcod            S                   LIKE(tbecod) inz('KPV')
021000121220     D kke1            S                   LIKE(tbeke1)
021100121220     D kke2            S                   LIKE(tbeke2)
021200121220     D savragr         S                   LIKE(v5cragr)
021300130115     d*---------------------------------------------------------------*
021400130115      /copy gaitrasrc/srcprotopr,xemail
021500930610     C*---------------------------------------------------------------*
021600950113     C* MAIN LINE                                                     *
021700930610     C*---------------------------------------------------------------*
021800930610     C     FMTD1         TAG
021900940331     C*
022000940331     C* EMISSIONE FORMATO VIDEO
022100950301     C                   CLEAR                   PARAM
022200951018     C                   CLEAR                   COMCLI
022300950301     C                   MOVEL     'D'           VIDELA                         'D'=PER DATA
022400950302     C                   MOVEL     'D'           VIDTOT                         'D'=IN DETTAGLIO
022500940331     C                   MOVEL     'S'           VIDACL
022600940331     C                   MOVEL     'P'           VIDBPA
022700940331     C                   MOVEL     'S'           VIDSPC
022800940527     C                   MOVEL     ' '           VIDBDS
022900940331     C                   Z-ADD     12            VIDGIO
023000960614     C                   MOVEL     'S'           VIDRIT
023100940331     C                   MOVEL     'S'           VIDVRA
023200050621     C                   MOVEL     'N'           VIDdpv
023300050622     C                   MOVEL     'N'           VIDdvv
023400050622     C                   MOVEL     'MA'          VIDMAG
023500940331     C                   MOVEL     'R'           VIDSRA
023600950301     C                   MOVEL     *BLANKS       DECTIP
023700121220     c                   clear                   v5cragr
023800121220     c                   clear                   v5ddesc1
023900121220     c                   clear                   v5ddesc2
024000950220     C                   SETOFF                                       0203
024100940331     C*
024200050621     c     fmtd11        tag
024300950220     C                   EXFMT     LRB6D01
024400930610     C*
024500950113     C* F03 - FINE LAVORO
024600930610     C   KC              GOTO      FINE
024700940331     C*
024800050621     c* Se immeddo SI a entrambe le richieste  errore
024900050622     c                   if        (viddpv='S' and vidvra='S')  or
025000050622     c                             (viddvv='S' and vidvra='S')
025100050621     c                   seton                                        40
025200050621     c                   goto      fmtd11
025300050621     c                   endif
025400050622     c*
025500050713     c                   setoff                                       0607
025600121220     c                   setoff                                       1415
025700050622     c                   if        viddpv='S' or viddvv='S'
025800050622     c                   seton                                        06
025900050713     c* se richeiste entrambe le differenze, impostabile solo NR
026000050713     c                   if        viddpv='S' and viddvv='S'
026100050713     c                   eval      vidmag='NR'
026200050713     c                   seton                                        07
026300050622     c                   endif
026400050713     c                   endif
026500121220     c
026600121220     c* se richiesti i pesi pemetto l'uso del raggruppamento codici
026700121220     c                   if        viddpv='S' and viddvv='N' and vidvra='N'
026800121220     c                   seton                                        14
026900121220     c                   endif
027000050622     c
027100940331     C* CARICAMENTO TABELLA L1 E L6  A SECONDA DEL FLAG RICHIESTO
027200940331     C                   EXSR      CARTAB
027300950301     C*
027400950301     C* SE LASCIATO VUOTO IL CAMPO, LO REIMPOSTO A 'N'
027500950302     C     VIDVRA        IFEQ      *BLANKS
027600950302     C                   MOVEL     'N'           VIDVRA
027700950301     C                   ENDIF
027800950302     C     VIDVRA        COMP      'S'                                    71     ON=CON AFFIDABILITA
027900950220     C*
028000950220     C* PER LE PARTENZE DEVO RIEMETTERE LA VIDEATA PER ALTRA DOMANDA
028100950220     C   02              EXFMT     LRB6D01
028200950220     C* F12 - RITORNO
028300950220     C   02
028400950220     CAN KL              GOTO      FMTD1
028500950303     C*
028600950303     C* SE RICHIESTO PER PADRONCINO, PROPONGO SIA CONS. CHE NON CONS.
028700950303     C     VIDELA        IFEQ      'P'
028800950303     C                   MOVEL     *BLANKS       VIDSPC
028900950303     C                   ENDIF
029000940331     C*
029100950220     C* IL SECONDO FORMATO VARIA SE RICHIESTO ARRIVI O PARTENZE
029200961022     C                   CLEAR                   VIDCLI
029300950328     C                   CLEAR                   VIDDCM
029400940331     C     FMTD2         TAG
029500950220     C   03              EXFMT     LRB6D02
029600950220     C   02VIDELA        IFEQ      'P'                                          'P'=PADRONCINO
029700950220     C                   EXFMT     LRB6D03                                      PER DATA
029800950220     C                   ELSE
029900950220     C                   EXFMT     LRB6D04                                      PER PADRONCINO
030000950220     C                   ENDIF
030100940331     C*
030200961021     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
030300961021     C                   CLEAR                   VIDMSG
030400961021     C                   SETOFF                                       285253
030500070515     C                   SETOFF                                       3982
030600020606     c                   eval      *in80 = *off
030700020607     c                   eval      *in81 = *off
030800961021     C*
030900950113     C* F03 - FINE LAVORO
031000940331     C   KC              GOTO      FINE
031100950113     C* F12 - RITORNO
031200940331     C   KL              GOTO      FMTD1
031300121220
031400121220     C* F09 - Richiesta raggruppamento codici
031500121220IF  1C     *INKI         IFEQ      *ON
031600121220     c                   exsr      wind05
031700121220     c                   goto      FMTD2
031800121220     c                   endif
031900930610     C*
032000930610     C* CONTROLLO DATI FORMATO VIDEO
032100930610     C                   EXSR      CONTR1
032200930610     C*
032300930610     C* ERRORI: RIEMETTO LA VIDEATA
032400940331     C   90              GOTO      FMTD2
032500930610     C*
032600950113     C* F06 - ELABORAZIONE
032700950113IF  1C     *INKF         IFEQ      *ON
032800950301     C  N01              MOVEL     SIMFEL        PARFEL
032900020513     C   01              MOVEL     simpou        PARFEL
033000950301     C                   Z-ADD     KCI           PARKCI                         SOTTOCONTO
033100940331     C*
033200940331     C* LINEE DI ARRIVO PER SIMFEL
033300940331     C   03
033400940331     CANN01VIDLNA        IFEQ      0
033500940331     C                   MOVEA     L1            LNA
033600931013     C                   ELSE
033700940331     C                   Z-ADD     VIDLNA        LNA(1)
033800931013     C                   ENDIF
033900940331     C* LINEE DI ARRIVO PER 2 LIVELLO
034000940331     C   03
034100940331     CAN 01VIDLNA        IFEQ      0
034200940331     C                   MOVEA     L6            LNA
034300940331     C                   ELSE
034400940331     C                   Z-ADD     VIDLNA        LNA(1)
034500940331     C                   ENDIF
034600940331     C*
034700940331     C* LINEE DI PARTENZA  PER SIMFEL
034800940331     C   02
034900940331     CANN01VIDLNP        IFEQ      0
035000940331     C                   MOVEA     L1            LNP
035100940331     C                   ELSE
035200940331     C                   Z-ADD     VIDLNP        LNP(1)
035300940331     C                   ENDIF
035400940331     C*
035500940331     C* LINEE DI PARTENZA  PER 2 LIVELLO
035600940331     C   02
035700940331     CAN 01              Z-ADD     VIDLNP        LNP(1)
035800930611     C*
035900961021     C                   MOVEL     VIDCLI        PARCLI
036000121220     c* e presenete raggruppamento lo indicao nell'apposito campo
036100121220     c                   if        v5cragr >*zeros
036200121220     c                   movel     v5cragr       PARCLI
036300121220     C                   EVAL      PARRAGR='R'
036400121220     C                   ENDIF
036500121220     C
036600050621     C* DIFFERENZE PESI VOLUMI
036700050623     c                   select
036800050623     C                   when      VIDDPV='S' and viddvv='S'
036900050621     C                   EVAL      PARVRA='D'
037000050623     C                   when      VIDDPV='S'
037100050623     C                   EVAL      PARVRA='P'
037200050623     C                   when      VIDDvV='S'
037300050623     C                   EVAL      PARVRA='V'
037400050623     C                   other
037500050621     C* ORE RITARDO S/N
037600050621     C                   EVAL      PARVRA=VIDVRA
037700050623     C                   ENDsl
037800050621     C
037900950113     C                   MOVEL     'LRB7'        KCOAZ
038000001219     C                   MOVEL     '3'           KRITB
038100020607     C                   CALL      'BCH09'
038200020607     C                   PARM                    KPJBA
038300130111     c
038400130111     C     KRITB         IFEQ      '2'
038500130111     C                   GOTO      FMTD2
038600130111     C                   ENDIF
038700130111     c
038800121221     c* Imposto l'indirizzo e-mail nel file di appoggio
038900130111     c                   clear                   parprg_kpjbu
039000130111     c
039100130111    1c                   if        videmail<>*blanks
039200121221     c                   clear                   trul01ds
039300130111     c                   eval      UL01OPZ='1'
039400130111     c                   eval      UL01azn = KCOAZ
039500130111     c                   movel(p)  videmail      UL01prm
039600130111     c                   eval      UL01err = *off
039700130111     c                   call      'TRUL01R'
039800130111     c                   parm                    TRUL01ds
039900130111if  2c                   if        UL01err = *on
040000130111     c                   seton                                        28  90
040100130111     C                   MOVEL     MSG(10)       VIDMSG
040200130111     C                   GOTO      FMTD2
040300130111e   2c                   endif
040400130111     c
040500130111     c                   z-add     UL01prg       parprg_kpjbu
040600130111    1c                   endif
040700130115     c
040800130115     C                   MOVEL     PARAM         KPJBU
040900001219      ***
041000001219      ***
041100130118     C                   CALL      'BCH10'
041200130118     c***                CALL      'FNLRB7R'
041300001218     C                   PARM                    KPJBA
041400930611E   1C                   ENDIF
041500930610     C*
041600930610     C* ENTER - RIMANE NELLA VIDEATA
041700930625IF  1C     *INKC         IFEQ      *OFF
041800950113     C     *INKF         ANDEQ     *OFF
041900940331     C                   GOTO      FMTD2
042000930610E   1C                   ENDIF
042100930610     C*
042200930610     C     FINE          TAG
042300930610     C                   SETON                                        LR
042400930610     C*---------------------------------------------------------------*
042500950113     C* CONTR1 - CONTROLLO DATI FORMATO VIDEO                         *
042600930610     C*---------------------------------------------------------------*
042700930610     C     CONTR1        BEGSR
042800930611     C*
042900930611     C                   SETOFF                                       9031
043000950301     C                   Z-ADD     *ZEROS        ROVDAL
043100950301     C                   Z-ADD     *ZEROS        ROVAL
043200020606     c                   clear                   rovdsd
043300020606     c                   clear                   rovdsa
043400020606     c                   clear                   rovdtd
043500020606     c                   clear                   rovdta
043600961021     C*
043700961022     C* LINEA PARTENZA CODICE CLIENTE
043800961021     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
043900961021    2C     VIDKLP        IFGT      0
044000961022     C     VIDKLP        CHAIN     AZORG01L                           33
044100961022    3C     *IN33         IFEQ      *ON
044200961022     C                   SETON                                        285290
044300961021     C                   MOVEL     MSG(1)        VIDMSG
044400961022     C                   GOTO      ENDCT1
044500961021    3C                   ENDIF
044600961021     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
044700961021    3C     VIDKSC        IFEQ      0
044800961022     C     VIDDCM        ANDEQ     *BLANKS
044900121220     c     v5cragr       andle     *zeros
045000961022     C                   SETON                                        285390
045100961022     C                   MOVEL     MSG(4)        VIDMSG
045200961022     C                   GOTO      ENDCT1
045300961021    3C                   ENDIF
045400961021     C*
045500961021   X2C                   ELSE
045600961021    3C     VIDKSC        IFGT      0
045700961022     C                   SETON                                        285290
045800961021     C                   MOVEL     MSG(1)        VIDMSG
045900961022     C                   GOTO      ENDCT1
046000961021    3C                   ENDIF
046100961021    2C                   ENDIF
046200121220     c
046300121220     c* Se immesso raggruppaoemnto non immettere codice e viceversa
046400121220     c                   if        vidksc>0  and v5cragr>*zeros
046500121220     C                   SETON                                        285390
046600121220     C                   MOVEL     MSG(3)        VIDMSG
046700121220     C                   GOTO      ENDCT1
046800121220    3C                   ENDIF
046900130115     c* se richiesto il file excel, immettere o codice cliente e raggruppamento
047000130114     c                   if        videxcel='S' and vidksc<=0 and
047100130114     c                             (v5cragr=*zeros  or v5cragr=*blanks)
047200130114     C                   SETON                                        285390
047300130114     C                   MOVEL     MSG(11)       VIDMSG
047400130114     C                   GOTO      ENDCT1
047500130114    3C                   ENDIF
047600961021     C*
047700950328     C* RICERCA ALFABETICA
047800961021     C     VIDKSC        IFEQ      *ZEROS
047900950328     C     VIDDCM        ANDNE     *BLANKS
048000950328     C                   EXSR      RICALF
048100950328     C                   GOTO      ENDCT1
048200950328     C                   END
048300940331     C*
048400070515     C* RICHIEDERE STAMPA PER AFFIDABILITA' CLIENTI O CONSEGNE o partenza
048500940331IF  1C     VIDACL        IFEQ      *BLANKS
048600940331     C     VIDACO        ANDEQ     *BLANKS
048700070515     C     VIDACP        ANDEQ     *BLANKS
048800940331     C                   SETON                                        5590
048900940331     C                   GOTO      ENDCT1
049000940331E   1C                   ENDIF
049100940331     C*
049200940331     C* IMPOSSIBILE SELEZIONARE ENTRAMBE LE STAMPE
049300940331IF  1C     VIDACL        IFEQ      'S'
049400940331     C     VIDACO        ANDEQ     'S'
049500070515IF  1C     VIDACL        oreq      'S'
049600070515     C     VIDACP        ANDEQ     'S'
049700070515IF  1C     VIDACO        oreq      'S'
049800070515     C     VIDACP        ANDEQ     'S'
049900940331     C                   SETON                                        5690
050000940331     C                   GOTO      ENDCT1
050100940331E   1C                   ENDIF
050200070515     c* Affidabilità partenza selezionabile soltanto per bolle partenza
050300070515     c                   if        vidacp='S' and vidbpa='A'
050400070515     c                   eval      vidmsg=msg(8)
050500070515     C                   SETON                                        829028
050600070515     C                   GOTO      ENDCT1
050700070515E   1C                   ENDIF
050800930611     C*
050900940331     C* D A T E   C O N S E G N A
051000950113     C*
051100940331     C* SPEDIZIONI NON CONSEGNATE: DATA DAL E AL VUOTE
051200930611IF  1C     VIDSPC        IFEQ      'N'
051300950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
051400930611IF  2C     VIDDAL        IFNE      *ZEROS
051500930611     C     VIDAL         ORNE      *ZEROS
051600930611     C                   SETON                                        4090
051700930611     C                   GOTO      ENDCT1
051800930611E   2C                   ENDIF
051900940331E   1C                   ENDIF
052000930611     C*
052100930611     C* SPEDIZIONI CONSEGNATE: ALMENO DATA DAL PIENA
052200931013     C* SE CHIESTE TUTTE SI PUO' IMMETTERE O NON IMMETTERE
052300020212      * Se richiesto bolle arrivo
052400020212     c                   if        vidbpa = 'A'
052500931013IF  1C     VIDSPC        IFEQ      'S'
052600950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
052700940331     C     VIDDAL        ANDEQ     *ZEROS
052800980210IF 1AC     *IN05         IFEQ      *OFF
052900980210     C                   SETON                                        4190
053000980210     C                   GOTO      ENDCT1
053100980210  X1AC                   ELSE
053200980210IF 1BC     VIDDTD        IFEQ      *ZEROS
053300980210     C                   SETON                                        7990
053400980210     C                   GOTO      ENDCT1
053500980210E  1BC                   ENDIF
053600980210E  1AC                   ENDIF
053700930611E   1C                   ENDIF
053800020212     c                   endif
053900020212
054000020212      * se richieste bolle in partenza
054100020212     c                   if        vidbpa = 'P' and vidspc = 'S' and
054200020212     c                             videla <> 'P' and viddal = *zeros
054300020212     c                   eval      *in41 = *on
054400020212     c                   eval      *in90 = *on
054500020212     c                   goto      endct1
054600020212     c                   endif
054700020212
054800940331     C*
054900940331     C* SE CHIESTE TUTTE O DATA CONSEGNA O DATA SPEDIZIONE DA IMMETTERE
055000940331IF  1C     VIDSPC        IFEQ      ' '
055100950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
055200940331     C     VIDDAL        ANDEQ     0
055300940331     C     VIDDSD        ANDEQ     0
055400980209     C     VIDDTD        ANDEQ     *ZEROS
055500980209     C                   SETON                                        6390
055600940331     C                   GOTO      ENDCT1
055700940331E   1C                   ENDIF
055800940527     C*
055900940601     C* SE SELEZIONATE LE SPEDIZIONI CONSEGNATE IL CAMPO DELLA
056000940601     C*   RICHIESTA DELLE BOLLE IN DISTINTA DEVE ESSERE BLANK
056100940527IF  1C   03VIDSPC        IFEQ      'S'
056200940601     C     VIDBDS        ANDNE     ' '
056300940527     C                   SETON                                        6490
056400940527     C                   GOTO      ENDCT1
056500940527E   1C                   ENDIF
056600930611     C*
056700930611     C* DATA AL SOLO SE IMMESSA DATA DAL
056800930611IF  1C     VIDAL         IFNE      *ZEROS
056900930611     C     VIDDAL        ANDEQ     *ZEROS
057000930611     C                   SETON                                        4290
057100930611     C                   GOTO      ENDCT1
057200930611E   1C                   ENDIF
057300940331     C*
057400940331     C* CONTROLLO FORMALE DATA DAL
057500940331IF  1C     VIDDAL        IFNE      *ZEROS
057600940331     C                   Z-ADD     VIDDAL        G02DAT
057700940331     C                   Z-ADD     *ZEROS        G02INV
057800940331     C                   MOVEL     *BLANKS       G02ERR
057900950113     C                   CALL      'XSRDA8'
058000940331     C                   PARM                    WLBDAT
058100950113     C                   Z-ADD     G02DAT        VIDDAL
058200940331     C* DATA ERRATA
058300940331IF  2C     G02ERR        IFEQ      '1'
058400940331     C                   SETON                                        4390
058500940331     C                   GOTO      ENDCT1
058600940331X   2C                   ELSE
058700950301     C                   Z-ADD     G02INV        ROVDAL
058800940331E   2C                   ENDIF
058900940331E   1C                   ENDIF
059000940331     C*
059100940331     C* CONTROLLO FORMALE DATA AL
059200940331IF  1C     VIDAL         IFNE      *ZEROS
059300940331     C                   Z-ADD     VIDAL         G02DAT
059400940331     C                   Z-ADD     *ZEROS        G02INV
059500940331     C                   MOVEL     *BLANKS       G02ERR
059600950113     C                   CALL      'XSRDA8'
059700940331     C                   PARM                    WLBDAT
059800950113     C                   Z-ADD     G02DAT        VIDAL
059900940331     C* DATA ERRATA
060000940331IF  2C     G02ERR        IFEQ      '1'
060100940331     C                   SETON                                        4490
060200940331     C                   GOTO      ENDCT1
060300940331X   2C                   ELSE
060400950301     C                   Z-ADD     G02INV        ROVAL
060500940331E   2C                   ENDIF
060600940331     C*
060700940331     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
060800950301    2C     ROVAL         IFLT      ROVDAL
060900940331     C                   SETON                                        4590
061000940331     C                   GOTO      ENDCT1
061100940331E   2C                   ENDIF
061200940331E   1C                   ENDIF
061300930719     C*
061400940331     C* D A T E   S P E D I Z I O N E
061500950113     C*
061600940331     C* DATA AL SOLO SE IMMESSA DATA DAL
061700940331IF  1C     VIDDSA        IFNE      *ZEROS
061800940331     C     VIDDSD        ANDEQ     *ZEROS
061900940331     C                   SETON                                        5890
062000940331     C                   GOTO      ENDCT1
062100940331E   1C                   ENDIF
062200940331     C*
062300940331     C* CONTROLLO FORMALE DATA DAL
062400940331IF  1C     VIDDSD        IFNE      *ZEROS
062500940331     C                   Z-ADD     VIDDSD        G02DAT
062600940331     C                   Z-ADD     *ZEROS        G02INV
062700940331     C                   MOVEL     *BLANKS       G02ERR
062800950113     C                   CALL      'XSRDA8'
062900940331     C                   PARM                    WLBDAT
063000950113     C                   Z-ADD     G02DAT        VIDDSD
063100940331     C* DATA ERRATA
063200940331IF  2C     G02ERR        IFEQ      '1'
063300940331     C                   SETON                                        5990
063400940331     C                   GOTO      ENDCT1
063500940331X   2C                   ELSE
063600950113     C                   Z-ADD     G02INV        ROVDSD
063700940331E   2C                   ENDIF
063800940331E   1C                   ENDIF
063900940331     C*
064000940331     C* CONTROLLO FORMALE DATA AL
064100940331IF  1C     VIDDSA        IFNE      *ZEROS
064200940331     C                   Z-ADD     VIDDSA        G02DAT
064300940331     C                   Z-ADD     *ZEROS        G02INV
064400940331     C                   MOVEL     *BLANKS       G02ERR
064500950113     C                   CALL      'XSRDA8'
064600940331     C                   PARM                    WLBDAT
064700950113     C                   Z-ADD     G02DAT        VIDDSA
064800940331     C* DATA ERRATA
064900940331IF  2C     G02ERR        IFEQ      '1'
065000940331     C                   SETON                                        6090
065100940331     C                   GOTO      ENDCT1
065200940331X   2C                   ELSE
065300940331     C                   Z-ADD     G02INV        ROVDSA
065400940331E   2C                   ENDIF
065500940331     C*
065600940331     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
065700940331    2C     ROVDSA        IFLT      ROVDSD
065800940331     C                   SETON                                        6190
065900940331     C                   GOTO      ENDCT1
066000940331E   2C                   ENDIF
066100940331E   1C                   ENDIF
066200980209AG*  C* D A T E   T R A S M I S S I O N E
066300980209     C*
066400980209     C* DATA AL SOLO SE IMMESSA DATA DAL
066500980209IF  1C     VIDDTA        IFNE      *ZEROS
066600980209     C     VIDDTD        ANDEQ     *ZEROS
066700980209     C                   SETON                                        7490
066800980209     C                   GOTO      ENDCT1
066900980209E   1C                   ENDIF
067000980209     C*
067100980209     C* CONTROLLO FORMALE DATA DAL
067200980209IF  1C     VIDDTD        IFNE      *ZEROS
067300980209     C                   Z-ADD     VIDDTD        G02DAT
067400980209     C                   Z-ADD     *ZEROS        G02INV
067500980209     C                   MOVEL     *BLANKS       G02ERR
067600980209     C                   CALL      'XSRDA8'
067700980209     C                   PARM                    WLBDAT
067800980209     C                   Z-ADD     G02DAT        VIDDTD
067900980209     C* DATA ERRATA
068000980209IF  2C     G02ERR        IFEQ      '1'
068100980209     C                   SETON                                        7390
068200980209     C                   GOTO      ENDCT1
068300980209X   2C                   ELSE
068400980209     C                   Z-ADD     G02INV        ROVDTD
068500980209E   2C                   ENDIF
068600980209E   1C                   ENDIF
068700980209     C*
068800980209     C* CONTROLLO FORMALE DATA AL
068900980209IF  1C     VIDDTA        IFNE      *ZEROS
069000980209     C                   Z-ADD     VIDDTA        G02DAT
069100980209     C                   Z-ADD     *ZEROS        G02INV
069200980209     C                   MOVEL     *BLANKS       G02ERR
069300980209     C                   CALL      'XSRDA8'
069400980209     C                   PARM                    WLBDAT
069500980209     C                   Z-ADD     G02DAT        VIDDTA
069600980209     C* DATA ERRATA
069700980209IF  2C     G02ERR        IFEQ      '1'
069800980209     C                   SETON                                        7590
069900980209     C                   GOTO      ENDCT1
070000980209X   2C                   ELSE
070100980209     C                   Z-ADD     G02INV        ROVDTA
070200980209E   2C                   ENDIF
070300980209     C*
070400980209     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
070500980209    2C     ROVDTA        IFLT      ROVDTD
070600980209     C                   SETON                                        7690
070700980209     C                   GOTO      ENDCT1
070800980209E   2C                   ENDIF
070900980209E   1C                   ENDIF
071000930611     C*
071100930611     C* P A R T E N Z A:   1° LIVELLO   SPEDIZIONI DI FILIALI IN L1
071200930611     C*                    2° LIVELLO   SPEDIZIONI DELLA FILIALE
071300930611     C*
071400940331IF  1C     VIDLNP        IFNE      *ZEROS
071500940331     C*** BOLLE IN PARTENZA
071600940331    2C     *IN02         IFEQ      *ON
071700120621     c* simfel può elaborare anche linea di partenza non proprio ma cod cliente
071800120621     c*  ddella filiale
071900120621     c                   if        not *in01
072000940331     C*
072100120621     C     VIDLNP        LOOKUP    L1                                     31
072200120621     c                   if        not *in31 and vidklp>0
072300120621     C     VIDklp        LOOKUP    L1                                     31
072400120621     c                   endif
072500930611     C*
072600120621     C* FILIALE NON IN GESTIONE
072700120621IF  3C     *IN31         IFEQ      *OFF
072800940331     C                   SETON                                          6290
072900930611     C                   GOTO      ENDCT1
073000940331E   3C                   ENDIF
073100120621E   3C                   ENDIF
073200940331     C*
073300940331   X2C                   ELSE
073400940331     C*** BOLLE IN ARRIVO
073500940331     C     VIDLNP        CHAIN     AZORG                              31
073600940331     C   31              SETON                                        4890
073700940331     C   31              GOTO      ENDCT1
073800940331    2C                   ENDIF
073900940331    1C                   ENDIF
074000930611     C*
074100930611     C* A R R I V O:   1° LIVELLO   SPEDIZIONI DI FILIALI IN L1
074200930611     C*                2° LIVELLO   SPEDIZIONI DI FILIALI IN L6
074300930611     C*
074400940331IF  1C     VIDLNA        IFNE      *ZEROS
074500940331     C*** BOLLE IN ARRIVO
074600940331    2C     *IN03         IFEQ      *ON
074700930611     C* REMOTO
074800930625IF  3C     *IN01         IFEQ      *ON
074900940331     C     VIDLNA        LOOKUP    L6                                     31
075000930611     C*
075100930611     C* FILIALE NON IN GESTIONE
075200930625IF  4C     *IN31         IFEQ      *OFF
075300930611     C                   SETON                                          4990
075400930611     C                   GOTO      ENDCT1
075500930611E   4C                   ENDIF
075600940331     C* SIMFEL
075700930611X   3C                   ELSE
075800940331     C     VIDLNA        LOOKUP    L1                                     31
075900930611     C*
076000930611     C* FILIALE NON IN GESTIONE
076100930625IF  4C     *IN31         IFEQ      *OFF
076200930611     C                   SETON                                          4990
076300930611     C                   GOTO      ENDCT1
076400930611E   4C                   ENDIF
076500930611E   3C                   ENDIF
076600980209AG*  C* CONTROLLO SE E' UNA LINEA ESTERA
076700980209     C     VIDLNA        CHAIN     AZORG                              31
076800980209IF  5C     *IN31         IFEQ      '0'
076900020212     c                   movel     orgde3        og143
077000020212     C**!!!ORGFL1        ANDNE     'E'
077100020212     c                   if        §ogntw <> 'DPD' and §ogntw <> 'FED' and
077200020212     c                             §ogntw <> 'EEX' and §ogntw <> 'EUP'
077300980209AG*  C* CONTROLLO SE E' STATA INSERITA LA DATA TRASMISSIONE
077400020212     c                              and rovdtd <> 0
077500020212AG*  C**!!!ROVDTD        ANDNE     *ZEROS
077600980209AG*  C                   SETON                                          7790
077700980209AG*  C                   GOTO      ENDCT1
077800020212     c                   endif
077900980209    5C                   ENDIF
078000980209     C*
078100940331   X2C                   ELSE
078200940331     C*** BOLLE IN PARTENZA
078300940331     C     VIDLNA        CHAIN     AZORG                              31
078400940331     C   31              SETON                                        4790
078500940331     C   31              GOTO      ENDCT1
078600930611E   2C                   ENDIF
078700980209     C*
078800980209   X1C                   ELSE
078900980209     C*SE LA LINEA DI ARRIVO E' A ZERO E LA DTA TRASMISSIONE E' VALORIZZATA E'
079000980209     C*ERRORE
079100980209AG*  C     VIDDTD        IFNE      *ZEROS
079200980209AG*  C                   SETON                                          7890
079300980209AG*  C                   GOTO      ENDCT1
079400980209AG*  C                   ENDIF
079500940331E   1C                   ENDIF
079600940331     C*
079700940331     C* C O D I C E   C L I E N T E
079800950113     C*
079900940331IF  1C     VIDCLI        IFNE      *ZEROS
080000940331     C                   MOVEL     VIDCLI        FILCLI            3 0
080100961016     C*
080200961016     C* RICERCA CLIENTE IN ANAGRAFICO
080300961016     C                   Z-ADD     VIDCLI        KSC
080400961016     C     KACO          CHAIN     CNACO00F                           32
080500961016     C* INESISTENTE
080600961016IF  2C     *IN32         IFEQ      *ON
080700961022     C                   SETON                                        285390
080800961022     C                   MOVEL     MSG(4)        VIDMSG
080900961016     C                   MOVEL     'S'           WFORZ             1
081000961016     C                   GOTO      ENDCT1
081100961016    2C                   ENDIF
081200961016     C*
081300961016     C* ESISTENTE MA ANNULLATO
081400961016IF  2C     ACOFLG        IFNE      *BLANKS
081500961022     C                   SETON                                        285390
081600961022     C                   MOVEL     MSG(5)        VIDMSG
081700961016     C                   MOVEL     'S'           WFORZ             1
081800961016     C                   GOTO      ENDCT1
081900961016E   2C                   ENDIF
082000961016     C                   MOVEL     ACORAG        VIDDCM
082100951018     C*
082200951018     C* WFORZ ---> CAMPO UTILIZZATO PER GESTIRE LA FORZATURA DEL CODICE
082300951018     C*   CLIENTE NEL CASO VENGANO RICHIESTE STAMPE RELATIVE A CLIENTI
082400951018     C*   DI ALTRA FILIALE
082500951018     C     VIDCLI        IFNE      COMCLI
082600951018     C                   CLEAR                   WFORZ
082700951018     C                   MOVEL     VIDCLI        COMCLI
082800951018     C                   ENDIF
082900951018     C*
083000951018   1AC     WFORZ         IFEQ      ' '
083100940331     C*** BOLLE IN PARTENZA
083200940331    2C     *IN02         IFEQ      *ON
083300940331     C* REMOTO
083400940331    3C     *IN01         IFEQ      *ON
083500940331     C* SIMFEL CON LNP IMMESSA
083600940331    3C     *IN01         OREQ      *OFF
083700940331     C     VIDLNP        ANDGT     0
083800120621IF  4C**   FILCLI        IFNE      VIDLNP
083900120621     C**                 SETON                                        522890
084000120621     C**                 MOVEL     MSG(3)        VIDMSG
084100120621     C**                 MOVEL     'S'           WFORZ             1
084200120621     C**                 GOTO      ENDCT1
084300120621E   4C**                 ENDIF
084400940331     C*
084500940331   X3C                   ELSE
084600940331     C*
084700940331     C* SIMFEL SENZA LNP IMMESSA
084800940331     C     FILCLI        LOOKUP    L1                                     31
084900940331     C*
085000940331     C* FILIALE NON IN GESTIONE
085100951018IF  4C     *IN31         IFEQ      *OFF
085200120621     C                   SETON                                        522890
085300961022     C                   MOVEL     MSG(2)        VIDMSG
085400951018     C                   MOVEL     'S'           WFORZ             1
085500940331     C                   GOTO      ENDCT1
085600940331E   4C                   ENDIF
085700940331E   3C                   ENDIF
085800940331     C*
085900940331   X2C                   ELSE
086000940331     C*
086100940331     C*** BOLLE IN ARRIVO
086200940331    3C     VIDLNA        IFGT      0
086300940331IF  3C     FILCLI        IFNE      VIDLNA
086400961022     C                   SETON                                        522890
086500961022     C                   MOVEL     MSG(3)        VIDMSG
086600951018     C                   MOVEL     'S'           WFORZ             1
086700940331     C                   GOTO      ENDCT1
086800940331E   4C                   ENDIF
086900940331     C*
087000940331   X3C                   ELSE
087100930611     C* REMOTO
087200940331     C   01FILCLI        LOOKUP    L6                                     31
087300940331     C* SIMFEL
087400940331     C  N01FILCLI        LOOKUP    L1                                     31
087500930611     C*
087600930611     C* FILIALE NON IN GESTIONE
087700930625IF  4C     *IN31         IFEQ      *OFF
087800961022     C                   SETON                                        522890
087900961022     C                   MOVEL     MSG(2)        VIDMSG
088000951018     C                   MOVEL     'S'           WFORZ             1
088100930611     C                   GOTO      ENDCT1
088200930611E   4C                   ENDIF
088300930611E   3C                   ENDIF
088400930611E   2C                   ENDIF
088500930611     C*
088600951018   1AC                   ENDIF
088700951018     C*
088800930611E   1C                   ENDIF
088900930611     C*
089000931014     C* ORE DI RITARDO: DA INSERIRE SONO SE RICHIESTO "R"
089100931014     C     VIDSRA        IFNE      'R'
089200931014     C     VIDGIO        ANDGT     0
089300931014     C                   SETON                                        5790
089400931014     C                   GOTO      ENDCT1
089500931014     C                   ENDIF
089600931014     C* ORE DI RITARDO: INSERIRE SOLO MULTIPLI DI 12 ORE
089700930719IF  1C     VIDGIO        IFNE      *ZEROS
089800931014IF  1C     VIDGIO        ANDNE     999
089900930719     C     VIDGIO        DIV       12            QUOZ              3 0
090000930719     C                   MVR                     REST              3 0
090100930719IF  2C     REST          IFNE      *ZEROS
090200930611     C                   SETON                                        9054
090300930611     C                   GOTO      ENDCT1
090400930719E   2C                   ENDIF
090500930719E   1C                   ENDIF
090600950113     C*
090700950113     C* TIPO SERVIZIO
090800950113IF  1C     VIDTIP        IFNE      *BLANKS
090900950113IF  2C     VIDTIP        IFEQ      '?'
091000950113     C                   MOVEL     CODUT         §KUT
091100950113     C                   MOVEL     '5E'          §COD
091200950113     C                   MOVEL     *BLANKS       VIDTIP
091300950113     C                   MOVEL     *BLANKS       §KEY
091400950113     C                   CALL      'X§TABER'
091500950113     C                   PARM                    §KUT
091600950113     C                   PARM                    §COD
091700950113     C                   PARM                    §KEY
091800950113     C                   PARM                    §DES             30
091900950113     C                   MOVEL     §KEY          VIDTIP
092000950301     C                   MOVEL     §DES          DECTIP
092100950113     C                   SETON                                        90
092200950113     C                   GOTO      ENDCT1
092300950113E   2C                   ENDIF
092400950113     C*
092500950301     C                   MOVEL     *BLANKS       DECTIP
092600950113     C                   MOVEL     '5E'          COD
092700950113     C                   MOVEL     *BLANKS       KEY
092800950113     C                   MOVEL     VIDTIP        KEY
092900950113IF  2C     KTAB          CHAIN     TABEL00F                           90
093000950113     C     *IN90         IFEQ      *ON
093100950113     C                   SETON                                        66
093200950113     C                   GOTO      ENDCT1
093300950113E   2C                   ENDIF
093400950301     C                   MOVEL     TBLUNI        DECTIP
093500950113E   1C                   ENDIF
093600070302     c
093700070302     c* Nazione e cap destinatario
093800070302    1c                   if        vidcad<>*blanks or vidnzd<>*blanks
093900070302     c                   clear                   dssi95
094000070302     c                   clear                   dslv13
094100070302     C                   MOVEL     '3'           I95TCN
094200070302     C                   MOVEL     vidcad        I95CAP
094300070302     C                   MOVEL     vidnzd        I95NAR
094400070302     C                   MOVEL     *date         I95DAT
094500070302c
094600070302    2C     vidCAd        IFNE      VARcap
094700070302     C                   CLEAR                   wfor1
094800070302     C                   CLEAR                   AAA
094900070302     C                   CLEAR                   AAB
095000070302     C                   CLEAR                   AAD
095100070302     C                   MOVEL     vidcad        VARCAp
095200070302    2C                   ENDIF
095300070302     C**
095400070302     C                   MOVEL     wfor1         E13FZV
095500070302     C                   MOVEL     AAA           E13FZ1
095600070302     C                   MOVEL     AAB           E13FZ2
095700070302     C                   MOVEL     AAD           E13FZ3
095800070302     C**
095900070302     C** CALL
096000070302     C                   CALL      'FNLV13R'
096100070302     C                   PARM                    KPJBA
096200070302     C                   PARM                    DSLV13
096300070302     C                   PARM                    DSSI95
096400070302     C*
096500070302     C                   MOVEL     O13ERR        WERR              1
096600070302     C                   MOVEL     O13MSG        vidMSG
096700070302     C**
096800070302     C* REIMPOSTO LE FORZATURE
096900070302     C                   MOVEL     E13FZV        wfor1
097000070302     C                   MOVEL     E13FZ1        AAA
097100070302     C                   MOVEL     E13FZ2        AAB
097200070302     C                   MOVEL     E13FZ3        AAD
097300070302     C**
097400070302    2C     WERR          IFNE      ' '
097500070302    3C     vidMSG        ifne      *BLANKS
097600070302     C                   SETON                                        28
097700070302    3c                   endif
097800070302     c                   seton                                        9039
097900070302     c                   goto      endct1
098000070302    2c                   endif
098100070302    1c                   endif
098200950301     C*
098300950301     C*----------------------------------------
098400950301     C* CODICE PADRONCINO DAL-AL
098500950301     C*----------------------------------------
098600950303     C* PADRONCINO OBBLIGATORIO SOLO SE RICHIESTA TALE ELABORAZIONE
098700950303     C     VIDBPA        IFEQ      'P'                                          'P'=PARTENZA
098800950303     C     VIDELA        ANDEQ     'P'                                          'P'=PADRONCINO
098900950303     C     VIDPD1        IFEQ      *ZERO
099000950303     C                   SETON                                        72  90
099100950303     C                   GOTO      ENDCT1
099200950303     C                   ENDIF
099300950303     C                   ENDIF
099400950301     C* CODICE INIZIALE <= A QUELLO FINALE
099500950301     C     VIDPD1        IFGT      VIDPD2
099600950301     C     VIDPD2        ANDGT     *ZERO
099700950301     C                   SETON                                        67  90
099800950301     C                   GOTO      ENDCT1
099900950301     C                   ENDIF
100000950301     C* SE CODICI UGUALI, PULISCO IL SECONDO
100100950301     C     VIDPD1        IFEQ      VIDPD2
100200950301     C                   CLEAR                   VIDPD2
100300950301     C                   ENDIF
100400020607
100500020607      * se inserito il solo codice "dal" deve:
100600020607 b1  c                   if        vidpd1 <> *zeros and vidpd2 = *zeros
100700020607      * esistere in anagrafica
100800021203     c*    vidpd1        chain     fnapd01l
100900021203 b2  c*                  if        not %found(fnapd01l)
101000021203     C                   movel     vidpd1        kpdpdr
101100021203     C                   move      'A'           kpdtip
101200021203     C     kfapd         chain     fiapd01L
101300021203 b2  c                   if        not %found(fiapd01l)
101400020607     c                   eval      *in80 = *on
101500020607     c                   eval      *in28 = *on
101600020607     c                   eval      *in90 = *on
101700020607     c                   eval      vidmsg = msg(6)
101800020607     c                   goto      endct1
101900020607 e2  c                   endif
102000020607 e1  c                   endif
102100020607      * se inserito il solo codice "dal" per lancio fatto dal primo livello
102200020607 b2  c                   if        vidpd1 <> *zeros and vidpd2 = *zeros
102300020607     c                             and not *in01
102400020607      * deve essere gestito dal p.o.
102500020607     c                   movel     vidpd1        wpdc
102600020607     c     wpdc          lookup    l1                                     31
102700020607 b3  c                   if        not *in31
102800020606     c                   eval      *in80 = *on
102900020606     c                   eval      *in28 = *on
103000020606     c                   eval      *in90 = *on
103100020606     c                   eval      vidmsg = msg(7)
103200020606     c                   goto      endct1
103300020607 e3  c                   endif
103400020607 e2  c                   endif
103500020607      * se inserita linea di partenza
103600020607 b2  c                   if        vidlnp <> *zeros and
103700020607      * inserito un range il codice deve essere gestito dalla linea di partenza
103800020607     c                             ((vidpd1 <> 0 and vidpd2 <> 0)  or
103900020607      * inserito solo il codice "dal" deve essere gestito dalla linea di partenza
104000020607     c                             (vidpd1 <> 0 and vidpd2 = 0))
104100020607     c                   movel     vidpd1        wpdc
104200020607 b3  c                   if        wpdc <> vidlnp
104300020607     c                   eval      *in80 = *on
104400020607     c                   eval      *in28 = *on
104500020607     c                   eval      *in90 = *on
104600020607     c                   eval      vidmsg = msg(8)
104700020607     c                   goto      endct1
104800020607 e3  c                   endif
104900020607 b3  c                   if        vidpd2 <> *zeros
105000020607     c                   movel     vidpd2        wpdc
105100020607 b4  c                   if        wpdc <> vidlnp
105200020607     c                   eval      *in81 = *on
105300020607     c                   eval      *in28 = *on
105400020607     c                   eval      *in90 = *on
105500020607     c                   eval      vidmsg = msg(8)
105600020607     c                   goto      endct1
105700020607 e4  c                   endif
105800020607 e3  c                   endif
105900020607 e2  c                   endif
106000950301     C*----------------------------------------
106100950301     C* (PER PADRONCINO) DATA RITIRO MERCE
106200950301     C*----------------------------------------
106300950301     C                   Z-ADD     *ZERO         ROVDR1
106400950301     C                   Z-ADD     *ZERO         ROVDR2
106500950301     C     VIDDR1        IFNE      *ZERO
106600950301     C                   Z-ADD     VIDDR1        G02DAT
106700950301     C                   Z-ADD     *ZEROS        G02INV
106800950301     C                   MOVEL     *BLANKS       G02ERR
106900950301     C                   CALL      'XSRDA8'
107000950301     C                   PARM                    WLBDAT
107100950301     C* DATA ERRATA
107200950301IF  2C     G02ERR        IFEQ      '1'
107300950301     C                   SETON                                        68  90
107400950301     C                   GOTO      ENDCT1
107500950301     C                   ENDIF
107600950301     C                   Z-ADD     G02DAT        VIDDR1
107700950301     C                   Z-ADD     G02INV        ROVDR1
107800950301     C                   ENDIF
107900950301     C*
108000950301     C     VIDDR2        IFNE      *ZERO
108100950301     C                   Z-ADD     VIDDR2        G02DAT
108200950301     C                   Z-ADD     *ZEROS        G02INV
108300950301     C                   MOVEL     *BLANKS       G02ERR
108400950301     C                   CALL      'XSRDA8'
108500950301     C                   PARM                    WLBDAT
108600950301     C* DATA ERRATA
108700950301IF  2C     G02ERR        IFEQ      '1'
108800950301     C                   SETON                                        69  90
108900950301     C                   GOTO      ENDCT1
109000950301     C                   ENDIF
109100950301     C                   Z-ADD     G02DAT        VIDDR2
109200950301     C                   Z-ADD     G02INV        ROVDR2
109300950301     C                   ENDIF
109400950301     C* DATA INIZIALE <= DATA FINALE
109500950301     C     ROVDR1        IFGT      ROVDR2
109600950301     C     ROVDR2        ANDGT     *ZERO
109700950301     C                   SETON                                        70  90
109800950301     C                   GOTO      ENDCT1
109900950301     C                   ENDIF
110000950301     C* SE DATA FINALE = DATA INIZIALE, PULISCO LA FINALE
110100950301     C     ROVDR2        IFEQ      ROVDR1
110200950301     C                   Z-ADD     *ZERO         VIDDR2
110300950301     C                   Z-ADD     *ZERO         ROVDR2
110400950301     C                   ENDIF
110500130115     c* se immesso ind e-mail immettere S in file escel e viceversa
110600130115     c                   if        (videxcel='S' and videmail=*blanks) or
110700130115     c                             (videxcel=' ' and videmail<>*blanks)
110800130115     C                   SETON                                        285190
110900130115     C                   MOVEL     MSG(12)       VIDMSG
111000130115     C                   GOTO      ENDCT1
111100130115    3C                   ENDIF
111200130115
111300130115      /free
111400130115       // controllo formale indirizzo email
111500130115       if videmail<>*blanks   ;
111600130115       clear dsemail  ;
111700130115       §EMLINDI=%trim(videmail)+'@brt.it'   ;
111800130115       callp XEMAIL   (dsemail)  ;
111900130115         if §EMLERRO<>*blanks and §EMLERRO<> '0';
112000130115             vidmsg = §emlmsgo   ;
112100130115             *in90='1'   ;
112200130115             *in28='1'   ;
112300130115             *in51='1'   ;
112400130115             leavesr;
112500130115         endif   ;
112600130115       endif   ;
112700130115      /end-free
112800130115
112900950301     C*
113000930611     C     ENDCT1        TAG
113100930610     C                   ENDSR
113200930610     C*---------------------------------------------------------------*
113300121220     c* videata di richeista cod raggruppamenti
113400121220     C*---------------------------------------------------------------*
113500121220     c     Wind05        BEGSR
113600121220     c
113700121220     c                   setoff                                       15
113800121220     c                   clear                   savragr
113900121220
114000121220     c     for05         TAG
114100121220     c                   exfmt     lrb6w05
114200121220
114300121220     c                   clear                   tntb96ds
114400121220     c                   clear                   v5ddesc1
114500121220     c                   clear                   v5ddesc2
114600121220     c                   setoff                                       2860
114700121220     c
114800121220     c* Manutenzione tabella raggruppamenti
114900121220     c                   if        *inkh='1'
115000121220     c                   movel     tntb96ds      kpjbu
115100121220     c                   call      'TNTB96R'
115200121220     c                   parm                    kpjba
115300121220     c
115400121220     c                   goto      for05
115500121220     c                   endif
115600121220     c
115700121220     c* Controllo se immesso il codice raggruppamenti
115800121220     c* ricerca
115900121220     c                   eval      Indx=%scan('?': v5cragr)
116000121220     c                   if        indx>0
116100121220     c                   eval      b96fun='1'
116200121220     c                   eval      b96ke1=%editc(simpou:'X')
116300121220     c
116400121220     c                   movel     tntb96ds      kpjbu
116500121220     c                   call      'TNTB96R'
116600121220     c                   parm                    kpjba
116700121220     c
116800121220     c                   movel     kpjbu         tntb96ds
116900121220     c                   if        b96ke2>0
117000121220     c                   eval      v5cragr=%editc(b96ke2:'X')
117100121220     c                   exsr      chainTNTBE
117200121220     c                   else
117300121220     c                   clear                   v5cragr
117400121220     c                   endif
117500121220     c
117600121220     c                   goto      for05
117700121220     c                   endif
117800121220     c
117900121220     c* Se immesso codice non immettere raggruppamento e viceversa
118000121221     c                   if        vidksc>0 and v5cragr<>*zeros
118100121221     c                             and v5cragr<>*blanks
118200121220     c                   seton                                        2860
118300121220     c                   eval      vd5msg=msg(3)
118400121220     c                   goto      for05
118500121220     c                   endif
118600121220     c
118700121220     c* controllo codice raggruppamento
118800121221     c                   if        v5cragr<>*zeros   and v5cragr<>*blanks
118900121220     c                   exsr      chainTNTBE
119000121221     c                   if        not %found(tntbe01l)  or tbeatb<>' '
119100121220     c                   seton                                        2860
119200121220     c                   eval      vd5msg=msg(9)
119300121220     c                   goto      for05
119400121220     c                   endif
119500121220     c                   endif
119600121220     c
119700121220     c                   if        v5cragr<>savragr
119800121220     c                   eval      savragr=v5cragr
119900121220     c                   goto      for05
120000121220     c                   endif
120100121220     c
120200121220     c
120300121220     c                   if        v5cragr>*zeros
120400121220     c                   seton                                        15
120500121220     c                   endif
120600121220     c
120700121220     c                   ENDSR
120800121220     C*---------------------------------------------------------------*
120900121220     c     chainTNTBE    BEGSR
121000121220     c
121100121220     c                   eval      kcod='KPV'
121200121220     c                   eval      kke1=%editc(simpou:'X')
121300121220     c                   eval      kke2=v5cragr
121400121220     c
121500121220     c     ktbe          chain     tntbe01l
121600121220     c                   if        %found(tntbe01l)
121700121220     c                   eval      v5ddesc1=%subst(tbeuni:1:30)
121800121220     c                   eval      v5ddesc2=%subst(tbeuni:31:30)
121900121220     c                   endif
122000121220     c                   ENDSR
122100121220     C*---------------------------------------------------------------*
122200930610     C*                    CARTAB                                     *
122300930610     C* ROUTINE DI CARICAMENTO TABELLA L1 E L6                        *
122400930610     C*---------------------------------------------------------------*
122500930610     C     CARTAB        BEGSR
122600940331     C                   SETOFF                                           04
122700940331     C* 02 ON - BOLLE IN PARTENZA
122800940331     C* 03 ON - BOLLE IN ARRIVO
122900940331     C     VIDBPA        COMP      'P'                                    02
123000940331     C     VIDBPA        COMP      'A'                                    03
123100930610     C*
123200020419     C* Carico  L1
123300020419     c                   clear                   trul06ds
123400020419     c                   eval      d06cod = '£1'
123500020419     c                   movel     simfel        d06key
123600020419     c                   movel     trul06ds      kpjbu
123700020419     c                   call      'TRUL06R'
123800020419     c                   parm                    kpjba
123900020419     c                   movel     kpjbu         trul06ds
124000020419     c                   movea     lin           l1
124100020419
124200980209AG*  C* CONTROLLO LINEA DI ARRIVO ESTERO SE FLAG AZORG UGUALE AD "E"
124300020419 b2  c                   do        30            wa
124400020419     c                   if        l1(wa) = *zeros or *in05 = *on
124500020419     c                   leave
124600020419     c                   endif
124700020419     C     l1(wa)        CHAIN     AZORG01L                           34
124800020419IF  3C     *IN34         IFEQ      '0'
124900020212     c                   movel     orgde3        og143
125000020212     c                   if        §ogntw = 'DPD' or §ogntw = 'FED' or
125100020212     c                             §ogntw = 'EEX' or §ogntw = 'EUP'
125200980209     C                   SETON                                        05
125300020212     c                   endif
125400020419E   3C                   ENDIF
125500020419 e1  c                   enddo
125600930610     C*
125700930610     C* CARICO LA TABELLA L6 SOLO SE SONO II LIVELLO
125800020513IF  1C*    REM           IFEQ      'REM'
125900020513     C*    REMFIL        ANDGT     *ZEROS
126000020513    1c     simtpp        ifeq      '2'
126100020513     c     simtpp        oreq      *blanks
126200940331     C*
126300930610     C                   SETON                                        01
126400940331     C***
126500940331     C* CARICO L6 SOLO PER BOLLE IN ARRIVO
126600940331     C***
126700940331   1AC     *IN03         IFEQ      *ON
126800020419
126900020419     c                   clear                   trul06ds
127000020419     c                   eval      d06cod = '£6'
127100020513     c*                  movel     remfil        d06key
127200020513     c                   movel     simpou        d06key
127300020419     c                   movel     trul06ds      kpjbu
127400020419     c                   call      'TRUL06R'
127500020419     c                   parm                    kpjba
127600020419     c                   movel     kpjbu         trul06ds
127700020419     c                   movea     lin           l6
127800020419
127900940331     C* SE BOLLE IN ARRIVO   PROPONGO LA LINEA NON PROTETTA (PER LA L6)
128000020513     C*                  MOVEL     REMFIL        VIDLNA
128100020513     C                   MOVEL     simpou        VIDLNA
128200940331     C* 04 ON -L6 HA SOLTANTO UN ELEMENTO --> PROTEGGO LA LINEA DI ARR
128300940331     C     L6(2)         IFEQ      0
128400940331     C                   SETON                                        04
128500940331     C                   ENDIF
128600940331E  1AC                   ENDIF
128700940331     C*
128800940331     C* SE BOLLE IN PARTENZA PROPONGO LA LINEA PROTETTA
128900020513     C   02              MOVEL     simpou        VIDLNP
129000940331E   1C                   ENDIF
129100930610     C*
129200930611     C                   SETOFF                                           31
129300930611     C*
129400930610     C                   ENDSR
129500950328     C*---------------------------------------------------------------*
129600950328     C*                    RICALF                                     *
129700950328     C* RICERCA ALFABETICA                                            *
129800950328     C*---------------------------------------------------------------*
129900950328     C     RICALF        BEGSR
130000950328     C* DESCRIZIONE AZIENDA
130100950328     C                   MOVEL     RSUT          PAXDUT           30
130200950328     C* PAXSTA=9 ESCLUDO ANNULLATI
130300950328     C                   Z-ADD     9             PAXSTA
130400950328     C* CAPOCONTO CLIENTI
130500950328     C                   Z-ADD     KCI           PAXCCC
130600950328     C*
130700950328     C                   CLEAR                   ACOKSC
130800950328     C*
130900950328     C                   MOVEL     VIDDCM        PAXDMT
131000961022     C                   MOVEL     VIDKLP        PAXFLR
131100950328     C*
131200950328     C                   CALL      'XALFA1'
131300950328     C                   PARM                    PAXDUT
131400950328     C                   PARM                    CODUT
131500950328     C                   PARM                    PAXDMT
131600950328     C                   PARM                    PAXCCC
131700950328     C                   PARM                    ACOKSC
131800950328     C                   PARM                    PAXSTA            1 0
131900961016     C                   PARM                    PAXFLR           90
132000950328     C*
132100950328     C* NON TROVATO O PREMUTO CMD12
132200950328    2C     PAXSTA        IFGT      -1
132300950328     C* CODICE SELEZIONATO: IMPOSTO A VIDEO
132400950328     C                   MOVE      ACOKSC        VIDCLI
132500950328     C                   MOVEL     PAXDMT        VIDDCM
132600950328    2C                   ENDIF
132700950328     C*
132800950328     C                   ENDSR
132900930610     C*---------------------------------------------------------------*
133000950113     C* *INZSR - OPERAZIONI INIZIALI                                  *
133100930610     C*---------------------------------------------------------------*
133200950113     C     *INZSR        BEGSR
133300930610     C*
133400930610     C     *ENTRY        PLIST
133500930610     C                   PARM                    KPJBA
133600950113     C* DEFINIZIONE CAMPI
133700950113     C     *LIKE         DEFINE    TBLKEY        §KEY
133800950113     C     *LIKE         DEFINE    TBLCOD        §COD
133900950113     C     *LIKE         DEFINE    TBLKUT        §KUT
134000950328     C     *LIKE         DEFINE    ACOKCC        PAXCCC
134100950328     C     *LIKE         DEFINE    ACORAG        PAXDMT
134200951018     C     *LIKE         DEFINE    VIDCLI        COMCLI
134300930610     C*
134400930611     C* RICERCA FILIALE IN GESTIONE
134500930610     C                   Z-ADD     1             CODUT
134600930610     C                   CALL      'X§PARUT'
134700930610     C                   PARM                    UT§DSE
134800930610     C                   MOVEL     REC80         CNCR80
134900930610     C                   MOVEL     RAGUT         RSUT             20
135000930610     C*
135100930610     C* TABEL00F
135200930610     C     KTAB          KLIST
135300930610     C                   KFLD                    CODUT             1 0
135400930610     C                   KFLD                    COD               2
135500930610     C                   KFLD                    KEY               8
135600121220     C     KTbe          KLIST
135700121220     C                   KFLD                    kcod
135800121220     C                   KFLD                    kke1
135900121220     C                   KFLD                    kke2
136000930611     C*
136100930611     C* CNACO00F
136200930611     C     KACO          KLIST
136300930611     C                   KFLD                    CODUT
136400930611     C                   KFLD                    KCI
136500930611     C                   KFLD                    KSC               7 0
136600021203
136700021203     C     Kfapd         klist
136800021203     C                   kfld                    kpdtip
136900021203     C                   kfld                    kpdpdr
137000930610     C*
137100930611     C* RICERCA CAPOCONTI
137200930611DO  1C                   DO        50            X                 2 0
137300930611     C                   MOVE      TCU(X)        TCUDS
137400930611     C     F56           CABNE     'CG'          END1
137500930611     C     F4            COMP      '1'                                    21
137600930611     C     F4            COMP      '2'                                    22
137700930611     C     F4            COMP      '3'                                    23
137800930611     C     F4            COMP      '6'                                    27
137900930611     C*
138000930611     C* 1 CLIENTI    21
138100930611     C* 2 FORNITORI  22
138200930611     C* 3 AGENTI     23
138300930611     C*
138400930611     C     F3            COMP      '0'                                242425
138500930611     C     F3            COMP      'I'                                    26
138600930611     C*
138700930611     C* 0 ITALIA          25
138800930611     C* 1 ESTERO          24
138900930611     C* I CAPO CONTO IVA  26
139000930611     C*
139100930611     C   21
139200930611     CAN 24              Z-ADD     KCU(X)        KCE               4 0
139300930611     C   21
139400930611     CAN 25              Z-ADD     KCU(X)        KCI               4 0
139500930611     C   22
139600930611     CAN 24              Z-ADD     KCU(X)        KFE               4 0
139700930611     C   22
139800930611     CAN 25              Z-ADD     KCU(X)        KFI               4 0
139900930611     C   23
140000930611     CAN 24              Z-ADD     KCU(X)        KAE               4 0
140100930611     C   23
140200930611     CAN 25              Z-ADD     KCU(X)        KAI               4 0
140300930611     C   26              Z-ADD     KCU(X)        KIVA              4 0
140400930611     C   27              Z-ADD     KCU(X)        KBNA              4 0
140500930611     C     END1          TAG
140600930611E   1C                   ENDDO
140700930611     C*
140800930610     C                   ENDSR
140900961022**  MSG
141000961022Immettere linea di partenza valida                                            1
141100961022Cliente non in gestione: Enter per forzare                                    2
141200121220Se immesso raggruppamento clienti NON immettere il codice cliente e viceversa 3
141300961022Cliente non presente in anagrafico                                            4
141400961022Codice cliente annullato                                                      5
141500020606Codice autotrasportatore inesistente                                          6
141600020606Autotrasportatore in gestione ad altro p.o.                                   7
141700070515Affidabilità PARTENZA richiedibile soltanto per bolle Partenza
141800121220Codice raggruppamento inesitente                                              9
141900130114Rilevati problemi nel lancio del programma in coda : uscire e riprovare!      10
142000130114Se richiesto file excel immettere il codice cliente o il raggruppamento       11
142100130115Se inserito indirizzo  e-mail, impostare la richiesta del file excel          12
