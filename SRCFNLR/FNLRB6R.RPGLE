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
003900150610      * P.O. gestibili dall'utente
004000150610     d fig             s              3    dim(250) inz(*Zeros)
004100961021     D*------------------
004200961021     D* MESSAGGI ERRORE
004300961021     D*------------------
004400130115     D MSG             S             78    DIM(13) CTDATA PERRCD(1)             MESSAGGI ERRORE
004500930610     D*---------------------------------------------------------------*
004600930610     D* DS                                                            *
004700930610     D*---------------------------------------------------------------*
004800961021     D                 DS
004900961021     D  VIDKLP                 1      3  0
005000961021     D  VIDKSC                 4      7  0
005100961021     D  VIDCLI                 1      7  0
005200930611     D*------------------
005300930611     D* DS RICERCA CAPOCONTO
005400930611     D*------------------
005500930611     D TCUDS           DS
005600930611     D  F1                     1      1
005700930611     D  F3                     3      3
005800930611     D  F2                     2      2
005900930611     D  F4                     4      4
006000930611     D  F56                    5      6
006100930610     D*------------------
006200950113     D* INVERSIONE DATA (8)
006300930610     D*------------------
006400930610     D WLBDAT          DS
006500950113     D  G02DAT                 1      8  0
006600950113     D  G02INV                 9     16  0
006700950113     D  G02ERR                17     17
006800950113     D  G02TGI                18     22  0
006900020212
007000020212      *------------------
007100020212      * Ds AZORG
007200020212      *------------------
007300020212     d og143         e ds
007400020419      *------------------
007500020419      * ds per trul06r
007600020419      *------------------
007700020419     d trul06ds      e ds
007800020419     d  lin                    1     90  0 dim(30)
007900070302     d
008000070302     D* DS PER FNLV13R - CONTROLLO DI UN CAP
008100070302     D DSLV13        E DS                  EXTNAME(FNLV13DS)
008200070302     D* DS PER TISI95R - CONTROLLO DI CAP
008300070302     D DSSI95        E DS                  EXTNAME(TISI95DS)
008400070302     d
008500121220     D tntb96ds      E DS
008600150610     d Tibs34ds      e ds
008700150610     d Trul31ds      e ds
008800150610     d Azuteds       e ds                  extname(Azute00f)
008900150610     d dDatiute      e ds
009000150610     d* Controllo abilitazioni utente
009100150610     d tntaa1ds      e ds
009200150610     d dUTE01        e ds
009300020212
009400930610     D*------------------
009500950113     D* PARAMETRI LANCIO
009600930610     D*------------------
009700950301     D PARAM           DS
009800950301     D* BOLLE PARTENZA O ARRIVO (P/A)
009900950301     D  VIDBPA                 1      1
010000950301     D* BOLLE CONSEGNATE O NON  (S/N)
010100950301     D  VIDSPC                 2      2
010200950301     D* DATA 'DAL' (GG/MM/AAAA)
010300950301     D  VIDDAL                 3      7P 0
010400950301     D* DATA 'AL'  (GG/MM/AAAA)
010500950301     D  VIDAL                  8     12P 0
010600950301     D* DATA 'DAL' (AAAA/MM/GG)
010700950301     D  ROVDAL                13     17P 0
010800950301     D* DATA 'AL'  (AAAA/MM/GG)
010900950301     D  ROVAL                 18     22P 0
011000950301     D* FILIALE DI PARTENZA
011100950301     D  VIDLNP                23     24P 0
011200950301     D* FILIALE DI ARRIVO
011300950301     D  VIDLNA                25     26P 0
011400950301     D* CODICE SOTTOCONTO
011500950301     D  PARKCI                27     29P 0
011600950301     D* BOLLE DEL CLIENTE
011700961021     D  PARCLI                30     33P 0
011800950301     D* GIORNI DI RITARDO
011900950301     D  VIDGIO                34     35P 0
012000950301     D* FILIALE ELABORATORE (SIMFEL O REMFIL)
012100950301     D  PARFEL                36     37P 0
012200050623     D* stampadifferenze peso MAggiori MInori EE-entrambe
012300050623     D  vidmag                38     39
012400121220     d*  indico se devo leggere il codice cliente o il raggruppaoemnto
012500121220     d  parragr               40     40
012600121221     d* richiesta file excel  --> l'indirizzo e-mail lo metto in un file di appoggio
012700121221     d  videxcel              41     41
012800130115     d  parprg_kpjbu          42     45P 0
012900121220     d
013000950301     D* PASSO LINEE DI PARTENZA DA ELABORARE
013100950301     D  LNP                   58    117P 0
013200940331     D                                     DIM(30)
013300950301     D* PASSO LINEE DI ARRIVO DA ELABORARE
013400950301     D  LNA                  118    177P 0
013500940331     D                                     DIM(30)
013600950301     D* TIPO SERVIZIO
013700950301     D  VIDTIP               178    178
013800950301     D* S=AFFIDABILITA' CLIENTI
013900950301     D  VIDACL               179    179
014000950301     D* S=AFFIDABILITA' CONSEGNE
014100950301     D  VIDACO               180    180
014200050623     D* STAMPA ORE RITARDO   (S/N)  O DIFFERENZE DI PESO/VOLUMI(D/P/V)
014300050621     D  PARVRA               181    181
014400950113     D* SPEDIZIONI IN RITARDO/ANTICIPO/TUTTE
014500950301     D  VIDSRA               182    182
014600950301     D* DATA SPEDIZIONE DAL (GG/MM/AAAA)
014700950301     D  VIDDSD               183    187P 0
014800950301     D* DATA SPEDIZIONE AL (GG/MM/AAAA)
014900950301     D  VIDDSA               188    192P 0
015000950301     D* DATA SPEDIZIONE DAL (AAAA/MM/GG)
015100950301     D  ROVDSD               193    197P 0
015200950301     D* DATA SPEDIZIONE AL (AAAA/MM/GG)
015300950301     D  ROVDSA               198    202P 0
015400940527     D* BOLLE IN DISTINTA O NON  (S/N)
015500950301     D  VIDBDS               203    203
015600950301     D* ELABORAZIONE: D=DATA CONS/SPED O P=PADRONCINO
015700950302     D  VIDELA               204    204
015800950301     D* STAMPA A T=TOTALE O IN D=DETTAGLIO
015900950302     D  VIDTOT               205    205
016000950220     D* COD.PADRONCINO DAL/AL
016100950302     D  VIDPD1               206    209P 0
016200950302     D  VIDPD2               210    213P 0
016300950301     D* M=MATTINA / P=POMERIGGIO (PER PADRONCINO)
016400950302     D  VIDFPP               214    214
016500950301     D* NUMERO RITIRO (PER PADRONCINO)
016600950302     D  VIDNRT               215    215
016700950301     D* DATA RITIRO (PER PADRONCINO) GG/MM/AAAA DAL
016800950302     D  VIDDR1               216    220P 0
016900950301     D* DATA RITIRO (PER PADRONCINO) GG/MM/AAAA AL
017000950302     D  VIDDR2               221    225P 0
017100070302     D* nazione detinatario
017200070302     D  VIDnzd               226    228
017300070515     D* S=AFFIDABILITA' partenza
017400070515     D  VIDACp               229    229
017500960614     D* STAMPA DEI SOLI RITARDI ("S"=SI/" "=NO)
017600960614     D  VIDRIT               236    236
017700980209     D* DATA TRASMISSIONE DAL (GG/MM/AAAA)
017800980209     D  VIDDTD               237    241P 0
017900980209     D* DATA TRASMISSIONE AL (GG/MM/AAAA)
018000980209     D  VIDDTA               242    246P 0
018100070302     D* cap destinatario
018200070302     D  VIDCAD               247    255
018300930610     D*------------------
018400950113     D* ARCHITETTURA
018500930610     D*------------------
018600930610     D KPJBA         E DS
018700121221     d
018800121221     D trul01ds      E DS
018900130115     d
019000130115     D dsemail       E DS
019100020419
019200020419     d wa              s              2  0
019300020607     d wpdc            s              3  0
019400020607     d wpopdc          s              3
019500121220     d Indx            s              3  0
019600020419
019700021203     D kpdpdr          S                   LIKE(APDpdr)
019800021203     D kpdtip          S                   LIKE(APDtip)
019900070302     D varcap          S                   LIKE(vidcad)
020000070302     D rovdtd          S              8  0
020100070302     D rovdta          S              8  0
020200070302     D rovdr1          S              8  0
020300070302     D rovdr2          S              8  0
020400070302     D AAA             S              1
020500070302     D AAB             S              1
020600070302     D AAD             S              1
020700070302     D wfor1           S              1
020800121220     D kcod            S                   LIKE(tbecod) inz('KPV')
020900121220     D kke1            S                   LIKE(tbeke1)
021000121220     D kke2            S                   LIKE(tbeke2)
021100121220     D savragr         S                   LIKE(v5cragr)
021200130115     d*---------------------------------------------------------------*
021300130115      /copy gaitrasrc/srcprotopr,xemail
021400930610     C*---------------------------------------------------------------*
021500950113     C* MAIN LINE                                                     *
021600930610     C*---------------------------------------------------------------*
021700930610     C     FMTD1         TAG
021800940331     C*
021900940331     C* EMISSIONE FORMATO VIDEO
022000950301     C                   CLEAR                   PARAM
022100951018     C                   CLEAR                   COMCLI
022200950301     C                   MOVEL     'D'           VIDELA                         'D'=PER DATA
022300950302     C                   MOVEL     'D'           VIDTOT                         'D'=IN DETTAGLIO
022400940331     C                   MOVEL     'S'           VIDACL
022500940331     C                   MOVEL     'P'           VIDBPA
022600940331     C                   MOVEL     'S'           VIDSPC
022700940527     C                   MOVEL     ' '           VIDBDS
022800940331     C                   Z-ADD     12            VIDGIO
022900960614     C                   MOVEL     'S'           VIDRIT
023000940331     C                   MOVEL     'S'           VIDVRA
023100050621     C                   MOVEL     'N'           VIDdpv
023200050622     C                   MOVEL     'N'           VIDdvv
023300050622     C                   MOVEL     'MA'          VIDMAG
023400940331     C                   MOVEL     'R'           VIDSRA
023500950301     C                   MOVEL     *BLANKS       DECTIP
023600121220     c                   clear                   v5cragr
023700121220     c                   clear                   v5ddesc1
023800121220     c                   clear                   v5ddesc2
023900950220     C                   SETOFF                                       0203
024000940331     C*
024100050621     c     fmtd11        tag
024200950220     C                   EXFMT     LRB6D01
024300930610     C*
024400950113     C* F03 - FINE LAVORO
024500930610     C   KC              GOTO      FINE
024600940331     C*
024700050621     c* Se immeddo SI a entrambe le richieste  errore
024800050622     c                   if        (viddpv='S' and vidvra='S')  or
024900050622     c                             (viddvv='S' and vidvra='S')
025000050621     c                   seton                                        40
025100050621     c                   goto      fmtd11
025200050621     c                   endif
025300050622     c*
025400050713     c                   setoff                                       0607
025500121220     c                   setoff                                       1415
025600050622     c                   if        viddpv='S' or viddvv='S'
025700050622     c                   seton                                        06
025800050713     c* se richeiste entrambe le differenze, impostabile solo NR
025900050713     c                   if        viddpv='S' and viddvv='S'
026000050713     c                   eval      vidmag='NR'
026100050713     c                   seton                                        07
026200050622     c                   endif
026300050713     c                   endif
026400121220     c
026500121220     c* se richiesti i pesi pemetto l'uso del raggruppamento codici
026600121220     c                   if        viddpv='S' and viddvv='N' and vidvra='N'
026700121220     c                   seton                                        14
026800121220     c                   endif
026900050622     c
027000940331     C* CARICAMENTO TABELLA L1 E L6  A SECONDA DEL FLAG RICHIESTO
027100940331     C                   EXSR      CARTAB
027200950301     C*
027300950301     C* SE LASCIATO VUOTO IL CAMPO, LO REIMPOSTO A 'N'
027400950302     C     VIDVRA        IFEQ      *BLANKS
027500950302     C                   MOVEL     'N'           VIDVRA
027600950301     C                   ENDIF
027700950302     C     VIDVRA        COMP      'S'                                    71     ON=CON AFFIDABILITA
027800950220     C*
027900950220     C* PER LE PARTENZE DEVO RIEMETTERE LA VIDEATA PER ALTRA DOMANDA
028000950220     C   02              EXFMT     LRB6D01
028100950220     C* F12 - RITORNO
028200950220     C   02
028300950220     CAN KL              GOTO      FMTD1
028400950303     C*
028500950303     C* SE RICHIESTO PER PADRONCINO, PROPONGO SIA CONS. CHE NON CONS.
028600950303     C     VIDELA        IFEQ      'P'
028700950303     C                   MOVEL     *BLANKS       VIDSPC
028800950303     C                   ENDIF
028900940331     C*
029000950220     C* IL SECONDO FORMATO VARIA SE RICHIESTO ARRIVI O PARTENZE
029100961022     C                   CLEAR                   VIDCLI
029200950328     C                   CLEAR                   VIDDCM
029300940331     C     FMTD2         TAG
029400950220     C   03              EXFMT     LRB6D02
029500950220     C   02VIDELA        IFEQ      'P'                                          'P'=PADRONCINO
029600950220     C                   EXFMT     LRB6D03                                      PER DATA
029700950220     C                   ELSE
029800950220     C                   EXFMT     LRB6D04                                      PER PADRONCINO
029900950220     C                   ENDIF
030000940331     C*
030100961021     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
030200961021     C                   CLEAR                   VIDMSG
030300961021     C                   SETOFF                                       285253
030400070515     C                   SETOFF                                       3982
030500020606     c                   eval      *in80 = *off
030600020607     c                   eval      *in81 = *off
030700961021     C*
030800950113     C* F03 - FINE LAVORO
030900940331     C   KC              GOTO      FINE
031000950113     C* F12 - RITORNO
031100940331     C   KL              GOTO      FMTD1
031200121220
031300121220     C* F09 - Richiesta raggruppamento codici
031400121220IF  1C     *INKI         IFEQ      *ON
031500121220     c                   exsr      wind05
031600121220     c                   goto      FMTD2
031700121220     c                   endif
031800930610     C*
031900930610     C* CONTROLLO DATI FORMATO VIDEO
032000930610     C                   EXSR      CONTR1
032100930610     C*
032200930610     C* ERRORI: RIEMETTO LA VIDEATA
032300940331     C   90              GOTO      FMTD2
032400930610     C*
032500950113     C* F06 - ELABORAZIONE
032600950113IF  1C     *INKF         IFEQ      *ON
032700950301     C  N01              MOVEL     SIMFEL        PARFEL
032800150610     C   01              MOVEL     dutpou        PARFEL
032900150610     C                   Z-ADD     dutkci        PARKCI                         SOTTOCONTO
033000940331     C*
033100940331     C* LINEE DI ARRIVO PER SIMFEL
033200940331     C   03
033300940331     CANN01VIDLNA        IFEQ      0
033400940331     C                   MOVEA     L1            LNA
033500931013     C                   ELSE
033600940331     C                   Z-ADD     VIDLNA        LNA(1)
033700931013     C                   ENDIF
033800940331     C* LINEE DI ARRIVO PER 2 LIVELLO
033900940331     C   03
034000940331     CAN 01VIDLNA        IFEQ      0
034100940331     C                   MOVEA     L6            LNA
034200940331     C                   ELSE
034300940331     C                   Z-ADD     VIDLNA        LNA(1)
034400940331     C                   ENDIF
034500940331     C*
034600940331     C* LINEE DI PARTENZA  PER SIMFEL
034700940331     C   02
034800940331     CANN01VIDLNP        IFEQ      0
034900940331     C                   MOVEA     L1            LNP
035000940331     C                   ELSE
035100940331     C                   Z-ADD     VIDLNP        LNP(1)
035200940331     C                   ENDIF
035300940331     C*
035400940331     C* LINEE DI PARTENZA  PER 2 LIVELLO
035500940331     C   02
035600940331     CAN 01              Z-ADD     VIDLNP        LNP(1)
035700930611     C*
035800961021     C                   MOVEL     VIDCLI        PARCLI
035900121220     c* e presenete raggruppamento lo indicao nell'apposito campo
036000121220     c                   if        v5cragr >*zeros
036100121220     c                   movel     v5cragr       PARCLI
036200121220     C                   EVAL      PARRAGR='R'
036300121220     C                   ENDIF
036400121220     C
036500050621     C* DIFFERENZE PESI VOLUMI
036600050623     c                   select
036700050623     C                   when      VIDDPV='S' and viddvv='S'
036800050621     C                   EVAL      PARVRA='D'
036900050623     C                   when      VIDDPV='S'
037000050623     C                   EVAL      PARVRA='P'
037100050623     C                   when      VIDDvV='S'
037200050623     C                   EVAL      PARVRA='V'
037300050623     C                   other
037400050621     C* ORE RITARDO S/N
037500050621     C                   EVAL      PARVRA=VIDVRA
037600050623     C                   ENDsl
037700050621     C
037800950113     C                   MOVEL     'LRB7'        KCOAZ
037900001219     C                   MOVEL     '3'           KRITB
038000020607     C                   CALL      'BCH09'
038100020607     C                   PARM                    KPJBA
038200130111     c
038300130111     C     KRITB         IFEQ      '2'
038400130111     C                   GOTO      FMTD2
038500130111     C                   ENDIF
038600130111     c
038700121221     c* Imposto l'indirizzo e-mail nel file di appoggio
038800130111     c                   clear                   parprg_kpjbu
038900130111     c
039000130111    1c                   if        videmail<>*blanks
039100121221     c                   clear                   trul01ds
039200130111     c                   eval      UL01OPZ='1'
039300130111     c                   eval      UL01azn = KCOAZ
039400130111     c                   movel(p)  videmail      UL01prm
039500130111     c                   eval      UL01err = *off
039600130111     c                   call      'TRUL01R'
039700130111     c                   parm                    TRUL01ds
039800130111if  2c                   if        UL01err = *on
039900130111     c                   seton                                        28  90
040000130111     C                   MOVEL     MSG(10)       VIDMSG
040100130111     C                   GOTO      FMTD2
040200130111e   2c                   endif
040300130111     c
040400130111     c                   z-add     UL01prg       parprg_kpjbu
040500130111    1c                   endif
040600130115     c
040700130115     C                   MOVEL     PARAM         KPJBU
040800001219      ***
040900001219      ***
041000130118     C                   CALL      'BCH10'
041100130118     c***                CALL      'FNLRB7R'
041200001218     C                   PARM                    KPJBA
041300930611E   1C                   ENDIF
041400930610     C*
041500930610     C* ENTER - RIMANE NELLA VIDEATA
041600930625IF  1C     *INKC         IFEQ      *OFF
041700950113     C     *INKF         ANDEQ     *OFF
041800940331     C                   GOTO      FMTD2
041900930610E   1C                   ENDIF
042000930610     C*
042100930610     C     FINE          TAG
042200930610     C                   SETON                                        LR
042300930610     C*---------------------------------------------------------------*
042400950113     C* CONTR1 - CONTROLLO DATI FORMATO VIDEO                         *
042500930610     C*---------------------------------------------------------------*
042600930610     C     CONTR1        BEGSR
042700930611     C*
042800930611     C                   SETOFF                                       9031
042900950301     C                   Z-ADD     *ZEROS        ROVDAL
043000950301     C                   Z-ADD     *ZEROS        ROVAL
043100020606     c                   clear                   rovdsd
043200020606     c                   clear                   rovdsa
043300020606     c                   clear                   rovdtd
043400020606     c                   clear                   rovdta
043500961021     C*
043600961022     C* LINEA PARTENZA CODICE CLIENTE
043700961021     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
043800961021    2C     VIDKLP        IFGT      0
043900961022     C     VIDKLP        CHAIN     AZORG01L                           33
044000961022    3C     *IN33         IFEQ      *ON
044100961022     C                   SETON                                        285290
044200961021     C                   MOVEL     MSG(1)        VIDMSG
044300961022     C                   GOTO      ENDCT1
044400961021    3C                   ENDIF
044500961021     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
044600961021    3C     VIDKSC        IFEQ      0
044700961022     C     VIDDCM        ANDEQ     *BLANKS
044800121220     c     v5cragr       andle     *zeros
044900961022     C                   SETON                                        285390
045000961022     C                   MOVEL     MSG(4)        VIDMSG
045100961022     C                   GOTO      ENDCT1
045200961021    3C                   ENDIF
045300961021     C*
045400961021   X2C                   ELSE
045500961021    3C     VIDKSC        IFGT      0
045600961022     C                   SETON                                        285290
045700961021     C                   MOVEL     MSG(1)        VIDMSG
045800961022     C                   GOTO      ENDCT1
045900961021    3C                   ENDIF
046000961021    2C                   ENDIF
046100121220     c
046200121220     c* Se immesso raggruppaoemnto non immettere codice e viceversa
046300121220     c                   if        vidksc>0  and v5cragr>*zeros
046400121220     C                   SETON                                        285390
046500121220     C                   MOVEL     MSG(3)        VIDMSG
046600121220     C                   GOTO      ENDCT1
046700121220    3C                   ENDIF
046800130115     c* se richiesto il file excel, immettere o codice cliente e raggruppamento
046900130114     c                   if        videxcel='S' and vidksc<=0 and
047000130114     c                             (v5cragr=*zeros  or v5cragr=*blanks)
047100130114     C                   SETON                                        285390
047200130114     C                   MOVEL     MSG(11)       VIDMSG
047300130114     C                   GOTO      ENDCT1
047400130114    3C                   ENDIF
047500961021     C*
047600950328     C* RICERCA ALFABETICA
047700961021     C     VIDKSC        IFEQ      *ZEROS
047800950328     C     VIDDCM        ANDNE     *BLANKS
047900950328     C                   EXSR      RICALF
048000950328     C                   GOTO      ENDCT1
048100950328     C                   END
048200940331     C*
048300070515     C* RICHIEDERE STAMPA PER AFFIDABILITA' CLIENTI O CONSEGNE o partenza
048400940331IF  1C     VIDACL        IFEQ      *BLANKS
048500940331     C     VIDACO        ANDEQ     *BLANKS
048600070515     C     VIDACP        ANDEQ     *BLANKS
048700940331     C                   SETON                                        5590
048800940331     C                   GOTO      ENDCT1
048900940331E   1C                   ENDIF
049000940331     C*
049100940331     C* IMPOSSIBILE SELEZIONARE ENTRAMBE LE STAMPE
049200940331IF  1C     VIDACL        IFEQ      'S'
049300940331     C     VIDACO        ANDEQ     'S'
049400070515IF  1C     VIDACL        oreq      'S'
049500070515     C     VIDACP        ANDEQ     'S'
049600070515IF  1C     VIDACO        oreq      'S'
049700070515     C     VIDACP        ANDEQ     'S'
049800940331     C                   SETON                                        5690
049900940331     C                   GOTO      ENDCT1
050000940331E   1C                   ENDIF
050100070515     c* Affidabilità partenza selezionabile soltanto per bolle partenza
050200070515     c                   if        vidacp='S' and vidbpa='A'
050300070515     c                   eval      vidmsg=msg(8)
050400070515     C                   SETON                                        829028
050500070515     C                   GOTO      ENDCT1
050600070515E   1C                   ENDIF
050700930611     C*
050800940331     C* D A T E   C O N S E G N A
050900950113     C*
051000940331     C* SPEDIZIONI NON CONSEGNATE: DATA DAL E AL VUOTE
051100930611IF  1C     VIDSPC        IFEQ      'N'
051200950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
051300930611IF  2C     VIDDAL        IFNE      *ZEROS
051400930611     C     VIDAL         ORNE      *ZEROS
051500930611     C                   SETON                                        4090
051600930611     C                   GOTO      ENDCT1
051700930611E   2C                   ENDIF
051800940331E   1C                   ENDIF
051900930611     C*
052000930611     C* SPEDIZIONI CONSEGNATE: ALMENO DATA DAL PIENA
052100931013     C* SE CHIESTE TUTTE SI PUO' IMMETTERE O NON IMMETTERE
052200020212      * Se richiesto bolle arrivo
052300020212     c                   if        vidbpa = 'A'
052400931013IF  1C     VIDSPC        IFEQ      'S'
052500950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
052600940331     C     VIDDAL        ANDEQ     *ZEROS
052700980210IF 1AC     *IN05         IFEQ      *OFF
052800980210     C                   SETON                                        4190
052900980210     C                   GOTO      ENDCT1
053000980210  X1AC                   ELSE
053100980210IF 1BC     VIDDTD        IFEQ      *ZEROS
053200980210     C                   SETON                                        7990
053300980210     C                   GOTO      ENDCT1
053400980210E  1BC                   ENDIF
053500980210E  1AC                   ENDIF
053600930611E   1C                   ENDIF
053700020212     c                   endif
053800020212
053900020212      * se richieste bolle in partenza
054000020212     c                   if        vidbpa = 'P' and vidspc = 'S' and
054100020212     c                             videla <> 'P' and viddal = *zeros
054200020212     c                   eval      *in41 = *on
054300020212     c                   eval      *in90 = *on
054400020212     c                   goto      endct1
054500020212     c                   endif
054600020212
054700940331     C*
054800940331     C* SE CHIESTE TUTTE O DATA CONSEGNA O DATA SPEDIZIONE DA IMMETTERE
054900940331IF  1C     VIDSPC        IFEQ      ' '
055000950303     C     VIDELA        ANDNE     'P'                                          'P'=PADRONCINO
055100940331     C     VIDDAL        ANDEQ     0
055200940331     C     VIDDSD        ANDEQ     0
055300980209     C     VIDDTD        ANDEQ     *ZEROS
055400980209     C                   SETON                                        6390
055500940331     C                   GOTO      ENDCT1
055600940331E   1C                   ENDIF
055700940527     C*
055800940601     C* SE SELEZIONATE LE SPEDIZIONI CONSEGNATE IL CAMPO DELLA
055900940601     C*   RICHIESTA DELLE BOLLE IN DISTINTA DEVE ESSERE BLANK
056000940527IF  1C   03VIDSPC        IFEQ      'S'
056100940601     C     VIDBDS        ANDNE     ' '
056200940527     C                   SETON                                        6490
056300940527     C                   GOTO      ENDCT1
056400940527E   1C                   ENDIF
056500930611     C*
056600930611     C* DATA AL SOLO SE IMMESSA DATA DAL
056700930611IF  1C     VIDAL         IFNE      *ZEROS
056800930611     C     VIDDAL        ANDEQ     *ZEROS
056900930611     C                   SETON                                        4290
057000930611     C                   GOTO      ENDCT1
057100930611E   1C                   ENDIF
057200940331     C*
057300940331     C* CONTROLLO FORMALE DATA DAL
057400940331IF  1C     VIDDAL        IFNE      *ZEROS
057500940331     C                   Z-ADD     VIDDAL        G02DAT
057600940331     C                   Z-ADD     *ZEROS        G02INV
057700940331     C                   MOVEL     *BLANKS       G02ERR
057800950113     C                   CALL      'XSRDA8'
057900940331     C                   PARM                    WLBDAT
058000950113     C                   Z-ADD     G02DAT        VIDDAL
058100940331     C* DATA ERRATA
058200940331IF  2C     G02ERR        IFEQ      '1'
058300940331     C                   SETON                                        4390
058400940331     C                   GOTO      ENDCT1
058500940331X   2C                   ELSE
058600950301     C                   Z-ADD     G02INV        ROVDAL
058700940331E   2C                   ENDIF
058800940331E   1C                   ENDIF
058900940331     C*
059000940331     C* CONTROLLO FORMALE DATA AL
059100940331IF  1C     VIDAL         IFNE      *ZEROS
059200940331     C                   Z-ADD     VIDAL         G02DAT
059300940331     C                   Z-ADD     *ZEROS        G02INV
059400940331     C                   MOVEL     *BLANKS       G02ERR
059500950113     C                   CALL      'XSRDA8'
059600940331     C                   PARM                    WLBDAT
059700950113     C                   Z-ADD     G02DAT        VIDAL
059800940331     C* DATA ERRATA
059900940331IF  2C     G02ERR        IFEQ      '1'
060000940331     C                   SETON                                        4490
060100940331     C                   GOTO      ENDCT1
060200940331X   2C                   ELSE
060300950301     C                   Z-ADD     G02INV        ROVAL
060400940331E   2C                   ENDIF
060500940331     C*
060600940331     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
060700950301    2C     ROVAL         IFLT      ROVDAL
060800940331     C                   SETON                                        4590
060900940331     C                   GOTO      ENDCT1
061000940331E   2C                   ENDIF
061100940331E   1C                   ENDIF
061200930719     C*
061300940331     C* D A T E   S P E D I Z I O N E
061400950113     C*
061500940331     C* DATA AL SOLO SE IMMESSA DATA DAL
061600940331IF  1C     VIDDSA        IFNE      *ZEROS
061700940331     C     VIDDSD        ANDEQ     *ZEROS
061800940331     C                   SETON                                        5890
061900940331     C                   GOTO      ENDCT1
062000940331E   1C                   ENDIF
062100940331     C*
062200940331     C* CONTROLLO FORMALE DATA DAL
062300940331IF  1C     VIDDSD        IFNE      *ZEROS
062400940331     C                   Z-ADD     VIDDSD        G02DAT
062500940331     C                   Z-ADD     *ZEROS        G02INV
062600940331     C                   MOVEL     *BLANKS       G02ERR
062700950113     C                   CALL      'XSRDA8'
062800940331     C                   PARM                    WLBDAT
062900950113     C                   Z-ADD     G02DAT        VIDDSD
063000940331     C* DATA ERRATA
063100940331IF  2C     G02ERR        IFEQ      '1'
063200940331     C                   SETON                                        5990
063300940331     C                   GOTO      ENDCT1
063400940331X   2C                   ELSE
063500950113     C                   Z-ADD     G02INV        ROVDSD
063600940331E   2C                   ENDIF
063700940331E   1C                   ENDIF
063800940331     C*
063900940331     C* CONTROLLO FORMALE DATA AL
064000940331IF  1C     VIDDSA        IFNE      *ZEROS
064100940331     C                   Z-ADD     VIDDSA        G02DAT
064200940331     C                   Z-ADD     *ZEROS        G02INV
064300940331     C                   MOVEL     *BLANKS       G02ERR
064400950113     C                   CALL      'XSRDA8'
064500940331     C                   PARM                    WLBDAT
064600950113     C                   Z-ADD     G02DAT        VIDDSA
064700940331     C* DATA ERRATA
064800940331IF  2C     G02ERR        IFEQ      '1'
064900940331     C                   SETON                                        6090
065000940331     C                   GOTO      ENDCT1
065100940331X   2C                   ELSE
065200940331     C                   Z-ADD     G02INV        ROVDSA
065300940331E   2C                   ENDIF
065400940331     C*
065500940331     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
065600940331    2C     ROVDSA        IFLT      ROVDSD
065700940331     C                   SETON                                        6190
065800940331     C                   GOTO      ENDCT1
065900940331E   2C                   ENDIF
066000940331E   1C                   ENDIF
066100980209AG*  C* D A T E   T R A S M I S S I O N E
066200980209     C*
066300980209     C* DATA AL SOLO SE IMMESSA DATA DAL
066400980209IF  1C     VIDDTA        IFNE      *ZEROS
066500980209     C     VIDDTD        ANDEQ     *ZEROS
066600980209     C                   SETON                                        7490
066700980209     C                   GOTO      ENDCT1
066800980209E   1C                   ENDIF
066900980209     C*
067000980209     C* CONTROLLO FORMALE DATA DAL
067100980209IF  1C     VIDDTD        IFNE      *ZEROS
067200980209     C                   Z-ADD     VIDDTD        G02DAT
067300980209     C                   Z-ADD     *ZEROS        G02INV
067400980209     C                   MOVEL     *BLANKS       G02ERR
067500980209     C                   CALL      'XSRDA8'
067600980209     C                   PARM                    WLBDAT
067700980209     C                   Z-ADD     G02DAT        VIDDTD
067800980209     C* DATA ERRATA
067900980209IF  2C     G02ERR        IFEQ      '1'
068000980209     C                   SETON                                        7390
068100980209     C                   GOTO      ENDCT1
068200980209X   2C                   ELSE
068300980209     C                   Z-ADD     G02INV        ROVDTD
068400980209E   2C                   ENDIF
068500980209E   1C                   ENDIF
068600980209     C*
068700980209     C* CONTROLLO FORMALE DATA AL
068800980209IF  1C     VIDDTA        IFNE      *ZEROS
068900980209     C                   Z-ADD     VIDDTA        G02DAT
069000980209     C                   Z-ADD     *ZEROS        G02INV
069100980209     C                   MOVEL     *BLANKS       G02ERR
069200980209     C                   CALL      'XSRDA8'
069300980209     C                   PARM                    WLBDAT
069400980209     C                   Z-ADD     G02DAT        VIDDTA
069500980209     C* DATA ERRATA
069600980209IF  2C     G02ERR        IFEQ      '1'
069700980209     C                   SETON                                        7590
069800980209     C                   GOTO      ENDCT1
069900980209X   2C                   ELSE
070000980209     C                   Z-ADD     G02INV        ROVDTA
070100980209E   2C                   ENDIF
070200980209     C*
070300980209     C* SE IMMESSA DATA AL MAGGIORE O UGUALE ALLA DATA AL
070400980209    2C     ROVDTA        IFLT      ROVDTD
070500980209     C                   SETON                                        7690
070600980209     C                   GOTO      ENDCT1
070700980209E   2C                   ENDIF
070800980209E   1C                   ENDIF
070900930611     C*
071000930611     C* P A R T E N Z A:   1° LIVELLO   SPEDIZIONI DI FILIALI IN L1
071100930611     C*                    2° LIVELLO   SPEDIZIONI DELLA FILIALE
071200930611     C*
071300940331IF  1C     VIDLNP        IFNE      *ZEROS
071400940331     C*** BOLLE IN PARTENZA
071500940331    2C     *IN02         IFEQ      *ON
071600120621     c* simfel può elaborare anche linea di partenza non proprio ma cod cliente
071700150611     c*  a cui è abilitato
071800120621     c                   if        not *in01
071900940331     C*
072000150611     C**   VIDLNP        LOOKUP    L1                                     31
072100150611     c                   movel     vidlnp        lnpalfa           3
072200150611     C     lnpalfa       LOOKUP    fig                                    31
072300120621     c                   if        not *in31 and vidklp>0
072400150610     C**   VIDklp        LOOKUP    L1                                     31
072500150610     c* Verifico le abilitazioni utente
072600150610     c                   clear                   tntaa1ds
072700150610     c                   eval      ITAA1tla = 'L'
072800150610     c                   eval      itaa1caut='§utegtc'
072900150610     c                   eval      itaa1ksc=vidcli
073000150610     c                   clear                   kpjbu
073100150610     c                   movel     tntaa1ds      kpjbu
073200150610     c                   call      'TNTAA1C'
073300150610     c                   parm                    kpjba
073400150610     c                   movel     kpjbu         tntaa1ds
073500150610     c
073600150611    1c                   if        otaa1fabi<>'N'
073700150610     c                   seton                                        31
073800120621     c                   endif
073900150610     c                   endif
074000930611     C*
074100120621     C* FILIALE NON IN GESTIONE
074200150617     c* In questo momento non vogliamo dare nessun blocco, per cui asterisco l'errore
074300150617     c* richeista di Nicola (ES)
074400150617IF  3C***  *IN31         IFEQ      *OFF
074500150617     C***                SETON                                          6290
074600150617     C***                GOTO      ENDCT1
074700150617E   3C***                ENDIF
074800120621E   3C                   ENDIF
074900940331     C*
075000940331   X2C                   ELSE
075100940331     C*** BOLLE IN ARRIVO
075200940331     C     VIDLNP        CHAIN     AZORG                              31
075300940331     C   31              SETON                                        4890
075400940331     C   31              GOTO      ENDCT1
075500940331    2C                   ENDIF
075600940331    1C                   ENDIF
075700930611     C*
075800930611     C* A R R I V O:   1° LIVELLO   SPEDIZIONI DI FILIALI IN L1
075900930611     C*                2° LIVELLO   SPEDIZIONI DI FILIALI IN L6
076000930611     C*
076100940331IF  1C     VIDLNA        IFNE      *ZEROS
076200940331     C*** BOLLE IN ARRIVO
076300940331    2C     *IN03         IFEQ      *ON
076400150610     C* secondo livello
076500930625IF  3C     *IN01         IFEQ      *ON
076600940331     C     VIDLNA        LOOKUP    L6                                     31
076700930611     C*
076800930611     C* FILIALE NON IN GESTIONE
076900150617IF  4C***  *IN31         IFEQ      *OFF
077000150617     C***                SETON                                          4990
077100150617     C***                GOTO      ENDCT1
077200150617E   4C***                ENDIF
077300940331     C* SIMFEL
077400930611X   3C                   ELSE
077500940331     C     VIDLNA        LOOKUP    L1                                     31
077600930611     C*
077700930611     C* FILIALE NON IN GESTIONE
077800150617IF  4C***  *IN31         IFEQ      *OFF
077900150617     C***                SETON                                          4990
078000150617     C***                GOTO      ENDCT1
078100150617E   4C***                ENDIF
078200930611E   3C                   ENDIF
078300980209AG*  C* CONTROLLO SE E' UNA LINEA ESTERA
078400980209     C     VIDLNA        CHAIN     AZORG                              31
078500980209IF  5C     *IN31         IFEQ      '0'
078600020212     c                   movel     orgde3        og143
078700020212     c                   if        §ogntw <> 'DPD' and §ogntw <> 'FED' and
078800020212     c                             §ogntw <> 'EEX' and §ogntw <> 'EUP'
078900980209AG*  C* CONTROLLO SE E' STATA INSERITA LA DATA TRASMISSIONE
079000020212     c                              and rovdtd <> 0
079100980209AG*  C                   SETON                                          7790
079200980209AG*  C                   GOTO      ENDCT1
079300020212     c                   endif
079400980209    5C                   ENDIF
079500980209     C*
079600940331   X2C                   ELSE
079700940331     C*** BOLLE IN PARTENZA
079800940331     C     VIDLNA        CHAIN     AZORG                              31
079900940331     C   31              SETON                                        4790
080000940331     C   31              GOTO      ENDCT1
080100930611E   2C                   ENDIF
080200980209     C*
080300980209   X1C                   ELSE
080400980209     C*SE LA LINEA DI ARRIVO E' A ZERO E LA DTA TRASMISSIONE E' VALORIZZATA E'
080500980209     C*ERRORE
080600980209AG*  C     VIDDTD        IFNE      *ZEROS
080700980209AG*  C                   SETON                                          7890
080800980209AG*  C                   GOTO      ENDCT1
080900980209AG*  C                   ENDIF
081000940331E   1C                   ENDIF
081100940331     C*
081200940331     C* C O D I C E   C L I E N T E
081300950113     C*
081400940331IF  1C     VIDCLI        IFNE      *ZEROS
081500940331     C                   MOVEL     VIDCLI        FILCLI            3 0
081600961016     C*
081700961016     C* RICERCA CLIENTE IN ANAGRAFICO
081800961016     C                   Z-ADD     VIDCLI        KSC
081900961016     C     KACO          CHAIN     CNACO00F                           32
082000961016     C* INESISTENTE
082100961016IF  2C     *IN32         IFEQ      *ON
082200961022     C                   SETON                                        285390
082300961022     C                   MOVEL     MSG(4)        VIDMSG
082400961016     C                   MOVEL     'S'           WFORZ             1
082500961016     C                   GOTO      ENDCT1
082600961016    2C                   ENDIF
082700961016     C*
082800961016     C* ESISTENTE MA ANNULLATO
082900961016IF  2C     ACOFLG        IFNE      *BLANKS
083000961022     C                   SETON                                        285390
083100961022     C                   MOVEL     MSG(5)        VIDMSG
083200961016     C                   MOVEL     'S'           WFORZ             1
083300961016     C                   GOTO      ENDCT1
083400961016E   2C                   ENDIF
083500961016     C                   MOVEL     ACORAG        VIDDCM
083600951018     C*
083700951018     C* WFORZ ---> CAMPO UTILIZZATO PER GESTIRE LA FORZATURA DEL CODICE
083800951018     C*   CLIENTE NEL CASO VENGANO RICHIESTE STAMPE RELATIVE A CLIENTI
083900951018     C*   DI ALTRA FILIALE
084000951018     C     VIDCLI        IFNE      COMCLI
084100951018     C                   CLEAR                   WFORZ
084200951018     C                   MOVEL     VIDCLI        COMCLI
084300951018     C                   ENDIF
084400951018     C*
084500951018   1AC     WFORZ         IFEQ      ' '
084600940331     C*** BOLLE IN PARTENZA
084700940331    2C     *IN02         IFEQ      *ON
084800150610     C* filiale secondo livello
084900940331    3C     *IN01         IFEQ      *ON
085000940331     C* SIMFEL CON LNP IMMESSA
085100940331    3C     *IN01         OREQ      *OFF
085200940331     C     VIDLNP        ANDGT     0
085300940331     C*
085400940331   X3C                   ELSE
085500940331     C*
085600940331     C* SIMFEL SENZA LNP IMMESSA
085700150611     C***  FILCLI        LOOKUP    L1                                     31
085800940331     C*
085900940331     C* FILIALE NON IN GESTIONE
086000150611IF  4C***  *IN31         IFEQ      *OFF
086100150611     C***                SETON                                        522890
086200150611     C***                MOVEL     MSG(2)        VIDMSG
086300150611     C***                MOVEL     'S'           WFORZ             1
086400150611     C***                GOTO      ENDCT1
086500150611E   4C***                ENDIF
086600940331E   3C                   ENDIF
086700940331     C*
086800940331   X2C                   ELSE
086900940331     C*
087000940331     C*** BOLLE IN ARRIVO
087100940331    3C     VIDLNA        IFGT      0
087200150617IF  3C***  FILCLI        IFNE      VIDLNA
087300150617     C***                SETON                                        522890
087400150617     C***                MOVEL     MSG(3)        VIDMSG
087500150617     C***                MOVEL     'S'           WFORZ             1
087600150617     C***                GOTO      ENDCT1
087700150617E   4C***                ENDIF
087800940331     C*
087900940331   X3C                   ELSE
088000930611     C* REMOTO
088100940331     C   01FILCLI        LOOKUP    L6                                     31
088200940331     C* SIMFEL
088300150611     C**N01FILCLI        LOOKUP    L1                                     31
088400930611     C*
088500930611     C* FILIALE NON IN GESTIONE
088600150611IF  4C***  *IN31         IFEQ      *OFF
088700150611     C***                SETON                                        522890
088800150611     C***                MOVEL     MSG(2)        VIDMSG
088900150611     C***                MOVEL     'S'           WFORZ             1
089000150611     C***                GOTO      ENDCT1
089100150611E   4C***                ENDIF
089200930611E   3C                   ENDIF
089300930611E   2C                   ENDIF
089400930611     C*
089500951018   1AC                   ENDIF
089600951018     C*
089700930611E   1C                   ENDIF
089800930611     C*
089900931014     C* ORE DI RITARDO: DA INSERIRE SONO SE RICHIESTO "R"
090000931014     C     VIDSRA        IFNE      'R'
090100931014     C     VIDGIO        ANDGT     0
090200931014     C                   SETON                                        5790
090300931014     C                   GOTO      ENDCT1
090400931014     C                   ENDIF
090500931014     C* ORE DI RITARDO: INSERIRE SOLO MULTIPLI DI 12 ORE
090600930719IF  1C     VIDGIO        IFNE      *ZEROS
090700931014IF  1C     VIDGIO        ANDNE     999
090800930719     C     VIDGIO        DIV       12            QUOZ              3 0
090900930719     C                   MVR                     REST              3 0
091000930719IF  2C     REST          IFNE      *ZEROS
091100930611     C                   SETON                                        9054
091200930611     C                   GOTO      ENDCT1
091300930719E   2C                   ENDIF
091400930719E   1C                   ENDIF
091500950113     C*
091600950113     C* TIPO SERVIZIO
091700950113IF  1C     VIDTIP        IFNE      *BLANKS
091800950113IF  2C     VIDTIP        IFEQ      '?'
091900950113     C                   MOVEL     CODUT         §KUT
092000950113     C                   MOVEL     '5E'          §COD
092100950113     C                   MOVEL     *BLANKS       VIDTIP
092200950113     C                   MOVEL     *BLANKS       §KEY
092300950113     C                   CALL      'X§TABER'
092400950113     C                   PARM                    §KUT
092500950113     C                   PARM                    §COD
092600950113     C                   PARM                    §KEY
092700950113     C                   PARM                    §DES             30
092800950113     C                   MOVEL     §KEY          VIDTIP
092900950301     C                   MOVEL     §DES          DECTIP
093000950113     C                   SETON                                        90
093100950113     C                   GOTO      ENDCT1
093200950113E   2C                   ENDIF
093300950113     C*
093400950301     C                   MOVEL     *BLANKS       DECTIP
093500950113     C                   MOVEL     '5E'          COD
093600950113     C                   MOVEL     *BLANKS       KEY
093700950113     C                   MOVEL     VIDTIP        KEY
093800950113IF  2C     KTAB          CHAIN     TABEL00F                           90
093900950113     C     *IN90         IFEQ      *ON
094000950113     C                   SETON                                        66
094100950113     C                   GOTO      ENDCT1
094200950113E   2C                   ENDIF
094300950301     C                   MOVEL     TBLUNI        DECTIP
094400950113E   1C                   ENDIF
094500070302     c
094600070302     c* Nazione e cap destinatario
094700070302    1c                   if        vidcad<>*blanks or vidnzd<>*blanks
094800070302     c                   clear                   dssi95
094900070302     c                   clear                   dslv13
095000070302     C                   MOVEL     '3'           I95TCN
095100070302     C                   MOVEL     vidcad        I95CAP
095200070302     C                   MOVEL     vidnzd        I95NAR
095300070302     C                   MOVEL     *date         I95DAT
095400070302c
095500070302    2C     vidCAd        IFNE      VARcap
095600070302     C                   CLEAR                   wfor1
095700070302     C                   CLEAR                   AAA
095800070302     C                   CLEAR                   AAB
095900070302     C                   CLEAR                   AAD
096000070302     C                   MOVEL     vidcad        VARCAp
096100070302    2C                   ENDIF
096200070302     C**
096300070302     C                   MOVEL     wfor1         E13FZV
096400070302     C                   MOVEL     AAA           E13FZ1
096500070302     C                   MOVEL     AAB           E13FZ2
096600070302     C                   MOVEL     AAD           E13FZ3
096700070302     C**
096800070302     C** CALL
096900070302     C                   CALL      'FNLV13R'
097000070302     C                   PARM                    KPJBA
097100070302     C                   PARM                    DSLV13
097200070302     C                   PARM                    DSSI95
097300070302     C*
097400070302     C                   MOVEL     O13ERR        WERR              1
097500070302     C                   MOVEL     O13MSG        vidMSG
097600070302     C**
097700070302     C* REIMPOSTO LE FORZATURE
097800070302     C                   MOVEL     E13FZV        wfor1
097900070302     C                   MOVEL     E13FZ1        AAA
098000070302     C                   MOVEL     E13FZ2        AAB
098100070302     C                   MOVEL     E13FZ3        AAD
098200070302     C**
098300070302    2C     WERR          IFNE      ' '
098400070302    3C     vidMSG        ifne      *BLANKS
098500070302     C                   SETON                                        28
098600070302    3c                   endif
098700070302     c                   seton                                        9039
098800070302     c                   goto      endct1
098900070302    2c                   endif
099000070302    1c                   endif
099100950301     C*
099200950301     C*----------------------------------------
099300950301     C* CODICE PADRONCINO DAL-AL
099400950301     C*----------------------------------------
099500950303     C* PADRONCINO OBBLIGATORIO SOLO SE RICHIESTA TALE ELABORAZIONE
099600950303     C     VIDBPA        IFEQ      'P'                                          'P'=PARTENZA
099700950303     C     VIDELA        ANDEQ     'P'                                          'P'=PADRONCINO
099800950303     C     VIDPD1        IFEQ      *ZERO
099900950303     C                   SETON                                        72  90
100000950303     C                   GOTO      ENDCT1
100100950303     C                   ENDIF
100200950303     C                   ENDIF
100300950301     C* CODICE INIZIALE <= A QUELLO FINALE
100400950301     C     VIDPD1        IFGT      VIDPD2
100500950301     C     VIDPD2        ANDGT     *ZERO
100600950301     C                   SETON                                        67  90
100700950301     C                   GOTO      ENDCT1
100800950301     C                   ENDIF
100900950301     C* SE CODICI UGUALI, PULISCO IL SECONDO
101000950301     C     VIDPD1        IFEQ      VIDPD2
101100950301     C                   CLEAR                   VIDPD2
101200950301     C                   ENDIF
101300020607
101400020607      * se inserito il solo codice "dal" deve:
101500020607 b1  c                   if        vidpd1 <> *zeros and vidpd2 = *zeros
101600020607      * esistere in anagrafica
101700021203     c*    vidpd1        chain     fnapd01l
101800021203 b2  c*                  if        not %found(fnapd01l)
101900021203     C                   movel     vidpd1        kpdpdr
102000021203     C                   move      'A'           kpdtip
102100021203     C     kfapd         chain     fiapd01L
102200021203 b2  c                   if        not %found(fiapd01l)
102300020607     c                   eval      *in80 = *on
102400020607     c                   eval      *in28 = *on
102500020607     c                   eval      *in90 = *on
102600020607     c                   eval      vidmsg = msg(6)
102700020607     c                   goto      endct1
102800020607 e2  c                   endif
102900020607 e1  c                   endif
103000020607      * se inserito il solo codice "dal" per lancio fatto dal primo livello
103100020607 b2  c                   if        vidpd1 <> *zeros and vidpd2 = *zeros
103200020607     c                             and not *in01
103300020607      * deve essere gestito dal p.o.
103400020607     c                   movel     vidpd1        wpdc
103500020607     c     wpdc          lookup    l1                                     31
103600020607 b3  c                   if        not *in31
103700020606     c                   eval      *in80 = *on
103800020606     c                   eval      *in28 = *on
103900020606     c                   eval      *in90 = *on
104000020606     c                   eval      vidmsg = msg(7)
104100020606     c                   goto      endct1
104200020607 e3  c                   endif
104300020607 e2  c                   endif
104400020607      * se inserita linea di partenza
104500020607 b2  c                   if        vidlnp <> *zeros and
104600020607      * inserito un range il codice deve essere gestito dalla linea di partenza
104700020607     c                             ((vidpd1 <> 0 and vidpd2 <> 0)  or
104800020607      * inserito solo il codice "dal" deve essere gestito dalla linea di partenza
104900020607     c                             (vidpd1 <> 0 and vidpd2 = 0))
105000020607     c                   movel     vidpd1        wpdc
105100020607 b3  c                   if        wpdc <> vidlnp
105200020607     c                   eval      *in80 = *on
105300020607     c                   eval      *in28 = *on
105400020607     c                   eval      *in90 = *on
105500020607     c                   eval      vidmsg = msg(8)
105600020607     c                   goto      endct1
105700020607 e3  c                   endif
105800020607 b3  c                   if        vidpd2 <> *zeros
105900020607     c                   movel     vidpd2        wpdc
106000020607 b4  c                   if        wpdc <> vidlnp
106100020607     c                   eval      *in81 = *on
106200020607     c                   eval      *in28 = *on
106300020607     c                   eval      *in90 = *on
106400020607     c                   eval      vidmsg = msg(8)
106500020607     c                   goto      endct1
106600020607 e4  c                   endif
106700020607 e3  c                   endif
106800020607 e2  c                   endif
106900950301     C*----------------------------------------
107000950301     C* (PER PADRONCINO) DATA RITIRO MERCE
107100950301     C*----------------------------------------
107200950301     C                   Z-ADD     *ZERO         ROVDR1
107300950301     C                   Z-ADD     *ZERO         ROVDR2
107400950301     C     VIDDR1        IFNE      *ZERO
107500950301     C                   Z-ADD     VIDDR1        G02DAT
107600950301     C                   Z-ADD     *ZEROS        G02INV
107700950301     C                   MOVEL     *BLANKS       G02ERR
107800950301     C                   CALL      'XSRDA8'
107900950301     C                   PARM                    WLBDAT
108000950301     C* DATA ERRATA
108100950301IF  2C     G02ERR        IFEQ      '1'
108200950301     C                   SETON                                        68  90
108300950301     C                   GOTO      ENDCT1
108400950301     C                   ENDIF
108500950301     C                   Z-ADD     G02DAT        VIDDR1
108600950301     C                   Z-ADD     G02INV        ROVDR1
108700950301     C                   ENDIF
108800950301     C*
108900950301     C     VIDDR2        IFNE      *ZERO
109000950301     C                   Z-ADD     VIDDR2        G02DAT
109100950301     C                   Z-ADD     *ZEROS        G02INV
109200950301     C                   MOVEL     *BLANKS       G02ERR
109300950301     C                   CALL      'XSRDA8'
109400950301     C                   PARM                    WLBDAT
109500950301     C* DATA ERRATA
109600950301IF  2C     G02ERR        IFEQ      '1'
109700950301     C                   SETON                                        69  90
109800950301     C                   GOTO      ENDCT1
109900950301     C                   ENDIF
110000950301     C                   Z-ADD     G02DAT        VIDDR2
110100950301     C                   Z-ADD     G02INV        ROVDR2
110200950301     C                   ENDIF
110300950301     C* DATA INIZIALE <= DATA FINALE
110400950301     C     ROVDR1        IFGT      ROVDR2
110500950301     C     ROVDR2        ANDGT     *ZERO
110600950301     C                   SETON                                        70  90
110700950301     C                   GOTO      ENDCT1
110800950301     C                   ENDIF
110900950301     C* SE DATA FINALE = DATA INIZIALE, PULISCO LA FINALE
111000950301     C     ROVDR2        IFEQ      ROVDR1
111100950301     C                   Z-ADD     *ZERO         VIDDR2
111200950301     C                   Z-ADD     *ZERO         ROVDR2
111300950301     C                   ENDIF
111400130115     c* se immesso ind e-mail immettere S in file escel e viceversa
111500130115     c                   if        (videxcel='S' and videmail=*blanks) or
111600130115     c                             (videxcel=' ' and videmail<>*blanks)
111700130115     C                   SETON                                        285190
111800130115     C                   MOVEL     MSG(12)       VIDMSG
111900130115     C                   GOTO      ENDCT1
112000130115    3C                   ENDIF
112100130115
112200130115      /free
112300130115       // controllo formale indirizzo email
112400130115       if videmail<>*blanks   ;
112500130115       clear dsemail  ;
112600130115       §EMLINDI=%trim(videmail)+'@brt.it'   ;
112700130115       callp XEMAIL   (dsemail)  ;
112800130115         if §EMLERRO<>*blanks and §EMLERRO<> '0';
112900130115             vidmsg = §emlmsgo   ;
113000130115             *in90='1'   ;
113100130115             *in28='1'   ;
113200130115             *in51='1'   ;
113300130115             leavesr;
113400130115         endif   ;
113500130115       endif   ;
113600130115      /end-free
113700130115
113800950301     C*
113900930611     C     ENDCT1        TAG
114000930610     C                   ENDSR
114100930610     C*---------------------------------------------------------------*
114200121220     c* videata di richeista cod raggruppamenti
114300121220     C*---------------------------------------------------------------*
114400121220     c     Wind05        BEGSR
114500121220     c
114600121220     c                   setoff                                       15
114700121220     c                   clear                   savragr
114800121220
114900121220     c     for05         TAG
115000121220     c                   exfmt     lrb6w05
115100121220
115200121220     c                   clear                   tntb96ds
115300121220     c                   clear                   v5ddesc1
115400121220     c                   clear                   v5ddesc2
115500121220     c                   setoff                                       2860
115600121220     c
115700121220     c* Manutenzione tabella raggruppamenti
115800121220     c                   if        *inkh='1'
115900121220     c                   movel     tntb96ds      kpjbu
116000121220     c                   call      'TNTB96R'
116100121220     c                   parm                    kpjba
116200121220     c
116300121220     c                   goto      for05
116400121220     c                   endif
116500121220     c
116600121220     c* Controllo se immesso il codice raggruppamenti
116700121220     c* ricerca
116800121220     c                   eval      Indx=%scan('?': v5cragr)
116900121220     c                   if        indx>0
117000121220     c                   eval      b96fun='1'
117100150610     c                   eval      b96ke1=%editc(dutpou:'X')
117200121220     c
117300121220     c                   movel     tntb96ds      kpjbu
117400121220     c                   call      'TNTB96R'
117500121220     c                   parm                    kpjba
117600121220     c
117700121220     c                   movel     kpjbu         tntb96ds
117800121220     c                   if        b96ke2>0
117900121220     c                   eval      v5cragr=%editc(b96ke2:'X')
118000121220     c                   exsr      chainTNTBE
118100121220     c                   else
118200121220     c                   clear                   v5cragr
118300121220     c                   endif
118400121220     c
118500121220     c                   goto      for05
118600121220     c                   endif
118700121220     c
118800121220     c* Se immesso codice non immettere raggruppamento e viceversa
118900121221     c                   if        vidksc>0 and v5cragr<>*zeros
119000121221     c                             and v5cragr<>*blanks
119100121220     c                   seton                                        2860
119200121220     c                   eval      vd5msg=msg(3)
119300121220     c                   goto      for05
119400121220     c                   endif
119500121220     c
119600121220     c* controllo codice raggruppamento
119700121221     c                   if        v5cragr<>*zeros   and v5cragr<>*blanks
119800121220     c                   exsr      chainTNTBE
119900121221     c                   if        not %found(tntbe01l)  or tbeatb<>' '
120000121220     c                   seton                                        2860
120100121220     c                   eval      vd5msg=msg(9)
120200121220     c                   goto      for05
120300121220     c                   endif
120400121220     c                   endif
120500121220     c
120600121220     c                   if        v5cragr<>savragr
120700121220     c                   eval      savragr=v5cragr
120800121220     c                   goto      for05
120900121220     c                   endif
121000121220     c
121100121220     c
121200121220     c                   if        v5cragr>*zeros
121300121220     c                   seton                                        15
121400121220     c                   endif
121500121220     c
121600121220     c                   ENDSR
121700121220     C*---------------------------------------------------------------*
121800121220     c     chainTNTBE    BEGSR
121900121220     c
122000121220     c                   eval      kcod='KPV'
122100150610     c                   eval      kke1=%editc(dutpou:'X')
122200121220     c                   eval      kke2=v5cragr
122300121220     c
122400121220     c     ktbe          chain     tntbe01l
122500121220     c                   if        %found(tntbe01l)
122600121220     c                   eval      v5ddesc1=%subst(tbeuni:1:30)
122700121220     c                   eval      v5ddesc2=%subst(tbeuni:31:30)
122800121220     c                   endif
122900121220     c                   ENDSR
123000121220     C*---------------------------------------------------------------*
123100930610     C*                    CARTAB                                     *
123200930610     C* ROUTINE DI CARICAMENTO TABELLA L1 E L6                        *
123300930610     C*---------------------------------------------------------------*
123400930610     C     CARTAB        BEGSR
123500940331     C                   SETOFF                                           04
123600940331     C* 02 ON - BOLLE IN PARTENZA
123700940331     C* 03 ON - BOLLE IN ARRIVO
123800940331     C     VIDBPA        COMP      'P'                                    02
123900940331     C     VIDBPA        COMP      'A'                                    03
124000930610     C*
124100020419     C* Carico  L1
124200020419     c                   clear                   trul06ds
124300020419     c                   eval      d06cod = '£1'
124400020419     c                   movel     simfel        d06key
124500020419     c                   movel     trul06ds      kpjbu
124600020419     c                   call      'TRUL06R'
124700020419     c                   parm                    kpjba
124800020419     c                   movel     kpjbu         trul06ds
124900020419     c                   movea     lin           l1
125000020419
125100980209AG*  C* CONTROLLO LINEA DI ARRIVO ESTERO SE FLAG AZORG UGUALE AD "E"
125200020419 b2  c                   do        30            wa
125300020419     c                   if        l1(wa) = *zeros or *in05 = *on
125400020419     c                   leave
125500020419     c                   endif
125600020419     C     l1(wa)        CHAIN     AZORG01L                           34
125700020419IF  3C     *IN34         IFEQ      '0'
125800020212     c                   movel     orgde3        og143
125900020212     c                   if        §ogntw = 'DPD' or §ogntw = 'FED' or
126000020212     c                             §ogntw = 'EEX' or §ogntw = 'EUP'
126100980209     C                   SETON                                        05
126200020212     c                   endif
126300020419E   3C                   ENDIF
126400020419 e1  c                   enddo
126500930610     C*
126600930610     C* CARICO LA TABELLA L6 SOLO SE SONO II LIVELLO
126700150610    1c     dutlpo        ifeq      '2'
126800150610     c     dutlpo        oreq      *blanks
126900940331     C*
127000930610     C                   SETON                                        01
127100940331     C***
127200940331     C* CARICO L6 SOLO PER BOLLE IN ARRIVO
127300940331     C***
127400940331   1AC     *IN03         IFEQ      *ON
127500020419
127600020419     c                   clear                   trul06ds
127700020419     c                   eval      d06cod = '£6'
127800020513     c*                  movel     remfil        d06key
127900150610     c                   movel     dutpou        d06key
128000020419     c                   movel     trul06ds      kpjbu
128100020419     c                   call      'TRUL06R'
128200020419     c                   parm                    kpjba
128300020419     c                   movel     kpjbu         trul06ds
128400020419     c                   movea     lin           l6
128500020419
128600940331     C* SE BOLLE IN ARRIVO   PROPONGO LA LINEA NON PROTETTA (PER LA L6)
128700150610     C                   MOVEL     dutpou        VIDLNA
128800940331     C* 04 ON -L6 HA SOLTANTO UN ELEMENTO --> PROTEGGO LA LINEA DI ARR
128900940331     C     L6(2)         IFEQ      0
129000940331     C                   SETON                                        04
129100940331     C                   ENDIF
129200940331E  1AC                   ENDIF
129300940331     C*
129400940331     C* SE BOLLE IN PARTENZA PROPONGO LA LINEA PROTETTA
129500150610     C   02              MOVEL     dutpou        VIDLNP
129600940331E   1C                   ENDIF
129700930610     C*
129800930611     C                   SETOFF                                           31
129900930611     C*
130000930610     C                   ENDSR
130100950328     C*---------------------------------------------------------------*
130200950328     C*                    RICALF                                     *
130300950328     C* RICERCA ALFABETICA                                            *
130400950328     C*---------------------------------------------------------------*
130500950328     C     RICALF        BEGSR
130600950328     C* DESCRIZIONE AZIENDA
130700950328     C                   MOVEL     RSUT          PAXDUT           30
130800950328     C* PAXSTA=9 ESCLUDO ANNULLATI
130900950328     C                   Z-ADD     9             PAXSTA
131000150611     C                   CLEAR                   PAXDIT
131100150611     C                   Z-ADD     1             PAXNUM
131200950328     C* CAPOCONTO CLIENTI
131300150610     C                   Z-ADD     dutkci        PAXCCC
131400950328     C*
131500950328     C                   CLEAR                   ACOKSC
131600950328     C*
131700950328     C                   MOVEL     VIDDCM        PAXDMT
131800961022     C                   MOVEL     VIDKLP        PAXFLR
131900950328     C*
132000150611     C                   CALL      'XALFA3BR'
132100950328     C                   PARM                    PAXDUT
132200950328     C                   PARM                    CODUT
132300950328     C                   PARM                    PAXDMT
132400950328     C                   PARM                    PAXCCC
132500950328     C                   PARM                    PAXSTA            1 0
132600961016     C                   PARM                    PAXFLR           90
132700150611     C                   PARM                    PAXDIT            3
132800150611     C                   PARM                    PAXNUM            2 0
132900150611     C                   PARM                    PAXKCM           80
133000150611     C                   PARM                    PAXKSM          140
133100150611     C                   PARM                    PAXKDM           60
133200950328     C*
133300950328     C* NON TROVATO O PREMUTO CMD12
133400950328    2C     PAXSTA        IFGT      -1
133500950328     C* CODICE SELEZIONATO: IMPOSTO A VIDEO
133600150611     C                   MOVEl     paxksm        VIDCLI
133700950328     C                   MOVEL     PAXDMT        VIDDCM
133800950328    2C                   ENDIF
133900950328     C*
134000950328     C                   ENDSR
134100930610     C*---------------------------------------------------------------*
134200950113     C* *INZSR - OPERAZIONI INIZIALI                                  *
134300930610     C*---------------------------------------------------------------*
134400950113     C     *INZSR        BEGSR
134500930610     C*
134600930610     C     *ENTRY        PLIST
134700930610     C                   PARM                    KPJBA
134800950113     C* DEFINIZIONE CAMPI
134900950113     C     *LIKE         DEFINE    TBLKEY        §KEY
135000950113     C     *LIKE         DEFINE    TBLCOD        §COD
135100950113     C     *LIKE         DEFINE    TBLKUT        §KUT
135200950328     C     *LIKE         DEFINE    ACOKCC        PAXCCC
135300950328     C     *LIKE         DEFINE    ACORAG        PAXDMT
135400951018     C     *LIKE         DEFINE    VIDCLI        COMCLI
135500930610     C*
135600150610     c     *dtaara       define    §azute        azuteds
135700150610     c     *dtaara       define    §datiute      ddatiute
135800150610     c                   in(E)     *dtaara
135900150610     c                   If        %error  or RSUT = *blanks
136000150610     c                   Clear                   Tibs34ds
136100150611     c                   Call      'TIBS34R'
136200150610     c                   Parm                    Tibs34ds
136300150610     c                   In        *dtaara
136400150610     c                   EndIf
136500150610
136600150610       //?Recupero il flag di 'Modifica tariffe'
136700150610     c                   eval      dUTE01 = UTEfaf
136800150610
136900150610     c* Richiamo solo per avere codice di abilitazione
137000150610     c                   clear                   tntaa1ds
137100150610     c                   eval      ITAA1tla = 'L'
137200150611     c                   eval      itaa1caut='§utembf'
137300150610     c                   clear                   kpjbu
137400150610     c                   movel     tntaa1ds      kpjbu
137500150610     c                   call      'TNTAA1C'
137600150610     c                   parm                    kpjba
137700150610     c                   movel     kpjbu         tntaa1ds
137800150610     c
137900150610     c* Utente non abilitato alla gestione tariffe
138000150610     c                   if        otaa1fabi='N'
138100150610     c                   clear                   fig
138200150610e   2c                   else
138300150610      *
138400150610      * Reperimento dei P.O. gestibili dall'utente
138500150610     c                   clear                   TRUL31DS
138600150610     c                   eval      I31abi = otaa1cabi
138700150610     c                   eval      I31cdi = DUTdis
138800150610     c                   eval      I31car = DUTare
138900150610     c                   eval      I31cpo = DUTpou
139000150610     c                   call      'TRUL31R'
139100150610     c                   parm                    KPJBA
139200150610     c                   parm                    TRUL31DS
139300150610if  2c                   if        O31pog > *zeros
139400150610     c                   movea     O31pog        fig
139500150610x   2c                   endif
139600150610x   2c                   endif
139700930610     C*
139800930610     C* TABEL00F
139900930610     C     KTAB          KLIST
140000930610     C                   KFLD                    CODUT             1 0
140100930610     C                   KFLD                    COD               2
140200930610     C                   KFLD                    KEY               8
140300121220     C     KTbe          KLIST
140400121220     C                   KFLD                    kcod
140500121220     C                   KFLD                    kke1
140600121220     C                   KFLD                    kke2
140700930611     C*
140800930611     C* CNACO00F
140900930611     C     KACO          KLIST
141000930611     C                   KFLD                    CODUT
141100150610     C                   KFLD                    dutKCI
141200930611     C                   KFLD                    KSC               7 0
141300021203
141400021203     C     Kfapd         klist
141500021203     C                   kfld                    kpdtip
141600021203     C                   kfld                    kpdpdr
141700150611     c
141800150611     c                   z-add     1             codut
141900930611     C*
142000930610     C                   ENDSR
142100961022**  MSG
142200961022Immettere linea di partenza valida                                            1
142300961022Cliente non in gestione: Enter per forzare                                    2
142400121220Se immesso raggruppamento clienti NON immettere il codice cliente e viceversa 3
142500961022Cliente non presente in anagrafico                                            4
142600961022Codice cliente annullato                                                      5
142700020606Codice autotrasportatore inesistente                                          6
142800020606Autotrasportatore in gestione ad altro p.o.                                   7
142900070515Affidabilità PARTENZA richiedibile soltanto per bolle Partenza
143000121220Codice raggruppamento inesitente                                              9
143100130114Rilevati problemi nel lancio del programma in coda : uscire e riprovare!      10
143200130114Se richiesto file excel immettere il codice cliente o il raggruppamento       11
143300130115Se inserito indirizzo  e-mail, impostare la richiesta del file excel          12
