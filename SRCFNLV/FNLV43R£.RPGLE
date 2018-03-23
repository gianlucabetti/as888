000100020417     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200941216     H* FNLV43R *----------------------------------------------------*
000300940228     H*             STAMPA TESTI LETTERE         - FILTRO -          *
000400000000     H*--------------------------------------------------------------*
000500941216     FFNLV43D   CF   E             WORKSTN
000600941216     F                                     SFILE(LV43S03:NRR)
000700940224     FTABEL00F  IF   E           K DISK
000800051026     FTntbe01l  IF   E           K DISK
000900980127     FCNACO00F  IF   E           K DISK
001000961203     FCNIND00F  IF   E           K DISK
001100091021     fTivis05l  if   e           k disk    usropn
001200980127     FCNCLP00F  IF   E           K DISK
001300041029     FTFNTC01L  IF   E           K DISK
001400980127     FAZORG01L  IF   E           K DISK
001500060601     fazlin03l  if   e           k disk
001600130731       // -?Anagrafica Commerciali?
001700130731     fAZCMM01L  if   e           k disk
001800130802       // -?Note/Rubrica Commerciali?
001900131003     fAZNTC01L  if   e           k disk    prefix(AZ)
002000940224     D*
002100940224     D TES             S             37    DIM(1) CTDATA PERRCD(1)
002200050131     D TSL             S             50    DIM(3) CTDATA PERRCD(1)
002300060919     D MSG             S             78    DIM(15) CTDATA PERRCD(1)
002400020417     d l1              s              3  0 dim(30)
002500051107     D CMD1            S             15    DIM(1) CTDATA PERRCD(1)
002600020417
002700950905     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
002800940224     D* SCHIERE PER RICERCA CAPOCONTI
002900940224     D*
003000961016     D                 DS
003100961016     D  COM                    1     90  0
003200961016     D                                     DIM(30)
003300940311     D***
003400940311     D***
003500941216     D* RICHIAMO PGM GESTIONE OPZIONI STAMPA           - FNLV42R -
003600940224     D PARAM3          DS
003700940224     D  PA3TTX                 1      2
003800940311     D***
003900940311     D***
004000941216     D* RICHIAMO PGM GESTIONE TESTI LETTERE            - FNLV40R -
004100940228     D PARAM4          DS
004200940228     D  PA4FLG                 1      1
004300940228     D  PA4TTX                 2      3
004400060607     d  pa4lin                 4      5
004500940228     D  PA4CTX                 6      7
004600940311     D***
004700940311     D* PARAM PASSATA DA PGM RICHIAMANTI
004800940311     D PARAM7          DS
004900940311     D* NUMERO VISITA E PROGRESSIVO
005000020304     D  PA7NRV                 3      8  0
005100020304     D  PA7PRV                 9      9  0
005200940311     D* CODICE CLIENTE
005300940311     D  PA7CLI                10     16  0
005400940311     D* CODICE CLIENTE POTENZIALE
005500940311     D  PA7CPO                17     27  0
005600941219     D* TIPO STAMPA TARIFFA -->PER FARE LE GIUSTE OVRDBF  AL TNTA43R
005700941219     D  PA7TST                98     98
005800940311     D* AMBIENTE STAMPA TESTI ("V"=VISITE/"C"=CLIENTI/"P"=POTENZIALI)
005900091021      * NEW 'T'=TRATTATIVE
006000941219     D  PA7AST                99     99
006100941216     D* COD TARIFFA E PROGRESSIVO PER STAMPA TARIFFA  -TNTA43R
006200940311     D  PA7CTR               100    102  0
006300940311     D  PA7PRG               103    105  0
006400940311*****D* FLAG USATO PER SAPERE SE PGM RICHIAMATO
006500940311*N.B.D* ATTENZIONE: PRIMA DI UTILIZZARE QUESTO FLAG LEGGERE I COMMENTI
006600940311*****D*             SITUATI ALL'INIZIO DEL FLUSSO PRINCIPALE DEL PGM
006700940311     D  PA7FLG               106    106
006800940311     D* RAGIONE SOCIALE
006900940311     D  PA7RGS               107    152
007000940314     D***
007100041103     D DSEML           DS
007200041103     D  EMAILI                 1    100
007300041103     D  EMAILO               101    200
007400041103     D  EMAILE               201    201
007500041103     D  EMAILM               202    281
007600020417
007700020417      * DS PER TRUL06R (CARICAMENTO £1)
007800020417     D trul06ds      E DS
007900020417     D  LIN                    1     90  0 dim(30)
008000020417
008100941216     D*
008200931109     D WLBDAT          DS
008300941216     D  G02DAT                 1      8  0
008400941216     D  G02INV                 9     16  0
008500941216     D  G02ERR                17     17
008600941216     D  G02TGI                18     22  0
008700050907     D*
008800051026     d og143         e ds
008900940224     D DS1H          E DS
009000051110     D Dmra          E DS
009100940224     D DS1I          E DS
009200021024     D  SKI                   26     88
009300021024     D                                     DIM(21)
009400900517     D KPJBA         E DS
009500020513     D  PROF                   1      3
009501161223      // - flag di errore nel caso in cui non sia riuscita a stampare
009502161223      //   gli allegati condizioni generali e privacy.
009503161223      //   viene valorizzato dal pgm TNTA49R e TNTA50R
009504161223     d err_allegati          497    497
009600060919     d P_numcli              498    502
009700970714     D* DS PER TISI95R - CONTROLLO CAP
009800970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
009900970714     D* DS PER FNLV13R - CONTROLLO CAP
010000970714     D DSLV13        E DS                  EXTNAME(FNLV13DS)
010100130802     D DSLV14        E DS                  EXTNAME(FNLV14DS)
010200970324     D* Ds esterna x richiamo pgm controllo fax
010300970324     D TRUL42        E DS                  EXTNAME(TRUL42DS)
010400051026     c*
010500051026     D fnlv54ds      E DS
010600981112     D****
010700981112     D* Ds esterna x STAMPA TESTI LETTERA
010800021024     d FNLV41DS      E DS
010900981113     D DSLV45        E DS                  EXTNAME(FNLV45DS)
011000130731     d TRMK43ds      e ds                  inz
011100021127     D TIBS02DS      e ds                  inz
011200060919     D Dvpo          e ds                  inz
011300941230     D*
011400941230     D* DEFINIZIONE COSTANTI
011500941230     D*
011600941230     D CNAZ            C                   CONST('999=TUTTE')
011700950905     D CDSL            C                   CONST('(T/L/A)  ')
011800950907     D CDS2            C                   CONST('(S/N)    ')
011900050131     D CDSLF           C                   CONST('(T/L)    ')
012000021108      *
012100021108     d Trul31ds      e ds                  INZ
012200021108      *
012300021115     d POG             S              3    DIM(250)
012400021108
012500041021     d codut           s              1  0 Inz(1)
012600041021     d wabi            s                   like(UteAut)
012700041029     d kapl            s                   like(ntcapl)
012800041029     d knk1            s                   like(ntcnk1)
012900041029     d knk2            s                   like(ntcnk2) inz('    ')
013000050907     d ktnt            s                   like(ntctnt)
013100051103     d wntcrnt         s                   like(ntcrnt)
013101161223     d Werr_allegati   s              1    inz(' ')
013200091021
013300091021     d $trmk17         s               n   inz
013400091021     d trmk17ds      e ds
013500060601
013600060601     d p37iso2         s                   like(liniso2)
013700060601     d p37desi         s                   like(lindesita)
013800060601
013900041021     d Azuteds       e ds                  extname(Azute00f)
014000041021     d dDatiute      e ds
014100041021     d dute01        e ds
014200041021     d Tibs34ds      e ds
014300051025     D XLSTSPLDS     E DS
014400051025     D XLSTSPLDS2    E DS
014500091106
014600091106      * Ds esterna x richiesta indirizzo e-mail
014700091106     d FNLV44ds      e ds
014800091106
014900051025     D********
015000051025     D* SCHIERE SPOOL RITORNATI IN OUT
015100051025     D********
015200051103     D DSSPOOL         DS
015300051103     D  dsVARFIL                     10
015400051103     D  dsVARJNM                     10
015500051103     D  dsVARUSN                     10
015600051103     D  dsJOBNUM                      6
015700051103     D  dsSPLNUM                      6
015800051103     D  dsVARDTA                     10
015900051103     D  dsNUMRIG                      9  0
016000051103     D  dsNUMCOL                      9  0
016100051103     D  dsNUMCPI                      9  1
016200051103     D  dpOUTQNM                     10
016300051103     D  ds$ROWS                      17
016400051103     D  dsS1TOTP                      6  0
016500051103     D  dsS1STAT                      6
016600051103     D SKSPOOL         S                   like(DSSPOOL) dim(99)
016700051103     D wskSpoolid      S                   like(XLSTSPLDS2) dim(99)
016800051103     D skSpoolid       S                   like(DSSPOOLID) dim(99)
016900051025     D                                     inz(*blanks)
017000051103     D DSSPOOLID       DS
017100051103     D  Sp_Job_ID                          like(XLSTSPLDS2)
017200051103     D  Sp_Job_Codr                   3
017300130802
017400130802      *****************************************************************
017500091021
017600130802     i*//tivis000
017700130802     i*//              visesi                      wisesi
017800091021
017900931109     C*****************************************************************
018000931109     C* RIEPILOGO INDICATORI
018100931109     C*****************************************************************
018200940224     C* 01    - VISITE
018300940224     C* 02    - CLIENTI
018400940224     C* 03    - CLIENTI POTENZIALI
018500940330     C* 04    - LIBERO
018600940224     C* 05    - TIPO VISITA RIGUARDANTE UN CLIENTE NUOVO
018700940228     C* 07    - E' STATO INSERITO IL CODICE TESTO
018800961129     C* 08    - IMMESSO UNO SPECIFICO CLIENTE/CLI.POTENZ./NUM.VISITA
018900960624     C* 10    - RICHIESTA STAMPA RIDOTTA
019000960624     C* 11    - EMETTO LA RICHIESTA DELLA VISUALIZZAZIONE DELLE RIGHE
019100950829     C*         DI DETTAGLIO IN STAMPA
019200091021      * 12    - Trattativa
019300940301     C* 13    - PROGRAMMA RICHIAMATO
019400940311     C* 15    - PROVENGO DA MENU'
019500940311     C* 16    - PROVENGO DA GESTIONE     OFFERTA/TARIFFA
019600940311     C* 17    - PROVENGO DA CONVALIDA    OFFERTA
019700940311     C* 18    - PROVENGO DA ANNULLAMENTO OFFERTA/TARIFFA
019800050907     c* 19    - Selezionato testo html
019900961129     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
020000961129     C* 30/32 - DI COMODO
020100940224     C* 35    - USATO PER LOKUP E TESTN
020200981116     C* 37/83 - INIDICATORI DI ERRORE
020300961129     C* 75    - ON --> SFLCLR     OFF --> SFLDSP / SFLEND
020400961129     C* 76    - INIDICATORI DI ERRORE
020500931109     C* 90    - SEGNALA LA PRESENZA DI ERRORI
020600091022      * 91    - potenzile non trovato non richiesta new attività
020601161223      * 92    - non sono stati stamapati gli allegati alle tariffe
020700931109     C*****************************************************************
020800000000     C     *ENTRY        PLIST
020900000000     C                   PARM                    KPJBA
021000041021
021100041021     c     *dtaara       define    §azute        azuteds
021200041021     c     *dtaara       define    §datiute      ddatiute
021300041021     c                   in(E)     *dtaara
021400041021     c                   If        %error  or RSUT = *blanks
021500041021     c                   Clear                   Tibs34ds
021600041021     c                   Call      'TIBS34R'
021700041021     c                   Parm                    Tibs34ds
021800041021     c                   In        *dtaara
021900041021     c                   EndIf
022000041021
022100041021      * Verifica autorità
022200041021s   1c                   Select
022300041021      * se non c'è l'abilitazione
022400041021      * --> se 1° livello, abilitazioni al terminal
022500041021      *     se 2° livello, abilitazioni al punto operativo
022600041021      *     se sede è impossibile ma se capita metto 'AZ'
022700041021w   1c                   When      UteAut = *Blanks
022800041021if  2c                   If        DutLpo = '1'
022900041021     c                   Eval      wabi   = 'TP'
023000041021e   2c                   EndIf
023100041021if  2c                   If        DutLpo = '2'
023200041021     c                   Eval      wabi   = 'PO'
023300041021e   2c                   EndIf
023400041021if  2c                   If        DutLpo = 'S'
023500041021     c                   Eval      wabi   = 'AZ'
023600041021e   2c                   EndIf
023700041021      * carica le abilitazioni del profilo
023800041021      * per la nuova offerta
023900041021x   1c                   Other
024000041021     c                   Movel     UteFaf        Dute01
024100041021     c                   If        §UteGtc <> *Blanks
024200041021     c                   Eval      wabi = §UteGtc
024300041021     c                   Else
024400041021     c                   Eval      wabi = UteAut
024500041021     c                   EndIf
024600041021e   1c                   EndSl
024700060919     c* reperisco variabili programmi operativi
024800060919     C                   CLEAR                   tibs02ds
024900060919     c                   clear                   dvpo
025000060919     C                   MOVEL     'C'           T02MOD
025100060919     C                   MOVEL     KNSIF         T02SIF
025200060919     C                   MOVEL     'VPO'         T02COD
025300060919     c                   movel(P)  'VPO'         t02ke1
025400060919     C                   CALL      'TIBS02R'
025500060919     C                   PARM                    KPJBA
025600060919     C                   PARM                    tibs02ds
025700060919    2C                   IF        T02ERR = *BLANKS
025800060919     C                   MOVEL     T02uni        dvpo
025900060919    2C                   ENDIF
026000091110      *
026100091110     C                   CLEAR                   DSLV45
026200091110     C                   MOVEL     KPJBU         PARAM7
026300981112     C**
026400021024      * per Simfel <> *zeros apro file visite e indirizzi
026500021024     C                   IF        Simfel <> *zeros
026600091110     c                   if        pa7ast = 'T'
026700091021     C                   open      Tivis05l
026800091110     c                   endif
026900021024     C                   ENDIF
027000021024      *
027100021114     C     SIMFEL        CHAIN     AZORG01L                           30
027200981112     C  N30              MOVEL     ORGDIT        WSDIT
027300981112     C     *LIKE         DEFINE    ORGDIT        WSDIT
027400110203     C     *LIKE         DEFINE    VIDDST        COMDST
027500981112     C     *LIKE         DEFINE    VIDAGE        WCMM
027600940301     C*
027700021115      *
027800021115      * Chiamo il trul31r per reperire schiera po
027900041214      * stesso raggruppamento po utente
028000021115     C                   CLEAR                   TRUL31DS
028100041214     C                   EVAL      I31ABI   = 'RA'
028200041021     C                   EVAL      I31CPO   = dutPou
028300021115     C                   CALL      'TRUL31R'
028400021115     C                   PARM                    KPJBA
028500021115     C                   PARM                    TRUL31DS
028600021115     C                   IF        O31POG > *zeros
028700021115     C                   MOVEA     O31POG        POG
028800021115     C                   ENDIF
028900100427     C*
029000100427     C* PRENDO LA DATA DEL GIORNO DA TIME
029100100427     C                   TIME                    W0140            14 0
029200100427     C                   MOVE      W0140         UDATE8            8 0
029300100427     C                   Z-ADD     udate8        G02DAT
029400100427     C                   MOVEL     *BLANK        G02ERR
029500100427     C                   CALL      'XSRDA8'
029600100427     C                   PARM                    WLBDAt
029700100427     C                   MOVEL     G02INV        DATEU             8 0
029800100427     C                   MOVEL     G02INV        alfaoggi          8
029900940224     C*
030000940224     C*
030100931109     C*---------------------------------------------------------------*
030200940224     C* ACCESSO TABEL00F
030300940224     C     KTAB          KLIST
030400940224     C                   KFLD                    CODUT
030500940224     C                   KFLD                    COD
030600940224     C                   KFLD                    KEY
030700051026     C     KTBE          KLIST
030800051026     C                   KFLD                    TBECOD
030900051026     C                   KFLD                    TBEKE1
031000051026     C                   KFLD                    TBEKE2
031100940307     C     KTAB2         KLIST
031200940307     C                   KFLD                    CODUT
031300940307     C                   KFLD                    COD
031400131003      * -?File AZNTC01L: Note/Rubrica Commerciali?
031500130802     c     k02azntc01    klist
031600131003     c                   kfld                    k_AZNTCcmm
031700131003     c                   kfld                    k_AZNTCtnt
031800961129     C* ACCESSO CNACO00F / CNIND00F
031900940224     C     KACO          KLIST
032000940224     C                   KFLD                    CODUT
032100041021     C                   KFLD                    dutKCI
032200940224     C                   KFLD                    VIDCLI
032300041029     C* ACCESSO file note TFNTC00F
032400041029     C     Kntc          KLIST
032500041029     C                   KFLD                    kapl
032600041029     C                   KFLD                    knk1
032700041029     C                   KFLD                    knk2
032800041029     C                   KFLD                    ktnt
032900931109     C*
033000940224     C* DEFINIZIONE VARIABILI
033100940224     C     *LIKE         DEFINE    TBLCOD        COD
033200940224     C     *LIKE         DEFINE    TBLKEY        KEY
033300970714     C     *LIKE         DEFINE    E14CAP        VARCAP
033400970714     C     *LIKE         DEFINE    E14CAP        VARCA1
033500970714     C     *LIKE         DEFINE    E14LOC        VARLOC
033600970714     C     *LIKE         DEFINE    E14PRV        VARPRV
033700970714     C     *LIKE         DEFINE    E14NAR        VARNAR
033800131003     c     *like         define    AZNTCcmm      k_AZNTCcmm
033900131003     c     *like         define    AZNTCtnt      k_AZNTCtnt
034000950118     C*
034100940224     C*---------------------------------------------------------------*
034200940308     C******
034300940310     C* PA7FLG >< BLANK  ====>  P R O G R A M M A   R I C H I A M A T O
034400940308     C******
034500940308     C*
034600940308     C******
034700941216     C* PA7FLG = "R"  ====>  SALTO LA PRIMA VIDEATA (FORMATO LV43D01)
034800940308     C******
034900940308     C*
035000940308     C******
035100940310     C* PA7FLG = "1"/"2"/"3"/"4"  ====>  ESEGUO LA SUBR RICHIA POI ESCO
035200940308     C*
035300940310     C* PA7FLG = "1"  ====>  PROVENGO DA MENU'
035400940310     C* PA7FLG = "2"  ====>  PROVENGO DA GESTIONE     OFFERTA/TARIFFA
035500940310     C* PA7FLG = "3"  ====>  PROVENGO DA CONVALIDA    OFFERTA
035600940310     C* PA7FLG = "4"  ====>  PROVENGO DA ANNULLAMENTO OFFERTA/TARIFFA
035700940308     C******
035800940307     C*
035900940311    1C     PA7FLG        IFEQ      '1'
036000940310     C     PA7FLG        OREQ      '2'
036100940310     C     PA7FLG        OREQ      '3'
036200940310     C     PA7FLG        OREQ      '4'
036300940307     C                   EXSR      RICHIA
036400940307     C                   GOTO      FINE
036500940311    1C                   ENDIF
036600940308     C***
036700940308     C***
036800941216     C* CARICAMENTO TABELLE
036900940228     C                   EXSR      CARTAB
037000940301     C*
037100940314     C     INIZIO        TAG
037200110203     C                   SETOFF                                       0204
037300940224     C* AZZERO CAMPI FORMATO
037400940224     C                   EXSR      AZZER
037500940314     C     FOR01         TAG
037600940301     C*
037700940308     C* 13 ON  - P R O G R A M M A   R I C H I A M A T O
037800940310    1C     PA7FLG        IFEQ      'R'
037900940301     C                   SETON                                        13
038000940310   X1C                   ELSE
038100940301     C*
038200941216     C                   EXFMT     LV43D01
038300940314     C** CMD3  - FINE LAVORO
038400940314     C   KC              GOTO      FINE
038500940314    1C                   ENDIF
038600940314     C*
038700940314     C                   EXSR      CONTR1
038800940314     C   90              GOTO      FOR01
038900940314     C*
039000060609     c                   eval      vidclin = vidlin
039100931109     C*
039200110203     C     VIDAST        COMP      'C'                                    02
039300940330     C     VIDAST        COMP      'L'                                    04
039400050905     c                   setoff                                       19
039500050905     c* 19 on--> specificato un codice testo html
039600110207     c                   if        §1html = 'S'
039700050905     c                   seton                                        19
039800050905     c                   endif
039900050930     c* se richiesto ambiente Libero e testo html
040000050930     c* inizializzo campi videata 06 per richiesta variabili testo html
040100050930     c                   if        vidast = 'L' and *in19 = *on
040200050905     c                   exsr      rie06
040300050905     c                   endif
040400940224     C*
040500940224     C     FOR02         TAG
040600940330     C***
040700941216     C  N04              EXFMT     LV43D02
040800940330     C***
040900050905     c                   if        *in19 = *off
041000050905     C   04              EXFMT     LV43D05
041100050905     c                   else
041200050905     C   04              EXFMT     LV43D06
041300050905     c                   endif
041400941230     C*
041500960531     C* PULIZIA CAMPO MESSAGGIO E RELATIVI INDICATORI
041600960531     C                   CLEAR                   VIDMSG
041700941230     C                   SETOFF                                       902862
041800970324     C                   SETOFF                                       747679
041900970714     C                   SETOFF                                       716177
042000981112     C                   SETOFF                                       065948
042100981113     C                   SETOFF                                       81
042200940301     C*
042300981112     C** F3  - FINE LAVORO
042400940224     C   KC              GOTO      FINE
042500940224     C*
042600981112     C** F12 - RITORNO
042700940314     C   KL              GOTO      INIZIO
042800981112     C**
042900940302     C*
043000931109     C* CONTROLLI FORMATO
043100940314     C                   EXSR      CONTR2
043200940224     C*
043300940224     C   90              GOTO      FOR02
043400940224     C*
043500981112     C** F14 - OPZIONI DI STAMPA TESTO
043600940311    1C     *INKN         IFEQ      *ON
043700940331     C*
043800941216     C  N04              WRITE     LV43D02
043900941216     C   04              WRITE     LV43D05
044000940302     C*
044100940224     C                   MOVEL     VIDTTX        PA3TTX
044200940224     C                   MOVEL     PARAM3        KPJBU
044300941216     C                   CALL      'FNLV42R'
044400940224     C                   PARM                    KPJBA
044500021024      *
044600021024     C                   CLEAR                   Ds1H
044700021024     C                   CLEAR                   Fnlv41ds
044800021024     C                   MOVEL     '2'           I41MOD
044900021024     C                   MOVEL     VIDTTX        I41LET
045000021024     C                   CALL      'FNLV41R'
045100021024     C                   PARM                    FNLV41DS
045200021024     C     O41ERR        IFNE      'E'
045300021024     C                   MOVEL     O41DTA        DS1H
045400021024     C                   ENDIF
045500021024      *
045600021024     C                   GOTO      FOR02
045700021024    1C                   ENDIF
045800050930     C  nkf
045900050930     cor 90              goto      for02
046000931109     C*
046100941216     C** CMD6 - ELABORAZIONE
046200940228     C*
046300940228     C* SE CI SONO PARAMETRI VARIABILI PRESENTO IL TESTO DELLA LETTERA
046400940228     C*   RICHIAMANDO IL PGM MANUTENZIONE TESTI
046500110207     c     *in19         ifeq      *off
046600940330    1C     §1HPVA        ANDEQ     'S'
046700940228     C                   CLEAR                   PARAM4
046800940228     C                   MOVEL     '1'           PA4FLG
046900940228     C                   MOVEL     VIDTTX        PA4TTX
047000060607     C**!!!              MOVEL     'I'           PA4LIN
047100060607     c                   move      liniso2       pa4lin
047200940228     C                   MOVEL     PARAM4        KPJBU
047300941216     C                   CALL      'FNLV40R'
047400940228     C                   PARM                    KPJBA
047500940330    1C                   ENDIF
047600940330     C*
047700981112     C** DATI COMUNI TRA TESTO LIBERO E GLI ALTRI
047800981112     C                   MOVEL     VIDTTX        I45TTX
047900050905     C                   MOVEL     VIDNR1        I45NR1
048000050905     C                   MOVEL     VIDNR2        I45NR2
048100050905     C                   MOVEL     VIDNCO        I45NCO
048200050905     C                   MOVEL     VIDDST        I45GDS
048300050905     C                   MOVEL     COMDST        I45ADS
048400060607     C**!!!              MOVEL     'I'           I45LIN
048500060607     c                   eval      i45lin = linfntxt
048600981112     C                   MOVEL     VIDFAX        I45FAX
048700981112     C                   MOVEL     VIDIMM        I45IMM
048800041021     C                   MOVEL     dutKCI        I45KCC
048900981209     C                   MOVEL     VIDAST        I45AST
049000041111     c                   movel     vidobj        i45obj
049100110207     c                   movel     §1html        i45htm
049200050930     c* variabili per testo html se ambiente stampa testi = Libero
049300050930     c                   if        *in04=*on
049400050907     c                   movel     v06nmc        i45nmc
049500050907     c                   movel     v06emc        i45emc
049600050907     c* ns.riferimenti
049700050907     c                   movel     v06nmn        i45nmn
049800050907     c                   movel     v06emn        i45emn
049900120220     c                   eval      i45emn = %trim(i45emn) + '@BRT.it'
050000050912     c****               movel     v06obj        i45hbj
050100050907     c                   endif
050200050907     c
050300981112     C**
050400940330     C     *IN04         IFEQ      *OFF
050500981112     C**
050600981112     C* SE DATA STATO AL = 0  CI IMPOSTO LA DATA STATO DAL
050700981112     C* PASSAGGIO DATI A PGM DI STAMPA
050800940330     C*
050900981112     C                   MOVEL     VIDCLI        I45KSC
051000110201     C                   MOVEL     VIDAGE        I45CMM
051100110203     C                   clear                   I45TPV
051200981112     C                   MOVEL     VIDSCT        I45SCT
051300110201     C                   MOVEL     VIDCLV        I45CLV
051400110203     C                   clear                   I45FST
051500981112     C                   MOVEL     VIDFIL        I45APP
051600981112     C                   MOVEL     VIDPRO        I45PRO
051700981112     C                   MOVEL     VIDCAP        I45CAP
051800981112     C                   MOVEL     VIDNAR        I45NAR
051900981112     C                   MOVEL     VIDABL        I45ABL
052000981112     C                   MOVEL     §1HPGS        I45PGM
052100980127AG*  C* DECODIFICO IL CODICE COMMERCIALE DA PASSARE AL PROGRAMMA CHE STAMPERA'
052200980127AG*  C* L'ORGANIGRAMMA
052300980127     C     CCOM          IFNE      *ZEROS
052400130731     C                   MOVE      CCOM          CMMcod
052500130731     C     CMMcod        CHAIN     AZCMM000
052600130731     C                   if        %found(AZCMM01L)
052700130731     C                   MOVEL     CMMdes        I45DCM
052800130731     C                   endif
052900130731     C                   ENDIF
053000940330     C*
053100940330     C                   ELSE
053200940330     C* NOME PROGRAMMA DI STAMPA
053300981112     C                   MOVEL     §1HPGS        I45PGM
053400981112     C                   MOVEL     VI4RAG        I45RAG
053500981112     C                   MOVEL     VI4VIA        I45VIA
053600981112     C                   MOVEL     VI4CIT        I45CIT
053700981112     C                   MOVEL     VI4CAP        I45CAP
053800981112     C                   MOVEL     VI4NAR        I45NAR
053900981112     C                   MOVEL     VI4PRV        I45PRO
054000981112     C                   MOVEL     VI4FIL        I45APP
054100041103     c     vi4nfx        ifne      *blanks
054200041103     c     wmail         andeq     'S'
054300041103     c                   movel     vi4nfx        i45mai
054400041103     c                   else
054500981112     C                   MOVEL     VI4NFX        I45NFX
054600041103     c                   endif
054700981112     C                   MOVEL     VI4RS1        I45ACA
054800041104     C                   MOVEL     VI4dco        I45dcm
054900940330     C                   ENDIF
055000940413     C*
055100981113     C                   MOVEL     DSLV45        KPJBU
055200941216     C                   CALL      'FNLV45C'
055300940301     C                   PARM                    KPJBA
055400981113     C                   PARM                    DSLV45
055500060919     c                   if        P_numcli > *zeros
055600060919     c     '0'           check     P_NUMCLI      N                 2 0
055700060919     c                   eval      P_NUMCLI=%subst(P_NUMCLI:N)
055800060919     C                   MOVEL     MSG(15)       VIDMSG
055900091021     c                   eval      vidmsg=%trim(vidmsg) + 'i'
056000060919     c                   eval      vidmsg=%trim(vidmsg) + ' ' + P_NUMCLI
056100110201     c                   if        *in02
056200060919     c                   eval      vidmsg = %trim(vidmsg) + ' clienti.'
056300110201     c                   endif
056400060919     c                   eval      vidmsg=%trim(vidmsg) + ' Limite max'
056500060919     c                   move      §vponclimx    W_nclimx          4
056600060919     c     '0'           check     w_nclimx      N                 2 0
056700060919     c                   eval      w_nclimx=%subst(w_nclimx:N)
056800060919     c                   eval      vidmsg=%trim(vidmsg) + ' ' + W_nclimx
056900060919     C                   SETON                                        28
057000060919     c                   goto      for02
057100060919     c                   endif
057101161223
057300940314     C                   GOTO      INIZIO
057400000000     C     FINE          TAG
057500970714     C*
057600970714     C                   MOVEL     'C'           I13TLA
057700970714     C                   CALL      'FNLV13R'
057800970714     C                   PARM                    KPJBA
057900970714     C                   PARM                    DSLV13
058000970714     C                   PARM                    DSSI95
058100970714     C*
058200000000     C                   SETON                                        LR
058300900524     C*
058400941216     C*--- CARICAMENTO TABELLE ---------------------------------------*
058500940228     C     CARTAB        BEGSR
058600941216     C***
058700941216     C* CARICAMENTO FILIALI GESTITE £1
058800941216     C***
058900020417     c                   if        simfel > 0
059000020417     c                   clear                   trul06ds
059100020417     c                   eval      d06cod = '£1'
059200020417     c                   movel     simfel        d06key
059300020417     c                   movel     trul06ds      kpjbu
059400020417     c                   call      'TRUL06R'
059500020417     c                   parm                    kpjba
059600020417     c                   movel     kpjbu         trul06ds
059700020417     c                   movea     lin           l1
059800020417     c                   endif
059900940330     C*
060000940330     C* VEDO DAL PROFILO CHE FILIALE IMPOSTARE
060100041021     c     dutlpo        ifeq      '2'
060200041021     c     dutlpo        oreq      *blanks
060300041021     c                   movel     dutpou        wfil              3 0
060400940330     C                   SETON                                        05
060500940330     C                   ELSE
060600940330     C                   MOVEL     SIMFEL        WFIL
060700940330     C                   END
060800941216     C*
060900940228     C                   ENDSR
061000940228     C*
061100940224     C*--- AZZERO CAMPI FORMATO --------------------------------------*
061200940224     C     AZZER         BEGSR
061300940224     C*
061400940310     C     PA7FLG        IFEQ      ' '
061500941216     C                   CLEAR                   LV43D01
061600060607     c                   eval      vidlin = 'it'
061700060607      * decodifico la lingua da azlin
061800060607     c     vidlin        chain     azlin03l
061900060607     c                   if        %found(azlin03l)
062000060607     c                   eval      deslin = lindesita
062100060607     c                   endif
062200940301     C                   ENDIF
062300940301     C*
062400941216     C                   CLEAR                   LV43D02
062500941216     C                   CLEAR                   LV43D05
062600940224     C*
062700041021     C**!!!              MOVEL     RAGUT         RSUT
062800050912     C                   MOVEL     Rsut          vRSUT
062900941220     C                   MOVEL     KNSIF         NSIF
063000941220     C                   MOVEL     KNMUS         NMUS
063100940308     C                   MOVE      TES(1)        VIDTES
063200940228     C                   MOVEL     'N'           VIDABL
063300940311     C                   Z-ADD     1             VIDNCO
063400941216     C                   Z-ADD     UDATE8        VIDDST
063500940330     C                   MOVEL     WFIL          VI4FIL
063600970326     C                   MOVEL     'I'           VIDIMM
063700110504     C                   eval      vidobj='Comunicazione da BRT S.p.A.'
063800940224     C*
063900940224     C                   CLEAR                   COMDST
064000941230     C                   MOVEL     '999'         VIDNAR
064100941230     C                   MOVEL     CNAZ          DESNAR
064200970714     C*
064300970714     C                   MOVEL     0             WZV               1 0
064400970714     C                   MOVEL     0             WZ1               1 0
064500970714     C                   MOVEL     0             WZ2               1 0
064600970714     C                   MOVEL     0             WZ3               1 0
064700980128AG*  C* PULISCO I CAMPI DI COMODO PER LA DECODIFICA DEL COMMERCIALE
064800980128AG*  C                   MOVEL     0             CCOM
064900981112AG*  C                   MOVEL     *BLANKS       I45DCM
065000981113     C                   CLEAR                   DSLV45
065100980128     C*
065200940224     C                   ENDSR
065300110207     C*--- CONTROLLI FORMATO 1 ---------------------------------------*
065400940314     C     CONTR1        BEGSR
065500110207     C                   SETOFF                                       90
065600940314     C*
065700940314     C****  CODICE TESTO  ****
065800940314     C     '?'           SCAN      VIDTTX                                 90
065900940314     C*
066000940314    1C     *IN90         IFEQ      *ON
066100021024     C                   MOVEL     *blanks       VIDTTX
066200021024     C                   MOVEL     *blanks       DESTTX
066300021024      *
066400021024      * Richiamo FnLv41 in modalità ricerca testi
066500021024     C                   CLEAR                   Ds1H
066600021024     C                   CLEAR                   Fnlv41ds
066700021024     C* PROFILO UTENTE
066800021024     C                   MOVEL     PROF          I41Pro
066900021119     C* I41Prv = 'S' ===> PROVENGO DA STAMPA TESTI
067000021024     C                   MOVEL     'S'           I41Prv
067100021024     C*
067200021024     C                   MOVEL     '1'           I41MOD
067300021024     C                   CALL      'FNLV41R'
067400021024     C                   PARM                    FNLV41DS
067500021024     C     O41LET        IFNE      *blanks
067600021024     C                   MOVEL     O41LET        VIDTTX
067700021024     C                   MOVEL     O41dta        DS1H
067800021024     C                   MOVEL     §1Hdes        DESTTX
067900021024     C                   ENDIF
068000021024     C                   GOTO      ENDCT1
068100021024    1C                   ENDIF
068200940314     C*
068300940314     C* CODICE TESTO OBBLIGATORIO
068400940314    1C     VIDTTX        IFEQ      '  '
068500940314     C                   CLEAR                   DESTTX
068600940314     C*
068700940314     C                   SETON                                        4090
068800940314     C                   GOTO      ENDCT1
068900940314     C*
069000940314   X1C                   ELSE
069100021024      *
069200021024      * Richiamo FnLv41 per verificare/creare file testi e reperire ds1h
069300021024     C                   CLEAR                   Ds1H
069400021024     C                   CLEAR                   Fnlv41ds
069500021024     C                   MOVEL     '3'           I41MOD
069600021024     C                   MOVEL     VIDTTX        I41LET
069700021024     C                   CALL      'FNLV41R'
069800021024     C                   PARM                    FNLV41DS
069900021024     C     O41ERR        IFNE      'E'
070000021024     C                   MOVEL     O41DTA        DS1H
070100021024     C                   ELSE
070200021024     C                   SETON                                        4190
070300021024     C                   GOTO      ENDCT1
070400021024     C                   ENDIF
070500940314     C*
070600940314     C* SI TRATTA DI UNA LETTERA DA NON PRESENTARE A MENU'
070700940314    2C     §1HFL1        IFNE      'S'
070800110201     C                   SETON                                        4290
070900940314     C                   GOTO      ENDCT1
071000940314    2C                   ENDIF
071100940314     C*
071200940314     C* CONTROLLO SE PROFILO VALIDO PER LA GESTIONE DEL TESTO RICHIESTO
071300940314     C                   MOVEL     '1I'          COD
071400940314     C                   MOVEL     *BLANKS       KEY
071500940314     C                   MOVEL     §1HFGT        KEY
071600940314     C     KTAB          CHAIN     TABEL                              30
071700940314    2C  N30TBLFLG        IFEQ      ' '
071800940314     C                   MOVEL     TBLUNI        DS1I
071900940314     C*
072000940314     C     '999'         LOOKUP    SKI                                    35
072100940314     C  N35PROF          LOOKUP    SKI                                    35
072200940314     C  N35              SETON                                        30
072300940314     C                   ELSE
072400940314     C                   SETON                                        30
072500940314    2C                   ENDIF
072600940314     C*
072700940314     C   30              SETON                                        4390
072800940314     C   30              GOTO      ENDCT1
072900940314     C*
073000940314     C* DECODIFICA TIPO TESTO
073100940314     C                   MOVEL     §1HDES        DESTTX
073200940314     C*
073300940314     C* IMPOSTO NUMERO COPIE
073400940314     C                   Z-ADD     §1HNCO        VIDNCO
073500940314    1C                   ENDIF
073600060607
073700060607      * lingua ricerca
073800060607     c                   if        %scan('?':vidlin) > 0
073900060607     c                   clear                   p37iso2
074000060607     c                   clear                   p37desi
074100060607     c                   call      'TRUL37R'
074200060607     c                   parm                    p37iso2
074300060607     c                   parm                    p37desi
074400060607     c                   eval      vidlin = p37iso2
074500060607     c                   eval      deslin = p37desi
074600060607     c     vidlin        chain     azlin03l
074700060607     c                   eval      *in90 = *on
074800060607     c                   leavesr
074900060607     c                   endif
075000060607      * lingua obbligatoria
075100060607     c                   if        vidlin = *blanks
075200060607     c                   eval      *in82 = *on
075300060607     c                   eval      *in90 = *on
075400060607     c                   leavesr
075500060607     c                   endif
075600060607      * lingua controllo
075700060607     c                   clear                   deslin
075800060607     c     vidlin        chain     azlin03l
075900060607     c                   if        not %found(azlin03l)
076000060607     c                   eval      *in82 = *on
076100060607     c                   eval      *in90 = *on
076200060607     c                   leavesr
076300060607     c                   endif
076400060607     c                   eval      deslin = lindesita
076500060607      * se non è attiva per i testi errore
076600060607     c                   if        linfntxt = '0'
076700060607     c                   eval      *in82 = *on
076800060607     c                   eval      *in90 = *on
076900060607     c                   leavesr
077000060607     c                   endif
077100060607      * se ho scelto di stampare in lingua il testo deve essere gestito
077200060607      * in lingua
077300110207     c                   if        vidlin <> 'it'
077400060607     c                             and §1hlin = *blanks
077500060607     c                   eval      *in89 = *on
077600060607     c                   eval      *in90 = *on
077700060607     c                   leavesr
077800060607     c                   endif
077900940314     C*
078000940314     C     ENDCT1        ENDSR
078100940224     C*
078200940314     C*--- CONTROLLI FORMATO 2----------------------------------------*
078300940314     C     CONTR2        BEGSR
078400961129     C                   SETOFF                                       900508
078500051010     C                   SETOFF                                       85
078600941230     C                   CLEAR                   DESNAR
078700041103     C                   CLEAR                   wmail
078800941230     C                   MOVEL     CNAZ          DESNAR
078900940314     C*
079000940314     C****  DATA STAMPA  ****
079100940314    1C     VIDDST        IFNE      0
079200940314     C                   MOVE      VIDDST        G02DAT
079300940314     C                   MOVEL     *BLANKS       G02ERR
079400941216     C                   CALL      'XSRDA8'
079500940314     C                   PARM                    WLBDAT
079600940314    2C     G02ERR        IFEQ      '1'
079700940314     C                   SETON                                        5690
079800940314     C                   GOTO      ENDCTR
079900940314    2C                   ENDIF
080000941216     C                   Z-ADD     G02INV        COMDST
080100941216     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
080200941216     C                   Z-ADD     G02DAT        VIDDST
080300940314    1C                   ENDIF
080400940330     C***
080500940330    0C     *IN04         IFEQ      *OFF
080600931109     C*
080700940224     C****  CODICE CLIENTE  ****
080800940228     C* SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
080900940228     C*   IMMESSO IL CODICE CLIENTE CALL PGM
081000110203    1C     VIDCLI        IFEQ      0
081100940310    2C     RAGDES        IFNE      *BLANKS
081200940228     C* PARSTA = 9  ESCLUDO ANNULLATI
081300940228     C                   Z-ADD     9             PARSTA
081400940228     C                   MOVEL     RSUT          PARDUT           30
081500940228     C                   MOVEL     *BLANKS       DESCR            48
081600940228     C                   MOVEL     RAGDES        DESCR
081700041021     C                   Z-ADD     dutKCI        CCC               4 0
081800000905     C                   Z-ADD     1             PAXNUM
081900961016     C     SIMFEL        IFGT      0
082000961016     C                   MOVEA     L1            COM
082100981112     C                   MOVEL     WSDIT         PDIT
082200961016     C                   ENDIF
082300000905     C                   CALL      'XALFA3BR'
082400940228     C                   PARM                    PARDUT
082500940228     C                   PARM                    CODUT
082600940228     C                   PARM                    DESCR
082700940228     C                   PARM                    CCC
082800940228     C                   PARM                    PARSTA            1 0
082900961016     C                   PARM                    COM
083000981112     C                   PARM                    PDIT              3
083100000905     C                   PARM                    PAXNUM            2 0
083200000905     C                   PARM                    PAXKCM           80
083300000905     C                   PARM                    PAXKSM          140
083400000905     C                   PARM                    PAXKDM           60
083500940228     C     PARSTA        IFNE      -1
083600000905     C                   MOVEL     PAXKSM        VIDCLI
083700940228     C                   MOVEL     DESCR         RAGDES
083800940228     C                   ELSE
083900940228     C                   CLEAR                   RAGDES
084000940228     C                   ENDIF
084100940228     C*
084200940228     C                   SETON                                        90
084300940228     C                   GOTO      ENDCTR
084400940310    2C                   ENDIF
084500940228     C*
084600940310   X1C                   ELSE
084700961129     C* 08 ON  - IMMESSO UNO SPECIFICO CODICE CLIENTE
084800961129     C                   SETON                                        08
084900940224     C*
085000940224     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
085100940224     C     KACO          CHAIN     CNACO000                           30
085200940310    2C  N30ACOFLG        IFNE      ' '
085300940224     C                   SETON                                        30
085400940310    2C                   ENDIF
085500940224     C   30              SETON                                        4690
085600940224     C   30              GOTO      ENDCTR
085700970319     C*
085800970319     C     SIMFEL        IFGT      0
085900030218     C                   MOVEL     ACOKSC        W003a             3
086000030218     C     W003a         LOOKUP    pog                                    30
086100970319     C     *IN30         IFEQ      *OFF
086200970319     C                   SETON                                        3990
086300970319     C                   GOTO      ENDCTR
086400970319     C                   END
086500970319     C                   END
086600940224     C*
086700940224     C* DECODIFICA CLIENTE
086800940224     C                   MOVEL     ACORAG        RAGDES
086900980127AG*  C* RECUPERO CODICE DEL COMMERCIALE
087000980127AG*  C     KACO          CHAIN     CNCLP00F                           OC
087100980127AG*  C  NOC              Z-ADD     CLPAGE        CCOM
087200940310    1C                   ENDIF
087300940314     C*
087400091021      *
087500980127     C*
087600980127AG*  C* CONTROLLO CHE SIA STATO INSERITO ALMENO UN CODICE SE RICHIESTO
087700980127AG*  C* IL TESTO 20 ORGANIGRAMMA
087800980127AG*  C     VIDTTX        IFEQ      '20'
087900980127AG*  C*
088000980127AG*  C     VIDCLI        ANDEQ     *ZEROS
088100980127AG*  C                   SETON                                        3790
088200980127AG*  C                   GOTO      ENDCTR
088300980127AG*  C*
088400980127AG*  C                   ENDIF
088500981112AG*  C*
088600981112     C****  CODICE COMMERCIALE  ****
088700981112     C                   MOVEL     VIDAGE        WCMM
088800981112     C                   CLEAR                   WDESCM
088900981112     C                   EXSR      CTRCMM
089000981112     C                   MOVEL     WCMM          VIDAGE
089100981112     C                   MOVEL     WDESCM        DESAGE
089200981112     C   28              SETON                                        48
089300981112     C   90              GOTO      ENDCTR
089400940224     C*
089500940228     C****  FILIALE  ****
089600940228     C     VIDFIL        IFNE      0
089700020724     c                   if        simfel > 0
089800030218     c                   move      vidfil        w003a
089900030218     C     w003a         LOOKUP    pog                                    35
090000030218     C  N35              SETON                                        5890
090100020724     C  N35              GOTO      ENDCTR
090200020724     C                   else
090300020724     C     VIDFIL        IFNE      0
090400020724     C     VIDFIL        CHAIN     AZORG01L                           30
090500020724     C   30              SETON                                        6090
090600020724     C   30              GOTO      ENDCTR
090700020724     C                   ENDIF
090800020724     C                   endif
090900940228     C*
091000940309     C* SE INSERITO IL CODICE COMMERCIALE CONTROLLO CONGRUENZA FILIALI
091100940311     C     VIDAGE        IFNE      *ZEROS
091200940311     C     VIDAGE        ANDNE     *BLANKS
091300940309     C                   MOVEL     VIDAGE        NUM3              3 0
091400940309     C     VIDFIL        IFNE      NUM3
091500940309     C                   SETON                                        6790
091600940309     C                   GOTO      ENDCTR
091700940309     C                   ENDIF
091800940311     C                   ENDIF
091900940309     C*
092000940228     C     VIDFIL        CHAIN     AZORG                              30
092100940228     C     *IN30         IFEQ      *OFF
092200940228     C                   MOVEL     ORGDES        DESFIL
092300940228     C                   ELSE
092400940228     C                   CLEAR                   DESFIL
092500940228     C                   ENDIF
092600940228     C*
092700940228     C                   ELSE
092800940228     C                   CLEAR                   DESFIL
092900940228     C                   ENDIF
093000941219     C*
093100941219     C****  NAZIONE  ****
093200941230     C     VIDNAR        IFNE      '999'
093300941219     C     '?'           SCAN      VIDNAR                                 90
093400941219     C*
093500941219     C     *IN90         IFEQ      *ON
093600941219     C                   MOVEL     '15'          COD
093700941219     C                   MOVEL     *BLANKS       VIDNAR
093800941219     C                   MOVEL     *BLANKS       DES              25
093900981112     C                   CLEAR                   KEY
094000941219     C                   CALL      'X§TABER'
094100941219     C                   PARM                    CODUT
094200941219     C                   PARM                    COD
094300941219     C                   PARM                    KEY
094400941219     C                   PARM                    DES
094500941219     C                   MOVEL     KEY           VIDNAR
094600941219     C                   MOVEL     DES           DESNAR
094700941219     C                   GOTO      ENDCTR
094800941219     C                   ENDIF
094900970714     C* CONTROLLO VALIDITA'
095000970714     C                   MOVEL     '15'          COD
095100970714     C                   MOVEL     *BLANKS       KEY
095200970714     C                   MOVEL     VIDNAR        KEY
095300970714     C*
095400970714     C     KTAB          CHAIN     TABEL                              30
095500970714     C  N30TBLFLG        IFNE      ' '
095600970714     C                   SETON                                        30
095700970714     C                   ENDIF
095800970714     C* NAZIONE ERRATA
095900970714     C   30              SETON                                        7190
096000970714     C   30              GOTO      ENDCTR
096100970714     C* DECODIFICA NAZIONE
096200970714     C                   MOVEL     TBLUNI        DESNAR
096300941230     C                   ENDIF
096400941230     C*
096500941230     C*
096600941230     C****  PROVINCIA  ****
096700941230     C     '?'           SCAN      VIDPRO                                 90
096800941230     C*
096900941230     C     *IN90         IFEQ      *ON
097000941230     C                   MOVEL     'PR'          COD
097100941230     C                   MOVEL     *BLANKS       VIDPRO
097200941230     C                   MOVEL     *BLANKS       DES              25
097300981112     C                   CLEAR                   KEY
097400941230     C                   CALL      'X§TABER'
097500941230     C                   PARM                    CODUT
097600941230     C                   PARM                    COD
097700941230     C                   PARM                    KEY
097800941230     C                   PARM                    DES
097900941230     C                   MOVEL     KEY           VIDPRO
098000941230     C                   MOVEL     DES           DESPRO
098100941230     C                   GOTO      ENDCTR
098200941230     C                   ENDIF
098300941230     C     VIDPRO        IFNE      '  '
098400941230     C*
098500941230     C* CONTROLLO VALIDITA' DELLA PROVINCIA IMMESSA
098600941230     C                   MOVEL     'PR'          COD
098700941230     C                   MOVEL     *BLANKS       KEY
098800941230     C                   MOVEL     VIDPRO        KEY
098900941230     C*
099000941230     C     KTAB          CHAIN     TABEL                              30
099100941230     C  N30TBLFLG        IFNE      ' '
099200941230     C                   SETON                                        30
099300941230     C                   ENDIF
099400941230     C* PROVINCIA ERRATA
099500941230     C   30              SETON                                        6190
099600941230     C   30              GOTO      ENDCTR
099700941230     C*
099800941230     C* DECODIFICA PROVINCIA
099900941230     C                   MOVEL     TBLUNI        DESPRO
100000941230     C*
100100941230     C                   ELSE
100200941230     C                   CLEAR                   DESPRO
100300941230     C                   ENDIF
100400940228     C*
100500940228     C****  C. A. P.  ****
100600941219     C     VIDCAP        IFNE      *BLANKS
100700970714     C                   CLEAR                   DSSI95
100800970714     C                   CLEAR                   DSLV13
100900970714     C                   MOVEL     VIDPRO        I95PRV
101000970714     C                   MOVEL     VIDCAP        I95CAP
101100970714     C                   MOVEL     VIDNAR        I95NAR
101200970714     C                   MOVEL     COMDST        I95DAT
101300970714     C                   MOVEL     '3'           I95TCN
101400970716     C* CONTROLLO CON LA PROVNCIA SOLO SE C'E'
101500970716     C     VIDPRO        IFNE      *BLANKS
101600970715     C                   MOVEL     'S'           I13CNG
101700970714     C                   MOVEL     'S'           I13AF0
101800970716     C                   ENDIF
101900970714     C*
102000970714     C*
102100970714     C                   CALL      'FNLV13R'
102200970714     C                   PARM                    KPJBA
102300970714     C                   PARM                    DSLV13
102400970714     C                   PARM                    DSSI95
102500970714     C*
102600941230     C* ERRORE
102700970715     C                   MOVEL     O13ERR        WERR              1
102800970715     C                   MOVEL     O13MSG        VIDMSG
102900970715     C                   EXSR      CTRERR
103000970715     C     *IN90         IFEQ      *ON
103100970715     C                   GOTO      ENDCTR
103200970715     C                   ENDIF
103300970716     C                   ENDIF
103400940224     C*
103500940224     C****  CODICE CATEGORIA SEAT  ****
103600940224     C     '?'           SCAN      VIDSCT                                 90
103700940224     C*
103800940224     C     *IN90         IFEQ      *ON
103900940224     C                   MOVEL     '1L'          COD
104000940224     C                   MOVEL     *BLANKS       VIDSCT
104100940224     C                   MOVEL     *BLANKS       DES              25
104200981112     C                   CLEAR                   KEY
104300940224     C                   CALL      'X§TABER'
104400940224     C                   PARM                    CODUT
104500940224     C                   PARM                    COD
104600940224     C                   PARM                    KEY
104700940224     C                   PARM                    DES
104800940224     C                   MOVEL     KEY           VIDSCT
104900940224     C                   MOVEL     DES           DESSCT
105000940224     C                   GOTO      ENDCTR
105100940224     C                   ENDIF
105200940224     C*
105300940224     C     VIDSCT        IFNE      *BLANKS
105400940224     C     VIDSCT        ANDNE     *ZEROS
105500940224     C*
105600940224     C*
105700940224     C* CONTROLLO VALIDITA' DEL CODICE CATEGORIA SEAT IMMESSO
105800940224     C                   TESTN                   VIDSCT               35
105900940224     C     *IN35         IFEQ      *OFF
106000940224     C                   SETON                                        5190
106100940224     C                   GOTO      ENDCTR
106200940224     C                   ENDIF
106300940224     C*
106400940224     C* CONTROLLO SE ESISTE IN TABELLA
106500940224     C                   MOVEL     '1L'          COD
106600940224     C                   MOVEL     *BLANKS       KEY
106700940224     C                   MOVEL     VIDSCT        KEY
106800940224     C*
106900940224     C     KTAB          CHAIN     TABEL                              30
107000940224     C  N30TBLFLG        IFNE      ' '
107100940224     C                   SETON                                        30
107200940224     C                   ENDIF
107300940224     C* CODICE CATEGORIA SEAT ERRATA
107400940224     C   30              SETON                                        5190
107500940224     C   30              GOTO      ENDCTR
107600940224     C*
107700940224     C* DECODIFICA CODICE CATEGORIA SEAT
107800940224     C                   MOVEL     TBLUNI        DESSCT
107900940224     C*
108000940224     C                   ELSE
108100940225     C                   MOVEL     *ZEROS        VIDSCT
108200940224     C                   CLEAR                   DESSCT
108300940224     C                   ENDIF
108400940224     C*
108500980518     C*
108600980518     C** RICERCA IMPORTANZA CLIENTE
108700980518     C     VIDCLV        IFEQ      '?'
108800980518     C                   MOVEL     'IC'          COD
108900980518     C                   MOVEL     *BLANKS       VIDCLV
109000980518     C                   MOVEL     *BLANKS       DES              25
109100981112     C                   CLEAR                   KEY
109200980518     C                   CALL      'X§TABER'
109300980518     C                   PARM                    CODUT
109400980518     C                   PARM                    COD
109500980518     C                   PARM                    KEY
109600980518     C                   PARM                    DES
109700980518     C                   MOVEL     KEY           VIDCLV
109800980518     C                   SETON                                        5990
109900980518     C                   GOTO      ENDCTR
110000980518     C                   ENDIF
110100980518     C*
110200980518     C     VIDCLV        IFNE      *BLANKS
110300980518     C* CONTROLLO SE ESISTE IN TABELLA
110400980518     C                   MOVEL     'IC'          COD
110500980518     C                   MOVEL(P)  VIDCLV        KEY
110600980518     C*
110700980518     C     KTAB          CHAIN     TABEL                              30
110800980518     C  N30TBLFLG        IFNE      ' '
110900980518     C                   SETON                                        30
111000980518     C                   ENDIF
111100980518     C     *IN30         IFEQ      *ON
111200980518     C                   MOVEL     MSG(3)        VIDMSG
111300980518     C                   SETON                                        599028
111400980518     C                   GOTO      ENDCTR
111500980518     C                   ENDIF
111600980518     C                   ENDIF
111700940314     C*
111800940314     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
111900981112     C     VIDFIL        IFEQ      0
112000940314     C     VIDPRO        ANDEQ     '  '
112100981112     C     VIDNAR        ANDEQ     *BLANKS
112200981112     C     VIDCAP        ANDEQ     *BLANKS
112300940314    3C     VIDSCT        IFEQ      *ZEROS
112400940314     C     VIDSCT        OREQ      *BLANKS
112500940314     C*
112600110203    4C     VIDCLI        IFEQ      0
112700940314     C     RAGDES        ANDEQ     *BLANKS
112800940314     C     VIDABL        ANDEQ     ' '
112900940314     C     VIDCLV        ANDEQ     ' '
113000981112    1C     VIDAGE        ANDLE     *ZEROS
113100940314     C                   SETON                                        6890
113200940314     C                   GOTO      ENDCTR
113300940314    4C                   ENDIF
113400940314    3C                   ENDIF
113500940314    1C                   ENDIF
113600050907     c*
113700110203     c* SE IMMESSO UNO SPECIFICO CLIENTE ed
113800050930     c* è possibile che venga richiesto l'invio con mail cerco
113900050930     c* l'indirizzo e-mail
114000050907     c                   clear                   wnota06           1
114100050907    1C     *IN08         IFEQ      *ON
114200050930    2c                   if        *in19=*on or vidfax='S'
114300060919     c                             or vidfax='M'
114400050907     c* indirizzo e-mail
114500050907     c                   movel     '06'          ktnt
114600050907     c                   exsr      repntc
114700050907    3c                   if        %found(tfntc01l)
114800050907     c* memorizzo che ho trovato un indirizzo
114900050907     c                   if        ntcrnt <> *blanks
115000050907     c                   eval      wnota06='S'
115100050907     c                   endif
115200050907    3c                   endif
115300050907    2c                   endif
115400050930     c* se testo html e non ho trovato indirizzo e-mail del cliente
115500050930     c* errore bloccante
115600060919     c* Idem se no testo html ma richiesta modalità invio M=Mail
115700060919     c                   if        (*in19 = *on or vidfax='M') and
115800060919     c                             wnota06=*blanks
115900050930     C                   MOVEL     msg(14)       VIDMSG
116000051010     C                   SETON                                        902885
116100050930     C                   GOTO      ENDCTR
116200050930     c                   endif
116300050907    1c                   endif
116400940330     C*
116500050930     C****  INVIO TRAMITE E-mail/FAX/Lettera  ****
116600110203     C* SE IMMESSO UNO SPECIFICO            CLIENTE             E NEL
116700041029     C*  CAMPO INVIO E-mail/FAX C'E' "S" CONTROLLO CHE CI SIA almeno uno
116800041029     c* dei due
116900961129    1C     *IN08         IFEQ      *ON
117000050905     c     *in19         andeq     *off
117100961129     C     VIDFAX        ANDEQ     'S'
117200041029     c**********************************************************
117300041029     c* se non trovato indirizzo E-mail provo a cercare il n.fax
117400041029     c**********************************************************
117500050907     c                   if        wnota06 = *blanks
117600041029     c
117700110203     C     KACO          CHAIN     CNIND00F                           32
117800961203     C*
117900961203     C* SE NON PRESENTE IL NUMERO DEL FAX IN ANAGRAFICA: ERRORE
118000961203    2C  N32INDTLF        IFEQ      *BLANKS
118100041029     C                   MOVEL     msg(2)        VIDMSG
118200961203     C                   SETON                                        769028
118300961203     C                   GOTO      ENDCTR
118400961203    2C                   ENDIF
118500041029     c                   endif
118600961129    1C                   ENDIF
118700961129     C*
118800940330   X0C                   ELSE
118900940330     C*
119000050905   1ac     *in19         ifeq      *off
119100940330    1C     VI4FIL        IFNE      0
119200020724     c                   if        simfel > 0
119300030218     c                   move      vi4fil        w003a
119400030218     C     w003a         LOOKUP    pog                                    35
119500030218     C  N35              SETON                                        5890
119600940330     C  N35              GOTO      ENDCTR
119700020724     C                   else
119800020724     C     VI4FIL        CHAIN     AZORG01L                           30
119900020724     C   30              SETON                                        6090
120000020724     C   30              GOTO      ENDCTR
120100020724     C                   endif
120200940330     C                   ELSE
120300940330     C                   SETON                                        6090
120400940330     C                   GOTO      ENDCTR
120500940330    1C                   ENDIF
120600970716     C*
120700970716     C* CONTROLLO VALIDITA' DELLA NAZIONE IMMESSA
120800970716     C                   MOVEL     '15'          COD
120900970716     C                   MOVEL     *BLANKS       KEY
121000970716     C                   MOVEL     VI4NAR        KEY
121100970716     C*
121200970716     C     KTAB          CHAIN     TABEL                              30
121300970716     C  N30TBLFLG        IFNE      ' '
121400970716     C                   SETON                                        30
121500970716     C                   ENDIF
121600970716     C* NAZIONE ERRATA
121700970716     C   30              SETON                                        7190
121800970716     C   30              GOTO      ENDCTR
121900940330     C*
122000970716     C**
122100970716     C                   MOVEL     VI4RAG        I14RSC
122200970716     C                   MOVE      *BLANKS       I14RS2
122300970716     C                   MOVEL     VI4VIA        I14IND
122400970716     C                   MOVEL     VI4CIT        E14LOC
122500970716     C                   CLEAR                   E14PIP
122600970716     C                   MOVEL     VI4PRV        E14PRV
122700970716     C                   MOVEL     VI4NAR        E14NAR
122800970716     C                   MOVEL     VI4CAP        E14CAP
122900970716     C                   CLEAR                   I14ISO
123000970716     C                   MOVEL     COMDST        I14DRI
123100970716     C*
123200970716     C* CONTROLLO SE HO CAP O SOLO LOCALITA' E PROVINCIA
123300970716     C     VI4CAP        IFNE      *BLANKS
123400970716     C     VI4CIT        ORNE      *BLANKS
123500970716     C     VI4PRV        ANDNE     *BLANKS
123600970714     C* CONTROLLO CAP
123700970716     C* SE IMMESSO CAP E LOCALITA' CONTROLLO ENTRAMBI ALTRIMENTI SOLO
123800970716     C*  CAP O CAP CON PROVINCIA SE C'E'
123900970716     C                   CLEAR                   DSSI95
124000970716     C                   CLEAR                   DSLV13
124100970714     C                   MOVEL     E14CAP        I95CAP
124200970714     C                   MOVEL     E14LOC        I95LOC
124300970714     C                   MOVEL     E14PRV        I95PRV
124400970714     C                   MOVEL     E14NAR        I95NAR
124500970714     C                   MOVEL     COMDST        I95DAT
124600970716     C                   SELECT
124700970716     C     E14CAP        WHENNE    *BLANKS
124800970716     C     E14LOC        ANDNE     *BLANKS
124900970716     C     E14LOC        ORNE      *BLANKS
125000970716     C     E14PRV        ANDNE     '  '
125100970716     C                   MOVEL     '7'           I95TCN
125200970716     C                   MOVEL     'S'           I13AF0
125300970716     C     E14CAP        WHENNE    *BLANKS
125400970716     C     E14PRV        ANDNE     *BLANKS
125500970716     C                   MOVEL     '3'           I95TCN
125600970716     C                   MOVEL     'S'           I13CNG
125700970716     C                   MOVEL     'S'           I13AF0
125800970716     C     E14CAP        WHENNE    '  '
125900970716     C                   MOVEL     '3'           I95TCN
126000970716     C                   ENDSL
126100970716     C**
126200970714     C                   MOVEL     'S'           I13CNV
126300970714     C                   MOVEL     'S'           I13SZV
126400970716     C**
126500970716     C* AFFIDABILITA' 1 SOLO SE CI SONO TUTTI E 3
126600970716     C     E14CAP        IFNE      *BLANKS
126700970716     C     E14LOC        ANDNE     *BLANKS
126800970716     C     E14PRV        ANDNE     *BLANKS
126900970714     C                   MOVEL     'S'           I13AF1
127000970714     C                   MOVEL     'S'           I13SZ2
127100970714     C                   MOVEL     'S'           I13LA3
127200970714     C                   MOVEL     'S'           I13SZ3
127300970716     C                   ENDIF
127400970714     C*
127500970714    2C     E14CAP        IFNE      VARCAP
127600970714     C                   CLEAR                   WZV
127700970714     C                   CLEAR                   WZ1
127800970714     C                   CLEAR                   WZ2
127900970714     C                   CLEAR                   WZ3
128000970714     C                   MOVEL     E14CAP        VARCAP
128100970714     C                   END
128200970714     C     E14LOC        IFNE      VARLOC
128300970714     C                   CLEAR                   WZ1
128400970714     C                   CLEAR                   WZ2
128500970714     C                   CLEAR                   WZ3
128600970714     C                   MOVEL     E14LOC        VARLOC
128700970714     C                   END
128800970714     C     E14PRV        IFNE      VARPRV
128900970714     C     E14NAR        ORNE      VARNAR
129000970714     C                   CLEAR                   WZ3
129100970714     C                   MOVEL     E14PRV        VARPRV
129200970714     C                   MOVEL     E14NAR        VARNAR
129300970714     C                   END
129400970714     C                   MOVE      WZV           E13FZV
129500970714     C                   MOVE      WZ1           E13FZ1
129600970714     C                   MOVE      WZ2           E13FZ2
129700970714     C                   MOVE      WZ3           E13FZ3
129800970714     C*
129900970714     C                   CALL      'FNLV13R'
130000970714     C                   PARM                    KPJBA
130100970714     C                   PARM                    DSLV13
130200970714     C                   PARM                    DSSI95
130300970714     C*
130400970714     C                   MOVEL     O13ERR        WERR              1
130500970714     C                   MOVEL     O13MSG        VIDMSG
130600970714     C                   EXSR      CTRERR
130700970714     C*
130800970714     C                   MOVEL     E13FZV        WZV
130900970714     C                   MOVEL     E13FZ1        WZ1
131000970714     C                   MOVEL     E13FZ2        WZ2
131100970714     C                   MOVEL     E13FZ3        WZ3
131200970714     C     *IN90         IFEQ      *ON
131300970714     C                   GOTO      ENDCTR
131400970714     C                   ENDIF
131500970716     C                   ENDIF
131600970714     C*
131700940331     C*
131800970324     C* CONTROLLO IL NUMERO DI FAX
131900981112     C     VIDFAX        IFEQ      'S'
132000981112     C     VI4NFX        ANDEQ     *BLANKS
132100970324     C                   SETON                                        7890
132200970324     C                   GOTO      ENDCTR
132300970324     C                   ENDIF
132400970324     C*
132500041103     c* controllo prima come indirizzo E-mail
132600041103     c     vi4nfx        ifne      *blanks
132700041103     c                   Clear                   dsEml
132800041103     c                   Movel     Vi4nfx        Emaili
132900041103     c                   Call      'XEMAIL'
133000041103     c                   Parm                    dsEml
133100041103      * Errore --> provo come fax
133200041103     c                   If        Emaile <> '0'
133300970324     C                   CLEAR                   TRUL42
133400970324     C                   MOVEL     VI4NFX        D42FAX
133500970324     C                   CALL      'TRUL42R'
133600970324     C                   PARM                    TRUL42
133700970324     C     D42ERR        IFEQ      '1'
133800970324     C                   SETON                                        902879
133900041103     C*****              MOVEL     D42MSG        VIDMSG
134000041103     C                   MOVEL     msg(10)       VIDMSG
134100970324     C                   GOTO      ENDCTR
134200970324     C                   ENDIF
134300041103     c                   else
134400041103     c* no errore --> invio tramite e-mail
134500041103     c                   movel     'S'           wmail             1
134600041103     c                   endif
134700041103     c                   endif
134800050905  x1ac                   else
134900050907     C                   EXSR      CTRD06
135000050907     C   90              GOTO      ENDCTR
135100050907   1Ac                   ENDIF
135200940331     C*
135300940331    0C                   ENDIF
135400970714     C     ENDCTR        TAG
135500970714     C* IMPOSTO I CAMPI DI UNA EVENTUALE RICERCA
135600970714     C                   MOVEL     E14LOC        VI4CIT
135700970714     C                   MOVEL     E14CAP        VI4CAP
135800970714     C                   MOVEL     E14PRV        VI4PRV
135900940224     C*
136000970714     C                   ENDSR
136100970714     C**************************************************************************
136200970714     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
136300970714     C**************************************************************************
136400970714     C     CTRERR        BEGSR
136500970714     C* IMPOSTO CAMPI PASSATI
136600970714     C     O13RON        IFEQ      'S'
136700970714     C                   MOVEL     O95NAR        E14NAR
136800970714     C                   ENDIF
136900970714     C     O13ROC        IFEQ      'S'
137000970714     C                   MOVEL     O95CAP        E14CAP
137100970714     C                   ENDIF
137200970714     C     O13ROP        IFEQ      'S'
137300970714     C                   MOVEL     O95PRV        E14PRV
137400970714     C                   ENDIF
137500970714     C     O13ROL        IFEQ      'S'
137600970714     C                   MOVEL     O95LOC        E14LOC
137700970714     C                   ENDIF
137800970714     C**
137900970714     C* ERRORE O RICERCA CAMPO
138000970714    1C     WERR          IFNE      ' '
138100970714     C*
138200970714    2C     VIDMSG        IFNE      *BLANKS
138300970714     C                   SETON                                        28
138400970714    2C                   ENDIF
138500970714     C* POSIZIONAMENTO CURSORE
138600970714    2C                   SELECT
138700970714    2C     WERR          WHENEQ    '1'
138800970714     C     WERR          OREQ      '2'
138900970714     C                   SETON                                        06
139000970714    2C     WERR          WHENEQ    '3'
139100970714     C                   SETON                                        77
139200970714    2C     WERR          WHENEQ    '4'
139300970714     C                   SETON                                        61
139400970714    2C     WERR          WHENEQ    '5'
139500970714     C                   SETON                                        62
139600970714    2C     WERR          WHENEQ    '6'
139700970714     C                   SETON                                        71
139800970714    2C                   ENDSL
139900970714     C*
140000970714     C                   SETON                                        90
140100970714    1C                   ENDIF
140200970714     C                   ENDSR
140300940307     C*
140400940307     C*--- PROVENGO DA STAMPA/CONVALIDA/ANNULL. TARIFFA/OFFERTA ------*
140500940307     C     RICHIA        BEGSR
140600110201     C**                 SETOFF                                       151617
140700110201     C**                 SETOFF                                       18
140800110201     C                   SETOFF                                       161718
140900940308     C*
141000050912     C                   MOVEL     Rsut          vRSUT
141100950829     C                   MOVEL     KNSIF         NSIF
141200950829     C                   MOVEL     KNMUS         NMUS
141300940308     C                   MOVE      TES(1)        VIDTES
141400940310     C*
141500940310     C* IMPOSTO I CAMPI DELLA PARAM DA PARAM7
141600981112     C                   MOVEL     PA7CLI        I45KSC
141700981112     C                   Z-ADD     PA7PRV        I45NRV
141800981112     C                   MOVEL     PA7NRV        I45NRV
141900981112     C                   MOVEL     PA7CPO        I45CPO
142000981112     C                   MOVEL     PA7AST        I45AST
142100981112     C                   MOVEL     PA7CTR        I45CTR
142200981112     C                   MOVEL     PA7PRG        I45PRG
142300981112     C                   MOVEL     PA7TST        I45TST
142400980127AG*  C* PULISCO IL CAMPO DI COMODO CODICE COMMERCIALE
142500980127AG*  C                   Z-ADD     *ZEROS        CCOM              8 0
142600091021
142700091021      * Se Trattativa
142800091021    1c                   if        i45ast = 'T'
142900091021     c     i45nrv        chain     tivis000                           30
143000091021     C  N30              MOVEL     VISCMM        I45APP
143100091021     C   30              MOVEL     SIMFEL        I45APP
143200091021AG*  C* RECUPERO IL CODICE DEL COMMERCIALE
143300091021AG*  C                   Z-ADD     VISCMM        CCOM
143400091021     c                   movel     viscmm        keypo             3 0
143500091021    1C                   ENDIF
143600961203     C*
143700091021     C* SE CLIENTE
143800981112    1C     I45AST        IFEQ      'C'
143900981217     C                   MOVEL     I45KSC        VIDCLI
144000961203     C     KACO          CHAIN     CNIND00F                           32
144100980127AG*  C* RECUPERO IL CODICE DEL COMMERCIALE
144200980127AG*  C     KACO          CHAIN     CNCLP00F                           OC
144300980127AG*  C  NOC              Z-ADD     CLPAGE        CCOM
144400051026     c  NOC              movel     clpage        keypo
144500961203    1C                   ENDIF
144600961203     C*
144700050908     c* indirizzo e-mail
144800051103     c                   clear                   wntcrnt
144900050908     c                   movel     '06'          ktnt
145000050908     c                   exsr      repntc1
145100050930    4c                   if        %found(tfntc01l) and ntcrnt <> *blanks
145200050908     c* memorizzo che ho trovato un indirizzo
145300050908     c                   eval      wnota06='S'
145400051103     c                   eval      wntcrnt = ntcrnt
145500050908    4c                   endif
145600980127     C*
145700940308     C* PULIZIA CAMPI SFLCONTROL
145800940310     C                   MOVEL     PA7RGS        VI2RAG
145900940308     C                   CLEAR                   VI2NR1
146000940308     C                   CLEAR                   VI2NR2
146100941216     C                   Z-ADD     UDATE8        VI2DST
146200060601      * imposto di default la lingua italiano
146300060601     c                   eval      vi2lin = 'it'
146400060601     c                   clear                   vi2dlin
146500060601      * decodifico la lingua da azlin
146600060601     c     vi2lin        chain     azlin03l
146700060601     c                   if        %found(azlin03l)
146800060601     c                   eval      vi2dlin = lindesita
146900060601     c                   endif
147000980127AG*  C* DECODIFICO IL CODICE COMMERCIALE DA PASSARE AL PROGRAMMA CHE STAMPERA'
147100980127AG*  C* L'ORGANIGRAMMA
147200980127     C     CCOM          IFNE      *ZEROS
147300130731     C                   MOVE      CCOM          CMMcod
147400130731     C     CMMcod        CHAIN     AZCMM000
147500130731     C                   if        %found(AZCMM01L)
147600130731     C                   MOVEL     CMMdes        I45DCM
147700130731     C                   endif
147800130731     C                   ENDIF
147900940307     C*
148000110201     C***  PA7FLG        COMP      '1'                                    15
148100940310     C     PA7FLG        COMP      '2'                                    16
148200940310     C     PA7FLG        COMP      '3'                                    17
148300940310     C     PA7FLG        COMP      '4'                                    18
148400940307     C*
148500940307     C* CARICAMENTO SUBFILE
148600940307     C                   EXSR      CARSFL
148700091110
148800940307     C*
148900940307     C* EMETTO SUBFILE
149000940311    1C     NRR           IFGT      0
149100051116     c                   select
149200051116     c                   when      wflge <> *blanks or
149300051116     c                             (wflgm<>*blanks and wflgs<>*blanks)
149400051116     c                   eval      vi2opz = 'Opzioni: S=Stampa; M=Invio E-mail.'
149500051116     c                   when      wflgm <> *blanks
149600051116     c                   eval      vi2opz = 'Opzioni: M=Invio E-mail.          '
149700051116     c                   when      wflgs <> *blanks
149800051116     c                   eval      vi2opz = 'Opzioni: S=Stampa.                '
149900051116     c                   endsl
150000940307     C*
150100940307     C                   Z-ADD     1             REC
150200940308     C     FOR04         TAG
150300941216     C                   WRITE     LV43Z04
150400940308     C     FOR03         TAG
150500941230     C                   EXFMT     LV43C03
150600961203     C*
150700961203     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
150800961203     C                   CLEAR                   VIDMSG
150900961203     C                   SETOFF                                       2877
151000940307     C*
151100940308     C** CMD12 - RITORNO
151200940308     C   KL              GOTO      ENDRIC
151300940308     C*
151400940308     C** CMD7  - MANUTENZIONE TESTI
151500940311    2C     *INKG         IFEQ      *ON
151600940308     C                   CLEAR                   PARAM4
151700940308     C                   MOVEL     PARAM4        KPJBU
151800941216     C                   CALL      'FNLV40R'
151900940308     C                   PARM                    KPJBA
152000940308     C                   GOTO      FOR04
152100940311    2C                   ENDIF
152200060606      * lingua ricerca
152300060606     c                   if        %scan('?':vi2lin) > 0
152400060606     c                   clear                   p37iso2
152500060606     c                   clear                   p37desi
152600060606     c                   call      'TRUL37R'
152700060606     c                   parm                    p37iso2
152800060606     c                   parm                    p37desi
152900060606     c                   eval      vi2lin = p37iso2
153000060606     c                   eval      vi2dlin = p37desi
153100060606     c     vi2lin        chain     azlin03l
153200060606     c                   goto      for04
153300060606     c                   endif
153400940308     C*
153500940311     C* CONTROLLO DATI SFL E SFLCTL
153600940314     C                   EXSR      CONTR3
153700940308     C*
153800941216     C** CMD6  - ELABORAZIONE
153900941216     C  NKF
154000091125     COR 90              GOTO      FOR04
154100940308     C*
154200940311     C* STAMPO TESTI LETTERE SELEZIONATI
154300051128     c* Devo gestire la possibiolità che nel subfile siano state
154400051128     c* selezioni sia per "M" che per "S".
154500051128     c* In questo caso richiamo la routine 2 volte: prima elaborando le
154600051128     c* "M" e poi le "S". INFATTI NON SI POSSONO ELABORARE NELLO STESSO
154700051128     c* GIRO
154800051128     c                   if        wopz = 'M'
154900940311     C                   EXSR      LEGGI
155000051128     c                   end
155100051128     c                   if        wopz1 = 'S'
155200051128     c                   movel     wopz1         wopz
155300051128     c                   exsr      leggi
155301161223      * Verifico se il flag err_allegati della KPJBA che mi ha passato il pro-
155302161223      * gramma di stampa tariffa è diverso da 1 emetto messaggio di errore
155303161223     c                   If        WErr_allegati <> '1' and
155304161223     c                             WErr_allegati <> ' '
155306161223     C                   SETON                                        9290
155307161223     c                   goto      for03
155308161223     c                   endif
155309161223     C*
155400051128     c                   endif
155500940311    1C                   ENDIF
155600091021
155700091021      * se ho stampato il testo 10 o il testo 11 e sono da trattativa
155800091021      * richiamo trmk17r per nuova attività
155900091021     c                   if        $trmk17
156000091021     c                   exsr      sr_trmk17
156100091022      * se torna errore è perchè non ho trovato il potenziale
156200091022      * devo riemettere la videata del sbfl con l'errore
156300091022     c                   if        omk17err = '1'
156400091022     c                   eval      *in91 = *on
156500091022     c                   eval      *in90 = *on
156600091022     c                   goto      for03
156700091022     c                   endif
156800091021     c                   endif
156900091021
157000940311     C*
157100940308     C     ENDRIC        ENDSR
157200940307     C*
157300940307     C*--- CARICAMENTO SUBFILE ---------------------------------------*
157400940307     C     CARSFL        BEGSR
157500940307     C                   Z-ADD     0             NRR               4 0
157600051116     c                   clear                   wflge             1
157700051116     c                   clear                   wflgs             1
157800051116     c                   clear                   wflgm             1
157900940307     C*
158000940307     C* PULIZIA SUBFILE
158100940307     C                   SETON                                        75
158200941216     C                   WRITE     LV43C03
158300940307     C                   SETOFF                                       75
158400940307     C*
158500940307     C                   MOVEL     '1H'          COD
158600940307     C     KTAB2         SETLL     TABEL
158700940307     C     KTAB2         READE     TABEL                                  30
158800940307     C*
158900940311    1C     *IN30         DOWEQ     *OFF
159000091021     C                   SETOFF                                       2011
159100940307     C* ESCLUDO ANNULLATI
159200940311    2C     TBLFLG        IFEQ      ' '
159300940307     C                   MOVEL     TBLUNI        DS1H
159400940307     C*
159500940307     C* LETTERA DA PRESENTARE A MENU'
159600110201     C** 15§1HFL1        COMP      'N'                                    20
159700940307     C* LETTERA DA PRESENTARE NELLA STAMPA      OFFERTA/TARIFFA
159800940307     C   16§1HFL2        COMP      'N'                                    20
159900940307     C* LETTERA DA PRESENTARE NELLA CONVALIDA   OFFERTA
160000940307     C   17§1HFL3        COMP      'N'                                    20
160100940307     C* LETTERA DA PRESENTARE NELL'ANNULLAMENTO OFFERTA/TARIFFA
160200940307     C   18§1HFL4        COMP      'N'                                    20
160300030529     C* Se provengo da annullamento offerta/tariffa non carico il testo
160400030529     C* lettera se si tratta di tariffa
160500030529     C   18
160600030529     CANN20PA7AST        COMP      'C'                                    20
160700940307     C*
160800940307     C* 20 OFF - CARICO IL TESTO
160900940311    3C     *IN20         IFEQ      *OFF
161000030328      ***** ND
161100030328      * Richiamo FnLv41 per creare file testi
161200030328     C                   CLEAR                   Fnlv41ds
161300030328     C                   MOVEL     '3'           I41MOD
161400030328     C                   MOVEL     TBLKEY        I41LET
161500030328     C                   CALL      'FNLV41R'
161600030328     C                   PARM                    FNLV41DS
161700030402     C     O41ERR        IFNE      'E'
161800030402     C                   MOVEL     O41DTA        DS1H
161900030402     C                   ENDIF
162000030328      *****
162100940307     C                   MOVEL     ' '           VI2SCE
162200940307     C                   MOVEL     TBLKEY        VI2TTX
162300940307     C                   MOVEL     §1HDES        DE2TTX
162400940311     C                   MOVEL     §1HNCO        VI2NCO
162500960624     C     §1HRSR        IFEQ      'S'
162600960624     C                   SETON                                        10
162700960624     C                   MOVEL     '1'           VH2I10
162800960624     C                   CLEAR                   VI2RID
162900960624     C                   ELSE
163000960624     C                   SETOFF                                       10
163100960624     C                   MOVEL     '0'           VH2I10
163200960624     C                   ENDIF
163300960624     C*
163400950905     C                   MOVEL     'T'           VI2DET
163500091021     C                   MOVEL     'N'           VI2TPA            1
163600950905     C                   CLEAR                   VI2TSL
163700950905     C                   CLEAR                   VI2DSL
163800950907     C                   CLEAR                   VI2TS2
163900950907     C                   CLEAR                   VI2DS2
164000940307     C                   MOVEL     §1HPVA        SAVPVA
164100940307     C                   MOVEL     §1HPGS        SAVPGS
164200050912     C                   MOVEL     §1Html        S1html
164300060606     c                   eval      s1hlin = §1hlin
164400051116     c* memorizzo flag in campo hidden per poter controllare le opzioni
164500051116     c* eseguibili per cod.testo
164600051116     c                   clear                   vh2flg
164700110201     c** 15              movel     §1hfl1        vh2flg
164800051116     c   16              movel     §1hfl2        vh2flg
164900051116     C   17              movel     §1hfl3        vh2flg
165000051116     C   18              movel     §1hfl4        vh2flg
165100051116     c* imposto i flags per sapere che opzioni proporre a video
165200051116     c                   select
165300051116     c                   when      vh2flg = 'S'
165400051116     c                   eval      wflgs = 'S'
165500051116     c                   when      vh2flg = 'M'
165600051116     c                   eval      wflgm = 'S'
165700051116     c                   when      vh2flg = 'E'
165800051116     c                   eval      wflge = 'S'
165900051116     c                   endsl
166000051116     c
166100940307     C*
166200950829     C* 11 ON  - EMETTO LA RICHIESTA DELLA VISUALIZZAZIONE DELLE RIGHE
166300950829     C*   DI DETTAGLIO IN STAMPA
166400091021     C     VI2TTX        IfEQ      '10'
166500050131     C     VI2TTX        OREQ      '11'
166600000829     C                   SETON                                        11
166700091021     C     VI2TTX        IfEQ      '10'
166800950905     C                   MOVEA     TSL(1)        VI2TSL
166900950905     C                   MOVEL     CDSL          VI2DSL
167000050131     C                   ENDIF
167100050131     C     VI2TTX        IFEQ      '11'
167200050131     C                   MOVEA     TSL(3)        VI2TSL
167300050131     C                   MOVEL     CDSLF         VI2DSL
167400050131     C                   ENDIF
167500950829     C                   ENDIF
167600950829     C*
167700940307     C                   ADD       1             NRR
167800941216     C                   WRITE     LV43S03
167900940311    3C                   ENDIF
168000940307     C*
168100940311    2C                   ENDIF
168200940311     C     KTAB2         READE     TABEL                                  30
168300940311    1C                   ENDDO
168400940307     C*
168500940307     C                   ENDSR
168600940307     C*
168700940311     C*--- CONTROLLO SFL E SFLCTL ------------------------------------*
168800940314     C     CONTR3        BEGSR
168900940308     C                   SETOFF                                       90
169000940311     C******
169100940311     C** S U B F I L E    C O N T R O L
169200940311     C******
169300940308     C****  DATA STAMPA  ****
169400061207    1C     VI2DST        IFeq      0
169500061207     C                   SETON                                        5690
169600061207     C                   GOTO      ENDCT2
169700061207    2C                   ENDIF
169800940311    1C     VI2DST        IFNE      0
169900940308     C                   MOVE      VI2DST        G02DAT
170000940308     C                   MOVEL     *BLANKS       G02ERR
170100941216     C                   CALL      'XSRDA8'
170200940308     C                   PARM                    WLBDAT
170300940311    2C     G02ERR        IFEQ      '1'
170400940308     C                   SETON                                        5690
170500940311     C                   GOTO      ENDCT2
170600940311    2C                   ENDIF
170700940309     C                   Z-ADD     G02INV        COMDST
170800941216     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
170900941216     C                   Z-ADD     G02DAT        VI2DST
171000940311    1C                   ENDIF
171100060601
171200060601      * lingua obbligatoria
171300060601     c                   if        vi2lin = *blanks
171400060601     c                   eval      *in82 = *on
171500060601     c                   eval      *in90 = *on
171600060601     c                   leavesr
171700060601     c                   endif
171800060601      * lingua controllo
171900060601     c                   clear                   vi2dlin
172000060601     c     vi2lin        chain     azlin03l
172100060601     c                   if        not %found(azlin03l)
172200060601     c                   eval      *in82 = *on
172300060601     c                   eval      *in90 = *on
172400060601     c                   leavesr
172500060601     c                   endif
172600060601     c                   eval      vi2dlin = lindesita
172700060606      * se non è attiva per i testi errore
172800060606     c                   if        linfntxt = '0'
172900060606     c                   eval      *in82 = *on
173000060606     c                   eval      *in90 = *on
173100060606     c                   leavesr
173200060606     c                   endif
173300961203     C*
173400940311     C******
173500940311     C** S U B F I L E
173600940311     C******
173700940311     C                   Z-ADD     1             NRR
173800051024     c                   clear                   wopz              1
173900051128     c                   clear                   wopz1             1
174000051104     c                   clear                   wtxt07            1
174100941216     C     NRR           CHAIN     LV43S03                            30
174200940311     C*
174300950829    1C     *IN30         DOWEQ     *OFF
174400091021     C                   SETOFF                                       11
174500950829     C*
174600950829     C* 11 ON  - EMETTO LA RICHIESTA DELLA VISUALIZZAZIONE DELLE RIGHE
174700950829     C*   DI DETTAGLIO IN STAMPA
174800091021     C     VI2TTX        IFEQ      '10'
174900050131     C     VI2TTX        OREQ      '11'
175000000829     C* COLARI DALLA ATRIFFA DI CARTELLO
175100000829     C                   SETON                                        11
175200950829     C                   ENDIF
175300960624     C**
175400960624     C                   MOVEL     VH2I10        *IN10
175500950829     C*
175600940311     C****  NUMERO COPIE  ****
175700940311    2C     VI2NCO        IFEQ      0
175800940311     C     VI2NCO        ORGT      255
175900940311     C                   SETON                                        5590
176000940311    2C                   ENDIF
176100950829     C*
176200950829     C****  STAMPA DETTAGLIO  ****
176300950905    2C  N90VI2DET        IFNE      'T'
176400950829     C     VI2DET        ANDNE     'L'
176500950905     C     VI2DET        ANDNE     'A'
176600950829     C                   SETON                                        7290
176700950829    2C                   ENDIF
176800940311     C*
176900050930     c* se testo html e non ho indirizzo e-mail errore
177000051026     c  n90s1html        ifeq      'S'
177100051104     c     vi2sce        andne     *blanks
177200050930     c     wnota06       andeq     *blanks
177300050930     c                   seton                                        8490
177400050930     c                   endif
177500051104     c* testo html non selezionabile con "M"
177600051104     c  n90s1html        ifeq      'S'
177700051104     c     vi2sce        andeq     'M'
177800051104     c                   seton                                        2490
177900051104     c                   endif
178000051128     c* memorizzo se effettuate scelte con "M"
178100051128     c                   if        vi2sce = 'M'  and wopz = ' '
178200051128     c                   movel     vi2sce        wopz
178300051128     c                   endif
178400051128     c* memorizzo se effettuate scelte con "S"
178500051128     c                   if        vi2sce = 'S'  and wopz1 = ' '
178600051128     c                   movel     vi2sce        wopz1
178700051128     c                   endif
178800051104     c* memorizzo se è stato scelto testo 07 se invio con e-mail perchè
178900051104     c* è obbligatorio sceglierlo
179000051104     c  n90vi2ttx        ifeq      '07'
179100051104     c     vi2sce        andeq     'M'
179200051104     c                   eval      wtxt07 = 'S'
179300051104     c                   endif
179400051116     c* errore se invio con e-mail e testo non inviabile con e-mail
179500051116     c  n90              if        vi2sce = 'M' and vh2flg = 'S'
179600051115     c                   seton                                        2790
179700051115     c                   endif
179800051116     c* errore se richiesta la stampa e testo non stampabile
179900051116     c  n90              if        vi2sce = 'S' and vh2flg = 'M'
180000051116     c                   seton                                        2990
180100051116     c                   endif
180200051116     c* errore se non ho ind. e-mail e selezionato con "M"--> invio e-mail
180300091106     c****  n90wnota06       ifeq      *blanks
180400091106     c****     wopz          andeq     'M'
180500091106     c****                   seton                                        8790
180600091106     c****                   endif
180700051115     c
180800060606      * se ho scelto di stampare in lingua il testo deve essere gestito
180900060606      * in lingua
181000060606     c                   if        not *in90 and vi2lin <> 'it' and
181100060606     c                             vi2sce <> *blanks and s1hlin = *blanks
181200060606     c                   eval      *in89 = *on
181300060606     c                   eval      *in90 = *on
181400060606     c                   endif
181500051104     c
181600950907     C*
181700940311     C   90              Z-ADD     NRR           REC
181800941216     C                   UPDATE    LV43S03
181900940311     C   90              SETON                                        30
182000940311     C*
182100940311     C                   ADD       1             NRR
182200941216     C  N90NRR           CHAIN     LV43S03                            30
182300940311    1C                   ENDDO
182400051110     c
182500051110     c  n90              if        wopz ='M'  and wtxt07 = *blanks
182600051104     c                   seton                                        2590
182700051104     c                   endif
182800051110     c* se richiesto invio automatico a mezzo e-mail errore se non
182900051110     c* trovo record su tabella mra
183000051110     c  n90              if        wopz = 'M'
183100051110     c                   exsr      carmra
183200051110     c   90              seton                                        26
183300051110     c                   endif
183400091106
183500091125      * devo richiedere l'indirizzo mail se F6-Conferma
183600091125     c  n90              if        wopz = 'M' and *inkf
183700091106     c                   exsr      sr_indmail
183800091109     c                   if        LV44f12 <> '1'
183900091106     c                   eval      wntcrnt = LV44mail
184000100507     c                   eval      ccom    = LV44ccmm
184100091106     c                   eval      wnota06 = 'S'
184200091125     c                   else
184300091125     c                   eval      *inkf = '0'
184400091109     c                   endif
184500091106     c                   endif
184600091106
184700940311     C*
184800940311     C     ENDCT2        ENDSR
184900940308     C*
185000051025     C*--- LANCIO TESTI LETTERE SELEZIONATI --------------------------*
185100940311     C     LEGGI         BEGSR
185200091021
185300091021     c                   eval      $trmk17 = *off
185400091021
185500940307     C                   Z-ADD     1             NRR
185600050908     c                   clear                   wvolta            1
185700051024     c* Se scelto invio atomatico a mezzo e-mail richiamo routine per
185800051024     c* srcittura testo che costituirà il corpo della mail e per memorizzare
185900051024     c* i dati chiave dello spool da passare poi al pgm che effettua la
186000051024     c* costruzione/invio della mail
186100051024     c                   if        wopz = 'M'
186200060201     c                   clear                   joutqlib
186300060201     c                   clear                   joutq
186400060201     c                   call      'FNLV43C'
186500060201     c                   parm      'E'           jmod              1
186600060201     c                   parm                    joutqlib         10
186700060201     c                   parm                    joutq            10
186800060201     c*
186900051024     c                   exsr      SR_tappo
187000051024     c*
187100051024     c* riprendo orario iniziale che mi servirà per reperimento dati
187200051024     c* dei prossimi spool che si andranno a creare
187300051107     C** DLYJOB
187400051107     C                   MOVEL     *BLANKS       COMMAN           80
187500051107     c                   z-add     15            lung             15 5
187600051107     C                   MOVEL     CMD1(1)       COMMAN
187700051107     C                   CALL      'QCMDEXC'
187800051107     C                   PARM                    COMMAN
187900051107     C                   PARM                    LUNG
188000051107     c*
188100051024     c                   time                    timeini
188200051024     c                   endif
188300940307     C*
188400941216     C     NRR           CHAIN     LV43S03                            30
188500940307     C*
188600940311    1C     *IN30         DOWEQ     *OFF
188700940311     C*
188800051026     c* se il testo è lo 07 lo salto se sono in modalità di invio automatico
188900051026     c* e-mail in quanto già elaborato nella routine sr_tappo
189000051026   2ac                   if        vi2sce = 'M' and vi2ttx = '07'
189100051026     c                   else
189200940307     C* CONTROLLO SE TESTO SELEZIONATO
189300051024    2C     VI2SCE        IFne      *blanks
189400051128     c     vi2sce        andeq     wopz
189500940307     C*
189600940307     C* SE CI SONO PARAMETRI VARIABILI PRESENTO IL TESTO DELLA LETTERA
189700940307     C*   RICHIAMANDO IL PGM DI MANUTENZIONE TESTI
189800940311    3C     SAVPVA        IFEQ      'S'
189900050912     c     s1html        andeq     *blanks
190000940307     C                   CLEAR                   PARAM4
190100940308     C                   MOVEL     '1'           PA4FLG
190200940307     C                   MOVEL     VI2TTX        PA4TTX
190300060607     c                   move      liniso2       pa4lin
190400940307     C                   MOVEL     PARAM4        KPJBU
190500941216     C                   CALL      'FNLV40R'
190600940307     C                   PARM                    KPJBA
190700940311    3C                   ENDIF
190800091021
190900091021      * se da trattativa e sto stampando il testo 10 o 11 memorizzo flag per
191000091021      * richiamo pgm trmk17r immissione attività
191100100419      * e se trattativa non è fittizia
191200100419     c                   if        i45ast = 'T' and visffz <> 'S' and
191300091021     c                             (vi2ttx = '10' or vi2ttx = '11')
191400091021     c                   eval      $trmk17 = *on
191500091021     c                   endif
191600940307     C*
191700940309     C* PASSAGGIO PARAMETRI
191800981112     C                   MOVEL     VI2TTX        I45TTX
191900981112     C                   MOVEL     VI2NR1        I45NR1
192000981112     C                   MOVEL     VI2NR2        I45NR2
192100981112     C                   MOVEL     VI2NCO        I45NCO
192200981112     C                   MOVEL     VI2DST        I45GDS
192300981112     C                   MOVEL     COMDST        I45ADS
192400981112     C                   MOVEL     SAVPGS        I45PGM
192500981112     C                   MOVEL     VI2RID        I45RID
192600981112     C                   MOVEL     VI2TPA        I45TPA
192700981112     C                   MOVEL     VI2DET        I45DET
192800110201     c     i45ast        ifne      'T'
192900981116     C                   MOVEL     I45KSC        I45APP
193000940627     C                   ENDIF
193100041021     C                   MOVEL     dutKCI        I45KCC
193200060601     c                   eval      i45lin = linfntxt
193300050912     c                   movel     s1html        i45htm
193400050908     c* variabili per testo html se necessari
193500050912     c                   if        s1html  <> *blanks
193600060714      * devo passare l'oggetto in lingua
193700060714     c                   select
193800060714     c                   when      vi2lin = 'it'
193900110504     C                   eval      i45obj='Comunicazione da BRT S.p.A.'
194000060714     c                   when      vi2lin = 'en'
194100110504     C                   eval      i45obj='Comunication by BRT S.p.A.'
194200060714     c                   when      vi2lin = 'fr'
194300110504     C                   eval      i45obj='Comunicazione da BRT S.p.A.'
194400060714     c                   when      vi2lin = 'de'
194500110504     C                   eval      i45obj='Comunicazione da BRT S.p.A.'
194600060714     c                   endsl
194700050908     c                   endif
194701161223      * se stampo tariffe o offerte passo nel campo i45fax M=Mail se
194702161223      * scelgo di inviare la mail altrimenti lascio blank altrimenti
194703161223      * il programma di stampa tariffe stampa sempre gli allegati
194704161223     c                   if        vi2ttx = '10' or vi2ttx = '11'
194705161223     c                   if        vi2sce = 'M'
194706161223     c                   eval      i45fax = 'M'
194707161223     c                   else
194708161223     c                   clear                   i45fax
194709161223     c                   endif
194710161223     c                   endif
194800940309     C*
194900940309     C* LANCIO PGM
195000981113     C                   MOVEL     DSLV45        KPJBU
195100941216     C                   CALL      'FNLV45C'
195200940307     C                   PARM                    KPJBA
195300981113     C                   PARM                    DSLV45
195301161223      * Verifico se il flag err_allegati della KPJBA che mi ha passato il pro-
195302161223      * gramma di stampa tariffa è diverso da 1 vado a fine lettura
195303161223     c                   If        Err_allegati <> '1' and
195304161223     c                             Err_allegati <> ' ' and
195305161223     c                             (vi2ttx = '10' or vi2ttx = '11')
195307161223     c                   eval      Werr_allegati = Err_allegati
195310161223     c                   endif
195311161223     c                   clear                   vi2sce
195312161223     c                   update    lv43s03
195313161223     C*
195314161223     c
195400940311    2C                   ENDIF
195500051026   2aC                   ENDIF
195600940311     C*
195700940307     C                   ADD       1             NRR
195800941216     C     NRR           CHAIN     LV43S03                            30
195900940311    1C                   ENDDO
196000051026     c*
196100051024     c                   if        wopz = 'M'
196200060201     c*
196300060201     c                   call      'FNLV43C'
196400060201     c                   parm      'C'           jmod              1
196500060201     c                   parm                    joutqlib         10
196600060201     c                   parm                    joutq            10
196700060201     c*
196800051024     c                   time                    timeend
196900051025     c                   exsr      sr_CLstspl
197000051025     c* memorizzo a partire dal secondo elemento perchè nel primo ho memoriz
197100051025     c* zato i dati dello spool "tappo"
197200051025     C                   if        §LST_ESITO <> 'E'
197300051103     C                   movea     §LST_LISTA    wskSpoolid
197400051025     C                   endif
197500051103     c* aggiungo come ultimo elemento della schiera i dati dello spool tappo
197600051103     c                   z-add     1             xx                4 0
197700051103     c     *blanks       lookup    wskspoolid(xx)                         30
197800051103     c                   if        *in30 = *on
197900051103     c                   movel     xlstsplds2    wskspoolid(xx)
198000051103     c                   endif
198100051103     c*
198200051103     C                   z-add     1             wIdx              5 0
198300051103     C                   clear                   skspoolid
198400051103     C                   dow       wIdx <= %elem(wskspoolid)
198500051103     C                   if        wskspoolid(wIdx) = *blanks
198600051103     C                   leave
198700051103     C                   else
198800051108     c                   if        widx <> xx
198900051103     C                   eval      skspoolid(wIdx) = wskspoolid(wIdx)
199000051108     c                   else
199100051108     c* per spool tappo aggiungo anche la regola
199200051108     C                   eval      skspoolid(wIdx) = wskspoolid(wIdx)  +
199300051110     c                             §mrareg
199400051108     c                   endif
199500051103     C                   endif
199600051103     C                   add       1             wIdx
199700051103     C                   enddo
199800051103     c*
199900051103     c
200000051103     c
200100051025     c* col codice commerciale reperisco indirizzo e-mail mittente da passar
200200051025     c* a pgm che invia la mail (trul83r)
200300110203     c                   move      ccom          ccomx             7 0
200400051026     c                   exsr      RepEmaMit
200500060714      * in lingua
200600060714     c                   select
200700060714     c                   when      vi2lin = 'it'
200800051103     c                   eval      emailobj = 'Proposta Tariffaria'
200900060714     c                   when      vi2lin = 'en'
201000060714     C                   eval      emailobj = 'Tariff Proposal'
201100060714     c                   when      vi2lin = 'fr'
201200060912     C                   eval      emailobj = 'Offre tarifaire'
201300060714     c                   when      vi2lin = 'de'
201400060912     C                   eval      emailobj = 'Preisangebot'
201500060714     c                   endsl
201600051103     c                   eval      emaildest = wntcrnt
201700051103     c* richiamo pgm di invio e-mail al cliente
201800051103     C                   CALL      'TRUL83R1'
201900051103     C                   PARM      *blanks       SKSPOOL
202000051103     C                   PARM                    SKSPOOLID
202100051103     C                   PARM      'S'           FLGSND            1
202200051103     C                   PARM                    wemn
202300051103     C                   PARM                    EmailDest       110
202400051103     C                   PARM                    EmailObj         55
202500051103     C                   PARM      *blanks       EmailTxt1        75
202600051103     C                   PARM      *blanks       EmailTxt2        75
202700051103     C                   PARM      *blanks       EmailTxt3        75
202800051103     C                   PARM      *blanks       EmailTxt4        75
202900051103     C                   PARM      *blanks       EmailTxt5        75
203000051103     C                   PARM      *blanks       EmailTxt6        75
203100051103     C                   PARM      *blanks       EmailTxt7        75
203200051103     C                   PARM      *blanks       EmailTxt8        75
203300051103     C                   PARM      'S'           FlgTappoLast      1
203400051103     C                   PARM      'S'           FlgBypChSp        1
203500051103     C                   PARM      'N'           FlgDspVid         1
203600051103     C                   PARM      'S'           FlgDltSplIn       1
203700051103     C                   PARM      '0'           ESITO             1
203800051103     C                   PARM      *blanks       Messaggio        50
203900051026     c                   endif
204000940307     C*
204100940311     C                   ENDSR
204200091106
204300091106      *--------------------------------------------------------------*
204400091106      *  Richiamo pgm x richiesta indirizzo mail                     *
204500091106      *--------------------------------------------------------------*
204600091106     c     sr_indmail    begsr
204700091106
204800091106     c                   clear                   fnlv44ds
204900091106     c                   eval      lv44mail = wntcrnt
205000091106     c                   eval      lv44ccmm = ccom
205100091106     c                   eval      lv44dcmm = i45dcm
205200100511     c                   eval      lv44pcmm = 'S'
205300091106     c                   call      'FNLV44R'
205400100519     c                   parm                    kpjba
205500091106     c                   parm                    fnlv44ds
205600091106
205700091106     c                   endsr
205800091106
205900091021      *--------------------------------------------------------------*
206000091021      *  Richiamo trmk17r per nuova attività                         *
206100091021      *--------------------------------------------------------------*
206200091021     c     sr_trmk17     begsr
206300091021
206400091021     c                   clear                   kpjbu
206500091021     c                   clear                   trmk17ds
206600091230     c                   eval      imk17cpo = i45cpo
206700091230     c                   eval      imk17ksc = i45ksc
206800091021     c                   eval      imk17nrv = i45nrv
206900091021     c                   eval      imk17ctr = %editc(i45ctr:'X')
207000091204     c                   eval      imk17prg = %editc(i45prg:'X')
207100091230     c                   eval      imk17cmm = ccom
207200100311     c                   eval      imk17stp = 'S'
207300091230     c                   call      'TRMK17R'
207400091021     c                   parm                    kpjba
207500091021     c                   parm                    trmk17ds
207600091021
207700091021     c                   endsr
207800981112     C**
207900981112     C* CONTROLLO CODICE COMMERCIALE --------------------------------*
208000981112     C     CTRCMM        BEGSR
208100981112     C     '?'           SCAN      WCMM                                   90
208200981112     C*
208300981112    1C     *IN90         IFEQ      *ON
208400130731     C                   MOVEL     WCMM          VIDAGE
208500130731     C                   CLEAR                   TRMK43ds
208600130731     C                   CALL      'TRMK43R'
208700010620     C                   PARM                    KPJBA
208800130731     C                   PARM                    TRMK43ds
208900130731     c                   if        oMK43err = *off  and
209000130731     c                             oMK43fxx = *blank
209100130731     C                   MOVEL     oMK43cmm      WCMM
209200130731     C                   MOVEL     oMK43des      WDESCM           30
209300130731     c                   endif
209400981112     C                   GOTO      ENDCMM
209500981112    1C                   ENDIF
209600981112     C*
209700981112    1C     WCMM          IFNE      *BLANKS
209800981112     C     WCMM          ANDNE     *ZEROS
209900981112     C*
210000981112     C* CONTROLLO VALIDITA' DEL CODICE COMMERCIALE IMMESSO
210100981112     C                   TESTN                   WCMM                 35
210200981112    2C     *IN35         IFEQ      *OFF
210300981112     C                   SETON                                        2890
210400981112     C                   MOVEL     MSG(5)        VIDMSG
210500981112     C                   GOTO      ENDCMM
210600981112    2C                   ENDIF
210700981112     C*
210800981112     C* CONTROLLO SE ESISTE IN TABELLA '01'
210900130731     C                   MOVE      WCMM          CMMcod
211000130731     C     CMMcod        CHAIN     AZCMM000
211100130731     C*
211200981112     C* CODICE COMMERCIALE ERRATO
211300130731     C                   IF        Not %found(AZCMM01L)
211400981112     C                   SETON                                        2890
211500981112     C                   MOVEL     MSG(5)        VIDMSG
211600981112     C                   GOTO      ENDCMM
211700981112     C                   ENDIF
211800981112     C*
211900981112     C* DECODIFICA CODICE COMMERCIALE
212000130731     C                   MOVEL     CMMdes        WDESCM
212100981112     C*
212200981112   X1C                   ELSE
212300981112     C                   CLEAR                   WCMM
212400981112     C                   CLEAR                   WDESCM
212500981112    1C                   ENDIF
212600981112     C     ENDCMM        ENDSR
212700050905     C**
212800050905     C* Inizializzazione videata richiesta variabili testo html -----*
212900050905     C     rie06         BEGSR
213000050905     c                   clear                   v06nmc
213100050905     c                   clear                   v06emc
213200050905     c                   clear                   v06nmn
213300050905     c                   clear                   v06emn
213400050912     c*****              clear                   v06obj
213500050930     c* nominativo cliente di default
213600050907     c                   eval      v06nmc = 'Responsabile Trasporti'
213700050930     c* oggetto di dafault
213800110504     C                   eval      vidobj='Comunicazione da BRT S.p.A.'
213900050905     c                   endsr
214000050907     C* controlli videata richiesta variabili testo html -----*
214100050907     C     ctrd06        BEGSR
214200050907     c                   setoff                                       091421
214300050907     c                   setoff                                       2223
214400050907     c* nominativo obbligatorio
214500050907     c                   if        v06nmc = *blanks
214600050907     c                   seton                                        902809
214700050907     c                   movel     msg(11)       vidmsg
214800050907     c                   goto      endc06
214900050907     c                   endif
215000050907     c* E-mail obbligatorio + controllo formale
215100050907     c                   if        v06emc = *blanks
215200050907     c                   seton                                        902814
215300050907     c                   movel     msg(12)       vidmsg
215400050907     c                   goto      endc06
215500050907     c                   endif
215600050907     c                   Clear                   dsEml
215700050907     c                   Movel     V06emc        Emaili
215800050907     c                   call      'XEMAIL'
215900050907     c                   Parm                    dsEml
216000050907     c                   If        Emaile <> '0'
216100050907     c                   seton                                        902814
216200050907     c                   movel     msg(12)       vidmsg
216300050907     c                   goto      endc06
216400050907     c                   endif
216500050907     c* NS RIFERIMENTI
216600050907     c                   if        v06nmn = *blanks
216700050907     c                   seton                                        902821
216800050907     c                   movel     msg(11)       vidmsg
216900050907     c                   goto      endc06
217000050907     c                   endif
217100050907     c                   if        v06emn = *blanks
217200050907     c                   seton                                        902822
217300050907     c                   movel     msg(12)       vidmsg
217400050907     c                   goto      endc06
217500050907     c                   endif
217600050907     c                   Clear                   dsEml
217700050907     c                   Movel     V06emn        Emaili
217800120220     c                   eval      emaili = %trim(emaili) + '@BRT.it'
217900050907     c                   call      'XEMAIL'
218000050907     c                   Parm                    dsEml
218100050907     c                   If        Emaile <> '0'
218200050907     c                   seton                                        902822
218300050907     c                   movel     msg(12)       vidmsg
218400050907     c                   goto      endc06
218500050907     c                   endif
218600050907     c* oggetto mail
218700050912     c                   if        vidobj = *blanks
218800050907     c                   seton                                        902823
218900050907     c                   movel     msg(13)       vidmsg
219000050907     c                   goto      endc06
219100050907     c                   endif
219200050907     c
219300050907     c     endc06        endsr
219400050907     c* Reperimento nota ----------------------------------------------
219500050907     c     repntc        begsr
219600050907     C* C L I E N T E
219700050907     c                   movel     'C'           kapl
219800050907     c                   movel     dutkci        knk1
219900050907     c                   move      vidcli        knk1
220000050907     c     kntc          chain     tfntc01l
220100050907     c                   endsr
220200050908     c* Reperimento nota (da sfl) -------------------------------------
220300050908     c     repntc1       begsr
220400091113     C* T R A T T A T I V A
220500091113     c     i45ast        ifeq      'T'
220600091113     c                   movel     'T'           kapl
220700091113     c                   movel     dutkci        knk1
220800091113     c                   move      i45nrv        knk1
220900091113     c     kntc          chain     tfntc01l
221000091113     c                   if        not %found(tfntc01l)
221100091113     c                             and visksc > 0
221200091113     c                   movel     'C'           kapl
221300091113     c                   movel     dutkci        knk1
221400091113     c                   move      visksc        knk1
221500091113     c     kntc          chain     tfntc01l
221600091113     c                   endif
221700091113     c                   endif
221800050908     C* C L I E N T E
221900050908     c     i45ast        ifeq      'C'
222000050908     c                   movel     'C'           kapl
222100050908     c                   movel     dutkci        knk1
222200050908     c                   move      i45ksc        knk1
222300050908     c     kntc          chain     tfntc01l
222400050908     c                   endif
222500050908     c                   endsr
222600940307     C*---------------------------------------------------------------*
222700051024     c* Stampa corpo e-mail e memorizzazione dati spool "tappo" ------------
222800051024     c     SR_TAPPO      begsr
222900051024     c* memorizzo data/ora inizio prima della stampa del testo 07
223000051024     c                   time                    timeini          14 0
223100051024     c* richiamo pgm per stampa del testo 07 che sarà il corpo della mail
223200051024     C                   CLEAR                   Fnlv41ds
223300051024     C                   MOVEL     '3'           I41MOD
223400051024     C                   MOVEL     '07'          I41LET
223500051024     C                   CALL      'FNLV41R'
223600051024     C                   PARM                    FNLV41DS
223700051024    1C     O41ERR        IFNE      'E'
223800051024     C                   MOVEL     O41DTA        DS1H
223900051024    1C                   ENDIF
224000051024     C* SE CI SONO PARAMETRI VARIABILI PRESENTO IL TESTO DELLA LETTERA
224100051024     C*   RICHIAMANDO IL PGM DI MANUTENZIONE TESTI
224200051024    1C     §1hPVA        IFEQ      'S'
224300051024     c     §1html        andeq     *blanks
224400051024     C                   CLEAR                   PARAM4
224500051024     C                   MOVEL     '1'           PA4FLG
224600051024     C                   MOVEL     '07'          PA4TTX
224700060607     c                   move      liniso2       pa4lin
224800060607      * se ho richiesto la stampa in lingua e il testo non è gestito in lingua
224900060607      * passo come lingua italiano
225000060607     c                   if        linfntxt <> 'I' and §1hlin = *blanks
225100060607     c                   move      'it'          pa4lin
225200060607     c                   endif
225300051024     C                   MOVEL     PARAM4        KPJBU
225400051024     C                   CALL      'FNLV40R'
225500051024     C                   PARM                    KPJBA
225600051024    1C                   ENDIF
225700051024     C                   MOVEL     '07'          I45TTX
225800051024     C                   MOVEL     VI2NR1        I45NR1
225900051024     C                   MOVEL     VI2NR2        I45NR2
226000051024     c                   z-add     1             i45nco
226100051024     C                   MOVEL     VI2DST        I45GDS
226200051024     C                   MOVEL     COMDST        I45ADS
226300051024     C                   MOVEL     §1hPGS        I45PGM
226400110201     c     i45ast        ifne      'T'
226500051024     C                   MOVEL     I45KSC        I45APP
226600051024    1C                   ENDIF
226700051024     C                   MOVEL     dutKCI        I45KCC
226800060607     c                   eval      i45lin = linfntxt
226900060607     c                   if        linfntxt <> 'I' and §1hlin = *blanks
227000060607     c                   move      'I'           i45lin
227100060607     c                   endif
227200051024     c                   movel     §1html        i45htm
227300051024     C                   MOVEL     DSLV45        KPJBU
227400051024     C                   CALL      'FNLV45C'
227500051024     C                   PARM                    KPJBA
227600051024     C                   PARM                    DSLV45
227700051024     c
227800051024     c* memorizzo data ora/fine dopo la stampa del testo 07
227900051024     c                   time                    timeend          14 0
228000051024     c* richiamo pgm per reperire le informazioni sullo spool che memorizzo
228100051024     c* nei campi relativi lo "spool tappo" da passare a pgm che si occupa
228200051024     c* della trasmissione della mail
228300051025     c                   exsr      sr_CLstspl
228400051025     C                   if        §LST_ESITO <> 'E'
228500051103     C                   movel     §LST_LISTA    xlstsplds2
228600051025     C                   endif
228700051024     c                   endsr
228800051025     C*---------------------------------------------------------------*
228900051025     c* Richiamo pgm di reperimento lista spool del lavoro -----------------
229000051025     c     sr_CLstspl    begsr
229100051025     C* Valorizzo la ds d procedura x il *pgm utilità reperimento lista spool
229200051025     C                   clear                   XLSTSPLDS
229300051025     C                   eval      §LST_USER  = knmus
229400051025     C                   eval      §LST_QJOBN = '*'
229500051025     C                   CLEAR                   WLBDAT
229600051025     C                   MOVE      timeini       G02DAT
229700051025     C                   CALL      'XSRDA8'
229800051025     C                   PARM                    WLBDAT
229900051025     C                   move      G02INV        §lst_datda
230000051025     c                   movel     '1'           §lst_datda
230100051025     c                   movel     timeini       §lst_orada
230200051025     C                   CLEAR                   WLBDAT
230300051025     C                   MOVE      timeend       G02DAT
230400051025     C                   CALL      'XSRDA8'
230500051025     C                   PARM                    WLBDAT
230600051025     C                   move      G02INV        §lst_data
230700051025     c                   movel     '1'           §lst_data
230800051025     c                   movel     timeend       §lst_oraa
230900051128     c                   z-add     1             §LST_PAGDA
231000051128     c                   z-add     9999          §LST_PAGA
231100051025     C                   call      'XLSTSPL'
231200051025     C                   parm                    XLSTSPLDS
231300051025     c                   endsr
231400051026     C*---------------------------------------------------------------*
231500051026     c* reperimento indirizzo e-mail del commerciale ------------------
231600051026     c     RepEmaMit     begsr
231700051026     c*
231800051103     c                   clear                   wemn             30
231900051026     c* se ce l'ho uso il cod.comm
232000051026    2c                   if        ccomx   > *zeros
232100131003     c                   eval      k_AZNTCcmm = CComx
232200131003     c                   eval      k_AZNTCtnt = '02'
232300130802     c     k02azntc01    chain     AZNTC000
232400130802    3c                   if        %found(AZNTC01L)
232500131003     c                   eval      wemn = %trim(AZNTCrnt)
232600051026    3c                   endif
232700051026    2c                   endif
232800130802     c* se non c'è cd.comm o cd.comm senza e-mail su AZNTC, chaino AZORG con PO
232900130802    2c                   if        ccomx   = *zeros or  not %found(AZNTC01L)
233000130802     c                             or Wemn = *blanks
233100051026     c     keypo         chain     azorg01l
233200051026     C                   MOVEL     ORGDE3        OG143
233300051026     c* se non trovato nemmeno con keypo assumo INFOxxx o INTxxx
233400051026    4c                   if        §ogntw<>'EEX' and §ogntw<>'EUP' and
233500051026     c                             §ogntw<>'DPD' and §ogntw<>'FED'
233600051026     c                   move      orgfil        w003a
233700051103     c                   eval      wemn = 'INFO' + w003a
233800051026   x4c                   else
233900051026     c* determino ter-part del p.o.
234000051026     c                   clear                   fnlv54ds
234100051026     c                   movel     'P'           i54tpt
234200051026     c                   movel     keypo         i54lin
234300051026     c                   z-add     dateu         i54drf
234400051026     c                   call      'FNLV54R'
234500051026     C                   PARM                    fnlv54ds
234600051026     c                   move      o54tfp        w003a             3
234700051103     c                   eval      wemn = 'INT' + w003a
234800051026    4c                   endif
234900051026    2c                   endif
235000051026     c                   endsr
235100051110     C****************************************************************
235200051110     C* CARICA DATI TABELLA "MRA"
235300051110     C****************************************************************
235400051110     C     CARMRA        BEGSR
235500051110     C*
235600051110     C* Reperimento tabella MRA
235700051110     C                   CLEAR                   DMRA
235800051110     C                   MOVE(P)   'MRA'         tbeCOD
235900051110     C                   MOVEL(p)  'FNLV43RTAR'  tbeKE1
236000060714     c                   MOVEl     vi2lin        tbeKE2
236100051110     C     Ktbe          CHAIN     TNTBE01L
236200051110     C                   IF        %found(TNTBE01L) AND
236300051110     C                             tbeATB <> 'A'
236400051110     C                   MOVEL     tbeUNI        DMRA
236500051110     C                   ELSE
236600051110     C                   SETON                                        90
236700051110     C                   ENDIF
236800051110     c                   endsr
236900990707**
237000940224 ***    STAMPA  TESTI  LETTERE    ***
237100950905**
237200950915DETTAGLIO- Tutto/ LPar.del cli / LPar-arr.del cli:
237300950915Stampa TARIFFE PARTICOLARI da tariffa di cartello:
237400050131DETTAGLIO- Tutto/ Linea Partenza del cliente ....:
237500960531**   MSG
237600110203                                                                              1
237700041029Valore non ammesso:non presenti in anagrafica nè indirizzo E-mail nè il n.Fax 2
237800980518Codice importanza cliente errato
237900110203                                                                              4
238000981113Codice commerciale inesistente                                                5
238100110203                                                                               6
238200110203
238300161223                                                                               8
238400021129Priorità cliente non valida                                                   13
238500041103Immettere un indirizzo E-mail o un numero fax corretto                        10
238600050907Immettere il Nominativo                                                       11
238700050907E-mail mancante o errato                                                      12
238800050907Oggetto mail obbligatorio
238900050930Indirizzo E-mail inesistente per il codice richiesto                          14
239000060919Elaborazione non eseguita:selezionat                                          15
239100051107**         CMD1
239200051107DLYJOB DLY(1)
