000100970526     H*------------------------------------------------------------------------*
000200140904     H* Gestione log download in errore
000300970526     H*------------------------------------------------------------------------*
000400140703
000500140904      * per compilare questo pgm bisogna avere in lista di librerie il file WFLDE00F che viene,
000600140703      * però, creato e distrutto da questi pgm (è una table SQL).
000700140703      * quindi crearlo secondo lo statement in SRCTNDB nella propria libreria e mettere questa
000800140703      * in lista temporaneamente, ricordandosi di toglierla prima di lanciare l'esecuzione.
000900140703
001000140704     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR')
001100970526     H DECEDIT('0,') DATEDIT(*DMY.)
001200040121     F*------------------------------------------------------------------------*
001300970526     F* DATA BASE
001400970526     F*------------------------------------------------------------------------*
001500140701     Ftianv01l  IF   E           K DISK
001600140701     Ftntbe02l  IF   E           K DISK
001700140904     FTIS7GI1D4 CF   E             WORKSTN INFDS(DEVDS1)
001800140904     F                                     SFILE(s7GID05:SFLULT)
001900140904     F                                     SFILE(s7GIDMS:MSGULT)
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970528     D*------------
003200970528     D* COMANDI
003300970528     D*------------
003400970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI OPZIONI
003700970526     D*------------
003800000707     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI TASTI FUNZIONALI
004100970526     D*------------
004200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* OPZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970526     D OPV             S             19    DIM(24)
004700970526     D*------------
004800970526     D* FUNZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970527     D FUV             S             19    DIM(24)
005100991108     D*------------
005200991108     D* CODICI DI STATO DEI RECORD DI LOG
005300991108     D*------------
005400991108     D FLG             S              1    DIM(10) CTDATA PERRCD(1)
005500991108     D*------------
005600991108     D* DESCRIZIONI STATO DEI RECORD DI LOG
005700991108     D*------------
005800991112     D DES             S              5    DIM(10) CTDATA PERRCD(1)
005900970526     D*------------------------------------------------------------------------*
006000970526     D* INPUT
006100970526     D*------------------------------------------------------------------------*
006200970526     D*------------
006300970526     D* RIDEFINIZIONE OPZIONI VIDEO
006400970526     D*------------
006500970526     D OPDDS           DS
006600970526     D  OPDOPZ                 1      2
006700970526     D  OPDDE1                 3     19
006800970526     D  OPDOP1                21     22
006900970526     D  OPDFI0                23     34
007000970526     D  OPDATT                35     35
007100970526     D  OPDFI1                36     80
007200970526     D  OPDDES                 1     19
007300970526     D  OPDRIG                 1     80
007400970526     D*------------
007500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007600970526     D*------------
007700970526     D FUDDS           DS
007800970526     D  FUDDE1                 1      1
007900970526     D  FUDFUN                 2      3
008000970526     D  FUDDE2                 4     19
008100970526     D  FUDFI0                20     34
008200970526     D  FUDATT                35     35
008300970526     D  FUDFI1                36     80
008400970526     D  FUDDES                 1     19
008500970526     D  FUDRIG                 1     80
008600970526     D*------------
008700970526     D* DS GESTIONE TASTI FUNZIONALI
008800970526     D*------------
008900970526     D DSKEY         E DS
009000970526     D DEVDS1          DS
009100970526     D  KEY                  369    369
009200970526     D*------------
009300970526     D* DS "XMSG" - GESTIONE MESSAGGI
009400970526     D*------------
009500970526     D DSMSG         E DS
009600970526     D*------------
009700020916     D* DS REPERIMENTO DATI UTENTE
009800970526     D*------------
009900020916     D TIBS34DS      E DS                                                       *Profili utente
010000020916     D DDATIUTE      E DS                                                       *Dati utente
010100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010200141016     D tis720        E DS                  extname(tis720ds)
010300141022     D TRTB06DS      e ds
010400020218     D*------------
010500020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
010600020218     D*------------
010700020218     D DITF          E DS
010800970527     D*------------------
010900991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
011000970527     D*------------------
011100970527     D WLBDA8          DS                  INZ
011200970527     D  G08DAT                 1      8  0
011300970527     D  G08INV                 9     16  0
011400970527     D  G08ERR                17     17
011500970527     D  G08TGI                18     22  0
011600970526     D*------------
011700970526     D* ARCHITETTURA
011800970526     D*------------
011900970526     D KPJBA         E DS
012000991108     D*------------------
012100140701     D* DS - dati per fetch
012200140904     D WFLDEDS       E DS                  extname(WFLDE00F)
012300140701     D DSFetch         DS
012400140904     D  KSC                                like(LDEKsc)
012500140904     D  ISV                                like(LDEISV)
012600140904     D  TIP                                like(LDETip)
012700140904     D  TIPERR                             like(LDETipErr)
012800140904     D  ERR                                like(LDECntErr)
012900140701     D*------------------
013000140701     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
013100991108     D*------------------
013200991108     D                 DS                  INZ
013300991108     D  GG                     1      2  0
013400991108     D  MM                     3      4  0
013500991108     D  AAAA                   5      8  0
013600991108     D  CHKDATA                1      8  0
013700991108     D*------------------
013800991108     D                 DS                  INZ
013900991108     D  AAINV                  1      4  0
014000991108     D  MMINV                  5      6  0
014100991108     D  GGINV                  7      8  0
014200991108     D  INVDATA                1      8  0
014300991110     D*------------------
014400991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
014500991110     D*------------------
014600991110     D                 DS                  INZ
014700991110     D  GGA                    1      2  0
014800991110     D  SEP1                   3      3
014900991110     D  MMA                    4      5  0
015000991110     D  SEP2                   6      6
015100991110     D  AAAAA                  7     10  0
015200991110     D  DATAA                  1     10
015300991110     D*------------------
015400991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
015500991110     D*------------------
015600991110     D                 DS                  INZ
015700991110     D  HH                     1      2  0
015800991110     D  PP                     3      4  0
015900991110     D  SS                     5      6  0
016000991110     D  ORAN                   1      6  0
016100991110     D*------------------
016200991110     D                 DS                  INZ
016300991110     D  HHA                    1      2  0
016400991110     D  SEP3                   3      3
016500991110     D  PPA                    4      5  0
016600991110     D  SEP4                   6      6
016700991110     D  SSA                    7      8  0
016800991110     D  ORAA                   1      8
016900991110     D*------------------
017000991110     D* DS - COMPOSIZIONE NOME MEMBRO
017100991110     D*------------------
017200991110     D                 DS                  INZ
017300991110     D  M                      1      1
017400991110     D  HDL                    1     10
017500991110     D*------------------
017600991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
017700991110     D*------------------
017800991110     D                 DS                  INZ
017900001215     D  MS1                    1     42
018000001215     D  MS2                   43     80
018100991110     D  MESS                   1     80
018200991108     D*------------------
018300991108     D* DS REPERIMENTO DATI UTENTE
018400991108     D*-------------------
018500991108     DBS69DS         E DS                  extname(TIBS69DS)
018600991108     DACODS          E DS                  extname(CNACO00F)
018700991108     DINDDS          E DS                  extname(CNIND00F)
018800991108     DCLPDS          E DS                  extname(CNCLP00F)
018900991108     DCLSDS          E DS                  extname(FNCLS00F)
019000090519     D*------------
019100090519     D* VARIABILI D WRK
019200090519     D*------------
019300090519     D wrkprg          s              8  0 inz(*zeros)
019400090519     D uplprg          s             10    inz(*all'0')
019500991110     D*------------
019600140702     D* PASSAGGIO PARAMETRI IN INGRESSO
019700140702     D*------------
019800140702     D PARAM           DS
019900140702     D  DEPOP0                        3
020000140904     D  KSC7GI                        8
020100140904     D  TIP7GI                        2
020200141009     D  ISV7GI                        2
020300140702     D*------------
020400140701     D* Stringa SQL da eseguire
020500140701     d wSQL            s           2048    inz  varying
020600140701     D* appoggio ragione sociale cliente
020700140701     d wRag            s             30    inz
020800141015     D* salvataggio tipo errore
020900141015     D SAVTipErr       s                   like(VIDTipErr)
021000051013     D*------------
021100051013     D* PASSAGGIO PARAMETRI IN INGRESSO
021200051013     D*------------
021300140904     d DS7GIR2         ds
021400140701     d pIn_DatDa                      8s 0
021500140701     d pIn_DatA                       8s 0
021600140701     d pIn_Ksc                        8
021700140701     d pIn_Tip                        2
021800140904     d pIn_ISV                        2
021900140701     d pIn_Modo                       1
022000141015     d pOut_Esito                     2s 0
022100080320     D*---
022200080320     D* Costanti
022300080320     D*---
022400080320     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
022500080320     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
022600101005     D*-------------------
022700101005     D* DEFINIZIONI PROCEDURE ESTERNE
022800101005     D*-------------------
022900101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
023000101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
023100970526     C*------------------------------------------------------------------------*
023200970526     C* MAIN LINES
023300970526     C*------------------------------------------------------------------------*
023400970526     C*
023500970526     C* SE NON CI SONO ERRORI, PROSEGUE
023600970526IF  1C     ERRL00        IFNE      '1'
023700970526     C*
023800970526     C* CARICA IL SFILE
023900970526     C                   EXSR      POSSFL
024000970526     C*
024100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
024200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
024300141015     C     F06L00        OREQ      '1'                                          *F06 PREMUTO
024400141015     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
024500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
024600970526     C*
024700970526     C* REIMPOSTA ERRORE DI PROCERURA
024800970526     C                   MOVEL     '0'           ERRL00
024900970526     C*
025000970526     C* EMETTE IL SFILE
025100970526     C                   EXSR      OUTSFL
025200970526     C*
025300970526     C* GESTIONE TASTI FUNZIONALI
025400970526     C                   EXSR      FUNCON
025500970526     C*
025600970526E   2C                   ENDDO
025700970527     C*
025800970526E   1C                   ENDIF
025900991108     C*
026000991108     C                   EXSR      FINSR
026100970526     C* FINE PGM
026200970526     C                   SETON                                        LR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
026500970526     C*------------------------------------------------------------------------*
026600970526     C     F03GES        BEGSR
026700970526     C*
026800970526     C* IMPOSTA "PREMUTO F03"
026900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
027000970526     C*
027100970526     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
027400970526     C*------------------------------------------------------------------------*
027500970526     C     F06GES        BEGSR
027600141015     C*
027700141015     C* IMPOSTA "PREMUTO F06"
027800141015     C                   MOVEL     '1'           F06L00                         *A PROGRAMMA
027900141015     C* imposto i parametri di selezione con quelli a video
028000141015     C                   EVAL      pIn_Ksc = VIDKsc
028100141015     C                   EVAL      pIn_Tip = VIDTip
028200141015     C                   EVAL      pIn_ISV = VIDISV
028300141015     C* dico al chiamante che sono uscito per F06
028400141015     C                   EVAL      pOut_Esito = 6
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800991108     C* F12GES - GESTIONE F12 -> RITORNO
028900970526     C*------------------------------------------------------------------------*
029000970526     C     F12GES        BEGSR
029100970526     C*
029200970526     C* IMPOSTA "PREMUTO F12"
029300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
029400970526     C*
029500970526     C                   ENDSR
029600970528     C*------------------------------------------------------------------------*
029700970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
029800970528     C*------------------------------------------------------------------------*
029900970528     C     F21GES        BEGSR
030000970612     C*
030100991117     C* NON GESTITA
030200970528     C*
030300970528     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
030600970526     C*------------------------------------------------------------------------*
030700970526     C     F23GES        BEGSR
030800970526     C*
030900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
031400970526     C*------------------------------------------------------------------------*
031500970526     C     F24GES        BEGSR
031600970526     C*
031700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031800970526     C*
031900970526     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
032200970526     C*------------------------------------------------------------------------*
032300970526     C     RUPGES        BEGSR
032400970526     C*
032500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
032600970526     C*
032700970526     C                   ENDSR
032800970526     C*------------------------------------------------------------------------*
032900991108     C* ENTGES - GESTIONE ENTER -> INVIO
033000970526     C*------------------------------------------------------------------------*
033100970526     C     ENTGES        BEGSR
033200141015     C*
033300141015     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033400141015     C                   EXSR      NEWPOS
033500991108     C*
033600141015     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
033700141015IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033800141015IF  2C     SWPOSI        IFNE      0
033900141015     C                   EXSR      POSSFL                                       *RICARICA SFILE
034000141015X   2C                   ELSE
034100140702IF  3C     SFLULT        IFNE      0
034200140702     C                   EXSR      ENTATT                                       *GESTIONE ENTER
034300140702     C*
034400140702     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
034500140702IF  4C     SWMODI        IFNE      0
034600140702     C                   EXSR      POSSFL
034700140702E   4C                   ENDIF
034800140702E   3C                   ENDIF
034900141015E   2C                   ENDIF
035000141015E   1C                   ENDIF
035100991108     C*
035200970526     C                   ENDSR
035300141022     C*------------------------------------------------------------------------*
035400141022     C* O01GES - Abil.Servizi
035500141022     C*------------------------------------------------------------------------*
035600141022     C     O01GES        BEGSR
035700141022     C*
035800141022     C* REGOLE PER CLIENTE/TIPO FILE
035900141022     C     ERRL00        ifeq      '0'                                          *NO ERRORI
036000141022     C*
036100141022     C* IMPOSTA LA DS DI PROCEDURA
036200141022     C                   MOVEL     'P02'         s20OP0
036300141022     C                   MOVEL     *blank        s20OP1
036400141022     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
036500141022     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
036600141022     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
036700141022     C                   MOVEL     *blank        s20TIP                         *TOPO FILE
036800141022     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
036900141022     C                   MOVEL     *blank        s20tle
037000141022     C                   MOVEL     *blank        s20isv                         *TIPO SERVIZIO
037100141022     C                   MOVE      D05Ksc        s20ksc                         *CLIENTE UNIFICANTE
037200141022     C                   MOVE      *blank        S20sun                         *UTENTE STRATEGI
037300141022     C*
037400141022     C                   MOVEL     tis720        kpjbu
037500141022     C                   CALL      'TIS724R'
037600141022     C                   PARM                    KPJBA
037700141022     C*                  PARM                    tibds
037800141022     C                   MOVEL     kpjbu         tis720
037900141022     C*
038000141022     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
038100141022     C     s20F03        IFEQ      '1'                                          *PREMUTO F03
038200141022     C                   MOVEL     '1'           ERRL00                         *ERRORE
038300141022     C                   MOVEL     '1'           F03L00                         *FINE PGM
038400141022     C                   ELSE                                                   *NON PREMUTO F03
038500141022     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
038600141022     C                   ENDIF
038700141022     C***                z-add     1             SWMODI                     L
038800141022E   1C                   endif
038900141022     C*
039000141022     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039100141022     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039200141022     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039300141022     C                   Z-ADD     0             SWMODI            1 0
039400141022     C*
039500141022     C                   ENDSR
039600970526     C*------------------------------------------------------------------------*
039700141016     C* O02GES - Gestione Log
039800970526     C*------------------------------------------------------------------------*
039900141016     C     O02GES        BEGSR
040000141016     C*
040100141016     C* REGOLE PER CLIENTE/TIPO FILE
040200141016     C     ERRL00        ifeq      '0'                                          *NO ERRORI
040300141016     C*
040400141016     C* IMPOSTA LA DS DI PROCEDURA
040500141016     C                   MOVEL     'P02'         s20OP0
040600141016     C                   MOVEL     *blank        s20OP1
040700141016     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
040800141016     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
040900141016     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
041000141016     C                   MOVEL     D05Tip        s20TIP                         *TOPO FILE
041100141016     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
041200141016     C                   MOVEL     *blank        s20tle
041300141016     C                   MOVEL     *blank        s20isv                         *TIPO SERVIZIO
041400141016     C                   MOVE      D05Ksc        s20ksc                         *CLIENTE UNIFICANTE
041500141016     C                   MOVE      *blank        S20sun                         *UTENTE STRATEGI
041600141016     C*
041700141016     C                   MOVEL     tis720        kpjbu
041800141016     C                   CALL      'TIS722R'
041900141016     C                   PARM                    KPJBA
042000141016     C*                  PARM                    tibds
042100141016     C                   MOVEL     kpjbu         tis720
042200141016     C*
042300141016     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
042400141016     C     s20F03        IFEQ      '1'                                          *PREMUTO F03
042500141016     C                   MOVEL     '1'           ERRL00                         *ERRORE
042600141016     C                   MOVEL     '1'           F03L00                         *FINE PGM
042700141016     C                   ELSE                                                   *NON PREMUTO F03
042800141016     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
042900141016     C                   ENDIF
043000141016     C***                z-add     1             SWMODI                     L
043100141016E   1C                   endif
043200140702     C*
043300140702     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
043400140702     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
043500140702     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
043600140702     C                   Z-ADD     0             SWMODI            1 0
043700970526     C*
043800970526     C                   ENDSR
043900141016     C*------------------------------------------------------------------------*
044000141016     C* O05GES - Visualizza
044100141016     C*------------------------------------------------------------------------*
044200141016     C     O05GES        BEGSR
044300141016     C*
044400141016     C                   EVAL      W01Ksc = D05KSC
044500141016     C                   EVAL      W01KscD = D05Rag
044600141016     C                   EVAL      W01Tip = D05TIP
044700141016     C                   EVAL      W01TipD = D05TIPD
044800141016     C                   EVAL      W01ISV = D05ISV
044900141016     C                   EVAL      W01TipErr = D05TipErr
045000141016     C                   SELECT
045100141016     C                   WHEN      W01TipErr = '1'
045200141016     C                   EVAL      W01TipErD1 =
045300141016     C                             'Clienti non abilitati al servizio: verifica+
045400141016     C                             re se o'
045500141016     C                   EVAL      W01TipErD2 =
045600141016     C                             'ccorre abilitarli al servizio ed eventualme+
045700141016     C                             nte rie'
045800141016     C                   EVAL      W01TipErD3 =
045900141016     C                             'seguire l''operazione'
046000141016     C                   WHEN      W01TipErr = '2'
046100141016     C                   EVAL      W01TipErD1 =
046200141016     C                             'Tipo file non previsto per il cliente: veri+
046300141016     C                             ficare '
046400141016     C                   EVAL      W01TipErD2 =
046500141016     C                             'se necessario ed eventualmente annullare l+
046600141016     C                             ''abilita'
046700141016     C                   EVAL      W01TipErD3 =
046800141016     C                             'zione alla specifica tabella'
046900141016     C                   ENDSL
047000141016     C*
047100141016     C                   EXFMT     S7GIW01
047200141016     C*
047300141016     C                   ENDSR
047400141016     C*------------------------------------------------------------------------*
047500141016     C* O04GES - Gestione Log
047600141016     C*------------------------------------------------------------------------*
047700141016     C     O04GES        BEGSR
047800141016     C*
047900141016     C                   EVAL      DEPOP0 = 'P02'
048000141016     C                   EVAL      KSC7GI = D05KSC
048100141016     C                   EVAL      TIP7GI = D05TIP
048200141016     C                   EVAL      ISV7GI = D05ISV
048300141016     C                   EVAL      KPJBU = PARAM
048400141016     C                   CALL      'TIS7GI1R6'
048500141016     C                   PARM                    KPJBA
048600141016     C*
048700141016     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048800141016     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048900141016     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
049000141016     C                   Z-ADD     0             SWMODI            1 0
049100141016     C*
049200141016     C                   ENDSR
049300141022     C*------------------------------------------------------------------------*
049400141022     C* O08GES - VISUALIZZA TABELLE VAS
049500141022     C*------------------------------------------------------------------------*
049600141022     C     O08GES        BEGSR
049700141022     C*
049800141022     C                   CLEAR                   TRTB06DS
049900141022     C                   MOVE      D05ksc        D06KSC
050000141022     C                   MOVE      'G'           D06OP0
050100141022     C                   MOVEL     TRTB06DS      KPJBU
050200141022     C                   CALL      'TRTB06R'
050300141022     C                   PARM                    KPJBA
050400141022     C*
050500141022     C                   ENDSR
050600141009     C*------------------------------------------------------------------------*
050700141009     C* O10GES - Gestione Log
050800141009     C*------------------------------------------------------------------------*
050900141009     C     O10GES        BEGSR
051000141009     C*
051100141009     C                   EVAL      DEPOP0 = 'P02'
051200141009     C                   EVAL      KSC7GI = D05KSC
051300141009     C                   EVAL      TIP7GI = D05TIP
051400141009     C                   EVAL      KPJBU = PARAM
051500141009     C                   CALL      'TIS7GIR'
051600141009     C                   PARM                    KPJBA
051700141009     C*
051800141009     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
051900141009     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
052000141009     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
052100141009     C                   Z-ADD     0             SWMODI            1 0
052200141009     C*
052300141009     C                   ENDSR
052400970526     C*------------------------------------------------------------------------*
052500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
052600970526     C*------------------------------------------------------------------------*
052700970528     C     SETFIL        BEGSR
052800970526     C*
052900140702     C* leggo tutti i rcd a seconda dei parametri ricevuti dal lancio
053000141015     C                   EVAL      wSQL = 'with '
053100141015     C                   IF        VIDTipErr = *blank or VIDTipErr = '1'
053200141015     C                   EVAL      wSQL = %trim(wSQL) + ' uni1 as ( +
053300140904     C                                    select +
053400140910     C                                    ldeksc, ldetip, ldeisv, ldetiperr, +
053500140904     C                                    sum(ldecnterr)  +
053600140904     C                                    from WFLDE00F +
053700140904     C                                    where ldetiperr = ''1'' +
053800140910     C                                    group by ldeksc, ldetip, ldeisv, +
053900140910     C                                    ldetiperr +
054000140904     C                                    having sum(ldecnterr) > 0 )'
054100141015     C                   ENDIF
054200141015     C                   IF        VIDTipErr = *blank
054300141015     C                   EVAL      wSQL = %trim(wSQL) + ' , '
054400141015     C                   ENDIF
054500141015     C                   IF        VIDTipErr = *blank or VIDTipErr = '2'
054600141015     C                   EVAL      wSQL = %trim(wSQL) + ' uni2 as ( +
054700140904     C                                    select +
054800140910     C                                    ldeksc, ldetip, ldeisv, ldetiperr, +
054900140904     C                                    sum(ldecnterr)  +
055000140904     C                                    from WFLDE00F +
055100140904     C                                    where ldetiperr = ''2'' +
055200140910     C                                    group by ldeksc, ldetip, ldeisv, +
055300140910     C                                    ldetiperr +
055400140904     C                                    having sum(ldecnterr) > 0 )'
055500141015     C                   ENDIF
055600141015     C                   SELECT
055700141015     C                   WHEN      VIDTipErr = *blank
055800140904     C                   EVAL      wSQL = %trim(wSQL) +
055900140904     C                                   ' select * from uni1 +
056000140904     C                                   union all +
056100140904     C                                   select * from uni2 +
056200140904     C                                   order by 4 , 5 desc , 1  , 2 , 3'
056300141015     C                   WHEN      VIDTipErr = '1'
056400141015     C                   EVAL      wSQL = %trim(wSQL) +
056500141015     C                                   ' select * from uni1 +
056600141015     C                                   order by 4 , 5 desc , 1  , 2 , 3'
056700141015     C                   WHEN      VIDTipErr = '2'
056800141015     C                   EVAL      wSQL = %trim(wSQL) +
056900141015     C                                   ' select * from uni2 +
057000141015     C                                   order by 4 , 5 desc , 1  , 2 , 3'
057100141015     C                   ENDSL
057200140701     C*
057300140701     C/EXEC SQL
057400140701     C+ prepare S1 from :wSQL
057500140701     C/END-EXEC
057600140701     C*
057700140701     C*
057800140701     C/EXEC SQL
057900140701     C+ declare C1 cursor for S1
058000140701     C/END-EXEC
058100140701     C*
058200140701     C*
058300140701     C/EXEC SQL
058400140701     C+ open C1
058500140701     C/END-EXEC
058600140701     C*
058700970526     C                   ENDSR
058800970526     C*------------------------------------------------------------------------*
058900970526     C* LETFIL - LETTURA RECORD
059000970526     C*------------------------------------------------------------------------*
059100970526     C     LETFIL        BEGSR
059200970526     C*
059300970526     C* LEGGE FINO A:
059400970526     C*     - FINE FILE
059500970526     C*     - TROVATO RECORD VALIDO
059600140701     C                   MOVEL     'N'           WRECOK                         *RCD NON VALIDO
059700140701     C                   EVAL      SFLEOF = 0
059800140701
059900140701     C     SFLEOF        DOWEQ     0
060000970526     C     WRECOK        ANDEQ     'N'
060100140701     C*
060200140701     C/EXEC SQL
060300140701     C+ Fetch C1 into :DSFetch
060400140701     C/END-EXEC
060500970528     C*
060600140702     C                   SELECT
060700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
060800140702     C                   WHEN      SQLCod = 100                                 *FINE FILE
060900140702     C                   Z-ADD     1             SFLEOF
061000140702     C* SE Rcd letto, lo controllo
061100140702     C                   WHEN      SQLCod >= 0
061200970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
061300140702     C* SE l'SQL si è rotto, esco avvertendo il chiamante
061400140702     C                   WHEN      SQLCod < 0
061500140702     C                   Z-ADD     1             SFLEOF
061600140702     C                   EVAL      pOut_Esito = 4
061700140702     C                   ENDSL
061800001215     C                   ENDDO
061900970526     C*
062000970526     C                   ENDSR
062100970526     C*------------------------------------------------------------------------*
062200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
062300970526     C*------------------------------------------------------------------------*
062400970526     C     CHKREC        BEGSR
062500970526     C*
062600970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
062700001215     C*
062800970526     C                   ENDSR
062900010423     C*------------------------------------------------------------------------*
063000010423     C* dectib - Controlla esistenza codice cliente
063100010423     C*------------------------------------------------------------------------*
063200010423     C     dectib        begsr
063300010913     C*
063400140701     C     ANVCli        chain     tianv01l
063500010913     C                   if        %found(tianv01l)
063600140701     C                   movel(p)  anvdes        wRag
063700010913     C                   else
063800010423     C                   CLEAR                   BS69DS
063900010423     C                   CLEAR                   ACODS
064000010423     C                   MOVEL     KNSIF         I69SIF
064100140702     C                   CLEAR                   wRag
064200140701     C                   MONITOR
064300140701     C                   EVAL      I69KAC = %dec(%subst(ANVCli:2:7):7:0)
064400140701     C                   ON-ERROR  *all
064500140701     C                   MOVEL     *ALL'*'       wRag
064600140701     C                   ENDMON
064700140701     C*
064800140701     C                   IF        wRag <> *all'*'
064900010423     C                   CALL      'TIBS69R'
065000010423     C                   PARM                    BS69DS
065100010423     C                   PARM                    ACODS
065200010423     C                   PARM                    INDDS
065300010423     C                   PARM                    CLPDS
065400010423     C                   PARM                    CLSDS
065500010423     C     O69ERR        IFNE      '1'
065600140701     C                   MOVEL     ACORAG        wRag
065700010423     C                   ELSE
065800140701     C                   MOVEL     *ALL'*'       wRag
065900010913     C                   MOVE      '1'           errl00
066000010913     C                   ENDIF
066100140701     C                   ENDIF
066200140701     C*
066300140701     C                   ENDIF
066400010913     C*
066500010913     C                   endsr
066600970526     C*------------------------------------------------------------------------*
066700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
066800970526     C*------------------------------------------------------------------------*
066900970526     C     IRESFL        BEGSR
067000010329     C*
067100140904     C                   CLEAR                   S7GID05
067200010612     C*
067300970526     C                   MOVEL     *BLANKS       OPZKEY
067400140701     C                   EVAL      D05KSC = KSC
067500140701     C                   EVAL      ANVCli = D05Ksc
067600140701     C                   EXSR      DecTib
067700140701     C                   MOVEL     wRag          D05Rag
067800140701     C*
067900140701     C                   EVAL      D05TIP = TIP
068000140701     C* reperimento descrizione tipi file
068100140701     C                   MOVEL     'ITF'         KBEcod                         *tabella
068200140701     C                   MOVEL     *BLANKS       KBElin                         *tabella
068300140701     C                   MOVEL     *BLANKS       KBEsif                         *tabella
068400140701     C                   MOVEL     D05Tip        KBEke1                         *tabella
068500140701     C                   MOVEL     *BLANKS       KBEke2                         *tabella
068600140701     C     KE2tbe        CHAIN     tntbe02l
068700140701     C                   MOVEL     tbeuni        DITF
068800140701     C                   MOVEL     §ITFDESC      D05TipD                        *tabella
068900140701     C*
069000140904     C                   EVAL      D05ISV = ISV
069100140904     C                   EVAL      D05TipErr = TipErr
069200140701     C                   EVAL      D05Err = Err
069300991108     C*
069400970526     C                   ENDSR
069500970526     C*------------------------------------------------------------------------*
069600991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
069700970526     C*------------------------------------------------------------------------*
069800970526     C     CHKPAR        BEGSR
069900970528     C*
070000140701     C* non c'è richiesta ordinamento perché legge un file già ordinato via SQL
070100991012      *
070200970526     C                   ENDSR
070300970526     C*------------------------------------------------------------------------*
070400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
070500970526     C*------------------------------------------------------------------------*
070600970526     C     CHKFOP        BEGSR
070700991112     C*
070800991112     C* OPZIONI
070900970626     C*
071000140702     C* l'utente può fare tutte le opzioni/funzioni perché entra in un solo modo
071100141022     C                   MOVEL     OPD(01)       OPDRIG                         *Abil.Servizi
071200970626     C                   MOVEL     'S'           OPDATT
071300141022     C                   MOVEL     OPDRIG        OPD(01)
071400141022     C                   MOVEL     OPD(02)       OPDRIG                         *Regole Cli/TipoF
071500141022     C                   MOVEL     'S'           OPDATT
071600141022     C                   MOVEL     OPDRIG        OPD(02)
071700141016     C                   MOVEL     OPD(04)       OPDRIG                         *Canc. Err. Log
071800141016     C                   MOVEL     'S'           OPDATT
071900141016     C                   MOVEL     OPDRIG        OPD(04)
072000141016     C                   MOVEL     OPD(05)       OPDRIG                         *Canc. Err. Log
072100141016     C                   MOVEL     'S'           OPDATT
072200141016     C                   MOVEL     OPDRIG        OPD(05)
072300141022     C                   MOVEL     OPD(08)       OPDRIG                         *Tabelle VAS
072400141022     C                   MOVEL     'S'           OPDATT
072500141022     C                   MOVEL     OPDRIG        OPD(08)
072600141009     C                   MOVEL     OPD(10)       OPDRIG                         *Gestione LOG
072700141009     C                   MOVEL     'S'           OPDATT
072800141009     C                   MOVEL     OPDRIG        OPD(10)
072900970626     C*
073000970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
073100020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
073200970626E   1C                   ENDIF
073300970526     C*
073400970526     C* GESTIONE OPZIONI A VIDEO
073500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
073600970526     C                   EXSR      OPZGES
073700970526     C*
073800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
073900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
074000970526     C                   EXSR      FUNGES
074100970526     C*
074200970526     C                   ENDSR
074300030617     C*------------------------------------------------------------------------*
074400120112     C*CONTA0B - CONTA I RECORD DA ELABORARE (STATO = 0 / " ")
074500030617     C*------------------------------------------------------------------------*
074600101018     C     CONTA0B       BEGSR
074700030617     C*
074800030617     C*
074900030617     C                   ENDSR
075000141015     C*------------------------------------------------------------------------*
075100141015     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
075200141015     C*------------------------------------------------------------------------*
075300141015     C     NEWPOS        BEGSR
075400141015     C*
075500141015     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
075600141015     C*         1= RICHIESTO POSIZIONAMENTO
075700141015     C                   Z-ADD     0             SWPOSI            7 0
075800141015     C*
075900141015     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
076000141015     C*
076100141015     C                   IF        VIDTipErr <> SAVTipErr
076200141015     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
076300141015     C                   EVAL      SAVTipErr = VIDTipErr
076400141015     C*
076500141015     C/EXEC SQL
076600141015     C+ close C1
076700141015     C/END-EXEC
076800141015     C                   ENDIF
076900141015     C*
077000141015     C                   ENDSR
077100991108     C*------------------------------------------------------------------------*
077200991108     C* FINSR - OPERAZIONI FINALI
077300991108     C*------------------------------------------------------------------------*
077400991108     C     FINSR         BEGSR
077500991108     C*
077600991108     C* CHIUDE I FILE DEI PGM CHIAMATI
077700991108     C                   CLEAR                   BS69DS
077800991108     C                   CLEAR                   ACODS
077900991108     C                   CLEAR                   INDDS
078000991108     C                   CLEAR                   CLPDS
078100991108     C                   CLEAR                   CLSDS
078200991108     C                   MOVEL     'C'           I69TLA
078300991108     C                   MOVEL     KNSIF         I69SIF
078400991108     C                   CALL      'TIBS69R'
078500991108     C                   PARM                    BS69DS
078600991108     C                   PARM                    ACODS
078700991108     C                   PARM                    INDDS
078800991108     C                   PARM                    CLPDS
078900991108     C                   PARM                    CLSDS
079000140701     C*
079100140701     C/EXEC SQL
079200140701     C+ close C1
079300140701     C/END-EXEC
079400991108     C*
079500140702     C* cancello i file di appoggio in QTEMP (dovrebbe sempre funzionare, per cui non testo
079600140702     C* l'SQLCOD
079700140904     C                   EVAL      wSQL = 'drop   table QTEMP/WFLDE00F'
079800140702     C/EXEC SQL
079900140702     C+ execute immediate  :wSQL
080000140702     C/END-EXEC
080100140702     C*
080200141015     C* imposto parametri in uscita
080300141015     C                   EVAL      KPJBU = DS7GIR2
080400141015     C*
080500991108     C                   ENDSR
080600020916     C*--------------------------------------------------------------------------------------------*
080700020916     C* REPERISCE I DATI UTENTE
080800020916     C*--------------------------------------------------------------------------------------------*
080900020916     C     REPDATIUTE    BEGSR
081000020916     C*
081100020916     C* INIZIALIZZA VARIABILI DI WRK
081200020916     C                   CLEAR                   TIBS34DS
081300020916     C                   CLEAR                   AZUTEDS
081400020916     C                   CLEAR                   DDATIUTE
081500020916     C*
081600020916     C     *DTAARA       DEFINE    §azute        azuteds
081700020916     C     *DTAARA       DEFINE    §datiute      ddatiute
081800020916     C                   IN(E)     *DTAARA
081900020916if  1C                   IF        %Error
082000020916     c                   EVAL      I34Tla = 'L'
082100020916     C                   CALL      'TIBS34R'
082200020916     C                   PARM                    Tibs34Ds
082300020916     C                   IN        *DTAARA
082400020916e   1C                   ENDIF
082500020916     C*
082600020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
082700020916     C                   MOVEL(P)  RSUT          DSFIRS           20
082800020916     C*
082900020916     C                   ENDSR
083000130731     C*----------------------------------------------------*
083100130731     C*  CONTROLLO NUMERICITA' CAMPI
083200130731     C*----------------------------------------------------*
083300130731     C     CHKNUM        BEGSR
083400130731     C*
083500130731     C                   IF        PiDecChr = *blanks
083600130731     C                   EVAL      PiDecChr = '.'
083700130731     C                   ENDIF
083800130731     C*
083900130731     C                   CALL(e)   'ISNUMERIC'
084000130731     C                   PARM                    PiStr            30
084100130731     C                   PARM                    PiDecChr          1
084200130731     C                   PARM      *ZEROS        PiVal            30 9
084300130731     C                   PARM      '0'           PiInt             1
084400130731     C                   PARM      '0'           PiNum             1
084500130731     C                   IF        %error
084600130731     C                   EVAL      PiNum=*off
084700130731     C                   ENDIF
084800130731     C*
084900130731     C                   ENDSR
085000130731     C***
085100970526     C*------------------------------------------------------------------------*
085200970526     C* *INZSR - OPERAZIONI INIZIALI
085300970526     C*------------------------------------------------------------------------*
085400970526     C     *INZSR        BEGSR
085500970526     C*---
085600970526     C* RICEVIMENTO PARAMETRI
085700970526     C*---
085800970526     C     *ENTRY        PLIST
085900970612     C                   PARM                    KPJBA
086000140904     C                   MOVEL     KPJBU         DS7GIR2
086100080319     C*
086200080319     C* SE UTENTE NO EDP* => ACCENDO *IN55
086300080319     C                   SETOFF                                       55
086400101014     C***                IF        %subst(KNMUS:1:3) <> 'EDP'
086500101014     C***                SETON                                        55
086600101014     C***                ENDIF
086700970526     C*
086800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
086900970526     C     XMSGPA        PLIST
087000970526     C                   PARM                    DSMSMS
087100970526     C                   PARM                    DSMSPG
087200970526     C                   PARM                    DSMSKE
087300970526     C                   PARM                    DSMSER
087400970526     C*
087500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
087600970526     C                   CALL      'XKEY'
087700970526     C                   PARM                    DSKEY
087800970526     C*---
087900970526     C* VARIABILI NON RIFERITE AL DATA BASE
088000970526     C*---
088100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
088200090921     C***  *LIKE         DEFINE    SFLPAG        WFLPAG
088300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
088400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
088500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
088600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
088700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
088800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
088900140701     C     *LIKE         DEFINE    FUDFUN        FUNKEY
089000140701     C*---
089100140701     C     *LIKE         DEFINE    tbecod        kbecod
089200140701     C     *LIKE         DEFINE    tbelin        kbelin
089300140701     C     *LIKE         DEFINE    tbesif        kbesif
089400140701     C     *LIKE         DEFINE    tbeke1        kbeke1
089500140701     C     *LIKE         DEFINE    tbeke2        kbeke2
089600140701     C*---
089700140701     C* VARIABILI DI CONTROLLO
089800140701     C*---
089900140701     C                   MOVEL     '0'           FINL00            1            *FINE PGM
090000140701     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
090100141015     C                   MOVEL     '0'           F06L00            1            *PREMUTO F06
090200140701     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
090300140701     C                   MOVEL     '0'           ERRL00            1            *ERRORE
090400140701     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
090500140701     C                   Z-ADD     9999          SFLPAG                         *carico tutto
090600140701     C*---
090700991108     C* LETTURA "NUOVE" TABELLE
090800010524     C     KE2tbe        KLIST                                                  *TNTBE02L
090900991108     C                   KFLD                    KBEcod                          -tabella
091000991108     C                   KFLD                    KBElin                          -lingua
091100991108     C                   KFLD                    KBEsif                          -s.informativo
091200991108     C                   KFLD                    KBEke1                          -lingua
091300991108     C                   KFLD                    KBEke2                          -s.informativo
091400991108     C*
091500970527     C* REPERIMENTO DATI UTENTE
091600020916     C                   EXSR      REPDATIUTE
091700970526     C*
091800970526     C* CALCOLA LA DATA CORRENTE
091900120112     C                   Z-ADD     0             DATCOR            8 0          *DATA CORRENTE AA/M/
092000120112     C                   EVAL      DatCor = %dec(%date : *ISO)
092100010314     C*
092200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
092300970526     C                   EXSR      CHKFOP
092400051013     C*
092500051013     C* EFFETTUA PRIMA VERIFICA DEI PARAMETRI RICEVUTI
092600051013     C                   EXSR      CHKPAR
092700970526     C*
092800970526     C                   ENDSR
092900970526     C*------------------------------------------------------------------------*
093000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093200970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
093300970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
093400970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
093500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093700970526     C*------------------------------------------------------------------------*
093800970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
093900970526     C*------------------------------------------------------------------------*
094000970526     C     POSSFL        BEGSR
094100970526     C*
094200970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
094300970526     C                   EXSR      CLRSFL
094400970526     C*
094500970526     C                   EXSR      CARSFL
094600970526     C*
094700970526     C                   ENDSR
094800970526     C*------------------------------------------------------------------------*
094900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
095000970526     C*------------------------------------------------------------------------*
095100970526     C     ENTATT        BEGSR
095200970526     C*
095300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
095400970526     C                   Z-ADD     0             OPZOK             1 0
095500970526     C*
095600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
095700970526     C                   Z-ADD     0             SFLVAR            5 0
095800970526     C*
095900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
096000970526     C                   Z-ADD     0             SFLERR
096100970526     C*
096200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
096300970526     C                   Z-ADD     SFLULT        WFLULT
096400970526     C*
096500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
096600140904     C                   READC     s7GID05                                98
096700970526     C     *IN98         DOWEQ     '0'
096800970526     C*
096900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
097000970526     C                   ADD       1             SFLVAR
097100970526     C*
097200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
097300970526     C     OPZOK         IFEQ      0
097400970526     C*
097500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
097600970526     C     OPZKEY        IFNE      *BLANKS
097700970526     C                   EXSR      OPZCON
097800970526     C                   END
097900970526     C*
098000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
098100970526     C*            2. PULIZIA SCELTA IN SFL
098200970526     C     OPZOK         IFEQ      0
098300970526     C                   MOVEL     '0'           *IN41
098400970526     C                   MOVEL     *BLANKS       OPZKEY
098500970526     C                   ELSE
098600970526     C*
098700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
098800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
098900970526     C                   MOVEL     '1'           *IN41
099000970526     C                   Z-ADD     SFLULT        SFLERR
099100970526     C                   END
099200970526     C                   ELSE
099300970526     C*
099400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
099500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
099600970526     C                   MOVEL     '1'           *IN41
099700970526     C                   END                                                    OPZOK=0
099800970526     C*
099900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
100000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
100100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
100200970526     C     OPZOK         IFNE      0
100300970526     C                   Z-ADD     SFLERR        SFLNBR
100400970526     C                   ELSE
100500970526     C                   Z-ADD     SFLULT        SFLNBR
100600970526     C                   END
100700970526     C*
100800970526     C* AGGIORNO RECORD SFL
100900020212     C                   EVAL      *IN50 = HIN50
101000020212     C                   EVAL      *IN51 = HIN51
101100140904     C                   UPDATE    s7GID05
101200970526     C*
101300140904     C                   READC     s7GID05                                98
101400970526     C                   END                                                    DO WHILE
101500970526     C*
101600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
101700970526     C                   Z-ADD     WFLULT        SFLULT
101800970526     C*
101900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
102000970526     C     SFLVAR        IFEQ      0
102100970526     C*---                 MOVEL'1'       FINL00
102200970526     C                   END
102300991112     C*
102400970526     C                   ENDSR
102500970526     C*------------------------------------------------------------------------*
102600970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
102700970526     C*------------------------------------------------------------------------*
102800970526     C     CLRSFL        BEGSR
102900970526     C*
103000970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
103100970526     C* (0=NON FINE FILE  1=FINE FILE)
103200970526     C                   Z-ADD     0             SFLEOF
103300970526     C*
103400970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
103500970526     C                   Z-ADD     0             SFLULT
103600970526     C*
103700970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
103800970526     C                   MOVEL     '1'           *IN40
103900140904     C                   WRITE     s7GID06
104000970526     C                   MOVEL     '0'           *IN40
104100970526     C*
104200970526     C                   ENDSR
104300970526     C*------------------------------------------------------------------------*
104400970526     C* CARSFL - CARICAMENTO SFL
104500970526     C*------------------------------------------------------------------------*
104600970526     C     CARSFL        BEGSR
104700970526     C*
104800140701     C* valorizzo i campi di testata con i parametri
104900140701     C                   EVAL      VIDKsc = pIn_Ksc
105000140701     C                   IF        VIDKsc <> *blank
105100140701     C                   EVAL      ANVCli = VIDKsc
105200140701     C                   EXSR      DecTib
105300140701     C                   MOVEL     wRag          VIDRag
105400140701     C                   ENDIF
105500140701     C                   EVAL      VIDTip = pIn_Tip
105600140904     C                   EVAL      VIDISV = pIn_ISV
105700140701     C*
105800970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
105900970526     C     SFLULT        IFEQ      0
106000970526     C                   EXSR      SETFIL
106100970526     C                   END
106200970526     C*
106300970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
106400970526     C                   Z-ADD     SFLULT        WFLULT
106500140701     C*
106600140701     C* LETTURA FILE DA CARICARE
106700140701     C                   EXSR      LETFIL
106800970526     C*
106900970526     C* CICLO:
107000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
107100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
107200970526     C                   Z-ADD     1             SFLRIG
107300970526     C     SFLRIG        DOWLE     SFLPAG
107400970526     C     SFLEOF        ANDEQ     0
107500970526     C*
107600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
107700970526     C                   MOVEL     *BLANKS       OPZKEY
107800970526     C*
107900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
108000970526     C                   EXSR      IRESFL
108100970526     C*
108200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
108300970526     C                   ADD       1             SFLULT
108400970526     C*
108500970526     C* SCRITTURA RECORD SFL
108600020212     C                   EVAL      HIN50 = *IN50
108700020212     C                   EVAL      HIN51 = *IN51
108800140904     C                   WRITE     s7GID05
108900970526     C                   ADD       1             SFLRIG
109000970526     C*
109100970526     C* LETTURA FILE DA CARICARE
109200970526     C                   EXSR      LETFIL
109300970526     C                   END                                                    DO WHILE
109400970526     C*
109500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
109600970526     C     WFLULT        IFNE      SFLULT
109700970526     C                   Z-ADD     WFLULT        SFLNBR
109800970526     C                   ADD       1             SFLNBR
109900140701     C* mi salvo il posizionamento effettuato
110000140701     C* --- nessun posizionamento ---
110100970526     C                   END
110200970526     C*
110300970526     C                   ENDSR
110400970526     C*------------------------------------------------------------------------*
110500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
110600970526     C*------------------------------------------------------------------------*
110700970526     C     OUTSFL        BEGSR
110800970526     C*
110900970526     C* WRITE RECORD TESTATA VIDEO
111000140904     C                   WRITE     s7GID01
111100970526     C*
111200970526     C* VISUALIZZAZIONE MESSAGGIO
111300970526     C                   EXSR      SNDMSG
111400970526     C*
111500970526     C* WRITE RECORD OPZIONI VIDEO
111600140904     C                   WRITE     s7GID02
111700030617     C*
111800030617     C* CONTO I RECORD DA ELABORARE
111900120112     C                   EXSR      CONTA0B
112000970526     C*
112100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
112200140904     C                   WRITE     s7GID03
112300970526     C*
112400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
112500140904     C                   WRITE     s7GID04
112600970526     C*
112700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
112800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
112900970526     C     SFLEOF        IFEQ      0
113000970526     C                   MOVEL     '0'           *IN42
113100970526     C                   ELSE
113200970526     C*
113300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
113400970526     C                   MOVEL     '1'           *IN42
113500970526     C                   END
113600970526     C*
113700970526     C* WRITE RECORD TASTI FUNZIONALI
113800140904     C                   WRITE     s7GID09
113900970526     C*
114000970526     C* CASO DI SFL VUOTO
114100970526     C     SFLULT        IFLE      0
114200970526     C*
114300970526     C*    - WRITE RECORD VUOTO
114400140904     C                   WRITE     s7GID0A
114500140904     C                   READ      s7GID0A                                99
114600970526     C*
114700970526     C                   ELSE
114800970526     C*
114900970526     C* CASO DI SFL CON RECORDS
115000970526     C*
115100970526     C*    - WRITE RECORD CONTROLLO SFL
115200140904     C                   WRITE     s7GID06
115300140904     C                   READ      s7GID06                                99
115400970526     C                   END
115500970526     C*
115600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
115700140904     C                   READ      s7GID03                                99
115800970526     C*
115900970526     C                   ENDSR
116000970526     C*------------------------------------------------------------------------*
116100970526     C* SNDMSG - MANDA MESSAGGIO
116200970526     C*------------------------------------------------------------------------*
116300970526     C     SNDMSG        BEGSR
116400970526     C*
116500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
116600970526     C                   MOVEL     '1'           *IN40
116700140904     C                   WRITE     s7GIDMC
116800970526     C                   MOVEL     '0'           *IN40
116900970526     C*
117000970526     C* VISUALIZZAZIONE MESSAGGIO
117100970526     C                   Z-ADD     0             MSGULT
117200970526     C                   CALL      'XMSG'        XMSGPA
117300970526     C     DSMSER        IFEQ      0
117400970526     C                   MOVEL     DSMSKE        MSGKEY
117500970526     C                   END
117600970526     C                   Z-ADD     1             MSGULT
117700140904     C                   WRITE     s7GIDMS                                      SFL MESS
117800140904     C                   WRITE     s7GIDMC                                      CONTR MESS
117900970526     C*
118000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
118100970526     C                   MOVEL     *BLANKS       DSMSMS
118200970526     C*
118300970526     C                   ENDSR
118400970526     C*------------------------------------------------------------------------*
118500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
118600970526     C*------------------------------------------------------------------------*
118700970526     C     FUNCON        BEGSR
118800970526     C*
118900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
119000970526     C                   EXSR      FUNVAR
119100970526     C*
119200970526     C* PULISCO CAMPI DI LAVORO
119300970526     C                   Z-ADD     0             I                 5 0
119400970526     C                   Z-ADD     0             W                 5 0
119500970526     C*
119600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
119700970526     C     FUNKEY        IFNE      *BLANKS
119800970526     C     1             DO        24            I
119900970526     C                   MOVEL     FUD(I)        FUDRIG
120000970526     C*
120100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
120200970526     C     FUNKEY        IFEQ      FUDFUN
120300970526     C     FUDATT        ANDEQ     'S'
120400970526     C                   Z-ADD     I             W
120500970526     C                   END
120600970526     C                   END                                                    DO WHILE
120700970526     C*
120800970526     C* FUNZIONE NON ATTIVA (ERRORE)
120900970526     C     W             IFEQ      0
121000970526     C*
121100970526     C* EMISSIONE MESSAGGIO DI ERRORE
121200970526     C                   MOVEL     MSG(2)        DSMSMS
121300970526     C                   END                                                    W=0
121400970526     C                   END                                                    FUNKEY NE *BLANKS
121500970526     C*
121600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
121700970526     C     W             IFNE      0
121800970526     C     FUNKEY        OREQ      *BLANKS
121900970526     C*
122000970526     C     FUNKEY        IFEQ      *BLANKS
122100970526     C                   EXSR      ENTGES
122200970526     C                   END
122300970526     C     FUNKEY        IFEQ      '03'
122400970526     C                   EXSR      F03GES
122500970526     C                   END
122600970526     C     FUNKEY        IFEQ      '06'
122700970526     C                   EXSR      F06GES
122800970526     C                   END
122900970526     C     FUNKEY        IFEQ      '12'
123000970526     C                   EXSR      F12GES
123100970526     C                   END
123200970526     C     FUNKEY        IFEQ      '23'
123300970526     C                   EXSR      F23GES
123400970526     C                   END
123500970526     C     FUNKEY        IFEQ      '24'
123600970526     C                   EXSR      F24GES
123700970526     C                   END
123800970526     C     FUNKEY        IFEQ      'RU'
123900970526     C                   EXSR      RUPGES
124000970526     C                   END
124100970526     C*
124200970526     C                   END
124300970526     C*
124400970526     C                   ENDSR
124500970526     C*------------------------------------------------------------------------*
124600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
124700970526     C*------------------------------------------------------------------------*
124800970526     C     FUNVAR        BEGSR
124900970526     C*
125000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
125100970526     C     KEY           IFEQ      ENTER
125200970526     C                   MOVEL     *BLANKS       FUNKEY
125300970526     C                   END
125400970526     C     KEY           IFEQ      F1
125500970526     C                   MOVEL     '01'          FUNKEY
125600970526     C                   END
125700970526     C     KEY           IFEQ      F2
125800970526     C                   MOVEL     '02'          FUNKEY
125900970526     C                   END
126000970526     C     KEY           IFEQ      F3
126100970526     C                   MOVEL     '03'          FUNKEY
126200970526     C                   END
126300970526     C     KEY           IFEQ      F4
126400970526     C                   MOVEL     '04'          FUNKEY
126500970526     C                   END
126600970526     C     KEY           IFEQ      F5
126700970526     C                   MOVEL     '05'          FUNKEY
126800970526     C                   END
126900970526     C     KEY           IFEQ      F6
127000970526     C                   MOVEL     '06'          FUNKEY
127100970526     C                   END
127200970526     C     KEY           IFEQ      F7
127300970526     C                   MOVEL     '07'          FUNKEY
127400970526     C                   END
127500970526     C     KEY           IFEQ      F8
127600970526     C                   MOVEL     '08'          FUNKEY
127700970526     C                   END
127800970526     C     KEY           IFEQ      F9
127900970526     C                   MOVEL     '09'          FUNKEY
128000970526     C                   END
128100970526     C     KEY           IFEQ      F10
128200970526     C                   MOVEL     '10'          FUNKEY
128300970526     C                   END
128400970526     C     KEY           IFEQ      F11
128500970526     C                   MOVEL     '11'          FUNKEY
128600970526     C                   END
128700970526     C     KEY           IFEQ      F12
128800970526     C                   MOVEL     '12'          FUNKEY
128900970526     C                   END
129000970526     C     KEY           IFEQ      F13
129100970526     C                   MOVEL     '13'          FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F14
129400970526     C                   MOVEL     '14'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F15
129700970526     C                   MOVEL     '15'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F16
130000970526     C                   MOVEL     '16'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      F17
130300970526     C                   MOVEL     '17'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      F18
130600970526     C                   MOVEL     '18'          FUNKEY
130700970526     C                   END
130800970526     C     KEY           IFEQ      F19
130900970526     C                   MOVEL     '19'          FUNKEY
131000970526     C                   END
131100970526     C     KEY           IFEQ      F20
131200970526     C                   MOVEL     '20'          FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      F21
131500970526     C                   MOVEL     '21'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      F22
131800970526     C                   MOVEL     '22'          FUNKEY
131900970526     C                   END
132000970526     C     KEY           IFEQ      F23
132100970526     C                   MOVEL     '23'          FUNKEY
132200970526     C                   END
132300970526     C     KEY           IFEQ      F24
132400970526     C                   MOVEL     '24'          FUNKEY
132500970526     C                   END
132600970526     C     KEY           IFEQ      ROLLUP
132700970526     C                   MOVEL     'RU'          FUNKEY
132800970526     C                   END
132900970526     C     KEY           IFEQ      HELP
133000970526     C                   MOVEL     'HP'          FUNKEY
133100970526     C                   END
133200970526     C*
133300970526     C                   ENDSR
133400970526     C*------------------------------------------------------------------------*
133500970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
133600970526     C*------------------------------------------------------------------------*
133700970526     C     FUNGES        BEGSR
133800970526     C*
133900970526     C* PULISCO CAMPI DI LAVORO
134000970526     C                   Z-ADD     0             W                 5 0
134100970526     C*
134200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
134300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
134400970526     C     1             DO        24            I
134500970526     C                   MOVEL     FUD(I)        FUDRIG
134600970526     C*
134700970526     C* PULIZIA FUNZIONI PRECEDENTI
134800970526     C                   MOVEL     *BLANKS       FUV(I)
134900970526     C*
135000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
135100970526     C* (SALTA F23 E F24)
135200970526     C     FUDATT        IFEQ      'S'
135300970526     C     FUDFUN        ANDNE     '23'
135400970526     C     FUDFUN        ANDNE     '24'
135500970526     C                   ADD       1             W
135600970526     C                   MOVEL     FUDDES        FUV(W)
135700970526     C                   END
135800970526     C                   END                                                    DO WHILE
135900970526     C*
136000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
136100970526     C* ALTRIMENTI DISATTIVO F24
136200970526     C     1             DO        24            I
136300970526     C                   MOVEL     FUD(I)        FUDRIG
136400970526     C*
136500970526     C     FUDFUN        IFEQ      '24'
136600970526     C     W             IFGT      6
136700970526     C                   MOVEL     'S'           FUDATT
136800970526     C                   ELSE
136900970526     C                   MOVEL     *BLANKS       FUDATT
137000970526     C                   END                                                    W GT 8
137100970526     C                   MOVEL     FUDRIG        FUD(I)
137200970526     C                   END
137300970526     C                   END                                                    DO WHILE
137400970526     C*
137500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
137600970526     C     W             DIV       6             I
137700970526     C                   MVR                     Z
137800970526     C*
137900970526     C     Z             IFNE      0
138000970526     C     I             ADD       1             Z
138100970526     C                   ELSE
138200970526     C                   Z-ADD     I             Z
138300970526     C                   END
138400970526     C*
138500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
138600970526     C                   ADD       1             FUNGRU
138700970526     C     FUNGRU        IFGT      Z
138800970526     C     FUNGRU        ORLT      1
138900970526     C                   Z-ADD     1             FUNGRU
139000970526     C                   END
139100970526     C*
139200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
139300970526     C     FUNGRU        IFEQ      1
139400970526     C                   MOVEL     FUV(1)        FUN001
139500970526     C                   MOVEL     FUV(2)        FUN002
139600970526     C                   MOVEL     FUV(3)        FUN003
139700970526     C                   MOVEL     FUV(4)        FUN004
139800970526     C                   MOVEL     FUV(5)        FUN005
139900970526     C                   MOVEL     FUV(6)        FUN006
140000970526     C                   END
140100970526     C*
140200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
140300970526     C     FUNGRU        IFEQ      2
140400970526     C                   MOVEL     FUV(7)        FUN001
140500970526     C                   MOVEL     FUV(8)        FUN002
140600970526     C                   MOVEL     FUV(9)        FUN003
140700970526     C                   MOVEL     FUV(10)       FUN004
140800970526     C                   MOVEL     FUV(11)       FUN005
140900970526     C                   MOVEL     FUV(12)       FUN006
141000970526     C                   END
141100970526     C*
141200970526     C* TERZO GRUPPO DI 6 FUNZIONI
141300970526     C     FUNGRU        IFEQ      3
141400970526     C                   MOVEL     FUV(13)       FUN001
141500970526     C                   MOVEL     FUV(14)       FUN002
141600970526     C                   MOVEL     FUV(15)       FUN003
141700970526     C                   MOVEL     FUV(16)       FUN004
141800970526     C                   MOVEL     FUV(17)       FUN005
141900970526     C                   MOVEL     FUV(18)       FUN006
142000970526     C                   END
142100970526     C*
142200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
142300970526     C     FUNGRU        IFEQ      4
142400970526     C                   MOVEL     FUV(19)       FUN001
142500970526     C                   MOVEL     FUV(20)       FUN002
142600970526     C                   MOVEL     FUV(21)       FUN003
142700970526     C                   MOVEL     FUV(22)       FUN004
142800970526     C                   MOVEL     *BLANKS       FUN005
142900970526     C                   MOVEL     *BLANKS       FUN006
143000970526     C                   END
143100970526     C*
143200970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
143300970526     C                   MOVEL     *BLANKS       FUN007
143400970526     C                   MOVEL     *BLANKS       FUN008
143500970526     C*
143600970526     C     1             DO        24            I
143700970526     C                   MOVEL     FUD(I)        FUDRIG
143800970526     C     FUDFUN        IFEQ      '23'
143900970526     C     FUDATT        ANDEQ     'S'
144000970526     C                   MOVEL     FUDDES        FUN007
144100970526     C                   END
144200970526     C     FUDFUN        IFEQ      '24'
144300970526     C     FUDATT        ANDEQ     'S'
144400970526     C                   MOVEL     FUDDES        FUN008
144500970526     C                   END
144600970526     C                   END                                                    DO
144700970526     C*
144800970526     C                   ENDSR
144900970526     C*------------------------------------------------------------------------*
145000970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
145100970526     C*------------------------------------------------------------------------*
145200970526     C     OPZGES        BEGSR
145300970526     C*
145400970526     C* PULISCO CAMPI DI LAVORO
145500970526     C                   Z-ADD     0             W                 5 0
145600970526     C*
145700970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
145800970526     C* E CARICO TABELLA OPZIONI ATTIVE
145900970526     C     1             DO        24            I
146000970526     C                   MOVEL     OPD(I)        OPDRIG
146100970526     C*
146200970526     C* PULIZIA OPZIONI PRECEDENTI
146300970526     C                   MOVEL     *BLANKS       OPV(I)
146400970526     C*
146500970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
146600970526     C     OPDATT        IFEQ      'S'
146700970526     C                   ADD       1             W
146800970526     C                   MOVEL     OPDDES        OPV(W)
146900970526     C                   END
147000970526     C                   END                                                    DO WHILE
147100970526     C*
147200970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
147300970526     C* ALTRIMENTI DISATTIVO F23
147400970526     C     1             DO        24            I
147500970526     C                   MOVEL     FUD(I)        FUDRIG
147600970526     C*
147700970526     C     FUDFUN        IFEQ      '23'
147800970526     C     W             IFGT      8
147900970526     C                   MOVEL     'S'           FUDATT
148000970526     C                   ELSE
148100970526     C                   MOVEL     *BLANKS       FUDATT
148200970526     C                   END                                                    W GT 8
148300970526     C                   MOVEL     FUDRIG        FUD(I)
148400970526     C                   END
148500970526     C                   END                                                    DO WHILE
148600970526     C*
148700970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
148800970526     C     W             DIV       8             I
148900970526     C                   MVR                     Z                 5 0
149000970526     C*
149100970526     C     Z             IFNE      0
149200970526     C     I             ADD       1             Z
149300970526     C                   ELSE
149400970526     C                   Z-ADD     I             Z
149500970526     C                   END
149600970526     C*
149700970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
149800970526     C                   ADD       1             OPZGRU
149900970526     C     OPZGRU        IFGT      Z
150000970526     C     OPZGRU        ORLT      1
150100970526     C                   Z-ADD     1             OPZGRU
150200970526     C                   END
150300970526     C*
150400970526     C* PRIMO GRUPPO DI 8 OPZIONI
150500970526     C     OPZGRU        IFEQ      1
150600970526     C                   MOVEL     OPV(1)        OPZ001
150700970526     C                   MOVEL     OPV(2)        OPZ002
150800970526     C                   MOVEL     OPV(3)        OPZ003
150900970526     C                   MOVEL     OPV(4)        OPZ004
151000970526     C                   MOVEL     OPV(5)        OPZ005
151100970526     C                   MOVEL     OPV(6)        OPZ006
151200970526     C                   MOVEL     OPV(7)        OPZ007
151300970526     C                   MOVEL     OPV(8)        OPZ008
151400970526     C                   END
151500970526     C*
151600970526     C* SECONDO GRUPPO DI 8 OPZIONI
151700970526     C     OPZGRU        IFEQ      2
151800970526     C                   MOVEL     OPV(9)        OPZ001
151900970526     C                   MOVEL     OPV(10)       OPZ002
152000970526     C                   MOVEL     OPV(11)       OPZ003
152100970526     C                   MOVEL     OPV(12)       OPZ004
152200970526     C                   MOVEL     OPV(13)       OPZ005
152300970526     C                   MOVEL     OPV(14)       OPZ006
152400970526     C                   MOVEL     OPV(15)       OPZ007
152500970526     C                   MOVEL     OPV(16)       OPZ008
152600970526     C                   END
152700970526     C*
152800970526     C* TERZO GRUPPO DI 8 OPZIONI
152900970526     C     OPZGRU        IFEQ      3
153000970526     C                   MOVEL     OPV(17)       OPZ001
153100970526     C                   MOVEL     OPV(18)       OPZ002
153200970526     C                   MOVEL     OPV(19)       OPZ003
153300970526     C                   MOVEL     OPV(20)       OPZ004
153400970526     C                   MOVEL     OPV(21)       OPZ005
153500970526     C                   MOVEL     OPV(22)       OPZ006
153600970526     C                   MOVEL     OPV(23)       OPZ007
153700970526     C                   MOVEL     OPV(24)       OPZ008
153800970526     C                   END
153900970526     C*
154000970526     C                   ENDSR
154100970526     C*------------------------------------------------------------------------*
154200970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
154300970526     C*------------------------------------------------------------------------*
154400970526     C     OPZCON        BEGSR
154500970526     C*
154600970526     C* PULISCO CAMPI DI LAVORO
154700970526     C                   Z-ADD     0             I                 5 0
154800970526     C                   Z-ADD     0             W                 5 0
154900970526     C*
155000970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
155100970526     C     1             DO        24            I
155200970526     C                   MOVEL     OPD(I)        OPDRIG
155300970526     C*
155400970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
155500970526     C     OPZKEY        IFEQ      OPDOPZ
155600970526     C     OPZKEY        OREQ      OPDOP1
155700970526     C*
155800970526     C     OPDATT        IFEQ      'S'
155900970526     C                   MOVEL     OPDOPZ        OPZKEY
156000970526     C                   Z-ADD     I             W
156100970526     C                   END
156200970526     C                   END
156300970526     C*
156400970526     C                   END                                                    DO WHILE
156500970526     C*
156600970526     C* OPZIONE NON ATTIVA (ERRORE)
156700970526     C     W             IFEQ      0
156800970526     C*
156900970526     C* OPZIONE NON OK
157000970526     C                   Z-ADD     1             OPZOK
157100970526     C*
157200970526     C* EMISSIONE MESSAGGIO DI ERRORE
157300970526     C                   MOVEL     MSG(1)        DSMSMS
157400970526     C                   ELSE
157500970526     C*
157600970526     C* OPZIONE ATTIVA (PROCEDI ...)
157700010314     C*
157800141022     C     OPZKEY        IFEQ      ' 1'
157900141022     C                   EXSR      O01GES
158000140701     C                   ENDIF
158100141022     C*
158200141022     C     OPZKEY        IFEQ      ' 2'
158300141022     C                   EXSR      O02GES
158400141022     C                   ENDIF
158500141016     C*
158600141016     C     OPZKEY        IFEQ      ' 4'
158700141016     C                   EXSR      O04GES
158800141016     C                   ENDIF
158900141016     C*
159000141016     C     OPZKEY        IFEQ      ' 5'
159100141016     C                   EXSR      O05GES
159200141016     C                   ENDIF
159300141022     C*
159400141022     C     OPZKEY        IFEQ      ' 8'
159500141022     C                   EXSR      O08GES
159600141022     C                   ENDIF
159700141009     C*
159800141009     C     OPZKEY        IFEQ      '10'
159900141009     C                   EXSR      O10GES
160000141009     C                   ENDIF
160100140701     C                   ENDIF                                                  OPZ.NON ATTIVA
160200970526     C*
160300970526     C                   ENDSR
160400970526     C*------------------------------------------------------------------------*
160500970526     C* NOTE COSTRUZIONE SCHIERE
160600970526     C*------------------------------------------------------------------------*
160700970526     C*
160800970526     C* -MSG-MESSAGGI
160900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
161000970526     C* (NON TOCCARE MSG,1 E MSG,2)
161100970526     C*
161200970526     C* -CMD-COMANDI
161300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
161400970526     C*
161500970526     C* -OPD-OPZIONI
161600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
161700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
161800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
161900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
162000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
162100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
162200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
162300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
162400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
162500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
162600970526     C*
162700970526     C* -FUD-FUNZIONI
162800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
162900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
163000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
163100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
163200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
163300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
163400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
163500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
163600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
163700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
163800991108     C*
163900991110     C* -FLG-CODICI STATI DEI RECORD DI LOG
164000991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
164100991110     C* ================> IMPORTANTE <=================
164200991110     C* per codici di errore in -FLG- indicare in -DES- sempre
164300991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
164400991110     C* ERR
164500991108     C*
164600970526     O*------------------------------------------------------------------------*
164700970526** DEC - DECODIFICHE
164800970526    GESTIONE                                                     1
164900970526INTERROGAZIONE                                                   2
165000970530     SCELTA                                                      3
1651009705264
1652009705265
165300970526** MSG - MESSAGGI
165400140904TIS7GIR- Opzione non gestita                                                   1
165500140904TIS7GIR- Tasto funzionale non gestito                                          2
165600140904TIS7GIR- Storicizzazione già effettuata impossibile eseguire opzione richiesta.3
165700140904TIS7GIR- Elaborazione non effettuata, impossibile visualizzare storico.        4
165800140904TIS7GIR-                                                                       5
165900140904TIS7GIR- Accettato solo un parametro di inizio lista                           6
166000140904TIS7GIR- 7                                                                     7
166100140904TIS7GIR- 8                                                                     8
166200140904TIS7GIR- 9                                                                     9
166300140904TIS7GIR- 10                                                                   10
166400140904TIS7GIR- 11
166500140904TIS7GIR- Data errata                                                          12
166600140904TIS7GIR- Opzione valida per record di log con stato diverso da '0'            13
166700140904TIS7GIR- Per il cliente scelto non è consentita l'operazione richiesta.       14
166800140904TIS7GIR- L'opzione richiesta non è consentita per log a stato 'M'.            15
166900140904TIS7GIR- Effettuare selezione                                                 16
167000140904TIS7GIR- 17                                                                   17
167100140904TIS7GIR- 18                                                                   18
167200140904TIS7GIR- 19                                                                   19
167300140904TIS7GIR- 20                                                                   20
167400970528** CMD - COMANDI CL
167500991110DSPPFM FILE(
167600991110) MBR(
167700991110
167800970528
167900970528
168000970526** OPD - OPZIONI
168100141022 1=Abil.Servizi    -1 -     (S/N)=N                        opz. 01¬
168200141016 2=Regole Cli/TipoF-2 -     (S/N)=N                        opz. 02¬
168300141016                   -  -     (S/N)=N                        opz. 03¬
168400141016 4=Canc. log err.  -4 -     (S/N)=N                        opz. 04¬
168500141016 5=Visualizza      -5 -     (S/N)=N                        opz. 05¬
168600991110                   -  -     (S/N)=N                        opz. 06¬
168700141022                   -  -     (S/N)=N                        opz. 07¬
168800141022 8=Tabelle VAS     -8 -     (S/N)=S                        opz. 08¬
168900991110                   -  -     (S/N)=N                        opz. 09¬
16900014070210=Gestione Log    -10-     (S/N)=S                        opz. 10¬
169100140701                   -  -     (S/N)=N                        opz. 11¬
169200970526                   -  -     (S/N)=N                        opz. 12¬
169300970526                   -  -     (S/N)=N                        opz. 13¬
169400140701                   -  -     (S/N)=N                        opz. 14¬
169500140701                   -  -     (S/N)=N                        opz. 15¬
169600970526                   -  -     (S/N)=N                        opz. 16¬
169700970526                   -  -     (S/N)=N                        opz. 17¬
169800101005                   -  -     (S/N)=N                        opz. 18¬
169900970526                   -  -     (S/N)=N                        opz. 19¬
170000140701                   -  -     (S/N)=N                        opz. 20¬
170100970526                   -  -     (S/N)=N                        opz. 21¬
170200970526                   -  -     (S/N)=N                        opz. 22¬
170300970526                   -  -     (S/N)=N                        opz. 23¬
170400140701                   -  -     (S/N)=N                        opz. 25¬
170500970526** FUD - FUNZIONI
170600970526                   -        (S/N)=N                        funz.01¬
170700970526                   -        (S/N)=N                        funz.02¬
170800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
170900970526                   -        (S/N)=N                        funz.04¬
171000140701                   -        (S/N)=N                        funz.05¬
171100141015F06=Conferma       -        (S/N)=S                        funz.06¬
171200970526                   -        (S/N)=N                        funz.07¬
171300141022                   -        (S/N)=S                        funz.08¬
171400970527                   -        (S/N)=N                        funz.09¬
171500970527                   -        (S/N)=N                        funz.10¬
171600970526                   -        (S/N)=N                        funz.11¬
171700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
171800970526                   -        (S/N)=N                        funz.13¬
171900970526                   -        (S/N)=N                        funz.14¬
172000970526                   -        (S/N)=N                        funz.15¬
172100970526                   -        (S/N)=N                        funz.16¬
172200970526                   -        (S/N)=N                        funz.17¬
172300140701                   -        (S/N)=N                        funz.18¬
172400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
172500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
172600140701                   -        (S/N)=N                        funz.21¬
172700140701                   -        (S/N)=N                        funz.22¬
172800991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
172900991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
173000991108** FLG - STATI DEI RECORD DI LOG
1731009911080
1732009911081
1733009911082
1734009911083
1735009911084
173600060104M
173700991108
173800991108
173900991108
174000991108
174100991108** DES - STATI DEI RECORD DI LOG
174200991112Da El
174300991108OK
174400991108Err
174500991112R-OK
174600991112R-Err
174700060104Temp
174800101018Init
174900101018Init
175000101018Init
175100101018Init
