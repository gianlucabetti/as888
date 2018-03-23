000100970526     H*------------------------------------------------------------------------*
000200040621     H* Gestione file proposte Destinatari disagiati/supermercati/appuntamenti (AZDSTP0F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020416     FTISIT9D   CF   E             WORKSTN INFDS(DEVDS1)
000900020416     F                                     SFILE(SIT9D05:SFLULT)
001000020416     F                                     SFILE(SIT9DMS:MSGULT)
001100970526     F*------------
001200040621     Fazdstp1l  UF   E           K DISK
001300040621     Fazdstp2l  IF   E           K DISK    RENAME(azdstp00:azdstp02)
001400060131     ***Fazdst04l  IF   E           K DISK    PREFIX(i_)
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970528     D*------------
002700970528     D* COMANDI
002800970528     D*------------
002900970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI OPZIONI
003200970526     D*------------
003300970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI TASTI FUNZIONALI
003600970526     D*------------
003700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* OPZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970526     D OPV             S             19    DIM(24)
004200970526     D*------------
004300970526     D* FUNZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970527     D FUV             S             19    DIM(24)
004600991108     D*------------
004700970526     D*------------------------------------------------------------------------*
004800970526     D* INPUT
004900970526     D*------------------------------------------------------------------------*
005000970526     D*------------
005100970526     D* RIDEFINIZIONE OPZIONI VIDEO
005200970526     D*------------
005300970526     D OPDDS           DS
005400970526     D  OPDOPZ                 1      2
005500970526     D  OPDDE1                 3     19
005600970526     D  OPDOP1                21     22
005700970526     D  OPDFI0                23     34
005800970526     D  OPDATT                35     35
005900970526     D  OPDFI1                36     80
006000970526     D  OPDDES                 1     19
006100970526     D  OPDRIG                 1     80
006200970526     D*------------
006300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006400970526     D*------------
006500970526     D FUDDS           DS
006600970526     D  FUDDE1                 1      1
006700970526     D  FUDFUN                 2      3
006800970526     D  FUDDE2                 4     19
006900970526     D  FUDFI0                20     34
007000970526     D  FUDATT                35     35
007100970526     D  FUDFI1                36     80
007200970526     D  FUDDES                 1     19
007300970526     D  FUDRIG                 1     80
007400970526     D*------------
007500970526     D* DS GESTIONE TASTI FUNZIONALI
007600970526     D*------------
007700970526     D DSKEY         E DS
007800970526     D DEVDS1          DS
007900970526     D  KEY                  369    369
008000970526     D*------------
008100970526     D* DS "XMSG" - GESTIONE MESSAGGI
008200970526     D*------------
008300970526     D DSMSG         E DS
008400970526     D*------------
008500991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
008600970527     D*------------------
008700970527     D WLBDA8          DS                  INZ
008800970527     D  G08DAT                 1      8  0
008900970527     D  G08INV                 9     16  0
009000970527     D  G08ERR                17     17
009100970527     D  G08TGI                18     22  0
009200970526     D*------------
009300991110     D* DS DI PROCEDURA
009400970526     D*------------
009500970526     D* ARCHITETTURA
009600970526     D*------------
009700970526     D KPJBA         E DS
009800060104      *
009900060104     d Status         sds           333
010000060104     d   SDSpgm          *proc
010100040603     D*------------
010200040603     D* DS REPERIMENTO DATI UTENTE
010300040603     D*------------
010400040603     D TIBS34DS      E DS                                                       *Profili utente
010500040603     D DDATIUTE      E DS                                                       *Dati utente
010600040603     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010700040603     D DUTE01        E DS                                                       *Abilitazioni
010800040625     D*------------
010900040625     D* DS REPERIMENTO P.O.
011000040625     D*------------
011100040625     D TRUL31DS      E DS
011200040625     D*-------------------
011300040625     D* FILIALI DEI CLIENTI DA VISUALIZZARE
011400040625     D*-------------------
011500040625     D L1              S              3    DIM(250) INZ(*zeros)
011600991108     D*------------------
011700991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
011800991110     D*------------------
011900991110     D                 DS                  INZ
012000991110     D  MS1                    1     40
012100991110     D  MS2                   41     80
012200991110     D  MESS                   1     80
012300991108     D*------------------
012400020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
012500000407     D*------------------
012600020412     D TISIT0DS      E DS                  INZ
012700040621     D PARAM           DS
012800040621     D  DEPOP0                        3
012900040621     D  DEPPRO                        1
013000040621     D  DEPLNA                        3  0
013100020412     D*------------------
013200020412     D* VARIABILI DI WRK
013300020412     D*------------------
013400020412     D wrkRICERCA      S             35    VARYING
013500970526     C*------------------------------------------------------------------------*
013600970526     C* MAIN LINES
013700970526     C*------------------------------------------------------------------------*
013800970526     C*
013900970526     C* SE NON CI SONO ERRORI, PROSEGUE
014000970526IF  1C     ERRL00        IFNE      '1'
014100970526     C*
014200970526     C* CARICA IL SFILE
014300970526     C                   EXSR      POSSFL
014400970526     C*
014500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014900970526     C*
015000970526     C* REIMPOSTA ERRORE DI PROCERURA
015100970526     C                   MOVEL     '0'           ERRL00
015200970526     C*
015300970526     C* EMETTE IL SFILE
015400970526     C                   EXSR      OUTSFL
015500970526     C*
015600970526     C* GESTIONE TASTI FUNZIONALI
015700970526     C                   EXSR      FUNCON
015800970526     C*
015900970526E   2C                   ENDDO
016000970527     C*
016100970526E   1C                   ENDIF
016200040319     C*
016300040319     C* LANCIO IL DRIVER TISIT5R IN MODALITA' "SOLO CHIUSURA FILES"
016400040319     C*
016500040319     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
016600040319     C                   CLEAR                   TISIT0DS
016700040319     C                   MOVEL     'C'           IT0TLA
016800040319     C                   CALL      'TISIT5R'
016900040319     C                   PARM                    TISIT0DS
017000991108     C*
017100970526     C* FINE PGM
017200970526     C                   SETON                                        LR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F05GES        BEGSR
018600020716     C*
018700020716     C                   MOVEL     '0'           ERRL00
018800020716     C*
018900020716     C* CONTROLLA I PARAMETRI DI RICHIESTA POSIZIONAMENTO/SELEZIONE
019000020716     C                   EXSR      CHKPAR
019100020716     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019200970526     C*
019300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019400020716     C                   ENDIF
019500970526     C*
019600970526     C                   ENDSR
019700040621     C*------------------------------------------------------------------------*
019800040621     C* F06GES - GESTIONE F06 -> INSERIMENTO PROPOSTA APPUNTAMENTO
019900040621     C*------------------------------------------------------------------------*
020000040621     C     F06GES        BEGSR
020100040621     C*
020200040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
020300040621     C* LA PROVENIENZA DALLE PROPOSTE X PARTICOLARE "APPUNTAMENTI"
020400040621     C                   EVAL      DEPPRO = 'A'
020500040621     C                   EVAL      DEPLNA = *zeros
020600040621     C                   EVAL      KPJBU = PARAM
020700040621     C*
020800040621     C                   CLEAR                   TISIT0DS
020900040621     C                   MOVEL     'PF6'         IT0OP1
021000040621     C                   EVAL      IT0DOS = 'A'
021100040621     C                   CALL      'TISIT2R'
021200040621     C                   PARM                    KPJBA
021300040621     C                   PARM                    TISIT0DS
021400040621     C                   MOVEL     OT0MSG        DSMSMS
021500040621     C*
021600040621     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800991108     C* F12GES - GESTIONE F12 -> RITORNO
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F12GES        BEGSR
022100970526     C*
022200970526     C* IMPOSTA "PREMUTO F12"
022300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022400970526     C*
022500970526     C                   ENDSR
022600970528     C*------------------------------------------------------------------------*
022700970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
022800970528     C*------------------------------------------------------------------------*
022900970528     C     F21GES        BEGSR
023000970612     C*
023100991117     C* NON GESTITA
023200970528     C*
023300970528     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F23GES        BEGSR
023800970526     C*
023900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     RUPGES        BEGSR
025400970526     C*
025500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900020416     C* ENTGES - GESTIONE ENTER -> INVIO
026000970526     C*------------------------------------------------------------------------*
026100970526     C     ENTGES        BEGSR
026200000407     C*
026300000407     C                   MOVEL     '0'           ERRL00
026400020716     C*
026500020716     C* CONTROLLA I PARAMETRI DI RICHIESTA POSIZIONAMENTO/SELEZIONE
026600020716     C                   EXSR      CHKPAR
026700020716     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026800991108     C*
026900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027000970526     C                   EXSR      NEWPOS
027100970526     C*
027200991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027300020416     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027400020416     C     SWPOSI        IFNE      0
027500020416     C                   EXSR      POSSFL                                       *RICARICA SFILE
027600020416     C                   ELSE
027700020416     C     SFLULT        IFNE      0
027800020416     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027900020416     C*
028000020416     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
028100020416     C     SWMODI        IFNE      0
028200020416     C                   EXSR      POSSFL
028300020416     C                   ENDIF
028400020416     C                   ENDIF
028500020416     C                   ENDIF
028600020416     C                   ENDIF
028700020716     C                   ENDIF
028800020416     C*
028900020416     C                   ENDSR
029000020716     C*------------------------------------------------------------------------*
029100020716     C* CHKPAR - CONTROLLO PARAMETRI DI RICHIESTA POSIZIONAMENTO/SELEZIONE
029200020716     C*------------------------------------------------------------------------*
029300020716     C     CHKPAR        BEGSR
029400020716     C*
029500020716     C                   SETOFF                                       505152
029600040625     C                   SETOFF                                       53
029700020716     C*
029800020716     C* NELLA SECONDA RIGA DI PARAMETRI SI POSSONO IMPOSTARE SONO NEL SEGUENTE ORDINE:
029900020716     C* PRV - PRV+CAP - PRV+CAP+LOC, TUTTI GLI ALTRI ORDINI NN SONO CONSENTITI
030000020716     C                   IF        VIDPR2 =  *blanks AND
030100020716     C                             VIDCA2 =  *blanks AND
030200020716     C                             VIDLO2 <> *blanks
030300020716     C                   MOVEL     '1'           ERRL00
030400020716     C                   MOVEL     MSG(7)        DSMSMS
030500020716     C                   SETON                                        5051
030600020716     C                   ELSE
030700020716     C                   IF        VIDPR2 =  *blanks AND
030800020716     C                             VIDCA2 <> *blanks AND
030900020716     C                             VIDLO2 <> *blanks
031000020716     C                   MOVEL     '1'           ERRL00
031100020716     C                   MOVEL     MSG(8)        DSMSMS
031200020716     C                   SETON                                        50
031300020716     C                   ELSE
031400020716     C                   IF        VIDPR2 <> *blanks AND
031500020716     C                             VIDCA2 =  *blanks AND
031600020716     C                             VIDLO2 <> *blanks
031700020716     C                   MOVEL     '1'           ERRL00
031800020716     C                   MOVEL     MSG(9)        DSMSMS
031900020716     C                   SETON                                        51
032000020716     C                   ELSE
032100020716     C                   IF        VIDPR2 =  *blanks AND
032200020716     C                             VIDCA2 <> *blanks AND
032300020716     C                             VIDLO2 =  *blanks
032400020716     C                   MOVEL     '1'           ERRL00
032500020716     C                   MOVEL     MSG(10)       DSMSMS
032600020716     C                   SETON                                        5052
032700020716     C                   ENDIF
032800020716     C                   ENDIF
032900020716     C                   ENDIF
033000020716     C                   ENDIF
033100020716     C*
033200020716     C                   ENDSR
033300040621     C*------------------------------------------------------------------------*
033400040621     C* O02GES - GESTIONE OPZIONE 2=MODIFICA PROPOSTA
033500040621     C*------------------------------------------------------------------------*
033600040621     C     O02GES        BEGSR
033700040625     C*
033800040625     C* SE ENTRATA SOLO IN VISUALIZZAZOINE PROTEGGO VERIFICO ABILITAZIONE UTENTE ALL'ANAGRAFICA
033900040625     C                   IF        DEPOP0 = 'P05'
034000040625     C                   MOVEL     VIDLNA        wLNAAlfa          3
034100040625     C     wLNAAlfa      LOOKUP    L1                                     54
034200040625     C                   IF        *IN54 = *OFF
034300040625     C                   MOVEL     '1'           ERRL00
034400040625     C                   MOVEL     MSG(13)       DSMSMS
034500040625     C                   SETON                                        53
034600040625     C                   ENDIF
034700040625     C                   ENDIF
034800040621     C*
034900040621IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000040621     C     VIDATB        IFEQ      'A'
035100040621     C                   MOVEL     MSG(4)        DSMSMS
035200040621     C                   ELSE
035300040621     C     VIDDOS        IFNE      'A'
035400040621     C                   MOVEL     MSG(12)       DSMSMS
035500040621     C                   ELSE
035600040621     C*
035700040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
035800040621     C* LA PROVENIENZA DALLE PROPOSTE X PARTICOLARE "APPUNTAMENTI"
035900040621     C                   EVAL      DEPPRO = 'A'
036000040621     C                   EVAL      DEPLNA = VIDLNA
036100040621     C                   EVAL      KPJBU = PARAM
036200040621     C*
036300040621     C                   CLEAR                   TISIT0DS
036400040621     C                   MOVEL     'P02'         IT0OP1
036500040621     C                   EXSR      PASPAR
036600040621     C                   CALL      'TISIT2R'
036700040621     C                   PARM                    KPJBA
036800040621     C                   PARM                    TISIT0DS
036900040621     C                   MOVEL     OT0MSG        DSMSMS
037000040621     C                   ENDIF
037100040621E   1C                   ENDIF
037200040621     C                   ENDIF
037300040621     C*
037400040621     C                   ENDSR
037500000407     C*------------------------------------------------------------------------*
037600040621     C* O06GES - GESTIONE OPZIONE 6=CONFERMA PROPOSTA
037700000407     C*------------------------------------------------------------------------*
037800040621     C     O06GES        BEGSR
037900040621     C*
038000040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
038100040621     C* LA PROVENIENZA DALLE PROPOSTE NN X PARTICOLARE "APPUNTAMENTI"
038200040621     C                   EVAL      DEPPRO = 'P'
038300040621     C                   EVAL      DEPLNA = VIDLNA
038400040621     C                   EVAL      KPJBU = PARAM
038500000407     C*
038600000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038700000411     C     VIDATB        IFEQ      'A'
038800000411     C                   MOVEL     MSG(4)        DSMSMS
038900000411     C                   ELSE
039000020412     C                   CLEAR                   TISIT0DS
039100040621     C                   MOVEL     'P06'         IT0OP1
039200040621     C                   EXSR      PASPAR
039300020412     C                   CALL      'TISIT2R'
039400000407     C                   PARM                    KPJBA
039500020412     C                   PARM                    TISIT0DS
039600020416     C                   MOVEL     OT0MSG        DSMSMS
039700000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
039800000407E   1C                   ENDIF
039900000411     C                   ENDIF
040000000407     C*
040100000407     C                   ENDSR
040200020416     C*------------------------------------------------------------------------*
040300020416     C* O13GES - GESTIONE OPZIONE 13=CONFERMA PROPOSTA COME SINONIMO
040400020416     C*------------------------------------------------------------------------*
040500020416     C     O13GES        BEGSR
040600040621     C*
040700040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
040800040621     C* LA PROVENIENZA DALLE PROPOSTE NN X PARTICOLARE "APPUNTAMENTI"
040900040621     C                   EVAL      DEPPRO = 'P'
041000040621     C                   EVAL      DEPLNA = VIDLNA
041100040621     C                   EVAL      KPJBU = PARAM
041200020416     C*
041300020416IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041400020416     C     VIDATB        IFEQ      'A'
041500020416     C                   MOVEL     MSG(4)        DSMSMS
041600020416     C                   ELSE
041700020416     C                   CLEAR                   TISIT0DS
041800020416     C                   MOVEL     'P13'         IT0OP1
041900040621     C                   EXSR      PASPAR
042000040621     C                   EVAL      KPJBU = PARAM
042100020416     C                   CALL      'TISIT3R'
042200020416     C                   PARM                    KPJBA
042300040614     C                   PARM                    TISIT0DS
042400020416     C                   MOVEL     OT0MSG        DSMSMS
042500020416     C*                  EXSR      POSSFL                                       *RICARICA SFILE
042600020416E   1C                   ENDIF
042700020416     C                   ENDIF
042800020416     C*
042900020416     C                   ENDSR
043000991112     C*------------------------------------------------------------------------*
043100020416     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
043200991112     C*------------------------------------------------------------------------*
043300991112     C     O04GES        BEGSR
043400040625     C*
043500040625     C* SE ENTRATA SOLO IN VISUALIZZAZOINE PROTEGGO VERIFICO ABILITAZIONE UTENTE ALL'ANAGRAFICA
043600040625     C                   IF        DEPOP0 = 'P05'
043700040625     C                   MOVEL     VIDLNA        wLNAAlfa          3
043800040625     C     wLNAAlfa      LOOKUP    L1                                     54
043900040625     C                   IF        *IN54 = *OFF
044000040625     C                   MOVEL     '1'           ERRL00
044100040625     C                   MOVEL     MSG(13)       DSMSMS
044200040625     C                   SETON                                        53
044300040625     C                   ENDIF
044400040625     C                   ENDIF
044500991112     C*
044600000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044700000411     C     VIDATB        IFEQ      'A'
044800000411     C                   MOVEL     MSG(4)        DSMSMS
044900000411     C                   ELSE
045000020416     C                   EXSR      ANNULLA
045100000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
045200000407E   1C                   ENDIF
045300000411     C                   ENDIF
045400991112     C*
045500991112     C                   ENDSR
045600991014     C*------------------------------------------------------------------------*
045700000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
045800970526     C*------------------------------------------------------------------------*
045900970526     C     O05GES        BEGSR
046000040621     C*
046100040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
046200040621     C* LA PROVENIENZA DALLE PROPOSTE NN X PARTICOLARE "APPUNTAMENTI"
046300040621     C                   EVAL      DEPPRO = 'P'
046400040621     C                   EVAL      DEPLNA = VIDLNA
046500040621     C                   EVAL      KPJBU = PARAM
046600000407     C*
046700000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046800020412     C                   CLEAR                   TISIT0DS
046900020416     C                   MOVEL     'P05'         IT0OP1
047000040621     C                   EXSR      PASPAR
047100020412     C                   CALL      'TISIT2R'
047200000407     C                   PARM                    KPJBA
047300020412     C                   PARM                    TISIT0DS
047400000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
047500000407E   1C                   ENDIF
047600000407     C*
047700970526     C                   ENDSR
047800030423     C*------------------------------------------------------------------------*
047900030423     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE RECORD DESTINATARIO
048000030423     C*------------------------------------------------------------------------*
048100030423     C     O15GES        BEGSR
048200040621     C*
048300040621     C* VALORIZZO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
048400040621     C* LA PROVENIENZA DALLE PROPOSTE NN X PARTICOLARE "APPUNTAMENTI"
048500040621     C                   EVAL      DEPPRO = 'P'
048600040621     C                   EVAL      DEPLNA = VIDLNA
048700040621     C                   EVAL      KPJBU = PARAM
048800030423     C*
048900030423IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049000030424     C*
049100030424     C                   IF        V2DRAG = *blanks
049200030424     C                   MOVEL     MSG(11)       DSMSMS
049300030424     C                   ELSE
049400030423     C                   CLEAR                   TISIT0DS
049500030423     C                   MOVEL     'P15'         IT0OP1
049600040621     C                   EXSR      PASPAR
049700030423     C                   CALL      'TISIT2R'
049800030423     C                   PARM                    KPJBA
049900030423     C                   PARM                    TISIT0DS
050000030423     C*                  EXSR      POSSFL                                       *RICARICA SFILE
050100030424     C                   ENDIF
050200030423E   1C                   ENDIF
050300030423     C*
050400030423     C                   ENDSR
050500000411     C*------------------------------------------------------------------------*
050600020416     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
050700000411     C*------------------------------------------------------------------------*
050800000411     C     O07GES        BEGSR
050900040625     C*
051000040625     C* SE ENTRATA SOLO IN VISUALIZZAZOINE PROTEGGO VERIFICO ABILITAZIONE UTENTE ALL'ANAGRAFICA
051100040625     C                   IF        DEPOP0 = 'P05'
051200040625     C                   MOVEL     VIDLNA        wLNAAlfa          3
051300040625     C     wLNAAlfa      LOOKUP    L1                                     54
051400040625     C                   IF        *IN54 = *OFF
051500040625     C                   MOVEL     '1'           ERRL00
051600040625     C                   MOVEL     MSG(13)       DSMSMS
051700040625     C                   SETON                                        53
051800040625     C                   ENDIF
051900040625     C                   ENDIF
052000000411     C*
052100000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052200000411     C     VIDATB        IFEQ      *BLANKS
052300000411     C                   MOVEL     MSG(5)        DSMSMS
052400000411     C                   ELSE
052500020416     C                   EXSR      RIPRIST
052600000411     C*                  EXSR      POSSFL                                       *RICARICA SFILE
052700000411E   1C                   ENDIF
052800000411     C                   ENDIF
052900000411     C*
053000000411     C                   ENDSR
053100991028     C*------------------------------------------------------------------------*
053200040601     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGGIO PARAMETRI
053300991112     C*------------------------------------------------------------------------*
053400991112     C     PASPAR        BEGSR
053500060131      *
053600060131     c                   select
053700060131     c                   when      IT0OP1 = 'P15'
053800060131     ***C*
053900060131     ***C* "Chaino" il record del destinatario x reperire tutti i dati necessari da passare alla DS
054000060131     ***C* di wrk.
054100060131     ***C     V2HCOD        CHAIN     AZDST04L
054200060131     ***C                   IF        %found(AZDST04L)
054300060131     ***C                   EVAL      IT0NAZ = i_DSTNAZ
054400060131     ***C                   EVAL      IT0PRV = i_DSTPRV
054500060131     ***C                   EVAL      IT0CAP = i_DSTCAP
054600060131     ***C                   EVAL      IT0LOC = i_DSTLOC
054700060131     ***C                   EVAL      IT0IND = i_DSTIND
054800060131     ***C                   EVAL      IT0KWI = i_DSTKWI
054900060131     ***C                   EVAL      IT0RAG = i_DSTRAG
055000060131     ***C                   EVAL      IT0DOS = i_DSTDOS
055100060131     ***C                   EVAL      IT0TEL = i_DSTTEL
055200060131     ***C                   EVAL      IT0REF = i_DSTREF
055300060131     ***C                   ENDIF
055400060131      * Passo il codice; i dati anagrafici verranno reperiti dal chiamato
055500060131     c                   eval      IT0cod = V2Hcod
055600060131      *
055700060131     c                   when      IT0OP1 = 'P13'
055800020416     C                   EVAL      IT0NAZ = VIHNAZ
055900020416     C                   EVAL      IT0PRV = VIDPRV
056000040621     C                   EVAL      IT0CAP = VIHCAP
056100020416     C                   EVAL      IT0LOC = VIHLOC
056200020416     C                   EVAL      IT0IND = VIHIND
056300020416     C                   EVAL      IT0KWI = V2HKWI
056400020416     C                   EVAL      IT0RAG = V2HRAG
056500020416     C                   EVAL      IT0SIN = VIHRAG
056600060131      *
056700060131     c                   other
056800020412     C                   EVAL      IT0NAZ = VIHNAZ
056900020412     C                   EVAL      IT0PRV = VIDPRV
057000040621     C                   EVAL      IT0CAP = VIHCAP
057100020412     C                   EVAL      IT0LOC = VIHLOC
057200020416     C                   EVAL      IT0IND = VIHIND
057300020412     C                   EVAL      IT0RAG = VIHRAG
057400020422     C                   EVAL      IT0DOS = VIHDOS
057500040601     C                   EVAL      IT0TEL = VIHTEL
057600040601     C                   EVAL      IT0REF = VIHREF
057700060131      *
057800060131     c                   endsl
057900060131      *
058000020412     C                   ENDSR
058100020416     C*------------------------------------------------------------------------*
058200020416     C* ANNULLA - ANNULLAMENTO RECORD CORRENTE
058300020416     C*------------------------------------------------------------------------*
058400020416     C     ANNULLA       BEGSR
058500020416     C*
058600040621     C     KEYDST1       CHAIN     AZDSTP1L
058700040621     C                   IF        %found(AZDSTP1L)
058800020416     C                   EVAL      DSTATB = 'A'
058900040621     C                   UPDATE    AZDSTP00
059000020416     C                   ENDIF
059100020416     C*
059200020416     C                   ENDSR
059300020416     C*------------------------------------------------------------------------*
059400020416     C* RIPRIST - RIPRISTINO RECORD CORRENTE
059500020416     C*------------------------------------------------------------------------*
059600020416     C     RIPRIST       BEGSR
059700020416     C*
059800040621     C     KEYDST1       CHAIN     AZDSTP1L
059900040621     C                   IF        %found(AZDSTP1L)
060000020416     C                   EVAL      DSTATB = *blanks
060100040621     C                   UPDATE    AZDSTP00
060200020416     C                   ENDIF
060300020416     C*
060400020416     C                   ENDSR
060500970526     C*------------------------------------------------------------------------*
060600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
060700970526     C*------------------------------------------------------------------------*
060800970526     C     NEWPOS        BEGSR
060900970526     C*
061000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
061100970526     C*         1= RICHIESTO POSIZIONAMENTO
061200970526     C                   Z-ADD     0             SWPOSI            7 0
061300970526     C*
061400991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
061500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
061600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
061700970526     C                   Z-ADD     0             SWMODI            1 0
061800970530     C*
061900970530     C* REIMPOSTA INDICATORI DI ERRORE
062000991108     C                   SETOFF                                       808182
062100991108     C                   SETOFF                                       83
062200970526     C*
062300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
062400020416     C     VIDRS2        IFNE      deprs2
062500040625     C     VIDFLX        ORNE      depflx
062600020416     C     VIDPR2        ORNE      deppr2
062700020715     C     VIDCA2        ORNE      depca2
062800020715     C     VIDLO2        ORNE      deplo2
062900020416     C     VIDNM2        ORNE      depnm2
063000040625     C     VIDLA2        ORNE      depla2
063100040621     C     VIDSO2        ORNE      depso2
063200020412     C     ERRL00        IFEQ      '0'                                          *NO ERORI
063300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
063400020412     C                   MOVEL     VIDRS2        deprs2                         *DEPOSITI
063500040625     C                   MOVEL     VIDFLX        depflx                         *DEPOSITI
063600020416     C                   MOVEL     VIDPR2        deppr2                         *DEPOSITI
063700020715     C                   MOVEL     VIDCA2        depca2                         *DEPOSITI
063800020715     C                   MOVEL     VIDLO2        deplo2                         *DEPOSITI
063900020416     C                   MOVEL     VIDNM2        depnm2                         *DEPOSITI
064000040625     C                   MOVEL     VIDLA2        depla2                         *DEPOSITI
064100040621     C                   MOVEL     VIDSO2        depso2                         *DEPOSITI
064200020412     C                   ENDIF
064300020412     C                   ENDIF
064400970526     C*
064500020708     C* DI DEFUALT IMPOSTO COME LETTURA QUELLA X PRV+RAG
064600020716     C                   MOVEL     'PRV'         WTIPPOS           3
064700020708     C*
064800020708     C* DETERMINO IL TIPO DI POSIZIONAMENTO DA EFFETTUARE IN BASE ALLE RICHIESTE A VIDEO
064900020710     C                   IF        VIDFLX =  *blanks AND
065000020710     C                             VIDrs2 <> *blanks
065100020710     C                   MOVEL     'RAG'         WTIPPOS
065200020710     C                   ENDIF
065300020716     C                   IF        VIDpr2 <> *blanks OR
065400020716     C                             VIDca2 <> *blanks OR
065500020715     C                             VIDlo2 <> *blanks
065600020710     C                   MOVEL     'PRV'         WTIPPOS
065700020708     C                   ENDIF
065800020708     C*
065900970526     C                   ENDSR
066000970526     C*------------------------------------------------------------------------*
066100970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
066200970526     C*------------------------------------------------------------------------*
066300970528     C     SETFIL        BEGSR
066400020412     C*
066500020708     C* IN BASE AL TIPO DI POSIZIOINAMENTO EFFETTUO LETTURE DIFFERENTI
066600040625     C                   MOVEL(P)  VIDrs2        KDSRAG
066700020708     C                   IF        WTIPPOS = 'RAG'
066800040621     C     KEYDST2       SETLL     azdstp2l                           99
066900020708     C                   ELSE
067000020715     C                   MOVEL(P)  VIDLO2        KDSLOC
067100040621     C     KEYDST1_2     SETLL     azdstp1l                           99
067200020708     C                   ENDIF
067300980107     C*
067400970526     C* LETTURA RECORD
067500020412     C     *IN99         IFEQ      *OFF
067600970526     C                   EXSR      LETFIL
067700020412     C                   ENDIF
067800970526     C*
067900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
068000020412     C     *IN99         IFEQ      *ON
068100970526     C                   Z-ADD     1             SFLEOF
068200020412     C                   ENDIF
068300970526     C*
068400970526     C                   ENDSR
068500970526     C*------------------------------------------------------------------------*
068600970526     C* LETFIL - LETTURA RECORD
068700970526     C*------------------------------------------------------------------------*
068800970526     C     LETFIL        BEGSR
068900970526     C*
069000970526     C* LEGGE FINO A:
069100970526     C*     - FINE FILE
069200970526     C*     - TROVATO RECORD VALIDO
069300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
069400020412     C     *IN99         DOWEQ     *OFF
069500970526     C     WRECOK        ANDEQ     'N'
069600970526     C*
069700020708     C                   IF        WTIPPOS = 'RAG'
069800040621     C                   READ      azdstp2l                               99
069900020708     C                   ELSE
070000040621     C                   READ(N)   azdstp1l                               99
070100020708     C                   ENDIF
070200020412     C*
070300020412     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
070400020412     C     *IN99         IFEQ      *ON                                          *FINE FILE
070500020412     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
070600020412     C                   ELSE
070700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
070800020412     C                   ENDIF
070900020412     C                   ENDDO
071000020412     C*
071100020412     C                   ENDSR
071200020412     C*------------------------------------------------------------------------*
071300020412     C* CHKREC - CONTROLLA VALIDITA' RECORD
071400020412     C*------------------------------------------------------------------------*
071500020412     C     CHKREC        BEGSR
071600020412     C*
071700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
071800040618     C*
071900040618     C* SE ENTRATA IN INTERROGAZIONE (+ GESTIONE APPUNTAMNETI) FILTRO SOLO I TIPI 'A'
072000040618     C                   IF        WRECOK = 'S'
072100040618     C                   IF        DEPOP0 = 'P05'
072200040618     C                   IF        DSTDOS <> 'A'
072300040618     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072400040618     C                   ENDIF
072500040618     C                   ENDIF
072600040618     C                   ENDIF
072700020412     C*
072800020412     C* FILTRA SE RICHIESTE SELEZIONI
072900040618     C                   IF        WRECOK = 'S'
073000020708     C     VIDFLX        IFNE      *blanks
073100020412     C     VIDRS2        IFEQ      *blanks
073200020412     C                   ELSE
073300020412     C                   EVAL      %len(wrkRICERCA) = %len(%trim(VIDRS2))
073400020412     C                   EVAL      wrkRICERCA = %trim(VIDRS2)
073500020412     C     wrkRICERCA    SCAN      DSTRAG
073600020412     C                   IF        not %found
073700991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
073800020412     C                   ENDIF
073900020412     C                   ENDIF
074000020708     C                   ENDIF
074100040618     C                   ENDIF
074200020412     C*
074300040618     C                   IF        WRECOK = 'S'
074400020416     C     VIDPR2        IFEQ      *blanks
074500020412     C                   ELSE
074600020416     C     VIDPR2        IFNE      DSTPRV
074700020412     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074800020412     C                   ENDIF
074900020412     C                   ENDIF
075000040618     C                   ENDIF
075100020715     C*
075200040618     C                   IF        WRECOK = 'S'
075300020715     C     VIDCA2        IFEQ      *blanks
075400020715     C                   ELSE
075500020715     C     VIDCA2        IFNE      DSTCAP
075600020715     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075700020715     C                   ENDIF
075800040618     C                   ENDIF
075900020715     C                   ENDIF
076000040625     C*
076100040625     C                   IF        WRECOK = 'S'
076200040625     C     VIDLA2        IFEQ      *zeros
076300040625     C                   ELSE
076400040625     C     VIDLA2        IFNE      DSTLNA
076500040625     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076600040625     C                   ENDIF
076700040625     C                   ENDIF
076800040625     C                   ENDIF
076900040621     C*
077000040621     C                   IF        WRECOK = 'S'
077100040621     C     VIDSO2        IFEQ      *blanks
077200040621     C                   ELSE
077300040621     C     VIDSO2        IFNE      DSTDOS
077400040621     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
077500040621     C                   ENDIF
077600040621     C                   ENDIF
077700040621     C                   ENDIF
077800020715     C*
077900040618     C*                  IF        WRECOK = 'S'
078000020716     C*    VIDLO2        IFEQ      *blanks
078100020716     C*                  ELSE
078200020716     C*                  EVAL      %len(wrkRICERCA) = %len(%trim(VIDLO2))
078300020716     C*                  EVAL      wrkRICERCA = %trim(VIDLO2)
078400020716     C*    wrkRICERCA    SCAN      DSTLOC
078500020716     C*                  IF        not %found
078600020716     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
078700020716     C*                  ENDIF
078800020716     C*                  ENDIF
078900040618     C*                  ENDIF
079000020422     C*
079100040618     C                   IF        WRECOK = 'S'
079200020422     C                   IF        DSTNUM >= VIDNM2
079300020422     C                   ELSE
079400020422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
079500020422     C                   ENDIF
079600040618     C                   ENDIF
079700991108     C*
079800970526     C                   ENDSR
079900970526     C*------------------------------------------------------------------------*
080000000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
080100970526     C*------------------------------------------------------------------------*
080200970526     C     IRESFL        BEGSR
080300970526     C*
080400000407     C                   MOVEL     *blanks       OPZKEY
080500021205     C                   MOVEL     *blanks       OPZKE2
080600020412     C                   MOVEL     DSTATB        VIDATB
080700020412     C                   MOVEL     DSTRAG        VIDRAG
080800020412     C                   MOVEL     DSTRAG        VIHRAG
080900020412     C                   MOVEL     DSTNAZ        VIHNAZ
081000020412     C                   MOVEL     DSTPRV        VIDPRV
081100040621     C                   MOVEL     DSTCAP        VIHCAP
081200040621     C                   MOVEL     DSTCAP        VIDCAP
081300020412     C                   MOVEL     DSTLOC        VIDLOC
081400020412     C                   MOVEL     DSTLOC        VIHLOC
081500020416     C                   MOVEL     DSTIND        VIDIND
081600020416     C                   MOVEL     DSTIND        VIHIND
081700020422     C                   MOVEL     DSTDOS        VIDDOS
081800020422     C                   MOVEL     DSTDOS        VIHDOS
081900040601     C                   MOVEL     DSTTEL        VIHTEL
082000040601     C                   MOVEL     DSTREF        VIHREF
082100020416     C                   Z-ADD     DSTNUM        VIDNUM
082200040621     C                   Z-ADD     DSTLNA        VIDLNA
082300020415     C*
082400020416     C                   EXSR      CHKDST
082500020416     C*
082600970526     C                   ENDSR
082700020416     C*------------------------------------------------------------------------*
082800020416     C* CHKDST - OPERAZIONI X REPERIMENTO DESTINATARIO DISAG./SUPERMERC.
082900020416     C*------------------------------------------------------------------------*
083000020416     C     CHKDST        BEGSR
083100020416     C*
083200020416     C                   CLEAR                   TISIT0DS
083300020416     C*
083400020416     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
083500020416     C                   EXSR      IMPCAM
083600020416     C*
083700020416     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
083800040319     C                   EVAL      IT0TLA = 'P'
083900020416     C                   CALL      'TISIT5R'
084000020416     C                   PARM                    TISIT0DS
084100020416     C*
084200020416     C* IMPOSTO A VIDEO I CAMPI RESTITUITI NELLA DS DI PASSAGGIO DI PROCEDURA
084300020416     C                   EVAL      V2HNAZ = OT0NAZ
084400020416     C                   EVAL      V2DPRV = OT0PRV
084500020416     C                   EVAL      V2DCAP = OT0CAP
084600020416     C                   EVAL      V2DLOC = OT0LOC
084700020416     C                   EVAL      V2HLOC = OT0LOC
084800020416     C                   EVAL      V2DIND = OT0IND
084900020416     C                   EVAL      V2HIND = OT0IND
085000020416     C                   EVAL      V2DRAG = OT0RAG
085100020416     C                   EVAL      V2HRAG = OT0RAG
085200020416     C                   EVAL      V2HKWI = OT0KWI
085300030424     C                   EVAL      V2HCOD = OT0COD
085400030424     C                   EVAL      V2DDOS = OT0DOS
085500020416     C*
085600020416     C                   ENDSR
085700020416     C*------------------------------------------------------------------------*
085800020416     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
085900020416     C*------------------------------------------------------------------------*
086000020416     C     IMPCAM        BEGSR
086100020416     C*
086200020416     C                   MOVEL     VIHRAG        IT0RAG
086300020416     C                   MOVEL     VIHNAZ        IT0NAZ
086400020416     C                   MOVEL     VIDPRV        IT0PRV
086500040621     C                   MOVEL     VIHCAP        IT0CAP
086600020416     C                   MOVEL     VIHLOC        IT0LOC
086700020416     C                   MOVEL     VIHIND        IT0IND
086800020416     C*
086900020416     C                   ENDSR
087000970526     C*------------------------------------------------------------------------*
087100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
087200970526     C*------------------------------------------------------------------------*
087300970526     C     CHKFOP        BEGSR
087400991112     C*
087500991112     C* OPZIONI
087600970626     C*
087700000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
087800000420IF  1C     DEPOP0        IFEQ      'P01'
087900000420X   1C                   ELSE
088000000420     C* OPZIONI
088100040618     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA PROP.
088200040618     C                   MOVEL     'S'           OPDATT
088300040618     C                   MOVEL     OPDRIG        OPD(2)
088400021205     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZ. PROP.
088500970626     C                   MOVEL     'S'           OPDATT
088600970626     C                   MOVEL     OPDRIG        OPD(5)
088700040618     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE PROP.
088800040618     C                   MOVEL     'S'           OPDATT
088900040618     C                   MOVEL     OPDRIG        OPD(4)
089000040618     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO PROP.
089100040618     C                   MOVEL     'S'           OPDATT
089200040618     C                   MOVEL     OPDRIG        OPD(7)
089300040618     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO APPUNT.
089400040618     C                   MOVEL     'S'           FUDATT
089500040618     C                   MOVEL     FUDRIG        FUD(6)
089600970626E   1C                   ENDIF
089700970626     C*
089800000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
089900970626IF  1C     DEPOP0        IFEQ      'P02'
090000040618     C                   MOVEL     OPD(6)        OPDRIG                         *CONFERMA COME DEST.
090100970626     C                   MOVEL     'S'           OPDATT
090200040618     C                   MOVEL     OPDRIG        OPD(6)
090300020416     C                   MOVEL     OPD(13)       OPDRIG                         *CONFERMA COME SIN.
090400020416     C                   MOVEL     'S'           OPDATT
090500020416     C                   MOVEL     OPDRIG        OPD(13)
090600040618     C                   MOVEL     OPD(15)       OPDRIG                         *INTERROGAZ. DEST.
090700040618     C                   MOVEL     'S'           OPDATT
090800040618     C                   MOVEL     OPDRIG        OPD(15)
090900970626E   1C                   ENDIF
091000970526     C*
091100970526     C* GESTIONE OPZIONI A VIDEO
091200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
091300970526     C                   EXSR      OPZGES
091400970526     C*
091500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
091600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
091700970526     C                   EXSR      FUNGES
091800970526     C*
091900970526     C                   ENDSR
092000040603     C*--------------------------------------------------------------------------------------------*
092100040603     C* REPERISCE DATI UTENTE
092200040603     C*--------------------------------------------------------------------------------------------*
092300040603     C     REPDATIUTE    BEGSR
092400040603     C*
092500040603     C* REimposta le variabili di lavoro
092600040603     C                   CLEAR                   TIBS34DS
092700040603     C                   CLEAR                   AZUTEDS
092800040603     C                   CLEAR                   DDATIUTE
092900040603     C*
093000040603     C* Reperisce i dati del profilo in ingresso
093100040603     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
093200040603     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
093300040603     C                   IN(E)     *DTAARA
093400040603     C                   IF        %Error                                       *NO errori
093500040603     C                   EVAL      I34TLA = 'L'
093600040603     C                   CALL      'TIBS34R'
093700040603     C                   PARM                    TIBS34DS
093800040603     C                   IN        *DTAARA
093900040603     C                   ENDIF
094000040603     C*
094100040603     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
094200040603     C                   EVAL      DUTE01 = UTEFAF
094300040625     C*
094400040625     C* REPERISCO I P.O. A CUI L'UTENTE E' AUTORIZZATO IN BASE AL LIVELLO D AUTORIZZAZIONE SU AZUTE
094500040625     C                   CLEAR                   TRUL31DS
094600040625     C                   CLEAR                   L1
094700040625     C                   EVAL      I31abi = UTEAUT
094800040625     C                   EVAL      I31cpo = DUTPOU
094900040625     C                   CALL      'TRUL31R'
095000040625     C                   PARM                    KPJBA
095100040625     C                   PARM                    TRUL31DS
095200040625     C                   IF        O31pog > *zeros
095300040625     C                   MOVEA     O31pog        L1
095400040625     C                   ELSE
095500040625     C                   MOVEL     DUTPOU        L1(1)
095600040625     C                   ENDIF
095700040625     C* SE UTENTE CON LIVELLO AUTORITA 'AZ' => AGGIUNGO ALLA LISTA P.O. IN GESTIONE ANCHE 046
095800040625     C                   IF        UTEAUT = 'AZ'
095900040625     C                   MOVEL     '046'         L1(250)
096000040625     C                   ENDIF
096100040603     C*
096200040603     C                   ENDSR
096300970526     C*------------------------------------------------------------------------*
096400970526     C* *INZSR - OPERAZIONI INIZIALI
096500970526     C*------------------------------------------------------------------------*
096600970526     C     *INZSR        BEGSR
096700970526     C*---
096800970526     C* RICEVIMENTO PARAMETRI
096900970526     C*---
097000970526     C     *ENTRY        PLIST
097100970612     C                   PARM                    KPJBA
097200040621     C                   MOVEL     KPJBU         PARAM
097300060104     c                   movel     SDSpgm        V1Tpgm
097400040621     C*
097500040621     C* VALORIZZO SUBITO IL PARAMETRO (X PASSAGGIO PARAMETRI A *PGM SUCCESIVI) CHE INDICA
097600040621     C* LA PROVENIENZA DALLE PROPOSTE
097700040621     C                   EVAL      DEPPRO = 'P'
097800040603     C*
097900040603     C* REPERISCE I DATI DELL'UTENTE
098000040603     C                   EXSR      REPDATIUTE
098100040603     C*
098200040603     C* IN BASE ALLE AUTIZZAZIONI UTENTE PILOTO IL LIVELLO DI GESTIONE (QUINDI NN + DA KAZN)
098300040603     C* ...SE ENTRATA NN IN "SCELTA" VALORIZZO
098400040603     C                   IF        DEPOP0  <> 'P01'
098500040603     C* ...DI DEFAULT IMPOSTO SOLO VISUALIZZAZIONE
098600040603     C                   EVAL      DEPOP0  = 'P05'
098700040603     C* ...SE PREVISTO IMPOSTO GESTIONE COMPLETA
098800040603     C                   IF        §UTEPRO = 'S'
098900040603     C                   EVAL      DEPOP0  = 'P02'
099000040603     C                   ENDIF
099100040603     C                   ENDIF
099200040603     C*
099300040603     C* BY-PASSO L'ENTRATA IN KPJBU ORIGINALE CON QUELLA APPENA STABILITA
099400040621     C                   MOVEL     PARAM         KPJBU
099500040621     C*
099600040621     C* SE ENTRATA SOLO IN VISUALIZZAZOINE PROTEGGO IL CAMPO D SELEZIONE RELATIVO AL TIPO PROPOSTA
099700040621     C                   IF        DEPOP0 = 'P05'
099800040625     C                   EVAL      VIDSO2 = 'A'
099900040625     C                   EVAL      depSO2 = VIDSO2
100000040621     C                   SETON                                        96
100100040621     C                   ENDIF
100200970526     C*
100300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
100400970526     C     XMSGPA        PLIST
100500970526     C                   PARM                    DSMSMS
100600970526     C                   PARM                    DSMSPG
100700970526     C                   PARM                    DSMSKE
100800970526     C                   PARM                    DSMSER
100900970526     C*
101000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
101100970526     C                   CALL      'XKEY'
101200970526     C                   PARM                    DSKEY
101300970526     C*---
101400970526     C* VARIABILI NON RIFERITE AL DATA BASE
101500970526     C*---
101600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
101700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
101800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
101900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
102000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
102100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
102200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
102300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
102400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
102500970526     C*---
102600020412     C     *LIKE         DEFINE    vidrs2        deprs2
102700040625     C     *LIKE         DEFINE    vidflx        depflx
102800020416     C     *LIKE         DEFINE    vidpr2        deppr2
102900020715     C     *LIKE         DEFINE    vidca2        depca2
103000020715     C     *LIKE         DEFINE    vidlo2        deplo2
103100020416     C     *LIKE         DEFINE    vidnm2        depnm2
103200040625     C     *LIKE         DEFINE    vidla2        depla2
103300040621     C     *LIKE         DEFINE    vidso2        depso2
103400991108     C*---
103500970526     C* VARIABILI DI CONTROLLO
103600970526     C*---
103700970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
103800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
103900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
104000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
104100970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
104200000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
104300020416     C                   MOVEL     'P'           KDSTIP            1            *TIPO DEST.
104400020715     C                   MOVEL     *BLANKS       KDSNAZ            3            *NAZIONE
104500020715     C                   MOVEL     *BLANKS       KDSLOC           35            *LOCALITA
104600040625     C                   MOVEL     *BLANKS       KDSRAG           35            *RAGIONE SOCIALE
104700020422     C                   Z-ADD     *ZEROS        VIDNM2
104800970526     C*
104900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
105000020416     C                   MOVEL     'TISIT9R'     DSMSPG
105100020416     C                   MOVEL     'TISIT9R'     MSGQUE
105200970526     C                   MOVEL     *BLANKS       MSGKEY
105300970526     C                   MOVEL     *BLANKS       DSMSMS
105400970526     C                   MOVEL     *BLANKS       DSMSKE
105500970526     C                   Z-ADD     *ZEROS        DSMSER
105600970526     C*---
105700970526     C* CHIAVI DI LETTURA
105800970526     C*---
105900040621     C* LETTURA azdstp1l -
106000020412     C     KEYDST1       KLIST
106100020416     C                   KFLD                    KDSTIP
106200020416     C                   KFLD                    VIHNAZ
106300020712     C                   KFLD                    VIDPRV
106400020416     C                   KFLD                    VIHLOC
106500040621     C                   KFLD                    VIHCAP
106600020416     C                   KFLD                    VIHRAG
106700020426     C                   KFLD                    VIHIND
106800040707     C                   KFLD                    VIDLNA
106900020715     C     KEYDST1_2     KLIST
107000020715     C                   KFLD                    KDSTIP
107100020715     C                   KFLD                    KDSNAZ
107200020715     C                   KFLD                    VIDPR2
107300020715     C                   KFLD                    KDSLOC
107400020715     C                   KFLD                    VIDCA2
107500040625     C                   KFLD                    KDSRAG
107600040621     C* LETTURA azdstp2l -
107700020708     C     KEYDST2       KLIST
107800020708     C                   KFLD                    KDSTIP
107900040625     C                   KFLD                    KDSRAG
108000020708     C                   KFLD                    VIDpr2
108100991115     C*
108200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
108300970526     C                   EXSR      CHKFOP
108400970526     C*
108500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
108600020412     C     DEPOP0        IFEQ      'P01'
108700040603     C                   MOVEL     DEC(1)        VIDING
108800020412     C                   ENDIF
108900020412     C     DEPOP0        IFEQ      'P02'
109000040603     C                   MOVEL     DEC(2)        VIDING
109100020412     C                   ENDIF
109200020412     C     DEPOP0        IFEQ      'P05'
109300040603     C                   MOVEL     DEC(5)        VIDING
109400020412     C                   ENDIF
109500020412     C*
109600020412     C                   ENDSR
109700970526     C*------------------------------------------------------------------------*
109800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
110100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
110200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
110300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110500970526     C*------------------------------------------------------------------------*
110600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
110700970526     C*------------------------------------------------------------------------*
110800970526     C     POSSFL        BEGSR
110900970526     C*
111000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
111100970526     C                   EXSR      CLRSFL
111200970526     C*
111300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
111400970526     C                   EXSR      SETFIL
111500970526     C*
111600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
111700970526     C     SFLEOF        IFEQ      0
111800970526     C                   EXSR      CARSFL
111900970526     C                   END
112000970526     C*
112100970526     C                   ENDSR
112200970526     C*------------------------------------------------------------------------*
112300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
112400970526     C*------------------------------------------------------------------------*
112500970526     C     ENTATT        BEGSR
112600970526     C*
112700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
112800970526     C                   Z-ADD     0             OPZOK             1 0
112900970526     C*
113000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
113100970526     C                   Z-ADD     0             SFLVAR            5 0
113200970526     C*
113300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
113400970526     C                   Z-ADD     0             SFLERR
113500970526     C*
113600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
113700970526     C                   Z-ADD     SFLULT        WFLULT
113800970526     C*
113900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
114000020416     C                   READC     SIT9D05                                98
114100970526     C     *IN98         DOWEQ     '0'
114200970526     C*
114300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
114400970526     C                   ADD       1             SFLVAR
114500970526     C*
114600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
114700970526     C     OPZOK         IFEQ      0
114800970526     C*
114900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
115000970526     C     OPZKEY        IFNE      *BLANKS
115100970526     C                   EXSR      OPZCON
115200970526     C                   END
115300021205     C     OPZKE2        IFNE      *BLANKS
115400021205     C                   EXSR      OPZCON
115500021205     C                   END
115600970526     C*
115700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
115800970526     C*            2. PULIZIA SCELTA IN SFL
115900970526     C     OPZOK         IFEQ      0
116000970526     C                   MOVEL     '0'           *IN41
116100970526     C                   MOVEL     *BLANKS       OPZKEY
116200021205     C                   MOVEL     *BLANKS       OPZKE2
116300970526     C                   ELSE
116400970526     C*
116500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
116600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
116700970526     C                   MOVEL     '1'           *IN41
116800970526     C                   Z-ADD     SFLULT        SFLERR
116900970526     C                   END
117000970526     C                   ELSE
117100970526     C*
117200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
117300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
117400970526     C                   MOVEL     '1'           *IN41
117500970526     C                   END                                                    OPZOK=0
117600970526     C*
117700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
117800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
117900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
118000970526     C     OPZOK         IFNE      0
118100970526     C                   Z-ADD     SFLERR        SFLNBR
118200970526     C                   ELSE
118300970526     C                   Z-ADD     SFLULT        SFLNBR
118400970526     C                   END
118500970526     C*
118600970526     C* AGGIORNO RECORD SFL
118700020416     C                   UPDATE    SIT9D05
118800970526     C*
118900020416     C                   READC     SIT9D05                                98
119000970526     C                   END                                                    DO WHILE
119100970526     C*
119200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
119300970526     C                   Z-ADD     WFLULT        SFLULT
119400970526     C*
119500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
119600970526     C     SFLVAR        IFEQ      0
119700970526     C                   END
119800970526     C*
119900020708     C***  OPZKEY        IFNE      ' 5'
120000020708     C***                EXSR      F05GES
120100020708     C***                ENDIF
120200991112     C*
120300970526     C                   ENDSR
120400970526     C*------------------------------------------------------------------------*
120500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
120600970526     C*------------------------------------------------------------------------*
120700970526     C     CLRSFL        BEGSR
120800970526     C*
120900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
121000970526     C* (0=NON FINE FILE  1=FINE FILE)
121100970526     C                   Z-ADD     0             SFLEOF
121200970526     C*
121300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
121400970526     C                   Z-ADD     0             SFLULT
121500970526     C*
121600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
121700970526     C                   MOVEL     '1'           *IN40
121800020416     C                   WRITE     SIT9D06
121900970526     C                   MOVEL     '0'           *IN40
122000970526     C*
122100970526     C                   ENDSR
122200970526     C*------------------------------------------------------------------------*
122300970526     C* CARSFL - CARICAMENTO SFL
122400970526     C*------------------------------------------------------------------------*
122500970526     C     CARSFL        BEGSR
122600970526     C*
122700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
122800970526     C     SFLULT        IFEQ      0
122900970526     C                   EXSR      SETFIL
123000970526     C                   END
123100970526     C*
123200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
123300970526     C                   Z-ADD     SFLULT        WFLULT
123400970526     C*
123500970526     C* CICLO:
123600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
123700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
123800970526     C                   Z-ADD     1             SFLRIG
123900970526     C     SFLRIG        DOWLE     SFLPAG
124000970526     C     SFLEOF        ANDEQ     0
124100970526     C*
124200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
124300991116     C                   MOVEL     *BLANKS       OPZKEY
124400021205     C                   MOVEL     *BLANKS       OPZKE2
124500970526     C*
124600970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
124700970526     C                   EXSR      IRESFL
124800970526     C*
124900970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
125000970526     C                   ADD       1             SFLULT
125100970526     C*
125200970526     C* SCRITTURA RECORD SFL
125300020416     C                   WRITE     SIT9D05
125400970526     C                   ADD       1             SFLRIG
125500970526     C*
125600970526     C* LETTURA FILE DA CARICARE
125700970526     C                   EXSR      LETFIL
125800970526     C                   END                                                    DO WHILE
125900970526     C*
126000970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
126100970526     C     WFLULT        IFNE      SFLULT
126200970526     C                   Z-ADD     WFLULT        SFLNBR
126300970526     C                   ADD       1             SFLNBR
126400970526     C                   END
126500970526     C*
126600970526     C                   ENDSR
126700970526     C*------------------------------------------------------------------------*
126800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
126900970526     C*------------------------------------------------------------------------*
127000970526     C     OUTSFL        BEGSR
127100970526     C*
127200970526     C* WRITE RECORD TESTATA VIDEO
127300020416     C                   WRITE     SIT9D01
127400970526     C*
127500970526     C* VISUALIZZAZIONE MESSAGGIO
127600970526     C                   EXSR      SNDMSG
127700970526     C*
127800970526     C* WRITE RECORD OPZIONI VIDEO
127900020416     C                   WRITE     SIT9D02
128000970526     C*
128100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
128200020416     C                   WRITE     SIT9D03
128300970526     C*
128400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
128500020416     C                   WRITE     SIT9D04
128600970526     C*
128700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
128800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
128900970526     C     SFLEOF        IFEQ      0
129000970526     C                   MOVEL     '0'           *IN42
129100970526     C                   ELSE
129200970526     C*
129300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
129400970526     C                   MOVEL     '1'           *IN42
129500970526     C                   END
129600970526     C*
129700970526     C* WRITE RECORD TASTI FUNZIONALI
129800020416     C                   WRITE     SIT9D09
129900970526     C*
130000970526     C* CASO DI SFL VUOTO
130100970526     C     SFLULT        IFLE      0
130200970526     C*
130300970526     C*    - WRITE RECORD VUOTO
130400020416     C                   WRITE     SIT9D0A
130500020416     C                   READ      SIT9D0A                                99
130600970526     C*
130700970526     C                   ELSE
130800970526     C*
130900970526     C* CASO DI SFL CON RECORDS
131000970526     C*
131100970526     C*    - WRITE RECORD CONTROLLO SFL
131200020416     C                   WRITE     SIT9D06
131300020416     C                   READ      SIT9D06                                99
131400970526     C                   END
131500970526     C*
131600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
131700020416     C                   READ      SIT9D03                                99
131800970526     C*
131900970526     C                   ENDSR
132000970526     C*------------------------------------------------------------------------*
132100970526     C* SNDMSG - MANDA MESSAGGIO
132200970526     C*------------------------------------------------------------------------*
132300970526     C     SNDMSG        BEGSR
132400970526     C*
132500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
132600970526     C                   MOVEL     '1'           *IN40
132700020416     C                   WRITE     SIT9DMC
132800970526     C                   MOVEL     '0'           *IN40
132900970526     C*
133000970526     C* VISUALIZZAZIONE MESSAGGIO
133100970526     C                   Z-ADD     0             MSGULT
133200970526     C                   CALL      'XMSG'        XMSGPA
133300970526     C     DSMSER        IFEQ      0
133400970526     C                   MOVEL     DSMSKE        MSGKEY
133500970526     C                   END
133600970526     C                   Z-ADD     1             MSGULT
133700020416     C                   WRITE     SIT9DMS                                      SFL MESS
133800020416     C                   WRITE     SIT9DMC                                      CONTR MESS
133900970526     C*
134000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
134100970526     C                   MOVEL     *BLANKS       DSMSMS
134200970526     C*
134300970526     C                   ENDSR
134400970526     C*------------------------------------------------------------------------*
134500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
134600970526     C*------------------------------------------------------------------------*
134700970526     C     FUNCON        BEGSR
134800970526     C*
134900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
135000970526     C                   EXSR      FUNVAR
135100970526     C*
135200970526     C* PULISCO CAMPI DI LAVORO
135300970526     C                   Z-ADD     0             I                 5 0
135400970526     C                   Z-ADD     0             W                 5 0
135500970526     C*
135600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
135700970526     C     FUNKEY        IFNE      *BLANKS
135800970526     C     1             DO        24            I
135900970526     C                   MOVEL     FUD(I)        FUDRIG
136000970526     C*
136100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
136200970526     C     FUNKEY        IFEQ      FUDFUN
136300970526     C     FUDATT        ANDEQ     'S'
136400970526     C                   Z-ADD     I             W
136500970526     C                   END
136600970526     C                   END                                                    DO WHILE
136700970526     C*
136800970526     C* FUNZIONE NON ATTIVA (ERRORE)
136900970526     C     W             IFEQ      0
137000970526     C*
137100970526     C* EMISSIONE MESSAGGIO DI ERRORE
137200970526     C                   MOVEL     MSG(2)        DSMSMS
137300970526     C                   END                                                    W=0
137400970526     C                   END                                                    FUNKEY NE *BLANKS
137500970526     C*
137600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
137700970526     C     W             IFNE      0
137800970526     C     FUNKEY        OREQ      *BLANKS
137900970526     C*
138000970526     C     FUNKEY        IFEQ      *BLANKS
138100970526     C                   EXSR      ENTGES
138200970526     C                   END
138300970526     C     FUNKEY        IFEQ      '03'
138400970526     C                   EXSR      F03GES
138500970526     C                   END
138600970526     C     FUNKEY        IFEQ      '05'
138700970526     C                   EXSR      F05GES
138800970526     C                   END
138900040621     C     FUNKEY        IFEQ      '06'
139000040621     C                   EXSR      F06GES
139100040621     C                   END
139200970526     C     FUNKEY        IFEQ      '12'
139300970526     C                   EXSR      F12GES
139400970526     C                   END
139500970528     C     FUNKEY        IFEQ      '21'
139600970528     C                   EXSR      F21GES
139700970528     C                   END
139800970526     C     FUNKEY        IFEQ      '23'
139900970526     C                   EXSR      F23GES
140000970526     C                   END
140100970526     C     FUNKEY        IFEQ      '24'
140200970526     C                   EXSR      F24GES
140300970526     C                   END
140400970526     C     FUNKEY        IFEQ      'RU'
140500970526     C                   EXSR      RUPGES
140600970526     C                   END
140700970526     C*
140800970526     C                   END
140900970526     C*
141000970526     C                   ENDSR
141100970526     C*------------------------------------------------------------------------*
141200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
141300970526     C*------------------------------------------------------------------------*
141400970526     C     FUNVAR        BEGSR
141500970526     C*
141600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
141700970526     C     KEY           IFEQ      ENTER
141800970526     C                   MOVEL     *BLANKS       FUNKEY
141900970526     C                   END
142000970526     C     KEY           IFEQ      F1
142100970526     C                   MOVEL     '01'          FUNKEY
142200970526     C                   END
142300970526     C     KEY           IFEQ      F2
142400970526     C                   MOVEL     '02'          FUNKEY
142500970526     C                   END
142600970526     C     KEY           IFEQ      F3
142700970526     C                   MOVEL     '03'          FUNKEY
142800970526     C                   END
142900970526     C     KEY           IFEQ      F4
143000970526     C                   MOVEL     '04'          FUNKEY
143100970526     C                   END
143200970526     C     KEY           IFEQ      F5
143300970526     C                   MOVEL     '05'          FUNKEY
143400970526     C                   END
143500970526     C     KEY           IFEQ      F6
143600970526     C                   MOVEL     '06'          FUNKEY
143700970526     C                   END
143800970526     C     KEY           IFEQ      F7
143900970526     C                   MOVEL     '07'          FUNKEY
144000970526     C                   END
144100970526     C     KEY           IFEQ      F8
144200970526     C                   MOVEL     '08'          FUNKEY
144300970526     C                   END
144400970526     C     KEY           IFEQ      F9
144500970526     C                   MOVEL     '09'          FUNKEY
144600970526     C                   END
144700970526     C     KEY           IFEQ      F10
144800970526     C                   MOVEL     '10'          FUNKEY
144900970526     C                   END
145000970526     C     KEY           IFEQ      F11
145100970526     C                   MOVEL     '11'          FUNKEY
145200970526     C                   END
145300970526     C     KEY           IFEQ      F12
145400970526     C                   MOVEL     '12'          FUNKEY
145500970526     C                   END
145600970526     C     KEY           IFEQ      F13
145700970526     C                   MOVEL     '13'          FUNKEY
145800970526     C                   END
145900970526     C     KEY           IFEQ      F14
146000970526     C                   MOVEL     '14'          FUNKEY
146100970526     C                   END
146200970526     C     KEY           IFEQ      F15
146300970526     C                   MOVEL     '15'          FUNKEY
146400970526     C                   END
146500970526     C     KEY           IFEQ      F16
146600970526     C                   MOVEL     '16'          FUNKEY
146700970526     C                   END
146800970526     C     KEY           IFEQ      F17
146900970526     C                   MOVEL     '17'          FUNKEY
147000970526     C                   END
147100970526     C     KEY           IFEQ      F18
147200970526     C                   MOVEL     '18'          FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F19
147500970526     C                   MOVEL     '19'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F20
147800970526     C                   MOVEL     '20'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F21
148100970526     C                   MOVEL     '21'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F22
148400970526     C                   MOVEL     '22'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F23
148700970526     C                   MOVEL     '23'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F24
149000970526     C                   MOVEL     '24'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      ROLLUP
149300970526     C                   MOVEL     'RU'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      HELP
149600970526     C                   MOVEL     'HP'          FUNKEY
149700970526     C                   END
149800970526     C*
149900970526     C                   ENDSR
150000970526     C*------------------------------------------------------------------------*
150100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
150200970526     C*------------------------------------------------------------------------*
150300970526     C     FUNGES        BEGSR
150400970526     C*
150500970526     C* PULISCO CAMPI DI LAVORO
150600970526     C                   Z-ADD     0             W                 5 0
150700970526     C*
150800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
150900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
151000970526     C     1             DO        24            I
151100970526     C                   MOVEL     FUD(I)        FUDRIG
151200970526     C*
151300970526     C* PULIZIA FUNZIONI PRECEDENTI
151400970526     C                   MOVEL     *BLANKS       FUV(I)
151500970526     C*
151600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
151700970526     C* (SALTA F23 E F24)
151800970526     C     FUDATT        IFEQ      'S'
151900970526     C     FUDFUN        ANDNE     '23'
152000970526     C     FUDFUN        ANDNE     '24'
152100970526     C                   ADD       1             W
152200970526     C                   MOVEL     FUDDES        FUV(W)
152300970526     C                   END
152400970526     C                   END                                                    DO WHILE
152500970526     C*
152600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
152700970526     C* ALTRIMENTI DISATTIVO F24
152800970526     C     1             DO        24            I
152900970526     C                   MOVEL     FUD(I)        FUDRIG
153000970526     C*
153100970526     C     FUDFUN        IFEQ      '24'
153200970526     C     W             IFGT      6
153300970526     C                   MOVEL     'S'           FUDATT
153400970526     C                   ELSE
153500970526     C                   MOVEL     *BLANKS       FUDATT
153600970526     C                   END                                                    W GT 8
153700970526     C                   MOVEL     FUDRIG        FUD(I)
153800970526     C                   END
153900970526     C                   END                                                    DO WHILE
154000970526     C*
154100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
154200970526     C     W             DIV       6             I
154300970526     C                   MVR                     Z
154400970526     C*
154500970526     C     Z             IFNE      0
154600970526     C     I             ADD       1             Z
154700970526     C                   ELSE
154800970526     C                   Z-ADD     I             Z
154900970526     C                   END
155000970526     C*
155100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
155200970526     C                   ADD       1             FUNGRU
155300970526     C     FUNGRU        IFGT      Z
155400970526     C     FUNGRU        ORLT      1
155500970526     C                   Z-ADD     1             FUNGRU
155600970526     C                   END
155700970526     C*
155800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
155900970526     C     FUNGRU        IFEQ      1
156000970526     C                   MOVEL     FUV(1)        FUN001
156100970526     C                   MOVEL     FUV(2)        FUN002
156200970526     C                   MOVEL     FUV(3)        FUN003
156300970526     C                   MOVEL     FUV(4)        FUN004
156400970526     C                   MOVEL     FUV(5)        FUN005
156500970526     C                   MOVEL     FUV(6)        FUN006
156600970526     C                   END
156700970526     C*
156800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
156900970526     C     FUNGRU        IFEQ      2
157000970526     C                   MOVEL     FUV(7)        FUN001
157100970526     C                   MOVEL     FUV(8)        FUN002
157200970526     C                   MOVEL     FUV(9)        FUN003
157300970526     C                   MOVEL     FUV(10)       FUN004
157400970526     C                   MOVEL     FUV(11)       FUN005
157500970526     C                   MOVEL     FUV(12)       FUN006
157600970526     C                   END
157700970526     C*
157800970526     C* TERZO GRUPPO DI 6 FUNZIONI
157900970526     C     FUNGRU        IFEQ      3
158000970526     C                   MOVEL     FUV(13)       FUN001
158100970526     C                   MOVEL     FUV(14)       FUN002
158200970526     C                   MOVEL     FUV(15)       FUN003
158300970526     C                   MOVEL     FUV(16)       FUN004
158400970526     C                   MOVEL     FUV(17)       FUN005
158500970526     C                   MOVEL     FUV(18)       FUN006
158600970526     C                   END
158700970526     C*
158800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
158900970526     C     FUNGRU        IFEQ      4
159000970526     C                   MOVEL     FUV(19)       FUN001
159100970526     C                   MOVEL     FUV(20)       FUN002
159200970526     C                   MOVEL     FUV(21)       FUN003
159300970526     C                   MOVEL     FUV(22)       FUN004
159400970526     C                   MOVEL     *BLANKS       FUN005
159500970526     C                   MOVEL     *BLANKS       FUN006
159600970526     C                   END
159700970526     C*
159800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
159900970526     C                   MOVEL     *BLANKS       FUN007
160000970526     C                   MOVEL     *BLANKS       FUN008
160100970526     C*
160200970526     C     1             DO        24            I
160300970526     C                   MOVEL     FUD(I)        FUDRIG
160400970526     C     FUDFUN        IFEQ      '23'
160500970526     C     FUDATT        ANDEQ     'S'
160600970526     C                   MOVEL     FUDDES        FUN007
160700970526     C                   END
160800970526     C     FUDFUN        IFEQ      '24'
160900970526     C     FUDATT        ANDEQ     'S'
161000970526     C                   MOVEL     FUDDES        FUN008
161100970526     C                   END
161200970526     C                   END                                                    DO
161300970526     C*
161400970526     C                   ENDSR
161500970526     C*------------------------------------------------------------------------*
161600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
161700970526     C*------------------------------------------------------------------------*
161800970526     C     OPZGES        BEGSR
161900970526     C*
162000970526     C* PULISCO CAMPI DI LAVORO
162100970526     C                   Z-ADD     0             W                 5 0
162200970526     C*
162300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
162400970526     C* E CARICO TABELLA OPZIONI ATTIVE
162500991115     C     1             DO        24            I
162600970526     C                   MOVEL     OPD(I)        OPDRIG
162700970526     C*
162800970526     C* PULIZIA OPZIONI PRECEDENTI
162900970526     C                   MOVEL     *BLANKS       OPV(I)
163000970526     C*
163100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
163200970526     C     OPDATT        IFEQ      'S'
163300970526     C                   ADD       1             W
163400970526     C                   MOVEL     OPDDES        OPV(W)
163500970526     C                   END
163600970526     C                   END                                                    DO WHILE
163700970526     C*
163800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
163900970526     C* ALTRIMENTI DISATTIVO F23
164000991115     C     1             DO        24            I
164100970526     C                   MOVEL     FUD(I)        FUDRIG
164200970526     C*
164300970526     C     FUDFUN        IFEQ      '23'
164400970526     C     W             IFGT      8
164500970526     C                   MOVEL     'S'           FUDATT
164600970526     C                   ELSE
164700970526     C                   MOVEL     *BLANKS       FUDATT
164800970526     C                   END                                                    W GT 8
164900970526     C                   MOVEL     FUDRIG        FUD(I)
165000970526     C                   END
165100970526     C                   END                                                    DO WHILE
165200970526     C*
165300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
165400970526     C     W             DIV       8             I
165500970526     C                   MVR                     Z                 5 0
165600970526     C*
165700970526     C     Z             IFNE      0
165800970526     C     I             ADD       1             Z
165900970526     C                   ELSE
166000970526     C                   Z-ADD     I             Z
166100970526     C                   END
166200970526     C*
166300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
166400970526     C                   ADD       1             OPZGRU
166500970526     C     OPZGRU        IFGT      Z
166600970526     C     OPZGRU        ORLT      1
166700970526     C                   Z-ADD     1             OPZGRU
166800970526     C                   END
166900970526     C*
167000970526     C* PRIMO GRUPPO DI 8 OPZIONI
167100970526     C     OPZGRU        IFEQ      1
167200970526     C                   MOVEL     OPV(1)        OPZ001
167300970526     C                   MOVEL     OPV(2)        OPZ002
167400970526     C                   MOVEL     OPV(3)        OPZ003
167500970526     C                   MOVEL     OPV(4)        OPZ004
167600970526     C                   MOVEL     OPV(5)        OPZ005
167700970526     C                   MOVEL     OPV(6)        OPZ006
167800970526     C                   MOVEL     OPV(7)        OPZ007
167900970526     C                   MOVEL     OPV(8)        OPZ008
168000970526     C                   END
168100970526     C*
168200970526     C* SECONDO GRUPPO DI 8 OPZIONI
168300970526     C     OPZGRU        IFEQ      2
168400970526     C                   MOVEL     OPV(9)        OPZ001
168500970526     C                   MOVEL     OPV(10)       OPZ002
168600970526     C                   MOVEL     OPV(11)       OPZ003
168700970526     C                   MOVEL     OPV(12)       OPZ004
168800970526     C                   MOVEL     OPV(13)       OPZ005
168900970526     C                   MOVEL     OPV(14)       OPZ006
169000970526     C                   MOVEL     OPV(15)       OPZ007
169100970526     C                   MOVEL     OPV(16)       OPZ008
169200970526     C                   END
169300970526     C*
169400970526     C* TERZO GRUPPO DI 8 OPZIONI
169500970526     C     OPZGRU        IFEQ      3
169600970526     C                   MOVEL     OPV(17)       OPZ001
169700970526     C                   MOVEL     OPV(18)       OPZ002
169800970526     C                   MOVEL     OPV(19)       OPZ003
169900970526     C                   MOVEL     OPV(20)       OPZ004
170000970526     C                   MOVEL     OPV(21)       OPZ005
170100970526     C                   MOVEL     OPV(22)       OPZ006
170200970526     C                   MOVEL     OPV(23)       OPZ007
170300970526     C                   MOVEL     OPV(24)       OPZ008
170400970526     C                   END
170500970526     C*
170600970526     C                   ENDSR
170700970526     C*------------------------------------------------------------------------*
170800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
170900970526     C*------------------------------------------------------------------------*
171000970526     C     OPZCON        BEGSR
171100970526     C*
171200970526     C* PULISCO CAMPI DI LAVORO
171300970526     C                   Z-ADD     0             I                 5 0
171400970526     C                   Z-ADD     0             W                 5 0
171500970526     C*
171600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
171700970526     C     1             DO        24            I
171800970526     C                   MOVEL     OPD(I)        OPDRIG
171900970526     C*
172000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
172100970526     C     OPZKEY        IFEQ      OPDOPZ
172200970526     C     OPZKEY        OREQ      OPDOP1
172300970526     C*
172400970526     C     OPDATT        IFEQ      'S'
172500970526     C                   MOVEL     OPDOPZ        OPZKEY
172600970526     C                   Z-ADD     I             W
172700970526     C                   END
172800970526     C                   END
172900021205     C*
173000021205     C* SALVO INDICE DI OPZIONE DI 2° RIGA TROVATA ED ATTIVA (W)
173100021205     C     OPZKE2        IFEQ      OPDOPZ
173200021205     C     OPZKE2        OREQ      OPDOP1
173300021205     C*
173400021205     C     OPDATT        IFEQ      'S'
173500021205     C                   MOVEL     OPDOPZ        OPZKE2
173600021205     C                   Z-ADD     I             W
173700021205     C                   END
173800021205     C                   END
173900970526     C*
174000970526     C                   END                                                    DO WHILE
174100970526     C*
174200970526     C* OPZIONE NON ATTIVA (ERRORE)
174300970526     C     W             IFEQ      0
174400970526     C*
174500970526     C* OPZIONE NON OK
174600970526     C                   Z-ADD     1             OPZOK
174700970526     C*
174800970526     C* EMISSIONE MESSAGGIO DI ERRORE
174900970526     C                   MOVEL     MSG(1)        DSMSMS
175000970526     C                   ELSE
175100970526     C*
175200970526     C* OPZIONE ATTIVA (PROCEDI ...)
175300970526     C*
175400000407     C     OPZKEY        IFEQ      ' 2'
175500000407     C                   EXSR      O02GES
175600000407     C                   END
175700991110     C     OPZKEY        IFEQ      ' 4'
175800991110     C                   EXSR      O04GES
175900991014     C                   END
176000970526     C     OPZKEY        IFEQ      ' 5'
176100970526     C                   EXSR      O05GES
176200970526     C                   END
176300040621     C     OPZKEY        IFEQ      ' 6'
176400040621     C                   EXSR      O06GES
176500040621     C                   END
176600000411     C     OPZKEY        IFEQ      ' 7'
176700000411     C                   EXSR      O07GES
176800000411     C                   END
176900020416     C     OPZKEY        IFEQ      '13'
177000020416     C                   EXSR      O13GES
177100020416     C                   END
177200021205     C     OPZKE2        IFEQ      '15'
177300030423     C                   EXSR      O15GES
177400021205     C                   ELSE
177500021205     C     OPZKE2        IFNE      *BLANKS
177600021205     C* EMISSIONE MESSAGGIO DI ERRORE
177700021205     C                   MOVEL     MSG(1)        DSMSMS
177800021205     C                   END
177900021205     C                   END
178000970526     C                   END                                                    OPZ.NON ATTIVA
178100970526     C*
178200970526     C                   ENDSR
178300970526     C*------------------------------------------------------------------------*
178400970526     C* NOTE COSTRUZIONE SCHIERE
178500970526     C*------------------------------------------------------------------------*
178600970526     C*
178700970526     C* -MSG-MESSAGGI
178800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
178900970526     C* (NON TOCCARE MSG,1 E MSG,2)
179000970526     C*
179100970526     C* -CMD-COMANDI
179200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
179300970526     C*
179400970526     C* -OPD-OPZIONI
179500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
179600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
179700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
179800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
179900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
180000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
180100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
180200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
180300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
180400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
180500970526     C*
180600970526     C* -FUD-FUNZIONI
180700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
180800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
180900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
181000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
181100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
181200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
181300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
181400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
181500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
181600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
181700991108     C*
181800991115     C* -STS-CODICI STATI DEI RECORD DI LOG
181900991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
182000991110     C* ================> IMPORTANTE <=================
182100991115     C* per codici di errore in -STS- indicare in -DES- sempre
182200991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
182300991110     C* ERR
182400991108     C*
182500970526     O*------------------------------------------------------------------------*
182600970526** DEC - DECODIFICHE
182700040603     SCELTA                                                      1
182800040603    GESTIONE                                                     2
182900040603                                                                 3
183000040603                                                                 4
183100040618GESTIONE APPUNT.                                                 5
183200970526** MSG - MESSAGGI
183300020416TISIT9R- Opzione non gestita.                                                  1
183400020416TISIT9R- Tasto funzionale non gestito.                                         2
183500020416TISIT9R- Non abilitato all inserimento in filiale.                             3
183600020416TISIT9R- Record già annullato, consentito solo visualizz. e ripristino         4
183700020416TISIT9R- Record non annullato, impossibile ripristinare                        5
183800020416TISIT9R- Accettato solo un parametro di inizio lista                           6
183900020716TISIT9R- Indicare anche PRV e CAP.                                             7
184000020716TISIT9R- Indicare anche PRV.                                                   8
184100020716TISIT9R- Indicare anche CAP.                                                   9
184200020716TISIT9R- Indicare anche PRV e volendo anche LOC.                              10
184300030424TISIT9R- Opzione non consentita, nessun destinatario abbinato alla proposta.  11
184400040621TISIT9R- Opzione consentita solo per destinatari A=Appuntamento.              12
184500040625TISIT9R- Utente non autorizzato alla gestione dell'anagrafica.                13
184600020416TISIT9R- 14                                                                   14
184700020416TISIT9R- 15                                                                   15
184800020416TISIT9R- 16                                                                   16
184900020416TISIT9R- 17                                                                   17
185000020416TISIT9R- 18                                                                   18
185100020416TISIT9R- 19                                                                   19
185200020416TISIT9R- 20                                                                   20
185300970528** CMD - COMANDI CL
185400991110DSPPFM FILE(
185500991110) MBR(
185600991110
185700970528
185800970528
185900970526** OPD - OPZIONI
186000020416                   -  -     (S/N)=N                        opz. 01¬
186100040621 2=Modifica appunt.-2 -     (S/N)=N                        opz. 02¬
186200020416                   -  -     (S/N)=N                        opz. 03¬
186300000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
186400000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
186500040618 6=Conferma        -6 -     (S/N)=N                        opz. 06¬
186600000411 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
186700991110                   -  -     (S/N)=N                        opz. 08¬
186800991110                   -  -     (S/N)=N                        opz. 09¬
186900991110                   -  -     (S/N)=N                        opz. 10¬
187000000407                   -  -     (S/N)=N                        opz. 11¬
187100970526                   -  -     (S/N)=N                        opz. 12¬
18720002041613=Crea sinonimo   -13-     (S/N)=N                        opz. 13¬
187300021205                   -  -     (S/N)=N                        opz. 14¬
18740002120515=Visualizza Dest.-15-     (S/N)=N                        opz. 15¬
187500970526                   -  -     (S/N)=N                        opz. 16¬
187600970526                   -  -     (S/N)=N                        opz. 17¬
187700970526                   -  -     (S/N)=N                        opz. 18¬
187800970526                   -  -     (S/N)=N                        opz. 19¬
187900970526                   -  -     (S/N)=N                        opz. 20¬
188000970526                   -  -     (S/N)=N                        opz. 21¬
188100970526                   -  -     (S/N)=N                        opz. 22¬
188200970526                   -  -     (S/N)=N                        opz. 23¬
188300000407                   -  -     (S/N)=N                        opz. 25¬
188400970526** FUD - FUNZIONI
188500970526                   -        (S/N)=N                        funz.01¬
188600970526                   -        (S/N)=N                        funz.02¬
188700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
188800970526                   -        (S/N)=N                        funz.04¬
188900970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
189000040618F06=Ins. appunt.   -        (S/N)=S  <== S-FISSO           funz.06¬
189100970526                   -        (S/N)=N                        funz.07¬
189200970527                   -        (S/N)=N                        funz.08¬
189300970527                   -        (S/N)=N                        funz.09¬
189400970527                   -        (S/N)=N                        funz.10¬
189500970526                   -        (S/N)=N                        funz.11¬
189600970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
189700991116                   -        (S/N)=N                        funz.13¬
189800970526                   -        (S/N)=N                        funz.14¬
189900970526                   -        (S/N)=N                        funz.15¬
190000970526                   -        (S/N)=N                        funz.16¬
190100970526                   -        (S/N)=N                        funz.17¬
190200970526                   -        (S/N)=N                        funz.18¬
190300970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
190400970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
190500000407                   -        (S/N)=N                        funz.21¬
190600991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
190700991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
190800991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
