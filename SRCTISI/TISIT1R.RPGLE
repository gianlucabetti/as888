000100050923     H DFTACTGRP(*NO) ACTGRP(*NEW)
000200050923     H*
000300040601     H*
000400040601     H* ATTENZIONE!!!  => Compilare questo programma i seguenti parametri:
000500040601     H*
000600040601     H* Gruppo di attivazione assunto  . DFTACTGRP    > *NO
000700040601     H* Gruppo di attivazione  . . . . . ACTGRP       > *NEW
000800040601     H*
000900970526     H*------------------------------------------------------------------------*
001000020412     H* Gestione file anagrafico Destinatari disagiati/supermercati (AZDST00F)
001100970526     H*------------------------------------------------------------------------*
001200970526     H DECEDIT('0,') DATEDIT(*DMY.)
001300970526     F*------------------------------------------------------------------------*
001400970526     F* DATA BASE
001500970526     F*------------------------------------------------------------------------*
001600020412     FTISIT1D   CF   E             WORKSTN INFDS(DEVDS1)
001700020412     F                                     SFILE(SIT1D05:SFLULT)
001800020412     F                                     SFILE(SIT1DMS:MSGULT)
001900970526     F*------------
002000020415     Fazdst01l  IF   E           K DISK
002100020415     Fazdst02l  IF   E           K DISK
002200020412     F                                     RENAME(azdst000:azdstxx2)
002300020415     Fazdss03l  IF   E           K DISK
002400970526     D*------------------------------------------------------------------------*
002500970526     D* SCHIERE
002600970526     D*------------------------------------------------------------------------*
002700970526     D*------------
002800970526     D* DECODIFICHE
002900970526     D*------------
003000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* MESSAGGI
003300970526     D*------------
003400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003500970528     D*------------
003600970528     D* COMANDI
003700970528     D*------------
003800970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI OPZIONI
004100970526     D*------------
004200970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* DESCRIZIONI TASTI FUNZIONALI
004500970526     D*------------
004600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004700970526     D*------------
004800970526     D* OPZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970526     D OPV             S             19    DIM(24)
005100970526     D*------------
005200970526     D* FUNZIONI DA CARICARE A VIDEO
005300970526     D*------------
005400970527     D FUV             S             19    DIM(24)
005500991108     D*------------
005600970526     D*------------------------------------------------------------------------*
005700970526     D* INPUT
005800970526     D*------------------------------------------------------------------------*
005900970526     D*------------
006000970526     D* RIDEFINIZIONE OPZIONI VIDEO
006100970526     D*------------
006200970526     D OPDDS           DS
006300970526     D  OPDOPZ                 1      2
006400970526     D  OPDDE1                 3     19
006500970526     D  OPDOP1                21     22
006600970526     D  OPDFI0                23     34
006700970526     D  OPDATT                35     35
006800970526     D  OPDFI1                36     80
006900970526     D  OPDDES                 1     19
007000970526     D  OPDRIG                 1     80
007100970526     D*------------
007200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007300970526     D*------------
007400970526     D FUDDS           DS
007500970526     D  FUDDE1                 1      1
007600970526     D  FUDFUN                 2      3
007700970526     D  FUDDE2                 4     19
007800970526     D  FUDFI0                20     34
007900970526     D  FUDATT                35     35
008000970526     D  FUDFI1                36     80
008100970526     D  FUDDES                 1     19
008200970526     D  FUDRIG                 1     80
008300970526     D*------------
008400970526     D* DS GESTIONE TASTI FUNZIONALI
008500970526     D*------------
008600970526     D DSKEY         E DS
008700970526     D DEVDS1          DS
008800970526     D  KEY                  369    369
008900970526     D*------------
009000970526     D* DS "XMSG" - GESTIONE MESSAGGI
009100970526     D*------------
009200970526     D DSMSG         E DS
009300970526     D*------------
009400991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
009500020709     D*------------
009600970527     D WLBDA8          DS                  INZ
009700970527     D  G08DAT                 1      8  0
009800970527     D  G08INV                 9     16  0
009900970527     D  G08ERR                17     17
010000970527     D  G08TGI                18     22  0
010100970526     D*------------
010200991110     D* DS DI PROCEDURA
010300970526     D*------------
010400970526     D* ARCHITETTURA
010500970526     D*------------
010600970526     D KPJBA         E DS
010700991108     D*------------------
010800991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
010900991110     D*------------------
011000991110     D                 DS                  INZ
011100991110     D  MS1                    1     40
011200991110     D  MS2                   41     80
011300991110     D  MESS                   1     80
011400991108     D*------------------
011500020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
011600000407     D*------------------
011700020412     D TISIT0DS      E DS                  INZ
011800060104      *
011900060104     d Status         sds           333
012000060104     d   SDSpgm          *proc
012100020412     D*------------------
012200020412     D* VARIABILI DI WRK
012300020412     D*------------------
012400020412     D wrkRICERCA      S             35    VARYING
012500020925     D*------------
012600020925     D* DS REPERIMENTO DATI UTENTE
012700020925     D*------------
012800020925     D TIBS34DS      E DS                                                       *Profili utente
012900020925     D DDATIUTE      E DS                                                       *Dati utente
013000020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
013100040603     D DUTE01        E DS                                                       *Abilitazioni
013200021217     D*------------
013300021217     D* DS PASSAGGIO PARAMETRI
013400021217     D*------------
013500021217     D PARAM           DS
013600021217     D  POPZ                   1      3
013700021217     D  PCOD                   4     10  0
013800970526     C*------------------------------------------------------------------------*
013900970526     C* MAIN LINES
014000970526     C*------------------------------------------------------------------------*
014100970526     C*
014200970526     C* SE NON CI SONO ERRORI, PROSEGUE
014300970526IF  1C     ERRL00        IFNE      '1'
014400970526     C*
014500970526     C* CARICA IL SFILE
014600970526     C                   EXSR      POSSFL
014700970526     C*
014800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015200970526     C*
015300970526     C* REIMPOSTA ERRORE DI PROCERURA
015400970526     C                   MOVEL     '0'           ERRL00
015500970526     C*
015600970526     C* EMETTE IL SFILE
015700970526     C                   EXSR      OUTSFL
015800970526     C*
015900970526     C* GESTIONE TASTI FUNZIONALI
016000970526     C                   EXSR      FUNCON
016100970526     C*
016200970526E   2C                   ENDDO
016300970527     C*
016400970526E   1C                   ENDIF
016500991108     C*
016600970526     C* FINE PGM
016700970526     C                   SETON                                        LR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F03GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F03"
017400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F05GES        BEGSR
018100970526     C*
018200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600040601     C* F06GES - GESTIONE F06 -> INSERIMENTO
018700970526     C*------------------------------------------------------------------------*
018800000407     C     F06GES        BEGSR
018900000407     C*
019000000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019100020412     C                   CLEAR                   TISIT0DS
019200020412     C                   MOVEL     'F06'         IT0OP1
019300020412     C                   CALL      'TISIT2R'
019400000407     C                   PARM                    KPJBA
019500020412     C                   PARM                    TISIT0DS
019600000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
019700000407E   1C                   ENDIF
019800000407     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100991108     C* F12GES - GESTIONE F12 -> RITORNO
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F12GES        BEGSR
020400970526     C*
020500970526     C* IMPOSTA "PREMUTO F12"
020600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020700970526     C*
020800970526     C                   ENDSR
020900970528     C*------------------------------------------------------------------------*
021000970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021100970528     C*------------------------------------------------------------------------*
021200970528     C     F21GES        BEGSR
021300970612     C*
021400991117     C* NON GESTITA
021500970528     C*
021600970528     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F23GES        BEGSR
022100970526     C*
022200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F24GES        BEGSR
022900970526     C*
023000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023500970526     C*------------------------------------------------------------------------*
023600970526     C     RUPGES        BEGSR
023700970526     C*
023800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200991108     C* ENTGES - GESTIONE ENTER -> INVIO
024300970526     C*------------------------------------------------------------------------*
024400970526     C     ENTGES        BEGSR
024500000407     C*
024600000407     C                   MOVEL     '0'           ERRL00
024700991108     C*
024800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024900970526     C                   EXSR      NEWPOS
025000970526     C*
025100991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025300970526IF  2C     SWPOSI        IFNE      0
025400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025500970526X   2C                   ELSE
025600970526IF  3C     SFLULT        IFNE      0
025700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025800970526     C*
025900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026000970526IF  4C     SWMODI        IFNE      0
026100970526     C                   EXSR      POSSFL
026200970526E   4C                   ENDIF
026300970526E   3C                   ENDIF
026400970526E   2C                   ENDIF
026500970526E   1C                   ENDIF
026600991108     C*
026700970526     C                   ENDSR
026800000420     C*------------------------------------------------------------------------*
026900000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027000000420     C*------------------------------------------------------------------------*
027100000420     C     O01GES        BEGSR
027200000420     C*
027300000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027400020412     C                   EXSR      PASPAR
027500021217     C                   EVAL      POPZ = DEPOP0
027600040603     C                   EVAL      PCOD = VIDCOD
027700021217     C                   MOVEL     PARAM         KPJBU
027800000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
027900000420     C*
028000000420     C                   ENDSR
028100000407     C*------------------------------------------------------------------------*
028200000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
028300000407     C*------------------------------------------------------------------------*
028400000407     C     O02GES        BEGSR
028500000407     C*
028600000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028700000411     C     VIDATB        IFEQ      'A'
028800000411     C                   MOVEL     MSG(4)        DSMSMS
028900000411     C                   ELSE
029000020412     C                   CLEAR                   TISIT0DS
029100020412     C                   MOVEL     'O02'         IT0OP1
029200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
029300020412     C                   CALL      'TISIT2R'
029400000407     C                   PARM                    KPJBA
029500020412     C                   PARM                    TISIT0DS
029600040308     C***                EXSR      POSSFL                                       *RICARICA SFILE
029700000407E   1C                   ENDIF
029800000411     C                   ENDIF
029900000407     C*
030000000407     C                   ENDSR
030100020412     C*------------------------------------------------------------------------*
030200020412     C* O03GES - GESTIONE OPZIONE 3=COPIA RECORD
030300020412     C*------------------------------------------------------------------------*
030400020412     C     O03GES        BEGSR
030500020412     C*
030600020412IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030700020412     C     VIDATB        IFEQ      'A'
030800020412     C                   MOVEL     MSG(4)        DSMSMS
030900020412     C                   ELSE
031000020412     C                   CLEAR                   TISIT0DS
031100020412     C                   MOVEL     'O03'         IT0OP1
031200020412     C                   EXSR      PASPAR                                       *RICARICA SFILE
031300020412     C                   CALL      'TISIT2R'
031400020412     C                   PARM                    KPJBA
031500020412     C                   PARM                    TISIT0DS
031600020412     C*                  EXSR      POSSFL                                       *RICARICA SFILE
031700020412E   1C                   ENDIF
031800020412     C                   ENDIF
031900020412     C*
032000020412     C                   ENDSR
032100991112     C*------------------------------------------------------------------------*
032200000407     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
032300991112     C*------------------------------------------------------------------------*
032400991112     C     O04GES        BEGSR
032500991112     C*
032600000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700000411     C     VIDATB        IFEQ      'A'
032800000411     C                   MOVEL     MSG(4)        DSMSMS
032900000411     C                   ELSE
033000020412     C                   CLEAR                   TISIT0DS
033100020412     C                   MOVEL     'O04'         IT0OP1
033200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
033300020412     C                   CALL      'TISIT2R'
033400000407     C                   PARM                    KPJBA
033500020412     C                   PARM                    TISIT0DS
033600000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
033700000407E   1C                   ENDIF
033800000411     C                   ENDIF
033900991112     C*
034000991112     C                   ENDSR
034100991014     C*------------------------------------------------------------------------*
034200000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
034300970526     C*------------------------------------------------------------------------*
034400970526     C     O05GES        BEGSR
034500000407     C*
034600000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034700020412     C                   CLEAR                   TISIT0DS
034800020412     C                   MOVEL     'O05'         IT0OP1
034900000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
035000020412     C                   CALL      'TISIT2R'
035100000407     C                   PARM                    KPJBA
035200020412     C                   PARM                    TISIT0DS
035300000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
035400000407E   1C                   ENDIF
035500000407     C*
035600970526     C                   ENDSR
035700000411     C*------------------------------------------------------------------------*
035800000411     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
035900000411     C*------------------------------------------------------------------------*
036000000411     C     O07GES        BEGSR
036100000411     C*
036200000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036300000411     C     VIDATB        IFEQ      *BLANKS
036400000411     C                   MOVEL     MSG(5)        DSMSMS
036500000411     C                   ELSE
036600020412     C                   CLEAR                   TISIT0DS
036700020412     C                   MOVEL     'O07'         IT0OP1
036800000411     C                   EXSR      PASPAR                                       *RICARICA SFILE
036900020412     C                   CALL      'TISIT2R'
037000000411     C                   PARM                    KPJBA
037100020412     C                   PARM                    TISIT0DS
037200000411     C*                  EXSR      POSSFL                                       *RICARICA SFILE
037300000411E   1C                   ENDIF
037400000411     C                   ENDIF
037500000411     C*
037600000411     C                   ENDSR
037700040601     C*------------------------------------------------------------------------*
037800040601     C* O09GES - GESTIONE OPZIONE 9=COPIA SINONIMI SU
037900040601     C*------------------------------------------------------------------------*
038000040601     C     O09GES        BEGSR
038100040601     C*
038200040601IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038300040601     C     VIDESS        IFNE      'S'
038400040601     C                   MOVEL     MSG(7)        DSMSMS
038500040601     C                   ELSE
038600040601     C                   CALL      'TISIT11R'
038700040601     C                   PARM                    KPJBA
038800040603     C                   PARM      VIDCOD        W_COD             7 0
038900040601     C                   PARM      '0'           W_ERR             1
039000040601     C                   IF        W_ERR = '1'
039100040601     C                   MOVEL     MSG(9)        DSMSMS
039200040601     C                   ELSE
039300040601     C                   MOVEL     MSG(10)       DSMSMS
039400040601     C                   ENDIF
039500040601E   1C                   ENDIF
039600040601     C                   ENDIF
039700040601     C*
039800040601     C                   ENDSR
039900020415     C*------------------------------------------------------------------------*
040000020415     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
040100020415     C*------------------------------------------------------------------------*
040200020415     C     O13GES        BEGSR
040300020423     C*
040400020423     C* POSSIBILE SOLO SE ESISTONO SINONIMI
040500050418     C*    VIDESS        IFNE      'S'
040600050418     C*                  MOVEL     '1'           ERRL00
040700050418     C*                  MOVEL     MSG(7)        DSMSMS
040800050418     C*                  ENDIF
040900020423     C
041000020423     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041100020415     C                   CLEAR                   TISIT0DS
041200020415     C                   MOVEL     'O13'         IT0OP1
041300020415     C                   EXSR      PASPAR                                       *RICARICA SFILE
041400020415     C                   CALL      'TISIT3R'
041500020415     C                   PARM                    KPJBA
041600040603     C                   PARM                    TISIT0DS
041700020415     C*                  EXSR      POSSFL                                       *RICARICA SFILE
041800020415E   1C                   ENDIF
041900020415     C*
042000020415     C                   ENDSR
042100991028     C*------------------------------------------------------------------------*
042200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
042300991112     C*------------------------------------------------------------------------*
042400991112     C     PASPAR        BEGSR
042500991112     C*
042600040603     C                   EVAL      IT0COD = VIDCOD
042700020412     C                   EVAL      IT0NAZ = VIHNAZ
042800020412     C                   EVAL      IT0PRV = VIDPRV
042900040603     C                   EVAL      IT0CAP = VIHCAP
043000020412     C                   EVAL      IT0LOC = VIHLOC
043100020416     C                   EVAL      IT0KWI = VIHKWI
043200020412     C                   EVAL      IT0RAG = VIHRAG
043300991112     C*
043400020412     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
043700970526     C*------------------------------------------------------------------------*
043800970526     C     NEWPOS        BEGSR
043900970526     C*
044000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
044100970526     C*         1= RICHIESTO POSIZIONAMENTO
044200970526     C                   Z-ADD     0             SWPOSI            7 0
044300970526     C*
044400991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
044500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
044600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
044700970526     C                   Z-ADD     0             SWMODI            1 0
044800970530     C*
044900970530     C* REIMPOSTA INDICATORI DI ERRORE
045000991108     C                   SETOFF                                       808182
045100991108     C                   SETOFF                                       83
045200970526     C*
045300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
045400020412     C     VIDRS1        IFNE      deprs1
045500000407     C     VIDCA1        ORNE      depca1
045600020412     C     VIDRS2        ORNE      deprs2
045700020412     C     VIDCA2        ORNE      depca2
045800020412     C     VIDKW2        ORNE      depkw2
045900020621     C     VIDPR2        ORNE      deppr2
046000020621     C     VIDDS2        ORNE      depds2
046100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
046200020412     C     ERRL00        IFEQ      '0'                                          *NO ERORI
046300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
046400020412     C                   MOVEL     VIDRS1        deprs1                         *DEPOSITI
046500000407     C                   MOVEL     VIDCA1        depca1                         *DEPOSITI
046600020412     C                   MOVEL     VIDRS2        deprs2                         *DEPOSITI
046700020412     C                   MOVEL     VIDCA2        depca2                         *DEPOSITI
046800020412     C                   MOVEL     VIDKW2        depkw2                         *DEPOSITI
046900020621     C                   MOVEL     VIDPR2        deppr2                         *DEPOSITI
047000020621     C                   MOVEL     VIDDS2        depds2                         *DEPOSITI
047100020412     C                   ENDIF
047200020412     C                   ENDIF
047300970526     C*
047400970526     C                   ENDSR
047500970526     C*------------------------------------------------------------------------*
047600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
047700970526     C*------------------------------------------------------------------------*
047800970528     C     SETFIL        BEGSR
047900970526     C*
048000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
048100020412     C     VIDRS1        IFEQ      *zeros
048200020412     C                   MOVEL     *loval        KDSRAG
048300991108     C                   ELSE
048400020412     C                   MOVEL     VIDRS1        KDSRAG
048500991108     C                   ENDIF
048600000407     C     VIDCA1        IFEQ      *zeros
048700000407     C     VIDCA1        OREQ      *blanks
048800020412     C                   MOVEL     *loval        KDSCAP
048900991108     C                   ELSE
049000020412     C                   MOVEL     VIDCA1        KDSCAP
049100991108     C                   ENDIF
049200020412     C*
049300020412     C     WTIPRI        IFEQ      'CAP'
049400020412     C     KEYDST1       SETLL     azdst01l                           99
049500020412     C                   ENDIF
049600020412     C     WTIPRI        IFEQ      'RAG'
049700020412     C     KEYDST2       SETLL     azdst02l                           99
049800020412     C                   ENDIF
049900980107     C*
050000970526     C* LETTURA RECORD
050100020412     C     *IN99         IFEQ      *OFF
050200970526     C                   EXSR      LETFIL
050300020412     C                   ENDIF
050400970526     C*
050500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050600020412     C     *IN99         IFEQ      *ON
050700970526     C                   Z-ADD     1             SFLEOF
050800020412     C                   ENDIF
050900970526     C*
051000970526     C                   ENDSR
051100970526     C*------------------------------------------------------------------------*
051200970526     C* LETFIL - LETTURA RECORD
051300970526     C*------------------------------------------------------------------------*
051400970526     C     LETFIL        BEGSR
051500970526     C*
051600970526     C* LEGGE FINO A:
051700970526     C*     - FINE FILE
051800970526     C*     - TROVATO RECORD VALIDO
051900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
052000020412     C     *IN99         DOWEQ     *OFF
052100970526     C     WRECOK        ANDEQ     'N'
052200970526     C*
052300970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
052400020412     C     WTIPRI        IFEQ      'CAP'
052500020412     C                   READ      azdst01l                               99
052600020412     C                   ENDIF
052700020412     C     WTIPRI        IFEQ      'RAG'
052800020412     C                   READ      azdst02l                               99
052900020412     C                   ENDIF
053000020412     C*
053100020412     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053200020412     C     *IN99         IFEQ      *ON                                          *FINE FILE
053300020412     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
053400020412     C                   ELSE
053500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
053600020412     C                   ENDIF
053700020412     C                   ENDDO
053800020412     C*
053900020412     C                   ENDSR
054000020412     C*------------------------------------------------------------------------*
054100020412     C* CHKREC - CONTROLLA VALIDITA' RECORD
054200020412     C*------------------------------------------------------------------------*
054300020412     C     CHKREC        BEGSR
054400020412     C*
054500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054600020412     C*
054700020412     C* FILTRA SE RICHIESTE SELEZIONI
054800020412     C     VIDRS2        IFEQ      *blanks
054900020412     C                   ELSE
055000020412     C                   EVAL      %len(wrkRICERCA) = %len(%trim(VIDRS2))
055100020412     C                   EVAL      wrkRICERCA = %trim(VIDRS2)
055200020412     C     wrkRICERCA    SCAN      DSTRAG
055300020412     C                   IF        not %found
055400991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055500020412     C                   ENDIF
055600020412     C                   ENDIF
055700020412     C*
055800020412     C     VIDCA2        IFEQ      *blanks
055900020412     C                   ELSE
056000020412     C     VIDCA2        IFNE      DSTCAP
056100020412     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056200020412     C                   ENDIF
056300020412     C                   ENDIF
056400020412     C*
056500020412     C     VIDKW2        IFEQ      *blanks
056600020412     C                   ELSE
056700020412     C                   EVAL      %len(wrkRICERCA) = %len(%trim(VIDKW2))
056800020412     C                   EVAL      wrkRICERCA = %trim(VIDKW2)
056900020415     C     wrkRICERCA    SCAN      DSTKWI
057000020412     C                   IF        not %found
057100020412     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057200020412     C                   ENDIF
057300020412     C                   ENDIF
057400020621     C*
057500020621     C     VIDPR2        IFEQ      *blanks
057600020621     C                   ELSE
057700020621     C     VIDPR2        IFNE      DSTPRV
057800020621     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057900020621     C                   ENDIF
058000020621     C                   ENDIF
058100020621     C*
058200020621     C     VIDDS2        IFEQ      *blanks
058300020621     C                   ELSE
058400020621     C     VIDDS2        IFNE      DSTDOS
058500020621     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058600020621     C                   ENDIF
058700020621     C                   ENDIF
058800991108     C*
058900970526     C                   ENDSR
059000970526     C*------------------------------------------------------------------------*
059100000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
059200970526     C*------------------------------------------------------------------------*
059300970526     C     IRESFL        BEGSR
059400970526     C*
059500000407     C                   MOVEL     *blanks       OPZKEY
059600020412     C                   MOVEL     DSTATB        VIDATB
059700020412     C                   MOVEL     DSTRAG        VIDRAG
059800020412     C                   MOVEL     DSTRAG        VIHRAG
059900020412     C                   MOVEL     DSTNAZ        VIHNAZ
060000020412     C                   MOVEL     DSTPRV        VIDPRV
060100020423     C                   MOVEL     DSTDOS        VIDDOS
060200040603     C                   MOVEL     DSTCAP        VIHCAP
060300020412     C                   MOVEL     DSTLOC        VIDLOC
060400020412     C                   MOVEL     DSTLOC        VIHLOC
060500020412     C                   MOVEL     DSTKWI        VIDKWI
060600020412     C                   MOVEL     DSTKWI        VIHKWI
060700040603     C                   MOVEL     DSTCOD        VIDCOD
060800991108     C*
060900020415     C* VERIFICO SE X IL DESTINATARIO CORRENTE ESISTONO SINONIMI
061000020415     C                   EVAL      KDSNAZ = VIHNAZ
061100020415     C                   EVAL      KDSPRV = VIDPRV
061200040603     C                   EVAL      KDSCAP = VIHCAP
061300020415     C                   EVAL      KDSLOC = VIHLOC
061400020415     C                   EVAL      KDSRAG = VIHRAG
061500020415     C                   EVAL      KDSKWI = VIHKWI
061600020415     C*
061700020415     C     KEYDSS3       CHAIN     AZDSS03L
061800020415     C                   IF        %found(AZDSS03L)
061900020415     C                   EVAL      VIDESS = 'S'
062000020415     C                   ELSE
062100020415     C                   EVAL      VIDESS = *blanks
062200020415     C                   ENDIF
062300020415     C*
062400970526     C                   ENDSR
062500970526     C*------------------------------------------------------------------------*
062600991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
062700970526     C*------------------------------------------------------------------------*
062800970526     C     CHKPAR        BEGSR
062900970528     C*
063000020412     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO X RAGIONE SOCIALE
063100020412     C                   MOVEL     'RAG'         WTIPRI            3
063200000407     C*
063300020412     C     VIDRS1        IFEQ      *blanks
063400000407     C     VIDCA1        ANDEQ     *blanks
063500020412     C                   MOVEL     'RAG'         WTIPRI
063600020412     C                   ENDIF                                                  *ORDINAMENTO DEFAULT
063700970528     C*
063800970528     C* CONTROLLA IL TIPO RICERCA
063900000407     C     ERRL00        IFEQ      '0'
064000020412     C     VIDRS1        IFNE      *blanks
064100020412     C                   MOVEL     'RAG'         WTIPRI
064200000407     C                   ENDIF
064300000407     C     VIDCA1        IFNE      *blanks
064400000407     C                   MOVEL     'CAP'         WTIPRI
064500000407     C                   ENDIF
064600000407     C                   ENDIF
064700000407     C*
064800970526     C                   ENDSR
064900970526     C*------------------------------------------------------------------------*
065000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
065100970526     C*------------------------------------------------------------------------*
065200970526     C     CHKFOP        BEGSR
065300991112     C*
065400991112     C* OPZIONI
065500970626     C*
065600000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
065700000420IF  1C     DEPOP0        IFEQ      'P01'
065800000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
065900000420     C                   MOVEL     'S'           OPDATT
066000000420     C                   MOVEL     OPDRIG        OPD(1)
066100000420X   1C                   ELSE
066200000420     C* OPZIONI
066300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
066400970626     C                   MOVEL     'S'           OPDATT
066500970626     C                   MOVEL     OPDRIG        OPD(5)
066600040622     C                   MOVEL     OPD(13)       OPDRIG                         *CREA SINONIMO
066700040622     C                   MOVEL     'S'           OPDATT
066800040622     C                   MOVEL     OPDRIG        OPD(13)
066900970626E   1C                   ENDIF
067000970626     C*
067100000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
067200970626IF  1C     DEPOP0        IFEQ      'P02'
067300000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
067400970626     C                   MOVEL     'S'           OPDATT
067500000407     C                   MOVEL     OPDRIG        OPD(2)
067600020412     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
067700020412     C                   MOVEL     'S'           OPDATT
067800020412     C                   MOVEL     OPDRIG        OPD(3)
067900000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
068000000411     C                   MOVEL     'S'           OPDATT
068100000411     C                   MOVEL     OPDRIG        OPD(4)
068200991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
068300991112     C                   MOVEL     'S'           OPDATT
068400991112     C                   MOVEL     OPDRIG        OPD(5)
068500040601     C                   MOVEL     OPD(9)        OPDRIG                         *COPIA SIN. SU
068600040601     C                   MOVEL     'S'           OPDATT
068700040601     C                   MOVEL     OPDRIG        OPD(9)
068800040601     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
068900000407     C                   MOVEL     'S'           FUDATT
069000000407     C                   MOVEL     FUDRIG        FUD(6)
069100970626E   1C                   ENDIF
069200040603     C*
069300040603     C* SE L'UTENTE E' IN GESTIONE SOLO X TELEFONO E REFERENTE
069400040603IF  1C     DEPOP0        IFEQ      'P03'
069500040603     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
069600040603     C                   MOVEL     'S'           OPDATT
069700040603     C                   MOVEL     OPDRIG        OPD(2)
069800040603E   1C                   ENDIF
069900970526     C*
070000970526     C* GESTIONE OPZIONI A VIDEO
070100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
070200970526     C                   EXSR      OPZGES
070300970526     C*
070400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
070500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
070600970526     C                   EXSR      FUNGES
070700970526     C*
070800970526     C                   ENDSR
070900020925     C*--------------------------------------------------------------------------------------------*
071000020925     C* REPERISCE DATI UTENTE
071100020925     C*--------------------------------------------------------------------------------------------*
071200020925     C     REPDATIUTE    BEGSR
071300020925     C*
071400020925     C* REimposta le variabili di lavoro
071500020925     C                   CLEAR                   TIBS34DS
071600020925     C                   CLEAR                   AZUTEDS
071700020925     C                   CLEAR                   DDATIUTE
071800020925     C*
071900020925     C* Reperisce i dati del profilo in ingresso
072000020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
072100020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
072200020925     C                   IN(E)     *DTAARA
072300020925     C                   IF        %Error                                       *NO errori
072400020925     C                   EVAL      I34TLA = 'L'
072500020925     C                   CALL      'TIBS34R'
072600020925     C                   PARM                    TIBS34DS
072700020925     C                   IN        *DTAARA
072800020925     C                   ENDIF
072900040603     C*
073000040603     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
073100040603     C                   EVAL      DUTE01 = UTEFAF
073200020925     C*
073300020925     C                   ENDSR
073400970526     C*------------------------------------------------------------------------*
073500970526     C* *INZSR - OPERAZIONI INIZIALI
073600970526     C*------------------------------------------------------------------------*
073700970526     C     *INZSR        BEGSR
073800970526     C*---
073900970526     C* RICEVIMENTO PARAMETRI
074000970526     C*---
074100970526     C     *ENTRY        PLIST
074200970612     C                   PARM                    KPJBA
074300020412     C                   MOVEL     KPJBU         DEPOP0            3
074400020925     C*
074500020925     C* REPERISCE I DATI DELL'UTENTE
074600020925     C                   EXSR      REPDATIUTE
074700040603     C*
074800040603     C* IN BASE ALLE AUTIZZAZIONI UTENTE PILOTO IL LIVELLO DI GESTIONE (QUINDI NN + DA KAZN)
074900040603     C* ...SE ENTRATA NN IN "SCELTA" VALORIZZO
075000040603     C                   IF        DEPOP0  <> 'P01'
075100040603     C* ...DI DEFAULT IMPOSTO SOLO VISUALIZZAZIONE
075200040603     C                   EVAL      DEPOP0  = 'P05'
075300040603     C* ...SE PREVISTO IMPOSTO GESTIONE SOLO TELEFONO E REFERENTE
075400040603     C                   IF        §UTEDST = '3'
075500040603     C                   EVAL      DEPOP0  = 'P03'
075600040603     C                   ENDIF
075700040603     C* ...SE PREVISTO IMPOSTO GESTIONE COMPLETA
075800090707     C                   IF        §UTEDST >= '7'
075900040603     C                   EVAL      DEPOP0  = 'P02'
076000040603     C                   ENDIF
076100040603     C                   ENDIF
076200040603     C*
076300040603     C* BY-PASSO L'ENTRATA IN KPJBU ORIGINALE CON QUELLA APPENA STABILITA
076400040603     C                   MOVEL     DEPOP0        KPJBU
076500970526     C*
076600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
076700970526     C     XMSGPA        PLIST
076800970526     C                   PARM                    DSMSMS
076900970526     C                   PARM                    DSMSPG
077000970526     C                   PARM                    DSMSKE
077100970526     C                   PARM                    DSMSER
077200970526     C*
077300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
077400970526     C                   CALL      'XKEY'
077500970526     C                   PARM                    DSKEY
077600970526     C*---
077700970526     C* VARIABILI NON RIFERITE AL DATA BASE
077800970526     C*---
077900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
078000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
078100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
078200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
078300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
078400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
078500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
078600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
078700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
078800970526     C*---
078900020412     C     *LIKE         DEFINE    vidrs1        deprs1
079000020412     C     *LIKE         DEFINE    vidrs2        deprs2
079100000407     C     *LIKE         DEFINE    vidca1        depca1
079200020412     C     *LIKE         DEFINE    vidca2        depca2
079300020412     C     *LIKE         DEFINE    vidkw2        depkw2
079400020621     C     *LIKE         DEFINE    vidpr2        deppr2
079500020621     C     *LIKE         DEFINE    vidds2        depds2
079600020415     C     *LIKE         DEFINE    dsttip        kdstip
079700020412     C     *LIKE         DEFINE    dstrag        kdsrag
079800020412     C     *LIKE         DEFINE    dstcap        kdscap
079900020415     C     *LIKE         DEFINE    dstnaz        kdsnaz
080000020415     C     *LIKE         DEFINE    dstprv        kdsprv
080100020415     C     *LIKE         DEFINE    dstloc        kdsloc
080200020415     C     *LIKE         DEFINE    dstkwi        kdskwi
080300991108     C*---
080400970526     C* VARIABILI DI CONTROLLO
080500970526     C*---
080600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
080700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
080800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
080900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
081000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
081100000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
081200020415     C                   EVAL      KDSTIP = 'C'                                 *SOLO TIPI "C"
081300970526     C*
081400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
081500020412     C                   MOVEL     'TISIT1R'     DSMSPG
081600020412     C                   MOVEL     'TISIT1R'     MSGQUE
081700970526     C                   MOVEL     *BLANKS       MSGKEY
081800970526     C                   MOVEL     *BLANKS       DSMSMS
081900970526     C                   MOVEL     *BLANKS       DSMSKE
082000970526     C                   Z-ADD     *ZEROS        DSMSER
082100970526     C*---
082200970526     C* CHIAVI DI LETTURA
082300970526     C*---
082400020412     C* LETTURA azdst01l -
082500020412     C     KEYDST1       KLIST
082600020415     C                   KFLD                    KDSTIP
082700020412     C                   KFLD                    KDSCAP
082800020412     C                   KFLD                    KDSRAG
082900020412     C* LETTURA azdst02l -
083000020412     C     KEYDST2       KLIST
083100020415     C                   KFLD                    KDSTIP
083200020412     C                   KFLD                    KDSRAG
083300020412     C                   KFLD                    KDSCAP
083400020415     C* LETTURA azdss03l
083500020415     C     KEYDSS3       KLIST
083600020415     C                   KFLD                    KDSNAZ
083700020415     C                   KFLD                    KDSPRV
083800020415     C                   KFLD                    KDSCAP
083900020415     C                   KFLD                    KDSLOC
084000020415     C                   KFLD                    KDSRAG
084100020415     C                   KFLD                    KDSKWI
084200060104      *
084300060104      * Visualizza il nome del pgm
084400060104     c                   movel     SDSpgm        V1Tpgm
084500970526     C*
084600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
084700970526     C                   EXSR      CHKPAR
084800991115     C*
084900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
085000970526     C                   EXSR      CHKFOP
085100970526     C*
085200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
085300020412     C     DEPOP0        IFEQ      'P01'
085400040603     C                   MOVEL     DEC(1)        VIDING
085500020412     C                   ENDIF
085600020412     C     DEPOP0        IFEQ      'P02'
085700040603     C                   MOVEL     DEC(2)        VIDING
085800020412     C                   ENDIF
085900040603     C     DEPOP0        IFEQ      'P03'
086000040603     C                   MOVEL     DEC(3)        VIDING
086100040603     C                   ENDIF
086200020412     C     DEPOP0        IFEQ      'P05'
086300040603     C                   MOVEL     DEC(5)        VIDING
086400020412     C                   ENDIF
086500020412     C*
086600020412     C                   ENDSR
086700970526     C*------------------------------------------------------------------------*
086800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
086900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
087100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
087200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
087300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087500970526     C*------------------------------------------------------------------------*
087600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
087700970526     C*------------------------------------------------------------------------*
087800970526     C     POSSFL        BEGSR
087900970526     C*
088000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
088100970526     C                   EXSR      CLRSFL
088200970526     C*
088300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
088400970526     C                   EXSR      SETFIL
088500970526     C*
088600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
088700970526     C     SFLEOF        IFEQ      0
088800970526     C                   EXSR      CARSFL
088900970526     C                   END
089000970526     C*
089100970526     C                   ENDSR
089200970526     C*------------------------------------------------------------------------*
089300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
089400970526     C*------------------------------------------------------------------------*
089500970526     C     ENTATT        BEGSR
089600970526     C*
089700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
089800970526     C                   Z-ADD     0             OPZOK             1 0
089900970526     C*
090000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
090100970526     C                   Z-ADD     0             SFLVAR            5 0
090200970526     C*
090300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
090400970526     C                   Z-ADD     0             SFLERR
090500970526     C*
090600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
090700970526     C                   Z-ADD     SFLULT        WFLULT
090800970526     C*
090900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
091000020412     C                   READC     SIT1D05                                98
091100970526     C     *IN98         DOWEQ     '0'
091200970526     C*
091300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
091400970526     C                   ADD       1             SFLVAR
091500970526     C*
091600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
091700970526     C     OPZOK         IFEQ      0
091800970526     C*
091900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
092000970526     C     OPZKEY        IFNE      *BLANKS
092100970526     C                   EXSR      OPZCON
092200970526     C                   END
092300970526     C*
092400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
092500970526     C*            2. PULIZIA SCELTA IN SFL
092600970526     C     OPZOK         IFEQ      0
092700970526     C                   MOVEL     '0'           *IN41
092800970526     C                   MOVEL     *BLANKS       OPZKEY
092900970526     C                   ELSE
093000970526     C*
093100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
093200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
093300970526     C                   MOVEL     '1'           *IN41
093400970526     C                   Z-ADD     SFLULT        SFLERR
093500970526     C                   END
093600970526     C                   ELSE
093700970526     C*
093800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
093900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
094000970526     C                   MOVEL     '1'           *IN41
094100970526     C                   END                                                    OPZOK=0
094200970526     C*
094300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
094400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
094500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
094600970526     C     OPZOK         IFNE      0
094700970526     C                   Z-ADD     SFLERR        SFLNBR
094800970526     C                   ELSE
094900970526     C                   Z-ADD     SFLULT        SFLNBR
095000970526     C                   END
095100970526     C*
095200970526     C* AGGIORNO RECORD SFL
095300020412     C                   UPDATE    SIT1D05
095400970526     C*
095500020412     C                   READC     SIT1D05                                98
095600970526     C                   END                                                    DO WHILE
095700970526     C*
095800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
095900970526     C                   Z-ADD     WFLULT        SFLULT
096000970526     C*
096100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
096200970526     C     SFLVAR        IFEQ      0
096300970526     C                   END
096400970526     C*
096500040308     C***  OPZKEY        IFNE      ' 5'
096600040308     C***                EXSR      F05GES
096700040308     C***                ENDIF
096800991112     C*
096900970526     C                   ENDSR
097000970526     C*------------------------------------------------------------------------*
097100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
097200970526     C*------------------------------------------------------------------------*
097300970526     C     CLRSFL        BEGSR
097400970526     C*
097500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
097600970526     C* (0=NON FINE FILE  1=FINE FILE)
097700970526     C                   Z-ADD     0             SFLEOF
097800970526     C*
097900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
098000970526     C                   Z-ADD     0             SFLULT
098100970526     C*
098200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
098300970526     C                   MOVEL     '1'           *IN40
098400020412     C                   WRITE     SIT1D06
098500970526     C                   MOVEL     '0'           *IN40
098600970526     C*
098700970526     C                   ENDSR
098800970526     C*------------------------------------------------------------------------*
098900970526     C* CARSFL - CARICAMENTO SFL
099000970526     C*------------------------------------------------------------------------*
099100970526     C     CARSFL        BEGSR
099200970526     C*
099300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
099400970526     C     SFLULT        IFEQ      0
099500970526     C                   EXSR      SETFIL
099600970526     C                   END
099700970526     C*
099800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
099900970526     C                   Z-ADD     SFLULT        WFLULT
100000970526     C*
100100970526     C* CICLO:
100200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
100400970526     C                   Z-ADD     1             SFLRIG
100500970526     C     SFLRIG        DOWLE     SFLPAG
100600970526     C     SFLEOF        ANDEQ     0
100700970526     C*
100800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
100900991116     C                   MOVEL     *BLANKS       OPZKEY
101000970526     C*
101100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
101200970526     C                   EXSR      IRESFL
101300970526     C*
101400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
101500970526     C                   ADD       1             SFLULT
101600970526     C*
101700970526     C* SCRITTURA RECORD SFL
101800020412     C                   WRITE     SIT1D05
101900970526     C                   ADD       1             SFLRIG
102000970526     C*
102100970526     C* LETTURA FILE DA CARICARE
102200970526     C                   EXSR      LETFIL
102300970526     C                   END                                                    DO WHILE
102400970526     C*
102500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
102600970526     C     WFLULT        IFNE      SFLULT
102700970526     C                   Z-ADD     WFLULT        SFLNBR
102800970526     C                   ADD       1             SFLNBR
102900970526     C                   END
103000970526     C*
103100970526     C                   ENDSR
103200970526     C*------------------------------------------------------------------------*
103300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
103400970526     C*------------------------------------------------------------------------*
103500970526     C     OUTSFL        BEGSR
103600970526     C*
103700970526     C* WRITE RECORD TESTATA VIDEO
103800020412     C                   WRITE     SIT1D01
103900970526     C*
104000970526     C* VISUALIZZAZIONE MESSAGGIO
104100970526     C                   EXSR      SNDMSG
104200970526     C*
104300970526     C* WRITE RECORD OPZIONI VIDEO
104400020412     C                   WRITE     SIT1D02
104500970526     C*
104600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
104700020412     C                   WRITE     SIT1D03
104800970526     C*
104900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
105000020412     C                   WRITE     SIT1D04
105100970526     C*
105200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
105300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
105400970526     C     SFLEOF        IFEQ      0
105500970526     C                   MOVEL     '0'           *IN42
105600970526     C                   ELSE
105700970526     C*
105800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
105900970526     C                   MOVEL     '1'           *IN42
106000970526     C                   END
106100970526     C*
106200970526     C* WRITE RECORD TASTI FUNZIONALI
106300020412     C                   WRITE     SIT1D09
106400970526     C*
106500970526     C* CASO DI SFL VUOTO
106600970526     C     SFLULT        IFLE      0
106700970526     C*
106800970526     C*    - WRITE RECORD VUOTO
106900020412     C                   WRITE     SIT1D0A
107000020412     C                   READ      SIT1D0A                                99
107100970526     C*
107200970526     C                   ELSE
107300970526     C*
107400970526     C* CASO DI SFL CON RECORDS
107500970526     C*
107600970526     C*    - WRITE RECORD CONTROLLO SFL
107700020412     C                   WRITE     SIT1D06
107800020412     C                   READ      SIT1D06                                99
107900970526     C                   END
108000970526     C*
108100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
108200020412     C                   READ      SIT1D03                                99
108300970526     C*
108400970526     C                   ENDSR
108500970526     C*------------------------------------------------------------------------*
108600970526     C* SNDMSG - MANDA MESSAGGIO
108700970526     C*------------------------------------------------------------------------*
108800970526     C     SNDMSG        BEGSR
108900970526     C*
109000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
109100970526     C                   MOVEL     '1'           *IN40
109200020412     C                   WRITE     SIT1DMC
109300970526     C                   MOVEL     '0'           *IN40
109400970526     C*
109500970526     C* VISUALIZZAZIONE MESSAGGIO
109600970526     C                   Z-ADD     0             MSGULT
109700970526     C                   CALL      'XMSG'        XMSGPA
109800970526     C     DSMSER        IFEQ      0
109900970526     C                   MOVEL     DSMSKE        MSGKEY
110000970526     C                   END
110100970526     C                   Z-ADD     1             MSGULT
110200020412     C                   WRITE     SIT1DMS                                      SFL MESS
110300020412     C                   WRITE     SIT1DMC                                      CONTR MESS
110400970526     C*
110500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
110600970526     C                   MOVEL     *BLANKS       DSMSMS
110700970526     C*
110800970526     C                   ENDSR
110900970526     C*------------------------------------------------------------------------*
111000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
111100970526     C*------------------------------------------------------------------------*
111200970526     C     FUNCON        BEGSR
111300970526     C*
111400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
111500970526     C                   EXSR      FUNVAR
111600970526     C*
111700970526     C* PULISCO CAMPI DI LAVORO
111800970526     C                   Z-ADD     0             I                 5 0
111900970526     C                   Z-ADD     0             W                 5 0
112000970526     C*
112100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
112200970526     C     FUNKEY        IFNE      *BLANKS
112300970526     C     1             DO        24            I
112400970526     C                   MOVEL     FUD(I)        FUDRIG
112500970526     C*
112600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
112700970526     C     FUNKEY        IFEQ      FUDFUN
112800970526     C     FUDATT        ANDEQ     'S'
112900970526     C                   Z-ADD     I             W
113000970526     C                   END
113100970526     C                   END                                                    DO WHILE
113200970526     C*
113300970526     C* FUNZIONE NON ATTIVA (ERRORE)
113400970526     C     W             IFEQ      0
113500970526     C*
113600970526     C* EMISSIONE MESSAGGIO DI ERRORE
113700970526     C                   MOVEL     MSG(2)        DSMSMS
113800970526     C                   END                                                    W=0
113900970526     C                   END                                                    FUNKEY NE *BLANKS
114000970526     C*
114100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
114200970526     C     W             IFNE      0
114300970526     C     FUNKEY        OREQ      *BLANKS
114400970526     C*
114500970526     C     FUNKEY        IFEQ      *BLANKS
114600970526     C                   EXSR      ENTGES
114700970526     C                   END
114800970526     C     FUNKEY        IFEQ      '03'
114900970526     C                   EXSR      F03GES
115000970526     C                   END
115100970526     C     FUNKEY        IFEQ      '05'
115200970526     C                   EXSR      F05GES
115300970526     C                   END
115400970526     C     FUNKEY        IFEQ      '06'
115500970526     C                   EXSR      F06GES
115600970526     C                   END
115700970526     C     FUNKEY        IFEQ      '12'
115800970526     C                   EXSR      F12GES
115900970526     C                   END
116000970528     C     FUNKEY        IFEQ      '21'
116100970528     C                   EXSR      F21GES
116200970528     C                   END
116300970526     C     FUNKEY        IFEQ      '23'
116400970526     C                   EXSR      F23GES
116500970526     C                   END
116600970526     C     FUNKEY        IFEQ      '24'
116700970526     C                   EXSR      F24GES
116800970526     C                   END
116900970526     C     FUNKEY        IFEQ      'RU'
117000970526     C                   EXSR      RUPGES
117100970526     C                   END
117200970526     C*
117300970526     C                   END
117400970526     C*
117500970526     C                   ENDSR
117600970526     C*------------------------------------------------------------------------*
117700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117800970526     C*------------------------------------------------------------------------*
117900970526     C     FUNVAR        BEGSR
118000970526     C*
118100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
118200970526     C     KEY           IFEQ      ENTER
118300970526     C                   MOVEL     *BLANKS       FUNKEY
118400970526     C                   END
118500970526     C     KEY           IFEQ      F1
118600970526     C                   MOVEL     '01'          FUNKEY
118700970526     C                   END
118800970526     C     KEY           IFEQ      F2
118900970526     C                   MOVEL     '02'          FUNKEY
119000970526     C                   END
119100970526     C     KEY           IFEQ      F3
119200970526     C                   MOVEL     '03'          FUNKEY
119300970526     C                   END
119400970526     C     KEY           IFEQ      F4
119500970526     C                   MOVEL     '04'          FUNKEY
119600970526     C                   END
119700970526     C     KEY           IFEQ      F5
119800970526     C                   MOVEL     '05'          FUNKEY
119900970526     C                   END
120000970526     C     KEY           IFEQ      F6
120100970526     C                   MOVEL     '06'          FUNKEY
120200970526     C                   END
120300970526     C     KEY           IFEQ      F7
120400970526     C                   MOVEL     '07'          FUNKEY
120500970526     C                   END
120600970526     C     KEY           IFEQ      F8
120700970526     C                   MOVEL     '08'          FUNKEY
120800970526     C                   END
120900970526     C     KEY           IFEQ      F9
121000970526     C                   MOVEL     '09'          FUNKEY
121100970526     C                   END
121200970526     C     KEY           IFEQ      F10
121300970526     C                   MOVEL     '10'          FUNKEY
121400970526     C                   END
121500970526     C     KEY           IFEQ      F11
121600970526     C                   MOVEL     '11'          FUNKEY
121700970526     C                   END
121800970526     C     KEY           IFEQ      F12
121900970526     C                   MOVEL     '12'          FUNKEY
122000970526     C                   END
122100970526     C     KEY           IFEQ      F13
122200970526     C                   MOVEL     '13'          FUNKEY
122300970526     C                   END
122400970526     C     KEY           IFEQ      F14
122500970526     C                   MOVEL     '14'          FUNKEY
122600970526     C                   END
122700970526     C     KEY           IFEQ      F15
122800970526     C                   MOVEL     '15'          FUNKEY
122900970526     C                   END
123000970526     C     KEY           IFEQ      F16
123100970526     C                   MOVEL     '16'          FUNKEY
123200970526     C                   END
123300970526     C     KEY           IFEQ      F17
123400970526     C                   MOVEL     '17'          FUNKEY
123500970526     C                   END
123600970526     C     KEY           IFEQ      F18
123700970526     C                   MOVEL     '18'          FUNKEY
123800970526     C                   END
123900970526     C     KEY           IFEQ      F19
124000970526     C                   MOVEL     '19'          FUNKEY
124100970526     C                   END
124200970526     C     KEY           IFEQ      F20
124300970526     C                   MOVEL     '20'          FUNKEY
124400970526     C                   END
124500970526     C     KEY           IFEQ      F21
124600970526     C                   MOVEL     '21'          FUNKEY
124700970526     C                   END
124800970526     C     KEY           IFEQ      F22
124900970526     C                   MOVEL     '22'          FUNKEY
125000970526     C                   END
125100970526     C     KEY           IFEQ      F23
125200970526     C                   MOVEL     '23'          FUNKEY
125300970526     C                   END
125400970526     C     KEY           IFEQ      F24
125500970526     C                   MOVEL     '24'          FUNKEY
125600970526     C                   END
125700970526     C     KEY           IFEQ      ROLLUP
125800970526     C                   MOVEL     'RU'          FUNKEY
125900970526     C                   END
126000970526     C     KEY           IFEQ      HELP
126100970526     C                   MOVEL     'HP'          FUNKEY
126200970526     C                   END
126300970526     C*
126400970526     C                   ENDSR
126500970526     C*------------------------------------------------------------------------*
126600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
126700970526     C*------------------------------------------------------------------------*
126800970526     C     FUNGES        BEGSR
126900970526     C*
127000970526     C* PULISCO CAMPI DI LAVORO
127100970526     C                   Z-ADD     0             W                 5 0
127200970526     C*
127300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
127400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
127500970526     C     1             DO        24            I
127600970526     C                   MOVEL     FUD(I)        FUDRIG
127700970526     C*
127800970526     C* PULIZIA FUNZIONI PRECEDENTI
127900970526     C                   MOVEL     *BLANKS       FUV(I)
128000970526     C*
128100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
128200970526     C* (SALTA F23 E F24)
128300970526     C     FUDATT        IFEQ      'S'
128400970526     C     FUDFUN        ANDNE     '23'
128500970526     C     FUDFUN        ANDNE     '24'
128600970526     C                   ADD       1             W
128700970526     C                   MOVEL     FUDDES        FUV(W)
128800970526     C                   END
128900970526     C                   END                                                    DO WHILE
129000970526     C*
129100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
129200970526     C* ALTRIMENTI DISATTIVO F24
129300970526     C     1             DO        24            I
129400970526     C                   MOVEL     FUD(I)        FUDRIG
129500970526     C*
129600970526     C     FUDFUN        IFEQ      '24'
129700970526     C     W             IFGT      6
129800970526     C                   MOVEL     'S'           FUDATT
129900970526     C                   ELSE
130000970526     C                   MOVEL     *BLANKS       FUDATT
130100970526     C                   END                                                    W GT 8
130200970526     C                   MOVEL     FUDRIG        FUD(I)
130300970526     C                   END
130400970526     C                   END                                                    DO WHILE
130500970526     C*
130600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
130700970526     C     W             DIV       6             I
130800970526     C                   MVR                     Z
130900970526     C*
131000970526     C     Z             IFNE      0
131100970526     C     I             ADD       1             Z
131200970526     C                   ELSE
131300970526     C                   Z-ADD     I             Z
131400970526     C                   END
131500970526     C*
131600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
131700970526     C                   ADD       1             FUNGRU
131800970526     C     FUNGRU        IFGT      Z
131900970526     C     FUNGRU        ORLT      1
132000970526     C                   Z-ADD     1             FUNGRU
132100970526     C                   END
132200970526     C*
132300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
132400970526     C     FUNGRU        IFEQ      1
132500970526     C                   MOVEL     FUV(1)        FUN001
132600970526     C                   MOVEL     FUV(2)        FUN002
132700970526     C                   MOVEL     FUV(3)        FUN003
132800970526     C                   MOVEL     FUV(4)        FUN004
132900970526     C                   MOVEL     FUV(5)        FUN005
133000970526     C                   MOVEL     FUV(6)        FUN006
133100970526     C                   END
133200970526     C*
133300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
133400970526     C     FUNGRU        IFEQ      2
133500970526     C                   MOVEL     FUV(7)        FUN001
133600970526     C                   MOVEL     FUV(8)        FUN002
133700970526     C                   MOVEL     FUV(9)        FUN003
133800970526     C                   MOVEL     FUV(10)       FUN004
133900970526     C                   MOVEL     FUV(11)       FUN005
134000970526     C                   MOVEL     FUV(12)       FUN006
134100970526     C                   END
134200970526     C*
134300970526     C* TERZO GRUPPO DI 6 FUNZIONI
134400970526     C     FUNGRU        IFEQ      3
134500970526     C                   MOVEL     FUV(13)       FUN001
134600970526     C                   MOVEL     FUV(14)       FUN002
134700970526     C                   MOVEL     FUV(15)       FUN003
134800970526     C                   MOVEL     FUV(16)       FUN004
134900970526     C                   MOVEL     FUV(17)       FUN005
135000970526     C                   MOVEL     FUV(18)       FUN006
135100970526     C                   END
135200970526     C*
135300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
135400970526     C     FUNGRU        IFEQ      4
135500970526     C                   MOVEL     FUV(19)       FUN001
135600970526     C                   MOVEL     FUV(20)       FUN002
135700970526     C                   MOVEL     FUV(21)       FUN003
135800970526     C                   MOVEL     FUV(22)       FUN004
135900970526     C                   MOVEL     *BLANKS       FUN005
136000970526     C                   MOVEL     *BLANKS       FUN006
136100970526     C                   END
136200970526     C*
136300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
136400970526     C                   MOVEL     *BLANKS       FUN007
136500970526     C                   MOVEL     *BLANKS       FUN008
136600970526     C*
136700970526     C     1             DO        24            I
136800970526     C                   MOVEL     FUD(I)        FUDRIG
136900970526     C     FUDFUN        IFEQ      '23'
137000970526     C     FUDATT        ANDEQ     'S'
137100970526     C                   MOVEL     FUDDES        FUN007
137200970526     C                   END
137300970526     C     FUDFUN        IFEQ      '24'
137400970526     C     FUDATT        ANDEQ     'S'
137500970526     C                   MOVEL     FUDDES        FUN008
137600970526     C                   END
137700970526     C                   END                                                    DO
137800970526     C*
137900970526     C                   ENDSR
138000970526     C*------------------------------------------------------------------------*
138100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
138200970526     C*------------------------------------------------------------------------*
138300970526     C     OPZGES        BEGSR
138400970526     C*
138500970526     C* PULISCO CAMPI DI LAVORO
138600970526     C                   Z-ADD     0             W                 5 0
138700970526     C*
138800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
138900970526     C* E CARICO TABELLA OPZIONI ATTIVE
139000991115     C     1             DO        24            I
139100970526     C                   MOVEL     OPD(I)        OPDRIG
139200970526     C*
139300970526     C* PULIZIA OPZIONI PRECEDENTI
139400970526     C                   MOVEL     *BLANKS       OPV(I)
139500970526     C*
139600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
139700970526     C     OPDATT        IFEQ      'S'
139800970526     C                   ADD       1             W
139900970526     C                   MOVEL     OPDDES        OPV(W)
140000970526     C                   END
140100970526     C                   END                                                    DO WHILE
140200970526     C*
140300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
140400970526     C* ALTRIMENTI DISATTIVO F23
140500991115     C     1             DO        24            I
140600970526     C                   MOVEL     FUD(I)        FUDRIG
140700970526     C*
140800970526     C     FUDFUN        IFEQ      '23'
140900970526     C     W             IFGT      8
141000970526     C                   MOVEL     'S'           FUDATT
141100970526     C                   ELSE
141200970526     C                   MOVEL     *BLANKS       FUDATT
141300970526     C                   END                                                    W GT 8
141400970526     C                   MOVEL     FUDRIG        FUD(I)
141500970526     C                   END
141600970526     C                   END                                                    DO WHILE
141700970526     C*
141800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
141900970526     C     W             DIV       8             I
142000970526     C                   MVR                     Z                 5 0
142100970526     C*
142200970526     C     Z             IFNE      0
142300970526     C     I             ADD       1             Z
142400970526     C                   ELSE
142500970526     C                   Z-ADD     I             Z
142600970526     C                   END
142700970526     C*
142800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
142900970526     C                   ADD       1             OPZGRU
143000970526     C     OPZGRU        IFGT      Z
143100970526     C     OPZGRU        ORLT      1
143200970526     C                   Z-ADD     1             OPZGRU
143300970526     C                   END
143400970526     C*
143500970526     C* PRIMO GRUPPO DI 8 OPZIONI
143600970526     C     OPZGRU        IFEQ      1
143700970526     C                   MOVEL     OPV(1)        OPZ001
143800970526     C                   MOVEL     OPV(2)        OPZ002
143900970526     C                   MOVEL     OPV(3)        OPZ003
144000970526     C                   MOVEL     OPV(4)        OPZ004
144100970526     C                   MOVEL     OPV(5)        OPZ005
144200970526     C                   MOVEL     OPV(6)        OPZ006
144300970526     C                   MOVEL     OPV(7)        OPZ007
144400970526     C                   MOVEL     OPV(8)        OPZ008
144500970526     C                   END
144600970526     C*
144700970526     C* SECONDO GRUPPO DI 8 OPZIONI
144800970526     C     OPZGRU        IFEQ      2
144900970526     C                   MOVEL     OPV(9)        OPZ001
145000970526     C                   MOVEL     OPV(10)       OPZ002
145100970526     C                   MOVEL     OPV(11)       OPZ003
145200970526     C                   MOVEL     OPV(12)       OPZ004
145300970526     C                   MOVEL     OPV(13)       OPZ005
145400970526     C                   MOVEL     OPV(14)       OPZ006
145500970526     C                   MOVEL     OPV(15)       OPZ007
145600970526     C                   MOVEL     OPV(16)       OPZ008
145700970526     C                   END
145800970526     C*
145900970526     C* TERZO GRUPPO DI 8 OPZIONI
146000970526     C     OPZGRU        IFEQ      3
146100970526     C                   MOVEL     OPV(17)       OPZ001
146200970526     C                   MOVEL     OPV(18)       OPZ002
146300970526     C                   MOVEL     OPV(19)       OPZ003
146400970526     C                   MOVEL     OPV(20)       OPZ004
146500970526     C                   MOVEL     OPV(21)       OPZ005
146600970526     C                   MOVEL     OPV(22)       OPZ006
146700970526     C                   MOVEL     OPV(23)       OPZ007
146800970526     C                   MOVEL     OPV(24)       OPZ008
146900970526     C                   END
147000970526     C*
147100970526     C                   ENDSR
147200970526     C*------------------------------------------------------------------------*
147300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
147400970526     C*------------------------------------------------------------------------*
147500970526     C     OPZCON        BEGSR
147600970526     C*
147700970526     C* PULISCO CAMPI DI LAVORO
147800970526     C                   Z-ADD     0             I                 5 0
147900970526     C                   Z-ADD     0             W                 5 0
148000970526     C*
148100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
148200970526     C     1             DO        24            I
148300970526     C                   MOVEL     OPD(I)        OPDRIG
148400970526     C*
148500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
148600970526     C     OPZKEY        IFEQ      OPDOPZ
148700970526     C     OPZKEY        OREQ      OPDOP1
148800970526     C*
148900970526     C     OPDATT        IFEQ      'S'
149000970526     C                   MOVEL     OPDOPZ        OPZKEY
149100970526     C                   Z-ADD     I             W
149200970526     C                   END
149300970526     C                   END
149400970526     C*
149500970526     C                   END                                                    DO WHILE
149600970526     C*
149700970526     C* OPZIONE NON ATTIVA (ERRORE)
149800970526     C     W             IFEQ      0
149900970526     C*
150000970526     C* OPZIONE NON OK
150100970526     C                   Z-ADD     1             OPZOK
150200970526     C*
150300970526     C* EMISSIONE MESSAGGIO DI ERRORE
150400970526     C                   MOVEL     MSG(1)        DSMSMS
150500970526     C                   ELSE
150600970526     C*
150700970526     C* OPZIONE ATTIVA (PROCEDI ...)
150800970526     C*
150900000420     C     OPZKEY        IFEQ      ' 1'
151000000420     C                   EXSR      O01GES
151100000420     C                   END
151200000407     C     OPZKEY        IFEQ      ' 2'
151300000407     C                   EXSR      O02GES
151400000407     C                   END
151500020415     C     OPZKEY        IFEQ      ' 3'
151600020415     C                   EXSR      O03GES
151700020415     C                   END
151800991110     C     OPZKEY        IFEQ      ' 4'
151900991110     C                   EXSR      O04GES
152000991014     C                   END
152100970526     C     OPZKEY        IFEQ      ' 5'
152200970526     C                   EXSR      O05GES
152300970526     C                   END
152400000411     C     OPZKEY        IFEQ      ' 7'
152500000411     C                   EXSR      O07GES
152600000411     C                   END
152700040601     C     OPZKEY        IFEQ      ' 9'
152800040601     C                   EXSR      O09GES
152900040601     C                   END
153000020415     C     OPZKEY        IFEQ      '13'
153100020415     C                   EXSR      O13GES
153200020415     C                   END
153300970526     C                   END                                                    OPZ.NON ATTIVA
153400970526     C*
153500970526     C                   ENDSR
153600970526     C*------------------------------------------------------------------------*
153700970526     C* NOTE COSTRUZIONE SCHIERE
153800970526     C*------------------------------------------------------------------------*
153900970526     C*
154000970526     C* -MSG-MESSAGGI
154100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
154200970526     C* (NON TOCCARE MSG,1 E MSG,2)
154300970526     C*
154400970526     C* -CMD-COMANDI
154500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
154600970526     C*
154700970526     C* -OPD-OPZIONI
154800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
154900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
155000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
155100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
155200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
155300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
155400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
155500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
155600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
155700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
155800970526     C*
155900970526     C* -FUD-FUNZIONI
156000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
156100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
156200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
156300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
156400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
156500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
156600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
156700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
156800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
156900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
157000991108     C*
157100991115     C* -STS-CODICI STATI DEI RECORD DI LOG
157200991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
157300991110     C* ================> IMPORTANTE <=================
157400991115     C* per codici di errore in -STS- indicare in -DES- sempre
157500991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
157600991110     C* ERR
157700991108     C*
157800970526     O*------------------------------------------------------------------------*
157900970526** DEC - DECODIFICHE
158000040603     SCELTA                                                      1
158100040603    GESTIONE                                                     2
158200040603MODIFICA TEL/REF                                                 3
158300040603                                                                 4
158400040603 INTERROGAZIONE                                                  5
158500970526** MSG - MESSAGGI
158600020412TISIT1R- Opzione non gestita.                                                  1
158700020412TISIT1R- Tasto funzionale non gestito.                                         2
158800020412TISIT1R- Non abilitato all inserimento in filiale.                             3
158900020412TISIT1R- Record già annullato, consentito solo visualizz. e ripristino         4
159000020412TISIT1R- Record non annullato, impossibile ripristinare                        5
159100020412TISIT1R- Accettato solo un parametro di inizio lista                           6
159200020423TISIT1R- Non esistono sinonimi per il destinatario selezionato.                7
159300040601TISIT1R- Il destinatario indicato errato, correggere.                          8
159400040601TISIT1R- Copia sinonimi su altro destinatario non effettuata.                  9
159500040601TISIT1R- Copia sinonimi su altro destinatario effettuata.                     10
159600020412TISIT1R- 11                                                                   11
159700020412TISIT1R- 12                                                                   12
159800020412TISIT1R- 13                                                                   13
159900020412TISIT1R- 14                                                                   14
160000020412TISIT1R- 15                                                                   15
160100020412TISIT1R- 16                                                                   16
160200020412TISIT1R- 17                                                                   17
160300020412TISIT1R- 18                                                                   18
160400020412TISIT1R- 19                                                                   19
160500020412TISIT1R- 20                                                                   20
160600970528** CMD - COMANDI CL
160700991110DSPPFM FILE(
160800991110) MBR(
160900991110
161000970528
161100970528
161200970526** OPD - OPZIONI
161300000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
161400000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
161500020412 3=Copia           -3 -     (S/N)=N                        opz. 03¬
161600000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
161700000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
161800991115                   -  -     (S/N)=N                        opz. 06¬
161900000411 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
162000991110                   -  -     (S/N)=N                        opz. 08¬
162100040601 9=Copia sinonimi  -9 -     (S/N)=N                        opz. 09¬
162200991110                   -  -     (S/N)=N                        opz. 10¬
162300000407                   -  -     (S/N)=N                        opz. 11¬
162400970526                   -  -     (S/N)=N                        opz. 12¬
16250002041513=Sinonimi        -13-     (S/N)=N                        opz. 13¬
162600000407                   -  -     (S/N)=N                        opz. 14¬
162700000407                   -  -     (S/N)=N                        opz. 15¬
162800970526                   -  -     (S/N)=N                        opz. 16¬
162900970526                   -  -     (S/N)=N                        opz. 17¬
163000970526                   -  -     (S/N)=N                        opz. 18¬
163100970526                   -  -     (S/N)=N                        opz. 19¬
163200970526                   -  -     (S/N)=N                        opz. 20¬
163300970526                   -  -     (S/N)=N                        opz. 21¬
163400970526                   -  -     (S/N)=N                        opz. 22¬
163500970526                   -  -     (S/N)=N                        opz. 23¬
163600000407                   -  -     (S/N)=N                        opz. 25¬
163700970526** FUD - FUNZIONI
163800970526                   -        (S/N)=N                        funz.01¬
163900970526                   -        (S/N)=N                        funz.02¬
164000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
164100970526                   -        (S/N)=N                        funz.04¬
164200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
164300000407F06=Inserimento    -        (S/N)=N                        funz.06¬
164400970526                   -        (S/N)=N                        funz.07¬
164500970527                   -        (S/N)=N                        funz.08¬
164600970527                   -        (S/N)=N                        funz.09¬
164700970527                   -        (S/N)=N                        funz.10¬
164800970526                   -        (S/N)=N                        funz.11¬
164900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
165000991116                   -        (S/N)=N                        funz.13¬
165100970526                   -        (S/N)=N                        funz.14¬
165200970526                   -        (S/N)=N                        funz.15¬
165300970526                   -        (S/N)=N                        funz.16¬
165400970526                   -        (S/N)=N                        funz.17¬
165500970526                   -        (S/N)=N                        funz.18¬
165600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
165700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
165800000407                   -        (S/N)=N                        funz.21¬
165900991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
166000991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
166100991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
