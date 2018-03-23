000100001106     H*------------------------------------------------------------------------*
000200970603     H* CAPPARIO - GESTIONE SINONIMI LOCALITA' - SCELTA
000300970603     H*------------------------------------------------------------------------*
000400970603     H DECEDIT('0,') DATEDIT(*DMY.)
000500970603     F*------------------------------------------------------------------------*
000600970603     F* DATA BASE
000700970603     F*------------------------------------------------------------------------*
000800970603     FTISI35D   CF   E             WORKSTN INFDS(DEVDS1)
000900970603     F                                     SFILE(SI35D05:SFLULT)
001000970603     F                                     SFILE(SI35DMS:MSGULT)
001100970603     F*------------
001200970603     F* SINONIMI LOCALITA'
001300970603     F*------------
001400080407     FAZCPS01L  UF A E           K DISK
001500970627     FAZCPS02L  IF   E           K DISK
001600970627     F                                     RENAME(AZCPS000:AZCPS2)
001700970624     FAZCPS03L  IF   E           K DISK
001800970624     F                                     RENAME(AZCPS000:AZCPS3)
001900970603     F*------------
002000970603     F* CAPPARIO
002100970603     F*------------
002200970603     FAZCPC01L  IF   E           K DISK
002300061201     F*------------
002400061201     F* LOCALITA'
002500061201     F*------------
002600080407     FAZCPL01L  UF   E           K DISK
002700970603     D*------------------------------------------------------------------------*
002800970603     D* SCHIERE
002900970603     D*------------------------------------------------------------------------*
003000970603     D*------------
003100970603     D* DECODIFICHE
003200970603     D*------------
003300970603     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003400970603     D*------------
003500970603     D* MESSAGGI
003600970603     D*------------
003700970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003800970603     D*------------
003900970603     D* DESCRIZIONI OPZIONI
004000970603     D*------------
004100970603     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004200970603     D*------------
004300970603     D* DESCRIZIONI TASTI FUNZIONALI
004400970603     D*------------
004500970603     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004600970603     D*------------
004700970603     D* OPZIONI DA CARICARE A VIDEO
004800970603     D*------------
004900970603     D OPV             S             19    DIM(24)
005000970603     D*------------
005100970603     D* FUNZIONI DA CARICARE A VIDEO
005200970603     D*------------
005300970603     D FUV             S             19    DIM(24)
005400970603     D*------------------------------------------------------------------------*
005500970603     D* INPUT
005600970603     D*------------------------------------------------------------------------*
005700970603     D*------------
005800970603     D* RIDEFINIZIONE OPZIONI VIDEO
005900970603     D*------------
006000970603     D OPDDS           DS
006100970603     D  OPDOPZ                 1      2
006200970603     D  OPDDE1                 3     19
006300970603     D  OPDOP1                21     22
006400970603     D  OPDFI0                23     34
006500970603     D  OPDATT                35     35
006600970603     D  OPDFI1                36     80
006700970603     D  OPDDES                 1     19
006800970603     D  OPDRIG                 1     80
006900970603     D*------------
007000970603     D* RIDEFINIZIONE FUNZIONI VIDEO
007100970603     D*------------
007200970603     D FUDDS           DS
007300970603     D  FUDDE1                 1      1
007400970603     D  FUDFUN                 2      3
007500970603     D  FUDDE2                 4     19
007600970603     D  FUDFI0                20     34
007700970603     D  FUDATT                35     35
007800970603     D  FUDFI1                36     80
007900970603     D  FUDDES                 1     19
008000970603     D  FUDRIG                 1     80
008100970603     D*------------
008200970603     D* DS GESTIONE TASTI FUNZIONALI
008300970603     D*------------
008400970603     D DSKEY         E DS
008500970603     D DEVDS1          DS
008600970603     D  KEY                  369    369
008700970603     D*------------
008800970603     D* DS "XMSG" - GESTIONE MESSAGGI
008900970603     D*------------
009000970603     D DSMSG         E DS
009100970603     D*------------
009200020919     D* DS REPERIMENTO DATI UTENTE
009300970603     D*------------
009400020919     D TIBS34DS      E DS                                                       *Profili utente
009500020919     D DDATIUTE      E DS                                                       *Dati utente
009600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009700080401     D DUTE01        E DS                                                       *Abilitazioni
009800080407     D*---------------------
009900080407     D* DS "TISI95R" - CONTROLLA CAP
010000080407     D*---------------------
010100080407     D DSSI95        E DS                  EXTNAME(TISI95DS)
010200970603     D*------------------
010300970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400970603     D*------------------
010500970603     D WLBDA8          DS                  INZ
010600970603     D  G08DAT                 1      8  0
010700970603     D  G08INV                 9     16  0
010800970603     D  G08ERR                17     17
010900970603     D  G08TGI                18     22  0
011000970603     D*------------
011100970603     D* DS DI PROCEDURA
011200970603     D*------------
011300970603     D DSSI00        E DS                  EXTNAME(TISI00DS)
011400970603     D*------------
011500970603     D* ARCHITETTURA
011600970603     D*------------
011700970603     D KPJBA         E DS
011800970603     C*------------------------------------------------------------------------*
011900970603     C* MAIN LINES
012000970603     C*------------------------------------------------------------------------*
012100970603     C*
012200970603     C* SE NON CI SONO ERRORI, PROSEGUE
012300970603IF  1C     ERRL00        IFNE      '1'
012400970603     C*
012500970603     C* CARICA IL SFILE
012600970603     C                   EXSR      POSSFL
012700970603     C*
012800970603     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012900970603DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013000970603     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013100970603     C     FINL00        OREQ      '1'                                          *FINE PGM
013200970603     C*
013300970603     C* REIMPOSTA ERRORE DI PROCERURA
013400970603     C                   MOVEL     '0'           ERRL00
013500970603     C*
013600970603     C* EMETTE IL SFILE
013700970603     C                   EXSR      OUTSFL
013800970603     C*
013900970603     C* GESTIONE TASTI FUNZIONALI
014000970603     C                   EXSR      FUNCON
014100970603     C*
014200970603E   2C                   ENDDO
014300970603     C*
014400970603     C* CI SONO ERRORI IN ENTRATA
014500970603X   1C                   ELSE
014600970603     C                   MOVEL     '1'           D00ERR
014700970603     C                   MOVEL     DSMSMS        D00MSG
014800970603E   1C                   ENDIF
014900080407     C*
015000080407     C* OPERAZIONI FINALI
015100080407     C                   EXSR      FINSR
015200080407     C*
015300970603     C* FINE PGM
015400970603     C                   SETON                                        LR
015500970603     C*------------------------------------------------------------------------*
015600970603     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015700970603     C*------------------------------------------------------------------------*
015800970603     C     F03GES        BEGSR
015900970603     C*
016000970603     C* IMPOSTA "PREMUTO F03"
016100970603     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016200970603     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016300970603     C*
016400970603     C                   ENDSR
016500970603     C*------------------------------------------------------------------------*
016600970603     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016700970603     C*------------------------------------------------------------------------*
016800970603     C     F05GES        BEGSR
016900970603     C*
017000970603     C                   EXSR      POSSFL                                       *RICARICA SFILE
017100970603     C*
017200970603     C                   ENDSR
017300970603     C*------------------------------------------------------------------------*
017400970603     C* F06GES - GESTIONE F06 -> INSERIMENTO
017500970603     C*------------------------------------------------------------------------*
017600970603     C     F06GES        BEGSR
017700970603     C*
017800970603     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017900021115IF  1C***  DUTLPO        IFNE      'S'
018000021115     C***                MOVEL     '1'           ERRL00
018100021115     C***                MOVEL     MSG(3)        DSMSMS
018200021115E   1C***                ENDIF
018300970603     C*
018400970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018500970603IF  2C     D00NAR        IFNE      *BLANKS
018600970603     C     D00CAP        ORNE      *BLANKS
018700970603     C                   MOVEL     D00NAR        DNAR
018800970603     C                   MOVEL     D00CAP        DCAP
018900970603     C                   MOVEL     D00LOC        DLOC
019000001031     C* INTRODUCO IL TEST SEGUENTE PER NON ALTERARE EVENTUALI RAGIONAMENTI PRE-ESISTENTI
019100001106     C     DEPOP1        IFEQ      'O09'
019200001106     C     DEPOP1        OREQ      'O08'
019300001106     C                   MOVEL     D00SIN        DSIN
019400001031     C                   ENDIF
019500970603X   2C                   ELSE
019600970603     C                   MOVEL     *BLANKS       DNAR
019700970603     C                   MOVEL     *BLANKS       DCAP
019800970603     C                   MOVEL     *BLANKS       DLOC
019900001031     C                   MOVEL     *BLANKS       DSIN
020000970603E   2C                   ENDIF
020100970603     C                   MOVEL     DEPOP0        WOP0
020200970603     C                   MOVEL     'F06'         WOP1
020300080402     C                   MOVEL(P)  'TISI36R'     WPGM              8
020400970603     C                   EXSR      LANPGM                                       *LANCIA PGM
020500970603     C                   EXSR      POSSFL                                       *RICARICA SFILE
020600970603E   1C                   ENDIF
020700970603     C*
020800970603     C                   ENDSR
020900970603     C*------------------------------------------------------------------------*
021000970603     C* F12GES - GESTIONE F12 -> RITORNO
021100970603     C*------------------------------------------------------------------------*
021200970603     C     F12GES        BEGSR
021300970603     C*
021400970603     C* IMPOSTA "PREMUTO F12"
021500970603     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021600970603     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021700970603     C*
021800970603     C                   ENDSR
021900970603     C*------------------------------------------------------------------------*
022000970603     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022100970603     C*------------------------------------------------------------------------*
022200970603     C     F23GES        BEGSR
022300970603     C*
022400970603     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022500970603     C*
022600970603     C                   ENDSR
022700970603     C*------------------------------------------------------------------------*
022800970603     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022900970603     C*------------------------------------------------------------------------*
023000970603     C     F24GES        BEGSR
023100970603     C*
023200970603     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023300970603     C*
023400970603     C                   ENDSR
023500970603     C*------------------------------------------------------------------------*
023600970603     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023700970603     C*------------------------------------------------------------------------*
023800970603     C     RUPGES        BEGSR
023900970603     C*
024000970603     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024100970603     C*
024200970603     C                   ENDSR
024300970603     C*------------------------------------------------------------------------*
024400970603     C* ENTGES - GESTIONE ENTER -> INVIO
024500970603     C*------------------------------------------------------------------------*
024600970603     C     ENTGES        BEGSR
024700970603     C*
024800970603     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024900970603     C                   EXSR      NEWPOS
025000970603     C*
025100970603     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025200970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025300970603IF  2C     SWPOSI        IFNE      0
025400970603     C                   EXSR      POSSFL                                       *RICARICA SFILE
025500970603X   2C                   ELSE
025600970603IF  3C     SFLULT        IFNE      0
025700970603     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025800970603     C*
025900970603     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026000970603IF  4C     SWMODI        IFNE      0
026100970603     C                   EXSR      POSSFL
026200970603E   4C                   ENDIF
026300970603E   3C                   ENDIF
026400970603E   2C                   ENDIF
026500970603E   1C                   ENDIF
026600970603     C*
026700970603     C                   ENDSR
026800970603     C*------------------------------------------------------------------------*
026900970603     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027000970603     C*------------------------------------------------------------------------*
027100970603     C     O01GES        BEGSR
027200970603     C*
027300970603     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027400061201     C***                MOVEL     VIDNAR        D00NAR
027500061201     C                   MOVEL     *BLANKS       D00NAR
027600970603     C                   MOVEL     VIDCAP        D00CAP
027700970603     C                   MOVEL     VIHLOC        D00LOC
027800970603     C                   MOVEL     VIDSIN        D00SIN
027900970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
028000970603     C*
028100970603     C                   ENDSR
028200970603     C*------------------------------------------------------------------------*
028300970603     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028400970603     C*------------------------------------------------------------------------*
028500970603     C     O02GES        BEGSR
028600970603     C*
028700970603     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028800970603IF  1C     VIDATB        IFEQ      'A'
028900970603     C                   MOVEL     '1'           ERRL00
029000970603     C                   MOVEL     MSG(4)        DSMSMS
029100970603E   1C                   ENDIF
029200970603     C* MODIFICA
029300970704IF  1C     ERRL00        IFEQ      '0'
029400061201     C***                MOVEL     VIDNAR        DNAR
029500061201     C                   MOVEL     *BLANKS       DNAR
029600970603     C                   MOVEL     VIDCAP        DCAP
029700970603     C                   MOVEL     VIHLOC        DLOC
029800970603     C                   MOVEL     VIDSIN        DSIN
029900970603     C                   MOVEL     DEPOP0        WOP0
030000970603     C                   MOVEL     'O02'         WOP1
030100080402     C                   MOVEL(P)  'TISI36R'     WPGM
030200970603     C                   EXSR      LANPGM
030300970603     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
030400970603E   1C                   ENDIF
030500970603     C*
030600970603     C                   ENDSR
030700970603     C*------------------------------------------------------------------------*
030800970603     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
030900970603     C*------------------------------------------------------------------------*
031000970603     C     O04GES        BEGSR
031100970603     C*
031200970603     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031300970603IF  1C     VIDATB        IFEQ      'A'
031400970603     C                   MOVEL     '1'           ERRL00
031500970603     C                   MOVEL     MSG(4)        DSMSMS
031600970603E   1C                   ENDIF
031700970603     C*
031800970603     C* CONTROLLA SE C'E' L'ABILITAZIONE PER GESTIRE IL CAP
031900970603IF  1C     ERRL00        IFEQ      '0'
032000061201     C***                MOVEL     VIDNAR        DNAR
032100061201     C                   MOVEL     *BLANKS       DNAR
032200970603     C                   MOVEL     VIDCAP        DCAP
032300970603     C                   MOVEL     VIHLOC        DLOC
032400970603     C                   MOVEL     VIDSIN        DSIN
032500970603     C                   MOVEL     DEPOP0        WOP0
032600970603     C                   MOVEL     'O04'         WOP1
032700080402     C                   MOVEL(P)  'TISI36R'     WPGM
032800970603     C                   EXSR      LANPGM
032900970603     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033000970603E   1C                   ENDIF
033100970603     C*
033200970603     C                   ENDSR
033300970603     C*------------------------------------------------------------------------*
033400970603     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
033500970603     C*------------------------------------------------------------------------*
033600970603     C     O05GES        BEGSR
033700970603     C*
033800061201     C***                MOVEL     VIDNAR        DNAR
033900061201     C                   MOVEL     *BLANKS       DNAR
034000970603     C                   MOVEL     VIDCAP        DCAP
034100970603     C                   MOVEL     VIHLOC        DLOC
034200970603     C                   MOVEL     VIDSIN        DSIN
034300970603     C                   MOVEL     DEPOP0        WOP0
034400970603     C                   MOVEL     'O05'         WOP1
034500080402     C                   MOVEL(P)  'TISI36R'     WPGM
034600970603     C                   EXSR      LANPGM
034700970603     C*
034800970603     C                   ENDSR
034900970603     C*------------------------------------------------------------------------*
035000970603     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035100970603     C*------------------------------------------------------------------------*
035200970603     C     O07GES        BEGSR
035300970603     C*
035400970603     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
035500970603IF  1C     VIDATB        IFNE      'A'
035600970603     C                   MOVEL     '1'           ERRL00
035700970603     C                   MOVEL     MSG(5)        DSMSMS
035800970603E   1C                   ENDIF
035900970603     C* RIPRISITINO
036000970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036100061201     C***                MOVEL     VIDNAR        DNAR
036200061201     C                   MOVEL     *BLANKS       DNAR
036300970603     C                   MOVEL     VIDCAP        DCAP
036400970603     C                   MOVEL     VIHLOC        DLOC
036500970603     C                   MOVEL     VIDSIN        DSIN
036600970603     C                   MOVEL     DEPOP0        WOP0
036700970603     C                   MOVEL     'O07'         WOP1
036800080402     C                   MOVEL(P)  'TISI36R'     WPGM
036900970603     C                   EXSR      LANPGM
037000970603     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
037100970603E   1C                   ENDIF
037200970603     C*
037300970603     C                   ENDSR
037400080401     C*------------------------------------------------------------------------*
037500080401     C* O11GES - GESTIONE OPZIONE 11=SPOSTA SINONIMO
037600080401     C*------------------------------------------------------------------------*
037700080401     C     O11GES        BEGSR
037800080401     C*
037900080401     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
038000080401IF  1C     VIDATB        IFEQ      'A'
038100080401     C                   MOVEL     '1'           ERRL00
038200080401     C                   MOVEL     MSG(4)        DSMSMS
038300080401E   1C                   ENDIF
038400080401     C* SPOSTAMENTO SINONIMO (*PGM ESTERNO)
038500080401IF  1C     ERRL00        IFEQ      '0'
038600080407     C                   EXSR      GESWND
038700080407     C                   ENDIF
038800080401     C*
038900080401     C                   ENDSR
039000001106     C*------------------------------------------------------------------------*
039100001106     C* O13GES - GESTIONE OPZIONE 13 -> INSERIMENTO DA OPZIONE
039200001106     C*------------------------------------------------------------------------*
039300001107     C     O13GES        BEGSR
039400001106     C*
039500001106     C* INSERIMENTO CONSENTITO SOLO IN SEDE
039600020919IF  1C     DUTLPO        IFNE      'S'
039700001106     C                   MOVEL     '1'           ERRL00
039800001106     C                   MOVEL     MSG(3)        DSMSMS
039900001106E   1C                   ENDIF
040000001106     C*
040100001106IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040200001106     C* INTRODUCO TEST (RIDONDANTE) SEGUENTE PER NON ALTERARE EVENTUALI RAGIONAMENTI PRE-ESISTENTI
040300001106     C     DEPOP1        IFEQ      'O09'
040400001106     C     DEPOP1        OREQ      'O08'
040500061201     C***                MOVEL     VIDNAR        DNAR
040600061201     C                   MOVEL     *BLANKS       DNAR
040700001106     C                   MOVEL     VIDCAP        DCAP
040800001106     C                   MOVEL     VIHLOC        DLOC
040900001106     C                   MOVEL     D00SIN        DSIN
041000001106     C                   ENDIF
041100001106     C                   MOVEL     DEPOP0        WOP0
041200001106     C                   MOVEL     'F06'         WOP1
041300080402     C                   MOVEL(P)  'TISI36R'     WPGM              8
041400001106     C                   EXSR      LANPGM                                       *LANCIA PGM
041500001107     C*                    EXSR POSSFL                     *RICARICA SFILE
041600001106E   1C                   ENDIF
041700001106     C*
041800001106     C                   ENDSR
041900080407     C*------------------------------------------------------------------------*
042000080407     C* GESWND - GESTIONE WINDOWS OPZIONE 11=SPOSTA SINONIMI
042100080407     C*------------------------------------------------------------------------*
042200080407     C     GESWND        BEGSR
042300080407     C*
042400080407     C* LEGGE IL SINONIMO
042500080407     C                   Z-ADD     D00VER        KCSVER
042600080407     C                   MOVEL     *BLANKS       KCSNAR
042700080407     C                   MOVEL     VIDCAP        KCSCAP
042800080407     C                   MOVEL     VIHLOC        KCSLOC
042900080407     C                   MOVEL     VIDSIN        KCSSIN
043000080407     C     KEYCPS01C     CHAIN(N)  AZCPS01L
043100080407IF  1C                   IF        %found(AZCPS01L)                             *TROVATO
043200080407     C* DATI OLD
043300080407     C                   MOVEL     CPSNAR        V1CNAR                         *NAZIONE
043400080407     C                   MOVEL     CPSCAP        V1CCAP                         *CAP
043500080407     C                   MOVEL     CPSLOC        V1CLOC                         *LOCALITA'
043600080407     C                   MOVEL     CPSSIN        V1CSIN                         *SINONIMO
043700080407     C* DATI NEW
043800080407     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
043900080407     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
044000080407     C                   MOVEL     V1CLOC        V1CLO2                         *LOCALITA'
044100080407X   1C                   ELSE                                                   *NON TROVATO
044200080407     C                   SETON                                        28
044300080407     C                   MOVEL     MSG(19)       WNDMSG                         *MESSSAGGIO
044400080407E   1C                   ENDIF
044500080407     C*
044600080407     C                   DO        *hival
044700080407     C                   EXFMT     SI35D0W
044800080407     C                   SETOFF                                       28
044900080407     C                   CLEAR                   WNDMSG
045000080407     C*
045100080407     C* Gestione tasto F12
045200080407     C                   IF        *INKL = *ON
045300080407     C                   LEAVE
045400080407     C                   ENDIF
045500080407     C*
045600080407     C* Gestione tasto ENTER
045700080407     C                   EXSR      DECWND
045800080407     C                   IF        *IN28
045900080407     C                   ITER
046000080407     C                   ENDIF
046100080407     C*
046200080407     C* Gestione tasto F06
046300080407     C                   IF        *INKF = *ON
046400080407     C                   EXSR      EXEWND
046500080407     C                   SETOFF                                       KF
046600080407     C                   LEAVE
046700080407     C                   ENDIF
046800080407     C                   ENDDO
046900080407     C*
047000080407     C                   ENDSR
047100080407     C*--------------------------------------------------------------------------------------------*
047200080407     C* DECWND - CONTROLLA CAMPI E DECODIFICA WINDOWS (OPZ11)
047300080407     C*--------------------------------------------------------------------------------------------*
047400080407     C     DECWND        BEGSR
047500080407     C*
047600080407     C* REIMPOSTA INDICATORI DI ERRORE
047700080407     C                   SETOFF                                       606162
047800080407     C*--------------------
047900080407     C* CAP
048000080407     C*--------------------
048100080407     C     *IN28         IFEQ      '0'
048200080407     C                   MOVEL     V1CNA2        WNAR
048300080407     C                   MOVEL     V1CCA2        WCAP
048400080407     C                   MOVEL     *BLANKS       WFI1
048500080407     C                   EXSR      CHKCAP
048600080407     C     *IN28         IFEQ      '1'
048700080407     C                   MOVEL     '1'           *IN28
048800080407     C                   MOVEL     MSG(8)        WNDMSG
048900080407     C                   SETON                                        61
049000080407     C                   ENDIF
049100080407     C                   ENDIF
049200080407     C*
049300080407     C* IL CAP NON PUO' ESSERE OBSOLETO
049400080407     C     *IN28         IFEQ      '0'
049500080407     C     WFI1          IFEQ      'O '
049600080407     C                   MOVEL     '1'           *IN28
049700080407     C                   MOVEL     MSG(7)        WNDMSG
049800080407     C                   SETON                                        6061
049900080407     C                   ENDIF
050000080407     C                   ENDIF
050100080407     C*--------------------
050200080407     C* LOCALITA => DEVE ESISTERE + NO ANNULLATA + NO OBSOLETA
050300080407     C*--------------------
050400080407     C     *IN28         IFEQ      '0'
050500080407     C                   Z-ADD     D00VER        KCSVER
050600080409     C                   MOVEL     V1CNA2        KCSNAR
050700080409     C                   MOVEL     V1CCA2        KCSCAP
050800080409     C                   MOVEL     V1CLO2        KCSLOC
050900080407     C*
051000080407     C     KEYCPL        CHAIN(N)  AZCPL01L
051100080407     C                   IF        not %found(AZCPL01L) OR
051200080407     C                             cplATB <> *blanks    OR
051300080407     C                             cplFI1 <> *blanks
051400080407     C                   MOVEL     '1'           *IN28
051500080407     C                   MOVEL     MSG(9)        WNDMSG
051600080407     C                   SETON                                        62
051700080407     C                   ENDIF
051800080407     C                   ENDIF
051900080407     C*
052000080407     C                   ENDSR
052100080407     C*------------------------------------------------------------------------*
052200080407     C* CHKCAP - CONTROLLA IL CAP
052300080407     C*------------------------------------------------------------------------*
052400080407     C     CHKCAP        BEGSR
052500080407     C*
052600080407     C     *LIKE         DEFINE    V1CNA2        WNAR
052700080407     C     *LIKE         DEFINE    V1CCA2        WCAP
052800080407     C     *LIKE         DEFINE    O95CF1        WFI1
052900080407     C*
053000080407     C* CONTROLLA IL CAP
053100080407     C                   CLEAR                   DSSI95
053200080407     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
053300080407     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
053400080407     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
053500080407     C                   MOVEL     WCAP          I95CAP                         *CAP
053600080407     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
053700080407     C                   CALL      'TISI95R'
053800080407     C                   PARM                    DSSI95
053900080407IF  1C     O95ERR        IFNE      *BLANKS
054000080407     C     O95LID        ORLT      '2'
054100080407     C                   MOVEL     '1'           *IN28
054200080407X   1C                   ELSE
054300080407     C                   MOVEL     O95CF1        WFI1
054400080407E   1C                   ENDIF
054500080407     C*
054600080407     C                   ENDSR
054700080407     C*--------------------------------------------------------------------------------------------*
054800080407     C* EXEWND - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA OPZ11
054900080407     C*--------------------------------------------------------------------------------------------*
055000080407     C     EXEWND        BEGSR
055100080407     C*
055200080407     C* RILEGGE IL SINONIMO DA CUI COPIARE
055300080407IF  1C     *IN28         IFEQ      '0'
055400080407     C                   Z-ADD     D00VER        KCSVER
055500080407     C                   MOVEL     V1CNAR        KCSNAR
055600080407     C                   MOVEL     V1CCAP        KCSCAP
055700080407     C                   MOVEL     V1CLOC        KCSLOC
055800080407     C                   MOVEL     V1CSIN        KCSSIN
055900080407     C     KEYCPS01C     CHAIN     AZCPS01L                           99
056000080407IF  2C     *IN99         IFEQ      *OFF
056100080407     C*
056200080407     C* ANNULLA IL SINONIMO DI PARTENZA
056300080407     C                   EXSR      ANNCPS                                       *ANNULLA SINONIMI
056400080407     C*
056500080407     C* CREA IL NUOVO SINONIMO
056600080407     C                   EXSR      CRECPS                                       *CREA SINONIMI
056700080407     C*
056800080407     C* VALORIZZA IL FLAG "ESITE SINONIMO" SULLA LOCALITA DEL SINONIMO DI DESTINAZIONE
056900080407     C     KEYCPL        CHAIN     AZCPL01L                           99
057000080407IF  3C     *IN99         IFEQ      *OFF
057100080407     C                   EVAL      CPLESS = 'S'
057200080407     C                   UPDATE    AZCPL000
057300080407E   3C                   ENDIF
057400080407E   2C                   ENDIF
057500080407     C*
057600080407     C* IMPOSTA FLAG DI FINE PROGRAMMA
057700080407     C                   MOVEL     MSG(20)       D00MSG
057800080407E   1C                   ENDIF
057900080407     C*
058000080407     C                   ENDSR
058100080407     C*--------------------------------------------------------------------------------------------*
058200080407     C* CRECPS - CREA I SINONIMI DELLA NUOVA LOCALITA'
058300080407     C*--------------------------------------------------------------------------------------------*
058400080407     C     CRECPS        BEGSR
058500080407     C*
058600080407     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
058700080407     C                   Z-ADD     D00VER        CPSVER                         *VERSIONE
058800080407     C                   MOVEL     V1CNA2        CPSNAR                         *NAZIONE
058900080407     C                   MOVEL     V1CCA2        CPSCAP                         *CAP
059000080407     C                   MOVEL     V1CLO2        CPSLOC                         *LOCALITA'
059100080407     C                   MOVEL     V1CSIN        CPSSIN                         *SINONIMO
059200080407     C                   MOVEL     *BLANKS       CPSATB                         *FLAG ANNULLAMENTO
059300080407     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
059400080407     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
059500080407     C                   WRITE(E)  AZCPS000
059600080407     C*
059700080407     C                   ENDSR
059800080407     C*--------------------------------------------------------------------------------------------*
059900080407     C* ANNCPS - ANNULLA SINONIMO ORIGINARIO
060000080407     C*--------------------------------------------------------------------------------------------*
060100080407     C     ANNCPS        BEGSR
060200080407     C*
060300080407     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
060400080407     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
060500080407     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
060600080407     C                   UPDATE    AZCPS000
060700080407     C*
060800080407     C                   ENDSR
060900970603     C*------------------------------------------------------------------------*
061000970603     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
061100970603     C*------------------------------------------------------------------------*
061200970603     C     NEWPOS        BEGSR
061300970603     C*
061400970603     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
061500970603     C*         1= RICHIESTO POSIZIONAMENTO
061600970603     C                   Z-ADD     0             SWPOSI            7 0
061700970603     C*
061800970603     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
061900970603     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
062000970603     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
062100970603     C                   Z-ADD     0             SWMODI            1 0
062200970603     C*
062300970603     C* REIMPOSTA INDICATORI DI ERRORE
062400970603     C                   SETOFF                                       808182
062500970627     C                   SETOFF                                       83
062600970603     C*
062700970603     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
062800970603IF  1C     VIDPNA        IFNE      DEPNAR
062900970603     C     VIDPCA        ORNE      DEPCAP
063000970603     C     VIDPLO        ORNE      DEPLOC
063100970627     C     VIDPSI        ORNE      DEPSIN
063200970603     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
063300970603IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
063400970603     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
063500970603     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
063600970603     C                   MOVEL     VIDPCA        DEPCAP
063700970603     C                   MOVEL     VIDPLO        DEPLOC
063800970627     C                   MOVEL     VIDPSI        DEPSIN
063900970603E   2C                   ENDIF
064000970603E   1C                   ENDIF
064100970603     C*
064200970603     C                   ENDSR
064300970603     C*------------------------------------------------------------------------*
064400970603     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
064500970603     C*------------------------------------------------------------------------*
064600970603     C     SETFIL        BEGSR
064700970603     C*
064800970627     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO RICERCA RICHIESTO
064900970624IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
065000970603     C                   Z-ADD     D00VER        KCSVER
065100970603     C                   MOVEL     VIDPNA        KCSNAR
065200970603     C                   MOVEL     VIDPCA        KCSCAP
065300970624     C                   MOVEL     VIDPLO        KCSLOC
065400970603     C     KEYCPS        SETLL     AZCPS01L                           99
065500970624E   1C                   ENDIF
065600970624IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
065700970624     C                   Z-ADD     D00VER        KCSVER
065800970624     C                   MOVEL     VIDPNA        KCSNAR
065900970624     C                   MOVEL     VIDPLO        KCSLOC
066000970624     C     KE3CPS        SETLL     AZCPS03L                           99
066100970624E   1C                   ENDIF
066200970627IF  1C     WTIPRI        IFEQ      'SIN'                                        *PER SINONIMO
066300970627     C                   Z-ADD     D00VER        KCSVER
066400970627     C                   MOVEL     VIDPNA        KCSNAR
066500970627     C                   MOVEL     VIDPSI        KCSSIN
066600970627     C     KE2CPS        SETLL     AZCPS02L                           99
066700970627E   1C                   ENDIF
066800970603     C* LETTURA RECORD
066900970603IF  1C     *IN99         IFEQ      *OFF
067000970603     C                   EXSR      LETFIL
067100970603E   1C                   ENDIF
067200970603     C*
067300970603     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
067400970603IF  1C     *IN99         IFEQ      *ON
067500970603     C                   Z-ADD     1             SFLEOF
067600970603E   1C                   ENDIF
067700970603     C*
067800970603     C                   ENDSR
067900970603     C*------------------------------------------------------------------------*
068000970603     C* LETFIL - LETTURA RECORD
068100970603     C*------------------------------------------------------------------------*
068200970603     C     LETFIL        BEGSR
068300970603     C*
068400970603     C* LEGGE FINO A:
068500970603     C*     - FINE FILE
068600970603     C*     - TROVATO RECORD VALIDO
068700970603     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
068800970603DO  1C     *IN99         DOWEQ     *OFF
068900970603     C     WRECOK        ANDEQ     'N'
069000970603     C*
069100970627     C* LETTURE SUCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA RICHIESTO
069200970624IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
069300080407     C     KCSVER        READE(N)  AZCPS01L                               99
069400970624E   1C                   ENDIF
069500970624IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
069600970624     C     KCSVER        READE     AZCPS03L                               99
069700970624E   1C                   ENDIF
069800970627IF  1C     WTIPRI        IFEQ      'SIN'                                        *PER SINONIMO
069900970627     C     KCSVER        READE     AZCPS02L                               99
070000970627E   1C                   ENDIF
070100970603     C*
070200970603     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
070300970603IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
070400970603     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
070500970603X   2C                   ELSE
070600970603     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
070700970603E   2C                   ENDIF
070800970603E   1C                   ENDDO
070900970603     C*
071000970603     C                   ENDSR
071100970603     C*------------------------------------------------------------------------*
071200970603     C* CHKREC - CONTROLLA VALIDITA' RECORD
071300970603     C*------------------------------------------------------------------------*
071400970603     C     CHKREC        BEGSR
071500970603     C*
071600970603     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
071700970624     C*
071800970624     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE I SINONIMI NON DEL CAP
071900970624IF  1C     VIDPNA        IFNE      *BLANKS
072000970624     C     VIDPCA        ORNE      *BLANKS
072100970624IF  2C     VIDPNA        IFNE      CPSNAR
072200970624     C     VIDPCA        ORNE      CPSCAP
072300970624     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072400970625E   2C                   ENDIF
072500970624E   1C                   ENDIF
072600970603     C*
072700970603     C                   ENDSR
072800970603     C*------------------------------------------------------------------------*
072900970603     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
073000970603     C*------------------------------------------------------------------------*
073100970603     C     IRESFL        BEGSR
073200970603     C*
073300970603     C                   MOVEL     *BLANKS       OPZKEY
073400970603     C                   MOVEL     CPSLOC        VIHLOC
073500970603     C                   MOVEL     CPSATB        VIDATB
073600061201     C***                MOVEL     CPSNAR        VIDNAR
073700970603     C                   MOVEL     CPSCAP        VIDCAP
073800970603     C                   MOVEL     CPSLOC        VIDLOC
073900970603     C                   MOVEL     CPSSIN        VIDSIN
074000970603     C* ALTRI DATI
074100970603     C                   Z-ADD     CPSVER        KCCVER
074200970603     C                   MOVEL     CPSNAR        KCCNAR
074300970603     C                   MOVEL     CPSCAP        KCCCAP
074400970603     C     KEYCPC        CHAIN     AZCPC01L                           99
074500970603IF  1C     *IN99         IFEQ      *OFF
074600970603     C                   Z-ADD     CPCLNA        VIHCLA
074700970603X   1C                   ELSE
074800970603     C                   Z-ADD     *ZEROS        VIHCLA
074900970603E   1C                   ENDIF
075000061201     C*
075100061201     C* ALTRI DATI LOCALITA'
075200061201     C                   Z-ADD     CPSVER        KCSVER
075300061201     C                   MOVEL     CPSNAR        KCSNAR
075400061201     C                   MOVEL     CPSCAP        KCSCAP
075500061201     C                   MOVEL     CPSLOC        KCSLOC
075600080407     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
075700061201     C                   IF        %found(AZCPL01L)
075800061201     C                   EVAL      VIDLOCOBS = CPLFI1
075900061201     C                   ENDIF
076000970603     C*
076100970603     C                   ENDSR
076200970603     C*------------------------------------------------------------------------*
076300970603     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
076400970603     C*------------------------------------------------------------------------*
076500970603     C     CHKPAR        BEGSR
076600970603     C*
076700970603     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
076800970603IF  1C     VIDPCA        IFEQ      *BLANKS
076900970603     C     VIDPLO        ANDEQ     *BLANKS
077000970603     C                   MOVEL     'CAP'         WTIPRI            3
077100970603E   1C                   ENDIF
077200970603     C*
077300970603     C* CONTROLLA IL TIPO RICERCA
077400970603IF  1C     ERRL00        IFEQ      '0'
077500970603IF  2C     VIDPCA        IFNE      *BLANKS
077600970603     C                   MOVEL     'CAP'         WTIPRI
077700970627X   2C                   ELSE
077800970627IF  3C     VIDPLO        IFNE      *BLANKS
077900970627     C                   MOVEL     'LOC'         WTIPRI
078000970627X   3C                   ELSE
078100970627IF  4C     VIDPSI        IFNE      *BLANKS
078200970627     C                   MOVEL     'SIN'         WTIPRI
078300970627E   4C                   ENDIF
078400970627E   3C                   ENDIF
078500970627E   2C                   ENDIF
078600970627E   1C                   ENDIF
078700970603     C*
078800970603     C                   ENDSR
078900970603     C*------------------------------------------------------------------------*
079000970603     C* LANPGM - LANCIA UN PROGRAMMA
079100970603     C*------------------------------------------------------------------------*
079200970603     C     LANPGM        BEGSR
079300970603     C*
079400970603     C* IMPOSTA LA DS DI PROCEDURA
079500970603     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
079600970603     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
079700970603     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
079800970603     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
079900970603     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
080000970603     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
080100970603     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
080200970603     C                   MOVEL     DCAP          D00CAP                         *CAP
080300970603     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
080400970603     C                   MOVEL     DSIN          D00SIN                         *SINONIMO
080500970603     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
080600970603     C                   MOVEL     *BLANKS       D00TSP
080700970603     C                   Z-ADD     *ZEROS        D00TFP
080800970603     C                   MOVEL     *BLANKS       D00EPA
080900970603     C                   Z-ADD     *ZEROS        D00ETP
081000970603     C                   Z-ADD     *ZEROS        D00ETA
081100970603     C                   Z-ADD     *ZEROS        D00EDD
081200970603     C                   MOVEL     *BLANKS       D00SPT
081300970603     C                   Z-ADD     *ZEROS        D00SLP
081400970603     C                   Z-ADD     *ZEROS        D00SLA
081500970603     C                   MOVEL     *BLANKS       D00STS
081600970603     C                   Z-ADD     *ZEROS        D00SDD
081700970603     C*
081800970603     C                   CALL      WPGM
081900970612     C                   PARM                    KPJBA
082000970612     C                   PARM                    DSSI00
082100970603     C*
082200970603     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
082300970603IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
082400970603     C                   MOVEL     '1'           ERRL00                         *ERRORE
082500970603     C                   MOVEL     '1'           F03L00                         *FINE PGM
082600970603X   1C                   ELSE                                                   *NON PREMUTO F03
082700970603     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
082800970603E   1C                   ENDIF
082900970603     C*
083000970603     C                   ENDSR
083100970603     C*------------------------------------------------------------------------*
083200970603     C* CARTAB - CARICA LE TABELLE OCCORRENTI
083300970603     C*------------------------------------------------------------------------*
083400970603     C     CARTAB        BEGSR
083500970603     C*
083600970603     C                   ENDSR
083700970603     C*------------------------------------------------------------------------*
083800970603     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
083900970603     C*------------------------------------------------------------------------*
084000970603     C     CHKFOP        BEGSR
084100970626     C*
084200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
084300970626IF  1C     DEPOP0        IFEQ      'S01'
084400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
084500970626     C                   MOVEL     'S'           OPDATT
084600970626     C                   MOVEL     OPDRIG        OPD(1)
084700970626X   1C                   ELSE
084800970626     C* OPZIONI
084900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
085000970626     C                   MOVEL     'S'           OPDATT
085100970626     C                   MOVEL     OPDRIG        OPD(5)
085200970626     C* FUNZIONI
085300970626E   1C                   ENDIF
085400970626     C*
085500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
085600970626IF  1C     DEPOP0        IFEQ      'S02'
085700971003     C*---                 MOVELOPD,2     OPDRIG           *MODIFICA
085800971003     C*---                 MOVEL'S'       OPDATT
085900971003     C*---                 MOVELOPDRIG    OPD,2
086000970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
086100970626     C                   MOVEL     'S'           OPDATT
086200970626     C                   MOVEL     OPDRIG        OPD(4)
086300970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
086400970626     C                   MOVEL     'S'           OPDATT
086500970626     C                   MOVEL     OPDRIG        OPD(7)
086600080401     C                   IF        §UTECAP = 'C'
086700080401     C                   MOVEL     OPD(11)       OPDRIG                         *SPOSTAMENTO
086800080401     C                   MOVEL     'S'           OPDATT
086900080401     C                   MOVEL     OPDRIG        OPD(11)
087000080401     C                   ENDIF
087100970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
087200970626     C                   MOVEL     'S'           FUDATT
087300970626     C                   MOVEL     FUDRIG        FUD(6)
087400001106     C* INTRODUCO IL TEST SEGUENTE PER NON ALTERARE EVENTUALI RAGIONAMENTI PRE-ESISTENTI
087500001106     C     DEPOP1        IFEQ      'O09'
087600001106     C     DEPOP1        OREQ      'O08'
087700001106     C                   MOVEL     OPD(13)       OPDRIG                         *INSERIMENTO SIN. DA
087800001106     C                   MOVEL     'S'           OPDATT
087900001106     C                   MOVEL     OPDRIG        OPD(13)
088000001106     C                   ENDIF
088100001106E   1C                   ENDIF
088200970626     C*
088300970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
088400021115IF  1C***  DUTLPO        IFNE      'S'                                          *FILIALE
088500021115     C***                MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
088600021115     C***                MOVEL     'N'           OPDATT
088700021115     C***                MOVEL     OPDRIG        OPD(4)
088800021115     C***                MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
088900021115     C***                MOVEL     'N'           OPDATT
089000021115     C***                MOVEL     OPDRIG        OPD(7)
089100021115     C***                MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
089200021115     C***                MOVEL     'N'           FUDATT
089300021115     C***                MOVEL     FUDRIG        FUD(6)
089400021115E   1C***                ENDIF
089500970603     C*
089600970603     C* GESTIONE OPZIONI A VIDEO
089700970603     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
089800970603     C                   EXSR      OPZGES
089900970603     C*
090000970603     C* GESTIONE TASTI FUNZIONALI A VIDEO
090100970603     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
090200970603     C                   EXSR      FUNGES
090300970603     C*
090400970603     C                   ENDSR
090500020919     C*--------------------------------------------------------------------------------------------*
090600020919     C* REPERISCE I DATI UTENTE
090700020919     C*--------------------------------------------------------------------------------------------*
090800020919     C     REPDATIUTE    BEGSR
090900020919     C*
091000020919     C* INIZIALIZZA VARIABILI DI WRK
091100020919     C                   CLEAR                   TIBS34DS
091200020919     C                   CLEAR                   AZUTEDS
091300020919     C                   CLEAR                   DDATIUTE
091400020919     C*
091500020919     C     *DTAARA       DEFINE    §azute        azuteds
091600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
091700020919     C                   IN(E)     *DTAARA
091800020919if  1C                   IF        %Error
091900020919     c                   EVAL      I34Tla = 'L'
092000020919     C                   CALL      'TIBS34R'
092100020919     C                   PARM                    Tibs34Ds
092200020919     C                   IN        *DTAARA
092300020919e   1C                   ENDIF
092400020919     C*
092500020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
092600020919     C                   MOVEL(P)  RSUT          DSFIRS           20
092700080401     C*
092800080401     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
092900080401     C                   EVAL      DUTE01 = UTEFAF
093000020919     C*
093100020919     C                   ENDSR
093200080407     C*------------------------------------------------------------------------*
093300080407     C* FINSR - OPERAZIONI FINALI
093400080407     C*------------------------------------------------------------------------*
093500080407     C     FINSR         BEGSR
093600080407     C*
093700080407     C                   CLEAR                   DSSI95
093800080407     C                   MOVEL     'C'           I95TLA
093900080407     C                   CALL      'TISI95R'
094000080407     C                   PARM                    DSSI95
094100080407     C*
094200080407     C                   ENDSR
094300970603     C*------------------------------------------------------------------------*
094400970603     C* *INZSR - OPERAZIONI INIZIALI
094500970603     C*------------------------------------------------------------------------*
094600970603     C     *INZSR        BEGSR
094700970603     C*---
094800970603     C* RICEVIMENTO PARAMETRI
094900970603     C*---
095000970603     C     *ENTRY        PLIST
095100970612     C                   PARM                    KPJBA
095200970612     C                   PARM                    DSSI00
095300970603     C*
095400970603     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
095500970603     C     XMSGPA        PLIST
095600970603     C                   PARM                    DSMSMS
095700970603     C                   PARM                    DSMSPG
095800970603     C                   PARM                    DSMSKE
095900970603     C                   PARM                    DSMSER
096000970603     C*
096100970603     C* IMPOSTA LA DS -TASTI PREMUTI-
096200970603     C                   CALL      'XKEY'
096300970603     C                   PARM                    DSKEY
096400970603     C*---
096500970603     C* VARIABILI NON RIFERITE AL DATA BASE
096600970603     C*---
096700970603     C     *LIKE         DEFINE    SFLNBR        SFLPAG
096800970603     C     *LIKE         DEFINE    SFLPAG        WFLPAG
096900970603     C     *LIKE         DEFINE    SFLNBR        SFLEOF
097000970603     C     *LIKE         DEFINE    SFLNBR        SFLERR
097100970603     C     *LIKE         DEFINE    SFLNBR        SFLULT
097200970603     C     *LIKE         DEFINE    SFLNBR        WFLULT
097300970603     C     *LIKE         DEFINE    SFLNBR        MSGULT
097400970603     C     *LIKE         DEFINE    SFLNBR        SFLRIG
097500970603     C     *LIKE         DEFINE    FUDFUN        FUNKEY
097600970603     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
097700970603     C     *LIKE         DEFINE    D00OP1        DEPOP1
097800970603     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
097900970603     C     *LIKE         DEFINE    D00OP1        WOP1
098000970603     C     *LIKE         DEFINE    D00NAR        DNAR
098100970603     C     *LIKE         DEFINE    D00CAP        DCAP
098200970603     C     *LIKE         DEFINE    D00LOC        DLOC
098300970603     C     *LIKE         DEFINE    D00SIN        DSIN
098400970603     C*---
098500970603     C* VARIABILI DI CONTROLLO
098600970603     C*---
098700970603     C                   MOVEL     '0'           FINL00            1            *FINE PGM
098800970603     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
098900970603     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
099000970603     C                   MOVEL     '0'           ERRL00            1            *ERRORE
099100970603     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
099200970627     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
099300970603     C*
099400970603     C* IMPOSTA LA DS -INVIO MESSAGGI-
099500970603     C                   MOVEL     'TISI35R'     DSMSPG
099600970603     C                   MOVEL     'TISI35R'     MSGQUE
099700970603     C                   MOVEL     *BLANKS       MSGKEY
099800970603     C                   MOVEL     *BLANKS       DSMSMS
099900970603     C                   MOVEL     *BLANKS       DSMSKE
100000970603     C                   Z-ADD     *ZEROS        DSMSER
100100970603     C*---
100200970603     C* VARIABILI RIFERITE AL DATA BASE
100300970603     C*---
100400970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
100500970603     C     *LIKE         DEFINE    CPSNAR        KCSNAR
100600970603     C     *LIKE         DEFINE    CPSCAP        KCSCAP
100700970603     C     *LIKE         DEFINE    CPSLOC        KCSLOC
100800970627     C     *LIKE         DEFINE    CPSSIN        KCSSIN
100900970603     C     *LIKE         DEFINE    CPSVER        KCCVER                         *AZCPC00F
101000970603     C     *LIKE         DEFINE    CPSNAR        KCCNAR
101100970603     C     *LIKE         DEFINE    CPSCAP        KCCCAP
101200970603     C     *LIKE         DEFINE    CPSNAR        DEPNAR                         *DEPOSITI
101300970603     C     *LIKE         DEFINE    CPSCAP        DEPCAP
101400970603     C     *LIKE         DEFINE    CPSLOC        DEPLOC
101500970627     C     *LIKE         DEFINE    CPSSIN        DEPSIN
101600970603     C*---
101700970603     C* CHIAVI DI LETTURA
101800970603     C*---
101900970624     C* LETTURA AZCPS01L - PARZIALE
102000970603     C     KEYCPS        KLIST
102100970603     C                   KFLD                    KCSVER                         *VERSIONE
102200970603     C                   KFLD                    KCSNAR                         *NAZIONE
102300970603     C                   KFLD                    KCSCAP                         *CAP
102400970603     C                   KFLD                    KCSLOC                         *LOCALITA'
102500080407     C* LETTURA AZCPS01L - COMPLETA
102600080407     C     KEYCPS01C     KLIST
102700080407     C                   KFLD                    KCSVER                         *VERSIONE
102800080407     C                   KFLD                    KCSNAR                         *NAZIONE
102900080407     C                   KFLD                    KCSCAP                         *CAP
103000080407     C                   KFLD                    KCSLOC                         *LOCALITA'
103100080407     C                   KFLD                    KCSSIN                         *SINONIMO
103200970627     C* LETTURA AZCPS02L - PARZIALE
103300970627     C     KE2CPS        KLIST
103400970627     C                   KFLD                    KCSVER                         *VERSIONE
103500970627     C                   KFLD                    KCSNAR                         *NAZIONE
103600970627     C                   KFLD                    KCSSIN                         *SINONIMO
103700970624     C* LETTURA AZCPS03L - PARZIALE
103800970624     C     KE3CPS        KLIST
103900970624     C                   KFLD                    KCSVER                         *VERSIONE
104000970624     C                   KFLD                    KCSNAR                         *NAZIONE
104100970624     C                   KFLD                    KCSLOC                         *LOCALITA'
104200970603     C* LETTURA AZCPC01L - COMPLETA
104300970603     C     KEYCPC        KLIST
104400970603     C                   KFLD                    KCCVER                         *VERSIONE
104500970603     C                   KFLD                    KCCNAR                         *NAZIONE
104600970603     C                   KFLD                    KCCCAP                         *CAP
104700061201     C* LETTURA AZCPL01L - COMPLETA
104800061201     C     KEYCPL        KLIST
104900061201     C                   KFLD                    KCSVER                         *VERSIONE
105000061201     C                   KFLD                    KCSNAR                         *NAZIONE
105100061201     C                   KFLD                    KCSCAP                         *CAP
105200061201     C                   KFLD                    KCSLOC                         *LOCALITA'
105300970603     C*
105400970603     C* REPERIMENTO DATI UTENTE
105500020919     C                   EXSR      REPDATIUTE
105600970603     C*
105700970603     C* CARICA LE TABELLE OCCORRENTI
105800970603     C                   EXSR      CARTAB
105900970603     C*
106000970603     C* DEPOSITA LE OPZIONI DI ENTRATA
106100970603     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
106200970603     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
106300970603     C*
106400970603     C* CALCOLA LA DATA CORRENTE
106500970603     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
106600970603     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
106700970603     C                   Z-ADD     WN8           G08DAT
106800970603     C                   Z-ADD     *ZEROS        G08INV
106900970603     C                   MOVEL     '0'           G08ERR
107000970603     C                   CALL      'XSRDA8'
107100970603     C                   PARM                    WLBDA8
107200970603     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
107300970603     C*
107400970603     C* IMPOSTA I PARAMETRI DI INGRESSO
107500970603     C                   Z-ADD     D00DRI        G08INV
107600970603     C                   Z-ADD     *ZEROS        G08DAT
107700970603     C                   MOVEL     '3'           G08ERR
107800970603     C                   CALL      'XSRDA8'
107900970603     C                   PARM                    WLBDA8
108000970603     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
108100970603     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
108200970603     C                   MOVEL     D00CAP        VIDPCA                         *CAP
108300970603     C                   MOVEL     D00LOC        VIDPLO                         *LOCALITA
108400001031     C                   MOVEL     D00SIN        VIDPSI                         *SINONIMO
108500970603     C                   EXSR      CHKPAR
108600970603IF  1C     ERRL00        IFEQ      '1'
108700970603     C                   MOVEL     '1'           D00ERR
108800970603     C                   MOVEL     DSMSMS        D00MSG
108900970603X   1C                   ELSE
109000970603     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
109100970603     C                   MOVEL     VIDPCA        DEPCAP
109200970603     C                   MOVEL     VIDPLO        DEPLOC
109300001106     C                   MOVEL     VIDPSI        DEPSIN
109400970603E   1C                   ENDIF
109500970703     C*
109600970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
109700970703IF  1C     D00VER        IFEQ      *ZEROS
109800970707     C                   Z-ADD     D00DRI        P96DRI            8 0
109900970707     C                   Z-ADD     *ZEROS        P96VER            5 0
110000970707     C                   CALL      'TISI96R'
110100970707     C                   PARM                    P96DRI
110200970707     C                   PARM                    P96VER
110300970707     C                   Z-ADD     P96VER        D00VER
110400970703E   1C                   ENDIF
110500970603     C*
110600970603     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
110700970603     C                   EXSR      CHKFOP
110800970603     C*
110900970603     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
111000970603IF  1C     DEPOP0        IFEQ      'S01'
111100970603     C                   MOVEL     DEC(3)        VIDING
111200970603E   1C                   ENDIF
111300970603IF  1C     DEPOP0        IFEQ      'S02'
111400970603     C                   MOVEL     DEC(1)        VIDING
111500970603E   1C                   ENDIF
111600970603IF  1C     DEPOP0        IFEQ      'S05'
111700970603     C                   MOVEL     DEC(2)        VIDING
111800970603E   1C                   ENDIF
111900970603     C*
112000970603     C                   ENDSR
112100970603     C*------------------------------------------------------------------------*
112200970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112300970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112400970603     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
112500970603     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
112600970603     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
112700970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112800970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112900970603     C*------------------------------------------------------------------------*
113000970603     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
113100970603     C*------------------------------------------------------------------------*
113200970603     C     POSSFL        BEGSR
113300970603     C*
113400970603     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
113500970603     C                   EXSR      CLRSFL
113600970603     C*
113700970603     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
113800970603     C                   EXSR      SETFIL
113900970603     C*
114000970603     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
114100970603     C     SFLEOF        IFEQ      0
114200970603     C                   EXSR      CARSFL
114300970603     C                   END
114400970603     C*
114500970603     C                   ENDSR
114600970603     C*------------------------------------------------------------------------*
114700970603     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
114800970603     C*------------------------------------------------------------------------*
114900970603     C     ENTATT        BEGSR
115000970603     C*
115100970603     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
115200970603     C                   Z-ADD     0             OPZOK             1 0
115300970603     C*
115400970603     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
115500970603     C                   Z-ADD     0             SFLVAR            5 0
115600970603     C*
115700970603     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
115800970603     C                   Z-ADD     0             SFLERR
115900970603     C*
116000970603     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
116100970603     C                   Z-ADD     SFLULT        WFLULT
116200970603     C*
116300970603     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
116400970603     C                   READC     SI35D05                                98
116500970603     C     *IN98         DOWEQ     '0'
116600970603     C*
116700970603     C* ADD CONTATORE DI RIGHE SFL VARIATE
116800970603     C                   ADD       1             SFLVAR
116900970603     C*
117000970603     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
117100970603     C     OPZOK         IFEQ      0
117200970603     C*
117300970603     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
117400970603     C     OPZKEY        IFNE      *BLANKS
117500970603     C                   EXSR      OPZCON
117600970603     C                   END
117700970603     C*
117800970603     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
117900970603     C*            2. PULIZIA SCELTA IN SFL
118000970603     C     OPZOK         IFEQ      0
118100970603     C                   MOVEL     '0'           *IN41
118200970603     C                   MOVEL     *BLANKS       OPZKEY
118300970603     C                   ELSE
118400970603     C*
118500970603     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
118600970603     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
118700970603     C                   MOVEL     '1'           *IN41
118800970603     C                   Z-ADD     SFLULT        SFLERR
118900970603     C                   END
119000970603     C                   ELSE
119100970603     C*
119200970603     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
119300970603     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
119400970603     C                   MOVEL     '1'           *IN41
119500970603     C                   END                                                    OPZOK=0
119600970603     C*
119700970603     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
119800970603     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
119900970603     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
120000970603     C     OPZOK         IFNE      0
120100970603     C                   Z-ADD     SFLERR        SFLNBR
120200970603     C                   ELSE
120300970603     C                   Z-ADD     SFLULT        SFLNBR
120400970603     C                   END
120500970603     C*
120600970603     C* AGGIORNO RECORD SFL
120700970603     C                   UPDATE    SI35D05
120800970603     C*
120900970603     C                   READC     SI35D05                                98
121000970603     C                   END                                                    DO WHILE
121100970603     C*
121200970603     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
121300970603     C                   Z-ADD     WFLULT        SFLULT
121400970603     C*
121500970603     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
121600970603     C     SFLVAR        IFEQ      0
121700970603     C*---                 MOVEL'1'       FINL00
121800970603     C                   END
121900970603     C*
122000970603     C                   ENDSR
122100970603     C*------------------------------------------------------------------------*
122200970603     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
122300970603     C*------------------------------------------------------------------------*
122400970603     C     CLRSFL        BEGSR
122500970603     C*
122600970603     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
122700970603     C* (0=NON FINE FILE  1=FINE FILE)
122800970603     C                   Z-ADD     0             SFLEOF
122900970603     C*
123000970603     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
123100970603     C                   Z-ADD     0             SFLULT
123200970603     C*
123300970603     C* ACCENSIONE INDICATORE PER CLEAR SFL
123400970603     C                   MOVEL     '1'           *IN40
123500970603     C                   WRITE     SI35D06
123600970603     C                   MOVEL     '0'           *IN40
123700970603     C*
123800970603     C                   ENDSR
123900970603     C*------------------------------------------------------------------------*
124000970603     C* CARSFL - CARICAMENTO SFL
124100970603     C*------------------------------------------------------------------------*
124200970603     C     CARSFL        BEGSR
124300970603     C*
124400970603     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
124500970603     C     SFLULT        IFEQ      0
124600970603     C                   EXSR      SETFIL
124700970603     C                   END
124800970603     C*
124900970603     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
125000970603     C                   Z-ADD     SFLULT        WFLULT
125100970603     C*
125200970603     C* CICLO:
125300970603     C*    -CARICO NUMERO DI RIGHE=SFLPAG
125400970603     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
125500970603     C                   Z-ADD     1             SFLRIG
125600970603     C     SFLRIG        DOWLE     SFLPAG
125700970603     C     SFLEOF        ANDEQ     0
125800970603     C*
125900970603     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
126000970603     C                   MOVEL     *BLANKS       OPZKEY
126100970603     C*
126200970603     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
126300970603     C                   EXSR      IRESFL
126400970603     C*
126500970603     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
126600970603     C                   ADD       1             SFLULT
126700970603     C*
126800970603     C* SCRITTURA RECORD SFL
126900970603     C                   WRITE     SI35D05
127000970603     C                   ADD       1             SFLRIG
127100970603     C*
127200970603     C* LETTURA FILE DA CARICARE
127300970603     C                   EXSR      LETFIL
127400970603     C                   END                                                    DO WHILE
127500970603     C*
127600970603     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
127700970603     C     WFLULT        IFNE      SFLULT
127800970603     C                   Z-ADD     WFLULT        SFLNBR
127900970603     C                   ADD       1             SFLNBR
128000970603     C                   END
128100970603     C*
128200970603     C                   ENDSR
128300970603     C*------------------------------------------------------------------------*
128400970603     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
128500970603     C*------------------------------------------------------------------------*
128600970603     C     OUTSFL        BEGSR
128700970603     C*
128800970603     C* WRITE RECORD TESTATA VIDEO
128900970603     C                   WRITE     SI35D01
129000970603     C*
129100970603     C* VISUALIZZAZIONE MESSAGGIO
129200970603     C                   EXSR      SNDMSG
129300970603     C*
129400970603     C* WRITE RECORD OPZIONI VIDEO
129500970603     C                   WRITE     SI35D02
129600970603     C*
129700970603     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
129800970603     C                   WRITE     SI35D03
129900970603     C*
130000970603     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
130100970603     C                   WRITE     SI35D04
130200970603     C*
130300970603     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
130400970603     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
130500970603     C     SFLEOF        IFEQ      0
130600970603     C                   MOVEL     '0'           *IN42
130700970603     C                   ELSE
130800970603     C*
130900970603     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
131000970603     C                   MOVEL     '1'           *IN42
131100970603     C                   END
131200970603     C*
131300970603     C* WRITE RECORD TASTI FUNZIONALI
131400970603     C                   WRITE     SI35D09
131500970603     C*
131600970603     C* CASO DI SFL VUOTO
131700970603     C     SFLULT        IFLE      0
131800970603     C*
131900970603     C*    - WRITE RECORD VUOTO
132000970603     C                   WRITE     SI35D0A
132100970603     C                   READ      SI35D0A                                99
132200970603     C*
132300970603     C                   ELSE
132400970603     C*
132500970603     C* CASO DI SFL CON RECORDS
132600970603     C*
132700970603     C*    - WRITE RECORD CONTROLLO SFL
132800970603     C                   WRITE     SI35D06
132900970603     C                   READ      SI35D06                                99
133000970603     C                   END
133100970603     C*
133200970603     C* READ RECORD PER POSIZIONAMEMTO VIDEO
133300970603     C                   READ      SI35D03                                99
133400970603     C*
133500970603     C                   ENDSR
133600970603     C*------------------------------------------------------------------------*
133700970603     C* SNDMSG - MANDA MESSAGGIO
133800970603     C*------------------------------------------------------------------------*
133900970603     C     SNDMSG        BEGSR
134000970603     C*
134100970603     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
134200970603     C                   MOVEL     '1'           *IN40
134300970603     C                   WRITE     SI35DMC
134400970603     C                   MOVEL     '0'           *IN40
134500970603     C*
134600970603     C* VISUALIZZAZIONE MESSAGGIO
134700970603     C                   Z-ADD     0             MSGULT
134800970603     C                   CALL      'XMSG'        XMSGPA
134900970603     C     DSMSER        IFEQ      0
135000970603     C                   MOVEL     DSMSKE        MSGKEY
135100970603     C                   END
135200970603     C                   Z-ADD     1             MSGULT
135300970603     C                   WRITE     SI35DMS                                      SFL MESS
135400970603     C                   WRITE     SI35DMC                                      CONTR MESS
135500970603     C*
135600970603     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
135700970603     C                   MOVEL     *BLANKS       DSMSMS
135800970603     C*
135900970603     C                   ENDSR
136000970603     C*------------------------------------------------------------------------*
136100970603     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
136200970603     C*------------------------------------------------------------------------*
136300970603     C     FUNCON        BEGSR
136400970603     C*
136500970603     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
136600970603     C                   EXSR      FUNVAR
136700970603     C*
136800970603     C* PULISCO CAMPI DI LAVORO
136900970603     C                   Z-ADD     0             I                 5 0
137000970603     C                   Z-ADD     0             W                 5 0
137100970603     C*
137200970603     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
137300970603     C     FUNKEY        IFNE      *BLANKS
137400970603     C     1             DO        24            I
137500970603     C                   MOVEL     FUD(I)        FUDRIG
137600970603     C*
137700970603     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
137800970603     C     FUNKEY        IFEQ      FUDFUN
137900970603     C     FUDATT        ANDEQ     'S'
138000970603     C                   Z-ADD     I             W
138100970603     C                   END
138200970603     C                   END                                                    DO WHILE
138300970603     C*
138400970603     C* FUNZIONE NON ATTIVA (ERRORE)
138500970603     C     W             IFEQ      0
138600970603     C*
138700970603     C* EMISSIONE MESSAGGIO DI ERRORE
138800970603     C                   MOVEL     MSG(2)        DSMSMS
138900970603     C                   END                                                    W=0
139000970603     C                   END                                                    FUNKEY NE *BLANKS
139100970603     C*
139200970603     C* FUNZIONE ATTIVA (PROCEDI ...)
139300970603     C     W             IFNE      0
139400970603     C     FUNKEY        OREQ      *BLANKS
139500970603     C*
139600970603     C     FUNKEY        IFEQ      *BLANKS
139700970603     C                   EXSR      ENTGES
139800970603     C                   END
139900970603     C     FUNKEY        IFEQ      '03'
140000970603     C                   EXSR      F03GES
140100970603     C                   END
140200970603     C     FUNKEY        IFEQ      '05'
140300970603     C                   EXSR      F05GES
140400970603     C                   END
140500970603     C     FUNKEY        IFEQ      '06'
140600970603     C                   EXSR      F06GES
140700970603     C                   END
140800970603     C     FUNKEY        IFEQ      '12'
140900970603     C                   EXSR      F12GES
141000970603     C                   END
141100970603     C     FUNKEY        IFEQ      '23'
141200970603     C                   EXSR      F23GES
141300970603     C                   END
141400970603     C     FUNKEY        IFEQ      '24'
141500970603     C                   EXSR      F24GES
141600970603     C                   END
141700970603     C     FUNKEY        IFEQ      'RU'
141800970603     C                   EXSR      RUPGES
141900970603     C                   END
142000970603     C*
142100970603     C                   END
142200970603     C*
142300970603     C                   ENDSR
142400970603     C*------------------------------------------------------------------------*
142500970603     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
142600970603     C*------------------------------------------------------------------------*
142700970603     C     FUNVAR        BEGSR
142800970603     C*
142900970603     C* FUNKEY=*BLANKS SIGNIFICA ENTER
143000970603     C     KEY           IFEQ      ENTER
143100970603     C                   MOVEL     *BLANKS       FUNKEY
143200970603     C                   END
143300970603     C     KEY           IFEQ      F1
143400970603     C                   MOVEL     '01'          FUNKEY
143500970603     C                   END
143600970603     C     KEY           IFEQ      F2
143700970603     C                   MOVEL     '02'          FUNKEY
143800970603     C                   END
143900970603     C     KEY           IFEQ      F3
144000970603     C                   MOVEL     '03'          FUNKEY
144100970603     C                   END
144200970603     C     KEY           IFEQ      F4
144300970603     C                   MOVEL     '04'          FUNKEY
144400970603     C                   END
144500970603     C     KEY           IFEQ      F5
144600970603     C                   MOVEL     '05'          FUNKEY
144700970603     C                   END
144800970603     C     KEY           IFEQ      F6
144900970603     C                   MOVEL     '06'          FUNKEY
145000970603     C                   END
145100970603     C     KEY           IFEQ      F7
145200970603     C                   MOVEL     '07'          FUNKEY
145300970603     C                   END
145400970603     C     KEY           IFEQ      F8
145500970603     C                   MOVEL     '08'          FUNKEY
145600970603     C                   END
145700970603     C     KEY           IFEQ      F9
145800970603     C                   MOVEL     '09'          FUNKEY
145900970603     C                   END
146000970603     C     KEY           IFEQ      F10
146100970603     C                   MOVEL     '10'          FUNKEY
146200970603     C                   END
146300970603     C     KEY           IFEQ      F11
146400970603     C                   MOVEL     '11'          FUNKEY
146500970603     C                   END
146600970603     C     KEY           IFEQ      F12
146700970603     C                   MOVEL     '12'          FUNKEY
146800970603     C                   END
146900970603     C     KEY           IFEQ      F13
147000970603     C                   MOVEL     '13'          FUNKEY
147100970603     C                   END
147200970603     C     KEY           IFEQ      F14
147300970603     C                   MOVEL     '14'          FUNKEY
147400970603     C                   END
147500970603     C     KEY           IFEQ      F15
147600970603     C                   MOVEL     '15'          FUNKEY
147700970603     C                   END
147800970603     C     KEY           IFEQ      F16
147900970603     C                   MOVEL     '16'          FUNKEY
148000970603     C                   END
148100970603     C     KEY           IFEQ      F17
148200970603     C                   MOVEL     '17'          FUNKEY
148300970603     C                   END
148400970603     C     KEY           IFEQ      F18
148500970603     C                   MOVEL     '18'          FUNKEY
148600970603     C                   END
148700970603     C     KEY           IFEQ      F19
148800970603     C                   MOVEL     '19'          FUNKEY
148900970603     C                   END
149000970603     C     KEY           IFEQ      F20
149100970603     C                   MOVEL     '20'          FUNKEY
149200970603     C                   END
149300970603     C     KEY           IFEQ      F21
149400970603     C                   MOVEL     '21'          FUNKEY
149500970603     C                   END
149600970603     C     KEY           IFEQ      F22
149700970603     C                   MOVEL     '22'          FUNKEY
149800970603     C                   END
149900970603     C     KEY           IFEQ      F23
150000970603     C                   MOVEL     '23'          FUNKEY
150100970603     C                   END
150200970603     C     KEY           IFEQ      F24
150300970603     C                   MOVEL     '24'          FUNKEY
150400970603     C                   END
150500970603     C     KEY           IFEQ      ROLLUP
150600970603     C                   MOVEL     'RU'          FUNKEY
150700970603     C                   END
150800970603     C     KEY           IFEQ      HELP
150900970603     C                   MOVEL     'HP'          FUNKEY
151000970603     C                   END
151100970603     C*
151200970603     C                   ENDSR
151300970603     C*------------------------------------------------------------------------*
151400970603     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
151500970603     C*------------------------------------------------------------------------*
151600970603     C     FUNGES        BEGSR
151700970603     C*
151800970603     C* PULISCO CAMPI DI LAVORO
151900970603     C                   Z-ADD     0             W                 5 0
152000970603     C*
152100970603     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
152200970603     C* E CARICO TABELLA FUNZIONI ATTIVE
152300970603     C     1             DO        24            I
152400970603     C                   MOVEL     FUD(I)        FUDRIG
152500970603     C*
152600970603     C* PULIZIA FUNZIONI PRECEDENTI
152700970603     C                   MOVEL     *BLANKS       FUV(I)
152800970603     C*
152900970603     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
153000970603     C* (SALTA F23 E F24)
153100970603     C     FUDATT        IFEQ      'S'
153200970603     C     FUDFUN        ANDNE     '23'
153300970603     C     FUDFUN        ANDNE     '24'
153400970603     C                   ADD       1             W
153500970603     C                   MOVEL     FUDDES        FUV(W)
153600970603     C                   END
153700970603     C                   END                                                    DO WHILE
153800970603     C*
153900970603     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
154000970603     C* ALTRIMENTI DISATTIVO F24
154100970603     C     1             DO        24            I
154200970603     C                   MOVEL     FUD(I)        FUDRIG
154300970603     C*
154400970603     C     FUDFUN        IFEQ      '24'
154500970603     C     W             IFGT      6
154600970603     C                   MOVEL     'S'           FUDATT
154700970603     C                   ELSE
154800970603     C                   MOVEL     *BLANKS       FUDATT
154900970603     C                   END                                                    W GT 8
155000970603     C                   MOVEL     FUDRIG        FUD(I)
155100970603     C                   END
155200970603     C                   END                                                    DO WHILE
155300970603     C*
155400970603     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
155500970603     C     W             DIV       6             I
155600970603     C                   MVR                     Z
155700970603     C*
155800970603     C     Z             IFNE      0
155900970603     C     I             ADD       1             Z
156000970603     C                   ELSE
156100970603     C                   Z-ADD     I             Z
156200970603     C                   END
156300970603     C*
156400970603     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
156500970603     C                   ADD       1             FUNGRU
156600970603     C     FUNGRU        IFGT      Z
156700970603     C     FUNGRU        ORLT      1
156800970603     C                   Z-ADD     1             FUNGRU
156900970603     C                   END
157000970603     C*
157100970603     C* PRIMO GRUPPO DI 6 FUNZIONI
157200970603     C     FUNGRU        IFEQ      1
157300970603     C                   MOVEL     FUV(1)        FUN001
157400970603     C                   MOVEL     FUV(2)        FUN002
157500970603     C                   MOVEL     FUV(3)        FUN003
157600970603     C                   MOVEL     FUV(4)        FUN004
157700970603     C                   MOVEL     FUV(5)        FUN005
157800970603     C                   MOVEL     FUV(6)        FUN006
157900970603     C                   END
158000970603     C*
158100970603     C* SECONDO GRUPPO DI 6 FUNZIONI
158200970603     C     FUNGRU        IFEQ      2
158300970603     C                   MOVEL     FUV(7)        FUN001
158400970603     C                   MOVEL     FUV(8)        FUN002
158500970603     C                   MOVEL     FUV(9)        FUN003
158600970603     C                   MOVEL     FUV(10)       FUN004
158700970603     C                   MOVEL     FUV(11)       FUN005
158800970603     C                   MOVEL     FUV(12)       FUN006
158900970603     C                   END
159000970603     C*
159100970603     C* TERZO GRUPPO DI 6 FUNZIONI
159200970603     C     FUNGRU        IFEQ      3
159300970603     C                   MOVEL     FUV(13)       FUN001
159400970603     C                   MOVEL     FUV(14)       FUN002
159500970603     C                   MOVEL     FUV(15)       FUN003
159600970603     C                   MOVEL     FUV(16)       FUN004
159700970603     C                   MOVEL     FUV(17)       FUN005
159800970603     C                   MOVEL     FUV(18)       FUN006
159900970603     C                   END
160000970603     C*
160100970603     C* QUARTO GRUPPO DI 6 FUNZIONI
160200970603     C     FUNGRU        IFEQ      4
160300970603     C                   MOVEL     FUV(19)       FUN001
160400970603     C                   MOVEL     FUV(20)       FUN002
160500970603     C                   MOVEL     FUV(21)       FUN003
160600970603     C                   MOVEL     FUV(22)       FUN004
160700970603     C                   MOVEL     *BLANKS       FUN005
160800970603     C                   MOVEL     *BLANKS       FUN006
160900970603     C                   END
161000970603     C*
161100970603     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
161200970603     C                   MOVEL     *BLANKS       FUN007
161300970603     C                   MOVEL     *BLANKS       FUN008
161400970603     C*
161500970603     C     1             DO        24            I
161600970603     C                   MOVEL     FUD(I)        FUDRIG
161700970603     C     FUDFUN        IFEQ      '23'
161800970603     C     FUDATT        ANDEQ     'S'
161900970603     C                   MOVEL     FUDDES        FUN007
162000970603     C                   END
162100970603     C     FUDFUN        IFEQ      '24'
162200970603     C     FUDATT        ANDEQ     'S'
162300970603     C                   MOVEL     FUDDES        FUN008
162400970603     C                   END
162500970603     C                   END                                                    DO
162600970603     C*
162700970603     C                   ENDSR
162800970603     C*------------------------------------------------------------------------*
162900970603     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
163000970603     C*------------------------------------------------------------------------*
163100970603     C     OPZGES        BEGSR
163200970603     C*
163300970603     C* PULISCO CAMPI DI LAVORO
163400970603     C                   Z-ADD     0             W                 5 0
163500970603     C*
163600970603     C* RICERCO QUANTE OPZIONI SONO ATTIVE
163700970603     C* E CARICO TABELLA OPZIONI ATTIVE
163800970603     C     1             DO        24            I
163900970603     C                   MOVEL     OPD(I)        OPDRIG
164000970603     C*
164100970603     C* PULIZIA OPZIONI PRECEDENTI
164200970603     C                   MOVEL     *BLANKS       OPV(I)
164300970603     C*
164400970603     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
164500970603     C     OPDATT        IFEQ      'S'
164600970603     C                   ADD       1             W
164700970603     C                   MOVEL     OPDDES        OPV(W)
164800970603     C                   END
164900970603     C                   END                                                    DO WHILE
165000970603     C*
165100970603     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
165200970603     C* ALTRIMENTI DISATTIVO F23
165300970603     C     1             DO        24            I
165400970603     C                   MOVEL     FUD(I)        FUDRIG
165500970603     C*
165600970603     C     FUDFUN        IFEQ      '23'
165700970603     C     W             IFGT      8
165800970603     C                   MOVEL     'S'           FUDATT
165900970603     C                   ELSE
166000970603     C                   MOVEL     *BLANKS       FUDATT
166100970603     C                   END                                                    W GT 8
166200970603     C                   MOVEL     FUDRIG        FUD(I)
166300970603     C                   END
166400970603     C                   END                                                    DO WHILE
166500970603     C*
166600970603     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
166700970603     C     W             DIV       8             I
166800970603     C                   MVR                     Z                 5 0
166900970603     C*
167000970603     C     Z             IFNE      0
167100970603     C     I             ADD       1             Z
167200970603     C                   ELSE
167300970603     C                   Z-ADD     I             Z
167400970603     C                   END
167500970603     C*
167600970603     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
167700970603     C                   ADD       1             OPZGRU
167800970603     C     OPZGRU        IFGT      Z
167900970603     C     OPZGRU        ORLT      1
168000970603     C                   Z-ADD     1             OPZGRU
168100970603     C                   END
168200970603     C*
168300970603     C* PRIMO GRUPPO DI 8 OPZIONI
168400970603     C     OPZGRU        IFEQ      1
168500970603     C                   MOVEL     OPV(1)        OPZ001
168600970603     C                   MOVEL     OPV(2)        OPZ002
168700970603     C                   MOVEL     OPV(3)        OPZ003
168800970603     C                   MOVEL     OPV(4)        OPZ004
168900970603     C                   MOVEL     OPV(5)        OPZ005
169000970603     C                   MOVEL     OPV(6)        OPZ006
169100970603     C                   MOVEL     OPV(7)        OPZ007
169200970603     C                   MOVEL     OPV(8)        OPZ008
169300970603     C                   END
169400970603     C*
169500970603     C* SECONDO GRUPPO DI 8 OPZIONI
169600970603     C     OPZGRU        IFEQ      2
169700970603     C                   MOVEL     OPV(9)        OPZ001
169800970603     C                   MOVEL     OPV(10)       OPZ002
169900970603     C                   MOVEL     OPV(11)       OPZ003
170000970603     C                   MOVEL     OPV(12)       OPZ004
170100970603     C                   MOVEL     OPV(13)       OPZ005
170200970603     C                   MOVEL     OPV(14)       OPZ006
170300970603     C                   MOVEL     OPV(15)       OPZ007
170400970603     C                   MOVEL     OPV(16)       OPZ008
170500970603     C                   END
170600970603     C*
170700970603     C* TERZO GRUPPO DI 8 OPZIONI
170800970603     C     OPZGRU        IFEQ      3
170900970603     C                   MOVEL     OPV(17)       OPZ001
171000970603     C                   MOVEL     OPV(18)       OPZ002
171100970603     C                   MOVEL     OPV(19)       OPZ003
171200970603     C                   MOVEL     OPV(20)       OPZ004
171300970603     C                   MOVEL     OPV(21)       OPZ005
171400970603     C                   MOVEL     OPV(22)       OPZ006
171500970603     C                   MOVEL     OPV(23)       OPZ007
171600970603     C                   MOVEL     OPV(24)       OPZ008
171700970603     C                   END
171800970603     C*
171900970603     C                   ENDSR
172000970603     C*------------------------------------------------------------------------*
172100970603     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
172200970603     C*------------------------------------------------------------------------*
172300970603     C     OPZCON        BEGSR
172400970603     C*
172500970603     C* PULISCO CAMPI DI LAVORO
172600970603     C                   Z-ADD     0             I                 5 0
172700970603     C                   Z-ADD     0             W                 5 0
172800970603     C*
172900970603     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
173000970603     C     1             DO        24            I
173100970603     C                   MOVEL     OPD(I)        OPDRIG
173200970603     C*
173300970603     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
173400970603     C     OPZKEY        IFEQ      OPDOPZ
173500970603     C     OPZKEY        OREQ      OPDOP1
173600970603     C*
173700970603     C     OPDATT        IFEQ      'S'
173800970603     C                   MOVEL     OPDOPZ        OPZKEY
173900970603     C                   Z-ADD     I             W
174000970603     C                   END
174100970603     C                   END
174200970603     C*
174300970603     C                   END                                                    DO WHILE
174400970603     C*
174500970603     C* OPZIONE NON ATTIVA (ERRORE)
174600970603     C     W             IFEQ      0
174700970603     C*
174800970603     C* OPZIONE NON OK
174900970603     C                   Z-ADD     1             OPZOK
175000970603     C*
175100970603     C* EMISSIONE MESSAGGIO DI ERRORE
175200970603     C                   MOVEL     MSG(1)        DSMSMS
175300970603     C                   ELSE
175400970603     C*
175500970603     C* OPZIONE ATTIVA (PROCEDI ...)
175600970603     C*
175700970603     C     OPZKEY        IFEQ      ' 1'
175800970603     C                   EXSR      O01GES
175900970603     C                   END
176000970603     C     OPZKEY        IFEQ      ' 2'
176100970603     C                   EXSR      O02GES
176200970603     C                   END
176300970603     C     OPZKEY        IFEQ      ' 4'
176400970603     C                   EXSR      O04GES
176500970603     C                   END
176600970603     C     OPZKEY        IFEQ      ' 5'
176700970603     C                   EXSR      O05GES
176800970603     C                   END
176900970603     C     OPZKEY        IFEQ      ' 7'
177000970603     C                   EXSR      O07GES
177100970603     C                   END
177200080401     C     OPZKEY        IFEQ      '11'
177300080401     C                   EXSR      O11GES
177400080401     C                   END
177500001107     C     OPZKEY        IFEQ      '13'
177600001106     C                   EXSR      O13GES
177700001106     C                   END
177800970603     C                   END                                                    OPZ.NON ATTIVA
177900970603     C*
178000970603     C                   ENDSR
178100970603     C*------------------------------------------------------------------------*
178200970603     C* NOTE COSTRUZIONE SCHIERE
178300970603     C*------------------------------------------------------------------------*
178400970603     C*
178500970603     C* -MSG-MESSAGGI
178600970603     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
178700970603     C* (NON TOCCARE MSG,1 E MSG,2)
178800970603     C*
178900970603     C* -CMD-COMANDI
179000970603     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
179100970603     C*
179200970603     C* -OPD-OPZIONI
179300970603     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
179400970603     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
179500970603     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
179600970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
179700970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
179800970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
179900970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
180000970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
180100970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
180200970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
180300970603     C*
180400970603     C* -FUD-FUNZIONI
180500970603     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
180600970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
180700970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
180800970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
180900970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
181000970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
181100970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
181200970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
181300970603     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
181400970603     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
181500970603     O*------------------------------------------------------------------------*
181600970603** DEC - DECODIFICHE
181700970603    GESTIONE                                                     1
181800970603INTERROGAZIONE                                                   2
181900970603     SCELTA                                                      3
1820009706034
1821009706035
182200970603** MSG - MESSAGGI
182300970603TISI35R- Opzione non gestita                                                   1
182400970603TISI35R- Tasto funzionale non gestito                                          2
182500970603TISI35R- Non abilitato alla gestione di questo CAP                             3
182600970603TISI35R- Record annullato. Consentito solo il ripristino                       4
182700970603TISI35R- Record non annullato. Impossibile ripristinare                        5
182800970603TISI35R- Accettato solo un parametro di inizio lista                           6
182900080407TISI35R- Impossibile inserire località di CAP obsoleti                         7
183000080407TISI35R- Cap inesistente o non valido                                          8
183100080407TISI35R- Località inesistente o non valida                                     9
183200970603TISI35R- 10                                                                   10
183300970603TISI35R- 11                                                                   11
183400970603TISI35R- 12                                                                   12
183500970603TISI35R- 13                                                                   13
183600970603TISI35R- 14                                                                   14
183700970603TISI35R- 15                                                                   15
183800970603TISI35R- 16                                                                   16
183900970603TISI35R- 17                                                                   17
184000970603TISI35R- 18                                                                   18
184100080407TISI35R- Errori in entrata spostamento. Non trovato record da cui copiare     19
184200080407TISI35R- La località e gli archivi correlati sono stati duplicati             20
184300970603** OPD - OPZIONI
184400970603 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
184500970603 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
184600970603                   -3 -     (S/N)=N                        opz. 03¬
184700970603 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
184800970603 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
184900970603                   -  -     (S/N)=N                        opz. 06¬
185000970603 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
185100001106                   -  -     (S/N)=N                        opz. 08¬
185200970603                   -  -     (S/N)=N                        opz. 09¬
185300970603                   -  -     (S/N)=N                        opz. 10¬
18540008033111=Sposta Sinonimo -11-     (S/N)=N                        opz. 11¬
185500970603                   -  -     (S/N)=N                        opz. 12¬
18560000110613=Ins. Sinonimo   -13-     (S/N)=N                        opz. 13¬
185700970603                   -  -     (S/N)=N                        opz. 14¬
185800970603                   -  -     (S/N)=N                        opz. 15¬
185900970603                   -  -     (S/N)=N                        opz. 16¬
186000970603                   -  -     (S/N)=N                        opz. 17¬
186100970603                   -  -     (S/N)=N                        opz. 18¬
186200970603                   -  -     (S/N)=N                        opz. 19¬
186300970603                   -  -     (S/N)=N                        opz. 20¬
186400970603                   -  -     (S/N)=N                        opz. 21¬
186500970603                   -  -     (S/N)=N                        opz. 22¬
186600970603                   -  -     (S/N)=N                        opz. 23¬
186700970603                   -  -     (S/N)=N                        opz. 24¬
186800970603** FUD - FUNZIONI
186900970603                   -        (S/N)=N                        funz.01¬
187000970603                   -        (S/N)=N                        funz.02¬
187100970603F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
187200970603                   -        (S/N)=N                        funz.04¬
187300970603F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
187400970603F06=Inserimento    -        (S/N)=N                        funz.06¬
187500970603                   -        (S/N)=N                        funz.07¬
187600970603                   -        (S/N)=N                        funz.08¬
187700970603                   -        (S/N)=N                        funz.09¬
187800970603                   -        (S/N)=N                        funz.10¬
187900970603                   -        (S/N)=N                        funz.11¬
188000970603F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
188100970603                   -        (S/N)=N                        funz.13¬
188200970603                   -        (S/N)=N                        funz.14¬
188300970603                   -        (S/N)=N                        funz.15¬
188400970603                   -        (S/N)=N                        funz.16¬
188500970603                   -        (S/N)=N                        funz.17¬
188600970603                   -        (S/N)=N                        funz.18¬
188700970603 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
188800970603 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
188900970603                   -        (S/N)=N                        funz.21¬
189000970603                   -        (S/N)=N                        funz.22¬
189100970603F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
189200970603F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
