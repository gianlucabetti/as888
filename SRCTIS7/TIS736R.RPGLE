000100970526     H*------------------------------------------------------------------------*
000200120622     H* VAS - Gestione invio cappario
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120528     FTIS736D   CF   E             WORKSTN INFDS(DEVDS1)
000900120528     F                                     SFILE(S736D05:SFLULT)
001000120528     F                                     SFILE(S736DMS:MSGULT)
001100970526     F*------------
001200120528     FTIVIC01I  UF   E           K DISK    rename(TIVIC000:TIVIC001)
001300120528     FTIVIC02I  IF   E           K DISK    rename(TIVIC000:TIVIC002)
001400120528     FTIVIC03I  IF   E           K DISK    rename(TIVIC000:TIVIC003)
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100090619     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
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
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D*------------
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D*------------
006400970526     D FUDDS           DS
006500970526     D  FUDDE1                 1      1
006600970526     D  FUDFUN                 2      3
006700970526     D  FUDDE2                 4     19
006800970526     D  FUDFI0                20     34
006900970526     D  FUDATT                35     35
007000970526     D  FUDFI1                36     80
007100970526     D  FUDDES                 1     19
007200970526     D  FUDRIG                 1     80
007300970526     D*------------
007400970526     D* DS GESTIONE TASTI FUNZIONALI
007500970526     D*------------
007600970526     D DSKEY         E DS
007700970526     D DEVDS1          DS
007800970526     D  KEY                  369    369
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020916     D* DS REPERIMENTO DATI UTENTE
008500970526     D*------------
008600020916     D TIBS34DS      E DS                                                       *Profili utente
008700020916     D DDATIUTE      E DS                                                       *Dati utente
008800020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008900001020     D*------------
009000001020     D* DS - REPERIMENTO TABELLE
009100001020     D*------------
009200991108     D TIBS02DS      E DS
009300991122     D*------------------
009400991122     D* DS REPERIMENTO DATI UTENTE
009500991122     D*-------------------
009600991122     DBS69DS         E DS                  extname(TIBS69DS)
009700991122     DACODS          E DS                  extname(CNACO00F)
009800991122     DINDDS          E DS                  extname(CNIND00F)
009900991122     DCLPDS          E DS                  extname(CNCLP00F)
010000991122     DCLSDS          E DS                  extname(FNCLS00F)
010100060731     D*------------------
010200060731     D* DS "XSRDA8" - CONTROLLA DATA (8)
010300060731     D*------------------
010400060731     D WLBDA8          DS                  INZ
010500060731     D  G08DAT                 1      8  0
010600060731     D  G08INV                 9     16  0
010700060731     D  G08ERR                17     17
010800060731     D  G08TGI                18     22  0
010900970527     D*------------------
011000970526     D* DS DI PROCEDURA
011100001020     D*------------
011200120528     D tiS736        e ds                  extname(tiS736ds)
011300970526     D*------------
011400970526     D* ARCHITETTURA
011500970526     D*------------
011600970526     D KPJBA         E DS
011700120103     D*------------
011800970526     C*------------------------------------------------------------------------*
011900970526     C* MAIN LINES
012000970526     C*------------------------------------------------------------------------*
012100111229     C*
012200120702     C* carico TIVIC da tab.3C
012300120702     C                   EXSR      WrtVIC3C
012400120702     C*
012500120702     C* inizializzo selezioni con dati in DS
012600120702     C                   EXSR      InzD03
012700970526     C*
012800970526     C* SE NON CI SONO ERRORI, PROSEGUE
012900001020     C     ERRL00        IFNE      '1'
013000970526     C*
013100970526     C* CARICA IL SFILE
013200970526     C                   EXSR      POSSFL
013300970526     C*
013400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013500001020     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013600000703     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013800970526     C*
013900970526     C* REIMPOSTA ERRORE DI PROCERURA
014000970526     C                   MOVEL     '0'           ERRL00
014100970526     C*
014200970526     C* EMETTE IL SFILE
014300970526     C                   EXSR      OUTSFL
014400970526     C*
014500970526     C* GESTIONE TASTI FUNZIONALI
014600970526     C                   EXSR      FUNCON
014700970526     C*
014800001020     C                   ENDDO
014900970527     C*
015000970527     C* CI SONO ERRORI IN ENTRATA
015100001020     C                   ELSE
015200120529     C                   MOVEL     '1'           S36ERR
015300120529     C                   MOVEL     DSMSMS        S36MSG
015400001020     C                   ENDIF
015500991122     C*
015600991122     C                   EXSR      FINSR
015700970526     C* FINE PGM
015800970526     C                   SETON                                        LR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016100970526     C*------------------------------------------------------------------------*
016200970526     C     F03GES        BEGSR
016300970526     C*
016400970526     C* IMPOSTA "PREMUTO F03"
016500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016600120529     C                   MOVEL     '1'           S36F03                         *DI PROCEDURA
016700970526     C*
016800970526     C                   ENDSR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017100970526     C*------------------------------------------------------------------------*
017200970526     C     F05GES        BEGSR
017300970526     C*
017400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800001020     C* F06GES - GESTIONE F06 -> INSERIMENTO
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F06GES        BEGSR
018100970526     C*
018200970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018300020916     C     DUTLPO        IFNE      'S'
018400970526     C                   MOVEL     '1'           ERRL00
018500970526     C                   MOVEL     MSG(3)        DSMSMS
018600001020     C                   ENDIF
018700970526     C*
018800001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018900120529     C                   MOVEL     D03CCM        S36CCM
019000120529     C                   MOVEL     D03Sts        S36Sts
019100120529     C                   MOVEL     D03ISV        S36ISV
019200120529     C                   MOVEL     D03Tip        S36Tip
019300970602     C                   MOVEL     DEPOP0        WOP0
019400970529     C                   MOVEL     'F06'         WOP1
019500120528     C                   MOVEL     'TIS737R'     WPGM              8
019600970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019800001020     C                   ENDIF
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F12GES - GESTIONE F12 -> RITORNO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F12GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F12"
020700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020800120529     C                   MOVEL     '1'           S36F12                         *DI PROCEDURA
020900970526     C*
021000970526     C                   ENDSR
021100970528     C*------------------------------------------------------------------------*
021200970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021300970528     C*------------------------------------------------------------------------*
021400970528     C     F21GES        BEGSR
021500970612     C*
021600970528     C*
021700970528     C                   ENDSR
021800970528     C*------------------------------------------------------------------------*
021900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022000970528     C*------------------------------------------------------------------------*
022100970528     C     F22GES        BEGSR
022200970528     C*
022300970528     C                   Z-ADD     80            LENGH            15 5
022400970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
022500970528     C                   PARM                    CMD(1)
022600970528     C                   PARM                    LENGH
022700970528     C*
022800970528     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F23GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023900970526     C*------------------------------------------------------------------------*
024000970526     C     F24GES        BEGSR
024100970526     C*
024200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024700970526     C*------------------------------------------------------------------------*
024800970526     C     RUPGES        BEGSR
024900970526     C*
025000970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* ENTGES - GESTIONE ENTER -> INVIO
025500970526     C*------------------------------------------------------------------------*
025600970526     C     ENTGES        BEGSR
025700000703     C*
025800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025900970526     C                   EXSR      NEWPOS
026000970526     C*
026100970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026200001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300001020     C     SWPOSI        IFNE      0
026400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026500001020     C                   ELSE
026600001020     C     SFLULT        IFNE      0
026700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026800970526     C*
026900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027000001020     C     SWMODI        IFNE      0
027100970526     C                   EXSR      POSSFL
027200001020     C                   ENDIF
027300001020     C                   ENDIF
027400001020     C                   ENDIF
027500001020     C                   ENDIF
027600970526     C*
027700970526     C                   ENDSR
027800000703     C*------------------------------------------------------------------------*
027900001020     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028000970526     C*------------------------------------------------------------------------*
028100970526     C     O01GES        BEGSR
028200970526     C*
028300970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028400120529     C                   MOVEL     D03isv        S36isv
028500120529     C                   MOVEL     D03tip        S36tip
028600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028700970526     C*
028800970526     C                   ENDSR
028900970526     C*------------------------------------------------------------------------*
029000001020     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029100970526     C*------------------------------------------------------------------------*
029200970526     C     O02GES        BEGSR
029300970526     C*
029400001020     C* MODIFICA
029500001020     C     ERRL00        IFEQ      '0'
029600120529     C                   MOVEL     VIDCCM        S36CCM
029700120529     C                   MOVEL     VIDPrg        S36Prg
029800120530     C                   EVAL      S36DDe = %dec(%date(VIDDDe:*eur):*iso)
029900970602     C                   MOVEL     DEPOP0        WOP0
030000970526     C                   MOVEL     'O02'         WOP1
030100120528     C                   MOVEL     'TIS737R'     WPGM
030200970526     C                   EXSR      LANPGM
030300060801     C                   Z-ADD     1             SWMODI
030400001020     C                   ENDIF
030500970526     C*
030600970526     C                   ENDSR
030700001020     C*------------------------------------------------------------------------*
030800001020     C* O06GES - GESTIONE OPZIONE 6=SFLEGGA X RE-INVIO
030900001020     C*------------------------------------------------------------------------*
031000001020     C     O06GES        BEGSR
031100001020     C*
031200120622     C* SFLEGGA PER RE-INVIO
031300001020     C     ERRL00        IFEQ      '0'
031400120529     C                   MOVEL     vidccm        VICCCM
031500120529     C                   MOVEL     vidprg        VICPrg
031600120530     C                   EVAL      VICDDe = %dec(%date(VIDDDe:*eur):*iso)
031700120529     C     K03VIC01      CHAIN     TIVIC001
031800001020     C                   IF        %found
031900120622     C                   MOVEL     '0'           VICSTS
032000120528     C                   UPDATE    TIVIC001
032100001020     C                   ENDIF
032200060801     C                   Z-ADD     1             SWMODI
032300001020     C                   ENDIF
032400001020     C*
032500001020     C                   ENDSR
032600991014     C*------------------------------------------------------------------------*
032700991014     C* O03GES - GESTIONE OPZIONE 3=copia
032800991014     C*------------------------------------------------------------------------*
032900991014     C     O03GES        BEGSR
033000991014     C*
033100991014     C*
033200991014     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
033500970526     C*------------------------------------------------------------------------*
033600001020     C     O04GES        BEGSR
033700970526     C*
033800970526     C*
033900001020     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100001020     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034200970526     C*------------------------------------------------------------------------*
034300970526     C     O05GES        BEGSR
034400970526     C*
034500120529     C                   MOVEL     VIDCCM        S36CCM
034600120529     C                   MOVEL     VIDPrg        S36Prg
034700120530     C                   EVAL      S36DDe = %dec(%date(VIDDDe:*eur):*iso)
034800970602     C                   MOVEL     DEPOP0        WOP0
034900970526     C                   MOVEL     'O05'         WOP1
035000120528     C                   MOVEL     'TIS737R'     WPGM
035100970526     C                   EXSR      LANPGM
035200970526     C*
035300970526     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035600970526     C*------------------------------------------------------------------------*
035700970526     C     O07GES        BEGSR
035800970526     C*
035900970526     C*
036000970526     C                   ENDSR
036100010810     C*------------------------------------------------------------------------*
036200010810     C* O08GES - GESTIONE OPZIONE 8=MODIFICA USO EDP
036300010810     C*------------------------------------------------------------------------*
036400010810     C     O08GES        BEGSR
036500010810     C*
036600010810     C* MODIFICA
036700010810     C     ERRL00        IFEQ      '0'
036800120529     C                   MOVEL     VIDCCM        S36CCM
036900120529     C                   MOVEL     VIDPrg        S36Prg
037000120530     C                   EVAL      S36DDe = %dec(%date(VIDDDe:*eur):*iso)
037100010810     C                   MOVEL     DEPOP0        WOP0
037200010810     C                   MOVEL     'O08'         WOP1
037300120528     C                   MOVEL     'TIS737R'     WPGM
037400010810     C                   EXSR      LANPGM
037500060801     C                   Z-ADD     1             SWMODI
037600010810     C                   ENDIF
037700010810     C*
037800010810     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
038100970526     C*------------------------------------------------------------------------*
038200970526     C     NEWPOS        BEGSR
038300970526     C*
038400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
038500970526     C*         1= RICHIESTO POSIZIONAMENTO
038600970526     C                   Z-ADD     0             SWPOSI            7 0
038700970526     C*
038800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
038900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039100970526     C                   Z-ADD     0             SWMODI            1 0
039200970530     C*
039300970530     C* REIMPOSTA INDICATORI DI ERRORE
039400970530     C                   SETOFF                                       808182
039500970530     C                   SETOFF                                       83
039600970526     C*
039700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
039800120529     C     D03tip        IFNE      DEPtip
039900120529     C     D03CCM        ORNE      DEPks1
040000120529     C     D03ISV        ORNE      DEPISV
040100120529     C     D03Sts        ORNE      DEPSts
040200120529     C     D03Eml        ORNE      DEPeml
040300120529     C     D03Rag        ORNE      DEPSRS
040400120702     C     D03Cmp        ORNE      DEPCmp
040500120702     C     D03No3C       ORNE      DEPNo3C
040600970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
040700001020     C     ERRL00        IFEQ      '0'                                          *NO ERORI
040800970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
040900120529     C                   MOVEL     D03CCM        DEPks1                         *DEPOSITI
041000120529     C                   MOVEL     D03Tip        DEPtip
041100120529     C                   MOVEL     D03ISV        DEPISV
041200120529     C                   MOVEL     D03Sts        DEPSts
041300120529     C                   MOVEL     D03Eml        DEPeml                         *DEPOSITI
041400120529     C                   MOVEL     D03Rag        DEPSRS                         Ragione sociale
041500120702     C                   MOVEL     D03Cmp        DEPCmp
041600120702     C                   MOVEL     D03No3C       DEPNo3C
041700001020     C                   ENDIF
041800001020     C                   ENDIF
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200001020     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
042300970526     C*------------------------------------------------------------------------*
042400970528     C     SETFIL        BEGSR
042500970526     C*
042600970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
042700111229     C*
042800120529     C                   MOVE      D03CCM        VICCCM
042900120622     C                   MOVE      D03STS        VICSTS
043000120622     C                   MOVE      D03ISV        VICISV
043100120622     C                   MOVE      D03TIP        VICTIP
043200120102     C* se il campo di filtro sul cliente è tutti zeri lo trasformo in tutti blank
043300120102     C* (normalmente è un numerico per questo resta il CHECK(RZ))
043400120529     C                   IF        D03CCM = '0000000'
043500120529     C                   EVAL      D03CCM = *blank
043600120102     C                   ENDIF
043700120622     C*
043800120622     C                   SELECT
043900120622     C*
044000120622     C                   WHEN      WTIPRI = 'CCM'
044100120622     C* se il codice cliente non è stato valorizzato
044200120622     C                   IF        D03CCM = *blank
044300120622     C* allora il tipo ricerca è fittizio e devo leggere tutto
044400120622     C     *loval        SETLL     TIVIC01I                           99
044500120622     C                   ELSE
044600120622     C     VICCCM        SETLL     TIVIC01I                           99
044700120622     C                   ENDIF
044800120622     C*
044900120622     C                   WHEN      WTIPRI = 'STS'
045000120622     C* se impostato anche il tipo invio uso pure quello come chiave
045100120622     C                   IF        D03ISV <> *blank
045200120622     C     K02VIC02      SETLL     TIVIC02I                           99
045300120622     C                   ELSE
045400120622     C     VICSTS        SETLL     TIVIC02I                           99
045500120622     C                   ENDIF
045600120622     C*
045700120622     C                   WHEN      WTIPRI = 'TIP'
045800120622     C     VICTIP        SETLL     TIVIC03I                           99
045900120622     C*
046000120622     C                   ENDSL
046100980107     C*
046200970526     C* LETTURA RECORD
046300001020     C     *IN99         IFEQ      *OFF
046400970526     C                   EXSR      LETFIL
046500001020     C                   ENDIF
046600970526     C*
046700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
046800001020     C     *IN99         IFEQ      *ON
046900970526     C                   Z-ADD     1             SFLEOF
047000001020     C                   ENDIF
047100970526     C*
047200970526     C                   ENDSR
047300970526     C*------------------------------------------------------------------------*
047400970526     C* LETFIL - LETTURA RECORD
047500970526     C*------------------------------------------------------------------------*
047600970526     C     LETFIL        BEGSR
047700970526     C*
047800970526     C* LEGGE FINO A:
047900970526     C*     - FINE FILE
048000970526     C*     - TROVATO RECORD VALIDO
048100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048200001020     C     *IN99         DOWEQ     *OFF
048300970526     C     WRECOK        ANDEQ     'N'
048400970526     C*
048500970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
048600120622     C*
048700120622     C                   SELECT
048800120622     C*
048900120622     C                   WHEN      WTIPRI = 'CCM'
049000120622     C* se il codice cliente non è stato valorizzato
049100120622     C                   IF        D03CCM = *blank
049200120622     C* allora il tipo ricerca è fittizio e devo leggere tutto
049300120622     C                   READ      TIVIC01I                               99
049400120622     C                   ELSE
049500120622     C     VICCCM        READE     TIVIC01I                               99
049600120622     C                   ENDIF
049700120622     C*
049800120622     C                   WHEN      WTIPRI = 'STS'
049900120622     C* se impostato anche il tipo invio uso pure quello come chiave
050000120622     C                   IF        D03ISV <> *blank
050100120622     C     K02VIC02      READE     TIVIC02I                               99
050200120622     C                   ELSE
050300120622     C     VICSTS        READE     TIVIC02I                               99
050400120622     C                   ENDIF
050500120622     C*
050600120622     C                   WHEN      WTIPRI = 'TIP'
050700120622     C     VICTIP        READE     TIVIC03I                               99
050800120622     C*
050900120622     C                   ENDSL
051000970528     C*
051100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
051200001020     C     *IN99         IFEQ      *ON                                          *FINE FILE
051300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
051400001020     C                   ELSE
051500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
051600001020     C                   ENDIF
051700001020     C                   ENDDO
051800970526     C*
051900970526     C                   ENDSR
052000970526     C*------------------------------------------------------------------------*
052100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
052200970526     C*------------------------------------------------------------------------*
052300970526     C     CHKREC        BEGSR
052400970526     C*
052500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
052600120529     C*
052700060727     C* Filtra se impostato codice cliente da video
052800120529     C                   if        WRECOK = 'S'
052900120529     C     D03CCM        IFEQ      *blank
053000120529     C     D03CCM        OREQ      *zeros
053100001020     C                   ELSE
053200120529     C                   MOVE(p)   D03CCM        wrkcli7           7
053300120529     C                   IF        %char(VICCCM) <> wrkcli7
053400991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053500001020     C                   ENDIF
053600001020     C                   ENDIF
053700120529     C                   ENDIF
053800120529     C* Filtra se impostato tipo cappario da video
053900120529     C                   if        WRECOK = 'S'
054000120529     C     D03Tip        IFEQ      *blanks
054100001020     C                   ELSE
054200120529     C     D03Tip        IFNE      VICTip
054300000801     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054400001020     C                   ENDIF
054500001020     C                   ENDIF
054600120529     C                   ENDIF
054700120529     C* Filtra se impostato tipo invio    da video
054800120529     C                   if        WRECOK = 'S'
054900120529     C     D03ISV        IFEQ      *blanks
055000120529     C                   ELSE
055100120529     C     D03ISV        IFNE      VICISV
055200120529     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055300120529     C                   ENDIF
055400120529     C                   ENDIF
055500120529     C                   ENDIF
055600120529     C* Filtra se impostato stato      da video
055700120529     C                   if        WRECOK = 'S'
055800120529     C     D03Sts        IFEQ      *blanks
055900001020     C                   ELSE
056000120529     C     D03Sts        IFNE      VICSts
056100001020     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056200001020     C                   ENDIF
056300001020     C                   ENDIF
056400120529     C                   ENDIF
056500111229     C* Filtra se impostata una e-mail da video
056600010919     C                   if        WRECOK = 'S'
056700120529     C     D03Eml        ifeq      *blanks
056800010919     C                   else
056900010919     C                   z-add     *zeros        pos               2 0
057000120529     C                   eval      pos = %scan(%trim(D03Eml):VICeml)
057100010919     C                   if        pos = *zeros
057200010919     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
057300010919     C                   endif
057400010919     C                   endif
057500010919     C                   endif
057600120103     C* Filtra se impostata una ragione sociale da video
057700120103     C                   if        WRECOK = 'S'
057800120529     C     D03Rag        ifeq      *blanks
057900120103     C                   else
058000120103     C                   z-add     *zeros        pos               2 0
058100120622     C                   eval      pos = %scan(%trim(D03Rag):VICRag)
058200120103     C                   if        pos = *zeros
058300120103     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
058400120103     C                   endif
058500120103     C                   endif
058600120103     C                   endif
058700120702     C*
058800120702     C* Filtra rispetto a flag "da completare"
058900120702     C                   IF        WRECOK = 'S'
059000120702     C                   IF        D03Cmp <> *blank
059100120702     C                   IF        D03Cmp = '0' and VICCmp = *on  OR
059200120702     C                             D03Cmp = '1' and VICCmp = *off
059300120702     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059400120702     C                   endif
059500120702     C                   endif
059600120702     C                   endif
059700120702     C*
059800120702     C* Filtra rispetto a flag "cliente NON attivo tab.3c"
059900120702     C                   IF        WRECOK = 'S'
060000120702     C                   IF        D03No3C <> *blank
060100120702     C                   IF        D03No3C = '0' and VICNo3C = *on  OR
060200120702     C                             D03No3C = '1' and VICNo3C = *off
060300120702     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
060400120702     C                   endif
060500120702     C                   endif
060600120702     C                   endif
060700970526     C*
060800970526     C                   ENDSR
060900970526     C*------------------------------------------------------------------------*
061000001020     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
061100970526     C*------------------------------------------------------------------------*
061200970526     C     IRESFL        BEGSR
061300970526     C*
061400120702     C                   CLEAR                   S736D05
061500970526     C                   MOVEL     *BLANKS       OPZKEY
061600120529     C                   MOVEL     VICCCM        VIDCCM
061700120529     C                   MOVEL     VICPrg        VIDPrg
061800120529     C                   MOVEL     VICRag        VIDRag
061900120530     C                   EVAL      VIDDDe = %dec(%date(VICDDe:*iso):*eur)
062000120530     C                   EVAL      VIDDSc = %dec(%date(VICDSc:*iso):*eur)
062100120529     C                   MOVEL     VICEml        VIDEml
062200120529     C                   MOVEL     VICTip        VIDTip
062300120529     C                   MOVEL     VICISV        VIDISV
062400120529     C                   MOVEL     VICSts        VIDSts
062500120529     C                   MOVEL     VICNote       VIDNote
062600001020     C*
062700970526     C                   ENDSR
062800970526     C*------------------------------------------------------------------------*
062900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
063000970526     C*------------------------------------------------------------------------*
063100970526     C     CHKPAR        BEGSR
063200970528     C*
063300111229     C                   SELECT
063400120529
063500120529     C* Se impostato il cliente (a prescindere dagli altri dati)
063600120529     C     D03CCM        WHENNE    *blank
063700120529     C     D03CCM        ANDNE     *zeros
063800120529     C* la ricerca è per cliente
063900120529     C                   MOVEL     'CCM'         WTIPRI
064000120622
064100120622     C* Se impostato lo stato
064200120622     C     D03STS        WHENNE    *blank
064300120622     C* la ricerca è per stato
064400120622     C                   MOVEL     'STS'         WTIPRI
064500120622
064600120622     C* Se impostato il tipo cappario
064700120622     C     D03TIP        WHENNE    *blank
064800120622     C* la ricerca è per tipo cappario
064900120622     C                   MOVEL     'TIP'         WTIPRI
065000120529
065100111229     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CLIENTE
065200111229     C                   OTHER
065300120529     C                   MOVEL     'CCM'         WTIPRI
065400111229     C                   ENDSL
065500000801     C*
065600970526     C                   ENDSR
065700000801     C*------------------------------------------------------------------------*
065800001020     C* CNTISV - CONTROLLA CODICE TIPO SERVIZIO
065900000801     C*------------------------------------------------------------------------*
066000001020     C     CNTISV        BEGSR
066100000801     C*
066200001020     C                   CLEAR                   TIBS02DS
066300000801     C*
066400000801     C     '?'           SCAN      wvidsr1                                30
066500000801     C*  Ricerca
066600001020     C                   EVAL      t02tla = 'L'
066700001020     C                   IF        *IN30 = *ON
066800001020     C                   MOVEL     'R'           t02mod
066900001020     C                   ELSE
067000001020     C                   MOVEL     'C'           t02mod
067100001020     C                   MOVEL(p)  wvidsr1       t02ke1
067200001020     C                   ENDIF
067300001020     C                   MOVEL     'ISV'         t02cod
067400001020     C                   MOVEL     knsif         t02sif
067500000801     C                   CALL      'TIBS02R'
067600001020     C                   PARM                    KPJBA
067700001020     C                   PARM                    TIBS02DS
067800001020     C*
067900001020     C                   IF        T02err = *blanks
068000120529     C                   MOVEL     T02ke1        wvidsr1           2
068100001020     C                   ELSE
068200001020     C                   MOVEL     '1'           errl00
068300001020     C                   SETON                                        62
068400000801     C                   MOVEL     MSG(9)        DSMSMS
068500000801     C                   ENDIF
068600000801     C*
068700001020     C                   ENDSR
068800970526     C*------------------------------------------------------------------------*
068900970526     C* LANPGM - LANCIA UN PROGRAMMA
069000970526     C*------------------------------------------------------------------------*
069100970526     C     LANPGM        BEGSR
069200970526     C*
069300970526     C* IMPOSTA LA DS DI PROCEDURA
069400120529     C                   MOVEL     WOP0          S36OP0                         *LIVELLO PROCEDURA
069500120529     C                   MOVEL     WOP1          S36OP1                         *LIVELLO PGM
069600120529     C                   MOVEL     '0'           S36F03                         *PREMUTO F03=FINE
069700120529     C                   MOVEL     '0'           S36F12                         *PREMUTO F12=RITORNO
069800120529     C                   MOVEL     '0'           S36ERR                         *'1'=ERRORE
069900120529     C                   MOVEL     *BLANKS       S36MSG                         *MESSAGGIO DI RITORN
070000120528     C                   MOVEL(p)  tiS736        kpjbu
070100001020     C*
070200001020     C                   CALL      WPGM
070300001020     C                   PARM                    KPJBA
070400970526     C*
070500970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
070600120529     C     S36F03        IFEQ      '1'                                          *PREMUTO F03
070700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
070800970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
070900001020     C                   ELSE                                                   *NON PREMUTO F03
071000120529     C                   MOVEL     S36MSG        DSMSMS                         *MESSAGGIO DI RITORN
071100001020     C                   ENDIF
071200060801     C*
071300120529     C                   CLEAR                   s36CCM
071400120529     C                   CLEAR                   s36Prg
071500120529     C                   CLEAR                   s36DDe
071600970526     C*
071700970526     C                   ENDSR
071800970526     C*------------------------------------------------------------------------*
071900970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
072000970526     C*------------------------------------------------------------------------*
072100970526     C     CHKFOP        BEGSR
072200970626     C*
072300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
072400001020     C     DEPOP0        IFEQ      'P01'
072500970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
072600970626     C                   MOVEL     'S'           OPDATT
072700970626     C                   MOVEL     OPDRIG        OPD(1)
072800001020     C                   ELSE
072900970626     C* OPZIONI
073000970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
073100970626     C                   MOVEL     'S'           OPDATT
073200970626     C                   MOVEL     OPDRIG        OPD(5)
073300970626     C* FUNZIONI
073400001020     C                   ENDIF
073500970626     C*
073600970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
073700001020     C     DEPOP0        IFEQ      'P02'
073800970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
073900970626     C                   MOVEL     'S'           OPDATT
074000970626     C                   MOVEL     OPDRIG        OPD(2)
074100001020     C                   MOVEL     OPD(6)        OPDRIG                         *SFLEGGA INVIO
074200001020     C                   MOVEL     'S'           OPDATT
074300001020     C                   MOVEL     OPDRIG        OPD(6)
074400001020     C*                  MOVEL     OPD(3)        OPDRIG                         *MODIFICA
074500001020     C*                  MOVEL     'S'           OPDATT
074600001020     C*                  MOVEL     OPDRIG        OPD(3)
074700991104     C*                  MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
074800991104     C*                  MOVEL     'S'           OPDATT
074900991104     C*                  MOVEL     OPDRIG        OPD(4)
075000000711     C*                  MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
075100000711     C*                  MOVEL     'S'           OPDATT
075200000711     C*                  MOVEL     OPDRIG        OPD(7)
075300970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075400970626     C                   MOVEL     'S'           FUDATT
075500970626     C                   MOVEL     FUDRIG        FUD(6)
075600001020     C*                  MOVEL     OPD(9)        OPDRIG                         *CLI. UNIF.
075700001020     C*                  MOVEL     'S'           OPDATT
075800001020     C*                  MOVEL     OPDRIG        OPD(9)
075900010810     C* SE L'UTENTE E' UN EDP
076000010810     C                   IF        %subst(%trim(KNMUS):1:3) = 'EDP'
076100010810     C                   MOVEL     OPD(8)        OPDRIG                         *CLI. UNIF.
076200010810     C                   MOVEL     'S'           OPDATT
076300010810     C                   MOVEL     OPDRIG        OPD(8)
076400010810     C                   ENDIF
076500120622     C* per il momento tolgo questa opzione
076600120622     C                   MOVEL     'N'           OPDATT
076700120622     C                   MOVEL     OPDRIG        OPD(8)
076800001020     C                   ENDIF
076900970626     C*
077000970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
077100020916     C     DUTLPO        IFNE      'S'                                          *FILIALE
077200991104     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
077300991104     C*                  MOVEL     'N'           OPDATT
077400991104     C*                  MOVEL     OPDRIG        OPD(4)
077500001020     C*                  MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
077600001020     C*                  MOVEL     'N'           OPDATT
077700001020     C*                  MOVEL     OPDRIG        OPD(7)
077800970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
077900970626     C                   MOVEL     'N'           FUDATT
078000970626     C                   MOVEL     FUDRIG        FUD(6)
078100001020     C                   ENDIF
078200970526     C*
078300970526     C* GESTIONE OPZIONI A VIDEO
078400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
078500970526     C                   EXSR      OPZGES
078600970526     C*
078700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
078800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
078900970526     C                   EXSR      FUNGES
079000970526     C*
079100970526     C                   ENDSR
079200991122     C*------------------------------------------------------------------------*
079300991122     C* FINSR - OPERAZIONI FINALI
079400991122     C*------------------------------------------------------------------------*
079500991122     C     FINSR         BEGSR
079600991122     C*
079700991122     C* CHIUDE I FILE DEI PGM CHIAMATI
079800991122     C                   CLEAR                   BS69DS
079900991122     C                   CLEAR                   ACODS
080000991122     C                   CLEAR                   INDDS
080100991122     C                   CLEAR                   CLPDS
080200991122     C                   CLEAR                   CLSDS
080300991122     C                   MOVEL     'C'           I69TLA
080400991122     C                   MOVEL     KNSIF         I69SIF
080500991122     C                   CALL      'TIBS69R'
080600991122     C                   PARM                    BS69DS
080700991122     C                   PARM                    ACODS
080800991122     C                   PARM                    INDDS
080900991122     C                   PARM                    CLPDS
081000991122     C                   PARM                    CLSDS
081100991122     C*
081200991122     C                   ENDSR
081300020916     C*--------------------------------------------------------------------------------------------*
081400020916     C* REPERISCE I DATI UTENTE
081500020916     C*--------------------------------------------------------------------------------------------*
081600020916     C     REPDATIUTE    BEGSR
081700020916     C*
081800020916     C* INIZIALIZZA VARIABILI DI WRK
081900020916     C                   CLEAR                   TIBS34DS
082000020916     C                   CLEAR                   AZUTEDS
082100020916     C                   CLEAR                   DDATIUTE
082200020916     C*
082300020916     C     *DTAARA       DEFINE    §azute        azuteds
082400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
082500020916     C                   IN(E)     *DTAARA
082600020916if  1C                   IF        %Error
082700020916     c                   EVAL      I34Tla = 'L'
082800020916     C                   CALL      'TIBS34R'
082900020916     C                   PARM                    Tibs34Ds
083000020916     C                   IN        *DTAARA
083100020916e   1C                   ENDIF
083200020916     C*
083300020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
083400020916     C                   MOVEL(P)  RSUT          DSFIRS           20
083500020916     C*
083600020916     C                   ENDSR
083700970526     C*------------------------------------------------------------------------*
083800001020     C* *INZSR - OPERAZIONI INIZIALI
083900970526     C*------------------------------------------------------------------------*
084000970526     C     *INZSR        BEGSR
084100970526     C*---
084200970526     C* RICEVIMENTO PARAMETRI
084300970526     C*---
084400970526     C     *ENTRY        PLIST
084500970612     C                   PARM                    KPJBA
084600120528     C                   MOVEL     KPJBU         TIS736
084700120529     C                   MOVEL     'CCM'         WTIPRI            3
084800060731     C*
084900060731     C* CALCOLA LA DATA CORRENTE
085000060731     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
085100060731     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
085200060731     C                   Z-ADD     WN8           G08DAT
085300060731     C                   Z-ADD     *ZEROS        G08INV
085400060731     C                   MOVEL     '0'           G08ERR
085500060731     C                   CALL      'XSRDA8'
085600060731     C                   PARM                    WLBDA8
085700060731     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
085800970526     C*
085900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
086000970526     C     XMSGPA        PLIST
086100970526     C                   PARM                    DSMSMS
086200970526     C                   PARM                    DSMSPG
086300970526     C                   PARM                    DSMSKE
086400970526     C                   PARM                    DSMSER
086500970526     C*
086600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
086700970526     C                   CALL      'XKEY'
086800970526     C                   PARM                    DSKEY
086900970526     C*---
087000970526     C* VARIABILI NON RIFERITE AL DATA BASE
087100970526     C*---
087200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
087300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
087400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
087500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
087600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
087700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
087800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
087900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
088000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088100120529     C     *LIKE         DEFINE    S36OP0        DEPOP0                         *DI DEPOSITO
088200120529     C     *LIKE         DEFINE    S36OP1        DEPOP1
088300120529     C     *LIKE         DEFINE    S36OP0        WOP0                           *DI LAVORO
088400120529     C     *LIKE         DEFINE    S36OP1        WOP1
088500970526     C*---
088600120529     C     *LIKE         DEFINE    s36tip        DEPtip
088700120529     C     *LIKE         DEFINE    D03CCM        DEPks1
088800120529     C     *LIKE         DEFINE    D03Tip        DEPTip
088900120529     C     *LIKE         DEFINE    D03ISV        DEPISV
089000120529     C     *LIKE         DEFINE    D03Sts        DEPSts
089100120529     C     *LIKE         DEFINE    D03Eml        DEPeml
089200120529     C     *LIKE         DEFINE    D03Rag        DEPSRS
089300120702     C     *LIKE         DEFINE    D03Cmp        DEPCmp
089400120702     C     *LIKE         DEFINE    D03No3C       DEPNo3C
089500001020     C*---
089600001020     C* VARIABILI RIFERITE AL DATA BASE
089700001020     C*---
089800001020     C*---
089900970526     C* VARIABILI DI CONTROLLO
090000970526     C*---
090100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
090200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
090300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
090400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
090500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
090600970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
090700970526     C*
090800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
090900120528     C                   MOVEL     'TIS736R'     DSMSPG
091000120528     C                   MOVEL     'TIS736R'     MSGQUE
091100970526     C                   MOVEL     *BLANKS       MSGKEY
091200970526     C                   MOVEL     *BLANKS       DSMSMS
091300970526     C                   MOVEL     *BLANKS       DSMSKE
091400970526     C                   Z-ADD     *ZEROS        DSMSER
091500970526     C*---
091600970526     C* CHIAVI DI LETTURA
091700970526     C*---
091800120622     C* LETTURA TIVIC01I - completa
091900120529     C     K03VIC01      KLIST
092000120529     C                   KFLD                    VICCCM
092100120529     C                   KFLD                    VICPrg
092200120529     C                   KFLD                    VICDDe
092300120622     C* LETTURA TIVIC02I - completa
092400120622     C     K02VIC02      KLIST
092500120622     C                   KFLD                    VICSts
092600120622     C                   KFLD                    VICISV
092700970527     C*
092800970527     C* REPERIMENTO DATI UTENTE
092900020916     C                   EXSR      REPDATIUTE
093000970526     C*
093100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
093200120529     C                   MOVEL     S36OP0        DEPOP0                         *LIVELLO PROCEDURA
093300120529     C                   MOVEL     S36OP1        DEPOP1                         *LIVELLO PROGRAMMA
093400970526     C*
093500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
093600970526     C                   EXSR      CHKFOP
093700970526     C*
093800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
093900001020     C     DEPOP0        IFEQ      'P01'
094000970530     C                   MOVEL     DEC(3)        VIDING
094100001020     C                   ENDIF
094200001020     C     DEPOP0        IFEQ      'P02'
094300001020     C                   MOVEL     DEC(1)        VIDING
094400001020     C                   ENDIF
094500001020     C     DEPOP0        IFEQ      'P05'
094600970526     C                   MOVEL     DEC(2)        VIDING
094700001020     C                   ENDIF
094800970526     C*
094900970526     C                   ENDSR
095000970526     C*------------------------------------------------------------------------*
095100111229     C*
095200120702     C* Scrivo TIVIC caricandolo da tab.3C
095300120702     C*
095400970526     C*------------------------------------------------------------------------*
095500120702     C     WrtVIC3C      BEGSR
095600970526     C*
095700120702     C                   CALL      'TIS738R'
095800120702     C                   PARM                    Errore738         1
095900120702     C*
096000120702     C                   ENDSR
096100120702     C*------------------------------------------------------------------------*
096200120702     C*
096300120702     C*------------------------------------------------------------------------*
096400120702     C     InzD03        BEGSR
096500120702     C*
096600111229     C* imposto dati da DS di ingresso
096700111229     C*
096800111229     C*  codice cliente
096900120529     C                   IF        %subst(S36CCM : 2: 7) <> *blank
097000120529     C                   MOVE      s36CCM        D03CCM
097100111229     C                   ELSE
097200120529     C                   CLEAR                   D03CCM
097300111229     C                   ENDIF
097400120529     C*  progressivo
097500120529     C***                MOVEL     S36Prg        D03Prg
097600120529     C*  data decorrenza
097700120529     C***                MOVEL     S36DDe        D03DDe
097800120529     C*  tipo invio
097900120529     C                   MOVEL     S36ISV        D03ISV
098000120529     C*  tipo cappario
098100120529     C                   MOVEL     S36Tip        D03Tip
098200120529     C*  stato
098300120529     C                   MOVEL     S36Sts        D03Sts
098400111229     C*
098500111229     C* simulo un Enter per valorizzare il tipo ricerca
098600111229     C                   EXSR      EntGes
098700111229     C*
098800111229     C*
098900111229     C*
099000111229     C                   ENDSR
099100111229     C*------------------------------------------------------------------------*
099200111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099300111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099400111229     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
099500111229     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
099600111229     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
099700111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099800111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099900111229     C*------------------------------------------------------------------------*
100000111229     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
100100111229     C*------------------------------------------------------------------------*
100200111229     C     POSSFL        BEGSR
100300111229     C*
100400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
100500970526     C                   EXSR      CLRSFL
100600970526     C*
100700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
100800970526     C                   EXSR      SETFIL
100900970526     C*
101000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
101100970526     C     SFLEOF        IFEQ      0
101200970526     C                   EXSR      CARSFL
101300970526     C                   END
101400970526     C*
101500970526     C                   ENDSR
101600970526     C*------------------------------------------------------------------------*
101700970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
101800970526     C*------------------------------------------------------------------------*
101900970526     C     ENTATT        BEGSR
102000970526     C*
102100970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
102200970526     C                   Z-ADD     0             OPZOK             1 0
102300970526     C*
102400970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
102500970526     C                   Z-ADD     0             SFLVAR            5 0
102600970526     C*
102700970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
102800970526     C                   Z-ADD     0             SFLERR
102900970526     C*
103000970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
103100970526     C                   Z-ADD     SFLULT        WFLULT
103200970526     C*
103300970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
103400120528     C                   READC     S736D05                                98
103500970526     C     *IN98         DOWEQ     '0'
103600970526     C*
103700970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
103800970526     C                   ADD       1             SFLVAR
103900970526     C*
104000970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
104100970526     C     OPZOK         IFEQ      0
104200970526     C*
104300970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
104400970526     C     OPZKEY        IFNE      *BLANKS
104500970526     C                   EXSR      OPZCON
104600001020     C                   ENDIF
104700970526     C*
104800970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
104900970526     C*            2. PULIZIA SCELTA IN SFL
105000970526     C     OPZOK         IFEQ      0
105100970526     C                   MOVEL     '0'           *IN41
105200970526     C                   MOVEL     *BLANKS       OPZKEY
105300970526     C                   ELSE
105400970526     C*
105500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
105600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
105700970526     C                   MOVEL     '1'           *IN41
105800970526     C                   Z-ADD     SFLULT        SFLERR
105900001020     C                   ENDIF
106000970526     C                   ELSE
106100970526     C*
106200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
106300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
106400970526     C                   MOVEL     '1'           *IN41
106500001020     C                   ENDIF                                                  OPZOK=0
106600970526     C*
106700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
106800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
106900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
107000970526     C     OPZOK         IFNE      0
107100970526     C                   Z-ADD     SFLERR        SFLNBR
107200970526     C                   ELSE
107300970526     C                   Z-ADD     SFLULT        SFLNBR
107400001020     C                   ENDIF
107500970526     C*
107600970526     C* AGGIORNO RECORD SFL
107700120528     C                   UPDATE    S736D05
107800970526     C*
107900120528     C                   READC     S736D05                                98
108000001020     C                   ENDDO                                                  DO WHILE
108100970526     C*
108200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
108300970526     C                   Z-ADD     WFLULT        SFLULT
108400970526     C*
108500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
108600970526     C     SFLVAR        IFEQ      0
108700001020     C*                  MOVEL     '1'           FINL00
108800001020     C                   ENDIF
108900970526     C*
109000970526     C                   ENDSR
109100970526     C*------------------------------------------------------------------------*
109200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
109300970526     C*------------------------------------------------------------------------*
109400970526     C     CLRSFL        BEGSR
109500970526     C*
109600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
109700970526     C* (0=NON FINE FILE  1=FINE FILE)
109800970526     C                   Z-ADD     0             SFLEOF
109900970526     C*
110000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
110100970526     C                   Z-ADD     0             SFLULT
110200970526     C*
110300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
110400970526     C                   MOVEL     '1'           *IN40
110500120528     C                   WRITE     S736D06
110600970526     C                   MOVEL     '0'           *IN40
110700970526     C*
110800970526     C                   ENDSR
110900970526     C*------------------------------------------------------------------------*
111000970526     C* CARSFL - CARICAMENTO SFL
111100970526     C*------------------------------------------------------------------------*
111200970526     C     CARSFL        BEGSR
111300970526     C*
111400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
111500970526     C     SFLULT        IFEQ      0
111600970526     C                   EXSR      SETFIL
111700001020     C                   ENDIF
111800970526     C*
111900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
112000970526     C                   Z-ADD     SFLULT        WFLULT
112100970526     C*
112200970526     C* CICLO:
112300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
112400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
112500970526     C                   Z-ADD     1             SFLRIG
112600970526     C     SFLRIG        DOWLE     SFLPAG
112700970526     C     SFLEOF        ANDEQ     0
112800970526     C*
112900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
113000970526     C                   MOVEL     *BLANKS       OPZKEY
113100970526     C*
113200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
113300970526     C                   EXSR      IRESFL
113400970526     C*
113500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
113600970526     C                   ADD       1             SFLULT
113700970526     C*
113800970526     C* SCRITTURA RECORD SFL
113900120528     C                   WRITE     S736D05
114000970526     C                   ADD       1             SFLRIG
114100970526     C*
114200970526     C* LETTURA FILE DA CARICARE
114300970526     C                   EXSR      LETFIL
114400001020     C                   ENDDO                                                  DO WHILE
114500970526     C*
114600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
114700970526     C     WFLULT        IFNE      SFLULT
114800970526     C                   Z-ADD     WFLULT        SFLNBR
114900970526     C                   ADD       1             SFLNBR
115000001020     C                   ENDIF
115100970526     C*
115200970526     C                   ENDSR
115300970526     C*------------------------------------------------------------------------*
115400970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
115500970526     C*------------------------------------------------------------------------*
115600970526     C     OUTSFL        BEGSR
115700970526     C*
115800970526     C* WRITE RECORD TESTATA VIDEO
115900120528     C                   WRITE     S736D01
116000970526     C*
116100970526     C* VISUALIZZAZIONE MESSAGGIO
116200970526     C                   EXSR      SNDMSG
116300970526     C*
116400970526     C* WRITE RECORD OPZIONI VIDEO
116500120528     C                   WRITE     S736D02
116600970526     C*
116700970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
116800120528     C                   WRITE     S736D03
116900970526     C*
117000970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
117100120528     C                   WRITE     S736D04
117200970526     C*
117300970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
117400970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
117500970526     C     SFLEOF        IFEQ      0
117600970526     C                   MOVEL     '0'           *IN42
117700970526     C                   ELSE
117800970526     C*
117900970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
118000970526     C                   MOVEL     '1'           *IN42
118100001020     C                   ENDIF
118200970526     C*
118300970526     C* WRITE RECORD TASTI FUNZIONALI
118400120528     C                   WRITE     S736D09
118500970526     C*
118600970526     C* CASO DI SFL VUOTO
118700970526     C     SFLULT        IFLE      0
118800970526     C*
118900970526     C*    - WRITE RECORD VUOTO
119000120528     C                   WRITE     S736D0A
119100120528     C                   READ      S736D0A                                99
119200970526     C*
119300970526     C                   ELSE
119400970526     C*
119500970526     C* CASO DI SFL CON RECORDS
119600970526     C*
119700970526     C*    - WRITE RECORD CONTROLLO SFL
119800120528     C                   WRITE     S736D06
119900120528     C                   READ      S736D06                                99
120000001020     C                   ENDIF
120100970526     C*
120200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
120300120528     C                   READ      S736D03                                99
120400970526     C*
120500970526     C                   ENDSR
120600970526     C*------------------------------------------------------------------------*
120700970526     C* SNDMSG - MANDA MESSAGGIO
120800970526     C*------------------------------------------------------------------------*
120900970526     C     SNDMSG        BEGSR
121000970526     C*
121100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
121200970526     C                   MOVEL     '1'           *IN40
121300120528     C                   WRITE     S736DMC
121400970526     C                   MOVEL     '0'           *IN40
121500970526     C*
121600970526     C* VISUALIZZAZIONE MESSAGGIO
121700970526     C                   Z-ADD     0             MSGULT
121800970526     C                   CALL      'XMSG'        XMSGPA
121900970526     C     DSMSER        IFEQ      0
122000970526     C                   MOVEL     DSMSKE        MSGKEY
122100001020     C                   ENDIF
122200970526     C                   Z-ADD     1             MSGULT
122300120528     C                   WRITE     S736DMS                                      SFL MESS
122400120528     C                   WRITE     S736DMC                                      CONTR MESS
122500970526     C*
122600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
122700970526     C                   MOVEL     *BLANKS       DSMSMS
122800970526     C*
122900970526     C                   ENDSR
123000970526     C*------------------------------------------------------------------------*
123100970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
123200970526     C*------------------------------------------------------------------------*
123300970526     C     FUNCON        BEGSR
123400970526     C*
123500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
123600970526     C                   EXSR      FUNVAR
123700970526     C*
123800970526     C* PULISCO CAMPI DI LAVORO
123900970526     C                   Z-ADD     0             I                 5 0
124000970526     C                   Z-ADD     0             W                 5 0
124100970526     C*
124200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
124300970526     C     FUNKEY        IFNE      *BLANKS
124400970526     C     1             DO        24            I
124500970526     C                   MOVEL     FUD(I)        FUDRIG
124600970526     C*
124700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
124800970526     C     FUNKEY        IFEQ      FUDFUN
124900970526     C     FUDATT        ANDEQ     'S'
125000970526     C                   Z-ADD     I             W
125100001020     C                   ENDIF
125200001020     C                   ENDDO                                                  DO WHILE
125300970526     C*
125400970526     C* FUNZIONE NON ATTIVA (ERRORE)
125500970526     C     W             IFEQ      0
125600970526     C*
125700970526     C* EMISSIONE MESSAGGIO DI ERRORE
125800970526     C                   MOVEL     MSG(2)        DSMSMS
125900001020     C                   ENDIF                                                  W=0
126000001020     C                   ENDIF                                                  FUNKEY NE *BLANKS
126100970526     C*
126200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
126300970526     C     W             IFNE      0
126400970526     C     FUNKEY        OREQ      *BLANKS
126500970526     C*
126600970526     C     FUNKEY        IFEQ      *BLANKS
126700970526     C                   EXSR      ENTGES
126800001020     C                   ENDIF
126900970526     C     FUNKEY        IFEQ      '03'
127000970526     C                   EXSR      F03GES
127100001020     C                   ENDIF
127200970526     C     FUNKEY        IFEQ      '05'
127300970526     C                   EXSR      F05GES
127400001020     C                   ENDIF
127500970526     C     FUNKEY        IFEQ      '06'
127600970526     C                   EXSR      F06GES
127700001020     C                   ENDIF
127800970526     C     FUNKEY        IFEQ      '12'
127900970526     C                   EXSR      F12GES
128000001020     C                   ENDIF
128100970528     C     FUNKEY        IFEQ      '21'
128200970528     C                   EXSR      F21GES
128300001020     C                   ENDIF
128400970528     C     FUNKEY        IFEQ      '22'
128500970528     C                   EXSR      F22GES
128600001020     C                   ENDIF
128700970526     C     FUNKEY        IFEQ      '23'
128800970526     C                   EXSR      F23GES
128900001020     C                   ENDIF
129000970526     C     FUNKEY        IFEQ      '24'
129100970526     C                   EXSR      F24GES
129200001020     C                   ENDIF
129300970526     C     FUNKEY        IFEQ      'RU'
129400970526     C                   EXSR      RUPGES
129500001020     C                   ENDIF
129600970526     C*
129700001020     C                   ENDIF
129800970526     C*
129900970526     C                   ENDSR
130000970526     C*------------------------------------------------------------------------*
130100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
130200970526     C*------------------------------------------------------------------------*
130300970526     C     FUNVAR        BEGSR
130400970526     C*
130500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
130600970526     C     KEY           IFEQ      ENTER
130700970526     C                   MOVEL     *BLANKS       FUNKEY
130800001020     C                   ENDIF
130900970526     C     KEY           IFEQ      F1
131000970526     C                   MOVEL     '01'          FUNKEY
131100001020     C                   ENDIF
131200970526     C     KEY           IFEQ      F2
131300970526     C                   MOVEL     '02'          FUNKEY
131400001020     C                   ENDIF
131500970526     C     KEY           IFEQ      F3
131600970526     C                   MOVEL     '03'          FUNKEY
131700001020     C                   ENDIF
131800970526     C     KEY           IFEQ      F4
131900970526     C                   MOVEL     '04'          FUNKEY
132000001020     C                   ENDIF
132100970526     C     KEY           IFEQ      F5
132200970526     C                   MOVEL     '05'          FUNKEY
132300001020     C                   ENDIF
132400970526     C     KEY           IFEQ      F6
132500970526     C                   MOVEL     '06'          FUNKEY
132600001020     C                   ENDIF
132700970526     C     KEY           IFEQ      F7
132800970526     C                   MOVEL     '07'          FUNKEY
132900001020     C                   ENDIF
133000970526     C     KEY           IFEQ      F8
133100970526     C                   MOVEL     '08'          FUNKEY
133200001020     C                   ENDIF
133300970526     C     KEY           IFEQ      F9
133400970526     C                   MOVEL     '09'          FUNKEY
133500001020     C                   ENDIF
133600970526     C     KEY           IFEQ      F10
133700970526     C                   MOVEL     '10'          FUNKEY
133800001020     C                   ENDIF
133900970526     C     KEY           IFEQ      F11
134000970526     C                   MOVEL     '11'          FUNKEY
134100001020     C                   ENDIF
134200970526     C     KEY           IFEQ      F12
134300970526     C                   MOVEL     '12'          FUNKEY
134400001020     C                   ENDIF
134500970526     C     KEY           IFEQ      F13
134600970526     C                   MOVEL     '13'          FUNKEY
134700001020     C                   ENDIF
134800970526     C     KEY           IFEQ      F14
134900970526     C                   MOVEL     '14'          FUNKEY
135000001020     C                   ENDIF
135100970526     C     KEY           IFEQ      F15
135200970526     C                   MOVEL     '15'          FUNKEY
135300001020     C                   ENDIF
135400970526     C     KEY           IFEQ      F16
135500970526     C                   MOVEL     '16'          FUNKEY
135600001020     C                   ENDIF
135700970526     C     KEY           IFEQ      F17
135800970526     C                   MOVEL     '17'          FUNKEY
135900001020     C                   ENDIF
136000970526     C     KEY           IFEQ      F18
136100970526     C                   MOVEL     '18'          FUNKEY
136200001020     C                   ENDIF
136300970526     C     KEY           IFEQ      F19
136400970526     C                   MOVEL     '19'          FUNKEY
136500001020     C                   ENDIF
136600970526     C     KEY           IFEQ      F20
136700970526     C                   MOVEL     '20'          FUNKEY
136800001020     C                   ENDIF
136900970526     C     KEY           IFEQ      F21
137000970526     C                   MOVEL     '21'          FUNKEY
137100001020     C                   ENDIF
137200970526     C     KEY           IFEQ      F22
137300970526     C                   MOVEL     '22'          FUNKEY
137400001020     C                   ENDIF
137500970526     C     KEY           IFEQ      F23
137600970526     C                   MOVEL     '23'          FUNKEY
137700001020     C                   ENDIF
137800970526     C     KEY           IFEQ      F24
137900970526     C                   MOVEL     '24'          FUNKEY
138000001020     C                   ENDIF
138100970526     C     KEY           IFEQ      ROLLUP
138200970526     C                   MOVEL     'RU'          FUNKEY
138300001020     C                   ENDIF
138400970526     C     KEY           IFEQ      HELP
138500970526     C                   MOVEL     'HP'          FUNKEY
138600001020     C                   ENDIF
138700970526     C*
138800970526     C                   ENDSR
138900970526     C*------------------------------------------------------------------------*
139000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
139100970526     C*------------------------------------------------------------------------*
139200970526     C     FUNGES        BEGSR
139300970526     C*
139400970526     C* PULISCO CAMPI DI LAVORO
139500970526     C                   Z-ADD     0             W                 5 0
139600970526     C*
139700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
139800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
139900970526     C     1             DO        24            I
140000970526     C                   MOVEL     FUD(I)        FUDRIG
140100970526     C*
140200970526     C* PULIZIA FUNZIONI PRECEDENTI
140300970526     C                   MOVEL     *BLANKS       FUV(I)
140400970526     C*
140500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
140600970526     C* (SALTA F23 E F24)
140700970526     C     FUDATT        IFEQ      'S'
140800970526     C     FUDFUN        ANDNE     '23'
140900970526     C     FUDFUN        ANDNE     '24'
141000970526     C                   ADD       1             W
141100970526     C                   MOVEL     FUDDES        FUV(W)
141200001020     C                   ENDIF
141300001020     C                   ENDDO                                                  DO WHILE
141400970526     C*
141500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
141600970526     C* ALTRIMENTI DISATTIVO F24
141700970526     C     1             DO        24            I
141800970526     C                   MOVEL     FUD(I)        FUDRIG
141900970526     C*
142000970526     C     FUDFUN        IFEQ      '24'
142100970526     C     W             IFGT      6
142200970526     C                   MOVEL     'S'           FUDATT
142300970526     C                   ELSE
142400970526     C                   MOVEL     *BLANKS       FUDATT
142500001020     C                   ENDIF                                                  W GT 8
142600970526     C                   MOVEL     FUDRIG        FUD(I)
142700001020     C                   ENDIF
142800001020     C                   ENDDO                                                  DO WHILE
142900970526     C*
143000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
143100970526     C     W             DIV       6             I
143200970526     C                   MVR                     Z
143300970526     C*
143400970526     C     Z             IFNE      0
143500970526     C     I             ADD       1             Z
143600970526     C                   ELSE
143700970526     C                   Z-ADD     I             Z
143800001020     C                   ENDIF
143900970526     C*
144000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
144100970526     C                   ADD       1             FUNGRU
144200970526     C     FUNGRU        IFGT      Z
144300970526     C     FUNGRU        ORLT      1
144400970526     C                   Z-ADD     1             FUNGRU
144500001020     C                   ENDIF
144600970526     C*
144700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
144800970526     C     FUNGRU        IFEQ      1
144900970526     C                   MOVEL     FUV(1)        FUN001
145000970526     C                   MOVEL     FUV(2)        FUN002
145100970526     C                   MOVEL     FUV(3)        FUN003
145200970526     C                   MOVEL     FUV(4)        FUN004
145300970526     C                   MOVEL     FUV(5)        FUN005
145400970526     C                   MOVEL     FUV(6)        FUN006
145500001020     C                   ENDIF
145600970526     C*
145700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
145800970526     C     FUNGRU        IFEQ      2
145900970526     C                   MOVEL     FUV(7)        FUN001
146000970526     C                   MOVEL     FUV(8)        FUN002
146100970526     C                   MOVEL     FUV(9)        FUN003
146200970526     C                   MOVEL     FUV(10)       FUN004
146300970526     C                   MOVEL     FUV(11)       FUN005
146400970526     C                   MOVEL     FUV(12)       FUN006
146500001020     C                   ENDIF
146600970526     C*
146700970526     C* TERZO GRUPPO DI 6 FUNZIONI
146800970526     C     FUNGRU        IFEQ      3
146900970526     C                   MOVEL     FUV(13)       FUN001
147000970526     C                   MOVEL     FUV(14)       FUN002
147100970526     C                   MOVEL     FUV(15)       FUN003
147200970526     C                   MOVEL     FUV(16)       FUN004
147300970526     C                   MOVEL     FUV(17)       FUN005
147400970526     C                   MOVEL     FUV(18)       FUN006
147500001020     C                   ENDIF
147600970526     C*
147700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
147800970526     C     FUNGRU        IFEQ      4
147900970526     C                   MOVEL     FUV(19)       FUN001
148000970526     C                   MOVEL     FUV(20)       FUN002
148100970526     C                   MOVEL     FUV(21)       FUN003
148200970526     C                   MOVEL     FUV(22)       FUN004
148300970526     C                   MOVEL     *BLANKS       FUN005
148400970526     C                   MOVEL     *BLANKS       FUN006
148500001020     C                   ENDIF
148600970526     C*
148700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
148800970526     C                   MOVEL     *BLANKS       FUN007
148900970526     C                   MOVEL     *BLANKS       FUN008
149000970526     C*
149100970526     C     1             DO        24            I
149200970526     C                   MOVEL     FUD(I)        FUDRIG
149300970526     C     FUDFUN        IFEQ      '23'
149400970526     C     FUDATT        ANDEQ     'S'
149500970526     C                   MOVEL     FUDDES        FUN007
149600001020     C                   ENDIF
149700970526     C     FUDFUN        IFEQ      '24'
149800970526     C     FUDATT        ANDEQ     'S'
149900970526     C                   MOVEL     FUDDES        FUN008
150000001020     C                   ENDIF
150100001020     C                   ENDDO                                                  DO
150200970526     C*
150300970526     C                   ENDSR
150400970526     C*------------------------------------------------------------------------*
150500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
150600970526     C*------------------------------------------------------------------------*
150700970526     C     OPZGES        BEGSR
150800970526     C*
150900970526     C* PULISCO CAMPI DI LAVORO
151000970526     C                   Z-ADD     0             W                 5 0
151100970526     C*
151200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
151300970526     C* E CARICO TABELLA OPZIONI ATTIVE
151400970526     C     1             DO        24            I
151500970526     C                   MOVEL     OPD(I)        OPDRIG
151600970526     C*
151700970526     C* PULIZIA OPZIONI PRECEDENTI
151800970526     C                   MOVEL     *BLANKS       OPV(I)
151900970526     C*
152000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
152100970526     C     OPDATT        IFEQ      'S'
152200970526     C                   ADD       1             W
152300970526     C                   MOVEL     OPDDES        OPV(W)
152400001020     C                   ENDIF
152500001020     C                   ENDDO                                                  DO WHILE
152600970526     C*
152700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
152800970526     C* ALTRIMENTI DISATTIVO F23
152900970526     C     1             DO        24            I
153000970526     C                   MOVEL     FUD(I)        FUDRIG
153100970526     C*
153200970526     C     FUDFUN        IFEQ      '23'
153300970526     C     W             IFGT      8
153400970526     C                   MOVEL     'S'           FUDATT
153500970526     C                   ELSE
153600970526     C                   MOVEL     *BLANKS       FUDATT
153700001020     C                   ENDIF                                                  W GT 8
153800970526     C                   MOVEL     FUDRIG        FUD(I)
153900001020     C                   ENDIF
154000001020     C                   ENDDO                                                  DO WHILE
154100970526     C*
154200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
154300970526     C     W             DIV       8             I
154400970526     C                   MVR                     Z                 5 0
154500970526     C*
154600970526     C     Z             IFNE      0
154700970526     C     I             ADD       1             Z
154800970526     C                   ELSE
154900970526     C                   Z-ADD     I             Z
155000001020     C                   ENDIF
155100970526     C*
155200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
155300970526     C                   ADD       1             OPZGRU
155400970526     C     OPZGRU        IFGT      Z
155500970526     C     OPZGRU        ORLT      1
155600970526     C                   Z-ADD     1             OPZGRU
155700001020     C                   ENDIF
155800970526     C*
155900970526     C* PRIMO GRUPPO DI 8 OPZIONI
156000970526     C     OPZGRU        IFEQ      1
156100970526     C                   MOVEL     OPV(1)        OPZ001
156200970526     C                   MOVEL     OPV(2)        OPZ002
156300970526     C                   MOVEL     OPV(3)        OPZ003
156400970526     C                   MOVEL     OPV(4)        OPZ004
156500970526     C                   MOVEL     OPV(5)        OPZ005
156600970526     C                   MOVEL     OPV(6)        OPZ006
156700970526     C                   MOVEL     OPV(7)        OPZ007
156800970526     C                   MOVEL     OPV(8)        OPZ008
156900001020     C                   ENDIF
157000970526     C*
157100970526     C* SECONDO GRUPPO DI 8 OPZIONI
157200970526     C     OPZGRU        IFEQ      2
157300970526     C                   MOVEL     OPV(9)        OPZ001
157400970526     C                   MOVEL     OPV(10)       OPZ002
157500970526     C                   MOVEL     OPV(11)       OPZ003
157600970526     C                   MOVEL     OPV(12)       OPZ004
157700970526     C                   MOVEL     OPV(13)       OPZ005
157800970526     C                   MOVEL     OPV(14)       OPZ006
157900970526     C                   MOVEL     OPV(15)       OPZ007
158000970526     C                   MOVEL     OPV(16)       OPZ008
158100001020     C                   ENDIF
158200970526     C*
158300970526     C* TERZO GRUPPO DI 8 OPZIONI
158400970526     C     OPZGRU        IFEQ      3
158500970526     C                   MOVEL     OPV(17)       OPZ001
158600970526     C                   MOVEL     OPV(18)       OPZ002
158700970526     C                   MOVEL     OPV(19)       OPZ003
158800970526     C                   MOVEL     OPV(20)       OPZ004
158900970526     C                   MOVEL     OPV(21)       OPZ005
159000970526     C                   MOVEL     OPV(22)       OPZ006
159100970526     C                   MOVEL     OPV(23)       OPZ007
159200970526     C                   MOVEL     OPV(24)       OPZ008
159300001020     C                   ENDIF
159400970526     C*
159500970526     C                   ENDSR
159600970526     C*------------------------------------------------------------------------*
159700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
159800970526     C*------------------------------------------------------------------------*
159900970526     C     OPZCON        BEGSR
160000970526     C*
160100970526     C* PULISCO CAMPI DI LAVORO
160200970526     C                   Z-ADD     0             I                 5 0
160300970526     C                   Z-ADD     0             W                 5 0
160400970526     C*
160500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
160600970526     C     1             DO        24            I
160700970526     C                   MOVEL     OPD(I)        OPDRIG
160800970526     C*
160900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
161000970526     C     OPZKEY        IFEQ      OPDOPZ
161100970526     C     OPZKEY        OREQ      OPDOP1
161200970526     C*
161300970526     C     OPDATT        IFEQ      'S'
161400970526     C                   MOVEL     OPDOPZ        OPZKEY
161500970526     C                   Z-ADD     I             W
161600001020     C                   ENDIF
161700001020     C                   ENDIF
161800970526     C*
161900001020     C                   ENDDO                                                  DO WHILE
162000970526     C*
162100970526     C* OPZIONE NON ATTIVA (ERRORE)
162200970526     C     W             IFEQ      0
162300970526     C*
162400970526     C* OPZIONE NON OK
162500970526     C                   Z-ADD     1             OPZOK
162600970526     C*
162700970526     C* EMISSIONE MESSAGGIO DI ERRORE
162800970526     C                   MOVEL     MSG(1)        DSMSMS
162900970526     C                   ELSE
163000970526     C*
163100970526     C* OPZIONE ATTIVA (PROCEDI ...)
163200970526     C*
163300970526     C     OPZKEY        IFEQ      ' 1'
163400970526     C                   EXSR      O01GES
163500001020     C                   ENDIF
163600970526     C     OPZKEY        IFEQ      ' 2'
163700970526     C                   EXSR      O02GES
163800001020     C                   ENDIF
163900001020     C*    OPZKEY        IFEQ      ' 3'
164000001020     C*                  EXSR      O03GES
164100001020     C*                  ENDIF
164200991104     C*    OPZKEY        IFEQ      ' 4'
164300991104     C*                  EXSR      O04GES
164400001020     C*                  ENDIF
164500970526     C     OPZKEY        IFEQ      ' 5'
164600970526     C                   EXSR      O05GES
164700001020     C                   ENDIF
164800001020     C     OPZKEY        IFEQ      ' 6'
164900001020     C                   EXSR      O06GES
165000001020     C                   ENDIF
165100001020     C*    OPZKEY        IFEQ      ' 7'
165200001020     C*                  EXSR      O07GES
165300001020     C*                  ENDIF
165400010810     C     OPZKEY        IFEQ      ' 8'
165500010810     C                   EXSR      O08GES
165600010810     C                   ENDIF
165700001020     C*    OPZKEY        IFEQ      ' 9'
165800001020     C*                  EXSR      O09GES
165900001020     C*                  ENDIF
166000001020     C*    OPZKEY        IFEQ      '10'
166100001020     C*                  EXSR      O10GES
166200001020     C*                  ENDIF
166300001020     C                   ENDIF                                                  OPZ.NON ATTIVA
166400970526     C*
166500970526     C                   ENDSR
166600970526     C*------------------------------------------------------------------------*
166700970526     C* NOTE COSTRUZIONE SCHIERE
166800970526     C*------------------------------------------------------------------------*
166900970526     C*
167000970526     C* -MSG-MESSAGGI
167100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
167200970526     C* (NON TOCCARE MSG,1 E MSG,2)
167300970526     C*
167400970526     C* -CMD-COMANDI
167500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
167600970526     C*
167700970526     C* -OPD-OPZIONI
167800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
167900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
168000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
168100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
168200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
168300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
168400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
168500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
168600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
168700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
168800970526     C*
168900970526     C* -FUD-FUNZIONI
169000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
169100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
169200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
169300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
169400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
169500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
169600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
169700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
169800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
169900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
170000970526     O*------------------------------------------------------------------------*
170100970526** DEC - DECODIFICHE
170200970526    GESTIONE                                                     1
170300970526INTERROGAZIONE                                                   2
170400970530     SCELTA                                                      3
170500001020Inv
170600001020Da inv
170700090619Rtv
170800970526** MSG - MESSAGGI
170900120528TIS736R- Opzione non gestita                                                   1
171000120528TIS736R- Tasto funzionale non gestito                                          2
171100120528TIS736R- Inserimento consentito solo in Sede.                                  3
171200120528TIS736R- Utente Strategi: non abilitato al servizio 'TT'                       4
171300120528TIS736R- Utente Strategi: abilitazione al servizio 'TT' scaduta                5
171400120528TIS736R-                                                                       6
171500120528TIS736R- 7                                                                     7
171600120528TIS736R- 8                                                                     8
171700120528TIS736R- Tipo servizio errato o inesistente.                                   9
171800120528TIS736R- 10                                                                   10
171900120528TIS736R-                                                                      11
172000120528TIS736R-                                                                      12
172100120528TIS736R- 13                                                                   13
172200120528TIS736R- 14                                                                   14
172300120528TIS736R- 15                                                                   15
172400120528TIS736R- 16                                                                   16
172500120528TIS736R- 17                                                                   17
172600120528TIS736R- 18                                                                   18
172700120528TIS736R- 19                                                                   19
172800120528TIS736R- 20                                                                   20
172900970528** CMD - COMANDI CL
173000970528WRKSPLF
173100970528
173200970528
173300970528
173400970528
173500970526** OPD - OPZIONI
173600970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
173700010810 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
173800991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
173900991104 4=                -4 -     (S/N)=N                        opz. 04¬
174000970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
174100001020 6=Sflegga invio   -6 -     (S/N)=N                        opz. 06¬
174200970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
174300010810 8=Modifica uso EDP-8 -     (S/N)=N                        opz. 08¬
174400991028 9=Cliente Unif.   -9 -     (S/N)=N                        opz. 09¬
17450000070310=Legami tipi file-10-     (S/N)=N                        opz. 10¬
174600000703                   -  -     (S/N)=N                        opz. 11¬
174700970526                   -  -     (S/N)=N                        opz. 12¬
174800970526                   -  -     (S/N)=N                        opz. 13¬
174900970526                   -  -     (S/N)=N                        opz. 14¬
175000970526                   -  -     (S/N)=N                        opz. 15¬
175100970526                   -  -     (S/N)=N                        opz. 16¬
175200970526                   -  -     (S/N)=N                        opz. 17¬
175300970526                   -  -     (S/N)=N                        opz. 18¬
175400970526                   -  -     (S/N)=N                        opz. 19¬
175500970526                   -  -     (S/N)=N                        opz. 20¬
175600970526                   -  -     (S/N)=N                        opz. 21¬
175700970526                   -  -     (S/N)=N                        opz. 22¬
175800970526                   -  -     (S/N)=N                        opz. 23¬
175900970526                   -  -     (S/N)=N                        opz. 24¬
176000970526** FUD - FUNZIONI
176100970526                   -        (S/N)=N                        funz.01¬
176200970526                   -        (S/N)=N                        funz.02¬
176300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
176400970526                   -        (S/N)=N                        funz.04¬
176500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
176600970526F06=Inserimento    -        (S/N)=N                        funz.06¬
176700970526                   -        (S/N)=N                        funz.07¬
176800970527                   -        (S/N)=N                        funz.08¬
176900120529F09=Altri dati     -        (S/N)=S                        funz.09¬
177000970527                   -        (S/N)=N                        funz.10¬
177100970526                   -        (S/N)=N                        funz.11¬
177200970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
177300970526                   -        (S/N)=N                        funz.13¬
177400970526                   -        (S/N)=N                        funz.14¬
177500970526                   -        (S/N)=N                        funz.15¬
177600970526                   -        (S/N)=N                        funz.16¬
177700970526                   -        (S/N)=N                        funz.17¬
177800970526                   -        (S/N)=N                        funz.18¬
177900970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
178000970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
178100980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
178200970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
178300970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
178400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
