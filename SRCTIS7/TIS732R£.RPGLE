000100970526     H*------------------------------------------------------------------------*
000200001020     H* Gestione - Estensione anagrafica clienti abilitati
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800001020     FTIS732D   CF   E             WORKSTN INFDS(DEVDS1)
000900001020     F                                     SFILE(S732D05:SFLULT)
001000001020     F                                     SFILE(S732DMS:MSGULT)
001100970526     F*------------
001200001020     Ftiabl01l  UF   E           K DISK    rename(tiabl000:tiabl001)
001300001020     Ftiabl02l  IF   E           K DISK    rename(tiabl000:tiabl002)
001400001020     Ftiabl03l  IF   E           K DISK    rename(tiabl000:tiabl003)
001500001020     Ftianv01l  IF   E           K DISK
001600060731     Ftivss01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300090619     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970528     D*------------
002900970528     D* COMANDI
003000970528     D*------------
003100970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970527     D FUV             S             19    DIM(24)
004800970526     D*------------------------------------------------------------------------*
004900970526     D* INPUT
005000970526     D*------------------------------------------------------------------------*
005100970526     D*------------
005200970526     D* RIDEFINIZIONE OPZIONI VIDEO
005300970526     D*------------
005400970526     D OPDDS           DS
005500970526     D  OPDOPZ                 1      2
005600970526     D  OPDDE1                 3     19
005700970526     D  OPDOP1                21     22
005800970526     D  OPDFI0                23     34
005900970526     D  OPDATT                35     35
006000970526     D  OPDFI1                36     80
006100970526     D  OPDDES                 1     19
006200970526     D  OPDRIG                 1     80
006300970526     D*------------
006400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006500970526     D*------------
006600970526     D FUDDS           DS
006700970526     D  FUDDE1                 1      1
006800970526     D  FUDFUN                 2      3
006900970526     D  FUDDE2                 4     19
007000970526     D  FUDFI0                20     34
007100970526     D  FUDATT                35     35
007200970526     D  FUDFI1                36     80
007300970526     D  FUDDES                 1     19
007400970526     D  FUDRIG                 1     80
007500970526     D*------------
007600970526     D* DS GESTIONE TASTI FUNZIONALI
007700970526     D*------------
007800970526     D DSKEY         E DS
007900970526     D DEVDS1          DS
008000970526     D  KEY                  369    369
008100970526     D*------------
008200970526     D* DS "XMSG" - GESTIONE MESSAGGI
008300970526     D*------------
008400970526     D DSMSG         E DS
008500970526     D*------------
008600020916     D* DS REPERIMENTO DATI UTENTE
008700970526     D*------------
008800020916     D TIBS34DS      E DS                                                       *Profili utente
008900020916     D DDATIUTE      E DS                                                       *Dati utente
009000020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009100001020     D*------------
009200001020     D* DS - REPERIMENTO TABELLE
009300001020     D*------------
009400991108     D TIBS02DS      E DS
009500991122     D*------------------
009600991122     D* DS REPERIMENTO DATI UTENTE
009700991122     D*-------------------
009800991122     DBS69DS         E DS                  extname(TIBS69DS)
009900991122     DACODS          E DS                  extname(CNACO00F)
010000991122     DINDDS          E DS                  extname(CNIND00F)
010100991122     DCLPDS          E DS                  extname(CNCLP00F)
010200991122     DCLSDS          E DS                  extname(FNCLS00F)
010300060731     D*------------------
010400060731     D* DS "XSRDA8" - CONTROLLA DATA (8)
010500060731     D*------------------
010600060731     D WLBDA8          DS                  INZ
010700060731     D  G08DAT                 1      8  0
010800060731     D  G08INV                 9     16  0
010900060731     D  G08ERR                17     17
011000060731     D  G08TGI                18     22  0
011100970527     D*------------------
011200970526     D* DS DI PROCEDURA
011300001020     D*------------
011400001020     D tis732        e ds                  extname(tis732ds)
011500970526     D*------------
011600970526     D* ARCHITETTURA
011700970526     D*------------
011800970526     D KPJBA         E DS
011900120103     D*------------
012000120103     D WVIDRag         s                   like(VIDSRS)
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400111229     C*
012500111229     C* inizializzo selezioni con dati in DS
012600111229     C                   EXSR      InzD03
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
015200001020     C                   MOVEL     '1'           s32ERR
015300001020     C                   MOVEL     DSMSMS        s32MSG
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
016600001020     C                   MOVEL     '1'           s32F03                         *DI PROCEDURA
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
018900001020     C                   MOVEL     vihsun        s32sun
019000001020     C                   MOVEL     vidisv        s32isv
019100001020     C                   MOVEL     vidtip        s32tip
019200970602     C                   MOVEL     DEPOP0        WOP0
019300970529     C                   MOVEL     'F06'         WOP1
019400001020     C                   MOVEL     'TIS733R'     WPGM              8
019500970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019700001020     C                   ENDIF
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F12GES - GESTIONE F12 -> RITORNO
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F12GES        BEGSR
020400970526     C*
020500970526     C* IMPOSTA "PREMUTO F12"
020600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020700001020     C                   MOVEL     '1'           s32F12                         *DI PROCEDURA
020800970526     C*
020900970526     C                   ENDSR
021000970528     C*------------------------------------------------------------------------*
021100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021200970528     C*------------------------------------------------------------------------*
021300970528     C     F21GES        BEGSR
021400970612     C*
021500970528     C*
021600970528     C                   ENDSR
021700970528     C*------------------------------------------------------------------------*
021800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
021900970528     C*------------------------------------------------------------------------*
022000970528     C     F22GES        BEGSR
022100970528     C*
022200970528     C                   Z-ADD     80            LENGH            15 5
022300970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
022400970528     C                   PARM                    CMD(1)
022500970528     C                   PARM                    LENGH
022600970528     C*
022700970528     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023000970526     C*------------------------------------------------------------------------*
023100970526     C     F23GES        BEGSR
023200970526     C*
023300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023800970526     C*------------------------------------------------------------------------*
023900970526     C     F24GES        BEGSR
024000970526     C*
024100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024200970526     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024600970526     C*------------------------------------------------------------------------*
024700970526     C     RUPGES        BEGSR
024800970526     C*
024900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* ENTGES - GESTIONE ENTER -> INVIO
025400970526     C*------------------------------------------------------------------------*
025500970526     C     ENTGES        BEGSR
025600000703     C*
025700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025800970526     C                   EXSR      NEWPOS
025900970526     C*
026000970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026100001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026200001020     C     SWPOSI        IFNE      0
026300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026400001020     C                   ELSE
026500001020     C     SFLULT        IFNE      0
026600970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026700970526     C*
026800970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026900001020     C     SWMODI        IFNE      0
027000970526     C                   EXSR      POSSFL
027100001020     C                   ENDIF
027200001020     C                   ENDIF
027300001020     C                   ENDIF
027400001020     C                   ENDIF
027500970526     C*
027600970526     C                   ENDSR
027700000703     C*------------------------------------------------------------------------*
027800001020     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027900970526     C*------------------------------------------------------------------------*
028000970526     C     O01GES        BEGSR
028100970526     C*
028200970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028300001020     C                   MOVEL     vihsun        s32sun
028400001020     C                   MOVEL     vidisv        s32isv
028500001020     C                   MOVEL     vidtip        s32tip
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
029600001020     C                   MOVEL     vihsun        s32sun
029700001020     C                   MOVEL     vidisv        s32isv
029800001020     C                   MOVEL     vidtip        s32tip
029900970602     C                   MOVEL     DEPOP0        WOP0
030000970526     C                   MOVEL     'O02'         WOP1
030100001020     C                   MOVEL     'TIS733R'     WPGM
030200970526     C                   EXSR      LANPGM
030300060801     C                   Z-ADD     1             SWMODI
030400001020     C                   ENDIF
030500970526     C*
030600970526     C                   ENDSR
030700001020     C*------------------------------------------------------------------------*
030800001020     C* O06GES - GESTIONE OPZIONE 6=SFLEGGA X RE-INVIO
030900001020     C*------------------------------------------------------------------------*
031000001020     C     O06GES        BEGSR
031100060731     C*
031200170116     C* VERIFICA SE CLIENTE ABILITATO AL SERVIZIO
031300060731     C                   EVAL      VSSsun = VIDsun
031400170116     C                   EVAL      VSSisv = VIDisv
031500060731     C     KEYvss        CHAIN     tivss01l
031600060731     C                   IF        %found(tivss01l)
031700060731     C                   IF        DATCOR >= VSSdde AND DATCOR <= VSSdsc
031800060731     C                   ELSE
031900060731     C                   MOVEL     '1'           ERRL00
032000060731     C                   MOVEL     MSG(5)        DSMSMS
032100060731     C                   ENDIF
032200060731     C                   ELSE
032300060731     C                   MOVEL     '1'           ERRL00
032400060731     C                   MOVEL     MSG(4)        DSMSMS
032500060731     C                   ENDIF
032600001020     C*
032700001020     C* SLEGGA PER RE-INVIO
032800001020     C     ERRL00        IFEQ      '0'
032900001020     C                   MOVEL     vihsun        KABsun
033000001020     C                   MOVEL     vidisv        KABisv
033100001020     C                   MOVEL     vidtip        KABtip
033200001020     C     KEYabl        CHAIN     tiabl01l
033300001020     C                   IF        %found
033400001020     C                   MOVEL     '0'           ablinv
033500001020     C                   UPDATE    tiabl001
033600001020     C                   ENDIF
033700060801     C                   Z-ADD     1             SWMODI
033800001020     C                   ENDIF
033900001020     C*
034000001020     C                   ENDSR
034100991014     C*------------------------------------------------------------------------*
034200991014     C* O03GES - GESTIONE OPZIONE 3=copia
034300991014     C*------------------------------------------------------------------------*
034400991014     C     O03GES        BEGSR
034500991014     C*
034600991014     C*
034700991014     C                   ENDSR
034800970526     C*------------------------------------------------------------------------*
034900970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
035000970526     C*------------------------------------------------------------------------*
035100001020     C     O04GES        BEGSR
035200970526     C*
035300970526     C*
035400001020     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600001020     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
035700970526     C*------------------------------------------------------------------------*
035800970526     C     O05GES        BEGSR
035900970526     C*
036000001020     C                   MOVEL     vihsun        s32sun
036100001020     C                   MOVEL     vidisv        s32isv
036200001020     C                   MOVEL     vidtip        s32tip
036300970602     C                   MOVEL     DEPOP0        WOP0
036400970526     C                   MOVEL     'O05'         WOP1
036500001020     C                   MOVEL     'TIS733R'     WPGM
036600970526     C                   EXSR      LANPGM
036700970526     C*
036800970526     C                   ENDSR
036900970526     C*------------------------------------------------------------------------*
037000970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
037100970526     C*------------------------------------------------------------------------*
037200970526     C     O07GES        BEGSR
037300970526     C*
037400970526     C*
037500970526     C                   ENDSR
037600010810     C*------------------------------------------------------------------------*
037700010810     C* O08GES - GESTIONE OPZIONE 8=MODIFICA USO EDP
037800010810     C*------------------------------------------------------------------------*
037900010810     C     O08GES        BEGSR
038000060801     C*
038100170116     C* VERIFICA SE CLIENTE ABILITATO AL SERVIZIO
038200060801     C                   EVAL      VSSsun = VIDsun
038300170116     C                   EVAL      VSSisv = VIDisv
038400060801     C     KEYvss        CHAIN     tivss01l
038500060801     C                   IF        %found(tivss01l)
038600060801     C                   IF        DATCOR >= VSSdde AND DATCOR <= VSSdsc
038700060801     C                   ELSE
038800060801     C                   MOVEL     '1'           ERRL00
038900060801     C                   MOVEL     MSG(5)        DSMSMS
039000060801     C                   ENDIF
039100060801     C                   ELSE
039200060801     C                   MOVEL     '1'           ERRL00
039300060801     C                   MOVEL     MSG(4)        DSMSMS
039400060801     C                   ENDIF
039500010810     C*
039600010810     C* MODIFICA
039700010810     C     ERRL00        IFEQ      '0'
039800010810     C                   MOVEL     vihsun        s32sun
039900010810     C                   MOVEL     vidisv        s32isv
040000010810     C                   MOVEL     vidtip        s32tip
040100010810     C                   MOVEL     DEPOP0        WOP0
040200010810     C                   MOVEL     'O08'         WOP1
040300010810     C                   MOVEL     'TIS733R'     WPGM
040400010810     C                   EXSR      LANPGM
040500060801     C                   Z-ADD     1             SWMODI
040600010810     C                   ENDIF
040700010810     C*
040800010810     C                   ENDSR
040900970526     C*------------------------------------------------------------------------*
041000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
041100970526     C*------------------------------------------------------------------------*
041200970526     C     NEWPOS        BEGSR
041300970526     C*
041400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
041500970526     C*         1= RICHIESTO POSIZIONAMENTO
041600970526     C                   Z-ADD     0             SWPOSI            7 0
041700970526     C*
041800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
041900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
042000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
042100970526     C                   Z-ADD     0             SWMODI            1 0
042200970530     C*
042300970530     C* REIMPOSTA INDICATORI DI ERRORE
042400970530     C                   SETOFF                                       808182
042500970530     C                   SETOFF                                       83
042600970526     C*
042700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
042800001020     C     VIDti2        IFNE      DEPti2
042900001020     C     VIDks2        ORNE      DEPks2
043000001020     C     VIDks1        ORNE      DEPks1
043100001020     C     VIDti1        ORNE      DEPti1
043200001020     C     VIDsr1        ORNE      DEPsr1
043300001020     C     VIDin1        ORNE      DEPin1
043400010919     C     VIDeml        ORNE      DEPeml
043500120102     C     VIDSU1        ORNE      DEPSU1
043600120103     C     VIDSRS        ORNE      DEPSRS
043700970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
043800001020     C     ERRL00        IFEQ      '0'                                          *NO ERORI
043900970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
044000001020     C                   Z-ADD     VIDks2        DEPks2                         *DEPOSITI
044100991025     C                   MOVEL     VIDti2        DEPti2                         *DEPOSITI
044200111229     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
044300991025     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
044400000801     C                   MOVEL     VIDsr1        DEPsr1                         *DEPOSITI
044500001020     C                   MOVEL     VIDin1        DEPin1                         *DEPOSITI
044600010919     C                   MOVEL     VIDeml        DEPeml                         *DEPOSITI
044700120102     C                   MOVEL     VIDSU1        DEPSU1                         Codice Strategi
044800120103     C                   MOVEL     VIDSRS        DEPSRS                         Ragione sociale
044900001020     C                   ENDIF
045000001020     C                   ENDIF
045100970526     C*
045200970526     C                   ENDSR
045300970526     C*------------------------------------------------------------------------*
045400001020     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
045500970526     C*------------------------------------------------------------------------*
045600970528     C     SETFIL        BEGSR
045700970526     C*
045800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
045900111229     C*
046000111229     C                   MOVE      VIDks1        KABute
046100120102     C* se il campo di filtro sul cliente è tutti zeri lo trasformo in tutti blank
046200120102     C* (normalmente è un numerico per questo resta il CHECK(RZ))
046300120102     C                   IF        KABUte = '0000000'
046400120102     C                   EVAL      KABUte = *blank
046500120102     C                   ENDIF
046600111229     C                   MOVEL     VIDti1        KABtip
046700001020     C                   MOVEL     *LOVAL        KABisv
046800111229     C                   MOVEL     VIDSU1        KABSUN
046900120102     C* se il campo di filtro sull'utente Strategi è tutti zeri lo trasformo in tutti blank
047000120102     C* (normalmente è un numerico per questo resta il CHECK(RZ))
047100120102     C                   IF        KABSUN = *zeros
047200120102     C                   EVAL      KABSUN = *blank
047300120102     C                   ENDIF
047400001020     C     WTIPRI        IFEQ      'TIP'
047500001020     C     KE3abl        SETLL     tiabl03l                           99
047600001020     C                   ENDIF
047700001020     C     WTIPRI        IFEQ      'KSC'
047800001020     C     KE2abl        SETLL     tiabl02l                           99
047900001020     C                   ENDIF
048000111229     C     WTIPRI        IFEQ      'SUN'
048100111229     C     KEYabl        SETLL     tiabl01l                           99
048200111229     C                   ENDIF
048300980107     C*
048400970526     C* LETTURA RECORD
048500001020     C     *IN99         IFEQ      *OFF
048600970526     C                   EXSR      LETFIL
048700001020     C                   ENDIF
048800970526     C*
048900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
049000001020     C     *IN99         IFEQ      *ON
049100970526     C                   Z-ADD     1             SFLEOF
049200001020     C                   ENDIF
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600970526     C* LETFIL - LETTURA RECORD
049700970526     C*------------------------------------------------------------------------*
049800970526     C     LETFIL        BEGSR
049900970526     C*
050000970526     C* LEGGE FINO A:
050100970526     C*     - FINE FILE
050200970526     C*     - TROVATO RECORD VALIDO
050300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
050400001020     C     *IN99         DOWEQ     *OFF
050500970526     C     WRECOK        ANDEQ     'N'
050600970526     C*
050700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
050800001020     C     WTIPRI        IFEQ      'TIP'
050900111229     C     KABTip        READE     tiabl03l                               99
051000001020     C                   ENDIF
051100001020     C     WTIPRI        IFEQ      'KSC'
051200111229     C* se il codice cliente non è stato valorizzato
051300111229     C     KABUte        IFEQ      *blank
051400111229     C* allora il tipo ricerca è fittizio e devo leggere tutto
051500111229     C                   READ      tiabl02l                               99
051600111229     C                   ELSE
051700111229     C     KABUte        READE     tiabl02l                               99
051800001020     C                   ENDIF
051900111229     C                   ENDIF
052000111229     C     WTIPRI        IFEQ      'SUN'
052100120102     C* se l'utente Strategi non è stato valorizzato
052200120102     C     KABSUN        IFEQ      *blank
052300120102     C* allora il tipo ricerca è fittizio e devo leggere tutto
052400120102     C                   READ      tiabl01l                               99
052500120102     C                   ELSE
052600111229     C     KABSUN        READE     tiabl01l                               99
052700111229     C                   ENDIF
052800120102     C                   ENDIF
052900970528     C*
053000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053100001020     C     *IN99         IFEQ      *ON                                          *FINE FILE
053200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
053300001020     C                   ELSE
053400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
053500001020     C                   ENDIF
053600001020     C                   ENDDO
053700970526     C*
053800970526     C                   ENDSR
053900970526     C*------------------------------------------------------------------------*
054000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
054100970526     C*------------------------------------------------------------------------*
054200970526     C     CHKREC        BEGSR
054300970526     C*
054400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054500060727     C* Filtra se impostato codice cliente da video
054600111229     C     vidks1        IFEQ      *blank
054700120102     C     vidks1        OREQ      *zeros
054800001020     C                   ELSE
054900001020     C                   MOVE(p)   vidks1        wrkcli7           7
055000001020     C     wrkcli7       IFNE      ablute
055100991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055200001020     C                   ENDIF
055300001020     C                   ENDIF
055400111229     C* Filtra se impostato Strategi User Number da video
055500111229     C     VIDSU1        IFEQ      *blanks
055600120102     C     vidSU1        OREQ      *zeros
055700060727     C                   ELSE
055800111229     C     VIDSU1        IFNE      ablsun
055900060727     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056000060727     C                   ENDIF
056100060727     C                   ENDIF
056200111229     C* Filtra se impostato tipo servizio da video
056300001020     C     vidsr1        IFEQ      *blanks
056400001020     C                   ELSE
056500001020     C     vidsr1        IFNE      ablisv
056600000801     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056700001020     C                   ENDIF
056800001020     C                   ENDIF
056900111229     C* Filtra se impostato tipo testo da video
057000001020     C     vidti1        IFEQ      *blanks
057100001020     C                   ELSE
057200001020     C     vidti1        IFNE      abltip
057300001020     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057400001020     C                   ENDIF
057500001020     C                   ENDIF
057600111229     C* Filtra se impostato stato invio da video
057700001020     C     vidin1        IFEQ      *blanks
057800001020     C                   ELSE
057900001020     C     vidin1        IFNE      ablinv
058000001020     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058100001020     C                   ENDIF
058200001020     C                   ENDIF
058300010919     C*
058400111229     C* Filtra se impostata una e-mail da video
058500010919     C                   if        WRECOK = 'S'
058600010919     C     videml        ifeq      *blanks
058700010919     C                   else
058800010919     C                   z-add     *zeros        pos               2 0
058900010919     C                   eval      pos = %scan(%trim(videml):ableml)
059000010919     C                   if        pos = *zeros
059100010919     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059200010919     C                   endif
059300010919     C                   endif
059400010919     C                   endif
059500120103     C*
059600120103     C* Filtra se impostata una ragione sociale da video
059700120103     C                   if        WRECOK = 'S'
059800120103     C                   exsr      RepSRS
059900120103     C     VIDsrs        ifeq      *blanks
060000120103     C                   else
060100120103     C                   z-add     *zeros        pos               2 0
060200120103     C                   eval      pos = %scan(%trim(VIDsrs):WVIDRag)
060300120103     C                   if        pos = *zeros
060400120103     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
060500120103     C                   endif
060600120103     C                   endif
060700120103     C                   endif
060800970526     C*
060900970526     C                   ENDSR
061000970526     C*------------------------------------------------------------------------*
061100001020     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
061200970526     C*------------------------------------------------------------------------*
061300970526     C     IRESFL        BEGSR
061400970526     C*
061500970526     C                   MOVEL     *BLANKS       OPZKEY
061600001020     C                   MOVEL     ablsun        VIHsun
061700060727     C                   MOVEL     ablsun        VIDsun
061800001020     C                   MOVEL     abltip        VIDtip
061900001020     C                   MOVEL     ablute        VIDksc
062000120103     C                   MOVEL     WVIDRag       VIDRag
062100120103     C**reperimento "Ragione Sociale" del Cliente Unificante
062200120103     C**se il codice cliente è un numero (il che non è sempre vero)
062300120103     C**                 TESTN                   VIDKsc               21
062400120103     C**                 IF        *in21 = *on
062500120103     C**                 CLEAR                   BS69DS
062600120103     C**                 CLEAR                   ACODS
062700120103     C**                 MOVEL     KNSIF         I69SIF
062800120103     C**                 MOVE      VIDksc        I69KAC
062900120103     C**                 CALL      'TIBS69R'
063000120103     C**                 PARM                    BS69DS
063100120103     C**                 PARM                    ACODS
063200120103     C**                 PARM                    INDDS
063300120103     C**                 PARM                    CLPDS
063400120103     C**                 PARM                    CLSDS
063500120103     C**se il cod.cliente non è numerico simulo che sia andata male la chiamata al driver cosicche
063600120103     C**si cerchi la ragione sociale su TIANV dove il cliente può essere alfanumerico
063700120103     C**                 ELSE
063800120103     C**                 EVAL      O69ERR = '1'
063900120103     C**                 ENDIF
064000120103     C**
064100120103     C**   O69ERR        IFNE      '1'
064200120103     C**                 MOVEL     ACORAG        VIDRAG
064300120103     C**                 ELSE
064400120103     C**                 MOVE(p)   VIDksc        wrkcli8           8
064500120103     C**   wrkcli8       CHAIN     tianv01l
064600120103     C**                 IF        %found(tianv01l)
064700120103     C**                 MOVEL(p)  anvdes        VIDRAG
064800120103     C**                 ELSE
064900120103     C**                 MOVEL     *ALL'*'       VIDRAG
065000120103     C**                 ENDIF
065100120103     C**                 ENDIF
065200001020     C                   MOVEL     ablisv        VIDisv
065300001020     C                   MOVEL     ablinv        VIDinv
065400090619     C                   SELECT
065500090619     C                   WHEN      ablinv = '1'
065600001020     C                   MOVEL     DEC(4)        VDDinv
065700090619     C                   WHEN      ablinv = '0'
065800001020     C                   MOVEL     DEC(5)        VDDinv
065900090619     C                   WHEN      ablinv = '2'
066000090619     C                   MOVEL     DEC(6)        VDDinv
066100090619     C                   ENDSL
066200001020     C*
066300970526     C                   ENDSR
066400120103     C*------------------------------------------------------------------------*
066500120103     C* REPSRS - Reperisce la Ragione Sociale
066600120103     C*------------------------------------------------------------------------*
066700120103     C     REPSRS        BEGSR
066800120103     C*
066900120103     C                   EVAL      WVIDRag = *blanks
067000120103     C*
067100120103     C* reperimento "Ragione Sociale" del Cliente Unificante
067200120103     C* se il codice cliente è un numero (il che non è sempre vero)
067300120103     C                   TESTN                   ABLUte               21
067400120103     C                   IF        *in21 = *on
067500120103     C                   CLEAR                   BS69DS
067600120103     C                   CLEAR                   ACODS
067700120103     C                   MOVEL     KNSIF         I69SIF
067800120103     C                   MOVE      ABLUte        I69KAC
067900120103     C                   CALL      'TIBS69R'
068000120103     C                   PARM                    BS69DS
068100120103     C                   PARM                    ACODS
068200120103     C                   PARM                    INDDS
068300120103     C                   PARM                    CLPDS
068400120103     C                   PARM                    CLSDS
068500120103     C* se il cod.cliente non è numerico simulo che sia andata male la chiamata al driver cosicche
068600120103     C* si cerchi la ragione sociale su TIANV dove il cliente può essere alfanumerico
068700120103     C                   ELSE
068800120103     C                   EVAL      O69ERR = '1'
068900120103     C                   ENDIF
069000120103     C*
069100120103     C     O69ERR        IFNE      '1'
069200120103     C                   MOVEL     ACORAG        WVIDRAG
069300120103     C                   ELSE
069400120103     C                   MOVE(p)   ABLUte        wrkcli8           8
069500120103     C     wrkcli8       CHAIN     tianv01l
069600120103     C                   IF        %found(tianv01l)
069700120103     C                   MOVEL(p)  anvdes        WVIDRAG
069800120103     C                   ELSE
069900120103     C                   MOVEL     *ALL'*'       WVIDRAG
070000120103     C                   ENDIF
070100120103     C                   ENDIF
070200120103     C*
070300120103     C                   ENDSR
070400970526     C*------------------------------------------------------------------------*
070500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
070600970526     C*------------------------------------------------------------------------*
070700970526     C     CHKPAR        BEGSR
070800970528     C*
070900111229     C                   SELECT
071000111229     C* Se impostato lo S.U.N. (a prescindere dagli altri dati)
071100111229     C     VIDSU1        WHENNE    *blank
071200120103     C     VIDSU1        ANDNE     *zeros
071300111229     C* la ricerca è per S.U.N.
071400111229     C                   MOVEL     'SUN'         WTIPRI
071500111229     C* Se impostato il cliente (a prescindere dal tipo testo)
071600111229     C     VIDks1        WHENNE    *blank
071700120103     C     VIDks1        ANDNE     *zeros
071800111229     C* la ricerca è per cliente
071900111229     C                   MOVEL     'KSC'         WTIPRI
072000111229     C* Se impostato il tipo testo (e basta)
072100111229     C     VIDti1        WHENNE    *BLANKS
072200120103     C* la ricerca è per tipo testo
072300001020     C                   MOVEL     'TIP'         WTIPRI
072400111229     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CLIENTE
072500111229     C                   OTHER
072600111229     C                   MOVEL     'KSC'         WTIPRI
072700111229     C                   ENDSL
072800000801     C*
072900111229     C* se impostato il tipo servizio uso la chiave con TIPO RICERCA
073000120103     C*  P E R C H E'???
073100120103     C*  Non ce lo domandiamo più tanto è stato reso hidden il campo tipo servizio
073200001020     C                   SETOFF                                       89
073300001020     C     ERRL00        IFEQ      '0'
073400001020     C     VIDsr1        IFNE      *blanks
073500001020     C                   MOVEL     'TIP'         WTIPRI
073600001020     C                   MOVE      VIDsr1        wvidsr1           2
073700001020     C                   EXSR      CNTISV
073800000801     C                   IF        ERRL00 ='1'
073900001020     C                   SETON                                        89
074000001020     C                   ENDIF
074100001020     C                   MOVE      wvidsr1       VIDsr1
074200001020     C                   ENDIF
074300001020     C                   ENDIF
074400000801     C*
074500970526     C                   ENDSR
074600000801     C*------------------------------------------------------------------------*
074700001020     C* CNTISV - CONTROLLA CODICE TIPO SERVIZIO
074800000801     C*------------------------------------------------------------------------*
074900001020     C     CNTISV        BEGSR
075000000801     C*
075100001020     C                   CLEAR                   TIBS02DS
075200000801     C*
075300000801     C     '?'           SCAN      wvidsr1                                30
075400000801     C*  Ricerca
075500001020     C                   EVAL      t02tla = 'L'
075600001020     C                   IF        *IN30 = *ON
075700001020     C                   MOVEL     'R'           t02mod
075800001020     C                   ELSE
075900001020     C                   MOVEL     'C'           t02mod
076000001020     C                   MOVEL(p)  wvidsr1       t02ke1
076100001020     C                   ENDIF
076200001020     C                   MOVEL     'ISV'         t02cod
076300001020     C                   MOVEL     knsif         t02sif
076400000801     C                   CALL      'TIBS02R'
076500001020     C                   PARM                    KPJBA
076600001020     C                   PARM                    TIBS02DS
076700001020     C*
076800001020     C                   IF        T02err = *blanks
076900001020     C                   MOVEL     T02ke1        wvidsr1
077000001020     C                   ELSE
077100001020     C                   MOVEL     '1'           errl00
077200001020     C                   SETON                                        62
077300000801     C                   MOVEL     MSG(9)        DSMSMS
077400000801     C                   ENDIF
077500000801     C*
077600001020     C                   ENDSR
077700970526     C*------------------------------------------------------------------------*
077800970526     C* LANPGM - LANCIA UN PROGRAMMA
077900970526     C*------------------------------------------------------------------------*
078000970526     C     LANPGM        BEGSR
078100970526     C*
078200970526     C* IMPOSTA LA DS DI PROCEDURA
078300001020     C                   MOVEL     WOP0          s32OP0                         *LIVELLO PROCEDURA
078400001020     C                   MOVEL     WOP1          s32OP1                         *LIVELLO PGM
078500001020     C                   MOVEL     '0'           s32F03                         *PREMUTO F03=FINE
078600001020     C                   MOVEL     '0'           s32F12                         *PREMUTO F12=RITORNO
078700001020     C                   MOVEL     '0'           s32ERR                         *'1'=ERRORE
078800001020     C                   MOVEL     *BLANKS       s32MSG                         *MESSAGGIO DI RITORN
078900001020     C                   MOVEL(p)  tis732        kpjbu
079000001020     C*
079100001020     C                   CALL      WPGM
079200001020     C                   PARM                    KPJBA
079300970526     C*
079400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
079500001020     C     s32F03        IFEQ      '1'                                          *PREMUTO F03
079600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
079700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
079800001020     C                   ELSE                                                   *NON PREMUTO F03
079900001020     C                   MOVEL     s32MSG        DSMSMS                         *MESSAGGIO DI RITORN
080000001020     C                   ENDIF
080100060801     C*
080200060801     C                   MOVEL     *blanks       s32sun
080300120103     C                   MOVEL     *blanks       s32isv
080400060801     C                   MOVEL     *blanks       s32tip
080500970526     C*
080600970526     C                   ENDSR
080700970526     C*------------------------------------------------------------------------*
080800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
080900970526     C*------------------------------------------------------------------------*
081000970526     C     CHKFOP        BEGSR
081100970626     C*
081200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
081300001020     C     DEPOP0        IFEQ      'P01'
081400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
081500970626     C                   MOVEL     'S'           OPDATT
081600970626     C                   MOVEL     OPDRIG        OPD(1)
081700001020     C                   ELSE
081800970626     C* OPZIONI
081900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
082000970626     C                   MOVEL     'S'           OPDATT
082100970626     C                   MOVEL     OPDRIG        OPD(5)
082200970626     C* FUNZIONI
082300001020     C                   ENDIF
082400970626     C*
082500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
082600001020     C     DEPOP0        IFEQ      'P02'
082700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
082800970626     C                   MOVEL     'S'           OPDATT
082900970626     C                   MOVEL     OPDRIG        OPD(2)
083000001020     C                   MOVEL     OPD(6)        OPDRIG                         *SFLEGGA INVIO
083100001020     C                   MOVEL     'S'           OPDATT
083200001020     C                   MOVEL     OPDRIG        OPD(6)
083300001020     C*                  MOVEL     OPD(3)        OPDRIG                         *MODIFICA
083400001020     C*                  MOVEL     'S'           OPDATT
083500001020     C*                  MOVEL     OPDRIG        OPD(3)
083600991104     C*                  MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
083700991104     C*                  MOVEL     'S'           OPDATT
083800991104     C*                  MOVEL     OPDRIG        OPD(4)
083900000711     C*                  MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
084000000711     C*                  MOVEL     'S'           OPDATT
084100000711     C*                  MOVEL     OPDRIG        OPD(7)
084200970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
084300970626     C                   MOVEL     'S'           FUDATT
084400970626     C                   MOVEL     FUDRIG        FUD(6)
084500001020     C*                  MOVEL     OPD(9)        OPDRIG                         *CLI. UNIF.
084600001020     C*                  MOVEL     'S'           OPDATT
084700001020     C*                  MOVEL     OPDRIG        OPD(9)
084800010810     C* SE L'UTENTE E' UN EDP
084900010810     C                   IF        %subst(%trim(KNMUS):1:3) = 'EDP'
085000010810     C                   MOVEL     OPD(8)        OPDRIG                         *CLI. UNIF.
085100010810     C                   MOVEL     'S'           OPDATT
085200010810     C                   MOVEL     OPDRIG        OPD(8)
085300010810     C                   ENDIF
085400001020     C                   ENDIF
085500970626     C*
085600970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
085700020916     C     DUTLPO        IFNE      'S'                                          *FILIALE
085800991104     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
085900991104     C*                  MOVEL     'N'           OPDATT
086000991104     C*                  MOVEL     OPDRIG        OPD(4)
086100001020     C*                  MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
086200001020     C*                  MOVEL     'N'           OPDATT
086300001020     C*                  MOVEL     OPDRIG        OPD(7)
086400970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
086500970626     C                   MOVEL     'N'           FUDATT
086600970626     C                   MOVEL     FUDRIG        FUD(6)
086700001020     C                   ENDIF
086800970526     C*
086900970526     C* GESTIONE OPZIONI A VIDEO
087000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
087100970526     C                   EXSR      OPZGES
087200970526     C*
087300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
087400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
087500970526     C                   EXSR      FUNGES
087600970526     C*
087700970526     C                   ENDSR
087800991122     C*------------------------------------------------------------------------*
087900991122     C* FINSR - OPERAZIONI FINALI
088000991122     C*------------------------------------------------------------------------*
088100991122     C     FINSR         BEGSR
088200991122     C*
088300991122     C* CHIUDE I FILE DEI PGM CHIAMATI
088400991122     C                   CLEAR                   BS69DS
088500991122     C                   CLEAR                   ACODS
088600991122     C                   CLEAR                   INDDS
088700991122     C                   CLEAR                   CLPDS
088800991122     C                   CLEAR                   CLSDS
088900991122     C                   MOVEL     'C'           I69TLA
089000991122     C                   MOVEL     KNSIF         I69SIF
089100991122     C                   CALL      'TIBS69R'
089200991122     C                   PARM                    BS69DS
089300991122     C                   PARM                    ACODS
089400991122     C                   PARM                    INDDS
089500991122     C                   PARM                    CLPDS
089600991122     C                   PARM                    CLSDS
089700991122     C*
089800991122     C                   ENDSR
089900020916     C*--------------------------------------------------------------------------------------------*
090000020916     C* REPERISCE I DATI UTENTE
090100020916     C*--------------------------------------------------------------------------------------------*
090200020916     C     REPDATIUTE    BEGSR
090300020916     C*
090400020916     C* INIZIALIZZA VARIABILI DI WRK
090500020916     C                   CLEAR                   TIBS34DS
090600020916     C                   CLEAR                   AZUTEDS
090700020916     C                   CLEAR                   DDATIUTE
090800020916     C*
090900020916     C     *DTAARA       DEFINE    §azute        azuteds
091000020916     C     *DTAARA       DEFINE    §datiute      ddatiute
091100020916     C                   IN(E)     *DTAARA
091200020916if  1C                   IF        %Error
091300020916     c                   EVAL      I34Tla = 'L'
091400020916     C                   CALL      'TIBS34R'
091500020916     C                   PARM                    Tibs34Ds
091600020916     C                   IN        *DTAARA
091700020916e   1C                   ENDIF
091800020916     C*
091900020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
092000020916     C                   MOVEL(P)  RSUT          DSFIRS           20
092100020916     C*
092200020916     C                   ENDSR
092300970526     C*------------------------------------------------------------------------*
092400001020     C* *INZSR - OPERAZIONI INIZIALI
092500970526     C*------------------------------------------------------------------------*
092600970526     C     *INZSR        BEGSR
092700970526     C*---
092800970526     C* RICEVIMENTO PARAMETRI
092900970526     C*---
093000970526     C     *ENTRY        PLIST
093100970612     C                   PARM                    KPJBA
093200020916     C                   MOVEL     KPJBU         TIS732
093300001020     C                   MOVEL     'KSC'         WTIPRI            3
093400060731     C*
093500060731     C* CALCOLA LA DATA CORRENTE
093600060731     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
093700060731     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
093800060731     C                   Z-ADD     WN8           G08DAT
093900060731     C                   Z-ADD     *ZEROS        G08INV
094000060731     C                   MOVEL     '0'           G08ERR
094100060731     C                   CALL      'XSRDA8'
094200060731     C                   PARM                    WLBDA8
094300060731     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
094400970526     C*
094500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
094600970526     C     XMSGPA        PLIST
094700970526     C                   PARM                    DSMSMS
094800970526     C                   PARM                    DSMSPG
094900970526     C                   PARM                    DSMSKE
095000970526     C                   PARM                    DSMSER
095100970526     C*
095200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
095300970526     C                   CALL      'XKEY'
095400970526     C                   PARM                    DSKEY
095500970526     C*---
095600970526     C* VARIABILI NON RIFERITE AL DATA BASE
095700970526     C*---
095800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
095900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
096000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
096100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
096200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
096300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
096400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
096500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
096600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
096700001020     C     *LIKE         DEFINE    s32OP0        DEPOP0                         *DI DEPOSITO
096800001020     C     *LIKE         DEFINE    s32OP1        DEPOP1
096900001020     C     *LIKE         DEFINE    s32OP0        WOP0                           *DI LAVORO
097000001020     C     *LIKE         DEFINE    s32OP1        WOP1
097100970526     C*---
097200001020     C     *LIKE         DEFINE    s32tip        DEPti2
097300001020     C     *LIKE         DEFINE    s32tip        DEPti1
097400001020     C     *LIKE         DEFINE    vidks2        DEPks2
097500001020     C     *LIKE         DEFINE    vidks1        DEPks1
097600001020     C     *LIKE         DEFINE    vidsr1        DEPsr1
097700001020     C     *LIKE         DEFINE    vidin1        DEPin1
097800010919     C     *LIKE         DEFINE    videml        DEPeml
097900120102     C     *LIKE         DEFINE    vidsu1        DEPsu1
098000120103     C     *LIKE         DEFINE    vidSRS        DEPSRS
098100001020     C*---
098200001020     C* VARIABILI RIFERITE AL DATA BASE
098300001020     C*---
098400001020     C     *LIKE         DEFINE    ablsun        KABsun
098500001020     C     *LIKE         DEFINE    ablute        kABute
098600001020     C     *LIKE         DEFINE    ablisv        KABisv
098700001020     C     *LIKE         DEFINE    abltip        KABtip
098800001020     C*---
098900970526     C* VARIABILI DI CONTROLLO
099000970526     C*---
099100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
099200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
099300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
099400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
099500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
099600970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
099700970526     C*
099800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
099900001020     C                   MOVEL     'TIS732R'     DSMSPG
100000001020     C                   MOVEL     'TIS732R'     MSGQUE
100100970526     C                   MOVEL     *BLANKS       MSGKEY
100200970526     C                   MOVEL     *BLANKS       DSMSMS
100300970526     C                   MOVEL     *BLANKS       DSMSKE
100400970526     C                   Z-ADD     *ZEROS        DSMSER
100500970526     C*---
100600970526     C* CHIAVI DI LETTURA
100700970526     C*---
100800001020     C* LETTURA tiabl01L -
100900001020     C     KEYabl        KLIST
101000001020     C                   KFLD                    KABsun
101100001020     C                   KFLD                    KABisv
101200001020     C                   KFLD                    KABtip
101300001020     C* LETTURA tiabl02L -
101400001020     C     KE2abl        KLIST
101500001020     C                   KFLD                    KABute
101600001020     C                   KFLD                    KABtip
101700001020     C                   KFLD                    KABisv
101800001020     C* LETTURA tiabl01L -
101900001020     C     KE3abl        KLIST
102000001020     C                   KFLD                    KABtip
102100001020     C                   KFLD                    KABute
102200001020     C                   KFLD                    KABisv
102300060731     C* LETTURA tivss01L -
102400060731     C     KEYvss        KLIST
102500060731     C                   KFLD                    VSSsun
102600060731     C                   KFLD                    VSSisv
102700970527     C*
102800970527     C* REPERIMENTO DATI UTENTE
102900020916     C                   EXSR      REPDATIUTE
103000970526     C*
103100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
103200001020     C                   MOVEL     s32OP0        DEPOP0                         *LIVELLO PROCEDURA
103300001020     C                   MOVEL     s32OP1        DEPOP1                         *LIVELLO PROGRAMMA
103400970526     C*
103500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
103600970526     C                   EXSR      CHKFOP
103700970526     C*
103800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
103900001020     C     DEPOP0        IFEQ      'P01'
104000970530     C                   MOVEL     DEC(3)        VIDING
104100001020     C                   ENDIF
104200001020     C     DEPOP0        IFEQ      'P02'
104300001020     C                   MOVEL     DEC(1)        VIDING
104400001020     C                   ENDIF
104500001020     C     DEPOP0        IFEQ      'P05'
104600970526     C                   MOVEL     DEC(2)        VIDING
104700001020     C                   ENDIF
104800970526     C*
104900970526     C                   ENDSR
105000970526     C*------------------------------------------------------------------------*
105100111229     C*
105200970526     C*------------------------------------------------------------------------*
105300111229     C     InzD03        BEGSR
105400970526     C*
105500111229     C* imposto dati da DS di ingresso
105600111229     C*
105700111229     C*  codice cliente
105800111229     C                   IF        %subst(S32KsU : 2: 7) <> *blank
105900111229     C                   MOVE      s32ksu        VIDKs1
106000111229     C                   ELSE
106100111229     C                   CLEAR                   VIDKs1
106200111229     C                   ENDIF
106300111229     C*  strategi user number
106400111229     C                   MOVEL     S32SUN        VIDSu1
106500111229     C*  tipo servizio
106600111229     C                   MOVEL     S32ISV        VIDSr1
106700120103     C*  avendolo fatto diventare un campo hidden, forzo il suo valore a blank
106800120103     C                   MOVEL     *blanks       VIDSr1
106900111229     C*  tipo testo
107000111229     C                   MOVEL     S32Tip        VIDTi1
107100111229     C*
107200111229     C* simulo un Enter per valorizzare il tipo ricerca
107300111229     C                   EXSR      EntGes
107400111229     C*
107500111229     C*
107600111229     C*
107700111229     C                   ENDSR
107800111229     C*------------------------------------------------------------------------*
107900111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108000111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108100111229     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
108200111229     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
108300111229     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
108400111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108500111229     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108600111229     C*------------------------------------------------------------------------*
108700111229     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108800111229     C*------------------------------------------------------------------------*
108900111229     C     POSSFL        BEGSR
109000111229     C*
109100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
109200970526     C                   EXSR      CLRSFL
109300970526     C*
109400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
109500970526     C                   EXSR      SETFIL
109600970526     C*
109700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
109800970526     C     SFLEOF        IFEQ      0
109900970526     C                   EXSR      CARSFL
110000970526     C                   END
110100970526     C*
110200970526     C                   ENDSR
110300970526     C*------------------------------------------------------------------------*
110400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
110500970526     C*------------------------------------------------------------------------*
110600970526     C     ENTATT        BEGSR
110700970526     C*
110800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
110900970526     C                   Z-ADD     0             OPZOK             1 0
111000970526     C*
111100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
111200970526     C                   Z-ADD     0             SFLVAR            5 0
111300970526     C*
111400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
111500970526     C                   Z-ADD     0             SFLERR
111600970526     C*
111700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
111800970526     C                   Z-ADD     SFLULT        WFLULT
111900970526     C*
112000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
112100001020     C                   READC     S732D05                                98
112200970526     C     *IN98         DOWEQ     '0'
112300970526     C*
112400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
112500970526     C                   ADD       1             SFLVAR
112600970526     C*
112700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
112800970526     C     OPZOK         IFEQ      0
112900970526     C*
113000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
113100970526     C     OPZKEY        IFNE      *BLANKS
113200970526     C                   EXSR      OPZCON
113300001020     C                   ENDIF
113400970526     C*
113500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
113600970526     C*            2. PULIZIA SCELTA IN SFL
113700970526     C     OPZOK         IFEQ      0
113800970526     C                   MOVEL     '0'           *IN41
113900970526     C                   MOVEL     *BLANKS       OPZKEY
114000970526     C                   ELSE
114100970526     C*
114200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
114300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
114400970526     C                   MOVEL     '1'           *IN41
114500970526     C                   Z-ADD     SFLULT        SFLERR
114600001020     C                   ENDIF
114700970526     C                   ELSE
114800970526     C*
114900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
115000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
115100970526     C                   MOVEL     '1'           *IN41
115200001020     C                   ENDIF                                                  OPZOK=0
115300970526     C*
115400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
115500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
115600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
115700970526     C     OPZOK         IFNE      0
115800970526     C                   Z-ADD     SFLERR        SFLNBR
115900970526     C                   ELSE
116000970526     C                   Z-ADD     SFLULT        SFLNBR
116100001020     C                   ENDIF
116200970526     C*
116300970526     C* AGGIORNO RECORD SFL
116400001020     C                   UPDATE    S732D05
116500970526     C*
116600001020     C                   READC     S732D05                                98
116700001020     C                   ENDDO                                                  DO WHILE
116800970526     C*
116900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
117000970526     C                   Z-ADD     WFLULT        SFLULT
117100970526     C*
117200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
117300970526     C     SFLVAR        IFEQ      0
117400001020     C*                  MOVEL     '1'           FINL00
117500001020     C                   ENDIF
117600970526     C*
117700970526     C                   ENDSR
117800970526     C*------------------------------------------------------------------------*
117900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
118000970526     C*------------------------------------------------------------------------*
118100970526     C     CLRSFL        BEGSR
118200970526     C*
118300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
118400970526     C* (0=NON FINE FILE  1=FINE FILE)
118500970526     C                   Z-ADD     0             SFLEOF
118600970526     C*
118700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
118800970526     C                   Z-ADD     0             SFLULT
118900970526     C*
119000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
119100970526     C                   MOVEL     '1'           *IN40
119200001020     C                   WRITE     S732D06
119300970526     C                   MOVEL     '0'           *IN40
119400970526     C*
119500970526     C                   ENDSR
119600970526     C*------------------------------------------------------------------------*
119700970526     C* CARSFL - CARICAMENTO SFL
119800970526     C*------------------------------------------------------------------------*
119900970526     C     CARSFL        BEGSR
120000970526     C*
120100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
120200970526     C     SFLULT        IFEQ      0
120300970526     C                   EXSR      SETFIL
120400001020     C                   ENDIF
120500970526     C*
120600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
120700970526     C                   Z-ADD     SFLULT        WFLULT
120800970526     C*
120900970526     C* CICLO:
121000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
121100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
121200970526     C                   Z-ADD     1             SFLRIG
121300970526     C     SFLRIG        DOWLE     SFLPAG
121400970526     C     SFLEOF        ANDEQ     0
121500970526     C*
121600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
121700970526     C                   MOVEL     *BLANKS       OPZKEY
121800970526     C*
121900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
122000970526     C                   EXSR      IRESFL
122100970526     C*
122200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
122300970526     C                   ADD       1             SFLULT
122400970526     C*
122500970526     C* SCRITTURA RECORD SFL
122600001020     C                   WRITE     S732D05
122700970526     C                   ADD       1             SFLRIG
122800970526     C*
122900970526     C* LETTURA FILE DA CARICARE
123000970526     C                   EXSR      LETFIL
123100001020     C                   ENDDO                                                  DO WHILE
123200970526     C*
123300970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
123400970526     C     WFLULT        IFNE      SFLULT
123500970526     C                   Z-ADD     WFLULT        SFLNBR
123600970526     C                   ADD       1             SFLNBR
123700001020     C                   ENDIF
123800970526     C*
123900970526     C                   ENDSR
124000970526     C*------------------------------------------------------------------------*
124100970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
124200970526     C*------------------------------------------------------------------------*
124300970526     C     OUTSFL        BEGSR
124400970526     C*
124500970526     C* WRITE RECORD TESTATA VIDEO
124600001020     C                   WRITE     S732D01
124700970526     C*
124800970526     C* VISUALIZZAZIONE MESSAGGIO
124900970526     C                   EXSR      SNDMSG
125000970526     C*
125100970526     C* WRITE RECORD OPZIONI VIDEO
125200001020     C                   WRITE     S732D02
125300970526     C*
125400970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
125500001020     C                   WRITE     S732D03
125600970526     C*
125700970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
125800001020     C                   WRITE     S732D04
125900970526     C*
126000970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
126100970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
126200970526     C     SFLEOF        IFEQ      0
126300970526     C                   MOVEL     '0'           *IN42
126400970526     C                   ELSE
126500970526     C*
126600970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
126700970526     C                   MOVEL     '1'           *IN42
126800001020     C                   ENDIF
126900970526     C*
127000970526     C* WRITE RECORD TASTI FUNZIONALI
127100001020     C                   WRITE     S732D09
127200970526     C*
127300970526     C* CASO DI SFL VUOTO
127400970526     C     SFLULT        IFLE      0
127500970526     C*
127600970526     C*    - WRITE RECORD VUOTO
127700001020     C                   WRITE     S732D0A
127800001020     C                   READ      S732D0A                                99
127900970526     C*
128000970526     C                   ELSE
128100970526     C*
128200970526     C* CASO DI SFL CON RECORDS
128300970526     C*
128400970526     C*    - WRITE RECORD CONTROLLO SFL
128500001020     C                   WRITE     S732D06
128600001020     C                   READ      S732D06                                99
128700001020     C                   ENDIF
128800970526     C*
128900970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
129000001020     C                   READ      S732D03                                99
129100970526     C*
129200970526     C                   ENDSR
129300970526     C*------------------------------------------------------------------------*
129400970526     C* SNDMSG - MANDA MESSAGGIO
129500970526     C*------------------------------------------------------------------------*
129600970526     C     SNDMSG        BEGSR
129700970526     C*
129800970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
129900970526     C                   MOVEL     '1'           *IN40
130000001020     C                   WRITE     S732DMC
130100970526     C                   MOVEL     '0'           *IN40
130200970526     C*
130300970526     C* VISUALIZZAZIONE MESSAGGIO
130400970526     C                   Z-ADD     0             MSGULT
130500970526     C                   CALL      'XMSG'        XMSGPA
130600970526     C     DSMSER        IFEQ      0
130700970526     C                   MOVEL     DSMSKE        MSGKEY
130800001020     C                   ENDIF
130900970526     C                   Z-ADD     1             MSGULT
131000001020     C                   WRITE     S732DMS                                      SFL MESS
131100001020     C                   WRITE     S732DMC                                      CONTR MESS
131200970526     C*
131300970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
131400970526     C                   MOVEL     *BLANKS       DSMSMS
131500970526     C*
131600970526     C                   ENDSR
131700970526     C*------------------------------------------------------------------------*
131800970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
131900970526     C*------------------------------------------------------------------------*
132000970526     C     FUNCON        BEGSR
132100970526     C*
132200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
132300970526     C                   EXSR      FUNVAR
132400970526     C*
132500970526     C* PULISCO CAMPI DI LAVORO
132600970526     C                   Z-ADD     0             I                 5 0
132700970526     C                   Z-ADD     0             W                 5 0
132800970526     C*
132900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
133000970526     C     FUNKEY        IFNE      *BLANKS
133100970526     C     1             DO        24            I
133200970526     C                   MOVEL     FUD(I)        FUDRIG
133300970526     C*
133400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
133500970526     C     FUNKEY        IFEQ      FUDFUN
133600970526     C     FUDATT        ANDEQ     'S'
133700970526     C                   Z-ADD     I             W
133800001020     C                   ENDIF
133900001020     C                   ENDDO                                                  DO WHILE
134000970526     C*
134100970526     C* FUNZIONE NON ATTIVA (ERRORE)
134200970526     C     W             IFEQ      0
134300970526     C*
134400970526     C* EMISSIONE MESSAGGIO DI ERRORE
134500970526     C                   MOVEL     MSG(2)        DSMSMS
134600001020     C                   ENDIF                                                  W=0
134700001020     C                   ENDIF                                                  FUNKEY NE *BLANKS
134800970526     C*
134900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
135000970526     C     W             IFNE      0
135100970526     C     FUNKEY        OREQ      *BLANKS
135200970526     C*
135300970526     C     FUNKEY        IFEQ      *BLANKS
135400970526     C                   EXSR      ENTGES
135500001020     C                   ENDIF
135600970526     C     FUNKEY        IFEQ      '03'
135700970526     C                   EXSR      F03GES
135800001020     C                   ENDIF
135900970526     C     FUNKEY        IFEQ      '05'
136000970526     C                   EXSR      F05GES
136100001020     C                   ENDIF
136200970526     C     FUNKEY        IFEQ      '06'
136300970526     C                   EXSR      F06GES
136400001020     C                   ENDIF
136500970526     C     FUNKEY        IFEQ      '12'
136600970526     C                   EXSR      F12GES
136700001020     C                   ENDIF
136800970528     C     FUNKEY        IFEQ      '21'
136900970528     C                   EXSR      F21GES
137000001020     C                   ENDIF
137100970528     C     FUNKEY        IFEQ      '22'
137200970528     C                   EXSR      F22GES
137300001020     C                   ENDIF
137400970526     C     FUNKEY        IFEQ      '23'
137500970526     C                   EXSR      F23GES
137600001020     C                   ENDIF
137700970526     C     FUNKEY        IFEQ      '24'
137800970526     C                   EXSR      F24GES
137900001020     C                   ENDIF
138000970526     C     FUNKEY        IFEQ      'RU'
138100970526     C                   EXSR      RUPGES
138200001020     C                   ENDIF
138300970526     C*
138400001020     C                   ENDIF
138500970526     C*
138600970526     C                   ENDSR
138700970526     C*------------------------------------------------------------------------*
138800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
138900970526     C*------------------------------------------------------------------------*
139000970526     C     FUNVAR        BEGSR
139100970526     C*
139200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
139300970526     C     KEY           IFEQ      ENTER
139400970526     C                   MOVEL     *BLANKS       FUNKEY
139500001020     C                   ENDIF
139600970526     C     KEY           IFEQ      F1
139700970526     C                   MOVEL     '01'          FUNKEY
139800001020     C                   ENDIF
139900970526     C     KEY           IFEQ      F2
140000970526     C                   MOVEL     '02'          FUNKEY
140100001020     C                   ENDIF
140200970526     C     KEY           IFEQ      F3
140300970526     C                   MOVEL     '03'          FUNKEY
140400001020     C                   ENDIF
140500970526     C     KEY           IFEQ      F4
140600970526     C                   MOVEL     '04'          FUNKEY
140700001020     C                   ENDIF
140800970526     C     KEY           IFEQ      F5
140900970526     C                   MOVEL     '05'          FUNKEY
141000001020     C                   ENDIF
141100970526     C     KEY           IFEQ      F6
141200970526     C                   MOVEL     '06'          FUNKEY
141300001020     C                   ENDIF
141400970526     C     KEY           IFEQ      F7
141500970526     C                   MOVEL     '07'          FUNKEY
141600001020     C                   ENDIF
141700970526     C     KEY           IFEQ      F8
141800970526     C                   MOVEL     '08'          FUNKEY
141900001020     C                   ENDIF
142000970526     C     KEY           IFEQ      F9
142100970526     C                   MOVEL     '09'          FUNKEY
142200001020     C                   ENDIF
142300970526     C     KEY           IFEQ      F10
142400970526     C                   MOVEL     '10'          FUNKEY
142500001020     C                   ENDIF
142600970526     C     KEY           IFEQ      F11
142700970526     C                   MOVEL     '11'          FUNKEY
142800001020     C                   ENDIF
142900970526     C     KEY           IFEQ      F12
143000970526     C                   MOVEL     '12'          FUNKEY
143100001020     C                   ENDIF
143200970526     C     KEY           IFEQ      F13
143300970526     C                   MOVEL     '13'          FUNKEY
143400001020     C                   ENDIF
143500970526     C     KEY           IFEQ      F14
143600970526     C                   MOVEL     '14'          FUNKEY
143700001020     C                   ENDIF
143800970526     C     KEY           IFEQ      F15
143900970526     C                   MOVEL     '15'          FUNKEY
144000001020     C                   ENDIF
144100970526     C     KEY           IFEQ      F16
144200970526     C                   MOVEL     '16'          FUNKEY
144300001020     C                   ENDIF
144400970526     C     KEY           IFEQ      F17
144500970526     C                   MOVEL     '17'          FUNKEY
144600001020     C                   ENDIF
144700970526     C     KEY           IFEQ      F18
144800970526     C                   MOVEL     '18'          FUNKEY
144900001020     C                   ENDIF
145000970526     C     KEY           IFEQ      F19
145100970526     C                   MOVEL     '19'          FUNKEY
145200001020     C                   ENDIF
145300970526     C     KEY           IFEQ      F20
145400970526     C                   MOVEL     '20'          FUNKEY
145500001020     C                   ENDIF
145600970526     C     KEY           IFEQ      F21
145700970526     C                   MOVEL     '21'          FUNKEY
145800001020     C                   ENDIF
145900970526     C     KEY           IFEQ      F22
146000970526     C                   MOVEL     '22'          FUNKEY
146100001020     C                   ENDIF
146200970526     C     KEY           IFEQ      F23
146300970526     C                   MOVEL     '23'          FUNKEY
146400001020     C                   ENDIF
146500970526     C     KEY           IFEQ      F24
146600970526     C                   MOVEL     '24'          FUNKEY
146700001020     C                   ENDIF
146800970526     C     KEY           IFEQ      ROLLUP
146900970526     C                   MOVEL     'RU'          FUNKEY
147000001020     C                   ENDIF
147100970526     C     KEY           IFEQ      HELP
147200970526     C                   MOVEL     'HP'          FUNKEY
147300001020     C                   ENDIF
147400970526     C*
147500970526     C                   ENDSR
147600970526     C*------------------------------------------------------------------------*
147700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
147800970526     C*------------------------------------------------------------------------*
147900970526     C     FUNGES        BEGSR
148000970526     C*
148100970526     C* PULISCO CAMPI DI LAVORO
148200970526     C                   Z-ADD     0             W                 5 0
148300970526     C*
148400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
148500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
148600970526     C     1             DO        24            I
148700970526     C                   MOVEL     FUD(I)        FUDRIG
148800970526     C*
148900970526     C* PULIZIA FUNZIONI PRECEDENTI
149000970526     C                   MOVEL     *BLANKS       FUV(I)
149100970526     C*
149200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
149300970526     C* (SALTA F23 E F24)
149400970526     C     FUDATT        IFEQ      'S'
149500970526     C     FUDFUN        ANDNE     '23'
149600970526     C     FUDFUN        ANDNE     '24'
149700970526     C                   ADD       1             W
149800970526     C                   MOVEL     FUDDES        FUV(W)
149900001020     C                   ENDIF
150000001020     C                   ENDDO                                                  DO WHILE
150100970526     C*
150200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
150300970526     C* ALTRIMENTI DISATTIVO F24
150400970526     C     1             DO        24            I
150500970526     C                   MOVEL     FUD(I)        FUDRIG
150600970526     C*
150700970526     C     FUDFUN        IFEQ      '24'
150800970526     C     W             IFGT      6
150900970526     C                   MOVEL     'S'           FUDATT
151000970526     C                   ELSE
151100970526     C                   MOVEL     *BLANKS       FUDATT
151200001020     C                   ENDIF                                                  W GT 8
151300970526     C                   MOVEL     FUDRIG        FUD(I)
151400001020     C                   ENDIF
151500001020     C                   ENDDO                                                  DO WHILE
151600970526     C*
151700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
151800970526     C     W             DIV       6             I
151900970526     C                   MVR                     Z
152000970526     C*
152100970526     C     Z             IFNE      0
152200970526     C     I             ADD       1             Z
152300970526     C                   ELSE
152400970526     C                   Z-ADD     I             Z
152500001020     C                   ENDIF
152600970526     C*
152700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
152800970526     C                   ADD       1             FUNGRU
152900970526     C     FUNGRU        IFGT      Z
153000970526     C     FUNGRU        ORLT      1
153100970526     C                   Z-ADD     1             FUNGRU
153200001020     C                   ENDIF
153300970526     C*
153400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
153500970526     C     FUNGRU        IFEQ      1
153600970526     C                   MOVEL     FUV(1)        FUN001
153700970526     C                   MOVEL     FUV(2)        FUN002
153800970526     C                   MOVEL     FUV(3)        FUN003
153900970526     C                   MOVEL     FUV(4)        FUN004
154000970526     C                   MOVEL     FUV(5)        FUN005
154100970526     C                   MOVEL     FUV(6)        FUN006
154200001020     C                   ENDIF
154300970526     C*
154400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
154500970526     C     FUNGRU        IFEQ      2
154600970526     C                   MOVEL     FUV(7)        FUN001
154700970526     C                   MOVEL     FUV(8)        FUN002
154800970526     C                   MOVEL     FUV(9)        FUN003
154900970526     C                   MOVEL     FUV(10)       FUN004
155000970526     C                   MOVEL     FUV(11)       FUN005
155100970526     C                   MOVEL     FUV(12)       FUN006
155200001020     C                   ENDIF
155300970526     C*
155400970526     C* TERZO GRUPPO DI 6 FUNZIONI
155500970526     C     FUNGRU        IFEQ      3
155600970526     C                   MOVEL     FUV(13)       FUN001
155700970526     C                   MOVEL     FUV(14)       FUN002
155800970526     C                   MOVEL     FUV(15)       FUN003
155900970526     C                   MOVEL     FUV(16)       FUN004
156000970526     C                   MOVEL     FUV(17)       FUN005
156100970526     C                   MOVEL     FUV(18)       FUN006
156200001020     C                   ENDIF
156300970526     C*
156400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
156500970526     C     FUNGRU        IFEQ      4
156600970526     C                   MOVEL     FUV(19)       FUN001
156700970526     C                   MOVEL     FUV(20)       FUN002
156800970526     C                   MOVEL     FUV(21)       FUN003
156900970526     C                   MOVEL     FUV(22)       FUN004
157000970526     C                   MOVEL     *BLANKS       FUN005
157100970526     C                   MOVEL     *BLANKS       FUN006
157200001020     C                   ENDIF
157300970526     C*
157400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
157500970526     C                   MOVEL     *BLANKS       FUN007
157600970526     C                   MOVEL     *BLANKS       FUN008
157700970526     C*
157800970526     C     1             DO        24            I
157900970526     C                   MOVEL     FUD(I)        FUDRIG
158000970526     C     FUDFUN        IFEQ      '23'
158100970526     C     FUDATT        ANDEQ     'S'
158200970526     C                   MOVEL     FUDDES        FUN007
158300001020     C                   ENDIF
158400970526     C     FUDFUN        IFEQ      '24'
158500970526     C     FUDATT        ANDEQ     'S'
158600970526     C                   MOVEL     FUDDES        FUN008
158700001020     C                   ENDIF
158800001020     C                   ENDDO                                                  DO
158900970526     C*
159000970526     C                   ENDSR
159100970526     C*------------------------------------------------------------------------*
159200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
159300970526     C*------------------------------------------------------------------------*
159400970526     C     OPZGES        BEGSR
159500970526     C*
159600970526     C* PULISCO CAMPI DI LAVORO
159700970526     C                   Z-ADD     0             W                 5 0
159800970526     C*
159900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
160000970526     C* E CARICO TABELLA OPZIONI ATTIVE
160100970526     C     1             DO        24            I
160200970526     C                   MOVEL     OPD(I)        OPDRIG
160300970526     C*
160400970526     C* PULIZIA OPZIONI PRECEDENTI
160500970526     C                   MOVEL     *BLANKS       OPV(I)
160600970526     C*
160700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
160800970526     C     OPDATT        IFEQ      'S'
160900970526     C                   ADD       1             W
161000970526     C                   MOVEL     OPDDES        OPV(W)
161100001020     C                   ENDIF
161200001020     C                   ENDDO                                                  DO WHILE
161300970526     C*
161400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
161500970526     C* ALTRIMENTI DISATTIVO F23
161600970526     C     1             DO        24            I
161700970526     C                   MOVEL     FUD(I)        FUDRIG
161800970526     C*
161900970526     C     FUDFUN        IFEQ      '23'
162000970526     C     W             IFGT      8
162100970526     C                   MOVEL     'S'           FUDATT
162200970526     C                   ELSE
162300970526     C                   MOVEL     *BLANKS       FUDATT
162400001020     C                   ENDIF                                                  W GT 8
162500970526     C                   MOVEL     FUDRIG        FUD(I)
162600001020     C                   ENDIF
162700001020     C                   ENDDO                                                  DO WHILE
162800970526     C*
162900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
163000970526     C     W             DIV       8             I
163100970526     C                   MVR                     Z                 5 0
163200970526     C*
163300970526     C     Z             IFNE      0
163400970526     C     I             ADD       1             Z
163500970526     C                   ELSE
163600970526     C                   Z-ADD     I             Z
163700001020     C                   ENDIF
163800970526     C*
163900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
164000970526     C                   ADD       1             OPZGRU
164100970526     C     OPZGRU        IFGT      Z
164200970526     C     OPZGRU        ORLT      1
164300970526     C                   Z-ADD     1             OPZGRU
164400001020     C                   ENDIF
164500970526     C*
164600970526     C* PRIMO GRUPPO DI 8 OPZIONI
164700970526     C     OPZGRU        IFEQ      1
164800970526     C                   MOVEL     OPV(1)        OPZ001
164900970526     C                   MOVEL     OPV(2)        OPZ002
165000970526     C                   MOVEL     OPV(3)        OPZ003
165100970526     C                   MOVEL     OPV(4)        OPZ004
165200970526     C                   MOVEL     OPV(5)        OPZ005
165300970526     C                   MOVEL     OPV(6)        OPZ006
165400970526     C                   MOVEL     OPV(7)        OPZ007
165500970526     C                   MOVEL     OPV(8)        OPZ008
165600001020     C                   ENDIF
165700970526     C*
165800970526     C* SECONDO GRUPPO DI 8 OPZIONI
165900970526     C     OPZGRU        IFEQ      2
166000970526     C                   MOVEL     OPV(9)        OPZ001
166100970526     C                   MOVEL     OPV(10)       OPZ002
166200970526     C                   MOVEL     OPV(11)       OPZ003
166300970526     C                   MOVEL     OPV(12)       OPZ004
166400970526     C                   MOVEL     OPV(13)       OPZ005
166500970526     C                   MOVEL     OPV(14)       OPZ006
166600970526     C                   MOVEL     OPV(15)       OPZ007
166700970526     C                   MOVEL     OPV(16)       OPZ008
166800001020     C                   ENDIF
166900970526     C*
167000970526     C* TERZO GRUPPO DI 8 OPZIONI
167100970526     C     OPZGRU        IFEQ      3
167200970526     C                   MOVEL     OPV(17)       OPZ001
167300970526     C                   MOVEL     OPV(18)       OPZ002
167400970526     C                   MOVEL     OPV(19)       OPZ003
167500970526     C                   MOVEL     OPV(20)       OPZ004
167600970526     C                   MOVEL     OPV(21)       OPZ005
167700970526     C                   MOVEL     OPV(22)       OPZ006
167800970526     C                   MOVEL     OPV(23)       OPZ007
167900970526     C                   MOVEL     OPV(24)       OPZ008
168000001020     C                   ENDIF
168100970526     C*
168200970526     C                   ENDSR
168300970526     C*------------------------------------------------------------------------*
168400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
168500970526     C*------------------------------------------------------------------------*
168600970526     C     OPZCON        BEGSR
168700970526     C*
168800970526     C* PULISCO CAMPI DI LAVORO
168900970526     C                   Z-ADD     0             I                 5 0
169000970526     C                   Z-ADD     0             W                 5 0
169100970526     C*
169200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
169300970526     C     1             DO        24            I
169400970526     C                   MOVEL     OPD(I)        OPDRIG
169500970526     C*
169600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
169700970526     C     OPZKEY        IFEQ      OPDOPZ
169800970526     C     OPZKEY        OREQ      OPDOP1
169900970526     C*
170000970526     C     OPDATT        IFEQ      'S'
170100970526     C                   MOVEL     OPDOPZ        OPZKEY
170200970526     C                   Z-ADD     I             W
170300001020     C                   ENDIF
170400001020     C                   ENDIF
170500970526     C*
170600001020     C                   ENDDO                                                  DO WHILE
170700970526     C*
170800970526     C* OPZIONE NON ATTIVA (ERRORE)
170900970526     C     W             IFEQ      0
171000970526     C*
171100970526     C* OPZIONE NON OK
171200970526     C                   Z-ADD     1             OPZOK
171300970526     C*
171400970526     C* EMISSIONE MESSAGGIO DI ERRORE
171500970526     C                   MOVEL     MSG(1)        DSMSMS
171600970526     C                   ELSE
171700970526     C*
171800970526     C* OPZIONE ATTIVA (PROCEDI ...)
171900970526     C*
172000970526     C     OPZKEY        IFEQ      ' 1'
172100970526     C                   EXSR      O01GES
172200001020     C                   ENDIF
172300970526     C     OPZKEY        IFEQ      ' 2'
172400970526     C                   EXSR      O02GES
172500001020     C                   ENDIF
172600001020     C*    OPZKEY        IFEQ      ' 3'
172700001020     C*                  EXSR      O03GES
172800001020     C*                  ENDIF
172900991104     C*    OPZKEY        IFEQ      ' 4'
173000991104     C*                  EXSR      O04GES
173100001020     C*                  ENDIF
173200970526     C     OPZKEY        IFEQ      ' 5'
173300970526     C                   EXSR      O05GES
173400001020     C                   ENDIF
173500001020     C     OPZKEY        IFEQ      ' 6'
173600001020     C                   EXSR      O06GES
173700001020     C                   ENDIF
173800001020     C*    OPZKEY        IFEQ      ' 7'
173900001020     C*                  EXSR      O07GES
174000001020     C*                  ENDIF
174100010810     C     OPZKEY        IFEQ      ' 8'
174200010810     C                   EXSR      O08GES
174300010810     C                   ENDIF
174400001020     C*    OPZKEY        IFEQ      ' 9'
174500001020     C*                  EXSR      O09GES
174600001020     C*                  ENDIF
174700001020     C*    OPZKEY        IFEQ      '10'
174800001020     C*                  EXSR      O10GES
174900001020     C*                  ENDIF
175000001020     C                   ENDIF                                                  OPZ.NON ATTIVA
175100970526     C*
175200970526     C                   ENDSR
175300970526     C*------------------------------------------------------------------------*
175400970526     C* NOTE COSTRUZIONE SCHIERE
175500970526     C*------------------------------------------------------------------------*
175600970526     C*
175700970526     C* -MSG-MESSAGGI
175800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
175900970526     C* (NON TOCCARE MSG,1 E MSG,2)
176000970526     C*
176100970526     C* -CMD-COMANDI
176200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
176300970526     C*
176400970526     C* -OPD-OPZIONI
176500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
176600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
176700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
176800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
176900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
177000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
177100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
177200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
177300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
177400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
177500970526     C*
177600970526     C* -FUD-FUNZIONI
177700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
177800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
177900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
178000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
178100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
178200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
178300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
178400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
178500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
178600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
178700970526     O*------------------------------------------------------------------------*
178800970526** DEC - DECODIFICHE
178900970526    GESTIONE                                                     1
179000970526INTERROGAZIONE                                                   2
179100970530     SCELTA                                                      3
179200001020Inv
179300001020Da inv
179400090619Rtv
179500970526** MSG - MESSAGGI
179600001020TIS732R- Opzione non gestita                                                   1
179700001020TIS732R- Tasto funzionale non gestito                                          2
179800001024TIS732R- Inserimento consentito solo in Sede.                                  3
179900170116TIS732R- Utente Strategi: non abilitato al servizio.                           4
180000170116TIS732R- Utente Strategi: abilitazione al servizio scaduta.                    5
180100001024TIS732R-                                                                       6
180200001020TIS732R- 7                                                                     7
180300001020TIS732R- 8                                                                     8
180400001020TIS732R- Tipo servizio errato o inesistente.                                   9
180500001020TIS732R- 10                                                                   10
180600001024TIS732R-                                                                      11
180700001024TIS732R-                                                                      12
180800001020TIS732R- 13                                                                   13
180900001020TIS732R- 14                                                                   14
181000001020TIS732R- 15                                                                   15
181100001020TIS732R- 16                                                                   16
181200001020TIS732R- 17                                                                   17
181300001020TIS732R- 18                                                                   18
181400001020TIS732R- 19                                                                   19
181500001020TIS732R- 20                                                                   20
181600970528** CMD - COMANDI CL
181700970528WRKSPLF
181800970528
181900970528
182000970528
182100970528
182200970526** OPD - OPZIONI
182300970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
182400010810 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
182500991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
182600991104 4=                -4 -     (S/N)=N                        opz. 04¬
182700970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
182800001020 6=Sflegga invio   -6 -     (S/N)=N                        opz. 06¬
182900970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
183000010810 8=Modifica uso EDP-8 -     (S/N)=N                        opz. 08¬
183100991028 9=Cliente Unif.   -9 -     (S/N)=N                        opz. 09¬
18320000070310=Legami tipi file-10-     (S/N)=N                        opz. 10¬
183300000703                   -  -     (S/N)=N                        opz. 11¬
183400970526                   -  -     (S/N)=N                        opz. 12¬
183500970526                   -  -     (S/N)=N                        opz. 13¬
183600970526                   -  -     (S/N)=N                        opz. 14¬
183700970526                   -  -     (S/N)=N                        opz. 15¬
183800970526                   -  -     (S/N)=N                        opz. 16¬
183900970526                   -  -     (S/N)=N                        opz. 17¬
184000970526                   -  -     (S/N)=N                        opz. 18¬
184100970526                   -  -     (S/N)=N                        opz. 19¬
184200970526                   -  -     (S/N)=N                        opz. 20¬
184300970526                   -  -     (S/N)=N                        opz. 21¬
184400970526                   -  -     (S/N)=N                        opz. 22¬
184500970526                   -  -     (S/N)=N                        opz. 23¬
184600970526                   -  -     (S/N)=N                        opz. 24¬
184700970526** FUD - FUNZIONI
184800970526                   -        (S/N)=N                        funz.01¬
184900970526                   -        (S/N)=N                        funz.02¬
185000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
185100970526                   -        (S/N)=N                        funz.04¬
185200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
185300970526F06=Inserimento    -        (S/N)=N                        funz.06¬
185400970526                   -        (S/N)=N                        funz.07¬
185500970527                   -        (S/N)=N                        funz.08¬
185600970527                   -        (S/N)=N                        funz.09¬
185700970527                   -        (S/N)=N                        funz.10¬
185800970526                   -        (S/N)=N                        funz.11¬
185900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
186000970526                   -        (S/N)=N                        funz.13¬
186100970526                   -        (S/N)=N                        funz.14¬
186200970526                   -        (S/N)=N                        funz.15¬
186300970526                   -        (S/N)=N                        funz.16¬
186400970526                   -        (S/N)=N                        funz.17¬
186500970526                   -        (S/N)=N                        funz.18¬
186600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
186700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
186800980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
186900970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
187000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
187100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
