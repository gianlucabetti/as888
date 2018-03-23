000100950925     H*------------------------------------------------------------------------*
000200951024     H* ABBINAMENTO FOGLI VIAGGIO A GRIGLIA TRAINI                     NO      *
000300010822
000400010822      * ATTENZIONE: questo programma era il vecchio TNTL25R
000500010822
000600010822     H*------------------------------------------------------------------------*
000700950925     H DECEDIT('0,') DATEDIT(*DMY.)
000800950925     F*------------------------------------------------------------------------*
000900951005     FTNFVS02L  UF   E           K DISK
001000951117     FTNFVS01L  IF   E           K DISK
001100951117     F                                     RENAME(TNFVS000:TNFVS1)
001200010725     FTNFWS01L  IF   E           K DISK
001300950926     FTNTST01L  UF   E           K DISK
001400950926     FTNTSR01L  UF   E           K DISK
001500960205     FTNTSR03L  UF   E           K DISK
001600960205     F                                     RENAME(TNTSR000:TNTSR3)
001700951117     FTNTSR04L  IF   E           K DISK
001800951117     F                                     RENAME(TNTSR000:TNTSR4)
001900010725     FTITLL01L  IF   E           K DISK
002000951117     FTNTLT01L  IF   E           K DISK
002100951117     FTNTLR03L  IF   E           K DISK
002200951108     FTNTLZ01L  IF   E           K DISK
002300951030     FTABEL00F  IF   E           K DISK
002400951222     FAZORG01L  IF   E           K DISK
002500960213     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002600960213     F                                     USROPN
002700960213     FSYSPRT    O    F  132        PRINTER OFLIND(*INOG)
002800960213     F                                     USROPN
002900010725      *------------
003000960221     D PRT             S             66    DIM(28) CTDATA PERRCD(1)
003100960206     D K22             S              1    DIM(22)
003200951117     D SKI             S              1    DIM(15)
003300960627     D MSG             S             22    DIM(8) CTDATA PERRCD(1)
003400120904     D CM1             S              1    DIM(71) CTDATA PERRCD(71)
003500120904     D CM2             S              1    DIM(71) CTDATA PERRCD(71)
003600010725      *------------
003700010821     d p_savipt        s             11  2
003800010821     d p_savili        s             11  2
003900010821     d p_savico        s             11  2
004000010823     d p_fwsipt        s             11  2
004100010823     d p_comili        s             11  2
004200010823     d p_tstico        s             11  2
004300010822     d savdiv          s                   like(tstdiv)
004400010821     d w_delta         s                   like(fwsipt)
004500010821     d w_importo       s                   like(fwsipt)
004600010821     d w_divisa        s                   like(fwsvip)
004700010821     d sav_fwsipt      s                   like(fwsipt)
004800010725     d w_fwsipt        s                   like(fwsipt)
004900010725     d w_fwsies        s                   like(fwsies)
005000010725     d w_fwsvip        s                   like(fwsvip)
005100010725     d w_fwsves        s                   like(fwsves)
005200010822     d sav_tstili      s                   like(tstili)
005300010907     d sav_comili      s                   like(tstili)
005400010725      *------------
005500951030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600951107     D TNTL30        E DS                  EXTNAME(TNTL30DS)
005700960321     D DSTL33        E DS                  EXTNAME(TNTL33DS)
005800010725
005900010821     D yeurcods      E DS
006000010821     D tibs02ds      E DS
006100010821     D dscv          E DS
006200010821     D dgei          E DS
006300010725
006400951002     D KPJBA         E DS
006500951113     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL22R - CONTROLLO ABBINAB.
006600951113     D DSTL22        E DS                  EXTNAME(TNTL22DS)
006700010725      *------------
006800951113     D* DS PER PASSAGGIO PARAMETRI A TNTL24R - CREAZIONE GRIGLIA
006900951113     D DSTL24          DS
007000951113     D* Data traino dal
007100951113     D  D24DTD                 1      8  0
007200951113     D* Data traino al
007300951113     D  D24DTA                 9     16  0
007400951113     D* Numero traino
007500951113     D  D24TRN                17     23  0
007600951113     D* Terminal di partenza
007700951113     D  D24TFP                24     26  0
007800951113     D* Data decorrenza traino
007900951113     D  D24DDE                27     34  0
008000951205     D* Creazione se giorno festivo (' '=No controllo festivo
008100951205     D*                             ('S'=Controllo festivo e crea solo
008200951205     D*                                  se non festivo)
008300951205     D  D24FES                35     35
008400951117     D* RICEZIONE PARAMETRI
008500951117     D DSTL25          DS
008600951117     D* Data traino
008700951117     D  D25DTN                 1      8  0
008800951117     D* Numero traino
008900951117     D  D25TRN                 9     15  0
009000951117     D* lnp n. foglio viaggio
009100951117     D  D25LNP                16     18  0
009200951117     D  D25NFV                19     23  0
009300951117     D  D25TFM                24     24
009400951117     D* ric = 'S' --> pgm richiamato
009500951117     D  D25RIC                25     25
009600951117     D* IN OUTPUT --> ERRORE
009700951117     D  D25ERR                26     26
009800951117     D*
009900951113     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL29R - SBINAMENTO F.V.
010000951113     D DSTL29          DS
010100951115     D  D29TLA                 1      1
010200951115     D  D29LNP                 2      4  0
010300951115     D  D29NFV                 5      9  0
010400951115     D  D29DPD                10     19
010500951115     D  D29PRT                20     20
010600951115     D  D29ERR                21     21
010700951121     D  D29TRN                22     28  0
010800951121     D  D29DTN                29     36  0
010900951113     D WLBDA8          DS
011000951113     D  G08DAT                 1      8  0
011100951113     D  G08INV                 9     16  0
011200951113     D  G08ERR                17     17
011300951113     D  G08TGI                18     22  0
011400960206     D                 DS
011500960206     D  DSDATA                 1      8  0
011600960206     D  DSAA                   1      4  0
011700960206     D  DSMM                   5      6  0
011800960206     D  DSGG                   7      8  0
011900010725
012000960213     ITNTSR3
012100960213     I              TSRATB                      SRATB3
012200960213     I              TSRTRN                      SRTRN3
012300960213     I              TSRDTN                      SRDTN3
012400960213     I              TSRDTT                      SRDTT3
012500960301     I              TSRTFM                      SRTFM3
012600960213     I              TSRNFP                      SRNFP3
012700960213     I              TSRFFP                      SRFFP3
012800960213     I              TSRNFA                      SRNFA3
012900960213     I              TSRFFA                      SRFFA3
013000960213     I              TSRDTP                      SRDTP3
013100960213     I              TSRHTP                      SRHTP3
013200960213     I              TSRDRP                      SRDRP3
013300960213     I              TSRHRP                      SRHRP3
013400960305     I              TSRRTP                      SRRTP3
013500960213     I              TSRDTA                      SRDTA3
013600960213     I              TSRHTA                      SRHTA3
013700960213     I              TSRDRA                      SRDRA3
013800960213     I              TSRHRA                      SRHRA3
013900960213     I              TSRRTA                      SRRTA3
014000960213     I              TSRDKP                      SRDKP3
014100960301     I              TSRFKP                      SRFKP3
014200960213     I              TSRDKA                      SRDKA3
014300960301     I              TSRFKA                      SRFKA3
014400960213     I              TSRDKI                      SRDKI3
014500960301     I              TSRFKI                      SRFKI3
014600960213     I              TSRLNP                      SRLNP3
014700960213     I              TSRNFV                      SRNFV3
014800960213     I              TSRDFV                      SRDFV3
014900951009     C****************************************************************
015000951009     C*  RIEPILOGO INDICATORI
015100951009     C***************************************************************
015200951117     C* 01/02 - INDICA QUALI CAMPI SONO STATI MODIFICATI
015300951127     C* 10    - INDICA CHE IL PGM E' STATO RICHIAMATO
015400960205     C* 18    - DATA/ORA PARTENZA FV = DATA/ORA TEORICA
015500951213     C* 19    - LETTURA TESTATA STORICO
015600951117     C* 30    - COMODO
015700951009     C* 31    - COMODO
015800960221     C* 32    - COMODO
015900960221     C* 33    - COMODO
016000010822     C* 90    - CONDIZIONAMENTO IN STAMPA divisa
016100951108     C* 91    - CONDIZIONAMENTO IN STAMPA TRAZIONISTA
016200951108     C* 92    - CONDIZIONAMENTO IN STAMPA AUTOMEZZO
016300951108     C* 93    - CONDIZIONAMENTO IN STAMPA IMPORTO FOGLIO VIAGGIO
016400951108     C* 94    - CONDIZIONAMENTO IN STAMPA IMPORTO LISTINO
016500951108     C* 95    - CONDIZIONAMENTO IN STAMPA IMPORTO CONFERMATO
016600960205     C* 96    - CONFERMATO IMPORTO A FRONTE DI CHIUSURA BUCO DI FV CHE
016700960205     C*         NON ESPONE IMPORTO
016800960222     C* 97    - ON -->STAMPA SOLO I DATI DEL FOGLIO VIAGGIO
016900960222     C*         OFF-->STAMPA SU DUE RIGHE:
017000960222     C*               PRIMA RIGA  : DATI DEL TRAINO PRIMA DI VARIAZIONI
017100960222     C*               SECONDA RIGA: VARIAZIONI EFFETTUATE
017200951127     C* 98    - F.V. CHE ESPONE L'IMPORTO
017300951009     C***************************************************************
017400951117     C* PGM RICHIAMATO : IMPOSTO LA KEY E FACCIO AGGIORNAMENTI
017500960312    1C     *IN10         IFEQ      *ON
017600951117     C                   EXSR      PGMRIC
017700951117     C* SE HO TROVATO TUTTI I DATI AGGIORNO
017800960312    2C     D25ERR        IFEQ      ' '
017900951117     C                   EXSR      AGGSTO
018000960312    2C                   ENDIF
018100951117     C**
018200960312   X2C                   ELSE
018300951117     C**
018400951117     C* LEGGO SOLO F.V. SFLAGGATI (FVSABN=' ') ABBINATI DALLA FILIALE
018500951117     C* (Infatti il pgm che riceve il f.v. se trova FVSTRN=0 flagga
018600951117     C* subito il f.v. con FVSABN='S')
018700951005     C     ' '           SETLL     TNFVS02L
018800951005     C     ' '           READE     TNFVS02L                               30
018900960312    2C     *IN30         DOWEQ     *OFF
019000960322     C*
019100960322     C                   CLEAR                   WFIRST
019200950926     C* Escludo F.V. annullati
019300960312    3C     FVSATB        IFNE      ' '
019400951121     C                   MOVEL     *ALL'9'       FVSTRS
019500951121     C                   GOTO      LEGG1
019600960312    3C                   ENDIF
019700010822      * aggancio il file delle estensioni foglio viaggio per gli importi in
019800010822      * divisa
019900010907      * ATTENZIONE: il file delle estensioni non sempre esiste, o non sempre
020000010910      *             c'è come importo (es. se a 0), per questo uso dei campi di
020100010907      *             work!!!
020200010910     c                   clear                   fwsipt
020300010910     c                   clear                   fwsies
020400010910     c                   clear                   fwsvip
020500010910     c                   clear                   fwsves
020600010822     c     kfws          chain     tnfws01l
020700010907      * se l'importo dell'estensione è > 0 prendo quello che trovo
020800010907      * perchè vuol dire che stanno già girando i programmi nuovi modificati per l'euro
020900010907     c                   if        fwsipt > 0
021000010907     c                   eval      w_fwsvip = fwsvip
021100010907     c                   z-add     fwsipt        w_fwsipt
021200010907     c                   endif
021300010907     c                   if        fwsies > 0
021400010907     c                   eval      w_fwsves = fwsves
021500010907     c                   z-add     fwsies        w_fwsies
021600010907     c                   endif
021700010907      * se l'importo dell'estensione è = 0 e l'importo sul file
021800010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
021900010907      * la divisa è lire
022000010907     c                   if        fwsipt = *zeros and fvsipt <> 0
022100010907     c                   eval      w_fwsvip = 'ITL'
022200010907     c                   z-add     fvsipt        w_fwsipt
022300010907     c                   endif
022400010907     c                   if        fwsies = *zeros and fvsies <> 0
022500010907     c                   eval      w_fwsves = 'ITL'
022600010907     c                   z-add     fvsies        w_fwsies
022700010907     c                   endif
022800010907      * se l'importo dell'estensione è = 0 e l'importo sul file
022900010907      * principale è = 0, imposto la divisa a blank per poi prendere quella della
023000010907      * griglia, così non sto a convertire un importo a 0
023100010907     c                   if        fwsipt = *zeros and fvsipt = 0
023200010907     c                   z-add     fvsipt        w_fwsipt
023300010907     c                   clear                   w_fwsvip
023400010907     c                   endif
023500010907     c                   if        fwsies = *zeros and fvsies = 0
023600010907     c                   z-add     fvsies        w_fwsies
023700010907     c                   clear                   w_fwsves
023800010907     c                   endif
023900960311     C*
024000960311     C* Richiamo pgm di controllo abbinabilità del f.v.
024100951113     C                   MOVEL     '1'           WCL22             1
024200951121     C                   CLEAR                   DSTL22
024300951113     C                   MOVE      FVSLNP        D22LNP
024400951113     C                   MOVE      FVSNFV        D22NFV
024500951113     C                   MOVEL     'S'           D22FFS
024600951113     C                   MOVEL     DSTL22        KPJBU
024700951113     C                   CALL      'TNTL22R'
024800951113     C                   PARM                    KPJBA
024900951113     C                   MOVEL     KPJBU         DSTL22
025000960122     C* F . V .    N O N   A B B I N A B I L E
025100960312    3C     D22ERR        IFNE      ' '
025200951113     C* Verifico se il f.v era già abbinato ed in tal caso lo sbino
025300960312    4C     FVSTRS        IFGT      *ZEROS
025400951115     C                   MOVE      *BLANKS       D29TLA
025500951113     C                   MOVE      FVSLNP        D29LNP
025600951113     C                   MOVE      FVSNFV        D29NFV
025700951114     C                   MOVEL     FVSDPD        D29DPD
025800951113     C                   MOVEL     'S'           D29PRT
025900951113     C                   MOVEL     DSTL29        KPJBU
026000951113     C                   CALL      'TNTL29R'
026100951113     C                   PARM                    KPJBA
026200951113     C                   MOVEL     KPJBU         DSTL29
026300951113     C* Se lo sbinamento è riuscito imposto anche FVSTRS
026400951121     C* Non testo D22ERR = 2 perchè no viene gestito il caso di varia-
026500951121     C* zione di §tvutc del tipo traino. Per cui questo caso non può
026600951121     C* verificarsi.
026700951121     C* Lo imposto = 0 per abilitare l'abbinamento manuale
026800960312    5C     D29ERR        IFEQ      ' '
026900951109     C                   MOVE      *ZEROS        FVSTRS
027000960308     C                   CLEAR                   FVSTFS
027100960312   X5C                   ELSE
027200951121     C* Se lo sbinamento non è riuscito perchè già fatturato aggiorno
027300951121     C* comunque tstipt e tsties
027400951121     C     KTST1         CHAIN     TNTST01L                           30
027500960312    6C     *IN30         IFEQ      *OFF
027600010725     C**!!!              MOVE      FVSIPT        TSTIPT
027700010725     C**!!!              MOVE      FVSIES        TSTIES
027800010907      * se ho la divisa di comodo del foglio viaggio a blank devo prendere quella
027900010907      * della griglia
028000010907     c                   if        w_fwsvip = *blanks
028100010907     c                   eval      w_fwsvip = tstdiv
028200010907     c                   endif
028300010907     c                   if        w_fwsves = *blanks
028400010907     c                   eval      w_fwsves = tstdiv
028500010907     c                   endif
028600010822
028700010822      * aggancio la tabella CV per stabilire se la divisa della griglia accetta i
028800010822      * decimali
028900010822      * recupero anche i dati dalla tabella GEI per il controllo della tolleranza
029000010822      * lo faccio ora così non lo devo fare ogni volta che richiamo la routine sr_converti
029100010822      * o ogni volta che ho un controllo tra 2 campi importi che non appartengono alla
029200010822      * griglia
029300010822     c                   exsr      Sr_Cartab
029400010821
029500010725      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
029600010725      * devo convertire fwsipt e fwsies nella divisa della griglia
029700010821     c                   eval      w_importo = w_fwsipt
029800011023     c                   if        w_fwsvip <> tstdiv and w_fwsipt <> 1
029900011023     c                             and w_fwsipt <> 0
030000010821     c                   eval      w_divisa = w_fwsvip
030100010821     c                   exsr      Sr_Converti
030200010725     c                   endif
030300010821     c                   eval      tstipt = w_importo
030400010821
030500010821     c                   eval      w_importo = w_fwsies
030600011023     c                   if        w_fwsves <> tstdiv and w_fwsies <> 1
030700011023     c                             and w_fwsies <> 0
030800010821     c                   eval      w_divisa = w_fwsves
030900010821     c                   exsr      Sr_Converti
031000010725     c                   endif
031100010821     c                   eval      tsties = w_importo
031200010727
031300951121     C                   UPDATE    TNTST000
031400960312    6C                   END
031500960312    5C                   END
031600960312    4C                   END
031700951115     C* VADO AD UPDATARE FVS SENZA ABBINARE IL FV
031800951115     C                   GOTO      LEGG1
031900960312    3C                   END
032000960122     C* F . V .  A B B I N A B I L E
032100951109     C* Verifico se esiste griglia
032200951222     C     KTST          CHAIN(N)  TNTST01L                           30
032300951109     C* Se non esiste la creo
032400960312    3C     *IN30         IFEQ      *ON
032500951113     C                   CLEAR                   DSTL24
032600951113     C                   MOVE      FVSTRN        D24TRN
032700951113     C                   MOVE      D22DDE        D24DDE
032800951113     C                   MOVE      D22DTN        D24DTD
032900960122     C                   MOVE      'S'           D24FES
033000951113     C                   MOVEL     DSTL24        KPJBU
033100950928     C                   CALL      'TNTL24R'
033200950928     C                   PARM                    KPJBA
033300960312   X3C                   ELSE
033400960202     C* Trovata griglia ma annullata: non abbino e flaggo
033500960220     C     TSTATB        CABNE     *BLANKS       LEGG1
033600951221     C* Se griglia esiste verifico se il foglio viaggio non è ancora
033700951221     C* stato abbinato ed è relativo a traino già fatturato:
033800951221     C* in tal caso non abbino e imposto FVSTRS=9999998 e stampo errore
033900960312    4C     FVSTRS        IFEQ      *ZEROS
034000951221     C     TSTNFT        ANDGT     *ZEROS
034100951221     C                   MOVE      9999998       FVSTRS
034200960220     C                   MOVE      'F'           WERR1             1
034300951222     C                   EXSR      PRTER1
034400951221     C                   GOTO      LEGG1
034500960312    4C                   END
034600960312    3C                   END
034700950929     C*
034800950928     C                   EXSR      AGGSTO
034900960312    3C     WOKABB        IFEQ      ' '
035000960322     C* Memorizzo se si tratta di un FV ricevuto per la prima volta
035100960322     C     FVSTRS        IFEQ      *ZEROS
035200960322     C                   MOVE      '1'           WFIRST            1
035300960322     C                   END
035400951002     C* Aggiorno file fogli viaggio nel campo del N.Traino Sede a
035500951109     C* conferma dell'avvenuto abbinamento alla griglia
035600950929     C                   Z-ADD     FVSTRN        FVSTRS
035700960308     C                   MOVE      FVSTFM        FVSTFS
035800960314     C* Aggiorno anche data controllo fv in modo che se verrà sbinato
035900960314     C* compaia nella videata di conferma fv non abbinati senza dover
036000960314     C* cambiare l'opzione di selezione "fv Controllati/Non controllati
036100960314     C* /Tutti"
036200960314     C                   Z-ADD     FVSDFV        FVSDKF
036300960312    3C                   END
036400951005     C*
036500951109     C     LEGG1         TAG
036600951109     C*
036700951005     C                   MOVEL     'S'           FVSABN
036800951005     C                   UPDATE    TNFVS000
036900960322     C* Se foglio viaggio ricevuto per la prima volta richiamo pgm
037000960322     C* che aggiorna metri cubi reali su griglia
037100960322     C     WFIRST        IFEQ      '1'
037200960322     C                   MOVEL     '1'           WCL33             1
037300960322     C                   CLEAR                   DSTL33
037400960322     C                   MOVE      FVSLNP        D33LNP
037500960322     C                   MOVE      FVSNFV        D33NFV
037600960322     C                   MOVE      D22DTN        D33DTN
037700960322     C                   MOVEL     DSTL33        KPJBU
037800960322     C                   CALL      'TNTL33R'
037900960322     C                   PARM                    KPJBA
038000960322     C                   END
038100951109     C*
038200951017     C     ' '           READE     TNFVS02L                               30
038300960312    2C                   ENDDO
038400960322     C*
038500951113     C* Chiusura files
038600960312    2C     WCL22         IFEQ      '1'
038700951113     C                   MOVEL     'C'           D22TLA
038800951113     C                   MOVEL     DSTL22        KPJBU
038900951113     C                   CALL      'TNTL22R'
039000951113     C                   PARM                    KPJBA
039100960312    2C                   END
039200960322    2C     WCL33         IFEQ      '1'
039300960322     C                   MOVEL     'C'           D33TLA
039400960322     C                   MOVEL     DSTL33        KPJBU
039500960322     C                   CALL      'TNTL33R'
039600960322     C                   PARM                    KPJBA
039700960322    2C                   END
039800960312    1C                   END
039900951117     C*
040000951117    1C     WPRT          IFEQ      '1'
040100960206     C                   EXCEPT    FINES
040200951117     C                   EXCEPT    PIEDI
040300951117    1C                   END
040400950926     C*
040500950926     C                   SETON                                        LR
040600950928     C**************  AGGIORNAMENTO STORICI TRAINI  *******************
040700950928     C     AGGSTO        BEGSR
040800951213     C                   CLEAR                   WERR
040900960206     C                   MOVE      ' '           WBUCO             1
041000960206     C                   MOVE      ' '           WOKABB            1
041100960205     C                   CLEAR                   WMODIF
041200960222     C                   SETOFF                                       96
041300960205     C                   MOVE      ' '           WVOLTA            1
041400951115     C     KTSR          SETLL     TNTSR01L
041500951115     C     KTSR          READE     TNTSR01L                               30
041600960205    1C     *IN30         DOWEQ     *OFF
041700960220     C     TSRATB        ANDEQ     *BLANKS
041800960205    2C     WVOLTA        IFEQ      *BLANKS
041900960221     C* Se il foglio viaggio teorico risulta già abbinato ad un altro
042000960221     C* fv reale non posso abbinare e stampo errore
042100960221     C     TSRNFV        IFGT      *ZEROS
042200960221     C     TSRNFV        ANDNE     FVSNFV
042300960221     C                   MOVE      'N'           WOKABB
042400960221     C                   MOVE      'A'           WERR1
042500960221     C                   EXSR      PRTER1
042600960221     C                   GOTO      ENDAGG
042700960221     C                   END
042800960205     C* Salvo dati riga
042900960206     C                   MOVE      TSRDRP        SAVDRP
043000960206     C                   MOVE      TSRHRP        SAVHRP
043100960206     C                   MOVE      TSRRTP        SAVRTP
043200970328     C* Memorizzo se data/ora partenza del FV = data/ora partenza teor.
043300970328     C     FVSDRP        COMP      TSRDTP                                 18
043400970328     C   18FVSHRP        COMP      TSRHTP                                 18
043500960205     C* Controlli già effettuati: memorizzo se variato qualcosa
043600960301    3C     TSRFKP        IFEQ      'S'
043700960205    4C     FVSDRP        IFNE      TSRDRP
043800960205     C     FVSHRP        ORNE      TSRHRP
043900960205     C     FVSRTP        ORNE      TSRRTP
044000960205     C                   MOVE      'M'           WMODIF            1
044100970328     C* Non segnalo comunque variazione di data/ora se partenza puntua-
044200970328     C* le sia prima della variazione che adesso
044300970328    5C     *IN18         IFEQ      *ON
044400970328     C     TSRDKP        ANDEQ     99999999
044500970328     C     TSRRTP        ANDEQ     *BLANKS
044600970328     C     FVSRTP        ANDEQ     *BLANKS
044700970328     C                   CLEAR                   WMODIF
044800970328    5C                   END
044900970327    4C                   END
045000960205     C* Memorizzo chiusura buco
045100960222    4C     TSRNFV        IFEQ      *ZEROS
045200960205     C     TSRDKP        ANDNE     99999999
045300960206     C                   MOVEL     'B'           WBUCO
045400960222    4C                   END
045500960205    3C                   END
045600960205     C                   MOVE      '1'           WVOLTA
045700960205    2C                   END
045800960205     C* Aggiorno data controllo buchi/ritardi partenza
045900960229     C***18                MOVE 99999999  TSRDKP
046000960205     C* Se variato qualcosa e controlli già effettuati, solo sul primo
046100960205     C* record azzero la data controllo importi mentre sui record suc-
046200960205     C* cessivi la imposto=99999999 per non essere letti da pgm control
046300960229     C***        *IN18     IFEQ *OFF
046400960229    2C     WMODIF        IFEQ      'M'
046500960205    3C     WVOLTA        IFEQ      '1'
046600960205     C                   MOVE      *ZEROS        TSRDKP
046700960205     C                   MOVE      '2'           WVOLTA
046800960205   X3C                   ELSE
046900960205     C                   MOVE      99999999      TSRDKP
047000960301     C                   MOVE      'S'           TSRFKP
047100960205    3C                   END
047200960205    2C                   END
047300951006     C                   MOVE      FVSDRP        TSRDRP                         DATA PART.R.
047400951006     C                   MOVE      FVSHRP        TSRHRP                         ORA  PART.R.
047500960626     C* SE CAUSALE RITARDO DELLA GRIGLIA='K' (NON PRESENTE RITARDO PER
047600960626     C* SEDE) MANTENGO QUEST'ULTIMA PERDENDO UN'EVENTUALE VARIAZIONE
047700960626     C* DA PARTE DELLA FILIALE
047800960627     C     SAVRTP        IFNE      'K'
047900951114     C                   MOVE      FVSRTP        TSRRTP                         RITARD PART
048000960626     C                   END
048100951114     C                   MOVE      FVSLNP        TSRLNP                         LNP F.V.
048200951002     C                   MOVE      FVSNFV        TSRNFV                         NR.F.V.
048300951002     C                   MOVE      FVSDFV        TSRDFV                         DATA F.V.
048400950929     C                   UPDATE    TNTSR000
048500950928     C     KTSR          READE     TNTSR01L                               30
048600951107    1C                   ENDDO
048700960205     C* Non trovata riga griglia perchè annullata o inesistente a causa
048800960205     C* di festività: non faccio nulla
048900960205     C     WVOLTA        IFEQ      *BLANKS
049000960206     C                   MOVE      'N'           WOKABB
049100960205     C                   GOTO      ENDAGG
049200960205     C                   END
049300960208     C* Vado ad aggiornare dati di testata se è f.v. che espone importo
049400960208     C                   SETOFF                                       98
049500960208    1C     FVSLNP        IFEQ      D22FES
049600960208     C     KTST          CHAIN     TNTST01L                           19
049700960208    2C     *IN19         IFEQ      *OFF
049800960208     C     TSRNFP        ANDEQ     TSTNFE
049900960208     C                   SETON                                        98
050000960208    2C                   END
050100960208    1C                   END
050200960208     C*
050300960208    1C     *IN98         IFEQ      *ON
050400010822      * aggancio le tabelle
050500010822     c                   exsr      Sr_Cartab
050600960208     C                   EXSR      AGGTES
050700960208   x1C                   ELSE
050800951213     C* Se ho chiuso un buco di f.v. che non espone, se posso
050900951213     C* confermo importo e stampo comunque la chiusura buco
051000951213    2C     WBUCO         IFEQ      'B'
051100951213    3C     FVSLNP        IFNE      D22FES
051200951213     C     KTST          CHAIN     TNTST01L                           19
051300951213    3C                   END
051400951213    3C     *IN19         IFEQ      *OFF
051500010822      * aggancio le tabelle
051600010822     c                   exsr      Sr_Cartab
051700960222     C                   MOVE      TSTCNF        SAVCNF
051800960213     C                   EXSR      CONFIM
051900960213    4C     WCONF         IFEQ      '1'
052000960213     C                   UPDATE    TNTST000
052100960222     C     SAVCNF        COMP      ' '                                    96
052200960205     C     KTSR3         SETLL     TNTSR03L
052300960205     C     KTSR3         READE     TNTSR03L                               30
052400960213    5C     *IN30         DOWEQ     *OFF
052500960213     C                   MOVE      99999999      SRDKI3
052600960301     C                   MOVE      'S'           SRFKI3
052700960205     C                   UPDATE    TNTSR3
052800960205     C     KTSR3         READE     TNTSR03L                               30
052900960213    5C                   ENDDO
053000960213   X4C                   ELSE
053100951213     C                   UNLOCK    TNTST01L
053200960213    4C                   END
053300960213    3C                   END
053400960222     C                   MOVEL     'B'           WERR
053500951213     C                   EXSR      PRTERR
053600960205   X2C                   ELSE
053700960206     C* Se non è chiusura buco e sono stati variati dei dati imposto
053800960206     C* flag per stampa
053900960205     C     WMODIF        IFEQ      'M'
054000960205     C                   MOVE      'V'           WERR
054100960205     C                   EXSR      PRTERR
054200960205     C                   END
054300951107    2C                   END
054400951123    1C                   END
054500960320     C*
054600960122     C     ENDAGG        ENDSR
054700960205     C***************  AGGIORNAMENTO TESTATA TRAINO  ******************
054800960205     C     AGGTES        BEGSR
054900960205     C* SE TIPO AUTOMEZZO NON C'ERA LO PONIAMO = A QUELLO DEL FV
055000960205    2C     TSTTMZ        IFEQ      ' '
055100960205     C                   MOVEL     FVSTMZ        TSTTMZ
055200960205    2C                   ENDIF
055300960205     C* SE TRAZIONISTA NON C'E' --> GLI LASCIO QUELLO CHE GIA' C'E'
055400960205    2C     FVSPDR        IFEQ      0
055500960205     C                   MOVEL     TSTPDR        FVSPDR
055600960205    2C                   ENDIF
055700960205     C* Salvo dati di testata
055800010725     C**!!!              MOVE      TSTIPT        SAVIPT
055900010725     c                   z-add     tstipt        savipt
056000960205     C                   MOVE      TSTPDR        SAVPDR
056100960205     C                   MOVE      TSTTMZ        SAVTMZ
056200010727     C**!!!              MOVE      TSTICO        SAVICO
056300010727     C**!!!              MOVE      TSTILI        SAVILI
056400010727     c                   z-add     tstico        savico
056500010727     c                   z-add     tstili        savili
056600010822     c                   move      tstdiv        savdiv
056700960205     C* Reperimento importo di listino e caus.contabilizz.
056800960205     C                   CLEAR                   COMILI
056900960205     C                   CLEAR                   COMCAU
057000960205    2C     FVSPDR        IFGT      *ZEROS
057100960205     C                   EXSR      SRLIST
057200010907      * devo calcolare la tolleranza perchè dopo viene confrontato l'importo del
057300010907      * listino della griglia con l'importo del listino
057400010907      * uso un campo sav al posto di comili xchè poi comili viene memorizzato e
057500010907      * stampato
057600010907     c                   eval      sav_comili = comili
057700010907      * calcolo il delta e controllo la tolleranza
057800010907     c                   if        savili <> sav_comili
057900010907     c                   eval      w_delta = (savili - sav_comili)
058000010910      * il delta deve essere come valore assoluto
058100010910     c                   mllzo     1             w_delta
058200010907     c                   if        w_delta <= §gettr
058300010907     c                   eval      sav_comili = savili
058400010907     c                   endif
058500010907     c                   endif
058600010907
058700960205    2C                   ENDIF
058800960205     C*
058900060802    2C***  FVSPDR        IFLE      *ZEROS
059000060802     C***  COMILI        OREQ      *ZEROS
059100010727     C**!!!              MOVEL     TSTILI        COMILI
059200060802     c***                z-add     tstili        comili
059300060802     c***                eval      sav_comili = comili
059400060802     C***                MOVEL     TSTCAU        COMCAU
059500060802    2C***                ENDIF
059600960205     C**
059700960205     C* Controllo se sono variati i seguenti dati:
059800960205     C* - Cod.Trazionista
059900960205     C* - Tipo automezzo
060000960205     C* - Importo listino
060100960205     C* - Importo foglio viaggio
060200960205     C                   SETOFF                                       0102
060300960205    2C     SAVPDR        IFNE      FVSPDR
060400960205     C     SAVTMZ        ORNE      FVSTMZ
060500010907     C**!!!SAVILI        ORNE      COMILI
060600010907     C     SAVILI        ORNE      sav_COMILI
060700960205     C                   SETON                                        01
060800960205    2C                   ENDIF
060900960205     C*
061000010725    2C**!!!SAVIPT        IFNE      FVSIPT
061100010821      * devo confrontare l'importo pattuito della griglia con quello del
061200010821      * foglio viaggio, prima però devo controllare le 2 divise, se sono diverse
061300010821      * devo convertire fwsipt nella divisa della griglia e per fare il confronto
061400010821      * devo anche controllare che il delta non superi la tolleranza
061500010821     c                   eval      w_importo = w_fwsipt
061600011023     c                   if        w_fwsvip <> tstdiv and w_fwsipt <> 1
061700011023     c                             and w_fwsipt <> 0
061800010821     c                   eval      w_divisa = w_fwsvip
061900010821     c                   exsr      Sr_Converti
062000010821     c                   endif
062100010910     c                   eval      w_fwsipt = w_importo
062200010821      * uso un campo sav al posto di w_fwsipt xchè questo campo viene usato anche in altri
062300010821      * punti del programma
062400010821     c                   eval      sav_fwsipt = w_importo
062500010911      * secondo la tolleranza serve solo per dopo quando controlla tstico con fwsipt
062600010821      * calcolo il delta e controllo la tolleranza
062700010821     c                   if        savipt <> sav_fwsipt
062800010823     c                   eval      w_delta = (savipt - sav_fwsipt)
062900010910      * il delta deve essere come valore assoluto
063000010910     c                   mllzo     1             w_delta
063100010821     c                   if        w_delta <= §gettr
063200010821     c                   eval      sav_fwsipt = savipt
063300010821     c                   endif
063400010821     c                   endif
063500010821
063600010912     c                   if        savipt <> sav_fwsipt
063700960205     C                   SETON                                        02
063800960205    2C                   ENDIF
063900960205     C*
064000960205     c* Solo se ho modificato qualche cosa, faccio i controlli
064100960205     c* altrimenti non aggiorno nulla sulla griglia
064200960205    2C     *IN01         IFEQ      *OFF
064300960205     C     *IN02         ANDEQ     *OFF
064400960205     C                   UNLOCK    TNTST01L
064500960222    3C     WMODIF        IFEQ      'M'
064600960222     c* se non variati dati di testata ma variati dati di riga
064700960222     C* stampo le variazioni
064800960222     C                   MOVEL     'V'           WERR
064900960222     C                   EXSR      PRTERR
065000960222    3C                   END
065100960205   X2C                   ELSE
065200960205     C* campi aggiornati sempre
065300010821     C**!!!              Z-ADD     FVSIPT        TSTIPT
065400010821     C**!!!              MOVE      FVSIES        TSTIES
065500010821
065600010910      * per tstipt utilizzo w_fwsipt xchè ho già fatto i calcoli prima per questo campo
065700010910     c                   eval      tstipt = w_fwsipt
065800010821
065900010821      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
066000010821      * devo convertire fwsies nella divisa della griglia
066100010821     c                   eval      w_importo = w_fwsies
066200011023     c                   if        w_fwsves <> tstdiv and w_fwsies <> 1
066300011023     c                             and w_fwsies <> 0
066400010821     c                   eval      w_divisa = w_fwsves
066500010821     c                   exsr      Sr_Converti
066600010821     c                   endif
066700010821     c                   eval      tsties = w_importo
066800960205     C*
066900960205     C* Se già FATTURATO
067000960205     C* Se ho modificato solo l'importo del FV stampo solo se
067100960205     c*  diverso da quello confermato, altrimenti stampo sempre
067200960205    3C     TSTNFT        IFGT      *ZEROS
067300960205     C*
067400010821      * calcolo il delta e controllo la tolleranza
067500010821     c                   if        tstico <> sav_fwsipt
067600010823     c                   eval      w_delta = (tstico - sav_fwsipt)
067700010910      * il delta deve essere come valore assoluto
067800010910     c                   mllzo     1             w_delta
067900010821     c                   if        w_delta <= §gettr
068000010821     c                   eval      sav_fwsipt = tstico
068100010821     c                   endif
068200010821     c                   endif
068300010821
068400960205    4C     *IN01         IFEQ      *ON
068500960205     C     *IN02         OREQ      *ON
068600010821     C**!!!FVSIPT        ANDNE     TSTICO
068700010821     C     sav_fwsipt    ANDNE     TSTICO
068800960205     C                   MOVE      'F'           WERR              1
068900960205     C                   EXSR      PRTERR
069000960205    4C                   ENDIF
069100960205     C*
069200960205     C                   UPDATE    TNTST000
069300960205     C*
069400960205   X3C                   ELSE
069500960205     C                   MOVE      FVSPDR        TSTPDR
069600960205     C                   MOVE      FVSTMZ        TSTTMZ
069700010727     C**!!!              MOVE      COMILI        TSTILI
069800010727     c                   z-add     comili        tstili
069900960205     C                   MOVEL     COMCAU        TSTCAU
070000960205     c* Controlli gia' effettuati
070100960208     C     KTSR          CHAIN     TNTSR01L                           30
070200960301    4C     TSRFKI        IFEQ      'S'
070300960222     C* Provo a confermare importo in automatico
070400960222     C                   MOVEL     TSTCNF        SAVCNF
070500960222     C                   EXSR      CONFIM
070600960229     C***        WCONF     IFEQ '1'
070700960229     C***                  MOVE *HIVAL    TSRDKI
070800960229     C***                  END
070900960205     C*
071000960205    5C                   SELECT
071100960222     C* CONFERMA AUTOMATICA:
071200960222     C     SAVCNF        WHENEQ    '1'
071300960222    6C     WCONF         IFEQ      '1'
071400960222     C* Riconferma riuscita --> stampo le variazioni
071500960222     C                   MOVE      'V'           WERR
071600960222   X6C                   ELSE
071700960222     C* Riconferma non riuscita --> sconfermo e stampo
071800960222     C                   MOVE      ' '           TSTCNF
071900960222     C                   MOVE      *ZEROS        TSTICO
072000960222     C                   MOVE      *ZEROS        TSRDKI
072100960222     C                   MOVE      'S'           WERR                           SCONFERMA
072200960222    6C                   END
072300960222     C                   EXSR      PRTERR
072400960222     C* CONFERMA MANUALE
072500960222     C     SAVCNF        WHENEQ    '2'
072600960222     C* Conferma riuscita:
072700960222    6C     WCONF         IFEQ      '1'
072800960222     C* - Se è stato variato solo l'importo fv non stampo variazioni
072900960222     C   01              MOVEL     'V'           WERR
073000960222     C   01              EXSR      PRTERR
073100960222   x6C                   ELSE
073200960222     C* Conferma NON riuscita:
073300960222     C* - Se è stato variato solo importo FV lascio la conferma manuale
073400960222     C*   e stampo la variazione
073500960222    7C     *IN01         IFEQ      *OFF
073600960222     C                   MOVEL     'V'           WERR
073700960222     C                   ELSE
073800960222     C* - Altrimenti sconfermo l'importo e stampo
073900960222     C                   MOVE      ' '           TSTCNF
074000960222     C                   MOVE      *ZEROS        TSTICO
074100960301     C                   MOVE      *ZEROS        TSRDKI
074200960222     C                   MOVE      'S'           WERR                           SCONFERMA
074300960222    7C                   END
074400960222     C                   EXSR      PRTERR
074500960222    6C                   END
074600960205     c*
074700960205     C                   OTHER
074800960222    6C     WCONF         IFEQ      '1'
074900960222    7C     WBUCO         IFEQ      'B'
075000960222     C     SAVPDR        ANDEQ     *ZEROS
075100960222     C                   MOVEL     'B'           WERR
075200960222     C                   ELSE
075300960205     C                   MOVEL     'C'           WERR
075400960222    7C                   END
075500960222   X6C                   ELSE
075600960222    7C     WBUCO         IFEQ      'B'
075700960222     C     SAVPDR        ANDEQ     *ZEROS
075800960222     C                   MOVEL     'B'           WERR
075900960222   X7C                   ELSE
076000960222     C                   MOVEL     'V'           WERR
076100960222    7C                   END
076200960222    6C                   ENDIF
076300960205     C                   EXSR      PRTERR
076400960222     C*
076500960205    5C                   ENDSL
076600960208     C                   UPDATE    TNTSR000
076700960205     C* traino non ancora controllato --> per sicurezza tolgo l'importo
076800960205     c*   confermato
076900960205   X4C                   ELSE
077000960208     C                   UNLOCK    TNTSR01L
077100960205     C                   MOVE      ' '           TSTCNF
077200960205     C                   MOVE      *ZEROS        TSTICO
077300960205    4C                   ENDIF
077400960205     C*
077500960205     C                   UPDATE    TNTST000
077600960205    3C                   END
077700960205    2C                   END
077800960205     C                   ENDSR
077900960213     C********************  CONFERMA IMPORTO   ************************
078000960213     C     CONFIM        BEGSR
078100960213     C                   MOVE      ' '           WCONF             1
078200010823
078300010822      * calcolo il delta e controllo la tolleranza
078400010823     c                   eval      sav_tstili = tstili
078500010822     c                   if        tstipt <> tstili
078600010822     c                   eval      w_delta = (tstipt - tstili)
078700010910      * il delta deve essere come valore assoluto
078800010910     c                   mllzo     1             w_delta
078900010822     c                   if        w_delta <= §gettr
079000010822     c                   eval      sav_tstili = tstipt
079100010822     c                   endif
079200010822     c                   endif
079300010823
079400010822    1C     TSTIPT        IFEQ      sav_TSTILI
079500960213     C     TSTIPT        ANDGT     0
079600960213     C     TSTCAU        ANDNE     ' '
079700960213     C     TSTPDR        ANDGT     *ZEROS
079800010823
079900960213     C     KTSR4         SETLL     TNTSR04L                               30
080000960213    2C     *IN30         IFEQ      *OFF
080100010823      * calcolo il delta e controllo la tolleranza
080200010823     c                   eval      sav_tstili = tstili
080300010823     c                   if        tstico <> tstili
080400010823     c                   eval      w_delta = (tstico - tstili)
080500010910      * il delta deve essere come valore assoluto
080600010910     c                   mllzo     1             w_delta
080700010823     c                   if        w_delta <= §gettr
080800010823     c                   eval      sav_tstili = tstico
080900010823     c                   endif
081000010823     c                   endif
081100960226    3C     TSTCNF        IFNE      '2'
081200960221     C     TSTCNF        OREQ      '2'
081300010822     C     TSTICO        ANDEQ     sav_TSTILI
081400960213     C                   Z-ADD     TSTILI        TSTICO
081500960213     C                   MOVEL     '1'           TSTCNF
081600960213     C                   MOVE      '1'           WCONF             1
081700960221    3C                   END
081800960213    2C                   END
081900960213    1C                   END
082000960213     C                   ENDSR
082100960213     C****************  RICERCA LISTINO DA CARICARE  ******************
082200951130     C     SRLIST        BEGSR
082300951130     C* PRIMO TENTATIVO DI RICERCA : TRAZIONISTA E TIPO AUTOMEZZO DEL
082400951130     C*                              FOGLIO VIAGGIO
082500951130     C                   MOVE      '1'           W001A             1
082600951130     C                   MOVEL     FVSTMZ        KTMZ
082700951130     C                   MOVE      FVSPDR        KPDR
082800010725     C     KTLL          CHAIN     TITLL01L                           30
082900960906     C  N30TLLATB        COMP      ' '                                3030
083000951130    1C     *IN30         DOWEQ     *ON
083100951130     C     W001A         ANDLT     '4'
083200951130    2C                   SELECT
083300951130     C* SECONDO TENTATIVO DI RICERCA : TRAZIONISTA DEL FOGLIO VIAGGIO
083400951130     C*                                AUTOMEZZO GENERICO = '99'
083500951130     C     W001A         WHENEQ    '1'
083600951130     C                   MOVE      *ALL'9'       KTMZ
083700951130     C                   MOVE      '2'           W001A
083800951130     C* TERZO TENTATIVO DI RICERCA : TRAZIONISTA GENERICO = 9999999
083900951130     C*                              AUTOMEZZO DEL FOGLIO VIAGGIO
084000951130     C     W001A         WHENEQ    '2'
084100951130     C                   MOVE      *ALL'9'       KPDR
084200951130     C                   MOVEL     FVSTMZ        KTMZ
084300951130     C                   MOVE      '3'           W001A
084400951130     C* QUARTO TENTATIVO DI RICERCA : TRAZIONISTA E AUTOMEZZO GENERICI
084500951130     C     W001A         WHENEQ    '3'
084600951130     C                   MOVE      *ALL'9'       KTMZ
084700951130     C                   MOVE      '4'           W001A
084800951130    2C                   ENDSL
084900010725     C     KTLL          CHAIN     TITLL01L                           30
085000960906     C  N30TLLATB        COMP      ' '                                3030
085100951130    1C                   ENDDO
085200951130     C*
085300951130     C* LISTINO TROVATO: MEMORIZZO IMPORTO E CAUSALE CONTABILIZZAZIONE
085400951130    1C     *IN30         IFEQ      *OFF
085500010821      * se la divisa del listino non è uguale alla divisa della griglia
085600010821      * devo convertire tllili nella divisa della griglia
085700010821     c                   eval      w_importo = tllili
085800011023     c                   if        tlldiv <> tstdiv  and tllili <> 1
085900011023     c                             and tllili <> 0
086000010821     c                   eval      w_divisa = tlldiv
086100010821     c                   exsr      Sr_Converti
086200010821     c                   endif
086300010821     c                   eval      comili = w_importo
086400010821
086500010821     C**!!!              Z-ADD     TLLILI        COMILI
086600010821     C                   MOVEL     TLLCAU        COMCAU
086700951130    1C                   END
086800951130     C                   ENDSR
086900951108     C****************  STAMPA ERRORI SU VARIAZIONI  ******************
087000951108     C     PRTERR        BEGSR
087100960206     C                   SETOFF                                       151617
087200951117     C                   SETOFF                                       919293
087300951117     C                   SETOFF                                       949597
087400010822     c                   setoff                                       90
087500951117     C                   CLEAR                   PRTMS1
087600951117     C                   CLEAR                   PRTMS2
087700960222     C* DECODIFICHE PER LA PRIMA RIGA DI STAMPA
087800951108     C* Rovescio data traino
087900951108     C                   CLEAR                   WLBDA8
088000951123     C                   MOVE      D22DTN        G08INV
088100951108     C                   MOVEL     '3'           G08ERR
088200951108     C                   CALL      'XSRDA8'
088300951108     C                   PARM                    WLBDA8
088400951108     C                   MOVE      G08DAT        PRTDTN
088500960206     C* Decodifico filiale fermata partenza
088600960206     C     TSRFFP        CHAIN     AZORG01L                           30
088700960206     C  N30              MOVEL     ORGDES        PRTFFP           10
088800960206     C   30              CLEAR                   PRTFFP
088900960206     C* Rovescio data FV teorico
089000960206     C                   MOVE      TSRDTT        DSDATA
089100960206     C                   MOVEL     DSGG          PRTDTT            4 0
089200960206     C                   MOVE      DSMM          PRTDTT
089300960222     C* WERR='B' CHIUSURA BUCO E DEVO STAMPARE SOLO DATI FV
089400951127    1C     WERR          IFEQ      'B'
089500951123     C     WERR          OREQ      'E'
089600951117     C                   SETON                                        97
089700960304     C* Rovescio data partenza reale
089800960304     C                   MOVE      FVSDRP        DSDATA
089900960304     C                   MOVEL     DSGG          PRDRP1            4 0
090000960304     C                   MOVE      DSMM          PRDRP1
090100960304     C* Decodifico causale ritardo/anticipo
090200960304     C                   CLEAR                   PRCAU1
090300960304    2C     FVSRTP        IFNE      ' '
090400960304     C                   MOVEL     'TC'          KCOD
090500960304     C                   MOVEL(P)  FVSRTP        KKEY
090600960304     C     KTAB          CHAIN     TABEL00F                           30
090700011011     C  N30              MOVEL     TBLUNI        PRCAU1           22
090800960304    2C                   END
090900951127    2C     WERR          IFEQ      'B'
091000960304     C* Decodifica campi relativi agli impegni a pagare se è filiale
091100960304     C* che espone importo (*in98=on)
091200960304    3C     *IN98         IFEQ      *ON
091300960304     C                   CLEAR                   PRTAUT
091400960304     C                   MOVEL     'TU'          KCOD
091500960304     C                   MOVEL(P)  FVSTMZ        KKEY
091600960304     C     KTAB          CHAIN     TABEL00F                           30
091700960304     C  N30              MOVEL     TBLUNI        PRTAUT           10
091800960304     C   30              MOVE      *BLANKS       PRTAUT
091900960304     C                   MOVEL     FVSDPD        PRTDPD           12
092000960304    3C                   END
092100960206     C                   MOVEA     MSG(6)        K22
092200960206     C   96              MOVEL     MSG(5)        PRTMS2
092300960222   X2C                   ELSE
092400960222     C                   MOVEA     MSG(7)        K22
092500951127    2C                   ENDIF
092600951117     C                   MOVEL     FVSLNP        W003A             3
092700960206     C                   MOVEA     W003A         K22(14)
092800951117     C*
092900951117     C                   MOVEL     FVSNFV        W005A             5
093000951127     C                   MOVEA     W005A         SKI(11)
093100951117     C                   Z-ADD     11            I                 3 0
093200951117     C                   EXSR      ABBL
093300960206     C                   MOVEA     SKI(11)       K22(18)
093400960206     C                   MOVEA     K22           PRTMS1
093500951127   X1C                   ELSE
093600960208     C* Rovescio data partenza reale
093700960222     C                   CLEAR                   PRDRP1
093800960222     C     SAVDRP        IFGT      *ZEROS
093900960208     C                   MOVE      SAVDRP        DSDATA
094000960208     C                   MOVEL     DSGG          PRDRP1            4 0
094100960208     C                   MOVE      DSMM          PRDRP1
094200960222     C                   END
094300960208     C* Decodifico causale ritardo/anticipo
094400960208     C                   CLEAR                   PRCAU1
094500960208    2C     SAVRTP        IFNE      ' '
094600960208     C                   MOVEL     'TC'          KCOD
094700960208     C                   MOVEL(P)  SAVRTP        KKEY
094800960208     C     KTAB          CHAIN     TABEL00F                           30
094900011011     C  N30              MOVEL     TBLUNI        PRCAU1           22
095000960208    2C                   END
095100951117     C*
095200960206     C     *IN98         IFEQ      *ON
095300951108     C* Decodifico trazionista se diverso da 0 (Potrebbe essere = 0 se
095400951108     C* il traino non è ancora stato confermato)
095500951108     C                   CLEAR                   PRTRSC
095600951127    2C     SAVPDR        IFNE      *ZEROS
095700000807     C     KTNTLZ        CHAIN     TNTLZ01L                           30
095800951127    3C     *IN30         IFEQ      *OFF
095900951108     C     TLZATB        ANDEQ     *BLANKS
096000951127     C                   MOVEL     TLZRSC        PRTRSC           12
096100951127    3C                   END
096200951127    2C                   END
096300951108     C* Decodifico tipo automezzo
096400960222     C                   CLEAR                   PRTTMZ
096500960222     C     SAVTMZ        IFNE      *BLANKS
096600951108     C                   MOVEL     'TU'          KCOD
096700951108     C                   MOVEL(P)  SAVTMZ        KKEY
096800951108     C     KTAB          CHAIN     TABEL00F                           30
096900951108     C  N30              MOVEL     TBLUNI        PRTTMZ           10
097000960222     C                   END
097100951108     C* Rovescio data fattura
097200951108     C                   CLEAR                   PRTDFT
097300951127    2C     TSTDFT        IFGT      *ZEROS
097400951108     C                   CLEAR                   WLBDA8
097500951108     C                   MOVEL     TSTDFT        G08INV
097600951108     C                   MOVEL     '3'           G08ERR
097700951108     C                   CALL      'XSRDA8'
097800951108     C                   PARM                    WLBDA8
097900951108     C                   MOVEL     G08DAT        PRTDFT            4 0
098000951127    2C                   END
098100960206     C                   END
098200951108     C* DECODIFICHE SECONDA RIGA DI STAMPA
098300960206     C**
098400951117     C* INDICATORI PER STAMPA CAMPI SOLO SE VARIATI
098500960206     C     SAVDRP        COMP      FVSDRP                             1515
098600960206     C     SAVHRP        COMP      FVSHRP                             1616
098700960206     C     SAVRTP        COMP      FVSRTP                             1717
098800960209     C   98SAVPDR        COMP      FVSPDR                             9191
098900960209     C   98SAVTMZ        COMP      FVSTMZ                             9292
099000010725     C**!98SAVIPT        COMP      FVSIPT                             9393
099100010911      * non sto a convertire fwsipt perchè è già stato fatto prima
099200010911      * del richiamo a questa routine
099300010912     c   98savipt        comp      sav_fwsipt                         9393
099400960209     C   98SAVILI        COMP      TSTILI                             9494
099500960209     C   98SAVICO        COMP      TSTICO                             9595
099600010822     C   98SAVdiv        COMP      TSTdiv                             9090
099700960206     C* ROVESCIO DATA REALE PARTENZA SE VARIATA
099800960206     C     *IN15         IFEQ      *ON
099900960206     C                   MOVE      FVSDRP        DSDATA
100000960206     C                   MOVEL     DSGG          PRDRP2            4 0
100100960206     C                   MOVE      DSMM          PRDRP2
100200960206     C                   END
100300960206     C* DECODIFICO CAUSALE RITARDO/ANTICIPO SE VARIATA
100400960206     C     *IN17         IFEQ      *ON
100500960206     C                   MOVE      'TC'          KCOD
100600960206     C                   MOVEL(P)  FVSRTP        KKEY
100700960206     C     KTAB          CHAIN     TABEL00F                           30
100800011011     C  N30              MOVEL     TBLUNI        PRCAU2           22
100900960206     C   30              CLEAR                   PRCAU2
101000960206     C                   END
101100960206     C* Decodifico tipo automezzo del foglio viaggio se variato
101200960206    2C     *IN92         IFEQ      *ON
101300960206     C                   MOVEL     'TU'          KCOD
101400960206     C                   MOVEL(P)  FVSTMZ        KKEY
101500960206     C     KTAB          CHAIN     TABEL00F                           30
101600960206     C  N30              MOVEL     TBLUNI        PRTAUT           10
101700960206     C   30              MOVE      *BLANKS       PRTAUT
101800960206    2C                   END
101900960206     C**
102000960206     C                   MOVEL     FVSDPD        PRTDPD           12
102100951117     C**
102200951127    2C                   SELECT
102300960222     C* ERRORE: TRAINO GIA' FATTURATO
102400951108     C     WERR          WHENEQ    'F'
102500960206     C                   MOVEL     MSG(1)        PRTMS1           22
102600960206     C                   MOVEL     MSG(2)        PRTMS2           22
102700960222     C* SCONFERMATO IMPORTO
102800951108     C     WERR          WHENEQ    'S'
102900960222     C                   MOVEL     MSG(3)        PRTMS2                         SCONF.IMPORT
103000951108     C     WERR          WHENEQ    'R'
103100951108     C                   MOVEL     MSG(4)        PRTMS2
103200951114     C     WERR          WHENEQ    'C'
103300951114     C                   MOVEL     MSG(5)        PRTMS2
103400951127    2C                   ENDSL
103500951127    1C                   ENDIF
103600960222     C* SE CHIUSURA BUCO IMPOSTO ANCHE PRTMS1
103700960627    1C     WBUCO         IFEQ      'B'
103800960222     C                   MOVEA     MSG(6)        K22
103900960222     C                   MOVEL     FVSLNP        W003A             3
104000960222     C                   MOVEA     W003A         K22(14)
104100960222     C                   MOVEL     FVSNFV        W005A             5
104200960222     C                   MOVEA     W005A         SKI(11)
104300960222     C                   Z-ADD     11            I                 3 0
104400960222     C                   EXSR      ABBL
104500960222     C                   MOVEA     SKI(11)       K22(18)
104600960222     C                   MOVEA     K22           PRTMS1                         ABBIN.FV
104700960627   X1C                   ELSE
104800960627     C* IMPOSTO PRTMS1 ANCHE QUANDO NON E' POSSIBILE SOSTITUIRE LA
104900960627     C* CAUSALE RITARDO (LA CAUSALE K-NON PRESENTE RITARDO PER SEDE NON
105000960627     C* DEVE ESSERE SOSTITUITA)
105100960627    2C     SAVRTP        IFEQ      'K'
105200960627    3C     WERR          IFEQ      'V'
105300960627     C     WERR          OREQ      'S'
105400960627     C     WERR          OREQ      'C'
105500960627     C                   MOVEL     MSG(8)        PRTMS1
105600960627    3C                   END
105700960627    2C                   END
105800960627    1C                   END
105900951117     C**
106000951108     C* STAMPO INTESTAZIONE LA PRIMA VOLTA
106100951127    1C     WPRT          IFEQ      ' '
106200951108     C                   EXCEPT    TESTA
106300951108     C                   MOVE      '1'           WPRT              1
106400951127    1C                   END
106500951108     C   OF              EXCEPT    PIEDI
106600951108     C   OF              EXCEPT    TESTA
106700951108     C   OF              SETOFF                                       OF
106800010821
106900010823      * sposto i campi degli importi in campi di comodo per la stampa
107000010821     c                   z-add     savipt        p_savipt
107100010821     c                   z-add     savili        p_savili
107200010821     c                   z-add     savico        p_savico
107300010910     c                   z-add     w_fwsipt      p_fwsipt
107400010823     c                   z-add     comili        p_comili
107500010823     c                   z-add     tstico        p_tstico
107600951108     C                   EXCEPT    RIGERR
107700951108     C                   ENDSR
107800951222     C******************  STAMPA SU SYSPRT  ***************************
107900951222     C     PRTER1        BEGSR
108000951222     C     WPRT1         IFEQ      ' '
108100951222     C     *INOG         OREQ      *ON
108200951222     C                   MOVE      '1'           WPRT1             1
108300951222     C                   EXCEPT    TEST1
108400951222     C                   END
108500960221     C* DECODIFICO LNP FV
108600951222     C     FVSLNP        CHAIN     AZORG01L                           30
108700951222     C   30              CLEAR                   PRTDES
108800951222     C  N30              MOVEL     ORGDES        PRTDES           10
108900951222     C* ROVESCIO DATA TRAINO
109000951222     C                   CLEAR                   WLBDA8
109100951222     C                   MOVE      TSTDTN        G08INV
109200951222     C                   MOVEL     '3'           G08ERR
109300951222     C                   CALL      'XSRDA8'
109400951222     C                   PARM                    WLBDA8
109500951222     C                   MOVE      G08DAT        PRTDTR
109600960221     C* PREPARO CAMPO MESSAGGIO
109700960221     C                   SETOFF                                       3233
109800960221     C                   SELECT
109900960221     C* WERR1=F --> Traino già fatturato
110000960221     C     WERR1         WHENEQ    'F'
110100960221     C* ........... Rovescio numero fattura
110200960221     C                   CLEAR                   WLBDA8
110300960221     C                   MOVE      TSTDFT        G08INV
110400960221     C                   MOVEL     '3'           G08ERR
110500960221     C                   CALL      'XSRDA8'
110600960221     C                   PARM                    WLBDA8
110700960221     C                   MOVE      G08DAT        PRTFAT
110800960221     C                   SETON                                        32
110900960221     C* WERR1=A --> Traino già abbinato ad altro fv
111000960221     C     WERR1         WHENEQ    'A'
111100960221     C* ........... Rovescio data foglio viaggio
111200960221     C                   CLEAR                   WLBDA8
111300960221     C                   MOVE      TSRDFV        G08INV
111400960221     C                   MOVEL     '3'           G08ERR
111500960221     C                   CALL      'XSRDA8'
111600960221     C                   PARM                    WLBDA8
111700960221     C                   MOVE      G08DAT        PRTDFV
111800960221     C                   SETON                                        33
111900960221     C                   ENDSL
112000951222     C                   EXCEPT    RIG1
112100951222     C                   ENDSR
112200951222     C*************** ABBLANCA ZERO NON SIGNIFICATIVI *****************
112300951117     C     ABBL          BEGSR
112400951117     C*
112500951117     C     SKI(I)        DOWEQ     '0'
112600951117     C     I             ANDLT     15
112700951117     C                   MOVEL     ' '           SKI(I)
112800951117     C                   ADD       1             I
112900951117     C                   ENDDO
113000951117     C*
113100951117     C                   ENDSR
113200010821      *----------------------------------------------------------------
113300010821      * CONVERTI - Routine conversione importo
113400010821      *----------------------------------------------------------------
113500010821     c     Sr_Converti   begsr
113600010821
113700010821     c                   clear                   yeurcods
113800010821     c                   movel     w_divisa      yecdvi
113900010821     c                   movel     tstdiv        yecdvo
114000010821     c                   z-add     w_importo     yecimi
114100010821     c                   move      §cvdec        yecdco
114200010821     c                   call      'YEURCO'
114300010821     c                   parm                    yeurcods
114400010821    5c                   if        yecesi  = ' '
114500010821     c                   z-add     yecimo        w_importo
114600010821    5c                   endif
114700010821
114800010821     c                   endsr
114900010821
115000010725      *----------------------------------------------------------------
115100010821      * CARTAB   - Routine carico tabelle
115200010725      *----------------------------------------------------------------
115300010821     c     Sr_Cartab     begsr
115400010725
115500010821     c                   clear                   dscv
115600010821     c                   movel     'CV'          kcod
115700010821     c                   movel(p)  tstdiv        kkey
115800010821     c     ktab          chain     tabel00f
115900010821     c                   if        %found(tabel00f)
116000010821     c                   movel     tbluni        dscv
116100010821     c                   endif
116200010821
116300010821     c                   clear                   dgei
116400010821     c                   clear                   tibs02ds
116500010821     c                   movel     'L'           t02tla
116600010821     c                   movel     'C'           t02mod
116700010821     c                   movel     knsif         t02sif
116800010821     c                   movel     'GEI'         t02cod
116900010821     c                   movel     tstdiv        t02ke1
117000010821     c                   call      'TIBS02R'
117100010821     c                   parm                    kpjba
117200010821     c                   parm                    tibs02ds
117300010821    5c                   movel     t02uni        dgei
117400010725
117500010725     c                   endsr
117600010821
117700951117     C****************  PGM RICHIAMO: PRENDI I CAMPI CHE MI SERVONO****
117800951117     C     PGMRIC        BEGSR
117900951117     C                   CLEAR                   D25ERR
118000951117     C                   CLEAR                   FVSTRN
118100951117     C                   CLEAR                   D22DTN
118200951117     C                   CLEAR                   D22FES
118300951117     C                   CLEAR                   D22DDE
118400951117     C                   CLEAR                   D22NFM
118500951117     C*
118600951121     C*  Se data traino ricevuta = 0 la determino
118700951121     C     D25DTN        IFEQ      *ZEROS
118800951121     C     KTSR4A        CHAIN     TNTSR04L                           99
118900951121     C   99              MOVEL     '1'           D25ERR
119000951121     C  N99              Z-ADD     TSRDTN        D25DTN
119100951121     C                   END
119200951117     C                   Z-ADD     D25DTN        D22DTN
119300951117     C*
119400951117     C     KFVS          CHAIN     TNFVS01L                           99
119500951117     C   99              MOVEL     '1'           D25ERR
119600010822      * aggancio il file delle estensioni foglio viaggio per gli importi in
119700010822      * divisa
119800010822     c                   if        *in99 = *off
119900010910     c                   clear                   fwsipt
120000010910     c                   clear                   fwsies
120100010910     c                   clear                   fwsvip
120200010910     c                   clear                   fwsves
120300010822     c     kfws          chain     tnfws01l
120400010907      * se l'importo dell'estensione è > 0 prendo quello che trovo
120500010907      * perchè vuol dire che stanno già girando i programmi nuovi modificati per l'euro
120600010907     c                   if        fwsipt > 0
120700010907     c                   eval      w_fwsvip = fwsvip
120800010907     c                   z-add     fwsipt        w_fwsipt
120900010907     c                   endif
121000010907     c                   if        fwsies > 0
121100010907     c                   eval      w_fwsves = fwsves
121200010907     c                   z-add     fwsies        w_fwsies
121300010907     c                   endif
121400010907      * se l'importo dell'estensione è = 0 e l'importo sul file
121500010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
121600010907      * la divisa è lire
121700010907     c                   if        fwsipt = *zeros and fvsipt <> 0
121800010907     c                   eval      w_fwsvip = 'ITL'
121900010910     c                   z-add     fvsipt        w_fwsipt
122000010907     c                   endif
122100010907     c                   if        fwsies = *zeros and fvsies <> 0
122200010907     c                   eval      w_fwsves = 'ITL'
122300010907     c                   z-add     fvsies        w_fwsies
122400010907     c                   endif
122500010907      * se l'importo dell'estensione è = 0 e l'importo sul file
122600010907      * principale è = 0, imposto la divisa a blank per poi prendere quella della
122700010907      * griglia, così non sto a convertire un importo a 0
122800010907     c                   if        fwsipt = *zeros and fvsipt = 0
122900010907     c                   z-add     fvsipt        w_fwsipt
123000010907     c                   clear                   w_fwsvip
123100010907     c                   endif
123200010907     c                   if        fwsies = *zeros and fvsies = 0
123300010907     c                   z-add     fvsies        w_fwsies
123400010907     c                   clear                   w_fwsves
123500010907     c                   endif
123600010907     c                   endif
123700951117     C*
123800951117     C                   Z-ADD     FVSTRS        FVSTRN
123900951117     C*
124000951123     C     KTST          CHAIN(N)  TNTST01L                           99
124100951117     C  N99              MOVEL     TSTFES        D22FES
124200951117     C   99              MOVEL     '1'           D25ERR
124300010907      * se ho la divisa di comodo del foglio viaggio a blank devo prendere quella
124400010907      * della griglia
124500010907     c                   if        w_fwsvip = *blanks
124600010907     c                   eval      w_fwsvip = tstdiv
124700010907     c                   endif
124800010907     c                   if        w_fwsves = *blanks
124900010907     c                   eval      w_fwsves = tstdiv
125000010907     c                   endif
125100010822      * aggancio le tabelle
125200010822     c                   if        *in99 = *off
125300010822     c                   exsr      Sr_Cartab
125400010822     c                   endif
125500951117     C* CALCOLO DATA DECORRENZA
125600951117     C     D25TRN        SETLL     TNTLT01L
125700951117     C     D25TRN        READE     TNTLT01L                               30
125800951117     C     *IN30         DOWEQ     *OFF
125900960321     C     TLTATB        IFEQ      ' '
126000951117     C     D25DTN        ANDGE     TLTDDE
126100951117     C     D25DTN        ANDLE     TLTDSC
126200951117     C                   MOVEL     TLTDDE        D22DDE
126300951117     C                   SETON                                        30
126400951117     C                   ELSE
126500951117     C     D25TRN        READE     TNTLT01L                               30
126600951117     C                   ENDIF
126700951117     C                   ENDDO
126800951117     C     D22DDE        IFEQ      0
126900951117     C                   MOVEL     '1'           D25ERR
127000951117     C                   ENDIF
127100951117     C* NUMERO FERMATA
127200951123     C     KTLR3         CHAIN     TNTLR03L                           99
127300951117     C  N99              MOVEL     TLRNFM        D22NFM
127400951117     C   99              MOVEL     '1'           D25ERR
127500951123     C* ERRORE
127600951123     C     D25ERR        IFEQ      '1'
127700951123     C                   MOVEL     'E'           WERR
127800951123     C                   EXSR      PRTERR
127900951123     C                   CLEAR                   WERR
128000951123     C                   ENDIF
128100951117     C*
128200951117     C                   ENDSR
128300951117     C****************  OPERAZIONI INIZIALI   *************************
128400950926     C     *INZSR        BEGSR
128500951002     C     *ENTRY        PLIST
128600951002     C                   PARM                    KPJBA
128700951117     C                   MOVEL     KPJBU         DSTL25
128800960326     C* DETERMINO CODA PER INDIRIZZAMENTO STAMPE
128900960326     C                   MOVE      KNSIF         W004A             4
129000971217     C                   MOVEL     W004A         W001A             1
129100971217     C     W001A         IFEQ      'P'
129200040127     C                   MOVEL     'QPRINTS)'    WOUTQ
129300960326     C                   ELSE
129400120904     C                   MOVEL     'P046A034TR)' WOUTQ            11
129500960326     C                   END
129600960213     C* OVRPRTF SAVE(*YES)
129700960213     C                   CLEAR                   COMMAN
129800960326     C                   MOVEA     WOUTQ         CM1(28)
129900960326     C                   MOVEA     CM1           COMMAN           80
130000120904     C                   Z-ADD     71            LUNG             15 5
130100960213     C                   CALL      'QCMDEXC'                                                A
130200960213     C                   PARM                    COMMAN
130300960213     C                   PARM                    LUNG
130400960213     C                   OPEN      PRTF198
130500960213     C*
130600960213     C                   CLEAR                   COMMAN
130700040127     C                   MOVEA     WOUTQ         CM2(28)
130800960326     C                   MOVEA     CM2           COMMAN           80
130900120904     C                   Z-ADD     71            LUNG             15 5
131000960213     C                   CALL      'QCMDEXC'                                                A
131100960213     C                   PARM                    COMMAN
131200960213     C                   PARM                    LUNG
131300960213     C                   OPEN      SYSPRT
131400951117     C*
131500951117     C     D25RIC        IFEQ      'S'
131600951117     C                   SETON                                        10
131700951117     C                   ENDIF
131800951030     C*
131900951030     C                   Z-ADD     1             CODUT
132000951030     C                   CALL      'X§PARUT'
132100951030     C                   PARM                    UT§DSE
132200951107     C                   MOVEL     RAGUT         RSUT             20
132300951114     C* Rilevo la data del giorno
132400951107     C                   TIME                    WTIME            14 0          ORA E DATA
132500951107     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
132600951107     C                   MOVEL     WTIME         WORA              6 0
132700951107     C* Reperisco data ultimo controllo trazioni
132800951107     C     *DTAARA       DEFINE                  TNTL30
132900951107     C                   IN        TNTL30
133000951002     C*
133100000807     C     KTNTLZ        KLIST
133200000807     C                   KFLD                    KTLTIP            2
133300000807     C                   KFLD                    SAVPDR
133400000807     C                   MOVEL     'T '          KTLTIP
133500950928     C     KTSR          KLIST
133600951113     C                   KFLD                    FVSTRN
133700951113     C                   KFLD                    D22DTN
133800951113     C                   KFLD                    D22NFM
133900951123     C     KTLR3         KLIST
134000951117     C                   KFLD                    D25TRN
134100951117     C                   KFLD                    D22DDE
134200951117     C                   KFLD                    D25TFM
134300951117     C                   KFLD                    D25LNP
134400960206     C     KTSR3         KLIST
134500960206     C                   KFLD                    TSTTRN
134600960206     C                   KFLD                    TSTDTN
134700960206     C                   KFLD                    TSTFES
134800960206     C                   KFLD                    TSTNFE
134900951117     C     KTSR4         KLIST
135000951117     C                   KFLD                    KLNP
135100951117     C                   KFLD                    KNFV
135200951117     C                   KFLD                    TSTTRN
135300951117     C                   KFLD                    TSTDTN
135400951121     C     KTSR4A        KLIST
135500951121     C                   KFLD                    D25LNP
135600951121     C                   KFLD                    D25NFV
135700950929     C     KTST          KLIST
135800950929     C                   KFLD                    FVSTRN
135900951113     C                   KFLD                    D22DTN
136000951121     C     KTST1         KLIST
136100951121     C                   KFLD                    D29TRN
136200951121     C                   KFLD                    D29DTN
136300950929     C     KTLL          KLIST
136400950929     C                   KFLD                    FVSTRN
136500951114     C                   KFLD                    D22DDE
136600951130     C                   KFLD                    KTMZ
136700951130     C                   KFLD                    KPDR
136800951117     C     KFVS          KLIST
136900951117     C                   KFLD                    D25LNP
137000951117     C                   KFLD                    D25NFV
137100010725     C     KFWS          KLIST
137200010725     C                   KFLD                    fvsLNP
137300010725     C                   KFLD                    fvsNFV
137400951030     C     KTAB          KLIST
137500951030     C                   KFLD                    CODUT
137600951030     C                   KFLD                    KCOD
137700951030     C                   KFLD                    KKEY
137800950928     C*
137900951108     C     *LIKE         DEFINE    TSTDTN        PRTDTN
138000951030     C     *LIKE         DEFINE    TBLCOD        KCOD
138100951030     C     *LIKE         DEFINE    TBLKEY        KKEY
138200951123     C     *LIKE         DEFINE    TSTPDR        SAVPDR
138300951108     C     *LIKE         DEFINE    TSTTMZ        SAVTMZ
138400951108     C     *LIKE         DEFINE    TSTIPT        SAVIPT
138500951108     C     *LIKE         DEFINE    TSTILI        SAVILI
138600951108     C     *LIKE         DEFINE    TSTICO        SAVICO
138700951108     C     *LIKE         DEFINE    TSTILI        COMILI
138800951108     C     *LIKE         DEFINE    TSTCAU        COMCAU
138900951117     C     *LIKE         DEFINE    TSRLNP        KLNP
139000951117     C     *LIKE         DEFINE    TSRNFV        KNFV
139100951130     C     *LIKE         DEFINE    FVSPDR        KPDR
139200951130     C     *LIKE         DEFINE    FVSTMZ        KTMZ
139300951222     C     *LIKE         DEFINE    TSTDTN        PRTDTR
139400951222     C     *LIKE         DEFINE    TSTDFT        PRTFAT
139500960206     C     *LIKE         DEFINE    TSRDRP        SAVDRP
139600960206     C     *LIKE         DEFINE    TSRHRP        SAVHRP
139700960206     C     *LIKE         DEFINE    TSRRTP        SAVRTP
139800960221     C     *LIKE         DEFINE    TSRDFV        PRTDFV
139900960222     C     *LIKE         DEFINE    TSTCNF        SAVCNF
140000951117     C*
140100951117     C                   CLEAR                   KLNP
140200951117     C                   CLEAR                   KNFV
140300950926     C                   ENDSR
140400960205     OPRTF198   E            TESTA          1  1
140500951108     O                       PRT(2)             132
140600960206     O                       PRT(3)             198
140700951108     O                       RSUT                20
140800960205     O                       WDATE         8    185
140900960205     O                       PAGE          Z    198
141000951108     O          E            TESTA          2
141100951108     O                       KNSIF               10
141200951108     O                       KNMUS               21
141300960205     O                       WORA               185 '  :  :  '
141400951108     O          E            TESTA          1
141500960205     O                       PRT(6)              66
141600960205     O                       PRT(9)             132
141700960205     O                       PRT(12)            198
141800951108     O          E            TESTA          2
141900960205     O                       PRT(7)              66
142000960205     O                       PRT(10)            132
142100960205     O                       PRT(13)            198
142200960208     O          E            RIGERR         0
142300951123     O                       FVSTRN        Z      7
142400951127     O                       PRTDTN        8     18
142500960206     O                       TSRFFP              23
142600960206     O                       PRTFFP              34
142700011011     O                       PRTDTT              40 '  /  '
142800960206     O* PRIMA RIGA DI STAMPA
142900960208     O          E            RIGERR         1
143000960222     O*N97:STAMPA SU DUE RIGHE
143100011011     O              N97      PRDRP1              47 '  /  '
143200011011     O              N97      SAVHRP              53 '  :  '
143300011011     O              N97      SAVRTP              56
143400011011     O              N97      PRCAU1              79
143500960208     O*... 98:FV che espone importo
143600011011     O              N97 98   SAVPDR              88
143700011011     O              N97 98   PRTRSC             101
143800011011     O              N97 98   SAVTMZ             105
143900011011     O              N97 98   PRTTMZ             116
144000011011     o              n97 98   tstdiv             121
144100011011     O              N97 98   p_savili      4    134
144200011011     O              N97 98   p_savipt      4    147
144300101209     O              N97 98   TSTNFT        Z    168
144400011011     O              N97 98   p_savico      4    160
144500011011     O              N97 98   PRTDFT             174 '  /  '
144600960222     O*97:STAMPA SU UNA RIGA
144700011011     O               97      PRDRP1              47 '  /  '
144800011011     O               97      FVSHRP              53 '  :  '
144900011011     O               97      FVSRTP              56
145000011011     O               97      PRCAU1              79
145100960208     O*... 98:FV che espone importo
145200011011     O               97 98   FVSPDR              88
145300011011     O               97 98   PRTDPD             101
145400011011     O               97 98   FVSTMZ             105
145500011011     O               97 98   PRTAUT             116
145600011011     o               97 98   tstdiv             121
145700011011     O               97 98   p_COMILI      4    134
145800011011     O               97 98   p_fwsipt      4    147
145900011011     O               97 98   p_TSTICO      4    160
146000960222     O*96->Conferma importo
146100011011     o               97 96   tstdiv             121
146200011011     O               97 96   p_TSTICO      4    160
146300960205     O                       PRTMS1             198
146400960222     O* SECONDA RIGA DI STAMPA SE E' DA STAMPARE
146500951127     O          E   N97      RIGERR         1
146600011011     O               15      PRDRP2              47 '  /  '
146700011011     O               16      FVSHRP              53 '  :  '
146800011011     O               17      FVSRTP              56
146900011011     O               17      PRCAU2              79
147000011011     O               91      FVSPDR              88
147100011011     O               91      PRTDPD             101
147200011011     O               92      FVSTMZ             105
147300011011     O               92      PRTAUT             116
147400011011     o               90      tstdiv             121
147500011011     O               94      p_COMILI      4    134
147600011011     O               93      p_fwsipt      4    147
147700011011     O               95      p_TSTICO      4    160
147800960205     O                       PRTMS2             198
147900960206     O* SECONDA RIGA DI STAMPA PER STAMPA SECONDO MESSAGGIO 'CONFERMATO
148000960206     O* IMPORTO' A FRONTE DI CHIUSURA BUCO DI FV CHE NON ESPONE
148100960206     O          E    96      RIGERR         1
148200960206     O                       PRTMS2             198
148300960206     O*
148400951127     O          E            RIGERR         1
148500960205     O                       PRT(19)             66
148600960205     O                       PRT(19)            132
148700960205     O                       PRT(19)            198
148800960206     O          E            FINES       1
148900960206     O                       PRT(24)            132
149000951108     O          E            PIEDI            65
149100960205     O                       PRT(15)             66
149200960205     O                       PRT(16)            132
149300951108     O          E            PIEDI            66
149400960205     O                       PRT(17)             66
149500960205     O                       PRT(18)            132
149600951222     OSYSPRT    E            TEST1          1  1
149700960205     O                       PRT(20)             66
149800960205     O                       PRT(21)            132
149900951222     O                       RSUT                20
150000951222     O                       WDATE         8    119
150100951222     O                       PAGE1         Z    132
150200951222     O          E            TEST1          2
150300951222     O                       KNSIF               10
150400951222     O                       KNMUS               21
150500951222     O                       WORA               119 '  :  :  '
150600951222     O          E            TEST1          2
150700960205     O                       PRT(22)             66
150800951222     O          E            RIG1           1
150900960221     O               32      PRT(25)             66
151000960221     O               32      PRT(26)            132
151100960221     O               33      PRT(27)             66
151200960221     O               33      PRT(28)            132
151300951222     O                       FVSLNP        Z      3
151400951222     O                                            4 '/'
151500951222     O                       FVSNFV        Z      9
151600951222     O                       PRTDES              20
151700951222     O                       TSTTRN        Z     29
151800951222     O                       PRTDTR        8     40
151900101209     O               32      TSTNFT        Z     85
152000960221     O               32      PRTFAT        8    100
152100960221     O               33      TSRLNP              73
152200960221     O               33                          74 '/'
152300960221     O               33      TSRNFV              79
152400960221     O               33      PRTDFV        8     94
152500990716**  PRT
152600960205XXXXXXXXXXXXXXXXXXXX                                                 1
152700960205        ** ELENCO VARIAZIONI SU TRAINI GIA' CONTROLLATI  **          2
152800010725                               TNTL45R     XX/XX/XXXX    PAG. XXXX   3
152900960205XXXXXXXXXX XXXXXXXXXX                                                4
153000960205                                             XX:XX:XX                5
153100011016--- T R A I N O --|FOGLIO VIAGGIO TEORICO|-------- PARTENZA REALE    6
153200011011 NUMERO    DATA   | P.O.PARTENZA    DATA | DATA  ORA  CAUSALE RITA   7
153300011011XXXXXXX XX/XX/XXXX  XXX-1234567890 XX/XX  XX/XX XX:XX  X 123456789   8
153400011016-------------|---- TRAZIONISTA ----|TIPO AUTOMEZZO|---------------   9
153500011011RDO/ANTICIPO |                     |              | DIV      LISTI  10
1536000110110123456789012  1234567 123456789012  XX 1234567890  XXX XXXXXXXXX,  11
153700011011-- I M P O R T I -----------|F A T T U R A|                         12
153800011011NO    F.VIAGGIO   CONFERMATO| NUMERO  DATA|                         13
153900011011XX XXXXXXXXX,XX XXXXXXXXX,XX  XXXXXX XX/XX  1234567890123456789012  14
154000960205OGNI TRAINO VIENE STAMPATO SU DUE RIGHE RIPORTANDO: SULLA PRIMA RI  15
154100960213GA I DATI RELATIVI AL TRAINO PRIMA DELLE VARIAZIONI                 16
154200960206                                                    SULLA SECONDA   17
154300960205RIGA LE VARIAZIONI EFFETTUATE                                       18
154400011011------------------------------------------------------------------  19
154500960220XXXXXXXXXXXXXXXXXXXX ** ELENCO FOGLI VIAGGIO ABBINABILI A TRAINO M  20
154600010725A NON INSERIBILI IN GRIGLIA ** TNTL45R     XX/XX/XXXX    PAG. XXXX  21
154700960221FOGLIO VG.ABBINABILE  -- T R A I N O  --                            22
154800960221XXX/XXXXX XXXXXXXXXX  XXXXXXX XX/XX/XXXX                            23
154900960221              ***   FINE   STAMPA   ***                             24
155000960221                                            TRAINO GIA' FATTURATO   25
155100960221- FATTURA N.XXXXXX  DEL XX/XX/XXXX                                  26
155200960221                                            TRAINO GIA' ABBINATO A  27
155300960221 FV XXX/XXXXX DEL XX/XX/XXXX                                        28
155400951108** MSG
155500951108TRAINO GIA' FATTURATO                                                1
155600951108VARIAZ. NON EFFETTUATA                                               2
155700951108SCONFERMATO IMPORTO                                                  3
155800951108RICONFERMATO IMPORTO                                                 4
155900951114CONFERMATO   IMPORTO                                                 5
156000960206ABBINATO  FV XXX XXXXX                                               6
156100960206NON AGGIO.FV XXX XXXXX                                               7
156200960627NON AGGIO.CAUS.RITARDO                                               8
156300960326** CM1
156400120904OVRPRTF FILE(PRTF198) OUTQ(             SAVE(*YES) USRDTA('Ric.Variaz')
156500960326** CM2
156600120904OVRPRTF FILE(SYSPRT)  OUTQ(             SAVE(*YES) USRDTA('Ric.Variaz')
