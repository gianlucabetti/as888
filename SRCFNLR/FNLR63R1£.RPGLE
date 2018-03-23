000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200950313      *---------*----------------------------------------------------*
000300000728      * FNLR63R1*                                                    *
000400950313      *---------*                                                    *
000500950313      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *           -- DATI DISTINTA + CARICAMENTO --                  *
000700950313      *--------------------------------------------------------------*
000800950315     FFNLR63D   CF   E             WORKSTN
000900950317     F                                     SFILE(LR63S05:NRR5)
001000950315     FFNLR53D   O    E             WORKSTN
001100950315     F                                     SFILE(LR53S02:NRR2)
001200950315     F                                     SFILE(LR53S03:NRR3)
001300020902     FFNCDE01L  UF   E           K DISK    USROPN
001400020902     F                                     COMMIT
001500950315     FFNARB01L  UF   E           K DISK    USROPN
001600950313     F                                     COMMIT
001700990519     FFNANM01L  IF   E           K DISK
001800990519     F                                     INFDS(FNANM1)
001900990923     FFIARI01L  IF   E           K DISK
002000990923     FFIAR601L  IF   E           K DISK
002100051115     FFIAR901L  IF   E           K DISK    USROPN
002200950313     F                                     COMMIT
002300950313     FFNART01L  IF   E           K DISK    USROPN
002400950313     F                                     COMMIT
002500950320     FFNFVV01L  UF A E           K DISK    USROPN
002600950313     F                                     COMMIT
002700070515     FFidst01L  UF A E           K DISK    USROPN
002800070515     F                                     COMMIT
002900021204     FFIAPD01L  IF   E           K DISK
003000050406     Ftigcp21L  IF   E           K DISK
003100950606     FFNLBL02L  IF   E           K DISK
003200961114     FFNLBL01L  IF   E           K DISK
003300961114     F                                     RENAME(FNLBL000:FNLBL001)
003400950315     FTABEL00F  IF   E           K DISK
003500980323      *
003600950315     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003700980323      *
003800980323     FFNCDS03L  UF   E           K DISK    USROPN
003900980323     F                                     COMMIT
004000980323     F                                     RENAME(FNCDS000:FNCDS003)
004100991012     FTNTBE01L  IF   E           K DISK
004200110418     FFiar501l  IF   E           K DISK
004300950313      *----------------------------------------------------------------*
004400950313      *    s c h i e r e                                               *
004500950313      *----------------------------------------------------------------*
004600950314     D G15             S              8  0 DIM(5)                               DATA DISTINTA - 15 G
004700950314     D D3A             S             71    DIM(100)                             SKI TAB. 3A
004800950314     D C3A             S              2    DIM(100)                             SKI CHIAVI 3A
004900990923     D D1A             S             61    DIM(100)                             SKI TAB. 1A
005000990923     D C1A             S              2    DIM(100)                             SKI CHIAVI 1A
005100991012     D D7R             S             48    DIM(100)                             SKI TAB. 7R
005200991012     D C7R             S              2    DIM(100)                             SKI CHIAVI 7R
005300001019     D ERR             S             70    DIM(26) CTDATA PERRCD(1)             ERRORI
005400950315      *----------------------------------------------------------------
005500950315      *  DS
005600950315      *----------------------------------------------------------------
005700950313     D FNLR63        E DS                  EXTNAME(FNLR63DS)
005800950313      *
005900950313     D FNLR53        E DS                  EXTNAME(FNLR53DS)
006000950314     D  NDC                    1     35  0
006100950314     D                                     DIM(5)                               NUM.DISTINTA
006200950314     D  DTD                   36     75  0
006300950314     D                                     DIM(5)                               DATA DISTINTA
006400950314     D  DDC                   76    115  0
006500950314     D                                     DIM(5)                               DATA CHIUSURA
006600950314     D  HRC                  116    135  0
006700950314     D                                     DIM(5)                               ORA CHIUSURA
006800950314     D  FPP                  136    140
006900950314     D                                     DIM(5)                               TIPO USCITA
007000950314     D  PEP                  141    145
007100950314     D                                     DIM(5)                               PICKING S/N
007200950314     D  SET                  146    180  0
007300950314     D                                     DIM(5)                               ORA CHIUSURA
007400030204     D  CAR                  247    251
007500030204     D                                     DIM(5)                               Carico S/N
007600950314      *
007700000222     D Wdpd            s              1
007800020902     D dataiso         s               d
007900000222      *
008000950313     D KPJBA         E DS
008100950313     D  LIBSYS                92    100
008200950313     D  MSGERR               453    502
008300030718     D trul90ds      E DS                  inz
008400950313      *
008500110418     D dar5gen       E DS
008600110418     d TRULINTds     e ds                  inz
008700110418     d   iINTtip     e                     inz('I')
008800020121     D OG143         E DS
008900070515     D OG146         E DS
009000950315     D DS7R          E DS
009100950315     D DS3A          E DS
009200990923     D DS1A          E DS
009300991012     D DSCV          E DS
009400950313     D DS4G          E DS
009500991012     D DGED          E DS
009600051215     D fnlv80ds      e ds
009700021204     D fnlv24ds      e ds
009800021204     D fnlv55ds      e ds
009900020830     D ddatiute      e ds
010000020828     D azuteds       e ds                  extname(AZUTE00F)
010100020828     D tibs34ds      E DS                  inz
010200961114     D DSLR33        E DS                  EXTNAME(FNLR33DS)
010300991012     D YEURDS        E DS                  EXTNAME(YEURCODS)
010400990519     D FNANM1          DS
010500961114     D  ANMNR1               397    400B 0
010600950315      *
010700950315      *  DS per calcolo data da nr. giorni
010800950315     D WLBGIO          DS
010900950315     D  GIODAT                 1      8  0
011000950315     D  GIOINV                 9     16  0
011100950315     D  GIOTGI                17     21  0
011200950313      *  DS per inverisone data
011300950313     D WLBDA8          DS
011400950313     D  G02DAT                 1      8  0
011500950313     D  G02INV                 9     16  0
011600950313     D  G02ERR                17     17
011700950313     D  G02TGI                18     22  0
011800950314     D                 DS
011900950314     D  V2CHCO                 1      2  0
012000950314     D  V2CMCO                 3      4  0
012100950314     D  V2CHMC                 1      4  0
012200950314     D                 DS
012300950315     D  V2CFPD                 1      3  0
012400950315     D  V2CCPC                 4      7
012500950315     D  WCDCPC                 1      7
012600950315      * X  CHKOBJ FNFTDS0T
012700950315     D                 DS
012800950315     D  WFILC                 43     45  0
012900950315     D  WCMDC                  1     60
013000950315      * X  ALCOBJ FNFTDS0T
013100950315     D                 DS
013200950315     D  WFILA                 37     39  0
013300950315     D  WCMDA                  1     60
013400950315     D                SDS
013500950315     D  NOMPGM                 1     10
013600030318      *----------------------------------------------------------------
013700030318     D  Msg_alert1     c                   Const('Ricevuti più di 999 POD da el-
013800030318     D                                     aborare. <Proseguire -> ')
013900030318     D  Msg_alert2     c                   Const('ed in seguito effettuare una -
014000030318     D                                     ulteriore chiusura.>    ')
014100030318     D  Riesegui       c                   Const('Ci sono altre spediz.da chiud-
014200030318     D                                     ere: rieseguire il pgm')
014300950313      *----------------------------------------------------------------
014400950315      *  Definzione interna record AZORG
014500950315      *----------------------------------------------------------------
014600950315     IAZORG01L  AA
014700950315     I                             P    4    5 0ORGFIL
014800950315     I                                  6    6  ORGFVA
014900950315     I                                  7    7  ORGFAG
015000950315     I                                 14   33  ORGDES
015100000222     I                               3776 3800  ORGDE3
015200070515     i                               3851 3875  ORGDe6
015300950313      *---------------------------------------------------------------*
015400950313      * FLUSSO PRINCIPALE                                             *
015500950313      *---------------------------------------------------------------*
015600950313      *  Operazioni iniziali
015700950313     C     *ENTRY        PLIST
015800950313     C                   PARM                    KPJBA
015900950313     C                   MOVEL     KPJBU         FNLR63
016000020830     c* controllo se il po che mi arriva è gestito da qualcun'altro
016100020830     c                   exsr      srfgs
016200020830     c*
016300020830     C                   Z-ADD     D55tfa        KFGS
016400950322     C                   MOVEL     ' '           WFINE
016500950313      *  Se pgm richiamato per l'ultima volta chiudo in LR
016600950313     C     D63TRI        IFNE      'U'
016700950313      *  Impostazione campi distinta
016800950313     C                   EXSR      INZD02
016900950313      *  Loop gestione videate
017000950313     C     WFINE         DOUEQ     'S'
017100950313     C     WTPVID        CASEQ     '2'           GESD02
017200950313     C                   END
017300950313     C                   END
017400950313     C*
017500950313     C     FINE          TAG
017600950317     C                   MOVEL     FNLR63        KPJBU
017700950313     C                   SETON                                        RT
017800950313     C*
017900950313     C                   ELSE
018000950320     C     WCHIUS        IFEQ      'S'
018100950315     C                   EXSR      CALL53
018200950320     C                   END
018300950320     C     WSBLOC        IFEQ      'S'
018400950320     C                   MOVEL     FNLR63        KPJBU
018500950320     C                   CALL      'FNLR63R3'
018600950320     C                   PARM                    KPJBA
018700950320     C                   END
018800950426     C     WPGMER        IFEQ      'S'
018900950320     C                   MOVEL     FNLR63        KPJBU
019000950320     C                   CALL      'FNLR63R2'
019100950320     C                   PARM                    KPJBA
019200950320     C                   END
019300950320     C*
019400950320     C                   COMMIT
019500950313     C                   SETON                                        LR
019600950313     C                   END
019700950313      *---------------------------------------------------------*
019800950313      *  Inizializzo campi della prima videata                  *
019900950313      *---------------------------------------------------------*
020000950313     C     INZD02        BEGSR
020100950313      *
020200950313     C                   SETOFF                                       404142
020300950313     C                   SETOFF                                       2801
020400020904     C                   Z-ADD     D55tfa        V2CLNA
020500950313     C                   MOVEL     D63PDR        V2CFPD
020600950315     C                   MOVE      D63PDR        WCDCPC
020700950313     C                   Z-ADD     WOGGI         V2CDDC
020800950313     C                   Z-ADD     D63HMC        V2CHMC
020900950313     C                   MOVEL     D63FPP        V2CFPP
021000950313     C                   MOVEL     D63PEP        V2CPEP
021100950315     C                   MOVEL     D63NDT        V2CNDC
021200000426     C*--------------------------------------------------------------*
021300950313     C*  Controllo se ci sono record di FNCDE con il numero distinta
021400020830     C*  sia per il po richiesto che per il po di gestione:
021500000426     C*  in tal caso vuole dire che c'è stata una precedente elaboraz.
021600000426     C*  di chiusura distinte che si è interrotta quindi per poter
021700000426     C*  proseguire devo eseguire lo sblocco. (la chiusura distinta
021800020830     C*  estero prima imposta su FNCDE il nr. distinta poi una
021900000426     C*  volta confermati i dati e aggiornato FNARB cancella i record
022000000426     C*  di FNCDE aventi il nr. distinta = a quello del documento
022100020830     C*  appena confermato. Se viene richiesto di uscire
022200000426     C*  dal giro prima di aver completato la chiusura un apposito
022300000426     C*  pgm provvede a togliere da FNCDE i dati della distinta.)
022400000426     C*--------------------------------------------------------------*
022500020830     C                   Z-ADD     0             KNDC
022600020830     C                   Z-ADD     D63LNA        KLNA
022700950315     C     KCDE          SETGT     FNCDE01L
022800020830     C     klna          READE     FNCDE01L                               31
022900000426      * Se ce ne sono ...
023000950313     C     *IN31         IFEQ      '0'
023100000426      * ... libero CDE e
023200950320     C                   EXCEPT    NOAGG
023300000426      * ... accendo indicatore msg errore che avvisa di eseguire lo sblocco
023400950313     C                   SETON                                            01
023500950313     C                   END
023600950313     C*
023700950313     C                   ENDSR
023800950313      *---------------------------------------------------------*
023900950313      *  Gestione della seconda videata                         *
024000950313      *---------------------------------------------------------*
024100950313     C     GESD02        BEGSR
024200950313      *
024300001019     C*  Controllo numero righe fncde se 9999 mando messaggio
024400001019     C     d63nrr        IFEQ      9999
024500001019     C                   SETON                                        28
024600001019     C                   MOVEL     ERR(26)       $MSG
024700001019     C                   END
024800001019      *
024900950313     C                   EXFMT     LR63D02
025000950320     C                   SETOFF                                       404142
025100950320     C                   SETOFF                                       28
025200950313     C*  Fine Lavoro
025300950313     C     *INKC         IFEQ      '1'
025400950313     C                   MOVEL     '1'           D63F03
025500950313     C                   MOVEL     'S'           WFINE
025600950313     C                   GOTO      FINVD2
025700950313     C                   END
025800950313     C*  Ritorno
025900950313     C     *INKL         IFEQ      '1'
026000950320     C                   MOVEL     'S'           WFINE
026100950320     C                   GOTO      FINVD2
026200950313     C                   END
026300950313     C*  Sblocco
026400950313     C     *INKK         IFEQ      '1'
026500000426      * se richiesto richiamo pgm che va a togliere il nr. distinta
026600000426      * da FNCDE. Prima di richiedere lo sblocco bisogna appurare
026700000426      * che non ci sia un altro utente che sta chiudendo distinte
026800000426      * per la stessa lna
026900950317     C                   MOVEL     FNLR63        KPJBU
027000950313     C                   CALL      'FNLR63R3'
027100950313     C                   PARM                    KPJBA
027200950320     C                   MOVEL     KPJBU         FNLR63
027300950320     C                   MOVEL     'S'           WSBLOC
027400950317     C                   SETOFF                                       01
027500950313     C                   GOTO      FINVD2
027600950313     C                   END
027700950313     C*  Controlli
027800950313     C                   EXSR      CTR02
027900950313     C*
028000950313     C  N01*IN28         IFEQ      '0'
028100950314     C*  Esamino record di FNCDE per impostare nr.distinta
028200950314     C                   EXSR      UPDCDE
028300950314     C*
028400950313     C     WERROR        IFEQ      'S'
028500950313     C*  Se ho trovato degli errori chiamo pgm x la loro gestione
028600950313     C                   EXSR      GESERR
028700950313     C                   END
028800950314     C*  Carico dati per ditinta di consegna
028900950315     C     WFINE         IFNE      'S'
029000950320     C     D63NRR        IFEQ      0
029100950320     C                   MOVEL     'S'           WFINE
029200950320     C                   ELSE
029300950314     C                   EXSR      CARDIS
029400950313     C*  Richiamo pgm FNLR53R
029500961230     C     NRR2          IFNE      0
029600950313     C                   EXSR      CALL53
029700030319      *-
029800030319      * se aveva più di 999 P.O.D. deve uscire e rientrare per chiudere
029900030319      * una nuova distinta con i records residui.
030000030319      * Altrimenti tornerebbe sulla videata della distinta appena elaborata.
030100030319     c                   if        conta_cde = 999
030200030319     C                   movel     'S'           WFINE
030300030319     C                   goto      FINVD2
030400030319     c                   endiF
030500030319      *-
030600961230     C                   ELSE
030700961230     C                   SETON                                        28
030800961230     C                   MOVEL     ERR(23)       $MSG
030900961230     C                   END
031000950320     C                   END
031100950313     C                   END
031200950315     C*
031300950315     C                   END
031400950313     C*
031500950315     C     FINVD2        ENDSR
031600950314     C*-----------------------------------------------------*
031700950314     C*  Controlli su seconda videata
031800950314     C*-----------------------------------------------------*
031900950314     C     CTR02         BEGSR
032000000222      *
032100000222      * Verifico se LNA è DPD
032200021203     C     d63lna        CHAIN     azorg01l
032300020121      *
032400020121      * imposta i campi della DS
032500020121     c                   If        %found
032600020121     C                   movel     ORGDE3        OG143
032700070515     C                   movel     ORGDE6        OG146
032800020121     c                   endif
032900020121      *
033000020717      *  sostituito utilizzando il campo del network
033100020717     c                   EVAl      WDPD = *blank
033200020717     c                   if        §ogNTW = 'DPD'
033300020717     c                   EVAl      WDPD = 'S'
033400020717     c                   end
033500000222      *
033600000222      *   Controllo se richiesta ricerca codice padroncino
033700950314     C                   SETOFF                                           32
033800950315     C     '?'           SCAN      V2CCPC                                 32
033900950314     C   32              DO
034000021204     C                   CLEAR                   FNLV24DS
034100021204     C                   MOVEL     'R'           D24FLG
034200021204     C                   MOVEL     'A'           D24TIP
034300021204     C                   MOVEL     V2Cfpd        D24FIL
034400950315     C                   MOVE      *BLANKS       V2CCPC
034500021204     C                   MOVEL(P)  FNLV24DS      KPJBU
034600950314     C                   CALL      'FNLV24R'
034700950314     C                   PARM                    KPJBA
034800021204     C                   MOVEL     KPJBU         FNLV24DS
034900021204     C     D24PDR        IFNE      *ZEROS
035000021204     C                   MOVEL     D24PDR        WCDCPC
035100950314     C                   END
035200950315     C                   END
035300950314     C* Controllo se esiste nell'anagrafica padroncini
035400950314     C                   TESTN                   WCDCPC               32
035500950317     C     *IN32         IFEQ      '0'
035600950314     C                   SETON                                        2841
035700950314     C                   MOVEL     ERR(1)        $MSG
035800950314     C                   GOTO      FINCT2
035900950314     C                   END
036000950314     C*
036100950314     C                   MOVE      WCDCPC        APDPDR
036200021204     C     KAPD          CHAIN     FIAPD01L                           31
036300950314     C     *IN31         IFEQ      '1'
036400950314     C                   SETON                                        2841
036500950314     C                   MOVEL     ERR(2)        $MSG
036600950314     C                   GOTO      FINCT2
036700950314     C                   END
036800950316     C*
036900950316     C                   MOVE      'N'           D53WRT
037000971104     C     APDPDD        IFNE      'S'
037100971104     C*   e padroncino non è dipedente controllo se in filiale
037200971104     C*  di appartenza è attivata la gestione conteggi
037300971104     C                   MOVEL     APDPDR        ORGFIL
037400971104     C     ORGFIL        CHAIN     AZORG01L                           31
037500971104     C     *IN31         IFEQ      '0'
037600971104     C                   MOVE      'S'           D53WRT
037700971104     C                   END
037800971104     C                   END
037900971104     C*----------------------------------------------------*
038000971104     C*  Data distinta
038100971104     C     V2CDDC        IFEQ      0
038200971104     C                   SETON                                        2840
038300950314     C                   MOVEL     ERR(3)        $MSG
038400950314     C                   GOTO      FINCT2
038500950314     C                   END
038600950314     C*
038700950314     C                   CLEAR                   WLBDA8
038800950314     C                   MOVE      V2CDDC        G02DAT
038900950314     C                   CALL      'XSRDA8'
039000950314     C                   PARM                    WLBDA8
039100950314     C     G02ERR        IFEQ      '1'
039200950314     C                   SETON                                        2840
039300950314     C                   MOVEL     ERR(4)        $MSG
039400950314     C                   GOTO      FINCT2
039500950510     C                   ELSE
039600961114     C                   MOVEL     G02INV        §DDC
039700950510     C                   MOVE      G02DAT        V2CDDC
039800950314     C                   END
039900950314     C*
040000950314     C*  Controllo ora di consegna
040100950314     C     V2CHMC        IFEQ      0
040200950314     C                   SETON                                        4228
040300950314     C                   MOVEL     ERR(5)        $MSG
040400950314     C                   GOTO      FINCT2
040500950314     C                   END
040600950314     C*
040700950314     C     V2CHCO        IFGT      21
040800950314     C     V2CHCO        ORLT      7
040900950314     C     V2CHCO        ANDGT     0
041000950314     C     V2CMCO        ORNE      0
041100950314     C     V2CMCO        ANDGT     59
041200950314     C                   SETON                                        4228
041300950314     C                   MOVEL     ERR(6)        $MSG
041400950314     C                   END
041500950314     C*
041600950314     C     FINCT2        ENDSR
041700961114     C*--------------------------------------------------------------*
041800961114     C*  Sblocco spedizione se possibile
041900961114     C*--------------------------------------------------------------*
042000961114     C     SBLOCC        BEGSR
042100961114     C*
042200961114     C     ARBFBC        IFEQ      'A'
042300961114     C     ARBFBC        OREQ      'B'
042400961114     C                   MOVEL     *BLANKS       ARBFBC
042500961114     C                   MOVEL     *BLANKS       ARBCMC
042600961114     C                   ELSE
042700961114     C     ARBFBC        IFEQ      'G'
042800961114     C*  Controllo se la bolla ha una mamma
042900961114     C     KARB          CHAIN     FNLBL01L                           33
043000961114     C     *IN33         IFEQ      '0'
043100970521     C     LBLLAP        ANDEQ     LBLLAN
043200961114     C                   Z-ADD     LBLAAP        KAAS1
043300961114     C                   Z-ADD     LBLLPP        KLNP1
043400961114     C                   Z-ADD     LBLNRP        KNRS1
043500961114     C                   Z-ADD     LBLNSP        KNSP1
043600961114     C                   MOVEL     'S'           WFIGL
043700961114     C                   ELSE
043800961114     C                   Z-ADD     CDEAAS        KAAS1
043900961114     C                   Z-ADD     CDELNP        KLNP1
044000961114     C                   Z-ADD     CDENRS        KNRS1
044100961114     C                   Z-ADD     CDENSP        KNSP1
044200961114     C                   MOVEL     'N'           WFIGL             1
044300961114     C                   END
044400961114     C*  Verifico se posso sbloccare la spedizione
044500961114     C     WFIGL         IFEQ      'S'
044600140623     C     KGCA          CHAIN     tigcp21L
044700140623      ** tolto l'indicatore (35) xché condizionava campo a video in chiusura distinta
044800140623     C**** *IN35         IFEQ      '0'
044900140623     C**** GCPFAS        ANDNE     40
045000140623     C                   IF        %Found(tigcp21L) and gcpFAS <> 40
045100961114     C                   GOTO      FINSBL
045200961114     C                   END
045300961114     C                   ELSE
045400140623     C     KGCA          CHAIN     tigcp21L
045500140623     C**** *IN35         IFEQ      '0'
045600140623     C**** GCPFAS        ANDLT     35
045700060403      * adesso anche x fase 36
045800060403      *  ossia x disposizioni immesse in Partenza
045900140623     C**** *IN35         oreq      '0'
046000140623     C**** GCPFAS        ANDeq     36
046100140623      *
046200140623     C                   IF        %Found(tigcp21L) and gcpFAS < 35
046300140623     c                               or
046400140623     C                             %Found(tigcp21L) and gcpFAS = 36
046500961114     C                   GOTO      FINSBL
046600961114     C                   END
046700961114     C                   END
046800961114     C                   MOVEL     *BLANKS       ARBFBC
046900961114     C                   MOVEL     *BLANKS       ARBCMC
047000961114     C                   END
047100961114     C*
047200961114     C                   END
047300961114     C*
047400961114     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
047500961114     C                   CLEAR                   DSLR33
047600961114     C                   MOVE      '4'           D33FSC
047700961114     C                   Z-ADD     §DDC          D33DCH
047800961114     C                   MOVEL     'DI'          D33CCH
047900961114     C*
048000961114     C                   Z-ADD     140           KCAA
048100990519     C     KANM          CHAIN     FNANM01L                           37
048200961114     C     *IN37         DOWEQ     '0'
048300961114     C                   Z-ADD     ANMNR1        D33NRR
048400961114     C                   CALL      'FNLR33R'
048500961114     C                   PARM                    DSLR33
048600990519     C     KANM          READE     FNANM01L                               37
048700961114     C                   END
048800961114     C*
048900961114     C     FINSBL        ENDSR
049000950314     C*-----------------------------------------------------*
049100950314     C*  Aggiorno archivio FNCDE con il numero distinta
049200950314     C*-----------------------------------------------------*
049300950314     C     UPDCDE        BEGSR
049400980323      *
049500950320     C                   Z-ADD     0             D63NRR
049600030318     C                   Z-ADD     0             conta_cde         5 0
049700950322     C                   Z-ADD     0             KNDC
049800980324     C                   Z-ADD     *ZEROS        WDSE              1 0
049900980324     C                   MOVEL     'N'           WERROR            1
050000950317     C     KCDE          CHAIN     FNCDE01L                           31
050100030318      *
050200030318      * Al massimo il programma può gestire 999 records (STOP) per distinta
050300030318      * quindi occorre eseguire più volte il pgm creando più distinte
050400030318      * quando sono presenti più di 999 spedizioni da chiudere.
050500950317     C     *IN31         DOWEQ     '0'
050600030318     c     conta_cde     andlt     999
050700140620     C                   Z-ADD     0             WDSE                           azzera
050800030318      *
050900030318      * Il SFL può contenere al massimo 999 STOP,quindi,occorre fare
051000030318      *  più volte la distinta estero bloccando i records a gruppi
051100030318      *   di 999 per volta.
051200030318     c                   add       1             conta_cde
051300030318      *
051400961220     C                   MOVEL     'S'           WERRBL
051500961122     C                   Z-ADD     0             WNRERR            5 0
051600950315     C                   Z-ADD     CDEAAS        KAAS
051700950315     C                   Z-ADD     CDELNP        KLNP
051800950315     C                   Z-ADD     CDENRS        KNRS
051900950315     C                   Z-ADD     CDENSP        KNSP
052000980323      *  Bolla non trovata su FNARB
052100950314     C     KARB          CHAIN     FNARB01L                           32
052200950314     C     *IN32         IFEQ      '1'
052300980323     C                   Z-ADD     7             I
052400950314     C                   EXSR      WRTERR
052500950314     C                   GOTO      AGGREC
052600950314     C                   END
052700980323      *  Linea di arrivo archivio P.C. diversa da bolle arrivi
052800950315     C     ARBLNA        IFNE      CDELNA
052900980323     C                   Z-ADD     8             I
053000950314     C                   EXSR      WRTERR
053100950314     C                   GOTO      AGGREC
053200950314     C                   END
053300000222      *  Rif.numerico mittente diverso da bolle arrivi  NO x DPD
053400000222     C                   if        ARBRMN <> CDERMN and Wdpd <> 'S'
053500980323     C                   Z-ADD     9             I
053600950314     C                   EXSR      WRTERR
053700950314     C                   GOTO      AGGREC
053800950314     C                   END
053900980323      *  Spedizione già consegnata
054000950314     C     ARBDCM        IFNE      0
054100980323     C                   Z-ADD     11            I
054200950314     C                   EXSR      WRTERR
054300950314     C                   GOTO      AGGREC
054400950314     C                   END
054500980323      *  Spedizione in consegna in altra distinta
054600950314     C     ARBNDC        IFNE      0
054700020829     C     ARBNDC        ifne      CDENDC
054800980323     C                   Z-ADD     10            I
054900950314     C                   EXSR      WRTERR
055000950314     C                   GOTO      AGGREC
055100950314     C                   END
055200020829     C                   END
055300980323      *  Spedizione CON FIGLIA
055400950606     C     KARB          CHAIN     FNLBL02L                           33
055500950606     C     *IN33         IFEQ      '0'
055600980323     C                   Z-ADD     18            I
055700950606     C                   EXSR      WRTERR
055800950606     C                   GOTO      AGGREC
055900950606     C                   END
056000980323      *  Spedizione bloccata/giacente/lasc.avviso
056100950322     C     ARBFBC        IFNE      *BLANKS
056200000426      *  Verifico se è possibile sbloccare in automatico la sped.
056300961114     C                   EXSR      SBLOCC
056400000426      *  ... se ci riesco la sblocco, altrimenti se dopo l'esecuzione
056500000426      *  della subroutine la spedizione è ancora bloccata do errore
056600961114     C     ARBFBC        IFNE      *BLANKS
056700980323     C                   Z-ADD     16            I
056800950322     C                   EXSR      WRTERR
056900950322     C                   GOTO      AGGREC
057000950322     C                   END
057100961114     C                   END
057200980323      *  Spedizione non abilitata all'incasso del C/Ass.
057300950322     C                   Z-ADD     1             X
057400950322     C                   CLEAR                   DS3A
057500950322     C     ARBCBO        LOOKUP    C3A(X)                                 32
057600950322     C     *IN32         IFEQ      '1'
057700950322     C                   MOVEL     D3A(X)        DS3A
057800950322     C                   END
057900950321     C     ARBACC        IFNE      'S'
058000950322     C     §3AFCA        ANDEQ     0
058100950322     C     ARBICC        ANDEQ     ' '
058200980323     C                   Z-ADD     14            I
058300950321     C                   EXSR      WRTERR
058400950321     C                   GOTO      AGGREC
058500950321     C                   END
058600980323      *  Spedizione non abilitata all'incasso del Assegnato
058700950322     C                   MOVEL     §3ATB1        WPORTO            1
058800950322     C     ARBACA        IFNE      'S'
058900950322     C     WPORTO        IFEQ      'A'
059000950322     C     §3ATB2        ORNE      *BLANKS
059100980323     C                   Z-ADD     15            I
059200950321     C                   EXSR      WRTERR
059300961118     C                   GOTO      AGGREC
059400950321     C                   END
059500950322     C                   END
059600980323      *  Errore bloccante
059700961129     C     CDEFL1        IFEQ      'B'
059800980323     C                   Z-ADD     22            I
059900961129     C                   EXSR      WRTERR
060000961129     C                   GOTO      AGGREC
060100961129     C                   END
060200980323      *  Se c'è la data di consegna e quella di giacenza
060300961127     C     CDEDCM        IFNE      0
060400961127     C     CDEDAG        ANDNE     0
060500050406     C     KARB          CHAIN     tigcp21L                           33
060600961127     C                   MOVEL     'N'           WERRBL
060700980323      *  Se esiste giacenza con fase 35 prevale consegna
060800961127     C     *IN33         IFEQ      '0'
060900961127     C     GCPFAS        ANDEQ     35
061000961127     C                   Z-ADD     0             CDEDAG
061100961127     C                   ELSE
061200980323     C                   Z-ADD     19            I
061300961127     C                   EXSR      WRTERR
061400961127     C                   END
061500961127     C                   END
061600980323      *  Se c'è la data di consegna e (giacenza o lasciato avviso)
061700980323      *  oppure lasc.avviso e giacenza decide l'utente
061800961127     C     CDEDCM        IFNE      0
061900961127     C     CDEDLA        ANDNE     0
062000961127     C     CDEDLA        ORNE      0
062100961127     C     CDEDAG        ANDNE     0
062200970107     C                   MOVEL     'N'           WERRBL
062300980323     C                   Z-ADD     19            I
062400961127     C                   EXSR      WRTERR
062500961127     C                   END
062600980323      *  Motivo di consegna non gestibile
062700961118     C     CDEERR        IFEQ      'S'
062800980323     C                   Z-ADD     20            I
062900961118     C                   MOVEL     'N'           WERRBL
063000961118     C                   EXSR      WRTERR
063100961118     C                   END
063200000925      *contrassegno non annullato (distinta automatica) opzione 2 poste
063300000925     C     CDEERR        IFEQ      'C'
063400000925     C                   Z-ADD     25            I
063500000925     C                   MOVEL     'N'           WERRBL
063600000925     C                   EXSR      WRTERR
063700000925     C                   END
063800980324      *---------------------------------------
063900980324      *  * Se date a zero e non esiste dettaglio segnacolli
064000980402      *     - scrivo messaggio di impostare almeno una data.
064100980324      *  * Se date a zero ed esiste dettaglio segnacolli
064200980402      *     - scrivo messaggio di analizzare i segnacolli  V5CDSE = 2
064300980402      *-------------------------------------------------------------------------
064400980402      *  - scrivo msg di analizzare i segnacolli se eventi <>  V5CDSE = 2
064500980402      *  - non scrivo msg se segnacolli di eventi = (codice,data,ora) V5CDSE = 1
064600980402      *     SE RIPRISTINATA IMPLEMENTARE SCRIVENDO DATE E MSG DI FNCDS
064700980402      *-------------------------------------------------------------------------
064800980402      *
064900961120     C     CDEDCM        IFEQ      0
065000961120     C     CDEDLA        ANDEQ     0
065100961120     C     CDEDAG        ANDEQ     0
065200961120     C     CDEDRC        ANDEQ     0
065300980323      *
065400980323     C                   Z-ADD     CDEAAS        KAAS
065500980323     C                   Z-ADD     CDELNP        KLNP
065600980323     C                   Z-ADD     CDENRS        KNRS
065700980323     C                   Z-ADD     CDENSP        KNSP
065800980324     C     KARB          CHAIN     FNCDS003                           33
065900980324      *
066000980324     C     *IN33         IFEQ      '1'
066100980324     C                   Z-ADD     12            I
066200980324     C                   ELSE
066300980402      *
066400980402     C                   Z-ADD     24            I                              se ripristini
066500980402     C                   Z-ADD     2             WDSE                           specifiche *?*
066600980402     C                   END                                                    elimina queste
066700980324      *
066800970107     C                   MOVEL     'N'           WERRBL
066900961120     C                   EXSR      WRTERR
067000961120     C                   END
067100980324      *---------------------------------------
067200000210      *  Impossibile modificare codice cons.particolare ** FORZABILE **
067300000210      *   scrivo la consegna particolare nel msg e la levo dal sbf
067400000210     C                   SELECT
067500000210     C                   WHEN      CDEtc1 = *blanks
067600000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
067700000210     C                   WHEN      ARBtc1 = *blanks  or  ARBtc2 = *blanks
067800000210     C                   OTHER
067900000210     C                   movel     'N'           WERRBL
068000000210     C                   z-add     17            I
068100000210     C                   exsr      WRTERR
068200000210     C                   ENDSL
068300980406      *
068400980406      *  Se 2° evento mancanza scrivo messaggio
068500961120     C     CDEFL1        IFEQ      'S'
068600980323     C                   Z-ADD     22            I                 4 0
068700961120     C                   MOVEL     'N'           WERRBL            1
068800961120     C                   EXSR      WRTERR
068900961120     C                   END
069000980323      *
069100980323      *  Aggiorno record su FNARB
069200950320     C                   ADD       1             D63NRR
069300020830     C                   z-add     d55tfa        arbifp
069400020829     C                   Z-ADD     V2CNDC        ARBNDC
069500950314     C                   EXCEPT    AGGARB
069600980323      *
069700961120     C     AGGREC        TAG
069800980323      *
069900980323      *  Aggiorno FNCDE
070000950315     C                   Z-ADD     V2CNDC        CDENDC
070100950315     C                   EXCEPT    AGGCDE
070200980323      *
070300980323      *  Aggiorno FNCDS se esiste
070400980323     C                   Z-ADD     CDEAAS        KAAS
070500980323     C                   Z-ADD     CDELNP        KLNP
070600980323     C                   Z-ADD     CDENRS        KNRS
070700980323     C                   Z-ADD     CDENSP        KNSP
070800980323     C     KARB          SETLL     FNCDS003                               33
070900980323     C                   DO        *HIVAL
071000980323     C     KARB          READE     FNCDS003                               33
071100980323     C  N33              Z-ADD     V2CNDC        CDSNDC
071200980323     C  N33              EXCEPT    AGGCDS
071300980323     C  N33              END
071400980323      *
071500980323      *  Leggo prossimo record di FNCDE con numero distinta a 0
071600950317     C     KCDE          READE     FNCDE01L                               31
071700950315     C                   END
071800950314     C*
071900950317     C                   COMMIT
072000950317     C*
072100030318     C*  Se presenti più di 999 spedizioni manda un msg di Avviso
072200030318     C*  che non ha potuto elaborare tutto sotto la stessa distinta
072300030318     C*  e quindi occorre rieseguire il programma.
072400030318     c                   if        conta_cde = 999
072500030318     C*    INVIO MSG
072600030318     C                   movel     msg_alert1    WINMSG
072700030318     c                   exfmt     LR63W08
072800030318     C                   clear                   WINMSG
072900030318     C                   movel     msg_alert2    WINMSG
073000030318     c                   exfmt     LR63W08
073100030318     C                   clear                   WINMSG
073200030318     c                   endiF
073300030318     C*
073400950314     C                   ENDSR
073500950314     C*-----------------------------------------------------*
073600000426     C*  SCRITTURA ERRORI: la rountine gestisce la scrittura
073700000426     C*  di un subfile nascosto. Il programma di gestione
073800000426     C*  errori carica il sfl per la loro visualizzazione
073900000426     C*  da quest'ulitmo. L'utente esegue le operazioni per
074000000426     C*  eliminarli. Queste rimangono sospese fino a che non
074100000426     C*  viene data la conferma. Il programma procederà a
074200000426     C*  rileggere il sfl precedentemente aggiornato in   .
074300000426     C*  base alle scelte effettuate, a modificare altri
074400000426     C*  eventuali file.
074500000426     C*  Si caricherà poi gli eventuali errori rimasti a
074600000426     C*  video.
074700950314     C*-----------------------------------------------------*
074800950314     C     WRTERR        BEGSR
074900950314     C*
075000950314     C*  Se è la prima volta azzero subfile degli errori
075100950314     C     WERROR        IFEQ      'N'
075200980323     C                   Z-ADD     0             NRR5              4 0
075300950314     C                   MOVEL     'S'           WERROR
075400950315     C                   SETOFF                                       2021
075500950317     C                   WRITE     LR63C05
075600950314     C                   SETON                                        2021
075700950314     C                   END
075800961122     C*
075900961122     C*  Aggiorno numeratore errori
076000961122     C                   ADD       1             WNRERR
076100950314     C*  Se la data di consegna non c'è ma esiste la data
076200950314     C*  di riserva valorizzo la prima con la seconda
076300950315     C     CDEDRC        IFNE      0
076400950315     C     CDEDCM        ANDEQ     0
076500950315     C                   Z-ADD     CDEDRC        CDEDCM
076600950314     C                   END
076700950314     C*
076800950317     C                   ADD       1             NRR5
076900961122     C     WNRERR        IFGT      1
077000961122     C                   MOVEL     '1'           V5CIN3
077100961122     C                   ELSE
077200961122     C                   MOVEL     '0'           V5CIN3
077300961122     C                   END
077400961122     C                   MOVEL     *BLANKS       V5CSCE
077500950606     C                   MOVEL     *BLANKS       V5CTPD
077600980324     C                   MOVEL     WDSE          V5CDSE
077700000426     C                   MOVEL     WERRBL        V5CBLO                         errore bloc.
077800950317     C                   Z-ADD     CDEAAS        V5CAAS
077900950317     C                   Z-ADD     CDELNP        V5CLNP
078000950317     C                   Z-ADD     CDENRS        V5CNRS
078100950317     C                   Z-ADD     CDENSP        V5CNSP
078200950317     C                   Z-ADD     CDENSP        V5CNSP
078300950320     C                   Z-ADD     CDERMN        V5CRMN
078400961118     C                   MOVEL     CDECCA        V5CCCA
078500000210     C                   MOVEL     CDETC1        V5CTC1
078600961118     C                   MOVEL     CDESTS        V5CSTS
078700961120     C                   MOVEL     CDECOD        V5CCOD
078800961120     C                   MOVEL     CDEMOT        V5DCOD
078900961120     C                   MOVEL     CDEFTX        V5CFTX
079000961120     C                   MOVEL     CDEFL1        V5CFL1
079100950320     C                   CLEAR                   WLBDA8
079200950320     C                   Z-ADD     CDEDCM        G02INV
079300950320     C                   MOVEL     '3'           G02ERR
079400950320     C                   CALL      'XSRDA8'
079500950320     C                   PARM                    WLBDA8
079600950320     C                   Z-ADD     G02DAT        V5CDCM
079700950320     C                   Z-ADD     CDEDRC        G02INV
079800950320     C                   MOVEL     '3'           G02ERR
079900950320     C                   CALL      'XSRDA8'
080000950320     C                   PARM                    WLBDA8
080100950320     C                   Z-ADD     G02DAT        V5CDRC
080200950320     C                   Z-ADD     CDEDLA        G02INV
080300950320     C                   MOVEL     '3'           G02ERR
080400950320     C                   CALL      'XSRDA8'
080500950320     C                   PARM                    WLBDA8
080600950320     C                   Z-ADD     G02DAT        V5CDLA
080700950320     C                   Z-ADD     CDEDAG        G02INV
080800950320     C                   MOVEL     '3'           G02ERR
080900950320     C                   CALL      'XSRDA8'
081000950320     C                   PARM                    WLBDA8
081100950320     C                   Z-ADD     G02DAT        V5CGIA
081200000210      * MESSAGGIO DI ERRORE
081300000210     C                   SELECT
081400000210     C                   WHEN      I = 17
081500000210     C                   movel     ERR(I)        V5CERR
081600000210     c                   eval      %subst(V5Cerr:38:1) = CDEtc1
081700000210     C                   clear                   V5Ctc1
081800000210     C                   WHEN      I = 20
081900000210     C                   movel(P)  CDEMOT        V5CERR
082000000210     C                   OTHER
082100000210     C                   movel     ERR(I)        V5CERR
082200000210     C                   ENDSL
082300000426      * write riga sfl errori
082400950317     C                   WRITE     LR63S05
082500000210      *
082600950314     C                   ENDSR
082700950315     C*-----------------------------------------------------*
082800950315     C*  Gestione degli errori
082900950315     C*-----------------------------------------------------*
083000950315     C     GESERR        BEGSR
083100950315     C*
083200950315     C*  Richiamo pgm subfile gestione errori
083300950315     C                   MOVEL     FNLR63        KPJBU
083400950315     C                   CALL      'FNLR63R2'
083500950315     C                   PARM                    KPJBA
083600950320     C                   MOVEL     KPJBU         FNLR63
083700950426     C                   MOVEL     'S'           WPGMER
083800980323      *  Se non ho corretto tutti gli errori e sono uscita con
083900980323      *  F3 richiamo il programma per sbloccare le distinte
084000950315     C     D63F03        IFEQ      '1'
084100950315     C                   MOVEL     FNLR63        KPJBU
084200950315     C                   CALL      'FNLR63R3'
084300950315     C                   PARM                    KPJBA
084400950320     C                   MOVEL     KPJBU         FNLR63
084500950315     C                   MOVEL     'S'           WFINE
084600950320     C                   MOVEL     'S'           WSBLOC
084700950315     C                   END
084800980323      *
084900950315     C                   ENDSR
085000950314      *---------------------------------------------------------*
085100950314      *  Caricamento singole distinte                           *
085200950314      *---------------------------------------------------------*
085300950314     C     CARDIS        BEGSR
085400950314     C*
085500950315     C*  Impostazione dati in schiere
085600950315     C                   EXSR      INZSCH
085700950314     C*
085800950317     C                   Z-ADD     D63NDT        KNDC
085900950315     C     KCDE          CHAIN     FNCDE01L                           31
086000950320     C*  Aggiorno FNARB + archivio dei fogli viaggio
086100950320     C     *IN31         IFEQ      '0'
086200950320     C                   EXSR      UPDDIS
086300950320     C*
086400950314     C     *IN31         DOWEQ     '0'
086500950315     C*  Scrivo record di consegna e incassi
086600950315     C                   EXSR      WRTSBF
086700950314     C*
086800950320     C     KCDE          READE     FNCDE01L                               31
086900950314     C                   END
087000950320     C                   END
087100950317     C*
087200950317     C                   COMMIT
087300950315     C*
087400950315     C                   ENDSR
087500950315      *---------------------------------------------------------*
087600950315      *  Richiamo pgm. FNLR53R                                  *
087700950315      *---------------------------------------------------------*
087800950315     C     CALL53        BEGSR
087900950315     C*
088000950315     C                   MOVEL     D63TRI        D53TRI
088100950315     C                   Z-ADD     D63LNA        D53LNA
088200950315     C                   MOVEL     'E'           D53ITA
088300950315     C                   MOVEL     APDRSC        D53DSF
088400050516     C                   move      APDpdr        D53dsf
088500950530     C                   MOVEL     APDFCM        D53CMG
088600950315     C                   MOVEL     ' '           D53F03
088700950315     C                   MOVEL     'N'           D53WRT
088800950315     C                   MOVEL     FNLR53        KPJBU
088900950315     C                   CALL      'FNLR53R'
089000950315     C                   PARM                    KPJBA
089100030718     C                   PARM                    trul90ds
089200950315     C                   MOVEL     KPJBU         FNLR53
089300950320     C                   MOVEL     'S'           WCHIUS
089400950315     C*
089500020830     C*  Se non ho confermato la distinta richiamo pgm x sblocco
089600950323     C     D53F03        IFNE      ' '
089700950321     C                   MOVEL     FNLR63        KPJBU
089800950315     C                   CALL      'FNLR63R3'
089900950315     C                   PARM                    KPJBA
090000950320     C                   MOVEL     'S'           WSBLOC
090100950323     C     D53F03        IFEQ      '2'
090200950323     C                   MOVEL     'S'           WFINE
090300950323     C                   END
090400950322     C*  Ho confermato la distinta
090500950315     C                   ELSE
090600950322     C     D63TRI        IFNE      'U'
090700950322     C                   EXSR      CONFER
090800030318     C*  se confermato e ci sono altre bolle ancora da chiudere
090900030318     C*  viene lanciato un msg di avvertimento.
091000030318     c                   if        conta_cde = 999
091100030318     C*    INVIO MSG
091200030318     C                   movel     Riesegui      WINMSG
091300030318     c                   exfmt     LR63W08
091400030318     C                   clear                   WINMSG
091500030318     c                   endiF
091600950315     C                   END
091700950322     C*
091800950322     C                   END
091900950321     C                   CLEAR                   FNLR53
092000950317     C*
092100950317     C                   COMMIT
092200950315     C*
092300950315     C                   ENDSR
092400950315     C*-----------------------------------------------------*
092500950315     C*  Inizializzazione delle variabili nelle schiere
092600950315     C*-----------------------------------------------------*
092700950315     C     INZSCH        BEGSR
092800950315     C*
092900950315     C                   Z-ADD     0             NDC
093000950315     C                   Z-ADD     0             DTD
093100950315     C                   Z-ADD     0             DDC
093200950315     C                   Z-ADD     0             HRC
093300950315     C                   Z-ADD     0             SET
093400950315     C                   MOVEA     *BLANKS       FPP
093500950315     C                   MOVEA     *BLANKS       PEP
093600030204     C                   MOVEA     *BLANKS       CAR
093700950315     C*
093800950315     C*  Preimposto i dati nelle schiere in base al video
093900950315     C                   Z-ADD     V2CNDC        NDC(1)
094000950315     C                   Z-ADD     V2CDDC        DDC(1)
094100950315     C                   Z-ADD     V2CDDC        DTD(1)
094200950315     C                   Z-ADD     V2CHMC        HRC(1)
094300950315     C                   MOVEL     V2CFPP        FPP(1)
094400950315     C                   MOVEL     V2CPEP        PEP(1)
094500950315     C*  Converto la data della distinta nel formato AAAAMMGG
094600950315     C                   CLEAR                   WLBDA8
094700950315     C                   MOVE      V2CDDC        G02DAT
094800950315     C                   CALL      'XSRDA8'
094900950315     C                   PARM                    WLBDA8
095000950315     C                   Z-ADD     G02INV        §DDC
095100950315     C     G02TGI        SUB       15            GIOTGI
095200950315     C                   MOVE      *ZEROS        GIODAT
095300950315     C                   MOVE      *ZEROS        GIOINV
095400950315     C                   CALL      'XSRGI8'
095500950315     C                   PARM                    WLBGIO
095600950315     C                   MOVE      GIOINV        §G15              8 0
095700950315     C                   Z-ADD     §G15          G15(1)
095800950315     C*
095900950315     C                   SETON                                        62
096000950315     C                   WRITE     LR53C02
096100950315     C                   WRITE     LR53C03
096200950315     C                   SETOFF                                       6228
096300950315     C                   Z-ADD     0             SET
096400950315     C*
096500950315     C                   Z-ADD     0             NRR2              4 0
096600950315     C                   Z-ADD     0             NRR3              4 0
096700020611     C*
096800020611     C* inzializza il numeratore dello Stop se non è presente su ARB
096900020626     C                   Z-ADD     1             num_stop          5 0
097000950315     C*
097100950315     C                   ENDSR
097200950315      *---------------------------------------------------------*
097300950315      *  Aggiorno FNARB + FNFVV                                 *
097400950315      *---------------------------------------------------------*
097500950315     C     UPDDIS        BEGSR
097600950320     C*
097700950321     C                   Z-ADD     V2CNDC        KNFV
097800950321     C     KFVV          CHAIN     FNFVV01L                           96
097900950315     C* Scrivo il foglio viaggio bloccato
098000950315     C                   CLEAR                   FNFVV000
098100950317     C                   MOVEL     APDRSC        FVVDSF
098200020830     C                   MOVEL     d55tfa        FVVFGS
098300020916     C                   MOVEL     simFEL        FVVFLE
098400950315     C                   Z-ADD     §DDC          FVVDFV
098500950317     C                   Z-ADD     V2CNDC        FVVNFV
098600950315     C                   Z-ADD     4             FVVNPG
098700950315     C                   MOVEL     'S'           FVVFSC
098800950315     C                   MOVEL     V2CFPP        FVVFPP
098900950317     C                   MOVEL     V2CPEP        FVVPEP
099000950315     C                   MOVEL     'F'           FVVFCF
099100950321     C  N96              UPDATE    FNFVV000
099200950321     C   96              WRITE     FNFVV000
099300070515      *scrive nuovo file distinte FIDST se partita nuova gestione automatica
099400070515     c                   if        §ogdda <> *blank
099500070515     C                   Z-ADD     V2CNDC        KNFV6
099600070515     C     Kdst          CHAIN     Fidst01L                           96
099700070515     C* Scrivo il foglio viaggio bloccato
099800070515     c                   if        not %found(fidst01l)
099900070515     C                   clear                   Fidst000
100000070515     C                   MOVEL     APDRSC        dstnot
100100070515     C                   z-add     APDpdr        dstpdr
100200070515     C                   MOVEL     FVVFGS        dstfgs
100300070515     C                   Z-ADD     FVVDFV        dstdfv
100400070515     C                   Z-ADD     FVVNFV        dstnfv
100500070515     C                   Z-ADD     FVVNPG        dstnpg
100600070515     C                   MOVEL     FVVFSC        dstfca
100700070515     C                   MOVEL     FVVFPP        dstfpp
100800070515     C                   MOVEL     FVVPEP        dstpep
100900070515     C                   MOVE      'A'           dsttipa
101000070515     C                   MOVE      'N'           dstpda
101100070515     C                   WRITE     Fidst000
101200070515     c                   endif
101300070515     c                   endif
101400950315     C*
101500950315     C                   ENDSR
101600950315      *---------------------------------------------------------*
101700950315      *  Scrivo record subfile                                  *
101800950315      *---------------------------------------------------------*
101900950315     C     WRTSBF        BEGSR
102000950320     C*
102100950320     C* Eseguo chain su archivio bolle arrivo e lo fleggo
102200950320     C                   Z-ADD     CDEAAS        KAAS
102300950320     C                   Z-ADD     CDELNP        KLNP
102400950320     C                   Z-ADD     CDENRS        KNRS
102500950320     C                   Z-ADD     CDENSP        KNSP
102600950320     C     KARB          CHAIN     FNARB01L                           32
102700950320     C     *IN32         IFEQ      '0'
102800950320     C                   Z-ADD     §DDC          ARBDDC
102900950320     C                   Z-ADD     0             ARBNMI
103000950320     C                   MOVE      WCDCPC        ARBPDC
103100950321     C* Se merce disabilitata la abilito
103200950321     C     ARBAMA        IFEQ      0
103300950321     C                   MOVEL     3             ARBAMA
103400950321     C                   END
103500950320     C                   EXCEPT    AGGDIS
103600950320     C                   END
103700950315     C* Pulisco campi subfile
103800991012     C                   SETOFF                                       06
103900950315     C                   SETOFF                                       404142
104000950315     C                   SETOFF                                       434445
104100950315     C                   SETOFF                                       464748
104200991012     C                   SETOFF                                       080522
104300950315     C                   SETOFF                                       790703
104400950315     C                   CLEAR                   LR53S02
104500950315     C                   CLEAR                   LR53S03
104600950315     C                   Z-ADD     0             WIMA
104700970416     C                   Z-ADD     0             WCAS
104800970416     C                   MOVEL     *BLANKS       WVCA
104900990923     C                   MOVEL     *BLANKS       WDIV
105000970416     C                   MOVEL     *BLANKS       WTIC
105100950323     C                   Z-ADD     0             AR9CAS
105200950323     C                   MOVEL     *BLANKS       AR9VCA
105300950315     C* Incremento totale spedizioni x distinta
105400950315     C                   ADD       1             SET(1)
105500950315     C                   Z-ADD     V2CNDC        VNMDOC
105600950315     C                   Z-ADD     G15(1)        VH2G15
105700950315     C                   Z-ADD     §DDC          VH2DDC
105800950315     C                   Z-ADD     §DDC          VH2DDT
105900950315     C                   MOVEL     'NO'          V2CCON
106000950315     C*
106100110418     C                   clear                   V3CMIT
106200110418     C*                  MOVEL     ARBRSM        V3CMIT
106300110418     C*    ARBRMO        IFNE      *BLANKS
106400110418     C*                  MOVEL     ARBRMO        V3CMIT
106500110418     C*                  END
106600950315     C                   MOVEL     ARBRSD        V02DES
106700950315     C                   Z-ADD     ARBNTC        V2CNTC
106800950315     C                   Z-ADD     ARBAAS        V2CAAS
106900950315     C                   Z-ADD     ARBLNP        V2CLNP
107000950315     C                   Z-ADD     ARBNRS        V2CNRS
107100950315     C                   Z-ADD     ARBNSP        V2CNSP
107200950315     C                   MOVEL     ARBCBO        V2CCBO
107300950315     C                   MOVEL     ARBGMA        V2CGMA
107400950315     C     V2CGMA        COMP      *BLANKS                                08
107500950315     C                   MOVEL     ARBMGS        MM                2 0
107600950315     C                   MOVE      ARBMGS        GG                2 0
107700950315     C                   MOVEL     GG            V2CGMS
107800950315     C                   MOVE      MM            V2CGMS
107900950315     C                   MOVE      ARBSTP        V02STP
108000020611      *
108100020611      * Non è possibile avere lo stop a (0) quindi imposta il numero
108200020611      *  di stop tramite un numeratore
108300030318      *   ma attenzione si possono gestire al massimo 999 stop e non di più
108400030318      *   in tal caso si riinizia da (1).
108500020611     c                   if        v02stp = 0
108600020611     c                   eval      v02stp = num_stop
108700030318     c                   if        num_stop <> 999
108800020611     c                   eval      num_stop = num_stop + 1
108900030318     c                   else
109000030318     c                   eval      num_stop = 1
109100030318     c                   end
109200020611     c                   end
109300020611      *
109400970416     C* Controllo se esiste bolla mamma
109500970417     C     KARB          CHAIN     FNLBL01L                           76
109600970416     C     *IN76         IFEQ      '0'
109700970416     C     LBLLAN        ANDEQ     LBLLAP
109800980430     C                   Z-ADD     LBLAAP        KAAS
109900980430     C                   Z-ADD     LBLLPP        KLNP
110000980430     C                   Z-ADD     LBLNRP        KNRS
110100980430     C                   Z-ADD     LBLNSP        KNSP
110200980430     C     KARB          CHAIN     FNARB01L                           76
110300000426     C* Se bolla trovata e non chiusa con causale di reso ....
110400980430     C     *IN76         IFEQ      '0'
110500980430     C     ARBCCA        ANDNE     '2'
110600980430     C     ARBCCA        ANDNE     '6'
110700970416     C* Memorizzo il legame esistente per i successivi accessi su FNARB
110800970416     C                   Z-ADD     LBLAAP        VH2AAP
110900970416     C                   Z-ADD     LBLLPP        VH2LPP
111000970416     C                   Z-ADD     LBLNRP        VH2NRP
111100970416     C                   Z-ADD     LBLNSP        VH2NSP
111200970416     C                   END
111300000426     C* Riaggancio la figlia
111400980430     C                   Z-ADD     LBLAAN        KAAS
111500980430     C                   Z-ADD     LBLLPN        KLNP
111600980430     C                   Z-ADD     LBLNRN        KNRS
111700980430     C                   Z-ADD     LBLNSN        KNSP
111800980430     C     KARB          CHAIN     FNARB01L                           76
111900980430     C                   END
112000950315     C*  controllo se c'è il contrassegno
112100950315     C                   Z-ADD     1             X
112200950315     C                   CLEAR                   DS3A
112300950315     C     ARBCBO        LOOKUP    C3A(X)                                 32
112400950315     C     *IN32         IFEQ      '1'
112500950315     C                   MOVEL     D3A(X)        DS3A
112600950315     C                   END
112700950315     C*  Controllo se c'è assegnato
112800950315     C     ARBICA        IFEQ      ' '
112900990923     C     ARBICA        OREQ      'R'
113000990923     C     ARBFIP        OREQ      'I'
113100950315     C                   MOVEL     §3ATB1        WPORTO            1
113200950315     C     WPORTO        IFEQ      'A'
113300990923     C                   MOVEL     '1'           KTRC
113400990923     C                   ELSE
113500990923     C                   MOVEL     '2'           KTRC
113600990923     C                   END
113700001128     C     KAR6          CHAIN     FIAR601L                           32
113800990923     C     *IN32         IFEQ      '0'
113900001128     C     AR6DFT        ANDNE     *ZEROS
114000990923     C                   Z-ADD     AR6IFT        WIMA
114100990923     C                   MOVEL     AR6DIV        WDIV
114200990923     C* Loop su file incassi parziali x determinare importo da inc.
114300990923     C                   MOVEL     'A'           KTIP
114400990923     C     KARI          CHAIN     FIARI01L                           95
114500990923     C     *IN95         DOWEQ     '0'
114600991012     C     AR6DIV        IFEQ      ARIDIV
114700990923     C                   SUB       ARIIPP        WIMA
114800991012     C                   ELSE
114900991012     C* Se la divisa di incasso è diversa converto importo già
115000991012     C* incassato nella divisa della bolla
115100991012     C                   CLEAR                   YEURDS
115200991012     C                   MOVEL     ARIDIV        YECDVI
115300991012     C                   Z-ADD     ARIIPP        YECIMI
115400991012     C                   MOVEL     AR6DIV        YECDVO
115500991012     C                   MOVEL     'H'           YECTAR
115600991012     C                   CALL      'YEURCO'
115700991012     C                   PARM                    YEURDS
115800991012     C                   SUB       YECIMO        WIMA
115900991012     C                   END
116000990923     C     KARI          READE     FIARI01L                               95
116100990923     C                   END
116200990923     C                   Z-ADD     AR6DFT        V3CDFT
116300970416     C                   END
116400950315     C*
116500950315     C                   ELSE
116600950315     C                   MOVE      'S'           V2CICA
116700950315     C                   END
116800950315     C* Metto la 'V' in v02sel se per la bolla è prevista la gestione
116900950315     C* della firma del destinatario
117000950315     C     ARBGMA        IFNE      *BLANKS
117100950315     C                   Z-ADD     1             X
117200950315     C                   CLEAR                   DS7R
117300950315     C     ARBGMA        LOOKUP    C7R(X)                                 32
117400950315     C     *IN32         IFEQ      '0'
117500950315     C                   SETON                                        08
117600950315     C                   MOVEL     TBLUNI        DS7R
117700950315     C                   END
117800950315     C     §7R2FI        IFEQ      'S'
117900970107     C                   MOVEL     'V'           V02SEL
118000950315     C                   END
118100950315     C                   END
118200961121     C* Metto E se prevista visualizzazione note EDI
118300961121     C     CDEFTX        IFEQ      'S'
118400961211     C                   MOVEL     '*'           V02SEL
118500961125     C                   ELSE
118600961125     C     CDENO1        IFNE      *BLANKS
118700961125     C     CDENO2        ORNE      *BLANKS
118800970107     C                   MOVEL     '*'           V02SEL
118900961121     C                   END
119000961125     C                   END
119100950315     C*
119200950315     C                   MOVE      ARBTCR        V2CTCR
119300950315     C                   MOVE      ARBDCR        V2CDCR
119400950315     C                   MOVE      ARBHCR        V2CHCR
119500051215      * richiamo pgm per informazione relativamente alla data cons. rich.
119600051215      * se TASSATIVA
119700051215     c                   clear                   v2ctat
119800051215     c                   clear                   fnlv80ds
119900051215     c                   z-add     arbAAS        ILV80AAS
120000051215     c                   z-add     arbLNP        ILV80LNP
120100051215     c                   z-add     arbNRS        ILV80NRS
120200051215     c                   z-add     arbNSP        ILV80NSP
120300051215     c                   z-add     arbMGS        ILV80MGS
120400051215     c                   move      arbTCR        ILV80TCR
120500051215     c                   z-add     arbDCR        ILV80DCR
120600051215     c                   z-add     arbHCR        ILV80HCR
120700051215     c                   move      arbTC1        ILV80TC1
120800051215     c                   move      arbTC2        ILV80TC2
120900051215     c                   call      'FNLV80R'
121000051215     c                   parm                    kpjba
121100051215     c                   parm                    fnlv80ds
121200051215     c                   move      olv80tat      v2ctat
121300051215      *
121400950315     C*
121500950321     C     §3AFCA        IFGT      0
121600950921     C     ARBICC        IFEQ      'R'
121700950921     C     ARBICC        OREQ      ' '
121800950921     C* Controllo se C/Assegno sulla BOLLA
121900051115     C     KARB          CHAIN     FIAR901L                           32
122000970417     C     *IN32         IFEQ      '1'
122100970416     C* Controllo se C/Assegno sulla mamma
122200970416     C     VH2AAP        IFNE      0
122300970416     C                   Z-ADD     LBLAAP        KAAP
122400970416     C                   Z-ADD     LBLLPP        KLPP
122500970416     C                   Z-ADD     LBLNRP        KNRP
122600970417     C                   Z-ADD     LBLNSP        KNSPP
122700970416     C     KARB1         CHAIN     FNARB01L                           32
122800051115     C  N32KARB1         CHAIN     FIAR901L                           32
122900970416     C                   END
123000970416     C                   END                                                    --- 02 ---
123100970416     C     *IN32         IFEQ      '0'                                          --- 02 ---
123200970416     C                   Z-ADD     AR9CAS        WCAS
123300970416     C                   MOVEL     AR9VCA        WVCA
123400970416     C                   MOVEL     AR9TIC        WTIC
123500970416     C                   END                                                    --- 02 ---
123600950315     C* Carico cliente x controllo tabella "TM"
123700950315     C     ARBCCM        IFGT      *ZEROS
123800950315     C                   MOVE      ARBCCM        §CLITM
123900950315     C                   ELSE
124000950315     C                   MOVE      ARBKSC        §CLITM
124100950315     C                   END
124200950315     C                   ELSE
124300950315     C                   MOVE      'S'           V2CICC
124400950315     C                   END                                                    --- 01 ---
124500950315     C                   END                                                    --- 01 ---
124600950315     C* Conto i colli ancora da consegnare
124700970416     C*  Colli su figlia
124800950320     C     KARB          CHAIN     FNART01L                           32
124900950320     C     *IN32         IFEQ      '0'
125000950320     C     *IN32         DOWEQ     '0'
125100950315     C     ARTDCM        IFEQ      0
125200950315     C                   ADD       1             V02NCL
125300950315     C                   END
125400950320     C     KARB          READE     FNART01L                               32
125500950315     C                   END
125600970416     C*
125700970416     C                   ELSE
125800970416     C*  Colli su mamma
125900970416     C     VH2AAP        IFNE      0
126000970416     C                   Z-ADD     LBLAAP        KAAP
126100970416     C                   Z-ADD     LBLLPP        KLPP
126200970416     C                   Z-ADD     LBLNRP        KNRP
126300970417     C                   Z-ADD     LBLNSP        KNSPP
126400970416     C     KARB1         CHAIN     FNART01L                           95
126500970416     C     *IN95         DOWEQ     '0'
126600970416     C     ARTDCM        IFEQ      0
126700970416     C                   ADD       1             V02NCL
126800970416     C                   END
126900970416     C     KARB1         READE     FNART01L                               95
127000970416     C                   END
127100970416     C                   END
127200970416     C*
127300950315     C                   END
127400950315     C*
127500950315     C     V02NCL        IFNE      0
127600950315     C                   Z-ADD     ARTAAS        HARAAS
127700950315     C                   Z-ADD     ARTLNP        HARLNP
127800950315     C                   Z-ADD     ARTNRS        HARNRS
127900950315     C                   Z-ADD     ARTNSP        HARNSP
128000950315     C                   Z-ADD     ARTFLS        V2CFLS
128100950315     C                   END
128200950315     C                   ADD       1             NRR2
128300950315     C*  Routine per determinazione volume da utilizzare
128400950315     C                   EXSR      DETVOL
128500950315     C* Determino peso e volume ancora da consegnare
128600950315     C     ARBPKF        MULT      V02NCL        APPOG            18 3
128700950315     C     APPOG         DIV(H)    ARBNCL        V02PKG
128800950315     C     WVOL          MULT      V02NCL        APPOG            18 3
128900950315     C     APPOG         DIV(H)    ARBNCL        V02VOL
129000950315     C*  Scrivo record importi
129100950315     C     WIMA          IFNE      0
129200970416     C     WCAS          ORNE      0
129300950315     C                   Z-ADD     V2CAAS        V3CAAS
129400950315     C                   Z-ADD     V2CLNP        V3CLNP
129500950315     C                   Z-ADD     V2CNRS        V3CNRS
129600950315     C                   Z-ADD     V2CNSP        V3CNSP
129700950315     C                   MOVEL     V2CCBO        V3CCBO
129800950315     C                   MOVEL     V2CCON        V3CCON
129900950315     C                   MOVEL     V02DES        V3CDES
130000950315     C     NRR3          ADD       1             V2CNRD
130100950315     C                   MOVEL     *BLANKS       V3CTOT
130200950315     C                   Z-ADD     NRR2          V3CNRI
130300950315     C*  Controllo se esiste C/Assegno
130400970416     C     WCAS          IFGT      0
130500950315     C                   ADD       1             NRR3
130600950315     C                   MOVEL     'N'           V02SN1
130700950315     C                   MOVEL     'C/A'         V3DTPI
130800970416     C                   Z-ADD     WCAS          V3CIMP
130900970416     C                   MOVEL     WVCA          V3CVCA
131000990924     C                   MOVEL     WVCA          VH3DIV
131100110418     c* se incassi imposto la ragione sociale intestatario assegno.
131200110418     c     kar5          chain     fiar501l
131300110418     c                   if        %found(fiar501l)
131400110418     c                   movel     ar5uni        dar5gen
131500110418     c                   reset                   trulintds
131600110418     c                   eval      iINTtic  = wtic
131700110418     c                   eval      iINTrscM = arbrsm
131800110418     c                   eval      iINTrscO = arbrmo
131900110418     c                   eval      iINTcdi  = §AR5cdi
132000110418     c                   call      'TRULINTR'
132100110418     c                   parm                    TRULINTDS
132200110418     c                   if        oINTerr = *blank
132300110418     c                   eval      v3cmit  = oINTrsci
132400110418     c                   end
132500110418     c                   end
132600991012     C* Gestisco riga di totale solo se esiste anche assegnato
132700950315     C     WIMA          IFGT      0
132800991012     C* .. & la divisa di incasso assegnato è quella corrente...
132900011009     C     WDIV          IFEQ      §GEDCr
133000991012     C* .. o quella alternativa ..
133100991012     C     WDIV          OREQ      DIVEUR
133200991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
133300011009     C     WVCA          IFEQ      §GEDCr
133400991012     C* .. o quella alternativa ..
133500991012     C     WVCA          OREQ      DIVEUR
133600950315     C                   MOVEL     'NO'          V3CTOT
133700950315     C                   END
133800991012     C                   END
133900991012     C                   END
134000970416     C                   MOVEL     WTIC          V3CTIC
134100990923     C                   Z-ADD     1             X
134200990923     C     WTIC          LOOKUP    C1A(X)                                 95
134300990923     C                   MOVEL     D1A(X)        DS1A
134400991123     C     §1AFMB        COMP      'M'                                    06
134500950315     C                   WRITE     LR53S03
134600950315     C                   SETON                                        03
134700950315     C                   END
134800950315     C*  Controllo se esiste Assegnato
134900950315     C     WIMA          IFGT      0
135000950315     C                   ADD       1             NRR3
135100950315     C                   MOVEL     'N'           V02SN1
135200950315     C                   MOVEL     'Ass'         V3DTPI
135300950315     C                   Z-ADD     WIMA          V3CIMP
135400990924     C                   MOVEL     WDIV          VH3DIV
135500950315     C                   MOVEL     ARBMIF        V3CTIC
135600950315     C                   WRITE     LR53S03
135700950315     C                   MOVEL     *BLANKS       V3CDES
135800950315     C                   END
135900950315     C*  Se esitono entrambi  Controllo se esiste C/Assegno
136000970416     C     WCAS          IFGT      0
136100990923     C     WVCA          ANDEQ     WDIV
136200950315     C     WIMA          ANDGT     0
136300950315     C                   SETON                                        05
136400950315     C                   ADD       1             NRR3
136500950315     C                   MOVEL     'N'           V02SN1
136600950315     C                   MOVEL     'Tot'         V3DTPI
136700970416     C     WCAS          ADD       WIMA          V3CIMP
136800990923     C                   MOVEL     WDIV          V3CVCA
136900990924     C                   MOVEL     WDIV          VH3DIV
137000950315     C                   MOVEL     *BLANKS       V3CTIC
137100950315     C                   WRITE     LR53S03
137200950315     C                   END
137300950315     C*
137400991012     C*  Se esitono entrambi verifico se scrivere riga totale:
137500991012     C     WCAS          IFGT      0
137600991012     C     WIMA          ANDGT     0
137700991012     C* .. Divisa di incasso assegnato è quella corrente...
137800011009     C     WDIV          IFEQ      §GEDCr
137900991012     C* .. o quella alternativa ..
138000991012     C     WDIV          OREQ      DIVEUR
138100991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
138200011009     C     WVCA          IFEQ      §GEDCr
138300991012     C* .. o quella alternativa ..
138400991012     C     WVCA          OREQ      DIVEUR
138500991012     C                   SETON                                        05
138600991012     C                   ADD       1             NRR3
138700011009     C                   MOVEL     §GEDCr        V3DTPI
138800991012     C* Se l'assegnato non è in moneta di conto converto e sommo
138900991012     C* il risultato nel totale
139000011009     C     WDIV          IFNE      §GEDCr
139100991012     C                   CLEAR                   YEURDS
139200991012     C                   MOVEL     WDIV          YECDVI
139300991012     C                   Z-ADD     WIMA          YECIMI
139400011009     C                   MOVEL     §GEDCr        YECDVO
139500991012     C                   MOVEL     'H'           YECTAR
139600991012     C                   CALL      'YEURCO'
139700991012     C                   PARM                    YEURDS
139800991012     C                   Z-ADD     YECIMO        V3CIMP
139900991012     C                   ELSE
140000991012     C                   Z-ADD     WIMA          V3CIMP
140100991012     C                   END
140200991012     C* Se il C/Assegno non è in moneta di conto converto e sommo
140300991012     C* il risultato nel totale
140400011009     C     WVCA          IFNE      §GEDCr
140500991012     C                   CLEAR                   YEURDS
140600991012     C                   MOVEL     WVCA          YECDVI
140700991012     C                   Z-ADD     WCAS          YECIMI
140800011009     C                   MOVEL     §GEDCr        YECDVO
140900991012     C                   MOVEL     'H'           YECTAR
141000991012     C                   CALL      'YEURCO'
141100991012     C                   PARM                    YEURDS
141200991012     C                   ADD       YECIMO        V3CIMP
141300991012     C                   ELSE
141400991012     C                   ADD       WCAS          V3CIMP
141500991012     C                   END
141600011009     C                   MOVEL     §GEDCr        V3CVCA
141700011009     C                   MOVEL     §GEDCr        VH3DIV
141800991012     C                   MOVEL     *BLANKS       V3CTIC
141900991012     C                   WRITE     LR53S03
142000991012     C                   END
142100991012     C                   END
142200991012     C                   END
142300991012     C*
142400950315     C                   Z-ADD     NRR3          V2CNRA
142500950315     C                   END
142600950315     C*
142700950315     C*  Imposto i dati da archivio  P.C.
142800950315     C                   MOVEL     CDENO2        V2CFIR
142900950315     C                   MOVEL     CDENO1        V2CNT1
143000970107     C     CDEHMC        IFNE      9999
143100950321     C                   MOVEL     CDEHMC        V02HCO
143200950321     C                   MOVE      CDEHMC        V02MCO
143300970107     C                   END
143400950321     C                   MOVEL     ARBGC1        V021GC
143500950321     C                   MOVEL     ARBGC2        V022GC
143600950321     C*
143700950321     C     ARBGC1        IFNE      CDEGC1
143800950321     C     ARBGC2        ANDNE     CDEGC2
143900950322     C     ARBGC1        OREQ      CDEGC1
144000950322     C     ARBGC1        ANDEQ     *BLANKS
144100950322     C     ARBGC2        OREQ      CDEGC2
144200950322     C     ARBGC1        ANDEQ     *BLANKS
144300950321     C                   MOVEL     CDEGC1        V2CGC1
144400950321     C                   MOVEL     CDEGC2        V2CGC2
144500950321     C                   END
144600961118     C*  Gestisco consegne anomala
144700961118     C                   MOVEL     CDECCA        V02CCA
144800950321     C*  Gestisco consegne particolari
144900950321     C                   MOVEL     ARBTC1        V2CCP1
145000950321     C                   MOVEL     ARBTC1        V2HCP1
145100950321     C                   MOVEL     ARBTC2        V2CCP2
145200950321     C                   MOVEL     ARBTC2        V2HCP2
145300000210      *
145400000210     C                   SELECT
145500000210     C                   WHEN      CDEtc1 = *blanks
145600000210     C                   WHEN      ARBtc1 <> *blanks  and  ARBtc2 <> *blanks
145700000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
145800000210     C                   WHEN      ARBtc1 = *blanks
145900000210     C                   movel     CDETC1        V2CCP1
146000000210     C                   WHEN      ARBtc2 = *blanks
146100000210     C                   movel     CDETC1        V2CCP2
146200000210     C                   ENDSL
146300000210      *
146400950321     C                   CLEAR                   WLBDA8
146500950315     C     CDEDRC        IFNE      0
146600950315     C     CDEDCM        IFEQ      0
146700950315     C                   Z-ADD     CDEDRC        CDEDCM
146800950315     C                   END
146900950315     C                   MOVEL     'R  '         V02RIS
147000950315     C                   END
147100950315     C*  Se c'è la data di consegna o la data di riserva non considero
147200961128     C                   MOVEL     CDECMC        V02CCN
147300950315     C     CDEDCM        IFNE      0
147400950601     C                   Z-ADD     CDEDCM        G02INV
147500950315     C                   ELSE
147600950315     C     CDEDLA        IFGT      CDEDAG
147700990716     C     CDECMC        IFNE      'AV9'
147800950315     C                   MOVEL     'AVV'         V02CCN
147900990716     C                   ELSE
148000990716     C                   MOVEL     'AV9'         V02CCN
148100990716     C                   END
148200950315     C                   Z-ADD     CDEDLA        G02INV
148300950315     C                   ELSE
148400950315     C     CDECMC        IFNE      *BLANKS
148500950315     C                   MOVEL     CDECMC        V02CCN
148600950315     C                   ELSE
148700950315     C                   MOVEL     'G   '        V02CCN
148800950315     C                   END
148900950315     C                   Z-ADD     CDEDAG        G02INV
149000950315     C                   END
149100950315     C                   END
149200950315     C*
149300950315     C                   MOVEL     '3'           G02ERR
149400950315     C                   CALL      'XSRDA8'
149500950315     C                   PARM                    WLBDA8
149600950315     C                   MOVEL     G02DAT        V02DTC
149700950315     C*
149800950315     C                   WRITE     LR53S02
149900950315     C*
150000950315     C                   ENDSR
150100950315     C*-----------------------------------------------------*
150200950315     C     DETVOL        BEGSR
150300950315     C*-----------------------------------------------------*
150400950315     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
150500950315     C*            ALTRIMENTI
150600950315     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
150700950315     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
150800950315     C*                               ALTRIMENTI SOMMO ARBVOL
150900950315     C     ARBFVF        IFEQ      'T'
151000950315     C                   Z-ADD     ARBVLF        WVOL
151100950315     C                   ELSE
151200050208     C     ARBNCR        ifeq      ARBNCL
151300950315     C                   Z-ADD     ARBVLC        WVOL
151400950315     C*
151500950315     C                   ELSE
151600950315     C     ARBVLC        IFGE      ARBVLF
151700950315     C                   Z-ADD     ARBVLC        WVOL
151800950315     C                   ELSE
151900950315     C                   Z-ADD     ARBVLF        WVOL
152000950315     C                   ENDIF
152100950315     C                   ENDIF
152200950315     C                   ENDIF
152300950315     C*
152400950315     C                   ENDSR
152500950322     C*-----------------------------------------------------*
152600950322     C* Ho confermato la chiusura distinta
152700950322     C*-----------------------------------------------------*
152800950322     C     CONFER        BEGSR
152900950322     C*
153000950322     C                   Z-ADD     V2CNDC        KNFV
153100950322     C     KFVV          CHAIN     FNFVV01L                           96
153200950322     C* Controllo se il flag chiusura è uguale a 'F' lo pulisco
153300950322     C  N96FVVFCF        IFEQ      'F'
153400950322     C                   MOVE      ' '           FVVFCF
153500950322     C                   EXCEPT    AGGFVV
153600950322     C                   END
153700950322     C                   MOVEL     'S'           WFINE
153800980323      *
153900980323      *  Cancello record da FNCDE e/o FNCDS.
154000980323      *
154100950322     C                   Z-ADD     D63NDT        KNDC
154200961127     C     KCDE          CHAIN     FNCDE01L                           31
154300020902      *
154400980324     C     *IN31         DOWEQ     '0'                                               -- 1
154500980323      *
154600980323     C                   Z-ADD     CDEAAS        KAAS
154700980323     C                   Z-ADD     CDELNP        KLNP
154800980323     C                   Z-ADD     CDENRS        KNRS
154900980323     C                   Z-ADD     CDENSP        KNSP
155000980323     C     KARB          SETLL     FNCDS003                               33
155100980323      *
155200980324      *  Cancello record da FNCDS se il numero distinta è impostato.
155300980324     C                   DO        *HIVAL                                            -- 2
155400980323     C     KARB          READE     FNCDS003                               33
155500980324     C  N33CDSNDC        IFEQ      CDENDC                                            -- 3
155600980323     C                   DELETE    FNCDS003
155700980324     C                   END                                                         3 --
155800980324     C  N33              END                                                         2 --
155900980324      * Controllo se restano RCD di FNCDS
156000980324     C     KARB          CHAIN     FNCDS003                           33
156100980324      *  Cancello FNCDE se non esistono record di FNCDS e se non è il secondo
156200980324      *    evento 20 - 117 con bolla senza il primo già confermato.
156300980324     C     CDEFL2        IFEQ      'S'                                               -- 2
156400961127     C                   MOVEL     *BLANKS       CDEFL2
156500961127     C                   MOVEL     *BLANKS       CDEERR
156600970225     C                   Z-ADD     0             CDENDC
156700980401     C                   MOVEL     *BLANKS       CDEFL3
156800961127     C                   EXCEPT    AGGFL2
156900980324     C                   ELSE                                                        -- 2 --
157000980324      *
157100980324     C     *IN33         IFEQ      '1'                                               -- 3
157200980324     C                   DELETE    FNCDE01L
157300980324     C                   ELSE                                                        -- 3 --
157400980324      *  Se resta solo un rcd di FNCDS a totale aggiorno FNCDE e cancello FNCDS
157500980324     C     CDSSEE        IFEQ      *BLANKS                                           -- 4
157600980324     C     KARB          READE     FNCDS003                               33
157700980324     C     *IN33         IFEQ      '0'                                               -- 5
157800980324     C                   EXSR      MUOVI
157900980324     C                   UPDATE    FNCDE000
158000980324     C                   DELETE    FNCDS003
158100980324     C                   END                                                         5 --
158200980401      *
158300980401      *  Se restano FNCDS a dettaglio ed CDEFL3 = 1 normalizzo FNCDE
158400980401     C                   ELSE                                                        -- 4 --
158500980401     C     CDEFL3        IFEQ      '1'                                               -- 5
158600980401     C                   EXSR      MUOVI0
158700980401     C                   UPDATE    FNCDE000
158800980401     C                   END                                                         5 --
158900980324     C                   END                                                         4 --
159000980324      *
159100980324     C                   END                                                         3 --
159200980324     C                   END                                                         2 --
159300980323      *
159400961127     C     KCDE          READE     FNCDE01L                               31
159500980324     C                   END                                                         1 --
159600980323      *
159700950322     C                   ENDSR
159800980324      *-----------------------------------------------------*
159900980324      *  Aggiono FNCDE con valori di FNCDS
160000980324      *-----------------------------------------------------*
160100980324     C     MUOVI         BEGSR
160200980324      *
160300980324     C                   Z-ADD     CDSDCM        CDEDCM
160400980324     C                   Z-ADD     CDSHMC        CDEHMC
160500980324     C                   Z-ADD     CDSDLA        CDEDLA
160600980324     C                   Z-ADD     CDSDAG        CDEDAG
160700980324     C                   Z-ADD     CDSDRC        CDEDRC
160800980324     C                   MOVEL     CDSNO1        CDENO1
160900980324     C                   MOVEL     CDSNO2        CDENO2
161000980324     C                   MOVEL     CDSCMC        CDECMC
161100980324     C                   MOVEL     CDSCCA        CDECCA
161200000210     C                   MOVEL     CDSTC1        CDETC1
161300980324     C                   MOVEL     CDSERR        CDEERR
161400980324     C                   MOVEL     CDSSTS        CDESTS
161500980324     C                   MOVEL     CDSCOD        CDECOD
161600980324     C                   MOVEL     CDSMOT        CDEMOT
161700980324     C                   MOVEL     CDSFL1        CDEFL1
161800980401     C                   MOVEL     *BLANKS       CDEFL3
161900980324      *
162000980324     C                   ENDSR
162100980401      *-----------------------------------------------------*
162200980401      *  Azzero FNCDE
162300980401      *-----------------------------------------------------*
162400980401     C     MUOVI0        BEGSR
162500980401      *
162600980401     C                   Z-ADD     *ZEROS        CDEDCM
162700980401     C                   Z-ADD     *ZEROS        CDEHMC
162800980401     C                   Z-ADD     *ZEROS        CDEDLA
162900980401     C                   Z-ADD     *ZEROS        CDEDAG
163000980401     C                   Z-ADD     *ZEROS        CDEDRC
163100980401     C                   MOVEL     *BLANKS       CDENO1
163200980401     C                   MOVEL     *BLANKS       CDENO2
163300980401     C                   MOVEL     *BLANKS       CDECMC
163400980401     C                   MOVEL     *BLANKS       CDECCA
163500000210     C                   MOVEL     *BLANKS       CDETC1
163600980401     C                   MOVEL     *BLANKS       CDEERR
163700980401     C                   MOVEL     *BLANKS       CDESTS
163800980401     C                   MOVEL     *BLANKS       CDECOD
163900980401     C                   MOVEL     *BLANKS       CDEMOT
164000980401     C                   MOVEL     *BLANKS       CDEFL1
164100980401     C                   MOVEL     *BLANKS       CDEFL2
164200980401     C                   MOVEL     *BLANKS       CDEFL3
164300980401      *
164400980401     C                   ENDSR
164500980324     C*-----------------------------------------------------*
164600980324     C*  Operazioni iniziali
164700980324     C*-----------------------------------------------------*
164800980324     C     *INZSR        BEGSR
164900980324      *
165000980324      * Apertura controllata dei files
165100950315     C                   OPEN      FNCDE01L                             16
165200950320     C  N16              OPEN      FNFVV01L                             16
165300070515     C  N16              OPEN      Fidst01L                             16
165400950313     C  N16              OPEN      FNARB01L                             16
165500051115     C  N16              OPEN      FIAR901L                             16
165600950313     C  N16              OPEN      FNART01L                             16
165700980324     C  N16              OPEN      FNCDS03L                             16
165800950313     C   16              GOTO      FINE                                         OPEN vado a fine
165900980324      *
166000950313     C*  Definizione chiavi
166100950315     C     KARB          KLIST
166200950313     C                   KFLD                    KAAS
166300950313     C                   KFLD                    KLNP
166400950313     C                   KFLD                    KNRS
166500950313     C                   KFLD                    KNSP
166600990923     C     KAR6          KLIST
166700990923     C                   KFLD                    KAAS
166800990923     C                   KFLD                    KLNP
166900990923     C                   KFLD                    KNRS
167000990923     C                   KFLD                    KNSP
167100990923     C                   KFLD                    KTRC
167200990923     C     KARI          KLIST
167300990923     C                   KFLD                    KAAS
167400990923     C                   KFLD                    KLNP
167500990923     C                   KFLD                    KNRS
167600990923     C                   KFLD                    KNSP
167700990923     C                   KFLD                    KTIP
167800961114     C*
167900961114     C     KGCA          KLIST
168000961114     C                   KFLD                    KAAS1
168100961114     C                   KFLD                    KLNP1
168200961114     C                   KFLD                    KNRS1
168300961114     C                   KFLD                    KNSP1
168400961114     C     KANM          KLIST
168500990519     C                   KFLD                    KAAS1
168600961114     C                   KFLD                    KLNP1
168700961114     C                   KFLD                    KNRS1
168800961114     C                   KFLD                    KNSP1
168900961114     C                   KFLD                    KCAA
169000950313     C*
169100950313     C     KFVV          KLIST
169200950313     C                   KFLD                    KNPG
169300950313     C                   KFLD                    KNFV
169400950313     C                   KFLD                    KFGS
169500070515     C*
169600070515     C     Kdst          KLIST
169700070515     C                   KFLD                    KNPG
169800070515     C                   KFLD                    KNFV6             6 0
169900070515     C                   KFLD                    KFGS
170000950313     C*
170100950315     C     KTAB          KLIST
170200950313     C                   KFLD                    KKUT
170300950313     C                   KFLD                    KCOD
170400991012     C     KTBE          KLIST
170500991012     C                   KFLD                    KCODE
170600991012     C                   KFLD                    KKEYE
170700950315     C*
170800950315     C     KCDE          KLIST
170900950315     C                   KFLD                    KLNA
171000950315     C                   KFLD                    KNDC
171100970417     C*
171200970417     C     KARB1         KLIST
171300970417     C                   KFLD                    KAAP
171400970417     C                   KFLD                    KLPP
171500970417     C                   KFLD                    KNRP
171600970417     C                   KFLD                    KNSPP
171700021204     C*
171800021204     C     KAPD          KLIST
171900021204     C                   KFLD                    APDTIP
172000021204     C                   KFLD                    APDPDR
172100021204     C                   MOVEL     'A'           APDTIP
172200110418     C     Kar5          KLIST
172300110418     C                   KFLD                    v2cAAS
172400110418     C                   KFLD                    v2cLNP
172500110418     C                   KFLD                    v2cNRS
172600110418     C                   KFLD                    v2cNSP
172700110418     c                   kfld                    t5trd
172800110418     C                   move      'GEN'         T5trd             3
172900950313     C*----------------------------------------------------*
173000950313     C*  Definizione delle variabili
173100950313     C     *LIKE         DEFINE    ARBAAS        KAAS
173200950313     C     *LIKE         DEFINE    ARBLNP        KLNP
173300950313     C     *LIKE         DEFINE    ARBNRS        KNRS
173400950313     C     *LIKE         DEFINE    ARBNSP        KNSP
173500990923     C     *LIKE         DEFINE    ARITIP        KTIP
173600950313     C     *LIKE         DEFINE    FVVNPG        KNPG
173700950313     C     *LIKE         DEFINE    FVVNFV        KNFV
173800950313     C     *LIKE         DEFINE    FVVFGS        KFGS
173900950313     C     *LIKE         DEFINE    TBLKUT        KKUT
174000950313     C     *LIKE         DEFINE    TBLCOD        KCOD
174100950315     C     *LIKE         DEFINE    ARBDDC        §DDC
174200950315     C     *LIKE         DEFINE    CDELNA        KLNA
174300950315     C     *LIKE         DEFINE    CDENDC        KNDC
174400950315     C     *LIKE         DEFINE    AR6IFT        WIMA
174500950315     C     *LIKE         DEFINE    ARBVLF        WVOL
174600970417     C     *LIKE         DEFINE    AR9CAS        WCAS
174700970417     C     *LIKE         DEFINE    AR9VCA        WVCA
174800990923     C     *LIKE         DEFINE    AR6DIV        WDIV
174900970417     C     *LIKE         DEFINE    AR9TIC        WTIC
175000990923     C     *LIKE         DEFINE    AR6TRC        KTRC
175100961114     C     *LIKE         DEFINE    LBLAAP        KAAS1
175200961114     C     *LIKE         DEFINE    ANMLNP        KLNP1
175300961114     C     *LIKE         DEFINE    ANMNRS        KNRS1
175400961114     C     *LIKE         DEFINE    ANMNSP        KNSP1
175500961114     C     *LIKE         DEFINE    ANMCAA        KCAA
175600970417     C     *LIKE         DEFINE    LBLAAP        KAAP
175700970417     C     *LIKE         DEFINE    LBLLPP        KLPP
175800970417     C     *LIKE         DEFINE    LBLNRP        KNRP
175900970417     C     *LIKE         DEFINE    LBLNSP        KNSPP
176000991012     C     *LIKE         DEFINE    TBECOD        KCODE
176100991012     C     *LIKE         DEFINE    TBEKE1        KKEYE
176200950313     C*----------------------------------------------------*
176300950313     C* Giro data del giorno
176400950313     C                   TIME                    WHHDAT           14 0
176500950317     C                   MOVE      WHHDAT        WOGGI             8 0
176600020828     C* Decodifica Elaboratore
176700020828     c     *dtaara       define    §azute        azuteds
176800020828     c     *dtaara       define    §datiute      ddatiute
176900020828     C                   in(E)     *dtaara
177000020828     C                   IF        %Error  or  RSUT = *blanks
177100020828     C                   call      'TIBS34R'
177200020828     C                   parm                    Tibs34Ds
177300020828     C                   in        *dtaara
177400020828     c                   ENDIF
177500950315     C*----------------------------------------------------*
177600950315     C*  Caricamento Tabella 7R
177700950315     C                   Z-ADD     0             X                 3 0
177800950317     C                   Z-ADD     1             KKUT
177900950315     C                   MOVEL     '7R'          KCOD
178000950315     C     KTAB          CHAIN     TABEL00F                           96
178100950315     C     *IN96         DOWEQ     '0'
178200950315     C     X             ANDLT     100
178300950315     C     TBLFLG        IFEQ      ' '
178400950315     C                   ADD       1             X
178500950315     C                   MOVEL     TBLKEY        C7R(X)
178600950315     C                   MOVEL     TBLUNI        D7R(X)
178700950315     C                   END
178800950315     C     KTAB          READE     TABEL00F                               96
178900950315     C                   END
179000950315     C*  Caricamento Tabella 3A
179100950315     C                   Z-ADD     0             X                 3 0
179200950315     C                   MOVEL     '3A'          KCOD
179300950315     C     KTAB          CHAIN     TABEL00F                           96
179400950315     C     *IN96         DOWEQ     '0'
179500950315     C     X             ANDLT     100
179600950315     C     TBLFLG        IFEQ      ' '
179700950315     C                   ADD       1             X
179800950315     C                   MOVEL     TBLKEY        C3A(X)
179900950315     C                   MOVEL     TBLUNI        D3A(X)
180000950315     C                   END
180100950315     C     KTAB          READE     TABEL00F                               96
180200950315     C                   END
180300990923     C* Caricamento tipo incasso
180400990923     C                   Z-ADD     0             X                 3 0
180500990923     C                   MOVEL     '1A'          KCOD
180600990923     C     KTAB          CHAIN     TABEL00F                           96
180700990923     C     *IN96         DOWEQ     '0'
180800990923     C     X             ANDLT     100
180900990923     C     TBLFLG        IFEQ      ' '
181000990923     C                   ADD       1             X
181100990923     C                   MOVEL     TBLKEY        C1A(X)
181200990923     C                   MOVEL     TBLUNI        D1A(X)
181300990923     C                   END
181400990923     C     KTAB          READE     TABEL00F                               96
181500990923     C                   END
181600991012     C*  Aggancio tabella 'GED' x reperire la divisa corrente
181700991012     C                   MOVEL     'GED'         KCODE
181800991012     C                   MOVEL     *BLANKS       KKEYE
181900991012     C                   MOVEL     '1'           KKEYE
182000991012     C     KTBE          CHAIN     TNTBE01L                           13
182100991012     C     *IN13         IFEQ      '0'
182200991012     C                   MOVEL     TBEUNI        DGED
182300991012     C                   END
182400991012     C*
182500991012     C*  CARICO DATI TABELLA CV
182600991012     C                   MOVEL     *BLANKS       DIVEUR
182700991012     C                   MOVEL     'CV'          KCOD
182800991012     C     KTAB          CHAIN     TABEL00F                           13
182900991012     C     *IN13         DOWEQ     '0'
183000991012     C                   MOVEL     TBLKEY        DIVCV             3
183100991012     C                   MOVEL     TBLUNI        DSCV
183200991012     C* Se divisa diversa dalla moneta di conto reperisco data
183300991012     C* validità x trovare altra divisa da utilizzare x incassi
183400011009     C     §GEDCR        IFNE      DIVCV
183500991012     C     DIVEUR        ANDEQ     *BLANKS
183600991012     C     §CVDVA        ANDGE     'S'
183700991012     C                   MOVEL     DIVCV         DIVEUR            3
183800991012     C                   END
183900991012     C     KTAB          READE     TABEL00F                               13
184000991012     C                   END
184100950313     C*----------------------------------------------------*
184200950315     C                   CLEAR                   FNLR53
184300950315     C                   Z-ADD     4             KNPG
184400950313     C                   MOVEL     ' '           WFINE             1
184500950313     C                   MOVEL     '2'           WTPVID            1
184600950320     C                   MOVEL     'N'           WCHIUS            1
184700950426     C                   MOVEL     'N'           WPGMER            1
184800950320     C                   MOVEL     'N'           WSBLOC            1
184900950313     C*----------------------------------------------------*
185000950313     C                   ENDSR
185100020830      *---------------------------------------------------------*
185200020830      *  reperisce il po di gestione                            *
185300020830      *---------------------------------------------------------*
185400020830     C     srfgs         BEGSR
185500020830     C*
185600020830     c                   clear                   fnlv55ds
185700020830     c                   eval      d55tpt = '6'
185800020830     c                   movel     d63lna        d55lin
185900020830     c                   movel     *date         dataiso
186000020830     c                   movel     dataiso       d55drf
186100020830     c                   call      'FNLV55R'
186200020902     c                   parm                    fnlv55ds
186300020830     C*
186400020830     C                   ENDSR
186500950315     C*----------------------------------------------------*
186600950315     OFNCDE000  E            AGGCDE
186700950315     O                       CDENDC
186800950606     O                       CDEDAG
186900950606     O                       CDEDCM
187000961127     OFNCDE000  E            AGGFL2
187100961127     O                       CDEFL2
187200961127     O                       CDEERR
187300970225     O                       CDENDC
187400950320     OFNCDE000  E            NOAGG
187500950315     OFNARB000  E            AGGARB
187600020829     O                       ARBifp
187700020829     O                       ARBNDC
187800961114     O                       ARBFBC
187900961114     O                       ARBCMC
188000950315     OFNARB000  E            AGGDIS
188100950315     O                       ARBPDC
188200950315     O                       ARBDDC
188300950315     O                       ARBNMI
188400950323     O                       ARBAMA
188500950315     OFNFVV000  E            AGGFVV
188600950315     O                       FVVFCF
188700980323      *
188800980323     OFNCDS003  E            AGGCDS
188900980323     O                       CDSNDC
189000980323     O                       CDSDAG
189100980323     O                       CDSDCM
189200980323      *----------------------------------------------------*
189300980323**
189400950314Codice padroncino non valido                                           01
189500950314Codice padroncino inesistente                                          02
189600950314Immettere data di consegna                                             03
189700950314Data di consegna errata                                                04
189800950314Immettere ora di consegna                                              05
189900950314Ora di consegna errata                                                 06
190000950314Numero di spedizione inesitente                                        07
190100950314Linea di arrivo diversa dalla bolla                                    08
190200950314Riferimento mittente numerico diverso                                  09
190300950314Bolla con numero distinta già impostato                                10
190400950314Bolla già consegnata                                                   11
190500950314Deve essere impostata almeno una data                                  12
190600950315Non chiudere dist. di questo padr.x conflitti! Riprovare fra 2 min.    13
190700950321Sped. non abilitata all'incasso del C/A                                14
190800950321Spedizione da tassare                                                  15
190900961119Spediz.con giacenza in fase non congrua                                16
191000000210Impossibile aggiungere Cons.Partic. - -                                17
191100950606Bolla con figlia !!!                                                   18
191200961121Immetti G=sped.giac./C=Conse./A=L.Avv.                                 19
191300961120Evento da analizzare !!                                                20
191400961118Immetti E se vuoi visualizzare note EDI                                21
191500961120Ricevuto secondo evento colli mancanti                                 22
191600961230Non ci sono spedizioni valide da chiudere in distinta                  23
191700980323Analizzare dettaglio segnacolli !!!                                    24
191800000925C/A ancora presente da annullare !!                                    25
191900001019Esistono altre bolle da estrarre dopo la chiusura ripetere l'operazione26
