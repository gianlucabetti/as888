000100940509      ****************************************************************
000200970205      *                                                              *
000300931102      *   PARTENDO DALLA DS ESTERNA PASSATA DAL PGM DI LETTURA       *
000400940808      *   DELLA DATAQ, AGGIORNA I SALDI E I FILE DEL C/ECON.         *
000500930204      *                                                              *
000600930204      *   PARAMETRI UTILIZZATI DAL PGM:                              *
000700940808      *   INPUT: - DS ESTERNA 'DS$ECO' :                             *
000800940808      *            - PARAMETRI COMUNI                                *
000900940808      *            - IMMAGINE DELLA BOLLA PRIMA DELLA MODIFICA       *
001000940808      *            - IMMAGINE DELLA BOLLA DOPO LA MODIFICA           *
001100940808      *            - DATI DELLA BOLLA ORIGINALE                      *
001200940808      *   OUTUT: - AGGIORNAMENTO DEI FILE SALDI E C/ECON SE TUTTO OK *
001300931102      *          - SE VI SONO ERRORI NON AGGIORNA SALDI E GENERA     *
001400940808      *            UN RECORD DI ERRORE SUL FILE ERRORI               *
001500931102      *            CON IL CODICE DI ERRORE E I DATI DELLA DS ESTERNA *
001600930204      *                                                              *
001700930204      ****************************************************************
001800930413     H DECEDIT('0,') DATEDIT(*DMY.)
001900941215     FTABEL00F  UF   E           K DISK    USROPN
002000941025     FAZORG01L  IF   E           K DISK    USROPN
002100031105     FTNTBE01L  IF   E           K DISK    USROPN
002200030217     FECEVT01L  IF   E           K DISK
002300941215     FECECT01L  IF   E           K DISK
002400941215     FECECD01L  IF   E           K DISK
002500990617     FSISDR01L  UF A E           K DISK
002600990617     FSISDF01L  UF A E           K DISK
002700990617     FSISDT01L  UF A E           K DISK
002800990617     FSISDC01L  UF A E           K DISK
002900990617     FSISDO01L  UF A E           K DISK
003000990617     FSISDA01L  UF A E           K DISK
003100990617     FSISDP01L  UF A E           K DISK
003200040316     FSISDE01L  UF A E           K DISK
003300011127     FECMOV01L  UF A E           K DISK
003400950316     FECCET30C  UF A E           K DISK
003500950316     FECCED30C  UF A E           K DISK
003600980325     FECFIM30C  UF A E           K DISK
003700950123     FECERR01L  UF A E           K DISK
003800021205     FFIAPD01L  IF   E           K DISK
003900020531     FFIFCE01L  UF A E           K DISK
004000030114     FSICOP01L  UF A E           K DISK
004100030114     FECCEV30C  UF A E           K DISK
004200030506     FECC2V30C  UF A E           K DISK
004300040205     FQSYSPRT   O    F  132        PRINTER oflind(*inof)
004400950120     F*PRTF198 O   F     198     OF    LPRINTER                        UC
004500990806     D*------------------------------------------------------------------------*
004600950120     D*PRTF198  66FL 60OL
004700930304     D*****************************************************************
004800930304     D* MESSAGGI
004900031105     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
005000020130     d* Variabili temporali
005100020130     d WDATA           S               D   DATFMT(*ISO) INZ(D'1901-01-01')
005200090914     d* definizione campi work
005300090916     d wimp            s             19  5
005400090916     d wimp£           s             17  3
005500090916     d wpirp           s             15  5
005600090914     d wdirp           s             15  5
005700090914     d wcirp           s             15  5
005800090916     d wcirp£          s             13  3
005900090914     d woirp           s             15  5
006000090916     d woirp£          s             13  3
006100090914     d wdctl           s             15  5
006200090914     d wpctl           s             15  5
006300090914     d wtctl           s             15  5
006400090916     d wtctl£          s             13  3
006500090914     d wpct1           s             15  5
006600090914     d wpct2           s             15  5
006700090914     d wpct3           s             15  5
006800090914     d wpct4           s             15  5
006900090914     d wpct5           s             15  5
007000090914     d wdct1           s             15  5
007100090914     d wdct2           s             15  5
007200090914     d wdct3           s             15  5
007300090914     d wdct4           s             15  5
007400090914     d wdct5           s             15  5
007500090914     d wtct1           s             15  5
007600090914     d wtct2           s             15  5
007700090914     d wtct3           s             15  5
007800090914     d wtct4           s             15  5
007900090914     d wtct5           s             15  5
008000090916     d wtct1£          s             13  3
008100090916     d wtct2£          s             13  3
008200090916     d wtct3£          s             13  3
008300090916     d wtct4£          s             13  3
008400090916     d wtct5£          s             13  3
008500090914     d wcoimv          s             15  5
008600090916     d wcoimv£         s             13  3
008700030214     D*-------------------
008800030214     D* SCHIERA VOCI (INDICE=CODICE VOCE)
008900030214     D*-------------------
009000030214     D VCMP            S              3  0 DIM(999)
009100030217     D VDDV            S              8  0 DIM(999)
009200030217     D VDSV            S              8  0 DIM(999)
009300030217     D VCOP            S              1    DIM(999)
009400030217     D VDAT            S              1    DIM(999)
009500030217     D VAID            S              1    DIM(999)
009600941130     D* SCHIERE RICAVI -PRIMA- E -DOPO- E LAVORO
009700030505     D CRP             S              3  0 DIM(75)
009800030505     D IRP             S             13  4 DIM(75)
009900030505     D CRD             S              3  0 DIM(75)
010000030505     D IRD             S             13  4 DIM(75)
010100030505     D CR              S              3  0 DIM(75)
010200030505     D IR              S             13  4 DIM(75)
010300030505     D VR              S              1    DIM(75)
010400940808     D* SCHIERE CODICI E IMPORTI ALFANUMERICHE (PER PASSAGIO DATI)
010500030505     D CAL             S              2    DIM(75)
010600030505     D IAL             S              7    DIM(75)
010700941130     D* SCHIERE VARIE E IMPORTI VARIE PRIMA,DOPO E LAVORO
010800941130     D PSVN            S              1    DIM(20)
010900011214     D PVAA            S             11    DIM(20)
011000011214     D PVAN            S             11  3 DIM(20)
011100941201     D DSVN            S              1    DIM(20)
011200011214     D DVAA            S             11    DIM(20)
011300011214     D DVAN            S             11  3 DIM(20)
011400941130     D OSVN            S              1    DIM(20)
011500011214     D OVAA            S             11    DIM(20)
011600011214     D OVAN            S             11  3 DIM(20)
011700941130     D SVN             S              1    DIM(20)
011800011214     D VAN             S             11  3 DIM(20)
011900941215     D*****************************************************************
012000941215     D* SCHIERE CON TRACCIATO DI ECECT E ECECD    'PRIMA'
012100040120     D CDP             S              3  0 DIM(900)
012200040120     D FDP             S              1    DIM(900)
012300040120     D DDP             S              8  0 DIM(900)
012400040120     D DSP             S              8  0 DIM(900)
012500040120     D KCP             S              4  0 DIM(900)
012600040120     D KSP             S              7  0 DIM(900)
012700040120     D RIP             S              2    DIM(900)
012800040120     D DAP             S              1    DIM(900)
012900040120     D PEP             S              5  2 DIM(900)
013000040120     D FAP             S              1    DIM(900)
013100040120     D IEP             S              1    DIM(900)
013200040120     D CFP             S              7  0 DIM(900)
013300941215     D* SCHIERE CON TRACCIATO DI ECECT E ECECD    'DOPO'
013400040120     D CDD             S              3  0 DIM(900)
013500040120     D FDD             S              1    DIM(900)
013600040120     D DDD             S              8  0 DIM(900)
013700040120     D DSD             S              8  0 DIM(900)
013800040120     D KCD             S              4  0 DIM(900)
013900040120     D KSD             S              7  0 DIM(900)
014000040120     D RID             S              2    DIM(900)
014100040120     D DAD             S              1    DIM(900)
014200040120     D PED             S              5  2 DIM(900)
014300040120     D FAD             S              1    DIM(900)
014400040120     D IED             S              1    DIM(900)
014500040120     D CFD             S              7  0 DIM(900)
014600011213     D* SCHIERE CON TRACCIATO PER AGGIORNAMENTO ECMOV
014700040120     D CDPY            S              8  0 DIM(900)
014800040120     D DRGY            S              8  0 DIM(900)
014900040120     D SVOY            S              7  0 DIM(900)
015000040120     D CDCY            S              8    DIM(900)
015100040120     D DAY             S              1    DIM(900)
015200090915     D IMPY            S             19  5 DIM(900)
015300040120     D VOCY            S              8    DIM(900)
015400941215     D* SCHIERE RICAVI E COMPETENZE -PRIMA-
015500090914     D IMP             S             13  5 DIM(999)
015600941215     D MEP             S              1    DIM(999)
015700941223     D RCP             S              1    DIM(999)
015800980312     D RLP             S              2    DIM(999)
015900941215     D* SCHIERE RICAVI E COMPETENZE -DOPO-
016000090914     D IMD             S             13  5 DIM(999)
016100941215     D MED             S              1    DIM(999)
016200941223     D RCD             S              1    DIM(999)
016300980312     D RLD             S              2    DIM(999)
016400030114     D* SCHIERE VOCI COOP  -PRIMA-
016500090914     D IMPV            S             13  5 DIM(999)
016600030214     D COPPV           S              1    DIM(999)
016700030214     D DATPV           S              1    DIM(999)
016800030217     D AIDPV           S              1    DIM(999)
016900030214     D CMPPV           S              3  0 DIM(999)
017000030114     D* SCHIERE VOCI COOP  -DOPO-
017100090914     D IMDV            S             13  5 DIM(999)
017200030115     D COPDV           S              1    DIM(999)
017300030115     D DATDV           S              1    DIM(999)
017400030115     D AIDDV           S              1    DIM(999)
017500030115     D CMPDV           S              3  0 DIM(999)
017600941215     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
017700030505     D CAN             S              2    DIM(75)
017800030505     D IAN             S              7    DIM(75)
017900030505     D COP             S              1    DIM(75)
018000030505     D DAT             S              1    DIM(75)
018100030505     D AID             S              1    DIM(75)
018200030505     D CMPV            S              2    DIM(75)
018300941215     D* RIFERIMENTI CENTRI DI COSTO
018400941215     D RIC             S              2    DIM(30)
018500941215     D* RICERCA PARAMETRI UTENTE
018600020705     D*****************************************************************
018700020705     D* NETWORK
018800020705     D SNTW            S              3    DIM(20)
018900020705     D SNFIE           S              1    DIM(20)
019000931111     D*****************************************************************
019100940207     D* FILIALI CON RELATIVO RAGGRUPPAMENTO REGIONE
019200971124     D FIL             S              3  0 DIM(500)
019300971124     D RRE             S              3  0 DIM(500)
019400020514     D RRN             S              3  0 DIM(500)
019500020705     D* INDICE=COD.FILIALE
019600020705     D NTW             S              3    DIM(999)
019700020705     D FIE             S              1    DIM(999)
019800020705     D*FL1             S              1    DIM(999)
019900020705     D*DIT             S              3    DIM(999)
020000020705     D*DPD             S              1    DIM(999)
020100020705     D*POS             S              1    DIM(999)
020200930316     D*****************************************************************
020300931103     D* SCHIERA TABELLA FASCIE DI PESO (2L)
020400940103     D FPR             S              2    DIM(20)
020500940103     D KIR             S              6  0 DIM(20)
020600940103     D KFR             S              6  0 DIM(20)
020700940103     D FPC             S              2    DIM(20)
020800940103     D KIC             S              6  0 DIM(20)
020900940103     D KFC             S              6  0 DIM(20)
021000940103     D FPT             S              2    DIM(20)
021100940103     D KIT             S              6  0 DIM(20)
021200940103     D KFT             S              6  0 DIM(20)
021300931104     D*****************************************************************
021400931104     D* SCHIERA TABELLA CONTROPARTITE PER FATTURAZIONE (CC)
021500931104     D CCC             S              1    DIM(50)
021600931111     D CC1             S              1    DIM(50)
021700931111     D CC2             S              1    DIM(50)
021800931111     D CC3             S              1    DIM(50)
021900941130     D CC4             S              1    DIM(50)
022000941130     D CC5             S              1    DIM(50)
022100941130     D CC6             S              1    DIM(50)
022200941130     D CMP             S              3  0 DIM(50)
022300940207     D*****************************************************************
022400940314     D* SCHIERA TABELLA REGIONI (14) E RELATIVI CODICI RAGGRUPPAMENTO REGIONI
022500940207     D REG             S              3  0 DIM(30)
022600940207     D RAR             S              3  0 DIM(30)
022700020514     D RAN             S              3  0 DIM(30)
022800941123     D*****************************************************************
022900941123     D* SCHIERA TABELLA CONS.PART. (1P) E RELATIVE VARIE
023000020104     D C1P             S              1    DIM(50)
023100020104     D V1P             S              1    DIM(50)
023200940314     D*****************************************************************
023300940314     D* SCHIERE CONSEGNE PARTICOLARI
023400940314     D SVP             S              1    DIM(20)
023500940314     D SVD             S              1    DIM(20)
023600940315     D SVX             S              1    DIM(20)
023700011214     D IVP             S             13  4 DIM(20)
023800011214     D IVD             S             13  4 DIM(20)
023900011214     D IVX             S             13  4 DIM(20)
024000090304     D NSP             S              5  0 DIM(20)
024100090304     D NSD             S              5  0 DIM(20)
024200090304     D NSX             S              5  0 DIM(20)
024300940314     D CPP             S              1    DIM(20)
024400940314     D CPD             S              1    DIM(20)
024500940315     D CPX             S              1    DIM(20)
024600981012     D*****************************************************************
024700981012     D* SCHIERA NORMALIZZAZIONE VOCE PROJ
024800981012     D S4              S              1    DIM(4)
024900981012     D S4B             S              1    DIM(4)
025000941215     D*****************************************************************
025100930323     D* DS ESTERNA IN INPUT AL PGM
025200940808     D DS$ECO        E DS
025300940405     D*****************************************************************
025400990806     D* DS ESTERNA PER RIDEFINIRE IMMAGINE PRIMA E DOPO DI TITAS10F
025500941201     D TRECDS        E DS
025600940405     D  D$PDS1                 1    250
025700940405     D  D$PDS2               251    500
025800940405     D  D$DDS1               501    750
025900940405     D  D$DDS2               751   1000
026000941130     D  D$ODS1              1001   1250
026100941130     D  D$ODS2              1251   1500
026200930323     D*****************************************************************
026300040318     D* DS ESTERNA x tipo record BNB di sisde
026400040317     D Dar5bnbSDE    E DS
026500040318     D* DS ESTERNA x tipo record BAN di sisde
026600040318     D Dar5banSDE    E DS
026700040322     D* DS ESTERNA x sottocampatura TASFLO
026800040322     D Dtasflo       E DS
026900040317     D*****************************************************************
027000040317     D* DS ESTERNA TABELLA 2L (FASCIE DI PESO)
027100040317     D DS2L          E DS
027200930413     D*****************************************************************
027300931104     D* DS ESTERNA TABELLA CC (CONTROPARTITE PER FATTURAZIONE)
027400931104     D DSCC          E DS
027500940103     D*****************************************************************
027600940103     D* DS ESTERNA TABELLA 2K (DATI VARI PER SALDI)
027700940103     D DS2K          E DS
027800940207     D*****************************************************************
027900940207     D* DS ESTERNA TABELLA 14 (REGIONI E RAGGRUPPAMENTI REGIONI)
028000940207     D DS14          E DS
028100941123     D*****************************************************************
028200941123     D* DS ESTERNA TABELLA 1P (CONSEGNE PARTICOLARI)
028300941123     D DS1P          E DS
028400941215     D*****************************************************************
028500941215     D* DS RIFERIMENTI CENTO DI COSTO
028600941215     D DS2T          E DS
028700970901     D*****************************************************************
028800970901     D* DS ESTERNA TABELLA $2 (VARIE FORZATE A PGM)
028900970901     D DS$2          E DS
029000000302     D*****************************************************************
029100000302     D* DS ESTERNA DESCRIZIONE 43 ORGANIGRAMMA
029200000302     D OG143         E DS
029300000201     D*****************************************************************
029400000201     D* CONVERSIONI IMPORTI
029500000201     D YEUR          E DS                  EXTNAME(YEURCODS)
029600011218     D*****************************************************************
029700011218     D* DS DIVISE PER CONVERSIONI PER IL C/E
029800011218     D DSGEDCE       E DS
029900020704     D*****************************************************************
030000020704     D* DS NETWORK
030100020704     D DNTW          E DS
030200020822     D*****************************************************************
030300020822     D* DS DATE CONTABILIZZAZIONE CONTEGGI PADRONCINI
030400020822     D DBLC          E DS
030500930325     D***************************************************************
030600930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
030700941026     D DS§DAT          DS
030800941026     D  DSSEC                  1      2  0
030900941026     D  DSANN                  3      4  0
031000941026     D  DSMES                  5      6  0
031100941026     D  DSGIO                  7      8  0
031200941201     D  DSAAS                  1      4  0
031300941122     D  DSMGS                  5      8  0
031400941026     D  DSDAT6                 3      8  0
031500941026     D  DSDAT8                 1      8  0
031600941026     D DS§TIM          DS
031700941026     D  DTORA                  1      2  0
031800941026     D  DTMIN                  3      4  0
031900941026     D  DTSCN                  5      6  0
032000941026     D  DTGIO                  7      8  0
032100941026     D  DTMES                  9     10  0
032200941026     D  DTSEC                 11     12  0
032300941026     D  DTANN                 13     14  0
032400941026     D  DTORA6                 1      6  0
032500941026     D  DTTIME                 1     14  0
032600020830     D                 DS
032700020829     D  DSWAAS                 1      4  0
032800020829     D  DSWMGS                 5      8  0
032900020830     D DSWDAT                  1      8  0
033000940808     D***************************************************************
033100940808     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
033200940825     D                 DS
033300940825     D  DSWCN                  1      3P 0
033400940825     D  DSWCA                  1      3
033500090904     D  DSWIN                  4     13P 5
033600011214     D  DSWIA                  4     13
033700941026     D***************************************************************
033800941026     D* DS PER SCOMPOSIZIONE MESSAGGIO
033900941026     D                 DS
034000941026     D  DSMSG                  1     80
034100030217     D  DSVOCE                29     31
034200941026     D  DSDER                  1     74
034300941026     D  DSCER                 75     78
034400991116     D***************************************************************
034500991116     D* DS PER SCOMPOSIZIONE CAMPO D$GXXX
034600991116     D                 DS
034700991116     D  DSX                    1     33
034800991116     D  DSXASI                 1      4
034900991116     D  DSXFSI                 5      5
035000991116     D  DSXXXX                 6     33
035100011218     D***************************************************************
035200011218     D* DS  CONTROLLO DATA (8)
035300011218     D WLBDA8          DS                  INZ
035400011218     D  G08DAT                 1      8  0
035500011218     D  G08INV                 9     16  0
035600011218     D  G08ERR                17     17
035700011218     D  G08TGI                18     22  0
035800941215     D***************************************************************
035900941215     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
036000941215     D                SDS
036100941215     D* NOME UTENTE
036200941215     D  USER                 254    263
036300941215     D* NUMERO LAVORO
036400941215     D  JOBNUM               264    269  0
036500030122     D***************************************************************
036600030122     D* DEFINIZIONE CAMPI
036700030122     D N8              S              8  0 INZ
036800030122     D N8bis           S              8  0 INZ
036900040317     D wgessde         S              3    INZ
037000040322     D contavv         S              1  0 INZ
037100950316     I*****************************************************************
037200950316     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCET30C
037300010828     IECCET000      01
037400010828     IECCETP00      02
037500950316     I*****************************************************************
037600950316     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCED30C
037700010828     IECCED000      03
037800010828     IECCEDP00      04
037900030114     I*****************************************************************
038000030114     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
038100030114     IECCEV000      05
038200030114     IECCEVP00      06
038300030506     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECC2V30C
038400030506     IECC2V000      07
038500030506     IECC2VP00      08
038600930304     C*****************************************************************
038700930323     C*/ INIZIO MAIN DEL PGM
038800931105     C*
038900931105     C* ESEGUE ROUTINE INIZIALE
039000931105     C                   EXSR      RUTINZ
039100940919     C*
039200940405     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE)
039300931104     C* CHIUDO I FILE E FINISCO PGM
039400991214     C     D$GSTS        IFEQ      'C'
039500991214     C     D$GSTS        OREQ      'S'
039600991214     C     $ERR          ORNE      'N'
039700991214     C                   ELSE
039800991214     C*
039900991214     C***********D$GSTS    IFNE 'C'                        --- 1 -->
040000991214     C***********$ERR      ANDEQ'N'
040100940808     C*
040200940919     C                   ADD       1             LEECD7            9 0
040300940808     C                   EXSR      AZZW
040400940808     C*
040500941130     C* CERCA I RICAVI PRIMA E DOPO
040600941021     C     $ERR          IFEQ      'N'
040700941130     C                   EXSR      GESRIC
040800940808     C                   ENDIF
040900940808     C*
041000940808     C* ESEGUE CALCOLI SU IMMAGINE PRIMA SOLO SE VARIAZIONE O CANCELLAZIONE
041100941021     C     $ERR          IFEQ      'N'
041200940808     C     D$GSTS        IFEQ      'U'
041300940808     C     D$GSTS        OREQ      'D'
041400940808     C                   EXSR      GESDSP
041500940808     C                   ENDIF
041600940808     C                   ENDIF
041700940808     C*
041800940808     C* ESEGUE CALCOLI SU IMMAGINE DOPO SOLO SE VARIAZIONE O INSERIMENTO
041900941021     C     $ERR          IFEQ      'N'
042000940808     C     D$GSTS        IFEQ      'U'
042100940808     C     D$GSTS        OREQ      'I'
042200940808     C                   EXSR      GESDSD
042300940808     C                   ENDIF
042400940808     C                   ENDIF
042500930304     C*
042600940808     C* <<<<<<<<<< FINE PREPARAZIONE - INIZIO AGGIORNAMENTO >>>>>>>>>>>>>>>
042700011210     C*
042800011210     C* AGGIORNA ECMOV
042900011210IF  2C     $ERR          IFEQ      'N'
043000011210     C     D$GWFM        ANDEQ     'S'
043100011210     C                   EXSR      AGGMOV
043200011210E   2C                   ENDIF
043300941215     C*
043400941215     C* AGGIORNA ECCED (DETTAGLIO COMPETENZE)
043500941215IF  2C     $ERR          IFEQ      'N'
043600950104     C     D$GCET        ANDEQ     'S'
043700941215     C                   EXSR      AGGCED
043800941215E   2C                   ENDIF
043900941215     C*
044000941215     C* AGGIORNA ECCET (TESTATA COMPETENZE)
044100941215IF  2C     $ERR          IFEQ      'N'
044200941223     C     D$GCET        ANDEQ     'S'
044300941215     C                   EXSR      AGGCET
044400941215E   2C                   ENDIF
044500980317     C*
044600980317     C* AGGIORNA ECFIM (RIFATT. ITL)
044700980317     C     $ERR          IFEQ      'N'
044800020531     C     D$GFIM        ANDEQ     'S'
044900980317     C                   EXSR      AGGFIM
045000980317     C                   ENDIF
045100020531     C*
045200020531     C* AGGIORNA FIFCE (CONTEGGI PADRONCINI)
045300020531IF  2C     $ERR          IFEQ      'N'
045400020531     C     D$GFCE        ANDEQ     'S'
045500020531IF  3C     D$GSTS        IFEQ      'I'
045600020531     C     D$GSTS        OREQ      'U'
045700020531     C                   EXSR      AGGFCE
045800020531E   3C                   ENDIF
045900020531E   2C                   ENDIF
046000030122     C*
046100030122     C* AGGIORNA I PROSSIMI 2 FILES A PARTIRE DAL 2003
046200030131IF  2C                   IF        WPDATCOP > 20030000 or
046300030131     C                             WDDATCOP > 20030000
046400030114     C*
046500030509     C* AGGIORNA ECCEV (VOCI COOP) e ECC2V
046600030122IF  3C     $ERR          IFEQ      'N'
046700030114     C     D$GCOP        ANDEQ     'S'
046800030114     C                   EXSR      AGGCEV
046900030122E   3C                   ENDIF
047000030114     C*
047100030114     C* AGGIORNA SICOP (SALDI COOP)
047200030122IF  3C     $ERR          IFEQ      'N'
047300030114     C     D$GCOP        ANDEQ     'S'
047400031113IF  4C     D$PCCA        IFEQ      '2'
047500031113     C     D$DCCA        OREQ      '2'
047600031113X   4c                   ELSE
047700030122     C                   EXSR      GESCOP
047800031113E   4C                   ENDIF
047900030122E   3C                   ENDIF
048000030122E   2C                   ENDIF
048100980313     C*
048200980313     C* AGGIORNA ECFIC (COMPET. RIFATT. BAR/SDI)
048300011127     C*---       $ERR      IFEQ 'N'
048400011127     C*---       D$GCET    ANDEQ'S'
048500011127     C*---                 EXSR AGGFIC
048600011127     C*---                 ENDIF
048700940808     C*
048800931104     C* SCRIVE SUI SALDI BOLLE
048900941021     C     $ERR          IFEQ      'N'
049000931119     C     D$GSDR        ANDEQ     'S'
049100931104     C                   EXSR      GESSDR
049200930420     C                   END
049300931104     C*
049400931104     C* SCRIVE SUI SALDI FILIALE
049500941021     C     $ERR          IFEQ      'N'
049600931119     C     D$GSDF        ANDEQ     'S'
049700931104     C                   EXSR      GESSDF
049800931104     C                   END
049900931110     C*
050000931110     C* SCRIVE SUI SALDI TRAZIONI
050100941021     C     $ERR          IFEQ      'N'
050200931119     C     D$GSDT        ANDEQ     'S'
050300931110     C                   EXSR      GESSDT
050400931110     C                   END
050500941117     C*
050600941117     C* SE ESISTE LA BOLLA MAMMA
050700941117     C* SOSTITUISCO IL CLIENTE DELLA BOLLA MAMMA AL CLIENTE USATO PER LA
050800990617     C* SCRITTURA SUI FILE SISDC E SISDO (PRIMA E DOPO)
050900950203     C     D$GSTS        IFEQ      'U'
051000950203     C     D$GSTS        OREQ      'D'
051100941117     C                   Z-ADD     D$PKSC        WCPKSC
051200950203     C                   ENDIF
051300950203     C*
051400950203     C     D$GSTS        IFEQ      'U'
051500950203     C     D$GSTS        OREQ      'I'
051600941117     C                   Z-ADD     D$DKSC        WCDKSC
051700950203     C                   ENDIF
051800950203     C*
051900941117     C     D$OKSC        IFNE      *ZEROS
052000950203     C*
052100950203     C     D$GSTS        IFEQ      'U'
052200950203     C     D$GSTS        OREQ      'D'
052300941118     C     D$PKSC        IFNE      *ZEROS
052400941130     C     D$PDBO        ANDEQ     'S'
052500950905     C     D$PFIG        ANDEQ     'S'
052600941117     C                   Z-ADD     D$OKSC        WCPKSC
052700941117     C                   ENDIF
052800950203     C                   ENDIF
052900950203     C*
053000950203     C     D$GSTS        IFEQ      'U'
053100950203     C     D$GSTS        OREQ      'I'
053200941117     C     D$DKSC        IFNE      *ZEROS
053300941130     C     D$DDBO        ANDEQ     'S'
053400950905     C     D$DFIG        ANDEQ     'S'
053500941117     C                   Z-ADD     D$OKSC        WCDKSC
053600941117     C                   ENDIF
053700950203     C                   ENDIF
053800950203     C*
053900941117     C                   ENDIF
054000931111     C*
054100931111     C* SCRIVE SUI SALDI CLIENTE - REGIONE/REGIONE
054200941021     C     $ERR          IFEQ      'N'
054300931119     C     D$GSDC        ANDEQ     'S'
054400931111     C                   EXSR      GESSDC
054500931111     C                   END
054600931111     C*
054700931111     C* SCRIVE SUI SALDI CLIENTE - CONTABILI
054800941021     C     $ERR          IFEQ      'N'
054900931119     C     D$GSDO        ANDEQ     'S'
055000931111     C                   EXSR      GESSDO
055100931111     C                   END
055200931111     C*
055300931111     C* SCRIVE SUI SALDI CLIENTE - AFFIDABILITA'
055400941021     C     $ERR          IFEQ      'N'
055500931119     C     D$GSDA        ANDEQ     'S'
055600931111     C                   EXSR      GESSDA
055700931111     C                   END
055800931111     C*
055900931111     C* SCRIVE SUI SALDI CLIENTE - CONSEGNE PARTICOLARI
056000941021     C     $ERR          IFEQ      'N'
056100940103     C     D$GSDP        ANDEQ     'S'
056200940103     C                   EXSR      GESSDP
056300940103     C                   END
056400040316     C*
056500040316     C* SCRIVE ESTENSIONI SALDI
056600040323     C* AGGIORNA L'ESTENSIONE SALDI A PARTIRE DA APRILE 2004
056700040316     C     $ERR          IFEQ      'N'
056800040316     C     D$GSDE        ANDEQ     'S'
056900040316     C                   EXSR      GESSDE
057000040316     C                   END
057100950124     C*
057200950124     C* PORTA COD. E IMP. COMPETENZE/RICAVI IN OUTPUT SOLO PER PGM A VIDEO
057300950124     C     $ERR          IFEQ      'N'
057400950124     C     D$GPGF        IFEQ      'TREC78R'
057500950124     C     D$GPGF        OREQ      'TREC79R'
057600950124     C                   EXSR      GESCCD
057700950124     C                   ENDIF
057800950124     C                   ENDIF
057900931104     C*
058000940217     C                   END
058100930323     C*
058200991214     C* SE PGM LANCIATO CON 'C' (CLOSE) O 'S' (SHUTDOWN) CHIUDE CON LR
058300991214     C* (CHIUDE FILE) ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
058400991214IF  1C     D$GSTS        IFEQ      'C'
058500991214     C     D$GSTS        OREQ      'S'
058600011218     C*
058700011210     C* CICLO ULTIME SCRITTURE ECMOV -MOVIMENTI NUOVI-
058800011219DO  2C     1             DO        WULTY         IY
058900011219IF  3C     IMPY(IY)      IFNE      *ZEROS
059000090916     C                   Z-ADD     IMPY(IY)      WIMP
059100011210     C                   EXSR      AGGECMOV
059200011219E   3C                   ENDIF
059300011219E   2C                   ENDDO
059400011219     C*
059500950120     C*********************EXSR STAMPA                     *STAMPA STAT.LANCIO
059600941130     C                   CALL      'TRECD8R'                            98
059700940902     C                   PARM                    DS$ECO
059800930323     C                   SETON                                        LR
059900991214X   1C                   ELSE
060000930324     C                   SETON                                        RT
060100991214E   1C                   END
060200940916     C*/ FINE MAIN DEL PGM
060300940808     C********************************************************************
060400940808     C*   GESRIC - CALCOLA IL RICAVO BOLLA -PRIMA- E -DOPO-
060500940808     C********************************************************************
060600940808     C     GESRIC        BEGSR
060700940930     C*
060800940930     C* AZZERO SCHIERE RICAVI PRIMA E DOPO
060900940930     C                   CLEAR                   CRP
061000940930     C                   CLEAR                   IRP
061100940930     C                   CLEAR                   CRD
061200940930     C                   CLEAR                   IRD
061300940930     C* CONTROLLO FLAG RICAVI S/N (DAL TIPO BOLLA)
061400941005     C     D$GSTS        IFEQ      'U'
061500941005     C     D$GSTS        OREQ      'D'
061600941130     C     D$PFRI        IFEQ      'S'
061700950905     C     D$PFIG        OREQ      'S'
061800941130     C                   EXSR      GESRIP
061900940930     C                   ENDIF
062000941005     C                   ENDIF
062100941005     C     D$GSTS        IFEQ      'U'
062200941005     C     D$GSTS        OREQ      'I'
062300941130     C     D$DFRI        IFEQ      'S'
062400950905     C     D$DFIG        OREQ      'S'
062500941130     C                   EXSR      GESRID
062600940930     C                   ENDIF
062700941005     C                   ENDIF
062800940930     C*
062900941130     C* CONTROLLO SE ESISTE IMPONIBILE REALE
063000941005     C*
063100940930     C                   MOVEL     'N'           WPFIR             1
063200940930     C                   MOVEL     'N'           WDFIR             1
063300940930     C* SE E' FIGLIA E L'IMPONIBOLE REALE = 0 CERCO IMP.REALE SULLA MAMMA
063400941005     C     D$GSTS        IFEQ      'U'
063500941005     C     D$GSTS        OREQ      'D'
063600940930     C     WPIR          IFEQ      *ZEROS
063700950905     C     D$PFIG        ANDEQ     'S'
063800940930     C                   MOVEL     'S'           WPFIR
063900940930     C                   ENDIF
064000941005     C                   ENDIF
064100941005     C     D$GSTS        IFEQ      'U'
064200941005     C     D$GSTS        OREQ      'I'
064300940930     C     WDIR          IFEQ      *ZEROS
064400950905     C     D$DFIG        ANDEQ     'S'
064500940930     C                   MOVEL     'S'           WDFIR
064600940930     C                   ENDIF
064700941005     C                   ENDIF
064800940930     C*
064900941130     C* SE IMPONIBILE REALE PRIMA O DOPO NON E' PRESENTE, UTILIZZO QUELLA
065000941130     C* DELLA MAMMA
065100940930     C     WPFIR         IFEQ      'S'
065200940930     C     WDFIR         OREQ      'S'
065300940930     C*
065400941130     C* CALCOLO RICAVI DELLA BOLLA ORIGINALE
065500941130     C                   EXSR      GESRIO
065600941130     C*
065700941130     C* IMPOSTO NELLA SCHIERA RICAVI L'IMP.REALE DELLA BOLLA ORIG.
065800941130     C     WPFIR         IFEQ      'S'
065900941130     C                   Z-ADD     WPIIR         I
066000941130     C                   Z-ADD     WOIR          IRP(I)
066100941130     C                   Z-ADD     WOIR          WPIR
066200941130     C                   ENDIF
066300941130     C     WDFIR         IFEQ      'S'
066400941130     C                   Z-ADD     WDIIR         I
066500941130     C                   Z-ADD     WOIR          IRD(I)
066600941130     C                   Z-ADD     WOIR          WDIR
066700941130     C                   ENDIF
066800941130     C*
066900941130     C                   ENDIF
067000941223     C*
067100941223     C* IMPOSTA I RICAVI NELLE SCHIERE DI 999 E NELLA DS UNIFICATA
067200030505     C     1             DO        75            I
067300941223     C* IMPOSTA I RICAVI -PRIMA- NELLA SCHIERA DI 999
067400941223     C     CRP(I)        IFNE      *ZEROS
067500941223     C                   Z-ADD     CRP(I)        CP
067600050322     c* è stato creato il pgm TNVRRICR per sistemare gli importi dei ricavi
067700050322     c* nel file ECMOV. Tale pgm imposta il tasll2 = 'A' nelle sped.
067800050322     c* di gennaio e febbraio 2005 x cui:
067900050322     c* se la modifica del titas avviene da questo pgm deve ragionare in
068000050322     c* modo sbagliato PRIMA e in modo corretto DOPO
068100050322     c* se la modifica non avviene da questo pgm ci sono tre casi:
068200050322     c*1)il record di titas aggiornato non ha ancora subito la correzione
068300050322     c* quindi deve continuare a ragionare in modo sbagliato (xchè passerà
068400050322     c* il tnvrricr a sistemare)
068500050322     c*2)il record di titas aggiornato ha già subito la correzione
068600050322     c* quindi deve ragionare in modo corretto
068700050322     c*3)il record di titas aggiornato ha data spedizione maggiore di
068800050322     c*  febbraio 2005 quindi deve ragionare in modo corretto
068900050321     C                   IF        D$GPGF = 'TNVRRICR'
069000941223     C                   Z-ADD     IRP(I)        IMP(CP)
069100050321     C                   Else
069200050322     c                   if        wdsp > 20050300  or
069300050322     c                             (wdsp > 20050100 and
069400050322     c                             wdsp < 20050300 and d$pll2 ='A')
069500050321     C                   ADD       IRP(I)        IMP(CP)
069600050321     c                   else
069700050321     C                   Z-ADD     IRP(I)        IMP(CP)
069800050321     c                   END
069900050321     c                   END
070000050321     C                   ENDIF
070100941223     C* IMPOSTA I RICAVI -DOPO- NELLA SCHIERA DI 999
070200941223     C     CRD(I)        IFNE      *ZEROS
070300941223     C                   Z-ADD     CRD(I)        CD
070400050321     C                   IF        D$GPGF = 'TNVRRICR'
070500050321     C                   add       IRd(I)        IMd(Cd)
070600050321     C                   Else
070700050322     c                   if        wdsp > 20050300 or
070800050322     c                             (wdsp > 20050100 and
070900050322     c                             wdsp < 20050300 and d$dll2 ='A')
071000050321     C                   ADD       IRd(I)        IMd(Cd)
071100050321     c                   else
071200050321     C                   Z-ADD     IRd(I)        IMd(Cd)
071300050321     c                   END
071400050321     c                   END
071500941223     C                   ENDIF
071600941223     C* IMPOSTA I RICAVI -DOPO- NELLA DS UNIFICATA
071700941201     C                   Z-ADD     CRD(I)        DSWCN
071800941201     C                   MOVE      DSWCA         CAL(I)
071900941201     C                   Z-ADD     IRD(I)        DSWIN
072000941201     C                   MOVE      DSWIA         IAL(I)
072100941201     C                   ENDDO
072200950125     C                   MOVEA     CAL           D$DCCD
072300950125     C                   MOVEA     IAL           D$DICD
072400940930     C*
072500940930     C                   ENDSR
072600941130     C********************************************************************
072700941130     C*  GESRIP - GESTIONE RICAVO BOLLA -PRIMA-
072800941130     C********************************************************************
072900941130     C     GESRIP        BEGSR
073000941130     C*
073100941130     C* PORTA I CAMPI -PRIMA- NEI CAMPI DI LAVORO
073200941130     C                   MOVEL     D$PFIN        WFIN
073300941130     C                   MOVEL     D$PCTS        WCTS
073400941130     C                   Z-ADD     D$PKSC        WKSC
073500990824     C                   Z-ADD     D$PPKF        WPKF
073600941130     C                   Z-ADD     D$PPOR        WPOR
073700000201     C                   MOVEL     D$PDIV        WDIV
073800941130     C                   MOVEA     PSVN          SVN
073900941130     C                   MOVEA     PVAN          VAN
074000970729     C                   Z-ADD     D$PDFT        WDFT
074100990217     C                   Z-ADD     D$PMGS        DSMGS
074200990217     C                   Z-ADD     D$PAAS        DSAAS
074300990217     C                   Z-ADD     DSDAT8        WDSP
074400941130     C* CALCOLA RICAVI
074500941130     C                   EXSR      CALRIC
074600941130     C* PORTA I RICAVI NELLE SCHIERE PRIMA
074700941212     C     D$PFRI        IFEQ      'S'
074800941130     C                   MOVEA     CR            CRP
074900941130     C                   MOVEA     IR            IRP
075000941130     C                   Z-ADD     WIRB          WPIRB
075100941212     C                   Z-ADD     WIRDN         WPIRD
075200941130     C                   Z-ADD     WIRBC         WPIRBC
075300941130     C                   ENDIF
075400941130     C*
075500941130     C                   Z-ADD     WIMR          WPIR
075600941201     C                   Z-ADD     WIIR          WPIIR             6 0
075700941130     C*
075800941130     C                   ENDSR
075900941130     C********************************************************************
076000941130     C*  GESRID - GESTIONE RICAVO BOLLA -DOPO-
076100941130     C********************************************************************
076200941130     C     GESRID        BEGSR
076300941130     C*
076400941130     C* PORTA I CAMPI -DOPO- NEI CAMPI DI LAVORO
076500941130     C                   MOVEL     D$DFIN        WFIN
076600941130     C                   MOVEL     D$DCTS        WCTS
076700941130     C                   Z-ADD     D$DKSC        WKSC
076800980317     C******************** MOVELD$DSCL    WSCL
076900990824     C                   Z-ADD     D$DPKF        WPKF
077000990823     C******************** Z-ADDD$DINL    WINL
077100941130     C                   Z-ADD     D$DPOR        WPOR
077200000201     C                   MOVEL     D$DDIV        WDIV
077300990823     C******************** Z-ADDD$DDIF    WDIF
077400990823     C******************** Z-ADDD$DBOL    WBOL
077500990823     C******************** Z-ADDD$DANT    WANT
077600941130     C                   MOVEA     DSVN          SVN
077700941130     C                   MOVEA     DVAN          VAN
077800970729     C                   Z-ADD     D$DDFT        WDFT
077900990217     C                   Z-ADD     D$DMGS        DSMGS
078000990217     C                   Z-ADD     D$DAAS        DSAAS
078100990217     C                   Z-ADD     DSDAT8        WDSP
078200970724     C********             MOVELD$DTPO    WTPO
078300970724     C********             Z-ADDD$DLNP    WLNP
078400970724     C********             Z-ADDD$DLNA    WLNA
078500941130     C* CALCOLA RICAVI
078600941130     C                   EXSR      CALRIC
078700970724     C* PORTA I RICAVI NELLE SCHIERE DOPO
078800941130     C     D$DFRI        IFEQ      'S'
078900941130     C                   MOVEA     CR            CRD
079000941130     C                   MOVEA     IR            IRD
079100941130     C* MEMORIZZA IMPORTI PER SALDI
079200941130     C                   Z-ADD     WIRB          WDIRB
079300941212     C                   Z-ADD     WIRDN         WDIRD
079400941130     C                   Z-ADD     WIRBC         WDIRBC
079500941130     C                   ENDIF
079600941130     C*
079700941130     C                   Z-ADD     WIMR          WDIR
079800941201     C                   Z-ADD     WIIR          WDIIR             6 0
079900941130     C*
080000941130     C                   ENDSR
080100941130     C********************************************************************
080200941130     C*  GESRIO - GESTIONE RICAVO BOLLA ORIGINALE
080300941130     C********************************************************************
080400941130     C     GESRIO        BEGSR
080500941130     C*
080600941130     C* PORTA I CAMPI BOLLA ORIGNALE NEI CAMPI DI LAVORO
080700941130     C                   MOVEL     D$OFIN        WFIN
080800941130     C                   MOVEL     D$OCTS        WCTS
080900941130     C                   Z-ADD     D$OKSC        WKSC
081000980317     C******************** MOVELD$OSCL    WSCL
081100990824     C                   Z-ADD     D$OPKF        WPKF
081200990823     C******************** Z-ADDD$OINL    WINL
081300941130     C                   Z-ADD     D$OPOR        WPOR
081400000201     C                   MOVEL     D$ODIV        WDIV
081500990823     C******************** Z-ADDD$ODIF    WDIF
081600990823     C******************** Z-ADDD$OBOL    WBOL
081700990823     C******************** Z-ADDD$OANT    WANT
081800941130     C                   MOVEA     OSVN          SVN
081900941130     C                   MOVEA     OVAN          VAN
082000970729     C                   Z-ADD     D$ODFT        WDFT
082100990217     C                   Z-ADD     D$OMGS        DSMGS
082200990217     C                   Z-ADD     D$OAAS        DSAAS
082300990217     C                   Z-ADD     DSDAT8        WDSP
082400970724     C*********            MOVELD$OTBL    WTPO
082500970724     C*********            Z-ADDD$OLNP    WLNP
082600970724     C*********            Z-ADDD$OLNA    WLNA
082700941130     C* CALCOLA RICAVI
082800941130     C                   EXSR      CALRIC
082900011214     C                   Z-ADD     WIMR          WOIR             19 4
083000941130     C*
083100941130     C                   ENDSR
083200941130     C********************************************************************
083300941130     C*  CALRIC - CALCOLA IL RICAVO BOLLA
083400941130     C********************************************************************
083500941130     C     CALRIC        BEGSR
083600020705     C*
083700020705     C* Se divisa <> divisa C/E, applica il cambio della divisa C/E (YEURCO).
083800020104IF  2C     WDIV          IFNE      WDIVCE
083900020104     c* PORTO
084000020104IF  3C     WPOR          IFNE      *ZEROS
084100011218     C                   Z-ADD     WPOR          §IMI
084200011218     C                   MOVEL     WDIV          §DVI
084300011218     C                   EXSR      CNVEUR
084400011218     C                   Z-ADD     §IMO          WPOR
084500020104E   3C                   ENDIF
084600020104     c* VARIE
084700020104DO  3C     1             DO        20            I
084800020104IF  4C     SVN(I)        IFNE      *BLANKS
084900020104IF  5C     VAN(I)        IFNE      *ZEROS
085000011218     C                   Z-ADD     VAN(I)        §IMI
085100011218     C                   MOVEL     WDIV          §DVI
085200011218     C                   EXSR      CNVEUR
085300020104     C                   Z-ADD     §IMO          VAN(I)
085400020104E   5C                   ENDIF
085500020104E   4C                   ENDIF
085600020104E   3C                   ENDDO
085700020104E   2C                   ENDIF
085800941130     C*
085900950207     C* AZZERA CAMPI DI LAVORO
086000941130     C                   Z-ADD     *ZEROS        V                 6 0
086100941212     C                   Z-ADD     *ZEROS        WIRB
086200941212     C                   Z-ADD     *ZEROS        WIRD
086300941212     C                   Z-ADD     *ZEROS        WIRBC
086400941212     C                   Z-ADD     *ZEROS        WIRDN
086500941212     C                   Z-ADD     *ZEROS        WIRBE
086600941212     C                   Z-ADD     *ZEROS        WIMR
086700941130     C                   CLEAR                   CR
086800941130     C                   CLEAR                   IR
086900941130     C                   CLEAR                   VR
087000941130     C*
087100941130     C* CARICA SCHIERA VARIE
087200941130     C     1             DO        20            I
087300941130     C     SVN(I)        IFNE      *BLANKS
087400941130     C                   ADD       1             V
087500941130     C                   MOVEL     SVN(I)        VR(V)
087600941130     C                   Z-ADD     VAN(I)        IR(V)
087700941130     C                   ENDIF
087800941130     C                   ENDDO
087900941130     C* PORTO
088000941130IF  1C     WPOR          IFNE      *ZEROS
088100941130     C                   ADD       1             V
088200970901     C                   MOVEL     §$2FPO        VR(V)
088300941130     C                   Z-ADD     WPOR          IR(V)
088400941130E   1C                   ENDIF
088500941130     C*
088600941130     C* CIASCUN IMPORTO DI LAVORO VALORIZZATO LO CONTROLLO CON LA TABELLA "CC"
088700941130     C* PER REPERIRE IL CODICE COMPETENZA E PER AGGREGARE DEGLI IMPORTI
088800941209DO  1C     1             DO        V             II                6 0
088900941130     C*
089000941130     C                   Z-ADD     1             I
089100941130     C                   SETOFF                                           99
089200941209     C     VR(II)        LOOKUP    CCC(I)                                 99
089300941130IF  2C     *IN99         IFEQ      *ON
089400941212     C                   Z-ADD     CMP(I)        CR(II)
089500941130IF  3C     CC1(I)        IFEQ      'S'
089600941209     C                   ADD       IR(II)        WIRB
089700941130E   3C                   ENDIF
089800941130IF  3C     CC2(I)        IFEQ      'S'
089900941209     C                   ADD       IR(II)        WIRD
090000941130E   3C                   ENDIF
090100941130IF  3C     CC3(I)        IFEQ      'S'
090200941209     C                   ADD       IR(II)        WIRBC
090300941130E   3C                   ENDIF
090400941130IF  3C     CC4(I)        IFEQ      'S'
090500941209     C                   ADD       IR(II)        WIRDN
090600941130E   3C                   ENDIF
090700941130IF  3C     CC5(I)        IFEQ      'S'
090800941209     C                   ADD       IR(II)        WIRBE
090900941130E   3C                   ENDIF
091000941130IF  3C     CC6(I)        IFEQ      'S'
091100941209     C                   ADD       IR(II)        WIMR
091200941130E   3C                   ENDIF
091300941130E   2C                   ENDIF
091400941130E   1C                   ENDDO
091500941130     C*
091600990217     C* AZZERAMENTO RICAVO DELTA BOLLE > 3.500 KG
091700040429IF  1C*    WDSP          IFGT      19990100
091800040429IF  2C*    WDSP          IFLT      20000100
091900040429IF  3C*    WPKF          IFGT      5000
092000040429     C*                  Z-ADD     *ZEROS        WIRD
092100040429     C*                  Z-ADD     *ZEROS        WIRDN
092200040429E   3C*                  ENDIF
092300040429E   2C*                  ENDIF
092400040429X   1C*                  ELSE
092500040429IF  2C*    WPKF          IFGT      3500
092600040429     C*                  Z-ADD     *ZEROS        WIRD
092700040429     C*                  Z-ADD     *ZEROS        WIRDN
092800040429E   2C*                  ENDIF
092900040429E   1C*                  ENDIF
093000941130     C*
093100941130     C* RICAVO STATISTICA RICAVI
093200941130     C                   ADD       1             V
093300941130     C                   Z-ADD     551           CR(V)
093400941130     C                   Z-ADD     WIRB          IR(V)
093500941130     C* RICAVO STATISTICA DELTA OLD
093600941130     C                   ADD       1             V
093700941130     C                   Z-ADD     552           CR(V)
093800941130     C                   Z-ADD     WIRD          IR(V)
093900941130     C* RICAVO STATISTICA CLIENTI
094000941130     C                   ADD       1             V
094100941130     C                   Z-ADD     553           CR(V)
094200941130     C                   Z-ADD     WIRBC         IR(V)
094300941130     C* RICAVO STATISTICA DELTA NEW
094400941130     C                   ADD       1             V
094500941130     C                   Z-ADD     554           CR(V)
094600941130     C                   Z-ADD     WIRDN         IR(V)
094700941130     C* RICAVO STATISTICA CONTO ECONOMICO (ORA LIBERO)
094800941130     C                   ADD       1             V
094900941130     C                   Z-ADD     555           CR(V)
095000941130     C                   Z-ADD     WIRBE         IR(V)
095100941130     C* IMPONIBILE REALE
095200941130     C                   ADD       1             V
095300941130     C                   Z-ADD     556           CR(V)
095400941130     C                   Z-ADD     WIMR          IR(V)
095500941201     C                   Z-ADD     V             WIIR              6 0
095600941130     C*
095700941130     C                   ENDSR
095800000201     C*------------------------------------------------------------------------*
095900000201     C* CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
096000000201     C*------------------------------------------------------------------------*
096100000201     C     CNVEUR        BEGSR
096200000201     C*
096300000201     C* AZZERA VARIABILI DI LAVORO
096400000203     C                   Z-ADD     *ZEROS        §IMO
096500000201     C*
096600000201     C                   CLEAR                   YEUR
096700000201     C                   MOVEL     §DVI          YECDVI
096800000201     C                   Z-ADD     §IMI          YECIMI
096900011218     C                   MOVEL     WDIVCE        YECDVO
097000011218     C                   MOVEL     '3'           YECDCO
097100011221     C                   MOVEL     'H'           YECTAR
097200000201     C                   CALL      'YEURCO'
097300000201     C                   PARM                    YEUR
097400000201IF  1C     YECESI        IFEQ      ' '
097500000201     C                   Z-ADD     YECIMO        §IMO
097600000201X   1C                   ELSE
097700000201     C                   Z-ADD     §IMI          §IMO
097800000201E   1C                   ENDIF
097900000201     C*
098000000201     C                   ENDSR
098100940314     C***********************************************************************
098200940314     C*   GESDSP - GESTIONE DELL'IMMAGINE PRIMA
098300940314     C***********************************************************************
098400940314     C     GESDSP        BEGSR
098500941215     C*
098600941215     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
098700941215     C     D$OLNP        IFNE      *ZEROS
098800950913     C     D$PFIG        ANDEQ     'S'
098900941215     C                   Z-ADD     D$OLNP        W$PLNP
099000941215     C                   ELSE
099100941215     C                   Z-ADD     D$PLNP        W$PLNP
099200941215     C                   ENDIF
099300941215     C     D$OTFP        IFNE      *ZEROS
099400950913     C     D$PFIG        ANDEQ     'S'
099500941215     C                   Z-ADD     D$OTFP        W$PTFP
099600941215     C                   ELSE
099700941215     C                   Z-ADD     D$PTFP        W$PTFP
099800941215     C                   ENDIF
099900941215     C     D$OKSC        IFNE      *ZEROS
100000950913     C     D$PFIG        ANDEQ     'S'
100100941215     C                   Z-ADD     D$OKSC        W$PKSC
100200941215     C                   ELSE
100300941215     C                   Z-ADD     D$PKSC        W$PKSC
100400941215     C                   ENDIF
100500941215     C     D$OCCM        IFNE      *ZEROS
100600950913     C     D$PFIG        ANDEQ     'S'
100700941215     C                   Z-ADD     D$OCCM        W$PCCM
100800941215     C                   ELSE
100900941215     C                   Z-ADD     D$PCCM        W$PCCM
101000941215     C                   ENDIF
101100941215     C     D$OMCT        IFNE      *BLANKS
101200950913     C     D$PFIG        ANDEQ     'S'
101300941215     C                   MOVEL     D$OMCT        W$PMCT
101400941215     C                   ELSE
101500941215     C                   MOVEL     D$PMCT        W$PMCT
101600941215     C                   ENDIF
101700940314     C*
101800931111     C* PREPARO CAMPI DI LAVORO -PRIMA-
101900941123     C                   Z-ADD     D$PMGS        DSMGS
102000931110     C                   Z-ADD     DSMES         WPMES
102100940304     C     DSGIO         IFLE      10
102200940304     C                   MOVEL     '1'           WPDEC
102300940304     C                   END
102400940304     C     DSGIO         IFGE      11
102500940304     C     DSGIO         ANDLE     20
102600940304     C                   MOVEL     '2'           WPDEC
102700940304     C                   END
102800940304     C     DSGIO         IFGE      21
102900940304     C                   MOVEL     '3'           WPDEC
103000940304     C                   END
103100030131     C*
103200030131     C* ----- SICOP -----
103300030131     C* AGGIORNA DAL 2003
103400030131     C                   Z-ADD     D$PAAS        DSWAAS
103500030131     C                   Z-ADD     D$PMGS        DSWMGS
103600030131     C                   Z-ADD     DSWDAT        WPDATCOP
103700020829     C*
103800020829     C* ----- PESO -----
103900020829     c* PRIMA agosto 2002
104000020829     C* SEMPRE PESO DA FATTURARE
104100020829     C                   Z-ADD     D$PAAS        DSWAAS
104200020829     C                   Z-ADD     D$PMGS        DSWMGS
104300020829IF  1C                   IF        DSWDAT < 20020800
104400020829     C                   Z-ADD     D$PPKF        WPPES
104500020829X   1C                   ELSE
104600020829     c* DOPO agosto 2002
104700020829     C* SE I COLLI=COLLI CML PRENDO PESO CML
104800020829IF  2C     D$PPKC        IFGT      *ZEROS
104900020829     C     D$PNCL        ANDEQ     D$PNCP
105000020829     C                   Z-ADD     D$PPKC        WPPES
105100020829     C                   Z-ADD     D$PPKB        WPPCC
105200020829     C                   Z-ADD     D$PPKC        WPPLS
105300020829X   2C                   ELSE
105400020829     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
105500020829IF  3C     D$PPKF        IFGT      D$PPKC
105600020829     C                   Z-ADD     D$PPKF        WPPES
105700020829X   3C                   ELSE
105800020829     C                   Z-ADD     D$PPKC        WPPES
105900020829E   3C                   ENDIF
106000020829E   2C                   ENDIF
106100020829E   1C                   ENDIF
106200990218     C*
106300990218     C* CARICA TABELLA -2L- FASCIE DI PESO
106400990218     C                   Z-ADD     D$PAAS        WA2L              4 0
106500990218     C                   EXSR      CART2L
106600931110     C*
106700940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE RICAVI/MOVIMENTI
106800020829     C*--- D$PPKF        IFEQ      *ZEROS
106900020829     C     WPPES         IFEQ      *ZEROS
107000940103     C                   MOVEL     FPR(1)        WPFPR
107100940103     C                   ELSE
107200931110     C                   DO        20            I                 4 0
107300020829     C*--- D$PPKF        IFGT      KIR(I)
107400020829     C*--- D$PPKF        ANDLE     KFR(I)
107500020829     C     WPPES         IFGT      KIR(I)
107600020829     C     WPPES         ANDLE     KFR(I)
107700940103     C                   MOVEL     FPR(I)        WPFPR
107800950104     C                   LEAVE
107900931110     C                   ENDIF
108000931110     C                   ENDDO
108100940103     C                   ENDIF
108200940103     C*
108300940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
108400020829     C*--- D$PPKF        IFEQ      *ZEROS
108500020829     C     WPPES         IFEQ      *ZEROS
108600940103     C                   MOVEL     FPC(1)        WPFPC
108700940103     C                   ELSE
108800940317     C                   DO        20            I
108900020829     C*--- D$PPKF        IFGT      KIC(I)
109000020829     C*--- D$PPKF        ANDLE     KFC(I)
109100020829     C     WPPES         IFGT      KIC(I)
109200020829     C     WPPES         ANDLE     KFC(I)
109300940103     C                   MOVEL     FPC(I)        WPFPC
109400950104     C                   LEAVE
109500940103     C                   ENDIF
109600940103     C                   ENDDO
109700940103     C                   ENDIF
109800940103     C*
109900940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE TRAINI
110000020829     C*--- D$PPKF        IFEQ      *ZEROS
110100020829     C     WPPES         IFEQ      *ZEROS
110200940103     C                   MOVEL     FPT(1)        WPFPT
110300940103     C                   ELSE
110400940317     C                   DO        20            I
110500020829     C*--- D$PPKF        IFGT      KIT(I)
110600020829     C*--- D$PPKF        ANDLE     KFT(I)
110700020829     C     WPPES         IFGT      KIT(I)
110800020829     C     WPPES         ANDLE     KFT(I)
110900940103     C                   MOVEL     FPT(I)        WPFPT
111000950104     C                   LEAVE
111100940103     C                   ENDIF
111200940103     C                   ENDDO
111300940103     C                   ENDIF
111400931111     C*
111500931111     C* RECUPERO LA REGIONE PARTENZA
111600931111     C                   Z-ADD     1             I
111700950324     C     W$PLNP        LOOKUP    FIL(I)                                 99
111800931111     C     *IN99         IFEQ      '0'
111900941223     C                   MOVEL     MSG(18)       $MSG
112000941026     C                   EXSR      SCRERR
112100931111     C                   ELSE
112200020514     C     D$PAAS        IFLE      2001
112300940207     C                   Z-ADD     RRE(I)        WPREP
112400020514     C                   ELSE
112500020514     C                   Z-ADD     RRN(I)        WPREP
112600020514     C                   ENDIF
112700931111     C                   END
112800931111     C*
112900931111     C* RECUPERO LA REGIONE DI ARRIVO
113000931111     C                   Z-ADD     1             I
113100931111     C     D$PLNA        LOOKUP    FIL(I)                                 99
113200931111     C     *IN99         IFEQ      '0'
113300941223     C                   MOVEL     MSG(18)       $MSG
113400941026     C                   EXSR      SCRERR
113500931111     C                   ELSE
113600020514     C     D$PAAS        IFLE      2001
113700940207     C                   Z-ADD     RRE(I)        WPREA
113800020514     C                   ELSE
113900020514     C                   Z-ADD     RRN(I)        WPREA
114000020514     C                   ENDIF
114100931111     C                   END
114200931110     C*
114300940511     C* ----- VOLUME -----
114400990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
114500990422IF  1C     D$PFVF        IFEQ      'T'
114600990422     C                   Z-ADD     D$PVLF        WPVOL
114700990422     C                   Z-ADD     D$PVLB        WPVCC
114800990422     C                   Z-ADD     D$PVLF        WPVLS
114900990422X   1C                   ELSE
115000990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
115100990422IF  2C     D$PVLC        IFGT      *ZEROS
115200990422     C     D$PNCL        ANDEQ     D$PNCR
115300990422     C                   Z-ADD     D$PVLC        WPVOL
115400990422     C                   Z-ADD     D$PVLB        WPVCC
115500990422     C                   Z-ADD     D$PVLC        WPVLS
115600990422X   2C                   ELSE
115700990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
115800990422IF  3C     D$PVLF        IFGT      D$PVLC
115900990422     C                   Z-ADD     D$PVLF        WPVOL
116000990422X   3C                   ELSE
116100990422     C                   Z-ADD     D$PVLC        WPVOL
116200990422E   3C                   ENDIF
116300990422E   2C                   ENDIF
116400990422E   1C                   ENDIF
116500931110     C*
116600931110     C* PESO TASSABILE (ECCEDENZA)
116700990824     C     D$PPVL        IFGT      D$PPKF
116800990824     C     D$PPVL        SUB       D$PPKF        WPPTS
116900931110     C                   END
117000031029     C*
117100031029     C* BANCALI
117200031029     C                   EVAL      WPBAN = D$PAR5NBA + D$PAR5NB2
117300940315     C*
117400940315     C* IMPOSTA SCHIERE CONSEGNE PARTICOLARI
117500941130     C                   MOVEA     PSVN          SVN
117600941130     C                   MOVEA     PVAN          VAN
117700090304     C                   MOVEL     D$Pncl        WPncl
117800090304     C                   MOVEL     D$PFTC        WPFTC
117900941123     C                   MOVEL     D$PTC2        WPTC2
118000941123     C                   MOVEL     D$PFGC        WPFGC
118100040322     C                   MOVEL     D$Pflo        WPflo
118200941123     C                   MOVEL     D$PFCA        WPFCA
118300040524     C                   MOVEL     D$Psta        WPsta
118400990823     C                   Z-ADD     D$PIAS        WPIAS
118500970901     C                   Z-ADD     D$PPOR        WPPOR
118600970901     C                   MOVEL     D$PLL1        WPLL1
118700970901     C                   MOVEL     D$PTPO        WPTPO
118800970901     C                   MOVEL     D$PFAP        WPFAP
118900970901     C                   MOVEL     D$PFIN        WPFIN
119000970901     C                   Z-ADD     D$PDFT        WPDFT
119100981105     C                   Z-ADD     D$PKSC        WPKSC
119200000201     C                   MOVEL     D$PDIV        WPDIV
119300020620     C                   MOVEL     D$PTBL        WPTBL
119400020620     C                   MOVEL     D$PCBO        WPCBO
119500030114     C                   EXSR      GESCSP
119600940315     C                   MOVEA     SVX           SVP
119700940315     C                   Z-ADD     IVX           IVP
119800940330     C                   Z-ADD     NSX           NSP
119900940315     C                   MOVEA     CPX           CPP
120000990617     C                   MOVEL     WPREG         WPPREG
120100940407     C*
120200990617     C* CONTROLLO SE LA BOLLA PRIMA E' DA CONSIDERARE O NO IN SISDT
120300941201     C     D$PFSP        IFEQ      'N'
120400941021     C     D$PSCL        OREQ      '2'
120500990617     C                   MOVEL     'N'           WTPREG
120600941021     C                   END
120700940411     C*
120800990617     C* SE E' STATA CONSEGNATA BISOGNA TENERNE CONTO PER I SALDI SISDA
120900940411     C     D$PDCM        IFEQ      *ZEROS
121000941201     C     D$PRBL        ORNE      'N'
121100990617     C                   MOVEL     'N'           WAPREG
121200940411     C                   END
121300940411     C*
121400940411     C* SE LINEA PARTENZA = CAGLIARI,PORTO TORRES,ORISTANO,BARI
121500990218     C* SE IL PESO DELLA SPEDIZIONE E' SUPERIORE A 35 QLI('98)/ 50 QLI ('99)
121600940411     C* SE IL TIPO BOLLA E' F3
121700940411     C* SIGNIFICA CHE SI TRATTA DI UNA BOLLA DI COMPETENZA TRAINI
121800940411     C* ED E' QUINDI DA NON CONSIDERARE COME RICAVO DELLA SARDEGNA
121900040429     c* usato pkf xchè cml sempre = 0
122000990218     C     D$PAAS        IFLE      1998
122100990218     C                   Z-ADD     3500          WKGLIM
122200990218     C                   ELSE
122300990218     C                   Z-ADD     5000          WKGLIM
122400990218     C                   ENDIF
122500940411     C     D$PLNP        IFEQ      81
122600940411     C     D$PLNP        OREQ      82
122700940411     C     D$PLNP        OREQ      104
122800940411     C     D$PLNP        OREQ      7
122900990421     C     D$PLNP        OREQ      113
123000990824     C     D$PPKF        IFGT      WKGLIM
123100940411     C     D$PTBL        ANDEQ     'F3'
123200990617     C                   MOVEL     'N'           WCPREG
123300990617     C                   MOVEL     'N'           WOPREG
123400990617     C                   MOVEL     'N'           WRPREG
123500990617     C                   MOVEL     'N'           WFPREG
123600040317     C                   MOVEL     'N'           WEPREG
123700940411     C                   END
123800940411     C                   END
123900950104     C*
124000950104     C* CONTROLLO SE LE COMPETENZE PRIMA SONO DA CALCOLARE
124100950104     C*    SE FLAG 'COMP.DA CALC.SEMPRE' = 'S' --> LANCIO
124200950104     C     D$PCCS        IFEQ      'S'
124300950905     C                   MOVEL     'S'           WFCP
124400950104     C                   ELSE
124500950104     C*    SE BOLLA FIGLIA E FLAG 'COMPET.DA CALC. SE FIGLIA' = SI --> LANCIO
124600950905     C     D$PFIG        IFEQ      'S'
124700950104     C     D$PCSF        ANDEQ     'S'
124800950104     C                   MOVEL     'S'           WFCP
124900950104     C                   ENDIF
125000950104     C                   ENDIF
125100950104     C*    SE BOLLA COMPLETAMENTE STORNATA (NC. A STORNO TOTALE) --> NO LANCIO
125200020318     C     D$PFA2        IFEQ      'S'
125300020318     C                   MOVEL     'N'           WFCP
125400020318     C                   ENDIF
125500941215     C*
125600941215     C* CONTROLLO SE LA BOLLA DEVE ESSERE CALCOLATA (COME IMPORTI E DATI)
125700941215IF  1C     D$PFRI        IFEQ      'S'
125800950104     C     WFCP          OREQ      'S'
125900941215     C*
126000941215     C                   MOVEL     'P'           WPD               1
126100941223     C*
126200941215     C* CONVERTE DATA BOLLA
126300941215     C                   Z-ADD     D$PMGS        DSMGS
126400941215     C                   Z-ADD     D$PAAS        DSAAS
126500941215     C                   Z-ADD     DSDAT8        WPDSP8            8 0
126600990217     C*********************Z-ADDDSDAT8    WDSP
126700941215     C*
126800941215     C* LEGGE IL BLOCCO COMPET.ZE CON IL QUALE LA BOLLA E'STATA PRECED.CALCOLATA
126900941215     C                   EXSR      LETNBP
127000941215     C*
127100941215     C* CONTROLLA SE IL N.BLOCCO PRIMA CHE E' IN MEMORIA E' VALIDO PER LA BOLLA
127200941215     C     $ERR          IFEQ      'N'
127300941215     C                   EXSR      CHKNBC
127400941215     C                   ENDIF
127500941215     C*
127600941215     C* CARICA LE COMPETENZE PRIMA DAI FILE ECCET E ECCED
127700941215     C     $ERR          IFEQ      'N'
127800941215     C                   EXSR      CARCOP
127900941215     C                   ENDIF
128000030114     C*
128100030509     C* LEGGE LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
128200030131     C* DAL 2003
128300030131     C                   MOVEL     'N'           WCOPREG           1
128400030131     C     WPDATCOP      IFGT      20030000
128500030131     C     $ERR          IFEQ      'N'
128600030114     C                   EXSR      LETCEV
128700030114     C                   ENDIF
128800030114     C*
128900030509     C* CARICA LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
129000030114     C     $ERR          IFEQ      'N'
129100030114     C                   EXSR      CARCEV
129200030114     C                   ENDIF
129300030131     C                   ENDIF
129400941215     C*
129500980323     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE TRA BARTOLINI E SDI
129600980323     C* SE BOLLA VALIDA PER RIFATT. BAR/SDI, CERCO COMPET. GIA' FATTURATE
129700011127     C*---       D$PAAS    IFGE 1998                       *SOLO DAL '98
129800011127     C*---                 MOVEL'S'       WIPREG           *FLAG REG. ECFIC ?
129900011127     C*---                 EXSR CARFIC                     *LEGGE ECFIC
130000011127     C*---                 END
130100980318     C*
130200941215E   1C                   ENDIF
130300980313     C*
130400980313     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE DA ITL A BAR/SDI
130500980323     C* SE BOLLA VALIDA PER RIFATT. ITL, CERCO MC GIA' FATTURATI
130600980323     C     D$PTBL        IFEQ      'F1'
130700980323     C     D$PTBL        OREQ      'A2'
130800980323     C     D$PTFP        IFNE      D$PTFA
130900980323     C     D$PAAS        ANDGE     1998
131000980323     C                   MOVEL     'S'           WMPREG
131100980323     C                   EXSR      CARFIM
131200980313     C                   END
131300980323     C                   END
131400931110     C*
131500940314     C                   ENDSR
131600940314     C***********************************************************************
131700940314     C*   GESDSD - GESTIONE DELL'IMMAGINE DOPO
131800940314     C***********************************************************************
131900940314     C     GESDSD        BEGSR
132000941215     C*
132100941215     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
132200950905     C*
132300941215     C     D$OLNP        IFNE      *ZEROS
132400950913     C     D$DFIG        ANDEQ     'S'
132500941215     C                   Z-ADD     D$OLNP        W$DLNP
132600941215     C                   ELSE
132700941215     C                   Z-ADD     D$DLNP        W$DLNP
132800941215     C                   ENDIF
132900941215     C     D$OTFP        IFNE      *ZEROS
133000950913     C     D$DFIG        ANDEQ     'S'
133100941215     C                   Z-ADD     D$OTFP        W$DTFP
133200941215     C                   ELSE
133300941215     C                   Z-ADD     D$DTFP        W$DTFP
133400941215     C                   ENDIF
133500941215     C     D$OKSC        IFNE      *ZEROS
133600950913     C     D$DFIG        ANDEQ     'S'
133700941215     C                   Z-ADD     D$OKSC        W$DKSC
133800941215     C                   ELSE
133900941215     C                   Z-ADD     D$DKSC        W$DKSC
134000941215     C                   ENDIF
134100941215     C     D$OCCM        IFNE      *ZEROS
134200950913     C     D$DFIG        ANDEQ     'S'
134300941215     C                   Z-ADD     D$OCCM        W$DCCM
134400941215     C                   ELSE
134500941215     C                   Z-ADD     D$DCCM        W$DCCM
134600941215     C                   ENDIF
134700941215     C     D$OMCT        IFNE      *BLANKS
134800950913     C     D$DFIG        ANDEQ     'S'
134900941215     C                   MOVEL     D$OMCT        W$DMCT
135000941215     C                   ELSE
135100941215     C                   MOVEL     D$DMCT        W$DMCT
135200941215     C                   ENDIF
135300950905     C*
135400931111     C* PREPARO CAMPI DI LAVORO -DOPO-
135500941212     C                   Z-ADD     D$DMGS        DSMGS
135600931111     C                   Z-ADD     DSMES         WDMES
135700940304     C     DSGIO         IFLE      10
135800940304     C                   MOVEL     '1'           WDDEC
135900940304     C                   END
136000940304     C     DSGIO         IFGE      11
136100940304     C     DSGIO         ANDLE     20
136200940304     C                   MOVEL     '2'           WDDEC
136300940304     C                   END
136400940304     C     DSGIO         IFGE      21
136500940304     C                   MOVEL     '3'           WDDEC
136600940304     C                   END
136700030131     C*
136800030131     C* ----- SICOP -----
136900030131     C* AGGIORNA DAL 2003
137000030131     C                   Z-ADD     D$DAAS        DSWAAS
137100030131     C                   Z-ADD     D$DMGS        DSWMGS
137200030131     C                   Z-ADD     DSWDAT        WDDATCOP
137300020829     C*
137400020829     C* ----- PESO -----
137500020829     c* PRIMA agosto 2002
137600020829     C* SEMPRE PESO DA FATTURARE
137700020829     C                   Z-ADD     D$DAAS        DSWAAS
137800020829     C                   Z-ADD     D$DMGS        DSWMGS
137900020829IF  1C                   IF        DSWDAT < 20020800
138000020829     C                   Z-ADD     D$DPKF        WDPES
138100020829X   1C                   ELSE
138200020829     c* DOPO agosto 2002
138300020829     C* SE I COLLI=COLLI CML PRENDO PESO CML
138400020829IF  2C     D$DPKC        IFGT      *ZEROS
138500020829     C     D$DNCL        ANDEQ     D$DNCP
138600020829     C                   Z-ADD     D$DPKC        WDPES
138700020829     C                   Z-ADD     D$DPKB        WDPCC
138800020829     C                   Z-ADD     D$DPKC        WDPLS
138900020829X   2C                   ELSE
139000020829     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
139100020829IF  3C     D$DPKF        IFGT      D$DPKC
139200020829     C                   Z-ADD     D$DPKF        WDPES
139300020829X   3C                   ELSE
139400020829     C                   Z-ADD     D$DPKC        WDPES
139500020829E   3C                   ENDIF
139600020829E   2C                   ENDIF
139700020829E   1C                   ENDIF
139800990218     C*
139900990218     C* CARICA TABELLA -2L- FASCIE DI PESO
140000990218     C                   Z-ADD     D$DAAS        WA2L              4 0
140100990218     C                   EXSR      CART2L
140200931111     C*
140300940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE RICAVI/MOVIMENTI
140400020829     C*--- D$DPKF        IFEQ      *ZEROS
140500020829     C     WDPES         IFEQ      *ZEROS
140600940103     C                   MOVEL     FPR(1)        WDFPR
140700940103     C                   ELSE
140800940317     C                   DO        20            I
140900020829     C*--- D$DPKF        IFGT      KIR(I)
141000020829     C*--- D$DPKF        ANDLE     KFR(I)
141100020829     C     WDPES         IFGT      KIR(I)
141200020829     C     WDPES         ANDLE     KFR(I)
141300940103     C                   MOVEL     FPR(I)        WDFPR
141400950104     C                   LEAVE
141500931111     C                   ENDIF
141600931111     C                   ENDDO
141700940103     C                   ENDIF
141800940103     C*
141900940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
142000020829     C*--- D$DPKF        IFEQ      *ZEROS
142100020829     C     WDPES         IFEQ      *ZEROS
142200940103     C                   MOVEL     FPC(1)        WDFPC
142300940103     C                   ELSE
142400940317     C                   DO        20            I
142500020829     C*--- D$DPKF        IFGT      KIC(I)
142600020829     C*--- D$DPKF        ANDLE     KFC(I)
142700020829     C     WDPES         IFGT      KIC(I)
142800020829     C     WDPES         ANDLE     KFC(I)
142900940103     C                   MOVEL     FPC(I)        WDFPC
143000950104     C                   LEAVE
143100940103     C                   ENDIF
143200940103     C                   ENDDO
143300940103     C                   ENDIF
143400940103     C*
143500940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE TRAINI
143600020829     C*--- D$DPKF        IFEQ      *ZEROS
143700020829     C     WDPES         IFEQ      *ZEROS
143800940103     C                   MOVEL     FPT(1)        WDFPT
143900940103     C                   ELSE
144000940317     C                   DO        20            I
144100020829     C*--- D$DPKF        IFGT      KIT(I)
144200020829     C*--- D$DPKF        ANDLE     KFT(I)
144300020829     C     WDPES         IFGT      KIT(I)
144400020829     C     WDPES         ANDLE     KFT(I)
144500940103     C                   MOVEL     FPT(I)        WDFPT
144600950104     C                   LEAVE
144700940103     C                   ENDIF
144800940103     C                   ENDDO
144900940103     C                   ENDIF
145000931111     C*
145100931111     C* RECUPERO LA REGIONE PARTENZA
145200931111     C                   Z-ADD     1             I
145300950324     C     W$DLNP        LOOKUP    FIL(I)                                 99
145400931111     C     *IN99         IFEQ      '0'
145500941223     C                   MOVEL     MSG(18)       $MSG
145600941026     C                   EXSR      SCRERR
145700931111     C                   ELSE
145800020514     C     D$DAAS        IFLE      2001
145900940207     C                   Z-ADD     RRE(I)        WDREP
146000020514     C                   ELSE
146100020514     C                   Z-ADD     RRN(I)        WDREP
146200020514     C                   ENDIF
146300931111     C                   END
146400931111     C*
146500931111     C* RECUPERO LA REGIONE DI ARRIVO
146600931111     C                   Z-ADD     1             I
146700931111     C     D$DLNA        LOOKUP    FIL(I)                                 99
146800931111     C     *IN99         IFEQ      '0'
146900941223     C                   MOVEL     MSG(18)       $MSG
147000941026     C                   EXSR      SCRERR
147100931111     C                   ELSE
147200020514     C     D$DAAS        IFLE      2001
147300940207     C                   Z-ADD     RRE(I)        WDREA
147400020514     C                   ELSE
147500020514     C                   Z-ADD     RRN(I)        WDREA
147600020514     C                   ENDIF
147700931111     C                   END
147800940121     C*
147900940511     C* ----- VOLUME -----
148000990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
148100990422IF  1C     D$DFVF        IFEQ      'T'
148200990422     C                   Z-ADD     D$DVLF        WDVOL
148300990422     C                   Z-ADD     D$DVLB        WDVCC
148400990422     C                   Z-ADD     D$DVLF        WDVLS
148500990422X   1C                   ELSE
148600990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
148700990422IF  2C     D$DVLC        IFGT      *ZEROS
148800990422     C     D$DNCL        ANDEQ     D$DNCR
148900990422     C                   Z-ADD     D$DVLC        WDVOL
149000990422     C                   Z-ADD     D$DVLB        WDVCC
149100990422     C                   Z-ADD     D$DVLC        WDVLS
149200990422X   2C                   ELSE
149300990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
149400990422IF  3C     D$DVLF        IFGT      D$DVLC
149500990422     C                   Z-ADD     D$DVLF        WDVOL
149600990422X   3C                   ELSE
149700990422     C                   Z-ADD     D$DVLC        WDVOL
149800990422E   3C                   ENDIF
149900990422E   2C                   ENDIF
150000990422E   1C                   ENDIF
150100931111     C*
150200931111     C* PESO TASSABILE (ECCEDENZA)
150300990824     C     D$DPVL        IFGT      D$DPKF
150400990824     C     D$DPVL        SUB       D$DPKF        WDPTS
150500931111     C                   END
150600031029     C*
150700031029     C* BANCALI
150800031029     C                   EVAL      WDBAN = D$DAR5NBA + D$DAR5NB2
150900931111     C*
151000940315     C* IMPOSTA SCHIERE CONSEGNE PARTICOLARI
151100941130     C                   MOVEA     DSVN          SVN
151200941130     C                   MOVEA     DVAN          VAN
151300090304     C                   MOVEL     D$Dncl        WPncl
151400090304     C                   MOVEL     D$DFTC        WPFTC
151500941123     C                   MOVEL     D$DTC2        WPTC2
151600941123     C                   MOVEL     D$DFGC        WPFGC
151700040322     C                   MOVEL     D$DFlo        WPFlo
151800941123     C                   MOVEL     D$DFCA        WPFCA
151900040524     C                   MOVEL     D$Dsta        WPsta
152000990823     C                   Z-ADD     D$DIAS        WPIAS
152100970901     C                   Z-ADD     D$DPOR        WPPOR
152200970901     C                   MOVEL     D$DLL1        WPLL1
152300970901     C                   MOVEL     D$DTPO        WPTPO
152400970901     C                   MOVEL     D$DFAP        WPFAP
152500970901     C                   MOVEL     D$DFIN        WPFIN
152600970901     C                   Z-ADD     D$DDFT        WPDFT
152700981105     C                   Z-ADD     D$DKSC        WPKSC
152800000201     C                   MOVEL     D$DDIV        WPDIV
152900020620     C                   MOVEL     D$DTBL        WPTBL
153000020620     C                   MOVEL     D$DCBO        WPCBO
153100030114     C                   EXSR      GESCSP
153200940315     C                   MOVEA     SVX           SVD
153300940315     C                   Z-ADD     IVX           IVD
153400940330     C                   Z-ADD     NSX           NSD
153500940315     C                   MOVEA     CPX           CPD
153600990617     C                   MOVEL     WPREG         WPDREG
153700940407     C*
153800940407     C* CONTROLLO SE LA BOLLA DOPO E' DA CONSIDERARE O NO NELLA TRAINI
153900941201     C     D$DFSP        IFEQ      'N'
154000941021     C     D$DSCL        OREQ      '2'
154100990617     C                   MOVEL     'N'           WTDREG
154200941021     C                   END
154300940411     C*
154400940411     C* SE E' STATA CONSEGNATA BISOGNA TENERNE CONTO PER I SALDI AFFIDABILITA'
154500940411     C     D$DDCM        IFEQ      *ZEROS
154600941201     C     D$DRBL        ORNE      'N'
154700990617     C                   MOVEL     'N'           WADREG
154800940411     C                   END
154900940411     C*
155000940411     C* SE LINEA PARTENZA = CAGLIARI,PORTO TORRES,ORISTANO,BARI
155100990218     C* SE IL PESO DELLA SPEDIZIONE E' SUPERIORE A 35/50 QLI
155200940411     C* SE IL TIPO BOLLA E' F3
155300940411     C* SIGNIFICA CHE SI TRATTA DI UNA BOLLA DI COMPETENZA TRAINI
155400940411     C* ED E' QUINDI DA NON CONSIDERARE COME RICAVO DELLA SARDEGNA
155500990218     C     D$DAAS        IFLE      1998
155600990218     C                   Z-ADD     3500          WKGLIM
155700990218     C                   ELSE
155800990218     C                   Z-ADD     5000          WKGLIM
155900990218     C                   ENDIF
156000940411     C     D$DLNP        IFEQ      81
156100940411     C     D$DLNP        OREQ      82
156200940411     C     D$DLNP        OREQ      104
156300940411     C     D$DLNP        OREQ      7
156400990420     C     D$DLNP        OREQ      113
156500990824     C     D$DPKF        IFGT      WKGLIM
156600940411     C     D$DTBL        ANDEQ     'F3'
156700990617     C                   MOVEL     'N'           WCDREG
156800990617     C                   MOVEL     'N'           WODREG
156900990617     C                   MOVEL     'N'           WRDREG
157000990617     C                   MOVEL     'N'           WFDREG
157100040317     C                   MOVEL     'N'           WeDREG
157200940411     C                   END
157300940411     C                   END
157400950104     C*
157500950104     C* CONTROLLO SE LE COMPETENZE DOPO SONO DA CALCOLARE
157600950104     C*    SE FLAG 'COMP.DA CALC.SEMPRE' = 'S' --> LANCIO
157700950104     C     D$DCCS        IFEQ      'S'
157800950104     C                   MOVEL     'S'           WFCD
157900950104     C                   ELSE
158000950104     C*    SE BOLLA FIGLIA E FLAG 'COMPET.DA CALC. SE FIGLIA' = SI --> LANCIO
158100950905     C     D$DFIG        IFEQ      'S'
158200950104     C     D$DCSF        ANDEQ     'S'
158300950104     C                   MOVEL     'S'           WFCD
158400950104     C                   ENDIF
158500950104     C                   ENDIF
158600950104     C*    SE BOLLA COMPLETAMENTE STORNATA (NC. A STORNO TOTALE) --> NO LANCIO
158700020318     C     D$DFA2        IFEQ      'S'
158800020318     C                   MOVEL     'N'           WFCD
158900020318     C                   ENDIF
159000941215     C*
159100941215     C* CONTROLLO SE LA BOLLA DEVE ESSERE CALCOLATA (COME IMPORTI E DATI)
159200941215IF  1C     D$DFRI        IFEQ      'S'
159300950104     C     WFCD          OREQ      'S'
159400941215     C*
159500941215     C                   MOVEL     'D'           WPD               1
159600941215     C* CONVERTE DATA BOLLA DA LUNGA 6 A LUNGA 8
159700941215     C                   Z-ADD     D$DMGS        DSMGS
159800941215     C                   Z-ADD     D$DAAS        DSAAS
159900941215     C                   Z-ADD     DSDAT8        WDDSP8            8 0
160000941215     C*
160100941215     C* LEGGE IL BLOCCO COMPET.ZE ATTUALE DALLA TABELLA SE NON L'HA GIA' LETTO
160200941215     C     WFLNBD        IFNE      'S'
160300941215     C                   MOVEL     'S'           WFLNBD            1
160400941215     C                   EXSR      LETNBD
160500941215     C                   ENDIF
160600941215     C*
160700941215     C* CONTROLLA SE IL N.BLOCCO -DOPO- CHE E' IN MEMORIA E' VALIDO PER LA BOLLA
160800941215     C     $ERR          IFEQ      'N'
160900941215     C                   EXSR      CHKNBC
161000941215     C                   ENDIF
161100941223     C*
161200941223     C* CALCOLA LE COMPETENZE DOPO
161300941223     C     $ERR          IFEQ      'N'
161400941223     C                   EXSR      GESCOD
161500941223     C                   ENDIF
161600941215     C*
161700941215E   1C                   ENDIF
161800980313     C*
161900980313     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE DA ITL A BAR/SDI
162000980323     C     D$DTBL        IFEQ      'F1'
162100980323     C     D$DTBL        OREQ      'A2'
162200980323     C     D$DTFP        IFNE      D$DTFA
162300980323     C     D$DAAS        ANDGE     1998
162400980323     C                   MOVEL     'S'           WMDREG
162500980323     C                   Z-ADD     WDVOL         WDSMC
162600980313     C                   END
162700980323     C                   END
162800941215     C*
162900930323     C                   ENDSR
163000941215     C*------------------------------------------------------------------------*
163100941215     C*?CHKNBC - CONTROLLA N.BLOCCO COMPETENZE -PRIMA- O -DOPO-
163200941215     C*?         SE NON TROVATO CARICA DA FILE.
163300941215     C*------------------------------------------------------------------------*
163400941215     C     CHKNBC        BEGSR
163500941215     C*
163600941215     C                   MOVEL     'N'           WCARIC            1
163700941215     C*
163800941215     C* SE 'PRIMA' E SE N.BLOCCO RICHIESTO <> DAL BLOCCO ATTUALMENTE CARICATO
163900941215IF  1C     WPD           IFEQ      'P'
164000941215     C     WNBCP         ANDNE     WNBAP
164100941215     C*
164200941215     C* AZZERA LA SCHIERA PRIMA
164300941215     C                   CLEAR                   CDP
164400020130     C                   CLEAR                   FDP
164500941215     C                   CLEAR                   DDP
164600941215     C                   CLEAR                   DSP
164700941215     C                   CLEAR                   KCP
164800941215     C                   CLEAR                   KSP
164900941215     C                   CLEAR                   RIP
165000941215     C                   CLEAR                   DAP
165100941215     C                   CLEAR                   PEP
165200941215     C                   CLEAR                   FAP
165300941215     C                   CLEAR                   IEP
165400941215     C                   CLEAR                   CFP
165500941215     C                   CLEAR                   MEP
165600941223     C                   CLEAR                   RCP
165700980312     C                   CLEAR                   RLP
165800941215     C*
165900941215     C* SE IL BLOCCO RICHIESTO E' UGUALE AL BLOCCO ATTUALE 'DOPO' CARICO DA DOPO
166000941215IF  2C     WNBCP         IFEQ      WNBAD
166100941215     C                   Z-ADD     CDD           CDP
166200020130     C                   MOVEL     FDD           FDP
166300941215     C                   Z-ADD     DDD           DDP
166400941215     C                   Z-ADD     DSD           DSP
166500941215     C                   Z-ADD     KCD           KCP
166600941215     C                   Z-ADD     KSD           KSP
166700941215     C                   MOVEL     RID           RIP
166800941215     C                   MOVEL     DAD           DAP
166900941215     C                   Z-ADD     PED           PEP
167000941215     C                   MOVEL     FAD           FAP
167100941215     C                   MOVEL     IED           IEP
167200941215     C                   Z-ADD     CFD           CFP
167300941215     C                   MOVEL     MED           MEP
167400941223     C                   MOVEL     RCD           RCP
167500980312     C                   MOVEL     RLD           RLP
167600941215     C                   Z-ADD     WULTD         WULTP
167700941215X   2C                   ELSE
167800941215     C                   MOVEL     'S'           WCARIC
167900941215     C                   Z-ADD     WNBCP         WNBC
168000941215     C                   Z-ADD     *ZEROS        WULTP             5 0
168100941215E   2C                   ENDIF
168200941215E   1C                   ENDIF
168300941215     C*
168400941215     C* SE 'DOPO' E SE N.BLOCCO RICHIESTO <> DAL BLOCCO ATTUALMENTE CARICATO
168500941215IF  1C     WPD           IFEQ      'D'
168600941215     C     WNBCD         ANDNE     WNBAD
168700941215     C*
168800941215     C* AZZERA LA SCHIERA DOPO
168900941215     C                   CLEAR                   CDD
169000020130     C                   CLEAR                   FDD
169100941215     C                   CLEAR                   DDD
169200941215     C                   CLEAR                   DSD
169300941215     C                   CLEAR                   KCD
169400941215     C                   CLEAR                   KSD
169500941215     C                   CLEAR                   RID
169600941215     C                   CLEAR                   DAD
169700941215     C                   CLEAR                   PED
169800941215     C                   CLEAR                   FAD
169900941215     C                   CLEAR                   IED
170000941215     C                   CLEAR                   CFD
170100941215     C                   CLEAR                   MED
170200941223     C                   CLEAR                   RCD
170300980312     C                   CLEAR                   RLD
170400941215     C*
170500941215     C* SE IL BLOCCO RICHIESTO E' UGUALE AL BLOCCO ATTUALE 'DOPO' CARICO DA DOPO
170600941215IF  2C     WNBCD         IFEQ      WNBAP
170700941215     C                   Z-ADD     CDP           CDD
170800020130     C                   MOVEL     FDP           FDD
170900941215     C                   Z-ADD     DDP           DDD
171000941215     C                   Z-ADD     DSP           DSD
171100941215     C                   Z-ADD     KCP           KCD
171200941215     C                   Z-ADD     KSP           KSD
171300941215     C                   MOVEL     RIP           RID
171400941215     C                   MOVEL     DAP           DAD
171500941215     C                   Z-ADD     PEP           PED
171600941215     C                   MOVEL     FAP           FAD
171700941215     C                   MOVEL     IEP           IED
171800941215     C                   Z-ADD     CFP           CFD
171900941215     C                   MOVEL     MEP           MED
172000941223     C                   MOVEL     RCP           RCD
172100980312     C                   MOVEL     RLP           RLD
172200941215     C                   Z-ADD     WULTP         WULTD
172300941215X   2C                   ELSE
172400941215     C                   MOVEL     'S'           WCARIC
172500941215     C                   Z-ADD     WNBCD         WNBC
172600941215     C                   Z-ADD     *ZEROS        WULTD             5 0
172700941215E   2C                   ENDIF
172800941215E   1C                   ENDIF
172900941215     C*
173000941215     C* SE DEVO CARICARE DAL FILE
173100941215IF  1C     WCARIC        IFEQ      'S'
173200941215     C*
173300941215     C* IMPOSTO NUMERO BLOCCO ATTUALE
173400941215IF  2C     WPD           IFEQ      'P'
173500941215     C                   Z-ADD     WNBC          WNBAP
173600941215X   2C                   ELSE
173700941215     C                   Z-ADD     WNBC          WNBAD
173800941215E   2C                   ENDIF
173900941215     C*
174000941215     C* LEGGO TESTATA COMPETENZE
174100941215     C                   Z-ADD     WNBC          ECTNBC
174200941215     C     KEYECT        SETLL     ECECT01L
174300941215     C     KEYECT        READE     ECECT01L                               99
174400941215     C*
174500941215     C* FINO A FINE FILE
174600941215DO  2C     *IN99         DOWEQ     *OFF
174700941215     C     $ERR          ANDEQ     'N'
174800941215     C*
174900941215     C* CARICO SOLO QUELLE COMPETENZE CHE NON SONO ANNULLATE, CHE AGGIORNANO IL
175000941215     C* CONTO ECONOMICO E CHE SONO VALIDE ALLA DATA SPEDIZIONE
175100941215     C                   Z-ADD     ECTCMP        DEWCMP
175200941215DO  3C     *IN99         DOWEQ     *OFF
175300941215     C     ECTCMP        ANDEQ     DEWCMP
175400941215     C     $ERR          ANDEQ     'N'
175500941215     C*
175600941215     C* NEL FILE ECECT00F LE DATE (DECOR E SCAD) SONO MEMORIZZATE IN AAAA/MM/00
175700941215IF  4C     ECTATB        IFEQ      *BLANKS
175800941215     C     ECTMCE        ANDEQ     'S'
175900941215     C*
176000941215     C* LEGGO IL DETTAGLIO DELLA COMPETENZA VALIDA
176100941215     C                   Z-ADD     WNBC          ECDNBC
176200941215     C                   Z-ADD     DEWCMP        ECDCMP
176300941215     C     KEYECD        SETLL     ECECD01L
176400941215     C     KEYECD        READE     ECECD01L                               99
176500941215     C*
176600941215     C* FINO A FINE FILE
176700941215DO  5C     *IN99         DOWEQ     *OFF
176800941215     C     $ERR          ANDEQ     'N'
176900941215     C*
177000941215     C* NEL FILE ECECD00F LE DATE (DECOR E SCAD) SONO MEMORIZZATE IN AAAA/MM/00
177100941215IF  6C     ECDATB        IFEQ      *BLANKS
177200040120     C*
177300040120     C* carica solo i conti validi alla data decorrenza/scadenza della competenz
177400040120IF 6ac                   IF        ecdddo>=ectddc and ecdddo<=ectdsc and
177500040120     c                             ecddso>=ectddc and ecddso<=ectdsc
177600941215     C*
177700941215     C* CARICO LA SCHIERA PRIMA O DOPO
177800941215IF  7C     WPD           IFEQ      'P'
177900941215     C                   ADD       1             WULTP
178000941215     C                   Z-ADD     WULTP         IP
178100941215     C                   Z-ADD     ECTCMP        CDP(IP)
178200020130     C                   MOVEL     ECTFDR        FDP(IP)
178300941215     C                   Z-ADD     ECDDDO        DDP(IP)
178400941215     C                   Z-ADD     ECDDSO        DSP(IP)
178500941215     C                   Z-ADD     ECDKCC        KCP(IP)
178600941215     C                   Z-ADD     ECDKSC        KSP(IP)
178700941215     C                   MOVEL     ECDRIC        RIP(IP)
178800941215     C                   MOVEL     ECDSEG        DAP(IP)
178900941215     C                   Z-ADD     ECDPER        PEP(IP)
179000941215     C                   MOVEL     ECDFRA        FAP(IP)
179100941215     C                   MOVEL     ECDIOE        IEP(IP)
179200941215     C                   Z-ADD     ECDCCF        CFP(IP)
179300941215     C                   Z-ADD     ECDCMP        CP
179400941215     C                   MOVEL     ECTMCE        MEP(CP)
179500941223     C                   MOVEL     ECTFCR        RCP(CP)
179600980312     C                   MOVEL     ECTRIC        RLP(CP)
179700941215X   7C                   ELSE
179800941215     C                   ADD       1             WULTD
179900941215     C                   Z-ADD     WULTD         ID
180000941215     C                   Z-ADD     ECTCMP        CDD(ID)
180100020130     C                   MOVEL     ECTFDR        FDD(ID)
180200941215     C                   Z-ADD     ECDDDO        DDD(ID)
180300941215     C                   Z-ADD     ECDDSO        DSD(ID)
180400941215     C                   Z-ADD     ECDKCC        KCD(ID)
180500941215     C                   Z-ADD     ECDKSC        KSD(ID)
180600941215     C                   MOVEL     ECDRIC        RID(ID)
180700941215     C                   MOVEL     ECDSEG        DAD(ID)
180800941215     C                   Z-ADD     ECDPER        PED(ID)
180900941215     C                   MOVEL     ECDFRA        FAD(ID)
181000941215     C                   MOVEL     ECDIOE        IED(ID)
181100941215     C                   Z-ADD     ECDCCF        CFD(ID)
181200941215     C                   Z-ADD     ECDCMP        CD
181300941215     C                   MOVEL     ECTMCE        MED(CD)
181400941223     C                   MOVEL     ECTFCR        RCD(CD)
181500980312     C                   MOVEL     ECTRIC        RLD(CD)
181600941215E   7C                   ENDIF
181700941215E   6C                   ENDIF
181800040120E  6aC                   ENDIF
181900941215     C*
182000941215     C     KEYECD        READE     ECECD01L                               99
182100941215E   5C                   ENDDO
182200941215E   4C                   ENDIF
182300941215     C*
182400941215     C     KEYECT        READE     ECECT01L                               99
182500941215E   3C                   ENDDO
182600941215     C*
182700941215E   2C                   ENDDO
182800941215E   1C                   ENDIF
182900941215     C*
183000941215     C                   ENDSR
183100941215     C*------------------------------------------------------------------------*
183200941215     C*?LETNBP - LEGGO DALLA TESTATA IMP.COMPET.   IL N.BLOCCO USATO 'PRIMA'
183300941215     C*------------------------------------------------------------------------*
183400941215     C     LETNBP        BEGSR
183500941215     C*
183600941215     C                   Z-ADD     *ZEROS        WNBCP             9 0
183700941215     C*
183800941215     C* LEGGO LE COMPETENZE FISSE PER SAPERE IL BLOCCO COMPETENZA
183900941215     C                   Z-ADD     D$PAAS        CETAAS
184000941215     C                   Z-ADD     D$PLNP        CETLNP
184100941215     C                   Z-ADD     D$PNRS        CETNRS
184200941215     C                   Z-ADD     D$PNSP        CETNSP
184300941215     C                   MOVEL     D$PTBL        CETTBL
184400950316     C                   SETOFF                                       0102
184500950316     C     KEYCET        CHAIN     ECCET30C                           99
184600941215     C*
184700941215     C* IMPORTI FISSI NON TROVATI, ERRORE
184800941215IF  1C     *IN99         IFEQ      *ON
184900941215     C                   MOVEL     MSG(4)        $MSG
185000941215     C                   EXSR      SCRERR
185100030213E   2C*                  ENDIF
185200941215X   1C                   ELSE
185300941215     C                   Z-ADD     CETNBC        WNBCP
185400941215     C                   MOVEL     'S'           WCETPR
185500941215E   1C                   ENDIF
185600941215     C*
185700941215     C                   ENDSR
185800941215     C*------------------------------------------------------------------------*
185900980130     C*?LETNBD - LEGGO DALLA TABELLA '2K' L'ULTIMO N.BLOCCO   --> N.BLOCCO DOPO
186000941215     C*------------------------------------------------------------------------*
186100941215     C     LETNBD        BEGSR
186200941215     C*
186300941215     C                   OPEN      TABEL00F
186400941215     C*
186500950404     C                   Z-ADD     CODUT         KTBUTE
186600980130     C                   MOVEL     '2K'          KTBCOD
186700941215     C                   MOVEL     *BLANKS       KTBKEY
186800941215     C     KEYTAB        CHAIN     TABEL00F                           99
186900941215IF  1C     *IN99         IFEQ      *ON
187000980130     C                   MOVEL     MSG(1)        $MSG
187100941215     C                   EXSR      SCRERR
187200941215X   1C                   ELSE
187300980130     C                   MOVEL     TBLUNI        DS2K
187400980130     C                   Z-ADD     §2KNBC        WNBCD             9 0
187500941215     C*
187600941215     C* SE IL FLAG 'N.BLOCCO GIA UTILIZZATO ANCORA = A 'NO' LO AGGIORNO'
187700980130IF  2C     §2KUBC        IFEQ      'N'
187800991117IF  3C     D$GFSI        IFNE      'S'
187900980130     C                   MOVEL     'S'           §2KUBC
188000980130     C                   MOVEL     DS2K          TBLUNI
188100941215     C                   UPDATE    TABEL
188200991117E   3C                   ENDIF
188300991117E   2C                   ENDIF
188400941215E   1C                   ENDIF
188500941215     C*
188600941215     C                   CLOSE     TABEL00F
188700941215     C*
188800941215     C                   ENDSR
188900941215     C*------------------------------------------------------------------------*
189000941215     C*?CARCOP - CARICA GLI IMPORTI DELLE COMPETENZE -PRIMA-
189100941215     C*------------------------------------------------------------------------*
189200941215     C     CARCOP        BEGSR
189300941215     C*--------------------
189400941215     C* COMPETENZE FISSE
189500941215     C*--------------------
189600941215     C                   Z-ADD     CETPAR        IMP(1)
189700020129     C                   ADD       CETPAR        WPIRP
189800941215     C                   Z-ADD     CETPAM        IMP(2)
189900020129     C                   ADD       CETPAM        WPIRP
190000941215     C                   Z-ADD     CETPAU        IMP(3)
190100020129     C                   ADD       CETPAU        WPIRP
190200941215     C                   Z-ADD     CETPAV        IMP(4)
190300020129     C                   ADD       CETPAV        WPIRP
190400941215     C                   Z-ADD     CETARD        IMP(5)
190500020129     C                   ADD       CETARD        WPIRP
190600941215     C                   Z-ADD     CETARM        IMP(6)
190700020129     C                   ADD       CETARM        WPIRP
190800941215     C                   Z-ADD     CETARU        IMP(7)
190900020129     C                   ADD       CETARU        WPIRP
191000941215     C                   Z-ADD     CETARV        IMP(8)
191100020129     C                   ADD       CETARV        WPIRP
191200950315     C                   Z-ADD     CETMAF        IMP(9)
191300020129     C                   ADD       CETMAF        WPIRP
191400950315     C                   Z-ADD     CETMDE        IMP(10)
191500020129     C                   ADD       CETMDE        WPIRP
191600941215     C                   Z-ADD     CETTRL        IMP(13)
191700020129     C                   ADD       CETTRL        WPIRP
191800941223     C                   Z-ADD     CETTRL        WPCTL
191900950315     C                   Z-ADD     CETTAF        IMP(14)
192000020129     C                   ADD       CETTAF        WPIRP
192100950315     C                   Z-ADD     CETTDE        IMP(15)
192200020129     C                   ADD       CETTDE        WPIRP
192300941215     C                   Z-ADD     CETPAG        IMP(29)
192400020129     C                   ADD       CETPAG        WPIRP
192500941215     C                   Z-ADD     CETSEA        IMP(26)
192600020129     C                   ADD       CETSEA        WPIRP
192700941215     C                   Z-ADD     CETSEC        IMP(25)
192800020129     C                   ADD       CETSEC        WPIRP
192900941215     C                   Z-ADD     CETRCV        IMP(27)
193000020129     C                   ADD       CETRCV        WPIRP
193100010827     C                   Z-ADD     CETSES        IMP(33)
193200020129     C                   ADD       CETSES        WPIRP
193300941215     C*--------------------
193400941215     C* COMPETENZE VARIABILI
193500941215     C*--------------------
193600941215     C* LEGGO LE COMPETENZE VARIABILI SE CI SONO
193700020805IF  1C     CETDET        IFEQ      'S'
193800941215     C                   Z-ADD     D$PAAS        CEDAAS
193900941215     C                   Z-ADD     D$PLNP        CEDLNP
194000941215     C                   Z-ADD     D$PNRS        CEDNRS
194100941215     C                   Z-ADD     D$PNSP        CEDNSP
194200941215     C                   MOVEL     D$PTBL        CEDTBL
194300950316     C                   SETOFF                                       0304
194400950316     C     KEYCEP        SETLL     ECCED30C
194500950316     C     KEYCEP        READE     ECCED30C                               99
194600941215     C*
194700941215     C* IMPORTI COMPETENZE VARIABILI NON TROVATI (ANCHE SE PREVISTI), ERRORE
194800941215IF  2C     *IN99         IFEQ      *ON
194900020805     C                   MOVEL     MSG(5)        $MSG
195000020805     C                   EXSR      SCRERR
195100941215X   2C                   ELSE
195200941215     C*
195300941215     C* IMPORTI COMPETENZE VARIABILI TROVATI, CARICO
195400941215DO  3C     *IN99         DOWEQ     *OFF
195500941215     C                   Z-ADD     CEDCMP        CP
195600941215     C                   Z-ADD     CEDIMP        IMP(CP)
195700950124     C                   ADD       CEDIMP        WPIRP
195800941223     C     CP            IFEQ      11
195900941223     C                   Z-ADD     IMP(CP)       WPCT1
196000941223     C                   ENDIF
196100941223     C     CP            IFEQ      12
196200941223     C                   Z-ADD     IMP(CP)       WPCT2
196300941223     C                   ENDIF
196400040618     c* aggiunte competenze transiti 3/4/5 + quelle dimenticate
196500040618     c* per effetto dello sbinamento delle competenze transiti 1/2
196600040617     C     CP            IFEQ      205
196700040617     C                   add       IMP(CP)       WPCT2
196800040617     C                   ENDIF
196900040618     C     CP            IFEQ      204
197000040618     C                   add       IMP(CP)       WPCT1
197100040618     C                   ENDIF
197200040519     C     CP            IFEQ      53
197300040519     C                   Z-ADD     IMP(CP)       WPCT3
197400040519     C                   ENDIF
197500040617     C     CP            IFEQ      206
197600040617     C                   add       IMP(CP)       WPCT3
197700040617     C                   ENDIF
197800040519     C     CP            IFEQ      54
197900040519     C                   Z-ADD     IMP(CP)       WPCT4
198000040519     C                   ENDIF
198100040617     C     CP            IFEQ      207
198200040617     C                   add       IMP(CP)       WPCT4
198300040617     C                   ENDIF
198400040519     C     CP            IFEQ      55
198500040519     C                   Z-ADD     IMP(CP)       WPCT5
198600040519     C                   ENDIF
198700040617     C     CP            IFEQ      208
198800040617     C                   add       IMP(CP)       WPCT5
198900040617     C                   ENDIF
199000040618     c* obsoleta
199100040618     C*    CP            IFEQ      32
199200040618     C*                  Z-ADD     IMP(CP)       WPCTL
199300040618     C*                  ENDIF
199400950316     C                   SETOFF                                       0304
199500950316     C     KEYCEP        READE     ECCED30C                               99
199600030114E   3C                   ENDDO
199700970217     C* TOGLIE I TRANSITI AI SALDI CLIENTE
199800970217     C                   SUB       WPCT1         WPIRP
199900970217     C                   SUB       WPCT2         WPIRP
200000040519     C                   SUB       WPCT3         WPIRP
200100040519     C                   SUB       WPCT4         WPIRP
200200040519     C                   SUB       WPCT5         WPIRP
200300941215E   2C                   ENDIF
200400020805E   1C                   ENDIF
200500941215     C*
200600941223     C* TROVO COMP.DELTA PRESUNTO (SOMMA DI TUTTE LE COMPETENZE)
200700941223     C                   Z-ADD     900           CP
200800950124     C                   Z-ADD     WPIRP         IMP(CP)
200900950412     C*
201000950412     C* SE SPEDIZ. OLTRE 35 QLI AZZERO LA COMP.DELTA PRESUNTO ANCHE NEL -PRIMA-
201100950412     C* (NEL -DOPO- VIENE AZZERATA DALLA TARIFFA DELLA VOCE 900)
201200040429     C*    D$PAAS        IFLE      1998
201300040429     C*                  Z-ADD     3500          WKGLIM
201400040429     C*                  ELSE
201500040429     C*                  Z-ADD     5000          WKGLIM
201600040429     C*                  ENDIF
201700040429     C*    D$PAAS        IFLT      2000
201800040429     C*    D$PPKF        IFGT      WKGLIM
201900040429     C*                  Z-ADD     *ZEROS        WPIRP
202000040429     C*                  Z-ADD     900           CP
202100040429     C*                  Z-ADD     *ZEROS        IMP(CP)
202200040429     C*                  ENDIF
202300040429     C*                  ENDIF
202400941215     C*
202500941215     C                   ENDSR
202600030114     C*------------------------------------------------------------------------*
202700030509     C*?LETCEV - LEGGE LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
202800030114     C*------------------------------------------------------------------------*
202900030114     C     LETCEV        BEGSR
203000030114     C*
203100030114     C                   Z-ADD     D$PAAS        CEVAAS
203200030114     C                   Z-ADD     D$PLNP        CEVLNP
203300030114     C                   Z-ADD     D$PNRS        CEVNRS
203400030114     C                   Z-ADD     D$PNSP        CEVNSP
203500030114     C                   MOVEL     D$PTBL        CEVTBL
203600030114     C                   SETOFF                                       0506
203700030114     C     KEYCEV        CHAIN     ECCEV30C                           99
203800030114     C*
203900030114     C* RECORD NON TROVATO, ERRORE
204000030114IF  1C     *IN99         IFEQ      *ON
204100030114     C                   MOVEL     MSG(24)       $MSG
204200030114     C                   EXSR      SCRERR
204300030213E   2C*                  ENDIF
204400030114X   1C                   ELSE
204500030114     C                   MOVEL     'S'           WCEVPR
204600030114E   1C                   ENDIF
204700030114     C*
204800030114     C                   ENDSR
204900030114     C*------------------------------------------------------------------------*
205000030509     C*?CARCEV - CARICA LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
205100030114     C*------------------------------------------------------------------------*
205200030114     C     CARCEV        BEGSR
205300030114     C*
205400030512     C* IMPOSTA LE VARIABILI DI LAVORO
205500030512     c                   MOVEL     'N'           WC2VP             1
205600030512     C*
205700030507     C     CEVC01        IFGT      *ZEROS
205800030507     C                   Z-ADD     CEVI01        IMPV(CEVC01)
205900030507     C                   MOVEL     'S'           COPPV(CEVC01)
206000030507     C                   ENDIF
206100030507     C     CEVC02        IFGT      *ZEROS
206200030507     C                   Z-ADD     CEVI02        IMPV(CEVC02)
206300030507     C                   MOVEL     'S'           COPPV(CEVC02)
206400030507     C                   ENDIF
206500030507     C     CEVC03        IFGT      *ZEROS
206600030507     C                   Z-ADD     CEVI03        IMPV(CEVC03)
206700030507     C                   MOVEL     'S'           COPPV(CEVC03)
206800030507     C                   ENDIF
206900030124     C     CEVC04        IFGT      *ZEROS
207000030507     C                   Z-ADD     CEVI04        IMPV(CEVC04)
207100030507     C                   MOVEL     'S'           COPPV(CEVC04)
207200030507     C                   ENDIF
207300030124     C     CEVC05        IFGT      *ZEROS
207400030115     C                   Z-ADD     CEVI05        IMPV(CEVC05)
207500030507     C                   MOVEL     'S'           COPPV(CEVC05)
207600030507     C                   ENDIF
207700030124     C     CEVC06        IFGT      *ZEROS
207800030507     C                   Z-ADD     CEVI06        IMPV(CEVC06)
207900030507     C                   MOVEL     'S'           COPPV(CEVC06)
208000030507     C                   ENDIF
208100030507     C     CEVC07        IFGT      *ZEROS
208200030507     C                   Z-ADD     CEVI07        IMPV(CEVC07)
208300030507     C                   MOVEL     'S'           COPPV(CEVC07)
208400030507     C                   ENDIF
208500030507     C     CEVC08        IFGT      *ZEROS
208600030507     C                   Z-ADD     CEVI08        IMPV(CEVC08)
208700030214     C                   MOVEL     'S'           COPPV(CEVC08)
208800030124     C                   ENDIF
208900030124     C     CEVC09        IFGT      *ZEROS
209000030115     C                   Z-ADD     CEVI09        IMPV(CEVC09)
209100030214     C                   MOVEL     'S'           COPPV(CEVC09)
209200030124     C                   ENDIF
209300030124     C     CEVC10        IFGT      *ZEROS
209400030115     C                   Z-ADD     CEVI10        IMPV(CEVC10)
209500030214     C                   MOVEL     'S'           COPPV(CEVC10)
209600030124     C                   ENDIF
209700030124     C     CEVC11        IFGT      *ZEROS
209800030115     C                   Z-ADD     CEVI11        IMPV(CEVC11)
209900030214     C                   MOVEL     'S'           COPPV(CEVC11)
210000030124     C                   ENDIF
210100030124     C     CEVC12        IFGT      *ZEROS
210200030115     C                   Z-ADD     CEVI12        IMPV(CEVC12)
210300030214     C                   MOVEL     'S'           COPPV(CEVC12)
210400030124     C                   ENDIF
210500030124     C     CEVC13        IFGT      *ZEROS
210600030115     C                   Z-ADD     CEVI13        IMPV(CEVC13)
210700030214     C                   MOVEL     'S'           COPPV(CEVC13)
210800030124     C                   ENDIF
210900030124     C     CEVC14        IFGT      *ZEROS
211000030115     C                   Z-ADD     CEVI14        IMPV(CEVC14)
211100030214     C                   MOVEL     'S'           COPPV(CEVC14)
211200030124     C                   ENDIF
211300030124     C     CEVC15        IFGT      *ZEROS
211400030115     C                   Z-ADD     CEVI15        IMPV(CEVC15)
211500030214     C                   MOVEL     'S'           COPPV(CEVC15)
211600030124     C                   ENDIF
211700030124     C     CEVC16        IFGT      *ZEROS
211800030115     C                   Z-ADD     CEVI16        IMPV(CEVC16)
211900030214     C                   MOVEL     'S'           COPPV(CEVC16)
212000030124     C                   ENDIF
212100030124     C     CEVC17        IFGT      *ZEROS
212200030115     C                   Z-ADD     CEVI17        IMPV(CEVC17)
212300030214     C                   MOVEL     'S'           COPPV(CEVC17)
212400030124     C                   ENDIF
212500030124     C     CEVC18        IFGT      *ZEROS
212600030115     C                   Z-ADD     CEVI18        IMPV(CEVC18)
212700030214     C                   MOVEL     'S'           COPPV(CEVC18)
212800030124     C                   ENDIF
212900030124     C     CEVC19        IFGT      *ZEROS
213000030115     C                   Z-ADD     CEVI19        IMPV(CEVC19)
213100030214     C                   MOVEL     'S'           COPPV(CEVC19)
213200030124     C                   ENDIF
213300030124     C     CEVC20        IFGT      *ZEROS
213400030115     C                   Z-ADD     CEVI20        IMPV(CEVC20)
213500030214     C                   MOVEL     'S'           COPPV(CEVC20)
213600030124     C                   ENDIF
213700030124     C     CEVC21        IFGT      *ZEROS
213800030115     C                   Z-ADD     CEVI21        IMPV(CEVC21)
213900030214     C                   MOVEL     'S'           COPPV(CEVC21)
214000030124     C                   ENDIF
214100030124     C     CEVC22        IFGT      *ZEROS
214200030115     C                   Z-ADD     CEVI22        IMPV(CEVC22)
214300030214     C                   MOVEL     'S'           COPPV(CEVC22)
214400030124     C                   ENDIF
214500030124     C     CEVC23        IFGT      *ZEROS
214600030124     C                   Z-ADD     CEVI23        IMPV(CEVC23)
214700030214     C                   MOVEL     'S'           COPPV(CEVC23)
214800030124     C                   ENDIF
214900030124     C     CEVC24        IFGT      *ZEROS
215000030115     C                   Z-ADD     CEVI24        IMPV(CEVC24)
215100030214     C                   MOVEL     'S'           COPPV(CEVC24)
215200030124     C                   ENDIF
215300030124     C     CEVC25        IFGT      *ZEROS
215400030115     C                   Z-ADD     CEVI25        IMPV(CEVC25)
215500030214     C                   MOVEL     'S'           COPPV(CEVC25)
215600030124     C                   ENDIF
215700030124     C     CEVC26        IFGT      *ZEROS
215800030115     C                   Z-ADD     CEVI26        IMPV(CEVC26)
215900030214     C                   MOVEL     'S'           COPPV(CEVC26)
216000030124     C                   ENDIF
216100030124     C     CEVC27        IFGT      *ZEROS
216200030115     C                   Z-ADD     CEVI27        IMPV(CEVC27)
216300030214     C                   MOVEL     'S'           COPPV(CEVC27)
216400030124     C                   ENDIF
216500030124     C     CEVC28        IFGT      *ZEROS
216600030115     C                   Z-ADD     CEVI28        IMPV(CEVC28)
216700030214     C                   MOVEL     'S'           COPPV(CEVC28)
216800030124     C                   ENDIF
216900030124     C     CEVC29        IFGT      *ZEROS
217000030115     C                   Z-ADD     CEVI29        IMPV(CEVC29)
217100030214     C                   MOVEL     'S'           COPPV(CEVC29)
217200030124     C                   ENDIF
217300030124     C     CEVC30        IFGT      *ZEROS
217400030115     C                   Z-ADD     CEVI30        IMPV(CEVC30)
217500030214     C                   MOVEL     'S'           COPPV(CEVC30)
217600030124     C                   ENDIF
217700030124     C     CEVC31        IFGT      *ZEROS
217800030115     C                   Z-ADD     CEVI31        IMPV(CEVC31)
217900030214     C                   MOVEL     'S'           COPPV(CEVC31)
218000030124     C                   ENDIF
218100030124     C     CEVC32        IFGT      *ZEROS
218200030115     C                   Z-ADD     CEVI32        IMPV(CEVC32)
218300030214     C                   MOVEL     'S'           COPPV(CEVC32)
218400030124     C                   ENDIF
218500030124     C     CEVC33        IFGT      *ZEROS
218600030115     C                   Z-ADD     CEVI33        IMPV(CEVC33)
218700030214     C                   MOVEL     'S'           COPPV(CEVC33)
218800030124     C                   ENDIF
218900030124     C     CEVC34        IFGT      *ZEROS
219000030115     C                   Z-ADD     CEVI34        IMPV(CEVC34)
219100030214     C                   MOVEL     'S'           COPPV(CEVC34)
219200030124     C                   ENDIF
219300030124     C     CEVC35        IFGT      *ZEROS
219400030115     C                   Z-ADD     CEVI35        IMPV(CEVC35)
219500030214     C                   MOVEL     'S'           COPPV(CEVC35)
219600030124     C                   ENDIF
219700030124     C     CEVC36        IFGT      *ZEROS
219800030115     C                   Z-ADD     CEVI36        IMPV(CEVC36)
219900030214     C                   MOVEL     'S'           COPPV(CEVC36)
220000030124     C                   ENDIF
220100030124     C     CEVC37        IFGT      *ZEROS
220200030115     C                   Z-ADD     CEVI37        IMPV(CEVC37)
220300030214     C                   MOVEL     'S'           COPPV(CEVC37)
220400030124     C                   ENDIF
220500030124     C     CEVC38        IFGT      *ZEROS
220600030115     C                   Z-ADD     CEVI38        IMPV(CEVC38)
220700030214     C                   MOVEL     'S'           COPPV(CEVC38)
220800030124     C                   ENDIF
220900030124     C     CEVC39        IFGT      *ZEROS
221000030115     C                   Z-ADD     CEVI39        IMPV(CEVC39)
221100030214     C                   MOVEL     'S'           COPPV(CEVC39)
221200030124     C                   ENDIF
221300030124     C     CEVC40        IFGT      *ZEROS
221400030115     C                   Z-ADD     CEVI40        IMPV(CEVC40)
221500030214     C                   MOVEL     'S'           COPPV(CEVC40)
221600030124     C                   ENDIF
221700030124     C     CEVC41        IFGT      *ZEROS
221800030115     C                   Z-ADD     CEVI41        IMPV(CEVC41)
221900030214     C                   MOVEL     'S'           COPPV(CEVC41)
222000030124     C                   ENDIF
222100030124     C     CEVC42        IFGT      *ZEROS
222200030115     C                   Z-ADD     CEVI42        IMPV(CEVC42)
222300030214     C                   MOVEL     'S'           COPPV(CEVC42)
222400030124     C                   ENDIF
222500030124     C     CEVC43        IFGT      *ZEROS
222600030115     C                   Z-ADD     CEVI43        IMPV(CEVC43)
222700030214     C                   MOVEL     'S'           COPPV(CEVC43)
222800030124     C                   ENDIF
222900030124     C     CEVC44        IFGT      *ZEROS
223000030115     C                   Z-ADD     CEVI44        IMPV(CEVC44)
223100030214     C                   MOVEL     'S'           COPPV(CEVC44)
223200030124     C                   ENDIF
223300030124     C     CEVC45        IFGT      *ZEROS
223400030115     C                   Z-ADD     CEVI45        IMPV(CEVC45)
223500030214     C                   MOVEL     'S'           COPPV(CEVC45)
223600030124     C                   ENDIF
223700030124     C     CEVC46        IFGT      *ZEROS
223800030115     C                   Z-ADD     CEVI46        IMPV(CEVC46)
223900030214     C                   MOVEL     'S'           COPPV(CEVC46)
224000030124     C                   ENDIF
224100030124     C     CEVC47        IFGT      *ZEROS
224200030115     C                   Z-ADD     CEVI47        IMPV(CEVC47)
224300030214     C                   MOVEL     'S'           COPPV(CEVC47)
224400030124     C                   ENDIF
224500030124     C     CEVC48        IFGT      *ZEROS
224600030115     C                   Z-ADD     CEVI48        IMPV(CEVC48)
224700030214     C                   MOVEL     'S'           COPPV(CEVC48)
224800030124     C                   ENDIF
224900030124     C     CEVC49        IFGT      *ZEROS
225000030115     C                   Z-ADD     CEVI49        IMPV(CEVC49)
225100030214     C                   MOVEL     'S'           COPPV(CEVC49)
225200030124     C                   ENDIF
225300030124     C     CEVC50        IFGT      *ZEROS
225400030124     C                   Z-ADD     CEVI50        IMPV(CEVC50)
225500030214     C                   MOVEL     'S'           COPPV(CEVC50)
225600030124     C                   ENDIF
225700030506     c*
225800030506     c* LEGGE E CARICA (se ci sono) LE VOCI COOP ESTENSIONE
225900030509IF  1C     CEVC50        IFGT      *ZEROS
226000030507     C                   Z-ADD     D$PAAS        C2VAAS
226100030507     C                   Z-ADD     D$PLNP        C2VLNP
226200030507     C                   Z-ADD     D$PNRS        C2VNRS
226300030507     C                   Z-ADD     D$PNSP        C2VNSP
226400030507     C                   MOVEL     D$PTBL        C2VTBL
226500030507     C                   SETOFF                                       0708
226600030507     C     KEYC2V        CHAIN     ECC2V30C                           99
226700030509IF  2C     *IN99         IFEQ      *OFF
226800030512     c                   MOVEL     'S'           WC2VP
226900030509     C     C2VC51        IFGT      *ZEROS
227000030509     C                   Z-ADD     C2VI51        IMPV(C2VC51)
227100030509     C                   MOVEL     'S'           COPPV(C2VC51)
227200030509     C                   ENDIF
227300030509     C     C2VC52        IFGT      *ZEROS
227400030509     C                   Z-ADD     C2VI52        IMPV(C2VC52)
227500030509     C                   MOVEL     'S'           COPPV(C2VC52)
227600030509     C                   ENDIF
227700030509     C     C2VC53        IFGT      *ZEROS
227800030509     C                   Z-ADD     C2VI53        IMPV(C2VC53)
227900030509     C                   MOVEL     'S'           COPPV(C2VC53)
228000030509     C                   ENDIF
228100030509     C     C2VC54        IFGT      *ZEROS
228200030509     C                   Z-ADD     C2VI54        IMPV(C2VC54)
228300030509     C                   MOVEL     'S'           COPPV(C2VC54)
228400030509     C                   ENDIF
228500030509     C     C2VC55        IFGT      *ZEROS
228600030509     C                   Z-ADD     C2VI55        IMPV(C2VC55)
228700030509     C                   MOVEL     'S'           COPPV(C2VC55)
228800030509     C                   ENDIF
228900030509     C     C2VC56        IFGT      *ZEROS
229000030509     C                   Z-ADD     C2VI56        IMPV(C2VC56)
229100030509     C                   MOVEL     'S'           COPPV(C2VC56)
229200030509     C                   ENDIF
229300030509     C     C2VC57        IFGT      *ZEROS
229400030509     C                   Z-ADD     C2VI57        IMPV(C2VC57)
229500030509     C                   MOVEL     'S'           COPPV(C2VC57)
229600030509     C                   ENDIF
229700030509     C     C2VC58        IFGT      *ZEROS
229800030509     C                   Z-ADD     C2VI58        IMPV(C2VC58)
229900030509     C                   MOVEL     'S'           COPPV(C2VC58)
230000030509     C                   ENDIF
230100030509     C     C2VC59        IFGT      *ZEROS
230200030509     C                   Z-ADD     C2VI59        IMPV(C2VC59)
230300030509     C                   MOVEL     'S'           COPPV(C2VC59)
230400030509     C                   ENDIF
230500030509     C     C2VC60        IFGT      *ZEROS
230600030509     C                   Z-ADD     C2VI60        IMPV(C2VC60)
230700030509     C                   MOVEL     'S'           COPPV(C2VC60)
230800030509     C                   ENDIF
230900030509     C     C2VC61        IFGT      *ZEROS
231000030509     C                   Z-ADD     C2VI61        IMPV(C2VC61)
231100030509     C                   MOVEL     'S'           COPPV(C2VC61)
231200030509     C                   ENDIF
231300030509     C     C2VC62        IFGT      *ZEROS
231400030509     C                   Z-ADD     C2VI62        IMPV(C2VC62)
231500030509     C                   MOVEL     'S'           COPPV(C2VC62)
231600030509     C                   ENDIF
231700030509     C     C2VC63        IFGT      *ZEROS
231800030509     C                   Z-ADD     C2VI63        IMPV(C2VC63)
231900030509     C                   MOVEL     'S'           COPPV(C2VC63)
232000030509     C                   ENDIF
232100030509     C     C2VC64        IFGT      *ZEROS
232200030509     C                   Z-ADD     C2VI64        IMPV(C2VC64)
232300030509     C                   MOVEL     'S'           COPPV(C2VC64)
232400030509     C                   ENDIF
232500030509     C     C2VC65        IFGT      *ZEROS
232600030509     C                   Z-ADD     C2VI65        IMPV(C2VC65)
232700030509     C                   MOVEL     'S'           COPPV(C2VC65)
232800030509     C                   ENDIF
232900030509     C     C2VC66        IFGT      *ZEROS
233000030509     C                   Z-ADD     C2VI66        IMPV(C2VC66)
233100030509     C                   MOVEL     'S'           COPPV(C2VC66)
233200030509     C                   ENDIF
233300030509     C     C2VC67        IFGT      *ZEROS
233400030509     C                   Z-ADD     C2VI67        IMPV(C2VC67)
233500030509     C                   MOVEL     'S'           COPPV(C2VC67)
233600030509     C                   ENDIF
233700030509     C     C2VC68        IFGT      *ZEROS
233800030509     C                   Z-ADD     C2VI68        IMPV(C2VC68)
233900030509     C                   MOVEL     'S'           COPPV(C2VC68)
234000030509     C                   ENDIF
234100030509     C     C2VC69        IFGT      *ZEROS
234200030509     C                   Z-ADD     C2VI69        IMPV(C2VC69)
234300030509     C                   MOVEL     'S'           COPPV(C2VC69)
234400030509     C                   ENDIF
234500030509     C     C2VC70        IFGT      *ZEROS
234600030509     C                   Z-ADD     C2VI70        IMPV(C2VC70)
234700030509     C                   MOVEL     'S'           COPPV(C2VC70)
234800030509     C                   ENDIF
234900030509     C     C2VC71        IFGT      *ZEROS
235000030509     C                   Z-ADD     C2VI71        IMPV(C2VC71)
235100030509     C                   MOVEL     'S'           COPPV(C2VC71)
235200030509     C                   ENDIF
235300030509     C     C2VC72        IFGT      *ZEROS
235400030509     C                   Z-ADD     C2VI72        IMPV(C2VC72)
235500030509     C                   MOVEL     'S'           COPPV(C2VC72)
235600030509     C                   ENDIF
235700030509     C     C2VC73        IFGT      *ZEROS
235800030509     C                   Z-ADD     C2VI73        IMPV(C2VC73)
235900030509     C                   MOVEL     'S'           COPPV(C2VC73)
236000030509     C                   ENDIF
236100030509     C     C2VC74        IFGT      *ZEROS
236200030509     C                   Z-ADD     C2VI74        IMPV(C2VC74)
236300030509     C                   MOVEL     'S'           COPPV(C2VC74)
236400030509     C                   ENDIF
236500030509     C     C2VC75        IFGT      *ZEROS
236600030509     C                   Z-ADD     C2VI75        IMPV(C2VC75)
236700030509     C                   MOVEL     'S'           COPPV(C2VC75)
236800030509     C                   ENDIF
236900030509E   2C                   ENDIF
237000030509E   1C                   ENDIF
237100030509     C*
237200030509     C* CARICA I DATI DELLE VOCI IN DECORRENZA ALLA DATA BOLLA
237300030507     c                   XFOOT     IMPV          N213
237400030507IF  1C                   IF        N213 > *ZEROS
237500030507     C                   MOVEL     'S'           WCOPREG
237600030507     C                   EXSR      CHKVOC
237700030507     C                   Z-ADD     VCMP          CMPPV
237800030507     C                   MOVEL     VCOP          COPPV
237900030507     C                   MOVEL     VDAT          DATPV
238000030507     C                   MOVEL     VAID          AIDPV
238100030507E   1C                   ENDIF
238200030507     C*
238300030507     C                   ENDSR
238400030217     C*------------------------------------------------------------------------*
238500030217     C*? CHKVOC - CARICA I DATI DELLE VOCI IN DECORRENZA ALLA DATA BOLLA
238600030217     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
238700030217     C*            OUTPUT:- DATI VOCI IN DECORENZA ALLA DATA
238800030217     C*------------------------------------------------------------------------*
238900030217     C     CHKVOC        BEGSR
239000030217     C*
239100030217     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
239200030217DO  1C     1             DO        999           V
239300030217IF  2C     COPPV(V)      IFEQ      'S'
239400030217IF  3C     WPDSP8        IFLT      VDDV(V)
239500030217     C     WPDSP8        ORGT      VDSV(V)
239600030217     C                   Z-ADD     *ZEROS        VCMP(V)
239700030217     C                   Z-ADD     *ZEROS        VDDV(V)
239800030217     C                   Z-ADD     *ZEROS        VDSV(V)
239900030217     C                   MOVEL     *BLANKS       VCOP(V)
240000030217     C                   MOVEL     *BLANKS       VDAT(V)
240100030217     C                   MOVEL     *BLANKS       VAID(V)
240200030217     C                   Z-ADD     V             EVTVOC
240300030217     C                   Z-ADD     *ZEROS        EVTPRG
240400030217     C     KEYEVT        SETLL     ECEVT01L                           99
240500030217     C  N99              READ      ECEVT01L                               99
240600030217IF  4C     *IN99         IFEQ      *ON
240700030217     C     EVTVOC        ORNE      V
240800030217     C                   MOVEL     MSG(29)       DSMSG
240900030217     C                   MOVE      V             DSVOCE
241000030217     C                   MOVEL     DSMSG         $MSG
241100030217     C                   EXSR      SCRERR
241200030217E   4C                   ENDIF
241300030217IF  4C     *IN99         DOWEQ     *OFF
241400030217     C     EVTVOC        ANDEQ     V
241500030217IF  5C     EVTATB        IFEQ      *BLANKS
241600030217     C     WPDSP8        ANDGE     EVTDDV
241700030217     C     WPDSP8        ANDLE     EVTDSV
241800030217     C                   Z-ADD     EVTCMP        VCMP(V)
241900030217     C                   Z-ADD     EVTDDV        VDDV(V)
242000030217     C                   Z-ADD     EVTDSV        VDSV(V)
242100030217     C                   MOVEL     EVTCOP        VCOP(V)
242200030217     C                   MOVEL     EVTDAT        VDAT(V)
242300030217     C                   MOVEL     EVTAID        VAID(V)
242400030217     C                   LEAVE
242500030217E   5C                   ENDIF
242600030217     C                   READ      ECEVT01L                               99
242700030217E   4C                   ENDDO
242800030217E   3C                   ENDIF
242900030217     C*
243000030217E   2C                   ENDIF
243100030217E   1C                   ENDDO
243200030217     C*
243300030217     C                   ENDSR
243400980313     C*------------------------------------------------------------------------*
243500980313     C*?CARFIM - CARICA I METRICUBI X RIFATTURAZ. ITL -PRIMA-
243600980313     C*------------------------------------------------------------------------*
243700980313     C     CARFIM        BEGSR
243800980313     C*
243900980313     C                   Z-ADD     D$PAAS        FIMAAS
244000980313     C                   Z-ADD     D$PLNP        FIMLNP
244100980313     C                   Z-ADD     D$PNRS        FIMNRS
244200980313     C                   Z-ADD     D$PNSP        FIMNSP
244300980313     C                   MOVEL     D$PTBL        FIMTBL
244400980325     C     KEYFM1        SETLL     ECFIM30C
244500980325     C     KEYFM1        READE(N)  ECFIM30C                               99
244600980313     C     *IN99         DOWEQ     *OFF
244700980313     C                   ADD       FIMSMC        WPSMCT
244800980313     C     FIMDDO        IFNE      *ZEROS
244900980313     C                   ADD       FIMSMC        WPSMC
245000980313     C                   ENDIF
245100980325     C     KEYFM1        READE(N)  ECFIM30C                               99
245200980313     C                   ENDDO
245300980313     C*
245400980313     C                   ENDSR
245500980313     C*------------------------------------------------------------------------*
245600980313     C*?CARFIC - CARICA LE COMPETENZE X RIFATTURAZ. BAR/SDI -PRIMA-
245700980313     C*------------------------------------------------------------------------*
245800011127     C*---       CARFIC    BEGSR
245900011127     C*---
246000011127     C*---                 Z-ADDD$PAAS    FICAAS           ANNO SPEDIZIONE
246100011127     C*---                 Z-ADDD$PLNP    FICLNP           LINEA DI PARTENZA
246200011127     C*---                 Z-ADDD$PNRS    FICNRS           NUMERO SERIE
246300011127     C*---                 Z-ADDD$PNSP    FICNSP           NUMERO SPEDIZIONE
246400011127     C*---                 MOVELD$PTBL    FICTBL           TIPO BOLLA
246500011127     C*---       KEYFC1    SETLLECFIC30C
246600011127     C*---       KEYFC1    READEECFIC30C            N    99
246700011127     C*---       *IN99     DOWEQ*OFF
246800011127     C*---                 ADD  FICSCO    WPSCOT           *COMP.RIF.BAR/SDI TOT
246900011127     C*---       FICDDO    IFNE *ZEROS
247000011127     C*---                 ADD  FICSCO    WPSCO            *COMP.RIFATT.BAR/SDI
247100011127     C*---                 ENDIF
247200011127     C*---       KEYFC1    READEECFIC30C            N    99
247300011127     C*---                 ENDDO
247400011127     C*---
247500011127     C*---                 ENDSR
247600941223     C********************************************************************
247700941223     C*   GESCOD - CALCOLA LE COMPETENZE BOLLA -DOPO-
247800941223     C********************************************************************
247900941223     C     GESCOD        BEGSR
248000980317     C*
248100980317     C* DETERMINA SE BOLLA BARTOLINI O SDI
248200980924     C     D$OKSC        IFNE      *ZEROS
248300980924     C     D$DDBO        ANDEQ     'S'
248400980924     C     D$DFIG        ANDEQ     'S'
248500980924     C                   MOVEL     D$OKSC        WLNKSC            3 0
248600980924     C                   ELSE
248700980924     C                   MOVEL     D$DKSC        WLNKSC
248800980924     C                   ENDIF
248900980710     C                   Z-ADD     WLNKSC        F
249000980710     C********** D$DSCL    IFEQ 'S'
249100020709     C*--- DIT(F)        IFEQ      'SDI'
249200020709     C*---               MOVEL     'SDI'         WDSCL
249300020709     C*---               ELSE
249400020709     C*---               MOVEL     'BAR'         WDSCL
249500020709     C*---               ENDIF
249600941223     C*
249700941223     C* SE NON SONO DA CALCOLARE -DOPO- NON LANCIO PGM
249800941223     C     WFCD          IFEQ      'S'
249900941223     C*
250000941223     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
250100941223     C                   CALL      'TRECD8R'                            98
250200941223     C                   PARM                    DS$ECO
250300941223     C*
250400941223     C* SE RITORNA ERRORE ESCO PGM
250500941223     C     D$GERR        IFNE      *BLANKS
250600941223     C                   MOVEL     'S'           $ERR
250700941223     C                   ENDIF
250800941223     C*
250900941223     C     D$GERR        IFEQ      *BLANKS
251000030114     c*
251100030114     C* COMPETENZE
251200941223     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
251300950125     C                   MOVEA     D$DCCD        CAL
251400950125     C                   MOVEA     D$DICD        IAL
251500030505     C     1             DO        75            I
251600941223     C                   MOVE      CAL(I)        DSWCA
251700941223     C                   Z-ADD     DSWCN         CD
251800941223     C     CD            IFNE      *ZEROS
251900941223     C                   MOVE      IAL(I)        DSWIA
252000941223     C                   Z-ADD     DSWIN         IMD(CD)
252100941223     C     CD            IFEQ      900
252200941223     C                   Z-ADD     IMD(CD)       WDIRP
252300941223     C                   ENDIF
252400941223     C     CD            IFEQ      11
252500941223     C                   Z-ADD     IMD(CD)       WDCT1
252600941223     C                   ENDIF
252700941223     C     CD            IFEQ      12
252800941223     C                   Z-ADD     IMD(CD)       WDCT2
252900941223     C                   ENDIF
253000040618     C     CD            IFEQ      204
253100040618     C                   add       IMD(CD)       WDCT1
253200040618     C                   ENDIF
253300040617     C     CD            IFEQ      205
253400040617     C                   add       IMD(CD)       WDCT2
253500040617     C                   ENDIF
253600040519     C     CD            IFEQ      53
253700040519     C                   Z-ADD     IMD(CD)       WDCT3
253800040519     C                   ENDIF
253900040617     C     CD            IFEQ      206
254000040617     C                   add       IMD(CD)       WDCT3
254100040617     C                   ENDIF
254200040519     C     CD            IFEQ      54
254300040519     C                   Z-ADD     IMD(CD)       WDCT4
254400040519     C                   ENDIF
254500040617     C     CD            IFEQ      207
254600040617     C                   add       IMD(CD)       WDCT4
254700040617     C                   ENDIF
254800040519     C     CD            IFEQ      55
254900040519     C                   Z-ADD     IMD(CD)       WDCT5
255000040519     C                   ENDIF
255100040617     C     CD            IFEQ      208
255200040617     C                   add       IMD(CD)       WDCT5
255300040617     C                   ENDIF
255400980415     C     CD            IFEQ      13
255500040618     c* obsoleta
255600040618     C*    CD            OREQ      32
255700941223     C                   Z-ADD     IMD(CD)       WDCTL
255800941223     C                   ENDIF
255900980317     C* CONTROLLA SE LA COMPETENZA E' DA RIFATTURARE ALL'ALTRO NETWORK
256000980317     C                   SELECT
256100980317     C     RLD(CD)       WHENEQ    'LA'
256200980317     C                   Z-ADD     D$DLNA        LN
256300980317     C     RLD(CD)       WHENEQ    'LP'
256400980924     C                   Z-ADD     D$DLNP        LN
256500980317     C     RLD(CD)       WHENEQ    'TA'
256600980317     C                   Z-ADD     D$DTFA        LN
256700980317     C     RLD(CD)       WHENEQ    'TP'
256800980924     C                   Z-ADD     D$DTFP        LN
256900040518     C     RLD(CD)       WHENEQ    'T1'
257000040518     C                   Z-ADD     D$DAR5flp     LN
257100040518     C     RLD(CD)       WHENEQ    'T2'
257200040518     C                   Z-ADD     D$DAR5fl2     LN
257300040518     C     RLD(CD)       WHENEQ    'T3'
257400040518     C                   Z-ADD     D$DAR5fl3     LN
257500040518     C     RLD(CD)       WHENEQ    'T4'
257600040518     C                   Z-ADD     D$DAR5fl4     LN
257700040518     C     RLD(CD)       WHENEQ    'T5'
257800040518     C                   Z-ADD     D$DAR5fl5     LN
257900980317     C                   OTHER
258000980317     C                   Z-ADD     *ZEROS        LN                3 0
258100980317     C                   ENDSL
258200990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
258300990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
258400990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
258500990402     C* DAL 1° MARZO 1999 LE DEFLUENZE ESTERO SONO A CARICO DELLA ITL
258600990402     C* E NON DEVONO APPARIRE NEL TABULATO INFRAGRUPPO SDI-BART BART-SDI
258700990402     C* QUINDI NON LE FACCIO SOMMARE ALL'IMPORTO COMPETENZE DI ECFIC00F
258800990402     C* A GIUGNO/LUGLIO DEL 2000 TOGLIERE 'TA' DAL RIFERIMENTO CENTRO DI COSTO
258900990402     C* NELLE COMPETENZE 46 E 48 E CANCELLARE QUESTA MODIFICA
259000990402     C     WDDSP8        IFGT      19990300
259100990402     C     CD            IFEQ      46
259200990402     C     CD            OREQ      48
259300990402     C                   Z-ADD     *ZEROS        LN
259400990402     C                   ENDIF
259500990402     C                   ENDIF
259600990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259700990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259800990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259900011127     C*---       LN        IFNE *ZEROS
260000011127     C*---       DIT,LN    ANDNEWDSCL
260100011127     C*---                 ADD  IMD,CD    WDSCO
260200011127     C*---                 MOVEL'S'       WIDREG
260300011127     C*---                 ENDIF
260400980317     C*
260500941223     C                   ENDIF
260600941223     C                   ENDDO
260700970214     C*
260800970214     C* TOGLIE I TRANSITI AI SALDI CLIENTE
260900991027     C* ---- DA OGGI I TRANSITI SONO GIA' ESCLUSI DAL TRECD8R ------
261000991027     C******************** SUB  WDCT1     WDIRP
261100991027     C******************** SUB  WDCT2     WDIRP
261200941223     C*
261300030114     C* VOCI
261400030114     C* TRATTO CODICI E IMPORTI VOCI -DOPO-
261500030114     C                   MOVEA     D$DCVD        CAL
261600030114     C                   MOVEA     D$DIVD        IAL
261700030115     C                   MOVEA     D$DCOP        COP
261800030115     C                   MOVEA     D$DDAT        DAT
261900030115     C                   MOVEA     D$DAID        AID
262000030117     C                   MOVEA     D$DCMP        CMPV
262100030505DO  3C     1             DO        75            I
262200030114     C                   MOVE      CAL(I)        DSWCA
262300030114     C                   Z-ADD     DSWCN         CD
262400030124IF  4C     CD            IFGT      *ZEROS
262500030114     C                   MOVE      IAL(I)        DSWIA
262600030114     C                   Z-ADD     DSWIN         IMDV(CD)
262700030115     C                   MOVEL     COP(I)        COPDV(CD)
262800030115     C                   MOVEL     DAT(I)        DATDV(CD)
262900030115     C                   MOVEL     AID(I)        AIDDV(CD)
263000030117     C                   MOVE      CMPV(I)       DSWCA
263100030115     C                   Z-ADD     DSWCN         CMPDV(CD)
263200030124E   4C                   ENDIF
263300030114E   3C                   ENDDO
263400030114     C*
263500941223     C                   ENDIF
263600941223     C                   ENDIF
263700970903     C*
263800941223     C                   ENDSR
263900981012     C*------------------------------------------------------------------------*
264000981012     C*?NORVOC - NORMALIZZA COD.VOCE TOGLIENDO ZERI A SX E ALLINEANDO
264100981012     C* INPUT: WVOCE (NUMERICO)       OUTPUT: WVOCEA (ALFAB. ALL. SX)
264200981012     C*------------------------------------------------------------------------*
264300981012     C     NORVOC        BEGSR
264400981012     C*
264500981012     C                   CLEAR                   S4
264600981012     C                   CLEAR                   S4B
264700020205     C                   MOVEL     WVOCE         WVOCEA            5
264800981012     C                   MOVEA     WVOCEA        S4
264900981012     C* TOGLIE ZERI NON SIGNIFICATIVI
265000981012     C                   MOVEL     '0'           F4                1
265100981012     C                   Z-ADD     1             A4                6 0
265200981012     C     F4            DOWEQ     '0'
265300981012     C     A4            ANDLE     4
265400981012     C     S4(A4)        IFGT      *ZEROS
265500981012     C                   MOVEL     '1'           F4
265600981012     C                   SUB       1             A4
265700981012     C                   ELSE
265800981012     C                   MOVEL     *BLANKS       S4(A4)
265900981012     C                   ADD       1             A4
266000981012     C                   ENDIF
266100981012     C                   ENDDO
266200981012     C*
266300981012     C* ALLINEA IL CAMPO A SINISTRA
266400981012     C                   Z-ADD     *ZEROS        C4                6 0
266500981012     C     1             DO        4             A4
266600981012     C     S4(A4)        IFNE      *BLANKS
266700981012     C                   ADD       1             C4
266800981012     C                   MOVEL     S4(A4)        S4B(C4)
266900981012     C                   ENDIF
267000981012     C                   ENDDO
267100981012     C*
267200981012     C                   MOVEA     S4B           WVOCEA
267300981012     C*
267400981012     C                   ENDSR
267500011219     C********************************************************************
267600011219     C*?AGGMOV - AGGIORNA IL FILE ECMOV
267700011219     C********************************************************************
267800011219     C     AGGMOV        BEGSR
267900011219     C*
268000011219     C* PULISCE LE SCHIERE DI LAVORO
268100011219IF  1C     WPRGY         IFEQ      *ZEROS
268200011219     C                   CLEAR                   CDPY
268300020304     C                   CLEAR                   DRGY
268400011219     C                   CLEAR                   SVOY
268500011219     C                   CLEAR                   CDCY
268600011219     C                   CLEAR                   DAY
268700011219     C                   CLEAR                   IMPY
268800011219     C                   CLEAR                   VOCY
268900011219     C                   Z-ADD     *ZEROS        IY                6 0
269000011219     C                   Z-ADD     *ZEROS        WULTY             6 0
269100011219E   1C                   ENDIF
269200011219     C*
269300011219     C* SOLO PER 'VARIAZIONE' E 'CANCELLAZIONE' TRATTA IL -PRIMA-
269400011219IF  1C     D$GSTS        IFEQ      'U'
269500011219     C     D$GSTS        OREQ      'D'
269600011219IF  2C     D$PFRI        IFEQ      'S'
269700011219     C     WFCP          OREQ      'S'
269800011219     C                   EXSR      AGGMOP
269900011219E   2C                   ENDIF
270000011219E   1C                   ENDIF
270100011219     C*
270200011219     C* SOLO PER 'VARIAZIONE' E 'INSERIMENTO' TRATTA IL -DOPO-
270300011219     C     $ERR          IFEQ      'N'
270400011219IF  1C     D$GSTS        IFEQ      'U'
270500011219     C     D$GSTS        OREQ      'I'
270600011219IF  2C     D$DFRI        IFEQ      'S'
270700011219     C     WFCD          OREQ      'S'
270800011219     C                   EXSR      AGGMOD
270900011219E   2C                   ENDIF
271000011219E   1C                   ENDIF
271100011219     C                   ENDIF
271200011219     C*
271300011219     C* AGGIUNGO 1 BOLLA AL PROGRESSIVO BOLLE IN SCHIERA 'Y'
271400011219     C                   ADD       1             WPRGY
271500011219     C*
271600011219     C* LEGGE TUTTI GLI ELEMENTI DELLA SCHIERA E AGGIORNA ECMOV
271700011219IF  1C     $ERR          IFEQ      'N'
271800011219IF  2C     WPRGY         IFGE      MAXBOL
271900011219IF  2C     WULTY         ORGT      MAXIND
272000011219DO  3C     1             DO        WULTY         IY
272100011219IF  4C     IMPY(IY)      IFNE      *ZEROS
272200011221     C                   Z-ADD     IMPY(IY)      WIMP
272300011219     C                   EXSR      AGGECMOV
272400011219E   4C                   ENDIF
272500011219E   3C                   ENDDO
272600011219     C                   Z-ADD     *ZEROS        WPRGY             6 0
272700011219E   2C                   ENDIF
272800011219E   1C                   ENDIF
272900011220     C*
273000011219     C                   ENDSR
273100011220     C*------------------------------------------------------------------------*
273200011220     C*?AGGMOP - AGGIORNA IL FILE ECMOV - GESTIONE -PRIMA-
273300011220     C*------------------------------------------------------------------------*
273400011220     C     AGGMOP        BEGSR
273500011220     C*
273600011220     C                   MOVEL     'P'           WPD
273700011220     C*
273800011220     C* RECUPERA ITALIA/ESTERO DELLA BOLLA -PRIMA-
273900011220     C                   Z-ADD     W$PLNP        F
274000011220     C                   Z-ADD     D$PLNA        F1                4 0
274100040203     c                   clear                   wiep1
274200020705IF  1C     FIE(F)        IFEQ      'E'
274300020705     C     NTW(F)        OREQ      'DPD'
274400020705     C     FIE(F1)       OREQ      'E'
274500020705     C     NTW(F1)       OREQ      'DPD'
274600011220     C                   MOVEL     'E'           WIEP              1
274700040119     c* creo un nuovo campo di comodo (WRIEP1) per spaccare ulteriormente
274800040119     c* l'estero in dpd, euroexpress, fedex
274900040119     c                   select
275000040119     c                   when       NTW(F)='DPD' or NTW(F1)='DPD'
275100040119     C                   MOVEL     'D'           WIEP1             1
275200040119     c                   when       NTW(F)='EEX' or NTW(F1)='EEX'
275300040119     C                   MOVEL     'X'           WIEP1
275400040119     c                   when       NTW(F)='FED' or NTW(F1)='FED'
275500040119     C                   MOVEL     'F'           WIEP1
275600040119     c                   endsl
275700011220X   1C                   ELSE
275800011220     C                   MOVEL     'I'           WIEP
275900011220E   1C                   ENDIF
276000040202     C                   MOVEL     WPDSP8        COMDSPP           8 0
276100040202     C                   MOVE      00            COMDSPP
276200011220     C*
276300011220     C* LEGGE TUTTE LE COMPETENZE -PRIMA- E AGGIORNA LE SCHIERE DI LAVORO
276400011220DO  1C     1             DO        WULTP         IP
276500011220     C                   Z-ADD     CDP(IP)       CP
276600011220     C*
276700011220     C* CONTROLLA DATE DECORR. E SCADENZA
276800040202IF  2C     COMDSPP       IFLT      DDP(IP)
276900040202     C     COMDSPP       ORGT      DSP(IP)
277000011220     C     PEP(IP)       OREQ      *ZEROS
277100011220     C     IMP(CP)       OREQ      *ZEROS
277200011220     C     MEP(CP)       OREQ      'N'
277300011220     C                   ITER
277400011220E   2C                   ENDIF
277500011220     C* CONTROLLA FRANCO/ASSEGNATO
277600011220IF  2C     FAP(IP)       IFNE      *BLANKS
277700011220IF  3C     FAP(IP)       IFNE      D$PTPO
277800011220     C                   ITER
277900011220E   3C                   ENDIF
278000011220E   2C                   ENDIF
278100011220     C* CONTROLLA ITALIA/ESTERO
278200011220IF  2C     IEP(IP)       IFNE      *BLANKS
278300040119     c* se italia
278400040203IF  3C     wiep          ifeq      'I'
278500040203IF  3C     IEP(IP)       andne     WIEP
278600011220     C                   ITER
278700011220E   3C                   ENDIF
278800040119     c* se suddiviso in network
278900040203IF  3C     wiep          ifeq      'E'
279000040203IF  3C     IEP(IP)       IFeq      WIEP1
279100040203     c     iep(ip)       oreq      'E'
279200040203     c                   else
279300040203     C                   ITER
279400040203E   3C                   ENDIF
279500040203E   3C                   ENDIF
279600011220E   2C                   ENDIF
279700011220     C* CONTROLLA SE E' UN RICAVO E SE E' DA SCRIVERE
279800011220IF  2C     RCP(IP)       IFEQ      'R'
279900011220     C     D$PFRI        ANDNE     'S'
280000011220     C                   ITER
280100011220E   2C                   ENDIF
280200040119     C* Imposta la DATA REGISTRAZIONE = al mese della spedizione
280300040119     c                   CLEAR                   LDRG
280400040119     c                   MOVEL     wpdsp8        LDRG
280500040119     c                   MOVE      01            LDRG
280600040119     C* Imposta la DATA COMPETENZA dei movimenti contabili a seconda della data
280700040119     C* di riferimento associata a ciascuna anagrafica competenza e comunque
280800040119     C* solo la parte AVERE
280900040119     c                   MOVEL     wpdsp8        §cdpAM
281000040119if  4c                   if        DAP(IP) = 'A'
281100040119     c* per le spedizioni con data fino al 31/12/2003
281200040119if  2c                   IF        wpdsp8 > 20011200 and wpdsp8 < 20040101
281300040119if  3c                             and FDP(IP) = '1'
281400040119     c                   MOVEL     wpdsp8        §dspAM            6 0
281500040119     c                   MOVEL     d$pdcm        §dcmAM            6 0
281600040119     c                   Z-ADD     *zeros        §cdpAM            6 0
281700040119     c                   EXSR      CALDRF
281800040119e   3c                   ENDIF
281900040119     c* per le spedizioni con data maggiore al 2004
282000040119     c                   IF        wpdsp8 >=20040101
282100040119     C                   EVAL      $TDAT = fdp(ip)
282200040119     C                   EXSR      REPDAT
282300040129     C* se data non valorizzata scrivo la competenza nel mese della data
282400040129     c* spedizione o il mese successivo a seconda se ciclo partenza oppure
282500040129     c* arrivo
282600040129     C                   IF        $dat = 0
282700040129     C                   EXSR      SRcdp
282800040129     c                   else
282900040129     C                   movel     $DAT          §cdpAM
283000040119     C                   END
283100040119     c                   endif
283200040119e   2c                   ENDIF
283300011220     C*
283400020130     C                   MOVEL     §cdpAM        DSDAT8
283500020130     C                   MOVE      00            DSDAT8
283600011220IF  2C     CFP(IP)       IFEQ      *ZEROS
283700011220     C                   MOVEL     RIP(IP)       $RIC
283800011220     C                   EXSR      REPCDC
283900011220     C                   Z-ADD     $CDC          WCCX              7 0
284000011220X   XC                   ELSE
284100011220     C                   Z-ADD     CFP(IP)       WCCX
284200011220E   2C                   ENDIF
284300090915     C     IMP(CP)       DIV       100           WVVOCE           17 7
284400090915     C     WVVOCE        MULT      PEP(IP)       WIMY             19 5
284500011220     C*
284600011220     C* CONTROLLO SE ELEMENTO GIA' PRESENTE NELLA SCHIERA 'Y'
284700011220     C                   MOVEL     'N'           WFTROV            1
284800011220IF  2C     WULTY         IFGT      *ZEROS
284900011220     C                   Z-ADD     1             I
285000011220DO  3C     I             DOWLE     WULTY
285100011220     C     WFTROV        ANDEQ     'N'
285200011220     C*
285300011220     C* SE TUTTI I DATI 'CHIAVE' SONO UGUALI AGGIUNGO IMPORTO
285400020205     c                   MOVEL     KCP(IP)       WVOCE             4 0
285500020104     C                   EXSR      NORVOC
285600020104     C                   MOVEL     WVOCEA        LVOC
285700020102     c                   MOVEL     WCCX          A7                7
285800020102     c                   EVAL      LCDC = A7
285900011220IF  4C     DSDAT8        IFEQ      CDPY(I)
286000011220     C     LVOC          ANDEQ     VOCY(I)
286100011220     C     KSP(IP)       ANDEQ     SVOY(I)
286200011220     C     LCDC          ANDEQ     CDCY(I)
286300011220     C     DAP(IP)       ANDEQ     DAY(I)
286400020304     C     LDRG          ANDEQ     DRGY(I)
286500020103     C                   SUB       WIMY          IMPY(I)
286600011220     C                   MOVEL     'S'           WFTROV
286700011220E   4C                   ENDIF
286800011220     C*
286900011220     C                   ADD       1             I
287000011220E   3C                   ENDDO
287100011220E   2C                   ENDIF
287200011220     C*
287300011220     C* NON TROVATO NELLA SCHIERA 'Y'
287400011220IF  2C     WFTROV        IFEQ      'N'
287500011220     C                   ADD       1             IY
287600011220     C                   Z-ADD     IY            WULTY
287700011220     C                   Z-ADD     DSDAT8        CDPY(IY)
287800020304     C                   Z-ADD     LDRG          DRGY(IY)
287900020104     c                   MOVEL     KCP(IP)       WVOCE
288000020104     C                   EXSR      NORVOC
288100020104     C                   MOVEL     WVOCEA        VOCY(IY)
288200011220     C                   Z-ADD     KSP(IP)       SVOY(IY)
288300020102     C                   MOVEL     WCCX          A7
288400020102     C                   EVAL      CDCY(IY) = A7
288500011220     C                   MOVEL     DAP(IP)       DAY(IY)
288600020103     C                   Z-SUB     WIMY          IMPY(IY)
288700011220E   2C                   ENDIF
288800011220     C*
288900011220E   1C                   ENDDO
289000011220     C*
289100011220     C                   ENDSR
289200011220     C*------------------------------------------------------------------------*
289300011220     C*?AGGMOD - AGGIORNA IL FILE ECMOV - GESTIONE -DOPO-
289400011220     C*------------------------------------------------------------------------*
289500011220     C     AGGMOD        BEGSR
289600011220     C*
289700011220     C                   MOVEL     'D'           WPD
289800011220     C*
289900011220     C* RECUPERA ITALIA/ESTERO DELLA BOLLA -DOPO-
290000011220     C                   Z-ADD     W$DLNP        F
290100011220     C                   Z-ADD     D$DLNA        F1
290200040203     c                   clear                   wied1
290300020705IF  1C     FIE(F)        IFEQ      'E'
290400020705     C     NTW(F)        OREQ      'DPD'
290500020705     C     FIE(F1)       OREQ      'E'
290600020705     C     NTW(F1)       OREQ      'DPD'
290700011220     C                   MOVEL     'E'           WIED              1
290800040119     c* creo un nuovo campo di comodo (WIED1) per spaccare ulteriormente
290900040119     c* l'estero in dpd, euroexpress, fedex
291000040119     c                   select
291100040119     c                   when       NTW(F)='DPD' or NTW(F1)='DPD'
291200040119     C                   MOVEL     'D'           WIEd1             1
291300040119     c                   when       NTW(F)='EEX' or NTW(F1)='EEX'
291400040119     C                   MOVEL     'X'           WIEd1
291500040119     c                   when       NTW(F)='FED' or NTW(F1)='FED'
291600040119     C                   MOVEL     'F'           WIEd1
291700040119     c                   endsl
291800011220X   1C                   ELSE
291900011220     C                   MOVEL     'I'           WIED
292000011220E   1C                   ENDIF
292100040202     C                   MOVEL     WDDSP8        COMDSPD           8 0
292200040202     C                   MOVE      00            COMDSPD
292300011220     C*
292400011220     C* LEGGE TUTTE LE COMPETENZE -DOPO- E AGGIORNA LE SCHIERE DI LAVORO
292500011220DO  1C     1             DO        WULTD         ID
292600011220     C                   Z-ADD     CDD(ID)       CD
292700011220     C*
292800011220     C* CONTROLLA DATE DECORR. E SCADENZA
292900040202IF  2C     COMDSPD       IFLT      DDD(ID)
293000040202     C     COMDSPD       ORGT      DSD(ID)
293100011220     C     PED(ID)       OREQ      *ZEROS
293200011220     C     IMD(CD)       OREQ      *ZEROS
293300011220     C     MED(CD)       OREQ      'N'
293400011220     C                   ITER
293500011220E   2C                   ENDIF
293600011220     C* CONTROLLA FRANCO/ASSEGNATO
293700011220IF  2C     FAD(ID)       IFNE      *BLANKS
293800011220IF  3C     FAD(ID)       IFNE      D$DTPO
293900011220     C                   ITER
294000011220E   3C                   ENDIF
294100011220E   2C                   ENDIF
294200011220     C* CONTROLLA ITALIA/ESTERO
294300011220IF  2C     IED(ID)       IFNE      *BLANKS
294400040119     c* se italia
294500040203IF  3C     wied          ifeq      'I'
294600040203IF  3C     IEd(Id)       andne     WIEd
294700040203     C                   ITER
294800040203E   3C                   ENDIF
294900040203     c* se suddiviso in network
295000040203IF  3C     wied          ifeq      'E'
295100040203IF  3C     IEd(Id)       IFeq      WIEd1
295200040203     c     ied(id)       oreq      'E'
295300040203     c                   else
295400040203     C                   ITER
295500040203E   3C                   ENDIF
295600040203E   3C                   ENDIF
295700011220E   2C                   ENDIF
295800011220     C* CONTROLLA SE E' UN RICAVO E SE E' DA SCRIVERE
295900011220IF  2C     RCD(CD)       IFEQ      'R'
296000011220     C     D$DFRI        ANDNE     'S'
296100011220     C                   ITER
296200011220E   2C                   ENDIF
296300040119     C* Imposta la DATA REGISTRAZIONE = al mese della spedizione
296400040119     c                   CLEAR                   LDRG
296500040119     c                   MOVEL     wddsp8        LDRG
296600040119     c                   MOVE      01            LDRG
296700040119     C* Imposta la DATA COMPETENZA dei movimenti contabili a seconda della data
296800040119     C* di riferimento associata a ciascuna anagrafica comeptenza e comunque
296900040119     C* solo la parte AVERE
297000040119     c                   MOVEL     wddsp8        §cdpAM
297100040119if  4c                   if        DAD(ID) = 'A'
297200040119     c* per le spedizioni con data fino al 31/12/2003
297300040119if  2c                   IF        wddsp8 > 20011200 and wddsp8 < 20040101
297400040119if  3c                             and FDD(ID) = '1'
297500040119     c                   MOVEL     wddsp8        §dspAM            6 0
297600040119     c                   MOVEL     d$ddcm        §dcmAM            6 0
297700040119     c                   Z-ADD     *zeros        §cdpAM            6 0
297800040119     c                   EXSR      CALDRF
297900040119e   3c                   ENDIF
298000040119     c* per le spedizioni con data maggiore al 2004
298100040119     c                   if        wddsp8 >= 20040101
298200040119     C                   EVAL      $TDAT = fdd(id)
298300040119     C                   EXSR      REPDAT
298400040129     C* se data non valorizzata scrivo la competenza nel mese della data
298500040129     c* spedizione o il mese successivo a seconda se ciclo partenza oppure
298600040129     c* arrivo
298700040129     C                   IF        $dat = 0
298800040129     C                   EXSR      SRcdp
298900040129     c                   else
299000040129     C                   movel     $DAT          §cdpAM
299100040119     C                   END
299200040119     c                   end
299300040119e   2c                   ENDIF
299400011220     C*
299500020130     C                   MOVEL     §cdpAM        DSDAT8
299600020130     C                   MOVE      00            DSDAT8
299700011220IF  2C     CFD(ID)       IFEQ      *ZEROS
299800011220     C                   MOVEL     RID(ID)       $RIC
299900011220     C                   EXSR      REPCDC
300000011220     C                   Z-ADD     $CDC          WCCX
300100011220X   2C                   ELSE
300200011220     C                   Z-ADD     CFD(ID)       WCCX
300300011220E   2C                   ENDIF
300400011220     C     IMD(CD)       DIV       100           WVVOCE
300500020103     C     WVVOCE        MULT      PED(ID)       WIMY
300600011220     C*
300700011221     C* CONTROLLO SE ELEMENTO GIA' PRESENTE NELLA SCHIERA 'Y'
300800011220     C                   MOVEL     'N'           WFTROV
300900011220IF  2C     WULTY         IFGT      *ZEROS
301000011220     C                   Z-ADD     1             I
301100011220DO  3C     I             DOWLE     WULTY
301200011220     C     WFTROV        ANDEQ     'N'
301300020104     c                   MOVEL     KCD(ID)       WVOCE
301400020104     C                   EXSR      NORVOC
301500020104     C                   MOVEL     WVOCEA        LVOC
301600020104     c                   MOVEL     WCCX          A7
301700020104     c                   EVAL      LCDC = A7
301800011220IF  4C     DSDAT8        IFEQ      CDPY(I)
301900011220     C     LVOC          ANDEQ     VOCY(I)
302000020103     C     KSD(ID)       ANDEQ     SVOY(I)
302100011220     C     LCDC          ANDEQ     CDCY(I)
302200020103     C     DAD(ID)       ANDEQ     DAY(I)
302300020304     C     LDRG          ANDEQ     DRGY(I)
302400020103     C                   ADD       WIMY          IMPY(I)
302500011220     C                   MOVEL     'S'           WFTROV
302600011220E   4C                   ENDIF
302700011220     C*
302800011220     C                   ADD       1             I
302900011220E   3C                   ENDDO
303000011220E   2C                   ENDIF
303100011220     C*
303200011220     C* NON TROVATO NELLA SCHIERA 'Y'
303300011220IF  2C     WFTROV        IFEQ      'N'
303400011220     C                   ADD       1             IY
303500011220     C                   Z-ADD     IY            WULTY
303600011220     C                   Z-ADD     DSDAT8        CDPY(IY)
303700020304     C                   Z-ADD     LDRG          DRGY(IY)
303800020104     c                   MOVEL     KCD(ID)       WVOCE
303900020104     C                   EXSR      NORVOC
304000020104     C                   MOVEL     WVOCEA        VOCY(IY)
304100020103     C                   Z-ADD     KSD(ID)       SVOY(IY)
304200020102     C                   MOVEL     WCCX          A7
304300020102     C                   EVAL      CDCY(IY) = A7
304400020103     C                   MOVEL     DAD(ID)       DAY(IY)
304500020104     C                   Z-ADD     WIMY          IMPY(IY)
304600011220E   2C                   ENDIF
304700011220     C*
304800011220E   1C                   ENDDO
304900011220     C*
305000011220     C                   ENDSR
305100020130     C*------------------------------------------------------------------------*
305200020130     C*?CALDRF - CALCOLA LA DATA RIFERIMENTO PER IMPOSTARE LA DATA COMPETENZA
305300020130     c*        Input : §dspAM -> Anno/Mese spedizione
305400020130     c*                §dcmAM -> Anno/Mese consegna
305500020130     c*        Output: §cdpAM -> Anno/Mese competenza
305600020130     C*------------------------------------------------------------------------*
305700020130     C     caldrf        BEGSR
305800020130     c*---
305900020130     c* DSP =  DCM -> mese competenza = mese SPEDIZIONE
306000020130     c* DSP <> DCM -> mese competenza = mese SUCCESSIVO al mese SPEDIZIONE
306100020130     c*---
306200020130     c*
306300020130if  1C     §dspAM        IFEQ      §dcmAM
306400020130     C                   Z-ADD     §dspAM        §cdpAM
306500020130X   1c                   ELSE
306600020130     C                   MOVEL     §dspAM        n8                8 0
306700020130     c                   MOVE      01            n8
306800020130     c     *ISO          MOVE      n8            wdata
306900020130     c                   ADDDUR    1:*M          wdata
307000020130     c     *ISO          MOVE      wdata         n8
307100020130     c                   MOVEL     n8            §cdpAM
307200020130e   1C                   ENDIF
307300020130     c*
307400020130     C                   ENDSR
307500011219     C*------------------------------------------------------------------------*
307600011219     C*?AGGECMOV - AGGIORNA IL FILE ECMOV01L (MOVIMENTI CONTABILI)
307700011219     C*------------------------------------------------------------------------*
307800011219     C     AGGECMOV      BEGSR
307900011219     C*
308000011219     C* SOLO SE FLAG AGG. CONTO ECON. = SI
308100140321IF  0C*    D$GWFM        IFEQ      'S'
308200011219     C*
308300011219     C* CONTROLLO SE RECORD GIA' ESISTENTE
308400011219     C                   MOVEL     '201'         MOVSOC
308500011219     C                   Z-ADD     CDPY(IY)      MOVCDP
308600011219     C                   Z-ADD     SVOY(IY)      MOVSVO
308700011219     C                   MOVEL     CDCY(IY)      MOVCDC
308800011219     C                   MOVEL     DAY(IY)       MOVDA
308900020304     C                   Z-ADD     DRGY(IY)      MOVDRG
309000011219     C     KEYMOV        CHAIN     ECMOV01L                           99
309100011219IF  1C     *IN99         IFEQ      *OFF
309200011219     C                   EXSR      ADDECMOV
309300011219     C*
309400011219     C* SE L'IMPORTO DA AGGIORNARE E' A ZERO CANCELLO
309500011219IF  2C     MOVIMP        IFEQ      *ZERO
309600011219     C                   DELETE    ECMOV000
309700011219X   2C                   ELSE
309800011219     C                   UPDATE    ECMOV000
309900011219E   2C                   ENDIF
310000011219     C*
310100011219X   1C                   ELSE
310200011219     C*
310300011219     C                   EXSR      FISECMOV
310400011219     C                   EXSR      ADDECMOV
310500011219     C                   WRITE     ECMOV000
310600011219E   1C                   ENDIF
310700140321E   0C*                  ENDIF
310800011219     C*
310900011219     C                   ENDSR
311000011221     C*------------------------------------------------------------------------*
311100011221     C*?ADDECMOV - AGGIUNGE AI VALORI DEL FILE ECMOV01L (MOVIMENTI CONTABILI)
311200011221     C*------------------------------------------------------------------------*
311300011221     C     ADDECMOV      BEGSR
311400011221     C*
311500011221     C                   Z-ADD     MOVCDP        DSDAT8
311600011221     C                   Z-ADD     1             DSGIO
311700020304     C*---               Z-ADD     DSDAT8        MOVDRG
311800090916     C                   z-add(h)  WIMP          wimp£
311900090916     C                   ADD       WIMP£         MOVIMP
312000011221     C                   Z-ADD     DATCOR        MOVDUV
312100011221     C*
312200011221     C* SE L'IMPORTO E' POSITIVO SIGNIFICA CHE STO SCRIVENDO UN RECORD DI TIPO
312300020205     C* DOPO E CHE QUINDI DEVO ANCHE AGGIORNARE LA VOCE PROJ
312400011221IF  1C     WIMP          IFGT      *ZEROS
312500011221     C                   MOVEL     VOCY(IY)      MOVVOC
312600011221E   1C                   ENDIF
312700011221     C*
312800011221     C                   ENDSR
312900011221     C*------------------------------------------------------------------------*
313000011221     C*?FISECMOV - VALORIZZA I CAMPI FISSI DI ECMOV01L
313100011221     C*------------------------------------------------------------------------*
313200011221     C     FISECMOV      BEGSR
313300011221     C*
313400011221      *
313500011221     c                   EVAL      movsoc = '201'
313600011221     c                   EVAL      movtmo = 'G'
313700011221     c                   EVAL      movcot = 'COMP'
313800011221     c                   EVAL      movctb = 'CE'
313900011221     c                   Z-ADD     123456        movnrg
314000011221     c                   EVAL      movdiv = wdivce
314100011221     c                   EVAL      movdes = 'COMP.CE'
314200011221     c                   EVAL      movkcc = *blanks
314300011221     c                   EVAL      movksc = *blanks
314400011221     c                   EVAL      moven2 = *blanks
314500011221     c                   EVAL      moven3 = *blanks
314600020107     C                   Z-ADD     *ZEROS        movimp
314700011221     C*
314800011221     C                   ENDSR
314900941215     C*------------------------------------------------------------------------*
315000941215     C*?REPCDC - REPERISCE IL CENTRO DI COSTO
315100941215     C*          INPUT:  $RIC --> RIFERIMENTO CENTRO DI COSTO
315200941215     C*          OUTPUT: $CDC --> CENTRO DI COSTO
315300941215     C*------------------------------------------------------------------------*
315400941215     C     REPCDC        BEGSR
315500941215     C*
315600941215     C                   Z-ADD     *ZEROS        $FIL
315700941215     C                   Z-ADD     *ZEROS        $FIL5             5 0
315800941215     C*
315900941215     C* I CENTRI DI COSTO DI FILIALE SONO FORMATI IN QUESTO MODO:
316000941215     C*   - 2 ZERI + CODICE FILIALE + 2 ZERI
316100941215     C*     ES. BOLOGNA = 00+001+00 = 0000100
316200941215     C*
316300941215     C* SE E' STATO IMMESSO IL RIFERIMENTO CENTRO DI COSTO
316400941215IF  1C     $RIC          IFGT      *BLANKS
316500941215     C*
316600941215     C* CONTROLLA SE IL RIFERIMENTO ESISTE IN TABELLA
316700941215     C                   Z-ADD     1             I
316800941215     C                   SETOFF                                           99
316900941215     C     $RIC          LOOKUP    RIC(I)                                 99
317000941215IF  2C     *IN99         IFEQ      *OFF
317100941215     C                   MOVEL     MSG(3)        $MSG
317200941215     C                   EXSR      SCRERR
317300941215X   2C                   ELSE
317400941215     C*
317500941215     C* VEDO QUALE DEI CAMPI DELLA BOLLA DEVO CONSIDERARE COME C.D.C.
317600941215IF  3C     WPD           IFEQ      'P'
317700941215SL  4C                   SELECT
317800941215     C     $RIC          WHENEQ    'LA'
317900941215     C                   MOVE      D$PLNA        $FIL
318000941215     C     $RIC          WHENEQ    'LP'
318100941215     C                   MOVE      W$PLNP        $FIL
318200941215     C     $RIC          WHENEQ    'TA'
318300941215     C                   MOVE      D$PTFA        $FIL
318400941215     C     $RIC          WHENEQ    'TP'
318500941215     C                   MOVE      W$PTFP        $FIL
318600040518     C     $RIC          WHENEQ    'T1'
318700040518     C                   MOVE      D$par5flp     $FIL
318800040518     C     $RIC          WHENEQ    'T2'
318900040518     C                   MOVE      D$Par5fl2     $FIL
319000040518     C     $RIC          WHENEQ    'T3'
319100040518     C                   MOVE      D$Par5fl3     $FIL
319200040518     C     $RIC          WHENEQ    'T4'
319300040518     C                   MOVE      D$Par5fl4     $FIL
319400040518     C     $RIC          WHENEQ    'T5'
319500040518     C                   MOVE      D$Par5fl5     $FIL
319600941215E   4C                   ENDSL
319700941215X   3C                   ELSE
319800941215SL  4C                   SELECT
319900941215     C     $RIC          WHENEQ    'LA'
320000941215     C                   MOVE      D$DLNA        $FIL
320100941215     C     $RIC          WHENEQ    'LP'
320200941215     C                   MOVE      W$DLNP        $FIL
320300941215     C     $RIC          WHENEQ    'TA'
320400941215     C                   MOVE      D$DTFA        $FIL
320500941215     C     $RIC          WHENEQ    'TP'
320600941215     C                   MOVE      W$DTFP        $FIL
320700040518     C     $RIC          WHENEQ    'T1'
320800040518     C                   MOVE      D$Dar5flp     $FIL
320900040518     C     $RIC          WHENEQ    'T2'
321000040518     C                   MOVE      D$Dar5fl2     $FIL
321100040518     C     $RIC          WHENEQ    'T3'
321200040518     C                   MOVE      D$Dar5fl3     $FIL
321300040518     C     $RIC          WHENEQ    'T4'
321400040518     C                   MOVE      D$Dar5fl4     $FIL
321500040518     C     $RIC          WHENEQ    'T5'
321600040518     C                   MOVE      D$Dar5fl5     $FIL
321700941215E   4C                   ENDSL
321800941215E   3C                   ENDIF
321900941215     C*
322000941215     C                   MOVEL     $FIL          $FIL5
322100941215     C                   MOVE      $FIL5         $CDC
322200941215     C*
322300941215E   2C                   ENDIF
322400941215E   1C                   ENDIF
322500941215     C*
322600941215     C                   ENDSR
322700030120     C*------------------------------------------------------------------------*
322800030120     C*?REPDAT - REPERISCE LA DATA DI RIFERIMENTO C/E COOP
322900030120     c*          INPUT:  $TDAT   -> codice data
323000030120     C*          OUTPUT: $DAT   -->   data di riferimento
323100030120     C*------------------------------------------------------------------------*
323200030120     C     REPDAT        BEGSR
323300030120     c*
323400030120     c                   Z-ADD     *ZEROS        $DAT
323500030120     c* -PRIMA-
323600030120IF  1C     WPD           IFEQ      'P'
323700030120SL  2C                   SELECT
323800030120     C     $TDAT         WHENEQ    ' '
323900030120     C                   Z-ADD     D$PAAS        DSAAS
324000030120     C                   Z-ADD     D$PMGS        DSMGS
324100030120     C                   Z-ADD     DSDAT8        $DAT
324200030120     C     $TDAT         WHENEQ    'A'
324300030120     C                   Z-ADD     D$PDTI        $DAT
324400030120     C     $TDAT         WHENEQ    'C'
324500030120     C                   Z-ADD     D$PDCM        $DAT
324600040130     C                   WHEN      $TDAT = 'D'
324700030120     C                   Z-ADD     D$PDDC        $DAT
324800030120     C     $TDAT         WHENEQ    'G'
324900030120     C                   Z-ADD     D$PAGC        DSAAS
325000030120     C                   Z-ADD     D$PMGC        DSMGS
325100030120     C                   Z-ADD     DSDAT8        $DAT
325200030120     C     $TDAT         WHENEQ    'P'
325300030120     C                   Z-ADD     D$PDUC        $DAT
325400030120     C     $TDAT         WHENEQ    'R'
325500030120     C                   Z-ADD     D$PDRT        $DAT
325600040427     c* da maggio 2004 invertite le date xchè erano errate
325700030120     C     $TDAT         WHENEQ    'Y'
325800030120     C                   Z-ADD     D$PAR5DET     $DAT
325900040427     c                   if        wpdsp8 >=20040501
326000040427     C                   Z-ADD     D$PAR5DUT     $DAT
326100040427     c                   end
326200030120     C     $TDAT         WHENEQ    'X'
326300030120     C                   Z-ADD     D$PAR5DUT     $DAT
326400040427     c                   if        wpdsp8 >=20040501
326500040427     C                   Z-ADD     D$PAR5DET     $DAT
326600040427     c                   end
326700030120     C     $TDAT         WHENEQ    'Z'
326800030120     C                   Z-ADD     D$PAR5DE2     $DAT
326900040427     c                   if        wpdsp8 >=20040501
327000040427     C                   Z-ADD     D$PAR5DU2     $DAT
327100040427     c                   end
327200030120     C     $TDAT         WHENEQ    'W'
327300030120     C                   Z-ADD     D$PAR5DU2     $DAT
327400040427     c                   if        wpdsp8 >=20040501
327500040427     C                   Z-ADD     D$PAR5DE2     $DAT
327600040427     c                   end
327700040518     C     $TDAT         WHENEQ    'H'
327800040518     C                   Z-ADD     D$PAR5DE3     $DAT
327900040518     C     $TDAT         WHENEQ    'I'
328000040518     C                   Z-ADD     D$PAR5DU3     $DAT
328100040518     C     $TDAT         WHENEQ    'L'
328200040518     C                   Z-ADD     D$PAR5DE4     $DAT
328300040518     C     $TDAT         WHENEQ    'M'
328400040518     C                   Z-ADD     D$PAR5DU4     $DAT
328500040518     C     $TDAT         WHENEQ    'N'
328600040518     C                   Z-ADD     D$PAR5DE5     $DAT
328700040518     C     $TDAT         WHENEQ    'O'
328800040518     C                   Z-ADD     D$PAR5DU5     $DAT
328900030120E   2C                   ENDSL
329000030120     c*
329100030120     c* -DOPO-
329200030120X   1C                   ELSE
329300030120SL  2C                   SELECT
329400030120     C     $TDAT         WHENEQ    ' '
329500030120     C                   Z-ADD     D$DAAS        DSAAS
329600030120     C                   Z-ADD     D$DMGS        DSMGS
329700030120     C                   Z-ADD     DSDAT8        $DAT
329800030120     C     $TDAT         WHENEQ    'A'
329900030120     C                   Z-ADD     D$DDTI        $DAT
330000030120     C     $TDAT         WHENEQ    'C'
330100030120     C                   Z-ADD     D$DDCM        $DAT
330200040130     C                   WHEN      $TDAT = 'D'
330300030120     C                   Z-ADD     D$DDDC        $DAT
330400030120     C     $TDAT         WHENEQ    'G'
330500030120     C                   Z-ADD     D$DAGC        DSAAS
330600030120     C                   Z-ADD     D$DMGC        DSMGS
330700030120     C                   Z-ADD     DSDAT8        $DAT
330800030120     C     $TDAT         WHENEQ    'P'
330900030120     C                   Z-ADD     D$DDUC        $DAT
331000030120     C     $TDAT         WHENEQ    'R'
331100030120     C                   Z-ADD     D$DDRT        $DAT
331200040427     c* da maggio 2004 invertite le date xchè erano errate
331300030120     C     $TDAT         WHENEQ    'Y'
331400030120     C                   Z-ADD     D$DAR5DET     $DAT
331500040427     c                   if        wddsp8 >=20040501
331600040427     C                   Z-ADD     D$DAR5DUT     $DAT
331700040427     c                   end
331800030120     C     $TDAT         WHENEQ    'X'
331900030120     C                   Z-ADD     D$DAR5DUT     $DAT
332000040427     c                   if        wddsp8 >=20040501
332100040427     C                   Z-ADD     D$DAR5DET     $DAT
332200040427     c                   end
332300030120     C     $TDAT         WHENEQ    'Z'
332400030120     C                   Z-ADD     D$DAR5DE2     $DAT
332500040427     c                   if        wddsp8 >=20040501
332600040427     C                   Z-ADD     D$DAR5DU2     $DAT
332700040427     c                   end
332800030120     C     $TDAT         WHENEQ    'W'
332900030120     C                   Z-ADD     D$DAR5DU2     $DAT
333000040427     c                   if        wddsp8 >=20040501
333100040427     C                   Z-ADD     D$DAR5DE2     $DAT
333200040427     c                   end
333300040518     C     $TDAT         WHENEQ    'H'
333400040518     C                   Z-ADD     D$dAR5DE3     $DAT
333500040518     C     $TDAT         WHENEQ    'I'
333600040518     C                   Z-ADD     D$dAR5DU3     $DAT
333700040518     C     $TDAT         WHENEQ    'L'
333800040518     C                   Z-ADD     D$dAR5DE4     $DAT
333900040518     C     $TDAT         WHENEQ    'M'
334000040518     C                   Z-ADD     D$dAR5DU4     $DAT
334100040518     C     $TDAT         WHENEQ    'N'
334200040518     C                   Z-ADD     D$dAR5DE5     $DAT
334300040518     C     $TDAT         WHENEQ    'O'
334400040518     C                   Z-ADD     D$dAR5DU5     $DAT
334500030120E   2C                   ENDSL
334600030120E   1C                   ENDIF
334700030120     c*
334800030120     C                   ENDSR
334900941215     C*------------------------------------------------------------------------*
335000950316     C*?AGGCED - AGGIORNA IL FILE ECCED30C (COMPETENZE DETTAGLIO)
335100941215     C*------------------------------------------------------------------------*
335200941215     C     AGGCED        BEGSR
335300941215     C*
335400941215     C                   MOVEL     'N'           WDET              1
335500941215     C*
335600941215     C* SOLO SE FLAG AGG. CONTO ECON. = SI
335700970507IF -1C     D$GCET        IFEQ      'S'
335800970507     C*
335900970507     C* SE DELETE CANCELLO TUTTI I RECORD DELLA BOLLA IN ECCED
336000970507IF  0C     D$GSTS        IFEQ      'D'
336100970507     C*
336200970507     C                   Z-ADD     D$PAAS        CEDAAS
336300970507     C                   Z-ADD     D$PLNP        CEDLNP
336400970507     C                   Z-ADD     D$PNRS        CEDNRS
336500970507     C                   Z-ADD     D$PNSP        CEDNSP
336600970507     C                   MOVEL     D$PTBL        CEDTBL
336700970507     C     KEYCEP        SETLL     ECCED30C
336800970507     C     KEYCEP        READE     ECCED30C                               99
336900970507     C     *IN99         DOWEQ     *OFF
337000010828     C                   DELETE    ECCED000
337100970507     C     KEYCEP        READE     ECCED30C                               99
337200970507     C                   ENDDO
337300970507     C*
337400970507X   0C                   ELSE
337500941215     C*
337600941215     C* CICLO PER TUTTE LE COMPETENZE
337700941215DO  1C     1             DO        999           CD
337800941215     C*
337900941215     C     IMP(CD)       IFEQ      *ZEROS
338000941215     C     IMD(CD)       ANDEQ     *ZEROS
338100941215     C                   ITER
338200941215     C                   ENDIF
338300941215     C*
338400941215     C* SE NON E' UNA COMPETENZA DI TESTATA
338500941215IF  2C     CD            IFEQ      1
338600941215     C     CD            OREQ      2
338700941215     C     CD            OREQ      3
338800941215     C     CD            OREQ      4
338900941215     C     CD            OREQ      5
339000941215     C     CD            OREQ      6
339100941215     C     CD            OREQ      7
339200941215     C     CD            OREQ      8
339300950315     C     CD            OREQ      9
339400950315     C     CD            OREQ      10
339500941215     C     CD            OREQ      13
339600950315     C     CD            OREQ      14
339700950315     C     CD            OREQ      15
339800941215     C     CD            OREQ      29
339900941215     C     CD            OREQ      26
340000941215     C     CD            OREQ      25
340100941215     C     CD            OREQ      27
340200010827     C     CD            OREQ      33
340300941215X   2C                   ELSE
340400941215     C*
340500941215     C* E' UNA COMPETENZA VALIDA E DA SCRIVERE 'PRIMA'
340600941215IF  3C     IMP(CD)       IFNE      *ZEROS
340700941215     C     MEP(CD)       ANDEQ     'S'
340800941223     C     RCP(CD)       ANDEQ     'C'
340900950104     C     WFCP          ANDEQ     'S'
341000950104     C                   MOVEL     'S'           WFCEDP            1
341100941215X   3C                   ELSE
341200941215     C                   MOVEL     'N'           WFCEDP
341300941215E   3C                   ENDIF
341400941215     C*
341500941215     C* E' UNA COMPETENZA VALIDA E DA SCRIVERE 'DOPO'
341600941215IF  3C     IMD(CD)       IFNE      *ZEROS
341700941215     C     MED(CD)       ANDEQ     'S'
341800941223     C     RCD(CD)       ANDEQ     'C'
341900950104     C     WFCD          ANDEQ     'S'
342000941215     C                   MOVEL     'S'           WFCEDD            1
342100941215     C                   MOVEL     'S'           WDET
342200941215X   3C                   ELSE
342300941215     C                   MOVEL     'N'           WFCEDD
342400941215E   3C                   ENDIF
342500941215     C*
342600941215     C* CODICE COMPETENZA
342700941215     C                   Z-ADD     CD            CEDCMP
342800941215     C*
342900941215     C* SE PRIMA ESISTEVA ED ERA DA MOVIMENTARE IN C.E.
343000941215     C* E DOPO NON E' PIU' DA SCRIVERE, LO CANCELLO
343100020805     C     WFCEDP        IFEQ      'S'
343200020805     C     WFCEDD        ANDEQ     'N'
343300941215     C                   EXSR      DELCED
343400020805     C                   ENDIF
343500941215     C*
343600941215     C* SE PRIMA ESISTEVA ED ERA DA MOVIMENTARE IN C.E.
343700941215     C* E DOPO E' DA SCRIVERE CON IMPORTO DIVERSO, RISCRIVO
343800020805     C     WFCEDP        IFEQ      'S'
343900020805     C     WFCEDD        ANDEQ     'S'
344000941215     C     D$PTBL        IFEQ      D$DTBL
344100041110     C     IMP(CD)       ANDNE     IMD(CD)
344200941215     C                   EXSR      UPDCED
344300041110     C                   End
344400041110     C     D$PTBL        IFne      D$DTBL
344500941215     C                   EXSR      DELCED
344600941215     C                   EXSR      INSCED
344700941215     C                   ENDIF
344800941215     C                   ENDIF
344900950217     C*
345000941215     C* SE PRIMA NON ESISTEVA E DOPO E' DA SCRIVERE, SCRIVO
345100941215     C     WFCEDP        IFEQ      'N'
345200941215     C     WFCEDD        ANDEQ     'S'
345300030131     C*
345400030131     C* SE ESISTE, CMQ CANCELLA COMPETENZA
345500030131     C                   Z-ADD     D$DAAS        CEDAAS
345600030131     C                   Z-ADD     D$DLNP        CEDLNP
345700030131     C                   Z-ADD     D$DNRS        CEDNRS
345800030131     C                   Z-ADD     D$DNSP        CEDNSP
345900030131     C                   MOVEL     D$DTBL        CEDTBL
346000030131     C                   SETOFF                                       0304
346100030131     C     KEYCED        CHAIN     ECCED30C                           99
346200030131     C     *IN99         IFEQ      *ON
346300030131     C                   ELSE
346400030131     C     *IN04         IFEQ      *ON
346500030131     C                   DELETE    ECCEDP00
346600030131     C                   ENDIF
346700030131     C     *IN03         IFEQ      *ON
346800030131     C                   DELETE    ECCED000
346900030131     C                   ENDIF
347000030131     C                   ENDIF
347100030131     C*
347200970507     C                   EXSR      INSCED
347300941215     C                   ENDIF
347400941215     C*
347500941215E   2C                   ENDIF
347600941215E   1C                   ENDDO
347700941215E   0C                   ENDIF
347800970507E  -1C                   ENDIF
347900941215     C*
348000941215     C                   ENDSR
348100941215     C*------------------------------------------------------------------------*
348200941215     C*?DELCED - CANCELLA RECORD IN ECCED00F
348300941215     C*------------------------------------------------------------------------*
348400941215     C     DELCED        BEGSR
348500941215     C                   Z-ADD     D$PAAS        CEDAAS
348600941215     C                   Z-ADD     D$PLNP        CEDLNP
348700941215     C                   Z-ADD     D$PNRS        CEDNRS
348800941215     C                   Z-ADD     D$PNSP        CEDNSP
348900941215     C                   MOVEL     D$PTBL        CEDTBL
349000950316     C                   SETOFF                                       0304
349100950316     C     KEYCED        CHAIN     ECCED30C                           99
349200950316IF  1C     *IN99         IFEQ      *ON
349300020805     C                   MOVEL     MSG(9)        $MSG
349400020805     C                   EXSR      SCRERR
349500950316X   1C                   ELSE
349600950316IF  2C     *IN04         IFEQ      *ON
349700020805     C                   MOVEL     MSG(21)       $MSG
349800020805     C                   EXSR      SCRERR
349900950316X   2C                   ELSE
350000010828     C                   DELETE    ECCED000
350100950316E   2C                   ENDIF
350200950316E   1C                   ENDIF
350300941215     C                   ENDSR
350400941215     C*------------------------------------------------------------------------*
350500941215     C*?INSCED - INSERISCE RECORD IN ECCED00F
350600941215     C*------------------------------------------------------------------------*
350700941215     C     INSCED        BEGSR
350800941215     C                   Z-ADD     D$DAAS        CEDAAS
350900941215     C                   Z-ADD     D$DLNP        CEDLNP
351000941215     C                   Z-ADD     D$DNRS        CEDNRS
351100941215     C                   Z-ADD     D$DNSP        CEDNSP
351200941215     C                   MOVEL     D$DTBL        CEDTBL
351300020129     C                   Z-ADD     IMD(CD)       CEDIMP
351400950118     C     D$DMGS        DIV       100           CEDMES
351500010828     C                   WRITE     ECCED000
351600941215     C                   ENDSR
351700941215     C*------------------------------------------------------------------------*
351800941215     C*?UPDCED - RISCRIVE RECORD IN ECCED00F
351900941215     C*------------------------------------------------------------------------*
352000941215     C     UPDCED        BEGSR
352100941215     C                   Z-ADD     D$DAAS        CEDAAS
352200941215     C                   Z-ADD     D$DLNP        CEDLNP
352300941215     C                   Z-ADD     D$DNRS        CEDNRS
352400941215     C                   Z-ADD     D$DNSP        CEDNSP
352500941215     C                   MOVEL     D$DTBL        CEDTBL
352600950316     C                   SETOFF                                       0304
352700950316     C     KEYCED        CHAIN     ECCED30C                           99
352800950316IF  1C     *IN99         IFEQ      *ON
352900941215     C                   MOVEL     MSG(10)       $MSG
353000941215     C                   EXSR      SCRERR
353100950316X   1C                   ELSE
353200950316IF  2C     *IN04         IFEQ      *ON
353300950316     C                   MOVEL     MSG(22)       $MSG
353400950316     C                   EXSR      SCRERR
353500950316X   2C                   ELSE
353600020129     C                   Z-ADD     IMD(CD)       CEDIMP
353700950118     C     D$DMGS        DIV       100           CEDMES
353800010828     C                   UPDATE    ECCED000
353900950316E   2C                   ENDIF
354000950316E   1C                   ENDIF
354100941215     C                   ENDSR
354200941215     C*------------------------------------------------------------------------*
354300950316     C*?AGGCET - AGGIORNA IL FILE ECCET30C (COMPETENZE TESTATA)
354400941215     C*------------------------------------------------------------------------*
354500941215     C     AGGCET        BEGSR
354600941215     C*
354700941215     C* SOLO SE FLAG AGG. CONTO ECON. = SI
354800950104IF  1C     D$GCET        IFEQ      'S'
354900950104     C*
355000950104     C* SE NON SONO DA REGISTRARE NE' I RICAVI NE' LE COMPET.
355100950104IF  2C     D$DFRI        IFEQ      'N'
355200950104     C     WFCD          ANDEQ     'N'
355300950104     C*
355400950104     C* SE PRIMA ESISTEVA LO CANCELLO
355500020805     C     WCETPR        IFEQ      'S'
355600950104     C                   EXSR      DELCET
355700020805     C                   ENDIF
355800950913     C*
355900970507     C* SCRIVE COMUNQUE UN REC. CON COMPETENZE A ZERO (SE NON E' DELETE)
356000950913     C     $ERR          IFEQ      'N'
356100970507     C     D$GSTS        ANDNE     'D'
356200950914     C                   Z-ADD     D$DAAS        CETAAS
356300950914     C                   Z-ADD     D$DLNP        CETLNP
356400950914     C                   Z-ADD     D$DNRS        CETNRS
356500950914     C                   Z-ADD     D$DNSP        CETNSP
356600950914     C                   MOVEL     D$DTBL        CETTBL
356700950914     C     KEYCET        CHAIN     ECCET30C                           99
356800950914IF  1C     *IN99         IFEQ      *ON
356900950913     C                   EXSR      AZZCET
357000010828     C                   WRITE     ECCET000
357100950913     C                   ENDIF
357200950914     C                   ENDIF
357300950104     C*
357400950104     C* SE LE COMPET. O I RICAVI SONO DA CALCOLARE
357500950104X   2C                   ELSE
357600941215     C*
357700941215     C* SE GLI IMPORTI DOPO SONO TUTTI UGUALI A ZERO
357800950104IF  3C     IMD(1)        IFEQ      *ZEROS
357900941215     C     IMD(2)        ANDEQ     *ZEROS
358000941215     C     IMD(3)        ANDEQ     *ZEROS
358100941215     C     IMD(4)        ANDEQ     *ZEROS
358200941215     C     IMD(5)        ANDEQ     *ZEROS
358300941215     C     IMD(6)        ANDEQ     *ZEROS
358400941215     C     IMD(7)        ANDEQ     *ZEROS
358500941215     C     IMD(8)        ANDEQ     *ZEROS
358600950315     C     IMD(9)        ANDEQ     *ZEROS
358700950315     C     IMD(10)       ANDEQ     *ZEROS
358800941215     C     IMD(13)       ANDEQ     *ZEROS
358900950315     C     IMD(14)       ANDEQ     *ZEROS
359000950315     C     IMD(15)       ANDEQ     *ZEROS
359100941215     C     IMD(29)       ANDEQ     *ZEROS
359200941215     C     IMD(26)       ANDEQ     *ZEROS
359300941215     C     IMD(25)       ANDEQ     *ZEROS
359400941215     C     IMD(27)       ANDEQ     *ZEROS
359500010827     C     IMD(33)       ANDEQ     *ZEROS
359600941215     C*
359700941215     C* SE PRIMA ESISTEVA LO CANCELLO
359800020805     C     WCETPR        IFEQ      'S'
359900941215     C                   EXSR      DELCET
360000020805     C                   ENDIF
360100941215RN   C*
360200941215RN   C* NE INSERISCO UNO "VUOTO"
360300941215RN   C                   EXSR      INSCET
360400941215     C*
360500941215     C* IMPORTI DIVERSI DA ZERO
360600950104X   3C                   ELSE
360700941215     C*
360800950104IF  4C     WCETPR        IFEQ      'S'
360900941215     C*
361000950104IF  5C     D$PTBL        IFEQ      D$DTBL
361100941215     C                   EXSR      UPDCET
361200950104X   5C                   ELSE
361300941215     C                   EXSR      DELCET
361400941215     C                   EXSR      INSCET
361500950104E   5C                   ENDIF
361600941215     C*
361700950104X   4C                   ELSE
361800941215     C*
361900941215     C                   EXSR      INSCET
362000941215     C*
362100950104E   4C                   ENDIF
362200941215     C*
362300950104E   3C                   ENDIF
362400950104E   2C                   ENDIF
362500950104E   1C                   ENDIF
362600941215     C*
362700941215     C                   ENDSR
362800941215     C*------------------------------------------------------------------------*
362900941215     C*?INSCET - INSERISCE RECORD IN ECCET00F
363000941215     C*------------------------------------------------------------------------*
363100941215     C     INSCET        BEGSR
363200950104     C     $ERR          IFEQ      'N'
363300941215     C                   Z-ADD     D$DAAS        CETAAS
363400941215     C                   Z-ADD     D$DLNP        CETLNP
363500941215     C                   Z-ADD     D$DNRS        CETNRS
363600941215     C                   Z-ADD     D$DNSP        CETNSP
363700941215     C                   MOVEL     D$DTBL        CETTBL
363800941215     C                   EXSR      ADDCET
363900031105     C                   WRITE(e)  ECCET000
364000031105     C*
364100031105     c                   IF        %error
364200031105     C                   MOVEL     MSG(30)       $MSG
364300031105     C                   EXSR      SCRERR
364400031105     c                   ENDIF
364500031105     C*
364600950104     C                   ENDIF
364700941215     C                   ENDSR
364800941215     C*------------------------------------------------------------------------*
364900941215     C*?DELCET - CANCELLA RECORD IN ECCET00F
365000941215     C*------------------------------------------------------------------------*
365100941215     C     DELCET        BEGSR
365200941215     C                   Z-ADD     D$PAAS        CETAAS
365300941215     C                   Z-ADD     D$PLNP        CETLNP
365400941215     C                   Z-ADD     D$PNRS        CETNRS
365500941215     C                   Z-ADD     D$PNSP        CETNSP
365600941215     C                   MOVEL     D$PTBL        CETTBL
365700950316     C                   SETOFF                                       0102
365800950316     C     KEYCET        CHAIN     ECCET30C                           99
365900950316IF  1C     *IN99         IFEQ      *ON
366000020805     C                   MOVEL     MSG(11)       $MSG
366100020805     C                   EXSR      SCRERR
366200950316X   1C                   ELSE
366300950316IF  2C     *IN02         IFEQ      *ON
366400020805     C                   MOVEL     MSG(19)       $MSG
366500020805     C                   EXSR      SCRERR
366600950316X   2C                   ELSE
366700010828     C                   DELETE    ECCET000
366800950316E   2C                   ENDIF
366900950316E   1C                   ENDIF
367000941215     C                   ENDSR
367100941215     C*------------------------------------------------------------------------*
367200941215     C*?UPDCET - AGGIORNA RECORD IN ECCET00F
367300941215     C*------------------------------------------------------------------------*
367400941215     C     UPDCET        BEGSR
367500941215     C                   Z-ADD     D$DAAS        CETAAS
367600941215     C                   Z-ADD     D$DLNP        CETLNP
367700941215     C                   Z-ADD     D$DNRS        CETNRS
367800941215     C                   Z-ADD     D$DNSP        CETNSP
367900941215     C                   MOVEL     D$DTBL        CETTBL
368000950316     C                   SETOFF                                       0102
368100950316     C     KEYCET        CHAIN     ECCET30C                           99
368200950316IF  1C     *IN99         IFEQ      *ON
368300941215     C                   MOVEL     MSG(12)       $MSG
368400941215     C                   EXSR      SCRERR
368500950316X   1C                   ELSE
368600950316IF  2C     *IN02         IFEQ      *ON
368700950316     C                   MOVEL     MSG(20)       $MSG
368800950316     C                   EXSR      SCRERR
368900950316X   2C                   ELSE
369000941215     C                   EXSR      ADDCET
369100010828     C                   UPDATE    ECCET000
369200950316E   2C                   ENDIF
369300950316E   1C                   ENDIF
369400941215     C                   ENDSR
369500941215     C*------------------------------------------------------------------------*
369600950316     C*?ADDCET - IMPOSTA I CAMPI DEL FILE ECCET30C (COMPETENZA TESTATA)
369700941215     C*------------------------------------------------------------------------*
369800941215     C     ADDCET        BEGSR
369900941215     C*
370000020129     C                   Z-ADD     IMD(1)        CETPAR
370100020129     C                   Z-ADD     IMD(2)        CETPAM
370200020129     C                   Z-ADD     IMD(3)        CETPAU
370300020129     C                   Z-ADD     IMD(4)        CETPAV
370400020129     C                   Z-ADD     IMD(5)        CETARD
370500020129     C                   Z-ADD     IMD(6)        CETARM
370600020129     C                   Z-ADD     IMD(7)        CETARU
370700020129     C                   Z-ADD     IMD(8)        CETARV
370800020129     C                   Z-ADD     IMD(9)        CETMAF
370900020129     C                   Z-ADD     IMD(10)       CETMDE
371000020129     C                   Z-ADD     IMD(13)       CETTRL
371100020129     C                   Z-ADD     IMD(14)       CETTAF
371200020129     C                   Z-ADD     IMD(15)       CETTDE
371300020129     C                   Z-ADD     IMD(29)       CETPAG
371400020129     C                   Z-ADD     IMD(26)       CETSEA
371500020129     C                   Z-ADD     IMD(25)       CETSEC
371600020129     C                   Z-ADD     IMD(27)       CETRCV
371700020129     C                   Z-ADD     IMD(33)       CETSES
371800941215     C                   MOVEL     WDET          CETDET
371900941215     C                   Z-ADD     WNBCD         CETNBC
372000950118     C     D$DMGS        DIV       100           CETMES
372100941215     C*
372200941215     C                   ENDSR
372300950913     C*------------------------------------------------------------------------*
372400950913     C*?AZZCET - AZZERA I CAMPI DEL FILE ECCET30C (COMPETENZA TESTATA)
372500950913     C*------------------------------------------------------------------------*
372600950913     C     AZZCET        BEGSR
372700950913     C*
372800950913     C                   Z-ADD     D$DAAS        CETAAS
372900950913     C                   Z-ADD     D$DLNP        CETLNP
373000950913     C                   Z-ADD     D$DNRS        CETNRS
373100950913     C                   Z-ADD     D$DNSP        CETNSP
373200950913     C                   MOVEL     D$DTBL        CETTBL
373300950913     C                   Z-ADD     *ZEROS        CETPAR
373400950913     C                   Z-ADD     *ZEROS        CETPAM
373500950913     C                   Z-ADD     *ZEROS        CETPAU
373600950913     C                   Z-ADD     *ZEROS        CETPAV
373700950913     C                   Z-ADD     *ZEROS        CETARD
373800950913     C                   Z-ADD     *ZEROS        CETARM
373900950913     C                   Z-ADD     *ZEROS        CETARU
374000950913     C                   Z-ADD     *ZEROS        CETARV
374100950913     C                   Z-ADD     *ZEROS        CETMAF
374200950913     C                   Z-ADD     *ZEROS        CETMDE
374300950913     C                   Z-ADD     *ZEROS        CETTRL
374400950913     C                   Z-ADD     *ZEROS        CETTAF
374500950913     C                   Z-ADD     *ZEROS        CETTDE
374600950913     C                   Z-ADD     *ZEROS        CETPAG
374700950913     C                   Z-ADD     *ZEROS        CETSEA
374800950913     C                   Z-ADD     *ZEROS        CETSEC
374900950913     C                   Z-ADD     *ZEROS        CETRCV
375000010827     C                   Z-ADD     *ZEROS        CETSES
375100950913     C                   MOVEL     WDET          CETDET
375200950913     C                   Z-ADD     WNBCD         CETNBC
375300950913     C     D$DMGS        DIV       100           CETMES
375400980313     C*
375500980313     C                   ENDSR
375600030114     C*------------------------------------------------------------------------*
375700030509     C*?AGGCEV - AGGIORNA IL FILE ECCEV30C (VOCI COOP) e ECC2V
375800030114     C*------------------------------------------------------------------------*
375900030114     C     AGGCEV        BEGSR
376000030114     C*
376100030114     C* SOLO SE FLAG AGG.COOP = SI
376200030114IF  1C     D$GCOP        IFEQ      'S'
376300030114     C*
376400030114     C* SE NON SONO DA REGISTRARE NE' I RICAVI NE' LE COMPETENZE
376500030114IF  2C     D$DFRI        IFEQ      'N'
376600030114     C     WFCD          ANDEQ     'N'
376700030114     C*
376800030114     C* SE PRIMA ESISTEVA LO CANCELLO
376900030114IF  3C     WCEVPR        IFEQ      'S'
377000030114     C                   EXSR      DELCEV
377100030114E   3C                   ENDIF
377200030114     C*
377300030114     C* SCRIVE COMUNQUE UN REC. CON COMPETENZE A ZERO (SE NON E' DELETE)
377400030114IF  3C     $ERR          IFEQ      'N'
377500030114     C     D$GSTS        ANDNE     'D'
377600030114     C                   Z-ADD     D$DAAS        CEVAAS
377700030114     C                   Z-ADD     D$DLNP        CEVLNP
377800030114     C                   Z-ADD     D$DNRS        CEVNRS
377900030114     C                   Z-ADD     D$DNSP        CEVNSP
378000030114     C                   MOVEL     D$DTBL        CEVTBL
378100030114     C     KEYCEV        CHAIN     ECCEV30C                           99
378200030114IF  4C     *IN99         IFEQ      *ON
378300030114     C                   EXSR      AZZCEV
378400030114     C                   WRITE     ECCEV000
378500030114E   4C                   ENDIF
378600030114E   3C                   ENDIF
378700030114     C*
378800030114     C* SE LE COMPETENZE O I RICAVI SONO DA CALCOLARE
378900030114X   2C                   ELSE
379000030114     C*
379100030114     C* SE GLI IMPORTI DOPO SONO TUTTI UGUALI A ZERO
379200030123     C                   XFOOT     IMDV          N213             21 3
379300030123IF  3C     N213          IFEQ      *ZEROS
379400030114     C*
379500030114     C* SE PRIMA ESISTEVA LO CANCELLO
379600030114IF  4C     WCEVPR        IFEQ      'S'
379700030114     C                   EXSR      DELCEV
379800030114E   4C                   ENDIF
379900030114     C*
380000030114     C* NE INSERISCO UNO "VUOTO"
380100030114     C                   EXSR      INSCEV
380200030114     C*
380300030114     C* IMPORTI DIVERSI DA ZERO
380400030114X   3C                   ELSE
380500030114     C*
380600030114IF  4C     WCEVPR        IFEQ      'S'
380700030114     C*
380800030114IF  5C     D$PTBL        IFEQ      D$DTBL
380900030114     C                   EXSR      UPDCEV
381000030114X   5C                   ELSE
381100030114     C                   EXSR      DELCEV
381200030114     C                   EXSR      INSCEV
381300030114E   5C                   ENDIF
381400030114     C*
381500030114X   4C                   ELSE
381600030114     C*
381700030114     C                   EXSR      INSCEV
381800030114     C*
381900030114E   4C                   ENDIF
382000030114     C*
382100030114E   3C                   ENDIF
382200030114E   2C                   ENDIF
382300030114E   1C                   ENDIF
382400030114     C*
382500030114     C                   ENDSR
382600030114     C*------------------------------------------------------------------------*
382700030509     C*?INSCEV - INSERISCE RECORD IN ECCEV00F ed eventualmente di ECC2V00F
382800030114     C*------------------------------------------------------------------------*
382900030114     C     INSCEV        BEGSR
383000030509     C*
383100030509     C* azzera variabili di lavoro
383200030509     c                   Z-ADD     *ZEROS        ULTCD             5 0
383300030509     c*
383400030509     C* voci coop testata
383500030114IF  1C     $ERR          IFEQ      'N'
383600030114     C                   Z-ADD     D$DAAS        CEVAAS
383700030114     C                   Z-ADD     D$DLNP        CEVLNP
383800030114     C                   Z-ADD     D$DNRS        CEVNRS
383900030114     C                   Z-ADD     D$DNSP        CEVNSP
384000030114     C                   MOVEL     D$DTBL        CEVTBL
384100030128     C                   EXSR      AZZCEV
384200030114     C                   EXSR      ADDCEV
384300030114     C                   WRITE     ECCEV000
384400030509     c*
384500030509     C* voci coop estensione (se ce n'è)
384600030509if  2C     ULTCD         IFGT      *ZEROS
384700030509     C                   Z-ADD     D$DAAS        C2VAAS
384800030509     C                   Z-ADD     D$DLNP        C2VLNP
384900030509     C                   Z-ADD     D$DNRS        C2VNRS
385000030509     C                   Z-ADD     D$DNSP        C2VNSP
385100030509     C                   MOVEL     D$DTBL        C2VTBL
385200030509     C                   EXSR      AZZC2V
385300030509     C                   EXSR      ADDC2V
385400030509     C                   WRITE     ECC2V000
385500030509E   2C                   ENDIF
385600030509     C*
385700030509E   1C                   ENDIF
385800030114     C*
385900030114     C                   ENDSR
386000030114     C*------------------------------------------------------------------------*
386100030114     C*?DELCEV - CANCELLA RECORD IN ECCEV00F
386200030114     C*------------------------------------------------------------------------*
386300030114     C     DELCEV        BEGSR
386400030509     C*
386500030509     C* voci coop testata
386600030114     C                   Z-ADD     D$PAAS        CEVAAS
386700030114     C                   Z-ADD     D$PLNP        CEVLNP
386800030114     C                   Z-ADD     D$PNRS        CEVNRS
386900030114     C                   Z-ADD     D$PNSP        CEVNSP
387000030114     C                   MOVEL     D$PTBL        CEVTBL
387100030114     C                   SETOFF                                       0506
387200030114     C     KEYCEV        CHAIN     ECCEV30C                           99
387300030114IF  1C     *IN99         IFEQ      *ON
387400030114     C                   MOVEL     MSG(25)       $MSG
387500030114     C                   EXSR      SCRERR
387600030114X   1C                   ELSE
387700030509IF  2C     *IN06         IFEQ      *ON
387800030114     C                   MOVEL     MSG(26)       $MSG
387900030114     C                   EXSR      SCRERR
388000030114X   2C                   ELSE
388100030114     C                   DELETE    ECCEV000
388200030114E   2C                   ENDIF
388300030114E   1C                   ENDIF
388400030509     c*
388500030509     C* voci coop estensione
388600030509     C                   Z-ADD     D$PAAS        C2VAAS
388700030509     C                   Z-ADD     D$PLNP        C2VLNP
388800030509     C                   Z-ADD     D$PNRS        C2VNRS
388900030509     C                   Z-ADD     D$PNSP        C2VNSP
389000030509     C                   MOVEL     D$PTBL        C2VTBL
389100030509     C                   SETOFF                                       0708
389200030509     C     KEYC2V        CHAIN     ECC2V30C                           99
389300030509IF  1C     *IN99         IFEQ      *OFF
389400030509     C                   DELETE    ECC2V000
389500030509E   1C                   ENDIF
389600030509     C*
389700030114     C                   ENDSR
389800030114     C*------------------------------------------------------------------------*
389900030114     C*?UPDCEV - AGGIORNA RECORD IN ECCEV00F
390000030114     C*------------------------------------------------------------------------*
390100030114     C     UPDCEV        BEGSR
390200030509     C*
390300030509     C* azzera variabili di lavoro
390400030509     c                   Z-ADD     *ZEROS        ULTCD
390500030114     C*
390600030509     C* voci coop testata
390700030114     C                   Z-ADD     D$DAAS        CEVAAS
390800030114     C                   Z-ADD     D$DLNP        CEVLNP
390900030114     C                   Z-ADD     D$DNRS        CEVNRS
391000030114     C                   Z-ADD     D$DNSP        CEVNSP
391100030114     C                   MOVEL     D$DTBL        CEVTBL
391200030114     C                   SETOFF                                       0506
391300030114     C     KEYCEV        CHAIN     ECCEV30C                           99
391400030114IF  1C     *IN99         IFEQ      *ON
391500030114     C                   MOVEL     MSG(27)       $MSG
391600030114     C                   EXSR      SCRERR
391700030114X   1C                   ELSE
391800030509IF  2C     *IN06         IFEQ      *ON
391900030114     C                   MOVEL     MSG(28)       $MSG
392000030114     C                   EXSR      SCRERR
392100030114X   2C                   ELSE
392200030124     C                   EXSR      AZZCEV
392300030114     C                   EXSR      ADDCEV
392400030114     C                   UPDATE    ECCEV000
392500030114E   2C                   ENDIF
392600030114E   1C                   ENDIF
392700030509     c*
392800030509     C* voci coop estensione (se ce n'è)
392900030509     C                   Z-ADD     D$DAAS        C2VAAS
393000030509     C                   Z-ADD     D$DLNP        C2VLNP
393100030509     C                   Z-ADD     D$DNRS        C2VNRS
393200030509     C                   Z-ADD     D$DNSP        C2VNSP
393300030509     C                   MOVEL     D$DTBL        C2VTBL
393400030509     C                   SETOFF                                       0708
393500030509     C     KEYC2V        CHAIN     ECC2V30C                           99
393600030509     C                   EXSR      AZZC2V
393700030509     C                   EXSR      ADDC2V
393800030512IF  1C     *IN99         IFEQ      *OFF
393900030512IF  2C     ULTCD         IFGT      *ZEROS
394000030512     C     WC2VP         OREQ      'S'
394100030512     C                   UPDATE    ECC2V000
394200030512E   2C                   ENDIF
394300030512X   1C                   ELSE
394400030512IF  2C     ULTCD         IFGT      *ZEROS
394500030512     C                   WRITE     ECC2V000
394600030512E   2C                   ENDIF
394700030512E   1C                   ENDIF
394800030114     C*
394900030114     C                   ENDSR
395000030114     C*------------------------------------------------------------------------*
395100030114     C*?ADDCEV - IMPOSTA I CAMPI DEL FILE ECCEV30C (VOCI COOP)
395200030114     C*------------------------------------------------------------------------*
395300030114     C     ADDCEV        BEGSR
395400030115     C*
395500030115     C* SE C'É ALMENO UN DATO IN SCHIERA, 'S' -> FLAG REG. SICOP
395600030124     C                   MOVEL     'N'           WCODREG           1
395700030114     C*
395800030115     C* CICLO PER TUTTE LE VOCI
395900030115DO  1C     1             DO        999           CD
396000030115IF  2C     IMDV(CD)      IFGT      *ZEROS
396100030124     C                   MOVEL     'S'           WCODREG
396200030115     C*
396300030115     C* MEMORIZZA CODICE E IMPORTO NEL PRIMO CAMPO LIBERO
396400030115IF  3C                   IF        CEVI01 = *ZEROS
396500030115     C                   Z-ADD     CD            CEVC01
396600030115     C                   Z-ADD     IMDV(CD)      CEVI01
396700030115     C                   ITER
396800030115E   3C                   ENDIF
396900030115IF  3C                   IF        CEVI02 = *ZEROS
397000030115     C                   Z-ADD     CD            CEVC02
397100030115     C                   Z-ADD     IMDV(CD)      CEVI02
397200030115     C                   ITER
397300030115E   3C                   ENDIF
397400030115IF  3C                   IF        CEVI03 = *ZEROS
397500030115     C                   Z-ADD     CD            CEVC03
397600030115     C                   Z-ADD     IMDV(CD)      CEVI03
397700030115     C                   ITER
397800030115E   3C                   ENDIF
397900030115IF  3C                   IF        CEVI04 = *ZEROS
398000030115     C                   Z-ADD     CD            CEVC04
398100030115     C                   Z-ADD     IMDV(CD)      CEVI04
398200030115     C                   ITER
398300030115E   3C                   ENDIF
398400030115IF  3C                   IF        CEVI05 = *ZEROS
398500030115     C                   Z-ADD     CD            CEVC05
398600030115     C                   Z-ADD     IMDV(CD)      CEVI05
398700030115     C                   ITER
398800030115E   3C                   ENDIF
398900030115IF  3C                   IF        CEVI06 = *ZEROS
399000030115     C                   Z-ADD     CD            CEVC06
399100030115     C                   Z-ADD     IMDV(CD)      CEVI06
399200030115     C                   ITER
399300030115E   3C                   ENDIF
399400030115IF  3C                   IF        CEVI07 = *ZEROS
399500030115     C                   Z-ADD     CD            CEVC07
399600030115     C                   Z-ADD     IMDV(CD)      CEVI07
399700030115     C                   ITER
399800030115E   3C                   ENDIF
399900030115IF  3C                   IF        CEVI08 = *ZEROS
400000030115     C                   Z-ADD     CD            CEVC08
400100030115     C                   Z-ADD     IMDV(CD)      CEVI08
400200030115     C                   ITER
400300030115E   3C                   ENDIF
400400030115IF  3C                   IF        CEVI09 = *ZEROS
400500030115     C                   Z-ADD     CD            CEVC09
400600030115     C                   Z-ADD     IMDV(CD)      CEVI09
400700030115     C                   ITER
400800030115E   3C                   ENDIF
400900030115     C*
401000030115IF  3C                   IF        CEVI10 = *ZEROS
401100030115     C                   Z-ADD     CD            CEVC10
401200030115     C                   Z-ADD     IMDV(CD)      CEVI10
401300030115     C                   ITER
401400030115E   3C                   ENDIF
401500030115IF  3C                   IF        CEVI11 = *ZEROS
401600030115     C                   Z-ADD     CD            CEVC11
401700030115     C                   Z-ADD     IMDV(CD)      CEVI11
401800030115     C                   ITER
401900030115E   3C                   ENDIF
402000030115IF  3C                   IF        CEVI12 = *ZEROS
402100030115     C                   Z-ADD     CD            CEVC12
402200030115     C                   Z-ADD     IMDV(CD)      CEVI12
402300030115     C                   ITER
402400030115E   3C                   ENDIF
402500030115IF  3C                   IF        CEVI13 = *ZEROS
402600030115     C                   Z-ADD     CD            CEVC13
402700030115     C                   Z-ADD     IMDV(CD)      CEVI13
402800030115     C                   ITER
402900030115E   3C                   ENDIF
403000030115IF  3C                   IF        CEVI14 = *ZEROS
403100030115     C                   Z-ADD     CD            CEVC14
403200030115     C                   Z-ADD     IMDV(CD)      CEVI14
403300030115     C                   ITER
403400030115E   3C                   ENDIF
403500030115IF  3C                   IF        CEVI15 = *ZEROS
403600030115     C                   Z-ADD     CD            CEVC15
403700030115     C                   Z-ADD     IMDV(CD)      CEVI15
403800030115     C                   ITER
403900030115E   3C                   ENDIF
404000030115IF  3C                   IF        CEVI16 = *ZEROS
404100030115     C                   Z-ADD     CD            CEVC16
404200030115     C                   Z-ADD     IMDV(CD)      CEVI16
404300030115     C                   ITER
404400030115E   3C                   ENDIF
404500030115IF  3C                   IF        CEVI17 = *ZEROS
404600030115     C                   Z-ADD     CD            CEVC17
404700030115     C                   Z-ADD     IMDV(CD)      CEVI17
404800030115     C                   ITER
404900030115E   3C                   ENDIF
405000030115IF  3C                   IF        CEVI18 = *ZEROS
405100030115     C                   Z-ADD     CD            CEVC18
405200030115     C                   Z-ADD     IMDV(CD)      CEVI18
405300030115     C                   ITER
405400030115E   3C                   ENDIF
405500030115IF  3C                   IF        CEVI19 = *ZEROS
405600030115     C                   Z-ADD     CD            CEVC19
405700030115     C                   Z-ADD     IMDV(CD)      CEVI19
405800030115     C                   ITER
405900030115E   3C                   ENDIF
406000030115     C*
406100030115IF  3C                   IF        CEVI20 = *ZEROS
406200030115     C                   Z-ADD     CD            CEVC20
406300030115     C                   Z-ADD     IMDV(CD)      CEVI20
406400030115     C                   ITER
406500030115E   3C                   ENDIF
406600030115IF  3C                   IF        CEVI21 = *ZEROS
406700030115     C                   Z-ADD     CD            CEVC21
406800030115     C                   Z-ADD     IMDV(CD)      CEVI21
406900030115     C                   ITER
407000030115E   3C                   ENDIF
407100030115IF  3C                   IF        CEVI22 = *ZEROS
407200030115     C                   Z-ADD     CD            CEVC22
407300030115     C                   Z-ADD     IMDV(CD)      CEVI22
407400030115     C                   ITER
407500030115E   3C                   ENDIF
407600030115IF  3C                   IF        CEVI23 = *ZEROS
407700030115     C                   Z-ADD     CD            CEVC23
407800030115     C                   Z-ADD     IMDV(CD)      CEVI23
407900030115     C                   ITER
408000030115E   3C                   ENDIF
408100030115IF  3C                   IF        CEVI24 = *ZEROS
408200030115     C                   Z-ADD     CD            CEVC24
408300030115     C                   Z-ADD     IMDV(CD)      CEVI24
408400030115     C                   ITER
408500030115E   3C                   ENDIF
408600030115IF  3C                   IF        CEVI25 = *ZEROS
408700030115     C                   Z-ADD     CD            CEVC25
408800030115     C                   Z-ADD     IMDV(CD)      CEVI25
408900030115     C                   ITER
409000030115E   3C                   ENDIF
409100030115IF  3C                   IF        CEVI26 = *ZEROS
409200030115     C                   Z-ADD     CD            CEVC26
409300030115     C                   Z-ADD     IMDV(CD)      CEVI26
409400030115     C                   ITER
409500030115E   3C                   ENDIF
409600030115IF  3C                   IF        CEVI27 = *ZEROS
409700030115     C                   Z-ADD     CD            CEVC27
409800030115     C                   Z-ADD     IMDV(CD)      CEVI27
409900030115     C                   ITER
410000030115E   3C                   ENDIF
410100030115IF  3C                   IF        CEVI28 = *ZEROS
410200030115     C                   Z-ADD     CD            CEVC28
410300030115     C                   Z-ADD     IMDV(CD)      CEVI28
410400030115     C                   ITER
410500030115E   3C                   ENDIF
410600030115IF  3C                   IF        CEVI29 = *ZEROS
410700030115     C                   Z-ADD     CD            CEVC29
410800030115     C                   Z-ADD     IMDV(CD)      CEVI29
410900030115     C                   ITER
411000030115E   3C                   ENDIF
411100030115     C*
411200030115IF  3C                   IF        CEVI30 = *ZEROS
411300030115     C                   Z-ADD     CD            CEVC30
411400030115     C                   Z-ADD     IMDV(CD)      CEVI30
411500030115     C                   ITER
411600030115E   3C                   ENDIF
411700030115IF  3C                   IF        CEVI31 = *ZEROS
411800030115     C                   Z-ADD     CD            CEVC31
411900030115     C                   Z-ADD     IMDV(CD)      CEVI31
412000030115     C                   ITER
412100030115E   3C                   ENDIF
412200030115IF  3C                   IF        CEVI32 = *ZEROS
412300030115     C                   Z-ADD     CD            CEVC32
412400030115     C                   Z-ADD     IMDV(CD)      CEVI32
412500030115     C                   ITER
412600030115E   3C                   ENDIF
412700030115IF  3C                   IF        CEVI33 = *ZEROS
412800030115     C                   Z-ADD     CD            CEVC33
412900030115     C                   Z-ADD     IMDV(CD)      CEVI33
413000030115     C                   ITER
413100030115E   3C                   ENDIF
413200030115IF  3C                   IF        CEVI34 = *ZEROS
413300030115     C                   Z-ADD     CD            CEVC34
413400030115     C                   Z-ADD     IMDV(CD)      CEVI34
413500030115     C                   ITER
413600030115E   3C                   ENDIF
413700030115IF  3C                   IF        CEVI35 = *ZEROS
413800030115     C                   Z-ADD     CD            CEVC35
413900030115     C                   Z-ADD     IMDV(CD)      CEVI35
414000030115     C                   ITER
414100030115E   3C                   ENDIF
414200030115IF  3C                   IF        CEVI36 = *ZEROS
414300030115     C                   Z-ADD     CD            CEVC36
414400030115     C                   Z-ADD     IMDV(CD)      CEVI36
414500030115     C                   ITER
414600030115E   3C                   ENDIF
414700030115IF  3C                   IF        CEVI37 = *ZEROS
414800030115     C                   Z-ADD     CD            CEVC37
414900030115     C                   Z-ADD     IMDV(CD)      CEVI37
415000030115     C                   ITER
415100030115E   3C                   ENDIF
415200030115IF  3C                   IF        CEVI38 = *ZEROS
415300030115     C                   Z-ADD     CD            CEVC38
415400030115     C                   Z-ADD     IMDV(CD)      CEVI38
415500030115     C                   ITER
415600030115E   3C                   ENDIF
415700030115IF  3C                   IF        CEVI39 = *ZEROS
415800030115     C                   Z-ADD     CD            CEVC39
415900030115     C                   Z-ADD     IMDV(CD)      CEVI39
416000030115     C                   ITER
416100030115E   3C                   ENDIF
416200030115     C*
416300030115IF  3C                   IF        CEVI40 = *ZEROS
416400030115     C                   Z-ADD     CD            CEVC40
416500030115     C                   Z-ADD     IMDV(CD)      CEVI40
416600030115     C                   ITER
416700030115E   3C                   ENDIF
416800030115IF  3C                   IF        CEVI41 = *ZEROS
416900030115     C                   Z-ADD     CD            CEVC41
417000030115     C                   Z-ADD     IMDV(CD)      CEVI41
417100030115     C                   ITER
417200030115E   3C                   ENDIF
417300030115IF  3C                   IF        CEVI42 = *ZEROS
417400030115     C                   Z-ADD     CD            CEVC42
417500030115     C                   Z-ADD     IMDV(CD)      CEVI42
417600030115     C                   ITER
417700030115E   3C                   ENDIF
417800030115IF  3C                   IF        CEVI43 = *ZEROS
417900030115     C                   Z-ADD     CD            CEVC43
418000030115     C                   Z-ADD     IMDV(CD)      CEVI43
418100030115     C                   ITER
418200030115E   3C                   ENDIF
418300030115IF  3C                   IF        CEVI44 = *ZEROS
418400030115     C                   Z-ADD     CD            CEVC44
418500030115     C                   Z-ADD     IMDV(CD)      CEVI44
418600030115     C                   ITER
418700030115E   3C                   ENDIF
418800030115IF  3C                   IF        CEVI45 = *ZEROS
418900030115     C                   Z-ADD     CD            CEVC45
419000030115     C                   Z-ADD     IMDV(CD)      CEVI45
419100030115     C                   ITER
419200030115E   3C                   ENDIF
419300030115IF  3C                   IF        CEVI46 = *ZEROS
419400030115     C                   Z-ADD     CD            CEVC46
419500030115     C                   Z-ADD     IMDV(CD)      CEVI46
419600030115     C                   ITER
419700030115E   3C                   ENDIF
419800030115IF  3C                   IF        CEVI47 = *ZEROS
419900030115     C                   Z-ADD     CD            CEVC47
420000030115     C                   Z-ADD     IMDV(CD)      CEVI47
420100030115     C                   ITER
420200030115E   3C                   ENDIF
420300030115IF  3C                   IF        CEVI48 = *ZEROS
420400030115     C                   Z-ADD     CD            CEVC48
420500030115     C                   Z-ADD     IMDV(CD)      CEVI48
420600030115     C                   ITER
420700030115E   3C                   ENDIF
420800030115IF  3C                   IF        CEVI49 = *ZEROS
420900030115     C                   Z-ADD     CD            CEVC49
421000030115     C                   Z-ADD     IMDV(CD)      CEVI49
421100030115     C                   ITER
421200030115E   3C                   ENDIF
421300030115IF  3C                   IF        CEVI50 = *ZEROS
421400030115     C                   Z-ADD     CD            CEVC50
421500030115     C                   Z-ADD     IMDV(CD)      CEVI50
421600030509     c                   Z-ADD     CD            ULTCD
421700030115     C                   ITER
421800030115E   3C                   ENDIF
421900030115     C*
422000030115E   2C                   ENDIF
422100030115E   1C                   ENDDO
422200030115     C     D$DMGS        DIV       100           CEVMES
422300030114     C*
422400030114     C                   ENDSR
422500030114     C*------------------------------------------------------------------------*
422600030114     C*?AZZCEV - AZZERA I CAMPI DEL FILE ECCEV30C (VOCI COOP)
422700030114     C*------------------------------------------------------------------------*
422800030114     C     AZZCEV        BEGSR
422900030115     C*
423000030114     C                   Z-ADD     D$DAAS        CEVAAS
423100030114     C                   Z-ADD     D$DLNP        CEVLNP
423200030114     C                   Z-ADD     D$DNRS        CEVNRS
423300030114     C                   Z-ADD     D$DNSP        CEVNSP
423400030114     C                   MOVEL     D$DTBL        CEVTBL
423500030115     C     D$DMGS        DIV       100           CEVMES
423600030115     C* CODICE
423700030115     C                   Z-ADD     *ZEROS        CEVC01
423800030115     C                   Z-ADD     *ZEROS        CEVC02
423900030115     C                   Z-ADD     *ZEROS        CEVC03
424000030115     C                   Z-ADD     *ZEROS        CEVC04
424100030115     C                   Z-ADD     *ZEROS        CEVC05
424200030115     C                   Z-ADD     *ZEROS        CEVC06
424300030115     C                   Z-ADD     *ZEROS        CEVC07
424400030115     C                   Z-ADD     *ZEROS        CEVC08
424500030115     C                   Z-ADD     *ZEROS        CEVC09
424600030115     C                   Z-ADD     *ZEROS        CEVC10
424700030115     C                   Z-ADD     *ZEROS        CEVC11
424800030115     C                   Z-ADD     *ZEROS        CEVC12
424900030115     C                   Z-ADD     *ZEROS        CEVC13
425000030115     C                   Z-ADD     *ZEROS        CEVC14
425100030115     C                   Z-ADD     *ZEROS        CEVC15
425200030115     C                   Z-ADD     *ZEROS        CEVC16
425300030115     C                   Z-ADD     *ZEROS        CEVC17
425400030115     C                   Z-ADD     *ZEROS        CEVC18
425500030115     C                   Z-ADD     *ZEROS        CEVC19
425600030115     C                   Z-ADD     *ZEROS        CEVC20
425700030115     C                   Z-ADD     *ZEROS        CEVC21
425800030115     C                   Z-ADD     *ZEROS        CEVC22
425900030115     C                   Z-ADD     *ZEROS        CEVC23
426000030115     C                   Z-ADD     *ZEROS        CEVC24
426100030115     C                   Z-ADD     *ZEROS        CEVC25
426200030115     C                   Z-ADD     *ZEROS        CEVC26
426300030115     C                   Z-ADD     *ZEROS        CEVC27
426400030115     C                   Z-ADD     *ZEROS        CEVC28
426500030115     C                   Z-ADD     *ZEROS        CEVC29
426600030115     C                   Z-ADD     *ZEROS        CEVC30
426700030115     C                   Z-ADD     *ZEROS        CEVC31
426800030115     C                   Z-ADD     *ZEROS        CEVC32
426900030115     C                   Z-ADD     *ZEROS        CEVC33
427000030115     C                   Z-ADD     *ZEROS        CEVC34
427100030115     C                   Z-ADD     *ZEROS        CEVC35
427200030115     C                   Z-ADD     *ZEROS        CEVC36
427300030115     C                   Z-ADD     *ZEROS        CEVC37
427400030115     C                   Z-ADD     *ZEROS        CEVC38
427500030115     C                   Z-ADD     *ZEROS        CEVC39
427600030115     C                   Z-ADD     *ZEROS        CEVC40
427700030115     C                   Z-ADD     *ZEROS        CEVC41
427800030115     C                   Z-ADD     *ZEROS        CEVC42
427900030115     C                   Z-ADD     *ZEROS        CEVC43
428000030115     C                   Z-ADD     *ZEROS        CEVC44
428100030115     C                   Z-ADD     *ZEROS        CEVC45
428200030115     C                   Z-ADD     *ZEROS        CEVC46
428300030115     C                   Z-ADD     *ZEROS        CEVC47
428400030115     C                   Z-ADD     *ZEROS        CEVC48
428500030115     C                   Z-ADD     *ZEROS        CEVC49
428600030115     C                   Z-ADD     *ZEROS        CEVC50
428700030115     C* IMPORTO
428800030114     C                   Z-ADD     *ZEROS        CEVI01
428900030114     C                   Z-ADD     *ZEROS        CEVI02
429000030114     C                   Z-ADD     *ZEROS        CEVI03
429100030114     C                   Z-ADD     *ZEROS        CEVI04
429200030114     C                   Z-ADD     *ZEROS        CEVI05
429300030114     C                   Z-ADD     *ZEROS        CEVI06
429400030114     C                   Z-ADD     *ZEROS        CEVI07
429500030114     C                   Z-ADD     *ZEROS        CEVI08
429600030114     C                   Z-ADD     *ZEROS        CEVI09
429700030114     C                   Z-ADD     *ZEROS        CEVI10
429800030114     C                   Z-ADD     *ZEROS        CEVI11
429900030114     C                   Z-ADD     *ZEROS        CEVI12
430000030114     C                   Z-ADD     *ZEROS        CEVI13
430100030114     C                   Z-ADD     *ZEROS        CEVI14
430200030114     C                   Z-ADD     *ZEROS        CEVI15
430300030114     C                   Z-ADD     *ZEROS        CEVI16
430400030114     C                   Z-ADD     *ZEROS        CEVI17
430500030114     C                   Z-ADD     *ZEROS        CEVI18
430600030114     C                   Z-ADD     *ZEROS        CEVI19
430700030114     C                   Z-ADD     *ZEROS        CEVI20
430800030114     C                   Z-ADD     *ZEROS        CEVI21
430900030114     C                   Z-ADD     *ZEROS        CEVI22
431000030114     C                   Z-ADD     *ZEROS        CEVI23
431100030114     C                   Z-ADD     *ZEROS        CEVI24
431200030114     C                   Z-ADD     *ZEROS        CEVI25
431300030114     C                   Z-ADD     *ZEROS        CEVI26
431400030114     C                   Z-ADD     *ZEROS        CEVI27
431500030114     C                   Z-ADD     *ZEROS        CEVI28
431600030114     C                   Z-ADD     *ZEROS        CEVI29
431700030114     C                   Z-ADD     *ZEROS        CEVI30
431800030114     C                   Z-ADD     *ZEROS        CEVI31
431900030114     C                   Z-ADD     *ZEROS        CEVI32
432000030114     C                   Z-ADD     *ZEROS        CEVI33
432100030114     C                   Z-ADD     *ZEROS        CEVI34
432200030114     C                   Z-ADD     *ZEROS        CEVI35
432300030114     C                   Z-ADD     *ZEROS        CEVI36
432400030114     C                   Z-ADD     *ZEROS        CEVI37
432500030114     C                   Z-ADD     *ZEROS        CEVI38
432600030114     C                   Z-ADD     *ZEROS        CEVI39
432700030114     C                   Z-ADD     *ZEROS        CEVI40
432800030114     C                   Z-ADD     *ZEROS        CEVI41
432900030114     C                   Z-ADD     *ZEROS        CEVI42
433000030114     C                   Z-ADD     *ZEROS        CEVI43
433100030114     C                   Z-ADD     *ZEROS        CEVI44
433200030114     C                   Z-ADD     *ZEROS        CEVI45
433300030114     C                   Z-ADD     *ZEROS        CEVI46
433400030114     C                   Z-ADD     *ZEROS        CEVI47
433500030114     C                   Z-ADD     *ZEROS        CEVI48
433600030114     C                   Z-ADD     *ZEROS        CEVI49
433700030114     C                   Z-ADD     *ZEROS        CEVI50
433800030114     C*
433900030114     C                   ENDSR
434000030509     C*------------------------------------------------------------------------*
434100030509     C*?ADDC2V - IMPOSTA I CAMPI DEL FILE ECC2V30C (VOCI COOP ESTENSIONE)
434200030509     C*------------------------------------------------------------------------*
434300030509     C     ADDC2V        BEGSR
434400030509     C*
434500030509     C* SCRIVE IL FILE VOCI COOP ESTENSIONE SOLO SE PIÙ DI 50 VOCI
434600030509IF  1C     ULTCD         IFGT      *ZEROS
434700030509     C*
434800030509     C* CICLO PER TUTTE LE VOCI OLTRE 50
434900030509     C                   Z-ADD     *ZEROS        WCD               5 0
435000030512     C                   EVAL      WCD = ULTCD + 1
435100030509DO  2C     WCD           DO        999           CD
435200030509IF  3C     IMDV(CD)      IFGT      *ZEROS
435300030509     C*
435400030509     C* MEMORIZZA CODICE E IMPORTO NEL PRIMO CAMPO LIBERO
435500030509IF  4C                   IF        C2VI51 = *ZEROS
435600030509     C                   Z-ADD     CD            C2VC51
435700030509     C                   Z-ADD     IMDV(CD)      C2VI51
435800030509     C                   ITER
435900030509E   4C                   ENDIF
436000030509IF  4C                   IF        C2VI52 = *ZEROS
436100030509     C                   Z-ADD     CD            C2VC52
436200030509     C                   Z-ADD     IMDV(CD)      C2VI52
436300030509     C                   ITER
436400030509E   4C                   ENDIF
436500030509IF  4C                   IF        C2VI53 = *ZEROS
436600030509     C                   Z-ADD     CD            C2VC53
436700030509     C                   Z-ADD     IMDV(CD)      C2VI53
436800030509     C                   ITER
436900030509E   4C                   ENDIF
437000030509IF  4C                   IF        C2VI54 = *ZEROS
437100030509     C                   Z-ADD     CD            C2VC54
437200030509     C                   Z-ADD     IMDV(CD)      C2VI54
437300030509     C                   ITER
437400030509E   4C                   ENDIF
437500030509IF  4C                   IF        C2VI55 = *ZEROS
437600030509     C                   Z-ADD     CD            C2VC55
437700030509     C                   Z-ADD     IMDV(CD)      C2VI55
437800030509     C                   ITER
437900030509E   4C                   ENDIF
438000030509IF  4C                   IF        C2VI56 = *ZEROS
438100030509     C                   Z-ADD     CD            C2VC56
438200030509     C                   Z-ADD     IMDV(CD)      C2VI56
438300030509     C                   ITER
438400030509E   4C                   ENDIF
438500030509IF  4C                   IF        C2VI57 = *ZEROS
438600030509     C                   Z-ADD     CD            C2VC57
438700030509     C                   Z-ADD     IMDV(CD)      C2VI57
438800030509     C                   ITER
438900030509E   4C                   ENDIF
439000030509IF  4C                   IF        C2VI58 = *ZEROS
439100030509     C                   Z-ADD     CD            C2VC58
439200030509     C                   Z-ADD     IMDV(CD)      C2VI58
439300030509     C                   ITER
439400030509E   4C                   ENDIF
439500030509IF  4C                   IF        C2VI59 = *ZEROS
439600030509     C                   Z-ADD     CD            C2VC59
439700030509     C                   Z-ADD     IMDV(CD)      C2VI59
439800030509     C                   ITER
439900030509E   4C                   ENDIF
440000030509IF  4C                   IF        C2VI60 = *ZEROS
440100030509     C                   Z-ADD     CD            C2VC60
440200030509     C                   Z-ADD     IMDV(CD)      C2VI60
440300030509     C                   ITER
440400030509E   4C                   ENDIF
440500030509IF  4C                   IF        C2VI61 = *ZEROS
440600030509     C                   Z-ADD     CD            C2VC61
440700030509     C                   Z-ADD     IMDV(CD)      C2VI61
440800030509     C                   ITER
440900030509E   4C                   ENDIF
441000030509IF  4C                   IF        C2VI62 = *ZEROS
441100030509     C                   Z-ADD     CD            C2VC62
441200030509     C                   Z-ADD     IMDV(CD)      C2VI62
441300030509     C                   ITER
441400030509E   4C                   ENDIF
441500030509IF  4C                   IF        C2VI63 = *ZEROS
441600030509     C                   Z-ADD     CD            C2VC63
441700030509     C                   Z-ADD     IMDV(CD)      C2VI63
441800030509     C                   ITER
441900030509E   4C                   ENDIF
442000030509IF  4C                   IF        C2VI64 = *ZEROS
442100030509     C                   Z-ADD     CD            C2VC64
442200030509     C                   Z-ADD     IMDV(CD)      C2VI64
442300030509     C                   ITER
442400030509E   4C                   ENDIF
442500030509IF  4C                   IF        C2VI65 = *ZEROS
442600030509     C                   Z-ADD     CD            C2VC65
442700030509     C                   Z-ADD     IMDV(CD)      C2VI65
442800030509     C                   ITER
442900030509E   4C                   ENDIF
443000030509IF  4C                   IF        C2VI66 = *ZEROS
443100030509     C                   Z-ADD     CD            C2VC66
443200030509     C                   Z-ADD     IMDV(CD)      C2VI66
443300030509     C                   ITER
443400030509E   4C                   ENDIF
443500030509IF  4C                   IF        C2VI67 = *ZEROS
443600030509     C                   Z-ADD     CD            C2VC67
443700030509     C                   Z-ADD     IMDV(CD)      C2VI67
443800030509     C                   ITER
443900030509E   4C                   ENDIF
444000030509IF  4C                   IF        C2VI68 = *ZEROS
444100030509     C                   Z-ADD     CD            C2VC68
444200030509     C                   Z-ADD     IMDV(CD)      C2VI68
444300030509     C                   ITER
444400030509E   4C                   ENDIF
444500030509IF  4C                   IF        C2VI69 = *ZEROS
444600030509     C                   Z-ADD     CD            C2VC69
444700030509     C                   Z-ADD     IMDV(CD)      C2VI69
444800030509     C                   ITER
444900030509E   4C                   ENDIF
445000030509IF  4C                   IF        C2VI70 = *ZEROS
445100030509     C                   Z-ADD     CD            C2VC70
445200030509     C                   Z-ADD     IMDV(CD)      C2VI70
445300030509     C                   ITER
445400030509E   4C                   ENDIF
445500030509IF  4C                   IF        C2VI71 = *ZEROS
445600030509     C                   Z-ADD     CD            C2VC71
445700030509     C                   Z-ADD     IMDV(CD)      C2VI71
445800030509     C                   ITER
445900030509E   4C                   ENDIF
446000030509IF  4C                   IF        C2VI72 = *ZEROS
446100030509     C                   Z-ADD     CD            C2VC72
446200030509     C                   Z-ADD     IMDV(CD)      C2VI72
446300030509     C                   ITER
446400030509E   4C                   ENDIF
446500030509IF  4C                   IF        C2VI73 = *ZEROS
446600030509     C                   Z-ADD     CD            C2VC73
446700030509     C                   Z-ADD     IMDV(CD)      C2VI73
446800030509     C                   ITER
446900030509E   4C                   ENDIF
447000030509IF  4C                   IF        C2VI74 = *ZEROS
447100030509     C                   Z-ADD     CD            C2VC74
447200030509     C                   Z-ADD     IMDV(CD)      C2VI74
447300030509     C                   ITER
447400030509E   4C                   ENDIF
447500030509IF  4C                   IF        C2VI75 = *ZEROS
447600030509     C                   Z-ADD     CD            C2VC75
447700030509     C                   Z-ADD     IMDV(CD)      C2VI75
447800030509     C                   ITER
447900030509E   4C                   ENDIF
448000030509     C*
448100030509E   3C                   ENDIF
448200030509E   2C                   ENDDO
448300030509     C*
448400030509     C     D$DMGS        DIV       100           C2VMES
448500030509E   1C                   ENDIF
448600030509     C*
448700030509     C                   ENDSR
448800030509     C*------------------------------------------------------------------------*
448900030509     C*?AZZC2V - AZZERA I CAMPI DEL FILE ECC2V30C (VOCI COOP ESTENSIONE)
449000030509     C*------------------------------------------------------------------------*
449100030509     C     AZZC2V        BEGSR
449200030509     C*
449300030509     C                   Z-ADD     D$DAAS        C2VAAS
449400030509     C                   Z-ADD     D$DLNP        C2VLNP
449500030509     C                   Z-ADD     D$DNRS        C2VNRS
449600030509     C                   Z-ADD     D$DNSP        C2VNSP
449700030509     C                   MOVEL     D$DTBL        C2VTBL
449800030509     C     D$DMGS        DIV       100           C2VMES
449900030509     C* CODICE
450000030509     C                   Z-ADD     *ZEROS        C2VC51
450100030509     C                   Z-ADD     *ZEROS        C2VC52
450200030509     C                   Z-ADD     *ZEROS        C2VC53
450300030509     C                   Z-ADD     *ZEROS        C2VC54
450400030509     C                   Z-ADD     *ZEROS        C2VC55
450500030509     C                   Z-ADD     *ZEROS        C2VC56
450600030509     C                   Z-ADD     *ZEROS        C2VC57
450700030509     C                   Z-ADD     *ZEROS        C2VC58
450800030509     C                   Z-ADD     *ZEROS        C2VC59
450900030509     C                   Z-ADD     *ZEROS        C2VC60
451000030509     C                   Z-ADD     *ZEROS        C2VC61
451100030509     C                   Z-ADD     *ZEROS        C2VC62
451200030509     C                   Z-ADD     *ZEROS        C2VC63
451300030509     C                   Z-ADD     *ZEROS        C2VC64
451400030509     C                   Z-ADD     *ZEROS        C2VC65
451500030509     C                   Z-ADD     *ZEROS        C2VC66
451600030509     C                   Z-ADD     *ZEROS        C2VC67
451700030509     C                   Z-ADD     *ZEROS        C2VC68
451800030509     C                   Z-ADD     *ZEROS        C2VC69
451900030509     C                   Z-ADD     *ZEROS        C2VC70
452000030509     C                   Z-ADD     *ZEROS        C2VC71
452100030509     C                   Z-ADD     *ZEROS        C2VC72
452200030509     C                   Z-ADD     *ZEROS        C2VC73
452300030509     C                   Z-ADD     *ZEROS        C2VC74
452400030509     C                   Z-ADD     *ZEROS        C2VC75
452500030509     C* IMPORTO
452600030509     C                   Z-ADD     *ZEROS        C2VI51
452700030509     C                   Z-ADD     *ZEROS        C2VI52
452800030509     C                   Z-ADD     *ZEROS        C2VI53
452900030509     C                   Z-ADD     *ZEROS        C2VI54
453000030509     C                   Z-ADD     *ZEROS        C2VI55
453100030509     C                   Z-ADD     *ZEROS        C2VI56
453200030509     C                   Z-ADD     *ZEROS        C2VI57
453300030509     C                   Z-ADD     *ZEROS        C2VI58
453400030509     C                   Z-ADD     *ZEROS        C2VI59
453500030509     C                   Z-ADD     *ZEROS        C2VI60
453600030509     C                   Z-ADD     *ZEROS        C2VI61
453700030509     C                   Z-ADD     *ZEROS        C2VI62
453800030509     C                   Z-ADD     *ZEROS        C2VI63
453900030509     C                   Z-ADD     *ZEROS        C2VI64
454000030509     C                   Z-ADD     *ZEROS        C2VI65
454100030509     C                   Z-ADD     *ZEROS        C2VI66
454200030509     C                   Z-ADD     *ZEROS        C2VI67
454300030509     C                   Z-ADD     *ZEROS        C2VI68
454400030509     C                   Z-ADD     *ZEROS        C2VI69
454500030509     C                   Z-ADD     *ZEROS        C2VI70
454600030509     C                   Z-ADD     *ZEROS        C2VI71
454700030509     C                   Z-ADD     *ZEROS        C2VI72
454800030509     C                   Z-ADD     *ZEROS        C2VI73
454900030509     C                   Z-ADD     *ZEROS        C2VI74
455000030509     C                   Z-ADD     *ZEROS        C2VI75
455100030509     C*
455200030509     C                   ENDSR
455300980313     C*------------------------------------------------------------------------*
455400980325     C*?AGGFIM - AGGIORNA FILE ECFIM30C (MC RIFATTURAZIONE ITL)
455500980313     C*------------------------------------------------------------------------*
455600980313     C     AGGFIM        BEGSR
455700980313     C*
455800980313     C* SOLO SE PRIMA O DOPO SONO DA GESTIRE
455900980313IF  1C     WMPREG        IFEQ      'S'
456000980313     C     WMDREG        OREQ      'S'
456100980313     C*
456200980317     C* SOLO SE MC TOTALI LETTI DA ECFIM (CON DATA=0 E NON) DIVERSI MC DOPO
456300980323IF  2C     WPSMCT        IFNE      WDSMC
456400980313     C*
456500980317     C* TROVA DIFFERENZA DA SCRIVERE SUL FILE (VOLUME DOPO - VOLUME LETTO DA
456600980317     C* ECFIM SENZA REC CON DATA = 0)
456700980323     C     WDSMC         SUB       WPSMC         WSMC
456800980313     C*
456900980317     C                   Z-ADD     D$DAAS        FIMAAS
457000980317     C                   Z-ADD     D$DLNP        FIMLNP
457100980317     C                   Z-ADD     D$DNRS        FIMNRS
457200980317     C                   Z-ADD     D$DNSP        FIMNSP
457300980317     C                   MOVEL     D$DTBL        FIMTBL
457400980317     C                   Z-ADD     *ZEROS        FIMDDO
457500980325     C     KEYFIM        CHAIN     ECFIM30C                           99
457600980317     C* RECORD GIA' ESISTENTE
457700980317IF  3C     *IN99         IFEQ      '0'
457800980317     C* SE DIFFERENZA = ZERO CANC. REC
457900980317IF  4C     WSMC          IFEQ      *ZEROS
458000980317     C                   DELETE    ECFIM000
458100990408X   4C                   ELSE
458200980317     C* SE DIFF. <> ZERO RISCRIVO RECORD CON NUOVA DIFFERENZA
458300980319     C                   Z-ADD     D$DTFP        FIMTFP
458400980319     C                   Z-ADD     D$DTFA        FIMTFA
458500980317     C                   Z-ADD     WSMC          FIMSMC
458600980317     C                   UPDATE    ECFIM000
458700980317E   4C                   ENDIF
458800980317X   3C                   ELSE
458900980317     C* RECORD NON ESISTENTE, SCRIVO
459000980319     C                   Z-ADD     D$DTFP        FIMTFP
459100980319     C                   Z-ADD     D$DTFA        FIMTFA
459200980317     C                   Z-ADD     WSMC          FIMSMC
459300990408     C                   Z-ADD     D$DMGS        DSMGS
459400990408     C                   Z-ADD     DSMES         FIMMES
459500980317     C                   WRITE     ECFIM000
459600980317E   3C                   ENDIF
459700980313E   2C                   ENDIF
459800980313E   1C                   ENDIF
459900950913     C*
460000950913     C                   ENDSR
460100020531     C*------------------------------------------------------------------------*
460200020531     C*?AGGFCE - AGGIORNA IL FILE FIFCE00F (CONTEGGI PADRONCINI)
460300020531     C*------------------------------------------------------------------------*
460400020531     C     AGGFCE        BEGSR
460500020531     C*
460600020531     C* SOLO SE FLAG AGG. CONTO ECON. = SI
460700020531IF  1C     D$GFCE        IFEQ      'S'
460800020531     C*---
460900020531     C* RITIRI
461000020822     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
461100020531     C* . Controlla che il padroncino ritiro sia un dipendente
461200031031     C* . Controlla che la LNP sia uguale alla linea del padroncino
461300020531     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
461400020531     C*---
461500020822IF  2C                   IF        (D$DDRT > §BLCCLR) OR
461600020822     C                             (D$DDRT = *zeros)
461700021205     C                   MOVEL     'A'           APDTIP
461800021205     C                   Z-ADD     D$DPDR        APDPDR
461900021205     C     KEYAPD        CHAIN     FIAPD01L                           99
462000031031     C                   MOVEL     D$DPDR        N3                3 0
462100020822IF  3C     *IN99         IFEQ      *OFF
462200040510     C*    APDPDD        ANDEQ     'S'
462300031031     C     N3            ANDEQ     D$DLNP
462400040510     c* creo la spia nel FIFCE se padroncino escluso da autofatturazione
462500040511     c* oppure se padroncino non escluso ma tipo bolla FY modifica
462600040510     c* il cod. autotrasp. con uno fittizio
462700040510   a3c                   if        apdpdd = 'S' or
462800040511     c                             (apdpdd <>'S' and d$dcbo = 'FY')
462900020531     C                   MOVEL     D$DPDR        N3                3 0
463000020531     C                   Z-ADD     N3            FCEFGS
463100040510     c                   if        apdpdd = 'S'
463200020531     C                   Z-ADD     D$DPDR        FCEPDR
463300040510     c                   else
463400040510     c                   movel     fcefgs        fcepdr
463500040510     c                   move      0998          fcepdr
463600040510     c                   end
463700020531     C                   MOVEL     'R'           FCETSR
463800020531     C                   EVAL      FCENDC = (5000 + D$DNRT)
463900020531     C                   Z-ADD     D$DDRT        FCEDDC
464000020531     C                   Z-ADD     D$DAAS        FCEAAS
464100020531     C                   Z-ADD     D$DLNP        FCELNP
464200020531     C                   Z-ADD     D$DNRS        FCENRS
464300020531     C                   Z-ADD     D$DNSP        FCENSP
464400020531     C                   MOVEL     '001'         FCECCE
464500020531     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
464600020822IF  4C     *IN99         IFEQ      *ON
464700020531     C                   Z-ADD     *ZEROS        FCEICE
464800020531     C                   MOVEL     *BLANKS       FCESAL
464900030404     C                   MOVEL     D$DCBO        FCECBO
465000020531     C                   WRITE     FIFCE000
465100020822E   4C                   ENDIF
465200040510E  a3C                   ENDIF
465300040510E   3C                   ENDIF
465400020822E   2C                   ENDIF
465500020531     C*---
465600020531     C* CONSEGNE
465700040512     C* . Gestisce solo le bolle consegnate tranne x i dirottamenti
465800040512     C* . che scrive la spia su fifce x pagare le competenze di consegna
465900040512     C* . anche alla bolla originale
466000020822     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
466100020531     C* . Controlla che il padroncino consegna sia un dipendene
466200031031     C* . Controlla che la LNA sia uguale alla linea del padroncino
466300020531     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
466400020531     C*---
466500040607IF  2C                   IF        d$ddcm > 0 or (D$GSTS='U' and
466600040607IF  2C                             d$ddcm = 0 and d$dcca = '1' and
466700040512IF  2C                              d$dddc > 0 and d$pddc = 0)
466800020822IF  3C                   IF        (D$DDDC > §BLCCLR) OR
466900020822     C                             (D$DDDC = *zeros)
467000021205     C                   MOVEL     'A'           APDTIP
467100021205     C                   Z-ADD     D$DPDC        APDPDR
467200021205     C     KEYAPD        CHAIN     FIAPD01L                           99
467300031031     C                   MOVEL     D$DPDC        N3                3 0
467400020822IF  4C     *IN99         IFEQ      *OFF
467500020531     C     APDPDD        ANDEQ     'S'
467600031031     C     N3            ANDEQ     D$DLNA
467700031113IF  5C     D$GSTS        IFEQ      'U'
467800031113     C     D$PDCM        ANDEQ     *ZEROS
467900020531     C                   MOVEL     D$DPDC        N3                3 0
468000020531     C                   Z-ADD     N3            FCEFGS
468100020531     C                   Z-ADD     D$DPDC        FCEPDR
468200020531     C                   MOVEL     'C'           FCETSR
468300020531     C                   Z-ADD     D$DNDC        FCENDC
468400020531     C                   Z-ADD     D$DDDC        FCEDDC
468500020531     C                   Z-ADD     D$DAAS        FCEAAS
468600020531     C                   Z-ADD     D$DLNP        FCELNP
468700020531     C                   Z-ADD     D$DNRS        FCENRS
468800020531     C                   Z-ADD     D$DNSP        FCENSP
468900020531     C                   MOVEL     '005'         FCECCE
469000020531     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
469100031113IF  6C     *IN99         IFEQ      *ON
469200020531     C                   Z-ADD     *ZEROS        FCEICE
469300020531     C                   MOVEL     *BLANKS       FCESAL
469400030404     C                   MOVEL     D$DCBO        FCECBO
469500020531     C                   WRITE     FIFCE000
469600031113E   6C                   ENDIF
469700031113E   5C                   ENDIF
469800020822E   4C                   ENDIF
469900020531E   3C                   ENDIF
470000020531E   2C                   ENDIF
470100020531     C*
470200020531E   1C                   ENDIF
470300020531     C*
470400020531     C                   ENDSR
470500980317     C*------------------------------------------------------------------------*
470600980325     C*?AGGFIC - AGGIORNA FILE ECFIC30C (COMPAET. RIFATTURAZIONE BAR/SDI)
470700980317     C*------------------------------------------------------------------------*
470800011127     C*---       AGGFIC    BEGSR
470900011127     C*---
471000011127     C*-SOLO SE PRIMA O DOPO SONO DA GESTIRE
471100011127IF  1C*---       WIPREG    IFEQ 'S'
471200011127     C*---       WIDREG    OREQ 'S'
471300011127     C*---
471400011127     C*-SOLO SE LE COMPET.LETTE DA ECFIC (CON DATA=0 E NON) DIVERSE COMPET.DOPO
471500011127IF  2C*---       WPSCOT    IFNE WDSCO                      *COMPET.
471600011127     C*---
471700011127     C*-TROVA DIFFERENZA DA SCRIVERE SUL FILE (COMPET.DOPO - COMPET.LETTE DA
471800011127     C*-ECFIC SENZA REC CON DATA = 0)
471900011127     C*---       WDSCO     SUB  WPSCO     WSCO             *MC DA SCRIVERE
472000011127     C*---
472100011127     C*---                 Z-ADDD$DAAS    FICAAS           ANNO SPEDIZIONE
472200011127     C*---                 Z-ADDD$DLNP    FICLNP           LINEA DI PARTENZA
472300011127     C*---                 Z-ADDD$DNRS    FICNRS           NUMERO SERIE
472400011127     C*---                 Z-ADDD$DNSP    FICNSP           NUMERO SPEDIZIONE
472500011127     C*---                 MOVELD$DTBL    FICTBL           TIPO BOLLA
472600011127     C*---                 Z-ADD*ZEROS    FICDDO           DATA DOCUMENTO
472700011127     C*---       KEYFIC    CHAINECFIC30C             99
472800011127     C*-RECORD GIA' ESISTENTE
472900011127IF  3C*---       *IN99     IFEQ '0'
473000011127     C*-SE DIFFERENZA = ZERO CANC. REC
473100011127IF  4C*---       WSCO      IFEQ *ZEROS
473200011127     C*---                 DELETECFIC000
473300011127X   4C*---                 ELSE
473400011127     C*-SE DIFF. <> ZERO RISCRIVO RECORD CON NUOVA DIFFERENZA
473500011127     C*---                 Z-ADDWSCO      FICSCO           *SALDO COMPET.
473600011127     C*---                 UPDATECFIC000
473700011127E   4C*---                 ENDIF
473800011127X   3C*---                 ELSE
473900011127     C*-RECORD NON ESISTENTE, SCRIVO
474000011127     C*---                 Z-ADDWSCO      FICSCO           *SALDO COMPET.
474100011127     C*---                 Z-ADDD$DMGS    DSMGS
474200011127     C*---                 Z-ADDDSMES     FICMES           *MESE SPED.
474300011127     C*---                 WRITEECFIC000
474400011127E   3C*---                 ENDIF
474500011127E   2C*---                 ENDIF
474600011127E   1C*---                 ENDIF
474700011127     C*---
474800011127     C*---                 ENDSR
474900940315     C********************************************************************
475000030114     C*   GESCSP - GESTIONE DELLE CONSEGNE PARTICOLARI
475100990823     C*   INPUT: - SCHIERE SVN,VAN + CAMPI WPFTC,WPTC2,WPFGC,WPFCA,WPIAS
475200940330     C*   OUTOUT:- SCHIERE SVX,IVX,NSX,CPX
475300940315     C********************************************************************
475400030114     C     GESCSP        BEGSR
475500941123     C*
475600941123     C* AZZERAMENTI INIZIALI
475700941123     C                   Z-ADD     *ZEROS        P                 6 0
475800941123     C                   MOVEA     *BLANKS       SVX
475900941123     C                   Z-ADD     *ZEROS        IVX
476000941123     C                   Z-ADD     *ZEROS        NSX
476100941123     C                   MOVEA     *BLANKS       CPX
476200941123     C                   MOVEL     *BLANKS       WPVP1
476300941123     C                   MOVEL     *BLANKS       WPVP2
476400941123     C*
476500941123     C* CERCO LA VARIA DELLA PRIMA CONS.PARTICOLARE
476600941123     C     WPFTC         IFNE      *BLANKS
476700941123     C                   Z-ADD     1             I
476800941123     C     WPFTC         LOOKUP    C1P(I)                                 99
476900941123     C     *IN99         IFEQ      '1'
477000941123     C                   MOVEL     V1P(I)        WPVP1             1
477100941123     C                   ENDIF
477200941123     C                   ENDIF
477300941123     C*
477400941123     C* CERCO LA VARIA DELLA SECONDA CONS.PARTICOLARE
477500941123     C     WPTC2         IFNE      *BLANKS
477600941123     C                   Z-ADD     1             I
477700941123     C     WPTC2         LOOKUP    C1P(I)                                 99
477800941123     C     *IN99         IFEQ      '1'
477900941123     C                   MOVEL     V1P(I)        WPVP2             1
478000941123     C                   ENDIF
478100941123     C                   ENDIF
478200941123     C*
478300941123     C* PORTA VARIE ED IMPORTI DALLA SCHIERA INPUT ALLA SCHIERA OUTPUT
478400970902DO  1C     1             DO        20            I
478500990831     C* ESCLUDE ALCUNE VARIE PER LA REGISTRAZIONE DU SISDP
478600990831IF  2C     SVN(I)        IFEQ      *BLANKS
478700050119     C*    SVN(I)        OREQ      'T'
478800990831     C     SVN(I)        OREQ      '6'
478900990831     C     SVN(I)        OREQ      '8'
479000990831X   2C                   ELSE
479100941123     C                   ADD       1             P
479200970902IF  3C     SVN(I)        IFEQ      'E'
479300970901     C                   MOVEL     'R'           SVX(P)
479400970902X   3C                   ELSE
479500941130     C                   MOVEL     SVN(I)        SVX(P)
479600970902E   3C                   ENDIF
479700941130     C                   Z-ADD     VAN(I)        IVX(P)
479800970901     C* SE IL N. DI SPED. CON QUESTA VARIA E DETERMINABILE IN ALTRO MODO
479900020620     C* (o come consegna particolare o testando i campi della bolla)
480000970901     C* (VEDI RIGHE SUCCESSIVE) IMPOSTA IL N.DI SPED. DOPO
480100970901     C* ALTRIMENTI FORZA 1
480200970902IF  3C     SVN(I)        IFEQ      'A'
480300970901     C     SVN(I)        OREQ      §$2VRB
480400970901     C     SVN(I)        OREQ      'F'
480500970901     C     SVN(I)        OREQ      §$2VRG
480600970901     C     SVN(I)        OREQ      §$2VRI
480700970901     C     SVN(I)        OREQ      §$2VRJ
480800970901     C     SVN(I)        OREQ      §$2VRK
480900970901     C     SVN(I)        OREQ      'P'
481000970901     C     SVN(I)        OREQ      §$2VRQ
481100970901     C     SVN(I)        OREQ      §$2VRR
481200970901     C     SVN(I)        OREQ      'S'
481300011109     C     SVN(I)        OREQ      '3'
481400011109     C     SVN(I)        OREQ      '4'
481500020620     C     SVN(I)        OREQ      'X'
481600021014     C     SVN(I)        OREQ      §$2VRY
481700021014     C     SVN(I)        OREQ      §$2AST
481800040322     C     SVN(I)        OREQ      §$2vdf
481900040615     C     SVN(I)        OREQ      §$2vro
482000040924     C     SVN(I)        OREQ      §$2pod
482100040616     C     SVN(I)        OREQ      Wpvp1
482200040616     C     SVN(I)        OREQ      Wpvp2
482300050119     C     SVN(I)        OREQ      §$2vrt
482400120301     C     SVN(I)        OREQ      §$2VAD
482500970902X   3C                   ELSE
482600020620     C*
482700020620     c* QUI CI STANNO :
482800020620     C* - Diritto di pesatura (in TNTAM);
482900020620     C*
483000970901     C                   Z-ADD     1             NSX(P)
483100970902E   3C                   ENDIF
483200970902E   2C                   ENDIF
483300970902E   1C                   ENDDO
483400970901     C*
483500970901     C* PORTA VALORI FISSI DEI RICAVI DELLA BOLLA ALLA SCHIERA OUTPUT
483600990823     C********** WPDIF     IFNE *ZEROS                     *DIRITTO FISSO
483700990823     C******************** ADD  1         P
483800990823     C******************** MOVEL§$2VRH    SVX,P
483900990823     C******************** Z-ADDWPDIF     IVX,P
484000990823     C******************** Z-ADD1         NSX,P
484100990823     C******************** ENDIF
484200990823     C********** WPANT     IFNE *ZEROS                     *ANTICIPATA
484300990823     C******************** ADD  1         P
484400990823     C******************** MOVEL§$2VRT    SVX,P
484500990823     C******************** Z-ADDWPANT     IVX,P
484600990823     C******************** Z-ADD1         NSX,P
484700990823     C******************** ENDIF
484800970901     C     WPPOR         IFNE      *ZEROS
484900970901     C                   ADD       1             P
485000970901     C                   MOVEL     §$2FPO        SVX(P)
485100970901     C                   Z-ADD     WPPOR         IVX(P)
485200970901     C                   Z-ADD     1             NSX(P)
485300970901     C                   ENDIF
485400990823     C********** WPINL     IFNE *ZEROS                     *INOLTRO
485500990823     C******************** ADD  1         P
485600990823     C******************** MOVEL§$2FIN    SVX,P
485700990823     C******************** Z-ADDWPINL     IVX,P
485800990823     C******************** Z-ADD1         NSX,P
485900990823     C******************** ENDIF
486000941123     C*
486100941123     C* PRIMA CONS.PARTICOLARE
486200941123     C     WPVP1         IFNE      *BLANKS
486300941123     C                   Z-ADD     1             I
486400941123     C     WPVP1         LOOKUP    SVX(I)                                 99
486500941123     C     *IN99         IFEQ      '1'
486600941123     C                   ADD       1             NSX(I)
486700941123     C                   MOVEL     WPFTC         CPX(I)
486800941123     C                   ELSE
486900941123     C                   ADD       1             P
487000941123     C                   MOVEL     WPVP1         SVX(P)
487100941123     C                   Z-ADD     1             NSX(P)
487200941123     C                   MOVEL     WPFTC         CPX(P)
487300941123     C                   ENDIF
487400941123     C                   ENDIF
487500941123     C*
487600941123     C* SECONDA CONS.PARTICOLARE
487700941123     C     WPVP2         IFNE      *BLANKS
487800941123     C                   Z-ADD     1             I
487900941123     C     WPVP2         LOOKUP    SVX(I)                                 99
488000941123     C     *IN99         IFEQ      '1'
488100941123     C                   ADD       1             NSX(I)
488200941123     C                   MOVEL     WPTC2         CPX(I)
488300941123     C                   ELSE
488400941123     C                   ADD       1             P
488500941123     C                   MOVEL     WPVP2         SVX(P)
488600941123     C                   Z-ADD     1             NSX(P)
488700941123     C                   MOVEL     WPTC2         CPX(P)
488800941123     C                   ENDIF
488900941123     C                   ENDIF
489000941123     C*
489100941123     C* GIACENZA
489200941123     C     WPFGC         IFEQ      'S'
489300941123     C                   Z-ADD     1             I
489400970901     C     §$2VRI        LOOKUP    SVX(I)                                 99
489500941123     C     *IN99         IFEQ      '1'
489600941123     C                   ADD       1             NSX(I)
489700941123     C                   ELSE
489800941123     C                   ADD       1             P
489900970901     C                   MOVEL     §$2VRI        SVX(P)
490000941123     C                   Z-ADD     1             NSX(P)
490100941123     C                   ENDIF
490200941123     C                   ENDIF
490300040322     C*
490400040323     C* numero Lasciato avviso SOLO A PARTIRE DA APRILE 2004
490500040322     C                   IF        %subst(wpflo: 6: 1) <> ' '
490600040323     C                             AND (WPDATCOP > 20040400 OR
490700040323     C                             WDDATCOP > 20040400)
490800040322     c                   eval      dtasflo = wpflo
490900040322     c                   move      §flonav       contavv
491000040322     C                   Z-ADD     1             I
491100040322     C     §$2Vdf        LOOKUP    SVX(I)                                 99
491200040322     C     *IN99         IFEQ      '1'
491300040322     C                   ADD       contavv       NSX(I)
491400040322     C                   ELSE
491500040322     C                   ADD       1             P
491600040322     C                   MOVEL     §$2Vdf        SVX(P)
491700040322     C                   Z-ADD     contavv       NSX(P)
491800040322     C                   ENDIF
491900040322     C                   ENDIF
492000120301     C*
492100120301     C* numero mail destinatario
492200120301     C                   IF        %subst(wpflo: 9: 1) <> ' '
492300120301     C                   Z-ADD     1             I
492400120301     C     §$2VAD        LOOKUP    SVX(I)                                 99
492500120301     C     *IN99         IFEQ      '1'
492600120301     C                   ADD       1             NSX(I)
492700120301     C                   ELSE
492800120301     C                   ADD       1             P
492900120301     C                   MOVEL     §$2VAD        SVX(P)
493000120301     C                   Z-ADD     1             NSX(P)
493100120301     C                   ENDIF
493200120301     C                   ENDIF
493300941123     C*
493400941123     C* CONTRASSEGNO
493500941123     C     WPFCA         IFEQ      'S'
493600040524     C     WPsta         andne     ' '
493700941123     C                   Z-ADD     1             I
493800970901     C     §$2VRG        LOOKUP    SVX(I)                                 99
493900941123     C     *IN99         IFEQ      '1'
494000941123     C                   ADD       1             NSX(I)
494100941123     C                   ELSE
494200941123     C                   ADD       1             P
494300970901     C                   MOVEL     §$2VRG        SVX(P)
494400941123     C                   Z-ADD     1             NSX(P)
494500941123     C                   ENDIF
494600941123     C                   ENDIF
494700941123     C*
494800941123     C* ASSIC. X CONTO
494900990823     C********** WPFAS     IFEQ 'S'
495000990823     C     WPIAS         IFGT      *ZEROS
495100941123     C                   Z-ADD     1             I
495200970901     C     §$2VRR        LOOKUP    SVX(I)                                 99
495300941123     C     *IN99         IFEQ      '1'
495400941123     C                   ADD       1             NSX(I)
495500941123     C                   ELSE
495600941123     C                   ADD       1             P
495700970901     C                   MOVEL     §$2VRR        SVX(P)
495800941123     C                   Z-ADD     1             NSX(P)
495900941123     C                   ENDIF
496000941123     C                   ENDIF
496100970901     C*
496200970901     C* CONSEGNA DDT -VARIA B-
496300970901     C     WPLL1         IFEQ      'S'
496400970901     C     WPLL1         OREQ      'C'
496500970901     C                   Z-ADD     1             I
496600970901     C     §$2VRB        LOOKUP    SVX(I)                                 99
496700970901     C     *IN99         IFEQ      '1'
496800970901     C                   ADD       1             NSX(I)
496900970901     C                   ELSE
497000970901     C                   ADD       1             P
497100970901     C                   MOVEL     §$2VRB        SVX(P)
497200970901     C                   Z-ADD     1             NSX(P)
497300970901     C                   ENDIF
497400970901     C                   ENDIF
497500970901     C*
497600970901     C* ISOLA -VARIA J-
497700970902     C     WPDFT         IFGT      19970713
497800970901     C     WPTPO         IFEQ      'F'
497900970901     C     WPFIN         ANDEQ     'I'
498000970901     C     WPTPO         OREQ      'A'
498100970901     C     WPFAP         ANDEQ     'I'
498200970901     C                   Z-ADD     1             I
498300970901     C     §$2VRJ        LOOKUP    SVX(I)                                 99
498400970901     C     *IN99         IFEQ      '1'
498500970901     C                   ADD       1             NSX(I)
498600970901     C                   ELSE
498700970901     C                   ADD       1             P
498800970901     C                   MOVEL     §$2VRJ        SVX(P)
498900970901     C                   Z-ADD     1             NSX(P)
499000970901     C                   ENDIF
499100970901     C                   ENDIF
499200970901     C                   ENDIF
499300970901     C*
499400970901     C* LOC.DISAGIATE -VARIA K-
499500970901     C     WPTPO         IFEQ      'F'
499600970901     C     WPFIN         ANDEQ     'D'
499700970901     C     WPTPO         OREQ      'A'
499800970901     C     WPFAP         ANDEQ     'D'
499900970901     C                   Z-ADD     1             I
500000970901     C     §$2VRK        LOOKUP    SVX(I)                                 99
500100970901     C     *IN99         IFEQ      '1'
500200970901     C                   ADD       1             NSX(I)
500300970901     C                   ELSE
500400970901     C                   ADD       1             P
500500970901     C                   MOVEL     §$2VRK        SVX(P)
500600970901     C                   Z-ADD     1             NSX(P)
500700970901     C                   ENDIF
500800970901     C                   ENDIF
500900970901     C*
501000110919     c* non conteggiamo + la città ma solo la ZTL della città o della
501100110919     c* provincia (da ottobre 2011)
501200110919     C* ZTL -VARIA Q-
501300110919     C*    WPTPO         IFEQ      'F'
501400110919     C*    WPFIN         ANDEQ     'C'
501500110919     C*    WPTPO         OREQ      'A'
501600110919     C*    WPFAP         ANDEQ     'C'
501700110919     c                   if        (wptpo = 'F' and (wpfin = 'Z' or
501800110919     c                             wpfin = 'T')) or
501900110919     c                             (wptpo = 'A' and (wpfap = 'Z' or
502000110919     c                             wpfap = 'T'))
502100970901     C                   Z-ADD     1             I
502200970901     C     §$2VRQ        LOOKUP    SVX(I)                                 99
502300970901     C     *IN99         IFEQ      '1'
502400970901     C                   ADD       1             NSX(I)
502500970901     C                   ELSE
502600970901     C                   ADD       1             P
502700970901     C                   MOVEL     §$2VRQ        SVX(P)
502800970901     C                   Z-ADD     1             NSX(P)
502900970901     C                   ENDIF
503000970901     C                   ENDIF
503100020620     C*
503200020620     C* RICERCA DOCUMENTO -VARIA O-
503300040617     C*    WPTBL         IFEQ      'A3'
503400040617     C*    WPCBO         ANDEQ     'FO'
503500040617     C                   IF        WPTBL = 'A3' AND (WPCBO = 'FO' OR
503600040617     C                             WPCBO = 'FF')
503700020620     C                   Z-ADD     1             I
503800020620     C     §$2VRO        LOOKUP    SVX(I)                                 99
503900020620     C     *IN99         IFEQ      '1'
504000020620     C                   ADD       1             NSX(I)
504100020620     C                   ELSE
504200020620     C                   ADD       1             P
504300020620     C                   MOVEL     §$2VRO        SVX(P)
504400020620     C                   Z-ADD     1             NSX(P)
504500020620     C                   ENDIF
504600020620     C                   ENDIF
504700020620     C*
504800020620     C* RITIRO ANNULLATO -VARIA Y-
504900020620     C     WPTBL         IFEQ      'A3'
505000020620     C     WPCBO         ANDEQ     'FY'
505100020620     C                   Z-ADD     1             I
505200020620     C     §$2VRY        LOOKUP    SVX(I)                                 99
505300020620     C     *IN99         IFEQ      '1'
505400020620     C                   ADD       1             NSX(I)
505500020620     C                   ELSE
505600020620     C                   ADD       1             P
505700020620     C                   MOVEL     §$2VRY        SVX(P)
505800020620     C                   Z-ADD     1             NSX(P)
505900020620     C                   ENDIF
506000020620     C                   ENDIF
506100020620     C*
506200020620     C* DIROTTAMENTO -VARIA *-
506300020620     C     WPTBL         IFEQ      'A3'
506400020620     C     WPCBO         ANDEQ     'F*'
506500020620     C                   Z-ADD     1             I
506600020620     C     §$2AST        LOOKUP    SVX(I)                                 99
506700020620     C     *IN99         IFEQ      '1'
506800020620     C                   ADD       1             NSX(I)
506900020620     C                   ELSE
507000020620     C                   ADD       1             P
507100020620     C                   MOVEL     §$2AST        SVX(P)
507200020620     C                   Z-ADD     1             NSX(P)
507300020620     C                   ENDIF
507400020620     C                   ENDIF
507500040924     C*
507600040924     C* P.O.D Immage -VARIA a-
507700090304     c* per pod immage conto i colli e non le spedizioni perchè ci sarà
507800090304     c* una spedizione per enne immagini
507900040924     C     WPTBL         IFEQ      'A3'
508000040924     C     WPCBO         ANDEQ     'FI'
508100040924     C                   Z-ADD     1             I
508200040924     C     §$2pod        LOOKUP    SVX(I)                                 99
508300040924     C     *IN99         IFEQ      '1'
508400090304     C                   ADD       wpncl         NSX(I)
508500040924     C                   ELSE
508600040924     C                   ADD       1             P
508700040924     C                   MOVEL     §$2pod        SVX(P)
508800090304     C                   Z-ADD     wpncl         NSX(P)
508900040924     C                   ENDIF
509000040924     C                   ENDIF
509100050119     C*
509200050119     C* ORM commissionati
509300050119     C     WPTBL         IFEQ      'A3'
509400050119     C     WPCBO         ANDEQ     'FT'
509500050119     C                   Z-ADD     1             I
509600050119     C     §$2vrt        LOOKUP    SVX(I)                                 99
509700050119     C     *IN99         IFEQ      '1'
509800050119     C                   ADD       1             NSX(I)
509900050119     C                   ELSE
510000050119     C                   ADD       1             P
510100050119     C                   MOVEL     §$2vrt        SVX(P)
510200050119     C                   Z-ADD     1             NSX(P)
510300050119     C                   ENDIF
510400050119     C                   ENDIF
510500941123     C*
510600990617     C* SE E' STATO SCRITTO ALMENO UN DATO IN SCHIERA, 'S' -> FLAG REG. SISDP
510700941123     C     P             IFGT      *ZEROS
510800941123     C                   MOVEL     'S'           WPREG             1
510900941123     C                   ELSE
511000941123     C                   MOVEL     'N'           WPREG
511100941123     C                   ENDIF
511200020705     c*
511300020705     C* Se divisa <> divisa C/E, applica il cambio della divisa C/E (YEURCO).
511400020104DO  1C     1             DO        20            I
511500020104IF  2C     SVX(I)        IFNE      *BLANKS
511600020104IF  3C     IVX(I)        IFNE      *ZEROS
511700020104IF  5C     WPDIV         IFNE      WDIVCE
511800011219     C                   Z-ADD     IVX(I)        §IMI
511900011219     C                   MOVEL     WPDIV         §DVI
512000011219     C                   EXSR      CNVEUR
512100011219     C                   Z-ADD     §IMO          IVX(I)
512200020104E   5C                   ENDIF
512300020104E   3C                   ENDIF
512400020104E   2C                   ENDIF
512500020104E   1C                   ENDDO
512600940411     C*
512700940315     C                   ENDSR
512800930323     C********************************************************************
512900990617     C*   GESSDR - GESTISCE IL FILE SISDR00F (SALDI BOLLE)
513000930323     C********************************************************************
513100931104     C     GESSDR        BEGSR
513200940411     C*
513300090304     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO regREGISTRARE
513400940411     C     WRPREG        IFEQ      'S'
513500940411     C     WRDREG        OREQ      'S'
513600931111     C*
513700931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
513800940407     C     D$GSTS        IFEQ      'U'
513900940407     C     D$PAAS        ANDEQ     D$DAAS
514000931111     C     WPMES         ANDEQ     WDMES
514100931111     C     D$PLNP        ANDEQ     D$DLNP
514200931111     C     D$PLNA        ANDEQ     D$DLNA
514300940103     C     WPFPR         ANDEQ     WDFPR
514400931111     C     D$PTSP        ANDEQ     D$DTSP
514500941201     C     D$PTPO        ANDEQ     D$DTPO
514600941201     C     D$PRBL        ANDEQ     D$DRBL
514700011219     C     WPTPR         ANDEQ     WDTPR
514800940411     C     WRPREG        ANDEQ     WRDREG
514900931111     C*
515000931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
515100931111     C*
515200931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
515300931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
515400931110     C                   Z-ADD     D$DAAS        SDRANN
515500931111     C                   Z-ADD     WDMES         SDRMES
515600931111     C                   Z-ADD     D$DLNP        SDRFLP
515700931110     C                   Z-ADD     D$DLNA        SDRFLA
515800940103     C                   MOVEL     WDFPR         SDRFPE
515900931111     C                   MOVEL     D$DTSP        SDRTSP
516000941201     C                   MOVEL     D$DTPO        SDRTPO
516100941201     C                   MOVEL     D$DRBL        SDRRBL
516200931111     C                   MOVEL     WDTPR         SDRTPR
516300931111     C* VALORI = 'DOPO' - 'PRIMA'
516400940512     C                   Z-ADD     *ZEROS        WRNSP
516500020829     C*--- D$DPKF        SUB       D$PPKF        WRPKG
516600020829     C     WDPES         SUB       WPPES         WRPES
516700931111     C     D$DNCL        SUB       D$PNCL        WRNCO
516800931111     C     WDVOL         SUB       WPVOL         WRVOL
516900931111     C     WDPTS         SUB       WPPTS         WRPTS
517000931111     C     WDIRB         SUB       WPIRB         WRIRB
517100931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
517200931111     C                   EXSR      AGGSDR
517300931111     C*
517400931111     C                   ELSE
517500931111     C*
517600931111     C******************** E' CAMBIATA LA CHIAVE *************************
517700931111     C*
517800940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
517900940405     C     D$GSTS        IFEQ      'U'
518000940405     C     D$GSTS        OREQ      'D'
518100931111     C*
518200931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
518300931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
518400931111     C* CHIAVE = CHIAVE 'PRIMA'
518500931111     C                   Z-ADD     D$PAAS        SDRANN
518600931111     C                   Z-ADD     WPMES         SDRMES
518700931111     C                   Z-ADD     D$PLNP        SDRFLP
518800931111     C                   Z-ADD     D$PLNA        SDRFLA
518900940103     C                   MOVEL     WPFPR         SDRFPE
519000931111     C                   MOVEL     D$PTSP        SDRTSP
519100941201     C                   MOVEL     D$PTPO        SDRTPO
519200941201     C                   MOVEL     D$PRBL        SDRRBL
519300931111     C                   MOVEL     WPTPR         SDRTPR
519400931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
519500931111     C     *ZEROS        SUB       1             WRNSP
519600020829     C*--- *ZEROS        SUB       D$PPKF        WRPKG
519700020829     C     *ZEROS        SUB       WPPES         WRPES
519800931111     C     *ZEROS        SUB       D$PNCL        WRNCO
519900931111     C     *ZEROS        SUB       WPVOL         WRVOL
520000931111     C     *ZEROS        SUB       WPPTS         WRPTS
520100931111     C     *ZEROS        SUB       WPIRB         WRIRB
520200931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
520300940411     C     WRPREG        IFEQ      'S'
520400931111     C                   EXSR      AGGSDR
520500940411     C                   ENDIF
520600931111     C*
520700931111     C                   END
520800940202     C*
520900940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
521000940405     C     D$GSTS        IFEQ      'U'
521100940405     C     D$GSTS        OREQ      'I'
521200931111     C*
521300940202     C* NON E' UN ANNULLAMENTO, QUINDI
521400931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
521500931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
521600931111     C* CHIAVE = CHIAVE 'DOPO'
521700931111     C                   Z-ADD     D$DAAS        SDRANN
521800931111     C                   Z-ADD     WDMES         SDRMES
521900931111     C                   Z-ADD     D$DLNP        SDRFLP
522000931111     C                   Z-ADD     D$DLNA        SDRFLA
522100940103     C                   MOVEL     WDFPR         SDRFPE
522200931111     C                   MOVEL     D$DTSP        SDRTSP
522300941201     C                   MOVEL     D$DTPO        SDRTPO
522400941201     C                   MOVEL     D$DRBL        SDRRBL
522500931111     C                   MOVEL     WDTPR         SDRTPR
522600931111     C* VALORI = VALORI 'DOPO'
522700931111     C                   Z-ADD     1             WRNSP
522800020829     C*---               Z-ADD     D$DPKF        WRPKG
522900020829     C                   Z-ADD     WDPES         WRPES
523000931111     C                   Z-ADD     D$DNCL        WRNCO
523100931111     C                   Z-ADD     WDVOL         WRVOL
523200931111     C                   Z-ADD     WDPTS         WRPTS
523300931111     C                   Z-ADD     WDIRB         WRIRB
523400931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
523500940411     C     WRDREG        IFEQ      'S'
523600931111     C                   EXSR      AGGSDR
523700940411     C                   ENDIF
523800931111     C*
523900940202     C                   END
524000931111     C                   END
524100940411     C                   END
524200930323     C*
524300930323     C                   ENDSR
524400931111     C********************************************************************
524500990617     C*   AGGSDR - AGGIORNA IL FILE SISDR00F (SALDI BOLLE)
524600931111     C********************************************************************
524700931111     C     AGGSDR        BEGSR
524800931112     C     WRNSP         IFEQ      *ZEROS
524900020829     C*--- WRPKG         ANDEQ     *ZEROS
525000020829     C     WRPES         ANDEQ     *ZEROS
525100931112     C     WRNCO         ANDEQ     *ZEROS
525200931112     C     WRVOL         ANDEQ     *ZEROS
525300931112     C     WRPTS         ANDEQ     *ZEROS
525400931112     C     WRIRB         ANDEQ     *ZEROS
525500931112     C                   ELSE
525600990617     C     KEYSDR        CHAIN     SISDR01L                           99
525700931111     C     *IN99         IFEQ      '0'
525800931111     C                   EXSR      ADDSDR
525900990617     C                   UPDATE    SISDR000
526000931111     C                   ELSE
526100931111     C                   EXSR      AZZSDR
526200931111     C                   EXSR      ADDSDR
526300990617     C                   WRITE     SISDR000
526400931111     C                   END
526500931112     C                   END
526600931111     C                   ENDSR
526700931111     C********************************************************************
526800990617     C*   AZZSDR - AZZERA I CAMPI DEL FILE SISDR00F (SALDI BOLLE)
526900931111     C********************************************************************
527000931111     C     AZZSDR        BEGSR
527100931111     C                   Z-ADD     *ZEROS        SDRNSP
527200931111     C                   Z-ADD     *ZEROS        SDRPKG
527300931111     C                   Z-ADD     *ZEROS        SDRNCO
527400931111     C                   Z-ADD     *ZEROS        SDRVOL
527500931111     C                   Z-ADD     *ZEROS        SDRPTS
527600931111     C                   Z-ADD     *ZEROS        SDRIRB
527700931111     C                   Z-ADD     *ZEROS        SDRIRR
527800931111     C                   Z-ADD     *ZEROS        SDRINC
527900931111     C                   ENDSR
528000931111     C********************************************************************
528100990617     C*   ADDSDR - AGGIUNGE AI VALORI DEL FILE SISDR00F (SALDI BOLLE)
528200931111     C********************************************************************
528300931111     C     ADDSDR        BEGSR
528400931111     C                   ADD       WRNSP         SDRNSP
528500020829     C*---               ADD       WRPKG         SDRPKG
528600020829     C                   ADD       WRPES         SDRPKG
528700931111     C                   ADD       WRNCO         SDRNCO
528800931111     C                   ADD       WRVOL         SDRVOL
528900931111     C                   ADD       WRPTS         SDRPTS
529000020129     C                   ADD       WRIRB         SDRIRB
529100940317     C                   MOVEL     'S'           FLAGGS
529200931111     C                   ENDSR
529300931111     C*
529400931111     C*
529500931111     C*
529600931111     C*
529700930420     C********************************************************************
529800990617     C*   GESSDF - GESTISCE IL FILE SISDF00F (SALDI FILIALE)
529900930420     C********************************************************************
530000931104     C     GESSDF        BEGSR
530100940411     C*
530200940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
530300940411     C     WFPREG        IFEQ      'S'
530400940411     C     WFDREG        OREQ      'S'
530500931104     C*
530600931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
530700940407     C     D$GSTS        IFEQ      'U'
530800940407     C     D$PAAS        ANDEQ     D$DAAS
530900931111     C     WPMES         ANDEQ     WDMES
531000931111     C     D$PLNP        ANDEQ     D$DLNP
531100940411     C     WFPREG        ANDEQ     WFDREG
531200931111     C*
531300931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
531400931111     C*
531500931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
531600931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
531700931111     C                   Z-ADD     D$DAAS        SDFANN
531800931111     C                   Z-ADD     WDMES         SDFMES
531900931111     C                   Z-ADD     D$DLNP        SDFFIL
532000931111     C* VALORI = 'DOPO' - 'PRIMA'
532100940512     C                   Z-ADD     *ZEROS        WFNSP
532200020829     C*--- D$DPKF        SUB       D$PPKF        WFPKG
532300020829     C     WDPES         SUB       WPPES         WFPES
532400931111     C     D$DNCL        SUB       D$PNCL        WFNCO
532500931111     C     WDVOL         SUB       WPVOL         WFVOL
532600931111     C     WDPTS         SUB       WPPTS         WFPTS
532700931111     C     WDIRB         SUB       WPIRB         WFIRB
532800931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
532900931111     C                   EXSR      AGGSDF
533000931111     C*
533100931111     C                   ELSE
533200931111     C*
533300931111     C******************** E' CAMBIATA LA CHIAVE *************************
533400931111     C*
533500940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
533600940405     C     D$GSTS        IFEQ      'U'
533700940405     C     D$GSTS        OREQ      'D'
533800931111     C*
533900931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
534000931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
534100931111     C* CHIAVE = CHIAVE 'PRIMA'
534200931111     C                   Z-ADD     D$PAAS        SDFANN
534300931111     C                   Z-ADD     WPMES         SDFMES
534400931111     C                   Z-ADD     D$PLNP        SDFFIL
534500931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
534600931111     C     *ZEROS        SUB       1             WFNSP
534700020829     C*--- *ZEROS        SUB       D$PPKF        WFPKG
534800020829     C     *ZEROS        SUB       WPPES         WFPES
534900931111     C     *ZEROS        SUB       D$PNCL        WFNCO
535000931111     C     *ZEROS        SUB       WPVOL         WFVOL
535100931111     C     *ZEROS        SUB       WPPTS         WFPTS
535200931111     C     *ZEROS        SUB       WPIRB         WFIRB
535300931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
535400940411     C     WFPREG        IFEQ      'S'
535500931111     C                   EXSR      AGGSDF
535600940411     C                   ENDIF
535700931111     C*
535800931111     C                   END
535900940202     C*
536000940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
536100940405     C     D$GSTS        IFEQ      'U'
536200940405     C     D$GSTS        OREQ      'I'
536300931111     C*
536400940202     C* NON E' UN ANNULLAMENTO, QUINDI
536500931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
536600931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
536700931111     C* CHIAVE = CHIAVE 'DOPO'
536800931111     C                   Z-ADD     D$DAAS        SDFANN
536900931111     C                   Z-ADD     WDMES         SDFMES
537000931111     C                   Z-ADD     D$DLNP        SDFFIL
537100931111     C* VALORI = VALORI 'DOPO'
537200931111     C                   Z-ADD     1             WFNSP
537300020829     C*---               Z-ADD     D$DPKF        WFPKG
537400020829     C                   Z-ADD     WDPES         WFPES
537500931111     C                   Z-ADD     D$DNCL        WFNCO
537600931111     C                   Z-ADD     WDVOL         WFVOL
537700931111     C                   Z-ADD     WDPTS         WFPTS
537800931111     C                   Z-ADD     WDIRB         WFIRB
537900931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
538000940411     C     WFDREG        IFEQ      'S'
538100931111     C                   EXSR      AGGSDF
538200940411     C                   ENDIF
538300931111     C*
538400940202     C                   END
538500931111     C                   END
538600940411     C                   END
538700931111     C*
538800931111     C                   ENDSR
538900931111     C********************************************************************
539000990617     C*   AGGSDF - AGGIORNA IL FILE SISDF00F (SALDI FILIALE)
539100931111     C********************************************************************
539200931111     C     AGGSDF        BEGSR
539300931112     C     WFNSP         IFEQ      *ZEROS
539400020829     C*--- WFPKG         ANDEQ     *ZEROS
539500020829     C     WFPES         ANDEQ     *ZEROS
539600931112     C     WFNCO         ANDEQ     *ZEROS
539700931112     C     WFVOL         ANDEQ     *ZEROS
539800931112     C     WFPTS         ANDEQ     *ZEROS
539900931112     C     WFIRB         ANDEQ     *ZEROS
540000931112     C                   ELSE
540100990617     C     KEYSDF        CHAIN     SISDF01L                           99
540200931111     C     *IN99         IFEQ      '0'
540300931111     C                   EXSR      ADDSDF
540400990617     C                   UPDATE    SISDF000
540500931111     C                   ELSE
540600931111     C                   EXSR      AZZSDF
540700931111     C                   EXSR      ADDSDF
540800990617     C                   WRITE     SISDF000
540900931111     C                   END
541000931112     C                   END
541100931111     C                   ENDSR
541200931111     C********************************************************************
541300990617     C*   AZZSDF - AZZERA I CAMPI DEL FILE SISDF00F (SALDI FILIALE)
541400931111     C********************************************************************
541500931111     C     AZZSDF        BEGSR
541600931111     C                   Z-ADD     *ZEROS        SDFNSP
541700931111     C                   Z-ADD     *ZEROS        SDFPKG
541800931111     C                   Z-ADD     *ZEROS        SDFNCO
541900931111     C                   Z-ADD     *ZEROS        SDFVOL
542000931111     C                   Z-ADD     *ZEROS        SDFPTS
542100931111     C                   Z-ADD     *ZEROS        SDFIRB
542200931111     C                   Z-ADD     *ZEROS        SDFIRR
542300931111     C                   Z-ADD     *ZEROS        SDFINC
542400931111     C                   ENDSR
542500931111     C********************************************************************
542600990617     C*   ADDSDF - AGGIUNGE AI VALORI DEL FILE SISDF00F (SALDI FILIALE)
542700931111     C********************************************************************
542800931111     C     ADDSDF        BEGSR
542900931111     C                   ADD       WFNSP         SDFNSP
543000020829     C*---               ADD       WFPKG         SDFPKG
543100020829     C                   ADD       WFPES         SDFPKG
543200931111     C                   ADD       WFNCO         SDFNCO
543300931111     C                   ADD       WFVOL         SDFVOL
543400931111     C                   ADD       WFPTS         SDFPTS
543500020129     C                   ADD       WFIRB         SDFIRB
543600940317     C                   MOVEL     'S'           FLAGGS
543700931111     C                   ENDSR
543800931110     C********************************************************************
543900990617     C*   GESSDT - GESTISCE IL FILE SISDT00F (SALDI BOLLE - TRAZIONI)
544000931110     C********************************************************************
544100931110     C     GESSDT        BEGSR
544200940318     C*
544300940407     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
544400940407     C     WTPREG        IFEQ      'S'
544500940407     C     WTDREG        OREQ      'S'
544600000622     C* IMPOSTA I TERMINAL PARTENZA E ARRIVO NELLE VARIABILI DI LAVORO
544700000705     C                   CLEAR                   LPTFP
544800000705     C                   CLEAR                   LPTFA
544900000705     C                   CLEAR                   LDTFP
545000000705     C                   CLEAR                   LDTFA
545100000705IF  2C     D$GSTS        IFEQ      'U'
545200000705     C     D$GSTS        OREQ      'D'
545300000622     C                   Z-ADD     D$PTFP        LPTFP
545400000622     C                   Z-ADD     D$PTFA        LPTFA
545500000705E   2C                   ENDIF
545600000705IF  2C     D$GSTS        IFEQ      'U'
545700000705     C     D$GSTS        OREQ      'I'
545800000622     C                   Z-ADD     D$DTFP        LDTFP
545900000622     C                   Z-ADD     D$DTFA        LDTFA
546000000705E   2C                   ENDIF
546100000622     C* PRIMA
546200000622     C* SE LINEA POSTE, IL SUO TERMINAL DIVENTA SE STESSA
546300000705IF  2C     D$GSTS        IFEQ      'U'
546400000705     C     D$GSTS        OREQ      'D'
546500000622     C                   Z-ADD     W$PLNP        F
546600020709IF  3C     NTW(F)        IFEQ      'PPT'
546700000622     C                   Z-ADD     W$PLNP        LPTFP
546800000622E   3C                   ENDIF
546900000622     C                   Z-ADD     D$PLNA        F
547000020709IF  3C     NTW(F)        IFEQ      'PPT'
547100000622     C                   Z-ADD     D$PLNA        LPTFA
547200000622E   3C                   ENDIF
547300000622E   2C                   ENDIF
547400000622     C* DOPO
547500000622     C* SE LINEA POSTE, IL SUO TERMINAL DIVENTA SE STESSA
547600000705IF  2C     D$GSTS        IFEQ      'U'
547700000705     C     D$GSTS        OREQ      'I'
547800000622     C                   Z-ADD     W$DLNP        F
547900020709IF  3C     NTW(F)        IFEQ      'PPT'
548000000622     C                   Z-ADD     W$DLNP        LDTFP
548100000622E   3C                   ENDIF
548200000622     C                   Z-ADD     D$DLNA        F
548300020709IF  3C     NTW(F)        IFEQ      'PPT'
548400000622     C                   Z-ADD     D$DLNA        LDTFA
548500000622E   3C                   ENDIF
548600000622E   2C                   ENDIF
548700000622     C*
548800931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
548900940407     C     D$GSTS        IFEQ      'U'
549000940407     C     D$PAAS        ANDEQ     D$DAAS
549100931111     C     WPMES         ANDEQ     WDMES
549200940304     C     WPDEC         ANDEQ     WDDEC
549300000622     C     LPTFP         ANDEQ     LDTFP
549400000622     C     LPTFA         ANDEQ     LDTFA
549500040519     C*    D$PFLP        ANDEQ     D$DFLP
549600040519     C*    D$PFL2        ANDEQ     D$DFL2
549700040617     C     D$PAR5FLP     ANDEQ     D$DAR5FLP
549800040617     C     D$PAR5FL2     ANDEQ     D$DAR5FL2
549900040617     C     D$PAR5FL3     ANDEQ     D$DAR5FL3
550000040617     C     D$PAR5FL4     ANDEQ     D$DAR5FL4
550100040617     C     D$PAR5FL5     ANDEQ     D$DAR5FL5
550200940103     C     WPFPT         ANDEQ     WDFPT
550300940317     C     WTPREG        ANDEQ     WTDREG
550400931111     C*
550500931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
550600931111     C*
550700931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
550800931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
550900931111     C                   Z-ADD     D$DAAS        SDTANN
551000931111     C                   Z-ADD     WDMES         SDTMES
551100940304     C                   MOVEL     WDDEC         SDTDEC
551200000622     C                   Z-ADD     LDTFP         SDTTFP
551300000622     C                   Z-ADD     LDTFA         SDTTFA
551400040519     C*                  Z-ADD     D$DFLP        SDTTT1
551500040519     C*                  Z-ADD     D$DFL2        SDTTT2
551600040617     C                   z-add     D$DAR5FLP     SDTtt1
551700040617     C                   z-add     D$DAR5FL2     SDTtt2
551800040617     C                   z-add     D$DAR5FL3     SDTtt3
551900040617     C                   z-add     D$DAR5FL4     SDTtt4
552000040617     C                   z-add     D$DAR5FL5     SDTtt5
552100940103     C                   MOVEL     WDFPT         SDTFPE
552200931111     C* VALORI = 'DOPO' - 'PRIMA'
552300940512     C                   Z-ADD     *ZEROS        WTNSP
552400020829     C     WDPES         SUB       WPPES         WTPES
552500931111     C     D$DNCL        SUB       D$PNCL        WTNCO
552600931111     C     WDVOL         SUB       WPVOL         WTVOL
552700940823     C     WDCTL         SUB       WPCTL         WTCTL
552800940823     C     WDCT1         SUB       WPCT1         WTCT1
552900940823     C     WDCT2         SUB       WPCT2         WTCT2
553000040519     C     WDCT3         SUB       WPCT3         WTCT3
553100040519     C     WDCT4         SUB       WPCT4         WTCT4
553200040519     C     WDCT5         SUB       WPCT5         WTCT5
553300931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
553400931111     C                   EXSR      AGGSDT
553500931111     C*
553600931111     C                   ELSE
553700931111     C*
553800931111     C******************** E' CAMBIATA LA CHIAVE *************************
553900931111     C*
554000940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
554100940405     C     D$GSTS        IFEQ      'U'
554200940405     C     D$GSTS        OREQ      'D'
554300931111     C*
554400931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
554500931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
554600931111     C* CHIAVE = CHIAVE 'PRIMA'
554700931111     C                   Z-ADD     D$PAAS        SDTANN
554800931111     C                   Z-ADD     WPMES         SDTMES
554900940304     C                   MOVEL     WPDEC         SDTDEC
555000000622     C                   Z-ADD     LPTFP         SDTTFP
555100000622     C                   Z-ADD     LPTFA         SDTTFA
555200040519     C*                  Z-ADD     D$PFLP        SDTTT1
555300040519     C*                  Z-ADD     D$PFL2        SDTTT2
555400040617     C                   z-add     D$PAR5FLP     SDTtt1
555500040617     C                   z-add     D$PAR5FL2     SDTtt2
555600040617     C                   z-add     D$PAR5FL3     SDTtt3
555700040617     C                   z-add     D$PAR5FL4     SDTtt4
555800040617     C                   z-add     D$PAR5FL5     SDTtt5
555900940103     C                   MOVEL     WPFPT         SDTFPE
556000931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
556100931111     C     *ZEROS        SUB       1             WTNSP
556200020829     C     *ZEROS        SUB       WPPES         WTPES
556300931111     C     *ZEROS        SUB       D$PNCL        WTNCO
556400931111     C     *ZEROS        SUB       WPVOL         WTVOL
556500940823     C     *ZEROS        SUB       WPCTL         WTCTL
556600940823     C     *ZEROS        SUB       WPCT1         WTCT1
556700940823     C     *ZEROS        SUB       WPCT2         WTCT2
556800040519     C     *ZEROS        SUB       WPCT3         WTCT3
556900040519     C     *ZEROS        SUB       WPCT4         WTCT4
557000040519     C     *ZEROS        SUB       WPCT5         WTCT5
557100940317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE SOLO SE E' DA GESTIRE
557200940317     C     WTPREG        IFEQ      'S'
557300931111     C                   EXSR      AGGSDT
557400940317     C                   ENDIF
557500931111     C*
557600931111     C                   END
557700940202     C*
557800940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
557900940405     C     D$GSTS        IFEQ      'U'
558000940405     C     D$GSTS        OREQ      'I'
558100940202     C*
558200940202     C* NON E' UN ANNULLAMENTO, QUINDI
558300931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
558400931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
558500931111     C* CHIAVE = CHIAVE 'DOPO'
558600931111     C                   Z-ADD     D$DAAS        SDTANN
558700931111     C                   Z-ADD     WDMES         SDTMES
558800940304     C                   MOVEL     WDDEC         SDTDEC
558900000622     C                   Z-ADD     LDTFP         SDTTFP
559000000622     C                   Z-ADD     LDTFA         SDTTFA
559100040519     C*                  Z-ADD     D$DFLP        SDTTT1
559200040519     C*                  Z-ADD     D$DFL2        SDTTT2
559300040617     C                   z-add     D$DAR5FLP     SDTtt1
559400040617     C                   z-add     D$DAR5FL2     SDTtt2
559500040617     C                   z-add     D$DAR5FL3     SDTtt3
559600040617     C                   z-add     D$DAR5FL4     SDTtt4
559700040617     C                   z-add     D$DAR5FL5     SDTtt5
559800940103     C                   MOVEL     WDFPT         SDTFPE
559900931111     C* VALORI = VALORI 'DOPO'
560000931111     C                   Z-ADD     1             WTNSP
560100020829     C                   Z-ADD     WDPES         WTPES
560200931111     C                   Z-ADD     D$DNCL        WTNCO
560300931111     C                   Z-ADD     WDVOL         WTVOL
560400940823     C                   Z-ADD     WDCTL         WTCTL
560500940823     C                   Z-ADD     WDCT1         WTCT1
560600940823     C                   Z-ADD     WDCT2         WTCT2
560700040519     C                   Z-ADD     WDCT3         WTCT3
560800040519     C                   Z-ADD     WDCT4         WTCT4
560900040519     C                   Z-ADD     WDCT5         WTCT5
561000931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
561100940317     C     WTDREG        IFEQ      'S'
561200931111     C                   EXSR      AGGSDT
561300940317     C                   ENDIF
561400931111     C*
561500940202     C                   END
561600931111     C                   END
561700940318     C                   END
561800931111     C*
561900931111     C                   ENDSR
562000931111     C********************************************************************
562100990617     C*   AGGSDT - AGGIORNA IL FILE SISDT00F (SALDI TRAZIONI)
562200931111     C********************************************************************
562300931111     C     AGGSDT        BEGSR
562400931112     C     WTNSP         IFEQ      *ZEROS
562500020829     C*--- WTPKG         ANDEQ     *ZEROS
562600020829     C     WTPES         ANDEQ     *ZEROS
562700931112     C     WTNCO         ANDEQ     *ZEROS
562800931112     C     WTVOL         ANDEQ     *ZEROS
562900940823     C     WTCTL         ANDEQ     *ZEROS
563000940823     C     WTCT1         ANDEQ     *ZEROS
563100940823     C     WTCT2         ANDEQ     *ZEROS
563200040519     C     WTCT3         ANDEQ     *ZEROS
563300040519     C     WTCT4         ANDEQ     *ZEROS
563400040519     C     WTCT5         ANDEQ     *ZEROS
563500931112     C                   ELSE
563600990617     C     KEYSDT        CHAIN     SISDT01L                           99
563700931111     C     *IN99         IFEQ      '0'
563800931111     C                   EXSR      ADDSDT
563900990617     C                   UPDATE    SISDT000
564000931111     C                   ELSE
564100931111     C                   EXSR      AZZSDT
564200931111     C                   EXSR      ADDSDT
564300990617     C                   WRITE     SISDT000
564400931111     C                   END
564500931112     C                   END
564600931111     C                   ENDSR
564700931111     C********************************************************************
564800990617     C*   AZZSDT - AZZERA I CAMPI DEL FILE SISDT00F (SALDI TRAZIONI)
564900931111     C********************************************************************
565000931111     C     AZZSDT        BEGSR
565100931111     C                   Z-ADD     *ZEROS        SDTNSP
565200931111     C                   Z-ADD     *ZEROS        SDTPKG
565300931111     C                   Z-ADD     *ZEROS        SDTNCO
565400931111     C                   Z-ADD     *ZEROS        SDTVOL
565500931111     C                   Z-ADD     *ZEROS        SDTCTL
565600940314     C                   Z-ADD     *ZEROS        SDTCT1
565700940314     C                   Z-ADD     *ZEROS        SDTCT2
565800040519     C                   Z-ADD     *ZEROS        SDTCT3
565900040519     C                   Z-ADD     *ZEROS        SDTCT4
566000040519     C                   Z-ADD     *ZEROS        SDTCT5
566100931111     C                   ENDSR
566200931111     C********************************************************************
566300990617     C*   ADDSDT - AGGIUNGE AI VALORI DEL FILE SISDT00F (SALDI TRAZIONI)
566400931111     C********************************************************************
566500931111     C     ADDSDT        BEGSR
566600931111     C                   ADD       WTNSP         SDTNSP
566700020829     C*---               ADD       WTPKG         SDTPKG
566800020829     C                   ADD       WTPES         SDTPKG
566900931111     C                   ADD       WTNCO         SDTNCO
567000931111     C                   ADD       WTVOL         SDTVOL
567100090916     C                   z-add(h)  WTCTL         wtctl£
567200090916     C                   ADD       WTCTL£        SDTCTL
567300040519     c
567400090916     C                   z-add(h)  WTCT1         wtct1£
567500090916     C                   ADD       WTCT1£        SDTCT1
567600090916     C                   z-add(h)  WTCT2         wtct2£
567700090916     C                   ADD       WTCT2£        SDTCT2
567800090916     C                   z-add(h)  WTCT3         wtct3£
567900090916     C                   ADD       WTCT3£        SDTCT3
568000090916     C                   z-add(h)  WTCT4         wtct4£
568100090916     C                   ADD       WTCT4£        SDTCT4
568200090916     C                   z-add(h)  WTCT5         wtct5£
568300090916     C                   ADD       WTCT5£        SDTCT5
568400940317     C                   MOVEL     'S'           FLAGGS
568500931111     C                   ENDSR
568600931111     C*
568700931111     C********************************************************************
568800990617     C*   GESSDC - GESTISCE IL FILE SISDC00F (SALDI CLIENTE - REG/REG)
568900931111     C********************************************************************
569000931111     C     GESSDC        BEGSR
569100940411     C*
569200940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
569300940411     C     WCPREG        IFEQ      'S'
569400940411     C     WCDREG        OREQ      'S'
569500931111     C*
569600931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
569700940407     C     D$GSTS        IFEQ      'U'
569800940407     C     D$PAAS        ANDEQ     D$DAAS
569900931111     C     WPMES         ANDEQ     WDMES
570000941117     C     WCPKSC        ANDEQ     WCDKSC
570100931111     C     WPREP         ANDEQ     WDREP
570200931111     C     WPREA         ANDEQ     WDREA
570300940103     C     WPFPC         ANDEQ     WDFPC
570400931111     C     D$PTSP        ANDEQ     D$DTSP
570500931111     C     D$PCTR        ANDEQ     D$DCTR
570600941201     C     D$PTPO        ANDEQ     D$DTPO
570700941201     C     D$PRBL        ANDEQ     D$DRBL
570800931111     C     WPTPR         ANDEQ     WDTPR
570900940411     C     WCPREG        ANDEQ     WCDREG
571000931111     C*
571100931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
571200931111     C*
571300931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
571400931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
571500931111     C                   Z-ADD     D$DAAS        SDCANN
571600931111     C                   Z-ADD     WDMES         SDCMES
571700941117     C                   Z-ADD     WCDKSC        SDCKSC
571800940210     C                   Z-ADD     WDREP         SDCREP
571900940210     C                   Z-ADD     WDREA         SDCREA
572000940103     C                   MOVEL     WDFPC         SDCFPE
572100931111     C                   MOVEL     D$DTSP        SDCTSP
572200931111     C                   Z-ADD     D$DCTR        SDCCTR
572300941201     C                   MOVEL     D$DTPO        SDCTPO
572400941201     C                   MOVEL     D$DRBL        SDCRBL
572500931111     C                   MOVEL     WDTPR         SDCTPR
572600931111     C* VALORI = 'DOPO' - 'PRIMA'
572700940512     C                   Z-ADD     *ZEROS        WCNSP
572800020829     C*--- D$DPKF        SUB       D$PPKF        WCPKG
572900020829     C     WDPES         SUB       WPPES         WCPES
573000020829     C     WDPCC         SUB       WPPCC         WCPCC
573100020829     C     WDPLS         SUB       WPPLS         WCPLS
573200931111     C     D$DNCL        SUB       D$PNCL        WCNCO
573300931111     C     WDVOL         SUB       WPVOL         WCVOL
573400940121     C     WDVCC         SUB       WPVCC         WCVCC
573500940121     C     WDVLS         SUB       WPVLS         WCVLS
573600931112     C     WDIRBC        SUB       WPIRBC        WCIRB
573700931111     C     WDIRP         SUB       WPIRP         WCIRP
573800931111     C     WDIRD         SUB       WPIRD         WCIRD
573900931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
574000931111     C                   EXSR      AGGSDC
574100931111     C*
574200931111     C                   ELSE
574300931111     C*
574400931111     C******************** E' CAMBIATA LA CHIAVE *************************
574500931111     C*
574600940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
574700940405     C     D$GSTS        IFEQ      'U'
574800940405     C     D$GSTS        OREQ      'D'
574900931111     C*
575000931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
575100931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
575200931111     C* CHIAVE = CHIAVE 'PRIMA'
575300931111     C                   Z-ADD     D$PAAS        SDCANN
575400931111     C                   Z-ADD     WPMES         SDCMES
575500941117     C                   Z-ADD     WCPKSC        SDCKSC
575600931111     C                   Z-ADD     WPREP         SDCREP
575700931111     C                   Z-ADD     WPREA         SDCREA
575800940103     C                   MOVEL     WPFPC         SDCFPE
575900931111     C                   MOVEL     D$PTSP        SDCTSP
576000931111     C                   Z-ADD     D$PCTR        SDCCTR
576100941201     C                   MOVEL     D$PTPO        SDCTPO
576200941201     C                   MOVEL     D$PRBL        SDCRBL
576300931111     C                   MOVEL     WPTPR         SDCTPR
576400931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
576500931111     C     *ZEROS        SUB       1             WCNSP
576600020829     C*--- *ZEROS        SUB       D$PPKF        WCPKG
576700020829     C     *ZEROS        SUB       WPPES         WCPES
576800020829     C     *ZEROS        SUB       WPPCC         WCPCC
576900020829     C     *ZEROS        SUB       WPPLS         WCPLS
577000931111     C     *ZEROS        SUB       D$PNCL        WCNCO
577100020829     C     *ZEROS        SUB       WPVOL         WCVOL
577200931111     C     *ZEROS        SUB       WPVCC         WCVCC
577300931111     C     *ZEROS        SUB       WPVLS         WCVLS
577400931112     C     *ZEROS        SUB       WPIRBC        WCIRB
577500931111     C     *ZEROS        SUB       WPIRP         WCIRP
577600931111     C     *ZEROS        SUB       WPIRD         WCIRD
577700931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
577800940411     C     WCPREG        IFEQ      'S'
577900931111     C                   EXSR      AGGSDC
578000940411     C                   ENDIF
578100931111     C*
578200931111     C                   END
578300940202     C*
578400940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
578500940405     C     D$GSTS        IFEQ      'U'
578600940405     C     D$GSTS        OREQ      'I'
578700940202     C*
578800940202     C* NON E' UN ANNULLAMENTO, QUINDI
578900931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
579000931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
579100931111     C* CHIAVE = CHIAVE 'DOPO'
579200931111     C                   Z-ADD     D$DAAS        SDCANN
579300931111     C                   Z-ADD     WDMES         SDCMES
579400941117     C                   Z-ADD     WCDKSC        SDCKSC
579500931111     C                   Z-ADD     WDREP         SDCREP
579600931111     C                   Z-ADD     WDREA         SDCREA
579700940103     C                   MOVEL     WDFPC         SDCFPE
579800931111     C                   MOVEL     D$DTSP        SDCTSP
579900931111     C                   Z-ADD     D$DCTR        SDCCTR
580000941201     C                   MOVEL     D$DTPO        SDCTPO
580100941201     C                   MOVEL     D$DRBL        SDCRBL
580200931111     C                   MOVEL     WDTPR         SDCTPR
580300931111     C* VALORI = VALORI 'DOPO'
580400931111     C                   Z-ADD     1             WCNSP
580500020829     C*---               Z-ADD     D$DPKF        WCPKG
580600020829     C                   Z-ADD     WDPES         WCPES
580700020829     C                   Z-ADD     WDPCC         WCPCC
580800020829     C                   Z-ADD     WDPLS         WCPLS
580900931111     C                   Z-ADD     D$DNCL        WCNCO
581000931111     C                   Z-ADD     WDVOL         WCVOL
581100931111     C                   Z-ADD     WDVCC         WCVCC
581200931111     C                   Z-ADD     WDVLS         WCVLS
581300931112     C                   Z-ADD     WDIRBC        WCIRB
581400931111     C                   Z-ADD     WDIRP         WCIRP
581500931111     C                   Z-ADD     WDIRD         WCIRD
581600931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
581700940411     C     WCDREG        IFEQ      'S'
581800931111     C                   EXSR      AGGSDC
581900940411     C                   ENDIF
582000931111     C*
582100940202     C                   END
582200931111     C                   END
582300940411     C                   END
582400931111     C*
582500931111     C                   ENDSR
582600931111     C********************************************************************
582700990617     C*   AGGSDC - AGGIORNA IL FILE SISDC00F (SALDI CLIENTI - REG/REG)
582800931111     C********************************************************************
582900931111     C     AGGSDC        BEGSR
583000931112     C     WCNSP         IFEQ      *ZEROS
583100020829     C*--- WCPKG         ANDEQ     *ZEROS
583200020829     C     WCPES         ANDEQ     *ZEROS
583300020829     C     WCPCC         ANDEQ     *ZEROS
583400020829     C     WCPLS         ANDEQ     *ZEROS
583500931112     C     WCNCO         ANDEQ     *ZEROS
583600931112     C     WCVOL         ANDEQ     *ZEROS
583700931112     C     WCVCC         ANDEQ     *ZEROS
583800931112     C     WCVLS         ANDEQ     *ZEROS
583900931112     C     WCIRB         ANDEQ     *ZEROS
584000931112     C     WCIRP         ANDEQ     *ZEROS
584100931112     C     WCIRD         ANDEQ     *ZEROS
584200931112     C                   ELSE
584300990617     C     KEYSDC        CHAIN     SISDC01L                           99
584400931111     C     *IN99         IFEQ      '0'
584500931111     C                   EXSR      ADDSDC
584600990617     C                   UPDATE    SISDC000
584700931111     C                   ELSE
584800931111     C                   EXSR      AZZSDC
584900931111     C                   EXSR      ADDSDC
585000990617     C                   WRITE     SISDC000
585100931111     C                   END
585200931112     C                   END
585300931111     C                   ENDSR
585400931111     C********************************************************************
585500990617     C*   AZZSDC - AZZERA I CAMPI DEL FILE SISDC00F (SALDI CLIENTI - REG/REG)
585600931111     C********************************************************************
585700931111     C     AZZSDC        BEGSR
585800931111     C                   Z-ADD     *ZEROS        SDCNSP
585900931111     C                   Z-ADD     *ZEROS        SDCPKG
586000020829     C                   Z-ADD     *ZEROS        SDCPCC
586100020829     C                   Z-ADD     *ZEROS        SDCPLS
586200931111     C                   Z-ADD     *ZEROS        SDCNCO
586300931111     C                   Z-ADD     *ZEROS        SDCVOL
586400931111     C                   Z-ADD     *ZEROS        SDCVCC
586500931111     C                   Z-ADD     *ZEROS        SDCVLS
586600931111     C                   Z-ADD     *ZEROS        SDCIRB
586700931111     C                   Z-ADD     *ZEROS        SDCIRR
586800931111     C                   Z-ADD     *ZEROS        SDCINC
586900931111     C                   Z-ADD     *ZEROS        SDCIRP
587000931111     C                   Z-ADD     *ZEROS        SDCIRD
587100990617     C                   Z-ADD     *ZEROS        SDCIRC
587200940301     C                   MOVEL     *BLANKS       SDCFTR
587300940301     C                   Z-ADD     *ZEROS        SDCDTR
587400931111     C                   ENDSR
587500931111     C********************************************************************
587600990617     C*   ADDSDC - AGGIUNGE AI VALORI DEL FILE SISDC00F (SALDI CLIENTI-REG/REG)
587700931111     C********************************************************************
587800931111     C     ADDSDC        BEGSR
587900931111     C                   ADD       WCNSP         SDCNSP
588000020829     C*---               ADD       WCPKG         SDCPKG
588100020829     C                   ADD       WCPES         SDCPKG
588200020829     C                   ADD       WCPCC         SDCPCC
588300020829     C                   ADD       WCPLS         SDCPLS
588400931111     C                   ADD       WCNCO         SDCNCO
588500931111     C                   ADD       WCVOL         SDCVOL
588600931111     C                   ADD       WCVCC         SDCVCC
588700931111     C                   ADD       WCVLS         SDCVLS
588800020129     C                   ADD       WCIRB         SDCIRB
588900090916     C                   z-add(h)  WCIRP         wcirp£
589000090916     C                   ADD       WCIRP£        SDCIRP
589100020129     C                   ADD       WCIRD         SDCIRD
589200940301     C                   MOVEL     *BLANKS       SDCFTR
589300940301     C                   Z-ADD     *ZEROS        SDCDTR
589400940317     C                   MOVEL     'S'           FLAGGS
589500931111     C                   ENDSR
589600931110     C*
589700931111     C*
589800931111     C*
589900931111     C*
590000040316     C********************************************************************
590100040316     C*   GESSDE - GESTISCE IL FILE SISDE00F (ESTENSIONE SALDI)
590200040316     C********************************************************************
590300040316     C     GESSDE        BEGSR
590400040323     C                   CLEAR                   WGESSDE
590500040323     C     WePREG        IFEQ      'S'
590600040323     C     WeDREG        OREQ      'S'
590700040323     C                   SELECT
590800040323     c                   when      (%subst(d$dgva: 1: 1) = 'O' or
590900040323     c                              %subst(d$pgva: 1: 1) = 'O') and
591000040323     C                             (D$DAR5CSF <> D$PAR5CSF OR
591100040323     C                             D$DAR5COR <> D$PAR5COR or
591200040323     C                             D$DAR5nba <> D$PAR5nba or
591300040323     C                             D$DAR5nb2 <> D$PAR5nb2)
591400040323     C                   EVAL      WGESSDE= 'BNB'
591500040323     C                   EXSR      GESsdeBNB
591600040323     c                   when      (%subst(d$dgva: 1: 1) = 'B' or
591700040323     c                              %subst(d$pgva: 1: 1) = 'B') and
591800040323     C                             (D$DAR5nba <> D$PAR5nba or
591900040323     C                             D$DAR5nb2 <> D$PAR5nb2)
592000040323     C                   EVAL      WGESSDE= 'BAN'
592100040323     C                   EXSR      GESsdeBAN
592200040323     C                   ENDSL
592300040316     C                   END
592400040316     C*
592500040316     C                   ENDSR
592600040317     C********************************************************************
592700040317     C*   GESsdeBNB - GESTISCE IL FILE SISDE00F X TIPO RECORD 'BNB'
592800040317     C********************************************************************
592900040317     C     GESsdeBNB     BEGSR
593000040317     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
593100040317     C     D$GSTS        IFEQ      'U'
593200040317     C     D$PAAS        ANDEQ     D$DAAS
593300040317     C     WPMES         ANDEQ     WDMES
593400040317     C     WCPKSC        ANDEQ     WCDKSC
593500040317     C     WPREP         ANDEQ     WDREP
593600040317     C     WPREA         ANDEQ     WDREA
593700040317     C     WPFPC         ANDEQ     WDFPC
593800040317     C     D$PTSP        ANDEQ     D$DTSP
593900040317     C     D$PCTR        ANDEQ     D$DCTR
594000040317     C     D$PTPO        ANDEQ     D$DTPO
594100040317     C     D$PRBL        ANDEQ     D$DRBL
594200040317     C     WPTPR         ANDEQ     WDTPR
594300040317     C     WePREG        ANDEQ     WeDREG
594400040317     C*
594500040317     C******************** NON E' CAMBIATA LA CHIAVE *********************
594600040317     C*
594700040317     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
594800040317     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
594900040317     C                   Z-ADD     D$DAAS        SDEANN
595000040317     C                   Z-ADD     WDMES         SDEMES
595100040317     C                   Z-ADD     WCDKSC        SDEKSC
595200040317     C                   Z-ADD     WDREP         SDEREP
595300040317     C                   Z-ADD     WDREA         SDEREA
595400040317     C                   MOVEL     WDFPC         SDEFPE
595500040317     C                   MOVEL     D$DTSP        SDETSP
595600040317     C                   Z-ADD     D$DCTR        SDECTR
595700040317     C                   MOVEL     D$DTPO        SDETPO
595800040317     C                   MOVEL     D$DRBL        SDERBL
595900040317     C                   MOVEL     WDTPR         SDETPR
596000040317     C* VALORI = 'DOPO' - 'PRIMA'
596100040317     C     D$DAR5CSF     SUB       D$PAR5CSF     WECSF
596200040317     C     D$DAR5COR     SUB       D$PAR5COR     WECOR
596300040318     C     D$DAR5nba     SUB       D$PAR5nba     WEnba
596400040318     C     D$DAR5nb2     SUB       D$PAR5nb2     WEnb2
596500040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
596600040317     C                   EXSR      AGGSDEbnb
596700040317     C*
596800040317     C                   ELSE
596900040317     C*
597000040317     C******************** E' CAMBIATA LA CHIAVE *************************
597100040317     C*
597200040317     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
597300040317     C     D$GSTS        IFEQ      'U'
597400040317     C     D$GSTS        OREQ      'D'
597500040317     C*
597600040317     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
597700040317     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
597800040317     C* CHIAVE = CHIAVE 'PRIMA'
597900040317     C                   Z-ADD     D$PAAS        SDEANN
598000040317     C                   Z-ADD     WPMES         SDEMES
598100040317     C                   Z-ADD     WCPKSC        SDEKSC
598200040317     C                   Z-ADD     WPREP         SDEREP
598300040317     C                   Z-ADD     WPREA         SDEREA
598400040317     C                   MOVEL     WPFPC         SDEFPE
598500040317     C                   MOVEL     D$PTSP        SDETSP
598600040317     C                   Z-ADD     D$PCTR        SDECTR
598700040317     C                   MOVEL     D$PTPO        SDETPO
598800040317     C                   MOVEL     D$PRBL        SDERBL
598900040317     C                   MOVEL     WPTPR         SDETPR
599000040317     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
599100040317     C     *ZEROS        SUB       D$PAR5CSF     WECSF
599200040317     C     *ZEROS        SUB       D$PAR5COR     WECOR
599300040318     C     *zeros        SUB       D$PAR5nba     WEnba
599400040318     C     *zeros        SUB       D$PAR5nb2     WEnb2
599500040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
599600040317     C     WePREG        IFEQ      'S'
599700040317     C                   EXSR      AGGSDEbnb
599800040317     C                   ENDIF
599900040317     C*
600000040317     C                   END
600100040317     C*
600200040317     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
600300040317     C     D$GSTS        IFEQ      'U'
600400040317     C     D$GSTS        OREQ      'I'
600500040317     C*
600600040317     C* NON E' UN ANNULLAMENTO, QUINDI
600700040317     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
600800040317     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
600900040317     C* CHIAVE = CHIAVE 'DOPO'
601000040317     C                   Z-ADD     D$DAAS        SDEANN
601100040317     C                   Z-ADD     WDMES         SDEMES
601200040317     C                   Z-ADD     WCDKSC        SDEKSC
601300040317     C                   Z-ADD     WDREP         SDEREP
601400040317     C                   Z-ADD     WDREA         SDEREA
601500040317     C                   MOVEL     WDFPC         SDEFPE
601600040317     C                   MOVEL     D$DTSP        SDETSP
601700040317     C                   Z-ADD     D$DCTR        SDECTR
601800040317     C                   MOVEL     D$DTPO        SDETPO
601900040317     C                   MOVEL     D$DRBL        SDERBL
602000040317     C                   MOVEL     WDTPR         SDETPR
602100040317     C* VALORI = VALORI 'DOPO'
602200040317     C                   Z-ADD     D$DAR5CSF     WECSF
602300040317     C                   Z-ADD     D$DAR5COR     WECOR
602400040318     C                   z-add     D$dAR5nba     WEnba
602500040318     C                   z-add     D$dAR5nb2     WEnb2
602600040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
602700040317     C     WeDREG        IFEQ      'S'
602800040317     C                   EXSR      AGGSDEbnb
602900040317     C                   ENDIF
603000040317     C*
603100040317     C                   END
603200040317     C                   END
603300040317     C*
603400040317     C                   ENDSR
603500040317     C********************************************************************
603600040317     C*   AGGSDEbnb - AGGIORNA IL FILE SISDE00F tipo record bnb
603700040317     C********************************************************************
603800040317     C     AGGSDEbnb     BEGSR
603900040317     C     WECOR         IFEQ      *ZEROS
604000040317     C     WECSF         ANDEQ     *ZEROS
604100040403     C     wenba         andeq     *zeros
604200040403     C     wenb2         andeq     *zeros
604300040317     C                   ELSE
604400040317     C                   EVAL      SDEFILE = 'SDC'
604500040317     C                   EVAL      SDETRD = 'BNB'
604600040317     C     KEYSDE        CHAIN     SISDE01L                           99
604700040317     C     *IN99         IFEQ      '0'
604800040317     C                   EXSR      ADDSDE
604900040317     C                   UPDATE    SISDE000
605000040317     C                   ELSE
605100040317     C                   EXSR      AZZSDE
605200040317     C                   EXSR      ADDSDE
605300040317     C                   WRITE     SISDE000
605400040317     C                   END
605500040317     C                   END
605600040317     C                   ENDSR
605700040318     C********************************************************************
605800040318     C*   GESsdeBan - GESTISCE IL FILE SISDE00F X TIPO RECORD 'BAN'
605900040318     C********************************************************************
606000040318     C     GESsdeBAN     BEGSR
606100040318     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
606200040318     C     D$GSTS        IFEQ      'U'
606300040318     C     D$PAAS        ANDEQ     D$DAAS
606400040318     C     WPMES         ANDEQ     WDMES
606500040318     C     WCPKSC        ANDEQ     WCDKSC
606600040318     C     WPREP         ANDEQ     WDREP
606700040318     C     WPREA         ANDEQ     WDREA
606800040318     C     WPFPC         ANDEQ     WDFPC
606900040318     C     D$PTSP        ANDEQ     D$DTSP
607000040318     C     D$PCTR        ANDEQ     D$DCTR
607100040318     C     D$PTPO        ANDEQ     D$DTPO
607200040318     C     D$PRBL        ANDEQ     D$DRBL
607300040318     C     WPTPR         ANDEQ     WDTPR
607400040318     C     WePREG        ANDEQ     WeDREG
607500040318     C*
607600040318     C******************** NON E' CAMBIATA LA CHIAVE *********************
607700040318     C*
607800040318     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
607900040318     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
608000040318     C                   Z-ADD     D$DAAS        SDEANN
608100040318     C                   Z-ADD     WDMES         SDEMES
608200040318     C                   Z-ADD     WCDKSC        SDEKSC
608300040318     C                   Z-ADD     WDREP         SDEREP
608400040318     C                   Z-ADD     WDREA         SDEREA
608500040318     C                   MOVEL     WDFPC         SDEFPE
608600040318     C                   MOVEL     D$DTSP        SDETSP
608700040318     C                   Z-ADD     D$DCTR        SDECTR
608800040318     C                   MOVEL     D$DTPO        SDETPO
608900040318     C                   MOVEL     D$DRBL        SDERBL
609000040318     C                   MOVEL     WDTPR         SDETPR
609100040318     C* VALORI = 'DOPO' - 'PRIMA'
609200040318     C     D$DAR5nba     SUB       D$PAR5nba     WEnba
609300040318     C     D$DAR5nb2     SUB       D$PAR5nb2     WEnb2
609400040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
609500040318     C                   EXSR      AGGSDEban
609600040318     C*
609700040318     C                   ELSE
609800040318     C*
609900040318     C******************** E' CAMBIATA LA CHIAVE *************************
610000040318     C*
610100040318     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
610200040318     C     D$GSTS        IFEQ      'U'
610300040318     C     D$GSTS        OREQ      'D'
610400040318     C*
610500040318     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
610600040318     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
610700040318     C* CHIAVE = CHIAVE 'PRIMA'
610800040318     C                   Z-ADD     D$PAAS        SDEANN
610900040318     C                   Z-ADD     WPMES         SDEMES
611000040318     C                   Z-ADD     WCPKSC        SDEKSC
611100040318     C                   Z-ADD     WPREP         SDEREP
611200040318     C                   Z-ADD     WPREA         SDEREA
611300040318     C                   MOVEL     WPFPC         SDEFPE
611400040318     C                   MOVEL     D$PTSP        SDETSP
611500040318     C                   Z-ADD     D$PCTR        SDECTR
611600040318     C                   MOVEL     D$PTPO        SDETPO
611700040318     C                   MOVEL     D$PRBL        SDERBL
611800040318     C                   MOVEL     WPTPR         SDETPR
611900040318     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
612000040318     C     *zeros        SUB       D$PAR5nba     WEnba
612100040318     C     *zeros        SUB       D$PAR5nb2     WEnb2
612200040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
612300040318     C     WePREG        IFEQ      'S'
612400040318     C                   EXSR      AGGSDEban
612500040318     C                   ENDIF
612600040318     C*
612700040318     C                   END
612800040318     C*
612900040318     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
613000040318     C     D$GSTS        IFEQ      'U'
613100040318     C     D$GSTS        OREQ      'I'
613200040318     C*
613300040318     C* NON E' UN ANNULLAMENTO, QUINDI
613400040318     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
613500040318     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
613600040318     C* CHIAVE = CHIAVE 'DOPO'
613700040318     C                   Z-ADD     D$DAAS        SDEANN
613800040318     C                   Z-ADD     WDMES         SDEMES
613900040318     C                   Z-ADD     WCDKSC        SDEKSC
614000040318     C                   Z-ADD     WDREP         SDEREP
614100040318     C                   Z-ADD     WDREA         SDEREA
614200040318     C                   MOVEL     WDFPC         SDEFPE
614300040318     C                   MOVEL     D$DTSP        SDETSP
614400040318     C                   Z-ADD     D$DCTR        SDECTR
614500040318     C                   MOVEL     D$DTPO        SDETPO
614600040318     C                   MOVEL     D$DRBL        SDERBL
614700040318     C                   MOVEL     WDTPR         SDETPR
614800040318     C* VALORI = VALORI 'DOPO'
614900040318     C                   z-add     D$dAR5nba     WEnba
615000040318     C                   z-add     D$dAR5nb2     WEnb2
615100040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
615200040318     C     WeDREG        IFEQ      'S'
615300040318     C                   EXSR      AGGSDEban
615400040318     C                   ENDIF
615500040318     C*
615600040318     C                   END
615700040318     C                   END
615800040318     C*
615900040318     C                   ENDSR
616000040318     C********************************************************************
616100040318     C*   AGGSDEban - AGGIORNA IL FILE SISDE00F tipo record ban
616200040318     C********************************************************************
616300040318     C     AGGSDEban     BEGSR
616400040318     C     WEnba         IFEQ      *ZEROS
616500040318     C     WEnb2         ANDEQ     *ZEROS
616600040318     C                   ELSE
616700040318     C                   EVAL      SDEFILE = 'SDC'
616800040318     C                   EVAL      SDETRD = 'BAN'
616900040318     C     KEYSDE        CHAIN     SISDE01L                           99
617000040318     C     *IN99         IFEQ      '0'
617100040318     C                   EXSR      ADDSDE
617200040318     C                   UPDATE    SISDE000
617300040318     C                   ELSE
617400040318     C                   EXSR      AZZSDE
617500040318     C                   EXSR      ADDSDE
617600040318     C                   WRITE     SISDE000
617700040318     C                   END
617800040318     C                   END
617900040318     C                   ENDSR
618000040316     C********************************************************************
618100040316     C*   AZZSDE - AZZERA I CAMPI DEL FILE SISDE00F (ESTENSIONE SALDI)      )
618200040316     C********************************************************************
618300040316     C     AZZSDE        BEGSR
618400040318     c                   select
618500040318     c* tipo record BNB
618600040318     C                   when      wgessde='BNB'
618700040317     C                   CLEAR                   DAR5BNBSDE
618800040403     C                   eval      sdeuni = DAR5BNBSDE
618900040318     c* tipo record BAN
619000040318     C                   when      wgessde='BAN'
619100040318     C                   CLEAR                   DAR5BANSDE
619200040403     C                   eval      sdeuni = DAR5BANSDE
619300040318     C                   ENDsl
619400040316     C                   MOVEL     *BLANKS       SDEFTR
619500040316     C                   Z-ADD     *ZEROS        SDEDTR
619600040316     C                   ENDSR
619700040316     C********************************************************************
619800040316     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
619900040316     C********************************************************************
620000040316     C     ADDSDE        BEGSR
620100040318     c                   select
620200040318     c* tipo record BNB
620300040318     C                   when      wgessde='BNB'
620400040403     C                   eval      DAR5BNBSDE = SDEUNI
620500040403     C                   add       wecsf         §bnbBCSF
620600040403     C                   add       wecor         §bnbBCOR
620700040403     C                   add       wenba         §bnbBnba
620800040403     C                   add       wenb2         §bnbBnb2
620900040403     C                   eval      sdeuni = DAR5BNBSDE
621000040318     c* tipo record BAN
621100040318     C                   when      wgessde='BAN'
621200040403     C                   eval      DAR5BANSDE =  SDEUNI
621300040403     C                   add       wenba         §bannba
621400040403     C                   add       wenb2         §bannb2
621500040403     C                   eval      sdeuni = DAR5BANSDE
621600040318     C                   ENDsl
621700040316     C                   MOVEL     *BLANKS       SDEFTR
621800040316     C                   Z-ADD     *ZEROS        SDEDTR
621900040316     C                   MOVEL     'S'           FLAGGS
622000040316     C                   ENDSR
622100931112     C********************************************************************
622200990617     C*   GESSDO - GESTISCE IL FILE SISDO00F (SALDI CLIENTE - CONTABILI)
622300931112     C********************************************************************
622400931112     C     GESSDO        BEGSR
622500940411     C*
622600940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
622700940411     C     WOPREG        IFEQ      'S'
622800940411     C     WODREG        OREQ      'S'
622900931112     C*
623000931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
623100940407     C     D$GSTS        IFEQ      'U'
623200940407     C     D$PAAS        ANDEQ     D$DAAS
623300931112     C     WPMES         ANDEQ     WDMES
623400941117     C     WCPKSC        ANDEQ     WCDKSC
623500940411     C     WOPREG        ANDEQ     WODREG
623600931112     C*
623700931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
623800931112     C*
623900931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
624000931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
624100931112     C                   Z-ADD     D$DAAS        SDOANN
624200931112     C                   Z-ADD     WDMES         SDOMES
624300941117     C                   Z-ADD     WCDKSC        SDOKSC
624400931112     C* VALORI = 'DOPO' - 'PRIMA'
624500940512     C                   Z-ADD     *ZEROS        WONSP
624600020829     C*--- D$DPKF        SUB       D$PPKF        WOPKG
624700020829     C     WDPES         SUB       WPPES         WOPES
624800931112     C     D$DNCL        SUB       D$PNCL        WONCO
624900931112     C     WDVOL         SUB       WPVOL         WOVOL
625000931112     C     WDIRBC        SUB       WPIRBC        WOIRB
625100931112     C     WDIRP         SUB       WPIRP         WOIRP
625200931112     C     WDIRD         SUB       WPIRD         WOIRD
625300931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
625400931112     C                   EXSR      AGGSDO
625500931112     C*
625600931112     C                   ELSE
625700931112     C*
625800931112     C******************** E' CAMBIATA LA CHIAVE *************************
625900931112     C*
626000940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
626100940405     C     D$GSTS        IFEQ      'U'
626200940405     C     D$GSTS        OREQ      'D'
626300931112     C*
626400931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
626500931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
626600931112     C* CHIAVE = CHIAVE 'PRIMA'
626700931112     C                   Z-ADD     D$PAAS        SDOANN
626800931112     C                   Z-ADD     WPMES         SDOMES
626900941117     C                   Z-ADD     WCPKSC        SDOKSC
627000931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
627100931112     C     *ZEROS        SUB       1             WONSP
627200020829     C*--- *ZEROS        SUB       D$PPKF        WOPKG
627300020829     C     *ZEROS        SUB       WPPES         WOPES
627400931112     C     *ZEROS        SUB       D$PNCL        WONCO
627500931112     C     *ZEROS        SUB       WPVOL         WOVOL
627600931112     C     *ZEROS        SUB       WPIRBC        WOIRB
627700931112     C     *ZEROS        SUB       WPIRP         WOIRP
627800931112     C     *ZEROS        SUB       WPIRD         WOIRD
627900931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
628000940411     C     WOPREG        IFEQ      'S'
628100931112     C                   EXSR      AGGSDO
628200940411     C                   ENDIF
628300931112     C*
628400931112     C                   END
628500940202     C*
628600940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
628700940405     C     D$GSTS        IFEQ      'U'
628800940405     C     D$GSTS        OREQ      'I'
628900940202     C*
629000940202     C* NON E' UN ANNULLAMENTO, QUINDI
629100931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
629200931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
629300931112     C* CHIAVE = CHIAVE 'DOPO'
629400931112     C                   Z-ADD     D$DAAS        SDOANN
629500931112     C                   Z-ADD     WDMES         SDOMES
629600941117     C                   Z-ADD     WCDKSC        SDOKSC
629700931112     C* VALORI = VALORI 'DOPO'
629800931112     C                   Z-ADD     1             WONSP
629900020829     C*---               Z-ADD     D$DPKF        WOPKG
630000020829     C                   Z-ADD     WDPES         WOPES
630100931112     C                   Z-ADD     D$DNCL        WONCO
630200931112     C                   Z-ADD     WDVOL         WOVOL
630300931112     C                   Z-ADD     WDIRBC        WOIRB
630400931112     C                   Z-ADD     WDIRP         WOIRP
630500931112     C                   Z-ADD     WDIRD         WOIRD
630600931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
630700940411     C     WODREG        IFEQ      'S'
630800931112     C                   EXSR      AGGSDO
630900940411     C                   ENDIF
631000931112     C*
631100940202     C                   END
631200931112     C                   END
631300940411     C                   END
631400931112     C*
631500931112     C                   ENDSR
631600931112     C********************************************************************
631700990617     C*   AGGSDO - AGGIORNA IL FILE SISDO00F (SALDI CLIENTI - CONTABILI)
631800931112     C********************************************************************
631900931112     C     AGGSDO        BEGSR
632000931112     C     WONSP         IFEQ      *ZEROS
632100020829     C*--- WOPKG         ANDEQ     *ZEROS
632200020829     C     WOPES         ANDEQ     *ZEROS
632300931112     C     WONCO         ANDEQ     *ZEROS
632400931112     C     WOVOL         ANDEQ     *ZEROS
632500931112     C     WOIRB         ANDEQ     *ZEROS
632600931112     C     WOIRP         ANDEQ     *ZEROS
632700931112     C     WOIRD         ANDEQ     *ZEROS
632800931112     C                   ELSE
632900990617     C     KEYSDO        CHAIN     SISDO01L                           99
633000931112     C     *IN99         IFEQ      '0'
633100931112     C                   EXSR      ADDSDO
633200990617     C                   UPDATE    SISDO000
633300931112     C                   ELSE
633400931112     C                   EXSR      AZZSDO
633500931112     C                   EXSR      ADDSDO
633600990617     C                   WRITE     SISDO000
633700931112     C                   END
633800931112     C                   END
633900931112     C                   ENDSR
634000931112     C********************************************************************
634100990617     C*   AZZSDO - AZZERA I CAMPI DEL FILE SISDO00F (SALDI CLIENTI - CONTABILI)
634200931112     C********************************************************************
634300931112     C     AZZSDO        BEGSR
634400931112     C                   Z-ADD     *ZEROS        SDONSP
634500931112     C                   Z-ADD     *ZEROS        SDOPKG
634600931112     C                   Z-ADD     *ZEROS        SDONCO
634700931112     C                   Z-ADD     *ZEROS        SDOVOL
634800931112     C                   Z-ADD     *ZEROS        SDOIRB
634900931112     C                   Z-ADD     *ZEROS        SDOIRR
635000931112     C                   Z-ADD     *ZEROS        SDOINC
635100931112     C                   Z-ADD     *ZEROS        SDOIRP
635200931112     C                   Z-ADD     *ZEROS        SDOIRD
635300990617     C                   Z-ADD     *ZEROS        SDOIRC
635400940112     C                   MOVEL     *BLANKS       SDOCLN
635500931112     C                   Z-ADD     *ZEROS        SDONNC
635600940301     C                   MOVEL     *BLANKS       SDOFTR
635700940301     C                   Z-ADD     *ZEROS        SDODTR
635800931112     C                   ENDSR
635900931112     C********************************************************************
636000990617     C*   ADDSDO - AGGIUNGE AI VALORI DEL FILE SISDO00F (SALDI CLIENTI-CONTABILI
636100931112     C********************************************************************
636200931112     C     ADDSDO        BEGSR
636300931112     C                   ADD       WONSP         SDONSP
636400020829     C*---               ADD       WOPKG         SDOPKG
636500020829     C                   ADD       WOPES         SDOPKG
636600931112     C                   ADD       WONCO         SDONCO
636700931112     C                   ADD       WOVOL         SDOVOL
636800020129     C                   ADD       WOIRB         SDOIRB
636900090916     C                   z-add(h)  WOIRP         woirp£
637000090916     C                   ADD       WOIRP£        SDOIRP
637100020129     C                   ADD       WOIRD         SDOIRD
637200940301     C                   MOVEL     *BLANKS       SDOFTR
637300940301     C                   Z-ADD     *ZEROS        SDODTR
637400940317     C                   MOVEL     'S'           FLAGGS
637500931112     C                   ENDSR
637600931112     C*
637700931112     C*
637800931112     C*
637900931112     C*
638000931112     C********************************************************************
638100990617     C*   GESSDP - GESTISCE IL FILE SISDP00F (SALDI CLIENTE - CONS.PART.)
638200931112     C********************************************************************
638300940103     C     GESSDP        BEGSR
638400940411     C*
638500940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
638600940411     C     WPPREG        IFEQ      'S'
638700940411     C     WPDREG        OREQ      'S'
638800970128     C*
638900970128     C* FINO A FINE '96 CLIENTE = CLIENTE BOLLA
639000970128     C* DAL '97 CLIENTE = CLIENTE MAMMA (SE BOLLA FIGLIA)
639100970205     C     D$GSTS        IFEQ      'U'
639200970205     C     D$GSTS        OREQ      'D'
639300970205     C     D$PAAS        IFGE      1997
639400970205     C                   Z-ADD     WCPKSC        WPPKSC
639500970205     C                   ELSE
639600970205     C                   Z-ADD     D$PKSC        WPPKSC
639700970205     C                   ENDIF
639800970205     C                   ENDIF
639900970205     C*
640000970205     C     D$GSTS        IFEQ      'U'
640100970205     C     D$GSTS        OREQ      'I'
640200970205     C     D$DAAS        IFGE      1997
640300970205     C                   Z-ADD     WCDKSC        WPDKSC
640400970205     C                   ELSE
640500970205     C                   Z-ADD     D$DKSC        WPDKSC
640600970205     C                   ENDIF
640700970205     C                   ENDIF
640800940316     C*
640900940316     C* ESEGUO LOOP PER TUTTE LE POSSIBILI CONSEGNA PARTICOLARI
641000940316     C                   DO        20            I
641100940316     C*
641200940316     C* ESEGUO AGGIORNAMENTO SOLO SE VI SONO DATI VALIDI
641300940316     C     SVP(I)        IFEQ      *BLANKS
641400940316     C     SVD(I)        ANDEQ     *BLANKS
641500940316     C     IVP(I)        ANDEQ     *ZEROS
641600940316     C     IVD(I)        ANDEQ     *ZEROS
641700940330     C     NSP(I)        ANDEQ     *ZEROS
641800940330     C     NSD(I)        ANDEQ     *ZEROS
641900940316     C     CPP(I)        ANDEQ     *BLANKS
642000940316     C     CPD(I)        ANDEQ     *BLANKS
642100940316     C                   ELSE
642200931112     C*
642300931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
642400940407     C     D$GSTS        IFEQ      'U'
642500940407     C     D$PAAS        ANDEQ     D$DAAS
642600940103     C     WPMES         ANDEQ     WDMES
642700970128     C     WPPKSC        ANDEQ     WPDKSC
642800940316     C     D$PCTR        ANDEQ     D$DCTR
642900940316     C     SVP(I)        ANDEQ     SVD(I)
643000940316     C     CPP(I)        ANDEQ     CPD(I)
643100940411     C     WPPREG        ANDEQ     WPDREG
643200931112     C*
643300931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
643400931112     C*
643500931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
643600931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
643700940103     C                   Z-ADD     D$DAAS        SDPANN
643800940103     C                   Z-ADD     WDMES         SDPMES
643900970128     C                   Z-ADD     WPDKSC        SDPKSC
644000940316     C                   MOVEL     D$DCTR        SDPCTR
644100940316     C                   MOVEL     SVD(I)        SDPSVR
644200940316     C                   MOVEL     CPD(I)        SDPFTC
644300931112     C* VALORI = 'DOPO' - 'PRIMA'
644400940330     C     NSD(I)        SUB       NSP(I)        WPNSP
644500940316     C     IVD(I)        SUB       IVP(I)        WPRCP
644600931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
644700940103     C                   EXSR      AGGSDP
644800931112     C*
644900940316     C                   ELSE
645000931112     C*
645100931112     C******************** E' CAMBIATA LA CHIAVE *************************
645200931112     C*
645300940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
645400940411     C     D$GSTS        IFEQ      'U'
645500940405     C     D$GSTS        OREQ      'D'
645600931112     C*
645700931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
645800931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
645900931112     C* CHIAVE = CHIAVE 'PRIMA'
646000940103     C                   Z-ADD     D$PAAS        SDPANN
646100940103     C                   Z-ADD     WPMES         SDPMES
646200970128     C                   Z-ADD     WPPKSC        SDPKSC
646300940316     C                   MOVEL     D$PCTR        SDPCTR
646400940316     C                   MOVEL     SVP(I)        SDPSVR
646500940316     C                   MOVEL     CPP(I)        SDPFTC
646600931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
646700940330     C     *ZEROS        SUB       NSP(I)        WPNSP
646800940316     C     *ZEROS        SUB       IVP(I)        WPRCP
646900931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
647000940411     C     WPPREG        IFEQ      'S'
647100940103     C                   EXSR      AGGSDP
647200940411     C                   ENDIF
647300931112     C*
647400940411     C                   END
647500940202     C*
647600940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
647700940411     C     D$GSTS        IFEQ      'U'
647800940405     C     D$GSTS        OREQ      'I'
647900940202     C*
648000940202     C* NON E' UN ANNULLAMENTO, QUINDI
648100931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
648200931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
648300931112     C* CHIAVE = CHIAVE 'DOPO'
648400940103     C                   Z-ADD     D$DAAS        SDPANN
648500940103     C                   Z-ADD     WDMES         SDPMES
648600970128     C                   Z-ADD     WPDKSC        SDPKSC
648700940316     C                   MOVEL     D$DCTR        SDPCTR
648800940316     C                   MOVEL     SVD(I)        SDPSVR
648900940316     C                   MOVEL     CPD(I)        SDPFTC
649000931112     C* VALORI = VALORI 'DOPO'
649100940330     C                   Z-ADD     NSD(I)        WPNSP
649200940316     C                   Z-ADD     IVD(I)        WPRCP
649300931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
649400940411     C     WPDREG        IFEQ      'S'
649500940103     C                   EXSR      AGGSDP
649600940411     C                   ENDIF
649700931112     C*
649800940316     C                   ENDIF
649900940316     C                   ENDIF
650000940316     C                   ENDIF
650100940316     C                   ENDDO
650200940411     C                   ENDIF
650300931112     C*
650400940103     C                   ENDSR
650500931112     C********************************************************************
650600990617     C*   AGGSDP - AGGIORNA IL FILE SISDP00F (SALDI CLIENTI - CONS.PART)
650700931112     C********************************************************************
650800940103     C     AGGSDP        BEGSR
650900940103     C     WPNSP         IFEQ      *ZEROS
651000940103     C     WPRCP         ANDEQ     *ZEROS
651100940103     C                   ELSE
651200990617     C     KEYSDP        CHAIN     SISDP01L                           99
651300940103     C     *IN99         IFEQ      '0'
651400940103     C                   EXSR      ADDSDP
651500990617     C                   UPDATE    SISDP000
651600940103     C                   ELSE
651700940103     C                   EXSR      AZZSDP
651800940103     C                   EXSR      ADDSDP
651900990617     C                   WRITE     SISDP000
652000940103     C                   END
652100940103     C                   END
652200940103     C                   ENDSR
652300931112     C********************************************************************
652400990617     C*   AZZSDP - AZZERA I CAMPI DEL FILE SISDP00F (SALDI CLIENTI - CONS.PART)
652500931112     C********************************************************************
652600940103     C     AZZSDP        BEGSR
652700940103     C                   Z-ADD     *ZEROS        SDPNSP
652800940103     C                   Z-ADD     *ZEROS        SDPRCP
652900940103     C                   MOVEL     *BLANKS       SDPFTR
653000940103     C                   Z-ADD     *ZEROS        SDPDTR
653100940103     C                   ENDSR
653200931112     C********************************************************************
653300990617     C*   ADDSDP - AGGIUNGE AI VALORI DEL FILE SISDP00F (SALDI CLIENTI-CONS.PART
653400931112     C********************************************************************
653500940103     C     ADDSDP        BEGSR
653600940103     C                   ADD       WPNSP         SDPNSP
653700020129     C                   ADD       WPRCP         SDPRCP
653800940301     C                   MOVEL     *BLANKS       SDPFTR
653900940301     C                   Z-ADD     *ZEROS        SDPDTR
654000940317     C                   MOVEL     'S'           FLAGGS
654100940103     C                   ENDSR
654200931112     C********************************************************************
654300990617     C*   GESSDA - GESTISCE IL FILE SISDA00F (SALDI CLIENTE - AFFIDABIL.)
654400931112     C********************************************************************
654500931112     C     GESSDA        BEGSR
654600940411     C*
654700940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
654800940411     C     WAPREG        IFEQ      'S'
654900940411     C     WADREG        OREQ      'S'
655000931112     C*
655100931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
655200940407     C     D$GSTS        IFEQ      'U'
655300940407     C     D$PAAS        ANDEQ     D$DAAS
655400940125     C     D$PKSC        ANDEQ     D$DKSC
655500941212     C     D$PNRC        ANDEQ     D$DNRC
655600931112     C     WPMES         ANDEQ     WDMES
655700931112     C     D$PTSP        ANDEQ     D$DTSP
655800940411     C     WAPREG        ANDEQ     WADREG
655900931112     C*
656000931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
656100931112     C*
656200931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
656300931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
656400931112     C                   Z-ADD     D$DAAS        SDAANN
656500940125     C                   Z-ADD     D$DKSC        SDAKSC
656600941212     C                   Z-ADD     D$DNRC        SDADLV
656700931112     C                   Z-ADD     WDMES         SDAMES
656800931112     C                   MOVEL     D$DTSP        SDATSP
656900931112     C* VALORI = 'DOPO' - 'PRIMA'
657000940512     C                   Z-ADD     *ZEROS        WANSP
657100931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
657200931112     C*********************EXSR AGGSDA** NON E' NECESSARIO SE C'E' SOLO N.SPED.
657300931112     C*
657400931112     C                   ELSE
657500931112     C*
657600931112     C******************** E' CAMBIATA LA CHIAVE *************************
657700931112     C*
657800940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
657900940405     C     D$GSTS        IFEQ      'U'
658000940405     C     D$GSTS        OREQ      'D'
658100931112     C*
658200931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
658300931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
658400931112     C* CHIAVE = CHIAVE 'PRIMA'
658500931112     C                   Z-ADD     D$PAAS        SDAANN
658600940125     C                   Z-ADD     D$PKSC        SDAKSC
658700941212     C                   Z-ADD     D$PNRC        SDADLV
658800931112     C                   Z-ADD     WPMES         SDAMES
658900931112     C                   MOVEL     D$PTSP        SDATSP
659000931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
659100940411     C     *ZEROS        SUB       1             WANSP
659200931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
659300940411     C     WAPREG        IFEQ      'S'
659400931112     C                   EXSR      AGGSDA
659500940411     C                   ENDIF
659600931112     C*
659700931112     C                   END
659800940202     C*
659900940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
660000940405     C     D$GSTS        IFEQ      'U'
660100940405     C     D$GSTS        OREQ      'I'
660200940202     C*
660300940202     C* NON E' UN ANNULLAMENTO, QUINDI
660400931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
660500931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
660600931112     C* CHIAVE = CHIAVE 'DOPO'
660700931112     C                   Z-ADD     D$DAAS        SDAANN
660800940125     C                   Z-ADD     D$DKSC        SDAKSC
660900941212     C                   Z-ADD     D$DNRC        SDADLV
661000931112     C                   Z-ADD     WDMES         SDAMES
661100931112     C                   MOVEL     D$DTSP        SDATSP
661200931112     C* VALORI = VALORI 'DOPO'
661300940411     C                   Z-ADD     1             WANSP
661400931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
661500940411     C     WADREG        IFEQ      'S'
661600931112     C                   EXSR      AGGSDA
661700940411     C                   ENDIF
661800931112     C*
661900940202     C                   END
662000931112     C                   END
662100940411     C                   END
662200931112     C*
662300931112     C                   ENDSR
662400931112     C********************************************************************
662500990617     C*   AGGSDA - AGGIORNA IL FILE SISDA00F (SALDI CLIENTI - AFFIDAB. )
662600931112     C********************************************************************
662700931112     C     AGGSDA        BEGSR
662800931112     C     WANSP         IFEQ      *ZEROS
662900931112     C                   ELSE
663000990617     C     KEYSDA        CHAIN     SISDA01L                           99
663100931112     C     *IN99         IFEQ      '0'
663200931112     C                   EXSR      ADDSDA
663300990617     C                   UPDATE    SISDA000
663400931112     C                   ELSE
663500931112     C                   EXSR      AZZSDA
663600931112     C                   EXSR      ADDSDA
663700990617     C                   WRITE     SISDA000
663800931112     C                   END
663900931112     C                   END
664000931112     C                   ENDSR
664100931112     C********************************************************************
664200990617     C*   AZZSDA - AZZERA I CAMPI DEL FILE SISDA00F (SALDI CLIENTI - AFFIDAB. )
664300931112     C********************************************************************
664400931112     C     AZZSDA        BEGSR
664500931112     C                   Z-ADD     *ZEROS        SDANSP
664600940301     C                   MOVEL     *BLANKS       SDAFTR
664700940301     C                   Z-ADD     *ZEROS        SDADTR
664800931112     C                   ENDSR
664900931112     C********************************************************************
665000990617     C*   ADDSDA - AGGIUNGE AI VALORI DEL FILE SISDA00F (SALDI CLIENTI-AFFIDAB.
665100931112     C********************************************************************
665200931112     C     ADDSDA        BEGSR
665300931112     C                   ADD       WANSP         SDANSP
665400940301     C                   MOVEL     *BLANKS       SDAFTR
665500940301     C                   Z-ADD     *ZEROS        SDADTR
665600940317     C                   MOVEL     'S'           FLAGGS
665700931112     C                   ENDSR
665800931112     C*
665900931112     C*
666000940906     C*
666100950124     C*
666200030114     C********************************************************************
666300030114     C*   GESCOP - GESTISCE IL FILE SICOP00F (SALDI COOPERATIVE)
666400030114     C********************************************************************
666500030114     C     GESCOP        BEGSR
666600030122     c*
666700030114     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
666800030115IF  1C     WCOPREG       IFEQ      'S'
666900030115     C     WCODREG       OREQ      'S'
667000030115     C*
667100030122     C* PER CIASCUNA VOCE DA SCRIVERE (da scrivere per le COOP)
667200030115DO  2C     1             DO        999           CD
667300030217IF  3C     COPPV(CD)     IFEQ      'S'
667400030217     C     COPDV(CD)     OREQ      'S'
667500030115     C*
667600030115     C* ESEGUO AGGIORNAMENTO SOLO SE VI SONO DATI VALIDI
667700030124IF  4C     IMPV(CD)      IFEQ      *ZEROS
667800030124     C     IMDV(CD)      ANDEQ     *ZEROS
667900030124X   4C                   ELSE
668000030115     C*
668100030120     C* CALCOLO LA FILIALE e LA DATA DA CONSIDERARE
668200030117     C* -PRIMA-
668300030122if  5C     D$GSTS        IFEQ      'U'
668400030120     C     D$GSTS        OREQ      'D'
668500030120     C                   MOVEL     'P'           wpd
668600030217     C                   CLEAR                   WPFIL
668700030217     C                   CLEAR                   WPDAT
668800030217if  6C     COPPV(CD)     IFEQ      'S'
668900030120     c* filiale
669000030217     C                   EVAL      $RIC = RLP(CMPPV(CD))
669100030120     C                   EXSR      REPCDC
669200030120     C                   Z-ADD     $FIL          WPFIL
669300030120     c* data
669400030217     C                   EVAL      $TDAT = DATPV(CD)
669500030120     C                   EXSR      REPDAT
669600030120     C                   Z-ADD     $DAT          WPDAT
669700030217e   6c                   ENDIF
669800030217e   5c                   ENDIF
669900030120     C* -DOPO-
670000030122if  5C     D$GSTS        IFEQ      'U'
670100030120     C     D$GSTS        OREQ      'I'
670200030120     C                   MOVEL     'D'           wpd
670300030217     C                   CLEAR                   WDFIL
670400030217     C                   CLEAR                   WDDAT
670500030217if  6C     COPDV(CD)     IFEQ      'S'
670600030120     c* filiale
670700030122     C                   EVAL      $RIC = RLD(CMPDV(CD))
670800030120     C                   EXSR      REPCDC
670900030120     C                   Z-ADD     $FIL          WDFIL
671000030120     c* data
671100030120     C                   EVAL      $TDAT = DATDV(CD)
671200030120     C                   EXSR      REPDAT
671300030122     C                   Z-ADD     $DAT          WDDAT
671400030217e   6c                   ENDIF
671500030122e   5C                   ENDIF
671600030217     C***********************
671700030217     C***********************
671800030217     C***********************
671900030127     C* SE LE DUE DATE SONO DIVERSE E SONO IN MODIFICA E LA VOCE PREVEDE IL
672000030127     C* FATTO CHE L'IMPORTO VA SEMPRE INSERITO E MAI SOMMATO, ALLORA IMPOSTA UN
672100030127     C* FLAG PER NON ANDARE A MODIFICARE IL -PRIMA- MA INSERIRE SOLO IL -DOPO-
672200030127     c                   MOVEL     'N'           $INS              1
672300030127IF  5C     D$GSTS        IFEQ      'U'
672400030127     C     WPDAT         ANDNE     WDDAT
672500030127     C     AIDDV(CD)     ANDEQ     'S'
672600030127     c                   MOVEL     'S'           $INS
672700030127e   5C                   ENDIF
672800030217     C***********************
672900030217     C***********************
673000030217     C***********************
673100030114     C*
673200030114     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
673300030124IF  5C     D$GSTS        IFEQ      'U'
673400030122     C     WPFIL         ANDEQ     WDFIL
673500030120     C     WPDAT         ANDEQ     WDDAT
673600030120     C     CD            ANDEQ     CD
673700030131     C     WPMES         ANDEQ     WDMES
673800030131     C     D$PAAS        ANDEQ     D$DAAS
673900030120     C     WCOPREG       ANDEQ     WCODREG
674000030114     C*
674100030114     C******************** NON E' CAMBIATA LA CHIAVE *********************
674200030114     C*
674300030114     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
674400030114     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
674500030121     C                   Z-ADD     WDFIL         COPFIL
674600030121     C                   Z-ADD     WDDAT         COPDAT
674700030121     C                   Z-ADD     CD            COPVOC
674800030131     C                   Z-ADD     D$DAAS        COPAAS
674900030131     C                   Z-ADD     WDMES         COPMES
675000030214     c*---
675100030214     c* se una delle due immagini sono uguali a zero, significa che la voce C/E
675200030214     c* è nuova (>0 il -dopo-) o è da eliminare (>0 il -prima-). Questo lo fa
675300030214     c* bene per l'importo perchgè ha la situazione prima (IMPV) e dopo (IMDV)
675400030214     c* memorizzate, ma non per le quantità. Lo fa qui il pgm azzerando le qta
675500030214     c* della bolla per fare bene i calcoli (senno per es, 1 collo era prima, 1
675600030214     c* collo è ora, 1-1 = 0 e scrive l'importo della voce giusto ma su qta a 0)
675700030214     C*---
675800030214     C* VALORI = 'DOPO' - 'PRIMA'
675900030214     C* Voce nuova
676000030214     C                   IF        IMDV(CD) <> 0 AND IMPV(CD) = 0
676100030214     C                   Z-ADD     1             WCONSP
676200030214     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
676300030214     C     D$DNCL        SUB       *ZEROS        WCONCL
676400030214     C     WDPES         SUB       *ZEROS        WCOPKF
676500030214     C     WDVOL         SUB       *ZEROS        WCOVLF
676600040123     C     WDBAN         SUB       *ZEROS        WCOBAN
676700030214     c                   ENDIF
676800030214     C* Voce da eliminare
676900030214     C                   IF        IMDV(CD) = 0 AND IMPV(CD) <> 0
677000030214     C     *ZEROS        SUB       1             WCONSP
677100030214     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
677200030214     C     *ZEROS        SUB       D$PNCL        WCONCL
677300030214     C     *ZEROS        SUB       WPPES         WCOPKF
677400030214     C     *ZEROS        SUB       WPVOL         WCOVLF
677500040123     C     *ZEROS        SUB       WPBAN         WCOBAN
677600030214     c                   ENDIF
677700030214     C* Voce da modificare
677800030214     C                   IF        IMDV(CD) <> 0 AND IMPV(CD) <> 0
677900030121     C                   Z-ADD     *ZEROS        WCONSP
678000030122     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
678100030121     C     D$DNCL        SUB       D$PNCL        WCONCL
678200030121     C     WDPES         SUB       WPPES         WCOPKF
678300030121     C     WDVOL         SUB       WPVOL         WCOVLF
678400040123     C     WDBAN         SUB       WPBAN         WCOBAN
678500030214     c                   ENDIF
678600030214     c*
678700030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
678800030121     C                   EXSR      AGGCOP
678900030114     C*
679000030122X   5C                   ELSE
679100030114     C*
679200030114     C******************** E' CAMBIATA LA CHIAVE *************************
679300030114     C*
679400030114     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
679500030122IF  6C     D$GSTS        IFEQ      'U'
679600030114     C     D$GSTS        OREQ      'D'
679700030114     C*
679800030114     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
679900030114     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
680000030114     C* CHIAVE = CHIAVE 'PRIMA'
680100030217if  7C     COPPV(CD)     IFEQ      'S'
680200030217IF  8C     $INS          IFNE      'S'
680300030122     C                   Z-ADD     WPFIL         COPFIL
680400030122     C                   Z-ADD     WPDAT         COPDAT
680500030122     C                   Z-ADD     CD            COPVOC
680600030131     C                   Z-ADD     D$PAAS        COPAAS
680700030131     C                   Z-ADD     WPMES         COPMES
680800030114     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
680900030122     C     *ZEROS        SUB       1             WCONSP
681000030122     C     *ZEROS        SUB       IMPV(CD)      WCOIMV
681100030122     C     *ZEROS        SUB       D$PNCL        WCONCL
681200030122     C     *ZEROS        SUB       WPPES         WCOPKF
681300030122     C     *ZEROS        SUB       WPVOL         WCOVLF
681400040123     C     *ZEROS        SUB       WPBAN         WCOBAN
681500030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
681600030217IF  9C     WCOPREG       IFEQ      'S'
681700030122     C                   EXSR      AGGCOP
681800030217E   9C                   ENDIF
681900030217E   8C                   ENDIF
682000030217E   7C                   ENDIF
682100030114     C*
682200030122E   6C                   ENDIF
682300030114     C*
682400030114     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
682500030122IF  6C     D$GSTS        IFEQ      'U'
682600030114     C     D$GSTS        OREQ      'I'
682700030114     C*
682800030114     C* NON E' UN ANNULLAMENTO, QUINDI
682900030114     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
683000030114     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
683100030114     C* CHIAVE = CHIAVE 'DOPO'
683200030217if  7C     COPDV(CD)     IFEQ      'S'
683300030122     C                   Z-ADD     WDFIL         COPFIL
683400030122     C                   Z-ADD     WDDAT         COPDAT
683500030122     C                   Z-ADD     CD            COPVOC
683600030131     C                   Z-ADD     D$DAAS        COPAAS
683700030131     C                   Z-ADD     WDMES         COPMES
683800030114     C* VALORI = VALORI 'DOPO'
683900030122     C                   Z-ADD     1             WCONSP
684000030122     C                   Z-ADD     IMDV(CD)      WCOIMV
684100030122     C                   Z-ADD     D$DNCL        WCONCL
684200030122     C                   Z-ADD     WDPES         WCOPKF
684300030122     C                   Z-ADD     WDVOL         WCOVLF
684400040123     C                   Z-ADD     WDBAN         WCOBAN
684500030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
684600030217IF  8C     WCODREG       IFEQ      'S'
684700030122     C                   EXSR      AGGCOP
684800030217E   8C                   ENDIF
684900030217E   7C                   ENDIF
685000030114     C*
685100030122E   6C                   ENDIF
685200030122E   5C                   ENDIF
685300030124E   4C                   ENDIF
685400030122     c*
685500030122E   3C                   ENDIF
685600030122E   2C                   ENDDO
685700030122E   1C                   ENDIF
685800030114     C*
685900030114     C                   ENDSR
686000030114     C********************************************************************
686100030121     C*   AGGCOP - AGGIORNA IL FILE SICOP00F (SALDI COOPERATIVE)
686200030114     C********************************************************************
686300030115     C     AGGCOP        BEGSR
686400040126     c*
686500040126     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
686600040126     c* (equivalente a serie > 0) e particolarità consegne bancali
686700040126if  1c                   IF        copvoc = 34 and
686800040126     c                             ((d$gsto = 'I' and d$dnrs > 0 and
686900040126     c                              %subst(d$dgva: 1: 1) = 'O') or
687000040126     c                             (d$gsto = 'U' and d$dnrs > 0 and
687100040126     c                              %subst(d$dgva: 1: 1) = 'O') or
687200040126     c                             (d$gsto = 'D' and d$pnrs > 0 and
687300040126     c                              %subst(d$pgva: 1: 1) = 'O'))
687400040126     c                   z-add     0             wcoban
687500040126e   1c                   endif
687600040119     c*
687700030122IF  1C     WCOIMV        IFEQ      *ZEROS
687800030122     C     WCONSP        ANDEQ     *ZEROS
687900030122     C     WCONCL        ANDEQ     *ZEROS
688000030121     C     WCOPKF        ANDEQ     *ZEROS
688100030122     C     WCOVLF        ANDEQ     *ZEROS
688200040119     C     WCOBAN        ANDEQ     *ZEROS
688300030122X   1C                   ELSE
688400030122     C     KEYCOP        CHAIN     SICOP01L                           99
688500030122IF  2C     *IN99         IFEQ      '0'
688600030122     C                   EXSR      ADDCOP
688700030122     C                   UPDATE    SICOP000
688800030122X   2C                   ELSE
688900030122     C                   EXSR      AZZCOP
689000030122     C                   EXSR      ADDCOP
689100030122     C                   WRITE     SICOP000
689200030122E   2C                   ENDIF
689300030122E   1C                   ENDIF
689400030122     C*
689500030114     C                   ENDSR
689600030114     C********************************************************************
689700030122     C*   AZZCOP - AZZERA I CAMPI DEL FILE SICOP00F (SALDI COOPERATIVE)
689800030114     C********************************************************************
689900030115     C     AZZCOP        BEGSR
690000030122     C*
690100030122     C                   Z-ADD     *ZEROS        COPIMV
690200030122     C                   Z-ADD     *ZEROS        COPNSP
690300030122     C                   Z-ADD     *ZEROS        COPNCL
690400030122     C                   Z-ADD     *ZEROS        COPPKF
690500030122     C                   Z-ADD     *ZEROS        COPVLF
690600031029     C                   Z-ADD     *ZEROS        COPBAN
690700030122     C*
690800030114     C                   ENDSR
690900030114     C********************************************************************
691000030122     C*   ADDCOP - AGGIUNGE AI VALORI DEL FILE SICOP00F (SALDI COOPERATIVE)
691100030114     C********************************************************************
691200030115     C     ADDCOP        BEGSR
691300030122     C*
691400090916     C                   z-add(h)  WCOIMV        wcoimv£
691500090916     C                   ADD       WCOIMV£       COPIMV
691600030122     C                   ADD       WCONSP        COPNSP
691700030122     C                   ADD       WCONCL        COPNCL
691800030122     C                   ADD       WCOPKF        COPPKF
691900030122     C                   ADD       WCOVLF        COPVLF
692000031029     C                   ADD       WCOBAN        COPBAN
692100030114     C                   MOVEL     'S'           FLAGGS
692200030122     C*
692300030114     C                   ENDSR
692400950124     C*------------------------------------------------------------------------*
692500950124     C*?GESCCD - AGGIORNA LE SCHIERE COD.E IMP. COMPETENZE/RICAVI IN OUTPUT
692600950124     C*------------------------------------------------------------------------*
692700950124     C     GESCCD        BEGSR
692800950124     C*
692900950124     C                   Z-ADD     0             II
693000030505     C     1             DO        75            I
693100950124     C                   Z-ADD     *ZEROS        DSWCN
693200950124     C                   MOVE      DSWCA         CAN(I)
693300950124     C                   Z-ADD     *ZEROS        DSWIN
693400950124     C                   MOVE      DSWIA         IAN(I)
693500950124     C                   ENDDO
693600950124     C     1             DO        999           I
693700950124     C     IMD(I)        IFNE      *ZEROS
693800950124     C                   ADD       1             II
693900950124     C                   Z-ADD     I             DSWCN
694000950124     C                   MOVE      DSWCA         CAN(II)
694100950124     C                   Z-ADD     IMD(I)        DSWIN
694200950124     C                   MOVE      DSWIA         IAN(II)
694300950124     C                   ENDIF
694400950124     C                   ENDDO
694500950124     C                   MOVEA     CAN           D$DCCD
694600950124     C                   MOVEA     IAN           D$DICD
694700931112     C*
694800950124     C                   ENDSR
694900941026     C*------------------------------------------------------------------------*
695000990617     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SISDE (ERRORI SALDI)
695100941026     C*------------------------------------------------------------------------*
695200941026     C     SCRERR        BEGSR
695300941026     C*
695400941026     C                   MOVEL     'S'           $ERR
695500941026     C*
695600941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
695700941026     C     D$GERR        IFEQ      *BLANKS
695800941026     C*
695900941026     C                   EXSR      SUBDAT
696000941026     C                   Z-ADD     DSDAT8        ERRDAT
696100941026     C                   Z-ADD     DTORA6        ERRTIM
696200941026     C                   ADD       1             D$GPRE
696300941026     C                   Z-ADD     D$GPRE        ERRPRG
696400941201     C     D$GSTS        IFEQ      'I'
696500941124     C                   Z-ADD     D$DAAS        ERRAAS
696600941026     C                   MOVEL     D$DTBL        ERRTBL
696700941026     C                   Z-ADD     D$DLNP        ERRLNP
696800941026     C                   Z-ADD     D$DNRS        ERRNRS
696900941026     C                   Z-ADD     D$DNSP        ERRNSP
697000941201     C                   Z-ADD     D$DAAS        DSAAS
697100941123     C                   Z-ADD     D$DMGS        DSMGS
697200941123     C                   Z-ADD     DSDAT8        ERRDSP
697300941201     C                   ELSE
697400941201     C                   Z-ADD     D$PAAS        ERRAAS
697500941201     C                   MOVEL     D$PTBL        ERRTBL
697600941201     C                   Z-ADD     D$PLNP        ERRLNP
697700941201     C                   Z-ADD     D$PNRS        ERRNRS
697800941201     C                   Z-ADD     D$PNSP        ERRNSP
697900941201     C                   Z-ADD     D$PAAS        DSAAS
698000941201     C                   Z-ADD     D$PMGS        DSMGS
698100941201     C                   Z-ADD     DSDAT8        ERRDSP
698200941201     C                   ENDIF
698300941026     C                   MOVEL     $MSG          DSMSG
698400941026     C                   MOVEL     DSDER         ERRDER
698500941026     C                   MOVEL     DSCER         ERRCER
698600950123     C                   MOVEL     D$GSTO        ERRSTS
698700941026     C                   Z-ADD     D$GDAT        ERRDAM
698800941026     C                   Z-ADD     D$GTIM        ERRORM
698900941026     C                   MOVEL     D$GUSR        ERRUSR
699000941026     C                   MOVEL     D$GPGF        ERRPGF
699100950118     C                   MOVEL     D$GFIL        ERRFIL
699200950118     C                   MOVEL     D$GMBR        ERRMBR
699300950118     C                   MOVEL     D$GLIB        ERRLIB
699400990806     C     D$GFIL        IFEQ      'TITAS10F'
699500020527     C     D$GFIL        OREQ      'TITAS00F'
699600950118     C                   MOVEL     D$PRE1        ERRRP1
699700950118     C                   MOVEL     D$PRE2        ERRRP2
699800950118     C                   MOVEL     D$DRE1        ERRRD1
699900950118     C                   MOVEL     D$DRE2        ERRRD2
700000950123     C                   ENDIF
700100950123     C     D$GFIL        IFEQ      'TNCSB00F'
700200950123     C                   MOVEL     D$PCO1        ERRRP1
700300950123     C                   MOVEL     D$PCO2        ERRRP2
700400950123     C                   MOVEL     D$DCO1        ERRRD1
700500950123     C                   MOVEL     D$DCO2        ERRRD2
700600950123     C                   ENDIF
700700031127     C     D$GFIL        IFEQ      'FIAR500F'
700800031127     C     D$GTRD        IFEQ      'TRS'
700900031127     C                   MOVEL     D$PAR5T       ERRRP1
701000031127     C                   MOVEL     D$DAR5T       ERRRD1
701100031127     C                   ENDIF
701200031127     C     D$GTRD        IFEQ      'BAN'
701300031127     C     D$GTRD        OREQ      'BNB'
701400031127     C                   MOVEL     D$PAR5B       ERRRP1
701500031127     C                   MOVEL     D$DAR5B       ERRRD1
701600031127     C                   ENDIF
701700031127     C                   ENDIF
701800941026     C                   WRITE     ECERR
701900941026     C*
702000941026     C                   MOVEL     DSCER         D$GERR
702100941026     C                   MOVEL     DSDER         D$GMSG
702200941026     C*
702300941026     C                   ENDIF
702400941026     C*
702500941026     C                   ENDSR
702600941026     C*------------------------------------------------------------------------*
702700941026     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
702800941026     C*------------------------------------------------------------------------*
702900941026     C     SUBDAT        BEGSR
703000941026     C*
703100941026     C                   TIME                    DTTIME
703200941026     C                   Z-ADD     DTANN         DSANN
703300941026     C                   Z-ADD     DTMES         DSMES
703400941026     C                   Z-ADD     DTGIO         DSGIO
703500941026     C                   Z-ADD     DTSEC         DSSEC
703600941026     C*
703700941026     C                   ENDSR
703800940808     C********************************************************************
703900940808     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
704000940808     C********************************************************************
704100940808     C     AZZW          BEGSR
704200940808     C*
704300940808     C                   Z-ADD     *ZEROS        WPMES
704400940808     C                   Z-ADD     *ZEROS        WDMES
704500940808     C                   MOVEL     *BLANKS       WPDEC
704600940808     C                   MOVEL     *BLANKS       WDDEC
704700940808     C                   MOVEL     *BLANKS       WPFPR
704800940808     C                   MOVEL     *BLANKS       WDFPR
704900940808     C                   MOVEL     *BLANKS       WPFPC
705000940808     C                   MOVEL     *BLANKS       WDFPC
705100940808     C                   MOVEL     *BLANKS       WPFPT
705200940808     C                   MOVEL     *BLANKS       WDFPT
705300940808     C                   MOVEL     *BLANKS       WPTPR
705400940808     C                   MOVEL     *BLANKS       WDTPR
705500940809     C                   Z-ADD     *ZEROS        WPIRP
705600940808     C                   Z-ADD     *ZEROS        WDIRP
705700030131     C                   Z-ADD     *ZEROS        WPDATCOP
705800030131     C                   Z-ADD     *ZEROS        WDDATCOP
705900940808     C                   Z-ADD     *ZEROS        WPVOL
706000940808     C                   Z-ADD     *ZEROS        WDVOL
706100940808     C                   Z-ADD     *ZEROS        WPVCC
706200940808     C                   Z-ADD     *ZEROS        WDVCC
706300940808     C                   Z-ADD     *ZEROS        WPVLS
706400940808     C                   Z-ADD     *ZEROS        WDVLS
706500020829     C                   Z-ADD     *ZEROS        WPPES
706600020829     C                   Z-ADD     *ZEROS        WDPES
706700020829     C                   Z-ADD     *ZEROS        WPPCC
706800020829     C                   Z-ADD     *ZEROS        WDPCC
706900020829     C                   Z-ADD     *ZEROS        WPPLS
707000020829     C                   Z-ADD     *ZEROS        WDPLS
707100940808     C                   Z-ADD     *ZEROS        WPPTS
707200940808     C                   Z-ADD     *ZEROS        WDPTS
707300940808     C                   Z-ADD     *ZEROS        WPIRB
707400940808     C                   Z-ADD     *ZEROS        WDIRB
707500941024     C                   Z-ADD     *ZEROS        WPIRBC
707600941024     C                   Z-ADD     *ZEROS        WDIRBC
707700940808     C                   Z-ADD     *ZEROS        WPIRD
707800940808     C                   Z-ADD     *ZEROS        WDIRD
707900940930     C                   Z-ADD     *ZEROS        WPIR
708000940930     C                   Z-ADD     *ZEROS        WDIR
708100941209     C                   Z-ADD     *ZEROS        WOIR
708200940808     C                   Z-ADD     *ZEROS        WPREP
708300940808     C                   Z-ADD     *ZEROS        WDREP
708400940808     C                   Z-ADD     *ZEROS        WPREA
708500940808     C                   Z-ADD     *ZEROS        WDREA
708600940823     C                   Z-ADD     *ZEROS        WPCTL
708700940823     C                   Z-ADD     *ZEROS        WDCTL
708800940823     C                   Z-ADD     *ZEROS        WPCT1
708900940823     C                   Z-ADD     *ZEROS        WDCT1
709000940823     C                   Z-ADD     *ZEROS        WPCT2
709100940823     C                   Z-ADD     *ZEROS        WDCT2
709200040519     C                   Z-ADD     *ZEROS        WPCT3
709300040519     C                   Z-ADD     *ZEROS        WDCT3
709400040519     C                   Z-ADD     *ZEROS        WPCT4
709500040519     C                   Z-ADD     *ZEROS        WDCT4
709600040519     C                   Z-ADD     *ZEROS        WPCT5
709700040519     C                   Z-ADD     *ZEROS        WDCT5
709800980313     C                   Z-ADD     *ZEROS        WPSMC
709900980313     C                   Z-ADD     *ZEROS        WPSMCT
710000980313     C                   Z-ADD     *ZEROS        WDSMC
710100980319     C                   Z-ADD     *ZEROS        WSMC
710200940808     C                   MOVEA     *BLANKS       SVP
710300940808     C                   MOVEA     *BLANKS       SVD
710400940808     C                   MOVEA     *BLANKS       SVX
710500940808     C                   Z-ADD     *ZEROS        IVP
710600940808     C                   Z-ADD     *ZEROS        IVD
710700940808     C                   Z-ADD     *ZEROS        IVX
710800940808     C                   Z-ADD     *ZEROS        NSP
710900940808     C                   Z-ADD     *ZEROS        NSD
711000940808     C                   Z-ADD     *ZEROS        NSX
711100940808     C                   MOVEA     *BLANKS       CPP
711200940808     C                   MOVEA     *BLANKS       CPD
711300940808     C                   MOVEA     *BLANKS       CPX
711400990617     C                   MOVEL     'S'           WAPREG            1
711500990617     C                   MOVEL     'S'           WADREG            1
711600990617     C                   MOVEL     'S'           WCPREG            1
711700990617     C                   MOVEL     'S'           WCDREG            1
711800040317     C                   MOVEL     'S'           WePREG            1
711900040317     C                   MOVEL     'S'           WeDREG            1
712000990617     C                   MOVEL     'S'           WOPREG            1
712100990617     C                   MOVEL     'S'           WODREG            1
712200990617     C                   MOVEL     'S'           WRPREG            1
712300990617     C                   MOVEL     'S'           WRDREG            1
712400990617     C                   MOVEL     'S'           WFPREG            1
712500990617     C                   MOVEL     'S'           WFDREG            1
712600990617     C                   MOVEL     'S'           WTPREG            1
712700990617     C                   MOVEL     'S'           WTDREG            1
712800990617     C                   MOVEL     'S'           WPPREG            1
712900990617     C                   MOVEL     'S'           WPDREG            1
713000980323     C                   MOVEL     'N'           WMPREG            1
713100980323     C                   MOVEL     'N'           WMDREG            1
713200030124     C                   MOVEL     'S'           WCOPREG           1
713300030115     C                   MOVEL     'S'           WCODREG           1
713400950104     C                   MOVEL     'N'           WFCP              1
713500950104     C                   MOVEL     'N'           WFCD              1
713600941215     C                   CLEAR                   IMP
713700941215     C                   CLEAR                   IMD
713800030114     C                   CLEAR                   IMPV
713900030214     C                   CLEAR                   COPPV
714000030217     C                   CLEAR                   DATPV
714100030217     C                   CLEAR                   AIDPV
714200030217     C                   CLEAR                   CMPPV
714300030114     C                   CLEAR                   IMDV
714400030214     C                   CLEAR                   COPDV
714500030214     C                   CLEAR                   DATDV
714600030214     C                   CLEAR                   AIDDV
714700030214     C                   CLEAR                   CMPDV
714800031029     C                   CLEAR                   WDBAN
714900031029     C                   CLEAR                   WPBAN
715000940808     C*
715100940808     C                   ENDSR
715200930414     C********************************************************************
715300931105     C*   RUTINZ - ROUTINE INIZIALE
715400930414     C********************************************************************
715500931105     C     RUTINZ        BEGSR
715600930414     C*
715700930414     C* PARAMETRI IN INPUT ALLA ROUTINE
715800930414     C     *ENTRY        PLIST
715900940808     C                   PARM                    DS$ECO
716000930414     C*
716100930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
716200930414     C     KEYTAB        KLIST
716300930414     C                   KFLD                    KTBUTE
716400930414     C                   KFLD                    KTBCOD
716500930414     C                   KFLD                    KTBKEY
716600941223     C*
716700941223     C* CHIAVE LETTURA TABEL00F - PARZIALE
716800941223     C     KEYTA2        KLIST
716900941223     C                   KFLD                    KTBUTE
717000941223     C                   KFLD                    KTBCOD
717100011218     C*
717200011218     C* CHIAVE LETTURA TNTBE01L - PARZIALE
717300011218     C     KEYTBE        KLIST
717400011218     C                   KFLD                    KTECOD
717500011218     C                   KFLD                    KTEKE1
717600020705     C*
717700020705     C* CHIAVE LETTURA TNTBE01L - PARZIALE
717800020705     C     KEYTB2        KLIST
717900020705     C                   KFLD                    KTECOD
718000931111     C*
718100931111     C* CHIAVE LETTURA AZORG01L - COMPLETA
718200931111     C     KEYORG        KLIST
718300931111     C                   KFLD                    KORFIL
718400930414     C*
718500990617     C* CHIAVE LETTURA SISDR01L - COMPLETA
718600931104     C     KEYSDR        KLIST
718700931104     C                   KFLD                    SDRANN
718800931104     C                   KFLD                    SDRFLP
718900931104     C                   KFLD                    SDRFLA
719000940208     C                   KFLD                    SDRMES
719100931104     C                   KFLD                    SDRFPE
719200931104     C                   KFLD                    SDRTSP
719300931104     C                   KFLD                    SDRTPO
719400931104     C                   KFLD                    SDRRBL
719500931104     C                   KFLD                    SDRTPR
719600931104     C*
719700990617     C* CHIAVE LETTURA SISDF01L - COMPLETA
719800931104     C     KEYSDF        KLIST
719900931104     C                   KFLD                    SDFANN
720000931104     C                   KFLD                    SDFMES
720100931104     C                   KFLD                    SDFFIL
720200931110     C*
720300990617     C* CHIAVE LETTURA SISDT01L - COMPLETA
720400931110     C     KEYSDT        KLIST
720500931110     C                   KFLD                    SDTANN
720600931110     C                   KFLD                    SDTTFP
720700931110     C                   KFLD                    SDTTFA
720800931110     C                   KFLD                    SDTFPE
720900931110     C                   KFLD                    SDTMES
721000940304     C                   KFLD                    SDTDEC
721100940304     C                   KFLD                    SDTTT1
721200940304     C                   KFLD                    SDTTT2
721300040519     C                   KFLD                    SDTTT3
721400040519     C                   KFLD                    SDTTT4
721500040519     C                   KFLD                    SDTTT5
721600931111     C*
721700990617     C* CHIAVE LETTURA SISDC01L - COMPLETA
721800931111     C     KEYSDC        KLIST
721900940209     C                   KFLD                    SDCKSC
722000931111     C                   KFLD                    SDCANN
722100940209     C                   KFLD                    SDCMES
722200931111     C                   KFLD                    SDCREP
722300931111     C                   KFLD                    SDCREA
722400931111     C                   KFLD                    SDCFPE
722500931111     C                   KFLD                    SDCTSP
722600931111     C                   KFLD                    SDCCTR
722700931111     C                   KFLD                    SDCTPO
722800931111     C                   KFLD                    SDCRBL
722900931111     C                   KFLD                    SDCTPR
723000931111     C*
723100990617     C* CHIAVE LETTURA SISDO01L - COMPLETA
723200931111     C     KEYSDO        KLIST
723300940209     C                   KFLD                    SDOKSC
723400931111     C                   KFLD                    SDOANN
723500931111     C                   KFLD                    SDOMES
723600931111     C*
723700990617     C* CHIAVE LETTURA SISDA01L - COMPLETA
723800931111     C     KEYSDA        KLIST
723900940209     C                   KFLD                    SDAKSC
724000931111     C                   KFLD                    SDAANN
724100940209     C                   KFLD                    SDAMES
724200940125     C                   KFLD                    SDADLV
724300931111     C                   KFLD                    SDATSP
724400931111     C*
724500990617     C* CHIAVE LETTURA SISDP01L - COMPLETA
724600940103     C     KEYSDP        KLIST
724700940317     C                   KFLD                    SDPKSC
724800940103     C                   KFLD                    SDPANN
724900940103     C                   KFLD                    SDPMES
725000940316     C                   KFLD                    SDPCTR
725100940316     C                   KFLD                    SDPSVR
725200940103     C                   KFLD                    SDPFTC
725300040316     C* CHIAVE LETTURA SISDC01L - COMPLETA
725400040316     C     KEYSDE        KLIST
725500040316     C                   KFLD                    SDEFILE
725600040316     C                   KFLD                    SDETRD
725700040316     C                   KFLD                    SDEKSC
725800040316     C                   KFLD                    SDEANN
725900040316     C                   KFLD                    SDEMES
726000040316     C                   KFLD                    SDEREP
726100040316     C                   KFLD                    SDEREA
726200040316     C                   KFLD                    SDEFPE
726300040316     C                   KFLD                    SDETSP
726400040316     C                   KFLD                    SDECTR
726500040316     C                   KFLD                    SDETPO
726600040316     C                   KFLD                    SDERBL
726700040316     C                   KFLD                    SDETPR
726800030123     C*
726900030123     C* CHIAVE LETTURA SICOP01L - COMPLETA
727000030123     C     KEYCOP        KLIST
727100030123     C                   KFLD                    COPFIL
727200030123     C                   KFLD                    COPDAT
727300030123     C                   KFLD                    COPVOC
727400030212     C                   KFLD                    COPAAS
727500030212     C                   KFLD                    COPMES
727600941026     C*
727700941026     C* CHIAVE LETTURA ECERR01L - COMPLETA
727800941026     C     KEYERR        KLIST
727900941026     C                   KFLD                    ERRDAT
728000941026     C                   KFLD                    ERRTIM
728100941026     C                   KFLD                    ERRPRG
728200011210     C*
728300011210     C* CHIAVE LETTURA ECMOV01L - COMPLETA
728400011210     C     KEYMOV        KLIST
728500011210     C                   KFLD                    MOVSOC
728600011210     C                   KFLD                    MOVCDP
728700011210     C                   KFLD                    MOVSVO
728800011210     C                   KFLD                    MOVCDC
728900011210     C                   KFLD                    MOVDA
729000020304     C                   KFLD                    MOVDRG
729100941215     C*
729200950316     C* CHIAVE LETTURA ECCET30C - COMPLETA
729300941215     C     KEYCET        KLIST
729400941215     C                   KFLD                    CETAAS
729500941215     C                   KFLD                    CETLNP
729600941215     C                   KFLD                    CETNRS
729700941215     C                   KFLD                    CETNSP
729800941215     C                   KFLD                    CETTBL
729900941215     C*
730000950316     C* CHIAVE LETTURA ECCED30C - PARZIALE
730100941215     C     KEYCEP        KLIST
730200941215     C                   KFLD                    CEDAAS
730300941215     C                   KFLD                    CEDLNP
730400941215     C                   KFLD                    CEDNRS
730500941215     C                   KFLD                    CEDNSP
730600941215     C                   KFLD                    CEDTBL
730700941215     C*
730800950316     C* CHIAVE LETTURA ECCED30C - COMPLETA
730900941215     C     KEYCED        KLIST
731000941215     C                   KFLD                    CEDAAS
731100941215     C                   KFLD                    CEDLNP
731200941215     C                   KFLD                    CEDNRS
731300941215     C                   KFLD                    CEDNSP
731400941215     C                   KFLD                    CEDTBL
731500941215     C                   KFLD                    CEDCMP
731600030114     C*
731700030114     C* CHIAVE LETTURA ECCEV30C - COMPLETA
731800030114     C     KEYCEV        KLIST
731900030114     C                   KFLD                    CEVAAS
732000030114     C                   KFLD                    CEVLNP
732100030114     C                   KFLD                    CEVNRS
732200030114     C                   KFLD                    CEVNSP
732300030114     C                   KFLD                    CEVTBL
732400030512     C*
732500030512     C* CHIAVE LETTURA ECC2V30C - COMPLETA
732600030512     C     KEYC2V        KLIST
732700030512     C                   KFLD                    C2VAAS
732800030512     C                   KFLD                    C2VLNP
732900030512     C                   KFLD                    C2VNRS
733000030512     C                   KFLD                    C2VNSP
733100030512     C                   KFLD                    C2VTBL
733200980313     C*
733300980325     C* CHIAVE LETTURA ECFIM30C - PARZIALE
733400980313     C     KEYFM1        KLIST
733500980313     C                   KFLD                    FIMAAS
733600980313     C                   KFLD                    FIMLNP
733700980313     C                   KFLD                    FIMNRS
733800980313     C                   KFLD                    FIMNSP
733900980313     C                   KFLD                    FIMTBL
734000980313     C*
734100980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
734200980313     C     KEYFIM        KLIST
734300980313     C                   KFLD                    FIMAAS
734400980313     C                   KFLD                    FIMLNP
734500980313     C                   KFLD                    FIMNRS
734600980313     C                   KFLD                    FIMNSP
734700980313     C                   KFLD                    FIMTBL
734800980313     C                   KFLD                    FIMDDO
734900941215     C*
735000941215     C* CHIAVE LETTURA ECECT01L - PARZIALE
735100941215     C     KEYECT        KLIST
735200941215     C                   KFLD                    ECTNBC
735300941215     C*
735400941215     C* CHIAVE LETTURA ECECD01L - PARZIALE
735500941215     C     KEYECD        KLIST
735600941215     C                   KFLD                    ECDNBC
735700941215     C                   KFLD                    ECDCMP
735800020531     C*
735900020531     C* CHIAVE LETTURA FNAPD01L - COMPLETA
736000020531     C     KEYAPD        KLIST
736100021205     C                   KFLD                    APDTIP
736200021205     C                   KFLD                    APDPDR
736300020531     C*
736400020531     C* CHIAVE LETTURA FIFCE01L - COMPLETA
736500020531     C     KEYFCE        KLIST
736600020531     C                   KFLD                    FCEFGS
736700020531     C                   KFLD                    FCEPDR
736800020531     C                   KFLD                    FCETSR
736900020531     C                   KFLD                    FCENDC
737000020531     C                   KFLD                    FCEDDC
737100020531     C                   KFLD                    FCEAAS
737200020531     C                   KFLD                    FCELNP
737300020531     C                   KFLD                    FCENRS
737400020531     C                   KFLD                    FCENSP
737500020531     C                   KFLD                    FCECCE
737600030217     C*
737700030217     C* CHIAVE LETTURA ECEVT01L - COMPLETA
737800030217     C     KEYEVT        KLIST
737900030217     C                   KFLD                    EVTVOC
738000030217     C                   KFLD                    EVTPRG
738100930414     C*
738200931111     C* DEFINISCO CAMPI
738300930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
738400930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
738500930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
738600931111     C     *LIKE         DEFINE    ORGFIL        KORFIL
738700011218     C     *LIKE         DEFINE    TBECOD        KTECOD
738800011218     C     *LIKE         DEFINE    TBEKE1        KTEKE1
738900931111     C* -PRIMA- E -DOPO-
739000931111     C     *LIKE         DEFINE    SDRMES        WPMES
739100931111     C     *LIKE         DEFINE    SDRMES        WDMES
739200940103     C     *LIKE         DEFINE    SDRFPE        WPFPR
739300940103     C     *LIKE         DEFINE    SDRFPE        WDFPR
739400940103     C     *LIKE         DEFINE    SDCFPE        WPFPC
739500940103     C     *LIKE         DEFINE    SDCFPE        WDFPC
739600940103     C     *LIKE         DEFINE    SDTFPE        WPFPT
739700940103     C     *LIKE         DEFINE    SDTFPE        WDFPT
739800940304     C     *LIKE         DEFINE    SDTDEC        WPDEC
739900940304     C     *LIKE         DEFINE    SDTDEC        WDDEC
740000931111     C     *LIKE         DEFINE    SDRTPR        WPTPR
740100931111     C     *LIKE         DEFINE    SDRTPR        WDTPR
740200031029     C     *LIKE         DEFINE    SDRVOL        WPVOL
740300931111     C     *LIKE         DEFINE    SDRVOL        WDVOL
740400931111     C     *LIKE         DEFINE    SDCVCC        WPVCC
740500931111     C     *LIKE         DEFINE    SDCVCC        WDVCC
740600931111     C     *LIKE         DEFINE    SDCVLS        WPVLS
740700931111     C     *LIKE         DEFINE    SDCVLS        WDVLS
740800020829     C     *LIKE         DEFINE    SDRPKG        WPPES
740900020829     C     *LIKE         DEFINE    SDRPKG        WDPES
741000020829     C     *LIKE         DEFINE    SDCPCC        WPPCC
741100020829     C     *LIKE         DEFINE    SDCPCC        WDPCC
741200020829     C     *LIKE         DEFINE    SDCPLS        WPPLS
741300020829     C     *LIKE         DEFINE    SDCPLS        WDPLS
741400931111     C     *LIKE         DEFINE    SDRPTS        WPPTS
741500931111     C     *LIKE         DEFINE    SDRPTS        WDPTS
741600941201     C     *LIKE         DEFINE    SDRIRB        WIMR
741700941201     C     *LIKE         DEFINE    SDRIRB        WIRBE
741800941201     C     *LIKE         DEFINE    SDCIRB        WIRDN
741900941201     C     *LIKE         DEFINE    SDRIRB        WIRB
742000931111     C     *LIKE         DEFINE    SDRIRB        WPIRB
742100931111     C     *LIKE         DEFINE    SDRIRB        WDIRB
742200941201     C     *LIKE         DEFINE    SDCIRD        WIRD
742300941201     C     *LIKE         DEFINE    SDCIRD        WPIRD
742400931111     C     *LIKE         DEFINE    SDCIRD        WDIRD
742500941201     C     *LIKE         DEFINE    SDCIRB        WIRBC
742600931112     C     *LIKE         DEFINE    SDCIRB        WPIRBC
742700931112     C     *LIKE         DEFINE    SDCIRB        WDIRBC
742800090914     C*    *LIKE         DEFINE    SDCIRP        WPIRP
742900090914     C*    *LIKE         DEFINE    SDCIRP        WDIRP
743000090914     C     *LIKE         DEFINE    SDCIRP        WPIR
743100090914     C     *LIKE         DEFINE    SDCIRP        WDIR
743200931111     C     *LIKE         DEFINE    ORGCRE        WPREP
743300931111     C     *LIKE         DEFINE    ORGCRE        WDREP
743400931111     C     *LIKE         DEFINE    ORGCRE        WPREA
743500931111     C     *LIKE         DEFINE    ORGCRE        WDREA
743600090914     C*    *LIKE         DEFINE    SDTCTL        WPCTL
743700090914     C*    *LIKE         DEFINE    SDTCTL        WDCTL
743800090914     C*    *LIKE         DEFINE    SDTCT1        WPCT1
743900090914     C*    *LIKE         DEFINE    SDTCT1        WDCT1
744000090914     C*    *LIKE         DEFINE    SDTCT2        WPCT2
744100090914     C*    *LIKE         DEFINE    SDTCT2        WDCT2
744200090914     C*    *LIKE         DEFINE    SDTCT3        WPCT3
744300090914     C*    *LIKE         DEFINE    SDTCT3        WDCT3
744400090914     C*    *LIKE         DEFINE    SDTCT4        WPCT4
744500090914     C*    *LIKE         DEFINE    SDTCT4        WDCT4
744600090914     C*    *LIKE         DEFINE    SDTCT5        WPCT5
744700090914     C*    *LIKE         DEFINE    SDTCT5        WDCT5
744800990617     C* FILE SISDR
744900931111     C     *LIKE         DEFINE    SDRNSP        WRNSP
745000020829     C*--- *LIKE         DEFINE    SDRPKG        WRPKG
745100020829     C     *LIKE         DEFINE    SDRPKG        WRPES
745200931111     C     *LIKE         DEFINE    SDRNCO        WRNCO
745300931111     C     *LIKE         DEFINE    SDRVOL        WRVOL
745400931111     C     *LIKE         DEFINE    SDRPTS        WRPTS
745500931111     C     *LIKE         DEFINE    SDRIRB        WRIRB
745600990617     C* FILE SISDF
745700931111     C     *LIKE         DEFINE    SDFNSP        WFNSP
745800020829     C*--- *LIKE         DEFINE    SDFPKG        WFPKG
745900020829     C     *LIKE         DEFINE    SDFPKG        WFPES
746000931111     C     *LIKE         DEFINE    SDFNCO        WFNCO
746100931111     C     *LIKE         DEFINE    SDFVOL        WFVOL
746200931111     C     *LIKE         DEFINE    SDFPTS        WFPTS
746300931111     C     *LIKE         DEFINE    SDFIRB        WFIRB
746400990617     C* FILE SISDT
746500931111     C     *LIKE         DEFINE    SDTNSP        WTNSP
746600020829     C*--- *LIKE         DEFINE    SDTPKG        WTPKG
746700020829     C     *LIKE         DEFINE    SDTPKG        WTPES
746800931111     C     *LIKE         DEFINE    SDTNCO        WTNCO
746900931111     C     *LIKE         DEFINE    SDTVOL        WTVOL
747000090914     C*    *LIKE         DEFINE    SDTCTL        WTCTL
747100090914     C*    *LIKE         DEFINE    SDTCT1        WTCT1
747200090914     C*    *LIKE         DEFINE    SDTCT2        WTCT2
747300090914     C*    *LIKE         DEFINE    SDTCT3        WTCT3
747400090914     C*    *LIKE         DEFINE    SDTCT4        WTCT4
747500090914     C*    *LIKE         DEFINE    SDTCT5        WTCT5
747600990617     C* FILE SISDC
747700931112     C     *LIKE         DEFINE    SDCNSP        WCNSP
747800020829     C*--- *LIKE         DEFINE    SDCPKG        WCPKG
747900020829     C     *LIKE         DEFINE    SDCPKG        WCPES
748000020829     C     *LIKE         DEFINE    SDCPCC        WCPCC
748100020829     C     *LIKE         DEFINE    SDCPLS        WCPLS
748200931112     C     *LIKE         DEFINE    SDCNCO        WCNCO
748300931112     C     *LIKE         DEFINE    SDCVOL        WCVOL
748400931112     C     *LIKE         DEFINE    SDCVCC        WCVCC
748500931112     C     *LIKE         DEFINE    SDCVLS        WCVLS
748600931112     C     *LIKE         DEFINE    SDCIRB        WCIRB
748700090914     C*    *LIKE         DEFINE    SDCIRP        WCIRP
748800931112     C     *LIKE         DEFINE    SDCIRD        WCIRD
748900941117     C     *LIKE         DEFINE    SDCKSC        WCPKSC
749000941117     C     *LIKE         DEFINE    SDCKSC        WCDKSC
749100990617     C* FILE SISDO
749200931112     C     *LIKE         DEFINE    SDONSP        WONSP
749300020829     C*--- *LIKE         DEFINE    SDOPKG        WOPKG
749400020829     C     *LIKE         DEFINE    SDOPKG        WOPES
749500931112     C     *LIKE         DEFINE    SDONCO        WONCO
749600931112     C     *LIKE         DEFINE    SDOVOL        WOVOL
749700931112     C     *LIKE         DEFINE    SDOIRB        WOIRB
749800090914     C*    *LIKE         DEFINE    SDOIRP        WOIRP
749900931112     C     *LIKE         DEFINE    SDOIRD        WOIRD
750000990617     C* FILE SISDP
750100940103     C     *LIKE         DEFINE    SDPNSP        WPNSP
750200940103     C     *LIKE         DEFINE    SDPRCP        WPRCP
750300940317     C     *LIKE         DEFINE    SDPSVR        WPSV1
750400940317     C     *LIKE         DEFINE    SDPSVR        WPSV2
750500940317     C     *LIKE         DEFINE    SDPSVR        WPSV3
750600940317     C     *LIKE         DEFINE    SDPRCP        WPVA1
750700940317     C     *LIKE         DEFINE    SDPRCP        WPVA2
750800940317     C     *LIKE         DEFINE    SDPRCP        WPVA3
750900970128     C     *LIKE         DEFINE    SDPKSC        WPPKSC
751000970128     C     *LIKE         DEFINE    SDPKSC        WPDKSC
751100990617     C* FILE SISDA
751200931112     C     *LIKE         DEFINE    SDANSP        WANSP
751300040317     C* FILE SISDE
751400040318     C     *LIKE         DEFINE    §bnbbcor      Wecor
751500040318     C     *LIKE         DEFINE    §bnbbcsf      Wecsf
751600040318     C     *LIKE         DEFINE    §bnbbNBA      WeNBA
751700040318     C     *LIKE         DEFINE    §bnbbNB2      WeNB2
751800980313     C* FILE ECFIM
751900980313     C     *LIKE         DEFINE    FIMSMC        WPSMC
752000980313     C     *LIKE         DEFINE    FIMSMC        WPSMCT
752100980317     C     *LIKE         DEFINE    FIMSMC        WSMC
752200980313     C     *LIKE         DEFINE    FIMSMC        WDSMC
752300980313     C* FILE ECFIC
752400011127     C*---       *LIKE     DEFN FICSCO    WPSCO
752500011127     C*---       *LIKE     DEFN FICSCO    WPSCOT
752600011127     C*---       *LIKE     DEFN FICSCO    WDSCO
752700011127     C*---       *LIKE     DEFN FICSCO    WSCO
752800030121     C* FILE SICOP
752900090914     C*    *LIKE         DEFINE    COPIMV        WCOIMV
753000030121     C     *LIKE         DEFINE    COPNSP        WCONSP
753100030121     C     *LIKE         DEFINE    COPNCL        WCONCL
753200030121     C     *LIKE         DEFINE    COPPKF        WCOPKF
753300030121     C     *LIKE         DEFINE    COPVLF        WCOVLF
753400030121     C     *LIKE         DEFINE    COPBAN        WCOBAN
753500031029     C     *LIKE         DEFINE    COPBAN        WDBAN
753600031029     C     *LIKE         DEFINE    COPBAN        WPBAN
753700940505     C* CAMPI VARI
753800030131     C     *LIKE         DEFINE    COPDAT        WDDATCOP
753900030131     C     *LIKE         DEFINE    COPDAT        WPDATCOP
754000090304     C     *LIKE         DEFINE    D$Pncl        WPncl
754100090304     C     *LIKE         DEFINE    D$PFTC        WPFTC
754200941123     C     *LIKE         DEFINE    D$PTC2        WPTC2
754300040322     C     *LIKE         DEFINE    D$Pflo        WPflo
754400040322     C     *LIKE         DEFINE    D$PFGC        WPFGC
754500941123     C     *LIKE         DEFINE    D$PFCA        WPFCA
754600040524     C     *LIKE         DEFINE    D$Psta        WPsta
754700990823     C     *LIKE         DEFINE    D$PIAS        WPIAS
754800970901     C     *LIKE         DEFINE    D$PPOR        WPPOR
754900970901     C     *LIKE         DEFINE    D$PLL1        WPLL1
755000970901     C     *LIKE         DEFINE    D$PTPO        WPTPO
755100970901     C     *LIKE         DEFINE    D$PFAP        WPFAP
755200970901     C     *LIKE         DEFINE    D$PFIN        WPFIN
755300970901     C     *LIKE         DEFINE    D$PDFT        WPDFT
755400981105     C     *LIKE         DEFINE    D$PKSC        WPKSC
755500000201     C     *LIKE         DEFINE    D$PDIV        WPDIV
755600020620     C     *LIKE         DEFINE    D$PTBL        WPTBL
755700020620     C     *LIKE         DEFINE    D$PCBO        WPCBO
755800941201     C     *LIKE         DEFINE    D$PFIN        WFIN
755900941201     C     *LIKE         DEFINE    D$PCTS        WCTS
756000941201     C     *LIKE         DEFINE    D$PKSC        WKSC
756100020709     C*--- *LIKE         DEFINE    ORGDIT        WDSCL
756200990824     C     *LIKE         DEFINE    D$PPKF        WPKF
756300990824     C     *LIKE         DEFINE    D$PPKF        WKGLIM
756400941201     C     *LIKE         DEFINE    D$PPOR        WPOR
756500000201     C     *LIKE         DEFINE    D$PDIV        WDIV
756600970729     C     *LIKE         DEFINE    D$PDFT        WDFT
756700941215     C     *LIKE         DEFINE    ECDRIC        $RIC
756800020205     C     *LIKE         DEFINE    MOVSVO        $CDC
756900941215     C     *LIKE         DEFINE    D$DLNP        $FIL
757000941215     C     *LIKE         DEFINE    ECTCMP        DEWCMP
757100941215     C     *LIKE         DEFINE    ECTDDC        WDSP
757200941215     C     *LIKE         DEFINE    ECTNBC        WNBC
757300030117     C     *LIKE         DEFINE    COPFIL        WPFIL
757400030117     C     *LIKE         DEFINE    COPFIL        WDFIL
757500941215     C     *LIKE         DEFINE    D$DLNP        W$DLNP
757600941215     C     *LIKE         DEFINE    D$DTFP        W$DTFP
757700941215     C     *LIKE         DEFINE    D$DKSC        W$DKSC
757800941215     C     *LIKE         DEFINE    D$DCCM        W$DCCM
757900941215     C     *LIKE         DEFINE    D$DMCT        W$DMCT
758000941215     C     *LIKE         DEFINE    D$PLNP        W$PLNP
758100941215     C     *LIKE         DEFINE    D$PTFP        W$PTFP
758200941215     C     *LIKE         DEFINE    D$PKSC        W$PKSC
758300941215     C     *LIKE         DEFINE    D$PCCM        W$PCCM
758400941215     C     *LIKE         DEFINE    D$PMCT        W$PMCT
758500000622     C     *LIKE         DEFINE    D$PTFP        LPTFP
758600000622     C     *LIKE         DEFINE    D$PTFA        LPTFA
758700000622     C     *LIKE         DEFINE    D$DTFP        LDTFP
758800000622     C     *LIKE         DEFINE    D$DTFA        LDTFA
758900000201     C     *LIKE         DEFINE    YECIMI        §IMI
759000000203     C     *LIKE         DEFINE    YECDVI        §DVI
759100000201     C     *LIKE         DEFINE    YECIMO        §IMO
759200011220     c     *LIKE         DEFINE    MOVVOC        LVOC
759300011220     c     *LIKE         DEFINE    MOVCDC        LCDC
759400020304     c     *LIKE         DEFINE    MOVDRG        LDRG
759500030120     C     *LIKE         DEFINE    COPDAT        $DAT
759600030120     C     *LIKE         DEFINE    COPDAT        WPDAT
759700030120     C     *LIKE         DEFINE    COPDAT        WDDAT
759800950123     C*
759900950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
760000950123     C* AD OGNI APERTURA DI DEL FILE
760100950123     C   99
760200950123     CANN99KEYERR        CHAIN     ECERR01L                           99
760300931105     C*
760400931105     C* IMPOSTO CAMPI DI GESTIONE PGM
760500941021     C                   MOVEL     'N'           $ERR              1
760600950117     C                   MOVEL     *BLANKS       $MSG             78
760700940411     C                   MOVEL     'N'           FLAGGS            1
760800941215     C                   MOVEL     'N'           WCETPR            1
760900030114     C                   MOVEL     'N'           WCEVPR            1
761000030123     C                   MOVEL     *BLANKS       $TDAT             1
761100950125     C*********************ADD  1         WBOLLE  90
761200991116     C*
761300991116     C* RECUPERA I CAMPI PER LA SIMULAZIONE DEL BUDGET
761400991116     C                   MOVEL     D$GXXX        DSX
761500991116     C                   MOVEL     DSXFSI        D$GFSI            1
761600991116     C                   MOVE      DSXASI        D$GASI            4 0
761700941215     C*
761800941215     C* IMPOSTO CAMPI DI GESTIONE PGM
761900941215     C                   Z-ADD     *ZEROS        ID                4 0
762000941215     C                   Z-ADD     *ZEROS        IP                4 0
762100941215     C                   Z-ADD     *ZEROS        CD                4 0
762200941215     C                   Z-ADD     *ZEROS        CP                4 0
762300940405     C*
762400940405     C* PORTA LE IMMAGINI PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
762500940405     C                   MOVEL     D$PRE1        D$PDS1
762600940405     C                   MOVEL     D$PRE2        D$PDS2
762700940405     C                   MOVEL     D$DRE1        D$DDS1
762800940405     C                   MOVEL     D$DRE2        D$DDS2
762900941201     C                   MOVEL     D$ORE1        D$ODS1
763000941201     C                   MOVEL     D$ORE2        D$ODS2
763100941123     C*
763200941130     C* INSCHIERA SIGLE E IMPORTI VARIE -PRIMA- E -DOPO- E -ORIG.-
763300941130     C                   MOVEA     D$PSVN        PSVN
763400941201     C                   MOVEA     D$PVAN        PVAA
763500941130     C     1             DO        20            I
763600941201     C                   MOVE      PVAA(I)       PVAN(I)
763700941130     C                   ENDDO
763800941130     C                   MOVEA     D$DSVN        DSVN
763900941201     C                   MOVEA     D$DVAN        DVAA
764000941130     C     1             DO        20            I
764100941201     C                   MOVE      DVAA(I)       DVAN(I)
764200941130     C                   ENDDO
764300941130     C                   MOVEA     D$OSVN        OSVN
764400941201     C                   MOVEA     D$OVAN        OVAA
764500941130     C     1             DO        20            I
764600941201     C                   MOVE      OVAA(I)       OVAN(I)
764700941130     C                   ENDDO
764800930414     C*
764900930414     C* RECUPERO IL CODICE DELL'AZIENDA
765000930414     C                   Z-ADD     1             CODUT             1 0
765100931104     C*
765200931111     C* CARICA LE TABELLE NECESSARIE AL PGM
765300931104     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
765400931104     C     CTABSN        IFNE      'S'
765500931104     C                   MOVEL     'S'           CTABSN            1
765600931111     C                   EXSR      CARTAB
765700941215     C                   EXSR      SUBDAT
765800941223     C                   Z-ADD     300           MAXBOL            6 0
765900941223     C                   Z-ADD     150           MAXIND            6 0
766000941215     C     DSDAT8        IFNE      WDATA8
766100941215     C                   Z-ADD     DSDAT8        WDATA8            8 0
766200941215     C                   MOVE      DSDAT6        DATGRE            6
766300941215     C                   MOVE      *BLANK        DATGIU            5
766400941215     C                   MOVE      '2'           FMTDAT            1
766500941215     C                   CALL      'XCVTDAT'
766600941215     C                   PARM                    DATGRE
766700941215     C                   PARM                    DATGIU
766800941215     C                   PARM                    FMTDAT
766900941215     C                   ENDIF
767000040406     c                   seton                                        of
767100931104     C                   END
767200930414     C*
767300930414     C                   ENDSR
767400930414     C********************************************************************
767500931111     C*   CARTAB - CARICA LE TABELLE
767600930414     C********************************************************************
767700930414     C     CARTAB        BEGSR
767800020704     C*
767900020704     C* APRE I FILES OCCORRENTI
768000020704     C                   OPEN      TABEL00F
768100020704     C                   OPEN      TNTBE01L
768200020704     C                   OPEN      AZORG01L
768300011218     C*
768400011218     C* REPERISCE LA DATA CORRENTE
768500011218     C                   TIME                    N14              14 0
768600011218     C                   MOVE      N14           N8                8 0
768700011218     C                   Z-ADD     N8            G08DAT
768800011218     C                   Z-ADD     *ZEROS        G08INV
768900011218     C                   MOVEL     '0'           G08ERR
769000011218     C                   CALL      'XSRDA8'
769100011218     C                   PARM                    WLBDA8
769200011218     C                   Z-ADD     G08INV        DATCOR            8 0
769300941025     C*
769400020704     C* AZZERA LE VARIABILI DI LAVORO
769500941223     C                   Z-ADD     *ZEROS        WNBAP             5 0
769600941223     C                   Z-ADD     *ZEROS        WNBAD             5 0
769700990218     C                   Z-ADD     *ZEROS        DEPA2L            4 0
769800020801     C*
769900020801     C* CARICA TABELLA -14- REGIONI E RAGGRUPPAMENTI REGIONI
770000020801     C     $ERR          IFEQ      'N'
770100020801     C                   Z-ADD     0             I
770200020801     C                   Z-ADD     CODUT         KTBUTE
770300020801     C                   MOVEL     '14'          KTBCOD
770400020801     C                   MOVEL     *BLANKS       KTBKEY
770500020801     C     KEYTAB        SETLL     TABEL00F                           99
770600020801     C  N99              READ(N)   TABEL00F                               99
770700020801     C     *IN99         IFEQ      '1'
770800020801     C                   MOVEL     MSG(17)       $MSG
770900020801     C                   EXSR      SCRERR
771000020801     C                   END
771100020801     C     *IN99         DOWEQ     '0'
771200020801     C     TBLCOD        ANDEQ     '14'
771300020801     C     TBLFLG        IFEQ      *BLANKS
771400020801     C                   ADD       1             I
771500020801     C                   MOVEL     TBLKEY        REG(I)
771600020801     C                   MOVEL     TBLUNI        DS14
771700020801     C                   MOVE      §14CRR        RAR(I)
771800020801     C                   MOVE      §14CRN        RAN(I)
771900020801     C                   END
772000020801     C                   READ(N)   TABEL00F                               99
772100020801     C                   END
772200020801     C                   END
772300020704     C*
772400020704     C* CARICA TABELLA -NTW- NETWORK
772500020705IF  1C     $ERR          IFEQ      'N'
772600020705     C                   Z-ADD     *ZEROS        I
772700020705     C                   MOVEL(P)  'NTW'         KTECOD
772800020705     C     KEYTB2        SETLL     TNTBE01L                           99
772900020705     C     KEYTB2        READE     TNTBE01L                               99
773000020705IF  2C     *IN99         IFEQ      *ON
773100020705     C                   MOVEL     MSG(16)       $MSG
773200020705     C                   EXSR      SCRERR
773300020705E   2C                   ENDIF
773400020705DO  2C     *IN99         DOWEQ     *OFF
773500020705     C                   MOVEL     TBEUNI        DNTW
773600020705     C                   ADD       1             I
773700020705     C                   MOVEL     TBEKE1        SNTW(I)
773800020705     C                   MOVEL     §NTWFIE       SNFIE(I)
773900020705     C     KEYTB2        READE     TNTBE01L                               99
774000020705E   2C                   ENDDO
774100020705E   1C                   ENDIF
774200020704     C*
774300020704     C* CARICA ORGANIGRAMMA
774400020705IF  1C     $ERR          IFEQ      'N'
774500020704     C                   Z-ADD     *ZEROS        KORFIL
774600020704     C     KEYORG        SETLL     AZORG01L                           99
774700020704     C  N99              READ      AZORG01L                               99
774800020704     C                   Z-ADD     *ZEROS        I
774900020705DO  2C     *IN99         DOWEQ     *OFF
775000020705IF  3C     ORGFVA        IFEQ      *BLANKS
775100020705IF  4C     ORGFAG        IFNE      'V'
775200020704     C                   ADD       1             I
775300020704     C                   Z-ADD     ORGFIL        FIL(I)
775400020704     C                   Z-ADD     ORGFIL        F                 4 0
775500020705     C                   MOVEL     ORGDE3        OG143
775600020705     C* DATI FILIALE
775700020705     C                   Z-ADD     1             II                6 0
775800020705     C     §OGNTW        LOOKUP    SNTW(II)                               98
775900020705IF  5C     *IN98         IFEQ      *OFF
776000020705     C                   MOVEL     MSG(23)       $MSG
776100020705     C                   EXSR      SCRERR
776200020705X   5C                   ELSE
776300020705     C                   MOVEL     SNTW(II)      NTW(F)
776400020705     C                   MOVEL     SNFIE(II)     FIE(F)
776500020805IF  6C     NTW(F)        IFEQ      'DPD'
776600020805     C                   MOVEL     'E'           FIE(F)
776700020805E   6C                   ENDIF
776800020705E   5C                   ENDIF
776900020805     C
777000020705     C*---               MOVEL     ORGFL1        FL1(F)
777100020705     C*---               MOVEL     ORGDIT        DIT(F)
777200020705     C*---               MOVEL     §OGDPD        DPD(F)
777300020705     C*---               MOVEL     §OGPT         POS(F)
777400020705     C* RAGGRUPPAMENTI
777500020704     C                   Z-ADD     1             II                6 0
777600020704     C     ORGCRE        LOOKUP    REG(II)                                98
777700020705IF  5C     *IN98         IFEQ      *OFF
777800020704     C                   MOVEL     MSG(8)        $MSG
777900020704     C                   EXSR      SCRERR
778000020705X   5C                   ELSE
778100020704     C                   Z-ADD     RAR(II)       RRE(I)
778200020704     C                   Z-ADD     RAN(II)       RRN(I)
778300020705E   5C                   ENDIF
778400020705E   4C                   ENDIF
778500020705E   3C                   ENDIF
778600020704     C                   READ      AZORG01L                               99
778700020705E   2C                   ENDDO
778800020705E   1C                   ENDIF
778900930414     C*
779000931104     C* CARICA TABELLA -CC- CONTROPARTITE PER FATTURAZIONE
779100941021     C     $ERR          IFEQ      'N'
779200930421     C                   Z-ADD     0             I
779300930414     C                   Z-ADD     CODUT         KTBUTE
779400931104     C                   MOVEL     'CC'          KTBCOD
779500930414     C                   MOVEL     *BLANKS       KTBKEY
779600930414     C     KEYTAB        SETLL     TABEL00F                           99
779700950203     C  N99              READ(N)   TABEL00F                               99
779800930414     C     *IN99         IFEQ      '1'
779900941223     C                   MOVEL     MSG(15)       $MSG
780000941026     C                   EXSR      SCRERR
780100930414     C                   END
780200931104     C     *IN99         DOWEQ     '0'
780300931104     C     TBLCOD        ANDEQ     'CC'
780400931104     C     TBLFLG        IFEQ      *BLANKS
780500930421     C                   ADD       1             I
780600931104     C                   MOVE      TBLKEY        CCC(I)
780700931104     C                   MOVEL     TBLUNI        DSCC
780800931111     C                   MOVEL     §CCFL1        CC1(I)
780900931111     C                   MOVEL     §CCFL2        CC2(I)
781000931111     C                   MOVEL     §CCFL3        CC3(I)
781100941130     C                   MOVEL     §CCFL4        CC4(I)
781200941130     C                   MOVEL     §CCFL5        CC5(I)
781300941130     C                   MOVEL     §CCFL6        CC6(I)
781400941130     C                   MOVEL     §CCCMP        CMP(I)
781500931104     C                   END
781600950203     C                   READ(N)   TABEL00F                               99
781700931104     C                   END
781800930414     C                   END
781900941123     C*
782000941123     C* CARICA TABELLA -1P- CONSEGNE PARTICOLARI
782100941212     C     $ERR          IFEQ      'N'
782200941123     C                   Z-ADD     0             I
782300941123     C                   Z-ADD     CODUT         KTBUTE
782400941123     C                   MOVEL     '1P'          KTBCOD
782500941123     C                   MOVEL     *BLANKS       KTBKEY
782600941123     C     KEYTAB        SETLL     TABEL00F                           99
782700950203     C  N99              READ(N)   TABEL00F                               99
782800941123     C     *IN99         IFEQ      '1'
782900941123     C                   MOVEL     MSG(14)       $MSG
783000941123     C                   EXSR      SCRERR
783100941123     C                   END
783200941123     C     *IN99         DOWEQ     '0'
783300941212     C     TBLCOD        ANDEQ     '1P'
783400941123     C     TBLFLG        IFEQ      *BLANKS
783500941123     C                   ADD       1             I
783600941123     C                   MOVEL     TBLKEY        C1P(I)
783700941123     C                   MOVEL     TBLUNI        DS1P
783800941123     C                   MOVE      §1PVAR        V1P(I)
783900941123     C                   END
784000950203     C                   READ(N)   TABEL00F                               99
784100941123     C                   END
784200941123     C                   END
784300941215     C*
784400020704     C* CARICA TABELLA -2T- RIFERIMENTI CENTRI DI COSTO
784500950404     C     $ERR          IFEQ      'N'
784600950404     C                   Z-ADD     CODUT         KTBUTE
784700941215     C                   MOVEL     '2T'          KTBCOD
784800941215     C     KEYTA2        SETLL     TABEL00F
784900941215     C     KEYTA2        READE(N)  TABEL00F                               99
785000941215IF  1C     *IN99         IFEQ      *ON
785100941215     C                   MOVEL     MSG(2)        $MSG
785200941215     C                   EXSR      SCRERR
785300941215X   1C                   ELSE
785400941215     C                   Z-ADD     *ZEROS        I
785500941215DO  2C     *IN99         DOWEQ     *OFF
785600941215IF  3C     TBLFLG        IFEQ      *BLANKS
785700941215     C                   ADD       1             I
785800941215     C                   MOVEL     TBLUNI        DS2T
785900941215     C                   MOVEL     TBLKEY        RIC(I)
786000941215E   3C                   ENDIF
786100941215     C     KEYTA2        READE(N)  TABEL00F                               99
786200941215E   2C                   ENDDO
786300941215E   1C                   ENDIF
786400950404     C                   ENDIF
786500970901     C*
786600020704     C* CARICA TABELLA -$2- SIGLE VARIE FORZATE A PGM
786700970901     C     $ERR          IFEQ      'N'
786800970901     C                   Z-ADD     CODUT         KTBUTE
786900970901     C                   MOVEL     '$2'          KTBCOD
787000970902     C                   MOVEL     *BLANKS       KTBKEY
787100970902     C                   MOVEL     '1'           KTBKEY
787200970901     C     KEYTAB        CHAIN(N)  TABEL00F                           99
787300970901IF  1C     *IN99         IFEQ      *ON
787400970901     C                   MOVEL     MSG(7)        $MSG
787500970901     C                   EXSR      SCRERR
787600970901X   1C                   ELSE
787700970901     C                   MOVEL     TBLUNI        DS$2
787800970901E   1C                   ENDIF
787900970901     C                   ENDIF
788000011218     c*
788100020704     C* CARICA TABELLA -GED- DIVISE PER CONVERSIONE
788200011218     C                   CLEAR                   DSGEDCE
788300011221     C                   MOVEL     *blanks       WDIVCE            3
788400011218     C                   MOVEL(P)  'GED'         KTECOD
788500011218     C                   MOVEL(P)  'CE'          KTEKE1
788600011218     C     KEYTBE        CHAIN     TNTBE01L
788700011218IF  1C                   IF        %found(TNTBE01L)
788800011218     C                   MOVEL     TBEUNI        DSGEDCE
788900011218E   1C                   ENDIF
789000011218IF  1C                   IF        §GEDCEDAT > DATCOR
789100011218     C                   EVAL      WDIVCE = §GEDCEDIP
789200011218X   1C                   ELSE
789300011218     C                   EVAL      WDIVCE = §GEDCEDIV
789400011221E   1C                   ENDIF
789500011221IF  1C                   IF        WDIVCE = *blanks
789600011221     C                   EVAL      WDIVCE = 'ITL'
789700011221E   1C                   ENDIF
789800020822     c*
789900020822     C* CARICA TABELLA -BLC- DATE CONTABILIZZAZIONE CONTEGGI PADRONCINI
790000020822     C                   CLEAR                   DBLC
790100020822     C                   MOVEL(P)  'BLC'         KTECOD
790200020822     C                   MOVEL(P)  '1'           KTEKE1
790300020822     C     KEYTBE        CHAIN     TNTBE01L
790400020822IF  1C                   IF        %found(TNTBE01L)
790500020822     C                   MOVEL     TBEUNI        DBLC
790600020822E   1C                   ENDIF
790700020704     C*
790800020704     C* CHIUDE I FILES OCCORRENTI
790900020704     C                   CLOSE     TABEL00F
791000020704     C                   CLOSE     TNTBE01L
791100020704     C                   CLOSE     AZORG01L
791200930414     C*
791300930414     C                   ENDSR
791400990218     C********************************************************************
791500990218     C*   CART2L - CARICA LA TABELLA 2L PER L'ANNO DELLA BOLLA
791600990218     C********************************************************************
791700990218     C     CART2L        BEGSR
791800990218     C*
791900990218     C* SE L'ANNO E' CAMBIATO RILEGGE LA TAB. 2L
792000990218IF  1C     WA2L          IFNE      DEPA2L
792100990218     C*
792200990218     C                   OPEN      TABEL00F
792300990218     C*
792400990218     C* CARICA TABELLA -2L- FASCIE DI PESO
792500990218     C     $ERR          IFEQ      'N'
792600990218     C                   Z-ADD     *ZEROS        II                6 0
792700990218     C                   Z-ADD     *ZEROS        IC                6 0
792800990218     C                   Z-ADD     *ZEROS        IT                6 0
792900990218     C                   Z-ADD     CODUT         KTBUTE
793000990218     C                   MOVEL     '2L'          KTBCOD
793100990218     C                   MOVEL     *BLANKS       KTBKEY
793200990218     C     KEYTAB        SETLL     TABEL00F                           99
793300990218     C  N99              READ(N)   TABEL00F                               99
793400990218     C     *IN99         IFEQ      '1'
793500990218     C                   MOVEL     MSG(13)       $MSG
793600990218     C                   EXSR      SCRERR
793700990218     C                   END
793800990218     C     *IN99         DOWEQ     '0'
793900990218     C     TBLCOD        ANDEQ     '2L'
794000990218     C     TBLFLG        IFEQ      *BLANKS
794100990218     C                   MOVEL     TBLKEY        WTM03             3
794200990218     C                   MOVEL     TBLKEY        WTM05             5
794300990218     C                   MOVEL     TBLUNI        DS2L
794400020516     C     WTM03         IFEQ      'S9R'
794500990218     C                   ADD       1             II
794600990218     C                   MOVE      WTM05         FPR(II)
794700990218     C                   Z-ADD     §2LKGI        KIR(II)
794800990218     C                   Z-ADD     §2LKGF        KFR(II)
794900990218     C                   ENDIF
795000020513     C*--- WA2L          IFLE      1998
795100020513     C*--- WTM03         ANDEQ     'SDC'
795200020513     C*--- WA2L          ORGE      1999
795300020513     C*--- WTM03         ANDEQ     'S9C'
795400020513     C     WA2L          IFLE      2001
795500020513     C     WTM03         ANDEQ     'S9C'
795600020513     C     WA2L          ORGE      2002
795700020513     C     WTM03         ANDEQ     'S2C'
795800990218     C                   ADD       1             IC
795900011214     C                   MOVE      WTM05         FPC(IC)
796000990218     C                   Z-ADD     §2LKGI        KIC(IC)
796100990218     C                   Z-ADD     §2LKGF        KFC(IC)
796200990218     C                   ENDIF
796300020513     c*
796400990218     C     WA2L          IFLE      1998
796500990218     C     WTM03         ANDEQ     'SDT'
796600990218     C     WA2L          ORGE      1999
796700990218     C     WTM03         ANDEQ     'S9T'
796800990218     C                   ADD       1             IT
796900990218     C                   MOVE      WTM05         FPT(IT)
797000990218     C                   Z-ADD     §2LKGI        KIT(IT)
797100990218     C                   Z-ADD     §2LKGF        KFT(IT)
797200990218     C                   ENDIF
797300990218     C                   ENDIF
797400990218     C                   READ(N)   TABEL00F                               99
797500990218     C                   ENDDO
797600990218     C                   ENDIF
797700990218     C*
797800990218     C                   CLOSE     TABEL00F
797900990218     C                   Z-ADD     WA2L          DEPA2L
798000990218     C*
798100990218E   1C                   ENDIF
798200990218     C*
798300990218     C                   ENDSR
798400941215     C*------------------------------------------------------------------------*
798500941215     C*? STAMPA - STAMPA RIGA
798600941215     C*------------------------------------------------------------------------*
798700950120     C*****      STAMPA    BEGSR
798800941215     C*
798900950120     C*****                OPEN PRTF198
799000941215     C*
799100941215     C* RECUPERO LA DATA E L'ORA
799200950120     C*****                Z-ADDUDATE     DATA    60
799300950120     C*****                TIME           ORA     60
799400941215     C*
799500941215     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
799600950120     C*****                EXCPTTESTA
799700941215     C*
799800941215     C* STAMPA RIGA DI DETTAGLIO TEMPI
799900950120     C*****                EXCPTRIGA
800000941215     C*
800100941215     C* STAMPA RIGA FINE
800200950120     C*****                EXCPTSTAFIN
800300941215     C*
800400950120     C*****                CLOSEPRTF198
800500941215     C*
800600950120     C*****                ENDSR
800700950120     O****
800800040127     C*------------------------------------------------------------------------*
800900040129     C*? reperisce data competenza a seconda se ciclo partenza o arrivo
801000040127     C*------------------------------------------------------------------------*
801100040129     C     SRcdp         BEGSR
801200040129     c* -PRIMA-
801300040129IF  1C                   IF        $tdat<>' ' and $tdat<>'R'
801400040617     c* date arrivo transiti
801500040129IF  1C                             and $tdat<>'X' and $tdat<>'W'
801600040617IF  1C                             and $tdat<>'H' and $tdat<>'L'
801700040617IF  1C                             and $tdat<>'N'
801800040129IF  1C                   IF        wpd='P'
801900040129     c                   MOVEL     wpdsp8        §dspAM            6 0
802000040129     c                   MOVEL     d$pdcm        §dcmAM            6 0
802100040129     c                   Z-ADD     *zeros        §cdpAM            6 0
802200040129     c                   EXSR      CALDRF
802300040129X   1C                   else
802400040129     c* -DOPO-
802500040129     c                   MOVEL     wddsp8        §dspAM            6 0
802600040129     c                   MOVEL     d$ddcm        §dcmAM            6 0
802700040129     c                   Z-ADD     *zeros        §cdpAM            6 0
802800040129     c                   EXSR      CALDRF
802900040129E   1C                   ENDIF
803000040129E   1C                   ENDIF
803100040129     c* stampo l'anomalia per tenerlo monitorizzato
803200040204IF  1C                   IF        (wpd='P' AND d$pdcm <> 0
803300040205IF  1C                             and d$ptbl <> 'A3' and d$ptbl <> 'F3'
803400040205IF  1C                             and d$ptbl <> 'AR'
803500040205IF  1C                             and d$pcca <> '5' and d$pcca <> '7'
803600040205IF  1C                             and d$plna <> 81 and d$plna <> 82
803700040205IF  1C                             and d$plna <> 104 and d$plna <> 113) or
803800040204IF  1C                             (wpd='D' AND d$Ddcm <> 0
803900040205IF  1C                             and d$dtbl <> 'A3' and d$dtbl <> 'F3'
804000040205IF  1C                             and d$dtbl <> 'AR'
804100040205IF  1C                             and d$dcca <> '5' and d$dcca <> '7'
804200040205IF  1C                             and d$dlna <> 81 and d$dlna <> 82
804300040205IF  1C                             and d$dlna <> 104 and d$dlna <> 113)
804400041110     c* non stampa più la segnalazione dell'anomalia
804500041110     c*                  exsr      srstampa
804600040130E   1C                   ENDIF
804700040129     c*
804800040129     C                   ENDSR
804900040129     C*------------------------------------------------------------------------*
805000040129     C*? STAMPA ERRORE REPERIMENTO DATA COMPETENZA
805100040129     C*------------------------------------------------------------------------*
805200040129     C     SRSTAMPA      BEGSR
805300040130     C* APRO FILE DI STAMPA
805400040205     C                   IF        *inof
805500040130     C                   EXCEPT    TESTATA
805600040205     c                   setoff                                       of
805700040130     C                   END
805800040130     C*
805900040127     C*
806000040127     C* STAMPA RIGA DI DETTAGLIO X IMMAGINE PRIMA
806100040127     C                   IF        WPD = 'P'
806200040127     C                   EXCEPT    RIGAP
806300040127     C                   END
806400040127     C*
806500040127     C* STAMPA RIGA DI DETTAGLIO X IMMAGINE DOPO
806600040127     C                   IF        WPD = 'D'
806700040127     C                   EXCEPT    RIGAD
806800040127     C                   END
806900040127     C                   ENDSR
807000941215     O*------------------------------------------------------------------------*
807100040127     OQSYSPRT   E            TESTATA          01
807200040127     O                                            8 'TRECD7R'
807300040127     O                                           74 'ERRORE REPERIMENTO DATA CO'
807400040127     O                                           82 'MPETENZA'
807500040127     O          E            RIGAP      01
807600040127     O                                           21 'IMMAGINE PRIMA BOLLA:'
807700040127     O                       D$PAAS              +1
807800040127     O                       D$PLNP              +1
807900040127     O                       D$PNRS              +1
808000040127     O                       D$PNSP              +1
808100040205     O                                           +1 'LNA'
808200040205     O                       d$plna              +1
808300040205     O                                           +1 'TBL'
808400040205     O                       d$ptbl              +1
808500040205     O                                           +1 'COMP.'
808600040205     O                       CP                  +1
808700040127     O                                           +1 'TIPO DATA:'
808800040127     O                       $TDAT               +1
808900040127     O          E            RIGAD      01
809000040127     O                                           20 'IMMAGINE DOPO BOLLA:'
809100040127     O                       D$DAAS              +1
809200040127     O                       D$DLNP              +1
809300040127     O                       D$DNRS              +1
809400040127     O                       D$DNSP              +1
809500040205     O                                           +1 'LNA'
809600040205     O                       d$dlna              +1
809700040205     O                                           +1 'TBL'
809800040205     O                       d$dtbl              +1
809900040127     O                                           +1 'COMP.'
810000040127     O                       CD                  +1
810100040127     O                                           +1 'TIPO DATA:'
810200040127     O                       $TDAT               +1
810300040127     O*PRTF198 E  102           TESTA
810400950120     O****                                8 'TRECD7R '
810500950120     O****                               82 'PRESTAZIONI PGM'
810600950120     O****                              150 'Data:'
810700950120     O****                     DATA     159 '  /  /  '
810800950120     O****                              167 'Ora:'
810900950120     O****                     ORA      178 '  .  .  '
811000950120     O****                              190 'Pagina:'
811100950120     O****                     PAGE  Z  198
811200950120     O*------------------------
811300950120     O****    E  1             RIGA
811400950120     O****                               30 'NUMERO DI BOLLE.....:'
811500950120     O****                     WBOLLE2   40
811600950120     O****    E  1             RIGA
811700950120     O****                               30 'NUM.TOTALE SCRITTURE:'
811800950120     O****                     STTOT 2   40
811900950120     O****    E  1             RIGA
812000950120     O****                               30 'NUM.AGG.IN MEMORIA..:'
812100950120     O****                     STAGG 2   40
812200950120     O****    E  1             RIGA
812300950120     O****                               30 'RAGG. MAX N. BOLLE..:'
812400950120     O****                     STMAXP2   40
812500950120     O****    E  1             RIGA
812600950120     O****                               30 'RAGG. MAX INDICE....:'
812700950120     O****                     STMAXI2   40
812800950120     O****    E  1             RIGA
812900950120     O****                               30 'TOT.INDICI TUTTI GIRI'
813000950120     O****                     STTOTX2   40
813100950120     O****    E  1             RIGA
813200950120     O****                               30 'N.GIRI TOTALE.......:'
813300950120     O****                     STTOTG2   40
813400950120     O****    E  1             RIGA
813500950120     O****                               30 'INDICE X MASSIMO RAG:'
813600950120     O****                     STULTX2   40
813700950120     O****    E  1             RIGA
813800950120     O****                               30 'N.MASSIMO BOLLE.....:'
813900950120     O****                     MAXBOL2   40
814000950120     O****    E  1             RIGA
814100950120     O****                               30 'N.MASSIMO INDICE....:'
814200950120     O****                     MAXIND2   40
814300950120     O*------------------------
814400950120     O****    E   60           STAFIN
814500950120     O****                               76 '*** FINE STAMPA ***'
814600950120     O*------------------------------------------------------------------------*
814700950316** ======== SCHIERA: MSG  (MESSAGGI)                       ================
814800980130TRECD7R- Tab. '2K' (dati vari per saldi) non trovata.                     NO2K 1
814900941223TRECD7R- Tab. '2T' (riferim. centri costo) non trovata.                   NO2T 2
815000941223TRECD7R- Riferimento centro di costo inesistente.                         NRIC 3
815100941223TRECD7R- Non trovati importi x comp.fisse dell'immagine bolla -PRIMA-     NCET 4
815200941223TRECD7R- Non trovati importi x comp.variabili dell'immagine bolla -PRIMA- NCED 5
815300950203TRECD7R- Tab. 'CV' (divise cambio in corso),divisa ECU non trovata.       CVNT 6
815400970901TRECD7R- Tab. '$2' (sigle varie forzate a pgm) non trovate.               $2NT 7
815500941223TRECD7R- Regione non trovata in tab. '14'. Avvertire CED.                 RENT 8
815600941223TRECD7R- Si è cercato di annullare un rec. di ECCED00F inesistente.       ACED 9
815700941223TRECD7R- Si è cercato di modificare un rec.di ECCED00F inesistente.       MCED10
815800030114TRECD7R- Si è cercato di annullare  un rec.di ECCET00F inesistente.       ACET11
815900030114TRECD7R- Si è cercato di modificare un rec.di ECCET00F inesistente.       MCET12
816000941223TRECD7R- Tabella '2L' non trovata. Avvertire CED.                         TB2L13
816100941223TRECD7R- Tabella '1P' non trovata. Avvertire CED.                         TB1P14
816200941223TRECD7R- Tabella 'CC' non trovata. Avvertire CED.                         TBCC15
816300020705TRECD7R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW16
816400941223TRECD7R- Tabella '14' non trovata. Avvertire CED.                         TB1417
816500941223TRECD7R- Filiale non trovata in organigramma.                             FNTO18
816600030114TRECD7R- Si è cercato di annullare un rec. in ECCETP0F.                   DETP19
816700030114TRECD7R- Si è cercato di aggiornare un rec.in ECCETP0F.                   UETP20
816800030114TRECD7R- Si è cercato di annullare un rec. in ECCEDP0F.                   DEDP21
816900030114TRECD7R- Si è cercato di aggiornare un rec.in ECCEDP0F.                   UEDP22
817000020705TRECD7R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT23
817100030114TRECD7R- Non trovati importi x voci coop dell'immagine bolla -PRIMA-      NCEV24
817200030114TRECD7R- Si è cercato di annullare  un rec.di ECCEV00F inesistente.       ACEV25
817300030114TRECD7R- Si è cercato di annullare un rec. in ECCEVP0F.                   DEVP26
817400030114TRECD7R- Si è cercato di modificare un rec.di ECCEV00F inesistente.       MCET27
817500030114TRECD7R- Si è cercato di aggiornare un rec.in ECCEVP0F.                   UETP28
817600030217TRECD7R- Voce non trovata.  XXX                                           VNTR29
817700031105TRECD7R- Si è tentato di scrivere un record duplicato nel file ECCET30C   DCET30
817800031105TRECD7R-                                                                      31
817900031105TRECD7R-                                                                      32
818000031105TRECD7R-                                                                      33
818100031105TRECD7R-                                                                      34
818200031105TRECD7R-                                                                      35
818300031105TRECD7R-                                                                      36
818400031105TRECD7R-                                                                      37
818500031105TRECD7R-                                                                      38
818600031105TRECD7R-                                                                      39
818700031105TRECD7R-                                                                      40
