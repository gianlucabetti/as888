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
005300160405     d wlogicmpp       s             19  5
005301160405     d wlogicmpd       s             19  5
005302160405     d wimp            s             19  5
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
015801160405     D lcp             S              1    DIM(999)
015900941215     D* SCHIERE RICAVI E COMPETENZE -DOPO-
016000090914     D IMD             S             13  5 DIM(999)
016100941215     D MED             S              1    DIM(999)
016200941223     D RCD             S              1    DIM(999)
016300980312     D RLD             S              2    DIM(999)
016301160405     D lcd             S              1    DIM(999)
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
019200160405     D flog            S              3  0 DIM(100)
019201160405     D FIL             S              3  0 DIM(500)
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
021500141211     D CCC             S              1    DIM(100)
021600141211     D CC1             S              1    DIM(100)
021700141211     D CC2             S              1    DIM(100)
021800141211     D CC3             S              1    DIM(100)
021900141211     D CC4             S              1    DIM(100)
022000141211     D CC5             S              1    DIM(100)
022100141211     D CC6             S              1    DIM(100)
022200141211     D CMP             S              3  0 DIM(100)
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
026202160404     D* DS ESTERNA x tipo record LOG di sisde
026203160404     D dsdeLOG       e ds
026204160404     D* DS ESTERNA x tipo record K30 di sisde
026205160404     D dsdek30       e ds
026206161107     D* DS ESTERNA x tipo record K31 di sisde
026207161107     D dsdek31       e ds
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
034600140516     D*                DS
034700140516     D* DSX                    1     33
034800140516     D* DSXASI                 1      4
034900140516     D* DSXFSI                 5      5
035000140516     D* DSXXXX                 6     33
035100140516     D Dsd$g         E DS
035200011218     D***************************************************************
035300011218     D* DS  CONTROLLO DATA (8)
035400011218     D WLBDA8          DS                  INZ
035500011218     D  G08DAT                 1      8  0
035600011218     D  G08INV                 9     16  0
035700011218     D  G08ERR                17     17
035800011218     D  G08TGI                18     22  0
035900941215     D***************************************************************
036000941215     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
036100941215     D                SDS
036200941215     D* NOME UTENTE
036300941215     D  USER                 254    263
036400941215     D* NUMERO LAVORO
036500941215     D  JOBNUM               264    269  0
036600030122     D***************************************************************
036700030122     D* DEFINIZIONE CAMPI
036800030122     D N8              S              8  0 INZ
036900030122     D N8bis           S              8  0 INZ
037000040317     D wgessde         S              3    INZ
037100040322     D contavv         S              1  0 INZ
037200950316     I*****************************************************************
037300950316     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCET30C
037400010828     IECCET000      01
037500010828     IECCETP00      02
037600950316     I*****************************************************************
037700950316     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCED30C
037800010828     IECCED000      03
037900010828     IECCEDP00      04
038000030114     I*****************************************************************
038100030114     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
038200030114     IECCEV000      05
038300030114     IECCEVP00      06
038400030506     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECC2V30C
038500030506     IECC2V000      07
038600030506     IECC2VP00      08
038700930304     C*****************************************************************
038800930323     C*/ INIZIO MAIN DEL PGM
038900931105     C*
039000931105     C* ESEGUE ROUTINE INIZIALE
039100931105     C                   EXSR      RUTINZ
039200940919     C*
039300940405     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE)
039400931104     C* CHIUDO I FILE E FINISCO PGM
039500991214     C     D$GSTS        IFEQ      'C'
039600991214     C     D$GSTS        OREQ      'S'
039700991214     C     $ERR          ORNE      'N'
039800991214     C                   ELSE
039900991214     C*
040000991214     C***********D$GSTS    IFNE 'C'                        --- 1 -->
040100991214     C***********$ERR      ANDEQ'N'
040200940808     C*
040300940919     C                   ADD       1             LEECD7            9 0
040400940808     C                   EXSR      AZZW
040500940808     C*
040600941130     C* CERCA I RICAVI PRIMA E DOPO
040700941021     C     $ERR          IFEQ      'N'
040800941130     C                   EXSR      GESRIC
040900940808     C                   ENDIF
041000940808     C*
041100940808     C* ESEGUE CALCOLI SU IMMAGINE PRIMA SOLO SE VARIAZIONE O CANCELLAZIONE
041200941021     C     $ERR          IFEQ      'N'
041300940808     C     D$GSTS        IFEQ      'U'
041400940808     C     D$GSTS        OREQ      'D'
041500940808     C                   EXSR      GESDSP
041600940808     C                   ENDIF
041700940808     C                   ENDIF
041800940808     C*
041900940808     C* ESEGUE CALCOLI SU IMMAGINE DOPO SOLO SE VARIAZIONE O INSERIMENTO
042000941021     C     $ERR          IFEQ      'N'
042100940808     C     D$GSTS        IFEQ      'U'
042200940808     C     D$GSTS        OREQ      'I'
042300940808     C                   EXSR      GESDSD
042400940808     C                   ENDIF
042500940808     C                   ENDIF
042600930304     C*
042700940808     C* <<<<<<<<<< FINE PREPARAZIONE - INIZIO AGGIORNAMENTO >>>>>>>>>>>>>>>
042800011210     C*
042900011210     C* AGGIORNA ECMOV
043000011210IF  2C     $ERR          IFEQ      'N'
043100011210     C     D$GWFM        ANDEQ     'S'
043200011210     C                   EXSR      AGGMOV
043300011210E   2C                   ENDIF
043400941215     C*
043500941215     C* AGGIORNA ECCED (DETTAGLIO COMPETENZE)
043600941215IF  2C     $ERR          IFEQ      'N'
043700950104     C     D$GCET        ANDEQ     'S'
043800941215     C                   EXSR      AGGCED
043900941215E   2C                   ENDIF
044000941215     C*
044100941215     C* AGGIORNA ECCET (TESTATA COMPETENZE)
044200941215IF  2C     $ERR          IFEQ      'N'
044300941223     C     D$GCET        ANDEQ     'S'
044400941215     C                   EXSR      AGGCET
044500941215E   2C                   ENDIF
044600980317     C*
044700980317     C* AGGIORNA ECFIM (RIFATT. ITL)
044800980317     C     $ERR          IFEQ      'N'
044900020531     C     D$GFIM        ANDEQ     'S'
045000980317     C                   EXSR      AGGFIM
045100980317     C                   ENDIF
045200020531     C*
045300020531     C* AGGIORNA FIFCE (CONTEGGI PADRONCINI)
045400020531IF  2C     $ERR          IFEQ      'N'
045500020531     C     D$GFCE        ANDEQ     'S'
045600020531IF  3C     D$GSTS        IFEQ      'I'
045700020531     C     D$GSTS        OREQ      'U'
045800020531     C                   EXSR      AGGFCE
045900020531E   3C                   ENDIF
046000020531E   2C                   ENDIF
046100030122     C*
046200030122     C* AGGIORNA I PROSSIMI 2 FILES A PARTIRE DAL 2003
046300030131IF  2C                   IF        WPDATCOP > 20030000 or
046400030131     C                             WDDATCOP > 20030000
046500030114     C*
046600030509     C* AGGIORNA ECCEV (VOCI COOP) e ECC2V
046700030122IF  3C     $ERR          IFEQ      'N'
046800030114     C     D$GCOP        ANDEQ     'S'
046900030114     C                   EXSR      AGGCEV
047000030122E   3C                   ENDIF
047100030114     C*
047200030114     C* AGGIORNA SICOP (SALDI COOP)
047300030122IF  3C     $ERR          IFEQ      'N'
047400140516     c* forse (16/5/2014 ba e lb) si vuole evitare
047500140516     c* di aggiornare il sicop in caso di RESO perchè...:
047600140516     c* per evitare di spostare le competenze da una data ad
047700140516     c* un'altra, in quanto, in caso di reso, alcune date posso venir
047800140516     c* variate, conseguenza di variazioni fatte sulla figlia
047900030114     C     D$GCOP        ANDEQ     'S'
048000031113IF  4C     D$PCCA        IFEQ      '2'
048100140516     c     D$GFSI        andne     'S'
048200031113     C     D$DCCA        OREQ      '2'
048300140516     c     D$GFSI        andne     'S'
048400031113X   4c                   ELSE
048500030122     C                   EXSR      GESCOP
048600031113E   4C                   ENDIF
048700030122E   3C                   ENDIF
048800030122E   2C                   ENDIF
048900980313     C*
049000980313     C* AGGIORNA ECFIC (COMPET. RIFATT. BAR/SDI)
049100011127     C*---       $ERR      IFEQ 'N'
049200011127     C*---       D$GCET    ANDEQ'S'
049300011127     C*---                 EXSR AGGFIC
049400011127     C*---                 ENDIF
049500940808     C*
049600931104     C* SCRIVE SUI SALDI BOLLE
049700941021     C     $ERR          IFEQ      'N'
049800931119     C     D$GSDR        ANDEQ     'S'
049900931104     C                   EXSR      GESSDR
050000930420     C                   END
050100931104     C*
050200931104     C* SCRIVE SUI SALDI FILIALE
050300941021     C     $ERR          IFEQ      'N'
050400931119     C     D$GSDF        ANDEQ     'S'
050500931104     C                   EXSR      GESSDF
050600931104     C                   END
050700931110     C*
050800931110     C* SCRIVE SUI SALDI TRAZIONI
050900941021     C     $ERR          IFEQ      'N'
051000931119     C     D$GSDT        ANDEQ     'S'
051100931110     C                   EXSR      GESSDT
051200931110     C                   END
051300941117     C*
051400941117     C* SE ESISTE LA BOLLA MAMMA
051500941117     C* SOSTITUISCO IL CLIENTE DELLA BOLLA MAMMA AL CLIENTE USATO PER LA
051600990617     C* SCRITTURA SUI FILE SISDC E SISDO (PRIMA E DOPO)
051700950203     C     D$GSTS        IFEQ      'U'
051800950203     C     D$GSTS        OREQ      'D'
051900941117     C                   Z-ADD     D$PKSC        WCPKSC
052000950203     C                   ENDIF
052100950203     C*
052200950203     C     D$GSTS        IFEQ      'U'
052300950203     C     D$GSTS        OREQ      'I'
052400941117     C                   Z-ADD     D$DKSC        WCDKSC
052500950203     C                   ENDIF
052600950203     C*
052700941117     C     D$OKSC        IFNE      *ZEROS
052800950203     C*
052900950203     C     D$GSTS        IFEQ      'U'
053000950203     C     D$GSTS        OREQ      'D'
053100941118     C     D$PKSC        IFNE      *ZEROS
053200941130     C     D$PDBO        ANDEQ     'S'
053300950905     C     D$PFIG        ANDEQ     'S'
053400941117     C                   Z-ADD     D$OKSC        WCPKSC
053500941117     C                   ENDIF
053600950203     C                   ENDIF
053700950203     C*
053800950203     C     D$GSTS        IFEQ      'U'
053900950203     C     D$GSTS        OREQ      'I'
054000941117     C     D$DKSC        IFNE      *ZEROS
054100941130     C     D$DDBO        ANDEQ     'S'
054200950905     C     D$DFIG        ANDEQ     'S'
054300941117     C                   Z-ADD     D$OKSC        WCDKSC
054400941117     C                   ENDIF
054500950203     C                   ENDIF
054600950203     C*
054700941117     C                   ENDIF
054800931111     C*
054900931111     C* SCRIVE SUI SALDI CLIENTE - REGIONE/REGIONE
055000941021     C     $ERR          IFEQ      'N'
055100931119     C     D$GSDC        ANDEQ     'S'
055200931111     C                   EXSR      GESSDC
055300931111     C                   END
055400931111     C*
055500931111     C* SCRIVE SUI SALDI CLIENTE - CONTABILI
055600941021     C     $ERR          IFEQ      'N'
055700931119     C     D$GSDO        ANDEQ     'S'
055800931111     C                   EXSR      GESSDO
055900931111     C                   END
056000931111     C*
056100931111     C* SCRIVE SUI SALDI CLIENTE - AFFIDABILITA'
056200941021     C     $ERR          IFEQ      'N'
056300931119     C     D$GSDA        ANDEQ     'S'
056400931111     C                   EXSR      GESSDA
056500931111     C                   END
056600931111     C*
056700931111     C* SCRIVE SUI SALDI CLIENTE - CONSEGNE PARTICOLARI
056800941021     C     $ERR          IFEQ      'N'
056900940103     C     D$GSDP        ANDEQ     'S'
057000940103     C                   EXSR      GESSDP
057100940103     C                   END
057200040316     C*
057300040316     C* SCRIVE ESTENSIONI SALDI
057400040323     C* AGGIORNA L'ESTENSIONE SALDI A PARTIRE DA APRILE 2004
057500040316     C     $ERR          IFEQ      'N'
057600040316     C     D$GSDE        ANDEQ     'S'
057700040316     C                   EXSR      GESSDE
057800040316     C                   END
057900950124     C*
058000950124     C* PORTA COD. E IMP. COMPETENZE/RICAVI IN OUTPUT SOLO PER PGM A VIDEO
058100950124     C     $ERR          IFEQ      'N'
058200950124     C     D$GPGF        IFEQ      'TREC78R'
058300950124     C     D$GPGF        OREQ      'TREC79R'
058400950124     C                   EXSR      GESCCD
058500950124     C                   ENDIF
058600950124     C                   ENDIF
058700931104     C*
058800940217     C                   END
058900930323     C*
059000991214     C* SE PGM LANCIATO CON 'C' (CLOSE) O 'S' (SHUTDOWN) CHIUDE CON LR
059100991214     C* (CHIUDE FILE) ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
059200991214IF  1C     D$GSTS        IFEQ      'C'
059300991214     C     D$GSTS        OREQ      'S'
059400011218     C*
059500011210     C* CICLO ULTIME SCRITTURE ECMOV -MOVIMENTI NUOVI-
059600011219DO  2C     1             DO        WULTY         IY
059700011219IF  3C     IMPY(IY)      IFNE      *ZEROS
059800090916     C                   Z-ADD     IMPY(IY)      WIMP
059900011210     C                   EXSR      AGGECMOV
060000011219E   3C                   ENDIF
060100011219E   2C                   ENDDO
060200011219     C*
060300950120     C*********************EXSR STAMPA                     *STAMPA STAT.LANCIO
060400941130     C                   CALL      'TRECD8R'                            98
060500940902     C                   PARM                    DS$ECO
060600930323     C                   SETON                                        LR
060700991214X   1C                   ELSE
060800930324     C                   SETON                                        RT
060900991214E   1C                   END
061000940916     C*/ FINE MAIN DEL PGM
061100940808     C********************************************************************
061200940808     C*   GESRIC - CALCOLA IL RICAVO BOLLA -PRIMA- E -DOPO-
061300940808     C********************************************************************
061400940808     C     GESRIC        BEGSR
061500940930     C*
061600940930     C* AZZERO SCHIERE RICAVI PRIMA E DOPO
061700940930     C                   CLEAR                   CRP
061800940930     C                   CLEAR                   IRP
061900940930     C                   CLEAR                   CRD
062000940930     C                   CLEAR                   IRD
062100940930     C* CONTROLLO FLAG RICAVI S/N (DAL TIPO BOLLA)
062200941005     C     D$GSTS        IFEQ      'U'
062300941005     C     D$GSTS        OREQ      'D'
062400941130     C     D$PFRI        IFEQ      'S'
062500950905     C     D$PFIG        OREQ      'S'
062600941130     C                   EXSR      GESRIP
062700940930     C                   ENDIF
062800941005     C                   ENDIF
062900941005     C     D$GSTS        IFEQ      'U'
063000941005     C     D$GSTS        OREQ      'I'
063100941130     C     D$DFRI        IFEQ      'S'
063200950905     C     D$DFIG        OREQ      'S'
063300941130     C                   EXSR      GESRID
063400940930     C                   ENDIF
063500941005     C                   ENDIF
063600940930     C*
063700941130     C* CONTROLLO SE ESISTE IMPONIBILE REALE
063800941005     C*
063900940930     C                   MOVEL     'N'           WPFIR             1
064000940930     C                   MOVEL     'N'           WDFIR             1
064100940930     C* SE E' FIGLIA E L'IMPONIBOLE REALE = 0 CERCO IMP.REALE SULLA MAMMA
064200941005     C     D$GSTS        IFEQ      'U'
064300941005     C     D$GSTS        OREQ      'D'
064400940930     C     WPIR          IFEQ      *ZEROS
064500950905     C     D$PFIG        ANDEQ     'S'
064600940930     C                   MOVEL     'S'           WPFIR
064700940930     C                   ENDIF
064800941005     C                   ENDIF
064900941005     C     D$GSTS        IFEQ      'U'
065000941005     C     D$GSTS        OREQ      'I'
065100940930     C     WDIR          IFEQ      *ZEROS
065200950905     C     D$DFIG        ANDEQ     'S'
065300940930     C                   MOVEL     'S'           WDFIR
065400940930     C                   ENDIF
065500941005     C                   ENDIF
065600940930     C*
065700941130     C* SE IMPONIBILE REALE PRIMA O DOPO NON E' PRESENTE, UTILIZZO QUELLA
065800941130     C* DELLA MAMMA
065900940930     C     WPFIR         IFEQ      'S'
066000940930     C     WDFIR         OREQ      'S'
066100940930     C*
066200941130     C* CALCOLO RICAVI DELLA BOLLA ORIGINALE
066300941130     C                   EXSR      GESRIO
066400941130     C*
066500941130     C* IMPOSTO NELLA SCHIERA RICAVI L'IMP.REALE DELLA BOLLA ORIG.
066600941130     C     WPFIR         IFEQ      'S'
066700941130     C                   Z-ADD     WPIIR         I
066800941130     C                   Z-ADD     WOIR          IRP(I)
066900941130     C                   Z-ADD     WOIR          WPIR
067000941130     C                   ENDIF
067100941130     C     WDFIR         IFEQ      'S'
067200941130     C                   Z-ADD     WDIIR         I
067300941130     C                   Z-ADD     WOIR          IRD(I)
067400941130     C                   Z-ADD     WOIR          WDIR
067500941130     C                   ENDIF
067600941130     C*
067700941130     C                   ENDIF
067800941223     C*
067900941223     C* IMPOSTA I RICAVI NELLE SCHIERE DI 999 E NELLA DS UNIFICATA
068000030505     C     1             DO        75            I
068100941223     C* IMPOSTA I RICAVI -PRIMA- NELLA SCHIERA DI 999
068200941223     C     CRP(I)        IFNE      *ZEROS
068300941223     C                   Z-ADD     CRP(I)        CP
068400050322     c* è stato creato il pgm TNVRRICR per sistemare gli importi dei ricavi
068500050322     c* nel file ECMOV. Tale pgm imposta il tasll2 = 'A' nelle sped.
068600050322     c* di gennaio e febbraio 2005 x cui:
068700050322     c* se la modifica del titas avviene da questo pgm deve ragionare in
068800050322     c* modo sbagliato PRIMA e in modo corretto DOPO
068900050322     c* se la modifica non avviene da questo pgm ci sono tre casi:
069000050322     c*1)il record di titas aggiornato non ha ancora subito la correzione
069100050322     c* quindi deve continuare a ragionare in modo sbagliato (xchè passerà
069200050322     c* il tnvrricr a sistemare)
069300050322     c*2)il record di titas aggiornato ha già subito la correzione
069400050322     c* quindi deve ragionare in modo corretto
069500050322     c*3)il record di titas aggiornato ha data spedizione maggiore di
069600050322     c*  febbraio 2005 quindi deve ragionare in modo corretto
069700050321     C                   IF        D$GPGF = 'TNVRRICR'
069800941223     C                   Z-ADD     IRP(I)        IMP(CP)
069900050321     C                   Else
070000050322     c                   if        wdsp > 20050300  or
070100050322     c                             (wdsp > 20050100 and
070200050322     c                             wdsp < 20050300 and d$pll2 ='A')
070300050321     C                   ADD       IRP(I)        IMP(CP)
070400050321     c                   else
070500050321     C                   Z-ADD     IRP(I)        IMP(CP)
070600050321     c                   END
070700050321     c                   END
070800050321     C                   ENDIF
070900941223     C* IMPOSTA I RICAVI -DOPO- NELLA SCHIERA DI 999
071000941223     C     CRD(I)        IFNE      *ZEROS
071100941223     C                   Z-ADD     CRD(I)        CD
071200050321     C                   IF        D$GPGF = 'TNVRRICR'
071300050321     C                   add       IRd(I)        IMd(Cd)
071400050321     C                   Else
071500050322     c                   if        wdsp > 20050300 or
071600050322     c                             (wdsp > 20050100 and
071700050322     c                             wdsp < 20050300 and d$dll2 ='A')
071800050321     C                   ADD       IRd(I)        IMd(Cd)
071900050321     c                   else
072000050321     C                   Z-ADD     IRd(I)        IMd(Cd)
072100050321     c                   END
072200050321     c                   END
072300941223     C                   ENDIF
072400941223     C* IMPOSTA I RICAVI -DOPO- NELLA DS UNIFICATA
072500941201     C                   Z-ADD     CRD(I)        DSWCN
072600941201     C                   MOVE      DSWCA         CAL(I)
072700941201     C                   Z-ADD     IRD(I)        DSWIN
072800941201     C                   MOVE      DSWIA         IAL(I)
072900941201     C                   ENDDO
073000950125     C                   MOVEA     CAL           D$DCCD
073100950125     C                   MOVEA     IAL           D$DICD
073200940930     C*
073300940930     C                   ENDSR
073400941130     C********************************************************************
073500941130     C*  GESRIP - GESTIONE RICAVO BOLLA -PRIMA-
073600941130     C********************************************************************
073700941130     C     GESRIP        BEGSR
073800941130     C*
073900941130     C* PORTA I CAMPI -PRIMA- NEI CAMPI DI LAVORO
074000941130     C                   MOVEL     D$PFIN        WFIN
074100941130     C                   MOVEL     D$PCTS        WCTS
074200941130     C                   Z-ADD     D$PKSC        WKSC
074300990824     C                   Z-ADD     D$PPKF        WPKF
074400941130     C                   Z-ADD     D$PPOR        WPOR
074500000201     C                   MOVEL     D$PDIV        WDIV
074600941130     C                   MOVEA     PSVN          SVN
074700941130     C                   MOVEA     PVAN          VAN
074800970729     C                   Z-ADD     D$PDFT        WDFT
074900990217     C                   Z-ADD     D$PMGS        DSMGS
075000990217     C                   Z-ADD     D$PAAS        DSAAS
075100990217     C                   Z-ADD     DSDAT8        WDSP
075200941130     C* CALCOLA RICAVI
075300941130     C                   EXSR      CALRIC
075400941130     C* PORTA I RICAVI NELLE SCHIERE PRIMA
075500941212     C     D$PFRI        IFEQ      'S'
075600941130     C                   MOVEA     CR            CRP
075700941130     C                   MOVEA     IR            IRP
075800941130     C                   Z-ADD     WIRB          WPIRB
075900941212     C                   Z-ADD     WIRDN         WPIRD
076000941130     C                   Z-ADD     WIRBC         WPIRBC
076100941130     C                   ENDIF
076200941130     C*
076300941130     C                   Z-ADD     WIMR          WPIR
076400941201     C                   Z-ADD     WIIR          WPIIR             6 0
076500941130     C*
076600941130     C                   ENDSR
076700941130     C********************************************************************
076800941130     C*  GESRID - GESTIONE RICAVO BOLLA -DOPO-
076900941130     C********************************************************************
077000941130     C     GESRID        BEGSR
077100941130     C*
077200941130     C* PORTA I CAMPI -DOPO- NEI CAMPI DI LAVORO
077300941130     C                   MOVEL     D$DFIN        WFIN
077400941130     C                   MOVEL     D$DCTS        WCTS
077500941130     C                   Z-ADD     D$DKSC        WKSC
077600980317     C******************** MOVELD$DSCL    WSCL
077700990824     C                   Z-ADD     D$DPKF        WPKF
077800990823     C******************** Z-ADDD$DINL    WINL
077900941130     C                   Z-ADD     D$DPOR        WPOR
078000000201     C                   MOVEL     D$DDIV        WDIV
078100990823     C******************** Z-ADDD$DDIF    WDIF
078200990823     C******************** Z-ADDD$DBOL    WBOL
078300990823     C******************** Z-ADDD$DANT    WANT
078400941130     C                   MOVEA     DSVN          SVN
078500941130     C                   MOVEA     DVAN          VAN
078600970729     C                   Z-ADD     D$DDFT        WDFT
078700990217     C                   Z-ADD     D$DMGS        DSMGS
078800990217     C                   Z-ADD     D$DAAS        DSAAS
078900990217     C                   Z-ADD     DSDAT8        WDSP
079000970724     C********             MOVELD$DTPO    WTPO
079100970724     C********             Z-ADDD$DLNP    WLNP
079200970724     C********             Z-ADDD$DLNA    WLNA
079300941130     C* CALCOLA RICAVI
079400941130     C                   EXSR      CALRIC
079500970724     C* PORTA I RICAVI NELLE SCHIERE DOPO
079600941130     C     D$DFRI        IFEQ      'S'
079700941130     C                   MOVEA     CR            CRD
079800941130     C                   MOVEA     IR            IRD
079900941130     C* MEMORIZZA IMPORTI PER SALDI
080000941130     C                   Z-ADD     WIRB          WDIRB
080100941212     C                   Z-ADD     WIRDN         WDIRD
080200941130     C                   Z-ADD     WIRBC         WDIRBC
080300941130     C                   ENDIF
080400941130     C*
080500941130     C                   Z-ADD     WIMR          WDIR
080600941201     C                   Z-ADD     WIIR          WDIIR             6 0
080700941130     C*
080800941130     C                   ENDSR
080900941130     C********************************************************************
081000941130     C*  GESRIO - GESTIONE RICAVO BOLLA ORIGINALE
081100941130     C********************************************************************
081200941130     C     GESRIO        BEGSR
081300941130     C*
081400941130     C* PORTA I CAMPI BOLLA ORIGNALE NEI CAMPI DI LAVORO
081500941130     C                   MOVEL     D$OFIN        WFIN
081600941130     C                   MOVEL     D$OCTS        WCTS
081700941130     C                   Z-ADD     D$OKSC        WKSC
081800980317     C******************** MOVELD$OSCL    WSCL
081900990824     C                   Z-ADD     D$OPKF        WPKF
082000990823     C******************** Z-ADDD$OINL    WINL
082100941130     C                   Z-ADD     D$OPOR        WPOR
082200000201     C                   MOVEL     D$ODIV        WDIV
082300990823     C******************** Z-ADDD$ODIF    WDIF
082400990823     C******************** Z-ADDD$OBOL    WBOL
082500990823     C******************** Z-ADDD$OANT    WANT
082600941130     C                   MOVEA     OSVN          SVN
082700941130     C                   MOVEA     OVAN          VAN
082800970729     C                   Z-ADD     D$ODFT        WDFT
082900990217     C                   Z-ADD     D$OMGS        DSMGS
083000990217     C                   Z-ADD     D$OAAS        DSAAS
083100990217     C                   Z-ADD     DSDAT8        WDSP
083200970724     C*********            MOVELD$OTBL    WTPO
083300970724     C*********            Z-ADDD$OLNP    WLNP
083400970724     C*********            Z-ADDD$OLNA    WLNA
083500941130     C* CALCOLA RICAVI
083600941130     C                   EXSR      CALRIC
083700011214     C                   Z-ADD     WIMR          WOIR             19 4
083800941130     C*
083900941130     C                   ENDSR
084000941130     C********************************************************************
084100941130     C*  CALRIC - CALCOLA IL RICAVO BOLLA
084200941130     C********************************************************************
084300941130     C     CALRIC        BEGSR
084400020705     C*
084500020705     C* Se divisa <> divisa C/E, applica il cambio della divisa C/E (YEURCO).
084600020104IF  2C     WDIV          IFNE      WDIVCE
084700020104     c* PORTO
084800020104IF  3C     WPOR          IFNE      *ZEROS
084900011218     C                   Z-ADD     WPOR          §IMI
085000011218     C                   MOVEL     WDIV          §DVI
085100011218     C                   EXSR      CNVEUR
085200011218     C                   Z-ADD     §IMO          WPOR
085300020104E   3C                   ENDIF
085400020104     c* VARIE
085500020104DO  3C     1             DO        20            I
085600020104IF  4C     SVN(I)        IFNE      *BLANKS
085700020104IF  5C     VAN(I)        IFNE      *ZEROS
085800011218     C                   Z-ADD     VAN(I)        §IMI
085900011218     C                   MOVEL     WDIV          §DVI
086000011218     C                   EXSR      CNVEUR
086100020104     C                   Z-ADD     §IMO          VAN(I)
086200020104E   5C                   ENDIF
086300020104E   4C                   ENDIF
086400020104E   3C                   ENDDO
086500020104E   2C                   ENDIF
086600941130     C*
086700950207     C* AZZERA CAMPI DI LAVORO
086800941130     C                   Z-ADD     *ZEROS        V                 6 0
086900941212     C                   Z-ADD     *ZEROS        WIRB
087000941212     C                   Z-ADD     *ZEROS        WIRD
087100941212     C                   Z-ADD     *ZEROS        WIRBC
087200941212     C                   Z-ADD     *ZEROS        WIRDN
087300941212     C                   Z-ADD     *ZEROS        WIRBE
087400941212     C                   Z-ADD     *ZEROS        WIMR
087500941130     C                   CLEAR                   CR
087600941130     C                   CLEAR                   IR
087700941130     C                   CLEAR                   VR
087800941130     C*
087900941130     C* CARICA SCHIERA VARIE
088000941130     C     1             DO        20            I
088100941130     C     SVN(I)        IFNE      *BLANKS
088200941130     C                   ADD       1             V
088300941130     C                   MOVEL     SVN(I)        VR(V)
088400941130     C                   Z-ADD     VAN(I)        IR(V)
088500941130     C                   ENDIF
088600941130     C                   ENDDO
088700941130     C* PORTO
088800941130IF  1C     WPOR          IFNE      *ZEROS
088900941130     C                   ADD       1             V
089000970901     C                   MOVEL     §$2FPO        VR(V)
089100941130     C                   Z-ADD     WPOR          IR(V)
089200941130E   1C                   ENDIF
089300941130     C*
089400941130     C* CIASCUN IMPORTO DI LAVORO VALORIZZATO LO CONTROLLO CON LA TABELLA "CC"
089500941130     C* PER REPERIRE IL CODICE COMPETENZA E PER AGGREGARE DEGLI IMPORTI
089600941209DO  1C     1             DO        V             II                6 0
089700941130     C*
089800941130     C                   Z-ADD     1             I
089900941130     C                   SETOFF                                           99
090000941209     C     VR(II)        LOOKUP    CCC(I)                                 99
090100941130IF  2C     *IN99         IFEQ      *ON
090200941212     C                   Z-ADD     CMP(I)        CR(II)
090300941130IF  3C     CC1(I)        IFEQ      'S'
090400941209     C                   ADD       IR(II)        WIRB
090500941130E   3C                   ENDIF
090600941130IF  3C     CC2(I)        IFEQ      'S'
090700941209     C                   ADD       IR(II)        WIRD
090800941130E   3C                   ENDIF
090900941130IF  3C     CC3(I)        IFEQ      'S'
091000941209     C                   ADD       IR(II)        WIRBC
091100941130E   3C                   ENDIF
091200941130IF  3C     CC4(I)        IFEQ      'S'
091300941209     C                   ADD       IR(II)        WIRDN
091400941130E   3C                   ENDIF
091500941130IF  3C     CC5(I)        IFEQ      'S'
091600941209     C                   ADD       IR(II)        WIRBE
091700941130E   3C                   ENDIF
091800941130IF  3C     CC6(I)        IFEQ      'S'
091900941209     C                   ADD       IR(II)        WIMR
092000941130E   3C                   ENDIF
092100941130E   2C                   ENDIF
092200941130E   1C                   ENDDO
092300941130     C*
092400990217     C* AZZERAMENTO RICAVO DELTA BOLLE > 3.500 KG
092500040429IF  1C*    WDSP          IFGT      19990100
092600040429IF  2C*    WDSP          IFLT      20000100
092700040429IF  3C*    WPKF          IFGT      5000
092800040429     C*                  Z-ADD     *ZEROS        WIRD
092900040429     C*                  Z-ADD     *ZEROS        WIRDN
093000040429E   3C*                  ENDIF
093100040429E   2C*                  ENDIF
093200040429X   1C*                  ELSE
093300040429IF  2C*    WPKF          IFGT      3500
093400040429     C*                  Z-ADD     *ZEROS        WIRD
093500040429     C*                  Z-ADD     *ZEROS        WIRDN
093600040429E   2C*                  ENDIF
093700040429E   1C*                  ENDIF
093800941130     C*
093900941130     C* RICAVO STATISTICA RICAVI
094000941130     C                   ADD       1             V
094100941130     C                   Z-ADD     551           CR(V)
094200941130     C                   Z-ADD     WIRB          IR(V)
094300941130     C* RICAVO STATISTICA DELTA OLD
094400941130     C                   ADD       1             V
094500941130     C                   Z-ADD     552           CR(V)
094600941130     C                   Z-ADD     WIRD          IR(V)
094700941130     C* RICAVO STATISTICA CLIENTI
094800941130     C                   ADD       1             V
094900941130     C                   Z-ADD     553           CR(V)
095000941130     C                   Z-ADD     WIRBC         IR(V)
095100941130     C* RICAVO STATISTICA DELTA NEW
095200941130     C                   ADD       1             V
095300941130     C                   Z-ADD     554           CR(V)
095400941130     C                   Z-ADD     WIRDN         IR(V)
095500941130     C* RICAVO STATISTICA CONTO ECONOMICO (ORA LIBERO)
095600941130     C                   ADD       1             V
095700941130     C                   Z-ADD     555           CR(V)
095800941130     C                   Z-ADD     WIRBE         IR(V)
095900941130     C* IMPONIBILE REALE
096000941130     C                   ADD       1             V
096100941130     C                   Z-ADD     556           CR(V)
096200941130     C                   Z-ADD     WIMR          IR(V)
096300941201     C                   Z-ADD     V             WIIR              6 0
096400941130     C*
096500941130     C                   ENDSR
096600000201     C*------------------------------------------------------------------------*
096700000201     C* CNVEUR - CONVERTE I CAMPI DA DIVISA ESTERA A LIRE
096800000201     C*------------------------------------------------------------------------*
096900000201     C     CNVEUR        BEGSR
097000000201     C*
097100000201     C* AZZERA VARIABILI DI LAVORO
097200000203     C                   Z-ADD     *ZEROS        §IMO
097300000201     C*
097400000201     C                   CLEAR                   YEUR
097500000201     C                   MOVEL     §DVI          YECDVI
097600000201     C                   Z-ADD     §IMI          YECIMI
097700011218     C                   MOVEL     WDIVCE        YECDVO
097800011218     C                   MOVEL     '3'           YECDCO
097900011221     C                   MOVEL     'H'           YECTAR
098000000201     C                   CALL      'YEURCO'
098100000201     C                   PARM                    YEUR
098200000201IF  1C     YECESI        IFEQ      ' '
098300000201     C                   Z-ADD     YECIMO        §IMO
098400000201X   1C                   ELSE
098500000201     C                   Z-ADD     §IMI          §IMO
098600000201E   1C                   ENDIF
098700000201     C*
098800000201     C                   ENDSR
098900940314     C***********************************************************************
099000940314     C*   GESDSP - GESTIONE DELL'IMMAGINE PRIMA
099100940314     C***********************************************************************
099200940314     C     GESDSP        BEGSR
099300941215     C*
099400941215     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
099500941215     C     D$OLNP        IFNE      *ZEROS
099600950913     C     D$PFIG        ANDEQ     'S'
099700941215     C                   Z-ADD     D$OLNP        W$PLNP
099800941215     C                   ELSE
099900941215     C                   Z-ADD     D$PLNP        W$PLNP
100000941215     C                   ENDIF
100100941215     C     D$OTFP        IFNE      *ZEROS
100200950913     C     D$PFIG        ANDEQ     'S'
100300941215     C                   Z-ADD     D$OTFP        W$PTFP
100400941215     C                   ELSE
100500941215     C                   Z-ADD     D$PTFP        W$PTFP
100600941215     C                   ENDIF
100700941215     C     D$OKSC        IFNE      *ZEROS
100800950913     C     D$PFIG        ANDEQ     'S'
100900941215     C                   Z-ADD     D$OKSC        W$PKSC
101000941215     C                   ELSE
101100941215     C                   Z-ADD     D$PKSC        W$PKSC
101200941215     C                   ENDIF
101300941215     C     D$OCCM        IFNE      *ZEROS
101400950913     C     D$PFIG        ANDEQ     'S'
101500941215     C                   Z-ADD     D$OCCM        W$PCCM
101600941215     C                   ELSE
101700941215     C                   Z-ADD     D$PCCM        W$PCCM
101800941215     C                   ENDIF
101900941215     C     D$OMCT        IFNE      *BLANKS
102000950913     C     D$PFIG        ANDEQ     'S'
102100941215     C                   MOVEL     D$OMCT        W$PMCT
102200941215     C                   ELSE
102300941215     C                   MOVEL     D$PMCT        W$PMCT
102400941215     C                   ENDIF
102500940314     C*
102600931111     C* PREPARO CAMPI DI LAVORO -PRIMA-
102700941123     C                   Z-ADD     D$PMGS        DSMGS
102800931110     C                   Z-ADD     DSMES         WPMES
102900940304     C     DSGIO         IFLE      10
103000940304     C                   MOVEL     '1'           WPDEC
103100940304     C                   END
103200940304     C     DSGIO         IFGE      11
103300940304     C     DSGIO         ANDLE     20
103400940304     C                   MOVEL     '2'           WPDEC
103500940304     C                   END
103600940304     C     DSGIO         IFGE      21
103700940304     C                   MOVEL     '3'           WPDEC
103800940304     C                   END
103900030131     C*
104000030131     C* ----- SICOP -----
104100030131     C* AGGIORNA DAL 2003
104200030131     C                   Z-ADD     D$PAAS        DSWAAS
104300030131     C                   Z-ADD     D$PMGS        DSWMGS
104400030131     C                   Z-ADD     DSWDAT        WPDATCOP
104500020829     C*
104600020829     C* ----- PESO -----
104700020829     c* PRIMA agosto 2002
104800020829     C* SEMPRE PESO DA FATTURARE
104900020829     C                   Z-ADD     D$PAAS        DSWAAS
105000020829     C                   Z-ADD     D$PMGS        DSWMGS
105100020829IF  1C                   IF        DSWDAT < 20020800
105200020829     C                   Z-ADD     D$PPKF        WPPES
105300020829X   1C                   ELSE
105400020829     c* DOPO agosto 2002
105500020829     C* SE I COLLI=COLLI CML PRENDO PESO CML
105600020829IF  2C     D$PPKC        IFGT      *ZEROS
105700020829     C     D$PNCL        ANDEQ     D$PNCP
105800020829     C                   Z-ADD     D$PPKC        WPPES
105900020829     C                   Z-ADD     D$PPKB        WPPCC
106000020829     C                   Z-ADD     D$PPKC        WPPLS
106100020829X   2C                   ELSE
106200020829     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
106300020829IF  3C     D$PPKF        IFGT      D$PPKC
106400020829     C                   Z-ADD     D$PPKF        WPPES
106500020829X   3C                   ELSE
106600020829     C                   Z-ADD     D$PPKC        WPPES
106700020829E   3C                   ENDIF
106800020829E   2C                   ENDIF
106900020829E   1C                   ENDIF
107000990218     C*
107100990218     C* CARICA TABELLA -2L- FASCIE DI PESO
107200990218     C                   Z-ADD     D$PAAS        WA2L              4 0
107300990218     C                   EXSR      CART2L
107400931110     C*
107500940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE RICAVI/MOVIMENTI
107600020829     C*--- D$PPKF        IFEQ      *ZEROS
107700020829     C     WPPES         IFEQ      *ZEROS
107800940103     C                   MOVEL     FPR(1)        WPFPR
107900940103     C                   ELSE
108000931110     C                   DO        20            I                 4 0
108100020829     C*--- D$PPKF        IFGT      KIR(I)
108200020829     C*--- D$PPKF        ANDLE     KFR(I)
108300020829     C     WPPES         IFGT      KIR(I)
108400020829     C     WPPES         ANDLE     KFR(I)
108500940103     C                   MOVEL     FPR(I)        WPFPR
108600950104     C                   LEAVE
108700931110     C                   ENDIF
108800931110     C                   ENDDO
108900940103     C                   ENDIF
109000940103     C*
109100940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
109200020829     C*--- D$PPKF        IFEQ      *ZEROS
109300020829     C     WPPES         IFEQ      *ZEROS
109400940103     C                   MOVEL     FPC(1)        WPFPC
109500940103     C                   ELSE
109600940317     C                   DO        20            I
109700020829     C*--- D$PPKF        IFGT      KIC(I)
109800020829     C*--- D$PPKF        ANDLE     KFC(I)
109900020829     C     WPPES         IFGT      KIC(I)
110000020829     C     WPPES         ANDLE     KFC(I)
110100940103     C                   MOVEL     FPC(I)        WPFPC
110200950104     C                   LEAVE
110300940103     C                   ENDIF
110400940103     C                   ENDDO
110500940103     C                   ENDIF
110600940103     C*
110700940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE TRAINI
110800020829     C*--- D$PPKF        IFEQ      *ZEROS
110900020829     C     WPPES         IFEQ      *ZEROS
111000940103     C                   MOVEL     FPT(1)        WPFPT
111100940103     C                   ELSE
111200940317     C                   DO        20            I
111300020829     C*--- D$PPKF        IFGT      KIT(I)
111400020829     C*--- D$PPKF        ANDLE     KFT(I)
111500020829     C     WPPES         IFGT      KIT(I)
111600020829     C     WPPES         ANDLE     KFT(I)
111700940103     C                   MOVEL     FPT(I)        WPFPT
111800950104     C                   LEAVE
111900940103     C                   ENDIF
112000940103     C                   ENDDO
112100940103     C                   ENDIF
112200931111     C*
112300931111     C* RECUPERO LA REGIONE PARTENZA
112400931111     C                   Z-ADD     1             I
112500950324     C     W$PLNP        LOOKUP    FIL(I)                                 99
112600931111     C     *IN99         IFEQ      '0'
112700941223     C                   MOVEL     MSG(18)       $MSG
112800941026     C                   EXSR      SCRERR
112900931111     C                   ELSE
113000020514     C     D$PAAS        IFLE      2001
113100940207     C                   Z-ADD     RRE(I)        WPREP
113200020514     C                   ELSE
113300020514     C                   Z-ADD     RRN(I)        WPREP
113400020514     C                   ENDIF
113500931111     C                   END
113600931111     C*
113700931111     C* RECUPERO LA REGIONE DI ARRIVO
113800931111     C                   Z-ADD     1             I
113900931111     C     D$PLNA        LOOKUP    FIL(I)                                 99
114000931111     C     *IN99         IFEQ      '0'
114100941223     C                   MOVEL     MSG(18)       $MSG
114200941026     C                   EXSR      SCRERR
114300931111     C                   ELSE
114400020514     C     D$PAAS        IFLE      2001
114500940207     C                   Z-ADD     RRE(I)        WPREA
114600020514     C                   ELSE
114700020514     C                   Z-ADD     RRN(I)        WPREA
114800020514     C                   ENDIF
114900931111     C                   END
115000931110     C*
115100940511     C* ----- VOLUME -----
115200990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
115300990422IF  1C     D$PFVF        IFEQ      'T'
115400990422     C                   Z-ADD     D$PVLF        WPVOL
115500990422     C                   Z-ADD     D$PVLB        WPVCC
115600990422     C                   Z-ADD     D$PVLF        WPVLS
115700990422X   1C                   ELSE
115800990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
115900990422IF  2C     D$PVLC        IFGT      *ZEROS
116000990422     C     D$PNCL        ANDEQ     D$PNCR
116100990422     C                   Z-ADD     D$PVLC        WPVOL
116200990422     C                   Z-ADD     D$PVLB        WPVCC
116300990422     C                   Z-ADD     D$PVLC        WPVLS
116400990422X   2C                   ELSE
116500990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
116600990422IF  3C     D$PVLF        IFGT      D$PVLC
116700990422     C                   Z-ADD     D$PVLF        WPVOL
116800990422X   3C                   ELSE
116900990422     C                   Z-ADD     D$PVLC        WPVOL
117000990422E   3C                   ENDIF
117100990422E   2C                   ENDIF
117200990422E   1C                   ENDIF
117300931110     C*
117400931110     C* PESO TASSABILE (ECCEDENZA)
117500990824     C     D$PPVL        IFGT      D$PPKF
117600990824     C     D$PPVL        SUB       D$PPKF        WPPTS
117700931110     C                   END
117800031029     C*
117900031029     C* BANCALI
118000031029     C                   EVAL      WPBAN = D$PAR5NBA + D$PAR5NB2
118100940315     C*
118200940315     C* IMPOSTA SCHIERE CONSEGNE PARTICOLARI
118300941130     C                   MOVEA     PSVN          SVN
118400941130     C                   MOVEA     PVAN          VAN
118500090304     C                   MOVEL     D$Pncl        WPncl
118600090304     C                   MOVEL     D$PFTC        WPFTC
118700941123     C                   MOVEL     D$PTC2        WPTC2
118800941123     C                   MOVEL     D$PFGC        WPFGC
118900040322     C                   MOVEL     D$Pflo        WPflo
119000941123     C                   MOVEL     D$PFCA        WPFCA
119100040524     C                   MOVEL     D$Psta        WPsta
119200990823     C                   Z-ADD     D$PIAS        WPIAS
119300970901     C                   Z-ADD     D$PPOR        WPPOR
119400970901     C                   MOVEL     D$PLL1        WPLL1
119500970901     C                   MOVEL     D$PTPO        WPTPO
119600970901     C                   MOVEL     D$PFAP        WPFAP
119700970901     C                   MOVEL     D$PFIN        WPFIN
119800970901     C                   Z-ADD     D$PDFT        WPDFT
119900981105     C                   Z-ADD     D$PKSC        WPKSC
120000000201     C                   MOVEL     D$PDIV        WPDIV
120100020620     C                   MOVEL     D$PTBL        WPTBL
120200020620     C                   MOVEL     D$PCBO        WPCBO
120300030114     C                   EXSR      GESCSP
120400940315     C                   MOVEA     SVX           SVP
120500940315     C                   Z-ADD     IVX           IVP
120600940330     C                   Z-ADD     NSX           NSP
120700940315     C                   MOVEA     CPX           CPP
120800990617     C                   MOVEL     WPREG         WPPREG
120900940407     C*
121000990617     C* CONTROLLO SE LA BOLLA PRIMA E' DA CONSIDERARE O NO IN SISDT
121100941201     C     D$PFSP        IFEQ      'N'
121200941021     C     D$PSCL        OREQ      '2'
121300990617     C                   MOVEL     'N'           WTPREG
121400941021     C                   END
121500940411     C*
121600990617     C* SE E' STATA CONSEGNATA BISOGNA TENERNE CONTO PER I SALDI SISDA
121700940411     C     D$PDCM        IFEQ      *ZEROS
121800941201     C     D$PRBL        ORNE      'N'
121900990617     C                   MOVEL     'N'           WAPREG
122000940411     C                   END
122100940411     C*
122200940411     C* SE LINEA PARTENZA = CAGLIARI,PORTO TORRES,ORISTANO,BARI
122300990218     C* SE IL PESO DELLA SPEDIZIONE E' SUPERIORE A 35 QLI('98)/ 50 QLI ('99)
122400940411     C* SE IL TIPO BOLLA E' F3
122500940411     C* SIGNIFICA CHE SI TRATTA DI UNA BOLLA DI COMPETENZA TRAINI
122600940411     C* ED E' QUINDI DA NON CONSIDERARE COME RICAVO DELLA SARDEGNA
122700040429     c* usato pkf xchè cml sempre = 0
122800990218     C     D$PAAS        IFLE      1998
122900990218     C                   Z-ADD     3500          WKGLIM
123000990218     C                   ELSE
123100990218     C                   Z-ADD     5000          WKGLIM
123200990218     C                   ENDIF
123300940411     C     D$PLNP        IFEQ      81
123400940411     C     D$PLNP        OREQ      82
123500940411     C     D$PLNP        OREQ      104
123600940411     C     D$PLNP        OREQ      7
123700990421     C     D$PLNP        OREQ      113
123800990824     C     D$PPKF        IFGT      WKGLIM
123900940411     C     D$PTBL        ANDEQ     'F3'
124000990617     C                   MOVEL     'N'           WCPREG
124100990617     C                   MOVEL     'N'           WOPREG
124200990617     C                   MOVEL     'N'           WRPREG
124300990617     C                   MOVEL     'N'           WFPREG
124400040317     C                   MOVEL     'N'           WEPREG
124500940411     C                   END
124600940411     C                   END
124700950104     C*
124800950104     C* CONTROLLO SE LE COMPETENZE PRIMA SONO DA CALCOLARE
124900950104     C*    SE FLAG 'COMP.DA CALC.SEMPRE' = 'S' --> LANCIO
125000950104     C     D$PCCS        IFEQ      'S'
125100950905     C                   MOVEL     'S'           WFCP
125200950104     C                   ELSE
125300950104     C*    SE BOLLA FIGLIA E FLAG 'COMPET.DA CALC. SE FIGLIA' = SI --> LANCIO
125400950905     C     D$PFIG        IFEQ      'S'
125500950104     C     D$PCSF        ANDEQ     'S'
125600950104     C                   MOVEL     'S'           WFCP
125700950104     C                   ENDIF
125800950104     C                   ENDIF
125900950104     C*    SE BOLLA COMPLETAMENTE STORNATA (NC. A STORNO TOTALE) --> NO LANCIO
126000020318     C     D$PFA2        IFEQ      'S'
126100020318     C                   MOVEL     'N'           WFCP
126200020318     C                   ENDIF
126300941215     C*
126400941215     C* CONTROLLO SE LA BOLLA DEVE ESSERE CALCOLATA (COME IMPORTI E DATI)
126500941215IF  1C     D$PFRI        IFEQ      'S'
126600950104     C     WFCP          OREQ      'S'
126700941215     C*
126800941215     C                   MOVEL     'P'           WPD               1
126900941223     C*
127000941215     C* CONVERTE DATA BOLLA
127100941215     C                   Z-ADD     D$PMGS        DSMGS
127200941215     C                   Z-ADD     D$PAAS        DSAAS
127300941215     C                   Z-ADD     DSDAT8        WPDSP8            8 0
127400990217     C*********************Z-ADDDSDAT8    WDSP
127500941215     C*
127600941215     C* LEGGE IL BLOCCO COMPET.ZE CON IL QUALE LA BOLLA E'STATA PRECED.CALCOLATA
127700941215     C                   EXSR      LETNBP
127800941215     C*
127900941215     C* CONTROLLA SE IL N.BLOCCO PRIMA CHE E' IN MEMORIA E' VALIDO PER LA BOLLA
128000941215     C     $ERR          IFEQ      'N'
128100941215     C                   EXSR      CHKNBC
128200941215     C                   ENDIF
128300941215     C*
128400941215     C* CARICA LE COMPETENZE PRIMA DAI FILE ECCET E ECCED
128500941215     C     $ERR          IFEQ      'N'
128600941215     C                   EXSR      CARCOP
128700941215     C                   ENDIF
128800030114     C*
128900030509     C* LEGGE LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
129000030131     C* DAL 2003
129100030131     C                   MOVEL     'N'           WCOPREG           1
129200030131     C     WPDATCOP      IFGT      20030000
129300030131     C     $ERR          IFEQ      'N'
129400030114     C                   EXSR      LETCEV
129500030114     C                   ENDIF
129600030114     C*
129700030509     C* CARICA LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
129800030114     C     $ERR          IFEQ      'N'
129900030114     C                   EXSR      CARCEV
130000030114     C                   ENDIF
130100030131     C                   ENDIF
130200941215     C*
130300980323     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE TRA BARTOLINI E SDI
130400980323     C* SE BOLLA VALIDA PER RIFATT. BAR/SDI, CERCO COMPET. GIA' FATTURATE
130500011127     C*---       D$PAAS    IFGE 1998                       *SOLO DAL '98
130600011127     C*---                 MOVEL'S'       WIPREG           *FLAG REG. ECFIC ?
130700011127     C*---                 EXSR CARFIC                     *LEGGE ECFIC
130800011127     C*---                 END
130900980318     C*
131000941215E   1C                   ENDIF
131100980313     C*
131200980313     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE DA ITL A BAR/SDI
131300980323     C* SE BOLLA VALIDA PER RIFATT. ITL, CERCO MC GIA' FATTURATI
131400980323     C     D$PTBL        IFEQ      'F1'
131500980323     C     D$PTBL        OREQ      'A2'
131600980323     C     D$PTFP        IFNE      D$PTFA
131700980323     C     D$PAAS        ANDGE     1998
131800980323     C                   MOVEL     'S'           WMPREG
131900980323     C                   EXSR      CARFIM
132000980313     C                   END
132100980323     C                   END
132200931110     C*
132300940314     C                   ENDSR
132400940314     C***********************************************************************
132500940314     C*   GESDSD - GESTIONE DELL'IMMAGINE DOPO
132600940314     C***********************************************************************
132700940314     C     GESDSD        BEGSR
132800941215     C*
132900941215     C* SE RICEVO I DATI DELLA BOLLA ORIGINALE USO QUELLI AL POSTO DEI DATI DOPO
133000950905     C*
133100941215     C     D$OLNP        IFNE      *ZEROS
133200950913     C     D$DFIG        ANDEQ     'S'
133300941215     C                   Z-ADD     D$OLNP        W$DLNP
133400941215     C                   ELSE
133500941215     C                   Z-ADD     D$DLNP        W$DLNP
133600941215     C                   ENDIF
133700941215     C     D$OTFP        IFNE      *ZEROS
133800950913     C     D$DFIG        ANDEQ     'S'
133900941215     C                   Z-ADD     D$OTFP        W$DTFP
134000941215     C                   ELSE
134100941215     C                   Z-ADD     D$DTFP        W$DTFP
134200941215     C                   ENDIF
134300941215     C     D$OKSC        IFNE      *ZEROS
134400950913     C     D$DFIG        ANDEQ     'S'
134500941215     C                   Z-ADD     D$OKSC        W$DKSC
134600941215     C                   ELSE
134700941215     C                   Z-ADD     D$DKSC        W$DKSC
134800941215     C                   ENDIF
134900941215     C     D$OCCM        IFNE      *ZEROS
135000950913     C     D$DFIG        ANDEQ     'S'
135100941215     C                   Z-ADD     D$OCCM        W$DCCM
135200941215     C                   ELSE
135300941215     C                   Z-ADD     D$DCCM        W$DCCM
135400941215     C                   ENDIF
135500941215     C     D$OMCT        IFNE      *BLANKS
135600950913     C     D$DFIG        ANDEQ     'S'
135700941215     C                   MOVEL     D$OMCT        W$DMCT
135800941215     C                   ELSE
135900941215     C                   MOVEL     D$DMCT        W$DMCT
136000941215     C                   ENDIF
136100950905     C*
136200931111     C* PREPARO CAMPI DI LAVORO -DOPO-
136300941212     C                   Z-ADD     D$DMGS        DSMGS
136400931111     C                   Z-ADD     DSMES         WDMES
136500940304     C     DSGIO         IFLE      10
136600940304     C                   MOVEL     '1'           WDDEC
136700940304     C                   END
136800940304     C     DSGIO         IFGE      11
136900940304     C     DSGIO         ANDLE     20
137000940304     C                   MOVEL     '2'           WDDEC
137100940304     C                   END
137200940304     C     DSGIO         IFGE      21
137300940304     C                   MOVEL     '3'           WDDEC
137400940304     C                   END
137500030131     C*
137600030131     C* ----- SICOP -----
137700030131     C* AGGIORNA DAL 2003
137800030131     C                   Z-ADD     D$DAAS        DSWAAS
137900030131     C                   Z-ADD     D$DMGS        DSWMGS
138000030131     C                   Z-ADD     DSWDAT        WDDATCOP
138100020829     C*
138200020829     C* ----- PESO -----
138300020829     c* PRIMA agosto 2002
138400020829     C* SEMPRE PESO DA FATTURARE
138500020829     C                   Z-ADD     D$DAAS        DSWAAS
138600020829     C                   Z-ADD     D$DMGS        DSWMGS
138700020829IF  1C                   IF        DSWDAT < 20020800
138800020829     C                   Z-ADD     D$DPKF        WDPES
138900020829X   1C                   ELSE
139000020829     c* DOPO agosto 2002
139100020829     C* SE I COLLI=COLLI CML PRENDO PESO CML
139200020829IF  2C     D$DPKC        IFGT      *ZEROS
139300020829     C     D$DNCL        ANDEQ     D$DNCP
139400020829     C                   Z-ADD     D$DPKC        WDPES
139500020829     C                   Z-ADD     D$DPKB        WDPCC
139600020829     C                   Z-ADD     D$DPKC        WDPLS
139700020829X   2C                   ELSE
139800020829     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
139900020829IF  3C     D$DPKF        IFGT      D$DPKC
140000020829     C                   Z-ADD     D$DPKF        WDPES
140100020829X   3C                   ELSE
140200020829     C                   Z-ADD     D$DPKC        WDPES
140300020829E   3C                   ENDIF
140400020829E   2C                   ENDIF
140500020829E   1C                   ENDIF
140600990218     C*
140700990218     C* CARICA TABELLA -2L- FASCIE DI PESO
140800990218     C                   Z-ADD     D$DAAS        WA2L              4 0
140900990218     C                   EXSR      CART2L
141000931111     C*
141100940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE RICAVI/MOVIMENTI
141200020829     C*--- D$DPKF        IFEQ      *ZEROS
141300020829     C     WDPES         IFEQ      *ZEROS
141400940103     C                   MOVEL     FPR(1)        WDFPR
141500940103     C                   ELSE
141600940317     C                   DO        20            I
141700020829     C*--- D$DPKF        IFGT      KIR(I)
141800020829     C*--- D$DPKF        ANDLE     KFR(I)
141900020829     C     WDPES         IFGT      KIR(I)
142000020829     C     WDPES         ANDLE     KFR(I)
142100940103     C                   MOVEL     FPR(I)        WDFPR
142200950104     C                   LEAVE
142300931111     C                   ENDIF
142400931111     C                   ENDDO
142500940103     C                   ENDIF
142600940103     C*
142700940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
142800020829     C*--- D$DPKF        IFEQ      *ZEROS
142900020829     C     WDPES         IFEQ      *ZEROS
143000940103     C                   MOVEL     FPC(1)        WDFPC
143100940103     C                   ELSE
143200940317     C                   DO        20            I
143300020829     C*--- D$DPKF        IFGT      KIC(I)
143400020829     C*--- D$DPKF        ANDLE     KFC(I)
143500020829     C     WDPES         IFGT      KIC(I)
143600020829     C     WDPES         ANDLE     KFC(I)
143700940103     C                   MOVEL     FPC(I)        WDFPC
143800950104     C                   LEAVE
143900940103     C                   ENDIF
144000940103     C                   ENDDO
144100940103     C                   ENDIF
144200940103     C*
144300940103     C* FASCIA DI PESO DA TABELLA '2L' X FILE TRAINI
144400020829     C*--- D$DPKF        IFEQ      *ZEROS
144500020829     C     WDPES         IFEQ      *ZEROS
144600940103     C                   MOVEL     FPT(1)        WDFPT
144700940103     C                   ELSE
144800940317     C                   DO        20            I
144900020829     C*--- D$DPKF        IFGT      KIT(I)
145000020829     C*--- D$DPKF        ANDLE     KFT(I)
145100020829     C     WDPES         IFGT      KIT(I)
145200020829     C     WDPES         ANDLE     KFT(I)
145300940103     C                   MOVEL     FPT(I)        WDFPT
145400950104     C                   LEAVE
145500940103     C                   ENDIF
145600940103     C                   ENDDO
145700940103     C                   ENDIF
145800931111     C*
145900931111     C* RECUPERO LA REGIONE PARTENZA
146000931111     C                   Z-ADD     1             I
146100950324     C     W$DLNP        LOOKUP    FIL(I)                                 99
146200931111     C     *IN99         IFEQ      '0'
146300941223     C                   MOVEL     MSG(18)       $MSG
146400941026     C                   EXSR      SCRERR
146500931111     C                   ELSE
146600020514     C     D$DAAS        IFLE      2001
146700940207     C                   Z-ADD     RRE(I)        WDREP
146800020514     C                   ELSE
146900020514     C                   Z-ADD     RRN(I)        WDREP
147000020514     C                   ENDIF
147100931111     C                   END
147200931111     C*
147300931111     C* RECUPERO LA REGIONE DI ARRIVO
147400931111     C                   Z-ADD     1             I
147500931111     C     D$DLNA        LOOKUP    FIL(I)                                 99
147600931111     C     *IN99         IFEQ      '0'
147700941223     C                   MOVEL     MSG(18)       $MSG
147800941026     C                   EXSR      SCRERR
147900931111     C                   ELSE
148000020514     C     D$DAAS        IFLE      2001
148100940207     C                   Z-ADD     RRE(I)        WDREA
148200020514     C                   ELSE
148300020514     C                   Z-ADD     RRN(I)        WDREA
148400020514     C                   ENDIF
148500931111     C                   END
148600940121     C*
148700940511     C* ----- VOLUME -----
148800990422     C* SE VOLUME FATTURATO E' 'T' (TOTALE COLLI) PRENDO QUELLO
148900990422IF  1C     D$DFVF        IFEQ      'T'
149000990422     C                   Z-ADD     D$DVLF        WDVOL
149100990422     C                   Z-ADD     D$DVLB        WDVCC
149200990422     C                   Z-ADD     D$DVLF        WDVLS
149300990422X   1C                   ELSE
149400990422     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
149500990422IF  2C     D$DVLC        IFGT      *ZEROS
149600990422     C     D$DNCL        ANDEQ     D$DNCR
149700990422     C                   Z-ADD     D$DVLC        WDVOL
149800990422     C                   Z-ADD     D$DVLB        WDVCC
149900990422     C                   Z-ADD     D$DVLC        WDVLS
150000990422X   2C                   ELSE
150100990422     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
150200990422IF  3C     D$DVLF        IFGT      D$DVLC
150300990422     C                   Z-ADD     D$DVLF        WDVOL
150400990422X   3C                   ELSE
150500990422     C                   Z-ADD     D$DVLC        WDVOL
150600990422E   3C                   ENDIF
150700990422E   2C                   ENDIF
150800990422E   1C                   ENDIF
150900931111     C*
151000931111     C* PESO TASSABILE (ECCEDENZA)
151100990824     C     D$DPVL        IFGT      D$DPKF
151200990824     C     D$DPVL        SUB       D$DPKF        WDPTS
151300931111     C                   END
151400031029     C*
151500031029     C* BANCALI
151600031029     C                   EVAL      WDBAN = D$DAR5NBA + D$DAR5NB2
151700931111     C*
151800940315     C* IMPOSTA SCHIERE CONSEGNE PARTICOLARI
151900941130     C                   MOVEA     DSVN          SVN
152000941130     C                   MOVEA     DVAN          VAN
152100090304     C                   MOVEL     D$Dncl        WPncl
152200090304     C                   MOVEL     D$DFTC        WPFTC
152300941123     C                   MOVEL     D$DTC2        WPTC2
152400941123     C                   MOVEL     D$DFGC        WPFGC
152500040322     C                   MOVEL     D$DFlo        WPFlo
152600941123     C                   MOVEL     D$DFCA        WPFCA
152700040524     C                   MOVEL     D$Dsta        WPsta
152800990823     C                   Z-ADD     D$DIAS        WPIAS
152900970901     C                   Z-ADD     D$DPOR        WPPOR
153000970901     C                   MOVEL     D$DLL1        WPLL1
153100970901     C                   MOVEL     D$DTPO        WPTPO
153200970901     C                   MOVEL     D$DFAP        WPFAP
153300970901     C                   MOVEL     D$DFIN        WPFIN
153400970901     C                   Z-ADD     D$DDFT        WPDFT
153500981105     C                   Z-ADD     D$DKSC        WPKSC
153600000201     C                   MOVEL     D$DDIV        WPDIV
153700020620     C                   MOVEL     D$DTBL        WPTBL
153800020620     C                   MOVEL     D$DCBO        WPCBO
153900030114     C                   EXSR      GESCSP
154000940315     C                   MOVEA     SVX           SVD
154100940315     C                   Z-ADD     IVX           IVD
154200940330     C                   Z-ADD     NSX           NSD
154300940315     C                   MOVEA     CPX           CPD
154400990617     C                   MOVEL     WPREG         WPDREG
154500940407     C*
154600940407     C* CONTROLLO SE LA BOLLA DOPO E' DA CONSIDERARE O NO NELLA TRAINI
154700941201     C     D$DFSP        IFEQ      'N'
154800941021     C     D$DSCL        OREQ      '2'
154900990617     C                   MOVEL     'N'           WTDREG
155000941021     C                   END
155100940411     C*
155200940411     C* SE E' STATA CONSEGNATA BISOGNA TENERNE CONTO PER I SALDI AFFIDABILITA'
155300940411     C     D$DDCM        IFEQ      *ZEROS
155400941201     C     D$DRBL        ORNE      'N'
155500990617     C                   MOVEL     'N'           WADREG
155600940411     C                   END
155700940411     C*
155800940411     C* SE LINEA PARTENZA = CAGLIARI,PORTO TORRES,ORISTANO,BARI
155900990218     C* SE IL PESO DELLA SPEDIZIONE E' SUPERIORE A 35/50 QLI
156000940411     C* SE IL TIPO BOLLA E' F3
156100940411     C* SIGNIFICA CHE SI TRATTA DI UNA BOLLA DI COMPETENZA TRAINI
156200940411     C* ED E' QUINDI DA NON CONSIDERARE COME RICAVO DELLA SARDEGNA
156300990218     C     D$DAAS        IFLE      1998
156400990218     C                   Z-ADD     3500          WKGLIM
156500990218     C                   ELSE
156600990218     C                   Z-ADD     5000          WKGLIM
156700990218     C                   ENDIF
156800940411     C     D$DLNP        IFEQ      81
156900940411     C     D$DLNP        OREQ      82
157000940411     C     D$DLNP        OREQ      104
157100940411     C     D$DLNP        OREQ      7
157200990420     C     D$DLNP        OREQ      113
157300990824     C     D$DPKF        IFGT      WKGLIM
157400940411     C     D$DTBL        ANDEQ     'F3'
157500990617     C                   MOVEL     'N'           WCDREG
157600990617     C                   MOVEL     'N'           WODREG
157700990617     C                   MOVEL     'N'           WRDREG
157800990617     C                   MOVEL     'N'           WFDREG
157900040317     C                   MOVEL     'N'           WeDREG
158000940411     C                   END
158100940411     C                   END
158200950104     C*
158300950104     C* CONTROLLO SE LE COMPETENZE DOPO SONO DA CALCOLARE
158400950104     C*    SE FLAG 'COMP.DA CALC.SEMPRE' = 'S' --> LANCIO
158500950104     C     D$DCCS        IFEQ      'S'
158600950104     C                   MOVEL     'S'           WFCD
158700950104     C                   ELSE
158800950104     C*    SE BOLLA FIGLIA E FLAG 'COMPET.DA CALC. SE FIGLIA' = SI --> LANCIO
158900950905     C     D$DFIG        IFEQ      'S'
159000950104     C     D$DCSF        ANDEQ     'S'
159100950104     C                   MOVEL     'S'           WFCD
159200950104     C                   ENDIF
159300950104     C                   ENDIF
159400950104     C*    SE BOLLA COMPLETAMENTE STORNATA (NC. A STORNO TOTALE) --> NO LANCIO
159500020318     C     D$DFA2        IFEQ      'S'
159600020318     C                   MOVEL     'N'           WFCD
159700020318     C                   ENDIF
159800941215     C*
159900941215     C* CONTROLLO SE LA BOLLA DEVE ESSERE CALCOLATA (COME IMPORTI E DATI)
160000941215IF  1C     D$DFRI        IFEQ      'S'
160100950104     C     WFCD          OREQ      'S'
160200941215     C*
160300941215     C                   MOVEL     'D'           WPD               1
160400941215     C* CONVERTE DATA BOLLA DA LUNGA 6 A LUNGA 8
160500941215     C                   Z-ADD     D$DMGS        DSMGS
160600941215     C                   Z-ADD     D$DAAS        DSAAS
160700941215     C                   Z-ADD     DSDAT8        WDDSP8            8 0
160800941215     C*
160900941215     C* LEGGE IL BLOCCO COMPET.ZE ATTUALE DALLA TABELLA SE NON L'HA GIA' LETTO
161000941215     C     WFLNBD        IFNE      'S'
161100941215     C                   MOVEL     'S'           WFLNBD            1
161200941215     C                   EXSR      LETNBD
161300941215     C                   ENDIF
161400941215     C*
161500941215     C* CONTROLLA SE IL N.BLOCCO -DOPO- CHE E' IN MEMORIA E' VALIDO PER LA BOLLA
161600941215     C     $ERR          IFEQ      'N'
161700941215     C                   EXSR      CHKNBC
161800941215     C                   ENDIF
161900941223     C*
162000941223     C* CALCOLA LE COMPETENZE DOPO
162100941223     C     $ERR          IFEQ      'N'
162200941223     C                   EXSR      GESCOD
162300941223     C                   ENDIF
162400941215     C*
162500941215E   1C                   ENDIF
162600980313     C*
162700980313     C* CONTROLLO SE BOLLA VALIDA PER RIFATTURAZIONE DA ITL A BAR/SDI
162800980323     C     D$DTBL        IFEQ      'F1'
162900980323     C     D$DTBL        OREQ      'A2'
163000980323     C     D$DTFP        IFNE      D$DTFA
163100980323     C     D$DAAS        ANDGE     1998
163200980323     C                   MOVEL     'S'           WMDREG
163300980323     C                   Z-ADD     WDVOL         WDSMC
163400980313     C                   END
163500980323     C                   END
163600941215     C*
163700930323     C                   ENDSR
163800941215     C*------------------------------------------------------------------------*
163900941215     C*?CHKNBC - CONTROLLA N.BLOCCO COMPETENZE -PRIMA- O -DOPO-
164000941215     C*?         SE NON TROVATO CARICA DA FILE.
164100941215     C*------------------------------------------------------------------------*
164200941215     C     CHKNBC        BEGSR
164300941215     C*
164400941215     C                   MOVEL     'N'           WCARIC            1
164500941215     C*
164600941215     C* SE 'PRIMA' E SE N.BLOCCO RICHIESTO <> DAL BLOCCO ATTUALMENTE CARICATO
164700941215IF  1C     WPD           IFEQ      'P'
164800941215     C     WNBCP         ANDNE     WNBAP
164900941215     C*
165000941215     C* AZZERA LA SCHIERA PRIMA
165100941215     C                   CLEAR                   CDP
165200020130     C                   CLEAR                   FDP
165300941215     C                   CLEAR                   DDP
165400941215     C                   CLEAR                   DSP
165500941215     C                   CLEAR                   KCP
165600941215     C                   CLEAR                   KSP
165700941215     C                   CLEAR                   RIP
165800941215     C                   CLEAR                   DAP
165900941215     C                   CLEAR                   PEP
166000941215     C                   CLEAR                   FAP
166100941215     C                   CLEAR                   IEP
166200941215     C                   CLEAR                   CFP
166201160404     C                   CLEAR                   lcP
166300941215     C                   CLEAR                   MEP
166400941223     C                   CLEAR                   RCP
166500980312     C                   CLEAR                   RLP
166600941215     C*
166700941215     C* SE IL BLOCCO RICHIESTO E' UGUALE AL BLOCCO ATTUALE 'DOPO' CARICO DA DOPO
166800941215IF  2C     WNBCP         IFEQ      WNBAD
166900941215     C                   Z-ADD     CDD           CDP
167000020130     C                   MOVEL     FDD           FDP
167100941215     C                   Z-ADD     DDD           DDP
167200941215     C                   Z-ADD     DSD           DSP
167300941215     C                   Z-ADD     KCD           KCP
167400941215     C                   Z-ADD     KSD           KSP
167500941215     C                   MOVEL     RID           RIP
167600941215     C                   MOVEL     DAD           DAP
167700941215     C                   Z-ADD     PED           PEP
167800941215     C                   MOVEL     FAD           FAP
167900941215     C                   MOVEL     IED           IEP
168000941215     C                   Z-ADD     CFD           CFP
168001160405     C                   movel     lcD           lcP
168100941215     C                   MOVEL     MED           MEP
168200941223     C                   MOVEL     RCD           RCP
168300980312     C                   MOVEL     RLD           RLP
168400941215     C                   Z-ADD     WULTD         WULTP
168500941215X   2C                   ELSE
168600941215     C                   MOVEL     'S'           WCARIC
168700941215     C                   Z-ADD     WNBCP         WNBC
168800941215     C                   Z-ADD     *ZEROS        WULTP             5 0
168900941215E   2C                   ENDIF
169000941215E   1C                   ENDIF
169100941215     C*
169200941215     C* SE 'DOPO' E SE N.BLOCCO RICHIESTO <> DAL BLOCCO ATTUALMENTE CARICATO
169300941215IF  1C     WPD           IFEQ      'D'
169400941215     C     WNBCD         ANDNE     WNBAD
169500941215     C*
169600941215     C* AZZERA LA SCHIERA DOPO
169700941215     C                   CLEAR                   CDD
169800020130     C                   CLEAR                   FDD
169900941215     C                   CLEAR                   DDD
170000941215     C                   CLEAR                   DSD
170100941215     C                   CLEAR                   KCD
170200941215     C                   CLEAR                   KSD
170300941215     C                   CLEAR                   RID
170400941215     C                   CLEAR                   DAD
170500941215     C                   CLEAR                   PED
170600941215     C                   CLEAR                   FAD
170700941215     C                   CLEAR                   IED
170800941215     C                   CLEAR                   CFD
170801160404     C                   CLEAR                   lcD
170900941215     C                   CLEAR                   MED
171000941223     C                   CLEAR                   RCD
171100980312     C                   CLEAR                   RLD
171200941215     C*
171300941215     C* SE IL BLOCCO RICHIESTO E' UGUALE AL BLOCCO ATTUALE 'DOPO' CARICO DA DOPO
171400941215IF  2C     WNBCD         IFEQ      WNBAP
171500941215     C                   Z-ADD     CDP           CDD
171600020130     C                   MOVEL     FDP           FDD
171700941215     C                   Z-ADD     DDP           DDD
171800941215     C                   Z-ADD     DSP           DSD
171900941215     C                   Z-ADD     KCP           KCD
172000941215     C                   Z-ADD     KSP           KSD
172100941215     C                   MOVEL     RIP           RID
172200941215     C                   MOVEL     DAP           DAD
172300941215     C                   Z-ADD     PEP           PED
172400941215     C                   MOVEL     FAP           FAD
172500941215     C                   MOVEL     IEP           IED
172600941215     C                   Z-ADD     CFP           CFD
172601160405     C                   movel     lcP           lcD
172700941215     C                   MOVEL     MEP           MED
172800941223     C                   MOVEL     RCP           RCD
172900980312     C                   MOVEL     RLP           RLD
173000941215     C                   Z-ADD     WULTP         WULTD
173100941215X   2C                   ELSE
173200941215     C                   MOVEL     'S'           WCARIC
173300941215     C                   Z-ADD     WNBCD         WNBC
173400941215     C                   Z-ADD     *ZEROS        WULTD             5 0
173500941215E   2C                   ENDIF
173600941215E   1C                   ENDIF
173700941215     C*
173800941215     C* SE DEVO CARICARE DAL FILE
173900941215IF  1C     WCARIC        IFEQ      'S'
174000941215     C*
174100941215     C* IMPOSTO NUMERO BLOCCO ATTUALE
174200941215IF  2C     WPD           IFEQ      'P'
174300941215     C                   Z-ADD     WNBC          WNBAP
174400941215X   2C                   ELSE
174500941215     C                   Z-ADD     WNBC          WNBAD
174600941215E   2C                   ENDIF
174700941215     C*
174800941215     C* LEGGO TESTATA COMPETENZE
174900941215     C                   Z-ADD     WNBC          ECTNBC
175000941215     C     KEYECT        SETLL     ECECT01L
175100941215     C     KEYECT        READE     ECECT01L                               99
175200941215     C*
175300941215     C* FINO A FINE FILE
175400941215DO  2C     *IN99         DOWEQ     *OFF
175500941215     C     $ERR          ANDEQ     'N'
175600941215     C*
175700941215     C* CARICO SOLO QUELLE COMPETENZE CHE NON SONO ANNULLATE, CHE AGGIORNANO IL
175800941215     C* CONTO ECONOMICO E CHE SONO VALIDE ALLA DATA SPEDIZIONE
175900941215     C                   Z-ADD     ECTCMP        DEWCMP
176000941215DO  3C     *IN99         DOWEQ     *OFF
176100941215     C     ECTCMP        ANDEQ     DEWCMP
176200941215     C     $ERR          ANDEQ     'N'
176300941215     C*
176400941215     C* NEL FILE ECECT00F LE DATE (DECOR E SCAD) SONO MEMORIZZATE IN AAAA/MM/00
176500941215IF  4C     ECTATB        IFEQ      *BLANKS
176600941215     C     ECTMCE        ANDEQ     'S'
176700941215     C*
176800941215     C* LEGGO IL DETTAGLIO DELLA COMPETENZA VALIDA
176900941215     C                   Z-ADD     WNBC          ECDNBC
177000941215     C                   Z-ADD     DEWCMP        ECDCMP
177100941215     C     KEYECD        SETLL     ECECD01L
177200941215     C     KEYECD        READE     ECECD01L                               99
177300941215     C*
177400941215     C* FINO A FINE FILE
177500941215DO  5C     *IN99         DOWEQ     *OFF
177600941215     C     $ERR          ANDEQ     'N'
177700941215     C*
177800941215     C* NEL FILE ECECD00F LE DATE (DECOR E SCAD) SONO MEMORIZZATE IN AAAA/MM/00
177900941215IF  6C     ECDATB        IFEQ      *BLANKS
178000040120     C*
178100040120     C* carica solo i conti validi alla data decorrenza/scadenza della competenz
178200040120IF 6ac                   IF        ecdddo>=ectddc and ecdddo<=ectdsc and
178300040120     c                             ecddso>=ectddc and ecddso<=ectdsc
178400941215     C*
178500941215     C* CARICO LA SCHIERA PRIMA O DOPO
178600941215IF  7C     WPD           IFEQ      'P'
178700941215     C                   ADD       1             WULTP
178800941215     C                   Z-ADD     WULTP         IP
178900941215     C                   Z-ADD     ECTCMP        CDP(IP)
179000020130     C                   MOVEL     ECTFDR        FDP(IP)
179100941215     C                   Z-ADD     ECDDDO        DDP(IP)
179200941215     C                   Z-ADD     ECDDSO        DSP(IP)
179300941215     C                   Z-ADD     ECDKCC        KCP(IP)
179400941215     C                   Z-ADD     ECDKSC        KSP(IP)
179500941215     C                   MOVEL     ECDRIC        RIP(IP)
179600941215     C                   MOVEL     ECDSEG        DAP(IP)
179700941215     C                   Z-ADD     ECDPER        PEP(IP)
179800941215     C                   MOVEL     ECDFRA        FAP(IP)
179900941215     C                   MOVEL     ECDIOE        IEP(IP)
180000941215     C                   Z-ADD     ECDCCF        CFP(IP)
180100941215     C                   Z-ADD     ECDCMP        CP
180200941215     C                   MOVEL     ECTMCE        MEP(CP)
180300941223     C                   MOVEL     ECTFCR        RCP(CP)
180400980312     C                   MOVEL     ECTRIC        RLP(CP)
180401160405     c* centro di costo logistica
180403160408     c                   if        ecdccf  > 0 and ecdccf <= 99999
180404160408     C                   eval      wfil = ecdccf/100
180405160419     c                   if        wfil <> 0
180406160405     c                   z-add     1             l                 3 0
180407160405     c     wfil          lookup    flog(l)                                44
180408160405     c                   if        *in44
180409160405     C                   eval      lCP(cp)='S'
180410160405     c                   end
180411160419     c                   end
180412160405     c                   end
180500941215X   7C                   ELSE
180600941215     C                   ADD       1             WULTD
180700941215     C                   Z-ADD     WULTD         ID
180800941215     C                   Z-ADD     ECTCMP        CDD(ID)
180900020130     C                   MOVEL     ECTFDR        FDD(ID)
181000941215     C                   Z-ADD     ECDDDO        DDD(ID)
181100941215     C                   Z-ADD     ECDDSO        DSD(ID)
181200941215     C                   Z-ADD     ECDKCC        KCD(ID)
181300941215     C                   Z-ADD     ECDKSC        KSD(ID)
181400941215     C                   MOVEL     ECDRIC        RID(ID)
181500941215     C                   MOVEL     ECDSEG        DAD(ID)
181600941215     C                   Z-ADD     ECDPER        PED(ID)
181700941215     C                   MOVEL     ECDFRA        FAD(ID)
181800941215     C                   MOVEL     ECDIOE        IED(ID)
181900941215     C                   Z-ADD     ECDCCF        CFD(ID)
182000941215     C                   Z-ADD     ECDCMP        CD
182100941215     C                   MOVEL     ECTMCE        MED(CD)
182200941223     C                   MOVEL     ECTFCR        RCD(CD)
182300980312     C                   MOVEL     ECTRIC        RLD(CD)
182301160405     c* centro di costo logistica
182302160408     c                   if        ecdccf  > 0 and ecdccf <= 99999
182303160408     C                   eval      wfil = ecdccf/100
182304160419     c                   if        wfil <> 0
182305160405     c                   z-add     1             l
182306160405     c     wfil          lookup    flog(l)                                44
182307160405     c                   if        *in44
182308160405     C                   eval      lcd(cd) ='S'
182309160405     c                   end
182310160419     c                   end
182311160405     c                   end
182400941215E   7C                   ENDIF
182500941215E   6C                   ENDIF
182600040120E  6aC                   ENDIF
182700941215     C*
182800941215     C     KEYECD        READE     ECECD01L                               99
182900941215E   5C                   ENDDO
183000941215E   4C                   ENDIF
183100941215     C*
183200941215     C     KEYECT        READE     ECECT01L                               99
183300941215E   3C                   ENDDO
183400941215     C*
183500941215E   2C                   ENDDO
183600941215E   1C                   ENDIF
183700941215     C*
183800941215     C                   ENDSR
183900941215     C*------------------------------------------------------------------------*
184000941215     C*?LETNBP - LEGGO DALLA TESTATA IMP.COMPET.   IL N.BLOCCO USATO 'PRIMA'
184100941215     C*------------------------------------------------------------------------*
184200941215     C     LETNBP        BEGSR
184300941215     C*
184400941215     C                   Z-ADD     *ZEROS        WNBCP             9 0
184500941215     C*
184600941215     C* LEGGO LE COMPETENZE FISSE PER SAPERE IL BLOCCO COMPETENZA
184700941215     C                   Z-ADD     D$PAAS        CETAAS
184800941215     C                   Z-ADD     D$PLNP        CETLNP
184900941215     C                   Z-ADD     D$PNRS        CETNRS
185000941215     C                   Z-ADD     D$PNSP        CETNSP
185100941215     C                   MOVEL     D$PTBL        CETTBL
185200950316     C                   SETOFF                                       0102
185300950316     C     KEYCET        CHAIN     ECCET30C                           99
185400941215     C*
185500941215     C* IMPORTI FISSI NON TROVATI, ERRORE
185600941215IF  1C     *IN99         IFEQ      *ON
185700941215     C                   MOVEL     MSG(4)        $MSG
185800941215     C                   EXSR      SCRERR
185900030213E   2C*                  ENDIF
186000941215X   1C                   ELSE
186100941215     C                   Z-ADD     CETNBC        WNBCP
186200941215     C                   MOVEL     'S'           WCETPR
186300941215E   1C                   ENDIF
186400941215     C*
186500941215     C                   ENDSR
186600941215     C*------------------------------------------------------------------------*
186700980130     C*?LETNBD - LEGGO DALLA TABELLA '2K' L'ULTIMO N.BLOCCO   --> N.BLOCCO DOPO
186800941215     C*------------------------------------------------------------------------*
186900941215     C     LETNBD        BEGSR
187000941215     C*
187100941215     C                   OPEN      TABEL00F
187200941215     C*
187300950404     C                   Z-ADD     CODUT         KTBUTE
187400980130     C                   MOVEL     '2K'          KTBCOD
187500941215     C                   MOVEL     *BLANKS       KTBKEY
187600941215     C     KEYTAB        CHAIN     TABEL00F                           99
187700941215IF  1C     *IN99         IFEQ      *ON
187800980130     C                   MOVEL     MSG(1)        $MSG
187900941215     C                   EXSR      SCRERR
188000941215X   1C                   ELSE
188100980130     C                   MOVEL     TBLUNI        DS2K
188200980130     C                   Z-ADD     §2KNBC        WNBCD             9 0
188300941215     C*
188400941215     C* SE IL FLAG 'N.BLOCCO GIA UTILIZZATO ANCORA = A 'NO' LO AGGIORNO'
188500980130IF  2C     §2KUBC        IFEQ      'N'
188600991117IF  3C     D$GFSI        IFNE      'S'
188700980130     C                   MOVEL     'S'           §2KUBC
188800980130     C                   MOVEL     DS2K          TBLUNI
188900941215     C                   UPDATE    TABEL
189000991117E   3C                   ENDIF
189100991117E   2C                   ENDIF
189200941215E   1C                   ENDIF
189300941215     C*
189400941215     C                   CLOSE     TABEL00F
189500941215     C*
189600941215     C                   ENDSR
189700941215     C*------------------------------------------------------------------------*
189800941215     C*?CARCOP - CARICA GLI IMPORTI DELLE COMPETENZE -PRIMA-
189900941215     C*------------------------------------------------------------------------*
190000941215     C     CARCOP        BEGSR
190100941215     C*--------------------
190200941215     C* COMPETENZE FISSE
190300941215     C*--------------------
190400941215     C                   Z-ADD     CETPAR        IMP(1)
190500020129     C                   ADD       CETPAR        WPIRP
190600941215     C                   Z-ADD     CETPAM        IMP(2)
190700020129     C                   ADD       CETPAM        WPIRP
190800941215     C                   Z-ADD     CETPAU        IMP(3)
190900020129     C                   ADD       CETPAU        WPIRP
191000941215     C                   Z-ADD     CETPAV        IMP(4)
191100020129     C                   ADD       CETPAV        WPIRP
191200941215     C                   Z-ADD     CETARD        IMP(5)
191300020129     C                   ADD       CETARD        WPIRP
191400941215     C                   Z-ADD     CETARM        IMP(6)
191500020129     C                   ADD       CETARM        WPIRP
191600941215     C                   Z-ADD     CETARU        IMP(7)
191700020129     C                   ADD       CETARU        WPIRP
191800941215     C                   Z-ADD     CETARV        IMP(8)
191900020129     C                   ADD       CETARV        WPIRP
192000950315     C                   Z-ADD     CETMAF        IMP(9)
192100020129     C                   ADD       CETMAF        WPIRP
192200950315     C                   Z-ADD     CETMDE        IMP(10)
192300020129     C                   ADD       CETMDE        WPIRP
192400941215     C                   Z-ADD     CETTRL        IMP(13)
192500020129     C                   ADD       CETTRL        WPIRP
192600941223     C                   Z-ADD     CETTRL        WPCTL
192700950315     C                   Z-ADD     CETTAF        IMP(14)
192800020129     C                   ADD       CETTAF        WPIRP
192900950315     C                   Z-ADD     CETTDE        IMP(15)
193000020129     C                   ADD       CETTDE        WPIRP
193100941215     C                   Z-ADD     CETPAG        IMP(29)
193200020129     C                   ADD       CETPAG        WPIRP
193300941215     C                   Z-ADD     CETSEA        IMP(26)
193400020129     C                   ADD       CETSEA        WPIRP
193500941215     C                   Z-ADD     CETSEC        IMP(25)
193600020129     C                   ADD       CETSEC        WPIRP
193700941215     C                   Z-ADD     CETRCV        IMP(27)
193800020129     C                   ADD       CETRCV        WPIRP
193900010827     C                   Z-ADD     CETSES        IMP(33)
194000020129     C                   ADD       CETSES        WPIRP
194100941215     C*--------------------
194200941215     C* COMPETENZE VARIABILI
194300941215     C*--------------------
194400941215     C* LEGGO LE COMPETENZE VARIABILI SE CI SONO
194500020805IF  1C     CETDET        IFEQ      'S'
194600941215     C                   Z-ADD     D$PAAS        CEDAAS
194700941215     C                   Z-ADD     D$PLNP        CEDLNP
194800941215     C                   Z-ADD     D$PNRS        CEDNRS
194900941215     C                   Z-ADD     D$PNSP        CEDNSP
195000941215     C                   MOVEL     D$PTBL        CEDTBL
195100950316     C                   SETOFF                                       0304
195101160405     c                   clear                   wlogicmpp
195200950316     C     KEYCEP        SETLL     ECCED30C
195300950316     C     KEYCEP        READE     ECCED30C                               99
195400941215     C*
195500941215     C* IMPORTI COMPETENZE VARIABILI NON TROVATI (ANCHE SE PREVISTI), ERRORE
195600941215IF  2C     *IN99         IFEQ      *ON
195700020805     C                   MOVEL     MSG(5)        $MSG
195800020805     C                   EXSR      SCRERR
195900941215X   2C                   ELSE
196000941215     C*
196100941215     C* IMPORTI COMPETENZE VARIABILI TROVATI, CARICO
196200941215DO  3C     *IN99         DOWEQ     *OFF
196300941215     C                   Z-ADD     CEDCMP        CP
196400941215     C                   Z-ADD     CEDIMP        IMP(CP)
196500950124     C                   ADD       CEDIMP        WPIRP
196600941223     C     CP            IFEQ      11
196700941223     C                   Z-ADD     IMP(CP)       WPCT1
196800941223     C                   ENDIF
196900941223     C     CP            IFEQ      12
197000941223     C                   Z-ADD     IMP(CP)       WPCT2
197100941223     C                   ENDIF
197200040618     c* aggiunte competenze transiti 3/4/5 + quelle dimenticate
197300040618     c* per effetto dello sbinamento delle competenze transiti 1/2
197400040617     C     CP            IFEQ      205
197500040617     C                   add       IMP(CP)       WPCT2
197600040617     C                   ENDIF
197700040618     C     CP            IFEQ      204
197800040618     C                   add       IMP(CP)       WPCT1
197900040618     C                   ENDIF
198000040519     C     CP            IFEQ      53
198100040519     C                   Z-ADD     IMP(CP)       WPCT3
198200040519     C                   ENDIF
198300040617     C     CP            IFEQ      206
198400040617     C                   add       IMP(CP)       WPCT3
198500040617     C                   ENDIF
198600040519     C     CP            IFEQ      54
198700040519     C                   Z-ADD     IMP(CP)       WPCT4
198800040519     C                   ENDIF
198900040617     C     CP            IFEQ      207
199000040617     C                   add       IMP(CP)       WPCT4
199100040617     C                   ENDIF
199200040519     C     CP            IFEQ      55
199300040519     C                   Z-ADD     IMP(CP)       WPCT5
199400040519     C                   ENDIF
199500040617     C     CP            IFEQ      208
199600040617     C                   add       IMP(CP)       WPCT5
199700040617     C                   ENDIF
199800040618     c* obsoleta
199900040618     C*    CP            IFEQ      32
200000040618     C*                  Z-ADD     IMP(CP)       WPCTL
200100040618     C*                  ENDIF
200101160405     c* se competenza di logistica aggiungo importo
200102160405     c                   if        lcp(cp) = 'S'
200103160405     c                   add       imp(cp)       wlogicmpp
200104160405     c                   end
200200950316     C                   SETOFF                                       0304
200300950316     C     KEYCEP        READE     ECCED30C                               99
200400030114E   3C                   ENDDO
200500970217     C* TOGLIE I TRANSITI AI SALDI CLIENTE
200600970217     C                   SUB       WPCT1         WPIRP
200700970217     C                   SUB       WPCT2         WPIRP
200800040519     C                   SUB       WPCT3         WPIRP
200900040519     C                   SUB       WPCT4         WPIRP
201000040519     C                   SUB       WPCT5         WPIRP
201100941215E   2C                   ENDIF
201200020805E   1C                   ENDIF
201300941215     C*
201400941223     C* TROVO COMP.DELTA PRESUNTO (SOMMA DI TUTTE LE COMPETENZE)
201500941223     C                   Z-ADD     900           CP
201600950124     C                   Z-ADD     WPIRP         IMP(CP)
201700950412     C*
201800950412     C* SE SPEDIZ. OLTRE 35 QLI AZZERO LA COMP.DELTA PRESUNTO ANCHE NEL -PRIMA-
201900950412     C* (NEL -DOPO- VIENE AZZERATA DALLA TARIFFA DELLA VOCE 900)
202000040429     C*    D$PAAS        IFLE      1998
202100040429     C*                  Z-ADD     3500          WKGLIM
202200040429     C*                  ELSE
202300040429     C*                  Z-ADD     5000          WKGLIM
202400040429     C*                  ENDIF
202500040429     C*    D$PAAS        IFLT      2000
202600040429     C*    D$PPKF        IFGT      WKGLIM
202700040429     C*                  Z-ADD     *ZEROS        WPIRP
202800040429     C*                  Z-ADD     900           CP
202900040429     C*                  Z-ADD     *ZEROS        IMP(CP)
203000040429     C*                  ENDIF
203100040429     C*                  ENDIF
203200941215     C*
203300941215     C                   ENDSR
203400030114     C*------------------------------------------------------------------------*
203500030509     C*?LETCEV - LEGGE LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
203600030114     C*------------------------------------------------------------------------*
203700030114     C     LETCEV        BEGSR
203800030114     C*
203900030114     C                   Z-ADD     D$PAAS        CEVAAS
204000030114     C                   Z-ADD     D$PLNP        CEVLNP
204100030114     C                   Z-ADD     D$PNRS        CEVNRS
204200030114     C                   Z-ADD     D$PNSP        CEVNSP
204300030114     C                   MOVEL     D$PTBL        CEVTBL
204400030114     C                   SETOFF                                       0506
204500030114     C     KEYCEV        CHAIN     ECCEV30C                           99
204600030114     C*
204700030114     C* RECORD NON TROVATO, ERRORE
204800030114IF  1C     *IN99         IFEQ      *ON
204900030114     C                   MOVEL     MSG(24)       $MSG
205000030114     C                   EXSR      SCRERR
205100030213E   2C*                  ENDIF
205200030114X   1C                   ELSE
205300030114     C                   MOVEL     'S'           WCEVPR
205400030114E   1C                   ENDIF
205500030114     C*
205600030114     C                   ENDSR
205700030114     C*------------------------------------------------------------------------*
205800030509     C*?CARCEV - CARICA LE VOCI COOP PRIMA DAL FILE ECCEV e ECC2V
205900030114     C*------------------------------------------------------------------------*
206000030114     C     CARCEV        BEGSR
206100030114     C*
206200030512     C* IMPOSTA LE VARIABILI DI LAVORO
206300030512     c                   MOVEL     'N'           WC2VP             1
206400030512     C*
206500030507     C     CEVC01        IFGT      *ZEROS
206600030507     C                   Z-ADD     CEVI01        IMPV(CEVC01)
206700030507     C                   MOVEL     'S'           COPPV(CEVC01)
206800030507     C                   ENDIF
206900030507     C     CEVC02        IFGT      *ZEROS
207000030507     C                   Z-ADD     CEVI02        IMPV(CEVC02)
207100030507     C                   MOVEL     'S'           COPPV(CEVC02)
207200030507     C                   ENDIF
207300030507     C     CEVC03        IFGT      *ZEROS
207400030507     C                   Z-ADD     CEVI03        IMPV(CEVC03)
207500030507     C                   MOVEL     'S'           COPPV(CEVC03)
207600030507     C                   ENDIF
207700030124     C     CEVC04        IFGT      *ZEROS
207800030507     C                   Z-ADD     CEVI04        IMPV(CEVC04)
207900030507     C                   MOVEL     'S'           COPPV(CEVC04)
208000030507     C                   ENDIF
208100030124     C     CEVC05        IFGT      *ZEROS
208200030115     C                   Z-ADD     CEVI05        IMPV(CEVC05)
208300030507     C                   MOVEL     'S'           COPPV(CEVC05)
208400030507     C                   ENDIF
208500030124     C     CEVC06        IFGT      *ZEROS
208600030507     C                   Z-ADD     CEVI06        IMPV(CEVC06)
208700030507     C                   MOVEL     'S'           COPPV(CEVC06)
208800030507     C                   ENDIF
208900030507     C     CEVC07        IFGT      *ZEROS
209000030507     C                   Z-ADD     CEVI07        IMPV(CEVC07)
209100030507     C                   MOVEL     'S'           COPPV(CEVC07)
209200030507     C                   ENDIF
209300030507     C     CEVC08        IFGT      *ZEROS
209400030507     C                   Z-ADD     CEVI08        IMPV(CEVC08)
209500030214     C                   MOVEL     'S'           COPPV(CEVC08)
209600030124     C                   ENDIF
209700030124     C     CEVC09        IFGT      *ZEROS
209800030115     C                   Z-ADD     CEVI09        IMPV(CEVC09)
209900030214     C                   MOVEL     'S'           COPPV(CEVC09)
210000030124     C                   ENDIF
210100030124     C     CEVC10        IFGT      *ZEROS
210200030115     C                   Z-ADD     CEVI10        IMPV(CEVC10)
210300030214     C                   MOVEL     'S'           COPPV(CEVC10)
210400030124     C                   ENDIF
210500030124     C     CEVC11        IFGT      *ZEROS
210600030115     C                   Z-ADD     CEVI11        IMPV(CEVC11)
210700030214     C                   MOVEL     'S'           COPPV(CEVC11)
210800030124     C                   ENDIF
210900030124     C     CEVC12        IFGT      *ZEROS
211000030115     C                   Z-ADD     CEVI12        IMPV(CEVC12)
211100030214     C                   MOVEL     'S'           COPPV(CEVC12)
211200030124     C                   ENDIF
211300030124     C     CEVC13        IFGT      *ZEROS
211400030115     C                   Z-ADD     CEVI13        IMPV(CEVC13)
211500030214     C                   MOVEL     'S'           COPPV(CEVC13)
211600030124     C                   ENDIF
211700030124     C     CEVC14        IFGT      *ZEROS
211800030115     C                   Z-ADD     CEVI14        IMPV(CEVC14)
211900030214     C                   MOVEL     'S'           COPPV(CEVC14)
212000030124     C                   ENDIF
212100030124     C     CEVC15        IFGT      *ZEROS
212200030115     C                   Z-ADD     CEVI15        IMPV(CEVC15)
212300030214     C                   MOVEL     'S'           COPPV(CEVC15)
212400030124     C                   ENDIF
212500030124     C     CEVC16        IFGT      *ZEROS
212600030115     C                   Z-ADD     CEVI16        IMPV(CEVC16)
212700030214     C                   MOVEL     'S'           COPPV(CEVC16)
212800030124     C                   ENDIF
212900030124     C     CEVC17        IFGT      *ZEROS
213000030115     C                   Z-ADD     CEVI17        IMPV(CEVC17)
213100030214     C                   MOVEL     'S'           COPPV(CEVC17)
213200030124     C                   ENDIF
213300030124     C     CEVC18        IFGT      *ZEROS
213400030115     C                   Z-ADD     CEVI18        IMPV(CEVC18)
213500030214     C                   MOVEL     'S'           COPPV(CEVC18)
213600030124     C                   ENDIF
213700030124     C     CEVC19        IFGT      *ZEROS
213800030115     C                   Z-ADD     CEVI19        IMPV(CEVC19)
213900030214     C                   MOVEL     'S'           COPPV(CEVC19)
214000030124     C                   ENDIF
214100030124     C     CEVC20        IFGT      *ZEROS
214200030115     C                   Z-ADD     CEVI20        IMPV(CEVC20)
214300030214     C                   MOVEL     'S'           COPPV(CEVC20)
214400030124     C                   ENDIF
214500030124     C     CEVC21        IFGT      *ZEROS
214600030115     C                   Z-ADD     CEVI21        IMPV(CEVC21)
214700030214     C                   MOVEL     'S'           COPPV(CEVC21)
214800030124     C                   ENDIF
214900030124     C     CEVC22        IFGT      *ZEROS
215000030115     C                   Z-ADD     CEVI22        IMPV(CEVC22)
215100030214     C                   MOVEL     'S'           COPPV(CEVC22)
215200030124     C                   ENDIF
215300030124     C     CEVC23        IFGT      *ZEROS
215400030124     C                   Z-ADD     CEVI23        IMPV(CEVC23)
215500030214     C                   MOVEL     'S'           COPPV(CEVC23)
215600030124     C                   ENDIF
215700030124     C     CEVC24        IFGT      *ZEROS
215800030115     C                   Z-ADD     CEVI24        IMPV(CEVC24)
215900030214     C                   MOVEL     'S'           COPPV(CEVC24)
216000030124     C                   ENDIF
216100030124     C     CEVC25        IFGT      *ZEROS
216200030115     C                   Z-ADD     CEVI25        IMPV(CEVC25)
216300030214     C                   MOVEL     'S'           COPPV(CEVC25)
216400030124     C                   ENDIF
216500030124     C     CEVC26        IFGT      *ZEROS
216600030115     C                   Z-ADD     CEVI26        IMPV(CEVC26)
216700030214     C                   MOVEL     'S'           COPPV(CEVC26)
216800030124     C                   ENDIF
216900030124     C     CEVC27        IFGT      *ZEROS
217000030115     C                   Z-ADD     CEVI27        IMPV(CEVC27)
217100030214     C                   MOVEL     'S'           COPPV(CEVC27)
217200030124     C                   ENDIF
217300030124     C     CEVC28        IFGT      *ZEROS
217400030115     C                   Z-ADD     CEVI28        IMPV(CEVC28)
217500030214     C                   MOVEL     'S'           COPPV(CEVC28)
217600030124     C                   ENDIF
217700030124     C     CEVC29        IFGT      *ZEROS
217800030115     C                   Z-ADD     CEVI29        IMPV(CEVC29)
217900030214     C                   MOVEL     'S'           COPPV(CEVC29)
218000030124     C                   ENDIF
218100030124     C     CEVC30        IFGT      *ZEROS
218200030115     C                   Z-ADD     CEVI30        IMPV(CEVC30)
218300030214     C                   MOVEL     'S'           COPPV(CEVC30)
218400030124     C                   ENDIF
218500030124     C     CEVC31        IFGT      *ZEROS
218600030115     C                   Z-ADD     CEVI31        IMPV(CEVC31)
218700030214     C                   MOVEL     'S'           COPPV(CEVC31)
218800030124     C                   ENDIF
218900030124     C     CEVC32        IFGT      *ZEROS
219000030115     C                   Z-ADD     CEVI32        IMPV(CEVC32)
219100030214     C                   MOVEL     'S'           COPPV(CEVC32)
219200030124     C                   ENDIF
219300030124     C     CEVC33        IFGT      *ZEROS
219400030115     C                   Z-ADD     CEVI33        IMPV(CEVC33)
219500030214     C                   MOVEL     'S'           COPPV(CEVC33)
219600030124     C                   ENDIF
219700030124     C     CEVC34        IFGT      *ZEROS
219800030115     C                   Z-ADD     CEVI34        IMPV(CEVC34)
219900030214     C                   MOVEL     'S'           COPPV(CEVC34)
220000030124     C                   ENDIF
220100030124     C     CEVC35        IFGT      *ZEROS
220200030115     C                   Z-ADD     CEVI35        IMPV(CEVC35)
220300030214     C                   MOVEL     'S'           COPPV(CEVC35)
220400030124     C                   ENDIF
220500030124     C     CEVC36        IFGT      *ZEROS
220600030115     C                   Z-ADD     CEVI36        IMPV(CEVC36)
220700030214     C                   MOVEL     'S'           COPPV(CEVC36)
220800030124     C                   ENDIF
220900030124     C     CEVC37        IFGT      *ZEROS
221000030115     C                   Z-ADD     CEVI37        IMPV(CEVC37)
221100030214     C                   MOVEL     'S'           COPPV(CEVC37)
221200030124     C                   ENDIF
221300030124     C     CEVC38        IFGT      *ZEROS
221400030115     C                   Z-ADD     CEVI38        IMPV(CEVC38)
221500030214     C                   MOVEL     'S'           COPPV(CEVC38)
221600030124     C                   ENDIF
221700030124     C     CEVC39        IFGT      *ZEROS
221800030115     C                   Z-ADD     CEVI39        IMPV(CEVC39)
221900030214     C                   MOVEL     'S'           COPPV(CEVC39)
222000030124     C                   ENDIF
222100030124     C     CEVC40        IFGT      *ZEROS
222200030115     C                   Z-ADD     CEVI40        IMPV(CEVC40)
222300030214     C                   MOVEL     'S'           COPPV(CEVC40)
222400030124     C                   ENDIF
222500030124     C     CEVC41        IFGT      *ZEROS
222600030115     C                   Z-ADD     CEVI41        IMPV(CEVC41)
222700030214     C                   MOVEL     'S'           COPPV(CEVC41)
222800030124     C                   ENDIF
222900030124     C     CEVC42        IFGT      *ZEROS
223000030115     C                   Z-ADD     CEVI42        IMPV(CEVC42)
223100030214     C                   MOVEL     'S'           COPPV(CEVC42)
223200030124     C                   ENDIF
223300030124     C     CEVC43        IFGT      *ZEROS
223400030115     C                   Z-ADD     CEVI43        IMPV(CEVC43)
223500030214     C                   MOVEL     'S'           COPPV(CEVC43)
223600030124     C                   ENDIF
223700030124     C     CEVC44        IFGT      *ZEROS
223800030115     C                   Z-ADD     CEVI44        IMPV(CEVC44)
223900030214     C                   MOVEL     'S'           COPPV(CEVC44)
224000030124     C                   ENDIF
224100030124     C     CEVC45        IFGT      *ZEROS
224200030115     C                   Z-ADD     CEVI45        IMPV(CEVC45)
224300030214     C                   MOVEL     'S'           COPPV(CEVC45)
224400030124     C                   ENDIF
224500030124     C     CEVC46        IFGT      *ZEROS
224600030115     C                   Z-ADD     CEVI46        IMPV(CEVC46)
224700030214     C                   MOVEL     'S'           COPPV(CEVC46)
224800030124     C                   ENDIF
224900030124     C     CEVC47        IFGT      *ZEROS
225000030115     C                   Z-ADD     CEVI47        IMPV(CEVC47)
225100030214     C                   MOVEL     'S'           COPPV(CEVC47)
225200030124     C                   ENDIF
225300030124     C     CEVC48        IFGT      *ZEROS
225400030115     C                   Z-ADD     CEVI48        IMPV(CEVC48)
225500030214     C                   MOVEL     'S'           COPPV(CEVC48)
225600030124     C                   ENDIF
225700030124     C     CEVC49        IFGT      *ZEROS
225800030115     C                   Z-ADD     CEVI49        IMPV(CEVC49)
225900030214     C                   MOVEL     'S'           COPPV(CEVC49)
226000030124     C                   ENDIF
226100030124     C     CEVC50        IFGT      *ZEROS
226200030124     C                   Z-ADD     CEVI50        IMPV(CEVC50)
226300030214     C                   MOVEL     'S'           COPPV(CEVC50)
226400030124     C                   ENDIF
226500030506     c*
226600030506     c* LEGGE E CARICA (se ci sono) LE VOCI COOP ESTENSIONE
226700030509IF  1C     CEVC50        IFGT      *ZEROS
226800030507     C                   Z-ADD     D$PAAS        C2VAAS
226900030507     C                   Z-ADD     D$PLNP        C2VLNP
227000030507     C                   Z-ADD     D$PNRS        C2VNRS
227100030507     C                   Z-ADD     D$PNSP        C2VNSP
227200030507     C                   MOVEL     D$PTBL        C2VTBL
227300030507     C                   SETOFF                                       0708
227400030507     C     KEYC2V        CHAIN     ECC2V30C                           99
227500030509IF  2C     *IN99         IFEQ      *OFF
227600030512     c                   MOVEL     'S'           WC2VP
227700030509     C     C2VC51        IFGT      *ZEROS
227800030509     C                   Z-ADD     C2VI51        IMPV(C2VC51)
227900030509     C                   MOVEL     'S'           COPPV(C2VC51)
228000030509     C                   ENDIF
228100030509     C     C2VC52        IFGT      *ZEROS
228200030509     C                   Z-ADD     C2VI52        IMPV(C2VC52)
228300030509     C                   MOVEL     'S'           COPPV(C2VC52)
228400030509     C                   ENDIF
228500030509     C     C2VC53        IFGT      *ZEROS
228600030509     C                   Z-ADD     C2VI53        IMPV(C2VC53)
228700030509     C                   MOVEL     'S'           COPPV(C2VC53)
228800030509     C                   ENDIF
228900030509     C     C2VC54        IFGT      *ZEROS
229000030509     C                   Z-ADD     C2VI54        IMPV(C2VC54)
229100030509     C                   MOVEL     'S'           COPPV(C2VC54)
229200030509     C                   ENDIF
229300030509     C     C2VC55        IFGT      *ZEROS
229400030509     C                   Z-ADD     C2VI55        IMPV(C2VC55)
229500030509     C                   MOVEL     'S'           COPPV(C2VC55)
229600030509     C                   ENDIF
229700030509     C     C2VC56        IFGT      *ZEROS
229800030509     C                   Z-ADD     C2VI56        IMPV(C2VC56)
229900030509     C                   MOVEL     'S'           COPPV(C2VC56)
230000030509     C                   ENDIF
230100030509     C     C2VC57        IFGT      *ZEROS
230200030509     C                   Z-ADD     C2VI57        IMPV(C2VC57)
230300030509     C                   MOVEL     'S'           COPPV(C2VC57)
230400030509     C                   ENDIF
230500030509     C     C2VC58        IFGT      *ZEROS
230600030509     C                   Z-ADD     C2VI58        IMPV(C2VC58)
230700030509     C                   MOVEL     'S'           COPPV(C2VC58)
230800030509     C                   ENDIF
230900030509     C     C2VC59        IFGT      *ZEROS
231000030509     C                   Z-ADD     C2VI59        IMPV(C2VC59)
231100030509     C                   MOVEL     'S'           COPPV(C2VC59)
231200030509     C                   ENDIF
231300030509     C     C2VC60        IFGT      *ZEROS
231400030509     C                   Z-ADD     C2VI60        IMPV(C2VC60)
231500030509     C                   MOVEL     'S'           COPPV(C2VC60)
231600030509     C                   ENDIF
231700030509     C     C2VC61        IFGT      *ZEROS
231800030509     C                   Z-ADD     C2VI61        IMPV(C2VC61)
231900030509     C                   MOVEL     'S'           COPPV(C2VC61)
232000030509     C                   ENDIF
232100030509     C     C2VC62        IFGT      *ZEROS
232200030509     C                   Z-ADD     C2VI62        IMPV(C2VC62)
232300030509     C                   MOVEL     'S'           COPPV(C2VC62)
232400030509     C                   ENDIF
232500030509     C     C2VC63        IFGT      *ZEROS
232600030509     C                   Z-ADD     C2VI63        IMPV(C2VC63)
232700030509     C                   MOVEL     'S'           COPPV(C2VC63)
232800030509     C                   ENDIF
232900030509     C     C2VC64        IFGT      *ZEROS
233000030509     C                   Z-ADD     C2VI64        IMPV(C2VC64)
233100030509     C                   MOVEL     'S'           COPPV(C2VC64)
233200030509     C                   ENDIF
233300030509     C     C2VC65        IFGT      *ZEROS
233400030509     C                   Z-ADD     C2VI65        IMPV(C2VC65)
233500030509     C                   MOVEL     'S'           COPPV(C2VC65)
233600030509     C                   ENDIF
233700030509     C     C2VC66        IFGT      *ZEROS
233800030509     C                   Z-ADD     C2VI66        IMPV(C2VC66)
233900030509     C                   MOVEL     'S'           COPPV(C2VC66)
234000030509     C                   ENDIF
234100030509     C     C2VC67        IFGT      *ZEROS
234200030509     C                   Z-ADD     C2VI67        IMPV(C2VC67)
234300030509     C                   MOVEL     'S'           COPPV(C2VC67)
234400030509     C                   ENDIF
234500030509     C     C2VC68        IFGT      *ZEROS
234600030509     C                   Z-ADD     C2VI68        IMPV(C2VC68)
234700030509     C                   MOVEL     'S'           COPPV(C2VC68)
234800030509     C                   ENDIF
234900030509     C     C2VC69        IFGT      *ZEROS
235000030509     C                   Z-ADD     C2VI69        IMPV(C2VC69)
235100030509     C                   MOVEL     'S'           COPPV(C2VC69)
235200030509     C                   ENDIF
235300030509     C     C2VC70        IFGT      *ZEROS
235400030509     C                   Z-ADD     C2VI70        IMPV(C2VC70)
235500030509     C                   MOVEL     'S'           COPPV(C2VC70)
235600030509     C                   ENDIF
235700030509     C     C2VC71        IFGT      *ZEROS
235800030509     C                   Z-ADD     C2VI71        IMPV(C2VC71)
235900030509     C                   MOVEL     'S'           COPPV(C2VC71)
236000030509     C                   ENDIF
236100030509     C     C2VC72        IFGT      *ZEROS
236200030509     C                   Z-ADD     C2VI72        IMPV(C2VC72)
236300030509     C                   MOVEL     'S'           COPPV(C2VC72)
236400030509     C                   ENDIF
236500030509     C     C2VC73        IFGT      *ZEROS
236600030509     C                   Z-ADD     C2VI73        IMPV(C2VC73)
236700030509     C                   MOVEL     'S'           COPPV(C2VC73)
236800030509     C                   ENDIF
236900030509     C     C2VC74        IFGT      *ZEROS
237000030509     C                   Z-ADD     C2VI74        IMPV(C2VC74)
237100030509     C                   MOVEL     'S'           COPPV(C2VC74)
237200030509     C                   ENDIF
237300030509     C     C2VC75        IFGT      *ZEROS
237400030509     C                   Z-ADD     C2VI75        IMPV(C2VC75)
237500030509     C                   MOVEL     'S'           COPPV(C2VC75)
237600030509     C                   ENDIF
237700030509E   2C                   ENDIF
237800030509E   1C                   ENDIF
237900030509     C*
238000030509     C* CARICA I DATI DELLE VOCI IN DECORRENZA ALLA DATA BOLLA
238100030507     c                   XFOOT     IMPV          N213
238200030507IF  1C                   IF        N213 > *ZEROS
238300030507     C                   MOVEL     'S'           WCOPREG
238400030507     C                   EXSR      CHKVOC
238500030507     C                   Z-ADD     VCMP          CMPPV
238600030507     C                   MOVEL     VCOP          COPPV
238700030507     C                   MOVEL     VDAT          DATPV
238800030507     C                   MOVEL     VAID          AIDPV
238900030507E   1C                   ENDIF
239000030507     C*
239100030507     C                   ENDSR
239200030217     C*------------------------------------------------------------------------*
239300030217     C*? CHKVOC - CARICA I DATI DELLE VOCI IN DECORRENZA ALLA DATA BOLLA
239400030217     C*            INPUT: - WDDSP (DATA SPEDIZIONE BOLLA)
239500030217     C*            OUTPUT:- DATI VOCI IN DECORENZA ALLA DATA
239600030217     C*------------------------------------------------------------------------*
239700030217     C     CHKVOC        BEGSR
239800030217     C*
239900030217     C* CICLO PER TUTTE LE VOCI DA CALCOLARE
240000030217DO  1C     1             DO        999           V
240100030217IF  2C     COPPV(V)      IFEQ      'S'
240200030217IF  3C     WPDSP8        IFLT      VDDV(V)
240300030217     C     WPDSP8        ORGT      VDSV(V)
240400030217     C                   Z-ADD     *ZEROS        VCMP(V)
240500030217     C                   Z-ADD     *ZEROS        VDDV(V)
240600030217     C                   Z-ADD     *ZEROS        VDSV(V)
240700030217     C                   MOVEL     *BLANKS       VCOP(V)
240800030217     C                   MOVEL     *BLANKS       VDAT(V)
240900030217     C                   MOVEL     *BLANKS       VAID(V)
241000030217     C                   Z-ADD     V             EVTVOC
241100030217     C                   Z-ADD     *ZEROS        EVTPRG
241200030217     C     KEYEVT        SETLL     ECEVT01L                           99
241300030217     C  N99              READ      ECEVT01L                               99
241400030217IF  4C     *IN99         IFEQ      *ON
241500030217     C     EVTVOC        ORNE      V
241600030217     C                   MOVEL     MSG(29)       DSMSG
241700030217     C                   MOVE      V             DSVOCE
241800030217     C                   MOVEL     DSMSG         $MSG
241900030217     C                   EXSR      SCRERR
242000030217E   4C                   ENDIF
242100030217IF  4C     *IN99         DOWEQ     *OFF
242200030217     C     EVTVOC        ANDEQ     V
242300030217IF  5C     EVTATB        IFEQ      *BLANKS
242400030217     C     WPDSP8        ANDGE     EVTDDV
242500030217     C     WPDSP8        ANDLE     EVTDSV
242600030217     C                   Z-ADD     EVTCMP        VCMP(V)
242700030217     C                   Z-ADD     EVTDDV        VDDV(V)
242800030217     C                   Z-ADD     EVTDSV        VDSV(V)
242900030217     C                   MOVEL     EVTCOP        VCOP(V)
243000030217     C                   MOVEL     EVTDAT        VDAT(V)
243100030217     C                   MOVEL     EVTAID        VAID(V)
243200030217     C                   LEAVE
243300030217E   5C                   ENDIF
243400030217     C                   READ      ECEVT01L                               99
243500030217E   4C                   ENDDO
243600030217E   3C                   ENDIF
243700030217     C*
243800030217E   2C                   ENDIF
243900030217E   1C                   ENDDO
244000030217     C*
244100030217     C                   ENDSR
244200980313     C*------------------------------------------------------------------------*
244300980313     C*?CARFIM - CARICA I METRICUBI X RIFATTURAZ. ITL -PRIMA-
244400980313     C*------------------------------------------------------------------------*
244500980313     C     CARFIM        BEGSR
244600980313     C*
244700980313     C                   Z-ADD     D$PAAS        FIMAAS
244800980313     C                   Z-ADD     D$PLNP        FIMLNP
244900980313     C                   Z-ADD     D$PNRS        FIMNRS
245000980313     C                   Z-ADD     D$PNSP        FIMNSP
245100980313     C                   MOVEL     D$PTBL        FIMTBL
245200980325     C     KEYFM1        SETLL     ECFIM30C
245300980325     C     KEYFM1        READE(N)  ECFIM30C                               99
245400980313     C     *IN99         DOWEQ     *OFF
245500980313     C                   ADD       FIMSMC        WPSMCT
245600980313     C     FIMDDO        IFNE      *ZEROS
245700980313     C                   ADD       FIMSMC        WPSMC
245800980313     C                   ENDIF
245900980325     C     KEYFM1        READE(N)  ECFIM30C                               99
246000980313     C                   ENDDO
246100980313     C*
246200980313     C                   ENDSR
246300980313     C*------------------------------------------------------------------------*
246400980313     C*?CARFIC - CARICA LE COMPETENZE X RIFATTURAZ. BAR/SDI -PRIMA-
246500980313     C*------------------------------------------------------------------------*
246600011127     C*---       CARFIC    BEGSR
246700011127     C*---
246800011127     C*---                 Z-ADDD$PAAS    FICAAS           ANNO SPEDIZIONE
246900011127     C*---                 Z-ADDD$PLNP    FICLNP           LINEA DI PARTENZA
247000011127     C*---                 Z-ADDD$PNRS    FICNRS           NUMERO SERIE
247100011127     C*---                 Z-ADDD$PNSP    FICNSP           NUMERO SPEDIZIONE
247200011127     C*---                 MOVELD$PTBL    FICTBL           TIPO BOLLA
247300011127     C*---       KEYFC1    SETLLECFIC30C
247400011127     C*---       KEYFC1    READEECFIC30C            N    99
247500011127     C*---       *IN99     DOWEQ*OFF
247600011127     C*---                 ADD  FICSCO    WPSCOT           *COMP.RIF.BAR/SDI TOT
247700011127     C*---       FICDDO    IFNE *ZEROS
247800011127     C*---                 ADD  FICSCO    WPSCO            *COMP.RIFATT.BAR/SDI
247900011127     C*---                 ENDIF
248000011127     C*---       KEYFC1    READEECFIC30C            N    99
248100011127     C*---                 ENDDO
248200011127     C*---
248300011127     C*---                 ENDSR
248400941223     C********************************************************************
248500941223     C*   GESCOD - CALCOLA LE COMPETENZE BOLLA -DOPO-
248600941223     C********************************************************************
248700941223     C     GESCOD        BEGSR
248800980317     C*
248900980317     C* DETERMINA SE BOLLA BARTOLINI O SDI
249000980924     C     D$OKSC        IFNE      *ZEROS
249100980924     C     D$DDBO        ANDEQ     'S'
249200980924     C     D$DFIG        ANDEQ     'S'
249300980924     C                   MOVEL     D$OKSC        WLNKSC            3 0
249400980924     C                   ELSE
249500980924     C                   MOVEL     D$DKSC        WLNKSC
249600980924     C                   ENDIF
249700980710     C                   Z-ADD     WLNKSC        F
249800980710     C********** D$DSCL    IFEQ 'S'
249900020709     C*--- DIT(F)        IFEQ      'SDI'
250000020709     C*---               MOVEL     'SDI'         WDSCL
250100020709     C*---               ELSE
250200020709     C*---               MOVEL     'BAR'         WDSCL
250300020709     C*---               ENDIF
250400941223     C*
250500941223     C* SE NON SONO DA CALCOLARE -DOPO- NON LANCIO PGM
250600941223     C     WFCD          IFEQ      'S'
250700941223     C*
250800941223     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
250900941223     C                   CALL      'TRECD8R'                            98
251000941223     C                   PARM                    DS$ECO
251100941223     C*
251200941223     C* SE RITORNA ERRORE ESCO PGM
251300941223     C     D$GERR        IFNE      *BLANKS
251400941223     C                   MOVEL     'S'           $ERR
251500941223     C                   ENDIF
251600941223     C*
251700941223     C     D$GERR        IFEQ      *BLANKS
251800030114     c*
251900030114     C* COMPETENZE
252000941223     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
252100950125     C                   MOVEA     D$DCCD        CAL
252200950125     C                   MOVEA     D$DICD        IAL
252201160405     c                   clear                   wlogicmpd
252300030505     C     1             DO        75            I
252400941223     C                   MOVE      CAL(I)        DSWCA
252500941223     C                   Z-ADD     DSWCN         CD
252600941223     C     CD            IFNE      *ZEROS
252700941223     C                   MOVE      IAL(I)        DSWIA
252800941223     C                   Z-ADD     DSWIN         IMD(CD)
252900941223     C     CD            IFEQ      900
253000941223     C                   Z-ADD     IMD(CD)       WDIRP
253100941223     C                   ENDIF
253200941223     C     CD            IFEQ      11
253300941223     C                   Z-ADD     IMD(CD)       WDCT1
253400941223     C                   ENDIF
253500941223     C     CD            IFEQ      12
253600941223     C                   Z-ADD     IMD(CD)       WDCT2
253700941223     C                   ENDIF
253800040618     C     CD            IFEQ      204
253900040618     C                   add       IMD(CD)       WDCT1
254000040618     C                   ENDIF
254100040617     C     CD            IFEQ      205
254200040617     C                   add       IMD(CD)       WDCT2
254300040617     C                   ENDIF
254400040519     C     CD            IFEQ      53
254500040519     C                   Z-ADD     IMD(CD)       WDCT3
254600040519     C                   ENDIF
254700040617     C     CD            IFEQ      206
254800040617     C                   add       IMD(CD)       WDCT3
254900040617     C                   ENDIF
255000040519     C     CD            IFEQ      54
255100040519     C                   Z-ADD     IMD(CD)       WDCT4
255200040519     C                   ENDIF
255300040617     C     CD            IFEQ      207
255400040617     C                   add       IMD(CD)       WDCT4
255500040617     C                   ENDIF
255600040519     C     CD            IFEQ      55
255700040519     C                   Z-ADD     IMD(CD)       WDCT5
255800040519     C                   ENDIF
255900040617     C     CD            IFEQ      208
256000040617     C                   add       IMD(CD)       WDCT5
256100040617     C                   ENDIF
256200980415     C     CD            IFEQ      13
256300040618     c* obsoleta
256400040618     C*    CD            OREQ      32
256500941223     C                   Z-ADD     IMD(CD)       WDCTL
256600941223     C                   ENDIF
256700980317     C* CONTROLLA SE LA COMPETENZA E' DA RIFATTURARE ALL'ALTRO NETWORK
256800980317     C                   SELECT
256900980317     C     RLD(CD)       WHENEQ    'LA'
257000980317     C                   Z-ADD     D$DLNA        LN
257100980317     C     RLD(CD)       WHENEQ    'LP'
257200980924     C                   Z-ADD     D$DLNP        LN
257300980317     C     RLD(CD)       WHENEQ    'TA'
257400980317     C                   Z-ADD     D$DTFA        LN
257500980317     C     RLD(CD)       WHENEQ    'TP'
257600980924     C                   Z-ADD     D$DTFP        LN
257700040518     C     RLD(CD)       WHENEQ    'T1'
257800040518     C                   Z-ADD     D$DAR5flp     LN
257900040518     C     RLD(CD)       WHENEQ    'T2'
258000040518     C                   Z-ADD     D$DAR5fl2     LN
258100040518     C     RLD(CD)       WHENEQ    'T3'
258200040518     C                   Z-ADD     D$DAR5fl3     LN
258300040518     C     RLD(CD)       WHENEQ    'T4'
258400040518     C                   Z-ADD     D$DAR5fl4     LN
258500040518     C     RLD(CD)       WHENEQ    'T5'
258600040518     C                   Z-ADD     D$DAR5fl5     LN
258700980317     C                   OTHER
258800980317     C                   Z-ADD     *ZEROS        LN                3 0
258900980317     C                   ENDSL
259000990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259100990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259200990402     C* !!!!!!!!!!!!!!!! DA CANCELLARE !!!!!!!!!!!!!!!!!!!!!!!!!!
259300990402     C* DAL 1° MARZO 1999 LE DEFLUENZE ESTERO SONO A CARICO DELLA ITL
259400990402     C* E NON DEVONO APPARIRE NEL TABULATO INFRAGRUPPO SDI-BART BART-SDI
259500990402     C* QUINDI NON LE FACCIO SOMMARE ALL'IMPORTO COMPETENZE DI ECFIC00F
259600990402     C* A GIUGNO/LUGLIO DEL 2000 TOGLIERE 'TA' DAL RIFERIMENTO CENTRO DI COSTO
259700990402     C* NELLE COMPETENZE 46 E 48 E CANCELLARE QUESTA MODIFICA
259800990402     C     WDDSP8        IFGT      19990300
259900990402     C     CD            IFEQ      46
260000990402     C     CD            OREQ      48
260100990402     C                   Z-ADD     *ZEROS        LN
260200990402     C                   ENDIF
260300990402     C                   ENDIF
260301160405     c* se competenza della logistica aggiungo importo
260302160405     c                   if        lcd(cd) = 'S'
260303160405     c                   add       imd(cd)       wlogicmpd
260304160405     c                   end
261200980317     C*
261303160405     C                   ENDIF
261400941223     C                   ENDDO
261500970214     C*
261600970214     C* TOGLIE I TRANSITI AI SALDI CLIENTE
261700991027     C* ---- DA OGGI I TRANSITI SONO GIA' ESCLUSI DAL TRECD8R ------
261800991027     C******************** SUB  WDCT1     WDIRP
261900991027     C******************** SUB  WDCT2     WDIRP
262000941223     C*
262100030114     C* VOCI
262200030114     C* TRATTO CODICI E IMPORTI VOCI -DOPO-
262300030114     C                   MOVEA     D$DCVD        CAL
262400030114     C                   MOVEA     D$DIVD        IAL
262500030115     C                   MOVEA     D$DCOP        COP
262600030115     C                   MOVEA     D$DDAT        DAT
262700030115     C                   MOVEA     D$DAID        AID
262800030117     C                   MOVEA     D$DCMP        CMPV
262900030505DO  3C     1             DO        75            I
263000030114     C                   MOVE      CAL(I)        DSWCA
263100030114     C                   Z-ADD     DSWCN         CD
263200030124IF  4C     CD            IFGT      *ZEROS
263300030114     C                   MOVE      IAL(I)        DSWIA
263400030114     C                   Z-ADD     DSWIN         IMDV(CD)
263500030115     C                   MOVEL     COP(I)        COPDV(CD)
263600030115     C                   MOVEL     DAT(I)        DATDV(CD)
263700030115     C                   MOVEL     AID(I)        AIDDV(CD)
263800030117     C                   MOVE      CMPV(I)       DSWCA
263900030115     C                   Z-ADD     DSWCN         CMPDV(CD)
264000030124E   4C                   ENDIF
264100030114E   3C                   ENDDO
264200030114     C*
264300941223     C                   ENDIF
264400941223     C                   ENDIF
264500970903     C*
264600941223     C                   ENDSR
264700981012     C*------------------------------------------------------------------------*
264800981012     C*?NORVOC - NORMALIZZA COD.VOCE TOGLIENDO ZERI A SX E ALLINEANDO
264900981012     C* INPUT: WVOCE (NUMERICO)       OUTPUT: WVOCEA (ALFAB. ALL. SX)
265000981012     C*------------------------------------------------------------------------*
265100981012     C     NORVOC        BEGSR
265200981012     C*
265300981012     C                   CLEAR                   S4
265400981012     C                   CLEAR                   S4B
265500020205     C                   MOVEL     WVOCE         WVOCEA            5
265600981012     C                   MOVEA     WVOCEA        S4
265700981012     C* TOGLIE ZERI NON SIGNIFICATIVI
265800981012     C                   MOVEL     '0'           F4                1
265900981012     C                   Z-ADD     1             A4                6 0
266000981012     C     F4            DOWEQ     '0'
266100981012     C     A4            ANDLE     4
266200981012     C     S4(A4)        IFGT      *ZEROS
266300981012     C                   MOVEL     '1'           F4
266400981012     C                   SUB       1             A4
266500981012     C                   ELSE
266600981012     C                   MOVEL     *BLANKS       S4(A4)
266700981012     C                   ADD       1             A4
266800981012     C                   ENDIF
266900981012     C                   ENDDO
267000981012     C*
267100981012     C* ALLINEA IL CAMPO A SINISTRA
267200981012     C                   Z-ADD     *ZEROS        C4                6 0
267300981012     C     1             DO        4             A4
267400981012     C     S4(A4)        IFNE      *BLANKS
267500981012     C                   ADD       1             C4
267600981012     C                   MOVEL     S4(A4)        S4B(C4)
267700981012     C                   ENDIF
267800981012     C                   ENDDO
267900981012     C*
268000981012     C                   MOVEA     S4B           WVOCEA
268100981012     C*
268200981012     C                   ENDSR
268300011219     C********************************************************************
268400011219     C*?AGGMOV - AGGIORNA IL FILE ECMOV
268500011219     C********************************************************************
268600011219     C     AGGMOV        BEGSR
268700011219     C*
268800011219     C* PULISCE LE SCHIERE DI LAVORO
268900011219IF  1C     WPRGY         IFEQ      *ZEROS
269000011219     C                   CLEAR                   CDPY
269100020304     C                   CLEAR                   DRGY
269200011219     C                   CLEAR                   SVOY
269300011219     C                   CLEAR                   CDCY
269400011219     C                   CLEAR                   DAY
269500011219     C                   CLEAR                   IMPY
269600011219     C                   CLEAR                   VOCY
269700011219     C                   Z-ADD     *ZEROS        IY                6 0
269800011219     C                   Z-ADD     *ZEROS        WULTY             6 0
269900011219E   1C                   ENDIF
270000011219     C*
270100011219     C* SOLO PER 'VARIAZIONE' E 'CANCELLAZIONE' TRATTA IL -PRIMA-
270200011219IF  1C     D$GSTS        IFEQ      'U'
270300011219     C     D$GSTS        OREQ      'D'
270400011219IF  2C     D$PFRI        IFEQ      'S'
270500011219     C     WFCP          OREQ      'S'
270600011219     C                   EXSR      AGGMOP
270700011219E   2C                   ENDIF
270800011219E   1C                   ENDIF
270900011219     C*
271000011219     C* SOLO PER 'VARIAZIONE' E 'INSERIMENTO' TRATTA IL -DOPO-
271100011219     C     $ERR          IFEQ      'N'
271200011219IF  1C     D$GSTS        IFEQ      'U'
271300011219     C     D$GSTS        OREQ      'I'
271400011219IF  2C     D$DFRI        IFEQ      'S'
271500011219     C     WFCD          OREQ      'S'
271600011219     C                   EXSR      AGGMOD
271700011219E   2C                   ENDIF
271800011219E   1C                   ENDIF
271900011219     C                   ENDIF
272000011219     C*
272100011219     C* AGGIUNGO 1 BOLLA AL PROGRESSIVO BOLLE IN SCHIERA 'Y'
272200011219     C                   ADD       1             WPRGY
272300011219     C*
272400011219     C* LEGGE TUTTI GLI ELEMENTI DELLA SCHIERA E AGGIORNA ECMOV
272500011219IF  1C     $ERR          IFEQ      'N'
272600011219IF  2C     WPRGY         IFGE      MAXBOL
272700011219IF  2C     WULTY         ORGT      MAXIND
272800011219DO  3C     1             DO        WULTY         IY
272900011219IF  4C     IMPY(IY)      IFNE      *ZEROS
273000011221     C                   Z-ADD     IMPY(IY)      WIMP
273100011219     C                   EXSR      AGGECMOV
273200011219E   4C                   ENDIF
273300011219E   3C                   ENDDO
273400011219     C                   Z-ADD     *ZEROS        WPRGY             6 0
273500140516     C                   CLEAR                   CDPY
273600140516     C                   CLEAR                   DRGY
273700140516     C                   CLEAR                   SVOY
273800140516     C                   CLEAR                   CDCY
273900140516     C                   CLEAR                   DAY
274000140516     C                   CLEAR                   IMPY
274100140516     C                   CLEAR                   VOCY
274200140516     C                   Z-ADD     *ZEROS        IY
274300140516     C                   Z-ADD     *ZEROS        WULTY
274400011219E   2C                   ENDIF
274500011219E   1C                   ENDIF
274600011220     C*
274700011219     C                   ENDSR
274800011220     C*------------------------------------------------------------------------*
274900011220     C*?AGGMOP - AGGIORNA IL FILE ECMOV - GESTIONE -PRIMA-
275000011220     C*------------------------------------------------------------------------*
275100011220     C     AGGMOP        BEGSR
275200011220     C*
275300011220     C                   MOVEL     'P'           WPD
275400011220     C*
275500011220     C* RECUPERA ITALIA/ESTERO DELLA BOLLA -PRIMA-
275600011220     C                   Z-ADD     W$PLNP        F
275700011220     C                   Z-ADD     D$PLNA        F1                4 0
275800040203     c                   clear                   wiep1
275900020705IF  1C     FIE(F)        IFEQ      'E'
276000020705     C     NTW(F)        OREQ      'DPD'
276100020705     C     FIE(F1)       OREQ      'E'
276200020705     C     NTW(F1)       OREQ      'DPD'
276300011220     C                   MOVEL     'E'           WIEP              1
276400040119     c* creo un nuovo campo di comodo (WRIEP1) per spaccare ulteriormente
276500040119     c* l'estero in dpd, euroexpress, fedex
276600040119     c                   select
276700040119     c                   when       NTW(F)='DPD' or NTW(F1)='DPD'
276800040119     C                   MOVEL     'D'           WIEP1             1
276900040119     c                   when       NTW(F)='EEX' or NTW(F1)='EEX'
277000040119     C                   MOVEL     'X'           WIEP1
277100040119     c                   when       NTW(F)='FED' or NTW(F1)='FED'
277200040119     C                   MOVEL     'F'           WIEP1
277300040119     c                   endsl
277400011220X   1C                   ELSE
277500011220     C                   MOVEL     'I'           WIEP
277600011220E   1C                   ENDIF
277700040202     C                   MOVEL     WPDSP8        COMDSPP           8 0
277800040202     C                   MOVE      00            COMDSPP
277900011220     C*
278000011220     C* LEGGE TUTTE LE COMPETENZE -PRIMA- E AGGIORNA LE SCHIERE DI LAVORO
278100011220DO  1C     1             DO        WULTP         IP
278200011220     C                   Z-ADD     CDP(IP)       CP
278300011220     C*
278400011220     C* CONTROLLA DATE DECORR. E SCADENZA
278500040202IF  2C     COMDSPP       IFLT      DDP(IP)
278600040202     C     COMDSPP       ORGT      DSP(IP)
278700011220     C     PEP(IP)       OREQ      *ZEROS
278800011220     C     IMP(CP)       OREQ      *ZEROS
278900011220     C     MEP(CP)       OREQ      'N'
279000011220     C                   ITER
279100011220E   2C                   ENDIF
279200011220     C* CONTROLLA FRANCO/ASSEGNATO
279300011220IF  2C     FAP(IP)       IFNE      *BLANKS
279400011220IF  3C     FAP(IP)       IFNE      D$PTPO
279500011220     C                   ITER
279600011220E   3C                   ENDIF
279700011220E   2C                   ENDIF
279800011220     C* CONTROLLA ITALIA/ESTERO
279900011220IF  2C     IEP(IP)       IFNE      *BLANKS
280000040119     c* se italia
280100040203IF  3C     wiep          ifeq      'I'
280200040203IF  3C     IEP(IP)       andne     WIEP
280300011220     C                   ITER
280400011220E   3C                   ENDIF
280500040119     c* se suddiviso in network
280600040203IF  3C     wiep          ifeq      'E'
280700040203IF  3C     IEP(IP)       IFeq      WIEP1
280800040203     c     iep(ip)       oreq      'E'
280900040203     c                   else
281000040203     C                   ITER
281100040203E   3C                   ENDIF
281200040203E   3C                   ENDIF
281300011220E   2C                   ENDIF
281400011220     C* CONTROLLA SE E' UN RICAVO E SE E' DA SCRIVERE
281500011220IF  2C     RCP(IP)       IFEQ      'R'
281600011220     C     D$PFRI        ANDNE     'S'
281700011220     C                   ITER
281800011220E   2C                   ENDIF
281900040119     C* Imposta la DATA REGISTRAZIONE = al mese della spedizione
282000040119     c                   CLEAR                   LDRG
282100040119     c                   MOVEL     wpdsp8        LDRG
282200040119     c                   MOVE      01            LDRG
282300040119     C* Imposta la DATA COMPETENZA dei movimenti contabili a seconda della data
282400040119     C* di riferimento associata a ciascuna anagrafica competenza e comunque
282500040119     C* solo la parte AVERE
282600040119     c                   MOVEL     wpdsp8        §cdpAM
282700040119if  4c                   if        DAP(IP) = 'A'
282800040119     c* per le spedizioni con data fino al 31/12/2003
282900040119if  2c                   IF        wpdsp8 > 20011200 and wpdsp8 < 20040101
283000040119if  3c                             and FDP(IP) = '1'
283100040119     c                   MOVEL     wpdsp8        §dspAM            6 0
283200040119     c                   MOVEL     d$pdcm        §dcmAM            6 0
283300040119     c                   Z-ADD     *zeros        §cdpAM            6 0
283400040119     c                   EXSR      CALDRF
283500040119e   3c                   ENDIF
283600040119     c* per le spedizioni con data maggiore al 2004
283700040119     c                   IF        wpdsp8 >=20040101
283800040119     C                   EVAL      $TDAT = fdp(ip)
283900040119     C                   EXSR      REPDAT
284000040129     C* se data non valorizzata scrivo la competenza nel mese della data
284100040129     c* spedizione o il mese successivo a seconda se ciclo partenza oppure
284200040129     c* arrivo
284300040129     C                   IF        $dat = 0
284400040129     C                   EXSR      SRcdp
284500040129     c                   else
284600040129     C                   movel     $DAT          §cdpAM
284700040119     C                   END
284800040119     c                   endif
284900040119e   2c                   ENDIF
285000011220     C*
285100020130     C                   MOVEL     §cdpAM        DSDAT8
285200020130     C                   MOVE      00            DSDAT8
285300011220IF  2C     CFP(IP)       IFEQ      *ZEROS
285400011220     C                   MOVEL     RIP(IP)       $RIC
285500011220     C                   EXSR      REPCDC
285600011220     C                   Z-ADD     $CDC          WCCX              7 0
285700011220X   XC                   ELSE
285800011220     C                   Z-ADD     CFP(IP)       WCCX
285900011220E   2C                   ENDIF
286000090915     C     IMP(CP)       DIV       100           WVVOCE           17 7
286100090915     C     WVVOCE        MULT      PEP(IP)       WIMY             19 5
286200011220     C*
286300011220     C* CONTROLLO SE ELEMENTO GIA' PRESENTE NELLA SCHIERA 'Y'
286400011220     C                   MOVEL     'N'           WFTROV            1
286500011220IF  2C     WULTY         IFGT      *ZEROS
286600011220     C                   Z-ADD     1             I
286700011220DO  3C     I             DOWLE     WULTY
286800011220     C     WFTROV        ANDEQ     'N'
286900011220     C*
287000011220     C* SE TUTTI I DATI 'CHIAVE' SONO UGUALI AGGIUNGO IMPORTO
287100020205     c                   MOVEL     KCP(IP)       WVOCE             4 0
287200020104     C                   EXSR      NORVOC
287300020104     C                   MOVEL     WVOCEA        LVOC
287400020102     c                   MOVEL     WCCX          A7                7
287500020102     c                   EVAL      LCDC = A7
287600011220IF  4C     DSDAT8        IFEQ      CDPY(I)
287700011220     C     LVOC          ANDEQ     VOCY(I)
287800011220     C     KSP(IP)       ANDEQ     SVOY(I)
287900011220     C     LCDC          ANDEQ     CDCY(I)
288000011220     C     DAP(IP)       ANDEQ     DAY(I)
288100020304     C     LDRG          ANDEQ     DRGY(I)
288200020103     C                   SUB       WIMY          IMPY(I)
288300011220     C                   MOVEL     'S'           WFTROV
288400011220E   4C                   ENDIF
288500011220     C*
288600011220     C                   ADD       1             I
288700011220E   3C                   ENDDO
288800011220E   2C                   ENDIF
288900011220     C*
289000011220     C* NON TROVATO NELLA SCHIERA 'Y'
289100011220IF  2C     WFTROV        IFEQ      'N'
289200011220     C                   ADD       1             IY
289300011220     C                   Z-ADD     IY            WULTY
289400011220     C                   Z-ADD     DSDAT8        CDPY(IY)
289500020304     C                   Z-ADD     LDRG          DRGY(IY)
289600020104     c                   MOVEL     KCP(IP)       WVOCE
289700020104     C                   EXSR      NORVOC
289800020104     C                   MOVEL     WVOCEA        VOCY(IY)
289900011220     C                   Z-ADD     KSP(IP)       SVOY(IY)
290000020102     C                   MOVEL     WCCX          A7
290100020102     C                   EVAL      CDCY(IY) = A7
290200011220     C                   MOVEL     DAP(IP)       DAY(IY)
290300020103     C                   Z-SUB     WIMY          IMPY(IY)
290400011220E   2C                   ENDIF
290500011220     C*
290600011220E   1C                   ENDDO
290700011220     C*
290800011220     C                   ENDSR
290900011220     C*------------------------------------------------------------------------*
291000011220     C*?AGGMOD - AGGIORNA IL FILE ECMOV - GESTIONE -DOPO-
291100011220     C*------------------------------------------------------------------------*
291200011220     C     AGGMOD        BEGSR
291300011220     C*
291400011220     C                   MOVEL     'D'           WPD
291500011220     C*
291600011220     C* RECUPERA ITALIA/ESTERO DELLA BOLLA -DOPO-
291700011220     C                   Z-ADD     W$DLNP        F
291800011220     C                   Z-ADD     D$DLNA        F1
291900040203     c                   clear                   wied1
292000020705IF  1C     FIE(F)        IFEQ      'E'
292100020705     C     NTW(F)        OREQ      'DPD'
292200020705     C     FIE(F1)       OREQ      'E'
292300020705     C     NTW(F1)       OREQ      'DPD'
292400011220     C                   MOVEL     'E'           WIED              1
292500040119     c* creo un nuovo campo di comodo (WIED1) per spaccare ulteriormente
292600040119     c* l'estero in dpd, euroexpress, fedex
292700040119     c                   select
292800040119     c                   when       NTW(F)='DPD' or NTW(F1)='DPD'
292900040119     C                   MOVEL     'D'           WIEd1             1
293000040119     c                   when       NTW(F)='EEX' or NTW(F1)='EEX'
293100040119     C                   MOVEL     'X'           WIEd1
293200040119     c                   when       NTW(F)='FED' or NTW(F1)='FED'
293300040119     C                   MOVEL     'F'           WIEd1
293400040119     c                   endsl
293500011220X   1C                   ELSE
293600011220     C                   MOVEL     'I'           WIED
293700011220E   1C                   ENDIF
293800040202     C                   MOVEL     WDDSP8        COMDSPD           8 0
293900040202     C                   MOVE      00            COMDSPD
294000011220     C*
294100011220     C* LEGGE TUTTE LE COMPETENZE -DOPO- E AGGIORNA LE SCHIERE DI LAVORO
294200011220DO  1C     1             DO        WULTD         ID
294300011220     C                   Z-ADD     CDD(ID)       CD
294400011220     C*
294500011220     C* CONTROLLA DATE DECORR. E SCADENZA
294600040202IF  2C     COMDSPD       IFLT      DDD(ID)
294700040202     C     COMDSPD       ORGT      DSD(ID)
294800011220     C     PED(ID)       OREQ      *ZEROS
294900011220     C     IMD(CD)       OREQ      *ZEROS
295000011220     C     MED(CD)       OREQ      'N'
295100011220     C                   ITER
295200011220E   2C                   ENDIF
295300011220     C* CONTROLLA FRANCO/ASSEGNATO
295400011220IF  2C     FAD(ID)       IFNE      *BLANKS
295500011220IF  3C     FAD(ID)       IFNE      D$DTPO
295600011220     C                   ITER
295700011220E   3C                   ENDIF
295800011220E   2C                   ENDIF
295900011220     C* CONTROLLA ITALIA/ESTERO
296000011220IF  2C     IED(ID)       IFNE      *BLANKS
296100040119     c* se italia
296200040203IF  3C     wied          ifeq      'I'
296300040203IF  3C     IEd(Id)       andne     WIEd
296400040203     C                   ITER
296500040203E   3C                   ENDIF
296600040203     c* se suddiviso in network
296700040203IF  3C     wied          ifeq      'E'
296800040203IF  3C     IEd(Id)       IFeq      WIEd1
296900040203     c     ied(id)       oreq      'E'
297000040203     c                   else
297100040203     C                   ITER
297200040203E   3C                   ENDIF
297300040203E   3C                   ENDIF
297400011220E   2C                   ENDIF
297500011220     C* CONTROLLA SE E' UN RICAVO E SE E' DA SCRIVERE
297600011220IF  2C     RCD(CD)       IFEQ      'R'
297700011220     C     D$DFRI        ANDNE     'S'
297800011220     C                   ITER
297900011220E   2C                   ENDIF
298000040119     C* Imposta la DATA REGISTRAZIONE = al mese della spedizione
298100040119     c                   CLEAR                   LDRG
298200040119     c                   MOVEL     wddsp8        LDRG
298300040119     c                   MOVE      01            LDRG
298400040119     C* Imposta la DATA COMPETENZA dei movimenti contabili a seconda della data
298500040119     C* di riferimento associata a ciascuna anagrafica comeptenza e comunque
298600040119     C* solo la parte AVERE
298700040119     c                   MOVEL     wddsp8        §cdpAM
298800040119if  4c                   if        DAD(ID) = 'A'
298900040119     c* per le spedizioni con data fino al 31/12/2003
299000040119if  2c                   IF        wddsp8 > 20011200 and wddsp8 < 20040101
299100040119if  3c                             and FDD(ID) = '1'
299200040119     c                   MOVEL     wddsp8        §dspAM            6 0
299300040119     c                   MOVEL     d$ddcm        §dcmAM            6 0
299400040119     c                   Z-ADD     *zeros        §cdpAM            6 0
299500040119     c                   EXSR      CALDRF
299600040119e   3c                   ENDIF
299700040119     c* per le spedizioni con data maggiore al 2004
299800040119     c                   if        wddsp8 >= 20040101
299900040119     C                   EVAL      $TDAT = fdd(id)
300000040119     C                   EXSR      REPDAT
300100040129     C* se data non valorizzata scrivo la competenza nel mese della data
300200040129     c* spedizione o il mese successivo a seconda se ciclo partenza oppure
300300040129     c* arrivo
300400040129     C                   IF        $dat = 0
300500040129     C                   EXSR      SRcdp
300600040129     c                   else
300700040129     C                   movel     $DAT          §cdpAM
300800040119     C                   END
300900040119     c                   end
301000040119e   2c                   ENDIF
301100011220     C*
301200020130     C                   MOVEL     §cdpAM        DSDAT8
301300020130     C                   MOVE      00            DSDAT8
301400011220IF  2C     CFD(ID)       IFEQ      *ZEROS
301500011220     C                   MOVEL     RID(ID)       $RIC
301600011220     C                   EXSR      REPCDC
301700011220     C                   Z-ADD     $CDC          WCCX
301800011220X   2C                   ELSE
301900011220     C                   Z-ADD     CFD(ID)       WCCX
302000011220E   2C                   ENDIF
302100011220     C     IMD(CD)       DIV       100           WVVOCE
302200020103     C     WVVOCE        MULT      PED(ID)       WIMY
302300011220     C*
302400011221     C* CONTROLLO SE ELEMENTO GIA' PRESENTE NELLA SCHIERA 'Y'
302500011220     C                   MOVEL     'N'           WFTROV
302600011220IF  2C     WULTY         IFGT      *ZEROS
302700011220     C                   Z-ADD     1             I
302800011220DO  3C     I             DOWLE     WULTY
302900011220     C     WFTROV        ANDEQ     'N'
303000020104     c                   MOVEL     KCD(ID)       WVOCE
303100020104     C                   EXSR      NORVOC
303200020104     C                   MOVEL     WVOCEA        LVOC
303300020104     c                   MOVEL     WCCX          A7
303400020104     c                   EVAL      LCDC = A7
303500011220IF  4C     DSDAT8        IFEQ      CDPY(I)
303600011220     C     LVOC          ANDEQ     VOCY(I)
303700020103     C     KSD(ID)       ANDEQ     SVOY(I)
303800011220     C     LCDC          ANDEQ     CDCY(I)
303900020103     C     DAD(ID)       ANDEQ     DAY(I)
304000020304     C     LDRG          ANDEQ     DRGY(I)
304100020103     C                   ADD       WIMY          IMPY(I)
304200011220     C                   MOVEL     'S'           WFTROV
304300011220E   4C                   ENDIF
304400011220     C*
304500011220     C                   ADD       1             I
304600011220E   3C                   ENDDO
304700011220E   2C                   ENDIF
304800011220     C*
304900011220     C* NON TROVATO NELLA SCHIERA 'Y'
305000011220IF  2C     WFTROV        IFEQ      'N'
305100011220     C                   ADD       1             IY
305200011220     C                   Z-ADD     IY            WULTY
305300011220     C                   Z-ADD     DSDAT8        CDPY(IY)
305400020304     C                   Z-ADD     LDRG          DRGY(IY)
305500020104     c                   MOVEL     KCD(ID)       WVOCE
305600020104     C                   EXSR      NORVOC
305700020104     C                   MOVEL     WVOCEA        VOCY(IY)
305800020103     C                   Z-ADD     KSD(ID)       SVOY(IY)
305900020102     C                   MOVEL     WCCX          A7
306000020102     C                   EVAL      CDCY(IY) = A7
306100020103     C                   MOVEL     DAD(ID)       DAY(IY)
306200020104     C                   Z-ADD     WIMY          IMPY(IY)
306300011220E   2C                   ENDIF
306400011220     C*
306500011220E   1C                   ENDDO
306600011220     C*
306700011220     C                   ENDSR
306800020130     C*------------------------------------------------------------------------*
306900020130     C*?CALDRF - CALCOLA LA DATA RIFERIMENTO PER IMPOSTARE LA DATA COMPETENZA
307000020130     c*        Input : §dspAM -> Anno/Mese spedizione
307100020130     c*                §dcmAM -> Anno/Mese consegna
307200020130     c*        Output: §cdpAM -> Anno/Mese competenza
307300020130     C*------------------------------------------------------------------------*
307400020130     C     caldrf        BEGSR
307500020130     c*---
307600020130     c* DSP =  DCM -> mese competenza = mese SPEDIZIONE
307700020130     c* DSP <> DCM -> mese competenza = mese SUCCESSIVO al mese SPEDIZIONE
307800020130     c*---
307900020130     c*
308000020130if  1C     §dspAM        IFEQ      §dcmAM
308100020130     C                   Z-ADD     §dspAM        §cdpAM
308200020130X   1c                   ELSE
308300020130     C                   MOVEL     §dspAM        n8                8 0
308400020130     c                   MOVE      01            n8
308500020130     c     *ISO          MOVE      n8            wdata
308600020130     c                   ADDDUR    1:*M          wdata
308700020130     c     *ISO          MOVE      wdata         n8
308800020130     c                   MOVEL     n8            §cdpAM
308900020130e   1C                   ENDIF
309000020130     c*
309100020130     C                   ENDSR
309200011219     C*------------------------------------------------------------------------*
309300011219     C*?AGGECMOV - AGGIORNA IL FILE ECMOV01L (MOVIMENTI CONTABILI)
309400011219     C*------------------------------------------------------------------------*
309500011219     C     AGGECMOV      BEGSR
309600011219     C*
309700011219     C* SOLO SE FLAG AGG. CONTO ECON. = SI
309800140321IF  0C*    D$GWFM        IFEQ      'S'
309900011219     C*
310000011219     C* CONTROLLO SE RECORD GIA' ESISTENTE
310100011219     C                   MOVEL     '201'         MOVSOC
310200011219     C                   Z-ADD     CDPY(IY)      MOVCDP
310300011219     C                   Z-ADD     SVOY(IY)      MOVSVO
310400011219     C                   MOVEL     CDCY(IY)      MOVCDC
310500011219     C                   MOVEL     DAY(IY)       MOVDA
310600020304     C                   Z-ADD     DRGY(IY)      MOVDRG
310700011219     C     KEYMOV        CHAIN     ECMOV01L                           99
310800011219IF  1C     *IN99         IFEQ      *OFF
310900011219     C                   EXSR      ADDECMOV
311000011219     C*
311100011219     C* SE L'IMPORTO DA AGGIORNARE E' A ZERO CANCELLO
311200011219IF  2C     MOVIMP        IFEQ      *ZERO
311300011219     C                   DELETE    ECMOV000
311400011219X   2C                   ELSE
311500011219     C                   UPDATE    ECMOV000
311600011219E   2C                   ENDIF
311700011219     C*
311800011219X   1C                   ELSE
311900011219     C*
312000011219     C                   EXSR      FISECMOV
312100011219     C                   EXSR      ADDECMOV
312200011219     C                   WRITE     ECMOV000
312300011219E   1C                   ENDIF
312400140321E   0C*                  ENDIF
312500011219     C*
312600011219     C                   ENDSR
312700011221     C*------------------------------------------------------------------------*
312800011221     C*?ADDECMOV - AGGIUNGE AI VALORI DEL FILE ECMOV01L (MOVIMENTI CONTABILI)
312900011221     C*------------------------------------------------------------------------*
313000011221     C     ADDECMOV      BEGSR
313100011221     C*
313200011221     C                   Z-ADD     MOVCDP        DSDAT8
313300011221     C                   Z-ADD     1             DSGIO
313400020304     C*---               Z-ADD     DSDAT8        MOVDRG
313500090916     C                   z-add(h)  WIMP          wimp£
313600090916     C                   ADD       WIMP£         MOVIMP
313700011221     C                   Z-ADD     DATCOR        MOVDUV
313800011221     C*
313900011221     C* SE L'IMPORTO E' POSITIVO SIGNIFICA CHE STO SCRIVENDO UN RECORD DI TIPO
314000020205     C* DOPO E CHE QUINDI DEVO ANCHE AGGIORNARE LA VOCE PROJ
314100011221IF  1C     WIMP          IFGT      *ZEROS
314200011221     C                   MOVEL     VOCY(IY)      MOVVOC
314300011221E   1C                   ENDIF
314400011221     C*
314500011221     C                   ENDSR
314600011221     C*------------------------------------------------------------------------*
314700011221     C*?FISECMOV - VALORIZZA I CAMPI FISSI DI ECMOV01L
314800011221     C*------------------------------------------------------------------------*
314900011221     C     FISECMOV      BEGSR
315000011221     C*
315100011221      *
315200011221     c                   EVAL      movsoc = '201'
315300011221     c                   EVAL      movtmo = 'G'
315400011221     c                   EVAL      movcot = 'COMP'
315500011221     c                   EVAL      movctb = 'CE'
315600011221     c                   Z-ADD     123456        movnrg
315700011221     c                   EVAL      movdiv = wdivce
315800011221     c                   EVAL      movdes = 'COMP.CE'
315900011221     c                   EVAL      movkcc = *blanks
316000011221     c                   EVAL      movksc = *blanks
316100011221     c                   EVAL      moven2 = *blanks
316200011221     c                   EVAL      moven3 = *blanks
316300020107     C                   Z-ADD     *ZEROS        movimp
316400011221     C*
316500011221     C                   ENDSR
316600941215     C*------------------------------------------------------------------------*
316700941215     C*?REPCDC - REPERISCE IL CENTRO DI COSTO
316800941215     C*          INPUT:  $RIC --> RIFERIMENTO CENTRO DI COSTO
316900941215     C*          OUTPUT: $CDC --> CENTRO DI COSTO
317000941215     C*------------------------------------------------------------------------*
317100941215     C     REPCDC        BEGSR
317200941215     C*
317300941215     C                   Z-ADD     *ZEROS        $FIL
317400941215     C                   Z-ADD     *ZEROS        $FIL5             5 0
317500941215     C*
317600941215     C* I CENTRI DI COSTO DI FILIALE SONO FORMATI IN QUESTO MODO:
317700941215     C*   - 2 ZERI + CODICE FILIALE + 2 ZERI
317800941215     C*     ES. BOLOGNA = 00+001+00 = 0000100
317900941215     C*
318000941215     C* SE E' STATO IMMESSO IL RIFERIMENTO CENTRO DI COSTO
318100941215IF  1C     $RIC          IFGT      *BLANKS
318200941215     C*
318300941215     C* CONTROLLA SE IL RIFERIMENTO ESISTE IN TABELLA
318400941215     C                   Z-ADD     1             I
318500941215     C                   SETOFF                                           99
318600941215     C     $RIC          LOOKUP    RIC(I)                                 99
318700941215IF  2C     *IN99         IFEQ      *OFF
318800941215     C                   MOVEL     MSG(3)        $MSG
318900941215     C                   EXSR      SCRERR
319000941215X   2C                   ELSE
319100941215     C*
319200941215     C* VEDO QUALE DEI CAMPI DELLA BOLLA DEVO CONSIDERARE COME C.D.C.
319300941215IF  3C     WPD           IFEQ      'P'
319400941215SL  4C                   SELECT
319500941215     C     $RIC          WHENEQ    'LA'
319600941215     C                   MOVE      D$PLNA        $FIL
319700941215     C     $RIC          WHENEQ    'LP'
319800941215     C                   MOVE      W$PLNP        $FIL
319900941215     C     $RIC          WHENEQ    'TA'
320000941215     C                   MOVE      D$PTFA        $FIL
320100941215     C     $RIC          WHENEQ    'TP'
320200941215     C                   MOVE      W$PTFP        $FIL
320300040518     C     $RIC          WHENEQ    'T1'
320400040518     C                   MOVE      D$par5flp     $FIL
320500040518     C     $RIC          WHENEQ    'T2'
320600040518     C                   MOVE      D$Par5fl2     $FIL
320700040518     C     $RIC          WHENEQ    'T3'
320800040518     C                   MOVE      D$Par5fl3     $FIL
320900040518     C     $RIC          WHENEQ    'T4'
321000040518     C                   MOVE      D$Par5fl4     $FIL
321100040518     C     $RIC          WHENEQ    'T5'
321200040518     C                   MOVE      D$Par5fl5     $FIL
321300941215E   4C                   ENDSL
321400941215X   3C                   ELSE
321500941215SL  4C                   SELECT
321600941215     C     $RIC          WHENEQ    'LA'
321700941215     C                   MOVE      D$DLNA        $FIL
321800941215     C     $RIC          WHENEQ    'LP'
321900941215     C                   MOVE      W$DLNP        $FIL
322000941215     C     $RIC          WHENEQ    'TA'
322100941215     C                   MOVE      D$DTFA        $FIL
322200941215     C     $RIC          WHENEQ    'TP'
322300941215     C                   MOVE      W$DTFP        $FIL
322400040518     C     $RIC          WHENEQ    'T1'
322500040518     C                   MOVE      D$Dar5flp     $FIL
322600040518     C     $RIC          WHENEQ    'T2'
322700040518     C                   MOVE      D$Dar5fl2     $FIL
322800040518     C     $RIC          WHENEQ    'T3'
322900040518     C                   MOVE      D$Dar5fl3     $FIL
323000040518     C     $RIC          WHENEQ    'T4'
323100040518     C                   MOVE      D$Dar5fl4     $FIL
323200040518     C     $RIC          WHENEQ    'T5'
323300040518     C                   MOVE      D$Dar5fl5     $FIL
323400941215E   4C                   ENDSL
323500941215E   3C                   ENDIF
323600941215     C*
323700941215     C                   MOVEL     $FIL          $FIL5
323800941215     C                   MOVE      $FIL5         $CDC
323900941215     C*
324000941215E   2C                   ENDIF
324100941215E   1C                   ENDIF
324200941215     C*
324300941215     C                   ENDSR
324400030120     C*------------------------------------------------------------------------*
324500030120     C*?REPDAT - REPERISCE LA DATA DI RIFERIMENTO C/E COOP
324600030120     c*          INPUT:  $TDAT   -> codice data
324700030120     C*          OUTPUT: $DAT   -->   data di riferimento
324800030120     C*------------------------------------------------------------------------*
324900030120     C     REPDAT        BEGSR
325000030120     c*
325100030120     c                   Z-ADD     *ZEROS        $DAT
325200030120     c* -PRIMA-
325300030120IF  1C     WPD           IFEQ      'P'
325400030120SL  2C                   SELECT
325500030120     C     $TDAT         WHENEQ    ' '
325600030120     C                   Z-ADD     D$PAAS        DSAAS
325700030120     C                   Z-ADD     D$PMGS        DSMGS
325800030120     C                   Z-ADD     DSDAT8        $DAT
325900030120     C     $TDAT         WHENEQ    'A'
326000030120     C                   Z-ADD     D$PDTI        $DAT
326100030120     C     $TDAT         WHENEQ    'C'
326200030120     C                   Z-ADD     D$PDCM        $DAT
326300040130     C                   WHEN      $TDAT = 'D'
326400030120     C                   Z-ADD     D$PDDC        $DAT
326500030120     C     $TDAT         WHENEQ    'G'
326600030120     C                   Z-ADD     D$PAGC        DSAAS
326700030120     C                   Z-ADD     D$PMGC        DSMGS
326800030120     C                   Z-ADD     DSDAT8        $DAT
326900030120     C     $TDAT         WHENEQ    'P'
327000030120     C                   Z-ADD     D$PDUC        $DAT
327100030120     C     $TDAT         WHENEQ    'R'
327200030120     C                   Z-ADD     D$PDRT        $DAT
327300040427     c* da maggio 2004 invertite le date xchè erano errate
327400030120     C     $TDAT         WHENEQ    'Y'
327500030120     C                   Z-ADD     D$PAR5DET     $DAT
327600040427     c                   if        wpdsp8 >=20040501
327700040427     C                   Z-ADD     D$PAR5DUT     $DAT
327800040427     c                   end
327900030120     C     $TDAT         WHENEQ    'X'
328000030120     C                   Z-ADD     D$PAR5DUT     $DAT
328100040427     c                   if        wpdsp8 >=20040501
328200040427     C                   Z-ADD     D$PAR5DET     $DAT
328300040427     c                   end
328400030120     C     $TDAT         WHENEQ    'Z'
328500030120     C                   Z-ADD     D$PAR5DE2     $DAT
328600040427     c                   if        wpdsp8 >=20040501
328700040427     C                   Z-ADD     D$PAR5DU2     $DAT
328800040427     c                   end
328900030120     C     $TDAT         WHENEQ    'W'
329000030120     C                   Z-ADD     D$PAR5DU2     $DAT
329100040427     c                   if        wpdsp8 >=20040501
329200040427     C                   Z-ADD     D$PAR5DE2     $DAT
329300040427     c                   end
329400040518     C     $TDAT         WHENEQ    'H'
329500040518     C                   Z-ADD     D$PAR5DE3     $DAT
329600040518     C     $TDAT         WHENEQ    'I'
329700040518     C                   Z-ADD     D$PAR5DU3     $DAT
329800040518     C     $TDAT         WHENEQ    'L'
329900040518     C                   Z-ADD     D$PAR5DE4     $DAT
330000040518     C     $TDAT         WHENEQ    'M'
330100040518     C                   Z-ADD     D$PAR5DU4     $DAT
330200040518     C     $TDAT         WHENEQ    'N'
330300040518     C                   Z-ADD     D$PAR5DE5     $DAT
330400040518     C     $TDAT         WHENEQ    'O'
330500040518     C                   Z-ADD     D$PAR5DU5     $DAT
330600030120E   2C                   ENDSL
330700030120     c*
330800030120     c* -DOPO-
330900030120X   1C                   ELSE
331000030120SL  2C                   SELECT
331100030120     C     $TDAT         WHENEQ    ' '
331200030120     C                   Z-ADD     D$DAAS        DSAAS
331300030120     C                   Z-ADD     D$DMGS        DSMGS
331400030120     C                   Z-ADD     DSDAT8        $DAT
331500030120     C     $TDAT         WHENEQ    'A'
331600030120     C                   Z-ADD     D$DDTI        $DAT
331700030120     C     $TDAT         WHENEQ    'C'
331800030120     C                   Z-ADD     D$DDCM        $DAT
331900040130     C                   WHEN      $TDAT = 'D'
332000030120     C                   Z-ADD     D$DDDC        $DAT
332100030120     C     $TDAT         WHENEQ    'G'
332200030120     C                   Z-ADD     D$DAGC        DSAAS
332300030120     C                   Z-ADD     D$DMGC        DSMGS
332400030120     C                   Z-ADD     DSDAT8        $DAT
332500030120     C     $TDAT         WHENEQ    'P'
332600030120     C                   Z-ADD     D$DDUC        $DAT
332700030120     C     $TDAT         WHENEQ    'R'
332800030120     C                   Z-ADD     D$DDRT        $DAT
332900040427     c* da maggio 2004 invertite le date xchè erano errate
333000030120     C     $TDAT         WHENEQ    'Y'
333100030120     C                   Z-ADD     D$DAR5DET     $DAT
333200040427     c                   if        wddsp8 >=20040501
333300040427     C                   Z-ADD     D$DAR5DUT     $DAT
333400040427     c                   end
333500030120     C     $TDAT         WHENEQ    'X'
333600030120     C                   Z-ADD     D$DAR5DUT     $DAT
333700040427     c                   if        wddsp8 >=20040501
333800040427     C                   Z-ADD     D$DAR5DET     $DAT
333900040427     c                   end
334000030120     C     $TDAT         WHENEQ    'Z'
334100030120     C                   Z-ADD     D$DAR5DE2     $DAT
334200040427     c                   if        wddsp8 >=20040501
334300040427     C                   Z-ADD     D$DAR5DU2     $DAT
334400040427     c                   end
334500030120     C     $TDAT         WHENEQ    'W'
334600030120     C                   Z-ADD     D$DAR5DU2     $DAT
334700040427     c                   if        wddsp8 >=20040501
334800040427     C                   Z-ADD     D$DAR5DE2     $DAT
334900040427     c                   end
335000040518     C     $TDAT         WHENEQ    'H'
335100040518     C                   Z-ADD     D$dAR5DE3     $DAT
335200040518     C     $TDAT         WHENEQ    'I'
335300040518     C                   Z-ADD     D$dAR5DU3     $DAT
335400040518     C     $TDAT         WHENEQ    'L'
335500040518     C                   Z-ADD     D$dAR5DE4     $DAT
335600040518     C     $TDAT         WHENEQ    'M'
335700040518     C                   Z-ADD     D$dAR5DU4     $DAT
335800040518     C     $TDAT         WHENEQ    'N'
335900040518     C                   Z-ADD     D$dAR5DE5     $DAT
336000040518     C     $TDAT         WHENEQ    'O'
336100040518     C                   Z-ADD     D$dAR5DU5     $DAT
336200030120E   2C                   ENDSL
336300030120E   1C                   ENDIF
336400030120     c*
336500030120     C                   ENDSR
336600941215     C*------------------------------------------------------------------------*
336700950316     C*?AGGCED - AGGIORNA IL FILE ECCED30C (COMPETENZE DETTAGLIO)
336800941215     C*------------------------------------------------------------------------*
336900941215     C     AGGCED        BEGSR
337000941215     C*
337100941215     C                   MOVEL     'N'           WDET              1
337200941215     C*
337300941215     C* SOLO SE FLAG AGG. CONTO ECON. = SI
337400970507IF -1C     D$GCET        IFEQ      'S'
337500970507     C*
337600970507     C* SE DELETE CANCELLO TUTTI I RECORD DELLA BOLLA IN ECCED
337700970507IF  0C     D$GSTS        IFEQ      'D'
337800970507     C*
337900970507     C                   Z-ADD     D$PAAS        CEDAAS
338000970507     C                   Z-ADD     D$PLNP        CEDLNP
338100970507     C                   Z-ADD     D$PNRS        CEDNRS
338200970507     C                   Z-ADD     D$PNSP        CEDNSP
338300970507     C                   MOVEL     D$PTBL        CEDTBL
338400970507     C     KEYCEP        SETLL     ECCED30C
338500970507     C     KEYCEP        READE     ECCED30C                               99
338600970507     C     *IN99         DOWEQ     *OFF
338700010828     C                   DELETE    ECCED000
338800970507     C     KEYCEP        READE     ECCED30C                               99
338900970507     C                   ENDDO
339000970507     C*
339100970507X   0C                   ELSE
339200941215     C*
339300941215     C* CICLO PER TUTTE LE COMPETENZE
339400941215DO  1C     1             DO        999           CD
339500941215     C*
339600941215     C     IMP(CD)       IFEQ      *ZEROS
339700941215     C     IMD(CD)       ANDEQ     *ZEROS
339800941215     C                   ITER
339900941215     C                   ENDIF
340000941215     C*
340100941215     C* SE NON E' UNA COMPETENZA DI TESTATA
340200941215IF  2C     CD            IFEQ      1
340300941215     C     CD            OREQ      2
340400941215     C     CD            OREQ      3
340500941215     C     CD            OREQ      4
340600941215     C     CD            OREQ      5
340700941215     C     CD            OREQ      6
340800941215     C     CD            OREQ      7
340900941215     C     CD            OREQ      8
341000950315     C     CD            OREQ      9
341100950315     C     CD            OREQ      10
341200941215     C     CD            OREQ      13
341300950315     C     CD            OREQ      14
341400950315     C     CD            OREQ      15
341500941215     C     CD            OREQ      29
341600941215     C     CD            OREQ      26
341700941215     C     CD            OREQ      25
341800941215     C     CD            OREQ      27
341900010827     C     CD            OREQ      33
342000941215X   2C                   ELSE
342100941215     C*
342200941215     C* E' UNA COMPETENZA VALIDA E DA SCRIVERE 'PRIMA'
342300941215IF  3C     IMP(CD)       IFNE      *ZEROS
342400941215     C     MEP(CD)       ANDEQ     'S'
342500941223     C     RCP(CD)       ANDEQ     'C'
342600950104     C     WFCP          ANDEQ     'S'
342700950104     C                   MOVEL     'S'           WFCEDP            1
342800941215X   3C                   ELSE
342900941215     C                   MOVEL     'N'           WFCEDP
343000941215E   3C                   ENDIF
343100941215     C*
343200941215     C* E' UNA COMPETENZA VALIDA E DA SCRIVERE 'DOPO'
343300941215IF  3C     IMD(CD)       IFNE      *ZEROS
343400941215     C     MED(CD)       ANDEQ     'S'
343500941223     C     RCD(CD)       ANDEQ     'C'
343600950104     C     WFCD          ANDEQ     'S'
343700941215     C                   MOVEL     'S'           WFCEDD            1
343800941215     C                   MOVEL     'S'           WDET
343900941215X   3C                   ELSE
344000941215     C                   MOVEL     'N'           WFCEDD
344100941215E   3C                   ENDIF
344200941215     C*
344300941215     C* CODICE COMPETENZA
344400941215     C                   Z-ADD     CD            CEDCMP
344500941215     C*
344600941215     C* SE PRIMA ESISTEVA ED ERA DA MOVIMENTARE IN C.E.
344700941215     C* E DOPO NON E' PIU' DA SCRIVERE, LO CANCELLO
344800020805     C     WFCEDP        IFEQ      'S'
344900020805     C     WFCEDD        ANDEQ     'N'
345000941215     C                   EXSR      DELCED
345100020805     C                   ENDIF
345200941215     C*
345300941215     C* SE PRIMA ESISTEVA ED ERA DA MOVIMENTARE IN C.E.
345400941215     C* E DOPO E' DA SCRIVERE CON IMPORTO DIVERSO, RISCRIVO
345500020805     C     WFCEDP        IFEQ      'S'
345600020805     C     WFCEDD        ANDEQ     'S'
345700941215     C     D$PTBL        IFEQ      D$DTBL
345800041110     C     IMP(CD)       ANDNE     IMD(CD)
345900941215     C                   EXSR      UPDCED
346000041110     C                   End
346100041110     C     D$PTBL        IFne      D$DTBL
346200941215     C                   EXSR      DELCED
346300941215     C                   EXSR      INSCED
346400941215     C                   ENDIF
346500941215     C                   ENDIF
346600950217     C*
346700941215     C* SE PRIMA NON ESISTEVA E DOPO E' DA SCRIVERE, SCRIVO
346800941215     C     WFCEDP        IFEQ      'N'
346900941215     C     WFCEDD        ANDEQ     'S'
347000030131     C*
347100030131     C* SE ESISTE, CMQ CANCELLA COMPETENZA
347200030131     C                   Z-ADD     D$DAAS        CEDAAS
347300030131     C                   Z-ADD     D$DLNP        CEDLNP
347400030131     C                   Z-ADD     D$DNRS        CEDNRS
347500030131     C                   Z-ADD     D$DNSP        CEDNSP
347600030131     C                   MOVEL     D$DTBL        CEDTBL
347700030131     C                   SETOFF                                       0304
347800030131     C     KEYCED        CHAIN     ECCED30C                           99
347900030131     C     *IN99         IFEQ      *ON
348000030131     C                   ELSE
348100030131     C     *IN04         IFEQ      *ON
348200030131     C                   DELETE    ECCEDP00
348300030131     C                   ENDIF
348400030131     C     *IN03         IFEQ      *ON
348500030131     C                   DELETE    ECCED000
348600030131     C                   ENDIF
348700030131     C                   ENDIF
348800030131     C*
348900970507     C                   EXSR      INSCED
349000941215     C                   ENDIF
349100941215     C*
349200941215E   2C                   ENDIF
349300941215E   1C                   ENDDO
349400941215E   0C                   ENDIF
349500970507E  -1C                   ENDIF
349600941215     C*
349700941215     C                   ENDSR
349800941215     C*------------------------------------------------------------------------*
349900941215     C*?DELCED - CANCELLA RECORD IN ECCED00F
350000941215     C*------------------------------------------------------------------------*
350100941215     C     DELCED        BEGSR
350200941215     C                   Z-ADD     D$PAAS        CEDAAS
350300941215     C                   Z-ADD     D$PLNP        CEDLNP
350400941215     C                   Z-ADD     D$PNRS        CEDNRS
350500941215     C                   Z-ADD     D$PNSP        CEDNSP
350600941215     C                   MOVEL     D$PTBL        CEDTBL
350700950316     C                   SETOFF                                       0304
350800950316     C     KEYCED        CHAIN     ECCED30C                           99
350900950316IF  1C     *IN99         IFEQ      *ON
351000020805     C                   MOVEL     MSG(9)        $MSG
351100020805     C                   EXSR      SCRERR
351200950316X   1C                   ELSE
351300950316IF  2C     *IN04         IFEQ      *ON
351400020805     C                   MOVEL     MSG(21)       $MSG
351500020805     C                   EXSR      SCRERR
351600950316X   2C                   ELSE
351700010828     C                   DELETE    ECCED000
351800950316E   2C                   ENDIF
351900950316E   1C                   ENDIF
352000941215     C                   ENDSR
352100941215     C*------------------------------------------------------------------------*
352200941215     C*?INSCED - INSERISCE RECORD IN ECCED00F
352300941215     C*------------------------------------------------------------------------*
352400941215     C     INSCED        BEGSR
352500941215     C                   Z-ADD     D$DAAS        CEDAAS
352600941215     C                   Z-ADD     D$DLNP        CEDLNP
352700941215     C                   Z-ADD     D$DNRS        CEDNRS
352800941215     C                   Z-ADD     D$DNSP        CEDNSP
352900941215     C                   MOVEL     D$DTBL        CEDTBL
353000020129     C                   Z-ADD     IMD(CD)       CEDIMP
353100950118     C     D$DMGS        DIV       100           CEDMES
353200010828     C                   WRITE     ECCED000
353300941215     C                   ENDSR
353400941215     C*------------------------------------------------------------------------*
353500941215     C*?UPDCED - RISCRIVE RECORD IN ECCED00F
353600941215     C*------------------------------------------------------------------------*
353700941215     C     UPDCED        BEGSR
353800941215     C                   Z-ADD     D$DAAS        CEDAAS
353900941215     C                   Z-ADD     D$DLNP        CEDLNP
354000941215     C                   Z-ADD     D$DNRS        CEDNRS
354100941215     C                   Z-ADD     D$DNSP        CEDNSP
354200941215     C                   MOVEL     D$DTBL        CEDTBL
354300950316     C                   SETOFF                                       0304
354400950316     C     KEYCED        CHAIN     ECCED30C                           99
354500950316IF  1C     *IN99         IFEQ      *ON
354600941215     C                   MOVEL     MSG(10)       $MSG
354700941215     C                   EXSR      SCRERR
354800950316X   1C                   ELSE
354900950316IF  2C     *IN04         IFEQ      *ON
355000950316     C                   MOVEL     MSG(22)       $MSG
355100950316     C                   EXSR      SCRERR
355200950316X   2C                   ELSE
355300020129     C                   Z-ADD     IMD(CD)       CEDIMP
355400950118     C     D$DMGS        DIV       100           CEDMES
355500010828     C                   UPDATE    ECCED000
355600950316E   2C                   ENDIF
355700950316E   1C                   ENDIF
355800941215     C                   ENDSR
355900941215     C*------------------------------------------------------------------------*
356000950316     C*?AGGCET - AGGIORNA IL FILE ECCET30C (COMPETENZE TESTATA)
356100941215     C*------------------------------------------------------------------------*
356200941215     C     AGGCET        BEGSR
356300941215     C*
356400941215     C* SOLO SE FLAG AGG. CONTO ECON. = SI
356500950104IF  1C     D$GCET        IFEQ      'S'
356600950104     C*
356700950104     C* SE NON SONO DA REGISTRARE NE' I RICAVI NE' LE COMPET.
356800950104IF  2C     D$DFRI        IFEQ      'N'
356900950104     C     WFCD          ANDEQ     'N'
357000950104     C*
357100950104     C* SE PRIMA ESISTEVA LO CANCELLO
357200020805     C     WCETPR        IFEQ      'S'
357300950104     C                   EXSR      DELCET
357400020805     C                   ENDIF
357500950913     C*
357600970507     C* SCRIVE COMUNQUE UN REC. CON COMPETENZE A ZERO (SE NON E' DELETE)
357700950913     C     $ERR          IFEQ      'N'
357800970507     C     D$GSTS        ANDNE     'D'
357900950914     C                   Z-ADD     D$DAAS        CETAAS
358000950914     C                   Z-ADD     D$DLNP        CETLNP
358100950914     C                   Z-ADD     D$DNRS        CETNRS
358200950914     C                   Z-ADD     D$DNSP        CETNSP
358300950914     C                   MOVEL     D$DTBL        CETTBL
358400950914     C     KEYCET        CHAIN     ECCET30C                           99
358500950914IF  1C     *IN99         IFEQ      *ON
358600950913     C                   EXSR      AZZCET
358700010828     C                   WRITE     ECCET000
358800950913     C                   ENDIF
358900950914     C                   ENDIF
359000950104     C*
359100950104     C* SE LE COMPET. O I RICAVI SONO DA CALCOLARE
359200950104X   2C                   ELSE
359300941215     C*
359400941215     C* SE GLI IMPORTI DOPO SONO TUTTI UGUALI A ZERO
359500950104IF  3C     IMD(1)        IFEQ      *ZEROS
359600941215     C     IMD(2)        ANDEQ     *ZEROS
359700941215     C     IMD(3)        ANDEQ     *ZEROS
359800941215     C     IMD(4)        ANDEQ     *ZEROS
359900941215     C     IMD(5)        ANDEQ     *ZEROS
360000941215     C     IMD(6)        ANDEQ     *ZEROS
360100941215     C     IMD(7)        ANDEQ     *ZEROS
360200941215     C     IMD(8)        ANDEQ     *ZEROS
360300950315     C     IMD(9)        ANDEQ     *ZEROS
360400950315     C     IMD(10)       ANDEQ     *ZEROS
360500941215     C     IMD(13)       ANDEQ     *ZEROS
360600950315     C     IMD(14)       ANDEQ     *ZEROS
360700950315     C     IMD(15)       ANDEQ     *ZEROS
360800941215     C     IMD(29)       ANDEQ     *ZEROS
360900941215     C     IMD(26)       ANDEQ     *ZEROS
361000941215     C     IMD(25)       ANDEQ     *ZEROS
361100941215     C     IMD(27)       ANDEQ     *ZEROS
361200010827     C     IMD(33)       ANDEQ     *ZEROS
361300941215     C*
361400941215     C* SE PRIMA ESISTEVA LO CANCELLO
361500020805     C     WCETPR        IFEQ      'S'
361600941215     C                   EXSR      DELCET
361700020805     C                   ENDIF
361800941215RN   C*
361900941215RN   C* NE INSERISCO UNO "VUOTO"
362000941215RN   C                   EXSR      INSCET
362100941215     C*
362200941215     C* IMPORTI DIVERSI DA ZERO
362300950104X   3C                   ELSE
362400941215     C*
362500950104IF  4C     WCETPR        IFEQ      'S'
362600941215     C*
362700950104IF  5C     D$PTBL        IFEQ      D$DTBL
362800941215     C                   EXSR      UPDCET
362900950104X   5C                   ELSE
363000941215     C                   EXSR      DELCET
363100941215     C                   EXSR      INSCET
363200950104E   5C                   ENDIF
363300941215     C*
363400950104X   4C                   ELSE
363500941215     C*
363600941215     C                   EXSR      INSCET
363700941215     C*
363800950104E   4C                   ENDIF
363900941215     C*
364000950104E   3C                   ENDIF
364100950104E   2C                   ENDIF
364200950104E   1C                   ENDIF
364300941215     C*
364400941215     C                   ENDSR
364500941215     C*------------------------------------------------------------------------*
364600941215     C*?INSCET - INSERISCE RECORD IN ECCET00F
364700941215     C*------------------------------------------------------------------------*
364800941215     C     INSCET        BEGSR
364900950104     C     $ERR          IFEQ      'N'
365000941215     C                   Z-ADD     D$DAAS        CETAAS
365100941215     C                   Z-ADD     D$DLNP        CETLNP
365200941215     C                   Z-ADD     D$DNRS        CETNRS
365300941215     C                   Z-ADD     D$DNSP        CETNSP
365400941215     C                   MOVEL     D$DTBL        CETTBL
365500941215     C                   EXSR      ADDCET
365600031105     C                   WRITE(e)  ECCET000
365700031105     C*
365800031105     c                   IF        %error
365900031105     C                   MOVEL     MSG(30)       $MSG
366000031105     C                   EXSR      SCRERR
366100031105     c                   ENDIF
366200031105     C*
366300950104     C                   ENDIF
366400941215     C                   ENDSR
366500941215     C*------------------------------------------------------------------------*
366600941215     C*?DELCET - CANCELLA RECORD IN ECCET00F
366700941215     C*------------------------------------------------------------------------*
366800941215     C     DELCET        BEGSR
366900941215     C                   Z-ADD     D$PAAS        CETAAS
367000941215     C                   Z-ADD     D$PLNP        CETLNP
367100941215     C                   Z-ADD     D$PNRS        CETNRS
367200941215     C                   Z-ADD     D$PNSP        CETNSP
367300941215     C                   MOVEL     D$PTBL        CETTBL
367400950316     C                   SETOFF                                       0102
367500950316     C     KEYCET        CHAIN     ECCET30C                           99
367600950316IF  1C     *IN99         IFEQ      *ON
367700020805     C                   MOVEL     MSG(11)       $MSG
367800020805     C                   EXSR      SCRERR
367900950316X   1C                   ELSE
368000950316IF  2C     *IN02         IFEQ      *ON
368100020805     C                   MOVEL     MSG(19)       $MSG
368200020805     C                   EXSR      SCRERR
368300950316X   2C                   ELSE
368400010828     C                   DELETE    ECCET000
368500950316E   2C                   ENDIF
368600950316E   1C                   ENDIF
368700941215     C                   ENDSR
368800941215     C*------------------------------------------------------------------------*
368900941215     C*?UPDCET - AGGIORNA RECORD IN ECCET00F
369000941215     C*------------------------------------------------------------------------*
369100941215     C     UPDCET        BEGSR
369200941215     C                   Z-ADD     D$DAAS        CETAAS
369300941215     C                   Z-ADD     D$DLNP        CETLNP
369400941215     C                   Z-ADD     D$DNRS        CETNRS
369500941215     C                   Z-ADD     D$DNSP        CETNSP
369600941215     C                   MOVEL     D$DTBL        CETTBL
369700950316     C                   SETOFF                                       0102
369800950316     C     KEYCET        CHAIN     ECCET30C                           99
369900950316IF  1C     *IN99         IFEQ      *ON
370000941215     C                   MOVEL     MSG(12)       $MSG
370100941215     C                   EXSR      SCRERR
370200950316X   1C                   ELSE
370300950316IF  2C     *IN02         IFEQ      *ON
370400950316     C                   MOVEL     MSG(20)       $MSG
370500950316     C                   EXSR      SCRERR
370600950316X   2C                   ELSE
370700941215     C                   EXSR      ADDCET
370800010828     C                   UPDATE    ECCET000
370900950316E   2C                   ENDIF
371000950316E   1C                   ENDIF
371100941215     C                   ENDSR
371200941215     C*------------------------------------------------------------------------*
371300950316     C*?ADDCET - IMPOSTA I CAMPI DEL FILE ECCET30C (COMPETENZA TESTATA)
371400941215     C*------------------------------------------------------------------------*
371500941215     C     ADDCET        BEGSR
371600941215     C*
371700020129     C                   Z-ADD     IMD(1)        CETPAR
371800020129     C                   Z-ADD     IMD(2)        CETPAM
371900020129     C                   Z-ADD     IMD(3)        CETPAU
372000020129     C                   Z-ADD     IMD(4)        CETPAV
372100020129     C                   Z-ADD     IMD(5)        CETARD
372200020129     C                   Z-ADD     IMD(6)        CETARM
372300020129     C                   Z-ADD     IMD(7)        CETARU
372400020129     C                   Z-ADD     IMD(8)        CETARV
372500020129     C                   Z-ADD     IMD(9)        CETMAF
372600020129     C                   Z-ADD     IMD(10)       CETMDE
372700020129     C                   Z-ADD     IMD(13)       CETTRL
372800020129     C                   Z-ADD     IMD(14)       CETTAF
372900020129     C                   Z-ADD     IMD(15)       CETTDE
373000020129     C                   Z-ADD     IMD(29)       CETPAG
373100020129     C                   Z-ADD     IMD(26)       CETSEA
373200020129     C                   Z-ADD     IMD(25)       CETSEC
373300020129     C                   Z-ADD     IMD(27)       CETRCV
373400020129     C                   Z-ADD     IMD(33)       CETSES
373500941215     C                   MOVEL     WDET          CETDET
373600941215     C                   Z-ADD     WNBCD         CETNBC
373700950118     C     D$DMGS        DIV       100           CETMES
373800941215     C*
373900941215     C                   ENDSR
374000950913     C*------------------------------------------------------------------------*
374100950913     C*?AZZCET - AZZERA I CAMPI DEL FILE ECCET30C (COMPETENZA TESTATA)
374200950913     C*------------------------------------------------------------------------*
374300950913     C     AZZCET        BEGSR
374400950913     C*
374500950913     C                   Z-ADD     D$DAAS        CETAAS
374600950913     C                   Z-ADD     D$DLNP        CETLNP
374700950913     C                   Z-ADD     D$DNRS        CETNRS
374800950913     C                   Z-ADD     D$DNSP        CETNSP
374900950913     C                   MOVEL     D$DTBL        CETTBL
375000950913     C                   Z-ADD     *ZEROS        CETPAR
375100950913     C                   Z-ADD     *ZEROS        CETPAM
375200950913     C                   Z-ADD     *ZEROS        CETPAU
375300950913     C                   Z-ADD     *ZEROS        CETPAV
375400950913     C                   Z-ADD     *ZEROS        CETARD
375500950913     C                   Z-ADD     *ZEROS        CETARM
375600950913     C                   Z-ADD     *ZEROS        CETARU
375700950913     C                   Z-ADD     *ZEROS        CETARV
375800950913     C                   Z-ADD     *ZEROS        CETMAF
375900950913     C                   Z-ADD     *ZEROS        CETMDE
376000950913     C                   Z-ADD     *ZEROS        CETTRL
376100950913     C                   Z-ADD     *ZEROS        CETTAF
376200950913     C                   Z-ADD     *ZEROS        CETTDE
376300950913     C                   Z-ADD     *ZEROS        CETPAG
376400950913     C                   Z-ADD     *ZEROS        CETSEA
376500950913     C                   Z-ADD     *ZEROS        CETSEC
376600950913     C                   Z-ADD     *ZEROS        CETRCV
376700010827     C                   Z-ADD     *ZEROS        CETSES
376800950913     C                   MOVEL     WDET          CETDET
376900950913     C                   Z-ADD     WNBCD         CETNBC
377000950913     C     D$DMGS        DIV       100           CETMES
377100980313     C*
377200980313     C                   ENDSR
377300030114     C*------------------------------------------------------------------------*
377400030509     C*?AGGCEV - AGGIORNA IL FILE ECCEV30C (VOCI COOP) e ECC2V
377500030114     C*------------------------------------------------------------------------*
377600030114     C     AGGCEV        BEGSR
377700030114     C*
377800030114     C* SOLO SE FLAG AGG.COOP = SI
377900030114IF  1C     D$GCOP        IFEQ      'S'
378000030114     C*
378100030114     C* SE NON SONO DA REGISTRARE NE' I RICAVI NE' LE COMPETENZE
378200030114IF  2C     D$DFRI        IFEQ      'N'
378300030114     C     WFCD          ANDEQ     'N'
378400030114     C*
378500030114     C* SE PRIMA ESISTEVA LO CANCELLO
378600030114IF  3C     WCEVPR        IFEQ      'S'
378700030114     C                   EXSR      DELCEV
378800030114E   3C                   ENDIF
378900030114     C*
379000030114     C* SCRIVE COMUNQUE UN REC. CON COMPETENZE A ZERO (SE NON E' DELETE)
379100030114IF  3C     $ERR          IFEQ      'N'
379200030114     C     D$GSTS        ANDNE     'D'
379300030114     C                   Z-ADD     D$DAAS        CEVAAS
379400030114     C                   Z-ADD     D$DLNP        CEVLNP
379500030114     C                   Z-ADD     D$DNRS        CEVNRS
379600030114     C                   Z-ADD     D$DNSP        CEVNSP
379700030114     C                   MOVEL     D$DTBL        CEVTBL
379800030114     C     KEYCEV        CHAIN     ECCEV30C                           99
379900030114IF  4C     *IN99         IFEQ      *ON
380000030114     C                   EXSR      AZZCEV
380100030114     C                   WRITE     ECCEV000
380200030114E   4C                   ENDIF
380300030114E   3C                   ENDIF
380400030114     C*
380500030114     C* SE LE COMPETENZE O I RICAVI SONO DA CALCOLARE
380600030114X   2C                   ELSE
380700030114     C*
380800030114     C* SE GLI IMPORTI DOPO SONO TUTTI UGUALI A ZERO
380900030123     C                   XFOOT     IMDV          N213             21 3
381000030123IF  3C     N213          IFEQ      *ZEROS
381100030114     C*
381200030114     C* SE PRIMA ESISTEVA LO CANCELLO
381300030114IF  4C     WCEVPR        IFEQ      'S'
381400030114     C                   EXSR      DELCEV
381500030114E   4C                   ENDIF
381600030114     C*
381700030114     C* NE INSERISCO UNO "VUOTO"
381800030114     C                   EXSR      INSCEV
381900030114     C*
382000030114     C* IMPORTI DIVERSI DA ZERO
382100030114X   3C                   ELSE
382200030114     C*
382300030114IF  4C     WCEVPR        IFEQ      'S'
382400030114     C*
382500030114IF  5C     D$PTBL        IFEQ      D$DTBL
382600030114     C                   EXSR      UPDCEV
382700030114X   5C                   ELSE
382800030114     C                   EXSR      DELCEV
382900030114     C                   EXSR      INSCEV
383000030114E   5C                   ENDIF
383100030114     C*
383200030114X   4C                   ELSE
383300030114     C*
383400030114     C                   EXSR      INSCEV
383500030114     C*
383600030114E   4C                   ENDIF
383700030114     C*
383800030114E   3C                   ENDIF
383900030114E   2C                   ENDIF
384000030114E   1C                   ENDIF
384100030114     C*
384200030114     C                   ENDSR
384300030114     C*------------------------------------------------------------------------*
384400030509     C*?INSCEV - INSERISCE RECORD IN ECCEV00F ed eventualmente di ECC2V00F
384500030114     C*------------------------------------------------------------------------*
384600030114     C     INSCEV        BEGSR
384700030509     C*
384800030509     C* azzera variabili di lavoro
384900030509     c                   Z-ADD     *ZEROS        ULTCD             5 0
385000030509     c*
385100030509     C* voci coop testata
385200030114IF  1C     $ERR          IFEQ      'N'
385300030114     C                   Z-ADD     D$DAAS        CEVAAS
385400030114     C                   Z-ADD     D$DLNP        CEVLNP
385500030114     C                   Z-ADD     D$DNRS        CEVNRS
385600030114     C                   Z-ADD     D$DNSP        CEVNSP
385700030114     C                   MOVEL     D$DTBL        CEVTBL
385800030128     C                   EXSR      AZZCEV
385900030114     C                   EXSR      ADDCEV
386000030114     C                   WRITE     ECCEV000
386100030509     c*
386200030509     C* voci coop estensione (se ce n'è)
386300030509if  2C     ULTCD         IFGT      *ZEROS
386400030509     C                   Z-ADD     D$DAAS        C2VAAS
386500030509     C                   Z-ADD     D$DLNP        C2VLNP
386600030509     C                   Z-ADD     D$DNRS        C2VNRS
386700030509     C                   Z-ADD     D$DNSP        C2VNSP
386800030509     C                   MOVEL     D$DTBL        C2VTBL
386900030509     C                   EXSR      AZZC2V
387000030509     C                   EXSR      ADDC2V
387100030509     C                   WRITE     ECC2V000
387200030509E   2C                   ENDIF
387300030509     C*
387400030509E   1C                   ENDIF
387500030114     C*
387600030114     C                   ENDSR
387700030114     C*------------------------------------------------------------------------*
387800030114     C*?DELCEV - CANCELLA RECORD IN ECCEV00F
387900030114     C*------------------------------------------------------------------------*
388000030114     C     DELCEV        BEGSR
388100030509     C*
388200030509     C* voci coop testata
388300030114     C                   Z-ADD     D$PAAS        CEVAAS
388400030114     C                   Z-ADD     D$PLNP        CEVLNP
388500030114     C                   Z-ADD     D$PNRS        CEVNRS
388600030114     C                   Z-ADD     D$PNSP        CEVNSP
388700030114     C                   MOVEL     D$PTBL        CEVTBL
388800030114     C                   SETOFF                                       0506
388900030114     C     KEYCEV        CHAIN     ECCEV30C                           99
389000030114IF  1C     *IN99         IFEQ      *ON
389100030114     C                   MOVEL     MSG(25)       $MSG
389200030114     C                   EXSR      SCRERR
389300030114X   1C                   ELSE
389400030509IF  2C     *IN06         IFEQ      *ON
389500030114     C                   MOVEL     MSG(26)       $MSG
389600030114     C                   EXSR      SCRERR
389700030114X   2C                   ELSE
389800030114     C                   DELETE    ECCEV000
389900030114E   2C                   ENDIF
390000030114E   1C                   ENDIF
390100030509     c*
390200030509     C* voci coop estensione
390300030509     C                   Z-ADD     D$PAAS        C2VAAS
390400030509     C                   Z-ADD     D$PLNP        C2VLNP
390500030509     C                   Z-ADD     D$PNRS        C2VNRS
390600030509     C                   Z-ADD     D$PNSP        C2VNSP
390700030509     C                   MOVEL     D$PTBL        C2VTBL
390800030509     C                   SETOFF                                       0708
390900030509     C     KEYC2V        CHAIN     ECC2V30C                           99
391000030509IF  1C     *IN99         IFEQ      *OFF
391100030509     C                   DELETE    ECC2V000
391200030509E   1C                   ENDIF
391300030509     C*
391400030114     C                   ENDSR
391500030114     C*------------------------------------------------------------------------*
391600030114     C*?UPDCEV - AGGIORNA RECORD IN ECCEV00F
391700030114     C*------------------------------------------------------------------------*
391800030114     C     UPDCEV        BEGSR
391900030509     C*
392000030509     C* azzera variabili di lavoro
392100030509     c                   Z-ADD     *ZEROS        ULTCD
392200030114     C*
392300030509     C* voci coop testata
392400030114     C                   Z-ADD     D$DAAS        CEVAAS
392500030114     C                   Z-ADD     D$DLNP        CEVLNP
392600030114     C                   Z-ADD     D$DNRS        CEVNRS
392700030114     C                   Z-ADD     D$DNSP        CEVNSP
392800030114     C                   MOVEL     D$DTBL        CEVTBL
392900030114     C                   SETOFF                                       0506
393000030114     C     KEYCEV        CHAIN     ECCEV30C                           99
393100030114IF  1C     *IN99         IFEQ      *ON
393200030114     C                   MOVEL     MSG(27)       $MSG
393300030114     C                   EXSR      SCRERR
393400030114X   1C                   ELSE
393500030509IF  2C     *IN06         IFEQ      *ON
393600030114     C                   MOVEL     MSG(28)       $MSG
393700030114     C                   EXSR      SCRERR
393800030114X   2C                   ELSE
393900030124     C                   EXSR      AZZCEV
394000030114     C                   EXSR      ADDCEV
394100030114     C                   UPDATE    ECCEV000
394200030114E   2C                   ENDIF
394300030114E   1C                   ENDIF
394400030509     c*
394500030509     C* voci coop estensione (se ce n'è)
394600030509     C                   Z-ADD     D$DAAS        C2VAAS
394700030509     C                   Z-ADD     D$DLNP        C2VLNP
394800030509     C                   Z-ADD     D$DNRS        C2VNRS
394900030509     C                   Z-ADD     D$DNSP        C2VNSP
395000030509     C                   MOVEL     D$DTBL        C2VTBL
395100030509     C                   SETOFF                                       0708
395200030509     C     KEYC2V        CHAIN     ECC2V30C                           99
395300030509     C                   EXSR      AZZC2V
395400030509     C                   EXSR      ADDC2V
395500030512IF  1C     *IN99         IFEQ      *OFF
395600030512IF  2C     ULTCD         IFGT      *ZEROS
395700030512     C     WC2VP         OREQ      'S'
395800030512     C                   UPDATE    ECC2V000
395900030512E   2C                   ENDIF
396000030512X   1C                   ELSE
396100030512IF  2C     ULTCD         IFGT      *ZEROS
396200030512     C                   WRITE     ECC2V000
396300030512E   2C                   ENDIF
396400030512E   1C                   ENDIF
396500030114     C*
396600030114     C                   ENDSR
396700030114     C*------------------------------------------------------------------------*
396800030114     C*?ADDCEV - IMPOSTA I CAMPI DEL FILE ECCEV30C (VOCI COOP)
396900030114     C*------------------------------------------------------------------------*
397000030114     C     ADDCEV        BEGSR
397100030115     C*
397200030115     C* SE C'É ALMENO UN DATO IN SCHIERA, 'S' -> FLAG REG. SICOP
397300030124     C                   MOVEL     'N'           WCODREG           1
397400030114     C*
397500030115     C* CICLO PER TUTTE LE VOCI
397600030115DO  1C     1             DO        999           CD
397700030115IF  2C     IMDV(CD)      IFGT      *ZEROS
397800030124     C                   MOVEL     'S'           WCODREG
397900030115     C*
398000030115     C* MEMORIZZA CODICE E IMPORTO NEL PRIMO CAMPO LIBERO
398100030115IF  3C                   IF        CEVI01 = *ZEROS
398200030115     C                   Z-ADD     CD            CEVC01
398300030115     C                   Z-ADD     IMDV(CD)      CEVI01
398400030115     C                   ITER
398500030115E   3C                   ENDIF
398600030115IF  3C                   IF        CEVI02 = *ZEROS
398700030115     C                   Z-ADD     CD            CEVC02
398800030115     C                   Z-ADD     IMDV(CD)      CEVI02
398900030115     C                   ITER
399000030115E   3C                   ENDIF
399100030115IF  3C                   IF        CEVI03 = *ZEROS
399200030115     C                   Z-ADD     CD            CEVC03
399300030115     C                   Z-ADD     IMDV(CD)      CEVI03
399400030115     C                   ITER
399500030115E   3C                   ENDIF
399600030115IF  3C                   IF        CEVI04 = *ZEROS
399700030115     C                   Z-ADD     CD            CEVC04
399800030115     C                   Z-ADD     IMDV(CD)      CEVI04
399900030115     C                   ITER
400000030115E   3C                   ENDIF
400100030115IF  3C                   IF        CEVI05 = *ZEROS
400200030115     C                   Z-ADD     CD            CEVC05
400300030115     C                   Z-ADD     IMDV(CD)      CEVI05
400400030115     C                   ITER
400500030115E   3C                   ENDIF
400600030115IF  3C                   IF        CEVI06 = *ZEROS
400700030115     C                   Z-ADD     CD            CEVC06
400800030115     C                   Z-ADD     IMDV(CD)      CEVI06
400900030115     C                   ITER
401000030115E   3C                   ENDIF
401100030115IF  3C                   IF        CEVI07 = *ZEROS
401200030115     C                   Z-ADD     CD            CEVC07
401300030115     C                   Z-ADD     IMDV(CD)      CEVI07
401400030115     C                   ITER
401500030115E   3C                   ENDIF
401600030115IF  3C                   IF        CEVI08 = *ZEROS
401700030115     C                   Z-ADD     CD            CEVC08
401800030115     C                   Z-ADD     IMDV(CD)      CEVI08
401900030115     C                   ITER
402000030115E   3C                   ENDIF
402100030115IF  3C                   IF        CEVI09 = *ZEROS
402200030115     C                   Z-ADD     CD            CEVC09
402300030115     C                   Z-ADD     IMDV(CD)      CEVI09
402400030115     C                   ITER
402500030115E   3C                   ENDIF
402600030115     C*
402700030115IF  3C                   IF        CEVI10 = *ZEROS
402800030115     C                   Z-ADD     CD            CEVC10
402900030115     C                   Z-ADD     IMDV(CD)      CEVI10
403000030115     C                   ITER
403100030115E   3C                   ENDIF
403200030115IF  3C                   IF        CEVI11 = *ZEROS
403300030115     C                   Z-ADD     CD            CEVC11
403400030115     C                   Z-ADD     IMDV(CD)      CEVI11
403500030115     C                   ITER
403600030115E   3C                   ENDIF
403700030115IF  3C                   IF        CEVI12 = *ZEROS
403800030115     C                   Z-ADD     CD            CEVC12
403900030115     C                   Z-ADD     IMDV(CD)      CEVI12
404000030115     C                   ITER
404100030115E   3C                   ENDIF
404200030115IF  3C                   IF        CEVI13 = *ZEROS
404300030115     C                   Z-ADD     CD            CEVC13
404400030115     C                   Z-ADD     IMDV(CD)      CEVI13
404500030115     C                   ITER
404600030115E   3C                   ENDIF
404700030115IF  3C                   IF        CEVI14 = *ZEROS
404800030115     C                   Z-ADD     CD            CEVC14
404900030115     C                   Z-ADD     IMDV(CD)      CEVI14
405000030115     C                   ITER
405100030115E   3C                   ENDIF
405200030115IF  3C                   IF        CEVI15 = *ZEROS
405300030115     C                   Z-ADD     CD            CEVC15
405400030115     C                   Z-ADD     IMDV(CD)      CEVI15
405500030115     C                   ITER
405600030115E   3C                   ENDIF
405700030115IF  3C                   IF        CEVI16 = *ZEROS
405800030115     C                   Z-ADD     CD            CEVC16
405900030115     C                   Z-ADD     IMDV(CD)      CEVI16
406000030115     C                   ITER
406100030115E   3C                   ENDIF
406200030115IF  3C                   IF        CEVI17 = *ZEROS
406300030115     C                   Z-ADD     CD            CEVC17
406400030115     C                   Z-ADD     IMDV(CD)      CEVI17
406500030115     C                   ITER
406600030115E   3C                   ENDIF
406700030115IF  3C                   IF        CEVI18 = *ZEROS
406800030115     C                   Z-ADD     CD            CEVC18
406900030115     C                   Z-ADD     IMDV(CD)      CEVI18
407000030115     C                   ITER
407100030115E   3C                   ENDIF
407200030115IF  3C                   IF        CEVI19 = *ZEROS
407300030115     C                   Z-ADD     CD            CEVC19
407400030115     C                   Z-ADD     IMDV(CD)      CEVI19
407500030115     C                   ITER
407600030115E   3C                   ENDIF
407700030115     C*
407800030115IF  3C                   IF        CEVI20 = *ZEROS
407900030115     C                   Z-ADD     CD            CEVC20
408000030115     C                   Z-ADD     IMDV(CD)      CEVI20
408100030115     C                   ITER
408200030115E   3C                   ENDIF
408300030115IF  3C                   IF        CEVI21 = *ZEROS
408400030115     C                   Z-ADD     CD            CEVC21
408500030115     C                   Z-ADD     IMDV(CD)      CEVI21
408600030115     C                   ITER
408700030115E   3C                   ENDIF
408800030115IF  3C                   IF        CEVI22 = *ZEROS
408900030115     C                   Z-ADD     CD            CEVC22
409000030115     C                   Z-ADD     IMDV(CD)      CEVI22
409100030115     C                   ITER
409200030115E   3C                   ENDIF
409300030115IF  3C                   IF        CEVI23 = *ZEROS
409400030115     C                   Z-ADD     CD            CEVC23
409500030115     C                   Z-ADD     IMDV(CD)      CEVI23
409600030115     C                   ITER
409700030115E   3C                   ENDIF
409800030115IF  3C                   IF        CEVI24 = *ZEROS
409900030115     C                   Z-ADD     CD            CEVC24
410000030115     C                   Z-ADD     IMDV(CD)      CEVI24
410100030115     C                   ITER
410200030115E   3C                   ENDIF
410300030115IF  3C                   IF        CEVI25 = *ZEROS
410400030115     C                   Z-ADD     CD            CEVC25
410500030115     C                   Z-ADD     IMDV(CD)      CEVI25
410600030115     C                   ITER
410700030115E   3C                   ENDIF
410800030115IF  3C                   IF        CEVI26 = *ZEROS
410900030115     C                   Z-ADD     CD            CEVC26
411000030115     C                   Z-ADD     IMDV(CD)      CEVI26
411100030115     C                   ITER
411200030115E   3C                   ENDIF
411300030115IF  3C                   IF        CEVI27 = *ZEROS
411400030115     C                   Z-ADD     CD            CEVC27
411500030115     C                   Z-ADD     IMDV(CD)      CEVI27
411600030115     C                   ITER
411700030115E   3C                   ENDIF
411800030115IF  3C                   IF        CEVI28 = *ZEROS
411900030115     C                   Z-ADD     CD            CEVC28
412000030115     C                   Z-ADD     IMDV(CD)      CEVI28
412100030115     C                   ITER
412200030115E   3C                   ENDIF
412300030115IF  3C                   IF        CEVI29 = *ZEROS
412400030115     C                   Z-ADD     CD            CEVC29
412500030115     C                   Z-ADD     IMDV(CD)      CEVI29
412600030115     C                   ITER
412700030115E   3C                   ENDIF
412800030115     C*
412900030115IF  3C                   IF        CEVI30 = *ZEROS
413000030115     C                   Z-ADD     CD            CEVC30
413100030115     C                   Z-ADD     IMDV(CD)      CEVI30
413200030115     C                   ITER
413300030115E   3C                   ENDIF
413400030115IF  3C                   IF        CEVI31 = *ZEROS
413500030115     C                   Z-ADD     CD            CEVC31
413600030115     C                   Z-ADD     IMDV(CD)      CEVI31
413700030115     C                   ITER
413800030115E   3C                   ENDIF
413900030115IF  3C                   IF        CEVI32 = *ZEROS
414000030115     C                   Z-ADD     CD            CEVC32
414100030115     C                   Z-ADD     IMDV(CD)      CEVI32
414200030115     C                   ITER
414300030115E   3C                   ENDIF
414400030115IF  3C                   IF        CEVI33 = *ZEROS
414500030115     C                   Z-ADD     CD            CEVC33
414600030115     C                   Z-ADD     IMDV(CD)      CEVI33
414700030115     C                   ITER
414800030115E   3C                   ENDIF
414900030115IF  3C                   IF        CEVI34 = *ZEROS
415000030115     C                   Z-ADD     CD            CEVC34
415100030115     C                   Z-ADD     IMDV(CD)      CEVI34
415200030115     C                   ITER
415300030115E   3C                   ENDIF
415400030115IF  3C                   IF        CEVI35 = *ZEROS
415500030115     C                   Z-ADD     CD            CEVC35
415600030115     C                   Z-ADD     IMDV(CD)      CEVI35
415700030115     C                   ITER
415800030115E   3C                   ENDIF
415900030115IF  3C                   IF        CEVI36 = *ZEROS
416000030115     C                   Z-ADD     CD            CEVC36
416100030115     C                   Z-ADD     IMDV(CD)      CEVI36
416200030115     C                   ITER
416300030115E   3C                   ENDIF
416400030115IF  3C                   IF        CEVI37 = *ZEROS
416500030115     C                   Z-ADD     CD            CEVC37
416600030115     C                   Z-ADD     IMDV(CD)      CEVI37
416700030115     C                   ITER
416800030115E   3C                   ENDIF
416900030115IF  3C                   IF        CEVI38 = *ZEROS
417000030115     C                   Z-ADD     CD            CEVC38
417100030115     C                   Z-ADD     IMDV(CD)      CEVI38
417200030115     C                   ITER
417300030115E   3C                   ENDIF
417400030115IF  3C                   IF        CEVI39 = *ZEROS
417500030115     C                   Z-ADD     CD            CEVC39
417600030115     C                   Z-ADD     IMDV(CD)      CEVI39
417700030115     C                   ITER
417800030115E   3C                   ENDIF
417900030115     C*
418000030115IF  3C                   IF        CEVI40 = *ZEROS
418100030115     C                   Z-ADD     CD            CEVC40
418200030115     C                   Z-ADD     IMDV(CD)      CEVI40
418300030115     C                   ITER
418400030115E   3C                   ENDIF
418500030115IF  3C                   IF        CEVI41 = *ZEROS
418600030115     C                   Z-ADD     CD            CEVC41
418700030115     C                   Z-ADD     IMDV(CD)      CEVI41
418800030115     C                   ITER
418900030115E   3C                   ENDIF
419000030115IF  3C                   IF        CEVI42 = *ZEROS
419100030115     C                   Z-ADD     CD            CEVC42
419200030115     C                   Z-ADD     IMDV(CD)      CEVI42
419300030115     C                   ITER
419400030115E   3C                   ENDIF
419500030115IF  3C                   IF        CEVI43 = *ZEROS
419600030115     C                   Z-ADD     CD            CEVC43
419700030115     C                   Z-ADD     IMDV(CD)      CEVI43
419800030115     C                   ITER
419900030115E   3C                   ENDIF
420000030115IF  3C                   IF        CEVI44 = *ZEROS
420100030115     C                   Z-ADD     CD            CEVC44
420200030115     C                   Z-ADD     IMDV(CD)      CEVI44
420300030115     C                   ITER
420400030115E   3C                   ENDIF
420500030115IF  3C                   IF        CEVI45 = *ZEROS
420600030115     C                   Z-ADD     CD            CEVC45
420700030115     C                   Z-ADD     IMDV(CD)      CEVI45
420800030115     C                   ITER
420900030115E   3C                   ENDIF
421000030115IF  3C                   IF        CEVI46 = *ZEROS
421100030115     C                   Z-ADD     CD            CEVC46
421200030115     C                   Z-ADD     IMDV(CD)      CEVI46
421300030115     C                   ITER
421400030115E   3C                   ENDIF
421500030115IF  3C                   IF        CEVI47 = *ZEROS
421600030115     C                   Z-ADD     CD            CEVC47
421700030115     C                   Z-ADD     IMDV(CD)      CEVI47
421800030115     C                   ITER
421900030115E   3C                   ENDIF
422000030115IF  3C                   IF        CEVI48 = *ZEROS
422100030115     C                   Z-ADD     CD            CEVC48
422200030115     C                   Z-ADD     IMDV(CD)      CEVI48
422300030115     C                   ITER
422400030115E   3C                   ENDIF
422500030115IF  3C                   IF        CEVI49 = *ZEROS
422600030115     C                   Z-ADD     CD            CEVC49
422700030115     C                   Z-ADD     IMDV(CD)      CEVI49
422800030115     C                   ITER
422900030115E   3C                   ENDIF
423000030115IF  3C                   IF        CEVI50 = *ZEROS
423100030115     C                   Z-ADD     CD            CEVC50
423200030115     C                   Z-ADD     IMDV(CD)      CEVI50
423300030509     c                   Z-ADD     CD            ULTCD
423400030115     C                   ITER
423500030115E   3C                   ENDIF
423600030115     C*
423700030115E   2C                   ENDIF
423800030115E   1C                   ENDDO
423900030115     C     D$DMGS        DIV       100           CEVMES
424000030114     C*
424100030114     C                   ENDSR
424200030114     C*------------------------------------------------------------------------*
424300030114     C*?AZZCEV - AZZERA I CAMPI DEL FILE ECCEV30C (VOCI COOP)
424400030114     C*------------------------------------------------------------------------*
424500030114     C     AZZCEV        BEGSR
424600030115     C*
424700030114     C                   Z-ADD     D$DAAS        CEVAAS
424800030114     C                   Z-ADD     D$DLNP        CEVLNP
424900030114     C                   Z-ADD     D$DNRS        CEVNRS
425000030114     C                   Z-ADD     D$DNSP        CEVNSP
425100030114     C                   MOVEL     D$DTBL        CEVTBL
425200030115     C     D$DMGS        DIV       100           CEVMES
425300030115     C* CODICE
425400030115     C                   Z-ADD     *ZEROS        CEVC01
425500030115     C                   Z-ADD     *ZEROS        CEVC02
425600030115     C                   Z-ADD     *ZEROS        CEVC03
425700030115     C                   Z-ADD     *ZEROS        CEVC04
425800030115     C                   Z-ADD     *ZEROS        CEVC05
425900030115     C                   Z-ADD     *ZEROS        CEVC06
426000030115     C                   Z-ADD     *ZEROS        CEVC07
426100030115     C                   Z-ADD     *ZEROS        CEVC08
426200030115     C                   Z-ADD     *ZEROS        CEVC09
426300030115     C                   Z-ADD     *ZEROS        CEVC10
426400030115     C                   Z-ADD     *ZEROS        CEVC11
426500030115     C                   Z-ADD     *ZEROS        CEVC12
426600030115     C                   Z-ADD     *ZEROS        CEVC13
426700030115     C                   Z-ADD     *ZEROS        CEVC14
426800030115     C                   Z-ADD     *ZEROS        CEVC15
426900030115     C                   Z-ADD     *ZEROS        CEVC16
427000030115     C                   Z-ADD     *ZEROS        CEVC17
427100030115     C                   Z-ADD     *ZEROS        CEVC18
427200030115     C                   Z-ADD     *ZEROS        CEVC19
427300030115     C                   Z-ADD     *ZEROS        CEVC20
427400030115     C                   Z-ADD     *ZEROS        CEVC21
427500030115     C                   Z-ADD     *ZEROS        CEVC22
427600030115     C                   Z-ADD     *ZEROS        CEVC23
427700030115     C                   Z-ADD     *ZEROS        CEVC24
427800030115     C                   Z-ADD     *ZEROS        CEVC25
427900030115     C                   Z-ADD     *ZEROS        CEVC26
428000030115     C                   Z-ADD     *ZEROS        CEVC27
428100030115     C                   Z-ADD     *ZEROS        CEVC28
428200030115     C                   Z-ADD     *ZEROS        CEVC29
428300030115     C                   Z-ADD     *ZEROS        CEVC30
428400030115     C                   Z-ADD     *ZEROS        CEVC31
428500030115     C                   Z-ADD     *ZEROS        CEVC32
428600030115     C                   Z-ADD     *ZEROS        CEVC33
428700030115     C                   Z-ADD     *ZEROS        CEVC34
428800030115     C                   Z-ADD     *ZEROS        CEVC35
428900030115     C                   Z-ADD     *ZEROS        CEVC36
429000030115     C                   Z-ADD     *ZEROS        CEVC37
429100030115     C                   Z-ADD     *ZEROS        CEVC38
429200030115     C                   Z-ADD     *ZEROS        CEVC39
429300030115     C                   Z-ADD     *ZEROS        CEVC40
429400030115     C                   Z-ADD     *ZEROS        CEVC41
429500030115     C                   Z-ADD     *ZEROS        CEVC42
429600030115     C                   Z-ADD     *ZEROS        CEVC43
429700030115     C                   Z-ADD     *ZEROS        CEVC44
429800030115     C                   Z-ADD     *ZEROS        CEVC45
429900030115     C                   Z-ADD     *ZEROS        CEVC46
430000030115     C                   Z-ADD     *ZEROS        CEVC47
430100030115     C                   Z-ADD     *ZEROS        CEVC48
430200030115     C                   Z-ADD     *ZEROS        CEVC49
430300030115     C                   Z-ADD     *ZEROS        CEVC50
430400030115     C* IMPORTO
430500030114     C                   Z-ADD     *ZEROS        CEVI01
430600030114     C                   Z-ADD     *ZEROS        CEVI02
430700030114     C                   Z-ADD     *ZEROS        CEVI03
430800030114     C                   Z-ADD     *ZEROS        CEVI04
430900030114     C                   Z-ADD     *ZEROS        CEVI05
431000030114     C                   Z-ADD     *ZEROS        CEVI06
431100030114     C                   Z-ADD     *ZEROS        CEVI07
431200030114     C                   Z-ADD     *ZEROS        CEVI08
431300030114     C                   Z-ADD     *ZEROS        CEVI09
431400030114     C                   Z-ADD     *ZEROS        CEVI10
431500030114     C                   Z-ADD     *ZEROS        CEVI11
431600030114     C                   Z-ADD     *ZEROS        CEVI12
431700030114     C                   Z-ADD     *ZEROS        CEVI13
431800030114     C                   Z-ADD     *ZEROS        CEVI14
431900030114     C                   Z-ADD     *ZEROS        CEVI15
432000030114     C                   Z-ADD     *ZEROS        CEVI16
432100030114     C                   Z-ADD     *ZEROS        CEVI17
432200030114     C                   Z-ADD     *ZEROS        CEVI18
432300030114     C                   Z-ADD     *ZEROS        CEVI19
432400030114     C                   Z-ADD     *ZEROS        CEVI20
432500030114     C                   Z-ADD     *ZEROS        CEVI21
432600030114     C                   Z-ADD     *ZEROS        CEVI22
432700030114     C                   Z-ADD     *ZEROS        CEVI23
432800030114     C                   Z-ADD     *ZEROS        CEVI24
432900030114     C                   Z-ADD     *ZEROS        CEVI25
433000030114     C                   Z-ADD     *ZEROS        CEVI26
433100030114     C                   Z-ADD     *ZEROS        CEVI27
433200030114     C                   Z-ADD     *ZEROS        CEVI28
433300030114     C                   Z-ADD     *ZEROS        CEVI29
433400030114     C                   Z-ADD     *ZEROS        CEVI30
433500030114     C                   Z-ADD     *ZEROS        CEVI31
433600030114     C                   Z-ADD     *ZEROS        CEVI32
433700030114     C                   Z-ADD     *ZEROS        CEVI33
433800030114     C                   Z-ADD     *ZEROS        CEVI34
433900030114     C                   Z-ADD     *ZEROS        CEVI35
434000030114     C                   Z-ADD     *ZEROS        CEVI36
434100030114     C                   Z-ADD     *ZEROS        CEVI37
434200030114     C                   Z-ADD     *ZEROS        CEVI38
434300030114     C                   Z-ADD     *ZEROS        CEVI39
434400030114     C                   Z-ADD     *ZEROS        CEVI40
434500030114     C                   Z-ADD     *ZEROS        CEVI41
434600030114     C                   Z-ADD     *ZEROS        CEVI42
434700030114     C                   Z-ADD     *ZEROS        CEVI43
434800030114     C                   Z-ADD     *ZEROS        CEVI44
434900030114     C                   Z-ADD     *ZEROS        CEVI45
435000030114     C                   Z-ADD     *ZEROS        CEVI46
435100030114     C                   Z-ADD     *ZEROS        CEVI47
435200030114     C                   Z-ADD     *ZEROS        CEVI48
435300030114     C                   Z-ADD     *ZEROS        CEVI49
435400030114     C                   Z-ADD     *ZEROS        CEVI50
435500030114     C*
435600030114     C                   ENDSR
435700030509     C*------------------------------------------------------------------------*
435800030509     C*?ADDC2V - IMPOSTA I CAMPI DEL FILE ECC2V30C (VOCI COOP ESTENSIONE)
435900030509     C*------------------------------------------------------------------------*
436000030509     C     ADDC2V        BEGSR
436100030509     C*
436200030509     C* SCRIVE IL FILE VOCI COOP ESTENSIONE SOLO SE PIÙ DI 50 VOCI
436300030509IF  1C     ULTCD         IFGT      *ZEROS
436400030509     C*
436500030509     C* CICLO PER TUTTE LE VOCI OLTRE 50
436600030509     C                   Z-ADD     *ZEROS        WCD               5 0
436700030512     C                   EVAL      WCD = ULTCD + 1
436800030509DO  2C     WCD           DO        999           CD
436900030509IF  3C     IMDV(CD)      IFGT      *ZEROS
437000030509     C*
437100030509     C* MEMORIZZA CODICE E IMPORTO NEL PRIMO CAMPO LIBERO
437200030509IF  4C                   IF        C2VI51 = *ZEROS
437300030509     C                   Z-ADD     CD            C2VC51
437400030509     C                   Z-ADD     IMDV(CD)      C2VI51
437500030509     C                   ITER
437600030509E   4C                   ENDIF
437700030509IF  4C                   IF        C2VI52 = *ZEROS
437800030509     C                   Z-ADD     CD            C2VC52
437900030509     C                   Z-ADD     IMDV(CD)      C2VI52
438000030509     C                   ITER
438100030509E   4C                   ENDIF
438200030509IF  4C                   IF        C2VI53 = *ZEROS
438300030509     C                   Z-ADD     CD            C2VC53
438400030509     C                   Z-ADD     IMDV(CD)      C2VI53
438500030509     C                   ITER
438600030509E   4C                   ENDIF
438700030509IF  4C                   IF        C2VI54 = *ZEROS
438800030509     C                   Z-ADD     CD            C2VC54
438900030509     C                   Z-ADD     IMDV(CD)      C2VI54
439000030509     C                   ITER
439100030509E   4C                   ENDIF
439200030509IF  4C                   IF        C2VI55 = *ZEROS
439300030509     C                   Z-ADD     CD            C2VC55
439400030509     C                   Z-ADD     IMDV(CD)      C2VI55
439500030509     C                   ITER
439600030509E   4C                   ENDIF
439700030509IF  4C                   IF        C2VI56 = *ZEROS
439800030509     C                   Z-ADD     CD            C2VC56
439900030509     C                   Z-ADD     IMDV(CD)      C2VI56
440000030509     C                   ITER
440100030509E   4C                   ENDIF
440200030509IF  4C                   IF        C2VI57 = *ZEROS
440300030509     C                   Z-ADD     CD            C2VC57
440400030509     C                   Z-ADD     IMDV(CD)      C2VI57
440500030509     C                   ITER
440600030509E   4C                   ENDIF
440700030509IF  4C                   IF        C2VI58 = *ZEROS
440800030509     C                   Z-ADD     CD            C2VC58
440900030509     C                   Z-ADD     IMDV(CD)      C2VI58
441000030509     C                   ITER
441100030509E   4C                   ENDIF
441200030509IF  4C                   IF        C2VI59 = *ZEROS
441300030509     C                   Z-ADD     CD            C2VC59
441400030509     C                   Z-ADD     IMDV(CD)      C2VI59
441500030509     C                   ITER
441600030509E   4C                   ENDIF
441700030509IF  4C                   IF        C2VI60 = *ZEROS
441800030509     C                   Z-ADD     CD            C2VC60
441900030509     C                   Z-ADD     IMDV(CD)      C2VI60
442000030509     C                   ITER
442100030509E   4C                   ENDIF
442200030509IF  4C                   IF        C2VI61 = *ZEROS
442300030509     C                   Z-ADD     CD            C2VC61
442400030509     C                   Z-ADD     IMDV(CD)      C2VI61
442500030509     C                   ITER
442600030509E   4C                   ENDIF
442700030509IF  4C                   IF        C2VI62 = *ZEROS
442800030509     C                   Z-ADD     CD            C2VC62
442900030509     C                   Z-ADD     IMDV(CD)      C2VI62
443000030509     C                   ITER
443100030509E   4C                   ENDIF
443200030509IF  4C                   IF        C2VI63 = *ZEROS
443300030509     C                   Z-ADD     CD            C2VC63
443400030509     C                   Z-ADD     IMDV(CD)      C2VI63
443500030509     C                   ITER
443600030509E   4C                   ENDIF
443700030509IF  4C                   IF        C2VI64 = *ZEROS
443800030509     C                   Z-ADD     CD            C2VC64
443900030509     C                   Z-ADD     IMDV(CD)      C2VI64
444000030509     C                   ITER
444100030509E   4C                   ENDIF
444200030509IF  4C                   IF        C2VI65 = *ZEROS
444300030509     C                   Z-ADD     CD            C2VC65
444400030509     C                   Z-ADD     IMDV(CD)      C2VI65
444500030509     C                   ITER
444600030509E   4C                   ENDIF
444700030509IF  4C                   IF        C2VI66 = *ZEROS
444800030509     C                   Z-ADD     CD            C2VC66
444900030509     C                   Z-ADD     IMDV(CD)      C2VI66
445000030509     C                   ITER
445100030509E   4C                   ENDIF
445200030509IF  4C                   IF        C2VI67 = *ZEROS
445300030509     C                   Z-ADD     CD            C2VC67
445400030509     C                   Z-ADD     IMDV(CD)      C2VI67
445500030509     C                   ITER
445600030509E   4C                   ENDIF
445700030509IF  4C                   IF        C2VI68 = *ZEROS
445800030509     C                   Z-ADD     CD            C2VC68
445900030509     C                   Z-ADD     IMDV(CD)      C2VI68
446000030509     C                   ITER
446100030509E   4C                   ENDIF
446200030509IF  4C                   IF        C2VI69 = *ZEROS
446300030509     C                   Z-ADD     CD            C2VC69
446400030509     C                   Z-ADD     IMDV(CD)      C2VI69
446500030509     C                   ITER
446600030509E   4C                   ENDIF
446700030509IF  4C                   IF        C2VI70 = *ZEROS
446800030509     C                   Z-ADD     CD            C2VC70
446900030509     C                   Z-ADD     IMDV(CD)      C2VI70
447000030509     C                   ITER
447100030509E   4C                   ENDIF
447200030509IF  4C                   IF        C2VI71 = *ZEROS
447300030509     C                   Z-ADD     CD            C2VC71
447400030509     C                   Z-ADD     IMDV(CD)      C2VI71
447500030509     C                   ITER
447600030509E   4C                   ENDIF
447700030509IF  4C                   IF        C2VI72 = *ZEROS
447800030509     C                   Z-ADD     CD            C2VC72
447900030509     C                   Z-ADD     IMDV(CD)      C2VI72
448000030509     C                   ITER
448100030509E   4C                   ENDIF
448200030509IF  4C                   IF        C2VI73 = *ZEROS
448300030509     C                   Z-ADD     CD            C2VC73
448400030509     C                   Z-ADD     IMDV(CD)      C2VI73
448500030509     C                   ITER
448600030509E   4C                   ENDIF
448700030509IF  4C                   IF        C2VI74 = *ZEROS
448800030509     C                   Z-ADD     CD            C2VC74
448900030509     C                   Z-ADD     IMDV(CD)      C2VI74
449000030509     C                   ITER
449100030509E   4C                   ENDIF
449200030509IF  4C                   IF        C2VI75 = *ZEROS
449300030509     C                   Z-ADD     CD            C2VC75
449400030509     C                   Z-ADD     IMDV(CD)      C2VI75
449500030509     C                   ITER
449600030509E   4C                   ENDIF
449700030509     C*
449800030509E   3C                   ENDIF
449900030509E   2C                   ENDDO
450000030509     C*
450100030509     C     D$DMGS        DIV       100           C2VMES
450200030509E   1C                   ENDIF
450300030509     C*
450400030509     C                   ENDSR
450500030509     C*------------------------------------------------------------------------*
450600030509     C*?AZZC2V - AZZERA I CAMPI DEL FILE ECC2V30C (VOCI COOP ESTENSIONE)
450700030509     C*------------------------------------------------------------------------*
450800030509     C     AZZC2V        BEGSR
450900030509     C*
451000030509     C                   Z-ADD     D$DAAS        C2VAAS
451100030509     C                   Z-ADD     D$DLNP        C2VLNP
451200030509     C                   Z-ADD     D$DNRS        C2VNRS
451300030509     C                   Z-ADD     D$DNSP        C2VNSP
451400030509     C                   MOVEL     D$DTBL        C2VTBL
451500030509     C     D$DMGS        DIV       100           C2VMES
451600030509     C* CODICE
451700030509     C                   Z-ADD     *ZEROS        C2VC51
451800030509     C                   Z-ADD     *ZEROS        C2VC52
451900030509     C                   Z-ADD     *ZEROS        C2VC53
452000030509     C                   Z-ADD     *ZEROS        C2VC54
452100030509     C                   Z-ADD     *ZEROS        C2VC55
452200030509     C                   Z-ADD     *ZEROS        C2VC56
452300030509     C                   Z-ADD     *ZEROS        C2VC57
452400030509     C                   Z-ADD     *ZEROS        C2VC58
452500030509     C                   Z-ADD     *ZEROS        C2VC59
452600030509     C                   Z-ADD     *ZEROS        C2VC60
452700030509     C                   Z-ADD     *ZEROS        C2VC61
452800030509     C                   Z-ADD     *ZEROS        C2VC62
452900030509     C                   Z-ADD     *ZEROS        C2VC63
453000030509     C                   Z-ADD     *ZEROS        C2VC64
453100030509     C                   Z-ADD     *ZEROS        C2VC65
453200030509     C                   Z-ADD     *ZEROS        C2VC66
453300030509     C                   Z-ADD     *ZEROS        C2VC67
453400030509     C                   Z-ADD     *ZEROS        C2VC68
453500030509     C                   Z-ADD     *ZEROS        C2VC69
453600030509     C                   Z-ADD     *ZEROS        C2VC70
453700030509     C                   Z-ADD     *ZEROS        C2VC71
453800030509     C                   Z-ADD     *ZEROS        C2VC72
453900030509     C                   Z-ADD     *ZEROS        C2VC73
454000030509     C                   Z-ADD     *ZEROS        C2VC74
454100030509     C                   Z-ADD     *ZEROS        C2VC75
454200030509     C* IMPORTO
454300030509     C                   Z-ADD     *ZEROS        C2VI51
454400030509     C                   Z-ADD     *ZEROS        C2VI52
454500030509     C                   Z-ADD     *ZEROS        C2VI53
454600030509     C                   Z-ADD     *ZEROS        C2VI54
454700030509     C                   Z-ADD     *ZEROS        C2VI55
454800030509     C                   Z-ADD     *ZEROS        C2VI56
454900030509     C                   Z-ADD     *ZEROS        C2VI57
455000030509     C                   Z-ADD     *ZEROS        C2VI58
455100030509     C                   Z-ADD     *ZEROS        C2VI59
455200030509     C                   Z-ADD     *ZEROS        C2VI60
455300030509     C                   Z-ADD     *ZEROS        C2VI61
455400030509     C                   Z-ADD     *ZEROS        C2VI62
455500030509     C                   Z-ADD     *ZEROS        C2VI63
455600030509     C                   Z-ADD     *ZEROS        C2VI64
455700030509     C                   Z-ADD     *ZEROS        C2VI65
455800030509     C                   Z-ADD     *ZEROS        C2VI66
455900030509     C                   Z-ADD     *ZEROS        C2VI67
456000030509     C                   Z-ADD     *ZEROS        C2VI68
456100030509     C                   Z-ADD     *ZEROS        C2VI69
456200030509     C                   Z-ADD     *ZEROS        C2VI70
456300030509     C                   Z-ADD     *ZEROS        C2VI71
456400030509     C                   Z-ADD     *ZEROS        C2VI72
456500030509     C                   Z-ADD     *ZEROS        C2VI73
456600030509     C                   Z-ADD     *ZEROS        C2VI74
456700030509     C                   Z-ADD     *ZEROS        C2VI75
456800030509     C*
456900030509     C                   ENDSR
457000980313     C*------------------------------------------------------------------------*
457100980325     C*?AGGFIM - AGGIORNA FILE ECFIM30C (MC RIFATTURAZIONE ITL)
457200980313     C*------------------------------------------------------------------------*
457300980313     C     AGGFIM        BEGSR
457400980313     C*
457500980313     C* SOLO SE PRIMA O DOPO SONO DA GESTIRE
457600980313IF  1C     WMPREG        IFEQ      'S'
457700980313     C     WMDREG        OREQ      'S'
457800980313     C*
457900980317     C* SOLO SE MC TOTALI LETTI DA ECFIM (CON DATA=0 E NON) DIVERSI MC DOPO
458000980323IF  2C     WPSMCT        IFNE      WDSMC
458100980313     C*
458200980317     C* TROVA DIFFERENZA DA SCRIVERE SUL FILE (VOLUME DOPO - VOLUME LETTO DA
458300980317     C* ECFIM SENZA REC CON DATA = 0)
458400980323     C     WDSMC         SUB       WPSMC         WSMC
458500980313     C*
458600980317     C                   Z-ADD     D$DAAS        FIMAAS
458700980317     C                   Z-ADD     D$DLNP        FIMLNP
458800980317     C                   Z-ADD     D$DNRS        FIMNRS
458900980317     C                   Z-ADD     D$DNSP        FIMNSP
459000980317     C                   MOVEL     D$DTBL        FIMTBL
459100980317     C                   Z-ADD     *ZEROS        FIMDDO
459200980325     C     KEYFIM        CHAIN     ECFIM30C                           99
459300980317     C* RECORD GIA' ESISTENTE
459400980317IF  3C     *IN99         IFEQ      '0'
459500980317     C* SE DIFFERENZA = ZERO CANC. REC
459600980317IF  4C     WSMC          IFEQ      *ZEROS
459700980317     C                   DELETE    ECFIM000
459800990408X   4C                   ELSE
459900980317     C* SE DIFF. <> ZERO RISCRIVO RECORD CON NUOVA DIFFERENZA
460000980319     C                   Z-ADD     D$DTFP        FIMTFP
460100980319     C                   Z-ADD     D$DTFA        FIMTFA
460200980317     C                   Z-ADD     WSMC          FIMSMC
460300980317     C                   UPDATE    ECFIM000
460400980317E   4C                   ENDIF
460500980317X   3C                   ELSE
460600980317     C* RECORD NON ESISTENTE, SCRIVO
460700980319     C                   Z-ADD     D$DTFP        FIMTFP
460800980319     C                   Z-ADD     D$DTFA        FIMTFA
460900980317     C                   Z-ADD     WSMC          FIMSMC
461000990408     C                   Z-ADD     D$DMGS        DSMGS
461100990408     C                   Z-ADD     DSMES         FIMMES
461200980317     C                   WRITE     ECFIM000
461300980317E   3C                   ENDIF
461400980313E   2C                   ENDIF
461500980313E   1C                   ENDIF
461600950913     C*
461700950913     C                   ENDSR
461800020531     C*------------------------------------------------------------------------*
461900020531     C*?AGGFCE - AGGIORNA IL FILE FIFCE00F (CONTEGGI PADRONCINI)
462000020531     C*------------------------------------------------------------------------*
462100020531     C     AGGFCE        BEGSR
462200020531     C*
462300020531     C* SOLO SE FLAG AGG. CONTO ECON. = SI
462400020531IF  1C     D$GFCE        IFEQ      'S'
462500020531     C*---
462600020531     C* RITIRI
462700020822     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
462800020531     C* . Controlla che il padroncino ritiro sia un dipendente
462900031031     C* . Controlla che la LNP sia uguale alla linea del padroncino
463000020531     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
463100020531     C*---
463200020822IF  2C                   IF        (D$DDRT > §BLCCLR) OR
463300020822     C                             (D$DDRT = *zeros)
463400021205     C                   MOVEL     'A'           APDTIP
463500021205     C                   Z-ADD     D$DPDR        APDPDR
463600021205     C     KEYAPD        CHAIN     FIAPD01L                           99
463700031031     C                   MOVEL     D$DPDR        N3                3 0
463800020822IF  3C     *IN99         IFEQ      *OFF
463900040510     C*    APDPDD        ANDEQ     'S'
464000031031     C     N3            ANDEQ     D$DLNP
464100040510     c* creo la spia nel FIFCE se padroncino escluso da autofatturazione
464200040511     c* oppure se padroncino non escluso ma tipo bolla FY modifica
464300040510     c* il cod. autotrasp. con uno fittizio
464400040510   a3c                   if        apdpdd = 'S' or
464500040511     c                             (apdpdd <>'S' and d$dcbo = 'FY')
464600020531     C                   MOVEL     D$DPDR        N3                3 0
464700020531     C                   Z-ADD     N3            FCEFGS
464800040510     c                   if        apdpdd = 'S'
464900020531     C                   Z-ADD     D$DPDR        FCEPDR
465000040510     c                   else
465100040510     c                   movel     fcefgs        fcepdr
465200040510     c                   move      0998          fcepdr
465300040510     c                   end
465400020531     C                   MOVEL     'R'           FCETSR
465500020531     C                   EVAL      FCENDC = (5000 + D$DNRT)
465600020531     C                   Z-ADD     D$DDRT        FCEDDC
465700020531     C                   Z-ADD     D$DAAS        FCEAAS
465800020531     C                   Z-ADD     D$DLNP        FCELNP
465900020531     C                   Z-ADD     D$DNRS        FCENRS
466000020531     C                   Z-ADD     D$DNSP        FCENSP
466100020531     C                   MOVEL     '001'         FCECCE
466200020531     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
466300020822IF  4C     *IN99         IFEQ      *ON
466400020531     C                   Z-ADD     *ZEROS        FCEICE
466500020531     C                   MOVEL     *BLANKS       FCESAL
466600030404     C                   MOVEL     D$DCBO        FCECBO
466700020531     C                   WRITE     FIFCE000
466800020822E   4C                   ENDIF
466900040510E  a3C                   ENDIF
467000040510E   3C                   ENDIF
467100020822E   2C                   ENDIF
467200020531     C*---
467300020531     C* CONSEGNE
467400040512     C* . Gestisce solo le bolle consegnate tranne x i dirottamenti
467500040512     C* . che scrive la spia su fifce x pagare le competenze di consegna
467600040512     C* . anche alla bolla originale
467700020822     C* . Controlla se la variazione interessa (dopo l'ultima contabilzzazione)
467800020531     C* . Controlla che il padroncino consegna sia un dipendene
467900031031     C* . Controlla che la LNA sia uguale alla linea del padroncino
468000020531     C* . Scrive un record a zero nei conteggi padroncini solo se NON esiste già
468100020531     C*---
468200040607IF  2C                   IF        d$ddcm > 0 or (D$GSTS='U' and
468300040607IF  2C                             d$ddcm = 0 and d$dcca = '1' and
468400040512IF  2C                              d$dddc > 0 and d$pddc = 0)
468500020822IF  3C                   IF        (D$DDDC > §BLCCLR) OR
468600020822     C                             (D$DDDC = *zeros)
468700021205     C                   MOVEL     'A'           APDTIP
468800021205     C                   Z-ADD     D$DPDC        APDPDR
468900021205     C     KEYAPD        CHAIN     FIAPD01L                           99
469000031031     C                   MOVEL     D$DPDC        N3                3 0
469100020822IF  4C     *IN99         IFEQ      *OFF
469200020531     C     APDPDD        ANDEQ     'S'
469300031031     C     N3            ANDEQ     D$DLNA
469400031113IF  5C     D$GSTS        IFEQ      'U'
469500031113     C     D$PDCM        ANDEQ     *ZEROS
469600020531     C                   MOVEL     D$DPDC        N3                3 0
469700020531     C                   Z-ADD     N3            FCEFGS
469800020531     C                   Z-ADD     D$DPDC        FCEPDR
469900020531     C                   MOVEL     'C'           FCETSR
470000020531     C                   Z-ADD     D$DNDC        FCENDC
470100020531     C                   Z-ADD     D$DDDC        FCEDDC
470200020531     C                   Z-ADD     D$DAAS        FCEAAS
470300020531     C                   Z-ADD     D$DLNP        FCELNP
470400020531     C                   Z-ADD     D$DNRS        FCENRS
470500020531     C                   Z-ADD     D$DNSP        FCENSP
470600020531     C                   MOVEL     '005'         FCECCE
470700020531     C     KEYFCE        CHAIN(N)  FIFCE01L                           99
470800031113IF  6C     *IN99         IFEQ      *ON
470900020531     C                   Z-ADD     *ZEROS        FCEICE
471000020531     C                   MOVEL     *BLANKS       FCESAL
471100030404     C                   MOVEL     D$DCBO        FCECBO
471200020531     C                   WRITE     FIFCE000
471300031113E   6C                   ENDIF
471400031113E   5C                   ENDIF
471500020822E   4C                   ENDIF
471600020531E   3C                   ENDIF
471700020531E   2C                   ENDIF
471800020531     C*
471900020531E   1C                   ENDIF
472000020531     C*
472100020531     C                   ENDSR
472200980317     C*------------------------------------------------------------------------*
472300980325     C*?AGGFIC - AGGIORNA FILE ECFIC30C (COMPAET. RIFATTURAZIONE BAR/SDI)
472400980317     C*------------------------------------------------------------------------*
472500011127     C*---       AGGFIC    BEGSR
472600011127     C*---
472700011127     C*-SOLO SE PRIMA O DOPO SONO DA GESTIRE
472800011127IF  1C*---       WIPREG    IFEQ 'S'
472900011127     C*---       WIDREG    OREQ 'S'
473000011127     C*---
473100011127     C*-SOLO SE LE COMPET.LETTE DA ECFIC (CON DATA=0 E NON) DIVERSE COMPET.DOPO
473200011127IF  2C*---       WPSCOT    IFNE WDSCO                      *COMPET.
473300011127     C*---
473400011127     C*-TROVA DIFFERENZA DA SCRIVERE SUL FILE (COMPET.DOPO - COMPET.LETTE DA
473500011127     C*-ECFIC SENZA REC CON DATA = 0)
473600011127     C*---       WDSCO     SUB  WPSCO     WSCO             *MC DA SCRIVERE
473700011127     C*---
473800011127     C*---                 Z-ADDD$DAAS    FICAAS           ANNO SPEDIZIONE
473900011127     C*---                 Z-ADDD$DLNP    FICLNP           LINEA DI PARTENZA
474000011127     C*---                 Z-ADDD$DNRS    FICNRS           NUMERO SERIE
474100011127     C*---                 Z-ADDD$DNSP    FICNSP           NUMERO SPEDIZIONE
474200011127     C*---                 MOVELD$DTBL    FICTBL           TIPO BOLLA
474300011127     C*---                 Z-ADD*ZEROS    FICDDO           DATA DOCUMENTO
474400011127     C*---       KEYFIC    CHAINECFIC30C             99
474500011127     C*-RECORD GIA' ESISTENTE
474600011127IF  3C*---       *IN99     IFEQ '0'
474700011127     C*-SE DIFFERENZA = ZERO CANC. REC
474800011127IF  4C*---       WSCO      IFEQ *ZEROS
474900011127     C*---                 DELETECFIC000
475000011127X   4C*---                 ELSE
475100011127     C*-SE DIFF. <> ZERO RISCRIVO RECORD CON NUOVA DIFFERENZA
475200011127     C*---                 Z-ADDWSCO      FICSCO           *SALDO COMPET.
475300011127     C*---                 UPDATECFIC000
475400011127E   4C*---                 ENDIF
475500011127X   3C*---                 ELSE
475600011127     C*-RECORD NON ESISTENTE, SCRIVO
475700011127     C*---                 Z-ADDWSCO      FICSCO           *SALDO COMPET.
475800011127     C*---                 Z-ADDD$DMGS    DSMGS
475900011127     C*---                 Z-ADDDSMES     FICMES           *MESE SPED.
476000011127     C*---                 WRITEECFIC000
476100011127E   3C*---                 ENDIF
476200011127E   2C*---                 ENDIF
476300011127E   1C*---                 ENDIF
476400011127     C*---
476500011127     C*---                 ENDSR
476600940315     C********************************************************************
476700030114     C*   GESCSP - GESTIONE DELLE CONSEGNE PARTICOLARI
476800990823     C*   INPUT: - SCHIERE SVN,VAN + CAMPI WPFTC,WPTC2,WPFGC,WPFCA,WPIAS
476900940330     C*   OUTOUT:- SCHIERE SVX,IVX,NSX,CPX
477000940315     C********************************************************************
477100030114     C     GESCSP        BEGSR
477200941123     C*
477300941123     C* AZZERAMENTI INIZIALI
477400941123     C                   Z-ADD     *ZEROS        P                 6 0
477500941123     C                   MOVEA     *BLANKS       SVX
477600941123     C                   Z-ADD     *ZEROS        IVX
477700941123     C                   Z-ADD     *ZEROS        NSX
477800941123     C                   MOVEA     *BLANKS       CPX
477900941123     C                   MOVEL     *BLANKS       WPVP1
478000941123     C                   MOVEL     *BLANKS       WPVP2
478100941123     C*
478200941123     C* CERCO LA VARIA DELLA PRIMA CONS.PARTICOLARE
478300941123     C     WPFTC         IFNE      *BLANKS
478400941123     C                   Z-ADD     1             I
478500941123     C     WPFTC         LOOKUP    C1P(I)                                 99
478600941123     C     *IN99         IFEQ      '1'
478700941123     C                   MOVEL     V1P(I)        WPVP1             1
478800941123     C                   ENDIF
478900941123     C                   ENDIF
479000941123     C*
479100941123     C* CERCO LA VARIA DELLA SECONDA CONS.PARTICOLARE
479200941123     C     WPTC2         IFNE      *BLANKS
479300941123     C                   Z-ADD     1             I
479400941123     C     WPTC2         LOOKUP    C1P(I)                                 99
479500941123     C     *IN99         IFEQ      '1'
479600941123     C                   MOVEL     V1P(I)        WPVP2             1
479700941123     C                   ENDIF
479800941123     C                   ENDIF
479900941123     C*
480000941123     C* PORTA VARIE ED IMPORTI DALLA SCHIERA INPUT ALLA SCHIERA OUTPUT
480100970902DO  1C     1             DO        20            I
480200990831     C* ESCLUDE ALCUNE VARIE PER LA REGISTRAZIONE DU SISDP
480300990831IF  2C     SVN(I)        IFEQ      *BLANKS
480400050119     C*    SVN(I)        OREQ      'T'
480500990831     C     SVN(I)        OREQ      '6'
480600990831     C     SVN(I)        OREQ      '8'
480700990831X   2C                   ELSE
480800941123     C                   ADD       1             P
480900970902IF  3C     SVN(I)        IFEQ      'E'
481000970901     C                   MOVEL     'R'           SVX(P)
481100970902X   3C                   ELSE
481200941130     C                   MOVEL     SVN(I)        SVX(P)
481300970902E   3C                   ENDIF
481400941130     C                   Z-ADD     VAN(I)        IVX(P)
481500970901     C* SE IL N. DI SPED. CON QUESTA VARIA E DETERMINABILE IN ALTRO MODO
481600020620     C* (o come consegna particolare o testando i campi della bolla)
481700970901     C* (VEDI RIGHE SUCCESSIVE) IMPOSTA IL N.DI SPED. DOPO
481800970901     C* ALTRIMENTI FORZA 1
481900970902IF  3C     SVN(I)        IFEQ      'A'
482000970901     C     SVN(I)        OREQ      §$2VRB
482100970901     C     SVN(I)        OREQ      'F'
482200970901     C     SVN(I)        OREQ      §$2VRG
482300970901     C     SVN(I)        OREQ      §$2VRI
482400970901     C     SVN(I)        OREQ      §$2VRJ
482500970901     C     SVN(I)        OREQ      §$2VRK
482600970901     C     SVN(I)        OREQ      'P'
482700970901     C     SVN(I)        OREQ      §$2VRQ
482800970901     C     SVN(I)        OREQ      §$2VRR
482900970901     C     SVN(I)        OREQ      'S'
483000011109     C     SVN(I)        OREQ      '3'
483100011109     C     SVN(I)        OREQ      '4'
483200020620     C     SVN(I)        OREQ      'X'
483300021014     C     SVN(I)        OREQ      §$2VRY
483400021014     C     SVN(I)        OREQ      §$2AST
483500040322     C     SVN(I)        OREQ      §$2vdf
483600040615     C     SVN(I)        OREQ      §$2vro
483700040924     C     SVN(I)        OREQ      §$2pod
483800040616     C     SVN(I)        OREQ      Wpvp1
483900040616     C     SVN(I)        OREQ      Wpvp2
484000050119     C     SVN(I)        OREQ      §$2vrt
484100120301     C     SVN(I)        OREQ      §$2VAD
484200970902X   3C                   ELSE
484300020620     C*
484400020620     c* QUI CI STANNO :
484500020620     C* - Diritto di pesatura (in TNTAM);
484600020620     C*
484700970901     C                   Z-ADD     1             NSX(P)
484800970902E   3C                   ENDIF
484900970902E   2C                   ENDIF
485000970902E   1C                   ENDDO
485100970901     C*
485200970901     C* PORTA VALORI FISSI DEI RICAVI DELLA BOLLA ALLA SCHIERA OUTPUT
485300990823     C********** WPDIF     IFNE *ZEROS                     *DIRITTO FISSO
485400990823     C******************** ADD  1         P
485500990823     C******************** MOVEL§$2VRH    SVX,P
485600990823     C******************** Z-ADDWPDIF     IVX,P
485700990823     C******************** Z-ADD1         NSX,P
485800990823     C******************** ENDIF
485900990823     C********** WPANT     IFNE *ZEROS                     *ANTICIPATA
486000990823     C******************** ADD  1         P
486100990823     C******************** MOVEL§$2VRT    SVX,P
486200990823     C******************** Z-ADDWPANT     IVX,P
486300990823     C******************** Z-ADD1         NSX,P
486400990823     C******************** ENDIF
486500970901     C     WPPOR         IFNE      *ZEROS
486600970901     C                   ADD       1             P
486700970901     C                   MOVEL     §$2FPO        SVX(P)
486800970901     C                   Z-ADD     WPPOR         IVX(P)
486900970901     C                   Z-ADD     1             NSX(P)
487000970901     C                   ENDIF
487100990823     C********** WPINL     IFNE *ZEROS                     *INOLTRO
487200990823     C******************** ADD  1         P
487300990823     C******************** MOVEL§$2FIN    SVX,P
487400990823     C******************** Z-ADDWPINL     IVX,P
487500990823     C******************** Z-ADD1         NSX,P
487600990823     C******************** ENDIF
487700941123     C*
487800941123     C* PRIMA CONS.PARTICOLARE
487900941123     C     WPVP1         IFNE      *BLANKS
488000941123     C                   Z-ADD     1             I
488100941123     C     WPVP1         LOOKUP    SVX(I)                                 99
488200941123     C     *IN99         IFEQ      '1'
488300941123     C                   ADD       1             NSX(I)
488400941123     C                   MOVEL     WPFTC         CPX(I)
488500941123     C                   ELSE
488600941123     C                   ADD       1             P
488700941123     C                   MOVEL     WPVP1         SVX(P)
488800941123     C                   Z-ADD     1             NSX(P)
488900941123     C                   MOVEL     WPFTC         CPX(P)
489000941123     C                   ENDIF
489100941123     C                   ENDIF
489200941123     C*
489300941123     C* SECONDA CONS.PARTICOLARE
489400941123     C     WPVP2         IFNE      *BLANKS
489500941123     C                   Z-ADD     1             I
489600941123     C     WPVP2         LOOKUP    SVX(I)                                 99
489700941123     C     *IN99         IFEQ      '1'
489800941123     C                   ADD       1             NSX(I)
489900941123     C                   MOVEL     WPTC2         CPX(I)
490000941123     C                   ELSE
490100941123     C                   ADD       1             P
490200941123     C                   MOVEL     WPVP2         SVX(P)
490300941123     C                   Z-ADD     1             NSX(P)
490400941123     C                   MOVEL     WPTC2         CPX(P)
490500941123     C                   ENDIF
490600941123     C                   ENDIF
490700941123     C*
490800941123     C* GIACENZA
490900941123     C     WPFGC         IFEQ      'S'
491000941123     C                   Z-ADD     1             I
491100970901     C     §$2VRI        LOOKUP    SVX(I)                                 99
491200941123     C     *IN99         IFEQ      '1'
491300941123     C                   ADD       1             NSX(I)
491400941123     C                   ELSE
491500941123     C                   ADD       1             P
491600970901     C                   MOVEL     §$2VRI        SVX(P)
491700941123     C                   Z-ADD     1             NSX(P)
491800941123     C                   ENDIF
491900941123     C                   ENDIF
492000040322     C*
492100040323     C* numero Lasciato avviso SOLO A PARTIRE DA APRILE 2004
492200040322     C                   IF        %subst(wpflo: 6: 1) <> ' '
492300040323     C                             AND (WPDATCOP > 20040400 OR
492400040323     C                             WDDATCOP > 20040400)
492500040322     c                   eval      dtasflo = wpflo
492600040322     c                   move      §flonav       contavv
492700040322     C                   Z-ADD     1             I
492800040322     C     §$2Vdf        LOOKUP    SVX(I)                                 99
492900040322     C     *IN99         IFEQ      '1'
493000040322     C                   ADD       contavv       NSX(I)
493100040322     C                   ELSE
493200040322     C                   ADD       1             P
493300040322     C                   MOVEL     §$2Vdf        SVX(P)
493400040322     C                   Z-ADD     contavv       NSX(P)
493500040322     C                   ENDIF
493600040322     C                   ENDIF
493700120301     C*
493800120301     C* numero mail destinatario
493900120301     C                   IF        %subst(wpflo: 9: 1) <> ' '
494000120301     C                   Z-ADD     1             I
494100120301     C     §$2VAD        LOOKUP    SVX(I)                                 99
494200120301     C     *IN99         IFEQ      '1'
494300120301     C                   ADD       1             NSX(I)
494400120301     C                   ELSE
494500120301     C                   ADD       1             P
494600120301     C                   MOVEL     §$2VAD        SVX(P)
494700120301     C                   Z-ADD     1             NSX(P)
494800120301     C                   ENDIF
494900120301     C                   ENDIF
495000941123     C*
495100941123     C* CONTRASSEGNO
495200941123     C     WPFCA         IFEQ      'S'
495300040524     C     WPsta         andne     ' '
495400941123     C                   Z-ADD     1             I
495500970901     C     §$2VRG        LOOKUP    SVX(I)                                 99
495600941123     C     *IN99         IFEQ      '1'
495700941123     C                   ADD       1             NSX(I)
495800941123     C                   ELSE
495900941123     C                   ADD       1             P
496000970901     C                   MOVEL     §$2VRG        SVX(P)
496100941123     C                   Z-ADD     1             NSX(P)
496200941123     C                   ENDIF
496300941123     C                   ENDIF
496400941123     C*
496500941123     C* ASSIC. X CONTO
496600990823     C********** WPFAS     IFEQ 'S'
496700990823     C     WPIAS         IFGT      *ZEROS
496800941123     C                   Z-ADD     1             I
496900970901     C     §$2VRR        LOOKUP    SVX(I)                                 99
497000941123     C     *IN99         IFEQ      '1'
497100941123     C                   ADD       1             NSX(I)
497200941123     C                   ELSE
497300941123     C                   ADD       1             P
497400970901     C                   MOVEL     §$2VRR        SVX(P)
497500941123     C                   Z-ADD     1             NSX(P)
497600941123     C                   ENDIF
497700941123     C                   ENDIF
497800970901     C*
497900970901     C* CONSEGNA DDT -VARIA B-
498000970901     C     WPLL1         IFEQ      'S'
498100970901     C     WPLL1         OREQ      'C'
498200970901     C                   Z-ADD     1             I
498300970901     C     §$2VRB        LOOKUP    SVX(I)                                 99
498400970901     C     *IN99         IFEQ      '1'
498500970901     C                   ADD       1             NSX(I)
498600970901     C                   ELSE
498700970901     C                   ADD       1             P
498800970901     C                   MOVEL     §$2VRB        SVX(P)
498900970901     C                   Z-ADD     1             NSX(P)
499000970901     C                   ENDIF
499100970901     C                   ENDIF
499200970901     C*
499300970901     C* ISOLA -VARIA J-
499400970902     C     WPDFT         IFGT      19970713
499500970901     C     WPTPO         IFEQ      'F'
499600970901     C     WPFIN         ANDEQ     'I'
499700970901     C     WPTPO         OREQ      'A'
499800970901     C     WPFAP         ANDEQ     'I'
499900970901     C                   Z-ADD     1             I
500000970901     C     §$2VRJ        LOOKUP    SVX(I)                                 99
500100970901     C     *IN99         IFEQ      '1'
500200970901     C                   ADD       1             NSX(I)
500300970901     C                   ELSE
500400970901     C                   ADD       1             P
500500970901     C                   MOVEL     §$2VRJ        SVX(P)
500600970901     C                   Z-ADD     1             NSX(P)
500700970901     C                   ENDIF
500800970901     C                   ENDIF
500900970901     C                   ENDIF
501000970901     C*
501100970901     C* LOC.DISAGIATE -VARIA K-
501200970901     C     WPTPO         IFEQ      'F'
501300970901     C     WPFIN         ANDEQ     'D'
501400970901     C     WPTPO         OREQ      'A'
501500970901     C     WPFAP         ANDEQ     'D'
501600970901     C                   Z-ADD     1             I
501700970901     C     §$2VRK        LOOKUP    SVX(I)                                 99
501800970901     C     *IN99         IFEQ      '1'
501900970901     C                   ADD       1             NSX(I)
502000970901     C                   ELSE
502100970901     C                   ADD       1             P
502200970901     C                   MOVEL     §$2VRK        SVX(P)
502300970901     C                   Z-ADD     1             NSX(P)
502400970901     C                   ENDIF
502500970901     C                   ENDIF
502600970901     C*
502700110919     c* non conteggiamo + la città ma solo la ZTL della città o della
502800110919     c* provincia (da ottobre 2011)
502900110919     C* ZTL -VARIA Q-
503000110919     C*    WPTPO         IFEQ      'F'
503100110919     C*    WPFIN         ANDEQ     'C'
503200110919     C*    WPTPO         OREQ      'A'
503300110919     C*    WPFAP         ANDEQ     'C'
503400110919     c                   if        (wptpo = 'F' and (wpfin = 'Z' or
503500110919     c                             wpfin = 'T')) or
503600110919     c                             (wptpo = 'A' and (wpfap = 'Z' or
503700110919     c                             wpfap = 'T'))
503800970901     C                   Z-ADD     1             I
503900970901     C     §$2VRQ        LOOKUP    SVX(I)                                 99
504000970901     C     *IN99         IFEQ      '1'
504100970901     C                   ADD       1             NSX(I)
504200970901     C                   ELSE
504300970901     C                   ADD       1             P
504400970901     C                   MOVEL     §$2VRQ        SVX(P)
504500970901     C                   Z-ADD     1             NSX(P)
504600970901     C                   ENDIF
504700970901     C                   ENDIF
504800020620     C*
504900020620     C* RICERCA DOCUMENTO -VARIA O-
505000040617     C*    WPTBL         IFEQ      'A3'
505100040617     C*    WPCBO         ANDEQ     'FO'
505200040617     C                   IF        WPTBL = 'A3' AND (WPCBO = 'FO' OR
505300040617     C                             WPCBO = 'FF')
505400020620     C                   Z-ADD     1             I
505500020620     C     §$2VRO        LOOKUP    SVX(I)                                 99
505600020620     C     *IN99         IFEQ      '1'
505700020620     C                   ADD       1             NSX(I)
505800020620     C                   ELSE
505900020620     C                   ADD       1             P
506000020620     C                   MOVEL     §$2VRO        SVX(P)
506100020620     C                   Z-ADD     1             NSX(P)
506200020620     C                   ENDIF
506300020620     C                   ENDIF
506400020620     C*
506500020620     C* RITIRO ANNULLATO -VARIA Y-
506600020620     C     WPTBL         IFEQ      'A3'
506700020620     C     WPCBO         ANDEQ     'FY'
506800020620     C                   Z-ADD     1             I
506900020620     C     §$2VRY        LOOKUP    SVX(I)                                 99
507000020620     C     *IN99         IFEQ      '1'
507100020620     C                   ADD       1             NSX(I)
507200020620     C                   ELSE
507300020620     C                   ADD       1             P
507400020620     C                   MOVEL     §$2VRY        SVX(P)
507500020620     C                   Z-ADD     1             NSX(P)
507600020620     C                   ENDIF
507700020620     C                   ENDIF
507800020620     C*
507900020620     C* DIROTTAMENTO -VARIA *-
508000020620     C     WPTBL         IFEQ      'A3'
508100020620     C     WPCBO         ANDEQ     'F*'
508200020620     C                   Z-ADD     1             I
508300020620     C     §$2AST        LOOKUP    SVX(I)                                 99
508400020620     C     *IN99         IFEQ      '1'
508500020620     C                   ADD       1             NSX(I)
508600020620     C                   ELSE
508700020620     C                   ADD       1             P
508800020620     C                   MOVEL     §$2AST        SVX(P)
508900020620     C                   Z-ADD     1             NSX(P)
509000020620     C                   ENDIF
509100020620     C                   ENDIF
509200040924     C*
509300040924     C* P.O.D Immage -VARIA a-
509400090304     c* per pod immage conto i colli e non le spedizioni perchè ci sarà
509500090304     c* una spedizione per enne immagini
509600040924     C     WPTBL         IFEQ      'A3'
509700040924     C     WPCBO         ANDEQ     'FI'
509800040924     C                   Z-ADD     1             I
509900040924     C     §$2pod        LOOKUP    SVX(I)                                 99
510000040924     C     *IN99         IFEQ      '1'
510100090304     C                   ADD       wpncl         NSX(I)
510200040924     C                   ELSE
510300040924     C                   ADD       1             P
510400040924     C                   MOVEL     §$2pod        SVX(P)
510500090304     C                   Z-ADD     wpncl         NSX(P)
510600040924     C                   ENDIF
510700040924     C                   ENDIF
510800050119     C*
510900050119     C* ORM commissionati
511000050119     C     WPTBL         IFEQ      'A3'
511100050119     C     WPCBO         ANDEQ     'FT'
511200050119     C                   Z-ADD     1             I
511300050119     C     §$2vrt        LOOKUP    SVX(I)                                 99
511400050119     C     *IN99         IFEQ      '1'
511500050119     C                   ADD       1             NSX(I)
511600050119     C                   ELSE
511700050119     C                   ADD       1             P
511800050119     C                   MOVEL     §$2vrt        SVX(P)
511900050119     C                   Z-ADD     1             NSX(P)
512000050119     C                   ENDIF
512100050119     C                   ENDIF
512200941123     C*
512300990617     C* SE E' STATO SCRITTO ALMENO UN DATO IN SCHIERA, 'S' -> FLAG REG. SISDP
512400941123     C     P             IFGT      *ZEROS
512500941123     C                   MOVEL     'S'           WPREG             1
512600941123     C                   ELSE
512700941123     C                   MOVEL     'N'           WPREG
512800941123     C                   ENDIF
512900020705     c*
513000020705     C* Se divisa <> divisa C/E, applica il cambio della divisa C/E (YEURCO).
513100020104DO  1C     1             DO        20            I
513200020104IF  2C     SVX(I)        IFNE      *BLANKS
513300020104IF  3C     IVX(I)        IFNE      *ZEROS
513400020104IF  5C     WPDIV         IFNE      WDIVCE
513500011219     C                   Z-ADD     IVX(I)        §IMI
513600011219     C                   MOVEL     WPDIV         §DVI
513700011219     C                   EXSR      CNVEUR
513800011219     C                   Z-ADD     §IMO          IVX(I)
513900020104E   5C                   ENDIF
514000020104E   3C                   ENDIF
514100020104E   2C                   ENDIF
514200020104E   1C                   ENDDO
514300940411     C*
514400940315     C                   ENDSR
514500930323     C********************************************************************
514600990617     C*   GESSDR - GESTISCE IL FILE SISDR00F (SALDI BOLLE)
514700930323     C********************************************************************
514800931104     C     GESSDR        BEGSR
514900940411     C*
515000090304     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO regREGISTRARE
515100940411     C     WRPREG        IFEQ      'S'
515200940411     C     WRDREG        OREQ      'S'
515300931111     C*
515400931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
515500940407     C     D$GSTS        IFEQ      'U'
515600940407     C     D$PAAS        ANDEQ     D$DAAS
515700931111     C     WPMES         ANDEQ     WDMES
515800931111     C     D$PLNP        ANDEQ     D$DLNP
515900931111     C     D$PLNA        ANDEQ     D$DLNA
516000940103     C     WPFPR         ANDEQ     WDFPR
516100931111     C     D$PTSP        ANDEQ     D$DTSP
516200941201     C     D$PTPO        ANDEQ     D$DTPO
516300941201     C     D$PRBL        ANDEQ     D$DRBL
516400011219     C     WPTPR         ANDEQ     WDTPR
516500940411     C     WRPREG        ANDEQ     WRDREG
516600931111     C*
516700931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
516800931111     C*
516900931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
517000931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
517100931110     C                   Z-ADD     D$DAAS        SDRANN
517200931111     C                   Z-ADD     WDMES         SDRMES
517300931111     C                   Z-ADD     D$DLNP        SDRFLP
517400931110     C                   Z-ADD     D$DLNA        SDRFLA
517500940103     C                   MOVEL     WDFPR         SDRFPE
517600931111     C                   MOVEL     D$DTSP        SDRTSP
517700941201     C                   MOVEL     D$DTPO        SDRTPO
517800941201     C                   MOVEL     D$DRBL        SDRRBL
517900931111     C                   MOVEL     WDTPR         SDRTPR
518000931111     C* VALORI = 'DOPO' - 'PRIMA'
518100940512     C                   Z-ADD     *ZEROS        WRNSP
518200020829     C*--- D$DPKF        SUB       D$PPKF        WRPKG
518300020829     C     WDPES         SUB       WPPES         WRPES
518400931111     C     D$DNCL        SUB       D$PNCL        WRNCO
518500931111     C     WDVOL         SUB       WPVOL         WRVOL
518600931111     C     WDPTS         SUB       WPPTS         WRPTS
518700931111     C     WDIRB         SUB       WPIRB         WRIRB
518800931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
518900931111     C                   EXSR      AGGSDR
519000931111     C*
519100931111     C                   ELSE
519200931111     C*
519300931111     C******************** E' CAMBIATA LA CHIAVE *************************
519400931111     C*
519500940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
519600940405     C     D$GSTS        IFEQ      'U'
519700940405     C     D$GSTS        OREQ      'D'
519800931111     C*
519900931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
520000931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
520100931111     C* CHIAVE = CHIAVE 'PRIMA'
520200931111     C                   Z-ADD     D$PAAS        SDRANN
520300931111     C                   Z-ADD     WPMES         SDRMES
520400931111     C                   Z-ADD     D$PLNP        SDRFLP
520500931111     C                   Z-ADD     D$PLNA        SDRFLA
520600940103     C                   MOVEL     WPFPR         SDRFPE
520700931111     C                   MOVEL     D$PTSP        SDRTSP
520800941201     C                   MOVEL     D$PTPO        SDRTPO
520900941201     C                   MOVEL     D$PRBL        SDRRBL
521000931111     C                   MOVEL     WPTPR         SDRTPR
521100931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
521200931111     C     *ZEROS        SUB       1             WRNSP
521300020829     C*--- *ZEROS        SUB       D$PPKF        WRPKG
521400020829     C     *ZEROS        SUB       WPPES         WRPES
521500931111     C     *ZEROS        SUB       D$PNCL        WRNCO
521600931111     C     *ZEROS        SUB       WPVOL         WRVOL
521700931111     C     *ZEROS        SUB       WPPTS         WRPTS
521800931111     C     *ZEROS        SUB       WPIRB         WRIRB
521900931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
522000940411     C     WRPREG        IFEQ      'S'
522100931111     C                   EXSR      AGGSDR
522200940411     C                   ENDIF
522300931111     C*
522400931111     C                   END
522500940202     C*
522600940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
522700940405     C     D$GSTS        IFEQ      'U'
522800940405     C     D$GSTS        OREQ      'I'
522900931111     C*
523000940202     C* NON E' UN ANNULLAMENTO, QUINDI
523100931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
523200931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
523300931111     C* CHIAVE = CHIAVE 'DOPO'
523400931111     C                   Z-ADD     D$DAAS        SDRANN
523500931111     C                   Z-ADD     WDMES         SDRMES
523600931111     C                   Z-ADD     D$DLNP        SDRFLP
523700931111     C                   Z-ADD     D$DLNA        SDRFLA
523800940103     C                   MOVEL     WDFPR         SDRFPE
523900931111     C                   MOVEL     D$DTSP        SDRTSP
524000941201     C                   MOVEL     D$DTPO        SDRTPO
524100941201     C                   MOVEL     D$DRBL        SDRRBL
524200931111     C                   MOVEL     WDTPR         SDRTPR
524300931111     C* VALORI = VALORI 'DOPO'
524400931111     C                   Z-ADD     1             WRNSP
524500020829     C*---               Z-ADD     D$DPKF        WRPKG
524600020829     C                   Z-ADD     WDPES         WRPES
524700931111     C                   Z-ADD     D$DNCL        WRNCO
524800931111     C                   Z-ADD     WDVOL         WRVOL
524900931111     C                   Z-ADD     WDPTS         WRPTS
525000931111     C                   Z-ADD     WDIRB         WRIRB
525100931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
525200940411     C     WRDREG        IFEQ      'S'
525300931111     C                   EXSR      AGGSDR
525400940411     C                   ENDIF
525500931111     C*
525600940202     C                   END
525700931111     C                   END
525800940411     C                   END
525900930323     C*
526000930323     C                   ENDSR
526100931111     C********************************************************************
526200990617     C*   AGGSDR - AGGIORNA IL FILE SISDR00F (SALDI BOLLE)
526300931111     C********************************************************************
526400931111     C     AGGSDR        BEGSR
526500931112     C     WRNSP         IFEQ      *ZEROS
526600020829     C*--- WRPKG         ANDEQ     *ZEROS
526700020829     C     WRPES         ANDEQ     *ZEROS
526800931112     C     WRNCO         ANDEQ     *ZEROS
526900931112     C     WRVOL         ANDEQ     *ZEROS
527000931112     C     WRPTS         ANDEQ     *ZEROS
527100931112     C     WRIRB         ANDEQ     *ZEROS
527200931112     C                   ELSE
527300990617     C     KEYSDR        CHAIN     SISDR01L                           99
527400931111     C     *IN99         IFEQ      '0'
527500931111     C                   EXSR      ADDSDR
527600990617     C                   UPDATE    SISDR000
527700931111     C                   ELSE
527800931111     C                   EXSR      AZZSDR
527900931111     C                   EXSR      ADDSDR
528000990617     C                   WRITE     SISDR000
528100931111     C                   END
528200931112     C                   END
528300931111     C                   ENDSR
528400931111     C********************************************************************
528500990617     C*   AZZSDR - AZZERA I CAMPI DEL FILE SISDR00F (SALDI BOLLE)
528600931111     C********************************************************************
528700931111     C     AZZSDR        BEGSR
528800931111     C                   Z-ADD     *ZEROS        SDRNSP
528900931111     C                   Z-ADD     *ZEROS        SDRPKG
529000931111     C                   Z-ADD     *ZEROS        SDRNCO
529100931111     C                   Z-ADD     *ZEROS        SDRVOL
529200931111     C                   Z-ADD     *ZEROS        SDRPTS
529300931111     C                   Z-ADD     *ZEROS        SDRIRB
529400931111     C                   Z-ADD     *ZEROS        SDRIRR
529500931111     C                   Z-ADD     *ZEROS        SDRINC
529600931111     C                   ENDSR
529700931111     C********************************************************************
529800990617     C*   ADDSDR - AGGIUNGE AI VALORI DEL FILE SISDR00F (SALDI BOLLE)
529900931111     C********************************************************************
530000931111     C     ADDSDR        BEGSR
530100931111     C                   ADD       WRNSP         SDRNSP
530200020829     C*---               ADD       WRPKG         SDRPKG
530300020829     C                   ADD       WRPES         SDRPKG
530400931111     C                   ADD       WRNCO         SDRNCO
530500931111     C                   ADD       WRVOL         SDRVOL
530600931111     C                   ADD       WRPTS         SDRPTS
530700020129     C                   ADD       WRIRB         SDRIRB
530800940317     C                   MOVEL     'S'           FLAGGS
530900931111     C                   ENDSR
531000931111     C*
531100931111     C*
531200931111     C*
531300931111     C*
531400930420     C********************************************************************
531500990617     C*   GESSDF - GESTISCE IL FILE SISDF00F (SALDI FILIALE)
531600930420     C********************************************************************
531700931104     C     GESSDF        BEGSR
531800940411     C*
531900940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
532000940411     C     WFPREG        IFEQ      'S'
532100940411     C     WFDREG        OREQ      'S'
532200931104     C*
532300931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
532400940407     C     D$GSTS        IFEQ      'U'
532500940407     C     D$PAAS        ANDEQ     D$DAAS
532600931111     C     WPMES         ANDEQ     WDMES
532700931111     C     D$PLNP        ANDEQ     D$DLNP
532800940411     C     WFPREG        ANDEQ     WFDREG
532900931111     C*
533000931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
533100931111     C*
533200931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
533300931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
533400931111     C                   Z-ADD     D$DAAS        SDFANN
533500931111     C                   Z-ADD     WDMES         SDFMES
533600931111     C                   Z-ADD     D$DLNP        SDFFIL
533700931111     C* VALORI = 'DOPO' - 'PRIMA'
533800940512     C                   Z-ADD     *ZEROS        WFNSP
533900020829     C*--- D$DPKF        SUB       D$PPKF        WFPKG
534000020829     C     WDPES         SUB       WPPES         WFPES
534100931111     C     D$DNCL        SUB       D$PNCL        WFNCO
534200931111     C     WDVOL         SUB       WPVOL         WFVOL
534300931111     C     WDPTS         SUB       WPPTS         WFPTS
534400931111     C     WDIRB         SUB       WPIRB         WFIRB
534500931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
534600931111     C                   EXSR      AGGSDF
534700931111     C*
534800931111     C                   ELSE
534900931111     C*
535000931111     C******************** E' CAMBIATA LA CHIAVE *************************
535100931111     C*
535200940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
535300940405     C     D$GSTS        IFEQ      'U'
535400940405     C     D$GSTS        OREQ      'D'
535500931111     C*
535600931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
535700931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
535800931111     C* CHIAVE = CHIAVE 'PRIMA'
535900931111     C                   Z-ADD     D$PAAS        SDFANN
536000931111     C                   Z-ADD     WPMES         SDFMES
536100931111     C                   Z-ADD     D$PLNP        SDFFIL
536200931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
536300931111     C     *ZEROS        SUB       1             WFNSP
536400020829     C*--- *ZEROS        SUB       D$PPKF        WFPKG
536500020829     C     *ZEROS        SUB       WPPES         WFPES
536600931111     C     *ZEROS        SUB       D$PNCL        WFNCO
536700931111     C     *ZEROS        SUB       WPVOL         WFVOL
536800931111     C     *ZEROS        SUB       WPPTS         WFPTS
536900931111     C     *ZEROS        SUB       WPIRB         WFIRB
537000931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
537100940411     C     WFPREG        IFEQ      'S'
537200931111     C                   EXSR      AGGSDF
537300940411     C                   ENDIF
537400931111     C*
537500931111     C                   END
537600940202     C*
537700940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
537800940405     C     D$GSTS        IFEQ      'U'
537900940405     C     D$GSTS        OREQ      'I'
538000931111     C*
538100940202     C* NON E' UN ANNULLAMENTO, QUINDI
538200931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
538300931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
538400931111     C* CHIAVE = CHIAVE 'DOPO'
538500931111     C                   Z-ADD     D$DAAS        SDFANN
538600931111     C                   Z-ADD     WDMES         SDFMES
538700931111     C                   Z-ADD     D$DLNP        SDFFIL
538800931111     C* VALORI = VALORI 'DOPO'
538900931111     C                   Z-ADD     1             WFNSP
539000020829     C*---               Z-ADD     D$DPKF        WFPKG
539100020829     C                   Z-ADD     WDPES         WFPES
539200931111     C                   Z-ADD     D$DNCL        WFNCO
539300931111     C                   Z-ADD     WDVOL         WFVOL
539400931111     C                   Z-ADD     WDPTS         WFPTS
539500931111     C                   Z-ADD     WDIRB         WFIRB
539600931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
539700940411     C     WFDREG        IFEQ      'S'
539800931111     C                   EXSR      AGGSDF
539900940411     C                   ENDIF
540000931111     C*
540100940202     C                   END
540200931111     C                   END
540300940411     C                   END
540400931111     C*
540500931111     C                   ENDSR
540600931111     C********************************************************************
540700990617     C*   AGGSDF - AGGIORNA IL FILE SISDF00F (SALDI FILIALE)
540800931111     C********************************************************************
540900931111     C     AGGSDF        BEGSR
541000931112     C     WFNSP         IFEQ      *ZEROS
541100020829     C*--- WFPKG         ANDEQ     *ZEROS
541200020829     C     WFPES         ANDEQ     *ZEROS
541300931112     C     WFNCO         ANDEQ     *ZEROS
541400931112     C     WFVOL         ANDEQ     *ZEROS
541500931112     C     WFPTS         ANDEQ     *ZEROS
541600931112     C     WFIRB         ANDEQ     *ZEROS
541700931112     C                   ELSE
541800990617     C     KEYSDF        CHAIN     SISDF01L                           99
541900931111     C     *IN99         IFEQ      '0'
542000931111     C                   EXSR      ADDSDF
542100990617     C                   UPDATE    SISDF000
542200931111     C                   ELSE
542300931111     C                   EXSR      AZZSDF
542400931111     C                   EXSR      ADDSDF
542500990617     C                   WRITE     SISDF000
542600931111     C                   END
542700931112     C                   END
542800931111     C                   ENDSR
542900931111     C********************************************************************
543000990617     C*   AZZSDF - AZZERA I CAMPI DEL FILE SISDF00F (SALDI FILIALE)
543100931111     C********************************************************************
543200931111     C     AZZSDF        BEGSR
543300931111     C                   Z-ADD     *ZEROS        SDFNSP
543400931111     C                   Z-ADD     *ZEROS        SDFPKG
543500931111     C                   Z-ADD     *ZEROS        SDFNCO
543600931111     C                   Z-ADD     *ZEROS        SDFVOL
543700931111     C                   Z-ADD     *ZEROS        SDFPTS
543800931111     C                   Z-ADD     *ZEROS        SDFIRB
543900931111     C                   Z-ADD     *ZEROS        SDFIRR
544000931111     C                   Z-ADD     *ZEROS        SDFINC
544100931111     C                   ENDSR
544200931111     C********************************************************************
544300990617     C*   ADDSDF - AGGIUNGE AI VALORI DEL FILE SISDF00F (SALDI FILIALE)
544400931111     C********************************************************************
544500931111     C     ADDSDF        BEGSR
544600931111     C                   ADD       WFNSP         SDFNSP
544700020829     C*---               ADD       WFPKG         SDFPKG
544800020829     C                   ADD       WFPES         SDFPKG
544900931111     C                   ADD       WFNCO         SDFNCO
545000931111     C                   ADD       WFVOL         SDFVOL
545100931111     C                   ADD       WFPTS         SDFPTS
545200020129     C                   ADD       WFIRB         SDFIRB
545300940317     C                   MOVEL     'S'           FLAGGS
545400931111     C                   ENDSR
545500931110     C********************************************************************
545600990617     C*   GESSDT - GESTISCE IL FILE SISDT00F (SALDI BOLLE - TRAZIONI)
545700931110     C********************************************************************
545800931110     C     GESSDT        BEGSR
545900940318     C*
546000940407     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
546100940407     C     WTPREG        IFEQ      'S'
546200940407     C     WTDREG        OREQ      'S'
546300000622     C* IMPOSTA I TERMINAL PARTENZA E ARRIVO NELLE VARIABILI DI LAVORO
546400000705     C                   CLEAR                   LPTFP
546500000705     C                   CLEAR                   LPTFA
546600000705     C                   CLEAR                   LDTFP
546700000705     C                   CLEAR                   LDTFA
546800000705IF  2C     D$GSTS        IFEQ      'U'
546900000705     C     D$GSTS        OREQ      'D'
547000000622     C                   Z-ADD     D$PTFP        LPTFP
547100000622     C                   Z-ADD     D$PTFA        LPTFA
547200000705E   2C                   ENDIF
547300000705IF  2C     D$GSTS        IFEQ      'U'
547400000705     C     D$GSTS        OREQ      'I'
547500000622     C                   Z-ADD     D$DTFP        LDTFP
547600000622     C                   Z-ADD     D$DTFA        LDTFA
547700000705E   2C                   ENDIF
547800000622     C* PRIMA
547900000622     C* SE LINEA POSTE, IL SUO TERMINAL DIVENTA SE STESSA
548000000705IF  2C     D$GSTS        IFEQ      'U'
548100000705     C     D$GSTS        OREQ      'D'
548200000622     C                   Z-ADD     W$PLNP        F
548300020709IF  3C     NTW(F)        IFEQ      'PPT'
548400000622     C                   Z-ADD     W$PLNP        LPTFP
548500000622E   3C                   ENDIF
548600000622     C                   Z-ADD     D$PLNA        F
548700020709IF  3C     NTW(F)        IFEQ      'PPT'
548800000622     C                   Z-ADD     D$PLNA        LPTFA
548900000622E   3C                   ENDIF
549000000622E   2C                   ENDIF
549100000622     C* DOPO
549200000622     C* SE LINEA POSTE, IL SUO TERMINAL DIVENTA SE STESSA
549300000705IF  2C     D$GSTS        IFEQ      'U'
549400000705     C     D$GSTS        OREQ      'I'
549500000622     C                   Z-ADD     W$DLNP        F
549600020709IF  3C     NTW(F)        IFEQ      'PPT'
549700000622     C                   Z-ADD     W$DLNP        LDTFP
549800000622E   3C                   ENDIF
549900000622     C                   Z-ADD     D$DLNA        F
550000020709IF  3C     NTW(F)        IFEQ      'PPT'
550100000622     C                   Z-ADD     D$DLNA        LDTFA
550200000622E   3C                   ENDIF
550300000622E   2C                   ENDIF
550400000622     C*
550500931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
550600940407     C     D$GSTS        IFEQ      'U'
550700940407     C     D$PAAS        ANDEQ     D$DAAS
550800931111     C     WPMES         ANDEQ     WDMES
550900940304     C     WPDEC         ANDEQ     WDDEC
551000000622     C     LPTFP         ANDEQ     LDTFP
551100000622     C     LPTFA         ANDEQ     LDTFA
551200040519     C*    D$PFLP        ANDEQ     D$DFLP
551300040519     C*    D$PFL2        ANDEQ     D$DFL2
551400040617     C     D$PAR5FLP     ANDEQ     D$DAR5FLP
551500040617     C     D$PAR5FL2     ANDEQ     D$DAR5FL2
551600040617     C     D$PAR5FL3     ANDEQ     D$DAR5FL3
551700040617     C     D$PAR5FL4     ANDEQ     D$DAR5FL4
551800040617     C     D$PAR5FL5     ANDEQ     D$DAR5FL5
551900940103     C     WPFPT         ANDEQ     WDFPT
552000940317     C     WTPREG        ANDEQ     WTDREG
552100931111     C*
552200931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
552300931111     C*
552400931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
552500931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
552600931111     C                   Z-ADD     D$DAAS        SDTANN
552700931111     C                   Z-ADD     WDMES         SDTMES
552800940304     C                   MOVEL     WDDEC         SDTDEC
552900000622     C                   Z-ADD     LDTFP         SDTTFP
553000000622     C                   Z-ADD     LDTFA         SDTTFA
553100040519     C*                  Z-ADD     D$DFLP        SDTTT1
553200040519     C*                  Z-ADD     D$DFL2        SDTTT2
553300040617     C                   z-add     D$DAR5FLP     SDTtt1
553400040617     C                   z-add     D$DAR5FL2     SDTtt2
553500040617     C                   z-add     D$DAR5FL3     SDTtt3
553600040617     C                   z-add     D$DAR5FL4     SDTtt4
553700040617     C                   z-add     D$DAR5FL5     SDTtt5
553800940103     C                   MOVEL     WDFPT         SDTFPE
553900931111     C* VALORI = 'DOPO' - 'PRIMA'
554000940512     C                   Z-ADD     *ZEROS        WTNSP
554100020829     C     WDPES         SUB       WPPES         WTPES
554200931111     C     D$DNCL        SUB       D$PNCL        WTNCO
554300931111     C     WDVOL         SUB       WPVOL         WTVOL
554400940823     C     WDCTL         SUB       WPCTL         WTCTL
554500940823     C     WDCT1         SUB       WPCT1         WTCT1
554600940823     C     WDCT2         SUB       WPCT2         WTCT2
554700040519     C     WDCT3         SUB       WPCT3         WTCT3
554800040519     C     WDCT4         SUB       WPCT4         WTCT4
554900040519     C     WDCT5         SUB       WPCT5         WTCT5
555000931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
555100931111     C                   EXSR      AGGSDT
555200931111     C*
555300931111     C                   ELSE
555400931111     C*
555500931111     C******************** E' CAMBIATA LA CHIAVE *************************
555600931111     C*
555700940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
555800940405     C     D$GSTS        IFEQ      'U'
555900940405     C     D$GSTS        OREQ      'D'
556000931111     C*
556100931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
556200931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
556300931111     C* CHIAVE = CHIAVE 'PRIMA'
556400931111     C                   Z-ADD     D$PAAS        SDTANN
556500931111     C                   Z-ADD     WPMES         SDTMES
556600940304     C                   MOVEL     WPDEC         SDTDEC
556700000622     C                   Z-ADD     LPTFP         SDTTFP
556800000622     C                   Z-ADD     LPTFA         SDTTFA
556900040519     C*                  Z-ADD     D$PFLP        SDTTT1
557000040519     C*                  Z-ADD     D$PFL2        SDTTT2
557100040617     C                   z-add     D$PAR5FLP     SDTtt1
557200040617     C                   z-add     D$PAR5FL2     SDTtt2
557300040617     C                   z-add     D$PAR5FL3     SDTtt3
557400040617     C                   z-add     D$PAR5FL4     SDTtt4
557500040617     C                   z-add     D$PAR5FL5     SDTtt5
557600940103     C                   MOVEL     WPFPT         SDTFPE
557700931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
557800931111     C     *ZEROS        SUB       1             WTNSP
557900020829     C     *ZEROS        SUB       WPPES         WTPES
558000931111     C     *ZEROS        SUB       D$PNCL        WTNCO
558100931111     C     *ZEROS        SUB       WPVOL         WTVOL
558200940823     C     *ZEROS        SUB       WPCTL         WTCTL
558300940823     C     *ZEROS        SUB       WPCT1         WTCT1
558400940823     C     *ZEROS        SUB       WPCT2         WTCT2
558500040519     C     *ZEROS        SUB       WPCT3         WTCT3
558600040519     C     *ZEROS        SUB       WPCT4         WTCT4
558700040519     C     *ZEROS        SUB       WPCT5         WTCT5
558800940317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE SOLO SE E' DA GESTIRE
558900940317     C     WTPREG        IFEQ      'S'
559000931111     C                   EXSR      AGGSDT
559100940317     C                   ENDIF
559200931111     C*
559300931111     C                   END
559400940202     C*
559500940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
559600940405     C     D$GSTS        IFEQ      'U'
559700940405     C     D$GSTS        OREQ      'I'
559800940202     C*
559900940202     C* NON E' UN ANNULLAMENTO, QUINDI
560000931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
560100931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
560200931111     C* CHIAVE = CHIAVE 'DOPO'
560300931111     C                   Z-ADD     D$DAAS        SDTANN
560400931111     C                   Z-ADD     WDMES         SDTMES
560500940304     C                   MOVEL     WDDEC         SDTDEC
560600000622     C                   Z-ADD     LDTFP         SDTTFP
560700000622     C                   Z-ADD     LDTFA         SDTTFA
560800040519     C*                  Z-ADD     D$DFLP        SDTTT1
560900040519     C*                  Z-ADD     D$DFL2        SDTTT2
561000040617     C                   z-add     D$DAR5FLP     SDTtt1
561100040617     C                   z-add     D$DAR5FL2     SDTtt2
561200040617     C                   z-add     D$DAR5FL3     SDTtt3
561300040617     C                   z-add     D$DAR5FL4     SDTtt4
561400040617     C                   z-add     D$DAR5FL5     SDTtt5
561500940103     C                   MOVEL     WDFPT         SDTFPE
561600931111     C* VALORI = VALORI 'DOPO'
561700931111     C                   Z-ADD     1             WTNSP
561800020829     C                   Z-ADD     WDPES         WTPES
561900931111     C                   Z-ADD     D$DNCL        WTNCO
562000931111     C                   Z-ADD     WDVOL         WTVOL
562100940823     C                   Z-ADD     WDCTL         WTCTL
562200940823     C                   Z-ADD     WDCT1         WTCT1
562300940823     C                   Z-ADD     WDCT2         WTCT2
562400040519     C                   Z-ADD     WDCT3         WTCT3
562500040519     C                   Z-ADD     WDCT4         WTCT4
562600040519     C                   Z-ADD     WDCT5         WTCT5
562700931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
562800940317     C     WTDREG        IFEQ      'S'
562900931111     C                   EXSR      AGGSDT
563000940317     C                   ENDIF
563100931111     C*
563200940202     C                   END
563300931111     C                   END
563400940318     C                   END
563500931111     C*
563600931111     C                   ENDSR
563700931111     C********************************************************************
563800990617     C*   AGGSDT - AGGIORNA IL FILE SISDT00F (SALDI TRAZIONI)
563900931111     C********************************************************************
564000931111     C     AGGSDT        BEGSR
564100931112     C     WTNSP         IFEQ      *ZEROS
564200020829     C*--- WTPKG         ANDEQ     *ZEROS
564300020829     C     WTPES         ANDEQ     *ZEROS
564400931112     C     WTNCO         ANDEQ     *ZEROS
564500931112     C     WTVOL         ANDEQ     *ZEROS
564600940823     C     WTCTL         ANDEQ     *ZEROS
564700940823     C     WTCT1         ANDEQ     *ZEROS
564800940823     C     WTCT2         ANDEQ     *ZEROS
564900040519     C     WTCT3         ANDEQ     *ZEROS
565000040519     C     WTCT4         ANDEQ     *ZEROS
565100040519     C     WTCT5         ANDEQ     *ZEROS
565200931112     C                   ELSE
565300990617     C     KEYSDT        CHAIN     SISDT01L                           99
565400931111     C     *IN99         IFEQ      '0'
565500931111     C                   EXSR      ADDSDT
565600990617     C                   UPDATE    SISDT000
565700931111     C                   ELSE
565800931111     C                   EXSR      AZZSDT
565900931111     C                   EXSR      ADDSDT
566000990617     C                   WRITE     SISDT000
566100931111     C                   END
566200931112     C                   END
566300931111     C                   ENDSR
566400931111     C********************************************************************
566500990617     C*   AZZSDT - AZZERA I CAMPI DEL FILE SISDT00F (SALDI TRAZIONI)
566600931111     C********************************************************************
566700931111     C     AZZSDT        BEGSR
566800931111     C                   Z-ADD     *ZEROS        SDTNSP
566900931111     C                   Z-ADD     *ZEROS        SDTPKG
567000931111     C                   Z-ADD     *ZEROS        SDTNCO
567100931111     C                   Z-ADD     *ZEROS        SDTVOL
567200931111     C                   Z-ADD     *ZEROS        SDTCTL
567300940314     C                   Z-ADD     *ZEROS        SDTCT1
567400940314     C                   Z-ADD     *ZEROS        SDTCT2
567500040519     C                   Z-ADD     *ZEROS        SDTCT3
567600040519     C                   Z-ADD     *ZEROS        SDTCT4
567700040519     C                   Z-ADD     *ZEROS        SDTCT5
567800931111     C                   ENDSR
567900931111     C********************************************************************
568000990617     C*   ADDSDT - AGGIUNGE AI VALORI DEL FILE SISDT00F (SALDI TRAZIONI)
568100931111     C********************************************************************
568200931111     C     ADDSDT        BEGSR
568300931111     C                   ADD       WTNSP         SDTNSP
568400020829     C*---               ADD       WTPKG         SDTPKG
568500020829     C                   ADD       WTPES         SDTPKG
568600931111     C                   ADD       WTNCO         SDTNCO
568700931111     C                   ADD       WTVOL         SDTVOL
568800090916     C                   z-add(h)  WTCTL         wtctl£
568900090916     C                   ADD       WTCTL£        SDTCTL
569000040519     c
569100090916     C                   z-add(h)  WTCT1         wtct1£
569200090916     C                   ADD       WTCT1£        SDTCT1
569300090916     C                   z-add(h)  WTCT2         wtct2£
569400090916     C                   ADD       WTCT2£        SDTCT2
569500090916     C                   z-add(h)  WTCT3         wtct3£
569600090916     C                   ADD       WTCT3£        SDTCT3
569700090916     C                   z-add(h)  WTCT4         wtct4£
569800090916     C                   ADD       WTCT4£        SDTCT4
569900090916     C                   z-add(h)  WTCT5         wtct5£
570000090916     C                   ADD       WTCT5£        SDTCT5
570100940317     C                   MOVEL     'S'           FLAGGS
570200931111     C                   ENDSR
570300931111     C*
570400931111     C********************************************************************
570500990617     C*   GESSDC - GESTISCE IL FILE SISDC00F (SALDI CLIENTE - REG/REG)
570600931111     C********************************************************************
570700931111     C     GESSDC        BEGSR
570800940411     C*
570900940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
571000940411     C     WCPREG        IFEQ      'S'
571100940411     C     WCDREG        OREQ      'S'
571200931111     C*
571300931111     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
571400940407     C     D$GSTS        IFEQ      'U'
571500940407     C     D$PAAS        ANDEQ     D$DAAS
571600931111     C     WPMES         ANDEQ     WDMES
571700941117     C     WCPKSC        ANDEQ     WCDKSC
571800931111     C     WPREP         ANDEQ     WDREP
571900931111     C     WPREA         ANDEQ     WDREA
572000940103     C     WPFPC         ANDEQ     WDFPC
572100931111     C     D$PTSP        ANDEQ     D$DTSP
572200931111     C     D$PCTR        ANDEQ     D$DCTR
572300941201     C     D$PTPO        ANDEQ     D$DTPO
572400941201     C     D$PRBL        ANDEQ     D$DRBL
572500931111     C     WPTPR         ANDEQ     WDTPR
572600940411     C     WCPREG        ANDEQ     WCDREG
572700931111     C*
572800931111     C******************** NON E' CAMBIATA LA CHIAVE *********************
572900931111     C*
573000931111     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
573100931111     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
573200931111     C                   Z-ADD     D$DAAS        SDCANN
573300931111     C                   Z-ADD     WDMES         SDCMES
573400941117     C                   Z-ADD     WCDKSC        SDCKSC
573500940210     C                   Z-ADD     WDREP         SDCREP
573600940210     C                   Z-ADD     WDREA         SDCREA
573700940103     C                   MOVEL     WDFPC         SDCFPE
573800931111     C                   MOVEL     D$DTSP        SDCTSP
573900931111     C                   Z-ADD     D$DCTR        SDCCTR
574000941201     C                   MOVEL     D$DTPO        SDCTPO
574100941201     C                   MOVEL     D$DRBL        SDCRBL
574200931111     C                   MOVEL     WDTPR         SDCTPR
574300931111     C* VALORI = 'DOPO' - 'PRIMA'
574400940512     C                   Z-ADD     *ZEROS        WCNSP
574500020829     C*--- D$DPKF        SUB       D$PPKF        WCPKG
574600020829     C     WDPES         SUB       WPPES         WCPES
574700020829     C     WDPCC         SUB       WPPCC         WCPCC
574800020829     C     WDPLS         SUB       WPPLS         WCPLS
574900931111     C     D$DNCL        SUB       D$PNCL        WCNCO
575000931111     C     WDVOL         SUB       WPVOL         WCVOL
575100940121     C     WDVCC         SUB       WPVCC         WCVCC
575200940121     C     WDVLS         SUB       WPVLS         WCVLS
575300931112     C     WDIRBC        SUB       WPIRBC        WCIRB
575400931111     C     WDIRP         SUB       WPIRP         WCIRP
575500931111     C     WDIRD         SUB       WPIRD         WCIRD
575600931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
575700931111     C                   EXSR      AGGSDC
575800931111     C*
575900931111     C                   ELSE
576000931111     C*
576100931111     C******************** E' CAMBIATA LA CHIAVE *************************
576200931111     C*
576300940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
576400940405     C     D$GSTS        IFEQ      'U'
576500940405     C     D$GSTS        OREQ      'D'
576600931111     C*
576700931111     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
576800931111     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
576900931111     C* CHIAVE = CHIAVE 'PRIMA'
577000931111     C                   Z-ADD     D$PAAS        SDCANN
577100931111     C                   Z-ADD     WPMES         SDCMES
577200941117     C                   Z-ADD     WCPKSC        SDCKSC
577300931111     C                   Z-ADD     WPREP         SDCREP
577400931111     C                   Z-ADD     WPREA         SDCREA
577500940103     C                   MOVEL     WPFPC         SDCFPE
577600931111     C                   MOVEL     D$PTSP        SDCTSP
577700931111     C                   Z-ADD     D$PCTR        SDCCTR
577800941201     C                   MOVEL     D$PTPO        SDCTPO
577900941201     C                   MOVEL     D$PRBL        SDCRBL
578000931111     C                   MOVEL     WPTPR         SDCTPR
578100931111     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
578200931111     C     *ZEROS        SUB       1             WCNSP
578300020829     C*--- *ZEROS        SUB       D$PPKF        WCPKG
578400020829     C     *ZEROS        SUB       WPPES         WCPES
578500020829     C     *ZEROS        SUB       WPPCC         WCPCC
578600020829     C     *ZEROS        SUB       WPPLS         WCPLS
578700931111     C     *ZEROS        SUB       D$PNCL        WCNCO
578800020829     C     *ZEROS        SUB       WPVOL         WCVOL
578900931111     C     *ZEROS        SUB       WPVCC         WCVCC
579000931111     C     *ZEROS        SUB       WPVLS         WCVLS
579100931112     C     *ZEROS        SUB       WPIRBC        WCIRB
579200931111     C     *ZEROS        SUB       WPIRP         WCIRP
579300931111     C     *ZEROS        SUB       WPIRD         WCIRD
579400931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
579500940411     C     WCPREG        IFEQ      'S'
579600931111     C                   EXSR      AGGSDC
579700940411     C                   ENDIF
579800931111     C*
579900931111     C                   END
580000940202     C*
580100940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
580200940405     C     D$GSTS        IFEQ      'U'
580300940405     C     D$GSTS        OREQ      'I'
580400940202     C*
580500940202     C* NON E' UN ANNULLAMENTO, QUINDI
580600931111     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
580700931111     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
580800931111     C* CHIAVE = CHIAVE 'DOPO'
580900931111     C                   Z-ADD     D$DAAS        SDCANN
581000931111     C                   Z-ADD     WDMES         SDCMES
581100941117     C                   Z-ADD     WCDKSC        SDCKSC
581200931111     C                   Z-ADD     WDREP         SDCREP
581300931111     C                   Z-ADD     WDREA         SDCREA
581400940103     C                   MOVEL     WDFPC         SDCFPE
581500931111     C                   MOVEL     D$DTSP        SDCTSP
581600931111     C                   Z-ADD     D$DCTR        SDCCTR
581700941201     C                   MOVEL     D$DTPO        SDCTPO
581800941201     C                   MOVEL     D$DRBL        SDCRBL
581900931111     C                   MOVEL     WDTPR         SDCTPR
582000931111     C* VALORI = VALORI 'DOPO'
582100931111     C                   Z-ADD     1             WCNSP
582200020829     C*---               Z-ADD     D$DPKF        WCPKG
582300020829     C                   Z-ADD     WDPES         WCPES
582400020829     C                   Z-ADD     WDPCC         WCPCC
582500020829     C                   Z-ADD     WDPLS         WCPLS
582600931111     C                   Z-ADD     D$DNCL        WCNCO
582700931111     C                   Z-ADD     WDVOL         WCVOL
582800931111     C                   Z-ADD     WDVCC         WCVCC
582900931111     C                   Z-ADD     WDVLS         WCVLS
583000931112     C                   Z-ADD     WDIRBC        WCIRB
583100931111     C                   Z-ADD     WDIRP         WCIRP
583200931111     C                   Z-ADD     WDIRD         WCIRD
583300931111     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
583400940411     C     WCDREG        IFEQ      'S'
583500931111     C                   EXSR      AGGSDC
583600940411     C                   ENDIF
583700931111     C*
583800940202     C                   END
583900931111     C                   END
584000940411     C                   END
584100931111     C*
584200931111     C                   ENDSR
584300931111     C********************************************************************
584400990617     C*   AGGSDC - AGGIORNA IL FILE SISDC00F (SALDI CLIENTI - REG/REG)
584500931111     C********************************************************************
584600931111     C     AGGSDC        BEGSR
584700931112     C     WCNSP         IFEQ      *ZEROS
584800020829     C*--- WCPKG         ANDEQ     *ZEROS
584900020829     C     WCPES         ANDEQ     *ZEROS
585000020829     C     WCPCC         ANDEQ     *ZEROS
585100020829     C     WCPLS         ANDEQ     *ZEROS
585200931112     C     WCNCO         ANDEQ     *ZEROS
585300931112     C     WCVOL         ANDEQ     *ZEROS
585400931112     C     WCVCC         ANDEQ     *ZEROS
585500931112     C     WCVLS         ANDEQ     *ZEROS
585600931112     C     WCIRB         ANDEQ     *ZEROS
585700931112     C     WCIRP         ANDEQ     *ZEROS
585800931112     C     WCIRD         ANDEQ     *ZEROS
585900931112     C                   ELSE
586000990617     C     KEYSDC        CHAIN     SISDC01L                           99
586100931111     C     *IN99         IFEQ      '0'
586200931111     C                   EXSR      ADDSDC
586300990617     C                   UPDATE    SISDC000
586400931111     C                   ELSE
586500931111     C                   EXSR      AZZSDC
586600931111     C                   EXSR      ADDSDC
586700990617     C                   WRITE     SISDC000
586800931111     C                   END
586900931112     C                   END
587000931111     C                   ENDSR
587100931111     C********************************************************************
587200990617     C*   AZZSDC - AZZERA I CAMPI DEL FILE SISDC00F (SALDI CLIENTI - REG/REG)
587300931111     C********************************************************************
587400931111     C     AZZSDC        BEGSR
587500931111     C                   Z-ADD     *ZEROS        SDCNSP
587600931111     C                   Z-ADD     *ZEROS        SDCPKG
587700020829     C                   Z-ADD     *ZEROS        SDCPCC
587800020829     C                   Z-ADD     *ZEROS        SDCPLS
587900931111     C                   Z-ADD     *ZEROS        SDCNCO
588000931111     C                   Z-ADD     *ZEROS        SDCVOL
588100931111     C                   Z-ADD     *ZEROS        SDCVCC
588200931111     C                   Z-ADD     *ZEROS        SDCVLS
588300931111     C                   Z-ADD     *ZEROS        SDCIRB
588400931111     C                   Z-ADD     *ZEROS        SDCIRR
588500931111     C                   Z-ADD     *ZEROS        SDCINC
588600931111     C                   Z-ADD     *ZEROS        SDCIRP
588700931111     C                   Z-ADD     *ZEROS        SDCIRD
588800990617     C                   Z-ADD     *ZEROS        SDCIRC
588900940301     C                   MOVEL     *BLANKS       SDCFTR
589000940301     C                   Z-ADD     *ZEROS        SDCDTR
589100931111     C                   ENDSR
589200931111     C********************************************************************
589300990617     C*   ADDSDC - AGGIUNGE AI VALORI DEL FILE SISDC00F (SALDI CLIENTI-REG/REG)
589400931111     C********************************************************************
589500931111     C     ADDSDC        BEGSR
589600931111     C                   ADD       WCNSP         SDCNSP
589700020829     C*---               ADD       WCPKG         SDCPKG
589800020829     C                   ADD       WCPES         SDCPKG
589900020829     C                   ADD       WCPCC         SDCPCC
590000020829     C                   ADD       WCPLS         SDCPLS
590100931111     C                   ADD       WCNCO         SDCNCO
590200931111     C                   ADD       WCVOL         SDCVOL
590300931111     C                   ADD       WCVCC         SDCVCC
590400931111     C                   ADD       WCVLS         SDCVLS
590500020129     C                   ADD       WCIRB         SDCIRB
590600090916     C                   z-add(h)  WCIRP         wcirp£
590700090916     C                   ADD       WCIRP£        SDCIRP
590800020129     C                   ADD       WCIRD         SDCIRD
590900940301     C                   MOVEL     *BLANKS       SDCFTR
591000940301     C                   Z-ADD     *ZEROS        SDCDTR
591100940317     C                   MOVEL     'S'           FLAGGS
591200931111     C                   ENDSR
591300931110     C*
591400931111     C*
591500931111     C*
591600931111     C*
591700040316     C********************************************************************
591800040316     C*   GESSDE - GESTISCE IL FILE SISDE00F (ESTENSIONE SALDI)
591900040316     C********************************************************************
592000040316     C     GESSDE        BEGSR
592100040323     C                   CLEAR                   WGESSDE
592200040323     C     WePREG        IFEQ      'S'
592300040323     C     WeDREG        OREQ      'S'
592304160404     c*
592400040323     C                   SELECT
592500040323     c                   when      (%subst(d$dgva: 1: 1) = 'O' or
592600040323     c                              %subst(d$pgva: 1: 1) = 'O') and
592700040323     C                             (D$DAR5CSF <> D$PAR5CSF OR
592800040323     C                             D$DAR5COR <> D$PAR5COR or
592900040323     C                             D$DAR5nba <> D$PAR5nba or
593000040323     C                             D$DAR5nb2 <> D$PAR5nb2)
593100040323     C                   EVAL      WGESSDE= 'BNB'
593200040323     C                   EXSR      GESsdeBNB
593300040323     c                   when      (%subst(d$dgva: 1: 1) = 'B' or
593400040323     c                              %subst(d$pgva: 1: 1) = 'B') and
593500040323     C                             (D$DAR5nba <> D$PAR5nba or
593600040323     C                             D$DAR5nb2 <> D$PAR5nb2)
593700040323     C                   EVAL      WGESSDE= 'BAN'
593800040323     C                   EXSR      GESsdeBAN
593801160405     C                   ENDSL
593802160405     C* scrive il record K30 per peso compreso tra 25 e 30
593803160405     c                   if        (wppes >25 and wppes<=30 ) or
593804160404     c                             (wdpes >25 and wdpes<=30 )
593805160404     C                   EVAL      WGESSDE= 'K30'
593806160404     C                   EXSR      GESsdek30
593807160405     C                   END
593808161107     C* scrive il record K31 per peso compreso tra 30,1 e 31,5
593809161107     c                   if        (wppes >30 and wppes<=31,5) or
593810161107     c                             (wdpes >30 and wdpes<=31,5)
593811161107     C                   EVAL      WGESSDE= 'K31'
593812161107     C                   EXSR      GESsdek31
593813161107     C                   END
593814160405     C* scrive il record LOG per importo competenze logistica
593815160405     c                   if        wlogicmpp <> 0 or wlogicmpd <> 0
593816160404     C                   EVAL      WGESSDE= 'LOG'
593817160404     C                   EXSR      GESsdelog
594000040316     C                   END
594001160405     C                   END
594100040316     C*
594200040316     C                   ENDSR
594300040317     C********************************************************************
594400040317     C*   GESsdeBNB - GESTISCE IL FILE SISDE00F X TIPO RECORD 'BNB'
594500040317     C********************************************************************
594600040317     C     GESsdeBNB     BEGSR
594700040317     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
594800040317     C     D$GSTS        IFEQ      'U'
594900040317     C     D$PAAS        ANDEQ     D$DAAS
595000040317     C     WPMES         ANDEQ     WDMES
595100040317     C     WCPKSC        ANDEQ     WCDKSC
595200040317     C     WPREP         ANDEQ     WDREP
595300040317     C     WPREA         ANDEQ     WDREA
595400040317     C     WPFPC         ANDEQ     WDFPC
595500040317     C     D$PTSP        ANDEQ     D$DTSP
595600040317     C     D$PCTR        ANDEQ     D$DCTR
595700040317     C     D$PTPO        ANDEQ     D$DTPO
595800040317     C     D$PRBL        ANDEQ     D$DRBL
595900040317     C     WPTPR         ANDEQ     WDTPR
596000040317     C     WePREG        ANDEQ     WeDREG
596100040317     C*
596200040317     C******************** NON E' CAMBIATA LA CHIAVE *********************
596300040317     C*
596400040317     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
596500040317     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
596600040317     C                   Z-ADD     D$DAAS        SDEANN
596700040317     C                   Z-ADD     WDMES         SDEMES
596800040317     C                   Z-ADD     WCDKSC        SDEKSC
596900040317     C                   Z-ADD     WDREP         SDEREP
597000040317     C                   Z-ADD     WDREA         SDEREA
597100040317     C                   MOVEL     WDFPC         SDEFPE
597200040317     C                   MOVEL     D$DTSP        SDETSP
597300040317     C                   Z-ADD     D$DCTR        SDECTR
597400040317     C                   MOVEL     D$DTPO        SDETPO
597500040317     C                   MOVEL     D$DRBL        SDERBL
597600040317     C                   MOVEL     WDTPR         SDETPR
597700040317     C* VALORI = 'DOPO' - 'PRIMA'
597800040317     C     D$DAR5CSF     SUB       D$PAR5CSF     WECSF
597900040317     C     D$DAR5COR     SUB       D$PAR5COR     WECOR
598000040318     C     D$DAR5nba     SUB       D$PAR5nba     WEnba
598100040318     C     D$DAR5nb2     SUB       D$PAR5nb2     WEnb2
598200040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
598300040317     C                   EXSR      AGGSDEbnb
598400040317     C*
598500040317     C                   ELSE
598600040317     C*
598700040317     C******************** E' CAMBIATA LA CHIAVE *************************
598800040317     C*
598900040317     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
599000040317     C     D$GSTS        IFEQ      'U'
599100040317     C     D$GSTS        OREQ      'D'
599200040317     C*
599300040317     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
599400040317     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
599500040317     C* CHIAVE = CHIAVE 'PRIMA'
599600040317     C                   Z-ADD     D$PAAS        SDEANN
599700040317     C                   Z-ADD     WPMES         SDEMES
599800040317     C                   Z-ADD     WCPKSC        SDEKSC
599900040317     C                   Z-ADD     WPREP         SDEREP
600000040317     C                   Z-ADD     WPREA         SDEREA
600100040317     C                   MOVEL     WPFPC         SDEFPE
600200040317     C                   MOVEL     D$PTSP        SDETSP
600300040317     C                   Z-ADD     D$PCTR        SDECTR
600400040317     C                   MOVEL     D$PTPO        SDETPO
600500040317     C                   MOVEL     D$PRBL        SDERBL
600600040317     C                   MOVEL     WPTPR         SDETPR
600700040317     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
600800040317     C     *ZEROS        SUB       D$PAR5CSF     WECSF
600900040317     C     *ZEROS        SUB       D$PAR5COR     WECOR
601000040318     C     *zeros        SUB       D$PAR5nba     WEnba
601100040318     C     *zeros        SUB       D$PAR5nb2     WEnb2
601200040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
601300040317     C     WePREG        IFEQ      'S'
601400040317     C                   EXSR      AGGSDEbnb
601500040317     C                   ENDIF
601600040317     C*
601700040317     C                   END
601800040317     C*
601900040317     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
602000040317     C     D$GSTS        IFEQ      'U'
602100040317     C     D$GSTS        OREQ      'I'
602200040317     C*
602300040317     C* NON E' UN ANNULLAMENTO, QUINDI
602400040317     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
602500040317     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
602600040317     C* CHIAVE = CHIAVE 'DOPO'
602700040317     C                   Z-ADD     D$DAAS        SDEANN
602800040317     C                   Z-ADD     WDMES         SDEMES
602900040317     C                   Z-ADD     WCDKSC        SDEKSC
603000040317     C                   Z-ADD     WDREP         SDEREP
603100040317     C                   Z-ADD     WDREA         SDEREA
603200040317     C                   MOVEL     WDFPC         SDEFPE
603300040317     C                   MOVEL     D$DTSP        SDETSP
603400040317     C                   Z-ADD     D$DCTR        SDECTR
603500040317     C                   MOVEL     D$DTPO        SDETPO
603600040317     C                   MOVEL     D$DRBL        SDERBL
603700040317     C                   MOVEL     WDTPR         SDETPR
603800040317     C* VALORI = VALORI 'DOPO'
603900040317     C                   Z-ADD     D$DAR5CSF     WECSF
604000040317     C                   Z-ADD     D$DAR5COR     WECOR
604100040318     C                   z-add     D$dAR5nba     WEnba
604200040318     C                   z-add     D$dAR5nb2     WEnb2
604300040317     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
604400040317     C     WeDREG        IFEQ      'S'
604500040317     C                   EXSR      AGGSDEbnb
604600040317     C                   ENDIF
604700040317     C*
604800040317     C                   END
604900040317     C                   END
605000040317     C*
605100040317     C                   ENDSR
605200040317     C********************************************************************
605300040317     C*   AGGSDEbnb - AGGIORNA IL FILE SISDE00F tipo record bnb
605400040317     C********************************************************************
605500040317     C     AGGSDEbnb     BEGSR
605600040317     C     WECOR         IFEQ      *ZEROS
605700040317     C     WECSF         ANDEQ     *ZEROS
605800040403     C     wenba         andeq     *zeros
605900040403     C     wenb2         andeq     *zeros
606000040317     C                   ELSE
606100040317     C                   EVAL      SDEFILE = 'SDC'
606200040317     C                   EVAL      SDETRD = 'BNB'
606300040317     C     KEYSDE        CHAIN     SISDE01L                           99
606400040317     C     *IN99         IFEQ      '0'
606500040317     C                   EXSR      ADDSDE
606600040317     C                   UPDATE    SISDE000
606700040317     C                   ELSE
606800040317     C                   EXSR      AZZSDE
606900040317     C                   EXSR      ADDSDE
607000040317     C                   WRITE     SISDE000
607100040317     C                   END
607200040317     C                   END
607300040317     C                   ENDSR
607400040318     C********************************************************************
607500040318     C*   GESsdeBan - GESTISCE IL FILE SISDE00F X TIPO RECORD 'BAN'
607600040318     C********************************************************************
607700040318     C     GESsdeBAN     BEGSR
607800040318     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
607900040318     C     D$GSTS        IFEQ      'U'
608000040318     C     D$PAAS        ANDEQ     D$DAAS
608100040318     C     WPMES         ANDEQ     WDMES
608200040318     C     WCPKSC        ANDEQ     WCDKSC
608300040318     C     WPREP         ANDEQ     WDREP
608400040318     C     WPREA         ANDEQ     WDREA
608500040318     C     WPFPC         ANDEQ     WDFPC
608600040318     C     D$PTSP        ANDEQ     D$DTSP
608700040318     C     D$PCTR        ANDEQ     D$DCTR
608800040318     C     D$PTPO        ANDEQ     D$DTPO
608900040318     C     D$PRBL        ANDEQ     D$DRBL
609000040318     C     WPTPR         ANDEQ     WDTPR
609100040318     C     WePREG        ANDEQ     WeDREG
609200040318     C*
609300040318     C******************** NON E' CAMBIATA LA CHIAVE *********************
609400040318     C*
609500040318     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
609600040318     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
609700040318     C                   Z-ADD     D$DAAS        SDEANN
609800040318     C                   Z-ADD     WDMES         SDEMES
609900040318     C                   Z-ADD     WCDKSC        SDEKSC
610000040318     C                   Z-ADD     WDREP         SDEREP
610100040318     C                   Z-ADD     WDREA         SDEREA
610200040318     C                   MOVEL     WDFPC         SDEFPE
610300040318     C                   MOVEL     D$DTSP        SDETSP
610400040318     C                   Z-ADD     D$DCTR        SDECTR
610500040318     C                   MOVEL     D$DTPO        SDETPO
610600040318     C                   MOVEL     D$DRBL        SDERBL
610700040318     C                   MOVEL     WDTPR         SDETPR
610800040318     C* VALORI = 'DOPO' - 'PRIMA'
610900040318     C     D$DAR5nba     SUB       D$PAR5nba     WEnba
611000040318     C     D$DAR5nb2     SUB       D$PAR5nb2     WEnb2
611100040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
611200040318     C                   EXSR      AGGSDEban
611300040318     C*
611400040318     C                   ELSE
611500040318     C*
611600040318     C******************** E' CAMBIATA LA CHIAVE *************************
611700040318     C*
611800040318     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
611900040318     C     D$GSTS        IFEQ      'U'
612000040318     C     D$GSTS        OREQ      'D'
612100040318     C*
612200040318     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
612300040318     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
612400040318     C* CHIAVE = CHIAVE 'PRIMA'
612500040318     C                   Z-ADD     D$PAAS        SDEANN
612600040318     C                   Z-ADD     WPMES         SDEMES
612700040318     C                   Z-ADD     WCPKSC        SDEKSC
612800040318     C                   Z-ADD     WPREP         SDEREP
612900040318     C                   Z-ADD     WPREA         SDEREA
613000040318     C                   MOVEL     WPFPC         SDEFPE
613100040318     C                   MOVEL     D$PTSP        SDETSP
613200040318     C                   Z-ADD     D$PCTR        SDECTR
613300040318     C                   MOVEL     D$PTPO        SDETPO
613400040318     C                   MOVEL     D$PRBL        SDERBL
613500040318     C                   MOVEL     WPTPR         SDETPR
613600040318     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
613700040318     C     *zeros        SUB       D$PAR5nba     WEnba
613800040318     C     *zeros        SUB       D$PAR5nb2     WEnb2
613900040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
614000040318     C     WePREG        IFEQ      'S'
614100040318     C                   EXSR      AGGSDEban
614200040318     C                   ENDIF
614300040318     C*
614400040318     C                   END
614500040318     C*
614600040318     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
614700040318     C     D$GSTS        IFEQ      'U'
614800040318     C     D$GSTS        OREQ      'I'
614900040318     C*
615000040318     C* NON E' UN ANNULLAMENTO, QUINDI
615100040318     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
615200040318     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
615300040318     C* CHIAVE = CHIAVE 'DOPO'
615400040318     C                   Z-ADD     D$DAAS        SDEANN
615500040318     C                   Z-ADD     WDMES         SDEMES
615600040318     C                   Z-ADD     WCDKSC        SDEKSC
615700040318     C                   Z-ADD     WDREP         SDEREP
615800040318     C                   Z-ADD     WDREA         SDEREA
615900040318     C                   MOVEL     WDFPC         SDEFPE
616000040318     C                   MOVEL     D$DTSP        SDETSP
616100040318     C                   Z-ADD     D$DCTR        SDECTR
616200040318     C                   MOVEL     D$DTPO        SDETPO
616300040318     C                   MOVEL     D$DRBL        SDERBL
616400040318     C                   MOVEL     WDTPR         SDETPR
616500040318     C* VALORI = VALORI 'DOPO'
616600040318     C                   z-add     D$dAR5nba     WEnba
616700040318     C                   z-add     D$dAR5nb2     WEnb2
616800040318     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
616900040318     C     WeDREG        IFEQ      'S'
617000040318     C                   EXSR      AGGSDEban
617100040318     C                   ENDIF
617200040318     C*
617300040318     C                   END
617400040318     C                   END
617500040318     C*
617600040318     C                   ENDSR
617700040318     C********************************************************************
617800040318     C*   AGGSDEban - AGGIORNA IL FILE SISDE00F tipo record ban
617900040318     C********************************************************************
618000040318     C     AGGSDEban     BEGSR
618100040318     C     WEnba         IFEQ      *ZEROS
618200040318     C     WEnb2         ANDEQ     *ZEROS
618300040318     C                   ELSE
618400040318     C                   EVAL      SDEFILE = 'SDC'
618500040318     C                   EVAL      SDETRD = 'BAN'
618600040318     C     KEYSDE        CHAIN     SISDE01L                           99
618700040318     C     *IN99         IFEQ      '0'
618800040318     C                   EXSR      ADDSDE
618900040318     C                   UPDATE    SISDE000
619000040318     C                   ELSE
619100040318     C                   EXSR      AZZSDE
619200040318     C                   EXSR      ADDSDE
619300040318     C                   WRITE     SISDE000
619400040318     C                   END
619500040318     C                   END
619600040318     C                   ENDSR
619601160404     C********************************************************************
619602160404     C*   GESsdek30 - GESTISCE IL FILE SISDE00F X TIPO RECORD 'K30'
619603160404     C********************************************************************
619604160404     C     GESsdek30     BEGSR
619605160404     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
619606160404     C     D$GSTS        IFEQ      'U'
619607160404     C     D$PAAS        ANDEQ     D$DAAS
619608160404     C     WPMES         ANDEQ     WDMES
619609160404     C     WCPKSC        ANDEQ     WCDKSC
619610160404     C     WPREP         ANDEQ     WDREP
619611160404     C     WPREA         ANDEQ     WDREA
619612160404     C     WPFPC         ANDEQ     WDFPC
619613160404     C     D$PTSP        ANDEQ     D$DTSP
619614160404     C     D$PCTR        ANDEQ     D$DCTR
619615160404     C     D$PTPO        ANDEQ     D$DTPO
619616160404     C     D$PRBL        ANDEQ     D$DRBL
619617160404     C     WPTPR         ANDEQ     WDTPR
619618160404     C     WePREG        ANDEQ     WeDREG
619619161108     C     Wppes         ANDgt     25
619620161108     C     Wppes         ANDle     30
619621161109     C     Wdpes         andgt     25
619622161109     C     Wdpes         ANDle     30
619627160404     C*
619628160404     C******************** NON E' CAMBIATA LA CHIAVE *********************
619629160404     C*
619630160404     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
619631160404     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
619632160404     C                   Z-ADD     D$DAAS        SDEANN
619633160404     C                   Z-ADD     WDMES         SDEMES
619634160404     C                   Z-ADD     WCDKSC        SDEKSC
619635160404     C                   Z-ADD     WDREP         SDEREP
619636160404     C                   Z-ADD     WDREA         SDEREA
619637160404     C                   MOVEL     WDFPC         SDEFPE
619638160404     C                   MOVEL     D$DTSP        SDETSP
619639160404     C                   Z-ADD     D$DCTR        SDECTR
619640160404     C                   MOVEL     D$DTPO        SDETPO
619641160404     C                   MOVEL     D$DRBL        SDERBL
619642160404     C                   MOVEL     WDTPR         SDETPR
619643160404     C* VALORI = 'DOPO' - 'PRIMA'
619646161109     C                   z-add     0             wk30nsp
619647161109     C     d$pncl        SUB       d$dncl        wk30NCo
619648160404     C     wdpes         SUB       wppes         wk30PKG
619649160404     C     wdpcc         SUB       wppcc         wk30PCC
619650160404     C     wdpls         SUB       wppls         wk30PLS
619651160404     C     wdvol         SUB       wpvol         wk30VOL
619652160414     C     wdvls         SUB       wpvls         wk30VLS
619653160414     C     wdvls         SUB       wpvls         wk30VLS
619654160414     C     wdirb         SUB       wpirb         wk30irb
619655160414     C     wdirp         SUB       wpirp         wk30irp
619656160404     c*
619657160404     C                   EXSR      AGGSDEk30
619659160404     C*
619660160404     C                   ELSE
619661160404     C*
619662160404     C******************** E' CAMBIATA LA CHIAVE *************************
619663160404     C*
619664160404     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
619665161109     C     Wppes         ifgt      25
619666161109     C     Wppes         ANDle     30
619667160404     C     D$GSTS        IFEQ      'U'
619668160404     C     D$GSTS        OREQ      'D'
619669160404     C*
619670160404     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
619671160404     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
619672160404     C* CHIAVE = CHIAVE 'PRIMA'
619673160404     C                   Z-ADD     D$PAAS        SDEANN
619674160404     C                   Z-ADD     WPMES         SDEMES
619675160404     C                   Z-ADD     WCPKSC        SDEKSC
619676160404     C                   Z-ADD     WPREP         SDEREP
619677160404     C                   Z-ADD     WPREA         SDEREA
619678160404     C                   MOVEL     WPFPC         SDEFPE
619679160404     C                   MOVEL     D$PTSP        SDETSP
619680160404     C                   Z-ADD     D$PCTR        SDECTR
619681160404     C                   MOVEL     D$PTPO        SDETPO
619682160404     C                   MOVEL     D$PRBL        SDERBL
619683160404     C                   MOVEL     WPTPR         SDETPR
619684160404     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
619685160404     C     *ZEROS        SUB       1             wk30nsp
619686160404     C     *ZEROS        SUB       d$pncl        wk30nco
619687160404     C     *ZEROS        SUB       wppes         wk30PKG
619688160404     C     *ZEROS        SUB       wppcc         wk30PCC
619689160404     C     *zeros        SUB       wppls         wk30PLS
619690160404     C     *zeros        SUB       wpvol         wk30VOL
619691160404     C     *ZEROS        SUB       wpvcc         wk30VCC
619692160404     C     *ZEROS        SUB       wpvls         wk30VLS
619693160414     C     *ZEROS        SUB       wpirb         wk30irb
619694160414     C     *ZEROS        SUB       wpirp         wk30irp
619695160404     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
619696160404     C     WePREG        IFEQ      'S'
619697160404     C                   EXSR      AGGSDEk30
619698160404     C                   ENDIF
619699160404     C*
619700160404     C                   END
619701161109     C                   END
619702160404     C*
619703160404     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
619704161109     C     Wdpes         ifgt      25
619705161109     C     Wdpes         ANDle     30
619706160404     C     D$GSTS        IFEQ      'U'
619707160404     C     D$GSTS        OREQ      'I'
619708160404     C*
619709160404     C* NON E' UN ANNULLAMENTO, QUINDI
619710160404     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
619711160404     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
619712160404     C* CHIAVE = CHIAVE 'DOPO'
619713161109     C                   Z-ADD     D$DAAS        SDEANN
619714160404     C                   Z-ADD     WDMES         SDEMES
619715160404     C                   Z-ADD     WCDKSC        SDEKSC
619716160404     C                   Z-ADD     WDREP         SDEREP
619717160404     C                   Z-ADD     WDREA         SDEREA
619718160404     C                   MOVEL     WDFPC         SDEFPE
619719160404     C                   MOVEL     D$DTSP        SDETSP
619720160404     C                   Z-ADD     D$DCTR        SDECTR
619721160404     C                   MOVEL     D$DTPO        SDETPO
619722160404     C                   MOVEL     D$DRBL        SDERBL
619723160404     C                   MOVEL     WDTPR         SDETPR
619724160404     C* VALORI = VALORI 'DOPO'
619725161109     C                   Z-ADD     1             wk30nsp
619726161109     C                   Z-ADD     d$dncl        wk30nco
619727160404     C                   Z-ADD     wdpes         wk30PKG
619728160404     C                   Z-ADD     wdpcc         wk30PCC
619729160404     C                   z-add     wdpls         wk30PLS
619730160404     C                   z-add     wdvol         wk30VOL
619731160404     C                   Z-ADD     wdvcc         wk30VCC
619732160404     C                   Z-ADD     wdvls         wk30VLS
619733160414     C                   Z-ADD     wdirb         wk30irb
619734160414     C                   Z-ADD     wdirp         wk30irp
619735160404     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
619736160404     C     WeDREG        IFEQ      'S'
619737160404     C                   EXSR      AGGSDEk30
619738160404     C                   ENDIF
619739160404     C*
619740160404     C                   END
619741161109     C                   END
619742160404     C                   END
619743160404     C*
619744160404     C                   ENDSR
619745160404     C********************************************************************
619746160404     C*   AGGSDEk30 - AGGIORNA IL FILE SISDE00F tipo record k30
619747160404     C********************************************************************
619748160404     C     AGGSDEk30     BEGSR
619749160404     C     wk30PKG       IFEQ      *ZEROS
619750160404     C     wk30PCC       ANDEQ     *ZEROS
619751160404     C     wk30PLS       andeq     *zeros
619752160404     C     wk30VOL       andeq     *zeros
619753160404     C     wk30VCC       andeq     *ZEROS
619754160404     C     wk30VLS       ANDEQ     *ZEROS
619755160414     C     wk30irb       ANDEQ     *ZEROS
619756160414     C     wk30irp       ANDEQ     *ZEROS
619757160404     C                   ELSE
619758160404     C                   EVAL      SDEFILE = 'SDC'
619759160404     C                   EVAL      SDETRD = 'K30'
619760160404     C     KEYSDE        CHAIN     SISDE01L                           99
619761160404     C     *IN99         IFEQ      '0'
619762160404     C                   EXSR      ADDSDE
619763160404     C                   UPDATE    SISDE000
619764160404     C                   ELSE
619765160404     C                   EXSR      AZZSDE
619766160404     C                   EXSR      ADDSDE
619767160404     C                   WRITE     SISDE000
619768160404     C                   END
619769160404     C                   END
619770160404     C                   ENDSR
619771161107     C********************************************************************
619772161107     C*   GESsdek31 - GESTISCE IL FILE SISDE00F X TIPO RECORD 'K31'
619773161107     C********************************************************************
619774161107     C     GESsdek31     BEGSR
619775161107     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
619776161107     C     D$GSTS        IFEQ      'U'
619777161107     C     D$PAAS        ANDEQ     D$DAAS
619778161107     C     WPMES         ANDEQ     WDMES
619779161107     C     WCPKSC        ANDEQ     WCDKSC
619780161107     C     WPREP         ANDEQ     WDREP
619781161107     C     WPREA         ANDEQ     WDREA
619782161107     C     WPFPC         ANDEQ     WDFPC
619783161107     C     D$PTSP        ANDEQ     D$DTSP
619784161107     C     D$PCTR        ANDEQ     D$DCTR
619785161107     C     D$PTPO        ANDEQ     D$DTPO
619786161107     C     D$PRBL        ANDEQ     D$DRBL
619787161107     C     WPTPR         ANDEQ     WDTPR
619788161107     C     WePREG        ANDEQ     WeDREG
619789161109     C     Wppes         ANDgt     30
619790161109     C     Wppes         ANDle     31,5
619791161109     C     Wdpes         ANDgt     30
619792161109     C     Wdpes         ANDle     31.5
619793161107     C*
619794161107     C******************** NON E' CAMBIATA LA CHIAVE *********************
619795161107     C*
619796161107     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
619797161107     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
619798161107     C                   Z-ADD     D$DAAS        SDEANN
619799161107     C                   Z-ADD     WDMES         SDEMES
619800161107     C                   Z-ADD     WCDKSC        SDEKSC
619801161107     C                   Z-ADD     WDREP         SDEREP
619802161107     C                   Z-ADD     WDREA         SDEREA
619803161107     C                   MOVEL     WDFPC         SDEFPE
619804161107     C                   MOVEL     D$DTSP        SDETSP
619805161107     C                   Z-ADD     D$DCTR        SDECTR
619806161107     C                   MOVEL     D$DTPO        SDETPO
619807161107     C                   MOVEL     D$DRBL        SDERBL
619808161107     C                   MOVEL     WDTPR         SDETPR
619809161107     C* VALORI = 'DOPO' - 'PRIMA'
619810161109     c                   clear                   wk31nsp
619811161107     C     d$pncl        SUB       d$dncl        wk31NCo
619812161107     C     wdpes         SUB       wppes         wk31PKG
619813161107     C     wdpcc         SUB       wppcc         wk31PCC
619814161107     C     wdpls         SUB       wppls         wk31PLS
619815161107     C     wdvol         SUB       wpvol         wk31VOL
619816161107     C     wdvls         SUB       wpvls         wk31VLS
619817161107     C     wdvls         SUB       wpvls         wk31VLS
619818161107     C     wdirb         SUB       wpirb         wk31irb
619819161107     C     wdirp         SUB       wpirp         wk31irp
619820161107     c*
619821161107     C                   EXSR      AGGSDEk31
619822161107     C*
619823161107     C                   ELSE
619824161107     C*
619825161107     C******************** E' CAMBIATA LA CHIAVE *************************
619826161107     C*
619827161107     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
619828161109     C     Wppes         ifgt      30
619829161109     C     Wppes         ANDle     31,5
619830161107     C     D$GSTS        IFEQ      'U'
619831161107     C     D$GSTS        OREQ      'D'
619832161107     C*
619833161107     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
619834161107     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
619835161107     C* CHIAVE = CHIAVE 'PRIMA'
619836161107     C                   Z-ADD     D$PAAS        SDEANN
619837161107     C                   Z-ADD     WPMES         SDEMES
619838161107     C                   Z-ADD     WCPKSC        SDEKSC
619839161107     C                   Z-ADD     WPREP         SDEREP
619840161107     C                   Z-ADD     WPREA         SDEREA
619841161107     C                   MOVEL     WPFPC         SDEFPE
619842161107     C                   MOVEL     D$PTSP        SDETSP
619843161107     C                   Z-ADD     D$PCTR        SDECTR
619844161107     C                   MOVEL     D$PTPO        SDETPO
619845161107     C                   MOVEL     D$PRBL        SDERBL
619846161107     C                   MOVEL     WPTPR         SDETPR
619847161107     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
619848161107     C     *ZEROS        SUB       1             wk31nsp
619849161107     C     *ZEROS        SUB       d$pncl        wk31nco
619850161107     C     *ZEROS        SUB       wppes         wk31PKG
619851161107     C     *ZEROS        SUB       wppcc         wk31PCC
619852161107     C     *zeros        SUB       wppls         wk31PLS
619853161107     C     *zeros        SUB       wpvol         wk31VOL
619854161107     C     *ZEROS        SUB       wpvcc         wk31VCC
619855161107     C     *ZEROS        SUB       wpvls         wk31VLS
619856161107     C     *ZEROS        SUB       wpirb         wk31irb
619857161107     C     *ZEROS        SUB       wpirp         wk31irp
619858161107     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
619859161107     C     WePREG        IFEQ      'S'
619860161107     C                   EXSR      AGGSDEk31
619861161107     C                   ENDIF
619862161107     C*
619863161107     C                   END
619864161109     C                   END
619865161107     C*
619866161107     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
619867161109     C     Wdpes         ifgt      30
619868161109     C     Wdpes         ANDle     31,5
619869161107     C     D$GSTS        IFEQ      'U'
619870161107     C     D$GSTS        OREQ      'I'
619871161107     C*
619872161107     C* NON E' UN ANNULLAMENTO, QUINDI
619873161107     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
619874161107     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
619875161107     C* CHIAVE = CHIAVE 'DOPO'
619876161107     C                   Z-ADD     D$DAAS        SDEANN
619877161107     C                   Z-ADD     WDMES         SDEMES
619878161107     C                   Z-ADD     WCDKSC        SDEKSC
619879161107     C                   Z-ADD     WDREP         SDEREP
619880161107     C                   Z-ADD     WDREA         SDEREA
619881161107     C                   MOVEL     WDFPC         SDEFPE
619882161107     C                   MOVEL     D$DTSP        SDETSP
619883161107     C                   Z-ADD     D$DCTR        SDECTR
619884161107     C                   MOVEL     D$DTPO        SDETPO
619885161107     C                   MOVEL     D$DRBL        SDERBL
619886161107     C                   MOVEL     WDTPR         SDETPR
619887161107     C* VALORI = VALORI 'DOPO'
619888161109     c                   z-add     1             wk31nsp
619889161107     C                   Z-ADD     d$dncl        wk31nco
619890161107     C                   Z-ADD     wdpes         wk31PKG
619891161107     C                   Z-ADD     wdpcc         wk31PCC
619892161107     C                   z-add     wdpls         wk31PLS
619893161107     C                   z-add     wdvol         wk31VOL
619894161107     C                   Z-ADD     wdvcc         wk31VCC
619895161107     C                   Z-ADD     wdvls         wk31VLS
619896161107     C                   Z-ADD     wdirb         wk31irb
619897161107     C                   Z-ADD     wdirp         wk31irp
619898161107     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
619899161107     C     WeDREG        IFEQ      'S'
619900161107     C                   EXSR      AGGSDEk31
619901161107     C                   ENDIF
619902161107     C*
619903161107     C                   END
619904161107     C                   END
619905161109     C                   END
619906161107     C*
619907161107     C                   ENDSR
619908161107     C********************************************************************
619909161107     C*   AGGSDEk31 - AGGIORNA IL FILE SISDE00F tipo record k31
619910161107     C********************************************************************
619911161107     C     AGGSDEk31     BEGSR
619912161107     C     wk31PKG       IFEQ      *ZEROS
619913161107     C     wk31PCC       ANDEQ     *ZEROS
619914161107     C     wk31PLS       andeq     *zeros
619915161107     C     wk31VOL       andeq     *zeros
619916161107     C     wk31VCC       andeq     *ZEROS
619917161107     C     wk31VLS       ANDEQ     *ZEROS
619918161107     C     wk31irb       ANDEQ     *ZEROS
619919161107     C     wk31irp       ANDEQ     *ZEROS
619920161107     C                   ELSE
619921161107     C                   EVAL      SDEFILE = 'SDC'
619922161107     C                   EVAL      SDETRD = 'K31'
619923161107     C     KEYSDE        CHAIN     SISDE01L                           99
619924161107     C     *IN99         IFEQ      '0'
619925161107     C                   EXSR      ADDSDE
619926161107     C                   UPDATE    SISDE000
619927161107     C                   ELSE
619928161107     C                   EXSR      AZZSDE
619929161107     C                   EXSR      ADDSDE
619930161107     C                   WRITE     SISDE000
619931161107     C                   END
619932161107     C                   END
619933161107     C                   ENDSR
619934160404     C********************************************************************
619935160404     C*   GESsdelog - GESTISCE IL FILE SISDE00F X TIPO RECORD 'LOG'
619936160404     C********************************************************************
619937160404     C     GESsdelog     BEGSR
619938160404     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
619939160404     C     D$GSTS        IFEQ      'U'
619940160404     C     D$PAAS        ANDEQ     D$DAAS
619941160404     C     WPMES         ANDEQ     WDMES
619942160404     C     WCPKSC        ANDEQ     WCDKSC
619943160404     C     WPREP         ANDEQ     WDREP
619944160404     C     WPREA         ANDEQ     WDREA
619945160404     C     WPFPC         ANDEQ     WDFPC
619946160404     C     D$PTSP        ANDEQ     D$DTSP
619947160404     C     D$PCTR        ANDEQ     D$DCTR
619948160404     C     D$PTPO        ANDEQ     D$DTPO
619949160404     C     D$PRBL        ANDEQ     D$DRBL
619950160404     C     WPTPR         ANDEQ     WDTPR
619951160404     C     WePREG        ANDEQ     WeDREG
619952160404     C*
619953160404     C******************** NON E' CAMBIATA LA CHIAVE *********************
619954160404     C*
619955160404     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
619956160404     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
619957160404     C                   Z-ADD     D$DAAS        SDEANN
619958160404     C                   Z-ADD     WDMES         SDEMES
619959160404     C                   Z-ADD     WCDKSC        SDEKSC
619960160404     C                   Z-ADD     WDREP         SDEREP
619961160404     C                   Z-ADD     WDREA         SDEREA
619962160404     C                   MOVEL     WDFPC         SDEFPE
619963160404     C                   MOVEL     D$DTSP        SDETSP
619964160404     C                   Z-ADD     D$DCTR        SDECTR
619965160404     C                   MOVEL     D$DTPO        SDETPO
619966160404     C                   MOVEL     D$DRBL        SDERBL
619967160404     C                   MOVEL     WDTPR         SDETPR
619968160404     C* VALORI = 'DOPO' - 'PRIMA'
619969160405     C     wlogicmpd     SUB       wlogicmpp     wlogicmp
619970160404     c*
619971160404     C                   EXSR      AGGSDElog
619972160404     C*
619973160404     C                   ELSE
619974160404     C*
619975160404     C******************** E' CAMBIATA LA CHIAVE *************************
619976160404     C*
619977160404     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
619978160404     C     D$GSTS        IFEQ      'U'
619979160404     C     D$GSTS        OREQ      'D'
619980160404     C*
619981160404     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
619982160404     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
619983160404     C* CHIAVE = CHIAVE 'PRIMA'
619984160404     C                   Z-ADD     D$PAAS        SDEANN
619985160404     C                   Z-ADD     WPMES         SDEMES
619986160404     C                   Z-ADD     WCPKSC        SDEKSC
619987160404     C                   Z-ADD     WPREP         SDEREP
619988160404     C                   Z-ADD     WPREA         SDEREA
619989160404     C                   MOVEL     WPFPC         SDEFPE
619990160404     C                   MOVEL     D$PTSP        SDETSP
619991160404     C                   Z-ADD     D$PCTR        SDECTR
619992160404     C                   MOVEL     D$PTPO        SDETPO
619993160404     C                   MOVEL     D$PRBL        SDERBL
619994160404     C                   MOVEL     WPTPR         SDETPR
619995160404     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
619996160405     C     *zeros        SUB       wlogicmpp     wlogicmp
619997160404     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
619998160404     C     WePREG        IFEQ      'S'
619999160404     C                   EXSR      AGGSDElog
620000160404     C                   ENDIF
620001160404     C*
620002160404     C                   END
620003160404     C*
620004160404     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
620005160404     C     D$GSTS        IFEQ      'U'
620006160404     C     D$GSTS        OREQ      'I'
620007160404     C*
620008160404     C* NON E' UN ANNULLAMENTO, QUINDI
620009160404     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
620010160404     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
620011160404     C* CHIAVE = CHIAVE 'DOPO'
620012160404     C                   Z-ADD     D$DAAS        SDEANN
620013160404     C                   Z-ADD     WDMES         SDEMES
620014160404     C                   Z-ADD     WCDKSC        SDEKSC
620015160404     C                   Z-ADD     WDREP         SDEREP
620016160404     C                   Z-ADD     WDREA         SDEREA
620017160404     C                   MOVEL     WDFPC         SDEFPE
620018160404     C                   MOVEL     D$DTSP        SDETSP
620019160404     C                   Z-ADD     D$DCTR        SDECTR
620020160404     C                   MOVEL     D$DTPO        SDETPO
620021160404     C                   MOVEL     D$DRBL        SDERBL
620022160404     C                   MOVEL     WDTPR         SDETPR
620023160404     C* VALORI = VALORI 'DOPO'
620024160405     C                   Z-ADD     wlogicmpd     wlogicmp
620025160404     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
620026160404     C     WeDREG        IFEQ      'S'
620027160404     C                   EXSR      AGGSDElog
620028160404     C                   ENDIF
620029160404     C*
620030160404     C                   END
620031160404     C                   END
620032160404     C*
620033160404     C                   ENDSR
620034160404     C********************************************************************
620035160404     C*   AGGSDElog - AGGIORNA IL FILE SISDE00F tipo record log
620036160404     C********************************************************************
620037160404     C     AGGSDElog     BEGSR
620038160404     C     wlogicmp      IFEQ      *ZEROS
620039160404     C                   ELSE
620040160404     C                   EVAL      SDEFILE = 'SDC'
620041160404     C                   EVAL      SDETRD = 'LOG'
620042160404     C     KEYSDE        CHAIN     SISDE01L                           99
620043160404     C     *IN99         IFEQ      '0'
620044160404     C                   EXSR      ADDSDE
620045160404     C                   UPDATE    SISDE000
620046160404     C                   ELSE
620047160404     C                   EXSR      AZZSDE
620048160404     C                   EXSR      ADDSDE
620049160404     C                   WRITE     SISDE000
620050160404     C                   END
620051160404     C                   END
620052160404     C                   ENDSR
620053040316     C********************************************************************
620054040316     C*   AZZSDE - AZZERA I CAMPI DEL FILE SISDE00F (ESTENSIONE SALDI)      )
620055040316     C********************************************************************
620056040316     C     AZZSDE        BEGSR
620100040318     c                   select
620200040318     c* tipo record BNB
620300040318     C                   when      wgessde='BNB'
620400040317     C                   CLEAR                   DAR5BNBSDE
620500040403     C                   eval      sdeuni = DAR5BNBSDE
620600040318     c* tipo record BAN
620700040318     C                   when      wgessde='BAN'
620800040318     C                   CLEAR                   DAR5BANSDE
620900040403     C                   eval      sdeuni = DAR5BANSDE
620901160404     c* tipo record K30
620902160404     C                   when      wgessde='K30'
620903160404     C                   CLEAR                   dsdek30
620904160404     C                   eval      sdeuni = Dsdek30
620905161107     c* tipo record K31
620906161107     C                   when      wgessde='K31'
620907161107     C                   CLEAR                   dsdek31
620908161107     C                   eval      sdeuni = Dsdek31
620909161107     c* tipo record LOG
620910160404     C                   when      wgessde='LOG'
620911160404     C                   CLEAR                   dsdelog
620912160404     C                   eval      sdeuni = Dsdelog
621000040318     C                   ENDsl
621100040316     C                   MOVEL     *BLANKS       SDEFTR
621200040316     C                   Z-ADD     *ZEROS        SDEDTR
621300040316     C                   ENDSR
621400040316     C********************************************************************
621500040316     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
621600040316     C********************************************************************
621700040316     C     ADDSDE        BEGSR
621800040318     c                   select
621900040318     c* tipo record BNB
622000040318     C                   when      wgessde='BNB'
622100040403     C                   eval      DAR5BNBSDE = SDEUNI
622200040403     C                   add       wecsf         §bnbBCSF
622300040403     C                   add       wecor         §bnbBCOR
622400040403     C                   add       wenba         §bnbBnba
622500040403     C                   add       wenb2         §bnbBnb2
622600040403     C                   eval      sdeuni = DAR5BNBSDE
622700040318     c* tipo record BAN
622800040318     C                   when      wgessde='BAN'
622900040403     C                   eval      DAR5BANSDE =  SDEUNI
623000040403     C                   add       wenba         §bannba
623100040403     C                   add       wenb2         §bannb2
623200040403     C                   eval      sdeuni = DAR5BANSDE
623201160404     c* tipo record K30
623202160404     C                   when      wgessde='K30'
623203160404     C                   eval      Dsdek30 =  SDEUNI
623204161109     C                   ADD       wk30nsp       §k30nsp
623205160404     C                   ADD       wk30nco       §k30nco
623206160404     C                   ADD       wk30PKG       §k30PKG
623207160404     C                   ADD       wk30PCC       §k30PCC
623208160404     C                   add       wk30PLS       §k30PLS
623209160404     C                   add       wk30VOL       §k30VOL
623210160404     C                   ADD       wk30VCC       §k30VCC
623211160404     C                   ADD       wk30VLS       §k30VLS
623212160414     C                   ADD       wk30irb       §k30irb
623213160414     C                   ADD       wk30irp       §k30irp
623215160404     C                   eval      sdeuni = Dsdek30
623216161107     c* tipo record K31
623217161107     C                   when      wgessde='K31'
623218161107     C                   eval      Dsdek31 =  SDEUNI
623219161109     C                   ADD       wk31nsp       §k31nsp
623220161107     C                   ADD       wk31nco       §k31nco
623221161107     C                   ADD       wk31PKG       §k31PKG
623222161107     C                   ADD       wk31PCC       §k31PCC
623223161107     C                   add       wk31PLS       §k31PLS
623224161107     C                   add       wk31VOL       §k31VOL
623225161107     C                   ADD       wk31VCC       §k31VCC
623226161107     C                   ADD       wk31VLS       §k31VLS
623227161107     C                   ADD       wk31irb       §k31irb
623228161107     C                   ADD       wk31irp       §k31irp
623229161107     C                   eval      sdeuni = Dsdek31
623230160404     c* tipo record LOG
623231160404     C                   when      wgessde='LOG'
623232160404     C                   eval      Dsdelog =  SDEUNI
623233160404     C                   ADD       wlogicmp      §logicmp
623234160404     C                   eval      sdeuni = Dsdelog
623300040318     C                   ENDsl
623400040316     C                   MOVEL     *BLANKS       SDEFTR
623500040316     C                   Z-ADD     *ZEROS        SDEDTR
623600040316     C                   MOVEL     'S'           FLAGGS
623700040316     C                   ENDSR
623800931112     C********************************************************************
623900990617     C*   GESSDO - GESTISCE IL FILE SISDO00F (SALDI CLIENTE - CONTABILI)
624000931112     C********************************************************************
624100931112     C     GESSDO        BEGSR
624200940411     C*
624300940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
624400940411     C     WOPREG        IFEQ      'S'
624500940411     C     WODREG        OREQ      'S'
624600931112     C*
624700931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
624800940407     C     D$GSTS        IFEQ      'U'
624900940407     C     D$PAAS        ANDEQ     D$DAAS
625000931112     C     WPMES         ANDEQ     WDMES
625100941117     C     WCPKSC        ANDEQ     WCDKSC
625200940411     C     WOPREG        ANDEQ     WODREG
625300931112     C*
625400931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
625500931112     C*
625600931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
625700931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
625800931112     C                   Z-ADD     D$DAAS        SDOANN
625900931112     C                   Z-ADD     WDMES         SDOMES
626000941117     C                   Z-ADD     WCDKSC        SDOKSC
626100931112     C* VALORI = 'DOPO' - 'PRIMA'
626200940512     C                   Z-ADD     *ZEROS        WONSP
626300020829     C*--- D$DPKF        SUB       D$PPKF        WOPKG
626400020829     C     WDPES         SUB       WPPES         WOPES
626500931112     C     D$DNCL        SUB       D$PNCL        WONCO
626600931112     C     WDVOL         SUB       WPVOL         WOVOL
626700931112     C     WDIRBC        SUB       WPIRBC        WOIRB
626800931112     C     WDIRP         SUB       WPIRP         WOIRP
626900931112     C     WDIRD         SUB       WPIRD         WOIRD
627000931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
627100931112     C                   EXSR      AGGSDO
627200931112     C*
627300931112     C                   ELSE
627400931112     C*
627500931112     C******************** E' CAMBIATA LA CHIAVE *************************
627600931112     C*
627700940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
627800940405     C     D$GSTS        IFEQ      'U'
627900940405     C     D$GSTS        OREQ      'D'
628000931112     C*
628100931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
628200931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
628300931112     C* CHIAVE = CHIAVE 'PRIMA'
628400931112     C                   Z-ADD     D$PAAS        SDOANN
628500931112     C                   Z-ADD     WPMES         SDOMES
628600941117     C                   Z-ADD     WCPKSC        SDOKSC
628700931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
628800931112     C     *ZEROS        SUB       1             WONSP
628900020829     C*--- *ZEROS        SUB       D$PPKF        WOPKG
629000020829     C     *ZEROS        SUB       WPPES         WOPES
629100931112     C     *ZEROS        SUB       D$PNCL        WONCO
629200931112     C     *ZEROS        SUB       WPVOL         WOVOL
629300931112     C     *ZEROS        SUB       WPIRBC        WOIRB
629400931112     C     *ZEROS        SUB       WPIRP         WOIRP
629500931112     C     *ZEROS        SUB       WPIRD         WOIRD
629600931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
629700940411     C     WOPREG        IFEQ      'S'
629800931112     C                   EXSR      AGGSDO
629900940411     C                   ENDIF
630000931112     C*
630100931112     C                   END
630200940202     C*
630300940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
630400940405     C     D$GSTS        IFEQ      'U'
630500940405     C     D$GSTS        OREQ      'I'
630600940202     C*
630700940202     C* NON E' UN ANNULLAMENTO, QUINDI
630800931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
630900931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
631000931112     C* CHIAVE = CHIAVE 'DOPO'
631100931112     C                   Z-ADD     D$DAAS        SDOANN
631200931112     C                   Z-ADD     WDMES         SDOMES
631300941117     C                   Z-ADD     WCDKSC        SDOKSC
631400931112     C* VALORI = VALORI 'DOPO'
631500931112     C                   Z-ADD     1             WONSP
631600020829     C*---               Z-ADD     D$DPKF        WOPKG
631700020829     C                   Z-ADD     WDPES         WOPES
631800931112     C                   Z-ADD     D$DNCL        WONCO
631900931112     C                   Z-ADD     WDVOL         WOVOL
632000931112     C                   Z-ADD     WDIRBC        WOIRB
632100931112     C                   Z-ADD     WDIRP         WOIRP
632200931112     C                   Z-ADD     WDIRD         WOIRD
632300931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
632400940411     C     WODREG        IFEQ      'S'
632500931112     C                   EXSR      AGGSDO
632600940411     C                   ENDIF
632700931112     C*
632800940202     C                   END
632900931112     C                   END
633000940411     C                   END
633100931112     C*
633200931112     C                   ENDSR
633300931112     C********************************************************************
633400990617     C*   AGGSDO - AGGIORNA IL FILE SISDO00F (SALDI CLIENTI - CONTABILI)
633500931112     C********************************************************************
633600931112     C     AGGSDO        BEGSR
633700931112     C     WONSP         IFEQ      *ZEROS
633800020829     C*--- WOPKG         ANDEQ     *ZEROS
633900020829     C     WOPES         ANDEQ     *ZEROS
634000931112     C     WONCO         ANDEQ     *ZEROS
634100931112     C     WOVOL         ANDEQ     *ZEROS
634200931112     C     WOIRB         ANDEQ     *ZEROS
634300931112     C     WOIRP         ANDEQ     *ZEROS
634400931112     C     WOIRD         ANDEQ     *ZEROS
634500931112     C                   ELSE
634600990617     C     KEYSDO        CHAIN     SISDO01L                           99
634700931112     C     *IN99         IFEQ      '0'
634800931112     C                   EXSR      ADDSDO
634900990617     C                   UPDATE    SISDO000
635000931112     C                   ELSE
635100931112     C                   EXSR      AZZSDO
635200931112     C                   EXSR      ADDSDO
635300990617     C                   WRITE     SISDO000
635400931112     C                   END
635500931112     C                   END
635600931112     C                   ENDSR
635700931112     C********************************************************************
635800990617     C*   AZZSDO - AZZERA I CAMPI DEL FILE SISDO00F (SALDI CLIENTI - CONTABILI)
635900931112     C********************************************************************
636000931112     C     AZZSDO        BEGSR
636100931112     C                   Z-ADD     *ZEROS        SDONSP
636200931112     C                   Z-ADD     *ZEROS        SDOPKG
636300931112     C                   Z-ADD     *ZEROS        SDONCO
636400931112     C                   Z-ADD     *ZEROS        SDOVOL
636500931112     C                   Z-ADD     *ZEROS        SDOIRB
636600931112     C                   Z-ADD     *ZEROS        SDOIRR
636700931112     C                   Z-ADD     *ZEROS        SDOINC
636800931112     C                   Z-ADD     *ZEROS        SDOIRP
636900931112     C                   Z-ADD     *ZEROS        SDOIRD
637000990617     C                   Z-ADD     *ZEROS        SDOIRC
637100940112     C                   MOVEL     *BLANKS       SDOCLN
637200931112     C                   Z-ADD     *ZEROS        SDONNC
637300940301     C                   MOVEL     *BLANKS       SDOFTR
637400940301     C                   Z-ADD     *ZEROS        SDODTR
637500931112     C                   ENDSR
637600931112     C********************************************************************
637700990617     C*   ADDSDO - AGGIUNGE AI VALORI DEL FILE SISDO00F (SALDI CLIENTI-CONTABILI
637800931112     C********************************************************************
637900931112     C     ADDSDO        BEGSR
638000931112     C                   ADD       WONSP         SDONSP
638100020829     C*---               ADD       WOPKG         SDOPKG
638200020829     C                   ADD       WOPES         SDOPKG
638300931112     C                   ADD       WONCO         SDONCO
638400931112     C                   ADD       WOVOL         SDOVOL
638500020129     C                   ADD       WOIRB         SDOIRB
638600090916     C                   z-add(h)  WOIRP         woirp£
638700090916     C                   ADD       WOIRP£        SDOIRP
638800020129     C                   ADD       WOIRD         SDOIRD
638900940301     C                   MOVEL     *BLANKS       SDOFTR
639000940301     C                   Z-ADD     *ZEROS        SDODTR
639100940317     C                   MOVEL     'S'           FLAGGS
639200931112     C                   ENDSR
639300931112     C*
639400931112     C*
639500931112     C*
639600931112     C*
639700931112     C********************************************************************
639800990617     C*   GESSDP - GESTISCE IL FILE SISDP00F (SALDI CLIENTE - CONS.PART.)
639900931112     C********************************************************************
640000940103     C     GESSDP        BEGSR
640100940411     C*
640200940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
640300940411     C     WPPREG        IFEQ      'S'
640400940411     C     WPDREG        OREQ      'S'
640500970128     C*
640600970128     C* FINO A FINE '96 CLIENTE = CLIENTE BOLLA
640700970128     C* DAL '97 CLIENTE = CLIENTE MAMMA (SE BOLLA FIGLIA)
640800970205     C     D$GSTS        IFEQ      'U'
640900970205     C     D$GSTS        OREQ      'D'
641000970205     C     D$PAAS        IFGE      1997
641100970205     C                   Z-ADD     WCPKSC        WPPKSC
641200970205     C                   ELSE
641300970205     C                   Z-ADD     D$PKSC        WPPKSC
641400970205     C                   ENDIF
641500970205     C                   ENDIF
641600970205     C*
641700970205     C     D$GSTS        IFEQ      'U'
641800970205     C     D$GSTS        OREQ      'I'
641900970205     C     D$DAAS        IFGE      1997
642000970205     C                   Z-ADD     WCDKSC        WPDKSC
642100970205     C                   ELSE
642200970205     C                   Z-ADD     D$DKSC        WPDKSC
642300970205     C                   ENDIF
642400970205     C                   ENDIF
642500940316     C*
642600940316     C* ESEGUO LOOP PER TUTTE LE POSSIBILI CONSEGNA PARTICOLARI
642700940316     C                   DO        20            I
642800940316     C*
642900940316     C* ESEGUO AGGIORNAMENTO SOLO SE VI SONO DATI VALIDI
643000940316     C     SVP(I)        IFEQ      *BLANKS
643100940316     C     SVD(I)        ANDEQ     *BLANKS
643200940316     C     IVP(I)        ANDEQ     *ZEROS
643300940316     C     IVD(I)        ANDEQ     *ZEROS
643400940330     C     NSP(I)        ANDEQ     *ZEROS
643500940330     C     NSD(I)        ANDEQ     *ZEROS
643600940316     C     CPP(I)        ANDEQ     *BLANKS
643700940316     C     CPD(I)        ANDEQ     *BLANKS
643800940316     C                   ELSE
643900931112     C*
644000931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
644100940407     C     D$GSTS        IFEQ      'U'
644200940407     C     D$PAAS        ANDEQ     D$DAAS
644300940103     C     WPMES         ANDEQ     WDMES
644400970128     C     WPPKSC        ANDEQ     WPDKSC
644500940316     C     D$PCTR        ANDEQ     D$DCTR
644600940316     C     SVP(I)        ANDEQ     SVD(I)
644700940316     C     CPP(I)        ANDEQ     CPD(I)
644800940411     C     WPPREG        ANDEQ     WPDREG
644900931112     C*
645000931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
645100931112     C*
645200931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
645300931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
645400940103     C                   Z-ADD     D$DAAS        SDPANN
645500940103     C                   Z-ADD     WDMES         SDPMES
645600970128     C                   Z-ADD     WPDKSC        SDPKSC
645700940316     C                   MOVEL     D$DCTR        SDPCTR
645800940316     C                   MOVEL     SVD(I)        SDPSVR
645900940316     C                   MOVEL     CPD(I)        SDPFTC
646000931112     C* VALORI = 'DOPO' - 'PRIMA'
646100940330     C     NSD(I)        SUB       NSP(I)        WPNSP
646200940316     C     IVD(I)        SUB       IVP(I)        WPRCP
646300931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
646400940103     C                   EXSR      AGGSDP
646500931112     C*
646600940316     C                   ELSE
646700931112     C*
646800931112     C******************** E' CAMBIATA LA CHIAVE *************************
646900931112     C*
647000940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
647100940411     C     D$GSTS        IFEQ      'U'
647200940405     C     D$GSTS        OREQ      'D'
647300931112     C*
647400931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
647500931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
647600931112     C* CHIAVE = CHIAVE 'PRIMA'
647700940103     C                   Z-ADD     D$PAAS        SDPANN
647800940103     C                   Z-ADD     WPMES         SDPMES
647900970128     C                   Z-ADD     WPPKSC        SDPKSC
648000940316     C                   MOVEL     D$PCTR        SDPCTR
648100940316     C                   MOVEL     SVP(I)        SDPSVR
648200940316     C                   MOVEL     CPP(I)        SDPFTC
648300931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
648400940330     C     *ZEROS        SUB       NSP(I)        WPNSP
648500940316     C     *ZEROS        SUB       IVP(I)        WPRCP
648600931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
648700940411     C     WPPREG        IFEQ      'S'
648800940103     C                   EXSR      AGGSDP
648900940411     C                   ENDIF
649000931112     C*
649100940411     C                   END
649200940202     C*
649300940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
649400940411     C     D$GSTS        IFEQ      'U'
649500940405     C     D$GSTS        OREQ      'I'
649600940202     C*
649700940202     C* NON E' UN ANNULLAMENTO, QUINDI
649800931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
649900931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
650000931112     C* CHIAVE = CHIAVE 'DOPO'
650100940103     C                   Z-ADD     D$DAAS        SDPANN
650200940103     C                   Z-ADD     WDMES         SDPMES
650300970128     C                   Z-ADD     WPDKSC        SDPKSC
650400940316     C                   MOVEL     D$DCTR        SDPCTR
650500940316     C                   MOVEL     SVD(I)        SDPSVR
650600940316     C                   MOVEL     CPD(I)        SDPFTC
650700931112     C* VALORI = VALORI 'DOPO'
650800940330     C                   Z-ADD     NSD(I)        WPNSP
650900940316     C                   Z-ADD     IVD(I)        WPRCP
651000931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
651100940411     C     WPDREG        IFEQ      'S'
651200940103     C                   EXSR      AGGSDP
651300940411     C                   ENDIF
651400931112     C*
651500940316     C                   ENDIF
651600940316     C                   ENDIF
651700940316     C                   ENDIF
651800940316     C                   ENDDO
651900940411     C                   ENDIF
652000931112     C*
652100940103     C                   ENDSR
652200931112     C********************************************************************
652300990617     C*   AGGSDP - AGGIORNA IL FILE SISDP00F (SALDI CLIENTI - CONS.PART)
652400931112     C********************************************************************
652500940103     C     AGGSDP        BEGSR
652600940103     C     WPNSP         IFEQ      *ZEROS
652700940103     C     WPRCP         ANDEQ     *ZEROS
652800940103     C                   ELSE
652900990617     C     KEYSDP        CHAIN     SISDP01L                           99
653000940103     C     *IN99         IFEQ      '0'
653100940103     C                   EXSR      ADDSDP
653200990617     C                   UPDATE    SISDP000
653300940103     C                   ELSE
653400940103     C                   EXSR      AZZSDP
653500940103     C                   EXSR      ADDSDP
653600990617     C                   WRITE     SISDP000
653700940103     C                   END
653800940103     C                   END
653900940103     C                   ENDSR
654000931112     C********************************************************************
654100990617     C*   AZZSDP - AZZERA I CAMPI DEL FILE SISDP00F (SALDI CLIENTI - CONS.PART)
654200931112     C********************************************************************
654300940103     C     AZZSDP        BEGSR
654400940103     C                   Z-ADD     *ZEROS        SDPNSP
654500940103     C                   Z-ADD     *ZEROS        SDPRCP
654600940103     C                   MOVEL     *BLANKS       SDPFTR
654700940103     C                   Z-ADD     *ZEROS        SDPDTR
654800940103     C                   ENDSR
654900931112     C********************************************************************
655000990617     C*   ADDSDP - AGGIUNGE AI VALORI DEL FILE SISDP00F (SALDI CLIENTI-CONS.PART
655100931112     C********************************************************************
655200940103     C     ADDSDP        BEGSR
655300940103     C                   ADD       WPNSP         SDPNSP
655400020129     C                   ADD       WPRCP         SDPRCP
655500940301     C                   MOVEL     *BLANKS       SDPFTR
655600940301     C                   Z-ADD     *ZEROS        SDPDTR
655700940317     C                   MOVEL     'S'           FLAGGS
655800940103     C                   ENDSR
655900931112     C********************************************************************
656000990617     C*   GESSDA - GESTISCE IL FILE SISDA00F (SALDI CLIENTE - AFFIDABIL.)
656100931112     C********************************************************************
656200931112     C     GESSDA        BEGSR
656300940411     C*
656400940411     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
656500940411     C     WAPREG        IFEQ      'S'
656600940411     C     WADREG        OREQ      'S'
656700931112     C*
656800931112     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
656900940407     C     D$GSTS        IFEQ      'U'
657000940407     C     D$PAAS        ANDEQ     D$DAAS
657100940125     C     D$PKSC        ANDEQ     D$DKSC
657200941212     C     D$PNRC        ANDEQ     D$DNRC
657300931112     C     WPMES         ANDEQ     WDMES
657400931112     C     D$PTSP        ANDEQ     D$DTSP
657500940411     C     WAPREG        ANDEQ     WADREG
657600931112     C*
657700931112     C******************** NON E' CAMBIATA LA CHIAVE *********************
657800931112     C*
657900931112     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
658000931112     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
658100931112     C                   Z-ADD     D$DAAS        SDAANN
658200940125     C                   Z-ADD     D$DKSC        SDAKSC
658300941212     C                   Z-ADD     D$DNRC        SDADLV
658400931112     C                   Z-ADD     WDMES         SDAMES
658500931112     C                   MOVEL     D$DTSP        SDATSP
658600931112     C* VALORI = 'DOPO' - 'PRIMA'
658700940512     C                   Z-ADD     *ZEROS        WANSP
658800931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
658900931112     C*********************EXSR AGGSDA** NON E' NECESSARIO SE C'E' SOLO N.SPED.
659000931112     C*
659100931112     C                   ELSE
659200931112     C*
659300931112     C******************** E' CAMBIATA LA CHIAVE *************************
659400931112     C*
659500940202     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
659600940405     C     D$GSTS        IFEQ      'U'
659700940405     C     D$GSTS        OREQ      'D'
659800931112     C*
659900931112     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
660000931112     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
660100931112     C* CHIAVE = CHIAVE 'PRIMA'
660200931112     C                   Z-ADD     D$PAAS        SDAANN
660300940125     C                   Z-ADD     D$PKSC        SDAKSC
660400941212     C                   Z-ADD     D$PNRC        SDADLV
660500931112     C                   Z-ADD     WPMES         SDAMES
660600931112     C                   MOVEL     D$PTSP        SDATSP
660700931112     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
660800940411     C     *ZEROS        SUB       1             WANSP
660900931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
661000940411     C     WAPREG        IFEQ      'S'
661100931112     C                   EXSR      AGGSDA
661200940411     C                   ENDIF
661300931112     C*
661400931112     C                   END
661500940202     C*
661600940202     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
661700940405     C     D$GSTS        IFEQ      'U'
661800940405     C     D$GSTS        OREQ      'I'
661900940202     C*
662000940202     C* NON E' UN ANNULLAMENTO, QUINDI
662100931112     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
662200931112     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
662300931112     C* CHIAVE = CHIAVE 'DOPO'
662400931112     C                   Z-ADD     D$DAAS        SDAANN
662500940125     C                   Z-ADD     D$DKSC        SDAKSC
662600941212     C                   Z-ADD     D$DNRC        SDADLV
662700931112     C                   Z-ADD     WDMES         SDAMES
662800931112     C                   MOVEL     D$DTSP        SDATSP
662900931112     C* VALORI = VALORI 'DOPO'
663000940411     C                   Z-ADD     1             WANSP
663100931112     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
663200940411     C     WADREG        IFEQ      'S'
663300931112     C                   EXSR      AGGSDA
663400940411     C                   ENDIF
663500931112     C*
663600940202     C                   END
663700931112     C                   END
663800940411     C                   END
663900931112     C*
664000931112     C                   ENDSR
664100931112     C********************************************************************
664200990617     C*   AGGSDA - AGGIORNA IL FILE SISDA00F (SALDI CLIENTI - AFFIDAB. )
664300931112     C********************************************************************
664400931112     C     AGGSDA        BEGSR
664500931112     C     WANSP         IFEQ      *ZEROS
664600931112     C                   ELSE
664700990617     C     KEYSDA        CHAIN     SISDA01L                           99
664800931112     C     *IN99         IFEQ      '0'
664900931112     C                   EXSR      ADDSDA
665000990617     C                   UPDATE    SISDA000
665100931112     C                   ELSE
665200931112     C                   EXSR      AZZSDA
665300931112     C                   EXSR      ADDSDA
665400990617     C                   WRITE     SISDA000
665500931112     C                   END
665600931112     C                   END
665700931112     C                   ENDSR
665800931112     C********************************************************************
665900990617     C*   AZZSDA - AZZERA I CAMPI DEL FILE SISDA00F (SALDI CLIENTI - AFFIDAB. )
666000931112     C********************************************************************
666100931112     C     AZZSDA        BEGSR
666200931112     C                   Z-ADD     *ZEROS        SDANSP
666300940301     C                   MOVEL     *BLANKS       SDAFTR
666400940301     C                   Z-ADD     *ZEROS        SDADTR
666500931112     C                   ENDSR
666600931112     C********************************************************************
666700990617     C*   ADDSDA - AGGIUNGE AI VALORI DEL FILE SISDA00F (SALDI CLIENTI-AFFIDAB.
666800931112     C********************************************************************
666900931112     C     ADDSDA        BEGSR
667000931112     C                   ADD       WANSP         SDANSP
667100940301     C                   MOVEL     *BLANKS       SDAFTR
667200940301     C                   Z-ADD     *ZEROS        SDADTR
667300940317     C                   MOVEL     'S'           FLAGGS
667400931112     C                   ENDSR
667500931112     C*
667600931112     C*
667700940906     C*
667800950124     C*
667900030114     C********************************************************************
668000030114     C*   GESCOP - GESTISCE IL FILE SICOP00F (SALDI COOPERATIVE)
668100030114     C********************************************************************
668200030114     C     GESCOP        BEGSR
668300030122     c*
668400030114     C* ELABORO SOLO SE L'IMM. PRIMA O DOPO SONO DA REGISTRARE
668500030115IF  1C     WCOPREG       IFEQ      'S'
668600030115     C     WCODREG       OREQ      'S'
668700030115     C*
668800030122     C* PER CIASCUNA VOCE DA SCRIVERE (da scrivere per le COOP)
668900030115DO  2C     1             DO        999           CD
669000030217IF  3C     COPPV(CD)     IFEQ      'S'
669100030217     C     COPDV(CD)     OREQ      'S'
669200030115     C*
669300030115     C* ESEGUO AGGIORNAMENTO SOLO SE VI SONO DATI VALIDI
669400030124IF  4C     IMPV(CD)      IFEQ      *ZEROS
669500030124     C     IMDV(CD)      ANDEQ     *ZEROS
669600030124X   4C                   ELSE
669700030115     C*
669800030120     C* CALCOLO LA FILIALE e LA DATA DA CONSIDERARE
669900030117     C* -PRIMA-
670000030122if  5C     D$GSTS        IFEQ      'U'
670100030120     C     D$GSTS        OREQ      'D'
670200030120     C                   MOVEL     'P'           wpd
670300030217     C                   CLEAR                   WPFIL
670400030217     C                   CLEAR                   WPDAT
670500030217if  6C     COPPV(CD)     IFEQ      'S'
670600030120     c* filiale
670700030217     C                   EVAL      $RIC = RLP(CMPPV(CD))
670800030120     C                   EXSR      REPCDC
670900030120     C                   Z-ADD     $FIL          WPFIL
671000030120     c* data
671100030217     C                   EVAL      $TDAT = DATPV(CD)
671200030120     C                   EXSR      REPDAT
671300030120     C                   Z-ADD     $DAT          WPDAT
671400030217e   6c                   ENDIF
671500030217e   5c                   ENDIF
671600030120     C* -DOPO-
671700030122if  5C     D$GSTS        IFEQ      'U'
671800030120     C     D$GSTS        OREQ      'I'
671900030120     C                   MOVEL     'D'           wpd
672000030217     C                   CLEAR                   WDFIL
672100030217     C                   CLEAR                   WDDAT
672200030217if  6C     COPDV(CD)     IFEQ      'S'
672300030120     c* filiale
672400030122     C                   EVAL      $RIC = RLD(CMPDV(CD))
672500030120     C                   EXSR      REPCDC
672600030120     C                   Z-ADD     $FIL          WDFIL
672700030120     c* data
672800030120     C                   EVAL      $TDAT = DATDV(CD)
672900030120     C                   EXSR      REPDAT
673000030122     C                   Z-ADD     $DAT          WDDAT
673100030217e   6c                   ENDIF
673200030122e   5C                   ENDIF
673300030217     C***********************
673400030217     C***********************
673500030217     C***********************
673600030127     C* SE LE DUE DATE SONO DIVERSE E SONO IN MODIFICA E LA VOCE PREVEDE IL
673700030127     C* FATTO CHE L'IMPORTO VA SEMPRE INSERITO E MAI SOMMATO, ALLORA IMPOSTA UN
673800030127     C* FLAG PER NON ANDARE A MODIFICARE IL -PRIMA- MA INSERIRE SOLO IL -DOPO-
673900030127     c                   MOVEL     'N'           $INS              1
674000030127IF  5C     D$GSTS        IFEQ      'U'
674100030127     C     WPDAT         ANDNE     WDDAT
674200030127     C     AIDDV(CD)     ANDEQ     'S'
674300030127     c                   MOVEL     'S'           $INS
674400030127e   5C                   ENDIF
674500030217     C***********************
674600030217     C***********************
674700030217     C***********************
674800030114     C*
674900030114     C* CONTROLLO SE E' CAMBIATA LA CHIAVE
675000030124IF  5C     D$GSTS        IFEQ      'U'
675100030122     C     WPFIL         ANDEQ     WDFIL
675200030120     C     WPDAT         ANDEQ     WDDAT
675300030120     C     CD            ANDEQ     CD
675400030131     C     WPMES         ANDEQ     WDMES
675500030131     C     D$PAAS        ANDEQ     D$DAAS
675600030120     C     WCOPREG       ANDEQ     WCODREG
675700030114     C*
675800030114     C******************** NON E' CAMBIATA LA CHIAVE *********************
675900030114     C*
676000030114     C* AGGIORNO CON LA DIFFERENZA TRA -DOPO- E -PRIMA-
676100030114     C* CHIAVE = CHIAVE 'DOPO' (UGUALE A 'PRIMA')
676200030121     C                   Z-ADD     WDFIL         COPFIL
676300030121     C                   Z-ADD     WDDAT         COPDAT
676400030121     C                   Z-ADD     CD            COPVOC
676500030131     C                   Z-ADD     D$DAAS        COPAAS
676600030131     C                   Z-ADD     WDMES         COPMES
676700030214     c*---
676800030214     c* se una delle due immagini sono uguali a zero, significa che la voce C/E
676900030214     c* è nuova (>0 il -dopo-) o è da eliminare (>0 il -prima-). Questo lo fa
677000030214     c* bene per l'importo perchgè ha la situazione prima (IMPV) e dopo (IMDV)
677100030214     c* memorizzate, ma non per le quantità. Lo fa qui il pgm azzerando le qta
677200030214     c* della bolla per fare bene i calcoli (senno per es, 1 collo era prima, 1
677300030214     c* collo è ora, 1-1 = 0 e scrive l'importo della voce giusto ma su qta a 0)
677400030214     C*---
677500030214     C* VALORI = 'DOPO' - 'PRIMA'
677600030214     C* Voce nuova
677700030214     C                   IF        IMDV(CD) <> 0 AND IMPV(CD) = 0
677800030214     C                   Z-ADD     1             WCONSP
677900030214     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
678000030214     C     D$DNCL        SUB       *ZEROS        WCONCL
678100030214     C     WDPES         SUB       *ZEROS        WCOPKF
678200030214     C     WDVOL         SUB       *ZEROS        WCOVLF
678300040123     C     WDBAN         SUB       *ZEROS        WCOBAN
678400030214     c                   ENDIF
678500030214     C* Voce da eliminare
678600030214     C                   IF        IMDV(CD) = 0 AND IMPV(CD) <> 0
678700030214     C     *ZEROS        SUB       1             WCONSP
678800030214     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
678900030214     C     *ZEROS        SUB       D$PNCL        WCONCL
679000030214     C     *ZEROS        SUB       WPPES         WCOPKF
679100030214     C     *ZEROS        SUB       WPVOL         WCOVLF
679200040123     C     *ZEROS        SUB       WPBAN         WCOBAN
679300030214     c                   ENDIF
679400030214     C* Voce da modificare
679500030214     C                   IF        IMDV(CD) <> 0 AND IMPV(CD) <> 0
679600030121     C                   Z-ADD     *ZEROS        WCONSP
679700030122     C     IMDV(CD)      SUB       IMPV(CD)      WCOIMV
679800030121     C     D$DNCL        SUB       D$PNCL        WCONCL
679900030121     C     WDPES         SUB       WPPES         WCOPKF
680000030121     C     WDVOL         SUB       WPVOL         WCOVLF
680100040123     C     WDBAN         SUB       WPBAN         WCOBAN
680200030214     c                   ENDIF
680300030214     c*
680400030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
680500030121     C                   EXSR      AGGCOP
680600030114     C*
680700030122X   5C                   ELSE
680800030114     C*
680900030114     C******************** E' CAMBIATA LA CHIAVE *************************
681000030114     C*
681100030114     C* CONTROLLO SE IL RECORD -PRIMA- E' AZZERATO (INSERIMENTO NUOVA BOLLA)
681200030122IF  6C     D$GSTS        IFEQ      'U'
681300030114     C     D$GSTS        OREQ      'D'
681400030114     C*
681500030114     C* NON E' AZZERATO QUINDI E' UNA VARIAZIONE DI UNA BOLLA GIA' INSERITA
681600030114     C* NEI SALDI, TOLGO DAI SALDI CON CHIAVE -PRIMA- I VALORI -PRIMA-
681700030114     C* CHIAVE = CHIAVE 'PRIMA'
681800030217if  7C     COPPV(CD)     IFEQ      'S'
681900030217IF  8C     $INS          IFNE      'S'
682000030122     C                   Z-ADD     WPFIL         COPFIL
682100030122     C                   Z-ADD     WPDAT         COPDAT
682200030122     C                   Z-ADD     CD            COPVOC
682300030131     C                   Z-ADD     D$PAAS        COPAAS
682400030131     C                   Z-ADD     WPMES         COPMES
682500030114     C* VALORI = ZERO - 'PRIMA'    (QUINDI NEGATIVI)
682600030122     C     *ZEROS        SUB       1             WCONSP
682700030122     C     *ZEROS        SUB       IMPV(CD)      WCOIMV
682800030122     C     *ZEROS        SUB       D$PNCL        WCONCL
682900030122     C     *ZEROS        SUB       WPPES         WCOPKF
683000030122     C     *ZEROS        SUB       WPVOL         WCOVLF
683100040123     C     *ZEROS        SUB       WPBAN         WCOBAN
683200030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
683300030217IF  9C     WCOPREG       IFEQ      'S'
683400030122     C                   EXSR      AGGCOP
683500030217E   9C                   ENDIF
683600030217E   8C                   ENDIF
683700030217E   7C                   ENDIF
683800030114     C*
683900030122E   6C                   ENDIF
684000030114     C*
684100030114     C* CONTROLLO SE IL RECORD -DOPO- E' AZZERATO (ANNULLAMENTO BOLLA)
684200030122IF  6C     D$GSTS        IFEQ      'U'
684300030114     C     D$GSTS        OREQ      'I'
684400030114     C*
684500030114     C* NON E' UN ANNULLAMENTO, QUINDI
684600030114     C* SIA PER L'INSERIMENTO PER LA PRIMA VOLTA NEI SALDI DELLA BOLLA CHE
684700030114     C* PER LA VARIAZIONE DELLA BOLLA GIA' INS., AGGIORNO CON VALORI -DOPO-
684800030114     C* CHIAVE = CHIAVE 'DOPO'
684900030217if  7C     COPDV(CD)     IFEQ      'S'
685000030122     C                   Z-ADD     WDFIL         COPFIL
685100030122     C                   Z-ADD     WDDAT         COPDAT
685200030122     C                   Z-ADD     CD            COPVOC
685300030131     C                   Z-ADD     D$DAAS        COPAAS
685400030131     C                   Z-ADD     WDMES         COPMES
685500030114     C* VALORI = VALORI 'DOPO'
685600030122     C                   Z-ADD     1             WCONSP
685700030122     C                   Z-ADD     IMDV(CD)      WCOIMV
685800030122     C                   Z-ADD     D$DNCL        WCONCL
685900030122     C                   Z-ADD     WDPES         WCOPKF
686000030122     C                   Z-ADD     WDVOL         WCOVLF
686100040123     C                   Z-ADD     WDBAN         WCOBAN
686200030114     C* ESEGUE SCRITTURA O RISCRITTURA SUL FILE
686300030217IF  8C     WCODREG       IFEQ      'S'
686400030122     C                   EXSR      AGGCOP
686500030217E   8C                   ENDIF
686600030217E   7C                   ENDIF
686700030114     C*
686800030122E   6C                   ENDIF
686900030122E   5C                   ENDIF
687000030124E   4C                   ENDIF
687100030122     c*
687200030122E   3C                   ENDIF
687300030122E   2C                   ENDDO
687400030122E   1C                   ENDIF
687500030114     C*
687600030114     C                   ENDSR
687700030114     C********************************************************************
687800030121     C*   AGGCOP - AGGIORNA IL FILE SICOP00F (SALDI COOPERATIVE)
687900030114     C********************************************************************
688000030115     C     AGGCOP        BEGSR
688100040126     c*
688200040126     c* non aggiorna il numero dei bancali BNB se  voce 34 e DISK B
688300040126     c* (equivalente a serie > 0) e particolarità consegne bancali
688400040126if  1c                   IF        copvoc = 34 and
688500040126     c                             ((d$gsto = 'I' and d$dnrs > 0 and
688600040126     c                              %subst(d$dgva: 1: 1) = 'O') or
688700040126     c                             (d$gsto = 'U' and d$dnrs > 0 and
688800040126     c                              %subst(d$dgva: 1: 1) = 'O') or
688900040126     c                             (d$gsto = 'D' and d$pnrs > 0 and
689000040126     c                              %subst(d$pgva: 1: 1) = 'O'))
689100040126     c                   z-add     0             wcoban
689200040126e   1c                   endif
689300040119     c*
689400030122IF  1C     WCOIMV        IFEQ      *ZEROS
689500030122     C     WCONSP        ANDEQ     *ZEROS
689600030122     C     WCONCL        ANDEQ     *ZEROS
689700030121     C     WCOPKF        ANDEQ     *ZEROS
689800030122     C     WCOVLF        ANDEQ     *ZEROS
689900040119     C     WCOBAN        ANDEQ     *ZEROS
690000030122X   1C                   ELSE
690100030122     C     KEYCOP        CHAIN     SICOP01L                           99
690200030122IF  2C     *IN99         IFEQ      '0'
690300030122     C                   EXSR      ADDCOP
690400030122     C                   UPDATE    SICOP000
690500030122X   2C                   ELSE
690600030122     C                   EXSR      AZZCOP
690700030122     C                   EXSR      ADDCOP
690800030122     C                   WRITE     SICOP000
690900030122E   2C                   ENDIF
691000030122E   1C                   ENDIF
691100030122     C*
691200030114     C                   ENDSR
691300030114     C********************************************************************
691400030122     C*   AZZCOP - AZZERA I CAMPI DEL FILE SICOP00F (SALDI COOPERATIVE)
691500030114     C********************************************************************
691600030115     C     AZZCOP        BEGSR
691700030122     C*
691800030122     C                   Z-ADD     *ZEROS        COPIMV
691900030122     C                   Z-ADD     *ZEROS        COPNSP
692000030122     C                   Z-ADD     *ZEROS        COPNCL
692100030122     C                   Z-ADD     *ZEROS        COPPKF
692200030122     C                   Z-ADD     *ZEROS        COPVLF
692300031029     C                   Z-ADD     *ZEROS        COPBAN
692400030122     C*
692500030114     C                   ENDSR
692600030114     C********************************************************************
692700030122     C*   ADDCOP - AGGIUNGE AI VALORI DEL FILE SICOP00F (SALDI COOPERATIVE)
692800030114     C********************************************************************
692900030115     C     ADDCOP        BEGSR
693000030122     C*
693100090916     C                   z-add(h)  WCOIMV        wcoimv£
693200090916     C                   ADD       WCOIMV£       COPIMV
693300030122     C                   ADD       WCONSP        COPNSP
693400030122     C                   ADD       WCONCL        COPNCL
693500030122     C                   ADD       WCOPKF        COPPKF
693600030122     C                   ADD       WCOVLF        COPVLF
693700031029     C                   ADD       WCOBAN        COPBAN
693800030114     C                   MOVEL     'S'           FLAGGS
693900030122     C*
694000030114     C                   ENDSR
694100950124     C*------------------------------------------------------------------------*
694200950124     C*?GESCCD - AGGIORNA LE SCHIERE COD.E IMP. COMPETENZE/RICAVI IN OUTPUT
694300950124     C*------------------------------------------------------------------------*
694400950124     C     GESCCD        BEGSR
694500950124     C*
694600950124     C                   Z-ADD     0             II
694700030505     C     1             DO        75            I
694800950124     C                   Z-ADD     *ZEROS        DSWCN
694900950124     C                   MOVE      DSWCA         CAN(I)
695000950124     C                   Z-ADD     *ZEROS        DSWIN
695100950124     C                   MOVE      DSWIA         IAN(I)
695200950124     C                   ENDDO
695300950124     C     1             DO        999           I
695400950124     C     IMD(I)        IFNE      *ZEROS
695500950124     C                   ADD       1             II
695600950124     C                   Z-ADD     I             DSWCN
695700950124     C                   MOVE      DSWCA         CAN(II)
695800950124     C                   Z-ADD     IMD(I)        DSWIN
695900950124     C                   MOVE      DSWIA         IAN(II)
696000950124     C                   ENDIF
696100950124     C                   ENDDO
696200950124     C                   MOVEA     CAN           D$DCCD
696300950124     C                   MOVEA     IAN           D$DICD
696400931112     C*
696500950124     C                   ENDSR
696600941026     C*------------------------------------------------------------------------*
696700990617     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SISDE (ERRORI SALDI)
696800941026     C*------------------------------------------------------------------------*
696900941026     C     SCRERR        BEGSR
697000941026     C*
697100941026     C                   MOVEL     'S'           $ERR
697200941026     C*
697300941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
697400941026     C     D$GERR        IFEQ      *BLANKS
697500941026     C*
697600941026     C                   EXSR      SUBDAT
697700941026     C                   Z-ADD     DSDAT8        ERRDAT
697800941026     C                   Z-ADD     DTORA6        ERRTIM
697900941026     C                   ADD       1             D$GPRE
698000941026     C                   Z-ADD     D$GPRE        ERRPRG
698100941201     C     D$GSTS        IFEQ      'I'
698200941124     C                   Z-ADD     D$DAAS        ERRAAS
698300941026     C                   MOVEL     D$DTBL        ERRTBL
698400941026     C                   Z-ADD     D$DLNP        ERRLNP
698500941026     C                   Z-ADD     D$DNRS        ERRNRS
698600941026     C                   Z-ADD     D$DNSP        ERRNSP
698700941201     C                   Z-ADD     D$DAAS        DSAAS
698800941123     C                   Z-ADD     D$DMGS        DSMGS
698900941123     C                   Z-ADD     DSDAT8        ERRDSP
699000941201     C                   ELSE
699100941201     C                   Z-ADD     D$PAAS        ERRAAS
699200941201     C                   MOVEL     D$PTBL        ERRTBL
699300941201     C                   Z-ADD     D$PLNP        ERRLNP
699400941201     C                   Z-ADD     D$PNRS        ERRNRS
699500941201     C                   Z-ADD     D$PNSP        ERRNSP
699600941201     C                   Z-ADD     D$PAAS        DSAAS
699700941201     C                   Z-ADD     D$PMGS        DSMGS
699800941201     C                   Z-ADD     DSDAT8        ERRDSP
699900941201     C                   ENDIF
700000941026     C                   MOVEL     $MSG          DSMSG
700100941026     C                   MOVEL     DSDER         ERRDER
700200941026     C                   MOVEL     DSCER         ERRCER
700300950123     C                   MOVEL     D$GSTO        ERRSTS
700400941026     C                   Z-ADD     D$GDAT        ERRDAM
700500941026     C                   Z-ADD     D$GTIM        ERRORM
700600941026     C                   MOVEL     D$GUSR        ERRUSR
700700941026     C                   MOVEL     D$GPGF        ERRPGF
700800950118     C                   MOVEL     D$GFIL        ERRFIL
700900950118     C                   MOVEL     D$GMBR        ERRMBR
701000950118     C                   MOVEL     D$GLIB        ERRLIB
701100990806     C     D$GFIL        IFEQ      'TITAS10F'
701200020527     C     D$GFIL        OREQ      'TITAS00F'
701300950118     C                   MOVEL     D$PRE1        ERRRP1
701400950118     C                   MOVEL     D$PRE2        ERRRP2
701500950118     C                   MOVEL     D$DRE1        ERRRD1
701600950118     C                   MOVEL     D$DRE2        ERRRD2
701700950123     C                   ENDIF
701800950123     C     D$GFIL        IFEQ      'TNCSB00F'
701900950123     C                   MOVEL     D$PCO1        ERRRP1
702000950123     C                   MOVEL     D$PCO2        ERRRP2
702100950123     C                   MOVEL     D$DCO1        ERRRD1
702200950123     C                   MOVEL     D$DCO2        ERRRD2
702300950123     C                   ENDIF
702400031127     C     D$GFIL        IFEQ      'FIAR500F'
702500031127     C     D$GTRD        IFEQ      'TRS'
702600031127     C                   MOVEL     D$PAR5T       ERRRP1
702700031127     C                   MOVEL     D$DAR5T       ERRRD1
702800031127     C                   ENDIF
702900031127     C     D$GTRD        IFEQ      'BAN'
703000031127     C     D$GTRD        OREQ      'BNB'
703100031127     C                   MOVEL     D$PAR5B       ERRRP1
703200031127     C                   MOVEL     D$DAR5B       ERRRD1
703300031127     C                   ENDIF
703400031127     C                   ENDIF
703500941026     C                   WRITE     ECERR
703600941026     C*
703700941026     C                   MOVEL     DSCER         D$GERR
703800941026     C                   MOVEL     DSDER         D$GMSG
703900941026     C*
704000941026     C                   ENDIF
704100941026     C*
704200941026     C                   ENDSR
704300941026     C*------------------------------------------------------------------------*
704400941026     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
704500941026     C*------------------------------------------------------------------------*
704600941026     C     SUBDAT        BEGSR
704700941026     C*
704800941026     C                   TIME                    DTTIME
704900941026     C                   Z-ADD     DTANN         DSANN
705000941026     C                   Z-ADD     DTMES         DSMES
705100941026     C                   Z-ADD     DTGIO         DSGIO
705200941026     C                   Z-ADD     DTSEC         DSSEC
705300941026     C*
705400941026     C                   ENDSR
705500940808     C********************************************************************
705600940808     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
705700940808     C********************************************************************
705800940808     C     AZZW          BEGSR
705900940808     C*
706000940808     C                   Z-ADD     *ZEROS        WPMES
706100940808     C                   Z-ADD     *ZEROS        WDMES
706200940808     C                   MOVEL     *BLANKS       WPDEC
706300940808     C                   MOVEL     *BLANKS       WDDEC
706400940808     C                   MOVEL     *BLANKS       WPFPR
706500940808     C                   MOVEL     *BLANKS       WDFPR
706600940808     C                   MOVEL     *BLANKS       WPFPC
706700940808     C                   MOVEL     *BLANKS       WDFPC
706800940808     C                   MOVEL     *BLANKS       WPFPT
706900940808     C                   MOVEL     *BLANKS       WDFPT
707000940808     C                   MOVEL     *BLANKS       WPTPR
707100940808     C                   MOVEL     *BLANKS       WDTPR
707200940809     C                   Z-ADD     *ZEROS        WPIRP
707300940808     C                   Z-ADD     *ZEROS        WDIRP
707400030131     C                   Z-ADD     *ZEROS        WPDATCOP
707500030131     C                   Z-ADD     *ZEROS        WDDATCOP
707600940808     C                   Z-ADD     *ZEROS        WPVOL
707700940808     C                   Z-ADD     *ZEROS        WDVOL
707800940808     C                   Z-ADD     *ZEROS        WPVCC
707900940808     C                   Z-ADD     *ZEROS        WDVCC
708000940808     C                   Z-ADD     *ZEROS        WPVLS
708100940808     C                   Z-ADD     *ZEROS        WDVLS
708200020829     C                   Z-ADD     *ZEROS        WPPES
708300020829     C                   Z-ADD     *ZEROS        WDPES
708400020829     C                   Z-ADD     *ZEROS        WPPCC
708500020829     C                   Z-ADD     *ZEROS        WDPCC
708600020829     C                   Z-ADD     *ZEROS        WPPLS
708700020829     C                   Z-ADD     *ZEROS        WDPLS
708800940808     C                   Z-ADD     *ZEROS        WPPTS
708900940808     C                   Z-ADD     *ZEROS        WDPTS
709000940808     C                   Z-ADD     *ZEROS        WPIRB
709100940808     C                   Z-ADD     *ZEROS        WDIRB
709200941024     C                   Z-ADD     *ZEROS        WPIRBC
709300941024     C                   Z-ADD     *ZEROS        WDIRBC
709400940808     C                   Z-ADD     *ZEROS        WPIRD
709500940808     C                   Z-ADD     *ZEROS        WDIRD
709600940930     C                   Z-ADD     *ZEROS        WPIR
709700940930     C                   Z-ADD     *ZEROS        WDIR
709800941209     C                   Z-ADD     *ZEROS        WOIR
709900940808     C                   Z-ADD     *ZEROS        WPREP
710000940808     C                   Z-ADD     *ZEROS        WDREP
710100940808     C                   Z-ADD     *ZEROS        WPREA
710200940808     C                   Z-ADD     *ZEROS        WDREA
710300940823     C                   Z-ADD     *ZEROS        WPCTL
710400940823     C                   Z-ADD     *ZEROS        WDCTL
710500940823     C                   Z-ADD     *ZEROS        WPCT1
710600940823     C                   Z-ADD     *ZEROS        WDCT1
710700940823     C                   Z-ADD     *ZEROS        WPCT2
710800940823     C                   Z-ADD     *ZEROS        WDCT2
710900040519     C                   Z-ADD     *ZEROS        WPCT3
711000040519     C                   Z-ADD     *ZEROS        WDCT3
711100040519     C                   Z-ADD     *ZEROS        WPCT4
711200040519     C                   Z-ADD     *ZEROS        WDCT4
711300040519     C                   Z-ADD     *ZEROS        WPCT5
711400040519     C                   Z-ADD     *ZEROS        WDCT5
711500980313     C                   Z-ADD     *ZEROS        WPSMC
711600980313     C                   Z-ADD     *ZEROS        WPSMCT
711700980313     C                   Z-ADD     *ZEROS        WDSMC
711800980319     C                   Z-ADD     *ZEROS        WSMC
711900940808     C                   MOVEA     *BLANKS       SVP
712000940808     C                   MOVEA     *BLANKS       SVD
712100940808     C                   MOVEA     *BLANKS       SVX
712200940808     C                   Z-ADD     *ZEROS        IVP
712300940808     C                   Z-ADD     *ZEROS        IVD
712400940808     C                   Z-ADD     *ZEROS        IVX
712500940808     C                   Z-ADD     *ZEROS        NSP
712600940808     C                   Z-ADD     *ZEROS        NSD
712700940808     C                   Z-ADD     *ZEROS        NSX
712800940808     C                   MOVEA     *BLANKS       CPP
712900940808     C                   MOVEA     *BLANKS       CPD
713000940808     C                   MOVEA     *BLANKS       CPX
713100990617     C                   MOVEL     'S'           WAPREG            1
713200990617     C                   MOVEL     'S'           WADREG            1
713300990617     C                   MOVEL     'S'           WCPREG            1
713400990617     C                   MOVEL     'S'           WCDREG            1
713500040317     C                   MOVEL     'S'           WePREG            1
713600040317     C                   MOVEL     'S'           WeDREG            1
713700990617     C                   MOVEL     'S'           WOPREG            1
713800990617     C                   MOVEL     'S'           WODREG            1
713900990617     C                   MOVEL     'S'           WRPREG            1
714000990617     C                   MOVEL     'S'           WRDREG            1
714100990617     C                   MOVEL     'S'           WFPREG            1
714200990617     C                   MOVEL     'S'           WFDREG            1
714300990617     C                   MOVEL     'S'           WTPREG            1
714400990617     C                   MOVEL     'S'           WTDREG            1
714500990617     C                   MOVEL     'S'           WPPREG            1
714600990617     C                   MOVEL     'S'           WPDREG            1
714700980323     C                   MOVEL     'N'           WMPREG            1
714800980323     C                   MOVEL     'N'           WMDREG            1
714900030124     C                   MOVEL     'S'           WCOPREG           1
715000030115     C                   MOVEL     'S'           WCODREG           1
715100950104     C                   MOVEL     'N'           WFCP              1
715200950104     C                   MOVEL     'N'           WFCD              1
715300941215     C                   CLEAR                   IMP
715400941215     C                   CLEAR                   IMD
715500030114     C                   CLEAR                   IMPV
715600030214     C                   CLEAR                   COPPV
715700030217     C                   CLEAR                   DATPV
715800030217     C                   CLEAR                   AIDPV
715900030217     C                   CLEAR                   CMPPV
716000030114     C                   CLEAR                   IMDV
716100030214     C                   CLEAR                   COPDV
716200030214     C                   CLEAR                   DATDV
716300030214     C                   CLEAR                   AIDDV
716400030214     C                   CLEAR                   CMPDV
716500031029     C                   CLEAR                   WDBAN
716600031029     C                   CLEAR                   WPBAN
716700940808     C*
716800940808     C                   ENDSR
716900930414     C********************************************************************
717000931105     C*   RUTINZ - ROUTINE INIZIALE
717100930414     C********************************************************************
717200931105     C     RUTINZ        BEGSR
717300930414     C*
717400930414     C* PARAMETRI IN INPUT ALLA ROUTINE
717500930414     C     *ENTRY        PLIST
717600940808     C                   PARM                    DS$ECO
717700930414     C*
717800930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
717900930414     C     KEYTAB        KLIST
718000930414     C                   KFLD                    KTBUTE
718100930414     C                   KFLD                    KTBCOD
718200930414     C                   KFLD                    KTBKEY
718300941223     C*
718400941223     C* CHIAVE LETTURA TABEL00F - PARZIALE
718500941223     C     KEYTA2        KLIST
718600941223     C                   KFLD                    KTBUTE
718700941223     C                   KFLD                    KTBCOD
718800011218     C*
718900011218     C* CHIAVE LETTURA TNTBE01L - PARZIALE
719000011218     C     KEYTBE        KLIST
719100011218     C                   KFLD                    KTECOD
719200011218     C                   KFLD                    KTEKE1
719300020705     C*
719400020705     C* CHIAVE LETTURA TNTBE01L - PARZIALE
719500020705     C     KEYTB2        KLIST
719600020705     C                   KFLD                    KTECOD
719700931111     C*
719800931111     C* CHIAVE LETTURA AZORG01L - COMPLETA
719900931111     C     KEYORG        KLIST
720000931111     C                   KFLD                    KORFIL
720100930414     C*
720200990617     C* CHIAVE LETTURA SISDR01L - COMPLETA
720300931104     C     KEYSDR        KLIST
720400931104     C                   KFLD                    SDRANN
720500931104     C                   KFLD                    SDRFLP
720600931104     C                   KFLD                    SDRFLA
720700940208     C                   KFLD                    SDRMES
720800931104     C                   KFLD                    SDRFPE
720900931104     C                   KFLD                    SDRTSP
721000931104     C                   KFLD                    SDRTPO
721100931104     C                   KFLD                    SDRRBL
721200931104     C                   KFLD                    SDRTPR
721300931104     C*
721400990617     C* CHIAVE LETTURA SISDF01L - COMPLETA
721500931104     C     KEYSDF        KLIST
721600931104     C                   KFLD                    SDFANN
721700931104     C                   KFLD                    SDFMES
721800931104     C                   KFLD                    SDFFIL
721900931110     C*
722000990617     C* CHIAVE LETTURA SISDT01L - COMPLETA
722100931110     C     KEYSDT        KLIST
722200931110     C                   KFLD                    SDTANN
722300931110     C                   KFLD                    SDTTFP
722400931110     C                   KFLD                    SDTTFA
722500931110     C                   KFLD                    SDTFPE
722600931110     C                   KFLD                    SDTMES
722700940304     C                   KFLD                    SDTDEC
722800940304     C                   KFLD                    SDTTT1
722900940304     C                   KFLD                    SDTTT2
723000040519     C                   KFLD                    SDTTT3
723100040519     C                   KFLD                    SDTTT4
723200040519     C                   KFLD                    SDTTT5
723300931111     C*
723400990617     C* CHIAVE LETTURA SISDC01L - COMPLETA
723500931111     C     KEYSDC        KLIST
723600940209     C                   KFLD                    SDCKSC
723700931111     C                   KFLD                    SDCANN
723800940209     C                   KFLD                    SDCMES
723900931111     C                   KFLD                    SDCREP
724000931111     C                   KFLD                    SDCREA
724100931111     C                   KFLD                    SDCFPE
724200931111     C                   KFLD                    SDCTSP
724300931111     C                   KFLD                    SDCCTR
724400931111     C                   KFLD                    SDCTPO
724500931111     C                   KFLD                    SDCRBL
724600931111     C                   KFLD                    SDCTPR
724700931111     C*
724800990617     C* CHIAVE LETTURA SISDO01L - COMPLETA
724900931111     C     KEYSDO        KLIST
725000940209     C                   KFLD                    SDOKSC
725100931111     C                   KFLD                    SDOANN
725200931111     C                   KFLD                    SDOMES
725300931111     C*
725400990617     C* CHIAVE LETTURA SISDA01L - COMPLETA
725500931111     C     KEYSDA        KLIST
725600940209     C                   KFLD                    SDAKSC
725700931111     C                   KFLD                    SDAANN
725800940209     C                   KFLD                    SDAMES
725900940125     C                   KFLD                    SDADLV
726000931111     C                   KFLD                    SDATSP
726100931111     C*
726200990617     C* CHIAVE LETTURA SISDP01L - COMPLETA
726300940103     C     KEYSDP        KLIST
726400940317     C                   KFLD                    SDPKSC
726500940103     C                   KFLD                    SDPANN
726600940103     C                   KFLD                    SDPMES
726700940316     C                   KFLD                    SDPCTR
726800940316     C                   KFLD                    SDPSVR
726900940103     C                   KFLD                    SDPFTC
727000040316     C* CHIAVE LETTURA SISDC01L - COMPLETA
727100040316     C     KEYSDE        KLIST
727200040316     C                   KFLD                    SDEFILE
727300040316     C                   KFLD                    SDETRD
727400040316     C                   KFLD                    SDEKSC
727500040316     C                   KFLD                    SDEANN
727600040316     C                   KFLD                    SDEMES
727700040316     C                   KFLD                    SDEREP
727800040316     C                   KFLD                    SDEREA
727900040316     C                   KFLD                    SDEFPE
728000040316     C                   KFLD                    SDETSP
728100040316     C                   KFLD                    SDECTR
728200040316     C                   KFLD                    SDETPO
728300040316     C                   KFLD                    SDERBL
728400040316     C                   KFLD                    SDETPR
728500030123     C*
728600030123     C* CHIAVE LETTURA SICOP01L - COMPLETA
728700030123     C     KEYCOP        KLIST
728800030123     C                   KFLD                    COPFIL
728900030123     C                   KFLD                    COPDAT
729000030123     C                   KFLD                    COPVOC
729100030212     C                   KFLD                    COPAAS
729200030212     C                   KFLD                    COPMES
729300941026     C*
729400941026     C* CHIAVE LETTURA ECERR01L - COMPLETA
729500941026     C     KEYERR        KLIST
729600941026     C                   KFLD                    ERRDAT
729700941026     C                   KFLD                    ERRTIM
729800941026     C                   KFLD                    ERRPRG
729900011210     C*
730000011210     C* CHIAVE LETTURA ECMOV01L - COMPLETA
730100011210     C     KEYMOV        KLIST
730200011210     C                   KFLD                    MOVSOC
730300011210     C                   KFLD                    MOVCDP
730400011210     C                   KFLD                    MOVSVO
730500011210     C                   KFLD                    MOVCDC
730600011210     C                   KFLD                    MOVDA
730700020304     C                   KFLD                    MOVDRG
730800941215     C*
730900950316     C* CHIAVE LETTURA ECCET30C - COMPLETA
731000941215     C     KEYCET        KLIST
731100941215     C                   KFLD                    CETAAS
731200941215     C                   KFLD                    CETLNP
731300941215     C                   KFLD                    CETNRS
731400941215     C                   KFLD                    CETNSP
731500941215     C                   KFLD                    CETTBL
731600941215     C*
731700950316     C* CHIAVE LETTURA ECCED30C - PARZIALE
731800941215     C     KEYCEP        KLIST
731900941215     C                   KFLD                    CEDAAS
732000941215     C                   KFLD                    CEDLNP
732100941215     C                   KFLD                    CEDNRS
732200941215     C                   KFLD                    CEDNSP
732300941215     C                   KFLD                    CEDTBL
732400941215     C*
732500950316     C* CHIAVE LETTURA ECCED30C - COMPLETA
732600941215     C     KEYCED        KLIST
732700941215     C                   KFLD                    CEDAAS
732800941215     C                   KFLD                    CEDLNP
732900941215     C                   KFLD                    CEDNRS
733000941215     C                   KFLD                    CEDNSP
733100941215     C                   KFLD                    CEDTBL
733200941215     C                   KFLD                    CEDCMP
733300030114     C*
733400030114     C* CHIAVE LETTURA ECCEV30C - COMPLETA
733500030114     C     KEYCEV        KLIST
733600030114     C                   KFLD                    CEVAAS
733700030114     C                   KFLD                    CEVLNP
733800030114     C                   KFLD                    CEVNRS
733900030114     C                   KFLD                    CEVNSP
734000030114     C                   KFLD                    CEVTBL
734100030512     C*
734200030512     C* CHIAVE LETTURA ECC2V30C - COMPLETA
734300030512     C     KEYC2V        KLIST
734400030512     C                   KFLD                    C2VAAS
734500030512     C                   KFLD                    C2VLNP
734600030512     C                   KFLD                    C2VNRS
734700030512     C                   KFLD                    C2VNSP
734800030512     C                   KFLD                    C2VTBL
734900980313     C*
735000980325     C* CHIAVE LETTURA ECFIM30C - PARZIALE
735100980313     C     KEYFM1        KLIST
735200980313     C                   KFLD                    FIMAAS
735300980313     C                   KFLD                    FIMLNP
735400980313     C                   KFLD                    FIMNRS
735500980313     C                   KFLD                    FIMNSP
735600980313     C                   KFLD                    FIMTBL
735700980313     C*
735800980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
735900980313     C     KEYFIM        KLIST
736000980313     C                   KFLD                    FIMAAS
736100980313     C                   KFLD                    FIMLNP
736200980313     C                   KFLD                    FIMNRS
736300980313     C                   KFLD                    FIMNSP
736400980313     C                   KFLD                    FIMTBL
736500980313     C                   KFLD                    FIMDDO
736600941215     C*
736700941215     C* CHIAVE LETTURA ECECT01L - PARZIALE
736800941215     C     KEYECT        KLIST
736900941215     C                   KFLD                    ECTNBC
737000941215     C*
737100941215     C* CHIAVE LETTURA ECECD01L - PARZIALE
737200941215     C     KEYECD        KLIST
737300941215     C                   KFLD                    ECDNBC
737400941215     C                   KFLD                    ECDCMP
737500020531     C*
737600020531     C* CHIAVE LETTURA FNAPD01L - COMPLETA
737700020531     C     KEYAPD        KLIST
737800021205     C                   KFLD                    APDTIP
737900021205     C                   KFLD                    APDPDR
738000020531     C*
738100020531     C* CHIAVE LETTURA FIFCE01L - COMPLETA
738200020531     C     KEYFCE        KLIST
738300020531     C                   KFLD                    FCEFGS
738400020531     C                   KFLD                    FCEPDR
738500020531     C                   KFLD                    FCETSR
738600020531     C                   KFLD                    FCENDC
738700020531     C                   KFLD                    FCEDDC
738800020531     C                   KFLD                    FCEAAS
738900020531     C                   KFLD                    FCELNP
739000020531     C                   KFLD                    FCENRS
739100020531     C                   KFLD                    FCENSP
739200020531     C                   KFLD                    FCECCE
739300030217     C*
739400030217     C* CHIAVE LETTURA ECEVT01L - COMPLETA
739500030217     C     KEYEVT        KLIST
739600030217     C                   KFLD                    EVTVOC
739700030217     C                   KFLD                    EVTPRG
739800930414     C*
739900931111     C* DEFINISCO CAMPI
740000160404     C     *LIKE         DEFINE    orgfil        wfil
740001160404     C     *LIKE         DEFINE    TBLKUT        KTBUTE
740100930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
740200930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
740300931111     C     *LIKE         DEFINE    ORGFIL        KORFIL
740400011218     C     *LIKE         DEFINE    TBECOD        KTECOD
740500011218     C     *LIKE         DEFINE    TBEKE1        KTEKE1
740600931111     C* -PRIMA- E -DOPO-
740700931111     C     *LIKE         DEFINE    SDRMES        WPMES
740800931111     C     *LIKE         DEFINE    SDRMES        WDMES
740900940103     C     *LIKE         DEFINE    SDRFPE        WPFPR
741000940103     C     *LIKE         DEFINE    SDRFPE        WDFPR
741100940103     C     *LIKE         DEFINE    SDCFPE        WPFPC
741200940103     C     *LIKE         DEFINE    SDCFPE        WDFPC
741300940103     C     *LIKE         DEFINE    SDTFPE        WPFPT
741400940103     C     *LIKE         DEFINE    SDTFPE        WDFPT
741500940304     C     *LIKE         DEFINE    SDTDEC        WPDEC
741600940304     C     *LIKE         DEFINE    SDTDEC        WDDEC
741700931111     C     *LIKE         DEFINE    SDRTPR        WPTPR
741800931111     C     *LIKE         DEFINE    SDRTPR        WDTPR
741900031029     C     *LIKE         DEFINE    SDRVOL        WPVOL
742000931111     C     *LIKE         DEFINE    SDRVOL        WDVOL
742100931111     C     *LIKE         DEFINE    SDCVCC        WPVCC
742200931111     C     *LIKE         DEFINE    SDCVCC        WDVCC
742300931111     C     *LIKE         DEFINE    SDCVLS        WPVLS
742400931111     C     *LIKE         DEFINE    SDCVLS        WDVLS
742500020829     C     *LIKE         DEFINE    SDRPKG        WPPES
742600020829     C     *LIKE         DEFINE    SDRPKG        WDPES
742700020829     C     *LIKE         DEFINE    SDCPCC        WPPCC
742800020829     C     *LIKE         DEFINE    SDCPCC        WDPCC
742900020829     C     *LIKE         DEFINE    SDCPLS        WPPLS
743000020829     C     *LIKE         DEFINE    SDCPLS        WDPLS
743100931111     C     *LIKE         DEFINE    SDRPTS        WPPTS
743200931111     C     *LIKE         DEFINE    SDRPTS        WDPTS
743300941201     C     *LIKE         DEFINE    SDRIRB        WIMR
743400941201     C     *LIKE         DEFINE    SDRIRB        WIRBE
743500941201     C     *LIKE         DEFINE    SDCIRB        WIRDN
743600941201     C     *LIKE         DEFINE    SDRIRB        WIRB
743700931111     C     *LIKE         DEFINE    SDRIRB        WPIRB
743800931111     C     *LIKE         DEFINE    SDRIRB        WDIRB
743900941201     C     *LIKE         DEFINE    SDCIRD        WIRD
744000941201     C     *LIKE         DEFINE    SDCIRD        WPIRD
744100931111     C     *LIKE         DEFINE    SDCIRD        WDIRD
744200941201     C     *LIKE         DEFINE    SDCIRB        WIRBC
744300931112     C     *LIKE         DEFINE    SDCIRB        WPIRBC
744400931112     C     *LIKE         DEFINE    SDCIRB        WDIRBC
744500090914     C*    *LIKE         DEFINE    SDCIRP        WPIRP
744600090914     C*    *LIKE         DEFINE    SDCIRP        WDIRP
744700090914     C     *LIKE         DEFINE    SDCIRP        WPIR
744800090914     C     *LIKE         DEFINE    SDCIRP        WDIR
744900931111     C     *LIKE         DEFINE    ORGCRE        WPREP
745000931111     C     *LIKE         DEFINE    ORGCRE        WDREP
745100931111     C     *LIKE         DEFINE    ORGCRE        WPREA
745200931111     C     *LIKE         DEFINE    ORGCRE        WDREA
745300090914     C*    *LIKE         DEFINE    SDTCTL        WPCTL
745400090914     C*    *LIKE         DEFINE    SDTCTL        WDCTL
745500090914     C*    *LIKE         DEFINE    SDTCT1        WPCT1
745600090914     C*    *LIKE         DEFINE    SDTCT1        WDCT1
745700090914     C*    *LIKE         DEFINE    SDTCT2        WPCT2
745800090914     C*    *LIKE         DEFINE    SDTCT2        WDCT2
745900090914     C*    *LIKE         DEFINE    SDTCT3        WPCT3
746000090914     C*    *LIKE         DEFINE    SDTCT3        WDCT3
746100090914     C*    *LIKE         DEFINE    SDTCT4        WPCT4
746200090914     C*    *LIKE         DEFINE    SDTCT4        WDCT4
746300090914     C*    *LIKE         DEFINE    SDTCT5        WPCT5
746400090914     C*    *LIKE         DEFINE    SDTCT5        WDCT5
746500990617     C* FILE SISDR
746600931111     C     *LIKE         DEFINE    SDRNSP        WRNSP
746700020829     C*--- *LIKE         DEFINE    SDRPKG        WRPKG
746800020829     C     *LIKE         DEFINE    SDRPKG        WRPES
746900931111     C     *LIKE         DEFINE    SDRNCO        WRNCO
747000931111     C     *LIKE         DEFINE    SDRVOL        WRVOL
747100931111     C     *LIKE         DEFINE    SDRPTS        WRPTS
747200931111     C     *LIKE         DEFINE    SDRIRB        WRIRB
747300990617     C* FILE SISDF
747400931111     C     *LIKE         DEFINE    SDFNSP        WFNSP
747500020829     C*--- *LIKE         DEFINE    SDFPKG        WFPKG
747600020829     C     *LIKE         DEFINE    SDFPKG        WFPES
747700931111     C     *LIKE         DEFINE    SDFNCO        WFNCO
747800931111     C     *LIKE         DEFINE    SDFVOL        WFVOL
747900931111     C     *LIKE         DEFINE    SDFPTS        WFPTS
748000931111     C     *LIKE         DEFINE    SDFIRB        WFIRB
748100990617     C* FILE SISDT
748200931111     C     *LIKE         DEFINE    SDTNSP        WTNSP
748300020829     C*--- *LIKE         DEFINE    SDTPKG        WTPKG
748400020829     C     *LIKE         DEFINE    SDTPKG        WTPES
748500931111     C     *LIKE         DEFINE    SDTNCO        WTNCO
748600931111     C     *LIKE         DEFINE    SDTVOL        WTVOL
748700090914     C*    *LIKE         DEFINE    SDTCTL        WTCTL
748800090914     C*    *LIKE         DEFINE    SDTCT1        WTCT1
748900090914     C*    *LIKE         DEFINE    SDTCT2        WTCT2
749000090914     C*    *LIKE         DEFINE    SDTCT3        WTCT3
749100090914     C*    *LIKE         DEFINE    SDTCT4        WTCT4
749200090914     C*    *LIKE         DEFINE    SDTCT5        WTCT5
749300990617     C* FILE SISDC
749400931112     C     *LIKE         DEFINE    SDCNSP        WCNSP
749500020829     C*--- *LIKE         DEFINE    SDCPKG        WCPKG
749600020829     C     *LIKE         DEFINE    SDCPKG        WCPES
749700020829     C     *LIKE         DEFINE    SDCPCC        WCPCC
749800020829     C     *LIKE         DEFINE    SDCPLS        WCPLS
749900931112     C     *LIKE         DEFINE    SDCNCO        WCNCO
750000931112     C     *LIKE         DEFINE    SDCVOL        WCVOL
750100931112     C     *LIKE         DEFINE    SDCVCC        WCVCC
750200931112     C     *LIKE         DEFINE    SDCVLS        WCVLS
750300931112     C     *LIKE         DEFINE    SDCIRB        WCIRB
750400090914     C*    *LIKE         DEFINE    SDCIRP        WCIRP
750500931112     C     *LIKE         DEFINE    SDCIRD        WCIRD
750600941117     C     *LIKE         DEFINE    SDCKSC        WCPKSC
750700941117     C     *LIKE         DEFINE    SDCKSC        WCDKSC
750800990617     C* FILE SISDO
750900931112     C     *LIKE         DEFINE    SDONSP        WONSP
751000020829     C*--- *LIKE         DEFINE    SDOPKG        WOPKG
751100020829     C     *LIKE         DEFINE    SDOPKG        WOPES
751200931112     C     *LIKE         DEFINE    SDONCO        WONCO
751300931112     C     *LIKE         DEFINE    SDOVOL        WOVOL
751400931112     C     *LIKE         DEFINE    SDOIRB        WOIRB
751500090914     C*    *LIKE         DEFINE    SDOIRP        WOIRP
751600931112     C     *LIKE         DEFINE    SDOIRD        WOIRD
751700990617     C* FILE SISDP
751800940103     C     *LIKE         DEFINE    SDPNSP        WPNSP
751900940103     C     *LIKE         DEFINE    SDPRCP        WPRCP
752000940317     C     *LIKE         DEFINE    SDPSVR        WPSV1
752100940317     C     *LIKE         DEFINE    SDPSVR        WPSV2
752200940317     C     *LIKE         DEFINE    SDPSVR        WPSV3
752300940317     C     *LIKE         DEFINE    SDPRCP        WPVA1
752400940317     C     *LIKE         DEFINE    SDPRCP        WPVA2
752500940317     C     *LIKE         DEFINE    SDPRCP        WPVA3
752600970128     C     *LIKE         DEFINE    SDPKSC        WPPKSC
752700970128     C     *LIKE         DEFINE    SDPKSC        WPDKSC
752800990617     C* FILE SISDA
752900931112     C     *LIKE         DEFINE    SDANSP        WANSP
753000040317     C* FILE SISDE
753100040318     C     *LIKE         DEFINE    §bnbbcor      Wecor
753200040318     C     *LIKE         DEFINE    §bnbbcsf      Wecsf
753300040318     C     *LIKE         DEFINE    §bnbbNBA      WeNBA
753400040318     C     *LIKE         DEFINE    §bnbbNB2      WeNB2
753401160404     C* FILE SISDE record k30
753403160404     C     *LIKE         DEFINE    §K30NSP       wK30NSP
753404160404     C     *LIKE         DEFINE    §K30PKG       wK30PKG
753405160404     C     *LIKE         DEFINE    §K30PCC       wK30PCC
753406160404     C     *LIKE         DEFINE    §K30PLS       wK30PLS
753407160404     C     *LIKE         DEFINE    §K30NCO       wK30NCO
753408160404     C     *LIKE         DEFINE    §K30VOL       wK30VOL
753409160404     C     *LIKE         DEFINE    §K30VCC       wK30VCC
753410160404     C     *LIKE         DEFINE    §K30VLS       wK30VLS
753411160414     C     *LIKE         DEFINE    §K30irb       wK30irb
753412160414     C     *LIKE         DEFINE    §K30irp       wK30irp
753413161107     C* FILE SISDE record k31
753414161107     C     *LIKE         DEFINE    §K31NSP       wK31NSP
753415161107     C     *LIKE         DEFINE    §K31PKG       wK31PKG
753416161107     C     *LIKE         DEFINE    §K31PCC       wK31PCC
753417161107     C     *LIKE         DEFINE    §K31PLS       wK31PLS
753418161107     C     *LIKE         DEFINE    §K31NCO       wK31NCO
753419161107     C     *LIKE         DEFINE    §K31VOL       wK31VOL
753420161107     C     *LIKE         DEFINE    §K31VCC       wK31VCC
753421161107     C     *LIKE         DEFINE    §K31VLS       wK31VLS
753422161107     C     *LIKE         DEFINE    §K31irb       wK31irb
753423161107     C     *LIKE         DEFINE    §K31irp       wK31irp
753424160404     C* FILE SISDE record log
753425160404     C     *LIKE         DEFINE    §logicmp      wlogicmp
753500980313     C* FILE ECFIM
753600980313     C     *LIKE         DEFINE    FIMSMC        WPSMC
753700980313     C     *LIKE         DEFINE    FIMSMC        WPSMCT
753800980317     C     *LIKE         DEFINE    FIMSMC        WSMC
753900980313     C     *LIKE         DEFINE    FIMSMC        WDSMC
754000980313     C* FILE ECFIC
754100011127     C*---       *LIKE     DEFN FICSCO    WPSCO
754200011127     C*---       *LIKE     DEFN FICSCO    WPSCOT
754300011127     C*---       *LIKE     DEFN FICSCO    WDSCO
754400011127     C*---       *LIKE     DEFN FICSCO    WSCO
754500030121     C* FILE SICOP
754600090914     C*    *LIKE         DEFINE    COPIMV        WCOIMV
754700030121     C     *LIKE         DEFINE    COPNSP        WCONSP
754800030121     C     *LIKE         DEFINE    COPNCL        WCONCL
754900030121     C     *LIKE         DEFINE    COPPKF        WCOPKF
755000030121     C     *LIKE         DEFINE    COPVLF        WCOVLF
755100030121     C     *LIKE         DEFINE    COPBAN        WCOBAN
755200031029     C     *LIKE         DEFINE    COPBAN        WDBAN
755300031029     C     *LIKE         DEFINE    COPBAN        WPBAN
755400940505     C* CAMPI VARI
755500030131     C     *LIKE         DEFINE    COPDAT        WDDATCOP
755600030131     C     *LIKE         DEFINE    COPDAT        WPDATCOP
755700090304     C     *LIKE         DEFINE    D$Pncl        WPncl
755800090304     C     *LIKE         DEFINE    D$PFTC        WPFTC
755900941123     C     *LIKE         DEFINE    D$PTC2        WPTC2
756000040322     C     *LIKE         DEFINE    D$Pflo        WPflo
756100040322     C     *LIKE         DEFINE    D$PFGC        WPFGC
756200941123     C     *LIKE         DEFINE    D$PFCA        WPFCA
756300040524     C     *LIKE         DEFINE    D$Psta        WPsta
756400990823     C     *LIKE         DEFINE    D$PIAS        WPIAS
756500970901     C     *LIKE         DEFINE    D$PPOR        WPPOR
756600970901     C     *LIKE         DEFINE    D$PLL1        WPLL1
756700970901     C     *LIKE         DEFINE    D$PTPO        WPTPO
756800970901     C     *LIKE         DEFINE    D$PFAP        WPFAP
756900970901     C     *LIKE         DEFINE    D$PFIN        WPFIN
757000970901     C     *LIKE         DEFINE    D$PDFT        WPDFT
757100981105     C     *LIKE         DEFINE    D$PKSC        WPKSC
757200000201     C     *LIKE         DEFINE    D$PDIV        WPDIV
757300020620     C     *LIKE         DEFINE    D$PTBL        WPTBL
757400020620     C     *LIKE         DEFINE    D$PCBO        WPCBO
757500941201     C     *LIKE         DEFINE    D$PFIN        WFIN
757600941201     C     *LIKE         DEFINE    D$PCTS        WCTS
757700941201     C     *LIKE         DEFINE    D$PKSC        WKSC
757800020709     C*--- *LIKE         DEFINE    ORGDIT        WDSCL
757900990824     C     *LIKE         DEFINE    D$PPKF        WPKF
758000990824     C     *LIKE         DEFINE    D$PPKF        WKGLIM
758100941201     C     *LIKE         DEFINE    D$PPOR        WPOR
758200000201     C     *LIKE         DEFINE    D$PDIV        WDIV
758300970729     C     *LIKE         DEFINE    D$PDFT        WDFT
758400941215     C     *LIKE         DEFINE    ECDRIC        $RIC
758500020205     C     *LIKE         DEFINE    MOVSVO        $CDC
758600941215     C     *LIKE         DEFINE    D$DLNP        $FIL
758700941215     C     *LIKE         DEFINE    ECTCMP        DEWCMP
758800941215     C     *LIKE         DEFINE    ECTDDC        WDSP
758900941215     C     *LIKE         DEFINE    ECTNBC        WNBC
759000030117     C     *LIKE         DEFINE    COPFIL        WPFIL
759100030117     C     *LIKE         DEFINE    COPFIL        WDFIL
759200941215     C     *LIKE         DEFINE    D$DLNP        W$DLNP
759300941215     C     *LIKE         DEFINE    D$DTFP        W$DTFP
759400941215     C     *LIKE         DEFINE    D$DKSC        W$DKSC
759500941215     C     *LIKE         DEFINE    D$DCCM        W$DCCM
759600941215     C     *LIKE         DEFINE    D$DMCT        W$DMCT
759700941215     C     *LIKE         DEFINE    D$PLNP        W$PLNP
759800941215     C     *LIKE         DEFINE    D$PTFP        W$PTFP
759900941215     C     *LIKE         DEFINE    D$PKSC        W$PKSC
760000941215     C     *LIKE         DEFINE    D$PCCM        W$PCCM
760100941215     C     *LIKE         DEFINE    D$PMCT        W$PMCT
760200000622     C     *LIKE         DEFINE    D$PTFP        LPTFP
760300000622     C     *LIKE         DEFINE    D$PTFA        LPTFA
760400000622     C     *LIKE         DEFINE    D$DTFP        LDTFP
760500000622     C     *LIKE         DEFINE    D$DTFA        LDTFA
760600000201     C     *LIKE         DEFINE    YECIMI        §IMI
760700000203     C     *LIKE         DEFINE    YECDVI        §DVI
760800000201     C     *LIKE         DEFINE    YECIMO        §IMO
760900011220     c     *LIKE         DEFINE    MOVVOC        LVOC
761000011220     c     *LIKE         DEFINE    MOVCDC        LCDC
761100020304     c     *LIKE         DEFINE    MOVDRG        LDRG
761200030120     C     *LIKE         DEFINE    COPDAT        $DAT
761300030120     C     *LIKE         DEFINE    COPDAT        WPDAT
761400030120     C     *LIKE         DEFINE    COPDAT        WDDAT
761500950123     C*
761600950123     C* FACCIO LETTURA DI ECERR FASULLA PER NON FARE VENIRE MESSAGGIO
761700950123     C* AD OGNI APERTURA DI DEL FILE
761800950123     C   99
761900950123     CANN99KEYERR        CHAIN     ECERR01L                           99
762000931105     C*
762100931105     C* IMPOSTO CAMPI DI GESTIONE PGM
762200941021     C                   MOVEL     'N'           $ERR              1
762300950117     C                   MOVEL     *BLANKS       $MSG             78
762400940411     C                   MOVEL     'N'           FLAGGS            1
762500941215     C                   MOVEL     'N'           WCETPR            1
762600030114     C                   MOVEL     'N'           WCEVPR            1
762700030123     C                   MOVEL     *BLANKS       $TDAT             1
762800950125     C*********************ADD  1         WBOLLE  90
762900991116     C*
763000991116     C* RECUPERA I CAMPI PER LA SIMULAZIONE DEL BUDGET
763100140516     C*                  MOVEL     D$GXXX        DSX
763200140516     C*                  MOVEL     DSXFSI        D$GFSI            1
763300140516     C*                  MOVE      DSXASI        D$GASI            4 0
763400140516     C                   MOVEL     D$GXXX        dsd$g
763500941215     C*
763600941215     C* IMPOSTO CAMPI DI GESTIONE PGM
763700941215     C                   Z-ADD     *ZEROS        ID                4 0
763800941215     C                   Z-ADD     *ZEROS        IP                4 0
763900941215     C                   Z-ADD     *ZEROS        CD                4 0
764000941215     C                   Z-ADD     *ZEROS        CP                4 0
764100940405     C*
764200940405     C* PORTA LE IMMAGINI PRIMA E DOPO DALLA DS INPUT ALLA DS PGM
764300940405     C                   MOVEL     D$PRE1        D$PDS1
764400940405     C                   MOVEL     D$PRE2        D$PDS2
764500940405     C                   MOVEL     D$DRE1        D$DDS1
764600940405     C                   MOVEL     D$DRE2        D$DDS2
764700941201     C                   MOVEL     D$ORE1        D$ODS1
764800941201     C                   MOVEL     D$ORE2        D$ODS2
764900941123     C*
765000941130     C* INSCHIERA SIGLE E IMPORTI VARIE -PRIMA- E -DOPO- E -ORIG.-
765100941130     C                   MOVEA     D$PSVN        PSVN
765200941201     C                   MOVEA     D$PVAN        PVAA
765300941130     C     1             DO        20            I
765400941201     C                   MOVE      PVAA(I)       PVAN(I)
765500941130     C                   ENDDO
765600941130     C                   MOVEA     D$DSVN        DSVN
765700941201     C                   MOVEA     D$DVAN        DVAA
765800941130     C     1             DO        20            I
765900941201     C                   MOVE      DVAA(I)       DVAN(I)
766000941130     C                   ENDDO
766100941130     C                   MOVEA     D$OSVN        OSVN
766200941201     C                   MOVEA     D$OVAN        OVAA
766300941130     C     1             DO        20            I
766400941201     C                   MOVE      OVAA(I)       OVAN(I)
766500941130     C                   ENDDO
766600930414     C*
766700930414     C* RECUPERO IL CODICE DELL'AZIENDA
766800930414     C                   Z-ADD     1             CODUT             1 0
766900931104     C*
767000931111     C* CARICA LE TABELLE NECESSARIE AL PGM
767100931104     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
767200931104     C     CTABSN        IFNE      'S'
767300931104     C                   MOVEL     'S'           CTABSN            1
767400931111     C                   EXSR      CARTAB
767500941215     C                   EXSR      SUBDAT
767600941223     C                   Z-ADD     300           MAXBOL            6 0
767700941223     C                   Z-ADD     150           MAXIND            6 0
767800941215     C     DSDAT8        IFNE      WDATA8
767900941215     C                   Z-ADD     DSDAT8        WDATA8            8 0
768000941215     C                   MOVE      DSDAT6        DATGRE            6
768100941215     C                   MOVE      *BLANK        DATGIU            5
768200941215     C                   MOVE      '2'           FMTDAT            1
768300941215     C                   CALL      'XCVTDAT'
768400941215     C                   PARM                    DATGRE
768500941215     C                   PARM                    DATGIU
768600941215     C                   PARM                    FMTDAT
768700941215     C                   ENDIF
768800040406     c                   seton                                        of
768900931104     C                   END
769000930414     C*
769100930414     C                   ENDSR
769200930414     C********************************************************************
769300931111     C*   CARTAB - CARICA LE TABELLE
769400930414     C********************************************************************
769500930414     C     CARTAB        BEGSR
769600020704     C*
769700020704     C* APRE I FILES OCCORRENTI
769800020704     C                   OPEN      TABEL00F
769900020704     C                   OPEN      TNTBE01L
770000020704     C                   OPEN      AZORG01L
770100011218     C*
770200011218     C* REPERISCE LA DATA CORRENTE
770300011218     C                   TIME                    N14              14 0
770400011218     C                   MOVE      N14           N8                8 0
770500011218     C                   Z-ADD     N8            G08DAT
770600011218     C                   Z-ADD     *ZEROS        G08INV
770700011218     C                   MOVEL     '0'           G08ERR
770800011218     C                   CALL      'XSRDA8'
770900011218     C                   PARM                    WLBDA8
771000011218     C                   Z-ADD     G08INV        DATCOR            8 0
771100941025     C*
771200020704     C* AZZERA LE VARIABILI DI LAVORO
771300941223     C                   Z-ADD     *ZEROS        WNBAP             5 0
771400941223     C                   Z-ADD     *ZEROS        WNBAD             5 0
771500990218     C                   Z-ADD     *ZEROS        DEPA2L            4 0
771600020801     C*
771700020801     C* CARICA TABELLA -14- REGIONI E RAGGRUPPAMENTI REGIONI
771800020801     C     $ERR          IFEQ      'N'
771900020801     C                   Z-ADD     0             I
772000020801     C                   Z-ADD     CODUT         KTBUTE
772100020801     C                   MOVEL     '14'          KTBCOD
772200020801     C                   MOVEL     *BLANKS       KTBKEY
772300020801     C     KEYTAB        SETLL     TABEL00F                           99
772400020801     C  N99              READ(N)   TABEL00F                               99
772500020801     C     *IN99         IFEQ      '1'
772600020801     C                   MOVEL     MSG(17)       $MSG
772700020801     C                   EXSR      SCRERR
772800020801     C                   END
772900020801     C     *IN99         DOWEQ     '0'
773000020801     C     TBLCOD        ANDEQ     '14'
773100020801     C     TBLFLG        IFEQ      *BLANKS
773200020801     C                   ADD       1             I
773300020801     C                   MOVEL     TBLKEY        REG(I)
773400020801     C                   MOVEL     TBLUNI        DS14
773500020801     C                   MOVE      §14CRR        RAR(I)
773600020801     C                   MOVE      §14CRN        RAN(I)
773700020801     C                   END
773800020801     C                   READ(N)   TABEL00F                               99
773900020801     C                   END
774000020801     C                   END
774100020704     C*
774200020704     C* CARICA TABELLA -NTW- NETWORK
774300020705IF  1C     $ERR          IFEQ      'N'
774400020705     C                   Z-ADD     *ZEROS        I
774500020705     C                   MOVEL(P)  'NTW'         KTECOD
774600020705     C     KEYTB2        SETLL     TNTBE01L                           99
774700020705     C     KEYTB2        READE     TNTBE01L                               99
774800020705IF  2C     *IN99         IFEQ      *ON
774900020705     C                   MOVEL     MSG(16)       $MSG
775000020705     C                   EXSR      SCRERR
775100020705E   2C                   ENDIF
775200020705DO  2C     *IN99         DOWEQ     *OFF
775300020705     C                   MOVEL     TBEUNI        DNTW
775400020705     C                   ADD       1             I
775500020705     C                   MOVEL     TBEKE1        SNTW(I)
775600020705     C                   MOVEL     §NTWFIE       SNFIE(I)
775700020705     C     KEYTB2        READE     TNTBE01L                               99
775800020705E   2C                   ENDDO
775900020705E   1C                   ENDIF
776000020704     C*
776100020704     C* CARICA ORGANIGRAMMA
776200020705IF  1C     $ERR          IFEQ      'N'
776300020704     C                   Z-ADD     *ZEROS        KORFIL
776400020704     C     KEYORG        SETLL     AZORG01L                           99
776500020704     C  N99              READ      AZORG01L                               99
776600020704     C                   Z-ADD     *ZEROS        I
776601160405     C                   Z-ADD     *ZEROS        l
776700020705DO  2C     *IN99         DOWEQ     *OFF
776800020705IF  3C     ORGFVA        IFEQ      *BLANKS
776801160405     C                   MOVEL     ORGDE3        OG143
776900020705IF  4C     ORGFAG        IFNE      'V'
777000020704     C                   ADD       1             I
777100020704     C                   Z-ADD     ORGFIL        FIL(I)
777200020704     C                   Z-ADD     ORGFIL        F                 4 0
777400020705     C* DATI FILIALE
777500020705     C                   Z-ADD     1             II                6 0
777600020705     C     §OGNTW        LOOKUP    SNTW(II)                               98
777700020705IF  5C     *IN98         IFEQ      *OFF
777800020705     C                   MOVEL     MSG(23)       $MSG
777900020705     C                   EXSR      SCRERR
778000020705X   5C                   ELSE
778100020705     C                   MOVEL     SNTW(II)      NTW(F)
778200020705     C                   MOVEL     SNFIE(II)     FIE(F)
778300020805IF  6C     NTW(F)        IFEQ      'DPD'
778400020805     C                   MOVEL     'E'           FIE(F)
778500020805E   6C                   ENDIF
778600020705E   5C                   ENDIF
778700020805     C
778800020705     C*---               MOVEL     ORGFL1        FL1(F)
778900020705     C*---               MOVEL     ORGDIT        DIT(F)
779000020705     C*---               MOVEL     §OGDPD        DPD(F)
779100020705     C*---               MOVEL     §OGPT         POS(F)
779200020705     C* RAGGRUPPAMENTI
779300020704     C                   Z-ADD     1             II                6 0
779400020704     C     ORGCRE        LOOKUP    REG(II)                                98
779500020705IF  5C     *IN98         IFEQ      *OFF
779600020704     C                   MOVEL     MSG(8)        $MSG
779700020704     C                   EXSR      SCRERR
779800020705X   5C                   ELSE
779900020704     C                   Z-ADD     RAR(II)       RRE(I)
780000020704     C                   Z-ADD     RAN(II)       RRN(I)
780100020705E   5C                   ENDIF
780200020705E   4C                   ENDIF
780201160405     c* filiali logistica
780202160405     C                   if        §OGNTW = 'LOG'
780203160405     c                   add       1             l
780204160405     c                   if        l<= 100
780205160405     C                   Z-ADD     ORGFIL        flog(l)
780206160405     c                   end
780207160405     c                   end
780208160405     c*
780300020705E   3C                   ENDIF
780400020704     C                   READ      AZORG01L                               99
780500020705E   2C                   ENDDO
780600020705E   1C                   ENDIF
780700930414     C*
780800931104     C* CARICA TABELLA -CC- CONTROPARTITE PER FATTURAZIONE
780900941021     C     $ERR          IFEQ      'N'
781000930421     C                   Z-ADD     0             I
781100930414     C                   Z-ADD     CODUT         KTBUTE
781200931104     C                   MOVEL     'CC'          KTBCOD
781300930414     C                   MOVEL     *BLANKS       KTBKEY
781400930414     C     KEYTAB        SETLL     TABEL00F                           99
781500950203     C  N99              READ(N)   TABEL00F                               99
781600930414     C     *IN99         IFEQ      '1'
781700941223     C                   MOVEL     MSG(15)       $MSG
781800941026     C                   EXSR      SCRERR
781900930414     C                   END
782000931104     C     *IN99         DOWEQ     '0'
782100931104     C     TBLCOD        ANDEQ     'CC'
782200931104     C     TBLFLG        IFEQ      *BLANKS
782300930421     C                   ADD       1             I
782400931104     C                   MOVE      TBLKEY        CCC(I)
782500931104     C                   MOVEL     TBLUNI        DSCC
782600931111     C                   MOVEL     §CCFL1        CC1(I)
782700931111     C                   MOVEL     §CCFL2        CC2(I)
782800931111     C                   MOVEL     §CCFL3        CC3(I)
782900941130     C                   MOVEL     §CCFL4        CC4(I)
783000941130     C                   MOVEL     §CCFL5        CC5(I)
783100941130     C                   MOVEL     §CCFL6        CC6(I)
783200941130     C                   MOVEL     §CCCMP        CMP(I)
783300931104     C                   END
783400950203     C                   READ(N)   TABEL00F                               99
783500931104     C                   END
783600930414     C                   END
783700941123     C*
783800941123     C* CARICA TABELLA -1P- CONSEGNE PARTICOLARI
783900941212     C     $ERR          IFEQ      'N'
784000941123     C                   Z-ADD     0             I
784100941123     C                   Z-ADD     CODUT         KTBUTE
784200941123     C                   MOVEL     '1P'          KTBCOD
784300941123     C                   MOVEL     *BLANKS       KTBKEY
784400941123     C     KEYTAB        SETLL     TABEL00F                           99
784500950203     C  N99              READ(N)   TABEL00F                               99
784600941123     C     *IN99         IFEQ      '1'
784700941123     C                   MOVEL     MSG(14)       $MSG
784800941123     C                   EXSR      SCRERR
784900941123     C                   END
785000941123     C     *IN99         DOWEQ     '0'
785100941212     C     TBLCOD        ANDEQ     '1P'
785200941123     C     TBLFLG        IFEQ      *BLANKS
785300941123     C                   ADD       1             I
785400941123     C                   MOVEL     TBLKEY        C1P(I)
785500941123     C                   MOVEL     TBLUNI        DS1P
785600941123     C                   MOVE      §1PVAR        V1P(I)
785700941123     C                   END
785800950203     C                   READ(N)   TABEL00F                               99
785900941123     C                   END
786000941123     C                   END
786100941215     C*
786200020704     C* CARICA TABELLA -2T- RIFERIMENTI CENTRI DI COSTO
786300950404     C     $ERR          IFEQ      'N'
786400950404     C                   Z-ADD     CODUT         KTBUTE
786500941215     C                   MOVEL     '2T'          KTBCOD
786600941215     C     KEYTA2        SETLL     TABEL00F
786700941215     C     KEYTA2        READE(N)  TABEL00F                               99
786800941215IF  1C     *IN99         IFEQ      *ON
786900941215     C                   MOVEL     MSG(2)        $MSG
787000941215     C                   EXSR      SCRERR
787100941215X   1C                   ELSE
787200941215     C                   Z-ADD     *ZEROS        I
787300941215DO  2C     *IN99         DOWEQ     *OFF
787400941215IF  3C     TBLFLG        IFEQ      *BLANKS
787500941215     C                   ADD       1             I
787600941215     C                   MOVEL     TBLUNI        DS2T
787700941215     C                   MOVEL     TBLKEY        RIC(I)
787800941215E   3C                   ENDIF
787900941215     C     KEYTA2        READE(N)  TABEL00F                               99
788000941215E   2C                   ENDDO
788100941215E   1C                   ENDIF
788200950404     C                   ENDIF
788300970901     C*
788400020704     C* CARICA TABELLA -$2- SIGLE VARIE FORZATE A PGM
788500970901     C     $ERR          IFEQ      'N'
788600970901     C                   Z-ADD     CODUT         KTBUTE
788700970901     C                   MOVEL     '$2'          KTBCOD
788800970902     C                   MOVEL     *BLANKS       KTBKEY
788900970902     C                   MOVEL     '1'           KTBKEY
789000970901     C     KEYTAB        CHAIN(N)  TABEL00F                           99
789100970901IF  1C     *IN99         IFEQ      *ON
789200970901     C                   MOVEL     MSG(7)        $MSG
789300970901     C                   EXSR      SCRERR
789400970901X   1C                   ELSE
789500970901     C                   MOVEL     TBLUNI        DS$2
789600970901E   1C                   ENDIF
789700970901     C                   ENDIF
789800011218     c*
789900020704     C* CARICA TABELLA -GED- DIVISE PER CONVERSIONE
790000011218     C                   CLEAR                   DSGEDCE
790100011221     C                   MOVEL     *blanks       WDIVCE            3
790200011218     C                   MOVEL(P)  'GED'         KTECOD
790300011218     C                   MOVEL(P)  'CE'          KTEKE1
790400011218     C     KEYTBE        CHAIN     TNTBE01L
790500011218IF  1C                   IF        %found(TNTBE01L)
790600011218     C                   MOVEL     TBEUNI        DSGEDCE
790700011218E   1C                   ENDIF
790800011218IF  1C                   IF        §GEDCEDAT > DATCOR
790900011218     C                   EVAL      WDIVCE = §GEDCEDIP
791000011218X   1C                   ELSE
791100011218     C                   EVAL      WDIVCE = §GEDCEDIV
791200011221E   1C                   ENDIF
791300011221IF  1C                   IF        WDIVCE = *blanks
791400011221     C                   EVAL      WDIVCE = 'ITL'
791500011221E   1C                   ENDIF
791600020822     c*
791700020822     C* CARICA TABELLA -BLC- DATE CONTABILIZZAZIONE CONTEGGI PADRONCINI
791800020822     C                   CLEAR                   DBLC
791900020822     C                   MOVEL(P)  'BLC'         KTECOD
792000020822     C                   MOVEL(P)  '1'           KTEKE1
792100020822     C     KEYTBE        CHAIN     TNTBE01L
792200020822IF  1C                   IF        %found(TNTBE01L)
792300020822     C                   MOVEL     TBEUNI        DBLC
792400020822E   1C                   ENDIF
792500020704     C*
792600020704     C* CHIUDE I FILES OCCORRENTI
792700020704     C                   CLOSE     TABEL00F
792800020704     C                   CLOSE     TNTBE01L
792900020704     C                   CLOSE     AZORG01L
793000930414     C*
793100930414     C                   ENDSR
793200990218     C********************************************************************
793300990218     C*   CART2L - CARICA LA TABELLA 2L PER L'ANNO DELLA BOLLA
793400990218     C********************************************************************
793500990218     C     CART2L        BEGSR
793600990218     C*
793700990218     C* SE L'ANNO E' CAMBIATO RILEGGE LA TAB. 2L
793800990218IF  1C     WA2L          IFNE      DEPA2L
793900990218     C*
794000990218     C                   OPEN      TABEL00F
794100990218     C*
794200990218     C* CARICA TABELLA -2L- FASCIE DI PESO
794300990218     C     $ERR          IFEQ      'N'
794400990218     C                   Z-ADD     *ZEROS        II                6 0
794500990218     C                   Z-ADD     *ZEROS        IC                6 0
794600990218     C                   Z-ADD     *ZEROS        IT                6 0
794700990218     C                   Z-ADD     CODUT         KTBUTE
794800990218     C                   MOVEL     '2L'          KTBCOD
794900990218     C                   MOVEL     *BLANKS       KTBKEY
795000990218     C     KEYTAB        SETLL     TABEL00F                           99
795100990218     C  N99              READ(N)   TABEL00F                               99
795200990218     C     *IN99         IFEQ      '1'
795300990218     C                   MOVEL     MSG(13)       $MSG
795400990218     C                   EXSR      SCRERR
795500990218     C                   END
795600990218     C     *IN99         DOWEQ     '0'
795700990218     C     TBLCOD        ANDEQ     '2L'
795800990218     C     TBLFLG        IFEQ      *BLANKS
795900990218     C                   MOVEL     TBLKEY        WTM03             3
796000990218     C                   MOVEL     TBLKEY        WTM05             5
796100990218     C                   MOVEL     TBLUNI        DS2L
796200020516     C     WTM03         IFEQ      'S9R'
796300990218     C                   ADD       1             II
796400990218     C                   MOVE      WTM05         FPR(II)
796500990218     C                   Z-ADD     §2LKGI        KIR(II)
796600990218     C                   Z-ADD     §2LKGF        KFR(II)
796700990218     C                   ENDIF
796800020513     C*--- WA2L          IFLE      1998
796900020513     C*--- WTM03         ANDEQ     'SDC'
797000020513     C*--- WA2L          ORGE      1999
797100020513     C*--- WTM03         ANDEQ     'S9C'
797200020513     C     WA2L          IFLE      2001
797300020513     C     WTM03         ANDEQ     'S9C'
797400020513     C     WA2L          ORGE      2002
797500020513     C     WTM03         ANDEQ     'S2C'
797600990218     C                   ADD       1             IC
797700011214     C                   MOVE      WTM05         FPC(IC)
797800990218     C                   Z-ADD     §2LKGI        KIC(IC)
797900990218     C                   Z-ADD     §2LKGF        KFC(IC)
798000990218     C                   ENDIF
798100020513     c*
798200990218     C     WA2L          IFLE      1998
798300990218     C     WTM03         ANDEQ     'SDT'
798400990218     C     WA2L          ORGE      1999
798500990218     C     WTM03         ANDEQ     'S9T'
798600990218     C                   ADD       1             IT
798700990218     C                   MOVE      WTM05         FPT(IT)
798800990218     C                   Z-ADD     §2LKGI        KIT(IT)
798900990218     C                   Z-ADD     §2LKGF        KFT(IT)
799000990218     C                   ENDIF
799100990218     C                   ENDIF
799200990218     C                   READ(N)   TABEL00F                               99
799300990218     C                   ENDDO
799400990218     C                   ENDIF
799500990218     C*
799600990218     C                   CLOSE     TABEL00F
799700990218     C                   Z-ADD     WA2L          DEPA2L
799800990218     C*
799900990218E   1C                   ENDIF
800000990218     C*
800100990218     C                   ENDSR
800200941215     C*------------------------------------------------------------------------*
800300941215     C*? STAMPA - STAMPA RIGA
800400941215     C*------------------------------------------------------------------------*
800500950120     C*****      STAMPA    BEGSR
800600941215     C*
800700950120     C*****                OPEN PRTF198
800800941215     C*
800900941215     C* RECUPERO LA DATA E L'ORA
801000950120     C*****                Z-ADDUDATE     DATA    60
801100950120     C*****                TIME           ORA     60
801200941215     C*
801300941215     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
801400950120     C*****                EXCPTTESTA
801500941215     C*
801600941215     C* STAMPA RIGA DI DETTAGLIO TEMPI
801700950120     C*****                EXCPTRIGA
801800941215     C*
801900941215     C* STAMPA RIGA FINE
802000950120     C*****                EXCPTSTAFIN
802100941215     C*
802200950120     C*****                CLOSEPRTF198
802300941215     C*
802400950120     C*****                ENDSR
802500950120     O****
802600040127     C*------------------------------------------------------------------------*
802700040129     C*? reperisce data competenza a seconda se ciclo partenza o arrivo
802800040127     C*------------------------------------------------------------------------*
802900040129     C     SRcdp         BEGSR
803000040129     c* -PRIMA-
803100040129IF  1C                   IF        $tdat<>' ' and $tdat<>'R'
803200040617     c* date arrivo transiti
803300040129IF  1C                             and $tdat<>'X' and $tdat<>'W'
803400040617IF  1C                             and $tdat<>'H' and $tdat<>'L'
803500040617IF  1C                             and $tdat<>'N'
803600040129IF  1C                   IF        wpd='P'
803700040129     c                   MOVEL     wpdsp8        §dspAM            6 0
803800040129     c                   MOVEL     d$pdcm        §dcmAM            6 0
803900040129     c                   Z-ADD     *zeros        §cdpAM            6 0
804000040129     c                   EXSR      CALDRF
804100040129X   1C                   else
804200040129     c* -DOPO-
804300040129     c                   MOVEL     wddsp8        §dspAM            6 0
804400040129     c                   MOVEL     d$ddcm        §dcmAM            6 0
804500040129     c                   Z-ADD     *zeros        §cdpAM            6 0
804600040129     c                   EXSR      CALDRF
804700040129E   1C                   ENDIF
804800040129E   1C                   ENDIF
804900040129     c* stampo l'anomalia per tenerlo monitorizzato
805000040204IF  1C                   IF        (wpd='P' AND d$pdcm <> 0
805100040205IF  1C                             and d$ptbl <> 'A3' and d$ptbl <> 'F3'
805200040205IF  1C                             and d$ptbl <> 'AR'
805300040205IF  1C                             and d$pcca <> '5' and d$pcca <> '7'
805400040205IF  1C                             and d$plna <> 81 and d$plna <> 82
805500040205IF  1C                             and d$plna <> 104 and d$plna <> 113) or
805600040204IF  1C                             (wpd='D' AND d$Ddcm <> 0
805700040205IF  1C                             and d$dtbl <> 'A3' and d$dtbl <> 'F3'
805800040205IF  1C                             and d$dtbl <> 'AR'
805900040205IF  1C                             and d$dcca <> '5' and d$dcca <> '7'
806000040205IF  1C                             and d$dlna <> 81 and d$dlna <> 82
806100040205IF  1C                             and d$dlna <> 104 and d$dlna <> 113)
806200041110     c* non stampa più la segnalazione dell'anomalia
806300041110     c*                  exsr      srstampa
806400040130E   1C                   ENDIF
806500040129     c*
806600040129     C                   ENDSR
806700040129     C*------------------------------------------------------------------------*
806800040129     C*? STAMPA ERRORE REPERIMENTO DATA COMPETENZA
806900040129     C*------------------------------------------------------------------------*
807000040129     C     SRSTAMPA      BEGSR
807100040130     C* APRO FILE DI STAMPA
807200040205     C                   IF        *inof
807300040130     C                   EXCEPT    TESTATA
807400040205     c                   setoff                                       of
807500040130     C                   END
807600040130     C*
807700040127     C*
807800040127     C* STAMPA RIGA DI DETTAGLIO X IMMAGINE PRIMA
807900040127     C                   IF        WPD = 'P'
808000040127     C                   EXCEPT    RIGAP
808100040127     C                   END
808200040127     C*
808300040127     C* STAMPA RIGA DI DETTAGLIO X IMMAGINE DOPO
808400040127     C                   IF        WPD = 'D'
808500040127     C                   EXCEPT    RIGAD
808600040127     C                   END
808700040127     C                   ENDSR
808800941215     O*------------------------------------------------------------------------*
808900040127     OQSYSPRT   E            TESTATA          01
809000040127     O                                            8 'TRECD7R'
809100040127     O                                           74 'ERRORE REPERIMENTO DATA CO'
809200040127     O                                           82 'MPETENZA'
809300040127     O          E            RIGAP      01
809400040127     O                                           21 'IMMAGINE PRIMA BOLLA:'
809500040127     O                       D$PAAS              +1
809600040127     O                       D$PLNP              +1
809700040127     O                       D$PNRS              +1
809800040127     O                       D$PNSP              +1
809900040205     O                                           +1 'LNA'
810000040205     O                       d$plna              +1
810100040205     O                                           +1 'TBL'
810200040205     O                       d$ptbl              +1
810300040205     O                                           +1 'COMP.'
810400040205     O                       CP                  +1
810500040127     O                                           +1 'TIPO DATA:'
810600040127     O                       $TDAT               +1
810700040127     O          E            RIGAD      01
810800040127     O                                           20 'IMMAGINE DOPO BOLLA:'
810900040127     O                       D$DAAS              +1
811000040127     O                       D$DLNP              +1
811100040127     O                       D$DNRS              +1
811200040127     O                       D$DNSP              +1
811300040205     O                                           +1 'LNA'
811400040205     O                       d$dlna              +1
811500040205     O                                           +1 'TBL'
811600040205     O                       d$dtbl              +1
811700040127     O                                           +1 'COMP.'
811800040127     O                       CD                  +1
811900040127     O                                           +1 'TIPO DATA:'
812000040127     O                       $TDAT               +1
812100040127     O*PRTF198 E  102           TESTA
812200950120     O****                                8 'TRECD7R '
812300950120     O****                               82 'PRESTAZIONI PGM'
812400950120     O****                              150 'Data:'
812500950120     O****                     DATA     159 '  /  /  '
812600950120     O****                              167 'Ora:'
812700950120     O****                     ORA      178 '  .  .  '
812800950120     O****                              190 'Pagina:'
812900950120     O****                     PAGE  Z  198
813000950120     O*------------------------
813100950120     O****    E  1             RIGA
813200950120     O****                               30 'NUMERO DI BOLLE.....:'
813300950120     O****                     WBOLLE2   40
813400950120     O****    E  1             RIGA
813500950120     O****                               30 'NUM.TOTALE SCRITTURE:'
813600950120     O****                     STTOT 2   40
813700950120     O****    E  1             RIGA
813800950120     O****                               30 'NUM.AGG.IN MEMORIA..:'
813900950120     O****                     STAGG 2   40
814000950120     O****    E  1             RIGA
814100950120     O****                               30 'RAGG. MAX N. BOLLE..:'
814200950120     O****                     STMAXP2   40
814300950120     O****    E  1             RIGA
814400950120     O****                               30 'RAGG. MAX INDICE....:'
814500950120     O****                     STMAXI2   40
814600950120     O****    E  1             RIGA
814700950120     O****                               30 'TOT.INDICI TUTTI GIRI'
814800950120     O****                     STTOTX2   40
814900950120     O****    E  1             RIGA
815000950120     O****                               30 'N.GIRI TOTALE.......:'
815100950120     O****                     STTOTG2   40
815200950120     O****    E  1             RIGA
815300950120     O****                               30 'INDICE X MASSIMO RAG:'
815400950120     O****                     STULTX2   40
815500950120     O****    E  1             RIGA
815600950120     O****                               30 'N.MASSIMO BOLLE.....:'
815700950120     O****                     MAXBOL2   40
815800950120     O****    E  1             RIGA
815900950120     O****                               30 'N.MASSIMO INDICE....:'
816000950120     O****                     MAXIND2   40
816100950120     O*------------------------
816200950120     O****    E   60           STAFIN
816300950120     O****                               76 '*** FINE STAMPA ***'
816400950120     O*------------------------------------------------------------------------*
816500950316** ======== SCHIERA: MSG  (MESSAGGI)                       ================
816600980130TRECD7R- Tab. '2K' (dati vari per saldi) non trovata.                     NO2K 1
816700941223TRECD7R- Tab. '2T' (riferim. centri costo) non trovata.                   NO2T 2
816800941223TRECD7R- Riferimento centro di costo inesistente.                         NRIC 3
816900941223TRECD7R- Non trovati importi x comp.fisse dell'immagine bolla -PRIMA-     NCET 4
817000941223TRECD7R- Non trovati importi x comp.variabili dell'immagine bolla -PRIMA- NCED 5
817100950203TRECD7R- Tab. 'CV' (divise cambio in corso),divisa ECU non trovata.       CVNT 6
817200970901TRECD7R- Tab. '$2' (sigle varie forzate a pgm) non trovate.               $2NT 7
817300941223TRECD7R- Regione non trovata in tab. '14'. Avvertire CED.                 RENT 8
817400941223TRECD7R- Si è cercato di annullare un rec. di ECCED00F inesistente.       ACED 9
817500941223TRECD7R- Si è cercato di modificare un rec.di ECCED00F inesistente.       MCED10
817600030114TRECD7R- Si è cercato di annullare  un rec.di ECCET00F inesistente.       ACET11
817700030114TRECD7R- Si è cercato di modificare un rec.di ECCET00F inesistente.       MCET12
817800941223TRECD7R- Tabella '2L' non trovata. Avvertire CED.                         TB2L13
817900941223TRECD7R- Tabella '1P' non trovata. Avvertire CED.                         TB1P14
818000941223TRECD7R- Tabella 'CC' non trovata. Avvertire CED.                         TBCC15
818100020705TRECD7R- Tabella 'NTW' non trovata. Avvertire CED.                        TNTW16
818200941223TRECD7R- Tabella '14' non trovata. Avvertire CED.                         TB1417
818300941223TRECD7R- Filiale non trovata in organigramma.                             FNTO18
818400030114TRECD7R- Si è cercato di annullare un rec. in ECCETP0F.                   DETP19
818500030114TRECD7R- Si è cercato di aggiornare un rec.in ECCETP0F.                   UETP20
818600030114TRECD7R- Si è cercato di annullare un rec. in ECCEDP0F.                   DEDP21
818700030114TRECD7R- Si è cercato di aggiornare un rec.in ECCEDP0F.                   UEDP22
818800020705TRECD7R- Network filiale non trovato in tab. 'NTW'. Avvertire CED.        NWNT23
818900030114TRECD7R- Non trovati importi x voci coop dell'immagine bolla -PRIMA-      NCEV24
819000030114TRECD7R- Si è cercato di annullare  un rec.di ECCEV00F inesistente.       ACEV25
819100030114TRECD7R- Si è cercato di annullare un rec. in ECCEVP0F.                   DEVP26
819200030114TRECD7R- Si è cercato di modificare un rec.di ECCEV00F inesistente.       MCET27
819300030114TRECD7R- Si è cercato di aggiornare un rec.in ECCEVP0F.                   UETP28
819400030217TRECD7R- Voce non trovata.  XXX                                           VNTR29
819500031105TRECD7R- Si è tentato di scrivere un record duplicato nel file ECCET30C   DCET30
819600031105TRECD7R-                                                                      31
819700031105TRECD7R-                                                                      32
819800031105TRECD7R-                                                                      33
819900031105TRECD7R-                                                                      34
820000031105TRECD7R-                                                                      35
820100031105TRECD7R-                                                                      36
820200031105TRECD7R-                                                                      37
820300031105TRECD7R-                                                                      38
820400031105TRECD7R-                                                                      39
820500031105TRECD7R-                                                                      40
