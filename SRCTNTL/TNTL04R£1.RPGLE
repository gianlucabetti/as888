000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - FERMATE - SCELTA SUBFILE
000400000000     H*------------------------------------------------------------------------*
000500951130     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL04D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL04DVS:SFLULT)
001000000000     F                                     SFILE(TL04DVM:MSGULT)
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FAZCAE01L  IF   E           K DISK
001300000000     FTABEL00F  IF   E           K DISK
001400000000     FTNTLT01L  UF A E           K DISK
001500000000     F                                     COMMIT
001600000000     FTNTLR01L  UF A E           K DISK
001700000000     F                                     COMMIT
001800000000     FTNTLO01L  UF A E           K DISK
001900000000     F                                     COMMIT
002000951130     FTNTLN01L  UF A E           K DISK
002100951130     F                                     COMMIT
002200110519     FTNTLS01L  UF A E           K DISK
002300110519     F                                     COMMIT
002400110519     FTNpsc01L  IF A E           K DISK
002500000000     D*------------------------------------------------------------------------*
002600000000     D* SCHIERE
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
003000000000     D*-------------------
003100000000     D MSG             S             78    DIM(70) CTDATA PERRCD(1)             *MESSAGGI
003200000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
003300000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
003400000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003500000000     D*-------------------
003600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003700000000     D*-------------------
003800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
004000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
004100000000     D*-------------------
004200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
004300000000     D*-------------------
004400000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
004500000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
004600000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
004700000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004800000000     D*-------------------
004900000000     D* TABELLA DESCRIZIONI A VIDEO
005000000000     D*-------------------
005100000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
005200000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
005300000000     D*-------------------
005400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005500000000     D*-------------------
005600000000     D KEYS            S              1    DIM(40)
005700000000     D*-------------------
005800000000     D* GIORNI DI PARTENZA TRAINO
005900000000     D*-------------------
006000000000     D GIO             S              1    DIM(8)                               *GIORNI (1÷8)
006100000000     D DGIO            S              5    DIM(8) CTDATA PERRCD(1)              *DESCRIZIONE SINTETI
006200000000     D DGES            S             16    DIM(8) CTDATA PERRCD(1)              *DESCRIZIONE ESTESA
006300000000     D*-------------------
006400000000     D* PROGRESSIVO GIORNI
006500000000     D*-------------------
006600000000     D DALF            S             21    DIM(1) CTDATA PERRCD(1)              *ALFABETO (1÷21)
006700000000     D ALF             S              1    DIM(21)                              *ALFABETO (1÷21)
006800000000     D*-------------------
006900000000     D* GIORNI CARICATI IN MEMORIA
007000000000     D*-------------------
007100000000     D GCM             S              1    DIM(8)                               *GIORNI (1÷8)
007200000000     D GCA             S              1    DIM(8)                               *GIORNI ANNULLATI
007300000000     D*-------------------
007400000000     D* FILIALI
007500000000     D*-------------------
007600971125     D ORG             S              3  0 DIM(500)                             *CODICE
007700971125     D FEL             S              3  0 DIM(500)                             *FILIALE ELBORATORE
007800971125     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
007900971125     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
008000971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
008100000518     D PORG            S              1    DIM(500)                             *S=P.O. POSTE
008200090114     D NTW             S              3    DIM(500)                             *network
008300000000     D*-------------------
008400000000     D* SCHIERE DI MEMORIZZAZIONE  - FILE -
008500000000     D*-------------------
008600000000     D***
008700000000     D* FERMATE
008800000000     D***
008900000000     D FTT             S              1    DIM(50)                              *TIPO FERMATE
009000000000     D FIL             S              3  0 DIM(50)                              *FILIALE FERMATA
009100000000     D FCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
009200000000     D FAR             S              1    DIM(50)                              *CARICO IN PARTENZA
009300000000     D FMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
009400000000     D FEM             S              4  0 DIM(50)                              *TEMPO FERMATA
009500000000     D FFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
009600000000     D***
009700000000     D* ORARI - 8 GIORNI -
009800000000     D***
009900000000     D*-------------------
010000000000     D* SCHIERE DI MEMORIZZAZIONE    -DOPO-
010100000000     D*-------------------
010200000000     D DTT             S              1    DIM(50)                              *TIPO FERMATA
010300000000     D DIL             S              3  0 DIM(50)                              *FILIALE FERMATA
010400000000     D DGA             S              1    DIM(50)                              *GIORNO ARRIVO
010500000000     D DHA             S              4  0 DIM(50)                              *ORA ARRIVO
010600000000     D DCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
010700000000     D DGP             S              1    DIM(50)                              *GIORNO PARTENZA
010800000000     D DHP             S              4  0 DIM(50)                              *ORA PARTENZA
010900000000     D DAR             S              1    DIM(50)                              *CARICO IN PARTENZA
011000000000     D DMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
011100000000     D DEM             S              4  0 DIM(50)                              *TEMPO FERMATA
011200000000     D DEQ             S              3  0 DIM(50)                              *SEQUENZA
011300000000     D DFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
011400000000     D*-------------------
011500000000     D* SCHIERE DI MEMORIZZAZIONE    -PRIMA-
011600000000     D*-------------------
011700000000     D PTT             S              1    DIM(50)                              *TIPO FERMATA
011800000000     D PIL             S              3  0 DIM(50)                              *FILIALE FERMATA
011900000000     D PGA             S              1    DIM(50)                              *GIORNO ARRIVO
012000000000     D PHA             S              4  0 DIM(50)                              *ORA ARRIVO
012100000000     D PCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
012200000000     D PGP             S              1    DIM(50)                              *GIORNO PARTENZA
012300000000     D PHP             S              4  0 DIM(50)                              *ORA PARTENZA
012400000000     D PAR             S              1    DIM(50)                              *CARICO IN PARTENZA
012500000000     D PMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
012600000000     D PEM             S              4  0 DIM(50)                              *TEMPO FERMATA
012700000000     D PEQ             S              3  0 DIM(50)                              *SEQUENZA
012800000000     D PFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
012900000000     D*-------------------
013000000000     D* SCHIERE DI MEMORIZZAZIONE    -LAVORO-
013100000000     D*-------------------
013200000000     D LTT             S              1    DIM(50)                              *TIPO FERMATA
013300000000     D LIL             S              3  0 DIM(50)                              *FILIALE FERMATA
013400000000     D LGA             S              1    DIM(50)                              *GIORNO ARRIVO
013500000000     D LHA             S              4  0 DIM(50)                              *ORA ARRIVO
013600000000     D LCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
013700000000     D LGP             S              1    DIM(50)                              *GIORNO PARTENZA
013800000000     D LHP             S              4  0 DIM(50)                              *ORA PARTENZA
013900000000     D LAR             S              1    DIM(50)                              *CARICO IN PARTENZA
014000000000     D LMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
014100000000     D LEM             S              4  0 DIM(50)                              *TEMPO FERMATA
014200000000     D LEQ             S              3  0 DIM(50)                              *SEQUENZA
014300000000     D LFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
014400000000     D*-------------------
014500000000     D* SCHIERE DI LAVORO
014600000000     D*-------------------
014700000000     D JFT             S              4    DIM(50)                              FILIALE + TIPO FERMA
014800960423     D*-------------------
014900960423     D* SCHIERE ALFA/NUMERICHE DI LAVORO
015000960423     D*-------------------
015100960424     D SA7             S              1    DIM(7)                               *ALFANUMERICA 7
015200000000     D*------------------------------------------------------------------------*
015300000000     D* INPUT
015400000000     D*------------------------------------------------------------------------*
015500000000     D*-------------------
015600000000     D* SCHIERE DI MEMORIZZAZIONE    -FILE-          - ORARI X 8 GIORNI -
015700000000     D*-------------------
015800000000     D* Es. (4:2) + 1 = 3  /  (3 X 50) =  150
015900000000     D*-------------------
016000000000     D FGGG            DS                  OCCURS(8) INZ
016100000000     D  FGA                    1     50
016200000000     D                                     DIM(50)                              *GIORNO ARRIVO
016300000000     D  FHA                   51    200P 0
016400000000     D                                     DIM(50)                              *ORA ARRIVO
016500000000     D                                     PACKEVEN
016600000000     D  FGP                  201    250
016700000000     D                                     DIM(50)                              *GIORNO PARTENZA
016800000000     D  FHP                  251    400P 0
016900000000     D                                     DIM(50)                              *ORA PARTENZA
017000000000     D                                     PACKEVEN
017100000000     D*---------------------
017200000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
017300000000     D*---------------------
017400000000     D DSOPZ         E DS
017500000000     D  OPZRIG                 1     40
017600000000     D*---------------------
017700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
017800000000     D*---------------------
017900000000     D DSFUN         E DS
018000000000     D  FUNRIG                 1     40
018100000000     D*---------------------
018200000000     D* DS PER GESTIONE TASTI FUNZIONALI
018300000000     D*---------------------
018400000000     D DSKEYS        E DS
018500000000     D  KEYRIG                 1     40
018600000000     D*---------------------
018700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
018800000000     D*---------------------
018900000000     D                SDS
019000000000     D  USER                 254    263
019100000000     D*---------------------
019200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
019300000000     D*---------------------
019400000000     D DEVDS1          DS
019500000000     D  KEY                  369    369
019600000000     D  CURSOR               370    371
019700000000     D  FRTSFL               378    379B 0
019800000000     D*---------------------
019900000000     D* ARCHITETTURA
020000000000     D*---------------------
020100000000     D KPJBA         E DS
020200000000     D*---------------------
020300000000     D* DS ESTERNA DI PROCEDURA
020400000000     D*---------------------
020500000000     D DS00          E DS                  EXTNAME(TNTL00DS)
020600000000     D*---------------------
020700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
020800000000     D*---------------------
020900000000     D DSMSG         E DS
021000000000     D*---------------------
021100000000     D* DS TIPI TRAINI
021200000000     D*---------------------
021300000000     D DSTV          E DS
021400960208     D*---------------------
021500960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
021600960208     D*---------------------
021700960208     D OG148         E DS
021800000518     D*---------------------
021900000518     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
022000000518     D*---------------------
022100000518     D OG143         E DS
022200971020     D*------------------
022300971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
022400971020     D*------------------
022500971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
022600020715
022700020715     d DdatiUte      e ds
022800020715     d AzuteDs       e ds                  ExtName(Azute00f)
022900020715     d Tibs34Ds      e ds                  Inz
023000090115     d Ficna2ds      e ds                  Inz
023100110525     d Kgse            s                   like(d00gse)
023200110525     d NumFER          s                   like(sflult)
023300110519     d wESTLS          s              1
023400110525     d wVisualTLS      s              1
023500020715
023600000000     D*------------------
023700000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
023800000000     D*------------------
023900000000     D WLBDA8          DS
024000000000     D  G08DAT                 1      8  0
024100000000     D  G08INV                 9     16  0
024200000000     D  G08ERR                17     17
024300000000     D  G08TGI                18     22  0
024400000000     D*------------------
024500000000     D* COSTANTI
024600000000     D*------------------
024700000000     D* MENU
024800000000     D DESGES          C                   CONST('** GESTIONE **')
024900000000     D DESINT          C                   CONST('INTERROGAZIONE')
025000000000     D* COMMENTI
025100000000     D DESNEW          C                   CONST('GIORNO NUOVO')
025200000000     D DESATB          C                   CONST('  ANNULLATO ')
025300000000     D*------------------
025400961212     D* DS SCOMPOSIZIONE MESSAGGIO DI ERRORE
025500000000     D*------------------
025600000000     D* MSG,7
025700000000     D                 DS
025800000000     D  ERRS07                74     76
025900000000     D  ERRM07                 1     78
026000000000     D* MSG,8
026100000000     D                 DS
026200000000     D  ERRS08                74     76
026300000000     D  ERRM08                 1     78
026400000000     D* MSG,9
026500000000     D                 DS
026600000000     D  ERRS09                74     76
026700000000     D  ERRM09                 1     78
026800000000     D* MSG,10
026900000000     D                 DS
027000000000     D  ERRS10                74     76
027100000000     D  ERRM10                 1     78
027200000000     D* MSG,11
027300000000     D                 DS
027400000000     D  ERRS11                74     76
027500000000     D  ERRM11                 1     78
027600000000     D* MSG,13
027700000000     D                 DS
027800000000     D  ERRS13                74     76
027900000000     D  ERRM13                 1     78
028000000000     D* MSG,14
028100000000     D                 DS
028200000000     D  ERRS14                74     76
028300000000     D  ERRM14                 1     78
028400000000     D* MSG,15
028500000000     D                 DS
028600000000     D  ERRS15                74     76
028700000000     D  ERRM15                 1     78
028800000000     D* MSG,16
028900000000     D                 DS
029000000000     D  ERRS16                74     76
029100000000     D  ERRM16                 1     78
029200000000     D* MSG,18
029300000000     D                 DS
029400000000     D  ERRS18                74     76
029500000000     D  ERRM18                 1     78
029600000000     D* MSG,19
029700000000     D                 DS
029800000000     D  ERRS19                74     76
029900000000     D  ERRM19                 1     78
030000000000     D* MSG,20
030100000000     D                 DS
030200000000     D  ERRS20                74     76
030300000000     D  ERRM20                 1     78
030400000000     D* MSG,21
030500000000     D                 DS
030600000000     D  ERRS21                74     76
030700000000     D  ERRM21                 1     78
030800000000     D* MSG,22
030900000000     D                 DS
031000000000     D  ERRS22                74     76
031100000000     D  ERRM22                 1     78
031200000000     D* MSG,23
031300000000     D                 DS
031400000000     D  ERRS23                74     76
031500000000     D  ERRM23                 1     78
031600000000     D* MSG,25
031700000000     D                 DS
031800000000     D  ERRS25                74     76
031900000000     D  ERRM25                 1     78
032000000000     D* MSG,26
032100000000     D                 DS
032200000000     D  ERRS26                74     76
032300000000     D  ERRM26                 1     78
032400000000     D* MSG,27
032500000000     D                 DS
032600000000     D  ERRS27                74     76
032700000000     D  ERRM27                 1     78
032800000000     D* MSG,28
032900000000     D                 DS
033000000000     D  ERRS28                74     76
033100000000     D  ERRM28                 1     78
033200000000     D* MSG,29
033300000000     D                 DS
033400000000     D  ERRS29                74     76
033500000000     D  ERRM29                 1     78
033600000000     D* MSG,30
033700000000     D                 DS
033800000000     D  ERRS30                74     76
033900000000     D  ERRM30                 1     78
034000000000     D* MSG,31
034100000000     D                 DS
034200000000     D  ERRS31                74     76
034300000000     D  ERRM31                 1     78
034400000000     D* MSG,32
034500000000     D                 DS
034600000000     D  ERRS32                74     76
034700000000     D  ERRM32                 1     78
034800000000     D* MSG,33
034900000000     D                 DS
035000000000     D  ERRS33                74     76
035100000000     D  ERRM33                 1     78
035200000000     D* MSG,34
035300000000     D                 DS
035400000000     D  ERRS34                74     76
035500000000     D  ERRM34                 1     78
035600000000     D* MSG,35
035700000000     D                 DS
035800000000     D  ERRS35                74     76
035900000000     D  ERRM35                 1     78
036000960206     D* MSG,36
036100960206     D                 DS
036200960206     D  ERRS36                74     76
036300960206     D  ERRM36                 1     78
036400000000     D* MSG,38
036500000000     D                 DS
036600000000     D  ERRS38                74     76
036700000000     D  ERRM38                 1     78
036800000000     D* MSG,40
036900000000     D                 DS
037000000000     D  ERRS40                74     76
037100000000     D  ERRM40                 1     78
037200000000     D* MSG,41
037300000000     D                 DS
037400000000     D  ERRS41                74     76
037500000000     D  ERRM41                 1     78
037600000000     D* MSG,42
037700000000     D                 DS
037800000000     D  ERRS42                74     76
037900000000     D  ERRM42                 1     78
038000000000     D* MSG,43
038100000000     D                 DS
038200000000     D  ERRS43                74     76
038300000000     D  ERRM43                 1     78
038400000000     D* MSG,44
038500000000     D                 DS
038600000000     D  ERRS44                74     76
038700000000     D  ERRM44                 1     78
038800000000     D* MSG,45
038900000000     D                 DS
039000000000     D  ERRS45                74     76
039100000000     D  ERRM45                 1     78
039200000000     D* MSG,46
039300000000     D                 DS
039400000000     D  ERRS46                74     76
039500000000     D  ERRM46                 1     78
039600000000     D* MSG,47
039700000000     D                 DS
039800000000     D  ERRS47                74     76
039900000000     D  ERRM47                 1     78
040000000000     D* MSG,48
040100000000     D                 DS
040200000000     D  ERRS48                74     76
040300000000     D  ERRM48                 1     78
040400000000     D* MSG,49
040500000000     D                 DS
040600000000     D  ERRS49                74     76
040700000000     D  ERRM49                 1     78
040800000000     D* MSG,50
040900000000     D                 DS
041000000000     D  ERRS50                74     76
041100000000     D  ERRM50                 1     78
041200000000     D* MSG,51
041300000000     D                 DS
041400000000     D  ERRS51                74     76
041500000000     D  ERRM51                 1     78
041600000000     D* MSG,52
041700000000     D                 DS
041800000000     D  ERRS52                74     76
041900000000     D  ERRM52                 1     78
042000951218     D* MSG,53
042100951218     D                 DS
042200951218     D  ERRS53                74     76
042300951218     D  ERRM53                 1     78
042400000000     D* MSG,54
042500000000     D                 DS
042600000000     D  ERRS54                74     76
042700000000     D  ERRM54                 1     78
042800000000     D* MSG,55
042900000000     D                 DS
043000000000     D  ERRS55                74     76
043100000000     D  ERRM55                 1     78
043200000000     D* MSG,56
043300000000     D                 DS
043400000000     D  ERRS56                74     76
043500000000     D  ERRM56                 1     78
043600000000     D* MSG,57
043700000000     D                 DS
043800000000     D  ERRS57                74     76
043900000000     D  ERRM57                 1     78
044000000000     D* MSG,60
044100000000     D                 DS
044200000000     D  ERRS60                74     76
044300000000     D  ERRM60                 1     78
044400000000     D* MSG,61
044500000000     D                 DS
044600000000     D  ERRS61                74     76
044700000000     D  ERRM61                 1     78
044800000000     D* MSG,62
044900000000     D                 DS
045000000000     D  ERRGIO                22     37
045100000000     D  ERRM62                 1     78
045200951218     D* MSG,64
045300951218     D                 DS
045400951218     D  ERRS64                74     76
045500951218     D  ERRM64                 1     78
045600960717     D* MSG,65
045700960717     D                 DS
045800960717     D  ERRS65                74     76
045900960717     D  ERRM65                 1     78
046000090114     D* MSG,66
046100090114     D                 DS
046200090114     D  ERRS66                74     76
046300090114     D  ERRM66                 1     78
046400000000     C*------------------------------------------------------------------------*
046500000000     C* MAIN LINE
046600000000     C*------------------------------------------------------------------------*
046700000000     C*
046800000000     C* CICLO FINO A CHE:
046900000000DO  1C     $FINE         DOUEQ     '1'
047000000000     C*
047100000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
047200000000     C*
047300000000     C* CONTROLLA GESTIONE VIDEO
047400000000IF  2C     $GES          IFEQ      'VS'
047500000000     C                   EXSR      GESVS                                        *SUBFILE
047600000000E   2C                   ENDIF
047700000000     C*
047800000000E   1C                   ENDDO
047900000000     C*
048000000000IF  1C     $F03          IFEQ      '1'
048100000000     C                   MOVEL     '1'           D00F03
048200000000E   1C                   ENDIF
048300000000IF  1C     $F12          IFEQ      '1'
048400000000     C                   MOVEL     '1'           D00F12
048500000000E   1C                   ENDIF
048600971020     C* OPERAZIONI FINALI
048700971020     C                   EXSR      FINSR
048800971020     C*
048900000000     C                   MOVEL     DS00          KPJBU
049000000000     C                   SETON                                        LR
049100000000     C*------------------------------------------------------------------------*
049200000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
049300000000     C*------------------------------------------------------------------------*
049400000000     C     CARVS         BEGSR
049500000000     C*
049600000000     C* PULISCE IL -DOPO-
049700000000     C                   CLEAR                   DEQ                            *FERMATE
049800000000     C                   CLEAR                   DTT
049900000000     C                   CLEAR                   DIL
050000000000     C                   CLEAR                   DCA
050100000000     C                   CLEAR                   DAR
050200000000     C                   CLEAR                   DMT
050300000000     C                   CLEAR                   DEM
050400000000     C                   CLEAR                   DFV
050500000000     C                   CLEAR                   DGA                            *ORARI
050600000000     C                   CLEAR                   DHA
050700000000     C                   CLEAR                   DGP
050800000000     C                   CLEAR                   DHP
050900000000     C*
051000000000     C* EMETTE UN MESSAGGIO SE LE FERMATE SONO ANNULLATE (L'UNICO CASO E' QUANDO
051100000000     C* ARRIVA DALL'INTERROGAZIONE DI UN TRAINO ANNULLATO)
051200000000     C                   MOVEL     *BLANKS       VODATB                         *RECORD NON ANNULLAT
051300000000     C                   MOVEL     '0'           *IN48                          *NON VISUALIZZATO
051400000000IF  1C     TLTATB        IFNE      *BLANKS
051500000000     C                   MOVEL     DESATB        VODATB                         *RECORD ANNULLATO
051600000000     C                   MOVEL     '1'           *IN48                          *VISUALIZZATO
051700000000E   1C                   ENDIF
051800000000     C*
051900000000     C* DECODIFICA IL GIORNO RICHIESTO
052000000000     C                   MOVEL     D00GSE        VCCGSE                         *GIORNO SETTIMANA
052100000000IF  1C     VCCGSE        IFNE      *BLANKS
052200000000     C                   Z-ADD     *ZEROS        J
052300000000     C                   MOVE      VCCGSE        J
052400000000     C                   MOVEL     DGIO(J)       VCDGSE                         *DECOD. GG IN LINEA
052500000000X   1C                   ELSE
052600000000     C                   MOVEL     DGIO(8)       VCDGSE                         *DECOD. GG IN LINEA
052700000000E   1C                   ENDIF
052800000000     C*
052900000000     C* CARICA NELLE SCHIERE -DOPO- I DATI DEL GIORNO (GIA' MEMORIZZATI);
053000000000     C* SE IL GIORNO E' VUOTO CARICA SOLO I DATI DELLA FERMATA
053100000000IF  1C     D00GSE        IFEQ      *BLANKS
053200000000     C                   Z-ADD     8             JJ
053300000000X   1C                   ELSE
053400000000     C                   Z-ADD     *ZEROS        JJ
053500000000     C                   MOVE      D00GSE        JJ
053600000000E   1C                   ENDIF
053700000000     C     JJ            OCCUR     FGGG
053800000000     C*
053900000000     C                   Z-ADD     *ZEROS        DEPSEQ
054000000000     C                   MOVEL     DESNEW        VCCNEW                         *GIORNO NUOVO
054100000000     C                   MOVEL     '1'           *IN47                          *VISUALIZZATO
054200000000DO  1C     1             DO        50            J
054300000000IF  2C     FIL(J)        IFGT      *ZEROS
054400000000     C     FHA(J)        ORGT      *ZEROS
054500000000     C     FHP(J)        ORGT      *ZEROS
054600000000     C     J             MULT      10            DEPSEQ
054700000000     C                   Z-ADD     DEPSEQ        DEQ(J)                         *FERMATE
054800000000     C                   MOVEL     FTT(J)        DTT(J)
054900000000     C                   Z-ADD     FIL(J)        DIL(J)
055000000000     C                   MOVEL     FCA(J)        DCA(J)
055100000000     C                   MOVEL     FAR(J)        DAR(J)
055200000000     C                   Z-ADD     FMT(J)        DMT(J)
055300000000     C                   Z-ADD     FEM(J)        DEM(J)
055400000000     C                   MOVEL     FGA(J)        DGA(J)                         *ORARI
055500000000     C                   Z-ADD     FHA(J)        DHA(J)
055600000000     C                   MOVEL     FGP(J)        DGP(J)
055700000000     C                   Z-ADD     FHP(J)        DHP(J)
055800000000     C                   MOVEL     FFV(J)        DFV(J)
055900000000IF  3C     FHA(J)        IFGT      *ZEROS
056000000000     C     FHP(J)        ORGT      *ZEROS
056100000000     C                   MOVEL     *BLANKS       VCCNEW                         *GIORNO NON NUOVO
056200000000     C                   MOVEL     '0'           *IN47                          *NON VISUALIZZATO
056300000000E   3C                   ENDIF
056400000000E   2C                   ENDIF
056500000000E   1C                   ENDDO
056600000000     C* EMISSIONE
056700000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
056800000000     C*
056900000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
057000000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
057100000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
057200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
057300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
057400000000     C*
057500000000     C                   WRITE     TL04DVT                                      *TESTATA
057600000000     C                   WRITE     TL04DVO                                      *OPZIONI
057700000000     C                   WRITE     TL04DVF                                      *FUNZIONI
057800000000     C*
057900000000     C                   EXSR      POSSFL                                       *CARICA SFL
058000000000     C*
058100000000     C                   ENDSR
058200000000     C*------------------------------------------------------------------------*
058300000000     C* GESVS  - GESTIONE SUBFILE
058400000000     C*------------------------------------------------------------------------*
058500000000     C     GESVS         BEGSR
058600000000     C*
058700000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
058800000000     C                   WRITE     TL04DVT                                      *TESTATA
058900000000     C                   WRITE     TL04DVO                                      *OPZIONI
059000000000     C                   WRITE     TL04DVF                                      *FUNZIONI
059100000000E   1C                   ENDIF
059200000000     C*
059300000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
059400000000     C*
059500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
059600000000     C*
059700000000IF  1C     $ERR          IFEQ      '0'
059800000000SL  2C                   SELECT
059900110519     C     KEY           WHENEQ    F2
060000110519     C                   EXSR      F02GES                                       *punti scambio
060100000000     C     KEY           WHENEQ    F6
060200000000     C                   EXSR      F06GES                                       *REGISTRA SU FILE
060300000000     C     KEY           WHENEQ    F7
060400000000     C                   EXSR      F07GES                                       CANCELLA ORARI GIORN
060500000000     C     KEY           WHENEQ    F9
060600000000     C                   EXSR      F09GES                                       *CAMBIA GIORNO
060700000000     C     KEY           WHENEQ    F10
060800000000     C                   EXSR      F10GES                                       *INSERIMENTO
060900000000     C                   OTHER
061000000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
061100000000E   2C                   ENDSL
061200000000E   1C                   ENDIF
061300000000     C*
061400000000     C                   ENDSR
061500110519     C*------------------------------------------------------------------------*
061600110519     C* F02GES - Punti di scambio
061700110519     C*------------------------------------------------------------------------*
061800110519     C     F02GES        BEGSR
061900110526     C****               MOVEL     'F02'         D00OP1                         *LIVELLO PROGRAMMA
062000110519     C                   MOVEL     '0'           $ERR
062100110519     C*
062200110519IF  1C     SFLULT        IFGT      *ZEROS
062300110519     C                   EXSR      CARGIO
062400110519IF  2C     $ERR          IFEQ      '1'
062500110519     C                   Z-ADD     WNBR          VCCNBR                         *OUTPUT DI DECODI
062600110519     C     VCCNBR        CHAIN     TL04DVS                            99
062700110519     C                   MOVEL     '1'           *IN46
062800110519     C                   UPDATE    TL04DVS
062900110519E   2C                   ENDIF
063000110519E   1C                   ENDIF
063100110519     C*
063200110519     C* SE CI SONO ERRORI RIMANE NEL VIDEO, ALTRIMENTI CONTROLLA TUTTI I DATI
063300110519IF  1C     $ERR          IFEQ      '1'
063400110519     C                   EXSR      CARMSG
063500110519     C                   ELSE
063600110519     C*
063700110519     C* LANCIA LA GESTIONE
063800110519     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
063900110519     C                   Z-ADD     d00dde        WDDE                           *DATA DECORRENZA
064000110519     C*
064100110526     C                   MOVEL     d00op0        WOP0                           *LIVELLO PROCEDURA
064200110519     C                   MOVEL     'F02'         WOP1                           *LIVELLO PROGRAMMA
064300110519     C                   MOVEL     'TNTLZ4R'     WPGM                           *PGM DI GESTIONE
064400110519     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
064500110525     c
064600110525     c                   exsr      CercaTLS
064700110525     c                   exsr      possfl
064800110519E   1C                   ENDIF
064900110519     C*
065000110519     C                   ENDSR
065100000000     C*------------------------------------------------------------------------*
065200000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> REGISTRA SU FILE
065300000000     C*------------------------------------------------------------------------*
065400000000     C     F06GES        BEGSR
065500000000     C*
065600000000     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
065700000000     C                   MOVEL     '0'           $ERR
065800000000     C*
065900000000     C* CARICA L'ULTIMO GIORNO IN MEMORIA
066000000000IF  1C     SFLULT        IFGT      *ZEROS
066100000000     C                   EXSR      CARGIO
066200000000IF  2C     $ERR          IFEQ      '1'
066300000000     C                   Z-ADD     WNBR          VCCNBR                         *OUTPUT DI DECODI
066400000000     C     VCCNBR        CHAIN     TL04DVS                            99
066500000000     C                   MOVEL     '1'           *IN46
066600000000     C                   UPDATE    TL04DVS
066700000000E   2C                   ENDIF
066800000000E   1C                   ENDIF
066900000000     C*
067000000000     C* SE CI SONO ERRORI RIMANE NEL VIDEO, ALTRIMENTI CONTROLLA TUTTI I DATI
067100000000IF  1C     $ERR          IFEQ      '1'
067200000000     C                   EXSR      CARMSG
067300000000     C                   ELSE
067400000000     C*
067500000000     C* CONTROLLA CHE TUTTI GLI ORARI INSERITI SIANO CONGRUENTI CON LE FERMATE
067600000000     C* E CHE LO SIANO FRA LORO
067700000000IF  2C     FIL(1)        IFGT      *ZEROS                                       *QC. DA REGISTRARE
067800000000     C     FHA(1)        ORGT      *ZEROS
067900000000     C     FHP(1)        ORGT      *ZEROS
068000000000DO  3C     1             DO        8             IJ                4 0          *GIORNI
068100000000     C                   MOVEL     '0'           $ERR
068200000000IF  4C     GCM(IJ)       IFNE      *BLANKS                                      *GIORNO INSERITO
068300000000     C     GCA(IJ)       ANDNE     'A'                                          *NON ANNULLATO
068400000000     C     IJ            OCCUR     FGGG
068500000000     C                   MOVEL     FTT           LTT                            *FERMATE
068600000000     C                   Z-ADD     FIL           LIL
068700000000     C                   MOVEL     FCA           LCA
068800000000     C                   MOVEL     FAR           LAR
068900000000     C                   Z-ADD     FMT           LMT
069000000000     C                   Z-ADD     FEM           LEM
069100000000     C                   MOVEL     FFV           LFV
069200000000     C                   MOVEL     FGA           LGA
069300000000     C                   Z-ADD     FHA           LHA
069400000000     C                   MOVEL     FGP           LGP
069500000000     C                   Z-ADD     FHP           LHP
069600000000     C                   Z-ADD     *ZEROS        WNBR
069700000000     C                   EXSR      DECODI
069800000000IF  5C     $ERR          IFEQ      '1'                                          *ERRORE
069900000000IF  6C     IJ            IFEQ      8
070000000000     C                   MOVEL     *BLANKS       D00GSE
070100000000X   6C                   ELSE
070200000000     C                   MOVE      IJ            D00GSE                         *GIORNO DA FAR VEDER
070300000000E   6C                   ENDIF
070400000000     C                   LEAVE
070500000000E   5C                   ENDIF
070600000000E   4C                   ENDIF
070700000000E   3C                   ENDDO
070800000000     C*
070900000000X   2C                   ELSE                                                   *NULLA DA REGISRARE
071000000000     C                   MOVEL     '1'           $ERR
071100000000     C                   MOVEL     MSG(59)       $MSG
071200000000E   2C                   ENDIF
071300000000     C*
071400000000     C* CONTROLLA SE SONO STATI INSERITI TUTTI I GIORNI COME DA TESTATA
071500000000IF  2C     $ERR          IFEQ      '0'
071600000000IF  3C     GCM(8)        IFNE      *BLANKS
071700000000     C     GCA(8)        ANDEQ     *BLANKS
071800000000X   3C                   ELSE
071900000000DO  4C     1             DO        7             J
072000000000     C                   SETOFF                                           28
072100000000     C     GIO(J)        LOOKUP    GCM                                    28
072200000000IF  5C     *IN28         IFEQ      *OFF                                         *NON INSERITO
072300000000     C     GCA(J)        ORNE      *BLANKS                                      *O ANNULLATO
072400000000     C                   MOVEL     '1'           $ERR
072500000000     C                   MOVEL     MSG(58)       $MSG
072600000000E   5C                   ENDIF
072700000000E   4C                   ENDDO
072800000000E   3C                   ENDIF
072900000000E   2C                   ENDIF
073000000000     C*
073100000000     C* NESSUN ERRORE --> AGGOIRNA I FILES ED ESCE DAL PROGRAMMA
073200000000     C* ERRORE -->        PROPONE IL GIORNO SBAGLIATO
073300000000IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
073400000000     C                   EXSR      AGGFIL
073500000000     C                   MOVEL     MSG(37)       $MSG
073600000000     C                   MOVEL     '1'           $FINE                          *ESCE
073700000000     C                   MOVEL     '1'           $F12                           *NO FINESTRA GIORNI
073800000000X   2C                   ELSE                                                   *ERRORI
073900000000     C                   MOVEL     *BLANKS       MSGULT
074000000000     C                   EXSR      CARMSG
074100000000     C                   EXSR      CARVS
074200000000E   2C                   ENDIF
074300000000E   1C                   ENDIF
074400000000     C*
074500000000     C                   ENDSR
074600000000     C*------------------------------------------------------------------------*
074700000000     C* F07GES - GESTIONE TASTO FUNZIONALE F07 ==> CANCELLA ORARI GIORNO
074800000000     C*------------------------------------------------------------------------*
074900000000     C     F07GES        BEGSR
075000000000     C*
075100000000     C                   MOVEL     'F07'         D00OP1                         *LIVELLO PROGRAMMA
075200000000     C*
075300000000IF  1C     D00GSE        IFEQ      *BLANKS
075400000000     C                   Z-ADD     8             J
075500000000X   1C                   ELSE
075600000000     C                   Z-ADD     *ZEROS        J
075700000000     C                   MOVE      D00GSE        J
075800000000E   1C                   ENDIF
075900000000     C*
076000000000     C* SE E' L'UNICO GIORNO NON SI PUO' ANNULLARE
076100000000     C                   Z-ADD     1             JJ
076200000000     C                   MOVEL     '0'           WPIU              1
076300000000DO  1C     JJ            DOWLE     8
076400000000     C     WPIU          ANDEQ     '0'
076500000000IF  2C     GCM(JJ)       IFNE      *BLANKS
076600000000     C     GCA(JJ)       ANDEQ     *BLANKS
076700000000IF  3C     JJ            IFNE      J
076800000000     C                   MOVEL     '1'           WPIU
076900000000E   3C                   ENDIF
077000000000E   2C                   ENDIF
077100000000IF  2C     WPIU          IFEQ      '0'
077200000000     C                   ADD       1             JJ
077300000000E   2C                   ENDIF
077400000000E   1C                   ENDDO
077500000000IF  1C     WPIU          IFEQ      '0'
077600000000     C                   MOVEL     '1'           $ERR
077700000000     C                   MOVEL     MSG(24)       $MSG
077800000000     C                   EXSR      CARMSG
077900000000X   1C                   ELSE
078000000000     C*
078100000000     C* CANCELLA GLI ORARI DEL GIORNO
078200000000     C     J             OCCUR     FGGG
078300000000     C                   CLEAR                   FGA
078400000000     C                   CLEAR                   FHA
078500000000     C                   CLEAR                   FGP
078600000000     C                   CLEAR                   FHP
078700000000     C*
078800000000     C* ANNULLA IL GIORNO (FRA QUELLI CARICATI IN MEMORIA)
078900000000     C                   MOVEL     'A'           GCA(J)
079000000000     C*
079100000000     C* CARICA A VIDEO UN ALTRO GIORNO
079200000000     C                   EXSR      REIMPO                                       *REIMPOSTA VARIABILI
079300000000IF  2C     JJ            IFEQ      8
079400000000     C                   MOVEL     *BLANKS       D00GSE
079500000000X   2C                   ELSE
079600000000     C                   MOVE      JJ            D00GSE
079700000000E   2C                   ENDIF
079800000000     C*
079900000000     C* MESSAGGIO DI ANNULLAMENTO EFFETTUATO
080000000000     C                   MOVEL     MSG(62)       ERRM62
080100000000     C                   MOVEL     DGES(J)       ERRGIO
080200000000     C                   MOVEL     ERRM62        $MSG
080300000000     C                   EXSR      CARMSG                                       *EMETTE MESSAGGIO
080400000000     C*
080500000000     C                   EXSR      CARVS                                        *CARICA IL VIDEO
080600000000E   1C                   ENDIF
080700000000     C*
080800000000     C                   ENDSR
080900000000     C*------------------------------------------------------------------------*
081000000000     C* F09GES - GESTIONE TASTO FUNZIONALE F09 ===> CAMBIA GIORNO
081100000000     C*------------------------------------------------------------------------*
081200000000     C     F09GES        BEGSR
081300000000     C*
081400000000     C                   MOVEL     'F09'         D00OP1                         *LIVELLO PROGRAMMA
081500000000     C                   MOVEL     '0'           $ERR
081600000000     C*
081700000000     C* CARICA IL GIORNO PRECEDNETE IN MEMORIA
081800000000     C                   EXSR      CARGIO
081900000000     C*
082000000000     C* ERRORE--> NON RIEMETTE IL SFILE, MA FA VEDERE L'ERRORE
082100951212IF  1C*---       $ERR      IFEQ '1'
082200951212     C*---                 Z-ADDWNBR      VCCNBR           *OUTPUT DI DECODI
082300951212     C*---       VCCNBR    CHAINTL04DVS              99
082400951212     C*---                 MOVEL'1'       *IN46
082500951212     C*---                 UPDATTL04DVS
082600951212E   1C*---                 ENDIF
082700000000     C*
082800000000     C* AZZERA IL CONTATORE SEQUENZA E LE SCHIERE DI LAVORO
082900000000     C                   EXSR      REIMPO
083000000000     C*
083100000000     C* LANCIA LA RICHIESTA DI UN GIORNO SUCESSIVO
083200000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
083300000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
083400000000     C                   Z-ADD     *ZEROS        WSEQ                           *SEQUENZA
083500000000     C                   MOVEL     *BLANKS       WTFM                           *TIPO FERMATA
083600000000     C                   Z-ADD     *ZEROS        WFIL                           *FILIALE FERMATA
083700000000     C                   MOVEL     *BLANKS       WGGA                           *GIORNO ARRIVO
083800000000     C                   Z-ADD     *ZEROS        WHHA                           *ORA ARRIVO
083900000000     C                   MOVEL     *BLANKS       WSCA                           *SCARICO IN ARRIVO
084000000000     C                   MOVEL     *BLANKS       WGGP                           *GIORNO PARTENZA
084100000000     C                   Z-ADD     *ZEROS        WHHP                           *ORA PARTENZA
084200000000     C                   MOVEL     *BLANKS       WCAR                           *CARICO IN PARTENZA
084300000000     C                   Z-ADD     *ZEROS        WKMT                           *CHILOMETRI FERMATA
084400000000     C                   Z-ADD     *ZEROS        WTEM                           *TEMPO FERMATA
084500000000     C                   MOVEL     *BLANKS       WGFV                           *GIORNO F.V.
084600000000     C*
084700951212     C* SE ERRORE, EMETTE UN MESSAGGIO DI AVVERTIMENTO DI PERDITA MODIFICHE
084800951212     C                   MOVEL     *BLANKS       WMSG                           *MESSAGGIO
084900951212IF  1C     $ERR          IFEQ      '1'
085000951212     C                   MOVEL     '0'           $ERR
085100951212     C                   MOVEL     MSG(63)       WMSG
085200951212E   1C                   ENDIF
085300000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
085400000000     C                   MOVEL     'F09'         WOP1                           *LIVELLO PROGRAMMA
085500000000     C                   MOVEL     'TNTL06R'     WPGM                           *PGM DI GESTIONE
085600000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
085700000000     C*
085800000000     C* AGGIORNA LE SCHIERE DI MEMORIZZAZIONE E RICARICA IL SUBFILE
085900000000IF  1C     D00F03        IFNE      '1'
086000000000IF  2C     $ERR          IFEQ      '0'
086100000000     C                   MOVEL     *BLANKS       MSGULT
086200000000     C                   MOVEL     *BLANKS       $MSG
086300000000X   2C                   ELSE
086400000000     C                   MOVEL     '0'           $ERR
086500000000     C                   EXSR      CARMSG
086600000000E   2C                   ENDIF
086700000000     C                   EXSR      CARVS                                        *RICARICA SUBFILE
086800000000E   1C                   ENDIF
086900000000     C*
087000000000     C                   ENDSR
087100000000     C*------------------------------------------------------------------------*
087200000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
087300000000     C*------------------------------------------------------------------------*
087400000000     C     F10GES        BEGSR
087500000000     C*
087600000000     C* LANCIA L'INSERIMENTO
087700000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
087800000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
087900000000     C*
088000000000     C                   Z-ADD     *ZEROS        WSEQ                           *SEQUENZA
088100000000     C                   MOVEL     *BLANKS       WTFM                           *TIPO FERMATA
088200000000     C                   Z-ADD     *ZEROS        WFIL                           *FILIALE FERMATA
088300000000     C                   MOVEL     *BLANKS       WGGA                           *GIORNO ARRIVO
088400000000     C                   Z-ADD     *ZEROS        WHHA                           *ORA ARRIVO
088500000000     C                   MOVEL     *BLANKS       WSCA                           *SCARICO IN ARRIVO
088600000000     C                   MOVEL     *BLANKS       WGGP                           *GIORNO PARTENZA
088700000000     C                   Z-ADD     *ZEROS        WHHP                           *ORA PARTENZA
088800000000     C                   MOVEL     *BLANKS       WCAR                           *CARICO IN PARTENZA
088900000000     C                   Z-ADD     *ZEROS        WKMT                           *CHILOMETRI FERMATA
089000000000     C                   Z-ADD     *ZEROS        WTEM                           *TEMPO FERMATA
089100000000     C                   MOVEL     *BLANKS       WGFV                           *GIORNO F.V.
089200000000     C*
089300000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
089400000000     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
089500000000     C                   MOVEL     'TNTL05R'     WPGM              8            *PGM DI GESTIONE
089600951212     C                   MOVEL     *BLANKS       WMSG                           *MESSAGGIO
089700000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
089800000000     C                   MOVEL     *BLANKS       VSCOPZ
089900000000     C*
090000000000     C* AGGIORNA LE SCHIERE DI MEMORIZZAZIONE E RICARICA IL SUBFILE
090100000000IF  1C     D00ERR        IFEQ      '0'
090200000000IF  2C     D00F03        IFNE      '1'
090300000000     C     D00F12        ANDNE     '1'
090400000000     C                   EXSR      AGGSCH                                       *AGGIORNA SCHIERE
090500000000E   2C                   ENDIF
090600000000E   1C                   ENDIF
090700000000     C* ERRORE
090800000000IF  1C     $ERR          IFEQ      '1'
090900000000     C                   EXSR      CARMSG
091000000000X   1C                   ELSE
091100000000     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
091200000000E   1C                   ENDIF
091300000000     C*
091400000000     C                   ENDSR
091500000000     C*------------------------------------------------------------------------*
091600000000     C* CARGIO - CARICA IL GIORNO IN MEMORIA
091700000000     C*------------------------------------------------------------------------*
091800000000     C     CARGIO        BEGSR
091900000000     C*
092000000000     C                   MOVEL     '0'           $ERR
092100000000IF  0C     $TGES         IFEQ      'G'                                          *SOLO IN GESTIONE
092200000000     C*
092300000000     C* PULISCE LE SCHIERE -LAVORO-
092400000000     C                   CLEAR                   LEQ
092500000000     C                   CLEAR                   LTT
092600000000     C                   CLEAR                   LIL
092700000000     C                   CLEAR                   LCA
092800000000     C                   CLEAR                   LAR
092900000000     C                   CLEAR                   LMT
093000000000     C                   CLEAR                   LEM
093100000000     C                   CLEAR                   LGA
093200000000     C                   CLEAR                   LHA
093300000000     C                   CLEAR                   LGP
093400000000     C                   CLEAR                   LHP
093500000000     C                   CLEAR                   LFV
093600000000     C                   Z-ADD     *ZEROS        WNUM16
093700000000     C*
093800000000     C* CARICA TUTTO IL SUBFILE NELLE SCHIERE DI LAVORO
093900000000     C                   Z-ADD     *ZEROS        J
094000000000     C                   Z-ADD     *ZEROS        VCCNBR
094100000000     C                   SETOFF                                       99
094200110525     c
094300000000DO  1C     *IN99         DOWEQ     *OFF
094400110525     C***  VSCSEQ        ANDGT     *ZEROS
094500110525     c     vsctfm        andne     ' '
094600000000     C                   ADD       1             VCCNBR
094700000000     C     VCCNBR        CHAIN     TL04DVS                            99
094800110525     c
094900000000IF  2C     *IN99         IFEQ      *OFF
095000110525     c     vsctfm        andne     'S'
095100000000     C                   ADD       1             J
095200000000     C     J             MULT      10            LEQ(J)
095300000000     C                   MOVEL     VSCTFM        LTT(J)
095400000000     C                   Z-ADD     VSCFIL        LIL(J)
095500000000     C                   MOVEL     VSCSCA        LCA(J)
095600000000     C                   MOVEL     VSCCAR        LAR(J)
095700000000     C                   Z-ADD     VSCKMT        LMT(J)
095800000000     C                   Z-ADD     VSCTEM        LEM(J)
095900000000     C                   MOVEL     VSCGFV        LFV(J)
096000000000     C                   MOVEL     VSCGGA        LGA(J)
096100000000     C                   Z-ADD     VSCHHA        LHA(J)
096200000000     C                   MOVEL     VSCGGP        LGP(J)
096300000000     C                   Z-ADD     VSCHHP        LHP(J)
096400000000E   2C                   ENDIF
096500000000E   1C                   ENDDO
096600000000     C*
096700000000     C* GIORNO CON FERMATE PIENE --> CONTROLLA E MEMORIZZA I DATI
096800000000     C* GIORNO CON FERMATE VUOTE --> DA ANNULLARE
096900000000     C                   XFOOT     LIL           WNUM16           16 0
097000000000     C                   XFOOT     LMT           WNU216           16 0
097100000000     C     WNUM16        ADD       WNU216        WNUM16
097200000000IF  1C     WNUM16        IFGT      *ZEROS
097300000000     C*
097400000000     C* CONTROLLA TUTTI I DATI
097500000000     C                   Z-ADD     *ZEROS        WNBR                           *RIGA ERRATA
097600000000     C                   EXSR      DECODI
097700000000E   1C                   ENDIF
097800000000     C*
097900000000     C* ERRORE    -> RIEMETTE IL SUBFILE
098000000000     C* NO ERRORE -> CARICA IL GIORNO IN MEMORIA
098100000000IF  1C     $ERR          IFEQ      '0'
098200000000IF  2C     WNUM16        IFGT      *ZEROS                                       *SE GG NON ANNULLATO
098300000000     C                   MOVEL     LTT           FTT
098400000000     C                   Z-ADD     LIL           FIL
098500000000     C                   MOVEL     LCA           FCA
098600000000     C                   MOVEL     LAR           FAR
098700000000     C                   Z-ADD     LMT           FMT
098800000000     C                   Z-ADD     LEM           FEM
098900000000     C                   MOVEL     LFV           FFV
099000000000IF  3C     D00GSE        IFEQ      *BLANKS
099100000000     C                   Z-ADD     8             J
099200000000X   3C                   ELSE
099300000000     C                   Z-ADD     *ZEROS        J
099400000000     C                   MOVE      D00GSE        J
099500000000E   3C                   ENDIF
099600000000     C     J             OCCUR     FGGG
099700000000     C                   MOVEL     LGA           FGA
099800000000     C                   Z-ADD     LHA           FHA
099900000000     C                   MOVEL     LGP           FGP
100000000000     C                   Z-ADD     LHP           FHP
100100000000     C                   Z-ADD     1             VCCNBR
100200000000     C*
100300000000     C* INSERISCE IL GIORNO FRA QUELLI CARICATI IN MEMORIA
100400000000IF  3C     D00GSE        IFEQ      *BLANKS
100500000000     C                   Z-ADD     8             J
100600000000X   3C                   ELSE
100700000000     C                   Z-ADD     *ZEROS        J
100800000000     C                   MOVE      D00GSE        J
100900000000E   3C                   ENDIF
101000000000     C                   MOVE      J             GCM(J)                         GIORNO CARICATO
101100000000     C*
101200000000E   2C                   ENDIF
101300000000E   1C                   ENDIF
101400000000     C*
101500000000E   0C                   ENDIF
101600000000     C*
101700000000     C                   ENDSR
101800000000     C*------------------------------------------------------------------------*
101900000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
102000000000     C*------------------------------------------------------------------------*
102100000000     C     DECODI        BEGSR
102200000000     C*?I CONTROLLI DI QUESTA ROUTINE SONO DUPLICATI NEL PROGRAMMA "TNTL50R" CHE
102300000000     C*?CONTROLLA LA CONGRUITA' DEL DATABASE TRAINI.
102400000000     C*?OGNI MODIFICA APPORTATA A QUESTA ROUTINE QUINDI, DEVE ESSERE RICOPIATA
102500000000     C*?DENTRO IL PROGRAMMA ONDE EVITARE DISALLINEAMENTI.
102600000000     C*
102700000000     C* REIMPOSTA LE VARIABILI DI LAVORO
102800000000     C                   MOVEL     '0'           $ERR
102900000000     C                   MOVEL     *BLANKS       $MSG
103000000000     C                   MOVEL     *BLANKS       MSGULT
103100000000     C*
103200000000     C* REIMPOSTA IL FLAG PER EMETTERE IL MESSAGGIO DI AVVERTIMENTO DELL' F12
103300000000     C                   MOVEL     '0'           MSGF12                         *EMETTE MESSAGGIO
103400000000     C*
103500000000IF  0C     $TGES         IFEQ      'G'                                          *SOLO IN GESTIONE
103600000000     C*
103700000000     C* CONTA QUANTE FERMATE O ORARI SONO STATE INSERITI
103800000000     C     *LIKE         DEFINE    SFLULT        WULT
103900000000     C                   Z-ADD     *ZEROS        WULT
104000000000DO  1C     1             DO        50            JJ
104100000000IF  2C     LIL(JJ)       IFEQ      *ZEROS
104200000000     C     LHA(JJ)       ANDEQ     *ZEROS
104300000000     C     LHP(JJ)       ANDEQ     *ZEROS
104400000000     C     JJ            SUB       1             WULT
104500000000     C                   LEAVE
104600000000E   2C                   ENDIF
104700000000E   1C                   ENDDO
104800000000     C***
104900000000     C* CONTROLLA I DATI IN GENERALE
105000000000     C***
105100000000IF  1C     D00OP1        IFNE      'O04'
105200000000IF  2C     $ERR          IFEQ      '0'
105300000000DO  3C     1             DO        WULT          JJ
105400000000     C                   Z-ADD     JJ            WNBR                           *RECORD SUBFILE
105500000000     C*
105600000000     C* FERMATE DI TIPO "A"=ANDATA O "R"=RITORNO
105700000000IF  4C     LTT(JJ)       IFNE      'A'
105800000000     C     LTT(JJ)       ANDNE     'R'
105900000000     C                   MOVEL     '1'           $ERR
106000000000     C                   MOVEL     MSG(16)       ERRM16
106100000000     C                   MOVEL     LEQ(JJ)       ERRS16
106200000000     C                   MOVEL     ERRM16        $MSG
106300000000     C                   LEAVE
106400000000E   4C                   ENDIF
106500000000     C*
106600000000     C* GIORNO APERTURA FOGLIO VIAGGIO A,B,C ...
106700000000     C* SOLO SE FERMATA DI CARICO, ALTRIMENTI NO
106800000000IF  4C     LAR(JJ)       IFEQ      'S'
106900000000     C                   SETOFF                                           28
107000000000     C     LFV(JJ)       LOOKUP    ALF                                    28
107100000000IF  5C     *IN28         IFEQ      *OFF
107200000000     C                   MOVEL     '1'           $ERR
107300000000     C                   MOVEL     MSG(29)       ERRM29
107400000000     C                   MOVEL     LEQ(JJ)       ERRS29
107500000000     C                   MOVEL     ERRM29        $MSG
107600000000     C                   LEAVE
107700000000E   5C                   ENDIF
107800000000     C*
107900951201     C* GIORNO APERTURA F.V. MAGGIORE  UGUALE DEL PRECEDENTE (TRANNE LA PRIMA)
108000000000     C                   Z-ADD     *ZEROS        JI                4 0
108100000000IF  5C     JJ            IFNE      1
108200000000     C     JJ            SUB       1             JI
108300000000IF  6C     LFV(JI)       IFGT      LFV(JJ)
108400000000     C                   MOVEL     '1'           $ERR
108500000000     C                   MOVEL     MSG(32)       ERRM32
108600000000     C                   MOVEL     LEQ(JJ)       ERRS32
108700000000     C                   MOVEL     ERRM32        $MSG
108800000000     C                   LEAVE
108900000000E   6C                   ENDIF
109000000000E   5C                   ENDIF
109100000000X   4C                   ELSE
109200000000IF  5C     LFV(JJ)       IFNE      *BLANKS
109300000000     C                   MOVEL     '1'           $ERR
109400000000     C                   MOVEL     MSG(60)       ERRM60
109500000000     C                   MOVEL     LEQ(JJ)       ERRS60
109600000000     C                   MOVEL     ERRM60        $MSG
109700000000     C                   LEAVE
109800000000E   5C                   ENDIF
109900000000E   4C                   ENDIF
110000000000     C*
110100000000     C* SE ARRIVO E PARTENZA PIENI, LA PARTENZA DEVE ESSERE > DELL'ARRIVO (G+HH)
110200000000IF  4C     LHA(JJ)       IFGT      *ZEROS
110300000000     C     LHP(JJ)       ANDGT     *ZEROS
110400000000IF  5C     LGA(JJ)       IFGT      LGP(JJ)                                      GG ARR > GG PAR   O
110500000000     C     LGA(JJ)       OREQ      LGP(JJ)                                      GG ARR = GG PAR  MA
110600000000     C     LHA(JJ)       ANDGE     LHP(JJ)                                      HH ARR >= GG PAR
110700000000     C                   MOVEL     '1'           $ERR
110800000000     C                   MOVEL     MSG(56)       ERRM56
110900000000     C                   MOVEL     LEQ(JJ)       ERRS56
111000000000     C                   MOVEL     ERRM56        $MSG
111100000000     C                   LEAVE
111200000000E   5C                   ENDIF
111300000000E   4C                   ENDIF
111400000000     C*
111500000000     C* SE INSERITA LA FERMATA, INSERIRE ANCHE QUALCHE DATO
111600000000IF  4C     LIL(JJ)       IFGT      *ZEROS
111700000000IF  5C     LHA(JJ)       IFEQ      *ZEROS
111800000000     C     LHP(JJ)       ANDEQ     *ZEROS
111900000000     C     LGA(JJ)       ANDEQ     *BLANKS
112000000000     C     LGP(JJ)       ANDEQ     *BLANKS
112100000000     C                   MOVEL     '1'           $ERR
112200000000     C                   MOVEL     MSG(57)       ERRM57
112300000000     C                   MOVEL     LEQ(JJ)       ERRS57
112400000000     C                   MOVEL     ERRM57        $MSG
112500000000     C                   LEAVE
112600000000E   5C                   ENDIF
112700000000E   4C                   ENDIF
112800000000     C*
112900000000     C* SE INSERITO L'ORARIO, INSERIRE ANCHE LA FERMATA
113000000000IF  4C     LIL(JJ)       IFEQ      *ZEROS
113100000000IF  5C     LHA(JJ)       IFGT      *ZEROS
113200000000     C     LHP(JJ)       ORGT      *ZEROS
113300000000     C     LGA(JJ)       ORGT      *BLANKS
113400000000     C     LGP(JJ)       ORGT      *BLANKS
113500000000     C                   MOVEL     '1'           $ERR
113600000000     C                   MOVEL     MSG(57)       ERRM57
113700000000     C                   MOVEL     LEQ(JJ)       ERRS57
113800000000     C                   MOVEL     ERRM57        $MSG
113900000000     C                   LEAVE
114000000000E   5C                   ENDIF
114100000000E   4C                   ENDIF
114200000000     C*
114300000000E   3C                   ENDDO
114400000000E   2C                   ENDIF
114500000000     C***
114600000000     C* PRIMA FERMATA
114700000000     C***
114800000000IF  2C     $ERR          IFEQ      '0'
114900000000DO  3C                   DO        1
115000000000     C                   Z-ADD     1             JJ                             *PRIMA TRATTA
115100000000     C                   Z-ADD     JJ            WNBR                           *RECORD SUBFILE
115200000000     C* TIPO FERMATA = 'A'
115300000000IF  4C     LTT(JJ)       IFNE      'A'
115400000000     C                   MOVEL     '1'           $ERR
115500000000     C                   MOVEL     MSG(9)        ERRM09
115600000000     C                   MOVEL     LEQ(JJ)       ERRS09
115700000000     C                   MOVEL     ERRM09        $MSG
115800000000     C                   LEAVE
115900000000E   4C                   ENDIF
116000000000     C* DATI ARRIVO, KILOMETRI E TEMPO VUOTI
116100000000IF  4C     LGA(JJ)       IFNE      *BLANKS
116200000000     C     LHA(JJ)       ORNE      *ZEROS
116300000000     C     LCA(JJ)       ORNE      *BLANKS
116400000000     C     LMT(JJ)       ORNE      *ZEROS
116500000000     C     LEM(JJ)       ORNE      *ZEROS
116600000000     C                   MOVEL     '1'           $ERR
116700000000     C                   MOVEL     MSG(13)       ERRM13
116800000000     C                   MOVEL     LEQ(JJ)       ERRS13
116900000000     C                   MOVEL     ERRM13        $MSG
117000000000     C                   LEAVE
117100000000E   4C                   ENDIF
117200000000     C* DATI PARTENZA PIENI
117300000000IF  4C     LGP(JJ)       IFEQ      *BLANKS
117400000000     C     LHP(JJ)       OREQ      *ZEROS
117500000000     C     LAR(JJ)       OREQ      *BLANKS
117600000000     C                   MOVEL     '1'           $ERR
117700000000     C                   MOVEL     MSG(33)       ERRM33
117800000000     C                   MOVEL     LEQ(JJ)       ERRS33
117900000000     C                   MOVEL     ERRM33        $MSG
118000000000     C                   LEAVE
118100000000E   4C                   ENDIF
118200000000     C* GIORNO APERTURA F.V.='A'
118300000000IF  4C     LFV(JJ)       IFNE      'A'
118400000000     C                   MOVEL     '1'           $ERR
118500000000     C                   MOVEL     MSG(31)       ERRM31
118600000000     C                   MOVEL     LEQ(JJ)       ERRS31
118700000000     C                   MOVEL     ERRM31        $MSG
118800000000     C                   LEAVE
118900000000E   4C                   ENDIF
119000000000     C* FILIALE = TFP DEL TRAINO
119100000000IF  4C     LIL(JJ)       IFNE      TLTTFP
119200000000     C                   MOVEL     '1'           $ERR
119300000000     C                   MOVEL     MSG(10)       ERRM10
119400000000     C                   MOVEL     LEQ(JJ)       ERRS10
119500000000     C                   MOVEL     ERRM10        $MSG
119600000000     C                   LEAVE
119700000000E   4C                   ENDIF
119800000000     C***
119900000000     C* ULTIMA FERMATA
120000000000     C***
120100000000     C                   Z-ADD     WULT          JJ                             *ULRIMA TRATTA
120200000000     C                   Z-ADD     JJ            WNBR                           *RECORD SUBFILE
120300000000     C* DATI PARTENZA VUOTI
120400000000IF  4C     LGP(JJ)       IFNE      *BLANKS
120500000000     C     LHP(JJ)       ORNE      *ZEROS
120600000000     C     LAR(JJ)       ORNE      *BLANKS
120700000000     C                   MOVEL     '1'           $ERR
120800000000     C                   MOVEL     MSG(14)       ERRM14
120900000000     C                   MOVEL     LEQ(JJ)       ERRS14
121000000000     C                   MOVEL     ERRM14        $MSG
121100000000     C                   LEAVE
121200000000E   4C                   ENDIF
121300000000     C* DATI ARRIVO PIENI
121400000000IF  4C     LGA(JJ)       IFEQ      *BLANKS
121500000000     C     LHA(JJ)       OREQ      *ZEROS
121600000000     C     LCA(JJ)       OREQ      *BLANKS
121700000000     C                   MOVEL     '1'           $ERR
121800000000     C                   MOVEL     MSG(34)       ERRM34
121900000000     C                   MOVEL     LEQ(JJ)       ERRS34
122000000000     C                   MOVEL     ERRM34        $MSG
122100000000     C                   LEAVE
122200000000E   4C                   ENDIF
122300000000     C*
122400000000     C* TRAINO CHIUSO TIPO FERMATA = 'R'
122500000000     C*    "      "   FILIALE ARRIVO = TFP DEL TRAINO
122600000000IF  4C     TLTTTR        IFEQ      'C'                                          *CHIUSO
122700000000IF  5C     LTT(JJ)       IFNE      'R'
122800000000     C                   MOVEL     '1'           $ERR
122900000000     C                   MOVEL     MSG(8)        ERRM08
123000000000     C                   MOVEL     LEQ(JJ)       ERRS08
123100000000     C                   MOVEL     ERRM08        $MSG
123200000000     C                   LEAVE
123300000000E   5C                   ENDIF
123400000000IF  5C     LIL(JJ)       IFNE      TLTTFP
123500000000     C                   MOVEL     '1'           $ERR
123600000000     C                   MOVEL     MSG(7)        ERRM07
123700000000     C                   MOVEL     LEQ(JJ)       ERRS07
123800000000     C                   MOVEL     ERRM07        $MSG
123900000000     C                   LEAVE
124000000000E   5C                   ENDIF
124100000000X   4C                   ELSE
124200000000     C* TRAINO NON CHIUSO TIPO FERMATA = 'A'
124300000000     C*    "    "    "    FILIALE ARRIVO = TFA DEL TRAINO
124400000000IF  5C     LTT(JJ)       IFNE      'A'
124500000000     C                   MOVEL     '1'           $ERR
124600000000     C                   MOVEL     MSG(35)       ERRM35
124700000000     C                   MOVEL     LEQ(JJ)       ERRS35
124800000000     C                   MOVEL     ERRM35        $MSG
124900000000     C                   LEAVE
125000000000E   5C                   ENDIF
125100000000IF  5C     LIL(JJ)       IFNE      TLTTFA
125200000000     C                   MOVEL     '1'           $ERR
125300000000     C                   MOVEL     MSG(11)       ERRM11
125400000000     C                   MOVEL     LEQ(JJ)       ERRS11
125500000000     C                   MOVEL     ERRM11        $MSG
125600000000     C                   LEAVE
125700000000E   5C                   ENDIF
125800000000E   4C                   ENDIF
125900000000     C*
126000000000E   3C                   ENDDO
126100000000E   2C                   ENDIF
126200000000     C***
126300000000     C* CONTROLLA LE FILIALI E IL TIPO FERMATA
126400000000     C***
126500000000IF  2C     $ERR          IFEQ      '0'
126600000000     C                   MOVEL     '0'           WSCHIA            1            FERM 'A'TR CHIUSO
126700000000     C                   MOVEL     '0'           WSCHIR            1            FERM 'R'TR CHIUSO
126800000000     C                   MOVEL     '0'           WNCHIA            1            FERM 'A'TR NON CHIUS
126900000000     C                   MOVEL     '0'           WNCHIR            1            FERM 'R'TR NON CHIUS
127000000000     C                   Z-ADD     *ZEROS        F                 4 0
127100000000     C                   CLEAR                   JFT
127200000000     C                   Z-ADD     *ZEROS        JI
127300000000     C* TIPO
127400000000DO  3C     1             DO        WULT          JJ
127500000000     C                   Z-ADD     JJ            WNBR                           *RECORD SUBFILE
127600000000IF  4C     TLTTTR        IFEQ      'C'                                          *C H I U S O
127700000000IF  5C     LTT(JJ)       IFEQ      'R'
127800000000     C                   MOVEL     '1'           WSCHIR                         *CI SONO FERM RITORN
127900000000E   5C                   ENDIF
128000000000IF  5C     LTT(JJ)       IFEQ      'A'
128100000000     C                   MOVEL     '1'           WSCHIA                         *CI SONO FERM ANDATA
128200000000E   4C                   ENDIF
128300000000X   4C                   ELSE                                                   *N O N   C H I U S O
128400000000IF  5C     LTT(JJ)       IFEQ      'R'
128500000000     C                   MOVEL     '1'           WNCHIR                         *CI SONO FERM RITORN
128600000000E   5C                   ENDIF
128700000000IF  5C     LTT(JJ)       IFEQ      'A'
128800000000     C                   MOVEL     '1'           WNCHIA                         *CI SONO FERM ANDATA
128900000000E   5C                   ENDIF
129000000000E   4C                   ENDIF
129100000000     C*
129200951213     C* LA STESSA FERMATA (FIL+TIPO) NON PUO' ESSERE RIPETUTA, TRANNE NEI LOCALI
129300960423     C* O NEI TRAINI INOLTRO DI TIPO LOCALE
129400960423IF  4C     TLTTTR        IFNE      'L'                                          *NO LOCALE
129500960423     C     D00INL        ANDNE     'L'                                          *NO INOLTRO LOCALE
129600000000     C                   MOVEL     LIL(JJ)       ALF4              4            *FIL + A/R
129700000000     C                   MOVE      LTT(JJ)       ALF4
129800000000     C                   SETOFF                                       28
129900000000     C     ALF4          LOOKUP    JFT                                    28
130000951213IF  5C     *IN28         IFEQ      *ON                                          *TROVATO
130100000000     C                   MOVEL     '1'           $ERR
130200000000     C                   MOVEL     MSG(26)       ERRM26
130300000000     C                   MOVEL     LEQ(JJ)       ERRS26
130400000000     C                   MOVEL     ERRM26        $MSG
130500000000     C                   LEAVE
130600951213X   5C                   ELSE
130700000000     C                   ADD       1             F
130800000000     C                   MOVEL     ALF4          JFT(F)
130900951213E   5C                   ENDIF
131000951213E   4C                   ENDIF
131100000000     C*
131200000000     C* SE TRAINO CHIUSO LE FERMATE RITORNO DEVONO SEGUIRE LE ANDATA
131300000000     C* "     "      "   LE FERMATE ANDATA DEVONO PRECEDERE LE RITORNO
131400000000IF  4C     JJ            IFNE      1                                            *NO PRIMA FERMATA
131500000000     C     JJ            ANDNE     WULT                                         *NO ULTIMA FERMATA
131600000000IF  5C     LTT(JJ)       IFEQ      'A'
131700000000     C     JJ            SUB       1             JI
131800000000IF  6C     LTT(JI)       IFEQ      'R'
131900000000     C                   MOVEL     '1'           $ERR
132000000000     C                   MOVEL     MSG(38)       ERRM38
132100000000     C                   MOVEL     LEQ(JJ)       ERRS38
132200000000     C                   MOVEL     ERRM38        $MSG
132300000000     C                   LEAVE
132400000000E   6C                   ENDIF
132500000000X   5C                   ELSE
132600000000     C     JJ            ADD       1             JI
132700000000IF  6C     LTT(JI)       IFEQ      'A'
132800000000     C                   MOVEL     '1'           $ERR
132900000000     C                   MOVEL     MSG(40)       ERRM40
133000000000     C                   MOVEL     LEQ(JJ)       ERRS40
133100000000     C                   MOVEL     ERRM40        $MSG
133200000000     C                   LEAVE
133300000000E   6C                   ENDIF
133400000000E   5C                   ENDIF
133500000000E   4C                   ENDIF
133600000000     C*
133700000000E   3C                   ENDDO
133800000000     C*
133900000000     C* SE TRAINO CHIUSO, CI DEVONO ESSERE FERMATE DI ANDATA E RITORNO
134000000000     C* SE TRAINO NON CHIUSO, SI DEVONO ESSERE SOLO FERMATE DI ANDATA
134100000000IF  3C     $ERR          IFEQ      '0'
134200000000IF  4C     TLTTTR        IFEQ      'C'                                          C H I U S O
134300000000IF  5C     WSCHIR        IFEQ      '1'
134400000000     C     WSCHIA        ANDEQ     '1'
134500000000X   5C                   ELSE
134600000000     C                   MOVEL     '1'           $ERR
134700000000     C                   MOVEL     MSG(12)       $MSG
134800000000E   5C                   ENDIF
134900000000X   4C                   ELSE                                                   N O N  CHIUSO
135000000000IF  5C     WNCHIR        IFEQ      '1'
135100000000     C     WNCHIA        OREQ      '0'
135200000000     C                   MOVEL     '1'           $ERR
135300000000     C                   MOVEL     MSG(17)       $MSG
135400000000E   5C                   ENDIF
135500000000E   4C                   ENDIF
135600000000E   3C                   ENDIF
135700000000     C*
135800000000E   2C                   ENDIF
135900000000     C***
136000000000     C* ALTRE FERMATE
136100000000     C***
136200000000IF  2C     $ERR          IFEQ      '0'
136300000000     C                   Z-ADD     *ZEROS        JI
136400000000DO  3C     1             DO        WULT          JJ
136500000000     C                   Z-ADD     JJ            WNBR
136600090115     C* DATI FILIALE
136700090115     C                   Z-ADD     LIL(JJ)       WORG
136800090115     C                   Z-ADD     *ZEROS        WFEL
136900090115     C                   Z-ADD     *ZEROS        WTFP
137000090115     C                   Z-ADD     *ZEROS        WTFA
137100090115     C                   MOVEL     *BLANKS       WPOS
137200090115     C                   clear                   Wntw
137300090115     C                   EXSR      CHKFIL
137400090115     C                   Z-ADD     WFEL          DEPFEL
137500090115     C                   Z-ADD     WTFP          DEPTFP
137600090115     C                   Z-ADD     WTFA          DEPTFA
137700090115     C                   MOVEL     WPOS          DEPPOS
137800090115     C* network: memorizzo se estero o italia
137900090115     c                   clear                   depest            1
138000090115     c                   if        wntw='DPD' or wntw='FED' or wntw='EEX'
138100090115     c                   eval      depest='S'
138200090115     c                   endif
138300090115     C*
138400000000IF  4C     JJ            IFNE      1                                            *NO PRIMA
138500000000     C     JJ            ANDNE     WULT                                         *NO ULTIMA
138600000000     C* A F F L U E N Z A
138700000000IF  5C     TLTTTR        IFEQ      'A'
138800971128IF  6C     LIL(JJ)       IFNE      DEPFEL                                       *NO FIL ELAB
138900000000IF  7C     DEPTFP        IFEQ      TLTTFA                                       *TFP = TFA DEL TRAIN
139000000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTE
139100000000IF  8C     LAR(JJ)       IFEQ      'S'                                          *SI CARICO  e
139200000000     C     LCA(JJ)       ANDEQ     *BLANKS                                      *NO SCARICO
139300000000X   8C                   ELSE
139400000000     C                   MOVEL     '1'           $ERR
139500000000     C                   MOVEL     MSG(43)       ERRM43
139600000000     C                   MOVEL     LEQ(JJ)       ERRS43
139700000000     C                   MOVEL     ERRM43        $MSG
139800000000     C                   LEAVE
139900000000E   8C                   ENDIF
140000000000X   7C                   ELSE
140100000000     C                   MOVEL     '1'           $ERR
140200000000     C                   MOVEL     MSG(42)       ERRM42
140300000000     C                   MOVEL     LEQ(JJ)       ERRS42
140400000000     C                   MOVEL     ERRM42        $MSG
140500000000     C                   LEAVE
140600000000E   7C                   ENDIF
140700000000X   6C                   ELSE
140800000000     C                   MOVEL     '1'           $ERR
140900000000     C                   MOVEL     MSG(41)       ERRM41
141000000000     C                   MOVEL     LEQ(JJ)       ERRS41
141100000000     C                   MOVEL     ERRM41        $MSG
141200000000     C                   LEAVE
141300000000E   6C                   ENDIF
141400000000E   5C                   ENDIF
141500000000     C* D E F L U E N Z A
141600000000     C* L O C A L E
141700960423     C* I N O L T R O  -DEFLUENZA O LOCALE-
141800960423IF  5C     DEPDEF        IFEQ      'S'                                          *TIPO DEFLUENZA
141900960423     C     TLTTTR        OREQ      'L'                                          *LOCALE
142000960423     C     D00INL        OREQ      'D'                                          *INOLTRO DEFLUENZA
142100960423     C     D00INL        OREQ      'L'                                          *INOLTRO LOCALE
142200000000IF  6C     LCA(JJ)       IFEQ      'S'                                          *SI SCARICO  e
142300000000     C     LAR(JJ)       ANDEQ     *BLANKS                                      *NO CARICO
142400000000     C* ECCEZIONI
142500000000     C                   MOVEL     '0'           WECC              1
142600000000     C                   EXSR      CTRECC
142700000000IF  7C     DEPTFA        IFEQ      TLTTFP                                       *TERM ARR=TFP TRAINO
142800000000     C     WECC          OREQ      '1'                                          *TERM ARR ECC=TFP TR
142900000000IF  8C     LIL(JJ)       IFNE      TLTTFP                                       *FIL <> TFP TRAINO
143000000000X   8C                   ELSE
143100000000     C                   MOVEL     '1'           $ERR
143200000000     C                   MOVEL     MSG(47)       ERRM47
143300000000     C                   MOVEL     LEQ(JJ)       ERRS47
143400000000     C                   MOVEL     ERRM47        $MSG
143500000000     C                   LEAVE
143600000000E   8C                   ENDIF
143700000000X   7C                   ELSE
143800000000     C                   MOVEL     '1'           $ERR
143900000000     C                   MOVEL     MSG(46)       ERRM46
144000000000     C                   MOVEL     LEQ(JJ)       ERRS46
144100000000     C                   MOVEL     ERRM46        $MSG
144200000000     C                   LEAVE
144300000000E   7C                   ENDIF
144400000000X   6C                   ELSE
144500000000     C                   MOVEL     '1'           $ERR
144600000000     C                   MOVEL     MSG(45)       ERRM45
144700000000     C                   MOVEL     LEQ(JJ)       ERRS45
144800000000     C                   MOVEL     ERRM45        $MSG
144900000000     C                   LEAVE
145000000000E   6C                   ENDIF
145100000000E   5C                   ENDIF
145200000000     C* SOLO  A N D A T A
145300000000     C* C H I U S O
145400960423     C* I N O L T R O  -SOLO ANDATA-
145500960715     C*---
145600960715IF  5C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
145700960715     C*---       TLTTTR    OREQ 'C'                        *CHIUSO
145800960715     C*---       D00INL    OREQ 'S'                        *INOLTRO SOLO ANDATA
145900960715     C*-ECCEZIONI
146000960715     C*---                 MOVEL'0'       WECC
146100960715     C*---                 EXSR CTREC2
146200960715IF  6C*---       LAR,JJ    IFEQ 'S'                        SE FERMATA DI CARICO
146300971128     C*---       LIL,JJ    ANDNEDEPFEL                     *NO FIL ELABORATORE
146400960715     C*---                 MOVEL'1'       $ERR
146500960715     C*---                 MOVELMSG,44    ERRM44
146600960715     C*---                 MOVELLEQ,JJ    ERRS44
146700960715     C*---                 MOVELERRM44    $MSG
146800960715     C*---                 LEAVE
146900960715E   6C*---                 ENDIF
147000960715IF  6C*---       LCA,JJ    IFEQ 'S'                        SE FERMATA DI SCARICO
147100960715     C*---       LIL,JJ    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
147200960715     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
147300960715     C*---                 MOVEL'1'       $ERR
147400960715     C*---                 MOVELMSG,48    ERRM48
147500960715     C*---                 MOVELLEQ,JJ    ERRS48
147600960715     C*---                 MOVELERRM48    $MSG
147700960715     C*---                 LEAVE
147800960715E   6C*---                 ENDIF
147900960715E   5C*---                 ENDIF
148000960715E   4C*---                 ENDIF                           *NO PRIMA E NO ULTIMA
148100960719     C*
148200960715IF  5C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
148300960715     C     TLTTTR        OREQ      'C'                                          *CHIUSO
148400960715     C     D00INL        OREQ      'S'                                          *INOLTRO SOLO ANDATA
148500960717     C* ECCEZIONI
148600960715     C                   MOVEL     '0'           WECC
148700960715     C                   EXSR      CTREC2
148800980610     C*---CASO PARTICOLARE
148900980610IF  6C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
149000980610     C*---       LIL,JJ    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
149100980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
149200980610IF  7C*---       LCA,JJ    IFEQ 'S'                        *FERMATA DI SCARICO
149300980610     C*---       LAR,JJ    ANDNE'S'                        *NO FERMATA DI CARICO
149400980610X   7C*---                 ELSE
149500980610     C*---                 MOVEL'1'       $ERR
149600980610     C*---                 MOVELMSG,65    ERRM65
149700980610     C*---                 MOVELLEQ,JJ    ERRS65
149800980610     C*---                 MOVELERRM65    $MSG
149900980610     C*---                 LEAVE
150000980610E   7C*---                 ENDIF
150100980610X   6C*---                 ELSE                            *CASO NORMALE
150200960717     C*
150300000519IF  7C     LAR(JJ)       IFEQ      'S'                                          *SI CARICO
150400000519IF  8C     LIL(JJ)       IFEQ      DEPFEL                                       *SI FIL ELABORATORE
150500000519     C     DEPPOS        OREQ      'S'                                          *FILIALE POSTE
150600000519X   8C                   ELSE
150700960715     C                   MOVEL     '1'           $ERR
150800960715     C                   MOVEL     MSG(44)       ERRM44
150900960715     C                   MOVEL     LEQ(JJ)       ERRS44
151000960715     C                   MOVEL     ERRM44        $MSG
151100960715     C                   LEAVE
151200960717E   8C                   ENDIF
151300960717E   7C                   ENDIF
151400980610     C*---
151500980610IF  7C*---       LCA,JJ    IFEQ 'S'                        SE FERMATA DI SCARICO
151600980610     C*---       LIL,JJ    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
151700980610     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
151800980610     C*---                 MOVEL'1'       $ERR
151900980610     C*---                 MOVELMSG,48    ERRM48
152000980610     C*---                 MOVELLEQ,JJ    ERRS48
152100980610     C*---                 MOVELERRM48    $MSG
152200980610     C*---                 LEAVE
152300980610E   7C*---                 ENDIF
152400980610E   6C*---                 ENDIF
152500960717E   5C                   ENDIF
152600960717E   4C                   ENDIF                                                  *NO PRIMA E NO ULTIM
152700000000     C* C H I U S O
152800000000IF  4C     TLTTTR        IFEQ      'C'
152900000000     C* L'ULTIMA FERMATA DI ANDATA  DEVE ESSERE = TFA DEL TRAINO
153000000000     C* E DI SOLO SCARICO
153100000000IF  5C     LTT(JJ)       IFEQ      'A'
153200000000     C     JJ            ADD       1             JI                             *FERMATA SUCESSIVA
153300000000IF  6C     LTT(JI)       IFEQ      'R'                                          *RITORNO
153400000000IF  7C     LIL(JJ)       IFNE      TLTTFA
153500000000     C                   MOVEL     '1'           $ERR
153600000000     C                   MOVEL     MSG(49)       ERRM49
153700000000     C                   MOVEL     LEQ(JJ)       ERRS49
153800000000     C                   MOVEL     ERRM49        $MSG
153900000000     C                   LEAVE
154000000000E   7C                   ENDIF
154100000000IF  7C     LCA(JJ)       IFEQ      *BLANKS
154200000000     C                   MOVEL     '1'           $ERR
154300000000     C                   MOVEL     MSG(54)       ERRM54
154400000000     C                   MOVEL     LEQ(JJ)       ERRS54
154500000000     C                   MOVEL     ERRM54        $MSG
154600000000     C                   LEAVE
154700000000E   7C                   ENDIF
154800000000     C* E I DATI PARTENZA DEVONO ESSERE VUOTI
154900000000IF  7C     LGP(JJ)       IFNE      *BLANKS
155000000000     C     LHP(JJ)       ORNE      *ZEROS
155100000000     C     LAR(JJ)       ORNE      *BLANKS
155200000000     C                   MOVEL     '1'           $ERR
155300000000     C                   MOVEL     MSG(51)       ERRM51
155400000000     C                   MOVEL     LEQ(JJ)       ERRS51
155500000000     C                   MOVEL     ERRM51        $MSG
155600000000     C                   LEAVE
155700000000E   7C                   ENDIF
155800000000E   6C                   ENDIF
155900000000     C* LA PRIMA FERMATA DI RITORNO DEVE ESSERE = TFA DEL TRAINO
156000000000     C* E DI SOLO CARICO
156100000000X   5C                   ELSE
156200000000     C     JJ            SUB       1             JI                             *FERMATA PRECEDENTE
156300000000IF  6C     LTT(JI)       IFEQ      'A'                                          *ANDATA
156400000000IF  7C     LIL(JJ)       IFNE      TLTTFA
156500000000     C                   MOVEL     '1'           $ERR
156600000000     C                   MOVEL     MSG(50)       ERRM50
156700000000     C                   MOVEL     LEQ(JJ)       ERRS50
156800000000     C                   MOVEL     ERRM50        $MSG
156900000000     C                   LEAVE
157000000000E   7C                   ENDIF
157100000000IF  7C     LAR(JJ)       IFEQ      *BLANKS
157200000000     C                   MOVEL     '1'           $ERR
157300000000     C                   MOVEL     MSG(55)       ERRM55
157400000000     C                   MOVEL     LEQ(JJ)       ERRS55
157500000000     C                   MOVEL     ERRM55        $MSG
157600000000     C                   LEAVE
157700000000E   7C                   ENDIF
157800000000     C* E I DATI ARRIVO DEVONO ESSERE VUOTI
157900000000IF  7C     LGA(JJ)       IFNE      *BLANKS
158000000000     C     LHA(JJ)       ORNE      *ZEROS
158100000000     C     LCA(JJ)       ORNE      *BLANKS
158200000000     C                   MOVEL     '1'           $ERR
158300000000     C                   MOVEL     MSG(52)       ERRM52
158400000000     C                   MOVEL     LEQ(JJ)       ERRS52
158500000000     C                   MOVEL     ERRM52        $MSG
158600000000     C                   LEAVE
158700000000E   7C                   ENDIF
158800000000E   6C                   ENDIF
158900000000E   5C                   ENDIF
159000000000E   4C                   ENDIF
159100090114     C* C H I L O M E T R I - Solo se traino che paga la sede
159200090114     c*                       e solo se premuto F6
159300000000     C*
159400090508IF  4c***                if        deputc='S' and key=F6
159500090115     c* prima fermata: memorizzo la filiale
159600090508IF  5c***                if        jj=1
159700090508     c***                z-add     lil(jj)       wfilp
159800090508     c***                movel     depest        west
159900090508X   5c***                else
160000090115     c* dalla seconda fermata in poi verifico km mediante driver di ricerca:
160100090115     c* solo se km 0 (altrimenti non posso andare in sostituzione)
160200090115     c* se tratta da filiale estera a filiale estera non devo fare nulla
160300090508     c***                clear                   ficna2ds
160400090508IF  6c***                if        lmt(jj)=0  and (west=' ' or depest=' ')
160500090508     c***                z-add     wfilp         ia2lnp
160600090508     c***                z-add     lil(jj)       ia2lna
160700090508     c***                call      'FICNA2R1'
160800090508     c***                parm                    ficna2ds
160900090114     c* errore: km non trovati
161000090508IF  7c***                if        oa2error='1'
161100090508     C***                MOVEL     '1'           $ERR
161200090508     C***                MOVEL     MSG(66)       ERRM66
161300090508     C***                MOVEL     LEQ(JJ)       ERRS66
161400090508     C***                MOVEL     ERRM66        $MSG
161500090508     c***                leave
161600090508X   7c***                else
161700090508     c***                z-add     oa2km         lmt(jj)
161800090508E   7c***                endif
161900090508E   6c***                endif
162000090508     c***                z-add     lil(jj)       wfilp
162100090508     c***                movel     depest        west
162200090508E   5c***                endif
162300090508E   4c***                endif
162400090114     c*
162500000000E   3C                   ENDDO
162600000000E   2C                   ENDIF
162700000000     C***
162800000000     C* CONTROLLA GLI ORARI
162900000000     C***
163000000000IF  2C     $ERR          IFEQ      '0'
163100000000DO  3C     1             DO        WULT          JJ
163200000000     C                   Z-ADD     JJ            WNBR                           *RECORD SUBFILE
163300000000     C* A R R I V O
163400000000     C* PROGRESSIVO GIORNO A,B,C ...
163500000000IF  4C     LGA(JJ)       IFNE      *BLANKS
163600000000     C     LGA(JJ)       LOOKUP    ALF                                    28
163700000000IF  5C     *IN28         IFEQ      *OFF
163800000000     C                   MOVEL     '1'           $ERR
163900000000     C                   MOVEL     MSG(18)       ERRM18
164000000000     C                   MOVEL     LEQ(JJ)       ERRS18
164100000000     C                   MOVEL     ERRM18        $MSG
164200000000     C                   LEAVE
164300000000E   5C                   ENDIF
164400000000E   4C                   ENDIF
164500000000     C* ORA IN HH:MM
164600000000IF  4C     LHA(JJ)       IFNE      *ZEROS
164700000000     C                   Z-ADD     LHA(JJ)       WORA
164800000000     C                   EXSR      CHKORA
164900000000IF  5C     $ERR          IFEQ      '1'
165000000000     C                   MOVEL     MSG(20)       ERRM20
165100000000     C                   MOVEL     LEQ(JJ)       ERRS20
165200000000     C                   MOVEL     ERRM20        $MSG
165300000000     C                   LEAVE
165400000000E   5C                   ENDIF
165500000000E   4C                   ENDIF
165600000000     C* SCARICO
165700000000IF  4C     LCA(JJ)       IFNE      *BLANKS
165800000000IF  5C     LCA(JJ)       IFNE      'S'
165900000000     C                   MOVEL     '1'           $ERR
166000000000     C                   MOVEL     MSG(22)       ERRM22
166100000000     C                   MOVEL     LEQ(JJ)       ERRS22
166200000000     C                   MOVEL     ERRM22        $MSG
166300000000     C                   LEAVE
166400000000E   5C                   ENDIF
166500000000E   4C                   ENDIF
166600000000     C* SE SCARICO='S' ANCHE GG E ORA ARRIVO PIENI
166700000000IF  4C     LCA(JJ)       IFNE      *BLANKS
166800000000IF  5C     LGA(JJ)       IFEQ      *BLANKS
166900000000     C     LHA(JJ)       OREQ      *ZEROS
167000000000     C                   MOVEL     '1'           $ERR
167100000000     C                   MOVEL     MSG(28)       ERRM28
167200000000     C                   MOVEL     LEQ(JJ)       ERRS28
167300000000     C                   MOVEL     ERRM28        $MSG
167400000000     C                   LEAVE
167500000000E   5C                   ENDIF
167600000000E   4C                   ENDIF
167700000000     C* SE GG O ORA ARRIVO PIENI ANCHE LO SCARICO = 'S'
167800000000IF  5C     LGA(JJ)       IFNE      *BLANKS
167900000000     C     LHA(JJ)       ORGT      *ZEROS
168000000000IF  4C     LCA(JJ)       IFEQ      *BLANKS
168100000000     C                   MOVEL     '1'           $ERR
168200000000     C                   MOVEL     MSG(28)       ERRM28
168300000000     C                   MOVEL     LEQ(JJ)       ERRS28
168400000000     C                   MOVEL     ERRM28        $MSG
168500000000     C                   LEAVE
168600000000E   5C                   ENDIF
168700000000E   4C                   ENDIF
168800000000     C*
168900000000     C* P A R T E N Z A
169000000000     C* PROGRESSIVO GIORNO A,B,C ...
169100000000IF  4C     LGP(JJ)       IFNE      *BLANKS
169200000000     C                   SETOFF                                           28
169300000000     C     LGP(JJ)       LOOKUP    ALF                                    28
169400000000IF  5C     *IN28         IFEQ      *OFF
169500000000     C                   MOVEL     '1'           $ERR
169600000000     C                   MOVEL     MSG(19)       ERRM19
169700000000     C                   MOVEL     LEQ(JJ)       ERRS19
169800000000     C                   MOVEL     ERRM19        $MSG
169900000000     C                   LEAVE
170000000000E   5C                   ENDIF
170100000000E   4C                   ENDIF
170200000000     C* PROGRESSIVO GIORNO MAGGIORE/UGUALE GIORNO APERTURA FOGLIO VIAGGIO
170300000000IF  4C     LGP(JJ)       IFNE      *BLANKS
170400000000IF  5C     LGP(JJ)       IFLT      LFV(JJ)
170500000000     C                   MOVEL     '1'           $ERR
170600000000     C                   MOVEL     MSG(30)       ERRM30
170700000000     C                   MOVEL     LEQ(JJ)       ERRS30
170800000000     C                   MOVEL     ERRM30        $MSG
170900000000     C                   LEAVE
171000000000E   5C                   ENDIF
171100000000E   4C                   ENDIF
171200000000     C* ORA IN HH:MM
171300000000IF  4C     LHP(JJ)       IFNE      *ZEROS
171400000000     C                   Z-ADD     LHP(JJ)       WORA
171500000000     C                   EXSR      CHKORA
171600000000IF  5C     $ERR          IFEQ      '1'
171700000000     C                   MOVEL     MSG(21)       ERRM21
171800000000     C                   MOVEL     LEQ(JJ)       ERRS21
171900000000     C                   MOVEL     ERRM21        $MSG
172000000000     C                   LEAVE
172100000000E   5C                   ENDIF
172200000000E   4C                   ENDIF
172300000000     C* CARICO
172400000000IF  4C     LAR(JJ)       IFNE      *BLANKS
172500000000IF  5C     LAR(JJ)       IFNE      'S'
172600000000     C                   MOVEL     '1'           $ERR
172700000000     C                   MOVEL     MSG(23)       ERRM23
172800000000     C                   MOVEL     LEQ(JJ)       ERRS23
172900000000     C                   MOVEL     ERRM23        $MSG
173000000000     C                   LEAVE
173100000000E   5C                   ENDIF
173200000000E   4C                   ENDIF
173300000000     C* SE CARICO='S' ANCHE GG E ORA PARTENZA PIENI
173400000000IF  4C     LAR(JJ)       IFNE      *BLANKS
173500000000IF  5C     LGP(JJ)       IFEQ      *BLANKS
173600000000     C     LHP(JJ)       OREQ      *ZEROS
173700000000     C                   MOVEL     '1'           $ERR
173800000000     C                   MOVEL     MSG(27)       ERRM27
173900000000     C                   MOVEL     LEQ(JJ)       ERRS27
174000000000     C                   MOVEL     ERRM27        $MSG
174100000000     C                   LEAVE
174200000000E   5C                   ENDIF
174300000000E   4C                   ENDIF
174400000000     C* SE GG O ORA PARTENZA PIENI ANCHE IL CARICO = 'S'
174500000000IF  4C     LGP(JJ)       IFNE      *BLANKS
174600000000     C     LHP(JJ)       ORGT      *ZEROS
174700000000IF  5C     LAR(JJ)       IFEQ      *BLANKS
174800000000     C                   MOVEL     '1'           $ERR
174900000000     C                   MOVEL     MSG(27)       ERRM27
175000000000     C                   MOVEL     LEQ(JJ)       ERRS27
175100000000     C                   MOVEL     ERRM27        $MSG
175200000000     C                   LEAVE
175300000000E   5C                   ENDIF
175400000000E   4C                   ENDIF
175500951211     C* TEMPO
175600030715IF  4C**!!!LEM(JJ)       IFGT      *ZEROS
175700030715     C**!!!              Z-ADD     LEM(JJ)       WORA
175800030715     C**!!!              EXSR      CHKORA
175900030715IF  5C**!!!$ERR          IFEQ      '1'
176000030715     C**!!!              MOVEL     MSG(25)       ERRM25
176100030715     C**!!!              MOVEL     LEQ(JJ)       ERRS25
176200030715     C**!!!              MOVEL     ERRM25        $MSG
176300030715     C**!!!              LEAVE
176400030715E   5C**!!!              ENDIF
176500030715E   4C**!!!              ENDIF
176600000000     C*
176700000000E   3C                   ENDDO
176800000000E   2C                   ENDIF
176900000000     C*
177000000000     C* CIASCUN ORARIO DI PARTENZA DEVE ESSERE MINORE DI CISCUN ORARIO DI ARRIVO
177100000000     C* SUCCESSIVO, TRANNE L'ULTIMA FERMATA O SE INIZIANO LE FERMATE DI RITORNO
177200000000IF  2C     $ERR          IFEQ      '0'
177300000000     C                   MOVEL     *BLANKS       WALF5A            5
177400000000     C                   MOVEL     *BLANKS       WALF5P            5
177500951211     C                   Z-ADD     *ZEROS        P                 4 0          *INDICE PARTENZA
177600951211     C                   Z-ADD     *ZEROS        A                 4 0          *INDICE ARRIVO
177700000000     C                   Z-ADD     *ZEROS        AA                4 0          *INDICE ARRIVO-1
177800000000DO  3C     1             DO        WULT          P
177900000000     C     P             ADD       1             A
178000000000DO  4C     A             DO        WULT          A
178100000000     C     A             SUB       1             AA                             *ARRIVO PRECEDENTE
178200000000IF  5C     P             IFEQ      WULT                                         *ULTIMA FERMATA
178300000000     C     LTT(P)        ORNE      LTT(A)                                       *ANDATA --> RITORNO
178400000000     C     LTT(A)        ANDNE     LTT(AA)
178500951211X   5C                   ELSE
178600951211IF  6C     LGA(A)        IFNE      *BLANKS                                      *SE ARRIVO PIENO
178700951211     C     LHA(A)        ANDNE     *ZEROS
178800000000     C                   MOVEL     LGP(P)        WALF5P                         *PARTENZA='X+9999'
178900000000     C                   MOVE      LHP(P)        WALF5P
179000000000     C                   MOVEL     LGA(A)        WALF5A                         *ARRIVO='X+9999'
179100000000     C                   MOVE      LHA(A)        WALF5A
179200951211IF  7C     WALF5P        IFGE      WALF5A
179300000000     C                   Z-ADD     A             WNBR                           *RECORD SUBFILE
179400000000     C                   MOVEL     '1'           $ERR
179500000000     C                   MOVEL     MSG(61)       ERRM61
179600000000     C                   MOVEL     LEQ(A)        ERRS61
179700000000     C                   MOVEL     ERRM61        $MSG
179800000000     C                   LEAVE
179900951211E   7C                   ENDIF
180000951211E   6C                   ENDIF
180100000000E   5C                   ENDIF
180200000000E   4C                   ENDDO
180300951218     C*
180400951218IF  4C     $ERR          IFEQ      '1'
180500951218     C                   LEAVE
180600951218E   4C                   ENDIF
180700951218E   3C                   ENDDO
180800951218E   2C                   ENDIF
180900951218     C*
181000951218     C* CIASCUN ORARIO DI PARTENZA (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
181100951218     C* ORARIO DI PARTENZA PRECEDENTE TRANNE LA PRIMA FERMATA
181200951218IF  2C     $ERR          IFEQ      '0'
181300951218     C                   MOVEL     *BLANKS       WALF5A            5
181400951218     C                   MOVEL     *BLANKS       WALF5P            5
181500951218     C                   Z-ADD     *ZEROS        P                 4 0          *INDICE PARTENZA
181600951218     C                   Z-ADD     *ZEROS        PP                4 0          *INDICE PARTENZA-1
181700951218DO  3C     2             DO        WULT          P
181800951218IF  4C     LGP(P)        IFNE      *BLANKS                                      *SE PARTENZA PIENA
181900951218     C     P             SUB       1             PP
182000951218DO  5C     PP            DOWGE     1
182100951218     C                   MOVEL     LGP(P)        WALF5P                         *PARTENZA='X+9999'
182200951218     C                   MOVE      LHP(P)        WALF5P
182300951218     C                   MOVEL     LGP(PP)       WALF5A                         *PARTENZA-1=X+9999'
182400951218     C                   MOVE      LHP(PP)       WALF5A
182500951218IF  6C     WALF5P        IFLE      WALF5A
182600951218     C                   Z-ADD     P             WNBR                           *RECORD SUBFILE
182700951218     C                   MOVEL     '1'           $ERR
182800951218     C                   MOVEL     MSG(53)       ERRM53
182900951218     C                   MOVEL     LEQ(P)        ERRS53
183000951218     C                   MOVEL     ERRM53        $MSG
183100951218     C                   LEAVE
183200951218E   6C                   ENDIF
183300951218     C                   SUB       1             PP
183400951218E   5C                   ENDDO
183500951218E   4C                   ENDIF
183600951218     C*
183700000000IF  4C     $ERR          IFEQ      '1'
183800000000     C                   LEAVE
183900000000E   4C                   ENDIF
184000000000E   3C                   ENDDO
184100000000E   2C                   ENDIF
184200951218     C*
184300951218     C* CIASCUN ORARIO DI ARRIVO (SE PIENO) DEVE ESSERE MAGGIORE DI CIASCUN
184400951218     C* ORARIO DI ARRIVO PRECEDENTE TRANNE LA PRIMA FERMATA
184500951218IF  2C     $ERR          IFEQ      '0'
184600951218     C                   MOVEL     *BLANKS       WALF5A            5
184700951218     C                   MOVEL     *BLANKS       WALF5P            5
184800951218     C                   Z-ADD     *ZEROS        A                 4 0          *INDICE ARRIVO
184900951218     C                   Z-ADD     *ZEROS        AA                4 0          *INDICE ARRIVO-1
185000951218DO  3C     2             DO        WULT          A
185100951218IF  4C     LGA(A)        IFNE      *BLANKS                                      *SE ARRIVO PIENO
185200951218     C     A             SUB       1             AA
185300951218DO  5C     AA            DOWGE     1
185400951218     C                   MOVEL     LGA(A)        WALF5P                         *PARTENZA='X+9999'
185500951218     C                   MOVE      LHA(A)        WALF5P
185600951218     C                   MOVEL     LGA(AA)       WALF5A                         *PARTENZA-1=X+9999'
185700951218     C                   MOVE      LHA(AA)       WALF5A
185800951218IF  6C     WALF5P        IFLE      WALF5A
185900951218     C                   Z-ADD     A             WNBR                           *RECORD SUBFILE
186000951218     C                   MOVEL     '1'           $ERR
186100951218     C                   MOVEL     MSG(64)       ERRM64
186200951218     C                   MOVEL     LEQ(A)        ERRS64
186300951218     C                   MOVEL     ERRM64        $MSG
186400951218     C                   LEAVE
186500951218E   6C                   ENDIF
186600951218     C                   SUB       1             AA
186700951218E   5C                   ENDDO
186800951218E   4C                   ENDIF
186900951218     C*
187000951218IF  4C     $ERR          IFEQ      '1'
187100951218     C                   LEAVE
187200951218E   4C                   ENDIF
187300951218E   3C                   ENDDO
187400951218E   2C                   ENDIF
187500000000     C*
187600000000E   1C                   ENDIF
187700000000E   0C                   ENDIF
187800000000     C*
187900000000     C                   ENDSR
188000000000     C*------------------------------------------------------------------------*
188100000000     C* CTRECC - CONTROLLA LE ECCEZIONI (1)
188200000000     C*------------------------------------------------------------------------*
188300000000     C     CTRECC        BEGSR
188400000000     C*
188500000000     C     *LOVAL        SETLL     AZCAE01L
188600000000     C                   READ      AZCAE01L                               99
188700000000DO  1C     *IN99         DOWEQ     *OFF
188800000000     C     WECC          ANDEQ     '0'
188900970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
189000970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
189100970717     C     CAETFP        ANDGT     *ZEROS
189200970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
189300970717     C     CAETFA        ANDGT     *ZEROS
189400970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
189500000000     C     CAETFA        ANDEQ     LIL(JJ)                                      *FILIALE ARRIVO
189600000000     C     CAETFE        ANDEQ     TLTTFP                                       *TERM ARR ECC=TFP TR
189700000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
189800000000     C     CAEDSC        ANDGE     TLTDSC
189900000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
190000970717E   4C                   ENDIF
190100970717E   3C                   ENDIF
190200960821E   2C                   ENDIF
190300000000     C                   READ      AZCAE01L                               99
190400000000E   1C                   ENDDO
190500000000     C*
190600000000     C                   ENDSR
190700000000     C*------------------------------------------------------------------------*
190800000000     C* CTREC2 - CONTROLLA LE ECCEZIONI (2)
190900000000     C*------------------------------------------------------------------------*
191000000000     C     CTREC2        BEGSR
191100000000     C*
191200000000     C     *LOVAL        SETLL     AZCAE01L
191300000000     C                   READ      AZCAE01L                               99
191400000000DO  1C     *IN99         DOWEQ     *OFF
191500000000     C     WECC          ANDEQ     '0'
191600970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
191700970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
191800970717     C     CAETFP        ANDGT     *ZEROS
191900970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
192000970717     C     CAETFA        ANDGT     *ZEROS
192100970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
192200000000     C     CAETFE        ANDEQ     LIL(JJ)                                      *FILIALE ARRIVO
192300000000     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
192400000000     C     CAEDSC        ANDGE     TLTDSC
192500000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
192600970717E   4C                   ENDIF
192700970717E   3C                   ENDIF
192800960821E   2C                   ENDIF
192900000000     C                   READ      AZCAE01L                               99
193000000000E   1C                   ENDDO
193100000000     C*
193200000000     C                   ENDSR
193300000000     C*------------------------------------------------------------------------*
193400000000     C* CHKFIL - CONTROLLA LA FILIALE
193500000000     C*------------------------------------------------------------------------*
193600000000     C     CHKFIL        BEGSR
193700000000     C*
193800000000     C     *LIKE         DEFINE    VSCFIL        WORG                           *FILIALE
193900971128     C     *LIKE         DEFINE    ORGFIL        WFEL                           *FILIALE ELABORATORE
194000971020     C     *LIKE         DEFINE    ORGFIL        WTFP                           *TERMINAL PARTENZA
194100971020     C     *LIKE         DEFINE    ORGFIL        WTFA                           *TERMINAL PARTENZA
194200000519     C     *LIKE         DEFINE    CAEEPA        WPOS                           *CARICO/SCARICO
194300090114     C     *LIKE         DEFINE    §ogntw        Wntw                           *CARICO/SCARICO
194400000000     C*
194500000000IF  1C     WORG          IFEQ      *ZEROS
194600000000     C                   MOVEL     '1'           $ERR
194700000000X   1C                   ELSE
194800000000     C                   SETOFF                                           28
194900000000     C                   Z-ADD     1             F                 4 0
195000000000     C     WORG          LOOKUP    ORG(F)                                 28
195100000000IF  2C     *IN28         IFEQ      *OFF
195200000000     C                   MOVEL     '1'           $ERR
195300000000X   2C                   ELSE
195400000000     C                   Z-ADD     FEL(F)        WFEL
195500000000     C                   Z-ADD     TFP(F)        WTFP
195600000000     C                   Z-ADD     TFA(F)        WTFA
195700000519     C                   MOVEL     PORG(F)       WPOS
195800090114     c                   movel     ntw(F)        wntw
195900000519E   2C                   ENDIF
196000000000E   1C                   ENDIF
196100000000     C*
196200000000     C                   ENDSR
196300000000     C*------------------------------------------------------------------------*
196400000000     C* CHKORA - CONTROLLA L'ORA
196500000000     C*------------------------------------------------------------------------*
196600000000     C     CHKORA        BEGSR
196700000000     C*
196800000000     C     *LIKE         DEFINE    VSCHHA        WORA
196900000000     C                   Z-ADD     *ZEROS        WHH               2 0
197000000000     C                   Z-ADD     *ZEROS        WMM               2 0
197100000000     C* ORE E MINUTI
197200000000IF  1C     WORA          IFEQ      *ZEROS
197300000000     C                   MOVEL     '1'           $ERR
197400000000E   1C                   ENDIF
197500000000     C* ORE
197600000000     C                   MOVEL     WORA          WHH
197700000000IF  1C     WHH           IFGE      0                                            0 -> 24
197800000000     C     WHH           ANDLE     24
197900000000     C                   ELSE
198000000000     C                   MOVEL     '1'           $ERR
198100000000E   1C                   ENDIF
198200000000     C* MINUTI
198300000000     C                   MOVE      WORA          WMM
198400000000IF  1C     WMM           IFGE      0                                            0 -> 59
198500000000     C     WMM           ANDLE     59
198600000000     C                   ELSE
198700000000     C                   MOVEL     '1'           $ERR
198800000000E   1C                   ENDIF
198900000000     C* ORE 24:00
199000000000IF  1C     WHH           IFEQ      24                                           *SOLO 24:00
199100000000     C     WMM           ANDGT     0
199200000000     C                   MOVEL     '1'           $ERR
199300000000E   1C                   ENDIF
199400000000     C*
199500000000     C                   ENDSR
199600000000     C*------------------------------------------------------------------------*
199700000000     C* AGGFIL - AGGIORNA I FILES
199800000000     C*------------------------------------------------------------------------*
199900000000     C     AGGFIL        BEGSR
200000110525     c* se è presente lo scambio devo calare di uno il numero delle fermate caricate
200100110525     c*  nel sfl
200200110525     c                   if        wesTLS='S'
200300110525     c                   eval      NumFER=sflult-1
200400110525     c                   else
200500110525     c                   eval      NumFER=sflult
200600110525     c                   endif
200700000000     C*
200800000000     C* ANNULLA GLI ORARI DEL GIORNO ANNULLATO A VIDEO (CANC TUTTE LE FERMATE)
200900000000     C                   MOVEL     *BLANKS       JGSE
201000000000DO  1C     1             DO        8             J
201100000000IF  2C     GCA(J)        IFEQ      'A'
201200000000IF  3C     GCM(J)        IFEQ      '8'
201300000000     C                   MOVEL     *BLANKS       JGSE
201400000000X   3C                   ELSE
201500000000     C                   MOVEL     GCM(J)        JGSE
201600000000E   3C                   ENDIF
201700000000     C                   Z-ADD     D00TRN        KLOTRN
201800000000     C                   Z-ADD     D00DDE        KLODDE
201900000000     C     KE2TLO        SETLL     TNTLO01L
202000000000     C     KE2TLO        READE     TNTLO01L                               99
202100000000DO  3C     *IN99         DOWEQ     *OFF
202200000000IF  4C     TLOGSE        IFEQ      JGSE
202300000000     C* GIA' TRASMESSO ANNULLA
202400000000IF  5C     TLODTR        IFGT      *ZEROS
202500000000     C                   MOVEL     'A'           TLOATB                         *ANNULLAMENTO
202600000000     C                   Z-ADD     DATCOR        TLODUV                         *DATA VARIAZIONE
202700000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
202800000000     C                   UPDATE    TNTLO000
202900000000     C* NON TRASMESSO CANCELLA
203000000000X   5C                   ELSE
203100000000     C                   DELETE    TNTLO000
203200000000E   5C                   ENDIF
203300000000E   4C                   ENDIF
203400000000     C     KE2TLO        READE     TNTLO01L                               99
203500000000E   3C                   ENDDO
203600000000     C                   MOVEL     *BLANKS       GCM(J)
203700000000     C                   MOVEL     *BLANKS       GCA(J)
203800000000E   2C                   ENDIF
203900000000E   1C                   ENDDO
204000000000     C*
204100000000     C* LEGGE TUTTE LE FERMATE E GLI ORARI INSERITI (SE CE NE SONO)
204200000000IF  1C     FIL(1)        IFGT      *ZEROS
204300000000     C     FHA(1)        ORGT      *ZEROS
204400000000     C     FHP(1)        ORGT      *ZEROS
204500110525DO  2C***  1             DO        SFLULT        J
204600110525DO  2C     1             DO        NumFER        J
204700110525   2ac                   if        ftt(J)<>' '
204800000000     C* FERMATA
204900000000     C                   Z-ADD     D00TRN        KLRTRN                         *TRAINO
205000000000     C                   Z-ADD     D00DDE        KLRDDE                         *DECORRENZA
205100000000     C                   Z-ADD     J             KLRNFM                         *NUMERO FERMATA
205200000000     C     KE2TLR        CHAIN     TNTLR01L                           99
205300000000     C                   Z-ADD     D00TRN        TLRTRN                         *TRAINO
205400000000     C                   Z-ADD     D00DDE        TLRDDE                         *DECORRENZA
205500000000     C                   Z-ADD     J             TLRNFM                         *NUMERO FERMATA
205600000000     C                   MOVEL     FTT(J)        TLRTFM                         *TIPO FERMATA
205700000000     C                   Z-ADD     FIL(J)        TLRFIL                         *FILIALE
205800000000     C                   MOVEL     FCA(J)        TLRSCA                         *SCARICO
205900000000     C                   MOVEL     FAR(J)        TLRCAR                         *CARICO
206000000000     C                   Z-ADD     FMT(J)        TLRKMT                         *CHILOMETRI
206100000000     C                   Z-ADD     FEM(J)        TLRTEM                         *TEMPO
206200000000     C                   MOVEL     FFV(J)        TLRGFV                         *GIORNO F.VIAGGIO
206300000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
206400000000     C                   MOVEL     *BLANKS       TLRATB                         *ANNULLAMENTO
206500000000     C***
206600000000     C* INSERIMENTO FERMATA
206700000000     C***
206800000000IF  3C     *IN99         IFEQ      *ON
206900000000     C                   Z-ADD     DATCOR        TLRDIR                         *DATA INSERIMENTO
207000000000     C                   Z-ADD     *ZEROS        TLRDUV                         *DATA VARIAZIONE
207100000000     C                   Z-ADD     *ZEROS        TLRDTR                         *DATA TRASMISSIONE
207200000000     C                   WRITE     TNTLR000
207300000000     C***
207400000000     C* VARIAZIONE FERMATA
207500000000     C***
207600000000X   3C                   ELSE
207700000000     C                   Z-ADD     DATCOR        TLRDUV                         *DATA VARIAZIONE
207800000000     C                   UPDATE    TNTLR000
207900000000E   3C                   ENDIF
208000000000     C* ORARIO
208100000000DO  3C     1             DO        8             JJ
208200000000     C     JJ            OCCUR     FGGG
208300000000IF  4C     GCM(JJ)       IFNE      *BLANKS
208400000000     C     GCA(JJ)       ANDNE     'A'
208500000000     C                   Z-ADD     D00TRN        KLOTRN                         *TRAINO
208600000000     C                   Z-ADD     D00DDE        KLODDE                         *DECORRENZA
208700000000     C                   Z-ADD     J             KLONFM                         *NUMERO FERMATA
208800000000     C     JJ            IFEQ      8
208900000000     C                   MOVEL     *BLANKS       KLOGSE
209000000000     C                   ELSE
209100000000     C                   MOVE      JJ            KLOGSE                         *GIORNO SETTIMANA
209200000000     C                   ENDIF
209300000000     C     KEYTLO        CHAIN     TNTLO01L                           99        *FINE FILE
209400000000     C                   Z-ADD     D00TRN        TLOTRN                         *TRAINO
209500000000     C                   Z-ADD     D00DDE        TLODDE                         *DECORRENZA
209600000000     C                   Z-ADD     J             TLONFM                         *NUMERO FERMATA
209700000000     C                   MOVEL     KLOGSE        TLOGSE                         *GIORNO SETTIMANA
209800000000     C                   MOVEL     FGA(J)        TLOGGA                         *GIORNO ARRIVO
209900000000     C                   Z-ADD     FHA(J)        TLOHHA                         *ORA ARRIVO
210000000000     C                   MOVEL     FGP(J)        TLOGGP                         *GIORNO PARTENZA
210100000000     C                   Z-ADD     FHP(J)        TLOHHP                         *ORA PARTENZA
210200000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
210300000000     C                   MOVEL     *BLANKS       TLOATB                         *ANNULLAMENTO
210400000000     C***
210500000000     C* INSERIMENTO ORARIO
210600000000     C***
210700000000IF  5C     *IN99         IFEQ      *ON
210800000000     C                   Z-ADD     DATCOR        TLODIR                         *DATA INSERIMENTO
210900000000     C                   Z-ADD     *ZEROS        TLODUV                         *DATA VARIAZIONE
211000000000     C                   Z-ADD     *ZEROS        TLODTR                         *DATA TRASMISSIONE
211100000000     C                   WRITE     TNTLO000
211200000000     C***
211300000000     C* VARIAZIONE ORARIO
211400000000     C***
211500000000X   5C                   ELSE
211600000000     C                   Z-ADD     DATCOR        TLODUV                         *DATA VARIAZIONE
211700000000     C                   UPDATE    TNTLO000
211800000000E   5C                   ENDIF
211900000000E   4C                   ENDIF
212000000000E   3C                   ENDDO
212100000000     C*
212200110525   2ac                   endif
212300000000E   2C                   ENDDO
212400000000     C*
212500000000E   1C                   ENDIF
212600000000     C*
212700000000     C* ANNULLA LE FERMATE "IN PIU'"
212800000000     C                   Z-ADD     D00TRN        KLRTRN                         *TRAINO
212900000000     C                   Z-ADD     D00DDE        KLRDDE                         *DECORRENZA
213000110525     C****               Z-ADD     SFLULT        KLRNFM                         *ULTIMA FERMATA BUON
213100110525     C                   Z-ADD     NumFER        KLRNFM                         *ULTIMA FERMATA BUON
213200000000     C     KE2TLR        SETGT     TNTLR01L
213300000000     C     KEYTLR        READE     TNTLR01L                               99
213400000000DO  1C     *IN99         DOWEQ     *OFF
213500000000IF  2C     TLRATB        IFEQ      *BLANKS
213600000000     C* GIA' TRASMESSO ANNULLA
213700000000IF  3C     TLRDTR        IFGT      *ZEROS
213800000000     C                   MOVEL     '*'           TLRATB                         *ANNULLAMENTO
213900000000     C                   Z-ADD     DATCOR        TLRDUV                         *DATA VARIAZIONE
214000000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
214100000000     C                   UPDATE    TNTLR000
214200000000     C* NON TRASMESSO CANCELLA
214300000000X   3C                   ELSE
214400000000     C                   DELETE    TNTLR000
214500000000E   3C                   ENDIF
214600000000     C* ED I RELATIVI ORARI
214700000000DO  3C     1             DO        8             J
214800000000     C                   Z-ADD     D00TRN        KLOTRN                         *TRAINO
214900000000     C                   Z-ADD     D00DDE        KLODDE                         *DECORRENZA
215000000000     C                   Z-ADD     TLRNFM        KLONFM                         *NUMERO FERMATA
215100000000     C     J             IFEQ      8
215200000000     C                   MOVEL     *BLANKS       KLOGSE
215300000000     C                   ELSE
215400000000     C                   MOVE      J             KLOGSE
215500000000     C                   ENDIF
215600000000     C     KEYTLO        CHAIN     TNTLO01L                           99        *FINE FILE
215700000000IF  4C     *IN99         IFEQ      *OFF
215800000000     C     TLOATB        ANDEQ     *BLANKS
215900000000     C* GIA' TRASMESSO ANNULLA
216000000000IF  5C     TLODTR        IFGT      *ZEROS
216100000000     C                   MOVEL     '*'           TLOATB                         *ANNULLAMENTO
216200000000     C                   Z-ADD     DATCOR        TLODUV                         *DATA VARIAZIONE
216300000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
216400000000     C                   UPDATE    TNTLO000
216500000000     C* NON TRASMESSO CANCELLA
216600000000X   5C                   ELSE
216700000000     C                   DELETE    TNTLO000
216800000000E   5C                   ENDIF
216900000000E   4C                   ENDIF
217000000000E   3C                   ENDDO
217100951130     C* E LE RELATIVE NOTE
217200951130     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
217300951130     C                   Z-ADD     D00DDE        KLNDDE                         *DECORRENZA
217400951130     C                   Z-ADD     TLRNFM        KLNNFM                         *NUMERO FERMATA
217500951130     C     KEYTLN        SETLL     TNTLN01L
217600951130     C     KEYTLN        READE     TNTLN01L                               99
217700951130DO  3C     *IN99         DOWEQ     *OFF
217800951130IF  4C     TLNATB        IFEQ      *BLANKS
217900951130     C* GIA' TRASMESSO ANNULLA
218000951130IF  5C     TLNDTR        IFGT      *ZEROS
218100951130     C                   MOVEL     '*'           TLNATB                         *ANNULLAMENTO
218200951130     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
218300951130     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
218400951130     C                   UPDATE    TNTLN000
218500951130     C* NON TRASMESSO CANCELLA
218600951130X   5C                   ELSE
218700951130     C                   DELETE    TNTLN000
218800951130E   5C                   ENDIF
218900951130E   4C                   ENDIF
219000951130     C     KEYTLN        READE     TNTLN01L                               99
219100951130E   3C                   ENDDO
219200951130     C*
219300000000E   2C                   ENDIF
219400000000     C     KEYTLR        READE     TNTLR01L                               99
219500000000E   1C                   ENDDO
219600000000     C*
219700000000     C                   ENDSR
219800000000     C*------------------------------------------------------------------------*
219900000000     C* GESOPZ - GESTIONE OPZIONI
220000000000     C*------------------------------------------------------------------------*
220100000000     C     GESOPZ        BEGSR
220200000000     C*
220300000000     C                   MOVEL     '0'           $ERR
220400000000SL  1C                   SELECT
220500000000     C     VSCOPZ        WHENEQ    ' 2'
220600000000     C                   EXSR      O02GES                                       *MODIFICA
220700000000     C     VSCOPZ        WHENEQ    ' 4'
220800000000     C                   EXSR      O04GES                                       *CANCELLAZIONE
220900000000     C                   OTHER
221000000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
221100000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
221200000000E   1C                   ENDSL
221300000000     C*
221400000000     C                   ENDSR
221500000000     C*------------------------------------------------------------------------*
221600000000     C* O02GES - GESTIONE OPZIONE 2 ==> MODIFICA
221700000000     C*------------------------------------------------------------------------*
221800000000     C     O02GES        BEGSR
221900000000     C*
222000000000     C* LANCIA LA MODIFICA
222100000000IF  1C     $ERR          IFEQ      '0'
222200000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
222300000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
222400000000     C*
222500000000     C                   Z-ADD     VSCSEQ        WSEQ                           *SEQUENZA
222600000000     C                   MOVEL     VSCTFM        WTFM                           *TIPO FERMATA
222700000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE FERMATA
222800000000     C                   MOVEL     VSCGGA        WGGA                           *GIORNO ARRIVO
222900000000     C                   Z-ADD     VSCHHA        WHHA                           *ORA ARRIVO
223000000000     C                   MOVEL     VSCSCA        WSCA                           *SCARICO IN ARRIVO
223100000000     C                   MOVEL     VSCGGP        WGGP                           *GIORNO PARTENZA
223200000000     C                   Z-ADD     VSCHHP        WHHP                           *ORA PARTENZA
223300000000     C                   MOVEL     VSCCAR        WCAR                           *CARICO IN PARTENZA
223400000000     C                   Z-ADD     VSCKMT        WKMT                           *CHILOMETRI FERMATA
223500000000     C                   Z-ADD     VSCTEM        WTEM                           *TEMPO FERMATA
223600000000     C                   MOVEL     VSCGFV        WGFV                           *GIORNO F.V.
223700000000     C*
223800000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
223900000000     C                   MOVEL     'O02'         WOP1                           *LIVELLO PROGRAMMA
224000000000     C                   MOVEL     'TNTL05R'     WPGM                           *PGM DI GESTIONE
224100951212     C                   MOVEL     *BLANKS       WMSG                           *MESSAGGIO
224200000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
224300000000     C*
224400000000     C* AGGIORNA LE SCHIERE DI MEMORIZZAZIONE E RICARICA IL SUBFILE
224500000000IF  2C     D00F03        IFNE      '1'
224600000000     C     D00F12        ANDNE     '1'
224700000000     C                   EXSR      AGGSCH                                       *AGGIORNA SCHIERE
224800000000     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
224900000000E   2C                   ENDIF
225000000000E   1C                   ENDIF
225100000000     C*
225200000000     C                   ENDSR
225300000000     C*------------------------------------------------------------------------*
225400000000     C* O04GES - GESTIONE OPZIONE 4 ==> CANCELLAZIONE
225500000000     C*------------------------------------------------------------------------*
225600000000     C     O04GES        BEGSR
225700000000     C*
225800000000     C* LANCIA LA CANCELLAZIONE
225900000000IF  1C     $ERR          IFEQ      '0'
226000000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
226100000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
226200000000     C*
226300000000     C                   Z-ADD     VSCSEQ        WSEQ                           *SEQUENZA
226400000000     C                   MOVEL     VSCTFM        WTFM                           *TIPO FERMATA
226500000000     C                   Z-ADD     VSCFIL        WFIL                           *FILIALE FERMATA
226600000000     C                   MOVEL     VSCGGA        WGGA                           *GIORNO ARRIVO
226700000000     C                   Z-ADD     VSCHHA        WHHA                           *ORA ARRIVO
226800000000     C                   MOVEL     VSCSCA        WSCA                           *SCARICO IN ARRIVO
226900000000     C                   MOVEL     VSCGGP        WGGP                           *GIORNO PARTENZA
227000000000     C                   Z-ADD     VSCHHP        WHHP                           *ORA PARTENZA
227100000000     C                   MOVEL     VSCCAR        WCAR                           *CARICO IN PARTENZA
227200000000     C                   Z-ADD     VSCKMT        WKMT                           *CHILOMETRI FERMATA
227300000000     C                   Z-ADD     VSCTEM        WTEM                           *TEMPO FERMATA
227400000000     C                   MOVEL     VSCGFV        WGFV                           *GIORNO F.V.
227500000000     C*
227600000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
227700000000     C                   MOVEL     'O04'         WOP1                           *LIVELLO PROGRAMMA
227800000000     C                   MOVEL     'TNTL05R'     WPGM                           *PGM DI GESTIONE
227900951212     C                   MOVEL     *BLANKS       WMSG                           *MESSAGGIO
228000000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
228100000000     C*
228200000000     C* AGGIORNA LE SCHIERE DI MEMORIZZAZIONE E RICARICA IL SUBFILE
228300000000IF  2C     D00F03        IFNE      '1'
228400000000     C     D00F12        ANDNE     '1'
228500000000     C                   EXSR      AGGSCH                                       *AGGIORNA SCHIERE
228600000000     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
228700000000E   2C                   ENDIF
228800000000E   1C                   ENDIF
228900000000     C*
229000000000     C                   ENDSR
229100000000     C*------------------------------------------------------------------------*
229200000000     C* AGGSCH - AGGIORNA LE SCHIERE DI MEMORIZZAZIONE
229300000000     C*------------------------------------------------------------------------*
229400000000     C     AGGSCH        BEGSR
229500000000     C*
229600000000     C* PULISCE LE SCHIERE -PRIMA-
229700000000     C                   CLEAR                   PEQ
229800000000     C                   CLEAR                   PTT
229900000000     C                   CLEAR                   PIL
230000000000     C                   CLEAR                   PCA
230100000000     C                   CLEAR                   PAR
230200000000     C                   CLEAR                   PMT
230300000000     C                   CLEAR                   PEM
230400000000     C                   CLEAR                   PGA
230500000000     C                   CLEAR                   PHA
230600000000     C                   CLEAR                   PGP
230700000000     C                   CLEAR                   PHP
230800000000     C                   CLEAR                   PFV
230900000000     C*
231000000000     C* DUPLICA LA SITUAZIONE ATTUALE IN SCHIERE DI -LAVORO-
231100000000     C                   Z-ADD     DEQ           LEQ
231200000000     C                   MOVEL     DTT           LTT
231300000000     C                   Z-ADD     DIL           LIL
231400000000     C                   MOVEL     DCA           LCA
231500000000     C                   MOVEL     DAR           LAR
231600000000     C                   Z-ADD     DMT           LMT
231700000000     C                   Z-ADD     DEM           LEM
231800000000     C                   MOVEL     DFV           LFV
231900000000     C                   MOVEL     DGA           LGA
232000000000     C                   Z-ADD     DHA           LHA
232100000000     C                   MOVEL     DGP           LGP
232200000000     C                   Z-ADD     DHP           LHP
232300000000     C* OPZIONE RICHIESTA
232400000000SL  1C                   SELECT
232500000000     C     D00OP1        WHENEQ    'F10'
232600000000     C                   EXSR      INSERI                                       *INSERIMENTO
232700000000     C     D00OP1        WHENEQ    'O02'
232800000000     C                   EXSR      MODIFI                                       *MODIFICA
232900000000     C     D00OP1        WHENEQ    'O04'
233000000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
233100000000E   1C                   ENDSL
233200000000     C* NO ERRORI
233300000000IF  1C     $ERR          IFEQ      '0'
233400000000     C*
233500000000     C* ORDINA TUTTE LE SCHIERE IN BASE ALLE NUOVE SEQUENZE
233600000000DO  2C     1             DO        50            J
233700000000     C     1             ADD       J             JJ
233800000000DO  3C     JJ            DO        50            JJ                4 0
233900000000IF  4C     LEQ(J)        IFGT      LEQ(JJ)
234000000000     C* SEQUENZA
234100000000     C                   Z-ADD     LEQ(J)        PSEQ
234200000000     C                   Z-ADD     LEQ(JJ)       LEQ(J)
234300000000     C                   Z-ADD     PSEQ          LEQ(JJ)
234400000000     C* TIPO FERMATA
234500000000     C                   MOVEL     LTT(J)        PTFM
234600000000     C                   MOVEL     LTT(JJ)       LTT(J)
234700000000     C                   MOVEL     PTFM          LTT(JJ)
234800000000     C* FILIALE FERMATA
234900000000     C                   Z-ADD     LIL(J)        PFIL
235000000000     C                   Z-ADD     LIL(JJ)       LIL(J)
235100000000     C                   Z-ADD     PFIL          LIL(JJ)
235200000000     C* SCARICO
235300000000     C                   MOVEL     LCA(J)        PSCA
235400000000     C                   MOVEL     LCA(JJ)       LCA(J)
235500000000     C                   MOVEL     PSCA          LCA(JJ)
235600000000     C* CARICO
235700000000     C                   MOVEL     LAR(J)        PCAR
235800000000     C                   MOVEL     LAR(JJ)       LAR(J)
235900000000     C                   MOVEL     PCAR          LAR(JJ)
236000000000     C* CHILOMETRI
236100000000     C                   Z-ADD     LMT(J)        PKMT
236200000000     C                   Z-ADD     LMT(JJ)       LMT(J)
236300000000     C                   Z-ADD     PKMT          LMT(JJ)
236400000000     C* TEMPO
236500000000     C                   Z-ADD     LEM(J)        PTEM
236600000000     C                   Z-ADD     LEM(JJ)       LEM(J)
236700000000     C                   Z-ADD     PTEM          LEM(JJ)
236800000000     C* GIORNO ARRIVO
236900000000     C                   MOVEL     LGA(J)        PGGA
237000000000     C                   MOVEL     LGA(JJ)       LGA(J)
237100000000     C                   MOVEL     PGGA          LGA(JJ)
237200000000     C* ORA ARRIVO
237300000000     C                   Z-ADD     LHA(J)        PHHA
237400000000     C                   Z-ADD     LHA(JJ)       LHA(J)
237500000000     C                   Z-ADD     PHHA          LHA(JJ)
237600000000     C* GIORNO PARTENZA
237700000000     C                   MOVEL     LGP(J)        PGGP
237800000000     C                   MOVEL     LGP(JJ)       LGP(J)
237900000000     C                   MOVEL     PGGP          LGP(JJ)
238000000000     C* ORA PARTENZA
238100000000     C                   Z-ADD     LHP(J)        PHHP
238200000000     C                   Z-ADD     LHP(JJ)       LHP(J)
238300000000     C                   Z-ADD     PHHP          LHP(JJ)
238400000000     C* GIORNO F.V.
238500000000     C                   MOVEL     LFV(J)        PGFV
238600000000     C                   MOVEL     LFV(JJ)       LFV(J)
238700000000     C                   MOVEL     PGFV          LFV(JJ)
238800000000E   4C                   ENDIF
238900000000E   3C                   ENDDO
239000000000E   2C                   ENDDO
239100000000     C*
239200000000     C* COMPATTA I BUCHI VUOTI E RICALCOLA LE SEQUENZE DALL'ORDINAMENTO
239300000000     C                   Z-ADD     *ZEROS        DEPSEQ
239400000000     C                   Z-ADD     *ZEROS        DR                4 0
239500000000DO  2C     1             DO        50            J
239600000000IF  3C     LEQ(J)        IFGT      *ZEROS
239700000000     C                   ADD       1             DR                             *DEPOSITO INDICE
239800000000     C     DR            MULT      10            DEPSEQ
239900000000     C                   Z-ADD     DEPSEQ        PEQ(DR)                        *SCHIERE
240000000000     C                   MOVEL     LTT(J)        PTT(DR)
240100000000     C                   Z-ADD     LIL(J)        PIL(DR)
240200000000     C                   MOVEL     LCA(J)        PCA(DR)
240300000000     C                   MOVEL     LAR(J)        PAR(DR)
240400000000     C                   Z-ADD     LMT(J)        PMT(DR)
240500000000     C                   Z-ADD     LEM(J)        PEM(DR)
240600000000     C                   MOVEL     LFV(J)        PFV(DR)
240700000000     C                   MOVEL     LGA(J)        PGA(DR)
240800000000     C                   Z-ADD     LHA(J)        PHA(DR)
240900000000     C                   MOVEL     LGP(J)        PGP(DR)
241000000000     C                   Z-ADD     LHP(J)        PHP(DR)
241100000000E   3C                   ENDIF
241200000000E   2C                   ENDDO
241300000000     C*
241400000000     C* RIPORTA LA SITUAZIONE NEL -DOPO-
241500000000     C                   Z-ADD     PEQ           DEQ
241600000000     C                   MOVEL     PTT           DTT
241700000000     C                   Z-ADD     PIL           DIL
241800000000     C                   MOVEL     PCA           DCA
241900000000     C                   MOVEL     PAR           DAR
242000000000     C                   Z-ADD     PMT           DMT
242100000000     C                   Z-ADD     PEM           DEM
242200000000     C                   MOVEL     PFV           DFV
242300000000     C                   MOVEL     PGA           DGA
242400000000     C                   Z-ADD     PHA           DHA
242500000000     C                   MOVEL     PGP           DGP
242600000000     C                   Z-ADD     PHP           DHP
242700000000E   1C                   ENDIF
242800000000     C*
242900000000     C                   ENDSR
243000000000     C*------------------------------------------------------------------------*
243100000000     C* INSERI - FUNZIONE DI INSERIMENTO
243200000000     C*------------------------------------------------------------------------*
243300000000     C     INSERI        BEGSR
243400000000     C*
243500000000     C* INCREMENTA L'INDICE DI SCHIERA DEPOSITATO
243600000000IF  1C     DR            IFGE      50
243700000000     C                   MOVEL     '1'           $ERR
243800000000     C                   MOVEL     MSG(39)       $MSG
243900000000X   1C                   ELSE
244000000000     C                   ADD       1             DR
244100000000E   1C                   ENDIF
244200000000     C* -LAVORO-
244300000000     C                   Z-ADD     D00SEQ        LEQ(DR)                        *SEQUENZA
244400000000     C                   MOVEL     D00TFM        LTT(DR)                        *FERMATE
244500000000     C                   Z-ADD     D00FIL        LIL(DR)
244600000000     C                   MOVEL     D00SCA        LCA(DR)
244700000000     C                   MOVEL     D00CAR        LAR(DR)
244800000000     C                   Z-ADD     D00KMT        LMT(DR)
244900000000     C                   Z-ADD     D00TEM        LEM(DR)
245000000000     C                   MOVEL     D00GFV        LFV(DR)
245100000000     C                   MOVEL     D00GGA        LGA(DR)                        *ORARI
245200000000     C                   Z-ADD     D00HHA        LHA(DR)
245300000000     C                   MOVEL     D00GGP        LGP(DR)
245400000000     C                   Z-ADD     D00HHP        LHP(DR)
245500000000     C*
245600000000     C                   ENDSR
245700000000     C*------------------------------------------------------------------------*
245800000000     C* MODIFI - FUNZIONE DI MODIFICA
245900000000     C*------------------------------------------------------------------------*
246000000000     C     MODIFI        BEGSR
246100000000     C*
246200000000     C* AGGANCIA L'ELEMENTO DI SCHIERA DA MODIFICARE (CON SEQUENZA DEPOSITATA)
246300000000     C                   Z-ADD     1             J
246400000000     C                   SETOFF                                           28
246500000000     C     WSEQ          LOOKUP    LEQ(J)                                 28
246600000000     C* -LAVORO-
246700000000     C                   Z-ADD     D00SEQ        LEQ(J)                         *SEQUENZA
246800000000     C                   MOVEL     D00TFM        LTT(J)                         *FERMATE
246900000000     C                   Z-ADD     D00FIL        LIL(J)
247000000000     C                   MOVEL     D00SCA        LCA(J)
247100000000     C                   MOVEL     D00CAR        LAR(J)
247200000000     C                   Z-ADD     D00KMT        LMT(J)
247300000000     C                   Z-ADD     D00TEM        LEM(J)
247400000000     C                   MOVEL     D00GFV        LFV(J)
247500000000     C                   MOVEL     D00GGA        LGA(J)                         *ORARI
247600000000     C                   Z-ADD     D00HHA        LHA(J)
247700000000     C                   MOVEL     D00GGP        LGP(J)
247800000000     C                   Z-ADD     D00HHP        LHP(J)
247900000000     C*
248000000000     C                   ENDSR
248100000000     C*------------------------------------------------------------------------*
248200000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
248300000000     C*------------------------------------------------------------------------*
248400000000     C     ANNULL        BEGSR
248500000000     C*
248600000000     C* AGGANCIA L'ELEMENTO DI SCHIERA DA CANCELLARE (CON SEQUENZA DEPOSITATA)
248700000000     C                   Z-ADD     1             J
248800000000     C                   SETOFF                                           28
248900000000     C     WSEQ          LOOKUP    LEQ(J)                                 28
249000000000     C* -LAVORO-
249100000000     C                   Z-ADD     *ZEROS        LEQ(J)                         *SEQUENZA
249200000000     C                   MOVEL     *BLANKS       LTT(J)                         *FERMATE
249300000000     C                   Z-ADD     *ZEROS        LIL(J)
249400000000     C                   MOVEL     *BLANKS       LCA(J)
249500000000     C                   MOVEL     *BLANKS       LAR(J)
249600000000     C                   Z-ADD     *ZEROS        LMT(J)
249700000000     C                   Z-ADD     *ZEROS        LEM(J)
249800000000     C                   MOVEL     *BLANKS       LFV(J)
249900000000     C                   MOVEL     *BLANKS       LGA(J)                         *ORARI
250000000000     C                   Z-ADD     *ZEROS        LHA(J)
250100000000     C                   MOVEL     *BLANKS       LGP(J)
250200000000     C                   Z-ADD     *ZEROS        LHP(J)
250300000000     C*
250400000000     C                   ENDSR
250500000000     C*------------------------------------------------------------------------*
250600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
250700000000     C*------------------------------------------------------------------------*
250800000000     C     SETFIL        BEGSR
250900000000     C*
251000000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
251100000000     C*
251200000000     C                   Z-ADD     1             R
251300000000     C*
251400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
251500000000IF  1C     DIL(R)        IFEQ      *ZEROS                                       *SCHIERA VUOTA
251600000000     C     DHA(R)        ANDEQ     *ZEROS
251700000000     C     DHP(R)        ANDEQ     *ZEROS
251800000000     C                   Z-ADD     1             SFLEOF
251900000000E   1C                   ENDIF
252000000000     C*
252100000000     C                   ENDSR
252200000000     C*------------------------------------------------------------------------*
252300000000     C* LETFIL - LETTURA SUCESSIVA RECORD
252400000000     C*------------------------------------------------------------------------*
252500000000     C     LETFIL        BEGSR
252600000000     C*
252700000000     C                   ADD       1             R                              *INDICE SCHIERE VIDE
252800000000     C*
252900000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
253000000000IF  1C     R             IFGT      50                                           LUNGHEZZA MAX SCHIER
253100000000     C     DIL(R)        OREQ      *ZEROS                                       *SCHIERA VUOTA
253200000000     C     DHA(R)        ANDEQ     *ZEROS
253300000000     C     DHP(R)        ANDEQ     *ZEROS
253400000000     C                   Z-ADD     1             SFLEOF
253500000000E   1C                   ENDIF
253600000000     C*
253700000000     C                   ENDSR
253800000000     C*------------------------------------------------------------------------*
253900000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO MEMORIZZATO
254000000000     C*------------------------------------------------------------------------*
254100000000     C     IRESFL        BEGSR
254200110519     c* Se c'e' punto di scambio e la fermata ha orario > ddel punto di scambio
254300110519     c*  visualizzo prima punti di scambio
254400110519    1c                   if        westls='S'   and wVisualTLS=' '
254500110519
254600110525    2c                   if        dga(R)<>' '
254700110519    3c                   if        tlsggs<dga(R) or (tlsggs=dga(R) and
254800110519     c                             tlshhs<dha(R))
254900110525     c                   eval      wVisualTLS='S'
255000110519    3c                   endif
255100110519   x2c                   else
255200110519     c*
255300110519    3c                   if        tlsggs<dgp(R) or (tlsggs=dgp(R) and
255400110519     c                             tlshhs<dhp(R))
255500110525     c                   eval      wVisualTLS='S'
255600110519    3c                   endif
255700110519    2c                   endif
255800110519     c
255900110519    2c                   if        wvisualTLS='S'
256000110525     c                   exsr      sflTLS
256100110519    2c                   endif
256200110519    1c                   endif
256300000000     C*
256400000000     C                   MOVEL     *BLANKS       VSCOPZ                         *OPZIONE
256500000000     C* -DOPO-
256600000000     C                   Z-ADD     DEQ(R)        VSCSEQ                         *SEQUENZA
256700000000     C                   MOVEL     DTT(R)        VSCTFM                         *TIPO FERMATA
256800000000     C                   Z-ADD     DIL(R)        VSCFIL                         *FILIALE FERMATA
256900000000     C                   MOVEL     DCA(R)        VSCSCA                         *SCARICO
257000000000     C                   MOVEL     DAR(R)        VSCCAR                         *CARICO
257100000000     C                   Z-ADD     DMT(R)        VSCKMT                         *CHILOMETRI
257200000000     C                   Z-ADD     DEM(R)        VSCTEM                         *TEMPO
257300000000     C                   MOVEL     DFV(R)        VSCGFV                         *GIORNO F.VIAGGIO
257400000000     C                   MOVEL     DGA(R)        VSCGGA                         *GIORNO ARRIVO
257500000000     C                   Z-ADD     DHA(R)        VSCHHA                         *ORA ARRIVO
257600000000     C                   MOVEL     DGP(R)        VSCGGP                         *GIORNO PARTENZA
257700000000     C                   Z-ADD     DHP(R)        VSCHHP                         *ORA PARTENZA
257800000000     C* DECODIFICHE
257900000000     C                   SETOFF                                           28
258000000000     C                   Z-ADD     1             J
258100000000     C     VSCFIL        LOOKUP    ORG(J)                                 28
258200000000IF  1C     *IN28         IFEQ      *ON
258300000000     C                   MOVEL     DORG(J)       VSDFIL                         *DESCRIZIONE FILIALE
258400000000X   1C                   ELSE
258500000000     C                   MOVEL     *ALL'*'       VSDFIL                         *DESCRIZIONE FILIALE
258600000000E   1C                   ENDIF
258700000000     C*
258800000000     C                   ENDSR
258900110519     C*------------------------------------------------------------------------*
259000110519     C* riempo riga di sfl con lo scambio
259100110519     C*------------------------------------------------------------------------*
259200110519     c     sflTLS        BEGSR
259300110519     c                   seton                                        80
259400110519     C                   MOVEL     *BLANKS       VSCOPZ                         *OPZIONE
259500110519     C* -DOPO-
259600110519     C                   clear                   VSCSEQ                         *SEQUENZA
259700110519     C                   MOVEL     'S'           VSCTFM                         *TIPO FERMATA
259800110519     C                   clear                   VSCFIL                         *FILIALE FERMATA
259900110519     c* Nella decodifica filiale punto di scambio
260000110519     c     tlspsc        chain     tnpsc01l
260100110525    1c                   if        not%found(tnpsc01l)
260200110519     c                   eval      pscloc=*all'?'
260300110525    1c                   endif
260400110519     c                   eval      vsdfil=%trim(pscpsc)+'-'+
260500110519     c                             pscloc
260600110519     c
260700110519     C                   clear                   VSCSCA                         *SCARICO
260800110519     C                   clear                   VSCCAR                         *CARICO
260900110519     C                   clear                   VSCKMT                         *CHILOMETRI
261000110519     C                   clear                   VSCTEM                         *TEMPO
261100110519     C                   clear                   VSCGFV                         *GIORNO F.VIAGGIO
261200110519     C                   MOVEL     tlsggs        VSCGGA                         *GIORNO ARRIVO
261300110519     C                   Z-ADD     tlshhs        VSCHHA                         *ORA ARRIVO
261400110519     C                   clear                   VSCGGP                         *GIORNO PARTENZA
261500110519     C                   clear                   VSCHHP                         *ORA PARTENZA
261600110519     c
261700110519     C                   ADD       1             SFLULT
261800110519     C* SCRITTURA RECORD SFL
261900110519     C                   WRITE     TL04DVS
262000110519     C                   ADD       1             SFLRIG
262100110519     c                   setoff                                       80
262200110519     C*
262300110519     C                   ENDSR
262400000000     C*------------------------------------------------------------------------*
262500000000     C* LANPGM - LANCIA UN PROGRAMMA
262600000000     C*------------------------------------------------------------------------*
262700000000     C     LANPGM        BEGSR
262800000000     C***
262900000000     C* PARAMETRI INFORMATIVI
263000000000     C***
263100000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
263200000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
263300000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
263400000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
263500000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
263600951212     C                   MOVEL     WMSG          D00MSG                         *MESSAGGIO ERRORE
263700000000     C***
263800000000     C* PARAMETRI
263900000000     C***
264000000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
264100000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
264200000000     C                   Z-ADD     WSEQ          D00SEQ                         *SEQUENZA
264300000000     C                   MOVEL     WTFM          D00TFM                         *TIPO FERMATA
264400000000     C                   Z-ADD     WFIL          D00FIL                         *FILIALE FERMATA
264500000000     C                   MOVEL     WGGA          D00GGA                         *GIORNO ARRIVO
264600000000     C                   Z-ADD     WHHA          D00HHA                         *ORA ARRIVO
264700000000     C                   MOVEL     WSCA          D00SCA                         *SCARICO IN ARRIVO
264800000000     C                   MOVEL     WGGP          D00GGP                         *GIORNO PARTENZA
264900000000     C                   Z-ADD     WHHP          D00HHP                         *ORA PARTENZA
265000000000     C                   MOVEL     WCAR          D00CAR                         *CARICO IN PARTENZA
265100000000     C                   Z-ADD     WKMT          D00KMT                         *CHILOMETRI FERMATA
265200000000     C                   Z-ADD     WTEM          D00TEM                         *TEMPO FERMATA
265300000000     C                   MOVEL     WGFV          D00GFV                         *GIORNO F.V.
265400000000     C*--------------------
265500000000     C* L A N C I O
265600000000     C*--------------------
265700000000     C                   MOVEL     DS00          KPJBU
265800000000     C                   CALL      WPGM                                 99
265900000000     C                   PARM                    KPJBA
266000000000     C                   MOVEL     KPJBU         DS00
266100000000     C*
266200000000IF  1C     D00F03        IFEQ      '1'
266300000000     C                   MOVEL     '1'           $F03
266400000000     C                   MOVEL     '1'           $FINE
266500000000E   1C                   ENDIF
266600000000IF  1C     D00F12        IFEQ      '1'
266700000000     C                   MOVEL     '1'           $F12
266800000000E   1C                   ENDIF
266900000000     C                   MOVEL     D00MSG        $MSG
267000000000     C                   EXSR      CARMSG
267100000000     C*
267200000000     C                   ENDSR
267300000000     C*------------------------------------------------------------------------*
267400000000     C* IMPVID - IMPOSTAZIONI VIDEO
267500000000     C*------------------------------------------------------------------------*
267600000000     C     IMPVID        BEGSR
267700000000     C*
267800000000     C* CARICA L'ALFABETO
267900000000     C                   MOVEA     DALF          ALF
268000000000     C* DATI TRAINO
268100000000     C                   Z-ADD     D00TRN        VCCTRN                         *TRAINO
268200000000     C                   Z-ADD     D00DDE        G08INV
268300000000     C                   Z-ADD     *ZEROS        G08DAT
268400000000     C                   MOVEL     '3'           G08ERR
268500000000     C                   CALL      'XSRDA8'
268600000000     C                   PARM                    WLBDA8
268700000000     C                   Z-ADD     G08DAT        VCCDDE                         *DATA DECORRENZA
268800000000     C* LEGGE IL TRAINO
268900000000     C                   Z-ADD     D00TRN        KLTTRN
269000000000     C                   Z-ADD     D00DDE        KLTDDE
269100000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
269200020715     c                   z-add     1             ktbkut
269300000000     C                   MOVEL     'TV'          KTBCOD
269400000000     C                   MOVEL(P)  TLTTTR        KTBKEY
269500000000     C     KEYTAB        CHAIN     TABEL00F                           99
269600000000     C                   MOVEL     TBLUNI        DSTV
269700000000     C                   MOVEL     §TVDES        VCCTTR                         *TIPO TRAINO
269800000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
269900090115     C                   MOVEL     §TVutc        DEPutc
270000000000     C                   Z-ADD     TLTDSC        G08INV
270100000000     C                   Z-ADD     *ZEROS        G08DAT
270200000000     C                   MOVEL     '3'           G08ERR
270300000000     C                   CALL      'XSRDA8'
270400000000     C                   PARM                    WLBDA8
270500000000     C                   Z-ADD     G08DAT        VCCDSC                         *DATA SCADENEZA
270600000000     C                   Z-ADD     TLTTFP        VCCTFP                         *TERMINAL PARTENZA
270700000000     C                   Z-ADD     TLTTFA        VCCTFA                         *TERMINAL PARTENZA
270800000000     C                   MOVEA     TLTGSE        GIO                            *PARTENZE
270900000000     C                   MOVEL     GIO(1)        VCCGS1
271000000000     C                   MOVEL     GIO(2)        VCCGS2
271100000000     C                   MOVEL     GIO(3)        VCCGS3
271200000000     C                   MOVEL     GIO(4)        VCCGS4
271300000000     C                   MOVEL     GIO(5)        VCCGS5
271400000000     C                   MOVEL     GIO(6)        VCCGS6
271500000000     C                   MOVEL     GIO(7)        VCCGS7
271600960423     C*
271700960423     C* SE TIPO TRAINO INOLTRO METTE NELLA DESCRIZIONE IL TIPO TRAINO ASSOCIATO
271800960423IF  1C     TLTTTR        IFEQ      'I'
271900960423     C                   CLEAR                   SA7
272000960423     C                   MOVEA     'INL'         SA7
272100960423     C                   MOVEL     '('           SA7(5)
272200960423     C                   MOVEL     D00INL        SA7(6)
272300960423     C                   MOVEL     ')'           SA7(7)
272400960423     C                   MOVEA     SA7           VCCTTR
272500960423E   1C                   ENDIF
272600000000     C*
272700000000     C* MEMORIZZA IN SCHIERE LE FERMATE E GLI ORARI DEL FILE
272800000000     C                   EXSR      SETTEO
272900000000     C*
273000000000     C                   ENDSR
273100000000     C*------------------------------------------------------------------------*
273200000000     C* SETTEO - MEMORIZZA IN SCHIERE LE FERMATE E GLI ORARI (POSIZIONAMENTO)
273300000000     C*------------------------------------------------------------------------*
273400000000     C     SETTEO        BEGSR
273500000000     C*
273600000000     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE -FILE-
273700000000     C                   CLEAR                   FTT                            *FERMATE
273800000000     C                   CLEAR                   FIL
273900000000     C                   CLEAR                   FCA
274000000000     C                   CLEAR                   FAR
274100000000     C                   CLEAR                   FMT
274200000000     C                   CLEAR                   FEM
274300000000     C                   CLEAR                   FFV
274400000000DO  1C     1             DO        8             J                              *ORARI X 8 GIORNI
274500000000     C     J             OCCUR     FGGG
274600000000     C                   CLEAR                   FGA
274700000000     C                   CLEAR                   FHA
274800000000     C                   CLEAR                   FGP
274900000000     C                   CLEAR                   FHP
275000000000E   1C                   ENDDO
275100000000     C*
275200110525     c                   clear                   wVisualTLS
275300000000     C                   Z-ADD     D00TRN        KLRTRN
275400000000     C                   Z-ADD     D00DDE        KLRDDE
275500000000     C     KEYTLR        SETLL     TNTLR01L                           99        *FINE FILE
275600000000     C*
275700000000IF  1C     *IN99         DOWEQ     *OFF
275800000000     C                   EXSR      LETTEO
275900000000E   1C                   ENDDO
276000000000     C*
276100000000     C                   ENDSR
276200000000     C*------------------------------------------------------------------------*
276300000000     C* LETTEO - MEMORIZZA IN SCHIERE LE FERMATE E GLI ORARI (LETTURA SUCESSIVA)
276400000000     C*------------------------------------------------------------------------*
276500000000     C     LETTEO        BEGSR
276600000000     C*
276700000000     C* LEGGO FINO A:
276800000000     C*     - FINE FILE
276900000000     C*     - TROVATO RECORD VALIDO
277000000000     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
277100000000DO  1C     *IN99         DOWEQ     *OFF
277200000000     C     WRECOK        ANDEQ     'N'
277300000000     C*
277400000000     C     KEYTLR        READE(N)  TNTLR01L                               99
277500000000     C*
277600000000IF  2C     *IN99         IFEQ      *OFF
277700000000     C                   EXSR      CHKTEO                                       *CONTROLLA RECORD
277800000000E   2C                   ENDIF
277900000000     C*
278000000000E   1C                   ENDDO
278100000000     C* RECORD OK
278200000000IF  1C     WRECOK        IFEQ      'S'
278300000000     C     *IN99         ANDEQ     *OFF
278400000000     C                   EXSR      IRETEO
278500000000E   1C                   ENDIF
278600000000     C*
278700000000     C                   ENDSR
278800000000     C*------------------------------------------------------------------------*
278900000000     C* CHKTEO - MEMORIZZA IN SCHIERE LE FERMATE E GLI ORARI (CONTROLLA RECORD)
279000000000     C*------------------------------------------------------------------------*
279100000000     C     CHKTEO        BEGSR
279200000000     C*
279300000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
279400000000     C*
279500000000     C* TRAINO VALIDO -> RECORD ANNULLATI (DA PGM O MANUALMENTE), ESCLUDE
279600000000     C* TRAINO NON VALIDO -> RECORD ANNULLATI DA PROGRAMMA, ESCLUDE
279700000000IF  1C     TLTATB        IFEQ      *BLANKS
279800000000IF  2C     TLRATB        IFNE      *BLANKS
279900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
280000000000E   2C                   ENDIF
280100000000X   1C                   ELSE
280200000000IF  2C     TLRATB        IFEQ      '*'                                          *ANNULLATO DA PGM
280300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
280400000000E   2C                   ENDIF
280500000000E   1C                   ENDIF
280600000000     C*
280700000000     C                   ENDSR
280800000000     C*------------------------------------------------------------------------*
280900000000     C* IRETEO - MEMORIZZA IN SCHIERE LE FERMATE E GLI ORARI DI TUTTI I GIORNI
281000000000     C*------------------------------------------------------------------------*
281100000000     C     IRETEO        BEGSR
281200000000     C* NUMERO
281300000000     C                   ADD       1             R
281400000000     C*
281500000000     C* MEMORIZZA I DATI DELLA FERMATA
281600000000     C                   MOVEL     TLRTFM        FTT(R)
281700000000     C                   Z-ADD     TLRFIL        FIL(R)
281800000000     C                   MOVEL     TLRSCA        FCA(R)
281900000000     C                   MOVEL     TLRCAR        FAR(R)
282000000000     C                   Z-ADD     TLRKMT        FMT(R)
282100000000     C                   Z-ADD     TLRTEM        FEM(R)
282200000000     C                   MOVEL     TLRGFV        FFV(R)
282300000000     C*
282400000000     C* PER OGNI GIORNO DELLA SETTIMANA, MEMORIZZA GLI ORARI DELLA FERMATA
282500000000     C                   Z-ADD     TLRTRN        KLOTRN
282600000000     C                   Z-ADD     TLRDDE        KLODDE
282700000000     C                   Z-ADD     TLRNFM        KLONFM
282800000000DO  1C     1             DO        8             J
282900000000IF  2C     J             IFEQ      8
283000000000     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO 8 = ' '
283100000000X   2C                   ELSE
283200000000     C                   MOVE      J             KLOGSE                         *GIORNO 1÷8
283300000000E   2C                   ENDIF
283400000000     C     KEYTLO        CHAIN(N)  TNTLO01L                           98
283500000000IF  2C     *IN98         IFEQ      *OFF                                         *GIORNO ESISTENTE
283600000000     C*
283700000000     C* TRAINO VALIDO -> RECORD ANNULLATI (DA PGM O MANUALMENTE), ESCLUDE
283800000000     C* TRAINO NON VALIDO -> RECORD ANNULLATI DA PROGRAMMA, ESCLUDE
283900000000IF  3C     TLTATB        IFEQ      *BLANKS                                      *TRAINO VALIDO
284000000000     C     TLOATB        ANDEQ     *BLANKS                                      *ORARI NON ANNULLATI
284100000000     C     TLTATB        ORNE      *BLANKS                                      *TRAINO NON VALIDO
284200000000     C     TLOATB        ANDNE     '*'                                          *ORARI NO ANNUL PGM
284300000000     C*
284400000000     C     J             OCCUR     FGGG
284500000000     C                   MOVEL     TLOGGA        FGA(R)
284600000000     C                   Z-ADD     TLOHHA        FHA(R)
284700000000     C                   MOVEL     TLOGGP        FGP(R)
284800000000     C                   Z-ADD     TLOHHP        FHP(R)
284900000000     C* SCHIERA GIORNI
285000000000IF  4C     KLOGSE        IFEQ      *BLANKS
285100000000     C                   MOVEL     8             GCM(J)                         *GIORNO CARICATO
285200000000X   4C                   ELSE
285300000000     C                   MOVEL     KLOGSE        GCM(J)                         *GIORNO CARICATO
285400000000E   4C                   ENDIF
285500000000     C                   MOVEL     *BLANKS       GCA(J)                         *GIORNO NON ANNULLAT
285600000000E   3C                   ENDIF
285700000000E   2C                   ENDIF
285800000000E   1C                   ENDDO
285900000000     C*
286000000000     C* DEPOSITA IL NUMERO TOTALE DI FERMATE
286100000000     C                   Z-ADD     R             DR                             *DEPOSITO INDICE
286200000000     C*
286300000000     C                   ENDSR
286400110525     C*------------------------------------------------------------------------*
286500110525     C* Ricerca punto di scambio
286600110525     C*------------------------------------------------------------------------*
286700110525     c     CercaTLS      BEGSR
286800110525     c* Verifico se è presente un punto di scambio
286900110525     c                   eval      WESTLS='S'
287000110525     c                   eval      kgse=d00gse
287100110525     c     keytls        chain(N)  tntls01l
287200110525     c                   if        not %found(tntls01l) or tlsatb<>' '
287300110525     c                   clear                   kgse
287400110525     c     keytls        chain(N)  tntls01l
287500110525     c                   if        not %found(tntls01l) or tlsatb<>' '
287600110525     c                   eval      WESTLS='N'
287700110525     c                   endif
287800110525     c                   endif
287900110525     c                   ENDSR
288000000000     C*------------------------------------------------------------------------*
288100000000     C* REIMPO - AZZERA IL CONTATORE SEQUENZA E LE SCHIERE DI APPOGGIO
288200000000     C*------------------------------------------------------------------------*
288300000000     C     REIMPO        BEGSR
288400000000     C*
288500000000     C                   Z-ADD     *ZEROS        DEPSEQ                         *CONTATORE
288600000000     C                   Z-ADD     *ZEROS        R                 4 0          *INDICE
288700000000     C                   MOVEL     '0'           MSGF12                         *EMETTE MESSAGGIO
288800000000     C*
288900000000     C                   CLEAR                   DEQ                            *SCHIERE -DOPO-
289000000000     C                   CLEAR                   DTT
289100000000     C                   CLEAR                   DIL
289200000000     C                   CLEAR                   DGA
289300000000     C                   CLEAR                   DHA
289400000000     C                   CLEAR                   DCA
289500000000     C                   CLEAR                   DGP
289600000000     C                   CLEAR                   DHP
289700000000     C                   CLEAR                   DAR
289800000000     C                   CLEAR                   DMT
289900000000     C                   CLEAR                   DEM
290000000000     C                   CLEAR                   DFV
290100000000     C*
290200000000     C                   CLEAR                   PEQ                            *SCHIERE -PRIMA-
290300000000     C                   CLEAR                   PTT
290400000000     C                   CLEAR                   PIL
290500000000     C                   CLEAR                   PGA
290600000000     C                   CLEAR                   PHA
290700000000     C                   CLEAR                   PCA
290800000000     C                   CLEAR                   PGP
290900000000     C                   CLEAR                   PHP
291000000000     C                   CLEAR                   PAR
291100000000     C                   CLEAR                   PMT
291200000000     C                   CLEAR                   PEM
291300000000     C                   CLEAR                   PFV
291400000000     C*
291500000000     C                   CLEAR                   LEQ                            *SCHIERE -LAVORO-
291600000000     C                   CLEAR                   LTT
291700000000     C                   CLEAR                   LIL
291800000000     C                   CLEAR                   LGA
291900000000     C                   CLEAR                   LHA
292000000000     C                   CLEAR                   LCA
292100000000     C                   CLEAR                   LGP
292200000000     C                   CLEAR                   LHP
292300000000     C                   CLEAR                   LAR
292400000000     C                   CLEAR                   LMT
292500000000     C                   CLEAR                   LEM
292600000000     C                   CLEAR                   LFV
292700000000     C*
292800000000     C                   ENDSR
292900000000     C*------------------------------------------------------------------------*
293000000000     C* CARTAB - CARICA LE TABELLE
293100000000     C*------------------------------------------------------------------------*
293200000000     C     CARTAB        BEGSR
293300000000     C*--------------------
293400000000     C* FILIALI
293500000000     C*--------------------
293600000518     C                   CLEAR                   OG148
293700000518     C                   CLEAR                   OG143
293800000000     C                   Z-ADD     *ZEROS        J                 4 0
293900000000     C     *LOVAL        SETLL     AZORG01L
294000000000     C                   READ      AZORG01L                               99
294100000000DO  1C     *IN99         DOWEQ     *OFF
294200000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
294300960208     C                   MOVEL     ORGDE8        OG148
294400960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
294500000000     C                   ADD       1             J
294600000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
294700000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
294800000518     C                   MOVEL     ORGDE3        OG143
294900000518     C                   MOVEL     §OGPT         PORG(J)                        *P.O. POSTE
295000090115     C                   MOVEL     §OGntw        ntw(J)                         *network
295100971020     C                   CLEAR                   DSLV55
295200971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
295300971020     C                   MOVEL     *BLANKS       D55TPT                         *PARTENZA/ARRIVO
295400971020     C                   Z-ADD     ORGFIL        D55LIN                         *LINEA
295500980609     C                   Z-ADD     D00DDE        D55DRF                         *DATA RIFERIMENTO
295600971020     C                   CALL      'FNLV55R'
295700971020     C                   PARM                    DSLV55
295800971128IF  4C     D55ERR        IFNE      *BLANKS
295900971128     C                   Z-ADD     ORGFIL        D55TFP
296000971128     C                   Z-ADD     ORGFIL        D55TFA
296100971128E   4C                   ENDIF
296200971021     C                   Z-ADD     D55TFP        TFP(J)                         *TERMINAL PARTENZA
296300971021     C                   Z-ADD     D55TFA        TFA(J)                         *TERMINAL PARTENZA
296400971128     C                   Z-ADD     D55TFP        FEL(J)                         *FILIALE ELABORATORE
296500960208E   3C                   ENDIF
296600960208E   2C                   ENDIF
296700000000     C                   READ      AZORG01L                               99
296800000000E   1C                   ENDDO
296900000000     C*
297000000000     C                   ENDSR
297100971020     C*------------------------------------------------------------------------*
297200971020     C* FINSR - OPERAZIONI FINALI
297300971020     C*------------------------------------------------------------------------*
297400971020     C     FINSR         BEGSR
297500971020     C* FNLV55R
297600971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
297700971020     C                   MOVEL     'C'           D55TLA
297800971020     C                   CALL      'FNLV55R'
297900971020     C                   PARM                    DSLV55
298000971020     C*
298100971020     C                   ENDSR
298200000000     C*------------------------------------------------------------------------*
298300000000     C* *INZSR - OPERAZIONI INIZIALI
298400000000     C*------------------------------------------------------------------------*
298500000000     C     *INZSR        BEGSR
298600000000     C*--------------------
298700000000     C* RICEVIMENTO PARAMETRI
298800000000     C*--------------------
298900000000     C     *ENTRY        PLIST
299000000000     C                   PARM                    KPJBA
299100000000     C                   MOVEL     KPJBU         DS00
299200000000     C*--------------------
299300000000     C* VARIABILI RIFERITE AL DATA BASE
299400000000     C*--------------------
299500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
299600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
299700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
299800000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
299900000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
300000951130     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
300100951130     C     *LIKE         DEFINE    TLNDDE        KLNDDE
300200951130     C     *LIKE         DEFINE    TLNNFM        KLNNFM
300300000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
300400000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
300500000000     C     *LIKE         DEFINE    TLRNFM        KLRNFM
300600000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
300700000000     C     *LIKE         DEFINE    TLODDE        KLODDE
300800000000     C     *LIKE         DEFINE    TLONFM        KLONFM
300900000000     C     *LIKE         DEFINE    TLOGSE        KLOGSE
301000000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
301100000000     C     *LIKE         DEFINE    D00OP1        WOP1
301200000000     C     *LIKE         DEFINE    D00F03        WF03
301300000000     C     *LIKE         DEFINE    D00F12        WF12
301400000000     C     *LIKE         DEFINE    D00ERR        WERR
301500000000     C     *LIKE         DEFINE    D00MSG        WMSG
301600000000     C     *LIKE         DEFINE    D00TRN        WTRN
301700000000     C     *LIKE         DEFINE    D00DDE        WDDE
301800000000     C     *LIKE         DEFINE    D00SEQ        WSEQ
301900000000     C     *LIKE         DEFINE    D00TFM        WTFM
302000000000     C     *LIKE         DEFINE    D00FIL        WFIL
302100000000     C     *LIKE         DEFINE    D00GGA        WGGA
302200000000     C     *LIKE         DEFINE    D00HHA        WHHA
302300000000     C     *LIKE         DEFINE    D00SCA        WSCA
302400000000     C     *LIKE         DEFINE    D00GGP        WGGP
302500000000     C     *LIKE         DEFINE    D00HHP        WHHP
302600000000     C     *LIKE         DEFINE    D00CAR        WCAR
302700000000     C     *LIKE         DEFINE    D00KMT        WKMT
302800000000     C     *LIKE         DEFINE    D00TEM        WTEM
302900000000     C     *LIKE         DEFINE    D00GFV        WGFV
303000000000     C     *LIKE         DEFINE    D00SEQ        PSEQ                           *DI PARK
303100000000     C     *LIKE         DEFINE    D00TFM        PTFM
303200000000     C     *LIKE         DEFINE    D00FIL        PFIL
303300000000     C     *LIKE         DEFINE    D00GGA        PGGA
303400000000     C     *LIKE         DEFINE    D00HHA        PHHA
303500000000     C     *LIKE         DEFINE    D00SCA        PSCA
303600000000     C     *LIKE         DEFINE    D00GGP        PGGP
303700000000     C     *LIKE         DEFINE    D00HHP        PHHP
303800000000     C     *LIKE         DEFINE    D00CAR        PCAR
303900000000     C     *LIKE         DEFINE    D00KMT        PKMT
304000000000     C     *LIKE         DEFINE    D00TEM        PTEM
304100000000     C     *LIKE         DEFINE    D00GFV        PGFV
304200000000     C     *LIKE         DEFINE    D00SEQ        DEPSEQ                         *DEPOSITO
304300951130     C     *LIKE         DEFINE    VCCNBR        WNBR                           *RIGA ERRATA
304400000000     C     *LIKE         DEFINE    TLOGSE        JGSE                           *DI LAVORO
304500971128     C     *LIKE         DEFINE    ORGFIL        DEPFEL                         *DEPOSITI
304600971128     C     *LIKE         DEFINE    ORGFIL        DEPTFP
304700971128     C     *LIKE         DEFINE    ORGFIL        DEPTFA
304800000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF
304900090115     C     *LIKE         DEFINE    §TVutc        DEPutc
305000000519     C     *LIKE         DEFINE    CAEEPA        DEPPOS
305100090114     C     *LIKE         DEFINE    VSCFIL        Wfilp
305200090115     C     *LIKE         DEFINE    depest        West
305300000000     C*--------------------
305400000000     C* CHIAVI DI LETTURA
305500000000     C*--------------------
305600000000     C* LETTURA TABEL00F - COMPLETA
305700000000     C     KEYTAB        KLIST
305800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
305900000000     C                   KFLD                    KTBCOD                         *TABELLA
306000000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
306100000000     C* LETTURA TNTLT01L - COMPLETA
306200000000     C     KEYTLT        KLIST
306300000000     C                   KFLD                    KLTTRN                         *TRAINO
306400000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
306500951130     C* LETTURA TNTLN01L - PARZIALE
306600951130     C     KEYTLN        KLIST
306700951130     C                   KFLD                    KLNTRN                         *TRAINO
306800951130     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
306900951130     C                   KFLD                    KLNNFM                         *NUMERO FERMATA
307000000000     C* LETTURA TNTLR01L - PARZIALE
307100000000     C     KEYTLR        KLIST
307200000000     C                   KFLD                    KLRTRN                         *TRAINO
307300000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
307400000000     C* LETTURA TNTLR01L - COMPLETA
307500000000     C     KE2TLR        KLIST
307600000000     C                   KFLD                    KLRTRN                         *TRAINO
307700000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
307800000000     C                   KFLD                    KLRNFM                         *NUMERO FERMATA
307900000000     C* LETTURA TNTLO01L - COMPLETA
308000000000     C     KEYTLO        KLIST
308100000000     C                   KFLD                    KLOTRN                         *TRAINO
308200000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
308300000000     C                   KFLD                    KLONFM                         *NUMERO FERMATA
308400000000     C                   KFLD                    KLOGSE                         *GIORNO SETTIMANA
308500000000     C* LETTURA TNTLO01L - PARZIALE
308600000000     C     KE2TLO        KLIST
308700000000     C                   KFLD                    KLOTRN                         *TRAINO
308800000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
308900110519     C* LETTURA TNTLS01L - PARZIALE
309000110519     C     KEYTLS        KLIST
309100110519     C                   KFLD                    d00TRN                         *TRAINO
309200110519     C                   KFLD                    d00DDE                         *DATA DECORRENZA
309300110519     C                   KFLD                    kgse
309400000000     C*--------------------
309500000000     C* VARIABILI DI CONTROLLO
309600000000     C*--------------------
309700000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
309800000000     C                   MOVEL     '1'           MSGF12            1            *MESSAGGIO PER F12
309900971020     C*
310000971020     C* CALCOLA LA DATA CORRENTE
310100971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
310200971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
310300971020     C                   Z-ADD     N8            G08DAT
310400971020     C                   Z-ADD     *ZEROS        G08INV
310500971020     C                   MOVEL     '0'           G08ERR
310600971020     C                   CALL      'XSRDA8'
310700971020     C                   PARM                    WLBDA8
310800971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
310900000000     C***
311000000000     C* IMPOSTAZIONI VIDEO
311100000000     C***
311200000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
311300000000     C                   MOVE      D00OP0        INTGES            2
311400000000IF  1C     INTGES        IFEQ      '02'
311500000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
311600000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
311700000000X   1C                   ELSE
311800000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
311900000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
312000000000E   1C                   ENDIF
312100000000     C*
312200000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
312300000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
312400000000IF  1C     *IN99         IFEQ      *ON
312500000000     C     *IN99         ANDEQ     *OFF
312600000000     C* TRAINI
312700000000     C                   UPDATE    TNTLT000
312800000000     C                   WRITE     TNTLT000
312900951211     C* NOTE
313000951211     C                   WRITE     TNTLN000
313100110519     C                   WRITE     TNTLS000
313200000000E   1C                   ENDIF
313300110525     c                   exsr      CercaTLS
313400000000     C*
313500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
313600000000     C                   EXSR      INZSTD
313700000000     C*
313800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
313900000000     C                   EXSR      INZSFL
314000000000     C*
314100000000     C* IMPOSTAZIONE INIZIALE VIDEO
314200000000     C                   EXSR      IMPVID
314300000000     C*
314400000000     C* CARICA LE TABELLE
314500000000     C                   EXSR      CARTAB
314600000000     C*
314700000000     C* EMETTO VIDEATA SUBFILE
314800000000     C                   EXSR      CARVS                                        CARICA E EMETTE SFIL
314900000000     C*
315000000000     C                   ENDSR
315100000000     C*------------------------------------------------------------------------*
315200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
315300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
315400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
315500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
315600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
315700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
315800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
315900000000     C*------------------------------------------------------------------------*
316000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
316100000000     C*------------------------------------------------------------------------*
316200000000     C     INZSTD        BEGSR
316300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
316400000000     C                   CALL      'XKEYS'
316500000000     C                   PARM                    DSKEYS
316600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
316700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
316800000000     C     XMSGPA        PLIST
316900000000     C                   PARM                    DSMSMS
317000000000     C                   PARM                    DSMSPG
317100000000     C                   PARM                    DSMSKE
317200000000     C                   PARM                    DSMSER
317300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
317400000000     C                   MOVEL     'TNTL04R'     DSMSPG                         *NOME PGM CHIAMANTE
317500000000     C                   MOVEL     'TNTL04R'     VMCQUE                         *CODA MESSAGGI DESTI
317600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
317700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
317800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
317900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
318000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
318100000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
318200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
318300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
318400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
318500000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
318600971128     C*
318700971128     C* REPERIMENTO DATI UTENTE
318800020715
318900020715     c     *dtaara       define    §azute        azuteds
319000020715     c     *dtaara       define    §datiute      ddatiute
319100020715
319200020715     c                   in(E)     *dtaara
319300020715     c                   if        %Error or rsut = *blanks
319400020715     c                   clear                   tibs34ds
319500020715     c                   call      'TIBS34R'
319600020715     c                   parm                    Tibs34Ds
319700020715     c                   in        *dtaara
319800020715     c                   endif
319900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
320000000000     C                   BITOFF    '01234567'    POSMI1            1
320100000000     C                   BITOFF    '01234567'    POSMI2            1
320200000000     C                   BITOFF    '01234567'    POSMA1            1
320300000000     C                   BITOFF    '01234567'    POSMA2            1
320400000000     C                   BITON     '34'          POSMI1            1            *18
320500000000     C                   BITON     '7'           POSMI2            1            *01
320600000000     C                   BITON     '34'          POSMA1            1            *18
320700000000     C                   BITON     '0'           POSMA2            1            *80
320800000000     C                   MOVE      POSMI2        POSMIN            2
320900000000     C                   MOVEL     POSMI1        POSMIN
321000000000     C                   MOVE      POSMA2        POSMAX            2
321100000000     C                   MOVEL     POSMA1        POSMAX
321200000000     C                   ENDSR
321300000000     C*------------------------------------------------------------------------*
321400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
321500000000     C*------------------------------------------------------------------------*
321600000000     C     FUNSTD        BEGSR
321700000000     C                   SELECT
321800000000     C     KEY           WHENEQ    F3
321900000000     C                   EXSR      F03STD
322000000000     C     KEY           WHENEQ    F12
322100000000     C                   EXSR      F12STD
322200000000     C     KEY           WHENEQ    F24
322300000000     C                   EXSR      F24STD
322400000000     C                   OTHER
322500000000     C                   MOVEL     MSG(2)        $MSG
322600000000     C                   EXSR      CARMSG
322700000000     C                   ENDSL
322800000000     C                   ENDSR
322900000000     C*------------------------------------------------------------------------*
323000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
323100000000     C*------------------------------------------------------------------------*
323200000000     C     F03STD        BEGSR
323300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
323400000000     C                   MOVEL     '1'           $F03
323500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
323600000000     C                   MOVEL     '1'           $FINE
323700951212     C* PULISCE IL MESSAGGIO
323800951212     C                   MOVEL     *BLANKS       D00MSG
323900000000     C                   ENDSR
324000000000     C*------------------------------------------------------------------------*
324100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
324200000000     C*------------------------------------------------------------------------*
324300000000     C     F12STD        BEGSR
324400000000     C*
324500000000     C* SOLO LA PRIMA VOLTA MANDA UN MESSAGGIO DI CONTROLLO PER ESSERE SICURI DI
324600000000     C* VOLER TORNARE INDIETRO E NON REGISTRARE LE VARIAZIONI ESEGUITE FIN LI'
324700000000IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
324800000000     C     MSGF12        ANDEQ     '0'                                          *EMETTERE MESSAGGIO
324900000000     C                   MOVEL     MSG(70)       $MSG
325000000000     C                   EXSR      CARMSG
325100000000     C                   MOVEL     '1'           MSGF12                         *MESSAGGIO EMESSO
325200000000X   1C                   ELSE
325300000000     C*
325400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
325500000000     C                   MOVEL     '1'           $F12
325600000000     C* IMPOSTO FLAG DI FIN PGM
325700000000     C                   MOVEL     '1'           $FINE
325800951212     C* PULISCE IL MESSAGGIO
325900951212     C                   MOVEL     *BLANKS       D00MSG
326000000000E   1C                   ENDIF
326100000000     C*
326200000000     C                   ENDSR
326300000000     C*------------------------------------------------------------------------*
326400000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
326500000000     C*------------------------------------------------------------------------*
326600000000     C     F24STD        BEGSR
326700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
326800000000     C                   EXSR      OUTFUN
326900000000     C                   ENDSR
327000000000     C*------------------------------------------------------------------------*
327100000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
327200000000     C*------------------------------------------------------------------------*
327300000000     C     INZSFL        BEGSR
327400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
327500000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
327600000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
327700000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
327800000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
327900000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
328000000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
328100000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
328200000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
328300000000     C                   Z-ADD     09            SFLPAG
328400000000     C                   ENDSR
328500000000     C*------------------------------------------------------------------------*
328600000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
328700000000     C*------------------------------------------------------------------------*
328800000000     C     FUNSFL        BEGSR
328900000000     C                   SELECT
329000000000     C     KEY           WHENEQ    ENTER
329100000000     C                   EXSR      ENTSFL
329200000000     C     KEY           WHENEQ    F3
329300000000     C                   EXSR      F03STD
329400000000     C     KEY           WHENEQ    F5
329500000000     C                   EXSR      F05STD
329600000000     C     KEY           WHENEQ    F12
329700000000     C                   EXSR      F12STD
329800000000     C     KEY           WHENEQ    F17
329900000000     C                   EXSR      F17STD
330000000000     C     KEY           WHENEQ    F18
330100000000     C                   EXSR      F18STD
330200000000     C     KEY           WHENEQ    F23
330300000000     C                   EXSR      F23STD
330400000000     C     KEY           WHENEQ    F24
330500000000     C                   EXSR      F24STD
330600000000     C     KEY           WHENEQ    ROLLUP
330700000000     C                   EXSR      RUPSTD
330800000000     C     KEY           WHENEQ    ROLLDN
330900000000     C                   EXSR      RDNSTD
331000000000     C                   OTHER
331100000000     C                   MOVEL     MSG(2)        $MSG
331200000000     C                   EXSR      CARMSG
331300000000     C                   ENDSL
331400000000     C                   ENDSR
331500000000     C*------------------------------------------------------------------------*
331600000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
331700000000     C*------------------------------------------------------------------------*
331800000000     C     F05STD        BEGSR
331900000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
332000000000     C                   EXSR      POSSFL
332100000000     C                   ENDSR
332200000000     C*------------------------------------------------------------------------*
332300000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
332400000000     C*------------------------------------------------------------------------*
332500000000     C     F17STD        BEGSR
332600000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
332700000000     C                   Z-ADD     1             VCCNBR
332800000000     C                   ENDSR
332900000000     C*------------------------------------------------------------------------*
333000000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
333100000000     C*------------------------------------------------------------------------*
333200000000     C     F18STD        BEGSR
333300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
333400000000     C                   Z-ADD     SFLULT        VCCNBR
333500000000     C                   ENDSR
333600000000     C*------------------------------------------------------------------------*
333700000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
333800000000     C*------------------------------------------------------------------------*
333900000000     C     F23STD        BEGSR
334000000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
334100000000     C                   EXSR      OUTOPZ
334200000000     C                   ENDSR
334300000000     C*------------------------------------------------------------------------*
334400000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
334500000000     C*------------------------------------------------------------------------*
334600000000     C     RUPSTD        BEGSR
334700000000     C* SE IL CURSORE E' SULLA RIGA 18 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
334800000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
334900000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
335000000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
335100000000     C                   ELSE
335200000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
335300000000     C                   ENDIF
335400000000     C                   ENDSR
335500000000     C*------------------------------------------------------------------------*
335600000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
335700000000     C*------------------------------------------------------------------------*
335800000000     C     RDNSTD        BEGSR
335900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
336000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
336100000000     C     CURSOR        IFGE      POSMIN
336200000000     C     CURSOR        ANDLE     POSMAX
336300000000     C                   ELSE
336400000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
336500000000     C                   MOVEL     '1'           $ERR
336600000000     C                   MOVEL     MSG(5)        $MSG
336700000000     C                   EXSR      CARMSG
336800000000     C                   ENDIF
336900000000     C                   ENDSR
337000000000     C*------------------------------------------------------------------------*
337100000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
337200000000     C*------------------------------------------------------------------------*
337300000000     C     ENTSFL        BEGSR
337400000000     C*****
337500000000     C* GESTIONE NON STANDARD
337600000000     C*****
337700000000     C* CONTROLLA OPZIONI
337800000000IF  1C     SFLULT        IFNE      0
337900000000     C                   EXSR      OPZSFL
338000000000E   1C                   ENDIF
338100000000     C                   MOVEL     '0'           $ERR
338200000000     C                   EXSR      CARGIO
338300000000IF  1C     $ERR          IFEQ      '1'
338400000000     C                   Z-ADD     WNBR          VCCNBR
338500000000     C     VCCNBR        CHAIN     TL04DVS                            99
338600000000IF  2C     *IN99         IFEQ      *OFF
338700000000     C                   MOVEL     '1'           *IN46
338800000000     C                   UPDATE    TL04DVS
338900000000     C                   EXSR      CARMSG
339000000000E   2C                   ENDIF
339100000000X   1C                   ELSE
339200000000     C                   EXSR      POSSFL
339300000000E   1C                   ENDIF
339400000000     C*
339500000000     C                   ENDSR
339600000000     C*------------------------------------------------------------------------*
339700000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
339800000000     C*------------------------------------------------------------------------*
339900000000     C     POSSFL        BEGSR
340000000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
340100000000     C                   EXSR      CLRSFL
340200000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
340300000000     C                   EXSR      SETFIL
340400000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
340500000000     C     SFLEOF        IFEQ      0
340600000000     C                   EXSR      CARSFL
340700000000     C                   ENDIF
340800000000     C                   ENDSR
340900000000     C*------------------------------------------------------------------------*
341000000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
341100000000     C*------------------------------------------------------------------------*
341200000000     C     OPZSFL        BEGSR
341300000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
341400000000     C                   MOVEL     'S'           OPZOK             1
341500000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
341600000000     C                   Z-ADD     0             SFLVAR            5 0
341700000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
341800000000     C                   Z-ADD     0             SFLERR
341900000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
342000000000     C                   Z-ADD     SFLULT        WFLULT
342100000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
342200000000     C                   READC     TL04DVS                                49
342300000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
342400000000     C     $ERR          ANDEQ     '0'
342500000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
342600000000     C                   ADD       1             SFLVAR
342700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
342800000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
342900000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
343000000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
343100000000     C                   EXSR      CONOPZ
343200000000     C     OPZOK         IFEQ      'S'
343300000000     C     $ERR          ANDEQ     '0'
343400000000     C                   EXSR      GESOPZ
343500000000     C                   ENDIF
343600000000     C                   ENDIF                                                  <-- 3 ---
343700000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
343800000000     C*            2. PULIZIA SCELTA IN SFL
343900000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
344000000000     C                   MOVEL     '0'           *IN41
344100000000     C                   MOVEL     *BLANKS       VSCOPZ
344200000000     C                   ELSE                                                   <-- 3 -->
344300000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
344400000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
344500000000     C                   MOVEL     '1'           *IN41
344600000000     C                   Z-ADD     SFLULT        SFLERR
344700000000     C                   ENDIF                                                  <-- 3 ---
344800000000     C                   ELSE                                                   <-- 2 -->
344900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
345000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
345100000000     C                   MOVEL     '1'           *IN41
345200000000     C                   ENDIF                                                  <-- 2 ---
345300000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
345400000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
345500000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
345600000000     C     OPZOK         IFNE      'S'
345700000000     C                   Z-ADD     SFLERR        VCCNBR
345800000000     C                   ELSE
345900000000     C                   Z-ADD     SFLULT        VCCNBR
346000000000     C                   ENDIF
346100000000     C*--- AGGIORNO RECORD SFL
346200000000     C*---                 UPDATTL04DVS
346300000000     C     $ERR          IFEQ      '0'
346400000000     C                   READC     TL04DVS                                49
346500000000     C                   ENDIF
346600000000     C                   ENDDO                                                  <-- 1 ---
346700000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
346800000000     C                   Z-ADD     WFLULT        SFLULT
346900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
347000000000     C     SFLVAR        IFEQ      0
347100000000     C*---                 MOVEL'1'       $FINE
347200000000     C                   ENDIF
347300000000     C                   ENDSR
347400000000     C*------------------------------------------------------------------------*
347500000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
347600000000     C*------------------------------------------------------------------------*
347700000000     C     CLRSFL        BEGSR
347800000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
347900000000     C* (0=NON FINE FILE  1=FINE FILE)
348000000000     C                   Z-ADD     0             SFLEOF
348100000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
348200000000     C                   Z-ADD     0             SFLULT
348300000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
348400000000     C                   MOVEL     '1'           *IN40
348500000000     C                   WRITE     TL04DVC
348600000000     C                   MOVEL     '0'           *IN40
348700110525     c                   clear                   wVisualTLS
348800000000     C                   ENDSR
348900000000     C*------------------------------------------------------------------------*
349000000000     C* CARSFL - CARICAMENTO SFL
349100000000     C*------------------------------------------------------------------------*
349200000000     C     CARSFL        BEGSR
349300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
349400000000     C     SFLULT        IFEQ      0
349500000000     C                   EXSR      SETFIL
349600000000     C                   ENDIF
349700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
349800000000     C                   Z-ADD     SFLULT        WFLULT
349900000000     C* CICLO:
350000000000     C*---  CARICO NUMERO DI RIGHE=SFLPAG
350100000000     C*     SFLEOF=0 (NON FINE FILE "FISICO")
350200000000     C                   Z-ADD     1             SFLRIG
350300000000     C*---       SFLRIG    DOWLESFLPAG                     --- 1 -->
350400000000     C*---       SFLEOF    ANDEQ0
350500000000     C     SFLEOF        DOWEQ     0
350600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
350700000000     C                   MOVEL     *BLANKS       VSCOPZ
350800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
350900000000     C                   EXSR      IRESFL
351000000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
351100000000     C                   ADD       1             SFLULT
351200000000     C* SCRITTURA RECORD SFL
351300000000     C                   WRITE     TL04DVS
351400000000     C                   ADD       1             SFLRIG
351500000000     C* LETTURA FILE DA CARICARE
351600000000     C                   EXSR      LETFIL
351700000000     C                   ENDDO                                                  <-- 1 ---
351800110525     c* se non acnora memoriizzato punto di scambio lo faccio ora (ERRATO!!!)
351900110525    1c                   if        westls='S'   and wVisualTLS=' '
352000110525     c                   exsr      sflTLS
352100110525     c                   eval      wVisualTLS='S'
352200110525     c                   endif
352300110525     c
352400000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
352500000000     C     WFLULT        IFNE      SFLULT
352600000000     C                   Z-ADD     WFLULT        VCCNBR
352700000000     C                   ADD       1             VCCNBR
352800000000     C                   ENDIF
352900000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
353000000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
353100000000     C     SFLULT        ANDGT     *ZEROS
353200000000     C                   EXSR      CARMSG
353300000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
353400000000     C     SFLULT        CHAIN     TL04DVS                            99
353500000000     C     *IN99         IFEQ      '0'
353600000000     C                   SETON                                        45
353700110526     c                   if        vsctfm='S'
353800110526     C                   SETON                                        80
353900110526     c                   endif
354000000000     C                   UPDATE    TL04DVS
354100110526     C                   SETOFF                                       4580
354200000000     C                   ENDIF
354300000000     C                   ENDIF                                                  <-- 1 ---
354400000000     C                   ENDSR
354500000000     C*------------------------------------------------------------------------*
354600000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
354700000000     C*------------------------------------------------------------------------*
354800000000     C     OUTSFL        BEGSR
354900000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
355000000000     C     MSGULT        IFNE      *ZEROS
355100000000     C                   EXSR      OUTMSG
355200000000     C                   ENDIF
355300000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
355400000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
355500000000     C     SFLEOF        IFEQ      0
355600000000     C                   MOVEL     '0'           *IN42
355700000000     C                   ELSE
355800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
355900000000     C                   MOVEL     '1'           *IN42
356000000000     C                   ENDIF
356100000000     C* EMISSIONE SUBFILE
356200000000     C     SFLULT        IFLE      0
356300000000     C*  --> CASO DI SFL VUOTO
356400000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
356500000000     C                   WRITE     TL04DVV                                      *'NON VI SONO DATI'
356600000000     C                   WRITE     TL04DVC                                      *SFL CONTROL
356700000000     C                   READ      TL04DVC                                99    *SFL CONTROL
356800000000     C                   ELSE
356900000000     C*  --> CASO DI SFL CON RECORDS
357000000000     C                   WRITE     TL04DVC                                      *SFL CONTROL
357100000000     C                   READ      TL04DVC                                99    *SFL CONTROL
357200000000     C                   ENDIF
357300000000     C*
357400000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
357500000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
357600000000     C                   Z-ADD     FRTSFL        VCCNBR
357700000000     C                   ENDSR
357800000000     C*------------------------------------------------------------------------*
357900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
358000000000     C*          CODA MESSAGGI DEL PGM
358100000000     C*------------------------------------------------------------------------*
358200000000     C     CARMSG        BEGSR
358300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
358400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
358500000000     C     MSGULT        IFEQ      0
358600000000     C                   MOVEL     '1'           *IN40
358700000000     C                   WRITE     TL04DVK
358800000000     C                   MOVEL     '0'           *IN40
358900000000     C                   ENDIF
359000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
359100000000     C                   MOVEL     $MSG          DSMSMS
359200000000     C                   CALL      'XMSG'        XMSGPA
359300000000     C     DSMSER        IFEQ      0
359400000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
359500000000     C                   ENDIF
359600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
359700000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
359800000000     C                   WRITE     TL04DVM                                      *SFL MESS
359900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
360000000000     C                   MOVEL     *BLANKS       $MSG
360100000000     C                   ENDSR
360200000000     C*------------------------------------------------------------------------*
360300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
360400000000     C*------------------------------------------------------------------------*
360500000000     C     OUTMSG        BEGSR
360600000000     C* VISUALIZZAZIONE MESSAGGIO
360700000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
360800000000     C                   WRITE     TL04DVK                                      *CONTR MESS
360900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
361000000000     C                   Z-ADD     *ZEROS        MSGULT
361100000000     C                   ENDSR
361200000000     C*------------------------------------------------------------------------*
361300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
361400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
361500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
361600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
361700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
361800000000     C*------------------------------------------------------------------------*
361900000000     C     CARFUN        BEGSR
362000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
362100000000     C     1             DO        31            I                 5 0          --- 1 -->
362200000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
362300000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
362400000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
362500020715     c                   If        DutLpo = 'S'
362600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
362700000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
362800000000     C                   ELSE                                                   *INTERROG.
362900000000     C                   MOVEL     FUNASI        FAT(I)
363000000000     C                   ENDIF
363100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
363200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
363300000000     C                   MOVEL     FUNAFG        FAT(I)
363400000000     C                   ELSE                                                   *INTERROG.
363500000000     C                   MOVEL     FUNAFI        FAT(I)
363600000000     C                   ENDIF
363700000000     C                   ENDIF                                                  <-- 2 ---
363800000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
363900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
364000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
364100000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
364200000000     C                   ELSE
364300000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
364400000000     C                   ENDIF
364500000000     C                   ENDIF
364600000000     C                   ENDDO                                                  <-- 1 ---
364700000000     C                   ENDSR
364800000000     C*------------------------------------------------------------------------*
364900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
365000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
365100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
365200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
365300000000     C*------------------------------------------------------------------------*
365400000000     C     OUTFUN        BEGSR
365500000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
365600000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
365700000000     C                   ENDDO
365800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
365900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
366000000000     C     1             DO        31            I
366100000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
366200000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
366300000000     C                   ADD       1             FUNNAT
366400000000     C                   ENDIF
366500000000     C                   ENDDO
366600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
366700000000     C     FUNNAT        IFGT      8
366800000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
366900000000     C                   ENDIF
367000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
367100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
367200000000     C                   Z-ADD     8             FUNVIS            5 0
367300000000     C     FAT(23)       IFEQ      'S'
367400000000     C                   SUB       1             FUNVIS
367500000000     C                   ENDIF
367600000000     C     FAT(24)       IFEQ      'S'
367700000000     C                   SUB       1             FUNVIS
367800000000     C                   ENDIF
367900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
368000000000     C                   Z-ADD     *ZEROS        W                 5 0
368100000000     C                   ADD       1             FUNULT
368200000000     C     FUNULT        IFGT      31
368300000000     C                   Z-ADD     1             FUNULT
368400000000     C                   ENDIF
368500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
368600000000     C     FUNULT        DO        31            I                              --- 1 -->
368700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
368800000000     C     W             IFLT      FUNVIS                                       --- 2 -->
368900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
369000000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
369100000000     C     FDE(I)        ANDNE     *BLANKS
369200000000     C     I             IFEQ      23                                           *TOLGO F23
369300000000     C     I             OREQ      24                                           *E F24
369400000000     C                   ELSE
369500000000     C                   ADD       1             W
369600000000     C                   MOVEL     FDE(I)        FUV(W)
369700000000     C                   ENDIF
369800000000     C                   ENDIF                                                  <-- 3 ---
369900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
370000000000     C                   Z-ADD     I             FUNULT
370100000000     C                   ENDIF                                                  <-- 2 ---
370200000000     C                   ENDDO                                                  <-- 1 ---
370300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
370400000000     C                   MOVEL     FUV(1)        VFD001
370500000000     C                   MOVEL     FUV(2)        VFD002
370600000000     C                   MOVEL     FUV(3)        VFD003
370700000000     C                   MOVEL     FUV(4)        VFD004
370800000000     C                   MOVEL     FUV(5)        VFD005
370900000000     C                   MOVEL     FUV(6)        VFD006
371000000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
371100000000     C     FAT(24)       ANDEQ     'N'
371200000000     C                   MOVEL     FUV(7)        VFD007
371300000000     C                   MOVEL     FUV(8)        VFD008
371400000000     C                   ELSE                                                   <-- 1 -->
371500000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
371600000000     C     FAT(24)       ANDEQ     'S'
371700000000     C                   MOVEL     FDE(23)       VFD007
371800000000     C                   MOVEL     FDE(24)       VFD008
371900000000     C                   ELSE                                                   <-- 2 -->
372000000000     C                   MOVEL     FUV(7)        VFD007
372100000000     C     FAT(23)       IFEQ      'S'
372200000000     C                   MOVEL     FDE(23)       VFD008
372300000000     C                   ELSE
372400000000     C                   MOVEL     FDE(24)       VFD008
372500000000     C                   ENDIF
372600000000     C                   ENDIF                                                  <-- 2 ---
372700000000     C                   ENDIF                                                  <-- 1 ---
372800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
372900000000     C                   ENDSR
373000000000     C*------------------------------------------------------------------------*
373100000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
373200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
373300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
373400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
373500000000     C*------------------------------------------------------------------------*
373600000000     C     CONFUN        BEGSR
373700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
373800000000     C     1             DO        31            I                              --- 1 -->
373900000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
374000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
374100000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
374200000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
374300000000     C                   EXSR      CARMSG
374400000000     C                   MOVEL     '1'           $ERR
374500000000     C                   ENDIF                                                  <-- 3 ---
374600000000     C                   ENDIF                                                  <-- 2 ---
374700000000     C                   ENDDO                                                  <-- 1 ---
374800000000     C                   ENDSR
374900000000     C*------------------------------------------------------------------------*
375000000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
375100000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
375200000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
375300000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
375400000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
375500000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
375600000000     C*------------------------------------------------------------------------*
375700000000     C     CAROPZ        BEGSR
375800000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
375900000000     C     1             DO        24            I                              --- 1 -->
376000000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
376100000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
376200000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
376300000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
376400020715     c                   If        DutLpo = 'S'
376500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
376600000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
376700000000     C                   ELSE                                                   *INTERROG.
376800000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
376900000000     C                   ENDIF
377000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
377100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
377200000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
377300000000     C                   ELSE                                                   *INTERROG.
377400000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
377500000000     C                   ENDIF
377600000000     C                   ENDIF                                                  <-- 2 ---
377700000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
377800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
377900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
378000000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
378100000000     C                   ELSE
378200000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
378300000000     C                   ENDIF
378400000000     C                   ENDIF
378500000000     C                   ENDDO                                                  <-- 1 ---
378600000000     C                   ENDSR
378700000000     C*------------------------------------------------------------------------*
378800000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
378900000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
379000000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
379100000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
379200000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
379300000000     C*------------------------------------------------------------------------*
379400000000     C     OUTOPZ        BEGSR
379500000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
379600000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
379700000000     C                   ENDDO
379800000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
379900000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
380000000000     C                   ADD       1             OPZULT
380100000000     C     OPZULT        IFGT      24
380200000000     C                   Z-ADD     1             OPZULT
380300000000     C                   END
380400000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
380500000000     C     1             DO        24            I
380600000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
380700000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
380800000000     C                   ADD       1             OPZNAT
380900000000     C                   ENDIF
381000000000     C                   ENDDO
381100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
381200000000     C                   Z-ADD     0             W
381300000000     C     OPZULT        DO        24            I                              --- 1 -->
381400000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
381500000000     C     W             IFLT      8                                            --- 2 -->
381600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
381700000000     C     OAT(I)        IFEQ      'S'
381800000000     C     ODE(I)        ANDNE     *BLANKS
381900000000     C                   ADD       1             W
382000000000     C                   MOVEL     ODE(I)        OPV(W)
382100000000     C                   ENDIF
382200000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
382300000000     C                   Z-ADD     I             OPZULT
382400000000     C                   ENDIF                                                  <-- 2 ---
382500000000     C                   ENDDO                                                  <-- 1 ---
382600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
382700000000     C                   MOVEL     OPV(1)        VOD001
382800000000     C                   MOVEL     OPV(2)        VOD002
382900000000     C                   MOVEL     OPV(3)        VOD003
383000000000     C                   MOVEL     OPV(4)        VOD004
383100000000     C                   MOVEL     OPV(5)        VOD005
383200000000     C                   MOVEL     OPV(6)        VOD006
383300000000     C                   MOVEL     OPV(7)        VOD007
383400000000     C                   MOVEL     OPV(8)        VOD008
383500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
383600000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
383700000000     C     OPZNAT        IFGT      8
383800000000     C                   MOVEL     'S'           FAT(23)
383900000000     C                   ENDIF
384000000000     C                   ENDSR
384100000000     C*------------------------------------------------------------------------*
384200000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
384300000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
384400000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
384500000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
384600000000     C*------------------------------------------------------------------------*
384700000000     C     CONOPZ        BEGSR
384800000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
384900000000     C                   MOVEL     *BLANKS       OPZOK             1
385000000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
385100000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
385200000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
385300000000     C     OAT(I)        IFEQ      'S'
385400000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
385500000000     C                   MOVEL     'S'           OPZOK
385600000000     C                   ELSE
385700000000     C                   MOVEL     'N'           OPZOK             1
385800000000     C                   ENDIF
385900000000     C                   ENDIF                                                  <-- 2 ---
386000000000     C                   ENDDO                                                  <-- 1 ---
386100000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
386200000000     C     OPZOK         IFNE      'S'
386300000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
386400000000     C                   EXSR      CARMSG
386500000000     C                   MOVEL     '1'           $ERR
386600000000     C                   MOVEL     'N'           OPZOK             1
386700000000     C                   ENDIF
386800000000     C                   ENDSR
386900000000     C*------------------------------------------------------------------------*
387000000000** - MSG MESSAGGI                                                          .
387100000000TNTL04R- Tasto funzionale non valido.                                          1
387200000000TNTL04R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
387300000000TNTL04R- Opzione non valida.                                                   3
387400000000TNTL04R- Opzione abilitata ma non gestita. Avvertire CED.                      4
387500000000TNTL04R- Inizio elenco.                                                        5
387600000000TNTL04R- Fine elenco.                                                          6
387700000000TNTL04R- ULTIMA FERMATA RITORNO. Deve essere = filiale part. traino   Seq999   7
387800000000TNTL04R- ULTIMA FERMATA. Deve essere di R-Ritorno se traino chiuso    Seq999   8
387900000000TNTL04R- PRIMA FERMATA. Deve essere di Andata                         Seq999   9
388000000000TNTL04R- PRIMA FERMATA. Deve essere = filiale part. traino            Seq999  10
388100000000TNTL04R- ULTIMA FERMATA ANDATA. Deve essere = filiale arrivo traino   Seq999  11
388200000000TNTL04R- Se il traino è chiuso inserire fermate di Andata e Ritorno           12
388300000000TNTL04R- PRIMA FERMATA. Lasciare vuoti i dati arrivo, km e tempo      Seq999  13
388400000000TNTL04R- ULTIMA FERMATA. Lasciare vuoti i dati partenza               Seq999  14
388500951211*---   - Inserire giorno, ora partenza e carico                       Seq999  15
388600000000TNTL04R- Tipo fermata errato. Inserire A-Andata o R-Ritorno           Seq999  16
388700000000TNTL04R- Se il traino non è chiuso inserire solo fermate di A-Andata          17
388800000000TNTL04R- Giorno arrivo errato. Inserire A,B,C,...                     Seq999  18
388900000000TNTL04R- Giorno partenza errato. Inserire A,B,C,...                   Seq999  19
389000000000TNTL04R- Ora arrivo errata (hh:mm)                                    Seq999  20
389100000000TNTL04R- Ora partenza errata (hh:mm)                                  Seq999  21
389200000000TNTL04R- Inserire "S" solo se la merce viene scaricata                Seq999  22
389300000000TNTL04R- Inserire "S" solo se la merce viene caricata                 Seq999  23
389400000000TNTL04R- L'orario del gg non si può cancellare perchè non ve ne sono altri    24
389500000000TNTL04R- Tempo fermata errato (hh:mm)                                 Seq999  25
389600951213TNTL04R- NON LOCALE. La stessa fermata non può essere ripetuta        Seq999  26
389700000000TNTL04R- Se inserito il carico='S' inserire anche GG e ORA, e vicev.  Seq999  27
389800000000TNTL04R- Se inserito lo scarico='S' inserire anche GG e ORA, e vicev. Seq999  28
389900000000TNTL04R- Giorno apertura foglio viaggio errato. Inserire A,B,C,...    Seq999  29
390000000000TNTL04R- Il gg partenza deve essere maggiore/uguale gg apertura FV    Seq999  30
390100000000TNTL04R- Il primo giorno di apertura FV deve essere uguale a 'A'      Seq999  31
390200000000TNTL04R- Giorno apertura FV errato perchè non segue ordine ascendente Seq999  32
390300000000TNTL04R- PRIMA FERMATA. Inserire i dati partenza                      Seq999  33
390400000000TNTL04R- ULTIMA FERMATA. Inserire i dati arrivo                       Seq999  34
390500000000TNTL04R- ULTIMA FERMATA. Deve essere di Andata se traino non chiuso   Seq999  35
390600960206TNTL04R- INOLTRO: La fil deve essere term arr generico o della part.  Seq999  36
390700000000TNTL04R- Le fermate ed i relativi orari sono stati registrati su file         37
390800000000TNTL04R- Una fermata di Ritorno non può precedere una di Andata       Seq999  38
390900000000TNTL04R- Superato il numero massimo di fermate per traino. Avvertire il CED.  39
391000000000TNTL04R- Una fermata di Andata non puo' seguire una di Ritorno        Seq999  40
391100000000TNTL04R- AFFLUENZA: Solo filiali di 2°liv, tranne l'ultima fermata    Seq999  41
391200000000TNTL04R- AFFLUENZA: Il term part della fil deve essere=fil arr traino Seq999  42
391300000000TNTL04R- AFFLUENZA: Solo fermate di carico, tranne l'ultima           Seq999  43
391400000000TNTL04R- CHIUSO e SOLO AND: Le filiali di carico devono essere 1°liv. Seq999  44
391500000000TNTL04R- DEFLUENZA: Solo fermate di scarico, tranne la prima          Seq999  45
391600000000TNTL04R- DEFLUENZA: Il term arr della fil deve essere=fil part traino Seq999  46
391700000000TNTL04R- DEFLUENZA: Non c'è defluenza per la filiale partenza traino  Seq999  47
391800000000TNTL04R- CHIUSO e SOLO AND: Le fil di scarico devono essere term.arr. Seq999  48
391900000000TNTL04R- CHIUSO: Ultima fermata andata deve essere=fil arrivo traino  Seq999  49
392000000000TNTL04R- CHIUSO: Prima fermata ritorno deve essere=fil arrivo traino  Seq999  50
392100000000TNTL04R- ULTIMA FERMATA ANDATA. Lasciare vuoti i dati partenza        Seq999  51
392200000000TNTL04R- PRIMA FERMATA RITORNO. Lasciare vuoti dati arrivo,km e tempo Seq999  52
392300951218TNTL04R- GG+ORA partenza deve essere > GG+ORA partenza precedente     Seq999  53
392400000000TNTL04R- CHIUSO: L'ultima fermata di andata deve essere di scarico    Seq999  54
392500000000TNTL04R- CHIUSO: La prima fermata di ritorno deve essere di carico    Seq999  55
392600000000TNTL04R- GG+ORA partenza deve essere > GG+ORA arrivo                  Seq999  56
392700000000TNTL04R- Orario incompleto.                                           Seq999  57
392800000000TNTL04R- Inserire gli orari per tutti i giorni previsti dal traino            58
392900000000TNTL04R- Inserire i dati da registrare                                        59
393000000000TNTL04R- Inserire il gg apertura FV solo se la fermata è di carico    Seq999  60
393100000000TNTL04R- GG+ORA arrivo deve essere > GG+ORA partenza precedente       Seq999  61
393200000000TNTL04R- L'orario di xxxxxxxxxxxxxxxx è stato cancellato                      62
393300951212TNTL04R- Sono presenti degli errori.Cambiando gg verranno perse le modifiche  63
393400951218TNTL04R- GG+ORA arrivo deve essere > GG+ORA arrivo precedente         Seq999  64
393500960717TNTL04R- La filiale deve essere di solo scarico se non è term.arrivo  Seq999  65
393600090115TNTL04R- KM mancanti                                                  Seq999  66
393700000000TNTL04R- 67
393800000000TNTL04R- 68
393900000000TNTL04R- 69
394000000000Ritorno senza aggiornamento: premere nuovamente F12 per confermare l'uscita   70
394100000000** - OPZ OPZIONI          SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
394200000000- 1-1 -                   -    NN     NN
394300000000- 2-2 -2=Modifica         -    SN     SN
394400000000- 3-3 -                   -    NN     NN
394500000000- 4-4 -4=Cancellazione    -    SN     SN
394600000000- 5-5 -                   -    NN     NN
394700000000- 6-6 -                   -    NN     NN
394800000000- 7-7 -                   -    NN     NN
394900000000- 8-8 -                   -    NN     NN
395000000000- 9-9 -                   -    NN     NN
395100000000-10-10-                   -    NN     NN
395200000000-11-11-                   -    NN     NN
395300000000-12-12-                   -    NN     NN
395400000000-13-  -                   -    NN     NN
395500000000-14-  -                   -    NN     NN
395600000000-15-  -                   -    NN     NN
395700000000-16-  -                   -    NN     NN
395800000000-17-  -                   -    NN     NN
395900000000-18-  -                   -    NN     NN
396000000000-19-  -                   -    NN     NN
396100000000-20-  -                   -    NN     NN
396200000000-21-  -                   -    NN     NN
396300000000-22-  -                   -    NN     NN
396400000000-23-  -                   -    NN     NN
396500000000-24-  -                   -    NN     NN
396600000000** - FUN2 FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
396700000000F1    -                   -    NN     NN
396800110610F2    -F2=Punti di Scambio-    SS     SS
396900000000F3    -F3=Fine            -    NN     NN
397000000000F4    -                   -    NN     NN
397100000000F5    -                   -    NN     NN
397200000000F6    -F6=Conferma        -    SN     SN
397300000000F7    -F7=Cancella orario -    SN     SN
397400000000F8    -                   -    NN     NN
397500000000F9    -F9=Cambia giorno   -    SS     SS
397600000000F10   -F10=Inserimento    -    SN     SN
397700000000F11   -                   -    NN     NN
397800000000F12   -F12=Ritorno        -    SS     SS
397900000000F13   -                   -    NN     NN
398000000000F14   -                   -    NN     NN
398100000000F15   -                   -    NN     NN
398200000000F16   -                   -    NN     NN
398300000000F17   -                   -    NN     NN
398400000000F18   -                   -    NN     NN
398500000000F19   -                   -    NN     NN
398600000000F20   -                   -    NN     NN
398700000000F21   -                   -    NN     NN
398800000000F22   -                   -    NN     NN
398900000000F23   -F23=Altre opzioni  -    NN     NN
399000000000F24   -F24=Altri tasti    -    NN     NN
399100000000ENTER -                   -    SS     SS
399200000000ROLLUP-                   -    SS     SS
399300000000ROLLDN-                   -    SS     SS
399400000000HELP  -                   -    NN     NN
399500000000PRINT -                   -    NN     NN
399600000000BSPACE-                   -    NN     NN
399700000000CLEAR -                   -    NN     NN
399800000000** -DGIO- DESCRIZIONE GIORNI DELLA SETTIMANA
399900000000LUN  1
400000000000MAR  2
400100000000MER  3
400200000000GIO  4
400300000000VEN  5
400400000000SAB  6
400500000000DOM  7
400600000000TUTTI8
400700000000** -DGES- DESCRIZIONE GIORNI DELLA SETTIMANA ESTESA
400800000000LUNEDI'         1
400900000000MARTEDI'        2
401000000000MERCOLEDI'      3
401100000000GIOVEDI'        4
401200000000VENERDI'        5
401300000000SABATO          6
401400000000DOMENICA        7
401500000000"TUTTI I GIORNI"8
401600000000** -DALF- ALFABETO
401700000000ABCDEFGHILMNOPQRSTUVZ
