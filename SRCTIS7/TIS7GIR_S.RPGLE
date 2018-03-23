000100970526     H*------------------------------------------------------------------------*
000200991115     H* Gestione file di log TIVLT00F - Strategi Download
000300970526     H*------------------------------------------------------------------------*
000400101005     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991115     FTIS7GID   CF   E             WORKSTN INFDS(DEVDS1)
001000991115     F                                     SFILE(s7GID05:SFLULT)
001100991115     F                                     SFILE(s7GIDMS:MSGULT)
001200970526     F*------------
001300991115     Ftivlt03l  IF   E           K DISK
001400991117     Ftivlt04l  IF   E           K DISK
001500991117     F                                     RENAME(tivlt000:tivltxxx)
001600140606     Ftivlt01l  IF   E           K DISK    usropn
001700140606     F                                     RENAME(tivlt000:tivlt01)
001800991108     Ftivir02l  IF   E           K DISK
001900991108     Ftivtf01l  IF   E           K DISK
002000991110     Ftntbe02l  IF   E           K DISK
002100000728     Ftianv01l  IF   E           K DISK
002200050607     Ftivgd02l  IF   E           K DISK
002300060413     Ftivgd01l  IF   E           K DISK    rename(tivgd000:tivgd001)
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
004200010316     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
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
005600991108     D* CODICI DI STATO DEI RECORD DI LOG
005700991108     D*------------
005800991115     D STS             S              1    DIM(10) CTDATA PERRCD(1)
005900991108     D*------------
006000991108     D* DESCRIZIONI STATO DEI RECORD DI LOG
006100991108     D*------------
006200991112     D DES             S              5    DIM(10) CTDATA PERRCD(1)
006300970526     D*------------------------------------------------------------------------*
006400970526     D* INPUT
006500970526     D*------------------------------------------------------------------------*
006600970526     D*------------
006700970526     D* RIDEFINIZIONE OPZIONI VIDEO
006800970526     D*------------
006900970526     D OPDDS           DS
007000970526     D  OPDOPZ                 1      2
007100970526     D  OPDDE1                 3     19
007200970526     D  OPDOP1                21     22
007300970526     D  OPDFI0                23     34
007400970526     D  OPDATT                35     35
007500970526     D  OPDFI1                36     80
007600970526     D  OPDDES                 1     19
007700970526     D  OPDRIG                 1     80
007800970526     D*------------
007900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
008000970526     D*------------
008100970526     D FUDDS           DS
008200970526     D  FUDDE1                 1      1
008300970526     D  FUDFUN                 2      3
008400970526     D  FUDDE2                 4     19
008500970526     D  FUDFI0                20     34
008600970526     D  FUDATT                35     35
008700970526     D  FUDFI1                36     80
008800970526     D  FUDDES                 1     19
008900970526     D  FUDRIG                 1     80
009000970526     D*------------
009100970526     D* DS GESTIONE TASTI FUNZIONALI
009200970526     D*------------
009300970526     D DSKEY         E DS
009400970526     D DEVDS1          DS
009500970526     D  KEY                  369    369
009600970526     D*------------
009700970526     D* DS "XMSG" - GESTIONE MESSAGGI
009800970526     D*------------
009900970526     D DSMSG         E DS
010000970526     D*------------
010100020916     D* DS REPERIMENTO DATI UTENTE
010200970526     D*------------
010300020916     D TIBS34DS      E DS                                                       *Profili utente
010400020916     D DDATIUTE      E DS                                                       *Dati utente
010500020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010600020218     D*------------
010700020218     D* DS ITF - TABELLA SCAMBIO TIPI FILE
010800020218     D*------------
010900020218     D DITF          E DS
011000060807     D*------------
011100060807     D* DS PROCEDURA DOWNLOAD GENERICO AUTOMATICO (TIVGD)
011200060807     D*------------
011300060807     D TIS781DS      E DS
011400080903     D TIS781DFLO    E DS
011500970527     D*------------------
011600991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
011700970527     D*------------------
011800970527     D WLBDA8          DS                  INZ
011900970527     D  G08DAT                 1      8  0
012000970527     D  G08INV                 9     16  0
012100970527     D  G08ERR                17     17
012200970527     D  G08TGI                18     22  0
012300970526     D*------------
012400991110     D* DS DI PROCEDURA
012500970526     D*------------
012600970526     D* ARCHITETTURA
012700970526     D*------------
012800970526     D KPJBA         E DS
012900991108     D*------------------
013000991108     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
013100991108     D*------------------
013200991108     D                 DS                  INZ
013300991108     D  GG                     1      2  0
013400991108     D  MM                     3      4  0
013500991108     D  AAAA                   5      8  0
013600991108     D  CHKDATA                1      8  0
013700991108     D*------------------
013800991108     D                 DS                  INZ
013900991108     D  AAINV                  1      4  0
014000991108     D  MMINV                  5      6  0
014100991108     D  GGINV                  7      8  0
014200991108     D  INVDATA                1      8  0
014300991110     D*------------------
014400991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
014500991110     D*------------------
014600991110     D                 DS                  INZ
014700991110     D  GGA                    1      2  0
014800991110     D  SEP1                   3      3
014900991110     D  MMA                    4      5  0
015000991110     D  SEP2                   6      6
015100991110     D  AAAAA                  7     10  0
015200991110     D  DATAA                  1     10
015300991110     D*------------------
015400991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
015500991110     D*------------------
015600991110     D                 DS                  INZ
015700991110     D  HH                     1      2  0
015800991110     D  PP                     3      4  0
015900991110     D  SS                     5      6  0
016000991110     D  ORAN                   1      6  0
016100991110     D*------------------
016200991110     D                 DS                  INZ
016300991110     D  HHA                    1      2  0
016400991110     D  SEP3                   3      3
016500991110     D  PPA                    4      5  0
016600991110     D  SEP4                   6      6
016700991110     D  SSA                    7      8  0
016800991110     D  ORAA                   1      8
016900991110     D*------------------
017000991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
017100991110     D*------------------
017200991110     D                 DS                  INZ
017300001031     D  MS1                    1     42
017400001031     D  MS2                   43     80
017500001031     D  MESS                   1     80
017600991108     D*------------------
017700991108     D* DS REPERIMENTO DATI UTENTE
017800991108     D*-------------------
017900991108     DBS69DS         E DS                  extname(TIBS69DS)
018000991108     DACODS          E DS                  extname(CNACO00F)
018100991108     DINDDS          E DS                  extname(CNIND00F)
018200991108     DCLPDS          E DS                  extname(CNCLP00F)
018300991108     DCLSDS          E DS                  extname(FNCLS00F)
018400991110     D*------------
018500991110     D* PASSAGGIO PARAMETRI X CLP
018600991110     D*------------
018700991110     D PARAM           DS                  INZ
018800991112     D  PLIB                   1     10
018900991112     D  POBJ                  11     20
019000991112     D  PMBR                  21     30
019100991112     D  PDAT                  31     38  0
019200991112     D  PORA                  39     44  0
019300991112     D  PKSC                  45     52
019400991115     D  PSTS                  53     53
019500991112     D  PTIP                  54     55
019600991112     D  PSQZ                  59     61  0
019700991112     D  POPZ                  62     63
019800000629     D  PFID                  64     73
019900000629     D  PMBD                  74     83
020000030519     D  PPRG                  84     93
020100050901     D  PFILO18               94    103
020200050901     D  PMBRO18              104    113
020300050901     D  PAORO18              114    114
020400051013     D*------------
020500051013     D* PASSAGGIO PARAMETRI IN INGRESSO
020600051013     D*------------
020700051013     D PAR_INP         DS
020800051013     D  DEPOP0                        3
020900051013     D  DEPKSC                        8
021000051013     D  DEPTIP                        2
021100000629     D*-----------------------
021200060807     D* VARIABILI DI WORK
021300000629     D*-----------------------
021400000629     D  STATO          S              1    INZ(*blanks)
021500000713     D  ESITO          S              1    INZ(*blanks)
021600000713     D  OPE            S              1    INZ(*zeros)
021700120105     D  SRC            S              1    INZ(*blanks)
021800060807     D  ESITO781       S              1    INZ(*blanks)
021900060807     D  PRG781         S              1    INZ(*blanks)
022000101005     D*-------------------
022100101005     D* DEFINIZIONI PROCEDURE ESTERNE
022200101005     D*-------------------
022300101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
022400101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
022500970526     C*------------------------------------------------------------------------*
022600970526     C* MAIN LINES
022700970526     C*------------------------------------------------------------------------*
022800970526     C*
022900970526     C* SE NON CI SONO ERRORI, PROSEGUE
023000970526IF  1C     ERRL00        IFNE      '1'
023100970526     C*
023200970526     C* CARICA IL SFILE
023300970526     C                   EXSR      POSSFL
023400970526     C*
023500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
023600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
023700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
023800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
023900970526     C*
024000970526     C* REIMPOSTA ERRORE DI PROCERURA
024100970526     C                   MOVEL     '0'           ERRL00
024200970526     C*
024300970526     C* EMETTE IL SFILE
024400970526     C                   EXSR      OUTSFL
024500970526     C*
024600970526     C* GESTIONE TASTI FUNZIONALI
024700970526     C                   EXSR      FUNCON
024800970526     C*
024900970526E   2C                   ENDDO
025000970527     C*
025100970526E   1C                   ENDIF
025200991108     C*
025300991108     C                   EXSR      FINSR
025400970526     C* FINE PGM
025500970526     C                   SETON                                        LR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     F03GES        BEGSR
026000970526     C*
026100970526     C* IMPOSTA "PREMUTO F03"
026200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026300970526     C*
026400970526     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
026700970526     C*------------------------------------------------------------------------*
026800970526     C     F05GES        BEGSR
026900970526     C*
027000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027100970526     C*
027200970526     C                   ENDSR
027300970526     C*------------------------------------------------------------------------*
027400970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
027500970526     C*------------------------------------------------------------------------*
027600970526     C     F06GES        BEGSR
027700970526     C*
027800991108     C* NON GESTITO
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200991108     C* F12GES - GESTIONE F12 -> RITORNO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     F12GES        BEGSR
028500970526     C*
028600970526     C* IMPOSTA "PREMUTO F12"
028700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028800970526     C*
028900970526     C                   ENDSR
029000050722     C*------------------------------------------------------------------------*
029100050722     C* F15GES - GESTIONE F15 -> FORZATIURA ELABORAZIONE FILE GENRICO DOWNLOAD (TIVGD)
029200050722     C*------------------------------------------------------------------------*
029300050722     C     F15GES        BEGSR
029400050722     C*
029500080904     C* Effettuo verifiche x correttezza parametri selezione
029600080904     C                   IF        VIDTI1 = *blanks AND
029700080904     C                             VIDKS1 = *blanks
029800080904     C                   SETON                                        8384
029900080904     C                   MOVEL     MSG(14)       DSMSMS
030000080904     C                   ELSE
030100080904     C*
030200080903     C                   CLEAR                   TIS781DS
030300080903     C                   CLEAR                   TIS781DFLO
030400080904     C                   IF        VIDTI1 = '**'
030500080904     C                   EVAL      §781FLOTFS = *blanks
030600080904     C                   ELSE
030700080903     C                   EVAL      §781FLOTFS = VIDTI1
030800080904     C                   ENDIF
030900080904     C                   IF        VIDKS1 = '********'
031000080904     C                   EVAL      §781KSU    = *blanks
031100080904     C                   ELSE
031200080904     C                   EVAL      §781KSU    = VIDKS1
031300080904     C                   ENDIF
031400080904     C                   EVAL      §781FLO    = TIS781DFLO
031500080903     C*
031600060807     C                   CALL      'TIS781C1'                                   *LANCIA PGM
031700060807     C                   PARM      *blanks       ESITO781
031800060807     C                   PARM                    TIS781DS
031900060807     C                   PARM      *blanks       PRG781
032000060905     C*
032100060905     C* Ricarico il sub-file
032200060905     C                   CLEAR                   S7GID06
032300060905     C                   EXSR      F05GES
032400080904     C*
032500080904     C                   ENDIF
032600050722     C*
032700050722     C                   ENDSR
032800101005     C*------------------------------------------------------------------------*
032900101005     C* F18GES - GESTIONE F18 -> FORZATIURA ELABORAZIONE BARTVAS
033000101005     C*------------------------------------------------------------------------*
033100101005     C     F18GES        BEGSR
033200101005     C*
033300101005     C* Effettuo verifiche x correttezza parametri selezione
033400101005     C                   IF        VIDTI1 = *blanks OR
033500101005     C                             VIDKS1 = *blanks
033600101005     C                   SETON                                        8384
033700101005     C                   MOVEL     MSG(14)       DSMSMS
033800101005     C                   ELSE
033900101005     C*
034000101005     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
034100101005     C                                           VIDKS1
034200101005     C                                          :VIDTI1)
034300101005     C*
034400101005     C* Ricarico il sub-file
034500101005     C                   CLEAR                   S7GID06
034600101005     C                   EXSR      F05GES
034700101005     C*
034800101005     C                   ENDIF
034900101005     C*
035000101005     C                   ENDSR
035100970528     C*------------------------------------------------------------------------*
035200970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
035300970528     C*------------------------------------------------------------------------*
035400970528     C     F21GES        BEGSR
035500970612     C*
035600991117     C* NON GESTITA
035700970528     C*
035800970528     C                   ENDSR
035900970528     C*------------------------------------------------------------------------*
036000970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
036100970528     C*------------------------------------------------------------------------*
036200970528     C     F22GES        BEGSR
036300970528     C*
036400970528     C                   Z-ADD     80            LENGH            15 5
036500970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
036600970528     C                   PARM                    CMD(1)
036700970528     C                   PARM                    LENGH
036800970528     C*
036900970528     C                   ENDSR
037000970526     C*------------------------------------------------------------------------*
037100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
037200970526     C*------------------------------------------------------------------------*
037300970526     C     F23GES        BEGSR
037400970526     C*
037500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
037600970526     C*
037700970526     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
038000970526     C*------------------------------------------------------------------------*
038100970526     C     F24GES        BEGSR
038200970526     C*
038300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
038400970526     C*
038500970526     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
038800970526     C*------------------------------------------------------------------------*
038900970526     C     RUPGES        BEGSR
039000970526     C*
039100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
039200970526     C*
039300970526     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500991108     C* ENTGES - GESTIONE ENTER -> INVIO
039600970526     C*------------------------------------------------------------------------*
039700970526     C     ENTGES        BEGSR
039800991108     C*
039900991108     C* CONTROLLO RAPIDO DATA (FORMATO GGMMAAAA) SE NO => ERRORE
040000991108     C*
040100991108     C                   Z-ADD     0             DATAERR           1 0
040200991108     C                   MOVEL     VIDdt2        CHKDATA
040300991108     C     CHKDATA       IFGT      0
040400991108     C     GG            IFLT      1
040500991108     C     GG            ORGT      31
040600991108     C                   Z-ADD     1             DATAERR
040700991108     C                   SETON                                        80
040800991108     C                   MOVEL     MSG(12)       DSMSMS
040900991108     C                   ENDIF
041000991108     C     MM            IFLT      1
041100991108     C     MM            ORGT      12
041200991108     C                   Z-ADD     1             DATAERR
041300991108     C                   SETON                                        80
041400991108     C                   MOVEL     MSG(12)       DSMSMS
041500991108     C                   ENDIF
041600991108     C                   ENDIF
041700991108     C*
041800991108     C                   MOVEL     VIDdt1        CHKDATA
041900991108     C     CHKDATA       IFGT      0
042000991108     C     GG            IFLT      1
042100991108     C     GG            ORGT      31
042200991108     C                   Z-ADD     1             DATAERR
042300991108     C                   SETON                                        82
042400991108     C                   MOVEL     MSG(12)       DSMSMS
042500991108     C                   ENDIF
042600991108     C     MM            IFLT      1
042700991108     C     MM            ORGT      12
042800991108     C                   Z-ADD     1             DATAERR
042900991108     C                   SETON                                        82
043000991108     C                   MOVEL     MSG(12)       DSMSMS
043100991108     C                   ENDIF
043200991108     C                   ENDIF
043300991108     C*
043400991108     C     DATAERR       IFEQ      0
043500991108     C*
043600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
043700970526     C                   EXSR      NEWPOS
043800970526     C*
043900991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
044000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044100970526IF  2C     SWPOSI        IFNE      0
044200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
044300970526X   2C                   ELSE
044400970526IF  3C     SFLULT        IFNE      0
044500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
044600970526     C*
044700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
044800970526IF  4C     SWMODI        IFNE      0
044900970526     C                   EXSR      POSSFL
045000970526E   4C                   ENDIF
045100970526E   3C                   ENDIF
045200970526E   2C                   ENDIF
045300970526E   1C                   ENDIF
045400970526     C*
045500991108     C                   ENDIF
045600991108     C*
045700970526     C                   ENDSR
045800970526     C*------------------------------------------------------------------------*
045900991112     C* O01GES - GESTIONE OPZIONE 1=ESECUZIONE PARZIALE
046000970526     C*------------------------------------------------------------------------*
046100970526     C     O01GES        BEGSR
046200970526     C*
046300000707     C     VIDSTS        IFEQ      '0'
046400000707     C                   MOVEL     MSG(13)       DSMSMS
046500000707     C                   ELSE
046600010131     C     V2HFLD        IFEQ      *blanks
046700000329     C     V2HMBD        OREQ      *blanks
046800001031     C     VIDISV        OREQ      *blanks
046900000328     C                   MOVEL     MSG(7)        DSMSMS
047000000328     C                   ELSE
047100991112     C                   CLEAR                   PARAM
047200991112     C                   EXSR      PASPAR
047300000706     C                   MOVEL     PARAM         KPJBU
047400050607     C*
047500050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
047600050607     C                   IF        wNewLOG = 'N'
047700991115     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
047800991112     C                   PARM                    KPJBA
047900000726     C                   PARM      *blanks       STATO
048000050607     C                   ELSE
048100050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
048200050607     C                   PARM                    KPJBA
048300050607     C                   PARM      *blanks       STATO
048400050607     C                   PARM                    VIHPRG
048500090910     C                   PARM                    VIDTIP
048600050607     C                   ENDIF
048700000328     C                   ENDIF
048800000707     C                   ENDIF
048900970526     C*
049000970526     C                   ENDSR
049100000328     C*------------------------------------------------------------------------*
049200050607     C* O11GES - GESTIONE OPZIONE 11=ESECUZIONE TOTALE  (WITH TRADUZIONE)
049300000328     C*------------------------------------------------------------------------*
049400000328     C     O11GES        BEGSR
049500000629     C*
049600010608     C     VIPKSC        IFEQ      '1809000'
049700010608     C     VIDTIP        ANDEQ     'VC'
049800010608     C                   ELSE
049900000707     C     VIDSTS        IFEQ      '0'
050000010326     C     VIHTIA        ANDNE     'F'
050100000707     C                   MOVEL     MSG(13)       DSMSMS
050200000707     C                   ELSE
050300000713     C* controllo se presenti i dati necessari e sufficienti
050400000713     C     VIDKSC        IFEQ      *blanks
050500000713     C     VIHKSU        OREQ      *blanks
050600000713     C     VIDTIP        OREQ      *blanks
050700000713     C     VIHPRG        OREQ      *blanks
050800000713     C                   MOVEL     MSG(8)        DSMSMS
050900000713     C                   ELSE
051000000713     C* controllo se presenti i dati relativi ad una traduzione comunque già effettuata
051100000713     C     VIHFLD        IFNE      *blanks
051200000713     C     VIHMBD        ANDNE     *blanks
051300000713     C     VIHFLS        ANDNE     *blanks
051400000713     C     VIHMBS        ANDNE     *blanks
051500010131     C* se è avvenuta comunque una traduzione occorre ripartire ripristinando lo storico
051600000713     C                   CLEAR                   PARAM
051700000713     C                   EXSR      PASPAR
051800030521     C                   EVAL      PMBD='M'+%subst(VIDKSC:2:7)
051900030519     C                   EVAL      PPRG=PMBR
052000000713     C                   MOVEL     PARAM         KPJBU
052100050607     C*
052200050722     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" ESEGUO ELABORAZIONI DIVERSE
052300050607     C                   IF        wNewLOG = 'N'
052400050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
052500050607     C                   PARM                    KPJBA
052600050607     C                   PARM      *blanks       STATO
052700050607     C                   ELSE
052800050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
052900050607     C                   PARM                    KPJBA
053000050607     C                   PARM      *blanks       STATO
053100050607     C                   PARM                    VIHPRG
053200090910     C                   PARM                    VIDTIP
053300050722     C* AL TERMINE DELLE OPERAZIONI PREVISTE FORZO LA SCREENING DEL FILE GENERICO DOWNLOAD (TIVGD)
053400060807     C                   CLEAR                   TIS781DS
053500120105     C                   CLEAR                   TIS781DFLO
053600120105     C                   EVAL      §781FLOSRC = 'L'
053700120105     C                   EVAL      §781FLO = TIS781DFLO
053800060807     C                   EVAL      §781PRG = VIHPRG
053900060807     C                   EVAL      §781HLD = 'S'
054000060807     C                   CALL      'TIS781C1'                                   *LANCIA PGM
054100060807     C                   PARM      *blanks       ESITO781
054200060807     C                   PARM                    TIS781DS
054300060807     C                   PARM      *blanks       PRG781
054400050607     C                   ENDIF
054500000713     C                   ENDIF
054600050607     C*
054700050722     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" ESEGUO ELABORAZIONI DIVERSE
054800050607     C* effettuo un rilancio completo dall'inizio (compresa la verifica abil. e trad.)
054900050607     C                   IF        wNewLOG = 'N'
055000000713     C                   CALL      'TIS7T0R'
055100000713     C                   PARM                    VIDKSC
055200000713     C                   PARM                    VIHKSU
055300000713     C                   PARM                    VIDTIP
055400000713     C                   PARM                    ESITO
055500050607     C                   PARM      *blanks       VIHPRG
055600001031     C                   PARM      *blanks       VIDISV
055700000713     C                   PARM                    OPE
055800000713     C                   PARM                    VIHFL2
055900070207     C                   PARM                    VIHMBD
056000120105     C                   PARM      'L'           SRC
056100050607     C                   ENDIF
056200000713     C                   ENDIF
056300000713     C                   ENDIF
056400001012     C                   ENDIF
056500000328     C*
056600000328     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800010316     C* O24GES - GESTIONE OPZIONE 24=VISUALIZZAZIONE FILE INPUT
056900970526     C*------------------------------------------------------------------------*
057000010316     C     O24GES        BEGSR
057100970526     C*
057200991115     C                   CLEAR                   PARAM
057300991115     C                   EXSR      PASPAR
057400991115     C                   MOVEL     VIHLIS2       PLIB
057500991115     C                   MOVEL     VIHFLD        POBJ
057600030521     C                   EVAL      PMBD='M'+%subst(VIDKSC:2:7)
057700030519     C                   EVAL      PPRG=PMBD
057800000706     C                   MOVEL     PARAM         KPJBU
057900050607     C*
058000050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
058100050607     C                   IF        wNewLOG = 'N'
058200050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
058300050607     C                   PARM                    KPJBA
058400050607     C                   PARM      *blanks       STATO
058500050607     C                   ELSE
058600050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
058700050607     C                   PARM                    KPJBA
058800050607     C                   PARM      *blanks       STATO
058900050607     C                   PARM                    VIHPRG
059000090910     C                   PARM                    VIDTIP
059100050607     C                   ENDIF
059200970526     C*
059300970526     C                   ENDSR
059400991112     C*------------------------------------------------------------------------*
059500991112     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD LOG
059600991112     C*------------------------------------------------------------------------*
059700991112     C     O04GES        BEGSR
059800991112     C*
059900991112     C                   CLEAR                   PARAM
060000991112     C                   EXSR      PASPAR
060100000706     C                   MOVEL     PARAM         KPJBU
060200050607     C*
060300050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
060400050607     C                   IF        wNewLOG = 'N'
060500050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
060600050607     C                   PARM                    KPJBA
060700050607     C                   PARM      *blanks       STATO
060800050607     C                   ELSE
060900050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
061000050607     C                   PARM                    KPJBA
061100050607     C                   PARM      *blanks       STATO
061200050607     C                   PARM                    VIHPRG
061300090910     C                   PARM                    VIDTIP
061400050607     C                   ENDIF
061500991112     C*
061600991112     C                   ENDSR
061700991014     C*------------------------------------------------------------------------*
061800991112     C* O14GES - GESTIONE OPZIONE 14=ANNULLAMENTO MEMBRO STORICO
061900991014     C*------------------------------------------------------------------------*
062000991112     C     O14GES        BEGSR
062100991014     C*
062200000707     C     VIDSTS        IFEQ      '0'
062300000707     C                   MOVEL     MSG(13)       DSMSMS
062400000707     C                   ELSE
062500991112     C                   CLEAR                   PARAM
062600991112     C                   EXSR      PASPAR
062700000706     C                   MOVEL     PARAM         KPJBU
062800050607     C*
062900050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
063000050607     C                   IF        wNewLOG = 'N'
063100050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
063200050607     C                   PARM                    KPJBA
063300050607     C                   PARM      *blanks       STATO
063400050607     C                   ELSE
063500050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
063600050607     C                   PARM                    KPJBA
063700050607     C                   PARM      *blanks       STATO
063800050607     C                   PARM                    VIHPRG
063900090910     C                   PARM                    VIDTIP
064000050607     C                   ENDIF
064100000707     C                   ENDIF
064200991108     C*
064300991014     C                   ENDSR
064400010316     C*------------------------------------------------------------------------*
064500010316     C* O23GES - GESTIONE OPZIONE 23=VISUALIZZAZIONE MEBRO FILE ASCII
064600010316     C*------------------------------------------------------------------------*
064700010316     C     O23GES        BEGSR
064800010316     C*
064900010316     C                   CLEAR                   PARAM
065000010316     C                   EXSR      PASPAR
065100010316     C                   MOVEL     DESTIP        POBJ
065200010316     C                   MOVEL     PARAM         KPJBU
065300050607     C*
065400050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
065500050607     C                   IF        wNewLOG = 'N'
065600050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
065700050607     C                   PARM                    KPJBA
065800050607     C                   PARM      *blanks       STATO
065900050607     C                   ELSE
066000050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
066100050607     C                   PARM                    KPJBA
066200050607     C                   PARM      *blanks       STATO
066300050607     C                   PARM                    VIHPRG
066400090910     C                   PARM                    VIDTIP
066500050607     C                   ENDIF
066600010316     C*
066700010316     C                   ENDSR
066800970526     C*------------------------------------------------------------------------*
066900991110     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD DI LOG
067000970526     C*------------------------------------------------------------------------*
067100970526     C     O05GES        BEGSR
067200991110     C*
067300050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
067400050607     C                   IF        wNewLOG = 'N'
067500991115     C                   EXFMT     S7GIDWW
067600050607     C                   ELSE
067700050607     C                   EXFMT     S7GIDWWN
067800050607     C                   ENDIF
067900970526     C*
068000970526     C                   ENDSR
068100991110     C*------------------------------------------------------------------------*
068200010316     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE MEBRO FILE STORICO
068300991110     C*------------------------------------------------------------------------*
068400991110     C     O15GES        BEGSR
068500991110     C*
068600991110     C                   CLEAR                   PARAM
068700991112     C                   EXSR      PASPAR
068800000706     C                   MOVEL     PARAM         KPJBU
068900050607     C*
069000050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
069100050607     C                   IF        wNewLOG = 'N'
069200050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
069300050607     C                   PARM                    KPJBA
069400050607     C                   PARM      *blanks       STATO
069500050607     C                   ELSE
069600050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
069700050607     C                   PARM                    KPJBA
069800050607     C                   PARM      *blanks       STATO
069900050607     C                   PARM                    VIHPRG
070000090910     C                   PARM                    VIDTIP
070100050607     C                   ENDIF
070200991110     C*
070300991110     C                   ENDSR
070400050901     C*------------------------------------------------------------------------*
070500050901     C* O18GES - GESTIONE OPZIONE 18=CREAZIONE MEMBRO 00T DA STORICO TRANSAZIONE
070600050901     C*------------------------------------------------------------------------*
070700050901     C     O18GES        BEGSR
070800050901     C*
070900050901     C                   EVAL      VIDMBR = VIHMBI
071000050901     C                   EVAL      VIDAOR = 'S'
071100050901     C                   EXFMT     S7GIDWO18
071200050901     C*
071300050901     C* Se premuto F06 => elaboro la richiesta altrimenti torno alla videata principale
071400050901     C                   IF        *INKF = *on
071500050901     C                   CLEAR                   PARAM
071600050901     C                   EXSR      PASPAR
071700050901     C                   MOVEL     PARAM         KPJBU
071800050901     C*
071900050901     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
072000050901     C                   IF        wNewLOG = 'N'
072100050901     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
072200050901     C                   PARM                    KPJBA
072300050901     C                   PARM      *blanks       STATO
072400050901     C                   ELSE
072500050901     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
072600050901     C                   PARM                    KPJBA
072700050901     C                   PARM      *blanks       STATO
072800050901     C                   PARM                    VIHPRG
072900090910     C                   PARM                    VIDTIP
073000050901     C                   ENDIF
073100050901     C*
073200050901     C* SE OCCORSI ERRORI MANDO MESSAGGIO A VIDEO
073300050901     C                   MOVEL     KPJBU         PARAM
073400050901     C                   IF        STATO = '2'
073500050901     C                   MOVEL     MSG(9)        DSMSMS
073600050901     C                   ELSE
073700050901     C                   MOVEL     MSG(10)       DSMSMS
073800050901     C                   ENDIF
073900050901     C                   ENDIF
074000050901     C*
074100050901     C                   ENDSR
074200991112     C*------------------------------------------------------------------------*
074300991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
074400991112     C*------------------------------------------------------------------------*
074500991112     C     PASPAR        BEGSR
074600991112     C*
074700991112     C                   MOVEL     VIHLIS        PLIB
074800991115     C                   MOVEL     VIHFLS        POBJ
074900991115     C                   MOVEL     VIHMBS        PMBR
075000991112     C                   Z-ADD     VIHDATI       PDAT
075100991112     C                   Z-ADD     VIHORA        PORA
075200991112     C                   MOVEL     VIDKSC        PKSC
075300991115     C                   MOVEL     VIDSTS        PSTS
075400991112     C                   MOVEL     VIDTIP        PTIP
075500991115     C                   Z-ADD     VIHSQZ        PSQZ
075600991112     C                   MOVEL     OPZKEY        POPZ
075700000629     C                   MOVEL     VIHFLD        PFID
075800000706     C                   MOVEL     VIHMBD        PMBD
075900030519     C                   MOVEL     VIHMBS        PPRG
076000050901     C                   MOVEL     VIHFLD        PFILO18
076100050901     C                   MOVEL     VIDMBR        PMBRO18
076200050901     C                   MOVEL     VIDAOR        PAORO18
076300991112     C*
076400991112     C                   ENDSR
076500010316     C*
076600970526     C*------------------------------------------------------------------------*
076700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
076800970526     C*------------------------------------------------------------------------*
076900970526     C     NEWPOS        BEGSR
077000970526     C*
077100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
077200970526     C*         1= RICHIESTO POSIZIONAMENTO
077300970526     C                   Z-ADD     0             SWPOSI            7 0
077400970526     C*
077500991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
077600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
077700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
077800970526     C                   Z-ADD     0             SWMODI            1 0
077900970530     C*
078000970530     C* REIMPOSTA INDICATORI DI ERRORE
078100991108     C                   SETOFF                                       808182
078200991108     C                   SETOFF                                       83
078300970526     C*
078400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
078500010316     C     VIDti1        IFNE      depti1
078600010316     C     VIDst1        orne      depst1
078700010316     C     VIDks1        orne      depks1
078800010316     C     VIDdt1        orne      depdt1
078900010316     C     VIDsv1        orne      depsv1
079000010316     C     VIDks2        orne      depks2
079100010316     C     VIDdt2        orne      depdt2
079200010423     C     VIDsrs        orne      depsrs
079300090518     C     VIDspr        orne      depspr
079400100311     C     VIDpgi        orne      deppgi
079500101018     C     VIDproc       orne      depproc
079600010316     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
079700010316     C     ERRL00        IFEQ      '0'                                          *NO ERORI
079800970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
079900991108     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
080000991108     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
080100991108     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
080200991108     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
080300001031     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
080400991108     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
080500991108     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
080600010423     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
080700090518     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
080800100311     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
080900101018     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
081000010316     C                   ENDIF
081100010316     C                   ENDIF
081200970526     C*
081300970526     C                   ENDSR
081400970526     C*------------------------------------------------------------------------*
081500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
081600970526     C*------------------------------------------------------------------------*
081700970528     C     SETFIL        BEGSR
081800970526     C*
081900970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
082000991108     C     VIDdt2        IFEQ      *ZEROS
082100991115     C                   MOVEL     *hival        KLTdat
082200991108     C                   ELSE
082300991108     C                   Z-ADD     VIDdt2        CHKDATA
082400991108     C                   Z-ADD     AAAA          AAINV
082500991108     C                   Z-ADD     MM            MMINV
082600991108     C                   Z-ADD     GG            GGINV
082700991115     C                   MOVEL     INVDATA       KLTdat
082800991108     C                   ENDIF
082900991115     C                   MOVEL     *hival        KLTora
083000991108     C     VIDks2        IFEQ      *ZEROS
083100991108     C     VIDks2        OREQ      *BLANKS
083200991115     C                   MOVEL     *hival        KLTksc
083300991108     C                   ELSE
083400991115     C                   MOVEL     VIDks2        KLTksc
083500991108     C                   ENDIF
083600991115     C                   MOVEL     *hival        KLTsts
083700991115     C                   MOVEL     *hival        KLTtip
083800991115     C                   MOVEL     *hival        KLTsqz
083900991117IF  1C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
084000991117     C     KEYvlt3       SETLL     tivlt03l                           99
084100970528E   1C                   ENDIF
084200991117IF  1C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
084300991117     C     KEYvlt4       SETLL     tivlt04l                           99
084400991117E   1C                   ENDIF
084500140606     C* se K_T è il tipo caricamento, devo usare solo questi 2 dati
084600140606IF  1C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
084700140606     C                   MOVEL     VIDks1        KLTksc
084800140606     C                   MOVEL     VIDTi1        KLTtip
084900140606     C     KEYvlt1       SETLL     tivlt01l                           99
085000140606E   1C                   ENDIF
085100980107     C*
085200970526     C* LETTURA RECORD
085300970526IF  1C     *IN99         IFEQ      *OFF
085400970526     C                   EXSR      LETFIL
085500970526E   1C                   ENDIF
085600970526     C*
085700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
085800970526IF  1C     *IN99         IFEQ      *ON
085900970526     C                   Z-ADD     1             SFLEOF
086000140606     C* mi salvo il posizionamento effettuato
086100140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
086200140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
086300140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
086400140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
086500140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
086600140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
086700140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
086800140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
086900140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
087000140606     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
087100140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
087200970526E   1C                   ENDIF
087300970526     C*
087400970526     C                   ENDSR
087500970526     C*------------------------------------------------------------------------*
087600970526     C* LETFIL - LETTURA RECORD
087700970526     C*------------------------------------------------------------------------*
087800970526     C     LETFIL        BEGSR
087900970526     C*
088000970526     C* LEGGE FINO A:
088100970526     C*     - FINE FILE
088200970526     C*     - TROVATO RECORD VALIDO
088300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
088400970526DO  1C     *IN99         DOWEQ     *OFF
088500970526     C     WRECOK        ANDEQ     'N'
088600970526     C*
088700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
088800991117IF  2C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
088900991115     C                   READ      tivlt03l                               99
089000970528E   2C                   ENDIF
089100991117IF  2C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
089200991117     C                   READ      tivlt04l                               99
089300991117E   2C                   ENDIF
089400140606     C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
089500140606     C* la chiave è solo questa per questo tipo di caricamento
089600140606     C     KEYVlt1       READE     tivlt01l                               99
089700140606     C                   ENDIF
089800970528     C*
089900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
090000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
090100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
090200970530X   2C                   ELSE
090300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
090400970530E   2C                   ENDIF
090500970526E   1C                   ENDDO
090600970526     C*
090700970526     C                   ENDSR
090800970526     C*------------------------------------------------------------------------*
090900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
091000970526     C*------------------------------------------------------------------------*
091100970526     C     CHKREC        BEGSR
091200970526     C*
091300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
091400001228     C*
091500001228     C*filtra se impostato tipo file
091600010427     C                   if        WRECOK = 'S'
091700001228     C     vidti1        ifeq      *blank                                       *ALLA DATA
091800001228     C                   else
091900001228     C                   if        %subst(vidti1:1:1) = '*'
092000001228     C                   if        %subst(vidti1:2:1) <>
092100001228     C                             %subst(vlttip:2:1)
092200001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
092300001228     C                   endif
092400001228     C                   else
092500001228     C                   if        %subst(vidti1:2:1) = '*'
092600001228     C                   if        %subst(vidti1:1:1) <>
092700001228     C                             %subst(vlttip:1:1)
092800001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
092900001228     C                   endif
093000001228     C                   else
093100001228     C                   if        vidti1 <> vlttip
093200001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
093300001228     C                   endif
093400001228     C                   endif
093500001228     C                   endif
093600001228     C                   endif
093700010427     C                   endif
093800001228     C*
093900001031     C*filtra se impostato stato del log
094000010427     C                   if        WRECOK = 'S'
094100991108     C     vidst1        ifeq      *blank                                       *ALLA DATA
094200001228     C                   ELSE
094300991115     C     vidst1        ifne      vltsts                                       *ALLA DATA
094400991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
094500001228     C                   ENDIF
094600001228     C                   ENDIF
094700010427     C                   ENDIF
094800970526     C*
094900001031     C*filtra se impostato codice cliente
095000010427     C                   if        WRECOK = 'S'
095100001031     C     vidks1        ifeq      *zeros                                       *ALLA DATA
095200991108     C     vidks1        oreq      *blank                                       *ALLA DATA
095300001228     C                   ELSE
095400991115     C     vidks1        ifne      vltksc                                       *ALLA DATA
095500991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
095600001228     C                   ENDIF
095700001228     C                   ENDIF
095800010427     C                   ENDIF
095900991108     C*
096000001031     C*filtra se impostata data del log
096100010427     C                   if        WRECOK = 'S'
096200001031     C     viddt1        ifeq      *zeros                                       *ALLA DATA
096300001228     C                   ELSE
096400991108     C                   Z-ADD     viddt1        CHKDATA
096500991108     C                   Z-ADD     AAAA          AAINV
096600991108     C                   Z-ADD     MM            MMINV
096700991108     C                   Z-ADD     GG            GGINV
096800991115     C     invdata       ifne      vltdat                                       *ALLA DATA
096900991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
097000001228     C                   ENDIF
097100001228     C                   ENDIF
097200010427     C                   ENDIF
097300001228     C*
097400001228     C*filtra se impostato il tipo servizio
097500010427     C                   if        WRECOK = 'S'
097600001228     C     vidsv1        ifeq      *blank
097700001228     C                   else
097800001228     C                   if        %subst(vidsv1:1:1) = '*'
097900001228     C                   if        %subst(vidsv1:2:1) <>
098000001228     C                             %subst(vltisv:2:1)
098100001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
098200001228     C                   endif
098300001228     C                   else
098400001228     C                   if        %subst(vidsv1:2:1) = '*'
098500001228     C                   if        %subst(vidsv1:1:1) <>
098600001228     C                             %subst(vltisv:1:1)
098700001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
098800001228     C                   endif
098900001228     C                   else
099000001228     C                   if        vidsv1 <> vltisv
099100001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
099200001228     C                   endif
099300001228     C                   endif
099400001228     C                   endif
099500001228     C                   endif
099600010427     C                   endif
099700010427     C*
099800010427     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
099900010427     C                   if        WRECOK = 'S'
100000010427     C                   move      vltksu        wcli              7 0          -codice
100100010427     C                   exsr      dectib
100200010427     C     VIDsrs        ifeq      *blanks                                      *ALLA DATA
100300010427     C                   else
100400010709     C*                  if        %subst(wddksc:1:%len(%trim(VIDsrs)))<>
100500010709     C*                            %trim(VIDsrs)
100600010709     C                   z-add     *zeros        pos               2 0
100700010709     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
100800010709     C                   if        pos = *zeros
100900010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
101000010427     C                   endif
101100010427     C                   endif
101200010427     C                   endif
101300090518     C*
101400090518     C* PROGRESSIVO TRANSAZIONE DIVERSO DA QUELLA SCELTA
101500090518     C                   if        WRECOK = 'S'
101600090518     C     VIDspr        ifeq      *blanks
101700090518     C                   else
101800090518     C                   z-add     *zeros        pos               2 0
101900090518     C                   eval      pos = %scan(%trim(VIDspr):VLTprg)
102000090518     C                   if        pos = *zeros
102100090518     C                   z-add     *zeros        pos               2 0
102200090518     C                   eval      pos = %scan(%trim(VLTprg):VIDspr)
102300090518     C                   if        pos = *zeros
102400090518     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
102500090518     C                   endif
102600090518     C                   endif
102700090518     C                   endif
102800090518     C                   endif
102900100311     C*
103000100311     C*filtra se impostato il Traduttore
103100100311     C                   if        WRECOK = 'S'
103200100311     C     vidpgi        ifeq      *blanks
103300100311     C                   else
103400100311     C                   if        %scan(%trim(vidPGI):vltPGI) > *zeros
103500100311     C                   else
103600100311     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
103700100311     C                   endif
103800100311     C                   ENDIF
103900100311     C                   ENDIF
104000101018     C*
104100101018     C*filtra se impostato l'ID processo
104200101018     C                   if        WRECOK = 'S'
104300101018     C     vidproc       ifeq      *blanks
104400101018     C                   else
104500101018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
104600101018     C                   if        vltsts <> '0'             AND
104700101018     C                             %subst(vltmsg:28:1) = 'P' AND
104800101018     C                             (%subst(vltmsg:32:3) = 'FTP' OR
104900101018     C                              %subst(vltmsg:32:3) = 'ftp')
105000101018     C                   if        vidproc = %subst(vltmsg:29:2)
105100101018     C                   MOVEL     'S'           WRECOK                         *RECORD NON VALIDO
105200101018     C                   endif
105300101018     C                   endif
105400101018     C                   endif
105500101018     C                   endif
105600991108     C*
105700970526     C                   ENDSR
105800010423     C*------------------------------------------------------------------------*
105900010423     C* dectib - Controlla esistenza codice cliente
106000010423     C*------------------------------------------------------------------------*
106100010423     C     dectib        begsr
106200010913     C*
106300010913     C     vltksc        chain     tianv01l
106400010913     C                   if        %found(tianv01l)
106500010913     C                   movel(p)  anvdes        wddksc
106600010913     C                   else
106700010423     C                   CLEAR                   BS69DS
106800010423     C                   CLEAR                   ACODS
106900010423     C                   MOVEL     KNSIF         I69SIF
107000010423     C                   Z-ADD     WCLI          I69KAC
107100010423     C                   CALL      'TIBS69R'
107200010423     C                   PARM                    BS69DS
107300010423     C                   PARM                    ACODS
107400010423     C                   PARM                    INDDS
107500010423     C                   PARM                    CLPDS
107600010423     C                   PARM                    CLSDS
107700010423     C     O69ERR        IFNE      '1'
107800010423     C                   MOVEL     ACORAG        wddksc           40
107900010423     C                   ELSE
108000010423     C                   MOVEL     *ALL'*'       Wddksc
108100010913     C                   MOVE      '1'           errl00
108200010913     C                   ENDIF
108300010423     C                   ENDIF
108400010913     C*
108500010913     C                   endsr
108600970526     C*------------------------------------------------------------------------*
108700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
108800970526     C*------------------------------------------------------------------------*
108900970526     C     IRESFL        BEGSR
109000970526     C*
109100970526     C                   MOVEL     *BLANKS       OPZKEY
109200991115     C                   Z-ADD     vltdat        VIHdati
109300991115     C                   Z-ADD     vltdat        G08INV
109400991108     C                   Z-ADD     *ZEROS        G08DAT
109500991108     C                   MOVEL     '3'           G08ERR
109600991108     C                   CALL      'XSRDA8'
109700991108     C                   PARM                    WLBDA8
109800991110     C                   Z-ADD     G08DAT        VIHdat
109900991110     C                   Z-ADD     VIHdat        CHKDATA
110000991110     C                   Z-ADD     GG            GGA
110100991110     C                   Z-ADD     MM            MMA
110200991110     C                   Z-ADD     AAAA          AAAAA
110300991110     C                   MOVEL     DATAA         VIDdat
110400991115     C                   z-add     vltora        VIHora
110500991110     C                   z-add     VIHora        ORAN
110600991110     C                   z-add     HH            HHA
110700991110     C                   z-add     PP            PPA
110800991110     C                   z-add     SS            SSA
110900991110     C                   MOVEL     ORAA          VIDora
111000991115     C                   MOVEL     vltksc        VIDksc
111100991115     C                   MOVE      vltksc        VIPksc
111200010423     C                   MOVEL(p)  wddksc        VIDRAG
111300010423     C                   MOVEL(p)  wddksc        VIHRAG
111400991115     C                   MOVEL     vltsts        VIDsts
111500991108     C* controllo in schiera codici e descrizioni stati di log
111600991108     C                   SETOFF                                       28
111700991108     C                   Z-ADD     1             J                 2 0
111800991115     C     vltsts        LOOKUP    STS(J)                                 28
111900991108     C     *IN28         IFEQ      *ON
112000991115     C                   MOVEL     DES(J)        DESSTS
112100000211     C     DES(J)        IFEQ      'Err  '
112200991110     C                   SETON                                        50
112300991110     C                   ELSE
112400991110     C                   SETOFF                                       50
112500991110     C                   ENDIF
112600991108     C                   ENDIF
112700991115     C     vltsqz        IFGT      *ZEROS
112800991115     C     vltfls        ANDEQ     *BLANKS
112900000211     C                   MOVEL     'Canc '       DESSTS
113000991115     C                   ENDIF
113100991115     C                   MOVEL     vlttip        VIDtip
113200991115     C* reperimento descrizione tipi file
113300991110     C                   MOVEL     'ITF'         KBEcod                         *tabella
113400991110     C                   MOVEL     *BLANKS       KBElin                         *tabella
113500991110     C                   MOVEL     *BLANKS       KBEsif                         *tabella
113600991115     C                   MOVEL     vlttip        KBEke1                         *tabella
113700991110     C                   MOVEL     *BLANKS       KBEke2                         *tabella
113800991110     C     KE2tbe        CHAIN     tntbe02l
113900020218     C                   MOVEL     tbeuni        DITF
114000020218     C                   MOVEL     §ITFDESC      DEHtip                         *tabella
114100020218     C                   MOVEL     §ITFDESC      DEDtip                         *tabella
114200991115     C                   MOVEL     vltksc        KIRksc
114300991115     C                   MOVEL     vlttip        KIRtip
114400991108     C                   Z-ADD     *zeros        KIRdti
114500991108     C     KEYvir        CHAIN     tivir02l
114600991108     C     virfit        IFNE      *BLANKS
114700030924     C***                MOVEL     virfit        DEStip
114800991110     C                   MOVEL     virfid        DEHti1
114900030924     C***                MOVEL     virfis        DEHti2
115000011029     C                   ELSE
115100011029     C* Re-inizializzo degli importantissimi campi di wrk
115200011029     C                   EVAL      DEStip = *blanks
115300011029     C                   EVAL      DEHti1 = *blanks
115400011029     C                   EVAL      DEHti2 = *blanks
115500010910     C                   ENDIF
115600991115     C     vlttip        CHAIN     tivtf01l
115700010910     C                   IF        DEStip = *blanks
115800030924     C***                MOVEL     vtffit        DEStip
115900010910     C                   ENDIF
116000010910     C                   IF        DEHti1 = *blanks
116100991110     C                   MOVEL     vtffid        DEHti1
116200010910     C                   ENDIF
116300010910     C                   IF        DEHti2 = *blanks
116400030924     C***                MOVEL     vtffis        DEHti2
116500010910     C                   ENDIF
116600991115     C     vltfls        IFNE      *BLANKS
116700991115     C                   MOVEL     '*LIBL'       VIHlis
116800991115     C                   ELSE
116900991115     C                   MOVEL     *BLANKS       VIHlis
117000991115     C                   ENDIF
117100030924     C                   MOVEL     vltfld        DEStip
117200030924     C                   MOVEL     vltfls        DEHti2
117300991115     C                   MOVEL     '*LIBL'       VIHlis2
117400991115     C                   MOVEL     vltfls        VIHfls
117500000315     C                   MOVEL     DEHti1        VIHfld
117600991115     C                   MOVEL     vltmbs        VIHmbs
117700991115     C                   MOVEL     vltmbd        VIHmbd
117800030521     C                   EVAL      VIHMBI='M'+%subst(VIDKSC:2:7)
117900991115     C                   MOVEL     vltmsg        MESS
118000991110     C                   MOVEL     MS1           DEHMS1
118100991110     C                   MOVEL     MS2           DEHMS2
118200991115     C                   MOVEL     vltpgi        VIHpgi
118300991115     C                   Z-ADD     vltsqz        VIHsqz
118400000329     C                   MOVEL     vltfld        V2HFLD                         *very importante
118500000329     C                   MOVEL     vltmbd        V2HMBD                         *very importante
118600000329     C                   MOVEL     vltksu        VIHKSU                         *very importante
118700001031     C                   MOVEL     vltisv        VIDISV                         *very importante
118800000623     C                   MOVEL     vltprg        VIHPRG                         *very importante
118900000713     C                   MOVEL     vltfl2        VIHFL2                         *very importante
119000001031     C                   MOVEL     vlttia        VIHTIA
119100011029     C*
119200970526     C                   ENDSR
119300970526     C*------------------------------------------------------------------------*
119400140606     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
119500970526     C*------------------------------------------------------------------------*
119600970526     C     CHKPAR        BEGSR
119700970528     C*
119800991108     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *03L
119900991117     C                   MOVEL     'DAT'         WTIPRI            3
120000991108IF  1C     VIDdt2        IFEQ      *zeros
120100991025     C     VIDks2        ANDEQ     *ZEROS
120200991108IF  1C     VIDdt2        orEQ      *zeros
120300991025     C     VIDks2        ANDEQ     *blanks
120400991117     C                   MOVEL     'DAT'         WTIPRI
120500991025E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
120600970528     C*
120700970528     C* CONTROLLA IL TIPO RICERCA
120800991012IF  1C     ERRL00        IFEQ      '0'
120900991108IF  2C     VIDdt2        IFNE      *zeros
121000991117     C                   MOVEL     'DAT'         WTIPRI
121100991117X   2C                   ELSE
121200991117IF  3C     VIDks2        IFNE      *zeros
121300991117IF  3C     VIDks2        andne     *blanks
121400991117     C                   MOVEL     'KSC'         WTIPRI
121500991117E   3C                   ENDIF
121600991012E   2C                   ENDIF
121700991012E   1C                   ENDIF
121800140606     C* se i parametri di ingresso sono valorizzati
121900140610     C* e i rispettivi parametri video hanno gli stessi valori
122000140610     C* e nessun altro parametro video è valorizzato
122100140606     C* allora siamo nel caso di chiamata
122200140606     C* da pgm che passa i parametri in input e quindi uso la VL 01L
122300140606IF  3C     DEPksc        IFNE      *zeros
122400140606IF  3C     DEPksc        andne     *blanks
122500140606IF  3C     DEPTip        andne     *blanks
122600140610IF  3C     DEPksc        andeq     VIDKs1
122700140610IF  3C     DEPTip        andeq     VIDTi1
122800140606IF  3C     VIDst1        andeq     *blanks
122900140606IF  3C     VIDdt1        andeq     0
123000140606IF  3C     VIDsv1        andeq     *blanks
123100140606IF  3C     VIDks2        andeq     *blanks
123200140606IF  3C     VIDdt2        andeq     0
123300140606IF  3C     VIDsrs        andeq     *blanks
123400140606IF  3C     VIDspr        andeq     *blanks
123500140606IF  3C     VIDpgi        andeq     *blanks
123600140606IF  3C     VIDproc       andeq     *blanks
123700140606     C                   MOVEL     'K_T'         WTIPRI
123800140606     C* visto che so che la userò, apro la VL 01L
123900140606     C                   IF        not %open(TIVLT01L)
124000140606     C                   OPEN      TIVLT01L
124100140606     C                   ENDIF
124200140606E   3C                   ENDIF
124300991012      *
124400970526     C                   ENDSR
124500970526     C*------------------------------------------------------------------------*
124600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
124700970526     C*------------------------------------------------------------------------*
124800970526     C     CHKFOP        BEGSR
124900991112     C*
125000991112     C* OPZIONI
125100970626     C*
125200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
125300991112IF  1C     DEPOP0        IFEQ      'P05'
125400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
125500970626     C                   MOVEL     'S'           OPDATT
125600970626     C                   MOVEL     OPDRIG        OPD(5)
125700970626E   1C                   ENDIF
125800970626     C*
125900970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
126000970626IF  1C     DEPOP0        IFEQ      'P02'
126100000329     C                   MOVEL     OPD(1)        OPDRIG                         *RIESECUZIONE
126200970626     C                   MOVEL     'S'           OPDATT
126300991110     C                   MOVEL     OPDRIG        OPD(1)
126400000329     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE
126500000329     C                   MOVEL     'S'           OPDATT
126600000329     C                   MOVEL     OPDRIG        OPD(11)
126700000329     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
126800991014     C                   MOVEL     'S'           OPDATT
126900991110     C                   MOVEL     OPDRIG        OPD(4)
127000050607     C*                  MOVEL     OPD(14)       OPDRIG                         *CANCELLAZIONE
127100010403     C*                  MOVEL     'S'           OPDATT
127200010403     C*                  MOVEL     OPDRIG        OPD(14)
127300991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
127400991112     C                   MOVEL     'S'           OPDATT
127500991112     C                   MOVEL     OPDRIG        OPD(5)
127600991112     C                   MOVEL     OPD(15)       OPDRIG                         *INTERROGAZIONE
127700991112     C                   MOVEL     'S'           OPDATT
127800991112     C                   MOVEL     OPDRIG        OPD(15)
127900010316     C                   MOVEL     OPD(23)       OPDRIG                         *INTERROGAZIONE
128000991115     C                   MOVEL     'S'           OPDATT
128100010316     C                   MOVEL     OPDRIG        OPD(23)
128200010316     C                   MOVEL     OPD(24)       OPDRIG                         *INTERROGAZIONE
128300010316     C                   MOVEL     'S'           OPDATT
128400010316     C                   MOVEL     OPDRIG        OPD(24)
128500050901     C                   MOVEL     OPD(18)       OPDRIG                         *CREAZ. MEMBRO 00T
128600050901     C                   MOVEL     'S'           OPDATT
128700050901     C                   MOVEL     OPDRIG        OPD(18)
128800050722     C***
128900050722     C                   MOVEL     FUD(15)       FUDRIG                         *FORZA ELAB. VGD
129000050722     C                   MOVEL     'S'           FUDATT
129100050722     C                   MOVEL     FUDRIG        FUD(15)
129200101005     C                   MOVEL     FUD(18)       FUDRIG                         *FORZA ELAB. BARTVAS
129300101005     C                   MOVEL     'S'           FUDATT
129400101005     C                   MOVEL     FUDRIG        FUD(18)
129500050722     C***
129600970626E   1C                   ENDIF
129700970626     C*
129800970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
129900020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
130000991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
130100991112     C                   MOVEL     'N'           OPDATT
130200991112     C                   MOVEL     OPDRIG        OPD(4)
130300991110     C                   MOVEL     OPD(14)       OPDRIG                         *NO RIPRISTINO
130400970626     C                   MOVEL     'N'           OPDATT
130500991110     C                   MOVEL     OPDRIG        OPD(14)
130600991110     C                   MOVEL     FUD(1)        FUDRIG                         *NO INSERIMENTO
130700970626     C                   MOVEL     'N'           FUDATT
130800991110     C                   MOVEL     FUDRIG        FUD(1)
130900991110     C                   MOVEL     FUD(11)       FUDRIG                         *NO INSERIMENTO
131000991110     C                   MOVEL     'N'           FUDATT
131100991110     C                   MOVEL     FUDRIG        FUD(11)
131200970626E   1C                   ENDIF
131300970526     C*
131400970526     C* GESTIONE OPZIONI A VIDEO
131500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
131600970526     C                   EXSR      OPZGES
131700970526     C*
131800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
131900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
132000970526     C                   EXSR      FUNGES
132100970526     C*
132200970526     C                   ENDSR
132300991108     C*------------------------------------------------------------------------*
132400991108     C* FINSR - OPERAZIONI FINALI
132500991108     C*------------------------------------------------------------------------*
132600991108     C     FINSR         BEGSR
132700991108     C*
132800991108     C* CHIUDE I FILE DEI PGM CHIAMATI
132900991108     C                   CLEAR                   BS69DS
133000991108     C                   CLEAR                   ACODS
133100991108     C                   CLEAR                   INDDS
133200991108     C                   CLEAR                   CLPDS
133300991108     C                   CLEAR                   CLSDS
133400991108     C                   MOVEL     'C'           I69TLA
133500991108     C                   MOVEL     KNSIF         I69SIF
133600991108     C                   CALL      'TIBS69R'
133700991108     C                   PARM                    BS69DS
133800991108     C                   PARM                    ACODS
133900991108     C                   PARM                    INDDS
134000991108     C                   PARM                    CLPDS
134100991108     C                   PARM                    CLSDS
134200991108     C*
134300991108     C                   ENDSR
134400020916     C*--------------------------------------------------------------------------------------------*
134500020916     C* REPERISCE I DATI UTENTE
134600020916     C*--------------------------------------------------------------------------------------------*
134700020916     C     REPDATIUTE    BEGSR
134800020916     C*
134900020916     C* INIZIALIZZA VARIABILI DI WRK
135000020916     C                   CLEAR                   TIBS34DS
135100020916     C                   CLEAR                   AZUTEDS
135200020916     C                   CLEAR                   DDATIUTE
135300020916     C*
135400020916     C     *DTAARA       DEFINE    §azute        azuteds
135500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
135600020916     C                   IN(E)     *DTAARA
135700020916if  1C                   IF        %Error
135800020916     c                   EVAL      I34Tla = 'L'
135900020916     C                   CALL      'TIBS34R'
136000020916     C                   PARM                    Tibs34Ds
136100020916     C                   IN        *DTAARA
136200020916e   1C                   ENDIF
136300020916     C*
136400020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
136500020916     C                   MOVEL(P)  RSUT          DSFIRS           20
136600020916     C*
136700020916     C                   ENDSR
136800970526     C*------------------------------------------------------------------------*
136900970526     C* *INZSR - OPERAZIONI INIZIALI
137000970526     C*------------------------------------------------------------------------*
137100970526     C     *INZSR        BEGSR
137200970526     C*---
137300970526     C* RICEVIMENTO PARAMETRI
137400970526     C*---
137500970526     C     *ENTRY        PLIST
137600970612     C                   PARM                    KPJBA
137700051013     C                   MOVEL     KPJBU         PAR_INP
137800051013     C*
137900051013     C* TESTO SUBITO ALCCUNI PARAMETRI D INGRESSO
138000051013     C                   IF        DEPKSC <> *blanks
138100051013     C                   EVAL      VIDks1 = DEPKSC
138200051013     C                   ENDIF
138300051013     C                   IF        DEPTIP <> *blanks
138400051013     C                   EVAL      VIDti1 = DEPTIP
138500051013     C                   ENDIF
138600970526     C*
138700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
138800970526     C     XMSGPA        PLIST
138900970526     C                   PARM                    DSMSMS
139000970526     C                   PARM                    DSMSPG
139100970526     C                   PARM                    DSMSKE
139200970526     C                   PARM                    DSMSER
139300970526     C*
139400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
139500970526     C                   CALL      'XKEY'
139600970526     C                   PARM                    DSKEY
139700970526     C*---
139800970526     C* VARIABILI NON RIFERITE AL DATA BASE
139900970526     C*---
140000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
140100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
140200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
140300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
140400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
140500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
140600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
140700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
140800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
140900970526     C*---
141000991108     C     *LIKE         DEFINE    viddt2        Depdt2
141100991108     C     *LIKE         DEFINE    viddt1        Depdt1
141200991108     C     *LIKE         DEFINE    vidks2        Depks2
141300991108     C     *LIKE         DEFINE    vidks1        Depks1
141400991108     C     *LIKE         DEFINE    vidti1        Depti1
141500991108     C     *LIKE         DEFINE    vidst1        Depst1
141600001031     C     *LIKE         DEFINE    vidsv1        Depsv1
141700010423     C     *LIKE         DEFINE    vidsrs        Depsrs
141800090518     C     *LIKE         DEFINE    vidspr        Depspr
141900100311     C     *LIKE         DEFINE    vidpgi        Deppgi
142000101018     C     *LIKE         DEFINE    vidproc       Depproc
142100991115     C     *LIKE         DEFINE    vltdat        kltdat
142200991115     C     *LIKE         DEFINE    vltora        kltora
142300991115     C     *LIKE         DEFINE    vltksc        kltksc
142400991115     C     *LIKE         DEFINE    vltsts        kltsts
142500991115     C     *LIKE         DEFINE    vlttip        klttip
142600991115     C     *LIKE         DEFINE    vltsqz        kltsqz
142700991108     C*---
142800991108     C     *LIKE         DEFINE    tbecod        kbecod
142900991108     C     *LIKE         DEFINE    tbelin        kbelin
143000991108     C     *LIKE         DEFINE    tbesif        kbesif
143100991108     C     *LIKE         DEFINE    tbeke1        kbeke1
143200991108     C     *LIKE         DEFINE    tbeke2        kbeke2
143300991108     C     *LIKE         DEFINE    virksc        kirksc
143400991108     C     *LIKE         DEFINE    virtip        kirtip
143500991108     C     *LIKE         DEFINE    virdti        kirdti
143600970526     C* VARIABILI DI CONTROLLO
143700970526     C*---
143800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
143900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
144000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
144100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
144200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
144300970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
144400991110     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
144500991110     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
144600991110     C                   MOVEL     ':'           SEP3                           *VALIDITA' RECORD
144700991110     C                   MOVEL     ':'           SEP4                           *VALIDITA' RECORD
144800970526     C*
144900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
145000991115     C                   MOVEL     'TIS7GIR'     DSMSPG
145100991115     C                   MOVEL     'TIS7GIR'     MSGQUE
145200970526     C                   MOVEL     *BLANKS       MSGKEY
145300970526     C                   MOVEL     *BLANKS       DSMSMS
145400970526     C                   MOVEL     *BLANKS       DSMSKE
145500970526     C                   Z-ADD     *ZEROS        DSMSER
145600970526     C*---
145700970526     C* VARIABILI RIFERITE AL DATA BASE
145800970526     C*---
145900970526     C*---
146000970526     C* CHIAVI DI LETTURA
146100970526     C*---
146200140606     C* LETTURA parziale tivlt01L -
146300140606     C     KEYvlt1       KLIST
146400140606     C                   KFLD                    KLTksc
146500140606     C                   KFLD                    KLTtip
146600991115     C* LETTURA tivlt03L -
146700991117     C     KEYvlt3       KLIST
146800991115     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
146900991115     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
147000991115     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
147100991115     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
147200991115     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
147300991115     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
147400991117     C* LETTURA tivlt04L -
147500991117     C     KEYvlt4       KLIST
147600991117     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
147700991117     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
147800991117     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
147900991117     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
148000991117     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
148100991117     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
148200991108     C* LETTURA "NUOVE" TABELLE
148300991108     C     KE2tbe        KLIST                                                  *tntbe02l
148400991108     C                   KFLD                    KBEcod                          -tabella
148500991108     C                   KFLD                    KBElin                          -lingua
148600991108     C                   KFLD                    KBEsif                          -s.informativo
148700991108     C                   KFLD                    KBEke1                          -lingua
148800991108     C                   KFLD                    KBEke2                          -s.informativo
148900991108     C* LETTURA tivir02L -
149000991108     C     KEYvir        KLIST
149100991108     C                   KFLD                    KIRksc                         *PARTENZA/ARRIVO
149200991108     C                   KFLD                    KIRtip                         *PARTENZA/ARRIVO
149300060413     C* LETTURA tivgd01L -
149400060413     C     KEYvgd01      KLIST
149500060413     C                   KFLD                    vgdTIP
149600060413     C                   KFLD                    vgdKSU
149700060413     C                   KFLD                    vgdTSC
149800991108     C*
149900970527     C* REPERIMENTO DATI UTENTE
150000020916     C                   EXSR      REPDATIUTE
150100970526     C*
150200970526     C* CALCOLA LA DATA CORRENTE
150300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
150400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
150500970526     C                   Z-ADD     WN8           G08DAT
150600970526     C                   Z-ADD     *ZEROS        G08INV
150700970526     C                   MOVEL     '0'           G08ERR
150800970526     C                   CALL      'XSRDA8'
150900970526     C                   PARM                    WLBDA8
151000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
151100051013     C*
151200051013     C* EFFETTUA PRIMA VERIFICA DEI PARAMETRI RICEVUTI
151300051013     C                   EXSR      CHKPAR
151400991115     C*
151500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
151600970526     C                   EXSR      CHKFOP
151700970526     C*
151800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
151900970530IF  1C     DEPOP0        IFEQ      'P01'
152000970530     C                   MOVEL     DEC(3)        VIDING
152100970530E   1C                   ENDIF
152200970530IF  1C     DEPOP0        IFEQ      'P02'
152300970526     C                   MOVEL     DEC(1)        VIDING
152400970530E   1C                   ENDIF
152500970530IF  1C     DEPOP0        IFEQ      'P05'
152600970526     C                   MOVEL     DEC(2)        VIDING
152700970526E   1C                   ENDIF
152800970526     C*
152900970526     C                   ENDSR
153000970526     C*------------------------------------------------------------------------*
153100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
153200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
153300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
153400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
153500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
153600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
153700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
153800970526     C*------------------------------------------------------------------------*
153900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
154000970526     C*------------------------------------------------------------------------*
154100970526     C     POSSFL        BEGSR
154200070918     C*
154300070918     C* INIZIALIZZO OPZIONI SALVATE
154400070918     C                   CLEAR                   SAVOPZKEY
154500970526     C*
154600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
154700970526     C                   EXSR      CLRSFL
154800970526     C*
154900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
155000970526     C                   EXSR      SETFIL
155100970526     C*
155200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
155300970526     C     SFLEOF        IFEQ      0
155400970526     C                   EXSR      CARSFL
155500970526     C                   END
155600970526     C*
155700970526     C                   ENDSR
155800970526     C*------------------------------------------------------------------------*
155900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
156000970526     C*------------------------------------------------------------------------*
156100970526     C     ENTATT        BEGSR
156200970526     C*
156300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
156400970526     C                   Z-ADD     0             OPZOK             1 0
156500970526     C*
156600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
156700970526     C                   Z-ADD     0             SFLVAR            5 0
156800970526     C*
156900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
157000970526     C                   Z-ADD     0             SFLERR
157100970526     C*
157200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
157300970526     C                   Z-ADD     SFLULT        WFLULT
157400970526     C*
157500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
157600991115     C                   READC     s7GID05                                98
157700970526     C     *IN98         DOWEQ     '0'
157800970526     C*
157900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
158000970526     C                   ADD       1             SFLVAR
158100970526     C*
158200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
158300970526     C     OPZOK         IFEQ      0
158400970526     C*
158500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
158600970526     C     OPZKEY        IFNE      *BLANKS
158700970526     C                   EXSR      OPZCON
158800970526     C                   END
158900970526     C*
159000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
159100970526     C*            2. PULIZIA SCELTA IN SFL
159200970526     C     OPZOK         IFEQ      0
159300970526     C                   MOVEL     '0'           *IN41
159400970526     C                   MOVEL     *BLANKS       OPZKEY
159500970526     C                   ELSE
159600970526     C*
159700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
159800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
159900970526     C                   MOVEL     '1'           *IN41
160000970526     C                   Z-ADD     SFLULT        SFLERR
160100970526     C                   END
160200970526     C                   ELSE
160300970526     C*
160400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
160500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
160600970526     C                   MOVEL     '1'           *IN41
160700970526     C                   END                                                    OPZOK=0
160800970526     C*
160900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
161000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
161100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
161200970526     C     OPZOK         IFNE      0
161300970526     C                   Z-ADD     SFLERR        SFLNBR
161400970526     C                   ELSE
161500970526     C                   Z-ADD     SFLULT        SFLNBR
161600970526     C                   END
161700970526     C*
161800970526     C* AGGIORNO RECORD SFL
161900020212     C                   EVAL      *IN50 = HIN50
162000991115     C                   UPDATE    s7GID05
162100970526     C*
162200991115     C                   READC     s7GID05                                98
162300970526     C                   END                                                    DO WHILE
162400970526     C*
162500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
162600970526     C                   Z-ADD     WFLULT        SFLULT
162700970526     C*
162800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
162900970526     C     SFLVAR        IFEQ      0
163000970526     C*---                 MOVEL'1'       FINL00
163100970526     C                   END
163200970526     C*
163300070918     C     SAVOPZKEY     IFNE      ' 5'
163400070918     C     SAVOPZKEY     ANDNE     '15'
163500070918     C     SAVOPZKEY     ANDNE     '25'
163600070918     C                   EXSR      F05GES
163700070918     C                   ENDIF
163800991112     C*
163900970526     C                   ENDSR
164000970526     C*------------------------------------------------------------------------*
164100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
164200970526     C*------------------------------------------------------------------------*
164300970526     C     CLRSFL        BEGSR
164400970526     C*
164500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
164600970526     C* (0=NON FINE FILE  1=FINE FILE)
164700970526     C                   Z-ADD     0             SFLEOF
164800970526     C*
164900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
165000970526     C                   Z-ADD     0             SFLULT
165100970526     C*
165200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
165300970526     C                   MOVEL     '1'           *IN40
165400991115     C                   WRITE     s7GID06
165500970526     C                   MOVEL     '0'           *IN40
165600970526     C*
165700970526     C                   ENDSR
165800970526     C*------------------------------------------------------------------------*
165900970526     C* CARSFL - CARICAMENTO SFL
166000970526     C*------------------------------------------------------------------------*
166100970526     C     CARSFL        BEGSR
166200970526     C*
166300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
166400970526     C     SFLULT        IFEQ      0
166500970526     C                   EXSR      SETFIL
166600970526     C                   END
166700970526     C*
166800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
166900970526     C                   Z-ADD     SFLULT        WFLULT
167000970526     C*
167100970526     C* CICLO:
167200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
167300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
167400970526     C                   Z-ADD     1             SFLRIG
167500970526     C     SFLRIG        DOWLE     SFLPAG
167600970526     C     SFLEOF        ANDEQ     0
167700970526     C*
167800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
167900991116     C                   MOVEL     *BLANKS       OPZKEY
168000970526     C*
168100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
168200970526     C                   EXSR      IRESFL
168300970526     C*
168400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
168500970526     C                   ADD       1             SFLULT
168600970526     C*
168700970526     C* SCRITTURA RECORD SFL
168800020212     C                   EVAL      HIN50 = *IN50
168900991115     C                   WRITE     s7GID05
169000970526     C                   ADD       1             SFLRIG
169100970526     C*
169200970526     C* LETTURA FILE DA CARICARE
169300970526     C                   EXSR      LETFIL
169400970526     C                   END                                                    DO WHILE
169500970526     C*
169600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
169700970526     C     WFLULT        IFNE      SFLULT
169800970526     C                   Z-ADD     WFLULT        SFLNBR
169900970526     C                   ADD       1             SFLNBR
170000140606     C* mi salvo il posizionamento effettuato
170100140606     C* mi salvo il posizionamento effettuato
170200140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
170300140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
170400140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
170500140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
170600140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
170700140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
170800140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
170900140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
171000140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
171100140606     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
171200140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
171300970526     C                   END
171400970526     C*
171500970526     C                   ENDSR
171600970526     C*------------------------------------------------------------------------*
171700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
171800970526     C*------------------------------------------------------------------------*
171900970526     C     OUTSFL        BEGSR
172000970526     C*
172100970526     C* WRITE RECORD TESTATA VIDEO
172200991115     C                   WRITE     s7GID01
172300970526     C*
172400970526     C* VISUALIZZAZIONE MESSAGGIO
172500970526     C                   EXSR      SNDMSG
172600970526     C*
172700970526     C* WRITE RECORD OPZIONI VIDEO
172800991115     C                   WRITE     s7GID02
172900970526     C*
173000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
173100991115     C                   WRITE     s7GID03
173200970526     C*
173300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
173400991115     C                   WRITE     s7GID04
173500970526     C*
173600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
173700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
173800970526     C     SFLEOF        IFEQ      0
173900970526     C                   MOVEL     '0'           *IN42
174000970526     C                   ELSE
174100970526     C*
174200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
174300970526     C                   MOVEL     '1'           *IN42
174400970526     C                   END
174500970526     C*
174600970526     C* WRITE RECORD TASTI FUNZIONALI
174700991115     C                   WRITE     s7GID09
174800970526     C*
174900970526     C* CASO DI SFL VUOTO
175000970526     C     SFLULT        IFLE      0
175100970526     C*
175200970526     C*    - WRITE RECORD VUOTO
175300991115     C                   WRITE     s7GID0A
175400991115     C                   READ      s7GID0A                                99
175500970526     C*
175600970526     C                   ELSE
175700970526     C*
175800970526     C* CASO DI SFL CON RECORDS
175900970526     C*
176000970526     C*    - WRITE RECORD CONTROLLO SFL
176100991115     C                   WRITE     s7GID06
176200991115     C                   READ      s7GID06                                99
176300970526     C                   END
176400970526     C*
176500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
176600991115     C                   READ      s7GID03                                99
176700970526     C*
176800970526     C                   ENDSR
176900970526     C*------------------------------------------------------------------------*
177000970526     C* SNDMSG - MANDA MESSAGGIO
177100970526     C*------------------------------------------------------------------------*
177200970526     C     SNDMSG        BEGSR
177300970526     C*
177400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
177500970526     C                   MOVEL     '1'           *IN40
177600991115     C                   WRITE     s7GIDMC
177700970526     C                   MOVEL     '0'           *IN40
177800970526     C*
177900970526     C* VISUALIZZAZIONE MESSAGGIO
178000970526     C                   Z-ADD     0             MSGULT
178100970526     C                   CALL      'XMSG'        XMSGPA
178200970526     C     DSMSER        IFEQ      0
178300970526     C                   MOVEL     DSMSKE        MSGKEY
178400970526     C                   END
178500970526     C                   Z-ADD     1             MSGULT
178600991115     C                   WRITE     s7GIDMS                                      SFL MESS
178700991115     C                   WRITE     s7GIDMC                                      CONTR MESS
178800970526     C*
178900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
179000970526     C                   MOVEL     *BLANKS       DSMSMS
179100970526     C*
179200970526     C                   ENDSR
179300970526     C*------------------------------------------------------------------------*
179400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
179500970526     C*------------------------------------------------------------------------*
179600970526     C     FUNCON        BEGSR
179700970526     C*
179800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
179900970526     C                   EXSR      FUNVAR
180000970526     C*
180100970526     C* PULISCO CAMPI DI LAVORO
180200970526     C                   Z-ADD     0             I                 5 0
180300970526     C                   Z-ADD     0             W                 5 0
180400970526     C*
180500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
180600970526     C     FUNKEY        IFNE      *BLANKS
180700970526     C     1             DO        24            I
180800970526     C                   MOVEL     FUD(I)        FUDRIG
180900970526     C*
181000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
181100970526     C     FUNKEY        IFEQ      FUDFUN
181200970526     C     FUDATT        ANDEQ     'S'
181300970526     C                   Z-ADD     I             W
181400970526     C                   END
181500970526     C                   END                                                    DO WHILE
181600970526     C*
181700970526     C* FUNZIONE NON ATTIVA (ERRORE)
181800970526     C     W             IFEQ      0
181900970526     C*
182000970526     C* EMISSIONE MESSAGGIO DI ERRORE
182100970526     C                   MOVEL     MSG(2)        DSMSMS
182200970526     C                   END                                                    W=0
182300970526     C                   END                                                    FUNKEY NE *BLANKS
182400970526     C*
182500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
182600970526     C     W             IFNE      0
182700970526     C     FUNKEY        OREQ      *BLANKS
182800970526     C*
182900970526     C     FUNKEY        IFEQ      *BLANKS
183000970526     C                   EXSR      ENTGES
183100970526     C                   END
183200970526     C     FUNKEY        IFEQ      '03'
183300970526     C                   EXSR      F03GES
183400970526     C                   END
183500970526     C     FUNKEY        IFEQ      '05'
183600970526     C                   EXSR      F05GES
183700970526     C                   END
183800970526     C     FUNKEY        IFEQ      '06'
183900970526     C                   EXSR      F06GES
184000970526     C                   END
184100970526     C     FUNKEY        IFEQ      '12'
184200970526     C                   EXSR      F12GES
184300970526     C                   END
184400050722     C     FUNKEY        IFEQ      '15'
184500050722     C                   EXSR      F15GES
184600050722     C                   END
184700101005     C     FUNKEY        IFEQ      '18'
184800101005     C                   EXSR      F18GES
184900101005     C                   END
185000970528     C     FUNKEY        IFEQ      '21'
185100970528     C                   EXSR      F21GES
185200970528     C                   END
185300970528     C     FUNKEY        IFEQ      '22'
185400970528     C                   EXSR      F22GES
185500970528     C                   END
185600970526     C     FUNKEY        IFEQ      '23'
185700970526     C                   EXSR      F23GES
185800970526     C                   END
185900970526     C     FUNKEY        IFEQ      '24'
186000970526     C                   EXSR      F24GES
186100970526     C                   END
186200970526     C     FUNKEY        IFEQ      'RU'
186300970526     C                   EXSR      RUPGES
186400970526     C                   END
186500970526     C*
186600970526     C                   END
186700970526     C*
186800970526     C                   ENDSR
186900970526     C*------------------------------------------------------------------------*
187000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
187100970526     C*------------------------------------------------------------------------*
187200970526     C     FUNVAR        BEGSR
187300970526     C*
187400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
187500970526     C     KEY           IFEQ      ENTER
187600970526     C                   MOVEL     *BLANKS       FUNKEY
187700970526     C                   END
187800970526     C     KEY           IFEQ      F1
187900970526     C                   MOVEL     '01'          FUNKEY
188000970526     C                   END
188100970526     C     KEY           IFEQ      F2
188200970526     C                   MOVEL     '02'          FUNKEY
188300970526     C                   END
188400970526     C     KEY           IFEQ      F3
188500970526     C                   MOVEL     '03'          FUNKEY
188600970526     C                   END
188700970526     C     KEY           IFEQ      F4
188800970526     C                   MOVEL     '04'          FUNKEY
188900970526     C                   END
189000970526     C     KEY           IFEQ      F5
189100970526     C                   MOVEL     '05'          FUNKEY
189200970526     C                   END
189300970526     C     KEY           IFEQ      F6
189400970526     C                   MOVEL     '06'          FUNKEY
189500970526     C                   END
189600970526     C     KEY           IFEQ      F7
189700970526     C                   MOVEL     '07'          FUNKEY
189800970526     C                   END
189900970526     C     KEY           IFEQ      F8
190000970526     C                   MOVEL     '08'          FUNKEY
190100970526     C                   END
190200970526     C     KEY           IFEQ      F9
190300970526     C                   MOVEL     '09'          FUNKEY
190400970526     C                   END
190500970526     C     KEY           IFEQ      F10
190600970526     C                   MOVEL     '10'          FUNKEY
190700970526     C                   END
190800970526     C     KEY           IFEQ      F11
190900970526     C                   MOVEL     '11'          FUNKEY
191000970526     C                   END
191100970526     C     KEY           IFEQ      F12
191200970526     C                   MOVEL     '12'          FUNKEY
191300970526     C                   END
191400970526     C     KEY           IFEQ      F13
191500970526     C                   MOVEL     '13'          FUNKEY
191600970526     C                   END
191700970526     C     KEY           IFEQ      F14
191800970526     C                   MOVEL     '14'          FUNKEY
191900970526     C                   END
192000970526     C     KEY           IFEQ      F15
192100970526     C                   MOVEL     '15'          FUNKEY
192200970526     C                   END
192300970526     C     KEY           IFEQ      F16
192400970526     C                   MOVEL     '16'          FUNKEY
192500970526     C                   END
192600970526     C     KEY           IFEQ      F17
192700970526     C                   MOVEL     '17'          FUNKEY
192800970526     C                   END
192900970526     C     KEY           IFEQ      F18
193000970526     C                   MOVEL     '18'          FUNKEY
193100970526     C                   END
193200970526     C     KEY           IFEQ      F19
193300970526     C                   MOVEL     '19'          FUNKEY
193400970526     C                   END
193500970526     C     KEY           IFEQ      F20
193600970526     C                   MOVEL     '20'          FUNKEY
193700970526     C                   END
193800970526     C     KEY           IFEQ      F21
193900970526     C                   MOVEL     '21'          FUNKEY
194000970526     C                   END
194100970526     C     KEY           IFEQ      F22
194200970526     C                   MOVEL     '22'          FUNKEY
194300970526     C                   END
194400970526     C     KEY           IFEQ      F23
194500970526     C                   MOVEL     '23'          FUNKEY
194600970526     C                   END
194700970526     C     KEY           IFEQ      F24
194800970526     C                   MOVEL     '24'          FUNKEY
194900970526     C                   END
195000970526     C     KEY           IFEQ      ROLLUP
195100970526     C                   MOVEL     'RU'          FUNKEY
195200970526     C                   END
195300970526     C     KEY           IFEQ      HELP
195400970526     C                   MOVEL     'HP'          FUNKEY
195500970526     C                   END
195600970526     C*
195700970526     C                   ENDSR
195800970526     C*------------------------------------------------------------------------*
195900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
196000970526     C*------------------------------------------------------------------------*
196100970526     C     FUNGES        BEGSR
196200970526     C*
196300970526     C* PULISCO CAMPI DI LAVORO
196400970526     C                   Z-ADD     0             W                 5 0
196500970526     C*
196600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
196700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
196800970526     C     1             DO        24            I
196900970526     C                   MOVEL     FUD(I)        FUDRIG
197000970526     C*
197100970526     C* PULIZIA FUNZIONI PRECEDENTI
197200970526     C                   MOVEL     *BLANKS       FUV(I)
197300970526     C*
197400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
197500970526     C* (SALTA F23 E F24)
197600970526     C     FUDATT        IFEQ      'S'
197700970526     C     FUDFUN        ANDNE     '23'
197800970526     C     FUDFUN        ANDNE     '24'
197900970526     C                   ADD       1             W
198000970526     C                   MOVEL     FUDDES        FUV(W)
198100970526     C                   END
198200970526     C                   END                                                    DO WHILE
198300970526     C*
198400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
198500970526     C* ALTRIMENTI DISATTIVO F24
198600970526     C     1             DO        24            I
198700970526     C                   MOVEL     FUD(I)        FUDRIG
198800970526     C*
198900970526     C     FUDFUN        IFEQ      '24'
199000970526     C     W             IFGT      6
199100970526     C                   MOVEL     'S'           FUDATT
199200970526     C                   ELSE
199300970526     C                   MOVEL     *BLANKS       FUDATT
199400970526     C                   END                                                    W GT 8
199500970526     C                   MOVEL     FUDRIG        FUD(I)
199600970526     C                   END
199700970526     C                   END                                                    DO WHILE
199800970526     C*
199900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
200000970526     C     W             DIV       6             I
200100970526     C                   MVR                     Z
200200970526     C*
200300970526     C     Z             IFNE      0
200400970526     C     I             ADD       1             Z
200500970526     C                   ELSE
200600970526     C                   Z-ADD     I             Z
200700970526     C                   END
200800970526     C*
200900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
201000970526     C                   ADD       1             FUNGRU
201100970526     C     FUNGRU        IFGT      Z
201200970526     C     FUNGRU        ORLT      1
201300970526     C                   Z-ADD     1             FUNGRU
201400970526     C                   END
201500970526     C*
201600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
201700970526     C     FUNGRU        IFEQ      1
201800970526     C                   MOVEL     FUV(1)        FUN001
201900970526     C                   MOVEL     FUV(2)        FUN002
202000970526     C                   MOVEL     FUV(3)        FUN003
202100970526     C                   MOVEL     FUV(4)        FUN004
202200970526     C                   MOVEL     FUV(5)        FUN005
202300970526     C                   MOVEL     FUV(6)        FUN006
202400970526     C                   END
202500970526     C*
202600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
202700970526     C     FUNGRU        IFEQ      2
202800970526     C                   MOVEL     FUV(7)        FUN001
202900970526     C                   MOVEL     FUV(8)        FUN002
203000970526     C                   MOVEL     FUV(9)        FUN003
203100970526     C                   MOVEL     FUV(10)       FUN004
203200970526     C                   MOVEL     FUV(11)       FUN005
203300970526     C                   MOVEL     FUV(12)       FUN006
203400970526     C                   END
203500970526     C*
203600970526     C* TERZO GRUPPO DI 6 FUNZIONI
203700970526     C     FUNGRU        IFEQ      3
203800970526     C                   MOVEL     FUV(13)       FUN001
203900970526     C                   MOVEL     FUV(14)       FUN002
204000970526     C                   MOVEL     FUV(15)       FUN003
204100970526     C                   MOVEL     FUV(16)       FUN004
204200970526     C                   MOVEL     FUV(17)       FUN005
204300970526     C                   MOVEL     FUV(18)       FUN006
204400970526     C                   END
204500970526     C*
204600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
204700970526     C     FUNGRU        IFEQ      4
204800970526     C                   MOVEL     FUV(19)       FUN001
204900970526     C                   MOVEL     FUV(20)       FUN002
205000970526     C                   MOVEL     FUV(21)       FUN003
205100970526     C                   MOVEL     FUV(22)       FUN004
205200970526     C                   MOVEL     *BLANKS       FUN005
205300970526     C                   MOVEL     *BLANKS       FUN006
205400970526     C                   END
205500970526     C*
205600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
205700970526     C                   MOVEL     *BLANKS       FUN007
205800970526     C                   MOVEL     *BLANKS       FUN008
205900970526     C*
206000970526     C     1             DO        24            I
206100970526     C                   MOVEL     FUD(I)        FUDRIG
206200970526     C     FUDFUN        IFEQ      '23'
206300970526     C     FUDATT        ANDEQ     'S'
206400970526     C                   MOVEL     FUDDES        FUN007
206500970526     C                   END
206600970526     C     FUDFUN        IFEQ      '24'
206700970526     C     FUDATT        ANDEQ     'S'
206800970526     C                   MOVEL     FUDDES        FUN008
206900970526     C                   END
207000970526     C                   END                                                    DO
207100970526     C*
207200970526     C                   ENDSR
207300970526     C*------------------------------------------------------------------------*
207400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
207500970526     C*------------------------------------------------------------------------*
207600970526     C     OPZGES        BEGSR
207700970526     C*
207800970526     C* PULISCO CAMPI DI LAVORO
207900970526     C                   Z-ADD     0             W                 5 0
208000970526     C*
208100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
208200970526     C* E CARICO TABELLA OPZIONI ATTIVE
208300991115     C     1             DO        24            I
208400970526     C                   MOVEL     OPD(I)        OPDRIG
208500970526     C*
208600970526     C* PULIZIA OPZIONI PRECEDENTI
208700970526     C                   MOVEL     *BLANKS       OPV(I)
208800970526     C*
208900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
209000970526     C     OPDATT        IFEQ      'S'
209100970526     C                   ADD       1             W
209200970526     C                   MOVEL     OPDDES        OPV(W)
209300970526     C                   END
209400970526     C                   END                                                    DO WHILE
209500970526     C*
209600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
209700970526     C* ALTRIMENTI DISATTIVO F23
209800991115     C     1             DO        24            I
209900970526     C                   MOVEL     FUD(I)        FUDRIG
210000970526     C*
210100970526     C     FUDFUN        IFEQ      '23'
210200970526     C     W             IFGT      8
210300970526     C                   MOVEL     'S'           FUDATT
210400970526     C                   ELSE
210500970526     C                   MOVEL     *BLANKS       FUDATT
210600970526     C                   END                                                    W GT 8
210700970526     C                   MOVEL     FUDRIG        FUD(I)
210800970526     C                   END
210900970526     C                   END                                                    DO WHILE
211000970526     C*
211100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
211200970526     C     W             DIV       8             I
211300970526     C                   MVR                     Z                 5 0
211400970526     C*
211500970526     C     Z             IFNE      0
211600970526     C     I             ADD       1             Z
211700970526     C                   ELSE
211800970526     C                   Z-ADD     I             Z
211900970526     C                   END
212000970526     C*
212100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
212200970526     C                   ADD       1             OPZGRU
212300970526     C     OPZGRU        IFGT      Z
212400970526     C     OPZGRU        ORLT      1
212500970526     C                   Z-ADD     1             OPZGRU
212600970526     C                   END
212700970526     C*
212800970526     C* PRIMO GRUPPO DI 8 OPZIONI
212900970526     C     OPZGRU        IFEQ      1
213000970526     C                   MOVEL     OPV(1)        OPZ001
213100970526     C                   MOVEL     OPV(2)        OPZ002
213200970526     C                   MOVEL     OPV(3)        OPZ003
213300970526     C                   MOVEL     OPV(4)        OPZ004
213400970526     C                   MOVEL     OPV(5)        OPZ005
213500970526     C                   MOVEL     OPV(6)        OPZ006
213600970526     C                   MOVEL     OPV(7)        OPZ007
213700970526     C                   MOVEL     OPV(8)        OPZ008
213800970526     C                   END
213900970526     C*
214000970526     C* SECONDO GRUPPO DI 8 OPZIONI
214100970526     C     OPZGRU        IFEQ      2
214200970526     C                   MOVEL     OPV(9)        OPZ001
214300970526     C                   MOVEL     OPV(10)       OPZ002
214400970526     C                   MOVEL     OPV(11)       OPZ003
214500970526     C                   MOVEL     OPV(12)       OPZ004
214600970526     C                   MOVEL     OPV(13)       OPZ005
214700970526     C                   MOVEL     OPV(14)       OPZ006
214800970526     C                   MOVEL     OPV(15)       OPZ007
214900970526     C                   MOVEL     OPV(16)       OPZ008
215000970526     C                   END
215100970526     C*
215200970526     C* TERZO GRUPPO DI 8 OPZIONI
215300970526     C     OPZGRU        IFEQ      3
215400970526     C                   MOVEL     OPV(17)       OPZ001
215500970526     C                   MOVEL     OPV(18)       OPZ002
215600970526     C                   MOVEL     OPV(19)       OPZ003
215700970526     C                   MOVEL     OPV(20)       OPZ004
215800970526     C                   MOVEL     OPV(21)       OPZ005
215900970526     C                   MOVEL     OPV(22)       OPZ006
216000970526     C                   MOVEL     OPV(23)       OPZ007
216100970526     C                   MOVEL     OPV(24)       OPZ008
216200970526     C                   END
216300970526     C*
216400970526     C                   ENDSR
216500970526     C*------------------------------------------------------------------------*
216600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
216700970526     C*------------------------------------------------------------------------*
216800970526     C     OPZCON        BEGSR
216900050607     C*
217000050607     C* VERIFICO SUBITO SE IL RECORD D LOG CORRENTE E' TIPO "VECCHIO" O "NUOVO"
217100060413     C                   MOVEL     'N'           wNewLOG           1
217200060413     C     VIHPRG        CHAIN     tivgd02l                                     * già elaborato (S)
217300050607     C                   IF        %found(tivgd02l)
217400050607     C                   MOVEL     'S'           wNewLOG
217500060413     C                   ELSE
217600060413     C                   EVAL      vgdTIP = VIDTIP
217700060413     C                   EVAL      vgdKSU = VIHKSU
217800060413     C                   EVAL      vgdTSC = 'WW'
217900060413     C     KEYvgd01      CHAIN     tivgd01l                                     * da elaborare  ( )
218000060413     C                   IF        %found(tivgd01l)
218100060413     C                   MOVEL     'S'           wNewLOG
218200060413     C                   ENDIF
218300050607     C                   ENDIF
218400970526     C*
218500970526     C* PULISCO CAMPI DI LAVORO
218600970526     C                   Z-ADD     0             I                 5 0
218700970526     C                   Z-ADD     0             W                 5 0
218800970526     C*
218900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
219000970526     C     1             DO        24            I
219100970526     C                   MOVEL     OPD(I)        OPDRIG
219200970526     C*
219300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
219400970526     C     OPZKEY        IFEQ      OPDOPZ
219500970526     C     OPZKEY        OREQ      OPDOP1
219600970526     C*
219700970526     C     OPDATT        IFEQ      'S'
219800970526     C                   MOVEL     OPDOPZ        OPZKEY
219900970526     C                   Z-ADD     I             W
220000970526     C                   END
220100970526     C                   END
220200970526     C*
220300970526     C                   END                                                    DO WHILE
220400970526     C*
220500970526     C* OPZIONE NON ATTIVA (ERRORE)
220600970526     C     W             IFEQ      0
220700970526     C*
220800970526     C* OPZIONE NON OK
220900970526     C                   Z-ADD     1             OPZOK
221000970526     C*
221100970526     C* EMISSIONE MESSAGGIO DI ERRORE
221200970526     C                   MOVEL     MSG(1)        DSMSMS
221300970526     C                   ELSE
221400970526     C*
221500970526     C* OPZIONE ATTIVA (PROCEDI ...)
221600970526     C*
221700070918     C* SALVO L'OPZIONE CORRENTEMENTE RICHIESTA
221800070918     C                   MOVEL     OPZKEY        SAVOPZKEY         2
221900070918     C*
222000970526     C     OPZKEY        IFEQ      ' 1'
222100970526     C                   EXSR      O01GES
222200970526     C                   END
222300000328     C     OPZKEY        IFEQ      '11'
222400000328     C                   EXSR      O11GES
222500000328     C                   END
222600991110     C     OPZKEY        IFEQ      ' 4'
222700991110     C                   EXSR      O04GES
222800991014     C                   END
222900991112     C     OPZKEY        IFEQ      '14'
223000991112     C                   EXSR      O14GES
223100991112     C                   END
223200970526     C     OPZKEY        IFEQ      ' 5'
223300970526     C                   EXSR      O05GES
223400970526     C                   END
223500991112     C     OPZKEY        IFEQ      '15'
223600991112     C                   EXSR      O15GES
223700991112     C                   END
223800050901     C     OPZKEY        IFEQ      '18'
223900050901     C                   EXSR      O18GES
224000050901     C                   END
224100010316     C     OPZKEY        IFEQ      '23'
224200010316     C                   EXSR      O23GES
224300010316     C                   END
224400010316     C     OPZKEY        IFEQ      '24'
224500010316     C                   EXSR      O24GES
224600991115     C                   END
224700970526     C                   END                                                    OPZ.NON ATTIVA
224800970526     C*
224900970526     C                   ENDSR
225000970526     C*------------------------------------------------------------------------*
225100970526     C* NOTE COSTRUZIONE SCHIERE
225200970526     C*------------------------------------------------------------------------*
225300970526     C*
225400970526     C* -MSG-MESSAGGI
225500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
225600970526     C* (NON TOCCARE MSG,1 E MSG,2)
225700970526     C*
225800970526     C* -CMD-COMANDI
225900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
226000970526     C*
226100970526     C* -OPD-OPZIONI
226200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
226300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
226400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
226500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
226600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
226700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
226800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
226900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
227000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
227100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
227200970526     C*
227300970526     C* -FUD-FUNZIONI
227400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
227500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
227600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
227700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
227800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
227900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
228000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
228100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
228200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
228300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
228400991108     C*
228500991115     C* -STS-CODICI STATI DEI RECORD DI LOG
228600991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
228700991110     C* ================> IMPORTANTE <=================
228800991115     C* per codici di errore in -STS- indicare in -DES- sempre
228900991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
229000991110     C* ERR
229100991108     C*
229200970526     O*------------------------------------------------------------------------*
229300970526** DEC - DECODIFICHE
229400970526    GESTIONE                                                     1
229500970526INTERROGAZIONE                                                   2
229600970530     SCELTA                                                      3
2297009705264
2298009705265
229900970526** MSG - MESSAGGI
230000991115TIS7GIR- Opzione non gestita                                                   1
230100991115TIS7GIR- Tasto funzionale non gestito                                          2
230200080904TIS7GIR-                                                                       3
230300991115TIS7GIR- Record annullato. Consentito solo il ripristino                       4
230400991115TIS7GIR- Record non annullato. Impossibile ripristinare                        5
230500991115TIS7GIR- Accettato solo un parametro di inizio lista                           6
230600000713TIS7GIR- Opzione non valida in quanto traduzione mai eseguita (Opz.11)         7
230700000629TIS7GIR- Opzione non valida in quanto mancanti dati essenziali relativi ai file8
230800050901TIS7GIR- Errori in elaborazione del comando, verificare.                       9
230900050901TIS7GIR- Copia dati nel membro destinazione effettuata correttamente.         10
231000010316TIS7GIR- 11                                                                   11
231100991115TIS7GIR- Data errata                                                          12
231200000707TIS7GIR- Opzione consentita solo per stati record diversi da '0'              13
231300101005TIS7GIR- Effettuare selezione                                                 14
231400991115TIS7GIR- 15                                                                   15
231500991115TIS7GIR- 16                                                                   16
231600991115TIS7GIR- 17                                                                   17
231700991115TIS7GIR- 18                                                                   18
231800991115TIS7GIR- 19                                                                   19
231900001012TIS7GIR- Opzione non consentita per cliente in oggetto                        20
232000970528** CMD - COMANDI CL
232100991110DSPPFM FILE(
232200991110) MBR(
232300991110
232400970528
232500970528
232600970526** OPD - OPZIONI
232700991112 1=Esec. parziale  -1 -     (S/N)=N                        opz. 01¬
232800991110                   -  -     (S/N)=N                        opz. 02¬
232900991110                   -  -     (S/N)=N                        opz. 03¬
233000991112 4=Canc. LOG       -4 -     (S/N)=N                        opz. 04¬
233100991112 5=Visual. LOG     -5 -     (S/N)=N                        opz. 05¬
233200991115                   -  -     (S/N)=N                        opz. 06¬
233300991110                   -  -     (S/N)=N                        opz. 07¬
233400991110                   -  -     (S/N)=N                        opz. 08¬
233500991110                   -  -     (S/N)=N                        opz. 09¬
233600991110                   -  -     (S/N)=N                        opz. 10¬
23370000032911=Esec. totale    -11-     (S/N)=N                        opz. 11¬
233800970526                   -  -     (S/N)=N                        opz. 12¬
233900970526                   -  -     (S/N)=N                        opz. 13¬
23400099111214=Canc. STORICO   -14-     (S/N)=N                        opz. 14¬
23410099111215=Visual. STORICO -15-     (S/N)=N                        opz. 15¬
234200970526                   -  -     (S/N)=N                        opz. 16¬
234300970526                   -  -     (S/N)=N                        opz. 17¬
23440005083118=Crea membro 00T -18-     (S/N)=N                        opz. 18¬
234500970526                   -  -     (S/N)=N                        opz. 19¬
234600970526                   -  -     (S/N)=N                        opz. 20¬
234700970526                   -  -     (S/N)=N                        opz. 21¬
234800970526                   -  -     (S/N)=N                        opz. 22¬
23490001031623=Visual. ASCII   -23-     (S/N)=N                        opz. 23¬
23500001031624=Visual. INPUT   -24-     (S/N)=N                        opz. 24¬
235100970526** FUD - FUNZIONI
235200970526                   -        (S/N)=N                        funz.01¬
235300970526                   -        (S/N)=N                        funz.02¬
235400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
235500970526                   -        (S/N)=N                        funz.04¬
235600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
235700050901F06 NON USARE !!!  -        (S/N)=N                        funz.06¬
235800050722                   -        (S/N)=N                        funz.07¬
235900970527                   -        (S/N)=N                        funz.08¬
236000970527                   -        (S/N)=N                        funz.09¬
236100970527                   -        (S/N)=N                        funz.10¬
236200970526                   -        (S/N)=N                        funz.11¬
236300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
236400991116                   -        (S/N)=N                        funz.13¬
236500970526                   -        (S/N)=N                        funz.14¬
236600050722F15=Forza elab. VGD-        (S/N)=N                        funz.15¬
236700970526                   -        (S/N)=N                        funz.16¬
236800970526                   -        (S/N)=N                        funz.17¬
236900101005F18=Forza elab. BV -        (S/N)=N                        funz.18¬
237000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
237100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
237200991110F21=Stampa elenco  -        (S/N)=N                        funz.21¬
237300991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
237400991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
237500991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
237600991115** STS - STATI DEI RECORD DI LOG
2377009911080
2378009911081
2379009911082
2380009911083
2381009911084
2382000003215
238300000329
238400991108
238500991108
238600991108
238700991108** DES - STATI DEI RECORD DI LOG
238800991112Da El
238900991108OK
239000991108Err
239100991112R-OK
239200991112R-Err
239300000321DwnL
239400000329
239500991108
239600991108
239700991108
