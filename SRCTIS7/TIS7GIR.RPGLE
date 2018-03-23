000100970526     H*------------------------------------------------------------------------*
000200991115     H* Gestione file di log TIVLT00F - Strategi Download
000300970526     H*------------------------------------------------------------------------*
000400170118     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
053300050607     C                   ENDIF
053400000713     C                   ENDIF
053500050607     C*
053600050722     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" ESEGUO ELABORAZIONI DIVERSE
053700050607     C* effettuo un rilancio completo dall'inizio (compresa la verifica abil. e trad.)
053800050607     C                   IF        wNewLOG = 'N'
053900000713     C                   CALL      'TIS7T0R'
054000000713     C                   PARM                    VIDKSC
054100000713     C                   PARM                    VIHKSU
054200000713     C                   PARM                    VIDTIP
054300000713     C                   PARM                    ESITO
054400050607     C                   PARM      *blanks       VIHPRG
054500001031     C                   PARM      *blanks       VIDISV
054600000713     C                   PARM                    OPE
054700000713     C                   PARM                    VIHFL2
054800070207     C                   PARM                    VIHMBD
054900120105     C                   PARM      'L'           SRC
055000050607     C                   ENDIF
055100000713     C                   ENDIF
055200000713     C                   ENDIF
055300001012     C                   ENDIF
055400000328     C*
055500000328     C                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700010316     C* O24GES - GESTIONE OPZIONE 24=VISUALIZZAZIONE FILE INPUT
055800970526     C*------------------------------------------------------------------------*
055900010316     C     O24GES        BEGSR
056000970526     C*
056100991115     C                   CLEAR                   PARAM
056200991115     C                   EXSR      PASPAR
056300991115     C                   MOVEL     VIHLIS2       PLIB
056400991115     C                   MOVEL     VIHFLD        POBJ
056500030521     C                   EVAL      PMBD='M'+%subst(VIDKSC:2:7)
056600030519     C                   EVAL      PPRG=PMBD
056700000706     C                   MOVEL     PARAM         KPJBU
056800050607     C*
056900050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
057000050607     C                   IF        wNewLOG = 'N'
057100050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
057200050607     C                   PARM                    KPJBA
057300050607     C                   PARM      *blanks       STATO
057400050607     C                   ELSE
057500050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
057600050607     C                   PARM                    KPJBA
057700050607     C                   PARM      *blanks       STATO
057800050607     C                   PARM                    VIHPRG
057900090910     C                   PARM                    VIDTIP
058000050607     C                   ENDIF
058100970526     C*
058200970526     C                   ENDSR
058300991112     C*------------------------------------------------------------------------*
058400991112     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD LOG
058500991112     C*------------------------------------------------------------------------*
058600991112     C     O04GES        BEGSR
058700991112     C*
058800991112     C                   CLEAR                   PARAM
058900991112     C                   EXSR      PASPAR
059000000706     C                   MOVEL     PARAM         KPJBU
059100050607     C*
059200050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
059300050607     C                   IF        wNewLOG = 'N'
059400050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
059500050607     C                   PARM                    KPJBA
059600050607     C                   PARM      *blanks       STATO
059700050607     C                   ELSE
059800050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
059900050607     C                   PARM                    KPJBA
060000050607     C                   PARM      *blanks       STATO
060100050607     C                   PARM                    VIHPRG
060200090910     C                   PARM                    VIDTIP
060300050607     C                   ENDIF
060400991112     C*
060500991112     C                   ENDSR
060600991014     C*------------------------------------------------------------------------*
060700991112     C* O14GES - GESTIONE OPZIONE 14=ANNULLAMENTO MEMBRO STORICO
060800991014     C*------------------------------------------------------------------------*
060900991112     C     O14GES        BEGSR
061000991014     C*
061100000707     C     VIDSTS        IFEQ      '0'
061200000707     C                   MOVEL     MSG(13)       DSMSMS
061300000707     C                   ELSE
061400991112     C                   CLEAR                   PARAM
061500991112     C                   EXSR      PASPAR
061600000706     C                   MOVEL     PARAM         KPJBU
061700050607     C*
061800050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
061900050607     C                   IF        wNewLOG = 'N'
062000050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
062100050607     C                   PARM                    KPJBA
062200050607     C                   PARM      *blanks       STATO
062300050607     C                   ELSE
062400050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
062500050607     C                   PARM                    KPJBA
062600050607     C                   PARM      *blanks       STATO
062700050607     C                   PARM                    VIHPRG
062800090910     C                   PARM                    VIDTIP
062900050607     C                   ENDIF
063000000707     C                   ENDIF
063100991108     C*
063200991014     C                   ENDSR
063300010316     C*------------------------------------------------------------------------*
063400010316     C* O23GES - GESTIONE OPZIONE 23=VISUALIZZAZIONE MEBRO FILE ASCII
063500010316     C*------------------------------------------------------------------------*
063600010316     C     O23GES        BEGSR
063700010316     C*
063800010316     C                   CLEAR                   PARAM
063900010316     C                   EXSR      PASPAR
064000010316     C                   MOVEL     DESTIP        POBJ
064100010316     C                   MOVEL     PARAM         KPJBU
064200050607     C*
064300050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
064400050607     C                   IF        wNewLOG = 'N'
064500050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
064600050607     C                   PARM                    KPJBA
064700050607     C                   PARM      *blanks       STATO
064800050607     C                   ELSE
064900050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
065000050607     C                   PARM                    KPJBA
065100050607     C                   PARM      *blanks       STATO
065200050607     C                   PARM                    VIHPRG
065300090910     C                   PARM                    VIDTIP
065400050607     C                   ENDIF
065500010316     C*
065600010316     C                   ENDSR
065700970526     C*------------------------------------------------------------------------*
065800991110     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD DI LOG
065900970526     C*------------------------------------------------------------------------*
066000970526     C     O05GES        BEGSR
066100991110     C*
066200050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
066300050607     C                   IF        wNewLOG = 'N'
066400991115     C                   EXFMT     S7GIDWW
066500050607     C                   ELSE
066600050607     C                   EXFMT     S7GIDWWN
066700050607     C                   ENDIF
066800970526     C*
066900970526     C                   ENDSR
067000991110     C*------------------------------------------------------------------------*
067100010316     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE MEBRO FILE STORICO
067200991110     C*------------------------------------------------------------------------*
067300991110     C     O15GES        BEGSR
067400991110     C*
067500991110     C                   CLEAR                   PARAM
067600991112     C                   EXSR      PASPAR
067700000706     C                   MOVEL     PARAM         KPJBU
067800050607     C*
067900050607     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
068000050607     C                   IF        wNewLOG = 'N'
068100050607     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
068200050607     C                   PARM                    KPJBA
068300050607     C                   PARM      *blanks       STATO
068400050607     C                   ELSE
068500050607     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
068600050607     C                   PARM                    KPJBA
068700050607     C                   PARM      *blanks       STATO
068800050607     C                   PARM                    VIHPRG
068900090910     C                   PARM                    VIDTIP
069000050607     C                   ENDIF
069100991110     C*
069200991110     C                   ENDSR
069300050901     C*------------------------------------------------------------------------*
069400050901     C* O18GES - GESTIONE OPZIONE 18=CREAZIONE MEMBRO 00T DA STORICO TRANSAZIONE
069500050901     C*------------------------------------------------------------------------*
069600050901     C     O18GES        BEGSR
069700050901     C*
069800050901     C                   EVAL      VIDMBR = VIHMBI
069900050901     C                   EVAL      VIDAOR = 'S'
070000050901     C                   EXFMT     S7GIDWO18
070100050901     C*
070200050901     C* Se premuto F06 => elaboro la richiesta altrimenti torno alla videata principale
070300050901     C                   IF        *INKF = *on
070400050901     C                   CLEAR                   PARAM
070500050901     C                   EXSR      PASPAR
070600050901     C                   MOVEL     PARAM         KPJBU
070700050901     C*
070800050901     C* A SECONDA CHE IL RECORD D LOG SIA MODO "VECCHIO" O "NUOVO" VISUALIZZO FINESTRE DIVERSE
070900050901     C                   IF        wNewLOG = 'N'
071000050901     C                   CALL      'TIS7GIC1'                                   *LANCIA PGM
071100050901     C                   PARM                    KPJBA
071200050901     C                   PARM      *blanks       STATO
071300050901     C                   ELSE
071400050901     C                   CALL      'TIS7GIC2'                                   *LANCIA PGM
071500050901     C                   PARM                    KPJBA
071600050901     C                   PARM      *blanks       STATO
071700050901     C                   PARM                    VIHPRG
071800090910     C                   PARM                    VIDTIP
071900050901     C                   ENDIF
072000050901     C*
072100050901     C* SE OCCORSI ERRORI MANDO MESSAGGIO A VIDEO
072200050901     C                   MOVEL     KPJBU         PARAM
072300050901     C                   IF        STATO = '2'
072400050901     C                   MOVEL     MSG(9)        DSMSMS
072500050901     C                   ELSE
072600050901     C                   MOVEL     MSG(10)       DSMSMS
072700050901     C                   ENDIF
072800050901     C                   ENDIF
072900050901     C*
073000050901     C                   ENDSR
073100991112     C*------------------------------------------------------------------------*
073200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
073300991112     C*------------------------------------------------------------------------*
073400991112     C     PASPAR        BEGSR
073500991112     C*
073600991112     C                   MOVEL     VIHLIS        PLIB
073700991115     C                   MOVEL     VIHFLS        POBJ
073800991115     C                   MOVEL     VIHMBS        PMBR
073900991112     C                   Z-ADD     VIHDATI       PDAT
074000991112     C                   Z-ADD     VIHORA        PORA
074100991112     C                   MOVEL     VIDKSC        PKSC
074200991115     C                   MOVEL     VIDSTS        PSTS
074300991112     C                   MOVEL     VIDTIP        PTIP
074400991115     C                   Z-ADD     VIHSQZ        PSQZ
074500991112     C                   MOVEL     OPZKEY        POPZ
074600000629     C                   MOVEL     VIHFLD        PFID
074700000706     C                   MOVEL     VIHMBD        PMBD
074800030519     C                   MOVEL     VIHMBS        PPRG
074900050901     C                   MOVEL     VIHFLD        PFILO18
075000050901     C                   MOVEL     VIDMBR        PMBRO18
075100050901     C                   MOVEL     VIDAOR        PAORO18
075200991112     C*
075300991112     C                   ENDSR
075400010316     C*
075500970526     C*------------------------------------------------------------------------*
075600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
075700970526     C*------------------------------------------------------------------------*
075800970526     C     NEWPOS        BEGSR
075900970526     C*
076000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
076100970526     C*         1= RICHIESTO POSIZIONAMENTO
076200970526     C                   Z-ADD     0             SWPOSI            7 0
076300970526     C*
076400991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
076500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
076600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
076700970526     C                   Z-ADD     0             SWMODI            1 0
076800970530     C*
076900970530     C* REIMPOSTA INDICATORI DI ERRORE
077000991108     C                   SETOFF                                       808182
077100991108     C                   SETOFF                                       83
077200970526     C*
077300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
077400010316     C     VIDti1        IFNE      depti1
077500010316     C     VIDst1        orne      depst1
077600010316     C     VIDks1        orne      depks1
077700010316     C     VIDdt1        orne      depdt1
077800010316     C     VIDsv1        orne      depsv1
077900010316     C     VIDks2        orne      depks2
078000010316     C     VIDdt2        orne      depdt2
078100010423     C     VIDsrs        orne      depsrs
078200090518     C     VIDspr        orne      depspr
078300100311     C     VIDpgi        orne      deppgi
078400101018     C     VIDproc       orne      depproc
078500010316     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
078600010316     C     ERRL00        IFEQ      '0'                                          *NO ERORI
078700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
078800991108     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
078900991108     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
079000991108     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
079100991108     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
079200001031     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
079300991108     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
079400991108     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
079500010423     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
079600090518     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
079700100311     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
079800101018     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
079900010316     C                   ENDIF
080000010316     C                   ENDIF
080100970526     C*
080200970526     C                   ENDSR
080300970526     C*------------------------------------------------------------------------*
080400970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
080500970526     C*------------------------------------------------------------------------*
080600970528     C     SETFIL        BEGSR
080700970526     C*
080800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
080900991108     C     VIDdt2        IFEQ      *ZEROS
081000991115     C                   MOVEL     *hival        KLTdat
081100991108     C                   ELSE
081200991108     C                   Z-ADD     VIDdt2        CHKDATA
081300991108     C                   Z-ADD     AAAA          AAINV
081400991108     C                   Z-ADD     MM            MMINV
081500991108     C                   Z-ADD     GG            GGINV
081600991115     C                   MOVEL     INVDATA       KLTdat
081700991108     C                   ENDIF
081800991115     C                   MOVEL     *hival        KLTora
081900991108     C     VIDks2        IFEQ      *ZEROS
082000991108     C     VIDks2        OREQ      *BLANKS
082100991115     C                   MOVEL     *hival        KLTksc
082200991108     C                   ELSE
082300991115     C                   MOVEL     VIDks2        KLTksc
082400991108     C                   ENDIF
082500991115     C                   MOVEL     *hival        KLTsts
082600991115     C                   MOVEL     *hival        KLTtip
082700991115     C                   MOVEL     *hival        KLTsqz
082800991117IF  1C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
082900991117     C     KEYvlt3       SETLL     tivlt03l                           99
083000970528E   1C                   ENDIF
083100991117IF  1C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
083200991117     C     KEYvlt4       SETLL     tivlt04l                           99
083300991117E   1C                   ENDIF
083400140606     C* se K_T è il tipo caricamento, devo usare solo questi 2 dati
083500140606IF  1C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
083600140606     C                   MOVEL     VIDks1        KLTksc
083700140606     C                   MOVEL     VIDTi1        KLTtip
083800140606     C     KEYvlt1       SETLL     tivlt01l                           99
083900140606E   1C                   ENDIF
084000980107     C*
084100970526     C* LETTURA RECORD
084200970526IF  1C     *IN99         IFEQ      *OFF
084300970526     C                   EXSR      LETFIL
084400970526E   1C                   ENDIF
084500970526     C*
084600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
084700970526IF  1C     *IN99         IFEQ      *ON
084800970526     C                   Z-ADD     1             SFLEOF
084900140606     C* mi salvo il posizionamento effettuato
085000140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
085100140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
085200140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
085300140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
085400140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
085500140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
085600140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
085700140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
085800140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
085900140606     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
086000140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
086100970526E   1C                   ENDIF
086200970526     C*
086300970526     C                   ENDSR
086400970526     C*------------------------------------------------------------------------*
086500970526     C* LETFIL - LETTURA RECORD
086600970526     C*------------------------------------------------------------------------*
086700970526     C     LETFIL        BEGSR
086800970526     C*
086900970526     C* LEGGE FINO A:
087000970526     C*     - FINE FILE
087100970526     C*     - TROVATO RECORD VALIDO
087200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
087300970526DO  1C     *IN99         DOWEQ     *OFF
087400970526     C     WRECOK        ANDEQ     'N'
087500970526     C*
087600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
087700991117IF  2C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
087800991115     C                   READ      tivlt03l                               99
087900970528E   2C                   ENDIF
088000991117IF  2C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
088100991117     C                   READ      tivlt04l                               99
088200991117E   2C                   ENDIF
088300140606     C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
088400140606     C* la chiave è solo questa per questo tipo di caricamento
088500140606     C     KEYVlt1       READE     tivlt01l                               99
088600140606     C                   ENDIF
088700970528     C*
088800970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
088900970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
089000970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
089100970530X   2C                   ELSE
089200970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
089300970530E   2C                   ENDIF
089400970526E   1C                   ENDDO
089500970526     C*
089600970526     C                   ENDSR
089700970526     C*------------------------------------------------------------------------*
089800970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
089900970526     C*------------------------------------------------------------------------*
090000970526     C     CHKREC        BEGSR
090100970526     C*
090200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
090300001228     C*
090400001228     C*filtra se impostato tipo file
090500010427     C                   if        WRECOK = 'S'
090600001228     C     vidti1        ifeq      *blank                                       *ALLA DATA
090700001228     C                   else
090800001228     C                   if        %subst(vidti1:1:1) = '*'
090900001228     C                   if        %subst(vidti1:2:1) <>
091000001228     C                             %subst(vlttip:2:1)
091100001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
091200001228     C                   endif
091300001228     C                   else
091400001228     C                   if        %subst(vidti1:2:1) = '*'
091500001228     C                   if        %subst(vidti1:1:1) <>
091600001228     C                             %subst(vlttip:1:1)
091700001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
091800001228     C                   endif
091900001228     C                   else
092000001228     C                   if        vidti1 <> vlttip
092100001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
092200001228     C                   endif
092300001228     C                   endif
092400001228     C                   endif
092500001228     C                   endif
092600010427     C                   endif
092700001228     C*
092800001031     C*filtra se impostato stato del log
092900010427     C                   if        WRECOK = 'S'
093000991108     C     vidst1        ifeq      *blank                                       *ALLA DATA
093100001228     C                   ELSE
093200991115     C     vidst1        ifne      vltsts                                       *ALLA DATA
093300991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
093400001228     C                   ENDIF
093500001228     C                   ENDIF
093600010427     C                   ENDIF
093700970526     C*
093800001031     C*filtra se impostato codice cliente
093900010427     C                   if        WRECOK = 'S'
094000001031     C     vidks1        ifeq      *zeros                                       *ALLA DATA
094100991108     C     vidks1        oreq      *blank                                       *ALLA DATA
094200001228     C                   ELSE
094300991115     C     vidks1        ifne      vltksc                                       *ALLA DATA
094400991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
094500001228     C                   ENDIF
094600001228     C                   ENDIF
094700010427     C                   ENDIF
094800991108     C*
094900001031     C*filtra se impostata data del log
095000010427     C                   if        WRECOK = 'S'
095100001031     C     viddt1        ifeq      *zeros                                       *ALLA DATA
095200001228     C                   ELSE
095300991108     C                   Z-ADD     viddt1        CHKDATA
095400991108     C                   Z-ADD     AAAA          AAINV
095500991108     C                   Z-ADD     MM            MMINV
095600991108     C                   Z-ADD     GG            GGINV
095700991115     C     invdata       ifne      vltdat                                       *ALLA DATA
095800991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
095900001228     C                   ENDIF
096000001228     C                   ENDIF
096100010427     C                   ENDIF
096200001228     C*
096300001228     C*filtra se impostato il tipo servizio
096400010427     C                   if        WRECOK = 'S'
096500001228     C     vidsv1        ifeq      *blank
096600001228     C                   else
096700001228     C                   if        %subst(vidsv1:1:1) = '*'
096800001228     C                   if        %subst(vidsv1:2:1) <>
096900001228     C                             %subst(vltisv:2:1)
097000001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
097100001228     C                   endif
097200001228     C                   else
097300001228     C                   if        %subst(vidsv1:2:1) = '*'
097400001228     C                   if        %subst(vidsv1:1:1) <>
097500001228     C                             %subst(vltisv:1:1)
097600001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
097700001228     C                   endif
097800001228     C                   else
097900001228     C                   if        vidsv1 <> vltisv
098000001228     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
098100001228     C                   endif
098200001228     C                   endif
098300001228     C                   endif
098400001228     C                   endif
098500010427     C                   endif
098600010427     C*
098700010427     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
098800010427     C                   if        WRECOK = 'S'
098900010427     C                   move      vltksu        wcli              7 0          -codice
099000010427     C                   exsr      dectib
099100010427     C     VIDsrs        ifeq      *blanks                                      *ALLA DATA
099200010427     C                   else
099300010709     C*                  if        %subst(wddksc:1:%len(%trim(VIDsrs)))<>
099400010709     C*                            %trim(VIDsrs)
099500010709     C                   z-add     *zeros        pos               2 0
099600010709     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
099700010709     C                   if        pos = *zeros
099800010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
099900010427     C                   endif
100000010427     C                   endif
100100010427     C                   endif
100200090518     C*
100300090518     C* PROGRESSIVO TRANSAZIONE DIVERSO DA QUELLA SCELTA
100400090518     C                   if        WRECOK = 'S'
100500090518     C     VIDspr        ifeq      *blanks
100600090518     C                   else
100700090518     C                   z-add     *zeros        pos               2 0
100800090518     C                   eval      pos = %scan(%trim(VIDspr):VLTprg)
100900090518     C                   if        pos = *zeros
101000090518     C                   z-add     *zeros        pos               2 0
101100090518     C                   eval      pos = %scan(%trim(VLTprg):VIDspr)
101200090518     C                   if        pos = *zeros
101300090518     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
101400090518     C                   endif
101500090518     C                   endif
101600090518     C                   endif
101700090518     C                   endif
101800100311     C*
101900100311     C*filtra se impostato il Traduttore
102000100311     C                   if        WRECOK = 'S'
102100100311     C     vidpgi        ifeq      *blanks
102200100311     C                   else
102300100311     C                   if        %scan(%trim(vidPGI):vltPGI) > *zeros
102400100311     C                   else
102500100311     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
102600100311     C                   endif
102700100311     C                   ENDIF
102800100311     C                   ENDIF
102900101018     C*
103000101018     C*filtra se impostato l'ID processo
103100101018     C                   if        WRECOK = 'S'
103200101018     C     vidproc       ifeq      *blanks
103300101018     C                   else
103400101018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103500101018     C                   if        vltsts <> '0'             AND
103600101018     C                             %subst(vltmsg:28:1) = 'P' AND
103700101018     C                             (%subst(vltmsg:32:3) = 'FTP' OR
103800101018     C                              %subst(vltmsg:32:3) = 'ftp')
103900101018     C                   if        vidproc = %subst(vltmsg:29:2)
104000101018     C                   MOVEL     'S'           WRECOK                         *RECORD NON VALIDO
104100101018     C                   endif
104200101018     C                   endif
104300101018     C                   endif
104400101018     C                   endif
104500991108     C*
104600970526     C                   ENDSR
104700010423     C*------------------------------------------------------------------------*
104800010423     C* dectib - Controlla esistenza codice cliente
104900010423     C*------------------------------------------------------------------------*
105000010423     C     dectib        begsr
105100010913     C*
105200010913     C     vltksc        chain     tianv01l
105300010913     C                   if        %found(tianv01l)
105400010913     C                   movel(p)  anvdes        wddksc
105500010913     C                   else
105600010423     C                   CLEAR                   BS69DS
105700010423     C                   CLEAR                   ACODS
105800010423     C                   MOVEL     KNSIF         I69SIF
105900010423     C                   Z-ADD     WCLI          I69KAC
106000010423     C                   CALL      'TIBS69R'
106100010423     C                   PARM                    BS69DS
106200010423     C                   PARM                    ACODS
106300010423     C                   PARM                    INDDS
106400010423     C                   PARM                    CLPDS
106500010423     C                   PARM                    CLSDS
106600010423     C     O69ERR        IFNE      '1'
106700010423     C                   MOVEL     ACORAG        wddksc           40
106800010423     C                   ELSE
106900010423     C                   MOVEL     *ALL'*'       Wddksc
107000010913     C                   MOVE      '1'           errl00
107100010913     C                   ENDIF
107200010423     C                   ENDIF
107300010913     C*
107400010913     C                   endsr
107500970526     C*------------------------------------------------------------------------*
107600970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
107700970526     C*------------------------------------------------------------------------*
107800970526     C     IRESFL        BEGSR
107900970526     C*
108000970526     C                   MOVEL     *BLANKS       OPZKEY
108100991115     C                   Z-ADD     vltdat        VIHdati
108200991115     C                   Z-ADD     vltdat        G08INV
108300991108     C                   Z-ADD     *ZEROS        G08DAT
108400991108     C                   MOVEL     '3'           G08ERR
108500991108     C                   CALL      'XSRDA8'
108600991108     C                   PARM                    WLBDA8
108700991110     C                   Z-ADD     G08DAT        VIHdat
108800991110     C                   Z-ADD     VIHdat        CHKDATA
108900991110     C                   Z-ADD     GG            GGA
109000991110     C                   Z-ADD     MM            MMA
109100991110     C                   Z-ADD     AAAA          AAAAA
109200991110     C                   MOVEL     DATAA         VIDdat
109300991115     C                   z-add     vltora        VIHora
109400991110     C                   z-add     VIHora        ORAN
109500991110     C                   z-add     HH            HHA
109600991110     C                   z-add     PP            PPA
109700991110     C                   z-add     SS            SSA
109800991110     C                   MOVEL     ORAA          VIDora
109900991115     C                   MOVEL     vltksc        VIDksc
110000991115     C                   MOVE      vltksc        VIPksc
110100010423     C                   MOVEL(p)  wddksc        VIDRAG
110200010423     C                   MOVEL(p)  wddksc        VIHRAG
110300991115     C                   MOVEL     vltsts        VIDsts
110400991108     C* controllo in schiera codici e descrizioni stati di log
110500991108     C                   SETOFF                                       28
110600991108     C                   Z-ADD     1             J                 2 0
110700991115     C     vltsts        LOOKUP    STS(J)                                 28
110800991108     C     *IN28         IFEQ      *ON
110900991115     C                   MOVEL     DES(J)        DESSTS
111000000211     C     DES(J)        IFEQ      'Err  '
111100991110     C                   SETON                                        50
111200991110     C                   ELSE
111300991110     C                   SETOFF                                       50
111400991110     C                   ENDIF
111500991108     C                   ENDIF
111600991115     C     vltsqz        IFGT      *ZEROS
111700991115     C     vltfls        ANDEQ     *BLANKS
111800000211     C                   MOVEL     'Canc '       DESSTS
111900991115     C                   ENDIF
112000991115     C                   MOVEL     vlttip        VIDtip
112100991115     C* reperimento descrizione tipi file
112200991110     C                   MOVEL     'ITF'         KBEcod                         *tabella
112300991110     C                   MOVEL     *BLANKS       KBElin                         *tabella
112400991110     C                   MOVEL     *BLANKS       KBEsif                         *tabella
112500991115     C                   MOVEL     vlttip        KBEke1                         *tabella
112600991110     C                   MOVEL     *BLANKS       KBEke2                         *tabella
112700991110     C     KE2tbe        CHAIN     tntbe02l
112800020218     C                   MOVEL     tbeuni        DITF
112900020218     C                   MOVEL     §ITFDESC      DEHtip                         *tabella
113000020218     C                   MOVEL     §ITFDESC      DEDtip                         *tabella
113100991115     C                   MOVEL     vltksc        KIRksc
113200991115     C                   MOVEL     vlttip        KIRtip
113300991108     C                   Z-ADD     *zeros        KIRdti
113400991108     C     KEYvir        CHAIN     tivir02l
113500991108     C     virfit        IFNE      *BLANKS
113600030924     C***                MOVEL     virfit        DEStip
113700991110     C                   MOVEL     virfid        DEHti1
113800030924     C***                MOVEL     virfis        DEHti2
113900011029     C                   ELSE
114000011029     C* Re-inizializzo degli importantissimi campi di wrk
114100011029     C                   EVAL      DEStip = *blanks
114200011029     C                   EVAL      DEHti1 = *blanks
114300011029     C                   EVAL      DEHti2 = *blanks
114400010910     C                   ENDIF
114500991115     C     vlttip        CHAIN     tivtf01l
114600010910     C                   IF        DEStip = *blanks
114700030924     C***                MOVEL     vtffit        DEStip
114800010910     C                   ENDIF
114900010910     C                   IF        DEHti1 = *blanks
115000991110     C                   MOVEL     vtffid        DEHti1
115100010910     C                   ENDIF
115200010910     C                   IF        DEHti2 = *blanks
115300030924     C***                MOVEL     vtffis        DEHti2
115400010910     C                   ENDIF
115500991115     C     vltfls        IFNE      *BLANKS
115600991115     C                   MOVEL     '*LIBL'       VIHlis
115700991115     C                   ELSE
115800991115     C                   MOVEL     *BLANKS       VIHlis
115900991115     C                   ENDIF
116000030924     C                   MOVEL     vltfld        DEStip
116100030924     C                   MOVEL     vltfls        DEHti2
116200991115     C                   MOVEL     '*LIBL'       VIHlis2
116300991115     C                   MOVEL     vltfls        VIHfls
116400000315     C                   MOVEL     DEHti1        VIHfld
116500991115     C                   MOVEL     vltmbs        VIHmbs
116600991115     C                   MOVEL     vltmbd        VIHmbd
116700030521     C                   EVAL      VIHMBI='M'+%subst(VIDKSC:2:7)
116800991115     C                   MOVEL     vltmsg        MESS
116900991110     C                   MOVEL     MS1           DEHMS1
117000991110     C                   MOVEL     MS2           DEHMS2
117100991115     C                   MOVEL     vltpgi        VIHpgi
117200991115     C                   Z-ADD     vltsqz        VIHsqz
117300000329     C                   MOVEL     vltfld        V2HFLD                         *very importante
117400000329     C                   MOVEL     vltmbd        V2HMBD                         *very importante
117500000329     C                   MOVEL     vltksu        VIHKSU                         *very importante
117600001031     C                   MOVEL     vltisv        VIDISV                         *very importante
117700000623     C                   MOVEL     vltprg        VIHPRG                         *very importante
117800000713     C                   MOVEL     vltfl2        VIHFL2                         *very importante
117900001031     C                   MOVEL     vlttia        VIHTIA
118000011029     C*
118100970526     C                   ENDSR
118200970526     C*------------------------------------------------------------------------*
118300140606     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
118400970526     C*------------------------------------------------------------------------*
118500970526     C     CHKPAR        BEGSR
118600970528     C*
118700991108     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *03L
118800991117     C                   MOVEL     'DAT'         WTIPRI            3
118900991108IF  1C     VIDdt2        IFEQ      *zeros
119000991025     C     VIDks2        ANDEQ     *ZEROS
119100991108IF  1C     VIDdt2        orEQ      *zeros
119200991025     C     VIDks2        ANDEQ     *blanks
119300991117     C                   MOVEL     'DAT'         WTIPRI
119400991025E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
119500970528     C*
119600970528     C* CONTROLLA IL TIPO RICERCA
119700991012IF  1C     ERRL00        IFEQ      '0'
119800991108IF  2C     VIDdt2        IFNE      *zeros
119900991117     C                   MOVEL     'DAT'         WTIPRI
120000991117X   2C                   ELSE
120100991117IF  3C     VIDks2        IFNE      *zeros
120200991117IF  3C     VIDks2        andne     *blanks
120300991117     C                   MOVEL     'KSC'         WTIPRI
120400991117E   3C                   ENDIF
120500991012E   2C                   ENDIF
120600991012E   1C                   ENDIF
120700140606     C* se i parametri di ingresso sono valorizzati
120800140610     C* e i rispettivi parametri video hanno gli stessi valori
120900140610     C* e nessun altro parametro video è valorizzato
121000140606     C* allora siamo nel caso di chiamata
121100140606     C* da pgm che passa i parametri in input e quindi uso la VL 01L
121200140606IF  3C     DEPksc        IFNE      *zeros
121300140606IF  3C     DEPksc        andne     *blanks
121400140606IF  3C     DEPTip        andne     *blanks
121500140610IF  3C     DEPksc        andeq     VIDKs1
121600140610IF  3C     DEPTip        andeq     VIDTi1
121700140606IF  3C     VIDst1        andeq     *blanks
121800140606IF  3C     VIDdt1        andeq     0
121900140606IF  3C     VIDsv1        andeq     *blanks
122000140606IF  3C     VIDks2        andeq     *blanks
122100140606IF  3C     VIDdt2        andeq     0
122200140606IF  3C     VIDsrs        andeq     *blanks
122300140606IF  3C     VIDspr        andeq     *blanks
122400140606IF  3C     VIDpgi        andeq     *blanks
122500140606IF  3C     VIDproc       andeq     *blanks
122600140606     C                   MOVEL     'K_T'         WTIPRI
122700140606     C* visto che so che la userò, apro la VL 01L
122800140606     C                   IF        not %open(TIVLT01L)
122900140606     C                   OPEN      TIVLT01L
123000140606     C                   ENDIF
123100140606E   3C                   ENDIF
123200991012      *
123300970526     C                   ENDSR
123400970526     C*------------------------------------------------------------------------*
123500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
123600970526     C*------------------------------------------------------------------------*
123700970526     C     CHKFOP        BEGSR
123800991112     C*
123900991112     C* OPZIONI
124000970626     C*
124100970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
124200991112IF  1C     DEPOP0        IFEQ      'P05'
124300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
124400970626     C                   MOVEL     'S'           OPDATT
124500970626     C                   MOVEL     OPDRIG        OPD(5)
124600970626E   1C                   ENDIF
124700970626     C*
124800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
124900970626IF  1C     DEPOP0        IFEQ      'P02'
125000000329     C                   MOVEL     OPD(1)        OPDRIG                         *RIESECUZIONE
125100970626     C                   MOVEL     'S'           OPDATT
125200991110     C                   MOVEL     OPDRIG        OPD(1)
125300000329     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE
125400000329     C                   MOVEL     'S'           OPDATT
125500000329     C                   MOVEL     OPDRIG        OPD(11)
125600000329     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
125700991014     C                   MOVEL     'S'           OPDATT
125800991110     C                   MOVEL     OPDRIG        OPD(4)
125900050607     C*                  MOVEL     OPD(14)       OPDRIG                         *CANCELLAZIONE
126000010403     C*                  MOVEL     'S'           OPDATT
126100010403     C*                  MOVEL     OPDRIG        OPD(14)
126200991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
126300991112     C                   MOVEL     'S'           OPDATT
126400991112     C                   MOVEL     OPDRIG        OPD(5)
126500991112     C                   MOVEL     OPD(15)       OPDRIG                         *INTERROGAZIONE
126600991112     C                   MOVEL     'S'           OPDATT
126700991112     C                   MOVEL     OPDRIG        OPD(15)
126800010316     C                   MOVEL     OPD(23)       OPDRIG                         *INTERROGAZIONE
126900991115     C                   MOVEL     'S'           OPDATT
127000010316     C                   MOVEL     OPDRIG        OPD(23)
127100170118     C***                MOVEL     OPD(24)       OPDRIG                         *INTERROGAZIONE
127200170118     C***                MOVEL     'S'           OPDATT
127300170118     C***                MOVEL     OPDRIG        OPD(24)
127400050901     C                   MOVEL     OPD(18)       OPDRIG                         *CREAZ. MEMBRO 00T
127500050901     C                   MOVEL     'S'           OPDATT
127600050901     C                   MOVEL     OPDRIG        OPD(18)
127700050722     C***
127800050722     C                   MOVEL     FUD(15)       FUDRIG                         *FORZA ELAB. VGD
127900050722     C                   MOVEL     'S'           FUDATT
128000050722     C                   MOVEL     FUDRIG        FUD(15)
128100101005     C                   MOVEL     FUD(18)       FUDRIG                         *FORZA ELAB. BARTVAS
128200101005     C                   MOVEL     'S'           FUDATT
128300101005     C                   MOVEL     FUDRIG        FUD(18)
128400050722     C***
128500970626E   1C                   ENDIF
128600970626     C*
128700970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
128800020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
128900991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
129000991112     C                   MOVEL     'N'           OPDATT
129100991112     C                   MOVEL     OPDRIG        OPD(4)
129200991110     C                   MOVEL     OPD(14)       OPDRIG                         *NO RIPRISTINO
129300970626     C                   MOVEL     'N'           OPDATT
129400991110     C                   MOVEL     OPDRIG        OPD(14)
129500991110     C                   MOVEL     FUD(1)        FUDRIG                         *NO INSERIMENTO
129600970626     C                   MOVEL     'N'           FUDATT
129700991110     C                   MOVEL     FUDRIG        FUD(1)
129800991110     C                   MOVEL     FUD(11)       FUDRIG                         *NO INSERIMENTO
129900991110     C                   MOVEL     'N'           FUDATT
130000991110     C                   MOVEL     FUDRIG        FUD(11)
130100970626E   1C                   ENDIF
130200970526     C*
130300970526     C* GESTIONE OPZIONI A VIDEO
130400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
130500970526     C                   EXSR      OPZGES
130600970526     C*
130700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
130800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
130900970526     C                   EXSR      FUNGES
131000970526     C*
131100970526     C                   ENDSR
131200991108     C*------------------------------------------------------------------------*
131300991108     C* FINSR - OPERAZIONI FINALI
131400991108     C*------------------------------------------------------------------------*
131500991108     C     FINSR         BEGSR
131600991108     C*
131700991108     C* CHIUDE I FILE DEI PGM CHIAMATI
131800991108     C                   CLEAR                   BS69DS
131900991108     C                   CLEAR                   ACODS
132000991108     C                   CLEAR                   INDDS
132100991108     C                   CLEAR                   CLPDS
132200991108     C                   CLEAR                   CLSDS
132300991108     C                   MOVEL     'C'           I69TLA
132400991108     C                   MOVEL     KNSIF         I69SIF
132500991108     C                   CALL      'TIBS69R'
132600991108     C                   PARM                    BS69DS
132700991108     C                   PARM                    ACODS
132800991108     C                   PARM                    INDDS
132900991108     C                   PARM                    CLPDS
133000991108     C                   PARM                    CLSDS
133100991108     C*
133200991108     C                   ENDSR
133300020916     C*--------------------------------------------------------------------------------------------*
133400020916     C* REPERISCE I DATI UTENTE
133500020916     C*--------------------------------------------------------------------------------------------*
133600020916     C     REPDATIUTE    BEGSR
133700020916     C*
133800020916     C* INIZIALIZZA VARIABILI DI WRK
133900020916     C                   CLEAR                   TIBS34DS
134000020916     C                   CLEAR                   AZUTEDS
134100020916     C                   CLEAR                   DDATIUTE
134200020916     C*
134300020916     C     *DTAARA       DEFINE    §azute        azuteds
134400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
134500020916     C                   IN(E)     *DTAARA
134600020916if  1C                   IF        %Error
134700020916     c                   EVAL      I34Tla = 'L'
134800020916     C                   CALL      'TIBS34R'
134900020916     C                   PARM                    Tibs34Ds
135000020916     C                   IN        *DTAARA
135100020916e   1C                   ENDIF
135200020916     C*
135300020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
135400020916     C                   MOVEL(P)  RSUT          DSFIRS           20
135500020916     C*
135600020916     C                   ENDSR
135700970526     C*------------------------------------------------------------------------*
135800970526     C* *INZSR - OPERAZIONI INIZIALI
135900970526     C*------------------------------------------------------------------------*
136000970526     C     *INZSR        BEGSR
136100970526     C*---
136200970526     C* RICEVIMENTO PARAMETRI
136300970526     C*---
136400970526     C     *ENTRY        PLIST
136500970612     C                   PARM                    KPJBA
136600051013     C                   MOVEL     KPJBU         PAR_INP
136700051013     C*
136800051013     C* TESTO SUBITO ALCCUNI PARAMETRI D INGRESSO
136900051013     C                   IF        DEPKSC <> *blanks
137000051013     C                   EVAL      VIDks1 = DEPKSC
137100051013     C                   ENDIF
137200051013     C                   IF        DEPTIP <> *blanks
137300051013     C                   EVAL      VIDti1 = DEPTIP
137400051013     C                   ENDIF
137500970526     C*
137600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
137700970526     C     XMSGPA        PLIST
137800970526     C                   PARM                    DSMSMS
137900970526     C                   PARM                    DSMSPG
138000970526     C                   PARM                    DSMSKE
138100970526     C                   PARM                    DSMSER
138200970526     C*
138300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
138400970526     C                   CALL      'XKEY'
138500970526     C                   PARM                    DSKEY
138600970526     C*---
138700970526     C* VARIABILI NON RIFERITE AL DATA BASE
138800970526     C*---
138900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
139000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
139100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
139200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
139300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
139400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
139500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
139600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
139700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
139800970526     C*---
139900991108     C     *LIKE         DEFINE    viddt2        Depdt2
140000991108     C     *LIKE         DEFINE    viddt1        Depdt1
140100991108     C     *LIKE         DEFINE    vidks2        Depks2
140200991108     C     *LIKE         DEFINE    vidks1        Depks1
140300991108     C     *LIKE         DEFINE    vidti1        Depti1
140400991108     C     *LIKE         DEFINE    vidst1        Depst1
140500001031     C     *LIKE         DEFINE    vidsv1        Depsv1
140600010423     C     *LIKE         DEFINE    vidsrs        Depsrs
140700090518     C     *LIKE         DEFINE    vidspr        Depspr
140800100311     C     *LIKE         DEFINE    vidpgi        Deppgi
140900101018     C     *LIKE         DEFINE    vidproc       Depproc
141000991115     C     *LIKE         DEFINE    vltdat        kltdat
141100991115     C     *LIKE         DEFINE    vltora        kltora
141200991115     C     *LIKE         DEFINE    vltksc        kltksc
141300991115     C     *LIKE         DEFINE    vltsts        kltsts
141400991115     C     *LIKE         DEFINE    vlttip        klttip
141500991115     C     *LIKE         DEFINE    vltsqz        kltsqz
141600991108     C*---
141700991108     C     *LIKE         DEFINE    tbecod        kbecod
141800991108     C     *LIKE         DEFINE    tbelin        kbelin
141900991108     C     *LIKE         DEFINE    tbesif        kbesif
142000991108     C     *LIKE         DEFINE    tbeke1        kbeke1
142100991108     C     *LIKE         DEFINE    tbeke2        kbeke2
142200991108     C     *LIKE         DEFINE    virksc        kirksc
142300991108     C     *LIKE         DEFINE    virtip        kirtip
142400991108     C     *LIKE         DEFINE    virdti        kirdti
142500970526     C* VARIABILI DI CONTROLLO
142600970526     C*---
142700970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
142800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
142900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
143000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
143100970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
143200970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
143300991110     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
143400991110     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
143500991110     C                   MOVEL     ':'           SEP3                           *VALIDITA' RECORD
143600991110     C                   MOVEL     ':'           SEP4                           *VALIDITA' RECORD
143700970526     C*
143800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
143900991115     C                   MOVEL     'TIS7GIR'     DSMSPG
144000991115     C                   MOVEL     'TIS7GIR'     MSGQUE
144100970526     C                   MOVEL     *BLANKS       MSGKEY
144200970526     C                   MOVEL     *BLANKS       DSMSMS
144300970526     C                   MOVEL     *BLANKS       DSMSKE
144400970526     C                   Z-ADD     *ZEROS        DSMSER
144500970526     C*---
144600970526     C* VARIABILI RIFERITE AL DATA BASE
144700970526     C*---
144800970526     C*---
144900970526     C* CHIAVI DI LETTURA
145000970526     C*---
145100140606     C* LETTURA parziale tivlt01L -
145200140606     C     KEYvlt1       KLIST
145300140606     C                   KFLD                    KLTksc
145400140606     C                   KFLD                    KLTtip
145500991115     C* LETTURA tivlt03L -
145600991117     C     KEYvlt3       KLIST
145700991115     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
145800991115     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
145900991115     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
146000991115     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
146100991115     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
146200991115     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
146300991117     C* LETTURA tivlt04L -
146400991117     C     KEYvlt4       KLIST
146500991117     C                   KFLD                    KLTksc                         *PARTENZA/ARRIVO
146600991117     C                   KFLD                    KLTdat                         *PARTENZA/ARRIVO
146700991117     C                   KFLD                    KLTora                         *PARTENZA/ARRIVO
146800991117     C                   KFLD                    KLTtip                         *PARTENZA/ARRIVO
146900991117     C                   KFLD                    KLTsqz                         *PARTENZA/ARRIVO
147000991117     C                   KFLD                    KLTsts                         *PARTENZA/ARRIVO
147100991108     C* LETTURA "NUOVE" TABELLE
147200991108     C     KE2tbe        KLIST                                                  *tntbe02l
147300991108     C                   KFLD                    KBEcod                          -tabella
147400991108     C                   KFLD                    KBElin                          -lingua
147500991108     C                   KFLD                    KBEsif                          -s.informativo
147600991108     C                   KFLD                    KBEke1                          -lingua
147700991108     C                   KFLD                    KBEke2                          -s.informativo
147800991108     C* LETTURA tivir02L -
147900991108     C     KEYvir        KLIST
148000991108     C                   KFLD                    KIRksc                         *PARTENZA/ARRIVO
148100991108     C                   KFLD                    KIRtip                         *PARTENZA/ARRIVO
148200060413     C* LETTURA tivgd01L -
148300060413     C     KEYvgd01      KLIST
148400060413     C                   KFLD                    vgdTIP
148500060413     C                   KFLD                    vgdKSU
148600060413     C                   KFLD                    vgdTSC
148700991108     C*
148800970527     C* REPERIMENTO DATI UTENTE
148900020916     C                   EXSR      REPDATIUTE
149000970526     C*
149100970526     C* CALCOLA LA DATA CORRENTE
149200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
149300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
149400970526     C                   Z-ADD     WN8           G08DAT
149500970526     C                   Z-ADD     *ZEROS        G08INV
149600970526     C                   MOVEL     '0'           G08ERR
149700970526     C                   CALL      'XSRDA8'
149800970526     C                   PARM                    WLBDA8
149900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
150000051013     C*
150100051013     C* EFFETTUA PRIMA VERIFICA DEI PARAMETRI RICEVUTI
150200051013     C                   EXSR      CHKPAR
150300991115     C*
150400970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
150500970526     C                   EXSR      CHKFOP
150600970526     C*
150700970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
150800970530IF  1C     DEPOP0        IFEQ      'P01'
150900970530     C                   MOVEL     DEC(3)        VIDING
151000970530E   1C                   ENDIF
151100970530IF  1C     DEPOP0        IFEQ      'P02'
151200970526     C                   MOVEL     DEC(1)        VIDING
151300970530E   1C                   ENDIF
151400970530IF  1C     DEPOP0        IFEQ      'P05'
151500970526     C                   MOVEL     DEC(2)        VIDING
151600970526E   1C                   ENDIF
151700970526     C*
151800970526     C                   ENDSR
151900970526     C*------------------------------------------------------------------------*
152000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152200970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
152300970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
152400970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
152500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152700970526     C*------------------------------------------------------------------------*
152800970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
152900970526     C*------------------------------------------------------------------------*
153000970526     C     POSSFL        BEGSR
153100070918     C*
153200070918     C* INIZIALIZZO OPZIONI SALVATE
153300070918     C                   CLEAR                   SAVOPZKEY
153400970526     C*
153500970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
153600970526     C                   EXSR      CLRSFL
153700970526     C*
153800970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
153900970526     C                   EXSR      SETFIL
154000970526     C*
154100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
154200970526     C     SFLEOF        IFEQ      0
154300970526     C                   EXSR      CARSFL
154400970526     C                   END
154500970526     C*
154600970526     C                   ENDSR
154700970526     C*------------------------------------------------------------------------*
154800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
154900970526     C*------------------------------------------------------------------------*
155000970526     C     ENTATT        BEGSR
155100970526     C*
155200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
155300970526     C                   Z-ADD     0             OPZOK             1 0
155400970526     C*
155500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
155600970526     C                   Z-ADD     0             SFLVAR            5 0
155700970526     C*
155800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
155900970526     C                   Z-ADD     0             SFLERR
156000970526     C*
156100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
156200970526     C                   Z-ADD     SFLULT        WFLULT
156300970526     C*
156400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
156500991115     C                   READC     s7GID05                                98
156600970526     C     *IN98         DOWEQ     '0'
156700970526     C*
156800970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
156900970526     C                   ADD       1             SFLVAR
157000970526     C*
157100970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
157200970526     C     OPZOK         IFEQ      0
157300970526     C*
157400970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
157500970526     C     OPZKEY        IFNE      *BLANKS
157600970526     C                   EXSR      OPZCON
157700970526     C                   END
157800970526     C*
157900970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
158000970526     C*            2. PULIZIA SCELTA IN SFL
158100970526     C     OPZOK         IFEQ      0
158200970526     C                   MOVEL     '0'           *IN41
158300970526     C                   MOVEL     *BLANKS       OPZKEY
158400970526     C                   ELSE
158500970526     C*
158600970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
158700970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
158800970526     C                   MOVEL     '1'           *IN41
158900970526     C                   Z-ADD     SFLULT        SFLERR
159000970526     C                   END
159100970526     C                   ELSE
159200970526     C*
159300970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
159400970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
159500970526     C                   MOVEL     '1'           *IN41
159600970526     C                   END                                                    OPZOK=0
159700970526     C*
159800970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
159900970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
160000970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
160100970526     C     OPZOK         IFNE      0
160200970526     C                   Z-ADD     SFLERR        SFLNBR
160300970526     C                   ELSE
160400970526     C                   Z-ADD     SFLULT        SFLNBR
160500970526     C                   END
160600970526     C*
160700970526     C* AGGIORNO RECORD SFL
160800020212     C                   EVAL      *IN50 = HIN50
160900991115     C                   UPDATE    s7GID05
161000970526     C*
161100991115     C                   READC     s7GID05                                98
161200970526     C                   END                                                    DO WHILE
161300970526     C*
161400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
161500970526     C                   Z-ADD     WFLULT        SFLULT
161600970526     C*
161700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
161800970526     C     SFLVAR        IFEQ      0
161900970526     C*---                 MOVEL'1'       FINL00
162000970526     C                   END
162100970526     C*
162200070918     C     SAVOPZKEY     IFNE      ' 5'
162300070918     C     SAVOPZKEY     ANDNE     '15'
162400070918     C     SAVOPZKEY     ANDNE     '25'
162500070918     C                   EXSR      F05GES
162600070918     C                   ENDIF
162700991112     C*
162800970526     C                   ENDSR
162900970526     C*------------------------------------------------------------------------*
163000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
163100970526     C*------------------------------------------------------------------------*
163200970526     C     CLRSFL        BEGSR
163300970526     C*
163400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
163500970526     C* (0=NON FINE FILE  1=FINE FILE)
163600970526     C                   Z-ADD     0             SFLEOF
163700970526     C*
163800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
163900970526     C                   Z-ADD     0             SFLULT
164000970526     C*
164100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
164200970526     C                   MOVEL     '1'           *IN40
164300991115     C                   WRITE     s7GID06
164400970526     C                   MOVEL     '0'           *IN40
164500970526     C*
164600970526     C                   ENDSR
164700970526     C*------------------------------------------------------------------------*
164800970526     C* CARSFL - CARICAMENTO SFL
164900970526     C*------------------------------------------------------------------------*
165000970526     C     CARSFL        BEGSR
165100970526     C*
165200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
165300970526     C     SFLULT        IFEQ      0
165400970526     C                   EXSR      SETFIL
165500970526     C                   END
165600970526     C*
165700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
165800970526     C                   Z-ADD     SFLULT        WFLULT
165900970526     C*
166000970526     C* CICLO:
166100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
166200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
166300970526     C                   Z-ADD     1             SFLRIG
166400970526     C     SFLRIG        DOWLE     SFLPAG
166500970526     C     SFLEOF        ANDEQ     0
166600970526     C*
166700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
166800991116     C                   MOVEL     *BLANKS       OPZKEY
166900970526     C*
167000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
167100970526     C                   EXSR      IRESFL
167200970526     C*
167300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
167400970526     C                   ADD       1             SFLULT
167500970526     C*
167600970526     C* SCRITTURA RECORD SFL
167700020212     C                   EVAL      HIN50 = *IN50
167800991115     C                   WRITE     s7GID05
167900970526     C                   ADD       1             SFLRIG
168000970526     C*
168100970526     C* LETTURA FILE DA CARICARE
168200970526     C                   EXSR      LETFIL
168300970526     C                   END                                                    DO WHILE
168400970526     C*
168500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
168600970526     C     WFLULT        IFNE      SFLULT
168700970526     C                   Z-ADD     WFLULT        SFLNBR
168800970526     C                   ADD       1             SFLNBR
168900140606     C* mi salvo il posizionamento effettuato
169000140606     C* mi salvo il posizionamento effettuato
169100140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
169200140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
169300140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
169400140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
169500140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
169600140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
169700140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
169800140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
169900140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
170000140606     C                   MOVEL     VIDpgi        DEPpgi                         *DEPOSITI
170100140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
170200970526     C                   END
170300970526     C*
170400970526     C                   ENDSR
170500970526     C*------------------------------------------------------------------------*
170600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
170700970526     C*------------------------------------------------------------------------*
170800970526     C     OUTSFL        BEGSR
170900970526     C*
171000970526     C* WRITE RECORD TESTATA VIDEO
171100991115     C                   WRITE     s7GID01
171200970526     C*
171300970526     C* VISUALIZZAZIONE MESSAGGIO
171400970526     C                   EXSR      SNDMSG
171500970526     C*
171600970526     C* WRITE RECORD OPZIONI VIDEO
171700991115     C                   WRITE     s7GID02
171800970526     C*
171900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
172000991115     C                   WRITE     s7GID03
172100970526     C*
172200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
172300991115     C                   WRITE     s7GID04
172400970526     C*
172500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
172600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
172700970526     C     SFLEOF        IFEQ      0
172800970526     C                   MOVEL     '0'           *IN42
172900970526     C                   ELSE
173000970526     C*
173100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
173200970526     C                   MOVEL     '1'           *IN42
173300970526     C                   END
173400970526     C*
173500970526     C* WRITE RECORD TASTI FUNZIONALI
173600991115     C                   WRITE     s7GID09
173700970526     C*
173800970526     C* CASO DI SFL VUOTO
173900970526     C     SFLULT        IFLE      0
174000970526     C*
174100970526     C*    - WRITE RECORD VUOTO
174200991115     C                   WRITE     s7GID0A
174300991115     C                   READ      s7GID0A                                99
174400970526     C*
174500970526     C                   ELSE
174600970526     C*
174700970526     C* CASO DI SFL CON RECORDS
174800970526     C*
174900970526     C*    - WRITE RECORD CONTROLLO SFL
175000991115     C                   WRITE     s7GID06
175100991115     C                   READ      s7GID06                                99
175200970526     C                   END
175300970526     C*
175400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
175500991115     C                   READ      s7GID03                                99
175600970526     C*
175700970526     C                   ENDSR
175800970526     C*------------------------------------------------------------------------*
175900970526     C* SNDMSG - MANDA MESSAGGIO
176000970526     C*------------------------------------------------------------------------*
176100970526     C     SNDMSG        BEGSR
176200970526     C*
176300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
176400970526     C                   MOVEL     '1'           *IN40
176500991115     C                   WRITE     s7GIDMC
176600970526     C                   MOVEL     '0'           *IN40
176700970526     C*
176800970526     C* VISUALIZZAZIONE MESSAGGIO
176900970526     C                   Z-ADD     0             MSGULT
177000970526     C                   CALL      'XMSG'        XMSGPA
177100970526     C     DSMSER        IFEQ      0
177200970526     C                   MOVEL     DSMSKE        MSGKEY
177300970526     C                   END
177400970526     C                   Z-ADD     1             MSGULT
177500991115     C                   WRITE     s7GIDMS                                      SFL MESS
177600991115     C                   WRITE     s7GIDMC                                      CONTR MESS
177700970526     C*
177800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
177900970526     C                   MOVEL     *BLANKS       DSMSMS
178000970526     C*
178100970526     C                   ENDSR
178200970526     C*------------------------------------------------------------------------*
178300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
178400970526     C*------------------------------------------------------------------------*
178500970526     C     FUNCON        BEGSR
178600970526     C*
178700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
178800970526     C                   EXSR      FUNVAR
178900970526     C*
179000970526     C* PULISCO CAMPI DI LAVORO
179100970526     C                   Z-ADD     0             I                 5 0
179200970526     C                   Z-ADD     0             W                 5 0
179300970526     C*
179400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
179500970526     C     FUNKEY        IFNE      *BLANKS
179600970526     C     1             DO        24            I
179700970526     C                   MOVEL     FUD(I)        FUDRIG
179800970526     C*
179900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
180000970526     C     FUNKEY        IFEQ      FUDFUN
180100970526     C     FUDATT        ANDEQ     'S'
180200970526     C                   Z-ADD     I             W
180300970526     C                   END
180400970526     C                   END                                                    DO WHILE
180500970526     C*
180600970526     C* FUNZIONE NON ATTIVA (ERRORE)
180700970526     C     W             IFEQ      0
180800970526     C*
180900970526     C* EMISSIONE MESSAGGIO DI ERRORE
181000970526     C                   MOVEL     MSG(2)        DSMSMS
181100970526     C                   END                                                    W=0
181200970526     C                   END                                                    FUNKEY NE *BLANKS
181300970526     C*
181400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
181500970526     C     W             IFNE      0
181600970526     C     FUNKEY        OREQ      *BLANKS
181700970526     C*
181800970526     C     FUNKEY        IFEQ      *BLANKS
181900970526     C                   EXSR      ENTGES
182000970526     C                   END
182100970526     C     FUNKEY        IFEQ      '03'
182200970526     C                   EXSR      F03GES
182300970526     C                   END
182400970526     C     FUNKEY        IFEQ      '05'
182500970526     C                   EXSR      F05GES
182600970526     C                   END
182700970526     C     FUNKEY        IFEQ      '06'
182800970526     C                   EXSR      F06GES
182900970526     C                   END
183000970526     C     FUNKEY        IFEQ      '12'
183100970526     C                   EXSR      F12GES
183200970526     C                   END
183300050722     C     FUNKEY        IFEQ      '15'
183400050722     C                   EXSR      F15GES
183500050722     C                   END
183600101005     C     FUNKEY        IFEQ      '18'
183700101005     C                   EXSR      F18GES
183800101005     C                   END
183900970528     C     FUNKEY        IFEQ      '21'
184000970528     C                   EXSR      F21GES
184100970528     C                   END
184200970528     C     FUNKEY        IFEQ      '22'
184300970528     C                   EXSR      F22GES
184400970528     C                   END
184500970526     C     FUNKEY        IFEQ      '23'
184600970526     C                   EXSR      F23GES
184700970526     C                   END
184800970526     C     FUNKEY        IFEQ      '24'
184900970526     C                   EXSR      F24GES
185000970526     C                   END
185100970526     C     FUNKEY        IFEQ      'RU'
185200970526     C                   EXSR      RUPGES
185300970526     C                   END
185400970526     C*
185500970526     C                   END
185600970526     C*
185700970526     C                   ENDSR
185800970526     C*------------------------------------------------------------------------*
185900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
186000970526     C*------------------------------------------------------------------------*
186100970526     C     FUNVAR        BEGSR
186200970526     C*
186300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
186400970526     C     KEY           IFEQ      ENTER
186500970526     C                   MOVEL     *BLANKS       FUNKEY
186600970526     C                   END
186700970526     C     KEY           IFEQ      F1
186800970526     C                   MOVEL     '01'          FUNKEY
186900970526     C                   END
187000970526     C     KEY           IFEQ      F2
187100970526     C                   MOVEL     '02'          FUNKEY
187200970526     C                   END
187300970526     C     KEY           IFEQ      F3
187400970526     C                   MOVEL     '03'          FUNKEY
187500970526     C                   END
187600970526     C     KEY           IFEQ      F4
187700970526     C                   MOVEL     '04'          FUNKEY
187800970526     C                   END
187900970526     C     KEY           IFEQ      F5
188000970526     C                   MOVEL     '05'          FUNKEY
188100970526     C                   END
188200970526     C     KEY           IFEQ      F6
188300970526     C                   MOVEL     '06'          FUNKEY
188400970526     C                   END
188500970526     C     KEY           IFEQ      F7
188600970526     C                   MOVEL     '07'          FUNKEY
188700970526     C                   END
188800970526     C     KEY           IFEQ      F8
188900970526     C                   MOVEL     '08'          FUNKEY
189000970526     C                   END
189100970526     C     KEY           IFEQ      F9
189200970526     C                   MOVEL     '09'          FUNKEY
189300970526     C                   END
189400970526     C     KEY           IFEQ      F10
189500970526     C                   MOVEL     '10'          FUNKEY
189600970526     C                   END
189700970526     C     KEY           IFEQ      F11
189800970526     C                   MOVEL     '11'          FUNKEY
189900970526     C                   END
190000970526     C     KEY           IFEQ      F12
190100970526     C                   MOVEL     '12'          FUNKEY
190200970526     C                   END
190300970526     C     KEY           IFEQ      F13
190400970526     C                   MOVEL     '13'          FUNKEY
190500970526     C                   END
190600970526     C     KEY           IFEQ      F14
190700970526     C                   MOVEL     '14'          FUNKEY
190800970526     C                   END
190900970526     C     KEY           IFEQ      F15
191000970526     C                   MOVEL     '15'          FUNKEY
191100970526     C                   END
191200970526     C     KEY           IFEQ      F16
191300970526     C                   MOVEL     '16'          FUNKEY
191400970526     C                   END
191500970526     C     KEY           IFEQ      F17
191600970526     C                   MOVEL     '17'          FUNKEY
191700970526     C                   END
191800970526     C     KEY           IFEQ      F18
191900970526     C                   MOVEL     '18'          FUNKEY
192000970526     C                   END
192100970526     C     KEY           IFEQ      F19
192200970526     C                   MOVEL     '19'          FUNKEY
192300970526     C                   END
192400970526     C     KEY           IFEQ      F20
192500970526     C                   MOVEL     '20'          FUNKEY
192600970526     C                   END
192700970526     C     KEY           IFEQ      F21
192800970526     C                   MOVEL     '21'          FUNKEY
192900970526     C                   END
193000970526     C     KEY           IFEQ      F22
193100970526     C                   MOVEL     '22'          FUNKEY
193200970526     C                   END
193300970526     C     KEY           IFEQ      F23
193400970526     C                   MOVEL     '23'          FUNKEY
193500970526     C                   END
193600970526     C     KEY           IFEQ      F24
193700970526     C                   MOVEL     '24'          FUNKEY
193800970526     C                   END
193900970526     C     KEY           IFEQ      ROLLUP
194000970526     C                   MOVEL     'RU'          FUNKEY
194100970526     C                   END
194200970526     C     KEY           IFEQ      HELP
194300970526     C                   MOVEL     'HP'          FUNKEY
194400970526     C                   END
194500970526     C*
194600970526     C                   ENDSR
194700970526     C*------------------------------------------------------------------------*
194800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
194900970526     C*------------------------------------------------------------------------*
195000970526     C     FUNGES        BEGSR
195100970526     C*
195200970526     C* PULISCO CAMPI DI LAVORO
195300970526     C                   Z-ADD     0             W                 5 0
195400970526     C*
195500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
195600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
195700970526     C     1             DO        24            I
195800970526     C                   MOVEL     FUD(I)        FUDRIG
195900970526     C*
196000970526     C* PULIZIA FUNZIONI PRECEDENTI
196100970526     C                   MOVEL     *BLANKS       FUV(I)
196200970526     C*
196300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
196400970526     C* (SALTA F23 E F24)
196500970526     C     FUDATT        IFEQ      'S'
196600970526     C     FUDFUN        ANDNE     '23'
196700970526     C     FUDFUN        ANDNE     '24'
196800970526     C                   ADD       1             W
196900970526     C                   MOVEL     FUDDES        FUV(W)
197000970526     C                   END
197100970526     C                   END                                                    DO WHILE
197200970526     C*
197300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
197400970526     C* ALTRIMENTI DISATTIVO F24
197500970526     C     1             DO        24            I
197600970526     C                   MOVEL     FUD(I)        FUDRIG
197700970526     C*
197800970526     C     FUDFUN        IFEQ      '24'
197900970526     C     W             IFGT      6
198000970526     C                   MOVEL     'S'           FUDATT
198100970526     C                   ELSE
198200970526     C                   MOVEL     *BLANKS       FUDATT
198300970526     C                   END                                                    W GT 8
198400970526     C                   MOVEL     FUDRIG        FUD(I)
198500970526     C                   END
198600970526     C                   END                                                    DO WHILE
198700970526     C*
198800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
198900970526     C     W             DIV       6             I
199000970526     C                   MVR                     Z
199100970526     C*
199200970526     C     Z             IFNE      0
199300970526     C     I             ADD       1             Z
199400970526     C                   ELSE
199500970526     C                   Z-ADD     I             Z
199600970526     C                   END
199700970526     C*
199800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
199900970526     C                   ADD       1             FUNGRU
200000970526     C     FUNGRU        IFGT      Z
200100970526     C     FUNGRU        ORLT      1
200200970526     C                   Z-ADD     1             FUNGRU
200300970526     C                   END
200400970526     C*
200500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
200600970526     C     FUNGRU        IFEQ      1
200700970526     C                   MOVEL     FUV(1)        FUN001
200800970526     C                   MOVEL     FUV(2)        FUN002
200900970526     C                   MOVEL     FUV(3)        FUN003
201000970526     C                   MOVEL     FUV(4)        FUN004
201100970526     C                   MOVEL     FUV(5)        FUN005
201200970526     C                   MOVEL     FUV(6)        FUN006
201300970526     C                   END
201400970526     C*
201500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
201600970526     C     FUNGRU        IFEQ      2
201700970526     C                   MOVEL     FUV(7)        FUN001
201800970526     C                   MOVEL     FUV(8)        FUN002
201900970526     C                   MOVEL     FUV(9)        FUN003
202000970526     C                   MOVEL     FUV(10)       FUN004
202100970526     C                   MOVEL     FUV(11)       FUN005
202200970526     C                   MOVEL     FUV(12)       FUN006
202300970526     C                   END
202400970526     C*
202500970526     C* TERZO GRUPPO DI 6 FUNZIONI
202600970526     C     FUNGRU        IFEQ      3
202700970526     C                   MOVEL     FUV(13)       FUN001
202800970526     C                   MOVEL     FUV(14)       FUN002
202900970526     C                   MOVEL     FUV(15)       FUN003
203000970526     C                   MOVEL     FUV(16)       FUN004
203100970526     C                   MOVEL     FUV(17)       FUN005
203200970526     C                   MOVEL     FUV(18)       FUN006
203300970526     C                   END
203400970526     C*
203500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
203600970526     C     FUNGRU        IFEQ      4
203700970526     C                   MOVEL     FUV(19)       FUN001
203800970526     C                   MOVEL     FUV(20)       FUN002
203900970526     C                   MOVEL     FUV(21)       FUN003
204000970526     C                   MOVEL     FUV(22)       FUN004
204100970526     C                   MOVEL     *BLANKS       FUN005
204200970526     C                   MOVEL     *BLANKS       FUN006
204300970526     C                   END
204400970526     C*
204500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
204600970526     C                   MOVEL     *BLANKS       FUN007
204700970526     C                   MOVEL     *BLANKS       FUN008
204800970526     C*
204900970526     C     1             DO        24            I
205000970526     C                   MOVEL     FUD(I)        FUDRIG
205100970526     C     FUDFUN        IFEQ      '23'
205200970526     C     FUDATT        ANDEQ     'S'
205300970526     C                   MOVEL     FUDDES        FUN007
205400970526     C                   END
205500970526     C     FUDFUN        IFEQ      '24'
205600970526     C     FUDATT        ANDEQ     'S'
205700970526     C                   MOVEL     FUDDES        FUN008
205800970526     C                   END
205900970526     C                   END                                                    DO
206000970526     C*
206100970526     C                   ENDSR
206200970526     C*------------------------------------------------------------------------*
206300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
206400970526     C*------------------------------------------------------------------------*
206500970526     C     OPZGES        BEGSR
206600970526     C*
206700970526     C* PULISCO CAMPI DI LAVORO
206800970526     C                   Z-ADD     0             W                 5 0
206900970526     C*
207000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
207100970526     C* E CARICO TABELLA OPZIONI ATTIVE
207200991115     C     1             DO        24            I
207300970526     C                   MOVEL     OPD(I)        OPDRIG
207400970526     C*
207500970526     C* PULIZIA OPZIONI PRECEDENTI
207600970526     C                   MOVEL     *BLANKS       OPV(I)
207700970526     C*
207800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
207900970526     C     OPDATT        IFEQ      'S'
208000970526     C                   ADD       1             W
208100970526     C                   MOVEL     OPDDES        OPV(W)
208200970526     C                   END
208300970526     C                   END                                                    DO WHILE
208400970526     C*
208500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
208600970526     C* ALTRIMENTI DISATTIVO F23
208700991115     C     1             DO        24            I
208800970526     C                   MOVEL     FUD(I)        FUDRIG
208900970526     C*
209000970526     C     FUDFUN        IFEQ      '23'
209100970526     C     W             IFGT      8
209200970526     C                   MOVEL     'S'           FUDATT
209300970526     C                   ELSE
209400970526     C                   MOVEL     *BLANKS       FUDATT
209500970526     C                   END                                                    W GT 8
209600970526     C                   MOVEL     FUDRIG        FUD(I)
209700970526     C                   END
209800970526     C                   END                                                    DO WHILE
209900970526     C*
210000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
210100970526     C     W             DIV       8             I
210200970526     C                   MVR                     Z                 5 0
210300970526     C*
210400970526     C     Z             IFNE      0
210500970526     C     I             ADD       1             Z
210600970526     C                   ELSE
210700970526     C                   Z-ADD     I             Z
210800970526     C                   END
210900970526     C*
211000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
211100970526     C                   ADD       1             OPZGRU
211200970526     C     OPZGRU        IFGT      Z
211300970526     C     OPZGRU        ORLT      1
211400970526     C                   Z-ADD     1             OPZGRU
211500970526     C                   END
211600970526     C*
211700970526     C* PRIMO GRUPPO DI 8 OPZIONI
211800970526     C     OPZGRU        IFEQ      1
211900970526     C                   MOVEL     OPV(1)        OPZ001
212000970526     C                   MOVEL     OPV(2)        OPZ002
212100970526     C                   MOVEL     OPV(3)        OPZ003
212200970526     C                   MOVEL     OPV(4)        OPZ004
212300970526     C                   MOVEL     OPV(5)        OPZ005
212400970526     C                   MOVEL     OPV(6)        OPZ006
212500970526     C                   MOVEL     OPV(7)        OPZ007
212600970526     C                   MOVEL     OPV(8)        OPZ008
212700970526     C                   END
212800970526     C*
212900970526     C* SECONDO GRUPPO DI 8 OPZIONI
213000970526     C     OPZGRU        IFEQ      2
213100970526     C                   MOVEL     OPV(9)        OPZ001
213200970526     C                   MOVEL     OPV(10)       OPZ002
213300970526     C                   MOVEL     OPV(11)       OPZ003
213400970526     C                   MOVEL     OPV(12)       OPZ004
213500970526     C                   MOVEL     OPV(13)       OPZ005
213600970526     C                   MOVEL     OPV(14)       OPZ006
213700970526     C                   MOVEL     OPV(15)       OPZ007
213800970526     C                   MOVEL     OPV(16)       OPZ008
213900970526     C                   END
214000970526     C*
214100970526     C* TERZO GRUPPO DI 8 OPZIONI
214200970526     C     OPZGRU        IFEQ      3
214300970526     C                   MOVEL     OPV(17)       OPZ001
214400970526     C                   MOVEL     OPV(18)       OPZ002
214500970526     C                   MOVEL     OPV(19)       OPZ003
214600970526     C                   MOVEL     OPV(20)       OPZ004
214700970526     C                   MOVEL     OPV(21)       OPZ005
214800970526     C                   MOVEL     OPV(22)       OPZ006
214900970526     C                   MOVEL     OPV(23)       OPZ007
215000970526     C                   MOVEL     OPV(24)       OPZ008
215100970526     C                   END
215200970526     C*
215300970526     C                   ENDSR
215400970526     C*------------------------------------------------------------------------*
215500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
215600970526     C*------------------------------------------------------------------------*
215700970526     C     OPZCON        BEGSR
215800050607     C*
215900050607     C* VERIFICO SUBITO SE IL RECORD D LOG CORRENTE E' TIPO "VECCHIO" O "NUOVO"
216000060413     C                   MOVEL     'N'           wNewLOG           1
216100060413     C     VIHPRG        CHAIN     tivgd02l                                     * già elaborato (S)
216200050607     C                   IF        %found(tivgd02l)
216300050607     C                   MOVEL     'S'           wNewLOG
216400060413     C                   ELSE
216500060413     C                   EVAL      vgdTIP = VIDTIP
216600060413     C                   EVAL      vgdKSU = VIHKSU
216700060413     C                   EVAL      vgdTSC = 'WW'
216800060413     C     KEYvgd01      CHAIN     tivgd01l                                     * da elaborare  ( )
216900060413     C                   IF        %found(tivgd01l)
217000060413     C                   MOVEL     'S'           wNewLOG
217100060413     C                   ENDIF
217200050607     C                   ENDIF
217300970526     C*
217400970526     C* PULISCO CAMPI DI LAVORO
217500970526     C                   Z-ADD     0             I                 5 0
217600970526     C                   Z-ADD     0             W                 5 0
217700970526     C*
217800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
217900970526     C     1             DO        24            I
218000970526     C                   MOVEL     OPD(I)        OPDRIG
218100970526     C*
218200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
218300970526     C     OPZKEY        IFEQ      OPDOPZ
218400970526     C     OPZKEY        OREQ      OPDOP1
218500970526     C*
218600970526     C     OPDATT        IFEQ      'S'
218700970526     C                   MOVEL     OPDOPZ        OPZKEY
218800970526     C                   Z-ADD     I             W
218900970526     C                   END
219000970526     C                   END
219100970526     C*
219200970526     C                   END                                                    DO WHILE
219300970526     C*
219400970526     C* OPZIONE NON ATTIVA (ERRORE)
219500970526     C     W             IFEQ      0
219600970526     C*
219700970526     C* OPZIONE NON OK
219800970526     C                   Z-ADD     1             OPZOK
219900970526     C*
220000970526     C* EMISSIONE MESSAGGIO DI ERRORE
220100970526     C                   MOVEL     MSG(1)        DSMSMS
220200970526     C                   ELSE
220300970526     C*
220400970526     C* OPZIONE ATTIVA (PROCEDI ...)
220500970526     C*
220600070918     C* SALVO L'OPZIONE CORRENTEMENTE RICHIESTA
220700070918     C                   MOVEL     OPZKEY        SAVOPZKEY         2
220800070918     C*
220900970526     C     OPZKEY        IFEQ      ' 1'
221000970526     C                   EXSR      O01GES
221100970526     C                   END
221200000328     C     OPZKEY        IFEQ      '11'
221300000328     C                   EXSR      O11GES
221400000328     C                   END
221500991110     C     OPZKEY        IFEQ      ' 4'
221600991110     C                   EXSR      O04GES
221700991014     C                   END
221800991112     C     OPZKEY        IFEQ      '14'
221900991112     C                   EXSR      O14GES
222000991112     C                   END
222100970526     C     OPZKEY        IFEQ      ' 5'
222200970526     C                   EXSR      O05GES
222300970526     C                   END
222400991112     C     OPZKEY        IFEQ      '15'
222500991112     C                   EXSR      O15GES
222600991112     C                   END
222700050901     C     OPZKEY        IFEQ      '18'
222800050901     C                   EXSR      O18GES
222900050901     C                   END
223000010316     C     OPZKEY        IFEQ      '23'
223100010316     C                   EXSR      O23GES
223200010316     C                   END
223300010316     C     OPZKEY        IFEQ      '24'
223400010316     C                   EXSR      O24GES
223500991115     C                   END
223600970526     C                   END                                                    OPZ.NON ATTIVA
223700970526     C*
223800970526     C                   ENDSR
223900970526     C*------------------------------------------------------------------------*
224000970526     C* NOTE COSTRUZIONE SCHIERE
224100970526     C*------------------------------------------------------------------------*
224200970526     C*
224300970526     C* -MSG-MESSAGGI
224400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
224500970526     C* (NON TOCCARE MSG,1 E MSG,2)
224600970526     C*
224700970526     C* -CMD-COMANDI
224800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
224900970526     C*
225000970526     C* -OPD-OPZIONI
225100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
225200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
225300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
225400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
225500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
225600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
225700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
225800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
225900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
226000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
226100970526     C*
226200970526     C* -FUD-FUNZIONI
226300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
226400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
226500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
226600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
226700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
226800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
226900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
227000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
227100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
227200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
227300991108     C*
227400991115     C* -STS-CODICI STATI DEI RECORD DI LOG
227500991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
227600991110     C* ================> IMPORTANTE <=================
227700991115     C* per codici di errore in -STS- indicare in -DES- sempre
227800991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
227900991110     C* ERR
228000991108     C*
228100970526     O*------------------------------------------------------------------------*
228200970526** DEC - DECODIFICHE
228300970526    GESTIONE                                                     1
228400970526INTERROGAZIONE                                                   2
228500970530     SCELTA                                                      3
2286009705264
2287009705265
228800970526** MSG - MESSAGGI
228900991115TIS7GIR- Opzione non gestita                                                   1
229000991115TIS7GIR- Tasto funzionale non gestito                                          2
229100080904TIS7GIR-                                                                       3
229200991115TIS7GIR- Record annullato. Consentito solo il ripristino                       4
229300991115TIS7GIR- Record non annullato. Impossibile ripristinare                        5
229400991115TIS7GIR- Accettato solo un parametro di inizio lista                           6
229500000713TIS7GIR- Opzione non valida in quanto traduzione mai eseguita (Opz.11)         7
229600000629TIS7GIR- Opzione non valida in quanto mancanti dati essenziali relativi ai file8
229700050901TIS7GIR- Errori in elaborazione del comando, verificare.                       9
229800050901TIS7GIR- Copia dati nel membro destinazione effettuata correttamente.         10
229900010316TIS7GIR- 11                                                                   11
230000991115TIS7GIR- Data errata                                                          12
230100000707TIS7GIR- Opzione consentita solo per stati record diversi da '0'              13
230200101005TIS7GIR- Effettuare selezione                                                 14
230300991115TIS7GIR- 15                                                                   15
230400991115TIS7GIR- 16                                                                   16
230500991115TIS7GIR- 17                                                                   17
230600991115TIS7GIR- 18                                                                   18
230700991115TIS7GIR- 19                                                                   19
230800001012TIS7GIR- Opzione non consentita per cliente in oggetto                        20
230900970528** CMD - COMANDI CL
231000991110DSPPFM FILE(
231100991110) MBR(
231200991110
231300970528
231400970528
231500970526** OPD - OPZIONI
231600991112 1=Esec. parziale  -1 -     (S/N)=N                        opz. 01¬
231700991110                   -  -     (S/N)=N                        opz. 02¬
231800991110                   -  -     (S/N)=N                        opz. 03¬
231900991112 4=Canc. LOG       -4 -     (S/N)=N                        opz. 04¬
232000991112 5=Visual. LOG     -5 -     (S/N)=N                        opz. 05¬
232100991115                   -  -     (S/N)=N                        opz. 06¬
232200991110                   -  -     (S/N)=N                        opz. 07¬
232300991110                   -  -     (S/N)=N                        opz. 08¬
232400991110                   -  -     (S/N)=N                        opz. 09¬
232500991110                   -  -     (S/N)=N                        opz. 10¬
23260000032911=Esec. totale    -11-     (S/N)=N                        opz. 11¬
232700970526                   -  -     (S/N)=N                        opz. 12¬
232800970526                   -  -     (S/N)=N                        opz. 13¬
23290099111214=Canc. STORICO   -14-     (S/N)=N                        opz. 14¬
23300099111215=Visual. STORICO -15-     (S/N)=N                        opz. 15¬
233100970526                   -  -     (S/N)=N                        opz. 16¬
233200970526                   -  -     (S/N)=N                        opz. 17¬
23330005083118=Crea membro 00T -18-     (S/N)=N                        opz. 18¬
233400970526                   -  -     (S/N)=N                        opz. 19¬
233500970526                   -  -     (S/N)=N                        opz. 20¬
233600970526                   -  -     (S/N)=N                        opz. 21¬
233700970526                   -  -     (S/N)=N                        opz. 22¬
23380001031623=Visual. ASCII   -23-     (S/N)=N                        opz. 23¬
23390001031624=Visual. INPUT   -24-     (S/N)=N                        opz. 24¬
234000970526** FUD - FUNZIONI
234100970526                   -        (S/N)=N                        funz.01¬
234200970526                   -        (S/N)=N                        funz.02¬
234300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
234400970526                   -        (S/N)=N                        funz.04¬
234500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
234600050901F06 NON USARE !!!  -        (S/N)=N                        funz.06¬
234700050722                   -        (S/N)=N                        funz.07¬
234800970527                   -        (S/N)=N                        funz.08¬
234900970527                   -        (S/N)=N                        funz.09¬
235000970527                   -        (S/N)=N                        funz.10¬
235100970526                   -        (S/N)=N                        funz.11¬
235200970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
235300991116                   -        (S/N)=N                        funz.13¬
235400970526                   -        (S/N)=N                        funz.14¬
235500050722F15=Forza elab. VGD-        (S/N)=N                        funz.15¬
235600970526                   -        (S/N)=N                        funz.16¬
235700970526                   -        (S/N)=N                        funz.17¬
235800101005F18=Forza elab. BV -        (S/N)=N                        funz.18¬
235900970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
236000970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
236100991110F21=Stampa elenco  -        (S/N)=N                        funz.21¬
236200991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
236300991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
236400991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
236500991115** STS - STATI DEI RECORD DI LOG
2366009911080
2367009911081
2368009911082
2369009911083
2370009911084
2371000003215
237200000329
237300991108
237400991108
237500991108
237600991108** DES - STATI DEI RECORD DI LOG
237700991112Da El
237800991108OK
237900991108Err
238000991112R-OK
238100991112R-Err
238200000321DwnL
238300000329
238400991108
238500991108
238600991108
