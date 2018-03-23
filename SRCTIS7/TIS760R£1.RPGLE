000100970526     H*------------------------------------------------------------------------*
000200991108     H* Gestione file di log TIVLR00F - Strategi Upload
000300970526     H*------------------------------------------------------------------------*
000400101005     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600040121     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991105     FTIS760D   CF   E             WORKSTN INFDS(DEVDS1)
001000991105     F                                     SFILE(s760D05:SFLULT)
001100991105     F                                     SFILE(s760DMS:MSGULT)
001200970526     F*------------
001300120112     FtiVLRSSl  IF   E           K DISK
001400120112     F                                     RENAME(tivlr000:tivlr0SS)
001500120112     F                                     PREFIX(VLRSS : 3)
001600140606     Ftivlr01l  IF   E           K DISK    usropn
001700140606     F                                     RENAME(tivlr000:tivlr01)
001800140606     Ftivlr03l  IF   E           K DISK
001900991117     Ftivlr04l  IF   E           K DISK
002000991117     F                                     RENAME(tivlr000:tivlrxxx)
002100991108     Ftivir02l  IF   E           K DISK
002200991108     Ftivtf01l  IF   E           K DISK
002300991110     Ftntbe02l  IF   E           K DISK
002400991110     Fazorg01l  IF   E           K DISK
002500000728     Ftianv01l  IF   E           K DISK
002600010314     Ftianv03l  IF   E           K DISK
002700010314     F                                     RENAME(tianv000:tianv003)
002800090519     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002900090519     F                                     PREFIX(f_) USROPN
003000970526     D*------------------------------------------------------------------------*
003100970526     D* SCHIERE
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* DECODIFICHE
003500970526     D*------------
003600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* MESSAGGI
003900970526     D*------------
004000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
004100970528     D*------------
004200970528     D* COMANDI
004300970528     D*------------
004400970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
004500970526     D*------------
004600970526     D* DESCRIZIONI OPZIONI
004700970526     D*------------
004800000707     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004900970526     D*------------
005000970526     D* DESCRIZIONI TASTI FUNZIONALI
005100970526     D*------------
005200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
005300970526     D*------------
005400970526     D* OPZIONI DA CARICARE A VIDEO
005500970526     D*------------
005600970526     D OPV             S             19    DIM(24)
005700970526     D*------------
005800970526     D* FUNZIONI DA CARICARE A VIDEO
005900970526     D*------------
006000970527     D FUV             S             19    DIM(24)
006100991108     D*------------
006200991108     D* CODICI DI STATO DEI RECORD DI LOG
006300991108     D*------------
006400991108     D FLG             S              1    DIM(10) CTDATA PERRCD(1)
006500991108     D*------------
006600991108     D* DESCRIZIONI STATO DEI RECORD DI LOG
006700991108     D*------------
006800991112     D DES             S              5    DIM(10) CTDATA PERRCD(1)
006900970526     D*------------------------------------------------------------------------*
007000970526     D* INPUT
007100970526     D*------------------------------------------------------------------------*
007200970526     D*------------
007300970526     D* RIDEFINIZIONE OPZIONI VIDEO
007400970526     D*------------
007500970526     D OPDDS           DS
007600970526     D  OPDOPZ                 1      2
007700970526     D  OPDDE1                 3     19
007800970526     D  OPDOP1                21     22
007900970526     D  OPDFI0                23     34
008000970526     D  OPDATT                35     35
008100970526     D  OPDFI1                36     80
008200970526     D  OPDDES                 1     19
008300970526     D  OPDRIG                 1     80
008400970526     D*------------
008500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
008600970526     D*------------
008700970526     D FUDDS           DS
008800970526     D  FUDDE1                 1      1
008900970526     D  FUDFUN                 2      3
009000970526     D  FUDDE2                 4     19
009100970526     D  FUDFI0                20     34
009200970526     D  FUDATT                35     35
009300970526     D  FUDFI1                36     80
009400970526     D  FUDDES                 1     19
009500970526     D  FUDRIG                 1     80
009600970526     D*------------
009700970526     D* DS GESTIONE TASTI FUNZIONALI
009800970526     D*------------
009900970526     D DSKEY         E DS
010000970526     D DEVDS1          DS
010100970526     D  KEY                  369    369
010200970526     D*------------
010300970526     D* DS "XMSG" - GESTIONE MESSAGGI
010400970526     D*------------
010500970526     D DSMSG         E DS
010600970526     D*------------
010700020916     D* DS REPERIMENTO DATI UTENTE
010800970526     D*------------
010900020916     D TIBS34DS      E DS                                                       *Profili utente
011000020916     D DDATIUTE      E DS                                                       *Dati utente
011100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
011200020218     D*------------
011300020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
011400020218     D*------------
011500020218     D DITF          E DS
011600970527     D*------------------
011700991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
011800970527     D*------------------
011900970527     D WLBDA8          DS                  INZ
012000970527     D  G08DAT                 1      8  0
012100970527     D  G08INV                 9     16  0
012200970527     D  G08ERR                17     17
012300970527     D  G08TGI                18     22  0
012400970526     D*------------
012500970526     D* ARCHITETTURA
012600970526     D*------------
012700970526     D KPJBA         E DS
012800021119     D*------------
012900021119     D* DS RIDEFINIZIONE CAMPO FLAG OPERATIVI (VLRFL3) DI TIVLR00F
013000021119     D*------------
013100021119     D DSVLR3        E DS
013200991108     D*------------------
013300991108     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
013400991108     D*------------------
013500991108     D                 DS                  INZ
013600991108     D  GG                     1      2  0
013700991108     D  MM                     3      4  0
013800991108     D  AAAA                   5      8  0
013900991108     D  CHKDATA                1      8  0
014000991108     D*------------------
014100991108     D                 DS                  INZ
014200991108     D  AAINV                  1      4  0
014300991108     D  MMINV                  5      6  0
014400991108     D  GGINV                  7      8  0
014500991108     D  INVDATA                1      8  0
014600991110     D*------------------
014700991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
014800991110     D*------------------
014900991110     D                 DS                  INZ
015000991110     D  GGA                    1      2  0
015100991110     D  SEP1                   3      3
015200991110     D  MMA                    4      5  0
015300991110     D  SEP2                   6      6
015400991110     D  AAAAA                  7     10  0
015500991110     D  DATAA                  1     10
015600991110     D*------------------
015700991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
015800991110     D*------------------
015900991110     D                 DS                  INZ
016000991110     D  HH                     1      2  0
016100991110     D  PP                     3      4  0
016200991110     D  SS                     5      6  0
016300991110     D  ORAN                   1      6  0
016400991110     D*------------------
016500991110     D                 DS                  INZ
016600991110     D  HHA                    1      2  0
016700991110     D  SEP3                   3      3
016800991110     D  PPA                    4      5  0
016900991110     D  SEP4                   6      6
017000991110     D  SSA                    7      8  0
017100991110     D  ORAA                   1      8
017200991110     D*------------------
017300991110     D* DS - COMPOSIZIONE NOME MEMBRO
017400991110     D*------------------
017500991110     D                 DS                  INZ
017600991110     D  M                      1      1
017700991110     D  HDL                    1     10
017800991110     D*------------------
017900991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
018000991110     D*------------------
018100991110     D                 DS                  INZ
018200001215     D  MS1                    1     42
018300001215     D  MS2                   43     80
018400991110     D  MESS                   1     80
018500991108     D*------------------
018600991108     D* DS REPERIMENTO DATI UTENTE
018700991108     D*-------------------
018800991108     DBS69DS         E DS                  extname(TIBS69DS)
018900991108     DACODS          E DS                  extname(CNACO00F)
019000991108     DINDDS          E DS                  extname(CNIND00F)
019100991108     DCLPDS          E DS                  extname(CNCLP00F)
019200991108     DCLSDS          E DS                  extname(FNCLS00F)
019300090519     D*------------
019400090519     D* VARIABILI D WRK
019500090519     D*------------
019600090519     D wrkprg          s              8  0 inz(*zeros)
019700090519     D uplprg          s             10    inz(*all'0')
019800991110     D*------------
019900991110     D* PASSAGGIO PARAMETRI X CLP
020000991110     D*------------
020100991110     D PARAM           DS                  INZ
020200991112     D  PLIB                   1     10
020300991112     D  POBJ                  11     20
020400991112     D  PMBR                  21     30
020500991112     D  PDAT                  31     38  0
020600991112     D  PORA                  39     44  0
020700991112     D  PKSC                  45     52
020800991112     D  PFLG                  53     53
020900991112     D  PTIP                  54     55
021000991112     D  PPOI                  56     58  0
021100991112     D  PSQZ                  59     61  0
021200991112     D  POPZ                  62     63
021300021119     D  PFL3                  64     73
021400030616     D  PMSG                  74    103
021500090519     D  PMBRNEW              104    113
021600051013     D*------------
021700051013     D* PASSAGGIO PARAMETRI IN INGRESSO
021800051013     D*------------
021900051013     D PAR_INP         DS
022000051013     D  DEPOP0                        3
022100051013     D  DEPKSC                        8
022200051013     D  DEPTIP                        2
022300080320     D*---
022400080320     D* Costanti
022500080320     D*---
022600080320     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
022700080320     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
022800101005     D*-------------------
022900101005     D* DEFINIZIONI PROCEDURE ESTERNE
023000101005     D*-------------------
023100101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
023200101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
023300970526     C*------------------------------------------------------------------------*
023400970526     C* MAIN LINES
023500970526     C*------------------------------------------------------------------------*
023600970526     C*
023700970526     C* SE NON CI SONO ERRORI, PROSEGUE
023800970526IF  1C     ERRL00        IFNE      '1'
023900970526     C*
024000970526     C* CARICA IL SFILE
024100970526     C                   EXSR      POSSFL
024200970526     C*
024300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
024400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
024500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
024600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
024700970526     C*
024800970526     C* REIMPOSTA ERRORE DI PROCERURA
024900970526     C                   MOVEL     '0'           ERRL00
025000970526     C*
025100970526     C* EMETTE IL SFILE
025200970526     C                   EXSR      OUTSFL
025300970526     C*
025400970526     C* GESTIONE TASTI FUNZIONALI
025500970526     C                   EXSR      FUNCON
025600970526     C*
025700970526E   2C                   ENDDO
025800970527     C*
025900970526E   1C                   ENDIF
026000991108     C*
026100991108     C                   EXSR      FINSR
026200970526     C* FINE PGM
026300970526     C                   SETON                                        LR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
026600970526     C*------------------------------------------------------------------------*
026700970526     C     F03GES        BEGSR
026800970526     C*
026900970526     C* IMPOSTA "PREMUTO F03"
027000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
027100970526     C*
027200970526     C                   ENDSR
027300970526     C*------------------------------------------------------------------------*
027400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
027500970526     C*------------------------------------------------------------------------*
027600970526     C     F05GES        BEGSR
027700030617     C*
027800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     F06GES        BEGSR
028500970526     C*
028600991108     C* NON GESTITO
028700970526     C*
028800970526     C                   ENDSR
028900970526     C*------------------------------------------------------------------------*
029000991108     C* F12GES - GESTIONE F12 -> RITORNO
029100970526     C*------------------------------------------------------------------------*
029200970526     C     F12GES        BEGSR
029300970526     C*
029400970526     C* IMPOSTA "PREMUTO F12"
029500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
029600970526     C*
029700970526     C                   ENDSR
029800101005     C*------------------------------------------------------------------------*
029900101005     C* F18GES - GESTIONE F18 -> FORZATIURA ELABORAZIONE BARTVAS
030000101005     C*------------------------------------------------------------------------*
030100101005     C     F18GES        BEGSR
030200101005     C*
030300101005     C* Effettuo verifiche x correttezza parametri selezione
030400101005     C                   IF        VIDTI1 = *blanks OR
030500101005     C                             VIDKS1 = *blanks
030600101005     C                   SETON                                        8384
030700101005     C                   MOVEL     MSG(16)       DSMSMS
030800101005     C                   ELSE
030900101005     C*
031000101005     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
031100101005     C                                           VIDKS1
031200101005     C                                          :VIDTI1)
031300101005     C*
031400101005     C* Ricarico il sub-file
031500101005     C                   CLEAR                   S760D06
031600101005     C                   EXSR      F05GES
031700101005     C*
031800101005     C                   ENDIF
031900101005     C*
032000101005     C                   ENDSR
032100970528     C*------------------------------------------------------------------------*
032200970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
032300970528     C*------------------------------------------------------------------------*
032400970528     C     F21GES        BEGSR
032500970612     C*
032600991117     C* NON GESTITA
032700970528     C*
032800970528     C                   ENDSR
032900970528     C*------------------------------------------------------------------------*
033000970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
033100970528     C*------------------------------------------------------------------------*
033200970528     C     F22GES        BEGSR
033300970528     C*
033400970528     C                   Z-ADD     80            LENGH            15 5
033500970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
033600970528     C                   PARM                    CMD(1)
033700970528     C                   PARM                    LENGH
033800970528     C*
033900970528     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
034200970526     C*------------------------------------------------------------------------*
034300970526     C     F23GES        BEGSR
034400970526     C*
034500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
034600970526     C*
034700970526     C                   ENDSR
034800970526     C*------------------------------------------------------------------------*
034900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
035000970526     C*------------------------------------------------------------------------*
035100970526     C     F24GES        BEGSR
035200970526     C*
035300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
035400970526     C*
035500970526     C                   ENDSR
035600970526     C*------------------------------------------------------------------------*
035700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
035800970526     C*------------------------------------------------------------------------*
035900970526     C     RUPGES        BEGSR
036000970526     C*
036100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
036200970526     C*
036300970526     C                   ENDSR
036400970526     C*------------------------------------------------------------------------*
036500991108     C* ENTGES - GESTIONE ENTER -> INVIO
036600970526     C*------------------------------------------------------------------------*
036700970526     C     ENTGES        BEGSR
036800991108     C*
036900991108     C* CONTROLLO RAPIDO DATA (FORMATO GGMMAAAA) SE NO => ERRORE
037000991108     C*
037100991108     C                   Z-ADD     0             DATAERR           1 0
037200991108     C                   MOVEL     VIDdt2        CHKDATA
037300991108     C     CHKDATA       IFGT      0
037400991108     C     GG            IFLT      1
037500991108     C     GG            ORGT      31
037600991108     C                   Z-ADD     1             DATAERR
037700991108     C                   SETON                                        80
037800991108     C                   MOVEL     MSG(12)       DSMSMS
037900991108     C                   ENDIF
038000991108     C     MM            IFLT      1
038100991108     C     MM            ORGT      12
038200991108     C                   Z-ADD     1             DATAERR
038300991108     C                   SETON                                        80
038400991108     C                   MOVEL     MSG(12)       DSMSMS
038500991108     C                   ENDIF
038600991108     C                   ENDIF
038700991108     C*
038800991108     C                   MOVEL     VIDdt1        CHKDATA
038900991108     C     CHKDATA       IFGT      0
039000991108     C     GG            IFLT      1
039100991108     C     GG            ORGT      31
039200991108     C                   Z-ADD     1             DATAERR
039300991108     C                   SETON                                        82
039400991108     C                   MOVEL     MSG(12)       DSMSMS
039500991108     C                   ENDIF
039600991108     C     MM            IFLT      1
039700991108     C     MM            ORGT      12
039800991108     C                   Z-ADD     1             DATAERR
039900991108     C                   SETON                                        82
040000991108     C                   MOVEL     MSG(12)       DSMSMS
040100991108     C                   ENDIF
040200991108     C                   ENDIF
040300991108     C*
040400991108     C     DATAERR       IFEQ      0
040500991108     C*
040600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
040700970526     C                   EXSR      NEWPOS
040800970526     C*
040900991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
041000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041100970526IF  2C     SWPOSI        IFNE      0
041200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
041300970526X   2C                   ELSE
041400970526IF  3C     SFLULT        IFNE      0
041500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
041600970526     C*
041700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
041800970526IF  4C     SWMODI        IFNE      0
041900970526     C                   EXSR      POSSFL
042000970526E   4C                   ENDIF
042100970526E   3C                   ENDIF
042200970526E   2C                   ENDIF
042300970526E   1C                   ENDIF
042400970526     C*
042500991108     C                   ENDIF
042600991108     C*
042700970526     C                   ENDSR
042800970526     C*------------------------------------------------------------------------*
042900991112     C* O01GES - GESTIONE OPZIONE 1=ESECUZIONE PARZIALE
043000970526     C*------------------------------------------------------------------------*
043100970526     C     O01GES        BEGSR
043200970526     C*
043300000707     C     VIDflg        IFEQ      '0'
043400000707     C                   MOVEL     MSG(13)       DSMSMS
043500000707     C                   ELSE
043600090519     C                   exsr      calprog
043700021119     C                   MOVEL(P)  VIHfl3        DSVLR3
043800090526     C***                IF        §VLR33 = 'S'
043900090526     C***                MOVEL     MSG(3)        DSMSMS
044000090526     C***                ELSE
044100991112     C                   CLEAR                   PARAM
044200991112     C                   EXSR      PASPAR
044300991112     C                   CALL      'TIS761C'                                    *LANCIA PGM
044400991112     C                   PARM                    KPJBA
044500090526     C***                ENDIF
044600021119     C                   ENDIF
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000991112     C* O11GES - GESTIONE OPZIONE 11=ESECUZIONE COMPLETA
045100970526     C*------------------------------------------------------------------------*
045200991112     C     O11GES        BEGSR
045300970526     C*
045400000707     C     VIDflg        IFEQ      '0'
045500000707     C                   MOVEL     MSG(13)       DSMSMS
045600000707     C                   ELSE
045700991112     C                   CLEAR                   PARAM
045800991112     C                   EXSR      PASPAR
045900991112     C                   CALL      'TIS761C'                                    *LANCIA PGM
046000991112     C                   PARM                    KPJBA
046100000707     C                   ENDIF
046200970526     C*
046300970526     C                   ENDSR
046400991112     C*------------------------------------------------------------------------*
046500991112     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD LOG
046600991112     C*------------------------------------------------------------------------*
046700991112     C     O04GES        BEGSR
046800991112     C*
046900991112     C                   CLEAR                   PARAM
047000991112     C                   EXSR      PASPAR
047100991112     C                   CALL      'TIS761C'                                    *LANCIA PGM
047200991112     C                   PARM                    KPJBA
047300991112     C*
047400991112     C                   ENDSR
047500991014     C*------------------------------------------------------------------------*
047600021119     C* O14GES - GESTIONE OPZIONE 14=ANNULLAMENTO MEMBRO INPUT
047700991014     C*------------------------------------------------------------------------*
047800991112     C     O14GES        BEGSR
047900991014     C*
048000000707     C     VIDflg        IFEQ      '0'
048100000707     C                   MOVEL     MSG(13)       DSMSMS
048200000707     C                   ELSE
048300021119     C                   MOVEL(P)  VIHfl3        DSVLR3
048400021119     C                   IF        §VLR33 = 'S'
048500021119     C                   MOVEL     MSG(3)        DSMSMS
048600021119     C                   ELSE
048700991112     C                   CLEAR                   PARAM
048800991112     C                   EXSR      PASPAR
048900991112     C                   CALL      'TIS761C'                                    *LANCIA PGM
049000991112     C                   PARM                    KPJBA
049100000707     C                   ENDIF
049200021119     C                   ENDIF
049300991108     C*
049400991014     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600991110     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD DI LOG
049700970526     C*------------------------------------------------------------------------*
049800970526     C     O05GES        BEGSR
049900991110     C*
050000991110     C                   EXFMT     S760DWW
050100970526     C*
050200970526     C                   ENDSR
050300991110     C*------------------------------------------------------------------------*
050400991110     C* O15GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE MEBRO FILE STORICO
050500991110     C*------------------------------------------------------------------------*
050600991110     C     O15GES        BEGSR
050700991110     C*
050800080118     C                   MOVEL(P)  VIHfl3        DSVLR3
050900080118     C                   IF        §VLR33 <> 'S'
051000080118     C                   MOVEL     MSG(4)        DSMSMS
051100080118     C                   ELSE
051200991110     C                   CLEAR                   PARAM
051300991112     C                   EXSR      PASPAR
051400991112     C                   CALL      'TIS761C'                                    *LANCIA PGM
051500991110     C                   PARM                    KPJBA
051600080118     C                   ENDIF
051700991110     C*
051800991110     C                   ENDSR
051900000707     C*------------------------------------------------------------------------*
052000000707     C* O25GES - GESTIONE OPZIONE 25=VISUALIZZAZIONE MEBRO FILE DI INPUT
052100000707     C*------------------------------------------------------------------------*
052200000707     C     O25GES        BEGSR
052300000707     C*
052400021119     C                   MOVEL(P)  VIHfl3        DSVLR3
052500021119     C                   IF        §VLR33 = 'S'
052600021119     C                   MOVEL     MSG(3)        DSMSMS
052700021119     C                   ELSE
052800000707     C                   CLEAR                   PARAM
052900000707     C                   EXSR      PASPAR
053000000707     C                   CALL      'TIS761C'                                    *LANCIA PGM
053100000707     C                   PARM                    KPJBA
053200021119     C                   ENDIF
053300000707     C*
053400000707     C                   ENDSR
053500010313     C*------------------------------------------------------------------------*
053600010313     C* O20GES - GESTIONE OPZIONE 26=APERTURA MEMBRO CON DFU
053700010313     C*------------------------------------------------------------------------*
053800010313     C     O20GES        BEGSR
053900010313     C*
054000010313     C                   CLEAR                   PARAM
054100010313     C                   EXSR      PASPAR
054200010313     C                   CALL      'TIS761C'                                    *LANCIA PGM
054300010313     C                   PARM                    KPJBA
054400010313     C*
054500010313     C                   ENDSR
054600970526     C*------------------------------------------------------------------------*
054700970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
054800970526     C*------------------------------------------------------------------------*
054900970526     C     O07GES        BEGSR
055000970526     C*
055100991108     C* NON GESTITA
055200970526     C*
055300970526     C                   ENDSR
055400000707     C*
055500991028     C*------------------------------------------------------------------------*
055600991108     C* O09GES - GESTIONE OPZIONE 9=???
055700991028     C*------------------------------------------------------------------------*
055800991028     C     O09GES        BEGSR
055900991028     C*
056000991108     C* NON GESTITA
056100991108     C*
056200991028     C                   ENDSR
056300991112     C*------------------------------------------------------------------------*
056400991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
056500991112     C*------------------------------------------------------------------------*
056600991112     C     PASPAR        BEGSR
056700991112     C*
056800000711     C                   MOVEL     VIHLIS        PLIB
056900000711     C*                  MOVEL     VIHFIS        POBJ
057000000711     C                   MOVEL     'TIVIN00R'    POBJ
057100991112     C                   MOVEL     VIHHDL        PMBR
057200991112     C                   Z-ADD     VIHDATI       PDAT
057300991112     C                   Z-ADD     VIHORA        PORA
057400090519     C                   MOVEL     VIDKSC        PKSC
057500991112     C                   MOVEL     VIDFLG        PFLG
057600991112     C                   MOVEL     VIDTIP        PTIP
057700991112     C                   Z-ADD     VIDPOI        PPOI
057800991112     C                   Z-ADD     VIHSQZ        PSQZ
057900991112     C                   MOVEL     OPZKEY        POPZ
058000021120     C                   MOVEL(P)  VIHFL3        DSVLR3
058100021120     C                   MOVEL     §VLR33        PFL3
058200030613     C                   MOVE      VIHMSG        PMSG
058300090519     C                   MOVEL(P)  uplprg        PMBRNEW
058400090526     C                   MOVEL     'M'           PMBRNEW
058500991112     C                   MOVEL     PARAM         KPJBU
058600991112     C*
058700991112     C                   ENDSR
058800010313     C*
058900970526     C*------------------------------------------------------------------------*
059000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
059100970526     C*------------------------------------------------------------------------*
059200970526     C     NEWPOS        BEGSR
059300970526     C*
059400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
059500970526     C*         1= RICHIESTO POSIZIONAMENTO
059600970526     C                   Z-ADD     0             SWPOSI            7 0
059700970526     C*
059800991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
059900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
060000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
060100970526     C                   Z-ADD     0             SWMODI            1 0
060200970530     C*
060300970530     C* REIMPOSTA INDICATORI DI ERRORE
060400991108     C                   SETOFF                                       808182
060500991108     C                   SETOFF                                       83
060600970526     C*
060700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
060800010423     C     VIDti1        IFNE      depti1
060900010423     C     VIDst1        orne      depst1
061000010423     C     VIDks1        orne      depks1
061100010423     C     VIDdt1        orne      depdt1
061200010423     C     VIDsv1        orne      depsv1
061300040121     C     VIDpo1        orne      deppo1
061400010423     C     VIDks2        orne      depks2
061500010423     C     VIDdt2        orne      depdt2
061600010423     C     VIDsrs        orne      depsrs
061700070719     C     VIDspr        orne      depspr
061800040618     C     VIDpgt        orne      deppgt
061900080319     C     VIDnfil       orne      depnfil
062000070918     C     VIDproc       orne      depproc
062100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
062200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
062300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
062400991108     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
062500991108     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
062600991108     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
062700991108     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
062800001215     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
062900040121     C                   MOVEL     VIDpo1        DEPpo1                         *DEPOSITI
063000991108     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
063100991108     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
063200010423     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
063300070719     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
063400040618     C                   MOVEL     VIDpgt        DEPpgt                         *DEPOSITI
063500080319     C                   MOVEL     VIDnfil       DEPnfil                        *DEPOSITI
063600070918     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
063700970526E   2C                   ENDIF
063800970526E   1C                   ENDIF
063900970526     C*
064000970526     C                   ENDSR
064100970526     C*------------------------------------------------------------------------*
064200970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
064300970526     C*------------------------------------------------------------------------*
064400970528     C     SETFIL        BEGSR
064500970526     C*
064600970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
064700991108     C     VIDdt2        IFEQ      *ZEROS
064800991108     C                   MOVEL     *hival        KCEdat
064900991108     C                   ELSE
065000991108     C                   Z-ADD     VIDdt2        CHKDATA
065100991108     C                   Z-ADD     AAAA          AAINV
065200991108     C                   Z-ADD     MM            MMINV
065300991108     C                   Z-ADD     GG            GGINV
065400991108     C                   MOVEL     INVDATA       KCEdat
065500991108     C                   ENDIF
065600991108     C                   MOVEL     *hival        KCEora
065700991108     C     VIDks2        IFEQ      *ZEROS
065800991108     C     VIDks2        OREQ      *BLANKS
065900991108     C                   MOVEL     *hival        KCEksc
066000991108     C                   ELSE
066100991108     C                   MOVEL     VIDks2        KCEksc
066200991108     C                   ENDIF
066300991108     C                   MOVEL     *hival        KCEflg
066400991108     C                   MOVEL     *hival        KCEtip
066500991108     C                   MOVEL     *hival        KCEpoi
066600991112     C                   MOVEL     *hival        KCEsqz
066700991117IF  1C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
066800991117     C     KEYvlr3       SETLL     tivlr03l                           99
066900970528E   1C                   ENDIF
067000991117IF  1C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
067100991117     C     KEYvlr4       SETLL     tivlr04l                           99
067200991117E   1C                   ENDIF
067300140606     C* se K_T è il tipo caricamento, devo usare solo questi 2 dati
067400140606IF  1C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
067500140606     C                   MOVEL     VIDks1        KCEksc
067600140606     C                   MOVEL     VIDTi1        KCEtip
067700140606     C     KEYvlr1       SETLL     tivlr01l                           99
067800140606E   1C                   ENDIF
067900980107     C*
068000970526     C* LETTURA RECORD
068100970526IF  1C     *IN99         IFEQ      *OFF
068200970526     C                   EXSR      LETFIL
068300970526E   1C                   ENDIF
068400970526     C*
068500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
068600970526IF  1C     *IN99         IFEQ      *ON
068700970526     C                   Z-ADD     1             SFLEOF
068800140606     C* mi salvo il posizionamento effettuato
068900140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
069000140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
069100140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
069200140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
069300140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
069400140606     C                   MOVEL     VIDpo1        DEPpo1                         *DEPOSITI
069500140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
069600140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
069700140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
069800140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
069900140606     C                   MOVEL     VIDpgt        DEPpgt                         *DEPOSITI
070000140606     C                   MOVEL     VIDnfil       DEPnfil                        *DEPOSITI
070100140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
070200970526E   1C                   ENDIF
070300970526     C*
070400970526     C                   ENDSR
070500970526     C*------------------------------------------------------------------------*
070600970526     C* LETFIL - LETTURA RECORD
070700970526     C*------------------------------------------------------------------------*
070800970526     C     LETFIL        BEGSR
070900970526     C*
071000970526     C* LEGGE FINO A:
071100970526     C*     - FINE FILE
071200970526     C*     - TROVATO RECORD VALIDO
071300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
071400001215     C     *IN99         DOWEQ     *OFF
071500970526     C     WRECOK        ANDEQ     'N'
071600970526     C*
071700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
071800001215     C     WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
071900991108     C                   READ      tivlr03l                               99
072000001215     C                   ENDIF
072100001215     C     WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
072200991117     C                   READ      tivlr04l                               99
072300001215     C                   ENDIF
072400140606     C     WTIPRI        IFEQ      'K_T'                                        KSC/TIP
072500140606     C* la chiave è solo questa per questo tipo di caricamento
072600140606     C     KEYVlr1       READE     tivlr01l                               99
072700140606     C                   ENDIF
072800970528     C*
072900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
073000001215     C     *IN99         IFEQ      *ON                                          *FINE FILE
073100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
073200001215     C                   ELSE
073300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
073400001215     C                   ENDIF
073500001215     C                   ENDDO
073600970526     C*
073700970526     C                   ENDSR
073800970526     C*------------------------------------------------------------------------*
073900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
074000970526     C*------------------------------------------------------------------------*
074100970526     C     CHKREC        BEGSR
074200970526     C*
074300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
074400001215     C*
074500991026     C*filtra se impostato tipo file
074600010427     C                   if        WRECOK = 'S'
074700040121     C     vidti1        ifeq      *blank
074800001215     C                   else
074900001215     C                   if        %subst(vidti1:1:1) = '*'
075000001215     C                   if        %subst(vidti1:2:1) <>
075100001215     C                             %subst(vlrtip:2:1)
075200001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
075300001215     C                   endif
075400001215     C                   else
075500001215     C                   if        %subst(vidti1:2:1) = '*'
075600001215     C                   if        %subst(vidti1:1:1) <>
075700001215     C                             %subst(vlrtip:1:1)
075800001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
075900001215     C                   endif
076000001215     C                   else
076100001215     C                   if        vidti1 <> vlrtip
076200001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
076300001215     C                   endif
076400001215     C                   endif
076500001215     C                   endif
076600001215     C                   endif
076700010427     C                   endif
076800001215     C*
076900001215     C*filtra se impostato stato del log
077000010427     C                   if        WRECOK = 'S'
077100040121     C     vidst1        ifeq      *blank
077200001215     C                   ELSE
077300040121     C     vidst1        ifne      vlrflg
077400991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
077500001215     C                   ENDIF
077600001215     C                   ENDIF
077700010427     C                   ENDIF
077800970526     C*
077900001215     C*filtra se impostato codice cliente
078000010427     C                   if        WRECOK = 'S'
078100040121     C     vidks1        ifeq      *zeros
078200040121     C     vidks1        oreq      *blank
078300001215     C                   ELSE
078400040121     C     vidks1        ifne      vlrksc
078500991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
078600001215     C                   ENDIF
078700001215     C                   ENDIF
078800010427     C                   ENDIF
078900991108     C*
079000001215     C*filtra se impostata data del log
079100010427     C                   if        WRECOK = 'S'
079200001215     C     viddt1        ifeq      *zeros                                       *ALLA DATA
079300001215     C                   ELSE
079400991108     C                   Z-ADD     viddt1        CHKDATA
079500991108     C                   Z-ADD     AAAA          AAINV
079600991108     C                   Z-ADD     MM            MMINV
079700991108     C                   Z-ADD     GG            GGINV
079800991108     C     invdata       ifne      vlrdat                                       *ALLA DATA
079900991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080000001215     C                   ENDIF
080100001215     C                   ENDIF
080200010427     C                   ENDIF
080300991108     C*
080400001215     C*filtra se impostato il tipo servizio
080500010427     C                   if        WRECOK = 'S'
080600001215     C     vidsv1        ifeq      *blank
080700001215     C                   else
080800001215     C                   if        %subst(vidsv1:1:1) = '*'
080900001215     C                   if        %subst(vidsv1:2:1) <>
081000001215     C                             %subst(vlrisv:2:1)
081100001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081200001215     C                   endif
081300001215     C                   else
081400001215     C                   if        %subst(vidsv1:2:1) = '*'
081500001215     C                   if        %subst(vidsv1:1:1) <>
081600001215     C                             %subst(vlrisv:1:1)
081700001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081800001215     C                   endif
081900001215     C                   else
082000001215     C                   if        vidsv1 <> vlrisv
082100001215     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
082200001215     C                   endif
082300001215     C                   endif
082400001215     C                   endif
082500001215     C                   endif
082600010427     C                   endif
082700010427     C*
082800010427     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
082900010427     C                   if        WRECOK = 'S'
083000130731     C                   EVAL      PiStr=%trim(vlrksc)
083100130731     C                   EXSR      CHKNUM
083200130731     C                   IF        PiInt=*on
083300130731     C                   Z-ADD     PiVal         wcli              7 0
083400130731     C                   ENDIF
083500120112     C                   exsr      dectib
083600040121     C     VIDsrs        ifeq      *blanks
083700010427     C                   else
083800010709     C                   z-add     *zeros        pos               2 0
083900010709     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
084000010709     C                   if        pos = *zeros
084100070719     C                   z-add     *zeros        pos               2 0
084200070719     C                   eval      pos = %scan(%trim(wddksc):VIDsrs)
084300070719     C                   if        pos = *zeros
084400070719     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
084500070719     C                   endif
084600070719     C                   endif
084700010427     C                   endif
084800010427     C                   endif
084900070719     C*
085000070719     C* PROGRESSIVO TRANSAZIONE DIVERSO DA QUELLA SCELTA
085100070719     C                   if        WRECOK = 'S'
085200070719     C     VIDspr        ifeq      *blanks
085300070719     C                   else
085400070719     C                   z-add     *zeros        pos               2 0
085500140424     C                   eval      pos = %scan(%trim(VIDspr):VLRprg)
085600140424     C***                eval      pos = %scan(%trim(VIDspr):VLRhdl)
085700140424     C***                if        pos = *zeros
085800140424     C***                z-add     *zeros        pos               2 0
085900140424     C***                eval      pos = %scan(%trim(VLRhdl):VIDspr)
086000070719     C                   if        pos = *zeros
086100070719     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
086200070719     C                   endif
086300140424     C***                endif
086400070719     C                   endif
086500070719     C                   endif
086600040121     C*
086700040121     C*filtra se impostato il P.O. a cui spedire
086800040121     C                   if        WRECOK = 'S'
086900040121     C     vidpo1        ifeq      *zeros
087000040121     C                   ELSE
087100040121     C     vidpo1        ifne      vlrpoi                                       *P.O. A CUI SPEDIRE
087200040121     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087300040121     C                   ENDIF
087400040121     C                   ENDIF
087500040121     C                   ENDIF
087600040618     C*
087700040618     C*filtra se impostato il Traduttore
087800040618     C                   if        WRECOK = 'S'
087900040618     C     vidpgt        ifeq      *blanks
088000080319     C                   else
088100080319     C                   if        %scan(%trim(vidPGT):vlrPGT) > *zeros
088200080319     C                   else
088300080319     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
088400080319     C                   endif
088500040618     C                   ENDIF
088600040618     C                   ENDIF
088700080319     C*
088800080319     C*filtra se impostato il Nome File
088900080319     C                   if        WRECOK = 'S'
089000080319     C     vidnfil       ifeq      *blanks
089100080319     C                   else
089200080320     C     *LIKE         define    vlrMSG        wMAIUvlrMSG
089300080320     C     minu:maiu     xlate     vlrMSG        wMAIUvlrMSG
089400080320     C                   if        %scan(%trim(vidNFIL):wMAIUvlrMSG) > *zeros
089500080319     C                   else
089600080319     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
089700080319     C                   endif
089800080319     C                   ENDIF
089900080319     C                   ENDIF
090000070918     C*
090100070918     C*filtra se impostato l'ID processo
090200070918     C                   if        WRECOK = 'S'
090300070918     C     vidproc       ifeq      *blanks
090400070918     C                   ELSE
090500070918     C                   if        vidproc <> %subst(vlrfl3:4:2)                *PGM TRADUTTORE
090600070918     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090700070918     C                   ENDIF
090800070918     C                   ENDIF
090900070918     C                   ENDIF
091000001215     C*
091100970526     C                   ENDSR
091200010423     C*------------------------------------------------------------------------*
091300010423     C* dectib - Controlla esistenza codice cliente
091400010423     C*------------------------------------------------------------------------*
091500010423     C     dectib        begsr
091600010913     C*
091700010913     C     vlrksc        chain     tianv01l
091800010913     C                   if        %found(tianv01l)
091900010913     C                   movel(p)  anvdes        wddksc
092000010913     C                   else
092100010423     C                   CLEAR                   BS69DS
092200010423     C                   CLEAR                   ACODS
092300010423     C                   MOVEL     KNSIF         I69SIF
092400010423     C                   Z-ADD     WCLI          I69KAC
092500010423     C                   CALL      'TIBS69R'
092600010423     C                   PARM                    BS69DS
092700010423     C                   PARM                    ACODS
092800010423     C                   PARM                    INDDS
092900010423     C                   PARM                    CLPDS
093000010423     C                   PARM                    CLSDS
093100010423     C     O69ERR        IFNE      '1'
093200010423     C                   MOVEL     ACORAG        wddksc           40
093300010423     C                   ELSE
093400010423     C                   MOVEL     *ALL'*'       Wddksc
093500010913     C                   MOVE      '1'           errl00
093600010913     C                   ENDIF
093700010423     C                   ENDIF
093800010913     C*
093900010913     C                   endsr
094000970526     C*------------------------------------------------------------------------*
094100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
094200970526     C*------------------------------------------------------------------------*
094300970526     C     IRESFL        BEGSR
094400010329     C*
094500010612     C                   CLEAR                   S760D05
094600010612     C*
094700970526     C                   MOVEL     *BLANKS       OPZKEY
094800991112     C                   Z-ADD     vlrdat        VIHdati
094900991108     C                   Z-ADD     vlrdat        G08INV
095000991108     C                   Z-ADD     *ZEROS        G08DAT
095100991108     C                   MOVEL     '3'           G08ERR
095200991108     C                   CALL      'XSRDA8'
095300991108     C                   PARM                    WLBDA8
095400991110     C                   Z-ADD     G08DAT        VIHdat
095500991110     C                   Z-ADD     VIHdat        CHKDATA
095600991110     C                   Z-ADD     GG            GGA
095700991110     C                   Z-ADD     MM            MMA
095800991110     C                   Z-ADD     AAAA          AAAAA
095900991110     C                   MOVEL     DATAA         VIDdat
096000991110     C                   z-add     vlrora        VIHora
096100991110     C                   z-add     VIHora        ORAN
096200991110     C                   z-add     HH            HHA
096300991110     C                   z-add     PP            PPA
096400991110     C                   z-add     SS            SSA
096500991110     C                   MOVEL     ORAA          VIDora
096600991108     C                   MOVEL     vlrksc        VIDksc
096700991115     C                   MOVE      vlrksc        VIPksc
096800010423     C                   MOVEL(p)  wddksc        VIDRAG
096900010423     C                   MOVEL(p)  wddksc        VIHRAG
097000991108     C                   MOVEL     vlrflg        VIDflg
097100001215     C                   MOVEL     vlrisv        VIDisv
097200001215     C                   MOVEL     vlrtia        VIHtia
097300001215     C                   MOVEL     vlrfl2        VIHfl2
097400001215     C                   MOVEL     vlrprg        VIHprg
097500021119     C                   MOVEL     vlrfl3        VIHfl3
097600070918     C                   EVAL      VIHproc = %subst(vlrfl3:4:2)
097700030613     C                   MOVEL     vlrmsg        VIHmsg
097800991112     C     vlrsqz        IFGT      *ZEROS
097900991112     C     vlrlis        ANDEQ     *BLANKS
098000991112     C     vlrfis        ANDEQ     *BLANKS
098100000211     C                   MOVEL     'Canc '       DESFLG
098200991112     C                   ENDIF
098300991108     C                   MOVEL     vlrtip        VIDtip
098400010314     C* reperimento descrizione tipi file
098500991110     C                   MOVEL     'ITF'         KBEcod                         *tabella
098600991110     C                   MOVEL     *BLANKS       KBElin                         *tabella
098700991110     C                   MOVEL     *BLANKS       KBEsif                         *tabella
098800991110     C                   MOVEL     vlrtip        KBEke1                         *tabella
098900991110     C                   MOVEL     *BLANKS       KBEke2                         *tabella
099000991110     C     KE2tbe        CHAIN     tntbe02l
099100020218     C                   MOVEL     tbeuni        DITF
099200020218     C                   MOVEL     §ITFDESC      DEHtip                         *tabella
099300020218     C                   MOVEL     §ITFDESC      DEDtip                         *tabella
099400010612     C*
099500991108     C                   MOVEL     vlrksc        KIRksc
099600991108     C                   MOVEL     vlrtip        KIRtip
099700991108     C     KEYvir        CHAIN     tivir02l
099800991108     C     virfit        IFNE      *BLANKS
099900991108     C                   MOVEL     virfit        DEStip
100000991110     C                   MOVEL     virfid        DEHti1
100100991110     C                   MOVEL     virfis        DEHti2
100200001215     C                   MOVEL     virpgi        VIHpgi
100300991108     C                   ELSE
100400991108     C     vlrtip        CHAIN     tivtf01l
100500991108     C                   MOVEL     vtffit        DEStip
100600991110     C                   MOVEL     vtffid        DEHti1
100700991110     C                   MOVEL     vtffis        DEHti2
100800991108     C                   ENDIF
100900010314     C                   MOVEL     vlrfou        DEStip
101000001215     C                   MOVEL     vlrfis        DEHti1
101100001215     C                   Z-ADD     vlrpoi        VIDpoi
101200991110     C                   MOVEL     vlrhdl        HDL
101300000711     C                   MOVEL     'M'           M
101400991110     C                   MOVEL     HDL           VIHhdl
101500001215     C                   MOVEL     VIHhdl        VIHmbi
101600001215     C                   MOVEL     VIHhdl        VIHmbd
101700991110     C                   MOVEL     vlrlis        VIHlis
101800001215     C                   MOVEL(p)  '*LIBL'       VIHlib
101900991110     C                   MOVEL     vlrfis        VIHfis
102000991110     C                   MOVEL     vlrmsg        MESS
102100991110     C                   MOVEL     MS1           DEHMS1
102200010329     C*
102300010329     C* Effettuo test PALETTO x consentire una rapida identificazione degli errori...
102400010329     C* ...dovuti a mancanza di autorizzazioni
102500010612     C* ...e controllo in schiera codici e descrizioni stati di log
102600010612     C                   SETOFF                                       28
102700010612     C                   Z-ADD     1             J                 2 0
102800010612     C     vlrflg        LOOKUP    FLG(J)                                 28
102900010612     C     *IN28         IFEQ      *ON
103000010612     C                   MOVEL     DES(J)        DESFLG
103100010612     C                   IF        DES(J) = 'Err  ' and
103200010615     C                             %subst(DEHMS1:1:5) = 'Error'
103300010612     C                   SETON                                        50
103400010612     C                   ELSE
103500010612     C                   SETOFF                                       50
103600010612     C                   ENDIF
103700010612     C                   ENDIF
103800010612     C                   IF        %subst(DEHMS1:1:7) = 'Errore,'
103900010329     C                   SETON                                        51
104000010329     C                   ELSE
104100010329     C                   SETOFF                                       51
104200010329     C*
104300010329     C                   ENDIF
104400991110     C                   MOVEL     MS2           DEHMS2
104500991110     C     vlrpoi        CHAIN     azorg01l
104600991110     C                   MOVEL     orgdes        DEHpoi
104700991110     C                   MOVEL     vlrpgt        VIHpgt
104800991112     C                   Z-ADD     vlrsqz        VIHsqz
104900991108     C*
105000970526     C                   ENDSR
105100970526     C*------------------------------------------------------------------------*
105200991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
105300970526     C*------------------------------------------------------------------------*
105400970526     C     CHKPAR        BEGSR
105500970528     C*
105600991108     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *03L
105700991117     C                   MOVEL     'DAT'         WTIPRI            3
105800991108IF  1C     VIDdt2        IFEQ      *zeros
105900991025     C     VIDks2        ANDEQ     *ZEROS
106000991108IF  1C     VIDdt2        orEQ      *zeros
106100991025     C     VIDks2        ANDEQ     *blanks
106200991117     C                   MOVEL     'DAT'         WTIPRI
106300991025E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
106400970528     C*
106500970528     C* CONTROLLA IL TIPO RICERCA
106600991012IF  1C     ERRL00        IFEQ      '0'
106700991108IF  2C     VIDdt2        IFNE      *zeros
106800991117     C                   MOVEL     'DAT'         WTIPRI
106900991117X   2C                   ELSE
107000991117IF  3C     VIDks2        IFNE      *zeros
107100991117IF  3C     VIDks2        andne     *blanks
107200991117     C                   MOVEL     'KSC'         WTIPRI
107300991117E   3C                   ENDIF
107400140606     C* se i parametri di ingresso sono valorizzati
107500140610     C* e i rispettivi parametri video hanno gli stessi valori
107600140610     C* e nessun altro parametro video è valorizzato
107700140606     C* allora siamo nel caso di chiamata
107800140606     C* da pgm che passa i parametri in input e quindi uso la VL 01L
107900140606IF  3C     DEPksc        IFNE      *zeros
108000140606IF  3C     DEPksc        andne     *blanks
108100140606IF  3C     DEPTip        andne     *blanks
108200140610IF  3C     DEPksc        andeq     VIDKs1
108300140610IF  3C     DEPTip        andeq     VIDTi1
108400140606IF  3C     VIDst1        andeq     *blanks
108500140606IF  3C     VIDdt1        andeq     0
108600140606IF  3C     VIDsv1        andeq     *blanks
108700140606IF  3C     VIDpo1        andeq     0
108800140606IF  3C     VIDks2        andeq     *blanks
108900140606IF  3C     VIDdt2        andeq     0
109000140606IF  3C     VIDsrs        andeq     *blanks
109100140606IF  3C     VIDspr        andeq     *blanks
109200140606IF  3C     VIDpgt        andeq     *blanks
109300140606IF  3C     VIDnfil       andeq     *blanks
109400140606IF  3C     VIDproc       andeq     *blanks
109500140606     C                   MOVEL     'K_T'         WTIPRI
109600140606     C* visto che so che la userò, apro la VL 01L
109700140606     C                   IF        not %open(TIVLR01L)
109800140606     C                   OPEN      TIVLR01L
109900140606     C                   ENDIF
110000140606E   3C                   ENDIF
110100991012E   2C                   ENDIF
110200991012E   1C                   ENDIF
110300991012      *
110400970526     C                   ENDSR
110500970526     C*------------------------------------------------------------------------*
110600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
110700970526     C*------------------------------------------------------------------------*
110800970526     C     CHKFOP        BEGSR
110900991112     C*
111000991112     C* OPZIONI
111100970626     C*
111200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
111300991112IF  1C     DEPOP0        IFEQ      'P05'
111400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
111500970626     C                   MOVEL     'S'           OPDATT
111600970626     C                   MOVEL     OPDRIG        OPD(5)
111700080118     C                   MOVEL     OPD(15)       OPDRIG                         *VISUAL. STORICO
111800080118     C                   MOVEL     'S'           OPDATT
111900080118     C                   MOVEL     OPDRIG        OPD(15)
112000970626E   1C                   ENDIF
112100970626     C*
112200970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
112300970626IF  1C     DEPOP0        IFEQ      'P02'
112400991110     C                   MOVEL     OPD(1)        OPDRIG                         *MODIFICA
112500970626     C                   MOVEL     'S'           OPDATT
112600991110     C                   MOVEL     OPDRIG        OPD(1)
112700991110     C                   MOVEL     OPD(4)        OPDRIG                         *MODIFICA
112800991014     C                   MOVEL     'S'           OPDATT
112900991110     C                   MOVEL     OPDRIG        OPD(4)
113000991112     C                   MOVEL     OPD(11)       OPDRIG                         *ANNULLAMENTO
113100991112     C                   MOVEL     'S'           OPDATT
113200991112     C                   MOVEL     OPDRIG        OPD(11)
113300991110     C                   MOVEL     OPD(14)       OPDRIG                         *RIPRISTINO
113400970626     C                   MOVEL     'S'           OPDATT
113500991110     C                   MOVEL     OPDRIG        OPD(14)
113600991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
113700991112     C                   MOVEL     'S'           OPDATT
113800991112     C                   MOVEL     OPDRIG        OPD(5)
113900991112     C                   MOVEL     OPD(15)       OPDRIG                         *INTERROGAZIONE
114000991112     C                   MOVEL     'S'           OPDATT
114100991112     C                   MOVEL     OPDRIG        OPD(15)
114200010313     C                   MOVEL     OPD(20)       OPDRIG                         *INTERROGAZIONE
114300010313     C                   MOVEL     'S'           OPDATT
114400010313     C                   MOVEL     OPDRIG        OPD(20)
114500000707     C                   MOVEL     OPD(24)       OPDRIG                         *INTERROGAZIONE
114600000707     C                   MOVEL     'S'           OPDATT
114700000707     C                   MOVEL     OPDRIG        OPD(24)
114800101005     C***
114900101005     C                   MOVEL     FUD(18)       FUDRIG                         *FORZA ELAB. BARTVAS
115000101005     C                   MOVEL     'S'           FUDATT
115100101005     C                   MOVEL     FUDRIG        FUD(18)
115200101005     C***
115300970626E   1C                   ENDIF
115400970626     C*
115500970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
115600020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
115700991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
115800991112     C                   MOVEL     'N'           OPDATT
115900991112     C                   MOVEL     OPDRIG        OPD(4)
116000991110     C                   MOVEL     OPD(14)       OPDRIG                         *NO RIPRISTINO
116100970626     C                   MOVEL     'N'           OPDATT
116200991110     C                   MOVEL     OPDRIG        OPD(14)
116300991110     C                   MOVEL     FUD(1)        FUDRIG                         *NO INSERIMENTO
116400970626     C                   MOVEL     'N'           FUDATT
116500991110     C                   MOVEL     FUDRIG        FUD(1)
116600991110     C                   MOVEL     FUD(11)       FUDRIG                         *NO INSERIMENTO
116700991110     C                   MOVEL     'N'           FUDATT
116800991110     C                   MOVEL     FUDRIG        FUD(11)
116900970626E   1C                   ENDIF
117000970526     C*
117100970526     C* GESTIONE OPZIONI A VIDEO
117200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
117300970526     C                   EXSR      OPZGES
117400970526     C*
117500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
117600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
117700970526     C                   EXSR      FUNGES
117800970526     C*
117900970526     C                   ENDSR
118000030617     C*------------------------------------------------------------------------*
118100120112     C*CONTA0B - CONTA I RECORD DA ELABORARE (STATO = 0 / " ")
118200030617     C*------------------------------------------------------------------------*
118300101018     C     CONTA0B       BEGSR
118400030617     C*
118500060105     C                   Z-ADD     *zeros        VIDST0
118600101018     C                   Z-ADD     *zeros        VIDSTB
118700060105     C*
118800060105     C* CONTEGGIO RECORD A STATO '0'
118900120112     C     '0'           SETLL     tiVLRSSl
119000120112     C     '0'           READE     tiVLRSSl
119100120112     C                   DOW       not %eof(tiVLRSSl)
119200030617     C                   ADD       1             VIDST0
119300120112     C     '0'           READE     tiVLRSSl
119400030617     C                   ENDDO
119500060105     C*
119600060105     C* CONTEGGIO RECORD A STATO 'M'
119700120112     C     'M'           SETLL     tiVLRSSl
119800120112     C     'M'           READE     tiVLRSSl
119900120112     C                   DOW       not %eof(tiVLRSSl)
120000120112     C                   ADD       1             VIDST0
120100120112     C     'M'           READE     tiVLRSSl
120200060105     C                   ENDDO
120300101018     C*
120400101018     C* CONTEGGIO RECORD A STATO ' '
120500120112     C     *blank        SETLL     tiVLRSSl
120600120112     C     *blank        READE     tiVLRSSl
120700120112     C                   DOW       not %eof(tiVLRSSl)
120800101018     C                   ADD       1             VIDSTB
120900120112     C     *blank        READE     tiVLRSSl
121000101018     C                   ENDDO
121100030617     C*
121200030617     C                   ENDSR
121300991108     C*------------------------------------------------------------------------*
121400991108     C* FINSR - OPERAZIONI FINALI
121500991108     C*------------------------------------------------------------------------*
121600991108     C     FINSR         BEGSR
121700991108     C*
121800991108     C* CHIUDE I FILE DEI PGM CHIAMATI
121900991108     C                   CLEAR                   BS69DS
122000991108     C                   CLEAR                   ACODS
122100991108     C                   CLEAR                   INDDS
122200991108     C                   CLEAR                   CLPDS
122300991108     C                   CLEAR                   CLSDS
122400991108     C                   MOVEL     'C'           I69TLA
122500991108     C                   MOVEL     KNSIF         I69SIF
122600991108     C                   CALL      'TIBS69R'
122700991108     C                   PARM                    BS69DS
122800991108     C                   PARM                    ACODS
122900991108     C                   PARM                    INDDS
123000991108     C                   PARM                    CLPDS
123100991108     C                   PARM                    CLSDS
123200991108     C*
123300991108     C                   ENDSR
123400020916     C*--------------------------------------------------------------------------------------------*
123500020916     C* REPERISCE I DATI UTENTE
123600020916     C*--------------------------------------------------------------------------------------------*
123700020916     C     REPDATIUTE    BEGSR
123800020916     C*
123900020916     C* INIZIALIZZA VARIABILI DI WRK
124000020916     C                   CLEAR                   TIBS34DS
124100020916     C                   CLEAR                   AZUTEDS
124200020916     C                   CLEAR                   DDATIUTE
124300020916     C*
124400020916     C     *DTAARA       DEFINE    §azute        azuteds
124500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
124600020916     C                   IN(E)     *DTAARA
124700020916if  1C                   IF        %Error
124800020916     c                   EVAL      I34Tla = 'L'
124900020916     C                   CALL      'TIBS34R'
125000020916     C                   PARM                    Tibs34Ds
125100020916     C                   IN        *DTAARA
125200020916e   1C                   ENDIF
125300020916     C*
125400020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
125500020916     C                   MOVEL(P)  RSUT          DSFIRS           20
125600020916     C*
125700020916     C                   ENDSR
125800090519
125900090519
126000090519
126100090519      /TITLE Valorizzazione Progressivo Applicazione
126200090519     C     calprog       begsr
126300090519     C*
126400090519     C                   open      tis7prgf
126500090519     C                   read      tis7prgf
126600090519     C*
126700090519     C                   eval      uplprg = f_tis7prgf
126800090519     C                   move(p)   uplprg        wrkprg
126900090519     C                   add       1             wrkprg
127000090519     C                   move(p)   wrkprg        uplprg
127100090519     C                   movel     vidISV        uplprg
127200090519     C*
127300090519     C                   eval      f_tis7prgf = uplprg
127400090519     C                   update    tis7prg0
127500090519     C                   close     tis7prgf
127600090519     C*
127700090519     C                   endsr
127800130731
127900130731
128000130731
128100130731     C*----------------------------------------------------*
128200130731     C*  CONTROLLO NUMERICITA' CAMPI
128300130731     C*----------------------------------------------------*
128400130731     C     CHKNUM        BEGSR
128500130731     C*
128600130731     C                   IF        PiDecChr = *blanks
128700130731     C                   EVAL      PiDecChr = '.'
128800130731     C                   ENDIF
128900130731     C*
129000130731     C                   CALL(e)   'ISNUMERIC'
129100130731     C                   PARM                    PiStr            30
129200130731     C                   PARM                    PiDecChr          1
129300130731     C                   PARM      *ZEROS        PiVal            30 9
129400130731     C                   PARM      '0'           PiInt             1
129500130731     C                   PARM      '0'           PiNum             1
129600130731     C                   IF        %error
129700130731     C                   EVAL      PiNum=*off
129800130731     C                   ENDIF
129900130731     C*
130000130731     C                   ENDSR
130100130731     C***
130200130731
130300090519
130400090519
130500970526     C*------------------------------------------------------------------------*
130600970526     C* *INZSR - OPERAZIONI INIZIALI
130700970526     C*------------------------------------------------------------------------*
130800970526     C     *INZSR        BEGSR
130900970526     C*---
131000970526     C* RICEVIMENTO PARAMETRI
131100970526     C*---
131200970526     C     *ENTRY        PLIST
131300970612     C                   PARM                    KPJBA
131400051013     C                   MOVEL     KPJBU         PAR_INP
131500080319     C*
131600080319     C* SE UTENTE NO EDP* => ACCENDO *IN55
131700080319     C                   SETOFF                                       55
131800101014     C***                IF        %subst(KNMUS:1:3) <> 'EDP'
131900101014     C***                SETON                                        55
132000101014     C***                ENDIF
132100051013     C*
132200051013     C* TESTO SUBITO ALCCUNI PARAMETRI D INGRESSO
132300051013     C                   IF        DEPKSC <> *blanks
132400051013     C                   EVAL      VIDks1 = DEPKSC
132500051013     C                   ENDIF
132600051013     C                   IF        DEPTIP <> *blanks
132700051013     C                   EVAL      VIDti1 = DEPTIP
132800051013     C                   ENDIF
132900970526     C*
133000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
133100970526     C     XMSGPA        PLIST
133200970526     C                   PARM                    DSMSMS
133300970526     C                   PARM                    DSMSPG
133400970526     C                   PARM                    DSMSKE
133500970526     C                   PARM                    DSMSER
133600970526     C*
133700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
133800970526     C                   CALL      'XKEY'
133900970526     C                   PARM                    DSKEY
134000970526     C*---
134100970526     C* VARIABILI NON RIFERITE AL DATA BASE
134200970526     C*---
134300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
134400090921     C***  *LIKE         DEFINE    SFLPAG        WFLPAG
134500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
134600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
134700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
134800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
134900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
135000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
135100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
135200970526     C*---
135300991108     C     *LIKE         DEFINE    viddt2        Depdt2
135400991108     C     *LIKE         DEFINE    viddt1        Depdt1
135500991108     C     *LIKE         DEFINE    vidks2        Depks2
135600991108     C     *LIKE         DEFINE    vidks1        Depks1
135700991108     C     *LIKE         DEFINE    vidti1        Depti1
135800991108     C     *LIKE         DEFINE    vidti1        Deptip
135900991108     C     *LIKE         DEFINE    vidst1        Depst1
136000001215     C     *LIKE         DEFINE    vidsv1        Depsv1
136100040121     C     *LIKE         DEFINE    vidpo1        Deppo1
136200010423     C     *LIKE         DEFINE    vidsrs        Depsrs
136300070719     C     *LIKE         DEFINE    vidspr        Depspr
136400040618     C     *LIKE         DEFINE    vidpgt        Deppgt
136500080319     C     *LIKE         DEFINE    vidnfil       Depnfil
136600070918     C     *LIKE         DEFINE    vidproc       Depproc
136700991108     C     *LIKE         DEFINE    vlrdat        kcedat
136800991108     C     *LIKE         DEFINE    vlrora        kceora
136900991108     C     *LIKE         DEFINE    vlrksc        kceksc
137000991108     C     *LIKE         DEFINE    vlrflg        kceflg
137100991108     C     *LIKE         DEFINE    vlrtip        kcetip
137200991108     C     *LIKE         DEFINE    vlrpoi        kcepoi
137300991112     C     *LIKE         DEFINE    vlrsqz        kcesqz
137400991108     C*---
137500991108     C     *LIKE         DEFINE    tbecod        kbecod
137600991108     C     *LIKE         DEFINE    tbelin        kbelin
137700991108     C     *LIKE         DEFINE    tbesif        kbesif
137800991108     C     *LIKE         DEFINE    tbeke1        kbeke1
137900991108     C     *LIKE         DEFINE    tbeke2        kbeke2
138000991108     C     *LIKE         DEFINE    virksc        kirksc
138100991108     C     *LIKE         DEFINE    virtip        kirtip
138200991108     C     *LIKE         DEFINE    virdti        kirdti
138300010314     C*---
138400970526     C* VARIABILI DI CONTROLLO
138500970526     C*---
138600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
138700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
138800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
138900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
139000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
139100010314     C                   MOVEL     *blanks       chkCliAnv         8            *CODCLI VARIO X INET
139200010314     C                   MOVEL     'INET'        chkCodAnv        15            *CODTIP VARIO X INET
139300080319     C                   Z-ADD     09            SFLPAG                         *N°RIGHE X PAG
139400991110     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
139500991110     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
139600991110     C                   MOVEL     ':'           SEP3                           *VALIDITA' RECORD
139700991110     C                   MOVEL     ':'           SEP4                           *VALIDITA' RECORD
139800970526     C*
139900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
140000991105     C                   MOVEL     'TIS760R'     DSMSPG
140100991105     C                   MOVEL     'TIS760R'     MSGQUE
140200970526     C                   MOVEL     *BLANKS       MSGKEY
140300970526     C                   MOVEL     *BLANKS       DSMSMS
140400970526     C                   MOVEL     *BLANKS       DSMSKE
140500970526     C                   Z-ADD     *ZEROS        DSMSER
140600970526     C*---
140700970526     C* CHIAVI DI LETTURA
140800970526     C*---
140900140606     C* LETTURA parziale tivlr01L -
141000140606     C     KEYvlr1       KLIST
141100140606     C                   KFLD                    KCEksc
141200140606     C                   KFLD                    KCEtip
141300991108     C* LETTURA tivlr03L -
141400991117     C     KEYvlr3       KLIST
141500010524     C                   KFLD                    KCEdat
141600010524     C                   KFLD                    KCEora
141700010524     C                   KFLD                    KCEksc
141800010524     C                   KFLD                    KCEtip
141900010524     C                   KFLD                    KCEpoi
142000010524     C                   KFLD                    KCEsqz
142100010524     C                   KFLD                    KCEflg
142200991117     C* LETTURA tivlr04L -
142300991117     C     KEYvlr4       KLIST
142400010524     C                   KFLD                    KCEksc
142500010524     C                   KFLD                    KCEdat
142600010524     C                   KFLD                    KCEora
142700010524     C                   KFLD                    KCEtip
142800010524     C                   KFLD                    KCEpoi
142900010524     C                   KFLD                    KCEsqz
143000010524     C                   KFLD                    KCEflg
143100991108     C* LETTURA "NUOVE" TABELLE
143200010524     C     KE2tbe        KLIST                                                  *TNTBE02L
143300991108     C                   KFLD                    KBEcod                          -tabella
143400991108     C                   KFLD                    KBElin                          -lingua
143500991108     C                   KFLD                    KBEsif                          -s.informativo
143600991108     C                   KFLD                    KBEke1                          -lingua
143700991108     C                   KFLD                    KBEke2                          -s.informativo
143800010524     C* LETTURA tivir02L -  (PARZIALE)
143900010524     C     KEYvir        KLIST                                                  *Regole x cliente
144000010524     C                   KFLD                    KIRksc                          -Cliente
144100010524     C                   KFLD                    KIRtip                          -Tipo File
144200991108     C*
144300970527     C* REPERIMENTO DATI UTENTE
144400020916     C                   EXSR      REPDATIUTE
144500010314     C*
144600010314     C* REPERISCO IL CODICE ABBINATO AL "CONCENTRATORE" X SCANBI DATI VIA INTESA (INET)
144700010314     C     chkCodAnv     chain     tianv03l
144800010314     C                   if        %found(tianv03l)
144900010314     C                   movel(p)  anvcli        chkCliAnv         8
145000010314     C                   endif
145100970526     C*
145200970526     C* CALCOLA LA DATA CORRENTE
145300120112     C                   Z-ADD     0             DATCOR            8 0          *DATA CORRENTE AA/M/
145400120112     C                   EVAL      DatCor = %dec(%date : *ISO)
145500010314     C*
145600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
145700970526     C                   EXSR      CHKFOP
145800051013     C*
145900051013     C* EFFETTUA PRIMA VERIFICA DEI PARAMETRI RICEVUTI
146000051013     C                   EXSR      CHKPAR
146100970526     C*
146200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
146300010314     C     DEPOP0        IFEQ      'P01'
146400970530     C                   MOVEL     DEC(3)        VIDING
146500010314     C                   ENDIF
146600010314     C     DEPOP0        IFEQ      'P02'
146700970526     C                   MOVEL     DEC(1)        VIDING
146800010314     C                   ENDIF
146900010314     C     DEPOP0        IFEQ      'P05'
147000970526     C                   MOVEL     DEC(2)        VIDING
147100010314     C                   ENDIF
147200970526     C*
147300970526     C                   ENDSR
147400970526     C*------------------------------------------------------------------------*
147500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
147800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
147900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
148000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
148100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
148200970526     C*------------------------------------------------------------------------*
148300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
148400970526     C*------------------------------------------------------------------------*
148500970526     C     POSSFL        BEGSR
148600970526     C*
148700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
148800970526     C                   EXSR      CLRSFL
148900970526     C*
149000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
149100970526     C                   EXSR      SETFIL
149200970526     C*
149300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
149400970526     C     SFLEOF        IFEQ      0
149500970526     C                   EXSR      CARSFL
149600970526     C                   END
149700970526     C*
149800970526     C                   ENDSR
149900970526     C*------------------------------------------------------------------------*
150000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
150100970526     C*------------------------------------------------------------------------*
150200970526     C     ENTATT        BEGSR
150300970526     C*
150400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
150500970526     C                   Z-ADD     0             OPZOK             1 0
150600970526     C*
150700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
150800970526     C                   Z-ADD     0             SFLVAR            5 0
150900970526     C*
151000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
151100970526     C                   Z-ADD     0             SFLERR
151200970526     C*
151300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
151400970526     C                   Z-ADD     SFLULT        WFLULT
151500970526     C*
151600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
151700991105     C                   READC     s760D05                                98
151800970526     C     *IN98         DOWEQ     '0'
151900970526     C*
152000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
152100970526     C                   ADD       1             SFLVAR
152200970526     C*
152300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
152400970526     C     OPZOK         IFEQ      0
152500970526     C*
152600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
152700970526     C     OPZKEY        IFNE      *BLANKS
152800970526     C                   EXSR      OPZCON
152900970526     C                   END
153000970526     C*
153100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
153200970526     C*            2. PULIZIA SCELTA IN SFL
153300970526     C     OPZOK         IFEQ      0
153400970526     C                   MOVEL     '0'           *IN41
153500970526     C                   MOVEL     *BLANKS       OPZKEY
153600970526     C                   ELSE
153700970526     C*
153800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
153900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
154000970526     C                   MOVEL     '1'           *IN41
154100970526     C                   Z-ADD     SFLULT        SFLERR
154200970526     C                   END
154300970526     C                   ELSE
154400970526     C*
154500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
154600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
154700970526     C                   MOVEL     '1'           *IN41
154800970526     C                   END                                                    OPZOK=0
154900970526     C*
155000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
155100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
155200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
155300970526     C     OPZOK         IFNE      0
155400970526     C                   Z-ADD     SFLERR        SFLNBR
155500970526     C                   ELSE
155600970526     C                   Z-ADD     SFLULT        SFLNBR
155700970526     C                   END
155800970526     C*
155900970526     C* AGGIORNO RECORD SFL
156000020212     C                   EVAL      *IN50 = HIN50
156100020212     C                   EVAL      *IN51 = HIN51
156200991105     C                   UPDATE    s760D05
156300970526     C*
156400991105     C                   READC     s760D05                                98
156500970526     C                   END                                                    DO WHILE
156600970526     C*
156700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
156800970526     C                   Z-ADD     WFLULT        SFLULT
156900970526     C*
157000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
157100970526     C     SFLVAR        IFEQ      0
157200970526     C*---                 MOVEL'1'       FINL00
157300970526     C                   END
157400970526     C*
157500991115     C     OPZKEY        IFNE      ' 5'
157600991115     C     OPZKEY        ANDNE     '15'
157700000331     C     OPZKEY        ANDNE     '  '
157800991112     C                   EXSR      F05GES
157900991115     C                   ENDIF
158000991112     C*
158100970526     C                   ENDSR
158200970526     C*------------------------------------------------------------------------*
158300970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
158400970526     C*------------------------------------------------------------------------*
158500970526     C     CLRSFL        BEGSR
158600970526     C*
158700970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
158800970526     C* (0=NON FINE FILE  1=FINE FILE)
158900970526     C                   Z-ADD     0             SFLEOF
159000970526     C*
159100970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
159200970526     C                   Z-ADD     0             SFLULT
159300970526     C*
159400970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
159500970526     C                   MOVEL     '1'           *IN40
159600991105     C                   WRITE     s760D06
159700970526     C                   MOVEL     '0'           *IN40
159800970526     C*
159900970526     C                   ENDSR
160000970526     C*------------------------------------------------------------------------*
160100970526     C* CARSFL - CARICAMENTO SFL
160200970526     C*------------------------------------------------------------------------*
160300970526     C     CARSFL        BEGSR
160400970526     C*
160500970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
160600970526     C     SFLULT        IFEQ      0
160700970526     C                   EXSR      SETFIL
160800970526     C                   END
160900970526     C*
161000970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
161100970526     C                   Z-ADD     SFLULT        WFLULT
161200970526     C*
161300970526     C* CICLO:
161400970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
161500970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
161600970526     C                   Z-ADD     1             SFLRIG
161700970526     C     SFLRIG        DOWLE     SFLPAG
161800970526     C     SFLEOF        ANDEQ     0
161900970526     C*
162000970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
162100970526     C                   MOVEL     *BLANKS       OPZKEY
162200970526     C*
162300970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
162400970526     C                   EXSR      IRESFL
162500970526     C*
162600970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
162700970526     C                   ADD       1             SFLULT
162800970526     C*
162900970526     C* SCRITTURA RECORD SFL
163000020212     C                   EVAL      HIN50 = *IN50
163100020212     C                   EVAL      HIN51 = *IN51
163200991105     C                   WRITE     s760D05
163300970526     C                   ADD       1             SFLRIG
163400970526     C*
163500970526     C* LETTURA FILE DA CARICARE
163600970526     C                   EXSR      LETFIL
163700970526     C                   END                                                    DO WHILE
163800970526     C*
163900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
164000970526     C     WFLULT        IFNE      SFLULT
164100970526     C                   Z-ADD     WFLULT        SFLNBR
164200970526     C                   ADD       1             SFLNBR
164300140606     C* mi salvo il posizionamento effettuato
164400140606     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
164500140606     C                   MOVEL     VIDst1        DEPst1                         *DEPOSITI
164600140606     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
164700140606     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
164800140606     C                   MOVEL     VIDsv1        DEPsv1                         *DEPOSITI
164900140606     C                   MOVEL     VIDpo1        DEPpo1                         *DEPOSITI
165000140606     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
165100140606     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
165200140606     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
165300140606     C                   MOVEL     VIDspr        DEPspr                         *DEPOSITI
165400140606     C                   MOVEL     VIDpgt        DEPpgt                         *DEPOSITI
165500140606     C                   MOVEL     VIDnfil       DEPnfil                        *DEPOSITI
165600140606     C                   MOVEL     VIDproc       DEPproc                        *DEPOSITI
165700970526     C                   END
165800970526     C*
165900970526     C                   ENDSR
166000970526     C*------------------------------------------------------------------------*
166100970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
166200970526     C*------------------------------------------------------------------------*
166300970526     C     OUTSFL        BEGSR
166400970526     C*
166500970526     C* WRITE RECORD TESTATA VIDEO
166600991105     C                   WRITE     s760D01
166700970526     C*
166800970526     C* VISUALIZZAZIONE MESSAGGIO
166900970526     C                   EXSR      SNDMSG
167000970526     C*
167100970526     C* WRITE RECORD OPZIONI VIDEO
167200991105     C                   WRITE     s760D02
167300030617     C*
167400030617     C* CONTO I RECORD DA ELABORARE
167500120112     C                   EXSR      CONTA0B
167600970526     C*
167700970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
167800991105     C                   WRITE     s760D03
167900970526     C*
168000970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
168100991105     C                   WRITE     s760D04
168200970526     C*
168300970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
168400970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
168500970526     C     SFLEOF        IFEQ      0
168600970526     C                   MOVEL     '0'           *IN42
168700970526     C                   ELSE
168800970526     C*
168900970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
169000970526     C                   MOVEL     '1'           *IN42
169100970526     C                   END
169200970526     C*
169300970526     C* WRITE RECORD TASTI FUNZIONALI
169400991105     C                   WRITE     s760D09
169500970526     C*
169600970526     C* CASO DI SFL VUOTO
169700970526     C     SFLULT        IFLE      0
169800970526     C*
169900970526     C*    - WRITE RECORD VUOTO
170000991105     C                   WRITE     s760D0A
170100991105     C                   READ      s760D0A                                99
170200970526     C*
170300970526     C                   ELSE
170400970526     C*
170500970526     C* CASO DI SFL CON RECORDS
170600970526     C*
170700970526     C*    - WRITE RECORD CONTROLLO SFL
170800991105     C                   WRITE     s760D06
170900991105     C                   READ      s760D06                                99
171000970526     C                   END
171100970526     C*
171200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
171300991105     C                   READ      s760D03                                99
171400970526     C*
171500970526     C                   ENDSR
171600970526     C*------------------------------------------------------------------------*
171700970526     C* SNDMSG - MANDA MESSAGGIO
171800970526     C*------------------------------------------------------------------------*
171900970526     C     SNDMSG        BEGSR
172000970526     C*
172100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
172200970526     C                   MOVEL     '1'           *IN40
172300991105     C                   WRITE     s760DMC
172400970526     C                   MOVEL     '0'           *IN40
172500970526     C*
172600970526     C* VISUALIZZAZIONE MESSAGGIO
172700970526     C                   Z-ADD     0             MSGULT
172800970526     C                   CALL      'XMSG'        XMSGPA
172900970526     C     DSMSER        IFEQ      0
173000970526     C                   MOVEL     DSMSKE        MSGKEY
173100970526     C                   END
173200970526     C                   Z-ADD     1             MSGULT
173300991105     C                   WRITE     s760DMS                                      SFL MESS
173400991105     C                   WRITE     s760DMC                                      CONTR MESS
173500970526     C*
173600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
173700970526     C                   MOVEL     *BLANKS       DSMSMS
173800970526     C*
173900970526     C                   ENDSR
174000970526     C*------------------------------------------------------------------------*
174100970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
174200970526     C*------------------------------------------------------------------------*
174300970526     C     FUNCON        BEGSR
174400970526     C*
174500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
174600970526     C                   EXSR      FUNVAR
174700970526     C*
174800970526     C* PULISCO CAMPI DI LAVORO
174900970526     C                   Z-ADD     0             I                 5 0
175000970526     C                   Z-ADD     0             W                 5 0
175100970526     C*
175200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
175300970526     C     FUNKEY        IFNE      *BLANKS
175400970526     C     1             DO        24            I
175500970526     C                   MOVEL     FUD(I)        FUDRIG
175600970526     C*
175700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
175800970526     C     FUNKEY        IFEQ      FUDFUN
175900970526     C     FUDATT        ANDEQ     'S'
176000970526     C                   Z-ADD     I             W
176100970526     C                   END
176200970526     C                   END                                                    DO WHILE
176300970526     C*
176400970526     C* FUNZIONE NON ATTIVA (ERRORE)
176500970526     C     W             IFEQ      0
176600970526     C*
176700970526     C* EMISSIONE MESSAGGIO DI ERRORE
176800970526     C                   MOVEL     MSG(2)        DSMSMS
176900970526     C                   END                                                    W=0
177000970526     C                   END                                                    FUNKEY NE *BLANKS
177100970526     C*
177200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
177300970526     C     W             IFNE      0
177400970526     C     FUNKEY        OREQ      *BLANKS
177500970526     C*
177600970526     C     FUNKEY        IFEQ      *BLANKS
177700970526     C                   EXSR      ENTGES
177800970526     C                   END
177900970526     C     FUNKEY        IFEQ      '03'
178000970526     C                   EXSR      F03GES
178100970526     C                   END
178200970526     C     FUNKEY        IFEQ      '05'
178300970526     C                   EXSR      F05GES
178400970526     C                   END
178500970526     C     FUNKEY        IFEQ      '06'
178600970526     C                   EXSR      F06GES
178700970526     C                   END
178800970526     C     FUNKEY        IFEQ      '12'
178900970526     C                   EXSR      F12GES
179000970526     C                   END
179100101005     C     FUNKEY        IFEQ      '18'
179200101005     C                   EXSR      F18GES
179300101005     C                   END
179400970528     C     FUNKEY        IFEQ      '21'
179500970528     C                   EXSR      F21GES
179600970528     C                   END
179700970528     C     FUNKEY        IFEQ      '22'
179800970528     C                   EXSR      F22GES
179900970528     C                   END
180000970526     C     FUNKEY        IFEQ      '23'
180100970526     C                   EXSR      F23GES
180200970526     C                   END
180300970526     C     FUNKEY        IFEQ      '24'
180400970526     C                   EXSR      F24GES
180500970526     C                   END
180600970526     C     FUNKEY        IFEQ      'RU'
180700970526     C                   EXSR      RUPGES
180800970526     C                   END
180900970526     C*
181000970526     C                   END
181100970526     C*
181200970526     C                   ENDSR
181300970526     C*------------------------------------------------------------------------*
181400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
181500970526     C*------------------------------------------------------------------------*
181600970526     C     FUNVAR        BEGSR
181700970526     C*
181800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
181900970526     C     KEY           IFEQ      ENTER
182000970526     C                   MOVEL     *BLANKS       FUNKEY
182100970526     C                   END
182200970526     C     KEY           IFEQ      F1
182300970526     C                   MOVEL     '01'          FUNKEY
182400970526     C                   END
182500970526     C     KEY           IFEQ      F2
182600970526     C                   MOVEL     '02'          FUNKEY
182700970526     C                   END
182800970526     C     KEY           IFEQ      F3
182900970526     C                   MOVEL     '03'          FUNKEY
183000970526     C                   END
183100970526     C     KEY           IFEQ      F4
183200970526     C                   MOVEL     '04'          FUNKEY
183300970526     C                   END
183400970526     C     KEY           IFEQ      F5
183500970526     C                   MOVEL     '05'          FUNKEY
183600970526     C                   END
183700970526     C     KEY           IFEQ      F6
183800970526     C                   MOVEL     '06'          FUNKEY
183900970526     C                   END
184000970526     C     KEY           IFEQ      F7
184100970526     C                   MOVEL     '07'          FUNKEY
184200970526     C                   END
184300970526     C     KEY           IFEQ      F8
184400970526     C                   MOVEL     '08'          FUNKEY
184500970526     C                   END
184600970526     C     KEY           IFEQ      F9
184700970526     C                   MOVEL     '09'          FUNKEY
184800970526     C                   END
184900970526     C     KEY           IFEQ      F10
185000970526     C                   MOVEL     '10'          FUNKEY
185100970526     C                   END
185200970526     C     KEY           IFEQ      F11
185300970526     C                   MOVEL     '11'          FUNKEY
185400970526     C                   END
185500970526     C     KEY           IFEQ      F12
185600970526     C                   MOVEL     '12'          FUNKEY
185700970526     C                   END
185800970526     C     KEY           IFEQ      F13
185900970526     C                   MOVEL     '13'          FUNKEY
186000970526     C                   END
186100970526     C     KEY           IFEQ      F14
186200970526     C                   MOVEL     '14'          FUNKEY
186300970526     C                   END
186400970526     C     KEY           IFEQ      F15
186500970526     C                   MOVEL     '15'          FUNKEY
186600970526     C                   END
186700970526     C     KEY           IFEQ      F16
186800970526     C                   MOVEL     '16'          FUNKEY
186900970526     C                   END
187000970526     C     KEY           IFEQ      F17
187100970526     C                   MOVEL     '17'          FUNKEY
187200970526     C                   END
187300970526     C     KEY           IFEQ      F18
187400970526     C                   MOVEL     '18'          FUNKEY
187500970526     C                   END
187600970526     C     KEY           IFEQ      F19
187700970526     C                   MOVEL     '19'          FUNKEY
187800970526     C                   END
187900970526     C     KEY           IFEQ      F20
188000970526     C                   MOVEL     '20'          FUNKEY
188100970526     C                   END
188200970526     C     KEY           IFEQ      F21
188300970526     C                   MOVEL     '21'          FUNKEY
188400970526     C                   END
188500970526     C     KEY           IFEQ      F22
188600970526     C                   MOVEL     '22'          FUNKEY
188700970526     C                   END
188800970526     C     KEY           IFEQ      F23
188900970526     C                   MOVEL     '23'          FUNKEY
189000970526     C                   END
189100970526     C     KEY           IFEQ      F24
189200970526     C                   MOVEL     '24'          FUNKEY
189300970526     C                   END
189400970526     C     KEY           IFEQ      ROLLUP
189500970526     C                   MOVEL     'RU'          FUNKEY
189600970526     C                   END
189700970526     C     KEY           IFEQ      HELP
189800970526     C                   MOVEL     'HP'          FUNKEY
189900970526     C                   END
190000970526     C*
190100970526     C                   ENDSR
190200970526     C*------------------------------------------------------------------------*
190300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
190400970526     C*------------------------------------------------------------------------*
190500970526     C     FUNGES        BEGSR
190600970526     C*
190700970526     C* PULISCO CAMPI DI LAVORO
190800970526     C                   Z-ADD     0             W                 5 0
190900970526     C*
191000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
191100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
191200970526     C     1             DO        24            I
191300970526     C                   MOVEL     FUD(I)        FUDRIG
191400970526     C*
191500970526     C* PULIZIA FUNZIONI PRECEDENTI
191600970526     C                   MOVEL     *BLANKS       FUV(I)
191700970526     C*
191800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
191900970526     C* (SALTA F23 E F24)
192000970526     C     FUDATT        IFEQ      'S'
192100970526     C     FUDFUN        ANDNE     '23'
192200970526     C     FUDFUN        ANDNE     '24'
192300970526     C                   ADD       1             W
192400970526     C                   MOVEL     FUDDES        FUV(W)
192500970526     C                   END
192600970526     C                   END                                                    DO WHILE
192700970526     C*
192800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
192900970526     C* ALTRIMENTI DISATTIVO F24
193000970526     C     1             DO        24            I
193100970526     C                   MOVEL     FUD(I)        FUDRIG
193200970526     C*
193300970526     C     FUDFUN        IFEQ      '24'
193400970526     C     W             IFGT      6
193500970526     C                   MOVEL     'S'           FUDATT
193600970526     C                   ELSE
193700970526     C                   MOVEL     *BLANKS       FUDATT
193800970526     C                   END                                                    W GT 8
193900970526     C                   MOVEL     FUDRIG        FUD(I)
194000970526     C                   END
194100970526     C                   END                                                    DO WHILE
194200970526     C*
194300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
194400970526     C     W             DIV       6             I
194500970526     C                   MVR                     Z
194600970526     C*
194700970526     C     Z             IFNE      0
194800970526     C     I             ADD       1             Z
194900970526     C                   ELSE
195000970526     C                   Z-ADD     I             Z
195100970526     C                   END
195200970526     C*
195300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
195400970526     C                   ADD       1             FUNGRU
195500970526     C     FUNGRU        IFGT      Z
195600970526     C     FUNGRU        ORLT      1
195700970526     C                   Z-ADD     1             FUNGRU
195800970526     C                   END
195900970526     C*
196000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
196100970526     C     FUNGRU        IFEQ      1
196200970526     C                   MOVEL     FUV(1)        FUN001
196300970526     C                   MOVEL     FUV(2)        FUN002
196400970526     C                   MOVEL     FUV(3)        FUN003
196500970526     C                   MOVEL     FUV(4)        FUN004
196600970526     C                   MOVEL     FUV(5)        FUN005
196700970526     C                   MOVEL     FUV(6)        FUN006
196800970526     C                   END
196900970526     C*
197000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
197100970526     C     FUNGRU        IFEQ      2
197200970526     C                   MOVEL     FUV(7)        FUN001
197300970526     C                   MOVEL     FUV(8)        FUN002
197400970526     C                   MOVEL     FUV(9)        FUN003
197500970526     C                   MOVEL     FUV(10)       FUN004
197600970526     C                   MOVEL     FUV(11)       FUN005
197700970526     C                   MOVEL     FUV(12)       FUN006
197800970526     C                   END
197900970526     C*
198000970526     C* TERZO GRUPPO DI 6 FUNZIONI
198100970526     C     FUNGRU        IFEQ      3
198200970526     C                   MOVEL     FUV(13)       FUN001
198300970526     C                   MOVEL     FUV(14)       FUN002
198400970526     C                   MOVEL     FUV(15)       FUN003
198500970526     C                   MOVEL     FUV(16)       FUN004
198600970526     C                   MOVEL     FUV(17)       FUN005
198700970526     C                   MOVEL     FUV(18)       FUN006
198800970526     C                   END
198900970526     C*
199000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
199100970526     C     FUNGRU        IFEQ      4
199200970526     C                   MOVEL     FUV(19)       FUN001
199300970526     C                   MOVEL     FUV(20)       FUN002
199400970526     C                   MOVEL     FUV(21)       FUN003
199500970526     C                   MOVEL     FUV(22)       FUN004
199600970526     C                   MOVEL     *BLANKS       FUN005
199700970526     C                   MOVEL     *BLANKS       FUN006
199800970526     C                   END
199900970526     C*
200000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
200100970526     C                   MOVEL     *BLANKS       FUN007
200200970526     C                   MOVEL     *BLANKS       FUN008
200300970526     C*
200400970526     C     1             DO        24            I
200500970526     C                   MOVEL     FUD(I)        FUDRIG
200600970526     C     FUDFUN        IFEQ      '23'
200700970526     C     FUDATT        ANDEQ     'S'
200800970526     C                   MOVEL     FUDDES        FUN007
200900970526     C                   END
201000970526     C     FUDFUN        IFEQ      '24'
201100970526     C     FUDATT        ANDEQ     'S'
201200970526     C                   MOVEL     FUDDES        FUN008
201300970526     C                   END
201400970526     C                   END                                                    DO
201500970526     C*
201600970526     C                   ENDSR
201700970526     C*------------------------------------------------------------------------*
201800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
201900970526     C*------------------------------------------------------------------------*
202000970526     C     OPZGES        BEGSR
202100970526     C*
202200970526     C* PULISCO CAMPI DI LAVORO
202300970526     C                   Z-ADD     0             W                 5 0
202400970526     C*
202500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
202600970526     C* E CARICO TABELLA OPZIONI ATTIVE
202700970526     C     1             DO        24            I
202800970526     C                   MOVEL     OPD(I)        OPDRIG
202900970526     C*
203000970526     C* PULIZIA OPZIONI PRECEDENTI
203100970526     C                   MOVEL     *BLANKS       OPV(I)
203200970526     C*
203300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
203400970526     C     OPDATT        IFEQ      'S'
203500970526     C                   ADD       1             W
203600970526     C                   MOVEL     OPDDES        OPV(W)
203700970526     C                   END
203800970526     C                   END                                                    DO WHILE
203900970526     C*
204000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
204100970526     C* ALTRIMENTI DISATTIVO F23
204200970526     C     1             DO        24            I
204300970526     C                   MOVEL     FUD(I)        FUDRIG
204400970526     C*
204500970526     C     FUDFUN        IFEQ      '23'
204600970526     C     W             IFGT      8
204700970526     C                   MOVEL     'S'           FUDATT
204800970526     C                   ELSE
204900970526     C                   MOVEL     *BLANKS       FUDATT
205000970526     C                   END                                                    W GT 8
205100970526     C                   MOVEL     FUDRIG        FUD(I)
205200970526     C                   END
205300970526     C                   END                                                    DO WHILE
205400970526     C*
205500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
205600970526     C     W             DIV       8             I
205700970526     C                   MVR                     Z                 5 0
205800970526     C*
205900970526     C     Z             IFNE      0
206000970526     C     I             ADD       1             Z
206100970526     C                   ELSE
206200970526     C                   Z-ADD     I             Z
206300970526     C                   END
206400970526     C*
206500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
206600970526     C                   ADD       1             OPZGRU
206700970526     C     OPZGRU        IFGT      Z
206800970526     C     OPZGRU        ORLT      1
206900970526     C                   Z-ADD     1             OPZGRU
207000970526     C                   END
207100970526     C*
207200970526     C* PRIMO GRUPPO DI 8 OPZIONI
207300970526     C     OPZGRU        IFEQ      1
207400970526     C                   MOVEL     OPV(1)        OPZ001
207500970526     C                   MOVEL     OPV(2)        OPZ002
207600970526     C                   MOVEL     OPV(3)        OPZ003
207700970526     C                   MOVEL     OPV(4)        OPZ004
207800970526     C                   MOVEL     OPV(5)        OPZ005
207900970526     C                   MOVEL     OPV(6)        OPZ006
208000970526     C                   MOVEL     OPV(7)        OPZ007
208100970526     C                   MOVEL     OPV(8)        OPZ008
208200970526     C                   END
208300970526     C*
208400970526     C* SECONDO GRUPPO DI 8 OPZIONI
208500970526     C     OPZGRU        IFEQ      2
208600970526     C                   MOVEL     OPV(9)        OPZ001
208700970526     C                   MOVEL     OPV(10)       OPZ002
208800970526     C                   MOVEL     OPV(11)       OPZ003
208900970526     C                   MOVEL     OPV(12)       OPZ004
209000970526     C                   MOVEL     OPV(13)       OPZ005
209100970526     C                   MOVEL     OPV(14)       OPZ006
209200970526     C                   MOVEL     OPV(15)       OPZ007
209300970526     C                   MOVEL     OPV(16)       OPZ008
209400970526     C                   END
209500970526     C*
209600970526     C* TERZO GRUPPO DI 8 OPZIONI
209700970526     C     OPZGRU        IFEQ      3
209800970526     C                   MOVEL     OPV(17)       OPZ001
209900970526     C                   MOVEL     OPV(18)       OPZ002
210000970526     C                   MOVEL     OPV(19)       OPZ003
210100970526     C                   MOVEL     OPV(20)       OPZ004
210200970526     C                   MOVEL     OPV(21)       OPZ005
210300970526     C                   MOVEL     OPV(22)       OPZ006
210400970526     C                   MOVEL     OPV(23)       OPZ007
210500970526     C                   MOVEL     OPV(24)       OPZ008
210600970526     C                   END
210700970526     C*
210800970526     C                   ENDSR
210900970526     C*------------------------------------------------------------------------*
211000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
211100970526     C*------------------------------------------------------------------------*
211200970526     C     OPZCON        BEGSR
211300970526     C*
211400970526     C* PULISCO CAMPI DI LAVORO
211500970526     C                   Z-ADD     0             I                 5 0
211600970526     C                   Z-ADD     0             W                 5 0
211700970526     C*
211800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
211900970526     C     1             DO        24            I
212000970526     C                   MOVEL     OPD(I)        OPDRIG
212100970526     C*
212200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
212300970526     C     OPZKEY        IFEQ      OPDOPZ
212400970526     C     OPZKEY        OREQ      OPDOP1
212500970526     C*
212600970526     C     OPDATT        IFEQ      'S'
212700970526     C                   MOVEL     OPDOPZ        OPZKEY
212800970526     C                   Z-ADD     I             W
212900970526     C                   END
213000970526     C                   END
213100970526     C*
213200970526     C                   END                                                    DO WHILE
213300970526     C*
213400970526     C* OPZIONE NON ATTIVA (ERRORE)
213500970526     C     W             IFEQ      0
213600970526     C*
213700970526     C* OPZIONE NON OK
213800970526     C                   Z-ADD     1             OPZOK
213900970526     C*
214000970526     C* EMISSIONE MESSAGGIO DI ERRORE
214100970526     C                   MOVEL     MSG(1)        DSMSMS
214200970526     C                   ELSE
214300970526     C*
214400970526     C* OPZIONE ATTIVA (PROCEDI ...)
214500060104     C*
214600060104     C* GESTISCO OPZIONI SOLO SE TRATTASI D RECORD NN A STATO 'M'
214700060104     C     VIDFLG        IFEQ      'M'
214800060104     C                   MOVEL     MSG(15)       DSMSMS
214900060104     C                   ELSE
215000010314     C*
215100010315     C* INSERISCO VERIFICA X EDI
215200970526     C     OPZKEY        IFEQ      ' 1'
215300060104     C     VIDKSC        IFNE      chkCliAnv
215400010315     C                   EXSR      O01GES
215500010315     C                   ELSE
215600010315     C                   MOVEL     MSG(14)       DSMSMS
215700010315     C                   ENDIF
215800010315     C                   END
215900010315     C     OPZKEY        IFEQ      '11'
216000060104     C     VIDKSC        IFNE      chkCliAnv
216100021126     C                   EXSR      O11GES
216200010315     C                   ELSE
216300010315     C                   MOVEL     MSG(14)       DSMSMS
216400010315     C                   ENDIF
216500010315     C                   END
216600991110     C     OPZKEY        IFEQ      ' 4'
216700991110     C                   EXSR      O04GES
216800991014     C                   END
216900991112     C     OPZKEY        IFEQ      '14'
217000991112     C                   EXSR      O14GES
217100991112     C                   END
217200970526     C     OPZKEY        IFEQ      ' 5'
217300970526     C                   EXSR      O05GES
217400970526     C                   END
217500991112     C     OPZKEY        IFEQ      '15'
217600991112     C                   EXSR      O15GES
217700991112     C                   END
217800010313     C     OPZKEY        IFEQ      '20'
217900010313     C                   EXSR      O20GES
218000010313     C                   END
218100000707     C     OPZKEY        IFEQ      '25'
218200000707     C                   EXSR      O25GES
218300000707     C                   END
218400060104     C                   ENDIF                                                  OPZ.NON ATTIVA
218500060104     C                   ENDIF                                                  OPZ.NON ATTIVA
218600970526     C*
218700970526     C                   ENDSR
218800970526     C*------------------------------------------------------------------------*
218900970526     C* NOTE COSTRUZIONE SCHIERE
219000970526     C*------------------------------------------------------------------------*
219100970526     C*
219200970526     C* -MSG-MESSAGGI
219300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
219400970526     C* (NON TOCCARE MSG,1 E MSG,2)
219500970526     C*
219600970526     C* -CMD-COMANDI
219700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
219800970526     C*
219900970526     C* -OPD-OPZIONI
220000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
220100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
220200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
220300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
220400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
220500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
220600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
220700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
220800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
220900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
221000970526     C*
221100970526     C* -FUD-FUNZIONI
221200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
221300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
221400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
221500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
221600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
221700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
221800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
221900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
222000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
222100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
222200991108     C*
222300991110     C* -FLG-CODICI STATI DEI RECORD DI LOG
222400991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
222500991110     C* ================> IMPORTANTE <=================
222600991110     C* per codici di errore in -FLG- indicare in -DES- sempre
222700991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
222800991110     C* ERR
222900991108     C*
223000970526     O*------------------------------------------------------------------------*
223100970526** DEC - DECODIFICHE
223200970526    GESTIONE                                                     1
223300970526INTERROGAZIONE                                                   2
223400970530     SCELTA                                                      3
2235009705264
2236009705265
223700970526** MSG - MESSAGGI
223800991105TIS760R- Opzione non gestita                                                   1
223900991105TIS760R- Tasto funzionale non gestito                                          2
224000021119TIS760R- Storicizzazione già effettuata impossibile eseguire opzione richiesta.3
224100080118TIS760R- Elaborazione non effettuata, impossibile visualizzare storico.        4
224200010314TIS760R-                                                                       5
224300991105TIS760R- Accettato solo un parametro di inizio lista                           6
224400991105TIS760R- 7                                                                     7
224500991105TIS760R- 8                                                                     8
224600991105TIS760R- 9                                                                     9
224700991105TIS760R- 10                                                                   10
224800991105TIS760R- 11
224900991105TIS760R- Data errata                                                          12
225000000707TIS760R- Opzione valida per record di log con stato diverso da '0'            13
225100010315TIS760R- Per il cliente scelto non è consentita l'operazione richiesta.       14
225200060104TIS760R- L'opzione richiesta non è consentita per log a stato 'M'.            15
225300101005TIS760R- Effettuare selezione                                                 16
225400991105TIS760R- 17                                                                   17
225500991105TIS760R- 18                                                                   18
225600991105TIS760R- 19                                                                   19
225700991105TIS760R- 20                                                                   20
225800970528** CMD - COMANDI CL
225900991110DSPPFM FILE(
226000991110) MBR(
226100991110
226200970528
226300970528
226400970526** OPD - OPZIONI
226500991112 1=Esec. parziale  -1 -     (S/N)=N                        opz. 01¬
226600991110                   -  -     (S/N)=N                        opz. 02¬
226700991110                   -  -     (S/N)=N                        opz. 03¬
226800991112 4=Canc. LOG       -4 -     (S/N)=N                        opz. 04¬
226900991112 5=Visual. LOG     -5 -     (S/N)=N                        opz. 05¬
227000991110                   -  -     (S/N)=N                        opz. 06¬
227100991110                   -  -     (S/N)=N                        opz. 07¬
227200991110                   -  -     (S/N)=N                        opz. 08¬
227300991110                   -  -     (S/N)=N                        opz. 09¬
227400991110                   -  -     (S/N)=N                        opz. 10¬
22750099111211=Esec. completa  -11-     (S/N)=N                        opz. 11¬
227600970526                   -  -     (S/N)=N                        opz. 12¬
227700970526                   -  -     (S/N)=N                        opz. 13¬
22780002111914=Canc. LOG/INPUT -14-     (S/N)=N                        opz. 14¬
22790099111215=Visual. STORICO -15-     (S/N)=N                        opz. 15¬
228000970526                   -  -     (S/N)=N                        opz. 16¬
228100970526                   -  -     (S/N)=N                        opz. 17¬
228200101005                   -  -     (S/N)=N                        opz. 18¬
228300970526                   -  -     (S/N)=N                        opz. 19¬
22840001031520=Avvia WRKDBF    -20-     (S/N)=N                        opz. 20¬
228500970526                   -  -     (S/N)=N                        opz. 21¬
228600970526                   -  -     (S/N)=N                        opz. 22¬
228700970526                   -  -     (S/N)=N                        opz. 23¬
22880000070725=Visual. INPUT   -25-     (S/N)=N                        opz. 25¬
228900970526** FUD - FUNZIONI
229000970526                   -        (S/N)=N                        funz.01¬
229100970526                   -        (S/N)=N                        funz.02¬
229200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
229300970526                   -        (S/N)=N                        funz.04¬
229400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
229500991108                   -        (S/N)=N                        funz.06¬
229600970526                   -        (S/N)=N                        funz.07¬
229700970527                   -        (S/N)=N                        funz.08¬
229800970527                   -        (S/N)=N                        funz.09¬
229900970527                   -        (S/N)=N                        funz.10¬
230000970526                   -        (S/N)=N                        funz.11¬
230100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
230200970526                   -        (S/N)=N                        funz.13¬
230300970526                   -        (S/N)=N                        funz.14¬
230400970526                   -        (S/N)=N                        funz.15¬
230500970526                   -        (S/N)=N                        funz.16¬
230600970526                   -        (S/N)=N                        funz.17¬
230700101005F18=Forza elab. BV -        (S/N)=N                        funz.18¬
230800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
230900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
231000991110F21=Stampa elenco  -        (S/N)=N                        funz.21¬
231100991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
231200991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
231300991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
231400991108** FLG - STATI DEI RECORD DI LOG
2315009911080
2316009911081
2317009911082
2318009911083
2319009911084
232000060104M
232100991108
232200991108
232300991108
232400991108
232500991108** DES - STATI DEI RECORD DI LOG
232600991112Da El
232700991108OK
232800991108Err
232900991112R-OK
233000991112R-Err
233100060104Temp
233200101018Init
233300101018Init
233400101018Init
233500101018Init
