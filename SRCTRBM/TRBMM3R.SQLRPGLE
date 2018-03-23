000100970526     H*------------------------------------------------------------------------*
000200030509     H* Gestione file di log INVIO SPOOL VIA E-MAIL (TIMAL00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800100219     FTRBMM3D   CF   E             WORKSTN INFDS(DEVDS1)
000900030509     F                                     SFILE(TCM3D05:SFLULT)
001000030509     F                                     SFILE(TCM3DMS:MSGULT)
001100970526     F*------------
001200050808     Ftimal01l  IF   E           K DISK
001300050808     Ftimal02l  IF   E           K DISK    RENAME(timal000:timal002)
001400050808     Ftimal04l  IF   E           K DISK    RENAME(timal000:timal004)
001500050808     Ftimal05l  IF   E           K DISK    RENAME(timal000:timal005)
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700970528     D*------------
002800970528     D* COMANDI
002900970528     D*------------
003000970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI OPZIONI
003300970526     D*------------
003400970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI TASTI FUNZIONALI
003700970526     D*------------
003800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* OPZIONI DA CARICARE A VIDEO
004100970526     D*------------
004200970526     D OPV             S             19    DIM(24)
004300970526     D*------------
004400970526     D* FUNZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970527     D FUV             S             19    DIM(24)
004700991108     D*------------
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
008600020917     D* DS REPERIMENTO DATI UTENTE
008700970526     D*------------
008800020917     D TIBS34DS      E DS                                                       *Profili utente
008900020917     D DDATIUTE      E DS                                                       *Dati utente
009000020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009100970526     D*------------
009200970526     D* ARCHITETTURA
009300970526     D*------------
009400970526     D KPJBA         E DS
009500021217     D*------------
009600040818     D* DS D PROCEDURA
009700040818     D*------------
009800100219     D TRBMM1DS      E DS
009900050224     D*------------------
010000050224     D* DS D PROCEDURA X REPERIMENTO/IMPOSTAZIONE STATO PROGRESSIVO LOG
010100050224     D*------------------
010200100219     D TRBMM9DS      E DS
010300030128     D*------------------
010400030128     D* DS CONTROLLO DATA (8)
010500030128     D*------------------
010600030128     D WLBDA8          DS                  INZ
010700030128     D  G08DAT                 1      8  0
010800030128     D  G08INV                 9     16  0
010900030128     D  G08ERR                17     17
011000030128     D  G08TGI                18     22  0
011100021211     D*------------------
011200030509     D* PASSAGGIO PARAMETRI
011300021211     D*------------------
011400021211     D PARAM           DS                  INZ
011500021211     D  POP1                   1      3
011600050111     D  POP2                   4      4
011700050111     D  PPRG                   5     11
011800050111     D  PPDA                  12     13
011900050111     D  PPA                   14     15
012000050111     D  PMSG                  16     95
012100050216     D*------------------
012200050216     D* VARIABILI D WRK
012300050216     D*------------------
012400050216     D wOBJM           S                   LIKE(V1DOBJM)
012500050216     D wDST            S                   LIKE(V1DDST)
012600050216     D wCM1VAR         S                   LIKE(§CM1VAR)
012700050216     D wCM1DST         S                   LIKE(§CM1DST)
012800050808     D wDataDA         S               D   DATFMT(*iso)
012900050216     D*------------------
013000050216     D* COSTANTI
013100050216     D*------------------
013200050216     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013300050216     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013400970526     C*------------------------------------------------------------------------*
013500970526     C* MAIN LINES
013600970526     C*------------------------------------------------------------------------*
013700100202
013800100202
013900100202     C*
014000100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014100100202     C
014200100202     C/EXEC SQL
014300100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014400100202     C/END-EXEC
014500100202     C*
014600970526     C*
014700970526     C* SE NON CI SONO ERRORI, PROSEGUE
014800970526IF  1C     ERRL00        IFNE      '1'
014900970526     C*
015000970526     C* CARICA IL SFILE
015100970526     C                   EXSR      POSSFL
015200970526     C*
015300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015700970526     C*
015800970526     C* REIMPOSTA ERRORE DI PROCERURA
015900970526     C                   MOVEL     '0'           ERRL00
016000970526     C*
016100050114     C* EMETTE IL SFILE
016200970526     C                   EXSR      OUTSFL
016300970526     C*
016400970526     C* GESTIONE TASTI FUNZIONALI
016500970526     C                   EXSR      FUNCON
016600970526     C*
016700970526E   2C                   ENDDO
016800970527     C*
016900970526E   1C                   ENDIF
017000991108     C*
017100970526     C* FINE PGM
017200970526     C                   SETON                                        LR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F05GES        BEGSR
018600970526     C*
018700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100030509     C* F12GES - GESTIONE F12 -> RITORNO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F12GES        BEGSR
019400970526     C*
019500970526     C* IMPOSTA "PREMUTO F12"
019600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019700970526     C*
019800970526     C                   ENDSR
019900970528     C*------------------------------------------------------------------------*
020000970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
020100970528     C*------------------------------------------------------------------------*
020200970528     C     F21GES        BEGSR
020300970612     C*
020400991117     C* NON GESTITA
020500970528     C*
020600970528     C                   ENDSR
020700970528     C*------------------------------------------------------------------------*
020800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
020900970528     C*------------------------------------------------------------------------*
021000970528     C     F22GES        BEGSR
021100970528     C*
021200021211     C* NON GESTITA
021300970528     C*
021400970528     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F23GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F24GES        BEGSR
022700970526     C*
022800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023300970526     C*------------------------------------------------------------------------*
023400970526     C     RUPGES        BEGSR
023500970526     C*
023600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000991108     C* ENTGES - GESTIONE ENTER -> INVIO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     ENTGES        BEGSR
024300000407     C*
024400000407     C                   MOVEL     '0'           ERRL00
024500991108     C*
024600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024700970526     C                   EXSR      NEWPOS
024800021211     C*
024900021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025100021211     C     SWPOSI        IFNE      0
025200021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
025300021211     C                   ELSE
025400021211     C     SFLULT        IFNE      0
025500021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025600021211     C*
025700021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025800021211     C     SWMODI        IFNE      0
025900021211     C                   EXSR      POSSFL
026000021211     C                   ENDIF
026100021211     C                   ENDIF
026200021211     C                   ENDIF
026300021211     C                   ENDIF
026400021211     C*
026500021211     C                   ENDSR
026600030509     C*------------------------------------------------------------------------*
026700030509     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
026800030509     C*------------------------------------------------------------------------*
026900030509     C     O02GES        BEGSR
027000030509     C*
027100050410     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE" (X ORA SOLO SE UTENTE NN EDPFG)
027200050410     C                   IF        KNMUS <> 'EDPFG'
027300050224     C                   IF        VIDSTS = '0' OR
027400050224     C                             VIDSTS = 'A' OR
027500050224     C                             VIDSTS = 'B'
027600050224     C                   MOVEL     '1'           ERRL00
027700050224     C                   EVAL      PMSG = MSG(7)
027800050224     C                   ENDIF
027900050410     C                   ENDIF
028000050224     C*
028100030509     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028200030509     C                   MOVEL     'O02'         POP1
028300030509     C                   MOVEL     VIDPROG       PPRG
028400041006     C                   MOVEL     VIDATT        PPDA
028500041006     C                   MOVEL     VIDTOTATT     PPA
028600030509     C                   EXSR      PASPAR
028700100219     C                   CALL      'TRBMM4R'
028800030509     C                   PARM                    KPJBA
028900031210     C                   MOVEL     KPJBU         PARAM
029000030509     C                   ENDIF
029100030509     C*
029200030509     C                   ENDSR
029300031210     C*------------------------------------------------------------------------*
029400031210     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZOINE RECORD/FILE
029500031210     C*------------------------------------------------------------------------*
029600031210     C     O04GES        BEGSR
029700050224     C*
029800050224     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE"
029900050224     C                   IF        VIDSTS = '0' OR
030000050224     C                             VIDSTS = 'A' OR
030100050224     C                             VIDSTS = 'B'
030200050224     C                   MOVEL     '1'           ERRL00
030300050224     C                   EVAL      PMSG = MSG(7)
030400050224     C                   ENDIF
030500031210     C*
030600031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030700031210     C                   MOVEL     'O04'         POP1
030800031210     C                   MOVEL     VIDPROG       PPRG
030900041006     C                   MOVEL     VIDATT        PPDA
031000041006     C                   MOVEL     VIDTOTATT     PPA
031100031210     C                   EXSR      PASPAR
031200100219     C                   CALL      'TRBMM4R'
031300031210     C                   PARM                    KPJBA
031400031210     C                   MOVEL     KPJBU         PARAM
031500031210     C                   ENDIF
031600031210     C*
031700031210     C                   ENDSR
031800991014     C*------------------------------------------------------------------------*
031900000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
032000970526     C*------------------------------------------------------------------------*
032100970526     C     O05GES        BEGSR
032200000407     C*
032300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400021217     C                   MOVEL     'O05'         POP1
032500030509     C                   MOVEL     VIDPROG       PPRG
032600041006     C                   MOVEL     VIDATT        PPDA
032700041006     C                   MOVEL     VIDTOTATT     PPA
032800030509     C                   EXSR      PASPAR
032900100219     C                   CALL      'TRBMM4R'
033000000407     C                   PARM                    KPJBA
033100031210     C                   MOVEL     KPJBU         PARAM
033200021211     C                   ENDIF
033300000407     C*
033400970526     C                   ENDSR
033500031210     C*------------------------------------------------------------------------*
033600031210     C* O06GES - GESTIONE OPZIONE 6=RILASCIO RECORD
033700031210     C*------------------------------------------------------------------------*
033800050224     C     O06GES        BEGSR
033900050224     C*
034000050321     C* CONSENTO OPZIONE X TUTTI GLI STATI
034100050321     C*                  IF        VIDSTS = '0' OR
034200050321     C*                            VIDSTS = 'A' OR
034300050321     C*                            VIDSTS = 'B'
034400050321     C*                  MOVEL     '1'           ERRL00
034500050321     C*                  EVAL      PMSG = MSG(7)
034600050321     C*                  ENDIF
034700031210     C*
034800031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034900100219     C                   CLEAR                   TRBMM9DS
035000050224     C                   EVAL      §CM9TIP    = 'E'
035100050224     C                   EVAL      §CM9PROG   = VIDPROG
035200050224     C                   EVAL      §CM9STSIN  = '0'
035300050224     C                   EVAL      §CM9MSGIN  = *blanks
035400100219     C                   CALL      'TRBMM9R'
035500100219     C                   PARM                    TRBMM9DS
035600050224     C*                  MOVEL     'O06'         POP1
035700050224     C*                  MOVEL     VIDPROG       PPRG
035800050224     C*                  MOVEL     VIDATT        PPDA
035900050224     C*                  MOVEL     VIDTOTATT     PPA
036000050224     C*                  EXSR      PASPAR
036100100219     C*                  CALL      'TRBMM4R'
036200050224     C*                  PARM                    KPJBA
036300050224     C*                  MOVEL     KPJBU         PARAM
036400050224     C                   ENDIF
036500031210     C*
036600031210     C                   ENDSR
036700071001     C*------------------------------------------------------------------------*
036800071001     C* O11GES - GESTIONE OPZIONE 11=RIESECUZIONE SPOOL
036900071001     C*------------------------------------------------------------------------*
037000071001     C     O11GES        BEGSR
037100071001     C*
037200071001     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037300100219     C                   CALL      'TRBMM3C'
037400071001     C                   PARM                    VIDFNAM
037500071001     C                   PARM                    VIDJNAM
037600071001     C                   PARM                    VIDUNAM
037700071001     C                   PARM                    VIDJNUM
037800071001     C                   PARM                    VIDFNUM
037900071001     C                   PARM      *BLANKS       ESITO             1
038000071001     C                   PARM      'EMAILIN_N'   WOUTQ            10
038100071001     C*
038200071002     C                   IF        ESITO = '0'
038300071001     C                   MOVEL     'O04'         POP1
038400071001     C                   MOVEL     VIDPROG       PPRG
038500071001     C                   MOVEL     VIDATT        PPDA
038600071001     C                   MOVEL     VIDTOTATT     PPA
038700071001     C                   EXSR      PASPAR
038800100219     C                   CALL      'TRBMM4R'
038900071001     C                   PARM                    KPJBA
039000071001     C                   MOVEL     KPJBU         PARAM
039100071001     C                   ENDIF
039200071001     C                   ENDIF
039300071001     C*
039400071001     C                   ENDSR
039500031211     C*------------------------------------------------------------------------*
039600031211     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE SPOOL
039700031211     C*------------------------------------------------------------------------*
039800031211     C     O15GES        BEGSR
039900031211     C*
040000031211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040100031211     C                   MOVEL     'O15'         POP1
040200100219     C                   CALL      'TRBMM4C'
040300031211     C                   PARM                    POP1
040400031211     C                   PARM                    VIDFNAM
040500031211     C                   PARM                    VIDJNAM
040600031211     C                   PARM                    VIDUNAM
040700031211     C                   PARM                    VIDJNUM
040800031211     C                   PARM                    VIDFNUM
040900031211     C                   PARM                    ESITO             1
041000031211     C                   IF        ESITO <> '0'
041100031211     C                   EVAL      PMSG = MSG(5)
041200031211     C                   ENDIF
041300031211     C                   ENDIF
041400031211     C*
041500031211     C                   ENDSR
041600991028     C*------------------------------------------------------------------------*
041700991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
041800991112     C*------------------------------------------------------------------------*
041900991112     C     PASPAR        BEGSR
042000991112     C*
042100000407     C                   MOVEL     PARAM         KPJBU
042200991112     C*
042300991112     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
042600970526     C*------------------------------------------------------------------------*
042700970526     C     NEWPOS        BEGSR
042800970526     C*
042900970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
043000970526     C*         1= RICHIESTO POSIZIONAMENTO
043100970526     C                   Z-ADD     0             SWPOSI            7 0
043200970526     C*
043300991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
043400970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
043500970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
043600970526     C                   Z-ADD     0             SWMODI            1 0
043700970530     C*
043800970530     C* REIMPOSTA INDICATORI DI ERRORE
043900050111     C                   SETOFF                                       868788
044000050111     C                   SETOFF                                       89
044100050111     C                   MOVEL     '0'           ERRPAR            1
044200050111     C*
044300050111     C* CONTROLLO SE RICHIESTE RICERCE X STRINGHE CONTENUTE (OGGETTO MAIL E DESTINATARIO MAIL)
044400050111     C* CHE SIANO STATI SPECIFICATO ALMENO 5 CARATTERI
044500050111     C                   IF        V1DOBJM <> *blanks
044600050111     C                   IF        %len(%trim(V1DOBJM)) < 5
044700050111     C                   SETON                                        86
044800050111     C                   MOVEL     '1'           ERRPAR
044900050111     C                   MOVEL     MSG(6)        DSMSMS
045000050111     C                   ENDIF
045100050111     C                   ENDIF
045200050111     C*
045300050111     C                   IF        V1DDST <> *blanks
045400050111     C                   IF        %len(%trim(V1DDST)) < 5
045500050111     C                   SETON                                        87
045600050111     C                   MOVEL     '1'           ERRPAR
045700050111     C                   MOVEL     MSG(6)        DSMSMS
045800050111     C                   ENDIF
045900050111     C                   ENDIF
046000050111     C*
046100050111     C* PROSEGUO CON VERIFICA PARAMETRI E GESTIONE FILTRI SELEZIONE SOLO SE NO ERRORI NEI PARAMETRI
046200050111     C                   IF        ERRPAR = '0'
046300040729     C*
046400040729     C* ESEGUO "FORZATURA" CONTROLLO CAMPO SELEZIONE X PROGRESSIVO UNIVOCO (ID PROG) SE = *zeros
046500040729     C                   IF        V1DPRG = *all'0'
046600040729     C                   EVAL      V1DPRG = *blanks
046700040729     C                   ENDIF
046800051026     C*
046900051026     C* SE P.O. = *ZEROS FORZO *BLANKS X CONSENTIRE LA CORRETTA "NN SELEZIONE"
047000051026     C                   IF        V1DPO = *zeros
047100051026     C                   EVAL      V1DPO = *blanks
047200051026     C                   ENDIF
047300050110     C*
047400050110     C* ESEGUO "FORZATURA" P.O. = P.O. UTENTE IN RICHIAMO DA "FILIALE" E P.O. NN INDICATO
047500050111     C   50              IF        V1DPO = *zeros  OR
047600050110     C                             V1DPO = *blanks
047700050110     C                   MOVEL     DUTPOU        V1DPO
047800050110     C                   ENDIF
047900970526     C*
048000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
048100030305     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
048200050111     C                   IF        V1DDAD  <> depdad  OR
048300050111     C                             V1DDAA  <> depdaa  OR
048400050111     C                             V1DPRG  <> depprg  OR
048500050111     C                             V1DSTS  <> depsts  OR
048600050111     C                             V1DSNA  <> depsna  OR
048700050111     C                             V1DUNA  <> depuna  OR
048800050111     C                             V1DUDT  <> depudt  OR
048900050111     C                             V1DPO   <> deppo   OR
049000050111     C                             V1DCOR  <> depcor  OR
049100050111     C                             V1DIDP  <> depidp  OR
049200050111     C                             V1DDST  <> depdst  OR
049300050111     C                             V1DOBJM <> depobjm
049400021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
049500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
049600030509     C                   Z-ADD     V1DDAD        depdad                         *DEPOSITI
049700030509     C                   MOVEL     V1DDAA        depdaa                         *DEPOSITI
049800030509     C                   MOVEL     V1DPRG        depprg                         *DEPOSITI
049900030509     C                   MOVEL     V1DSTS        depsts                         *DEPOSITI
050000030604     C                   MOVEL     V1DSNA        depsna                         *DEPOSITI
050100030604     C                   MOVEL     V1DUNA        depuna                         *DEPOSITI
050200030604     C                   MOVEL     V1DUDT        depudt                         *DEPOSITI
050300040818     C                   MOVEL     V1DPO         deppo                          *DEPOSITI
050400040818     C                   MOVEL     V1DCOR        depcor                         *DEPOSITI
050500040914     C                   MOVEL     V1DIDP        depidp                         *DEPOSITI
050600050111     C                   MOVEL     V1DOBJM       depobjm                        *DEPOSITI
050700050111     C                   MOVEL     V1DDST        depdst                         *DEPOSITI
050800021211     C                   ENDIF
050900021211     C                   ENDIF
051000970526     C*
051100050111     C                   ENDIF
051200050111     C*
051300970526     C                   ENDSR
051400970526     C*------------------------------------------------------------------------*
051500021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
051600970526     C*------------------------------------------------------------------------*
051700970528     C     SETFIL        BEGSR
051800050808     C*
051900050808     C* COSTRUISCO SUBITO LA DATA DA CUI INIZIARE LA LETTURA
052000050808     C                   EVAL      wDATA_A =
052100050808     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
052200050808     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
052300050808     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
052400050808     C                             '99.99.99.999'
052500970526     C*
052600030509     C* POSIZIONAMENTO DEL FILE
052700050808     C     WTIPRI        IFEQ      'PRG'
052800050808     C     V1DPRG        SETLL     timal01l                           99
052900050808     C                   ENDIF
053000050808     C*
053100050808     C     WTIPRI        IFEQ      'PO'
053200050808     C                   EVAL      KEYDAOR = wDATA_A
053300050808     C                   MOVEL     V1DPO         KEYPO
053400050808     C     KEYMAL5       SETLL     timal05l                           99
053500050808     C                   ENDIF
053600050808     C*
053700050808     C     WTIPRI        IFEQ      'STS'
053800050808     C                   EVAL      KEYDAOR = wDATA_A
053900050808     C                   MOVEL     V1DSTS        KEYSTS
054000050808     C     KEYMAL4       SETLL     timal04l                           99
054100021211     C                   ENDIF
054200050808     C*
054300050808     C     WTIPRI        IFEQ      'DAT'
054400050808     C                   MOVEL     *HIVAL        KEYDAOR
054500050808     C     KEYMAL2P      SETLL     timal02l                           99
054600050808     C                   ENDIF
054700980107     C*
054800021211     C* LETTURA RECORD
054900021211     C     *IN99         IFEQ      *OFF
055000970526     C                   EXSR      LETFIL
055100021211     C                   ENDIF
055200970526     C*
055300970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
055400021211     C     *IN99         IFEQ      *ON
055500970526     C                   Z-ADD     1             SFLEOF
055600021211     C                   ENDIF
055700970526     C*
055800970526     C                   ENDSR
055900970526     C*------------------------------------------------------------------------*
056000970526     C* LETFIL - LETTURA RECORD
056100970526     C*------------------------------------------------------------------------*
056200970526     C     LETFIL        BEGSR
056300970526     C*
056400970526     C* LEGGE FINO A:
056500970526     C*     - FINE FILE
056600970526     C*     - TROVATO RECORD VALIDO
056700970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
056800021211     C     *IN99         DOWEQ     *OFF
056900970526     C     WRECOK        ANDEQ     'N'
057000970526     C*
057100970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
057200050808     C     WTIPRI        IFEQ      'PRG'
057300050808     C     V1DPRG        READE     timal01l                               99
057400050808     C                   ENDIF
057500050808     C     WTIPRI        IFEQ      'PO'
057600050808     C     V1DPO         READE     timal05l                               99
057700050808     C                   ENDIF
057800050808     C     WTIPRI        IFEQ      'STS'
057900050808     C     V1DSTS        READE     timal04l                               99
058000050808     C                   ENDIF
058100050808     C     WTIPRI        IFEQ      'DAT'
058200050808     C                   READ      timal02l                               99
058300021211     C                   ENDIF
058400970528     C*
058500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
058600021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
058700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
058800021211     C                   ELSE
058900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
059000021211     C                   ENDIF
059100021211     C                   ENDDO
059200970526     C*
059300970526     C                   ENDSR
059400970526     C*------------------------------------------------------------------------*
059500021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
059600970526     C*------------------------------------------------------------------------*
059700970526     C     CHKREC        BEGSR
059800970526     C*
059900970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
060000040818     C*
060100040818     C* MUOVE I DATI UTENTI ESTESI DELLO SPOOL NELLA APPOSITA DS D RIDEFINIZIONE
060200100219     C                   MOVEL     malDUEX       TRBMM1DS
060300030509     C*
060400030128     C* FILTRA SE RICHIESTO PERIODO DA->A
060500050216     C                   IF        WRECOK = 'S'
060600050110     C                   IF        V1DDADINV > *zeros OR
060700050110     C                             V1DDAAINV > *zeros
060800030509     C                   MOVEL     *all'0'       wDATA_DA         23
060900030509     C                   MOVEL     *all'0'       wDATA_A          23
061000030509     C                   EVAL      wDATA_DA =
061100050110     C                             %subst(%editw(V1DDADINV:'        '):1:4)+'-'+
061200050110     C                             %subst(%editw(V1DDADINV:'        '):5:2)+'-'+
061300050110     C                             %subst(%editw(V1DDADINV:'        '):7:2)+'-'+
061400030509     C                             '00.00.00.000'
061500030509     C                   EVAL      wDATA_A =
061600050110     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
061700050110     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
061800050110     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
061900030509     C                             '99.99.99.999'
062000030509     C                   IF        MALDAOR >= wDATA_DA AND MALDAOR <= wDATA_A
062100030509     C                   ELSE
062200030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
062300030509     C                   ENDIF
062400050808     C                   IF        MALDAOR < wDATA_DA
062500050808     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
062600050808     C                   Z-ADD     1             SFLEOF
062700050808     C                   SETON                                        99
062800050808     C                   ENDIF
062900030509     C                   ENDIF
063000050216     C                   ENDIF
063100030509     C*
063200030509     C* FILTRA SE RICHIESTO UN PROGRESSIVO
063300050216     C                   IF        WRECOK = 'S'
063400030509     C                   IF        V1DPRG  <> *blanks
063500030509     C                   IF        MALPROG <> V1DPRG
063600030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
063700030509     C                   ENDIF
063800030509     C                   ENDIF
063900050216     C                   ENDIF
064000030509     C*
064100030509     C* FILTRA SE RICHIESTO UNO STATO
064200050216     C                   IF        WRECOK = 'S'
064300030509     C                   IF        V1DSTS <> *blanks
064400050422     C                   IF        V1DSTS = '0'
064500050422     C                   IF        MALSTS <> '0' AND MALSTS <> 'X'
064600050422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
064700050422     C                   ENDIF
064800050422     C                   ELSE
064900030509     C                   IF        MALSTS <> V1DSTS
065000030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
065100030509     C                   ENDIF
065200050422     C                   ENDIF
065300030509     C                   ENDIF
065400050216     C                   ENDIF
065500030604     C*
065600030604     C* FILTRA SE RICHIESTO UNO SPOOL NAME
065700050216     C                   IF        WRECOK = 'S'
065800030604     C                   IF        V1DSNA  <> *blanks
065900030604     C                   IF        MALFNAM <> V1DSNA
066000030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
066100030604     C                   ENDIF
066200030604     C                   ENDIF
066300050216     C                   ENDIF
066400030604     C*
066500030604     C* FILTRA SE RICHIESTO UNO USER NAME
066600050216     C                   IF        WRECOK = 'S'
066700030604     C                   IF        V1DUNA  <> *blanks
066800030604     C                   IF        MALUNAM <> V1DUNA
066900030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067000030604     C                   ENDIF
067100030604     C                   ENDIF
067200050216     C                   ENDIF
067300030604     C*
067400030604     C* FILTRA SE RICHIESTO UNO USER DATA
067500050216     C                   IF        WRECOK = 'S'
067600030604     C                   IF        V1DUDT  <> *blanks
067700030604     C                   IF        MALUDTA <> V1DUDT
067800030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067900030604     C                   ENDIF
068000030604     C                   ENDIF
068100050216     C                   ENDIF
068200040818     C*
068300040818     C* FILTRA SE RICHIESTO UN P.O.
068400050216     C                   IF        WRECOK = 'S'
068500040818     C                   IF        V1DPO = *zeros
068600040818     C                   EVAL      V1DPO = *blanks
068700040818     C                   ENDIF
068800040818     C                   IF        V1DPO  <> *blanks
068900040818     C                   IF        §CM1PO <> V1DPO
069000040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069100040818     C                   ENDIF
069200040818     C                   ENDIF
069300050216     C                   ENDIF
069400040818     C*
069500040818     C* FILTRA SE RICHIESTA UNA REGOLA
069600050216     C                   IF        WRECOK = 'S'
069700040818     C                   IF        V1DCOR   <> *blanks
069800040818     C                   IF        §CM1TIPS <> V1DCOR
069900040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070000040818     C                   ENDIF
070100040818     C                   ENDIF
070200050216     C                   ENDIF
070300040914     C*
070400040914     C* FILTRA SE RICHIESTO UN PROCESSO
070500050216     C                   IF        WRECOK = 'S'
070600040914     C                   IF        V1DIDP <> *blanks
070700040914     C                   IF        MALIDP <> V1DIDP
070800040914     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070900040914     C                   ENDIF
071000040914     C                   ENDIF
071100050216     C                   ENDIF
071200050111     C*
071300050111     C* FILTRA SE RICHIESTO UN OGGETTO MAIL
071400050216     C                   IF        WRECOK = 'S'
071500050216     C                   IF        V1DOBJM <> *blanks
071600050216     C     minu:maiu     XLATE     V1DOBJM       wOBJM                          *Minus -> Maiuscolo
071700050216     C     minu:maiu     XLATE     §CM1VAR       wCM1VAR                        *Minus -> Maiuscolo
071800050216     C                   IF        %scan(%trim(wOBJM):wCM1VAR) > 0
071900050111     C                   ELSE
072000050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072100050111     C                   ENDIF
072200050111     C                   ENDIF
072300050216     C                   ENDIF
072400050111     C*
072500050111     C* FILTRA SE RICHIESTO UN DESTINATARIO MAIL
072600050216     C                   IF        WRECOK = 'S'
072700050111     C                   IF        V1DDST <> *blanks
072800050216     C     minu:maiu     XLATE     V1DDST        wDST                           *Minus -> Maiuscolo
072900050216     C     minu:maiu     XLATE     §CM1DST       wCM1DST                        *Minus -> Maiuscolo
073000050216     C                   IF        %scan(%trim(wDST):wCM1DST) > 0
073100050111     C                   ELSE
073200050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
073300050111     C                   ENDIF
073400050111     C                   ENDIF
073500050216     C                   ENDIF
073600991108     C*
073700970526     C                   ENDSR
073800970526     C*------------------------------------------------------------------------*
073900000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
074000970526     C*------------------------------------------------------------------------*
074100970526     C     IRESFL        BEGSR
074200970526     C*
074300000407     C                   MOVEL     *blanks       OPZKEY
074400040914     C                   MOVEL     malIDP        VIDIDP
074500041005     C                   MOVEL     malATT        VIDATT
074600041005     C                   MOVEL     malTOTATT     VIDTOTATT
074700050110     C                   EVAL      VIDDAT =
074800050110     C                             %subst(malDAOR:9:2)+'/'+
074900050110     C                             %subst(malDAOR:6:2)+'/'+
075000050110     C                             %subst(malDAOR:1:4)
075100030604     C                   MOVEL     malUDTA       VIDUDTA
075200030509     C                   MOVEL     malPROG       VIDPROG
075300030509     C                   MOVEL     malFNAM       VIDFNAM
075400030509     C                   MOVEL     malJNAM       VIDJNAM
075500030509     C                   MOVEL     malUNAM       VIDUNAM
075600030509     C                   MOVEL     malJNUM       VIDJNUM
075700030509     C                   MOVEL     malFNUM       VIDFNUM
075800030918     C                   Z-ADD     malTPAG       VIDTPAG
075900050418     C                   IF        malSTS = 'X'
076000050418     C                   MOVEL     '0'           VIDSTS
076100050418     C                   ELSE
076200040818     C                   MOVEL     malSTS        VIDSTS
076300050418     C                   ENDIF
076400100219     C                   MOVEL     malDUEX       TRBMM1DS
076500040818     C                   MOVEL     §CM1TIPS      VIDCOR
076600040818     C                   MOVEL     §CM1PO        VIDPO
076700050110     C                   MOVEL     §CM1DST       VIDDST
076800030509     C*
076900040818     C* DECODIFICHE
077000040818     C                   EVAL      VIDSTSD = *blanks
077100040818     C                   IF        VIDSTS = 'H'
077200040818     C                   EVAL      VIDSTSD = 'Hld'
077300040818     C                   ENDIF
077400040818     C                   IF        VIDSTS = '1'
077500040818     C                   EVAL      VIDSTSD = 'OK'
077600040818     C                   ENDIF
077700040818     C                   IF        VIDSTS = '2'
077800040818     C                   EVAL      VIDSTSD = 'Err'
077900040818     C                   ENDIF
078000040818     C                   IF        VIDSTS = '3'
078100040818     C                   EVAL      VIDSTSD = 'Aut'
078200040818     C                   ENDIF
078300040818     C*
078400970526     C                   ENDSR
078500970526     C*------------------------------------------------------------------------*
078600991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
078700970526     C*------------------------------------------------------------------------*
078800970526     C     CHKPAR        BEGSR
078900021218     C*
079000021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
079100030509     C                   MOVEL     '0'           W_ERR             1
079200050808     C*
079300050808     C* VERIFICO X QUALE VISTA LOGICA CONVIENE LEGGERE IN RELAZIONE AI PARAMETRI DI SELEZIONE
079400050808     C                   SELECT
079500050808     C                   WHEN      V1DPRG <> *blanks
079600050808     C                   EVAL      WTIPRI = 'PRG'
079700050808     C                   WHEN      V1DSTS = '2'
079800050808     C                   EVAL      WTIPRI = 'STS'
079900050808     C                   WHEN      V1DSTS = '3'
080000050808     C                   EVAL      WTIPRI = 'STS'
080100050808     C                   WHEN      V1DSTS = 'A'
080200050808     C                   EVAL      WTIPRI = 'STS'
080300050808     C                   WHEN      V1DSTS = 'B'
080400050808     C                   EVAL      WTIPRI = 'STS'
080500050808     C                   WHEN      V1DPO  <> *blanks
080600050808     C                   EVAL      WTIPRI = 'PO'
080700050808     C                   OTHER
080800050808     C                   MOVEL     'DAT'         WTIPRI            3
080900050808     C                   ENDSL
081000030128     C*
081100030128     C* CONTROLLO VALIDITA PERIODO DA
081200030128     C                   IF        W_ERR = '0'
081300050110     C                   Z-ADD     *ZEROS        V1DDADINV         8 0
081400030509     C                   IF        V1DDAD > *zeros
081500030509     C                   Z-ADD     V1DDAD        DAW8
081600030128     C                   EXSR      CHKDA8
081700030128     C                   IF        G08ERR = '1'
081800030128     C                   EVAL      W_ERR = '1'
081900030128     C                   SETON                                        88
082000050110     C                   ELSE
082100050110     C                   Z-ADD     WDA8          V1DDADINV
082200050110     C                   Z-ADD     DAW8          V1DDAD
082300030128     C                   ENDIF
082400030128     C                   ENDIF
082500030128     C                   ENDIF
082600030128     C*
082700030128     C* CONTROLLO VALIDITA PERIODO A
082800030128     C                   IF        W_ERR = '0'
082900050110     C                   Z-ADD     *ZEROS        V1DDAAINV         8 0
083000050110     C                   IF        V1DDAA  > *zeros
083100030509     C                   Z-ADD     V1DDAA        DAW8
083200030128     C                   EXSR      CHKDA8
083300030128     C                   IF        G08ERR = '1'
083400030128     C                   EVAL      W_ERR = '1'
083500030128     C                   SETON                                        89
083600050110     C                   ELSE
083700050110     C                   Z-ADD     WDA8          V1DDAAINV
083800050110     C                   Z-ADD     DAW8          V1DDAA
083900030128     C                   ENDIF
084000030128     C                   ENDIF
084100030128     C                   ENDIF
084200030128     C*
084300030128     C* CONTROLLO PERIODO DA <= PERIODO A
084400030128     C                   IF        W_ERR = '0'
084500050110     C                   IF        V1DDADINV > V1DDAAINV
084600030128     C                   EVAL      W_ERR = '1'
084700030128     C                   SETON                                        8889
084800030128     C                   ENDIF
084900030128     C                   ENDIF
085000000407     C*
085100970526     C                   ENDSR
085200970526     C*------------------------------------------------------------------------*
085300970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
085400970526     C*------------------------------------------------------------------------*
085500970526     C     CHKFOP        BEGSR
085600991112     C*
085700991112     C* OPZIONI
085800970626     C*
085900000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
086000021211     C     DEPOP0        IFEQ      'P01'
086100000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
086200000420     C                   MOVEL     'S'           OPDATT
086300000420     C                   MOVEL     OPDRIG        OPD(1)
086400021211     C                   ELSE
086500000420     C* OPZIONI
086600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
086700970626     C                   MOVEL     'S'           OPDATT
086800970626     C                   MOVEL     OPDRIG        OPD(5)
086900021211     C                   ENDIF
087000970626     C*
087100000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
087200021211     C     DEPOP0        IFEQ      'P02'
087300991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
087400991112     C                   MOVEL     'S'           OPDATT
087500991112     C                   MOVEL     OPDRIG        OPD(5)
087600050111     C                   MOVEL     OPD(15)       OPDRIG                         *VISUALIZZA SPOOL
087700050111     C                   MOVEL     'S'           OPDATT
087800050111     C                   MOVEL     OPDRIG        OPD(15)
087900030509     C                   IF        %subst(KNMUS:1:3) = 'EDP'
088000030509     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
088100030509     C                   MOVEL     'S'           OPDATT
088200030509     C                   MOVEL     OPDRIG        OPD(2)
088300031210     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
088400031210     C                   MOVEL     'S'           OPDATT
088500031210     C                   MOVEL     OPDRIG        OPD(4)
088600031210     C                   MOVEL     OPD(6)        OPDRIG                         *RILASCIO
088700031210     C                   MOVEL     'S'           OPDATT
088800031210     C                   MOVEL     OPDRIG        OPD(6)
088900071001     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE SPOOL
089000071001     C                   MOVEL     'S'           OPDATT
089100071001     C                   MOVEL     OPDRIG        OPD(11)
089200021211     C                   ENDIF
089300030509     C                   ENDIF
089400970526     C*
089500970526     C* GESTIONE OPZIONI A VIDEO
089600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
089700970526     C                   EXSR      OPZGES
089800970526     C*
089900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
090000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
090100970526     C                   EXSR      FUNGES
090200970526     C*
090300970526     C                   ENDSR
090400020917     C*--------------------------------------------------------------------------------------------*
090500020917     C* REPERISCE I DATI UTENTE
090600020917     C*--------------------------------------------------------------------------------------------*
090700020917     C     REPDATIUTE    BEGSR
090800020917     C*
090900020917     C* INIZIALIZZA VARIABILI DI WRK
091000020917     C                   CLEAR                   TIBS34DS
091100020917     C                   CLEAR                   AZUTEDS
091200020917     C                   CLEAR                   DDATIUTE
091300020917     C*
091400020917     C     *DTAARA       DEFINE    §azute        azuteds
091500020917     C     *DTAARA       DEFINE    §datiute      ddatiute
091600020917     C                   IN(E)     *DTAARA
091700021211     C                   IF        %Error
091800021211     C                   EVAL      I34Tla = 'L'
091900020917     C                   CALL      'TIBS34R'
092000020917     C                   PARM                    Tibs34Ds
092100020917     C                   IN        *DTAARA
092200021211     C                   ENDIF
092300020917     C*
092400020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
092500020917     C                   MOVEL(P)  RSUT          DSFIRS           20
092600020917     C*
092700020917     C                   ENDSR
092800030128     C*------------------------------------------------------------------------*
092900030128     C* CHKDA8 - CONTROLLA LA DATA (8)
093000030128     C*------------------------------------------------------------------------*
093100030128     C     CHKDA8        BEGSR
093200030128     C*
093300050110     C                   Z-ADD     DAW8          G08DAT
093400050110     C                   Z-ADD     *ZEROS        G08INV
093500050110     C                   MOVEL     '0'           G08ERR
093600030128     C                   CALL      'XSRDA8'
093700030128     C                   PARM                    WLBDA8
093800050110     C                   Z-ADD     G08INV        WDA8
093900050110     C                   Z-ADD     G08DAT        DAW8
094000030128     C*
094100030128     C                   ENDSR
094200970526     C*------------------------------------------------------------------------*
094300970526     C* *INZSR - OPERAZIONI INIZIALI
094400970526     C*------------------------------------------------------------------------*
094500970526     C     *INZSR        BEGSR
094600970526     C*---
094700970526     C* RICEVIMENTO PARAMETRI
094800970526     C*---
094900970526     C     *ENTRY        PLIST
095000970612     C                   PARM                    KPJBA
095100021211     C                   MOVEL     KPJBU         PARAM
095200030509     C                   MOVEL     POP1          DEPOP0            3
095300050110     C*
095400050110     C* REPERIMENTO DATI UTENTE
095500050110     C                   EXSR      REPDATIUTE
095600050808     C*
095700050808     C* CALCOLA LA DATA CORRENTE
095800050808     C                   time                    wn14             14 0
095900050808     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
096000050808     C                   z-add     wn8           g08dat
096100050808     C                   z-add     *zeros        g08inv
096200050808     C                   movel     '0'           g08err
096300050808     C                   call      'XSRDA8'
096400050808     C                   parm                    wlbda8
096500050808     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
096600030509     C*
096700030509     C* A SECONDA DELL'OPZOINE DI ENTRATA IMPOSTO LA MODALITA' A VIDEO
096800050110     C                   SETOFF                                       50
096900050111     C                   MOVEL     'S'           POP2                           * SEDE
097000030509     C                   IF        DEPOP0 = 'P05'
097100030509     C                   EVAL      VIDING = DEC(2)
097200050110     C                   MOVEL     DUTPOU        V1DPO
097300050111     C                   MOVEL     DUTPOU        deppo
097400050110     C                   SETON                                        50
097500050111     C                   MOVEL     'F'           POP2                           * FILIALE
097600030509     C                   ENDIF
097700030509     C                   IF        DEPOP0 = 'P02'
097800030509     C                   EVAL      VIDING = DEC(1)
097900030509     C                   ENDIF
098000030604     C*
098100030604     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
098200050808     C     *iso          move      datcor        wDataDA
098300050808     C                   subdur    30:*d         wDataDA
098400050808     C                   move      wDataDA       V1DDAD
098500050808     C                   z-add     *zeros        g08dat
098600050808     C                   z-add     V1DDAD        g08inv
098700050808     C                   movel     '3'           g08err
098800050808     C                   call      'XSRDA8'
098900050808     C                   parm                    wlbda8
099000050808     C                   z-add     g08dat        V1DDAD
099100050808     C                   EVAL      V1DDAA = 31122099
099200060731     C                   EVAL      depdad = V1DDAD
099300030604     C                   EVAL      depdaa = V1DDAA
099400970526     C*
099500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
099600970526     C     XMSGPA        PLIST
099700970526     C                   PARM                    DSMSMS
099800970526     C                   PARM                    DSMSPG
099900970526     C                   PARM                    DSMSKE
100000970526     C                   PARM                    DSMSER
100100970526     C*
100200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
100300970526     C                   CALL      'XKEY'
100400970526     C                   PARM                    DSKEY
100500970526     C*---
100600970526     C* VARIABILI NON RIFERITE AL DATA BASE
100700970526     C*---
100800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
100900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
101000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
101100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
101200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
101300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
101400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
101500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
101600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
101700970526     C*---
101800030509     C     *LIKE         DEFINE    V1DDAD        DAW8
101900030509     C     *LIKE         DEFINE    V1DDAD        WDA8
102000030509     C     *LIKE         DEFINE    V1DDAD        depdad
102100030509     C     *LIKE         DEFINE    V1DDAA        depdaa
102200030509     C     *LIKE         DEFINE    V1DSTS        depsts
102300040818     C     *LIKE         DEFINE    V1DPO         deppo
102400040818     C     *LIKE         DEFINE    V1DCOR        depcor
102500030509     C     *LIKE         DEFINE    V1DPRG        depprg
102600030604     C     *LIKE         DEFINE    V1DSNA        depsna
102700030604     C     *LIKE         DEFINE    V1DUNA        depuna
102800030604     C     *LIKE         DEFINE    V1DUDT        depudt
102900040914     C     *LIKE         DEFINE    V1DIDP        depidp
103000050111     C     *LIKE         DEFINE    V1DDST        depdst
103100050111     C     *LIKE         DEFINE    V1DOBJM       depobjm
103200030509     C     *LIKE         DEFINE    MALDAOR       KEYDAOR
103300050808     C     *LIKE         DEFINE    MALSTS        KEYSTS
103400050808     C     *LIKE         DEFINE    §CM1PO        KEYPO
103500021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
103600991108     C*---
103700970526     C* VARIABILI DI CONTROLLO
103800970526     C*---
103900970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
104000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
104100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
104200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
104300970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
104400050110     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
104500970526     C*
104600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
104700100219     C                   MOVEL     'TRBMM3R'     DSMSPG
104800100219     C                   MOVEL     'TRBMM3R'     MSGQUE
104900970526     C                   MOVEL     *BLANKS       MSGKEY
105000970526     C                   MOVEL     *BLANKS       DSMSMS
105100970526     C                   MOVEL     *BLANKS       DSMSKE
105200970526     C                   Z-ADD     *ZEROS        DSMSER
105300970526     C*---
105400970526     C* CHIAVI DI LETTURA
105500970526     C*---
105600050808     C* LETTURA timal02l - PARZIALE
105700050808     C     KEYMAL2P      KLIST
105800050808     C                   KFLD                    KEYDAOR
105900050808     C* LETTURA timal04l - COMPLETA
106000050808     C     KEYMAL4       KLIST
106100050808     C                   KFLD                    KEYSTS
106200050808     C                   KFLD                    KEYDAOR
106300050808     C* LETTURA timal05l - COMPLETA
106400050808     C     KEYMAL5       KLIST
106500050808     C                   KFLD                    KEYPO
106600050808     C                   KFLD                    KEYDAOR
106700970526     C*
106800970526     C* IMPOSTA I PARAMETRI DI INGRESSO
106900970526     C                   EXSR      CHKPAR
107000991115     C*
107100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
107200970526     C                   EXSR      CHKFOP
107300970526     C*
107400970526     C                   ENDSR
107500970526     C*------------------------------------------------------------------------*
107600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
107900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
108000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
108100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108300970526     C*------------------------------------------------------------------------*
108400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108500970526     C*------------------------------------------------------------------------*
108600970526     C     POSSFL        BEGSR
108700970526     C*
108800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
108900970526     C                   EXSR      CLRSFL
109000970526     C*
109100970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
109200970526     C                   EXSR      SETFIL
109300970526     C*
109400970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
109500970526     C     SFLEOF        IFEQ      0
109600970526     C                   EXSR      CARSFL
109700030305     C                   ENDIF
109800970526     C*
109900970526     C                   ENDSR
110000970526     C*------------------------------------------------------------------------*
110100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
110200970526     C*------------------------------------------------------------------------*
110300970526     C     ENTATT        BEGSR
110400970526     C*
110500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
110600970526     C                   Z-ADD     0             OPZOK             1 0
110700970526     C*
110800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
110900970526     C                   Z-ADD     0             SFLVAR            5 0
111000970526     C*
111100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
111200970526     C                   Z-ADD     0             SFLERR
111300970526     C*
111400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
111500970526     C                   Z-ADD     SFLULT        WFLULT
111600970526     C*
111700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
111800030509     C                   READC     TCM3D05                                98
111900970526     C     *IN98         DOWEQ     '0'
112000970526     C*
112100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
112200970526     C                   ADD       1             SFLVAR
112300970526     C*
112400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
112500970526     C     OPZOK         IFEQ      0
112600970526     C*
112700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
112800970526     C     OPZKEY        IFNE      *BLANKS
112900970526     C                   EXSR      OPZCON
113000970526     C                   END
113100970526     C*
113200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
113300970526     C*            2. PULIZIA SCELTA IN SFL
113400970526     C     OPZOK         IFEQ      0
113500970526     C                   MOVEL     '0'           *IN41
113600970526     C                   MOVEL     *BLANKS       OPZKEY
113700970526     C                   ELSE
113800970526     C*
113900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
114000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
114100970526     C                   MOVEL     '1'           *IN41
114200970526     C                   Z-ADD     SFLULT        SFLERR
114300970526     C                   END
114400970526     C                   ELSE
114500970526     C*
114600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
114700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
114800970526     C                   MOVEL     '1'           *IN41
114900970526     C                   END                                                    OPZOK=0
115000970526     C*
115100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
115200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
115300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
115400970526     C     OPZOK         IFNE      0
115500970526     C                   Z-ADD     SFLERR        SFLNBR
115600970526     C                   ELSE
115700970526     C                   Z-ADD     SFLULT        SFLNBR
115800970526     C                   END
115900970526     C*
116000970526     C* AGGIORNO RECORD SFL
116100030509     C                   UPDATE    TCM3D05
116200970526     C*
116300030509     C                   READC     TCM3D05                                98
116400970526     C                   END                                                    DO WHILE
116500970526     C*
116600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
116700970526     C                   Z-ADD     WFLULT        SFLULT
116800970526     C*
116900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
117000970526     C     SFLVAR        IFEQ      0
117100970526     C                   END
117200991112     C*
117300970526     C                   ENDSR
117400970526     C*------------------------------------------------------------------------*
117500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
117600970526     C*------------------------------------------------------------------------*
117700970526     C     CLRSFL        BEGSR
117800970526     C*
117900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
118000970526     C* (0=NON FINE FILE  1=FINE FILE)
118100970526     C                   Z-ADD     0             SFLEOF
118200970526     C*
118300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
118400970526     C                   Z-ADD     0             SFLULT
118500970526     C*
118600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
118700970526     C                   MOVEL     '1'           *IN40
118800030509     C                   WRITE     TCM3D06
118900970526     C                   MOVEL     '0'           *IN40
119000970526     C*
119100970526     C                   ENDSR
119200970526     C*------------------------------------------------------------------------*
119300970526     C* CARSFL - CARICAMENTO SFL
119400970526     C*------------------------------------------------------------------------*
119500970526     C     CARSFL        BEGSR
119600970526     C*
119700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
119800970526     C     SFLULT        IFEQ      0
119900970526     C                   EXSR      SETFIL
120000970526     C                   END
120100970526     C*
120200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
120300970526     C                   Z-ADD     SFLULT        WFLULT
120400970526     C*
120500970526     C* CICLO:
120600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
120700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
120800970526     C                   Z-ADD     1             SFLRIG
120900030509     C     SFLRIG        DOWLE     SFLPAG
121000030509     C     SFLEOF        ANDEQ     0
121100970526     C*
121200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
121300991116     C                   MOVEL     *BLANKS       OPZKEY
121400970526     C*
121500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
121600970526     C                   EXSR      IRESFL
121700970526     C*
121800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
121900970526     C                   ADD       1             SFLULT
122000970526     C*
122100970526     C* SCRITTURA RECORD SFL
122200030509     C                   WRITE     TCM3D05
122300970526     C                   ADD       1             SFLRIG
122400970526     C*
122500970526     C* LETTURA FILE DA CARICARE
122600970526     C                   EXSR      LETFIL
122700970526     C                   END                                                    DO WHILE
122800970526     C*
122900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
123000970526     C     WFLULT        IFNE      SFLULT
123100970526     C                   Z-ADD     WFLULT        SFLNBR
123200970526     C                   ADD       1             SFLNBR
123300970526     C                   END
123400970526     C*
123500970526     C                   ENDSR
123600970526     C*------------------------------------------------------------------------*
123700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
123800970526     C*------------------------------------------------------------------------*
123900970526     C     OUTSFL        BEGSR
124000970526     C*
124100970526     C* WRITE RECORD TESTATA VIDEO
124200030509     C                   WRITE     TCM3D01
124300970526     C*
124400970526     C* VISUALIZZAZIONE MESSAGGIO
124500970526     C                   EXSR      SNDMSG
124600970526     C*
124700970526     C* WRITE RECORD OPZIONI VIDEO
124800030509     C                   WRITE     TCM3D02
124900970526     C*
125000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
125100030509     C                   WRITE     TCM3D03
125200970526     C*
125300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
125400030509     C                   WRITE     TCM3D04
125500970526     C*
125600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
125700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
125800970526     C     SFLEOF        IFEQ      0
125900970526     C                   MOVEL     '0'           *IN42
126000970526     C                   ELSE
126100970526     C*
126200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
126300970526     C                   MOVEL     '1'           *IN42
126400970526     C                   END
126500970526     C*
126600970526     C* WRITE RECORD TASTI FUNZIONALI
126700030509     C                   WRITE     TCM3D09
126800970526     C*
126900970526     C* CASO DI SFL VUOTO
127000970526     C     SFLULT        IFLE      0
127100970526     C*
127200970526     C*    - WRITE RECORD VUOTO
127300030509     C                   WRITE     TCM3D0A
127400030509     C                   READ      TCM3D0A                                99
127500970526     C*
127600970526     C                   ELSE
127700970526     C*
127800970526     C* CASO DI SFL CON RECORDS
127900970526     C*
128000970526     C*    - WRITE RECORD CONTROLLO SFL
128100030509     C                   WRITE     TCM3D06
128200030509     C                   READ      TCM3D06                                99
128300970526     C                   END
128400970526     C*
128500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
128600030509     C                   READ      TCM3D03                                99
128700970526     C*
128800970526     C                   ENDSR
128900970526     C*------------------------------------------------------------------------*
129000970526     C* SNDMSG - MANDA MESSAGGIO
129100970526     C*------------------------------------------------------------------------*
129200970526     C     SNDMSG        BEGSR
129300970526     C*
129400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
129500970526     C                   MOVEL     '1'           *IN40
129600030509     C                   WRITE     TCM3DMC
129700970526     C                   MOVEL     '0'           *IN40
129800970526     C*
129900970526     C* VISUALIZZAZIONE MESSAGGIO
130000970526     C                   Z-ADD     0             MSGULT
130100970526     C                   CALL      'XMSG'        XMSGPA
130200970526     C     DSMSER        IFEQ      0
130300970526     C                   MOVEL     DSMSKE        MSGKEY
130400970526     C                   END
130500970526     C                   Z-ADD     1             MSGULT
130600030509     C                   WRITE     TCM3DMS                                      SFL MESS
130700030509     C                   WRITE     TCM3DMC                                      CONTR MESS
130800970526     C*
130900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
131000970526     C                   MOVEL     *BLANKS       DSMSMS
131100970526     C*
131200970526     C                   ENDSR
131300970526     C*------------------------------------------------------------------------*
131400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
131500970526     C*------------------------------------------------------------------------*
131600970526     C     FUNCON        BEGSR
131700970526     C*
131800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
131900970526     C                   EXSR      FUNVAR
132000970526     C*
132100970526     C* PULISCO CAMPI DI LAVORO
132200970526     C                   Z-ADD     0             I                 5 0
132300970526     C                   Z-ADD     0             W                 5 0
132400970526     C*
132500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
132600970526     C     FUNKEY        IFNE      *BLANKS
132700970526     C     1             DO        24            I
132800970526     C                   MOVEL     FUD(I)        FUDRIG
132900970526     C*
133000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
133100970526     C     FUNKEY        IFEQ      FUDFUN
133200970526     C     FUDATT        ANDEQ     'S'
133300970526     C                   Z-ADD     I             W
133400970526     C                   END
133500970526     C                   END                                                    DO WHILE
133600970526     C*
133700970526     C* FUNZIONE NON ATTIVA (ERRORE)
133800970526     C     W             IFEQ      0
133900970526     C*
134000970526     C* EMISSIONE MESSAGGIO DI ERRORE
134100970526     C                   MOVEL     MSG(2)        DSMSMS
134200970526     C                   END                                                    W=0
134300970526     C                   END                                                    FUNKEY NE *BLANKS
134400970526     C*
134500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
134600970526     C     W             IFNE      0
134700970526     C     FUNKEY        OREQ      *BLANKS
134800970526     C*
134900970526     C     FUNKEY        IFEQ      *BLANKS
135000970526     C                   EXSR      ENTGES
135100970526     C                   END
135200970526     C     FUNKEY        IFEQ      '03'
135300970526     C                   EXSR      F03GES
135400970526     C                   END
135500970526     C     FUNKEY        IFEQ      '05'
135600970526     C                   EXSR      F05GES
135700970526     C                   END
135800970526     C     FUNKEY        IFEQ      '12'
135900970526     C                   EXSR      F12GES
136000970526     C                   END
136100970528     C     FUNKEY        IFEQ      '21'
136200970528     C                   EXSR      F21GES
136300970528     C                   END
136400970528     C     FUNKEY        IFEQ      '22'
136500970528     C                   EXSR      F22GES
136600970528     C                   END
136700970526     C     FUNKEY        IFEQ      '23'
136800970526     C                   EXSR      F23GES
136900970526     C                   END
137000970526     C     FUNKEY        IFEQ      '24'
137100970526     C                   EXSR      F24GES
137200970526     C                   END
137300970526     C     FUNKEY        IFEQ      'RU'
137400970526     C                   EXSR      RUPGES
137500970526     C                   END
137600970526     C*
137700970526     C                   END
137800970526     C*
137900970526     C                   ENDSR
138000970526     C*------------------------------------------------------------------------*
138100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
138200970526     C*------------------------------------------------------------------------*
138300970526     C     FUNVAR        BEGSR
138400970526     C*
138500030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
138600970526     C     KEY           IFEQ      ENTER
138700970526     C                   MOVEL     *BLANKS       FUNKEY
138800970526     C                   END
138900970526     C     KEY           IFEQ      F1
139000970526     C                   MOVEL     '01'          FUNKEY
139100970526     C                   END
139200970526     C     KEY           IFEQ      F2
139300970526     C                   MOVEL     '02'          FUNKEY
139400970526     C                   END
139500970526     C     KEY           IFEQ      F3
139600970526     C                   MOVEL     '03'          FUNKEY
139700970526     C                   END
139800970526     C     KEY           IFEQ      F4
139900970526     C                   MOVEL     '04'          FUNKEY
140000970526     C                   END
140100970526     C     KEY           IFEQ      F5
140200970526     C                   MOVEL     '05'          FUNKEY
140300970526     C                   END
140400970526     C     KEY           IFEQ      F6
140500970526     C                   MOVEL     '06'          FUNKEY
140600970526     C                   END
140700970526     C     KEY           IFEQ      F7
140800970526     C                   MOVEL     '07'          FUNKEY
140900970526     C                   END
141000970526     C     KEY           IFEQ      F8
141100970526     C                   MOVEL     '08'          FUNKEY
141200970526     C                   END
141300970526     C     KEY           IFEQ      F9
141400970526     C                   MOVEL     '09'          FUNKEY
141500970526     C                   END
141600970526     C     KEY           IFEQ      F10
141700970526     C                   MOVEL     '10'          FUNKEY
141800970526     C                   END
141900970526     C     KEY           IFEQ      F11
142000970526     C                   MOVEL     '11'          FUNKEY
142100970526     C                   END
142200970526     C     KEY           IFEQ      F12
142300970526     C                   MOVEL     '12'          FUNKEY
142400970526     C                   END
142500970526     C     KEY           IFEQ      F13
142600970526     C                   MOVEL     '13'          FUNKEY
142700970526     C                   END
142800970526     C     KEY           IFEQ      F14
142900970526     C                   MOVEL     '14'          FUNKEY
143000970526     C                   END
143100970526     C     KEY           IFEQ      F15
143200970526     C                   MOVEL     '15'          FUNKEY
143300970526     C                   END
143400970526     C     KEY           IFEQ      F16
143500970526     C                   MOVEL     '16'          FUNKEY
143600970526     C                   END
143700970526     C     KEY           IFEQ      F17
143800970526     C                   MOVEL     '17'          FUNKEY
143900970526     C                   END
144000970526     C     KEY           IFEQ      F18
144100970526     C                   MOVEL     '18'          FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F19
144400970526     C                   MOVEL     '19'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F20
144700970526     C                   MOVEL     '20'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F21
145000970526     C                   MOVEL     '21'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F22
145300970526     C                   MOVEL     '22'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F23
145600970526     C                   MOVEL     '23'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F24
145900970526     C                   MOVEL     '24'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      ROLLUP
146200970526     C                   MOVEL     'RU'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      HELP
146500970526     C                   MOVEL     'HP'          FUNKEY
146600970526     C                   END
146700970526     C*
146800970526     C                   ENDSR
146900970526     C*------------------------------------------------------------------------*
147000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
147100970526     C*------------------------------------------------------------------------*
147200970526     C     FUNGES        BEGSR
147300970526     C*
147400970526     C* PULISCO CAMPI DI LAVORO
147500970526     C                   Z-ADD     0             W                 5 0
147600970526     C*
147700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
147800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
147900970526     C     1             DO        24            I
148000970526     C                   MOVEL     FUD(I)        FUDRIG
148100970526     C*
148200970526     C* PULIZIA FUNZIONI PRECEDENTI
148300970526     C                   MOVEL     *BLANKS       FUV(I)
148400970526     C*
148500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
148600970526     C* (SALTA F23 E F24)
148700970526     C     FUDATT        IFEQ      'S'
148800970526     C     FUDFUN        ANDNE     '23'
148900970526     C     FUDFUN        ANDNE     '24'
149000970526     C                   ADD       1             W
149100970526     C                   MOVEL     FUDDES        FUV(W)
149200970526     C                   END
149300970526     C                   END                                                    DO WHILE
149400970526     C*
149500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
149600970526     C* ALTRIMENTI DISATTIVO F24
149700970526     C     1             DO        24            I
149800970526     C                   MOVEL     FUD(I)        FUDRIG
149900970526     C*
150000970526     C     FUDFUN        IFEQ      '24'
150100970526     C     W             IFGT      6
150200970526     C                   MOVEL     'S'           FUDATT
150300970526     C                   ELSE
150400970526     C                   MOVEL     *BLANKS       FUDATT
150500970526     C                   END                                                    W GT 8
150600970526     C                   MOVEL     FUDRIG        FUD(I)
150700970526     C                   END
150800970526     C                   END                                                    DO WHILE
150900970526     C*
151000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
151100970526     C     W             DIV       6             I
151200970526     C                   MVR                     Z
151300970526     C*
151400970526     C     Z             IFNE      0
151500970526     C     I             ADD       1             Z
151600970526     C                   ELSE
151700970526     C                   Z-ADD     I             Z
151800970526     C                   END
151900970526     C*
152000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
152100970526     C                   ADD       1             FUNGRU
152200970526     C     FUNGRU        IFGT      Z
152300970526     C     FUNGRU        ORLT      1
152400970526     C                   Z-ADD     1             FUNGRU
152500970526     C                   END
152600970526     C*
152700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
152800970526     C     FUNGRU        IFEQ      1
152900970526     C                   MOVEL     FUV(1)        FUN001
153000970526     C                   MOVEL     FUV(2)        FUN002
153100970526     C                   MOVEL     FUV(3)        FUN003
153200970526     C                   MOVEL     FUV(4)        FUN004
153300970526     C                   MOVEL     FUV(5)        FUN005
153400970526     C                   MOVEL     FUV(6)        FUN006
153500970526     C                   END
153600970526     C*
153700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
153800970526     C     FUNGRU        IFEQ      2
153900970526     C                   MOVEL     FUV(7)        FUN001
154000970526     C                   MOVEL     FUV(8)        FUN002
154100970526     C                   MOVEL     FUV(9)        FUN003
154200970526     C                   MOVEL     FUV(10)       FUN004
154300970526     C                   MOVEL     FUV(11)       FUN005
154400970526     C                   MOVEL     FUV(12)       FUN006
154500970526     C                   END
154600970526     C*
154700970526     C* TERZO GRUPPO DI 6 FUNZIONI
154800970526     C     FUNGRU        IFEQ      3
154900970526     C                   MOVEL     FUV(13)       FUN001
155000970526     C                   MOVEL     FUV(14)       FUN002
155100970526     C                   MOVEL     FUV(15)       FUN003
155200970526     C                   MOVEL     FUV(16)       FUN004
155300970526     C                   MOVEL     FUV(17)       FUN005
155400970526     C                   MOVEL     FUV(18)       FUN006
155500970526     C                   END
155600970526     C*
155700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
155800970526     C     FUNGRU        IFEQ      4
155900970526     C                   MOVEL     FUV(19)       FUN001
156000970526     C                   MOVEL     FUV(20)       FUN002
156100970526     C                   MOVEL     FUV(21)       FUN003
156200970526     C                   MOVEL     FUV(22)       FUN004
156300970526     C                   MOVEL     *BLANKS       FUN005
156400970526     C                   MOVEL     *BLANKS       FUN006
156500970526     C                   END
156600970526     C*
156700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
156800970526     C                   MOVEL     *BLANKS       FUN007
156900970526     C                   MOVEL     *BLANKS       FUN008
157000970526     C*
157100970526     C     1             DO        24            I
157200970526     C                   MOVEL     FUD(I)        FUDRIG
157300970526     C     FUDFUN        IFEQ      '23'
157400970526     C     FUDATT        ANDEQ     'S'
157500970526     C                   MOVEL     FUDDES        FUN007
157600970526     C                   END
157700970526     C     FUDFUN        IFEQ      '24'
157800970526     C     FUDATT        ANDEQ     'S'
157900970526     C                   MOVEL     FUDDES        FUN008
158000970526     C                   END
158100970526     C                   END                                                    DO
158200970526     C*
158300970526     C                   ENDSR
158400970526     C*------------------------------------------------------------------------*
158500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
158600970526     C*------------------------------------------------------------------------*
158700970526     C     OPZGES        BEGSR
158800970526     C*
158900970526     C* PULISCO CAMPI DI LAVORO
159000970526     C                   Z-ADD     0             W                 5 0
159100970526     C*
159200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
159300970526     C* E CARICO TABELLA OPZIONI ATTIVE
159400991115     C     1             DO        24            I
159500970526     C                   MOVEL     OPD(I)        OPDRIG
159600970526     C*
159700970526     C* PULIZIA OPZIONI PRECEDENTI
159800970526     C                   MOVEL     *BLANKS       OPV(I)
159900970526     C*
160000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
160100970526     C     OPDATT        IFEQ      'S'
160200970526     C                   ADD       1             W
160300970526     C                   MOVEL     OPDDES        OPV(W)
160400970526     C                   END
160500970526     C                   END                                                    DO WHILE
160600970526     C*
160700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
160800970526     C* ALTRIMENTI DISATTIVO F23
160900991115     C     1             DO        24            I
161000970526     C                   MOVEL     FUD(I)        FUDRIG
161100970526     C*
161200970526     C     FUDFUN        IFEQ      '23'
161300970526     C     W             IFGT      8
161400970526     C                   MOVEL     'S'           FUDATT
161500970526     C                   ELSE
161600970526     C                   MOVEL     *BLANKS       FUDATT
161700970526     C                   END                                                    W GT 8
161800970526     C                   MOVEL     FUDRIG        FUD(I)
161900970526     C                   END
162000970526     C                   END                                                    DO WHILE
162100970526     C*
162200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
162300970526     C     W             DIV       8             I
162400970526     C                   MVR                     Z                 5 0
162500970526     C*
162600970526     C     Z             IFNE      0
162700970526     C     I             ADD       1             Z
162800970526     C                   ELSE
162900970526     C                   Z-ADD     I             Z
163000970526     C                   END
163100970526     C*
163200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
163300970526     C                   ADD       1             OPZGRU
163400970526     C     OPZGRU        IFGT      Z
163500970526     C     OPZGRU        ORLT      1
163600970526     C                   Z-ADD     1             OPZGRU
163700970526     C                   END
163800970526     C*
163900970526     C* PRIMO GRUPPO DI 8 OPZIONI
164000970526     C     OPZGRU        IFEQ      1
164100970526     C                   MOVEL     OPV(1)        OPZ001
164200970526     C                   MOVEL     OPV(2)        OPZ002
164300970526     C                   MOVEL     OPV(3)        OPZ003
164400970526     C                   MOVEL     OPV(4)        OPZ004
164500970526     C                   MOVEL     OPV(5)        OPZ005
164600970526     C                   MOVEL     OPV(6)        OPZ006
164700970526     C                   MOVEL     OPV(7)        OPZ007
164800970526     C                   MOVEL     OPV(8)        OPZ008
164900970526     C                   END
165000970526     C*
165100970526     C* SECONDO GRUPPO DI 8 OPZIONI
165200970526     C     OPZGRU        IFEQ      2
165300970526     C                   MOVEL     OPV(9)        OPZ001
165400970526     C                   MOVEL     OPV(10)       OPZ002
165500970526     C                   MOVEL     OPV(11)       OPZ003
165600970526     C                   MOVEL     OPV(12)       OPZ004
165700970526     C                   MOVEL     OPV(13)       OPZ005
165800970526     C                   MOVEL     OPV(14)       OPZ006
165900970526     C                   MOVEL     OPV(15)       OPZ007
166000970526     C                   MOVEL     OPV(16)       OPZ008
166100970526     C                   END
166200970526     C*
166300970526     C* TERZO GRUPPO DI 8 OPZIONI
166400970526     C     OPZGRU        IFEQ      3
166500970526     C                   MOVEL     OPV(17)       OPZ001
166600970526     C                   MOVEL     OPV(18)       OPZ002
166700970526     C                   MOVEL     OPV(19)       OPZ003
166800970526     C                   MOVEL     OPV(20)       OPZ004
166900970526     C                   MOVEL     OPV(21)       OPZ005
167000970526     C                   MOVEL     OPV(22)       OPZ006
167100970526     C                   MOVEL     OPV(23)       OPZ007
167200970526     C                   MOVEL     OPV(24)       OPZ008
167300970526     C                   END
167400970526     C*
167500970526     C                   ENDSR
167600970526     C*------------------------------------------------------------------------*
167700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
167800970526     C*------------------------------------------------------------------------*
167900970526     C     OPZCON        BEGSR
168000970526     C*
168100970526     C* PULISCO CAMPI DI LAVORO
168200970526     C                   Z-ADD     0             I                 5 0
168300970526     C                   Z-ADD     0             W                 5 0
168400970526     C*
168500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
168600970526     C     1             DO        24            I
168700970526     C                   MOVEL     OPD(I)        OPDRIG
168800970526     C*
168900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
169000970526     C     OPZKEY        IFEQ      OPDOPZ
169100970526     C     OPZKEY        OREQ      OPDOP1
169200970526     C*
169300970526     C     OPDATT        IFEQ      'S'
169400970526     C                   MOVEL     OPDOPZ        OPZKEY
169500970526     C                   Z-ADD     I             W
169600970526     C                   END
169700970526     C                   END
169800970526     C*
169900970526     C                   END                                                    DO WHILE
170000970526     C*
170100970526     C* OPZIONE NON ATTIVA (ERRORE)
170200970526     C     W             IFEQ      0
170300970526     C*
170400970526     C* OPZIONE NON OK
170500970526     C                   Z-ADD     1             OPZOK
170600970526     C*
170700970526     C* EMISSIONE MESSAGGIO DI ERRORE
170800970526     C                   MOVEL     MSG(1)        DSMSMS
170900970526     C                   ELSE
171000970526     C*
171100970526     C* OPZIONE ATTIVA (PROCEDI ...)
171200970526     C*
171300021211     C                   MOVEL     *BLANKS       PMSG
171400030509     C     OPZKEY        IFEQ      ' 2'
171500030509     C                   EXSR      O02GES
171600030509     C                   END
171700031210     C     OPZKEY        IFEQ      ' 4'
171800031210     C                   EXSR      O04GES
171900031210     C                   END
172000970526     C     OPZKEY        IFEQ      ' 5'
172100970526     C                   EXSR      O05GES
172200970526     C                   END
172300031210     C     OPZKEY        IFEQ      ' 6'
172400031210     C                   EXSR      O06GES
172500031210     C                   END
172600071001     C     OPZKEY        IFEQ      '11'
172700071001     C                   EXSR      O11GES
172800071001     C                   END
172900031211     C     OPZKEY        IFEQ      '15'
173000031211     C                   EXSR      O15GES
173100031211     C                   END
173200021211     C                   MOVEL     PMSG          DSMSMS
173300021211     C                   END                                                    OPZ.NON ATTIVA
173400970526     C*
173500970526     C                   ENDSR
173600970526     C*------------------------------------------------------------------------*
173700970526     C* NOTE COSTRUZIONE SCHIERE
173800970526     C*------------------------------------------------------------------------*
173900970526     C*
174000970526     C* -MSG-MESSAGGI
174100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
174200970526     C* (NON TOCCARE MSG,1 E MSG,2)
174300970526     C*
174400970526     C* -CMD-COMANDI
174500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
174600970526     C*
174700970526     C* -OPD-OPZIONI
174800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
174900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
175000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
175100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
175200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
175300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
175400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
175500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
175600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
175700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
175800970526     C*
175900970526     C* -FUD-FUNZIONI
176000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
176100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
176200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
176300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
176400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
176500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
176600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
176700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
176800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
176900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
177000991108     C*
177100991115     C* -STS-CODICI STATI DEI RECORD DI LOG
177200991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
177300991110     C* ================> IMPORTANTE <=================
177400991115     C* per codici di errore in -STS- indicare in -DES- sempre
177500991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
177600991110     C* ERR
177700991108     C*
177800970526     O*------------------------------------------------------------------------*
177900970526** DEC - DECODIFICHE
178000970526    GESTIONE                                                     1
178100000407 INTERROGAZIONE                                                  2
1782000305093
1783009705264
1784009705265
178500970526** MSG - MESSAGGI
178600100219TRBMM3R- Opzione non gestita.                                                  1
178700100219TRBMM3R- Tasto funzionale non gestito.                                         2
178800100219TRBMM3R- Non abilitato all inserimento in filiale.                             3
178900100219TRBMM3R- Utente non abilitato all'opzione richiesta.                           4
179000100219TRBMM3R- File di spool non trovato o inesistente, verificare.                  5
179100100219TRBMM3R- Specificare almeno 5 caratteri per la ricerca.                        6
179200100219TRBMM3R- Operazione non consentita su log ancora da elaborare.                 7
179300100219TRBMM3R- 8                                                                     8
179400100219TRBMM3R- 9                                                                     9
179500100219TRBMM3R- 10                                                                   10
179600100219TRBMM3R- 11                                                                   11
179700100219TRBMM3R- 12                                                                   12
179800100219TRBMM3R- 13                                                                   13
179900100219TRBMM3R- 14                                                                   14
180000100219TRBMM3R- 15                                                                   15
180100100219TRBMM3R- 16                                                                   16
180200100219TRBMM3R- 17                                                                   17
180300100219TRBMM3R- 18                                                                   18
180400100219TRBMM3R- 19                                                                   19
180500100219TRBMM3R- 20                                                                   20
180600970528** CMD - COMANDI CL
180700021211
180800021211
180900991110
181000970528
181100970528
181200970526** OPD - OPZIONI
181300030509                   -  -     (S/N)=N                        opz. 01¬
181400000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
181500030509                   -  -     (S/N)=N                        opz. 03¬
181600031210 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
181700000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
181800071001 6=Rilascio        -6 -     (S/N)=N                        opz. 06¬
181900021211                   -  -     (S/N)=N                        opz. 07¬
182000991110                   -  -     (S/N)=N                        opz. 08¬
182100991110                   -  -     (S/N)=N                        opz. 09¬
182200991110                   -  -     (S/N)=N                        opz. 10¬
18230007100111=Riesecuzione    -7 -     (S/N)=N                        opz. 11¬
182400970526                   -  -     (S/N)=N                        opz. 12¬
182500970526                   -  -     (S/N)=N                        opz. 13¬
182600000407                   -  -     (S/N)=N                        opz. 14¬
18270003121115=Visualizza spool-15-     (S/N)=N                        opz. 15¬
182800970526                   -  -     (S/N)=N                        opz. 16¬
182900970526                   -  -     (S/N)=N                        opz. 17¬
183000970526                   -  -     (S/N)=N                        opz. 18¬
183100970526                   -  -     (S/N)=N                        opz. 19¬
183200970526                   -  -     (S/N)=N                        opz. 20¬
183300970526                   -  -     (S/N)=N                        opz. 21¬
183400970526                   -  -     (S/N)=N                        opz. 22¬
183500970526                   -  -     (S/N)=N                        opz. 23¬
183600000407                   -  -     (S/N)=N                        opz. 25¬
183700970526** FUD - FUNZIONI
183800970526                   -        (S/N)=N                        funz.01¬
183900970526                   -        (S/N)=N                        funz.02¬
184000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
184100970526                   -        (S/N)=N                        funz.04¬
184200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
184300030509                   -        (S/N)=N                        funz.06¬
184400970526                   -        (S/N)=N                        funz.07¬
184500970527                   -        (S/N)=N                        funz.08¬
184600030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
184700970527                   -        (S/N)=N                        funz.10¬
184800970526                   -        (S/N)=N                        funz.11¬
184900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
185000991116                   -        (S/N)=N                        funz.13¬
185100970526                   -        (S/N)=N                        funz.14¬
185200970526                   -        (S/N)=N                        funz.15¬
185300970526                   -        (S/N)=N                        funz.16¬
185400970526                   -        (S/N)=N                        funz.17¬
185500970526                   -        (S/N)=N                        funz.18¬
185600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
185700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
185800000407                   -        (S/N)=N                        funz.21¬
185900991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
186000991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
186100991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
