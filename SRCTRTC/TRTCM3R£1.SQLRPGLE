000100970526     H*------------------------------------------------------------------------*
000200030509     H* Gestione file di log INVIO SPOOL VIA E-MAIL (TIMAL00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030509     FTRTCM3D   CF   E             WORKSTN INFDS(DEVDS1)
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
009500120510     D Sav_kpjba       s                   like(kpjba)
009600021217     D*------------
009700040818     D* DS D PROCEDURA
009800040818     D*------------
009900040818     D TRTCM1DS      E DS
010000050224     D*------------------
010100050224     D* DS D PROCEDURA X REPERIMENTO/IMPOSTAZIONE STATO PROGRESSIVO LOG
010200050224     D*------------------
010300050224     D TRTCM9DS      E DS
010400141211     D*------------------
010500141211     D* PASSAGGIO PARAMETRI TRTCMBR
010600141211     D*------------------
010700141211     D ParamTCMB       DS                  INZ
010800141211     D  pIn_POP1                      3
010900141211     D  pIn_POP2                      1
011000141211     D  pIn_Prog                      7
011100141211     D  pIn_Mit                      40
011200141211     D  pOut_Msg                     80
011300030128     D*------------------
011400030128     D* DS CONTROLLO DATA (8)
011500030128     D*------------------
011600030128     D WLBDA8          DS                  INZ
011700030128     D  G08DAT                 1      8  0
011800030128     D  G08INV                 9     16  0
011900030128     D  G08ERR                17     17
012000030128     D  G08TGI                18     22  0
012100021211     D*------------------
012200030509     D* PASSAGGIO PARAMETRI
012300021211     D*------------------
012400021211     D PARAM           DS                  INZ
012500021211     D  POP1                   1      3
012600050111     D  POP2                   4      4
012700050111     D  PPRG                   5     11
012800050111     D  PPDA                  12     13
012900050111     D  PPA                   14     15
013000050111     D  PMSG                  16     95
013100050216     D*------------------
013200050216     D* VARIABILI D WRK
013300050216     D*------------------
013400050216     D wOBJM           S                   LIKE(V1DOBJM)
013500050216     D wDST            S                   LIKE(V1DDST)
013600050216     D wCM1VAR         S                   LIKE(§CM1VAR)
013700050216     D wCM1DST         S                   LIKE(§CM1DST)
013800050808     D wDataDA         S               D   DATFMT(*iso)
013900050216     D*------------------
014000050216     D* COSTANTI
014100050216     D*------------------
014200050216     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
014300050216     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
014400970526     C*------------------------------------------------------------------------*
014500970526     C* MAIN LINES
014600970526     C*------------------------------------------------------------------------*
014700100202
014800100202
014900100202     C*
015000100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
015100100202     C
015200100202     C/EXEC SQL
015300100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
015400100202     C/END-EXEC
015500100202     C*
015600970526     C*
015700970526     C* SE NON CI SONO ERRORI, PROSEGUE
015800970526IF  1C     ERRL00        IFNE      '1'
015900970526     C*
016000970526     C* CARICA IL SFILE
016100970526     C                   EXSR      POSSFL
016200970526     C*
016300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016700970526     C*
016800970526     C* REIMPOSTA ERRORE DI PROCERURA
016900970526     C                   MOVEL     '0'           ERRL00
017000970526     C*
017100050114     C* EMETTE IL SFILE
017200970526     C                   EXSR      OUTSFL
017300970526     C*
017400970526     C* GESTIONE TASTI FUNZIONALI
017500970526     C                   EXSR      FUNCON
017600970526     C*
017700970526E   2C                   ENDDO
017800970527     C*
017900970526E   1C                   ENDIF
018000991108     C*
018100970526     C* FINE PGM
018200970526     C                   SETON                                        LR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F03GES        BEGSR
018700970526     C*
018800970526     C* IMPOSTA "PREMUTO F03"
018900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300120510     C* F04GES - GESTIONE F04 -> Delete log con prg=0
019400970526     C*------------------------------------------------------------------------*
019500120510     C     F04GES        BEGSR
019600970526     C*
019700120510     C* salvo kpjba prima di passarla
019800120510     C                   MOVEL     KPJBA         Sav_kpjba
019900120510     C* chiamata a pgm apposito
020000120510     C* (ha il video che chiede i parametri e non gliene servono altri)
020100120510     C                   CALL      'TRTCMAR'
020200120510     C                   PARM                    KPJBA
020300120510     C* ripristino kpjba
020400120510     C                   EVAL      KPJBA =       Sav_kpjba
020500970526     C*
020600970526     C                   ENDSR
020700120510     C*------------------------------------------------------------------------*
020800120510     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
020900120510     C*------------------------------------------------------------------------*
021000120510     C     F05GES        BEGSR
021100120510     C*
021200120510     C                   EXSR      POSSFL                                       *RICARICA SFILE
021300120510     C*
021400120510     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600030509     C* F12GES - GESTIONE F12 -> RITORNO
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F12GES        BEGSR
021900970526     C*
022000970526     C* IMPOSTA "PREMUTO F12"
022100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022200970526     C*
022300970526     C                   ENDSR
022400970528     C*------------------------------------------------------------------------*
022500970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
022600970528     C*------------------------------------------------------------------------*
022700970528     C     F21GES        BEGSR
022800970612     C*
022900991117     C* NON GESTITA
023000970528     C*
023100970528     C                   ENDSR
023200970528     C*------------------------------------------------------------------------*
023300970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
023400970528     C*------------------------------------------------------------------------*
023500970528     C     F22GES        BEGSR
023600970528     C*
023700021211     C* NON GESTITA
023800970528     C*
023900970528     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
024200970526     C*------------------------------------------------------------------------*
024300970526     C     F23GES        BEGSR
024400970526     C*
024500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025000970526     C*------------------------------------------------------------------------*
025100970526     C     F24GES        BEGSR
025200970526     C*
025300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     RUPGES        BEGSR
026000970526     C*
026100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500991108     C* ENTGES - GESTIONE ENTER -> INVIO
026600970526     C*------------------------------------------------------------------------*
026700970526     C     ENTGES        BEGSR
026800000407     C*
026900000407     C                   MOVEL     '0'           ERRL00
027000991108     C*
027100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027200970526     C                   EXSR      NEWPOS
027300021211     C*
027400021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027600021211     C     SWPOSI        IFNE      0
027700021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
027800021211     C                   ELSE
027900021211     C     SFLULT        IFNE      0
028000021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
028100021211     C*
028200021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
028300021211     C     SWMODI        IFNE      0
028400021211     C                   EXSR      POSSFL
028500021211     C                   ENDIF
028600021211     C                   ENDIF
028700021211     C                   ENDIF
028800021211     C                   ENDIF
028900021211     C*
029000021211     C                   ENDSR
029100030509     C*------------------------------------------------------------------------*
029200030509     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
029300030509     C*------------------------------------------------------------------------*
029400030509     C     O02GES        BEGSR
029500030509     C*
029600050410     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE" (X ORA SOLO SE UTENTE NN EDPFG)
029700050410     C                   IF        KNMUS <> 'EDPFG'
029800050224     C                   IF        VIDSTS = '0' OR
029900050224     C                             VIDSTS = 'A' OR
030000050224     C                             VIDSTS = 'B'
030100050224     C                   MOVEL     '1'           ERRL00
030200050224     C                   EVAL      PMSG = MSG(7)
030300050224     C                   ENDIF
030400050410     C                   ENDIF
030500050224     C*
030600030509     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030700030509     C                   MOVEL     'O02'         POP1
030800030509     C                   MOVEL     VIDPROG       PPRG
030900041006     C                   MOVEL     VIDATT        PPDA
031000041006     C                   MOVEL     VIDTOTATT     PPA
031100030509     C                   EXSR      PASPAR
031200030509     C                   CALL      'TRTCM4R'
031300030509     C                   PARM                    KPJBA
031400031210     C                   MOVEL     KPJBU         PARAM
031500030509     C                   ENDIF
031600030509     C*
031700030509     C                   ENDSR
031800031210     C*------------------------------------------------------------------------*
031900031210     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZOINE RECORD/FILE
032000031210     C*------------------------------------------------------------------------*
032100031210     C     O04GES        BEGSR
032200050224     C*
032300050224     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE"
032400050224     C                   IF        VIDSTS = '0' OR
032500050224     C                             VIDSTS = 'A' OR
032600050224     C                             VIDSTS = 'B'
032700050224     C                   MOVEL     '1'           ERRL00
032800050224     C                   EVAL      PMSG = MSG(7)
032900050224     C                   ENDIF
033000031210     C*
033100031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033200031210     C                   MOVEL     'O04'         POP1
033300031210     C                   MOVEL     VIDPROG       PPRG
033400041006     C                   MOVEL     VIDATT        PPDA
033500041006     C                   MOVEL     VIDTOTATT     PPA
033600031210     C                   EXSR      PASPAR
033700031210     C                   CALL      'TRTCM4R'
033800031210     C                   PARM                    KPJBA
033900031210     C                   MOVEL     KPJBU         PARAM
034000031210     C                   ENDIF
034100031210     C*
034200031210     C                   ENDSR
034300991014     C*------------------------------------------------------------------------*
034400000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
034500970526     C*------------------------------------------------------------------------*
034600970526     C     O05GES        BEGSR
034700000407     C*
034800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034900021217     C                   MOVEL     'O05'         POP1
035000030509     C                   MOVEL     VIDPROG       PPRG
035100041006     C                   MOVEL     VIDATT        PPDA
035200041006     C                   MOVEL     VIDTOTATT     PPA
035300030509     C                   EXSR      PASPAR
035400030509     C                   CALL      'TRTCM4R'
035500000407     C                   PARM                    KPJBA
035600031210     C                   MOVEL     KPJBU         PARAM
035700021211     C                   ENDIF
035800000407     C*
035900970526     C                   ENDSR
036000031210     C*------------------------------------------------------------------------*
036100031210     C* O06GES - GESTIONE OPZIONE 6=RILASCIO RECORD
036200031210     C*------------------------------------------------------------------------*
036300050224     C     O06GES        BEGSR
036400050224     C*
036500050321     C* CONSENTO OPZIONE X TUTTI GLI STATI
036600050321     C*                  IF        VIDSTS = '0' OR
036700050321     C*                            VIDSTS = 'A' OR
036800050321     C*                            VIDSTS = 'B'
036900050321     C*                  MOVEL     '1'           ERRL00
037000050321     C*                  EVAL      PMSG = MSG(7)
037100050321     C*                  ENDIF
037200031210     C*
037300031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037400050224     C                   CLEAR                   TRTCM9DS
037500050224     C                   EVAL      §CM9TIP    = 'E'
037600050224     C                   EVAL      §CM9PROG   = VIDPROG
037700050224     C                   EVAL      §CM9STSIN  = '0'
037800050224     C                   EVAL      §CM9MSGIN  = *blanks
037900050224     C                   CALL      'TRTCM9R'
038000080904     C                   PARM                    TRTCM9DS
038100050224     C*                  MOVEL     'O06'         POP1
038200050224     C*                  MOVEL     VIDPROG       PPRG
038300050224     C*                  MOVEL     VIDATT        PPDA
038400050224     C*                  MOVEL     VIDTOTATT     PPA
038500050224     C*                  EXSR      PASPAR
038600050224     C*                  CALL      'TRTCM4R'
038700050224     C*                  PARM                    KPJBA
038800050224     C*                  MOVEL     KPJBU         PARAM
038900050224     C                   ENDIF
039000031210     C*
039100031210     C                   ENDSR
039200141210     C*------------------------------------------------------------------------*
039300141210     C* O10GES - GESTIONE OPZIONE 10=VISUALIZZAZIONE STATUS INVIO EMAIL
039400141210     C*------------------------------------------------------------------------*
039500141210     C     O10GES        BEGSR
039600141211     C*
039700141211     C* salvo kpjba prima di passarla
039800141211     C                   MOVEL     KPJBA         Sav_kpjba
039900141211     C*
040000141211     C                   CLEAR                   ParamTCMB
040100141211     C                   MOVEL     VIDPROG       pIn_Prog
040200141211     C                   MOVEL     VIDMIT        pIn_Mit
040300141211     C                   EVAL      Kpjbu = ParamTCMB
040400141210     C*
040500141210     C                   CALL      'TRTCMBR'
040600141211     C                   PARM                    KPJBA
040700141210     C*
040800141211     C* ripristino kpjba
040900141211     C                   EVAL      KPJBA =       Sav_kpjba
041000141211     C*
041100141210     C                   ENDSR
041200071001     C*------------------------------------------------------------------------*
041300071001     C* O11GES - GESTIONE OPZIONE 11=RIESECUZIONE SPOOL
041400071001     C*------------------------------------------------------------------------*
041500071001     C     O11GES        BEGSR
041600071001     C*
041700071001     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041800071001     C                   CALL      'TRTCM3C'
041900071001     C                   PARM                    VIDFNAM
042000071001     C                   PARM                    VIDJNAM
042100071001     C                   PARM                    VIDUNAM
042200071001     C                   PARM                    VIDJNUM
042300071001     C                   PARM                    VIDFNUM
042400071001     C                   PARM      *BLANKS       ESITO             1
042500071001     C                   PARM      'EMAILIN_N'   WOUTQ            10
042600071001     C*
042700071002     C                   IF        ESITO = '0'
042800071001     C                   MOVEL     'O04'         POP1
042900071001     C                   MOVEL     VIDPROG       PPRG
043000071001     C                   MOVEL     VIDATT        PPDA
043100071001     C                   MOVEL     VIDTOTATT     PPA
043200071001     C                   EXSR      PASPAR
043300071001     C                   CALL      'TRTCM4R'
043400071001     C                   PARM                    KPJBA
043500071001     C                   MOVEL     KPJBU         PARAM
043600071001     C                   ENDIF
043700071001     C                   ENDIF
043800071001     C*
043900071001     C                   ENDSR
044000031211     C*------------------------------------------------------------------------*
044100031211     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE SPOOL
044200031211     C*------------------------------------------------------------------------*
044300031211     C     O15GES        BEGSR
044400031211     C*
044500031211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044600031211     C                   MOVEL     'O15'         POP1
044700031211     C                   CALL      'TRTCM4C'
044800031211     C                   PARM                    POP1
044900031211     C                   PARM                    VIDFNAM
045000031211     C                   PARM                    VIDJNAM
045100031211     C                   PARM                    VIDUNAM
045200031211     C                   PARM                    VIDJNUM
045300031211     C                   PARM                    VIDFNUM
045400031211     C                   PARM                    ESITO             1
045500031211     C                   IF        ESITO <> '0'
045600031211     C                   EVAL      PMSG = MSG(5)
045700031211     C                   ENDIF
045800031211     C                   ENDIF
045900031211     C*
046000031211     C                   ENDSR
046100991028     C*------------------------------------------------------------------------*
046200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
046300991112     C*------------------------------------------------------------------------*
046400991112     C     PASPAR        BEGSR
046500991112     C*
046600000407     C                   MOVEL     PARAM         KPJBU
046700991112     C*
046800991112     C                   ENDSR
046900970526     C*------------------------------------------------------------------------*
047000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
047100970526     C*------------------------------------------------------------------------*
047200970526     C     NEWPOS        BEGSR
047300970526     C*
047400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
047500970526     C*         1= RICHIESTO POSIZIONAMENTO
047600970526     C                   Z-ADD     0             SWPOSI            7 0
047700970526     C*
047800991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
047900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048100970526     C                   Z-ADD     0             SWMODI            1 0
048200970530     C*
048300970530     C* REIMPOSTA INDICATORI DI ERRORE
048400050111     C                   SETOFF                                       868788
048500050111     C                   SETOFF                                       89
048600050111     C                   MOVEL     '0'           ERRPAR            1
048700050111     C*
048800050111     C* CONTROLLO SE RICHIESTE RICERCE X STRINGHE CONTENUTE (OGGETTO MAIL E DESTINATARIO MAIL)
048900050111     C* CHE SIANO STATI SPECIFICATO ALMENO 5 CARATTERI
049000050111     C                   IF        V1DOBJM <> *blanks
049100050111     C                   IF        %len(%trim(V1DOBJM)) < 5
049200050111     C                   SETON                                        86
049300050111     C                   MOVEL     '1'           ERRPAR
049400050111     C                   MOVEL     MSG(6)        DSMSMS
049500050111     C                   ENDIF
049600050111     C                   ENDIF
049700050111     C*
049800050111     C                   IF        V1DDST <> *blanks
049900050111     C                   IF        %len(%trim(V1DDST)) < 5
050000050111     C                   SETON                                        87
050100050111     C                   MOVEL     '1'           ERRPAR
050200050111     C                   MOVEL     MSG(6)        DSMSMS
050300050111     C                   ENDIF
050400050111     C                   ENDIF
050500050111     C*
050600050111     C* PROSEGUO CON VERIFICA PARAMETRI E GESTIONE FILTRI SELEZIONE SOLO SE NO ERRORI NEI PARAMETRI
050700050111     C                   IF        ERRPAR = '0'
050800040729     C*
050900040729     C* ESEGUO "FORZATURA" CONTROLLO CAMPO SELEZIONE X PROGRESSIVO UNIVOCO (ID PROG) SE = *zeros
051000040729     C                   IF        V1DPRG = *all'0'
051100040729     C                   EVAL      V1DPRG = *blanks
051200040729     C                   ENDIF
051300051026     C*
051400051026     C* SE P.O. = *ZEROS FORZO *BLANKS X CONSENTIRE LA CORRETTA "NN SELEZIONE"
051500051026     C                   IF        V1DPO = *zeros
051600051026     C                   EVAL      V1DPO = *blanks
051700051026     C                   ENDIF
051800050110     C*
051900050110     C* ESEGUO "FORZATURA" P.O. = P.O. UTENTE IN RICHIAMO DA "FILIALE" E P.O. NN INDICATO
052000050111     C   50              IF        V1DPO = *zeros  OR
052100050110     C                             V1DPO = *blanks
052200050110     C                   MOVEL     DUTPOU        V1DPO
052300050110     C                   ENDIF
052400970526     C*
052500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
052600030305     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
052700050111     C                   IF        V1DDAD  <> depdad  OR
052800050111     C                             V1DDAA  <> depdaa  OR
052900050111     C                             V1DPRG  <> depprg  OR
053000050111     C                             V1DSTS  <> depsts  OR
053100050111     C                             V1DSNA  <> depsna  OR
053200050111     C                             V1DUNA  <> depuna  OR
053300050111     C                             V1DUDT  <> depudt  OR
053400050111     C                             V1DPO   <> deppo   OR
053500050111     C                             V1DCOR  <> depcor  OR
053600050111     C                             V1DIDP  <> depidp  OR
053700050111     C                             V1DDST  <> depdst  OR
053800050111     C                             V1DOBJM <> depobjm
053900021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
054000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
054100030509     C                   Z-ADD     V1DDAD        depdad                         *DEPOSITI
054200030509     C                   MOVEL     V1DDAA        depdaa                         *DEPOSITI
054300030509     C                   MOVEL     V1DPRG        depprg                         *DEPOSITI
054400030509     C                   MOVEL     V1DSTS        depsts                         *DEPOSITI
054500030604     C                   MOVEL     V1DSNA        depsna                         *DEPOSITI
054600030604     C                   MOVEL     V1DUNA        depuna                         *DEPOSITI
054700030604     C                   MOVEL     V1DUDT        depudt                         *DEPOSITI
054800040818     C                   MOVEL     V1DPO         deppo                          *DEPOSITI
054900040818     C                   MOVEL     V1DCOR        depcor                         *DEPOSITI
055000040914     C                   MOVEL     V1DIDP        depidp                         *DEPOSITI
055100050111     C                   MOVEL     V1DOBJM       depobjm                        *DEPOSITI
055200050111     C                   MOVEL     V1DDST        depdst                         *DEPOSITI
055300021211     C                   ENDIF
055400021211     C                   ENDIF
055500970526     C*
055600050111     C                   ENDIF
055700050111     C*
055800970526     C                   ENDSR
055900970526     C*------------------------------------------------------------------------*
056000021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
056100970526     C*------------------------------------------------------------------------*
056200970528     C     SETFIL        BEGSR
056300050808     C*
056400050808     C* COSTRUISCO SUBITO LA DATA DA CUI INIZIARE LA LETTURA
056500050808     C                   EVAL      wDATA_A =
056600050808     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
056700050808     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
056800050808     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
056900050808     C                             '99.99.99.999'
057000970526     C*
057100030509     C* POSIZIONAMENTO DEL FILE
057200050808     C     WTIPRI        IFEQ      'PRG'
057300050808     C     V1DPRG        SETLL     timal01l                           99
057400050808     C                   ENDIF
057500050808     C*
057600050808     C     WTIPRI        IFEQ      'PO'
057700050808     C                   EVAL      KEYDAOR = wDATA_A
057800050808     C                   MOVEL     V1DPO         KEYPO
057900050808     C     KEYMAL5       SETLL     timal05l                           99
058000050808     C                   ENDIF
058100050808     C*
058200050808     C     WTIPRI        IFEQ      'STS'
058300050808     C                   EVAL      KEYDAOR = wDATA_A
058400050808     C                   MOVEL     V1DSTS        KEYSTS
058500050808     C     KEYMAL4       SETLL     timal04l                           99
058600021211     C                   ENDIF
058700050808     C*
058800050808     C     WTIPRI        IFEQ      'DAT'
058900050808     C                   MOVEL     *HIVAL        KEYDAOR
059000050808     C     KEYMAL2P      SETLL     timal02l                           99
059100050808     C                   ENDIF
059200980107     C*
059300021211     C* LETTURA RECORD
059400021211     C     *IN99         IFEQ      *OFF
059500970526     C                   EXSR      LETFIL
059600021211     C                   ENDIF
059700970526     C*
059800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
059900021211     C     *IN99         IFEQ      *ON
060000970526     C                   Z-ADD     1             SFLEOF
060100021211     C                   ENDIF
060200970526     C*
060300970526     C                   ENDSR
060400970526     C*------------------------------------------------------------------------*
060500970526     C* LETFIL - LETTURA RECORD
060600970526     C*------------------------------------------------------------------------*
060700970526     C     LETFIL        BEGSR
060800970526     C*
060900970526     C* LEGGE FINO A:
061000970526     C*     - FINE FILE
061100970526     C*     - TROVATO RECORD VALIDO
061200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
061300021211     C     *IN99         DOWEQ     *OFF
061400970526     C     WRECOK        ANDEQ     'N'
061500970526     C*
061600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
061700050808     C     WTIPRI        IFEQ      'PRG'
061800050808     C     V1DPRG        READE     timal01l                               99
061900050808     C                   ENDIF
062000050808     C     WTIPRI        IFEQ      'PO'
062100050808     C     V1DPO         READE     timal05l                               99
062200050808     C                   ENDIF
062300050808     C     WTIPRI        IFEQ      'STS'
062400050808     C     V1DSTS        READE     timal04l                               99
062500050808     C                   ENDIF
062600050808     C     WTIPRI        IFEQ      'DAT'
062700050808     C                   READ      timal02l                               99
062800021211     C                   ENDIF
062900970528     C*
063000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063100021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
063200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
063300021211     C                   ELSE
063400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
063500021211     C                   ENDIF
063600021211     C                   ENDDO
063700970526     C*
063800970526     C                   ENDSR
063900970526     C*------------------------------------------------------------------------*
064000021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
064100970526     C*------------------------------------------------------------------------*
064200970526     C     CHKREC        BEGSR
064300970526     C*
064400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
064500040818     C*
064600040818     C* MUOVE I DATI UTENTI ESTESI DELLO SPOOL NELLA APPOSITA DS D RIDEFINIZIONE
064700040818     C                   MOVEL     malDUEX       TRTCM1DS
064800030509     C*
064900030128     C* FILTRA SE RICHIESTO PERIODO DA->A
065000050216     C                   IF        WRECOK = 'S'
065100050110     C                   IF        V1DDADINV > *zeros OR
065200050110     C                             V1DDAAINV > *zeros
065300030509     C                   MOVEL     *all'0'       wDATA_DA         23
065400030509     C                   MOVEL     *all'0'       wDATA_A          23
065500030509     C                   EVAL      wDATA_DA =
065600050110     C                             %subst(%editw(V1DDADINV:'        '):1:4)+'-'+
065700050110     C                             %subst(%editw(V1DDADINV:'        '):5:2)+'-'+
065800050110     C                             %subst(%editw(V1DDADINV:'        '):7:2)+'-'+
065900030509     C                             '00.00.00.000'
066000030509     C                   EVAL      wDATA_A =
066100050110     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
066200050110     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
066300050110     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
066400030509     C                             '99.99.99.999'
066500030509     C                   IF        MALDAOR >= wDATA_DA AND MALDAOR <= wDATA_A
066600030509     C                   ELSE
066700030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
066800030509     C                   ENDIF
066900050808     C                   IF        MALDAOR < wDATA_DA
067000050808     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067100050808     C                   Z-ADD     1             SFLEOF
067200050808     C                   SETON                                        99
067300050808     C                   ENDIF
067400030509     C                   ENDIF
067500050216     C                   ENDIF
067600030509     C*
067700030509     C* FILTRA SE RICHIESTO UN PROGRESSIVO
067800050216     C                   IF        WRECOK = 'S'
067900030509     C                   IF        V1DPRG  <> *blanks
068000030509     C                   IF        MALPROG <> V1DPRG
068100030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
068200030509     C                   ENDIF
068300030509     C                   ENDIF
068400050216     C                   ENDIF
068500030509     C*
068600030509     C* FILTRA SE RICHIESTO UNO STATO
068700050216     C                   IF        WRECOK = 'S'
068800030509     C                   IF        V1DSTS <> *blanks
068900050422     C                   IF        V1DSTS = '0'
069000050422     C                   IF        MALSTS <> '0' AND MALSTS <> 'X'
069100050422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069200050422     C                   ENDIF
069300050422     C                   ELSE
069400030509     C                   IF        MALSTS <> V1DSTS
069500030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069600030509     C                   ENDIF
069700050422     C                   ENDIF
069800030509     C                   ENDIF
069900050216     C                   ENDIF
070000030604     C*
070100030604     C* FILTRA SE RICHIESTO UNO SPOOL NAME
070200050216     C                   IF        WRECOK = 'S'
070300030604     C                   IF        V1DSNA  <> *blanks
070400030604     C                   IF        MALFNAM <> V1DSNA
070500030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070600030604     C                   ENDIF
070700030604     C                   ENDIF
070800050216     C                   ENDIF
070900030604     C*
071000030604     C* FILTRA SE RICHIESTO UNO USER NAME
071100050216     C                   IF        WRECOK = 'S'
071200030604     C                   IF        V1DUNA  <> *blanks
071300030604     C                   IF        MALUNAM <> V1DUNA
071400030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
071500030604     C                   ENDIF
071600030604     C                   ENDIF
071700050216     C                   ENDIF
071800030604     C*
071900030604     C* FILTRA SE RICHIESTO UNO USER DATA
072000050216     C                   IF        WRECOK = 'S'
072100030604     C                   IF        V1DUDT  <> *blanks
072200030604     C                   IF        MALUDTA <> V1DUDT
072300030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072400030604     C                   ENDIF
072500030604     C                   ENDIF
072600050216     C                   ENDIF
072700040818     C*
072800040818     C* FILTRA SE RICHIESTO UN P.O.
072900050216     C                   IF        WRECOK = 'S'
073000040818     C                   IF        V1DPO = *zeros
073100040818     C                   EVAL      V1DPO = *blanks
073200040818     C                   ENDIF
073300040818     C                   IF        V1DPO  <> *blanks
073400040818     C                   IF        §CM1PO <> V1DPO
073500040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
073600040818     C                   ENDIF
073700040818     C                   ENDIF
073800050216     C                   ENDIF
073900040818     C*
074000040818     C* FILTRA SE RICHIESTA UNA REGOLA
074100050216     C                   IF        WRECOK = 'S'
074200040818     C                   IF        V1DCOR   <> *blanks
074300040818     C                   IF        §CM1TIPS <> V1DCOR
074400040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074500040818     C                   ENDIF
074600040818     C                   ENDIF
074700050216     C                   ENDIF
074800040914     C*
074900040914     C* FILTRA SE RICHIESTO UN PROCESSO
075000050216     C                   IF        WRECOK = 'S'
075100040914     C                   IF        V1DIDP <> *blanks
075200040914     C                   IF        MALIDP <> V1DIDP
075300040914     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075400040914     C                   ENDIF
075500040914     C                   ENDIF
075600050216     C                   ENDIF
075700050111     C*
075800050111     C* FILTRA SE RICHIESTO UN OGGETTO MAIL
075900050216     C                   IF        WRECOK = 'S'
076000050216     C                   IF        V1DOBJM <> *blanks
076100050216     C     minu:maiu     XLATE     V1DOBJM       wOBJM                          *Minus -> Maiuscolo
076200050216     C     minu:maiu     XLATE     §CM1VAR       wCM1VAR                        *Minus -> Maiuscolo
076300050216     C                   IF        %scan(%trim(wOBJM):wCM1VAR) > 0
076400050111     C                   ELSE
076500050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076600050111     C                   ENDIF
076700050111     C                   ENDIF
076800050216     C                   ENDIF
076900050111     C*
077000050111     C* FILTRA SE RICHIESTO UN DESTINATARIO MAIL
077100050216     C                   IF        WRECOK = 'S'
077200050111     C                   IF        V1DDST <> *blanks
077300050216     C     minu:maiu     XLATE     V1DDST        wDST                           *Minus -> Maiuscolo
077400050216     C     minu:maiu     XLATE     §CM1DST       wCM1DST                        *Minus -> Maiuscolo
077500050216     C                   IF        %scan(%trim(wDST):wCM1DST) > 0
077600050111     C                   ELSE
077700050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
077800050111     C                   ENDIF
077900050111     C                   ENDIF
078000050216     C                   ENDIF
078100991108     C*
078200970526     C                   ENDSR
078300970526     C*------------------------------------------------------------------------*
078400000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
078500970526     C*------------------------------------------------------------------------*
078600970526     C     IRESFL        BEGSR
078700970526     C*
078800000407     C                   MOVEL     *blanks       OPZKEY
078900040914     C                   MOVEL     malIDP        VIDIDP
079000041005     C                   MOVEL     malATT        VIDATT
079100041005     C                   MOVEL     malTOTATT     VIDTOTATT
079200050110     C                   EVAL      VIDDAT =
079300050110     C                             %subst(malDAOR:9:2)+'/'+
079400050110     C                             %subst(malDAOR:6:2)+'/'+
079500050110     C                             %subst(malDAOR:1:4)
079600030604     C                   MOVEL     malUDTA       VIDUDTA
079700030509     C                   MOVEL     malPROG       VIDPROG
079800030509     C                   MOVEL     malFNAM       VIDFNAM
079900030509     C                   MOVEL     malJNAM       VIDJNAM
080000030509     C                   MOVEL     malUNAM       VIDUNAM
080100030509     C                   MOVEL     malJNUM       VIDJNUM
080200030509     C                   MOVEL     malFNUM       VIDFNUM
080300030918     C                   Z-ADD     malTPAG       VIDTPAG
080400050418     C                   IF        malSTS = 'X'
080500050418     C                   MOVEL     '0'           VIDSTS
080600050418     C                   ELSE
080700040818     C                   MOVEL     malSTS        VIDSTS
080800050418     C                   ENDIF
080900040818     C                   MOVEL     malDUEX       TRTCM1DS
081000040818     C                   MOVEL     §CM1TIPS      VIDCOR
081100040818     C                   MOVEL     §CM1PO        VIDPO
081200050110     C                   MOVEL     §CM1DST       VIDDST
081300141211     C                   EVAL      VIDMit = %trim(§CM1MITT) + '@brt.it'
081400030509     C*
081500040818     C* DECODIFICHE
081600040818     C                   EVAL      VIDSTSD = *blanks
081700040818     C                   IF        VIDSTS = 'H'
081800040818     C                   EVAL      VIDSTSD = 'Hld'
081900040818     C                   ENDIF
082000040818     C                   IF        VIDSTS = '1'
082100040818     C                   EVAL      VIDSTSD = 'OK'
082200040818     C                   ENDIF
082300040818     C                   IF        VIDSTS = '2'
082400040818     C                   EVAL      VIDSTSD = 'Err'
082500040818     C                   ENDIF
082600040818     C                   IF        VIDSTS = '3'
082700040818     C                   EVAL      VIDSTSD = 'Aut'
082800040818     C                   ENDIF
082900040818     C*
083000970526     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
083300970526     C*------------------------------------------------------------------------*
083400970526     C     CHKPAR        BEGSR
083500021218     C*
083600021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
083700030509     C                   MOVEL     '0'           W_ERR             1
083800050808     C*
083900050808     C* VERIFICO X QUALE VISTA LOGICA CONVIENE LEGGERE IN RELAZIONE AI PARAMETRI DI SELEZIONE
084000050808     C                   SELECT
084100050808     C                   WHEN      V1DPRG <> *blanks
084200050808     C                   EVAL      WTIPRI = 'PRG'
084300050808     C                   WHEN      V1DSTS = '2'
084400050808     C                   EVAL      WTIPRI = 'STS'
084500050808     C                   WHEN      V1DSTS = '3'
084600050808     C                   EVAL      WTIPRI = 'STS'
084700050808     C                   WHEN      V1DSTS = 'A'
084800050808     C                   EVAL      WTIPRI = 'STS'
084900050808     C                   WHEN      V1DSTS = 'B'
085000050808     C                   EVAL      WTIPRI = 'STS'
085100050808     C                   WHEN      V1DPO  <> *blanks
085200050808     C                   EVAL      WTIPRI = 'PO'
085300050808     C                   OTHER
085400050808     C                   MOVEL     'DAT'         WTIPRI            3
085500050808     C                   ENDSL
085600030128     C*
085700030128     C* CONTROLLO VALIDITA PERIODO DA
085800030128     C                   IF        W_ERR = '0'
085900050110     C                   Z-ADD     *ZEROS        V1DDADINV         8 0
086000030509     C                   IF        V1DDAD > *zeros
086100030509     C                   Z-ADD     V1DDAD        DAW8
086200030128     C                   EXSR      CHKDA8
086300030128     C                   IF        G08ERR = '1'
086400030128     C                   EVAL      W_ERR = '1'
086500030128     C                   SETON                                        88
086600050110     C                   ELSE
086700050110     C                   Z-ADD     WDA8          V1DDADINV
086800050110     C                   Z-ADD     DAW8          V1DDAD
086900030128     C                   ENDIF
087000030128     C                   ENDIF
087100030128     C                   ENDIF
087200030128     C*
087300030128     C* CONTROLLO VALIDITA PERIODO A
087400030128     C                   IF        W_ERR = '0'
087500050110     C                   Z-ADD     *ZEROS        V1DDAAINV         8 0
087600050110     C                   IF        V1DDAA  > *zeros
087700030509     C                   Z-ADD     V1DDAA        DAW8
087800030128     C                   EXSR      CHKDA8
087900030128     C                   IF        G08ERR = '1'
088000030128     C                   EVAL      W_ERR = '1'
088100030128     C                   SETON                                        89
088200050110     C                   ELSE
088300050110     C                   Z-ADD     WDA8          V1DDAAINV
088400050110     C                   Z-ADD     DAW8          V1DDAA
088500030128     C                   ENDIF
088600030128     C                   ENDIF
088700030128     C                   ENDIF
088800030128     C*
088900030128     C* CONTROLLO PERIODO DA <= PERIODO A
089000030128     C                   IF        W_ERR = '0'
089100050110     C                   IF        V1DDADINV > V1DDAAINV
089200030128     C                   EVAL      W_ERR = '1'
089300030128     C                   SETON                                        8889
089400030128     C                   ENDIF
089500030128     C                   ENDIF
089600000407     C*
089700970526     C                   ENDSR
089800970526     C*------------------------------------------------------------------------*
089900970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
090000970526     C*------------------------------------------------------------------------*
090100970526     C     CHKFOP        BEGSR
090200991112     C*
090300991112     C* OPZIONI
090400970626     C*
090500000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
090600021211     C     DEPOP0        IFEQ      'P01'
090700000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
090800000420     C                   MOVEL     'S'           OPDATT
090900000420     C                   MOVEL     OPDRIG        OPD(1)
091000021211     C                   ELSE
091100000420     C* OPZIONI
091200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
091300970626     C                   MOVEL     'S'           OPDATT
091400970626     C                   MOVEL     OPDRIG        OPD(5)
091500141210     C                   MOVEL     OPD(10)       OPDRIG                         *VISUAL.STATUS INVIO
091600141210     C                   MOVEL     'S'           OPDATT
091700141210     C                   MOVEL     OPDRIG        OPD(10)
091800021211     C                   ENDIF
091900970626     C*
092000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
092100021211     C     DEPOP0        IFEQ      'P02'
092200991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
092300991112     C                   MOVEL     'S'           OPDATT
092400991112     C                   MOVEL     OPDRIG        OPD(5)
092500050111     C                   MOVEL     OPD(15)       OPDRIG                         *VISUALIZZA SPOOL
092600050111     C                   MOVEL     'S'           OPDATT
092700050111     C                   MOVEL     OPDRIG        OPD(15)
092800030509     C                   IF        %subst(KNMUS:1:3) = 'EDP'
092900030509     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
093000030509     C                   MOVEL     'S'           OPDATT
093100030509     C                   MOVEL     OPDRIG        OPD(2)
093200031210     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
093300031210     C                   MOVEL     'S'           OPDATT
093400031210     C                   MOVEL     OPDRIG        OPD(4)
093500031210     C                   MOVEL     OPD(6)        OPDRIG                         *RILASCIO
093600031210     C                   MOVEL     'S'           OPDATT
093700031210     C                   MOVEL     OPDRIG        OPD(6)
093800071001     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE SPOOL
093900071001     C                   MOVEL     'S'           OPDATT
094000071001     C                   MOVEL     OPDRIG        OPD(11)
094100120510     C                   MOVEL     FUD(04)       FUDRIG                         *DLT LOG CON PRG=0
094200120510     C                   MOVEL     'S'           FUDATT
094300120510     C                   MOVEL     FUDRIG        FUD(04)
094400021211     C                   ENDIF
094500030509     C                   ENDIF
094600970526     C*
094700970526     C* GESTIONE OPZIONI A VIDEO
094800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
094900970526     C                   EXSR      OPZGES
095000970526     C*
095100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
095200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
095300970526     C                   EXSR      FUNGES
095400970526     C*
095500970526     C                   ENDSR
095600020917     C*--------------------------------------------------------------------------------------------*
095700020917     C* REPERISCE I DATI UTENTE
095800020917     C*--------------------------------------------------------------------------------------------*
095900020917     C     REPDATIUTE    BEGSR
096000020917     C*
096100020917     C* INIZIALIZZA VARIABILI DI WRK
096200020917     C                   CLEAR                   TIBS34DS
096300020917     C                   CLEAR                   AZUTEDS
096400020917     C                   CLEAR                   DDATIUTE
096500020917     C*
096600020917     C     *DTAARA       DEFINE    §azute        azuteds
096700020917     C     *DTAARA       DEFINE    §datiute      ddatiute
096800020917     C                   IN(E)     *DTAARA
096900021211     C                   IF        %Error
097000021211     C                   EVAL      I34Tla = 'L'
097100020917     C                   CALL      'TIBS34R'
097200020917     C                   PARM                    Tibs34Ds
097300020917     C                   IN        *DTAARA
097400021211     C                   ENDIF
097500020917     C*
097600020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
097700020917     C                   MOVEL(P)  RSUT          DSFIRS           20
097800020917     C*
097900020917     C                   ENDSR
098000030128     C*------------------------------------------------------------------------*
098100030128     C* CHKDA8 - CONTROLLA LA DATA (8)
098200030128     C*------------------------------------------------------------------------*
098300030128     C     CHKDA8        BEGSR
098400030128     C*
098500050110     C                   Z-ADD     DAW8          G08DAT
098600050110     C                   Z-ADD     *ZEROS        G08INV
098700050110     C                   MOVEL     '0'           G08ERR
098800030128     C                   CALL      'XSRDA8'
098900030128     C                   PARM                    WLBDA8
099000050110     C                   Z-ADD     G08INV        WDA8
099100050110     C                   Z-ADD     G08DAT        DAW8
099200030128     C*
099300030128     C                   ENDSR
099400970526     C*------------------------------------------------------------------------*
099500970526     C* *INZSR - OPERAZIONI INIZIALI
099600970526     C*------------------------------------------------------------------------*
099700970526     C     *INZSR        BEGSR
099800970526     C*---
099900970526     C* RICEVIMENTO PARAMETRI
100000970526     C*---
100100970526     C     *ENTRY        PLIST
100200970612     C                   PARM                    KPJBA
100300021211     C                   MOVEL     KPJBU         PARAM
100400030509     C                   MOVEL     POP1          DEPOP0            3
100500050110     C*
100600050110     C* REPERIMENTO DATI UTENTE
100700050110     C                   EXSR      REPDATIUTE
100800050808     C*
100900050808     C* CALCOLA LA DATA CORRENTE
101000050808     C                   time                    wn14             14 0
101100050808     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
101200050808     C                   z-add     wn8           g08dat
101300050808     C                   z-add     *zeros        g08inv
101400050808     C                   movel     '0'           g08err
101500050808     C                   call      'XSRDA8'
101600050808     C                   parm                    wlbda8
101700050808     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
101800030509     C*
101900030509     C* A SECONDA DELL'OPZOINE DI ENTRATA IMPOSTO LA MODALITA' A VIDEO
102000050110     C                   SETOFF                                       50
102100050111     C                   MOVEL     'S'           POP2                           * SEDE
102200030509     C                   IF        DEPOP0 = 'P05'
102300030509     C                   EVAL      VIDING = DEC(2)
102400050110     C                   MOVEL     DUTPOU        V1DPO
102500050111     C                   MOVEL     DUTPOU        deppo
102600050110     C                   SETON                                        50
102700050111     C                   MOVEL     'F'           POP2                           * FILIALE
102800030509     C                   ENDIF
102900030509     C                   IF        DEPOP0 = 'P02'
103000030509     C                   EVAL      VIDING = DEC(1)
103100030509     C                   ENDIF
103200030604     C*
103300030604     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
103400050808     C     *iso          move      datcor        wDataDA
103500050808     C                   subdur    30:*d         wDataDA
103600050808     C                   move      wDataDA       V1DDAD
103700050808     C                   z-add     *zeros        g08dat
103800050808     C                   z-add     V1DDAD        g08inv
103900050808     C                   movel     '3'           g08err
104000050808     C                   call      'XSRDA8'
104100050808     C                   parm                    wlbda8
104200050808     C                   z-add     g08dat        V1DDAD
104300050808     C                   EVAL      V1DDAA = 31122099
104400060731     C                   EVAL      depdad = V1DDAD
104500030604     C                   EVAL      depdaa = V1DDAA
104600970526     C*
104700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
104800970526     C     XMSGPA        PLIST
104900970526     C                   PARM                    DSMSMS
105000970526     C                   PARM                    DSMSPG
105100970526     C                   PARM                    DSMSKE
105200970526     C                   PARM                    DSMSER
105300970526     C*
105400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
105500970526     C                   CALL      'XKEY'
105600970526     C                   PARM                    DSKEY
105700970526     C*---
105800970526     C* VARIABILI NON RIFERITE AL DATA BASE
105900970526     C*---
106000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
106100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
106200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
106300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
106400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
106500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
106600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
106700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
106800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
106900970526     C*---
107000030509     C     *LIKE         DEFINE    V1DDAD        DAW8
107100030509     C     *LIKE         DEFINE    V1DDAD        WDA8
107200030509     C     *LIKE         DEFINE    V1DDAD        depdad
107300030509     C     *LIKE         DEFINE    V1DDAA        depdaa
107400030509     C     *LIKE         DEFINE    V1DSTS        depsts
107500040818     C     *LIKE         DEFINE    V1DPO         deppo
107600040818     C     *LIKE         DEFINE    V1DCOR        depcor
107700030509     C     *LIKE         DEFINE    V1DPRG        depprg
107800030604     C     *LIKE         DEFINE    V1DSNA        depsna
107900030604     C     *LIKE         DEFINE    V1DUNA        depuna
108000030604     C     *LIKE         DEFINE    V1DUDT        depudt
108100040914     C     *LIKE         DEFINE    V1DIDP        depidp
108200050111     C     *LIKE         DEFINE    V1DDST        depdst
108300050111     C     *LIKE         DEFINE    V1DOBJM       depobjm
108400030509     C     *LIKE         DEFINE    MALDAOR       KEYDAOR
108500050808     C     *LIKE         DEFINE    MALSTS        KEYSTS
108600050808     C     *LIKE         DEFINE    §CM1PO        KEYPO
108700021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
108800991108     C*---
108900970526     C* VARIABILI DI CONTROLLO
109000970526     C*---
109100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
109200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
109300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
109400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
109500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
109600050110     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
109700970526     C*
109800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
109900030509     C                   MOVEL     'TRTCM3R'     DSMSPG
110000030509     C                   MOVEL     'TRTCM3R'     MSGQUE
110100970526     C                   MOVEL     *BLANKS       MSGKEY
110200970526     C                   MOVEL     *BLANKS       DSMSMS
110300970526     C                   MOVEL     *BLANKS       DSMSKE
110400970526     C                   Z-ADD     *ZEROS        DSMSER
110500970526     C*---
110600970526     C* CHIAVI DI LETTURA
110700970526     C*---
110800050808     C* LETTURA timal02l - PARZIALE
110900050808     C     KEYMAL2P      KLIST
111000050808     C                   KFLD                    KEYDAOR
111100050808     C* LETTURA timal04l - COMPLETA
111200050808     C     KEYMAL4       KLIST
111300050808     C                   KFLD                    KEYSTS
111400050808     C                   KFLD                    KEYDAOR
111500050808     C* LETTURA timal05l - COMPLETA
111600050808     C     KEYMAL5       KLIST
111700050808     C                   KFLD                    KEYPO
111800050808     C                   KFLD                    KEYDAOR
111900970526     C*
112000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
112100970526     C                   EXSR      CHKPAR
112200991115     C*
112300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
112400970526     C                   EXSR      CHKFOP
112500970526     C*
112600970526     C                   ENDSR
112700970526     C*------------------------------------------------------------------------*
112800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
112900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
113100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
113200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
113300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113500970526     C*------------------------------------------------------------------------*
113600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
113700970526     C*------------------------------------------------------------------------*
113800970526     C     POSSFL        BEGSR
113900970526     C*
114000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
114100970526     C                   EXSR      CLRSFL
114200970526     C*
114300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
114400970526     C                   EXSR      SETFIL
114500970526     C*
114600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
114700970526     C     SFLEOF        IFEQ      0
114800970526     C                   EXSR      CARSFL
114900030305     C                   ENDIF
115000970526     C*
115100970526     C                   ENDSR
115200970526     C*------------------------------------------------------------------------*
115300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
115400970526     C*------------------------------------------------------------------------*
115500970526     C     ENTATT        BEGSR
115600970526     C*
115700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
115800970526     C                   Z-ADD     0             OPZOK             1 0
115900970526     C*
116000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
116100970526     C                   Z-ADD     0             SFLVAR            5 0
116200970526     C*
116300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
116400970526     C                   Z-ADD     0             SFLERR
116500970526     C*
116600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
116700970526     C                   Z-ADD     SFLULT        WFLULT
116800970526     C*
116900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
117000030509     C                   READC     TCM3D05                                98
117100970526     C     *IN98         DOWEQ     '0'
117200970526     C*
117300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
117400970526     C                   ADD       1             SFLVAR
117500970526     C*
117600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
117700970526     C     OPZOK         IFEQ      0
117800970526     C*
117900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
118000970526     C     OPZKEY        IFNE      *BLANKS
118100970526     C                   EXSR      OPZCON
118200970526     C                   END
118300970526     C*
118400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
118500970526     C*            2. PULIZIA SCELTA IN SFL
118600970526     C     OPZOK         IFEQ      0
118700970526     C                   MOVEL     '0'           *IN41
118800970526     C                   MOVEL     *BLANKS       OPZKEY
118900970526     C                   ELSE
119000970526     C*
119100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
119200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
119300970526     C                   MOVEL     '1'           *IN41
119400970526     C                   Z-ADD     SFLULT        SFLERR
119500970526     C                   END
119600970526     C                   ELSE
119700970526     C*
119800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
119900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
120000970526     C                   MOVEL     '1'           *IN41
120100970526     C                   END                                                    OPZOK=0
120200970526     C*
120300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
120400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
120500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
120600970526     C     OPZOK         IFNE      0
120700970526     C                   Z-ADD     SFLERR        SFLNBR
120800970526     C                   ELSE
120900970526     C                   Z-ADD     SFLULT        SFLNBR
121000970526     C                   END
121100970526     C*
121200970526     C* AGGIORNO RECORD SFL
121300030509     C                   UPDATE    TCM3D05
121400970526     C*
121500030509     C                   READC     TCM3D05                                98
121600970526     C                   END                                                    DO WHILE
121700970526     C*
121800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
121900970526     C                   Z-ADD     WFLULT        SFLULT
122000970526     C*
122100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
122200970526     C     SFLVAR        IFEQ      0
122300970526     C                   END
122400991112     C*
122500970526     C                   ENDSR
122600970526     C*------------------------------------------------------------------------*
122700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
122800970526     C*------------------------------------------------------------------------*
122900970526     C     CLRSFL        BEGSR
123000970526     C*
123100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
123200970526     C* (0=NON FINE FILE  1=FINE FILE)
123300970526     C                   Z-ADD     0             SFLEOF
123400970526     C*
123500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
123600970526     C                   Z-ADD     0             SFLULT
123700970526     C*
123800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
123900970526     C                   MOVEL     '1'           *IN40
124000030509     C                   WRITE     TCM3D06
124100970526     C                   MOVEL     '0'           *IN40
124200970526     C*
124300970526     C                   ENDSR
124400970526     C*------------------------------------------------------------------------*
124500970526     C* CARSFL - CARICAMENTO SFL
124600970526     C*------------------------------------------------------------------------*
124700970526     C     CARSFL        BEGSR
124800970526     C*
124900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
125000970526     C     SFLULT        IFEQ      0
125100970526     C                   EXSR      SETFIL
125200970526     C                   END
125300970526     C*
125400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
125500970526     C                   Z-ADD     SFLULT        WFLULT
125600970526     C*
125700970526     C* CICLO:
125800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
125900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
126000970526     C                   Z-ADD     1             SFLRIG
126100030509     C     SFLRIG        DOWLE     SFLPAG
126200030509     C     SFLEOF        ANDEQ     0
126300970526     C*
126400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
126500991116     C                   MOVEL     *BLANKS       OPZKEY
126600970526     C*
126700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
126800970526     C                   EXSR      IRESFL
126900970526     C*
127000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
127100970526     C                   ADD       1             SFLULT
127200970526     C*
127300970526     C* SCRITTURA RECORD SFL
127400030509     C                   WRITE     TCM3D05
127500970526     C                   ADD       1             SFLRIG
127600970526     C*
127700970526     C* LETTURA FILE DA CARICARE
127800970526     C                   EXSR      LETFIL
127900970526     C                   END                                                    DO WHILE
128000970526     C*
128100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
128200970526     C     WFLULT        IFNE      SFLULT
128300970526     C                   Z-ADD     WFLULT        SFLNBR
128400970526     C                   ADD       1             SFLNBR
128500970526     C                   END
128600970526     C*
128700970526     C                   ENDSR
128800970526     C*------------------------------------------------------------------------*
128900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
129000970526     C*------------------------------------------------------------------------*
129100970526     C     OUTSFL        BEGSR
129200970526     C*
129300970526     C* WRITE RECORD TESTATA VIDEO
129400030509     C                   WRITE     TCM3D01
129500970526     C*
129600970526     C* VISUALIZZAZIONE MESSAGGIO
129700970526     C                   EXSR      SNDMSG
129800970526     C*
129900970526     C* WRITE RECORD OPZIONI VIDEO
130000030509     C                   WRITE     TCM3D02
130100970526     C*
130200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
130300030509     C                   WRITE     TCM3D03
130400970526     C*
130500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
130600030509     C                   WRITE     TCM3D04
130700970526     C*
130800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
130900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
131000970526     C     SFLEOF        IFEQ      0
131100970526     C                   MOVEL     '0'           *IN42
131200970526     C                   ELSE
131300970526     C*
131400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
131500970526     C                   MOVEL     '1'           *IN42
131600970526     C                   END
131700970526     C*
131800970526     C* WRITE RECORD TASTI FUNZIONALI
131900030509     C                   WRITE     TCM3D09
132000970526     C*
132100970526     C* CASO DI SFL VUOTO
132200970526     C     SFLULT        IFLE      0
132300970526     C*
132400970526     C*    - WRITE RECORD VUOTO
132500030509     C                   WRITE     TCM3D0A
132600030509     C                   READ      TCM3D0A                                99
132700970526     C*
132800970526     C                   ELSE
132900970526     C*
133000970526     C* CASO DI SFL CON RECORDS
133100970526     C*
133200970526     C*    - WRITE RECORD CONTROLLO SFL
133300030509     C                   WRITE     TCM3D06
133400030509     C                   READ      TCM3D06                                99
133500970526     C                   END
133600970526     C*
133700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
133800030509     C                   READ      TCM3D03                                99
133900970526     C*
134000970526     C                   ENDSR
134100970526     C*------------------------------------------------------------------------*
134200970526     C* SNDMSG - MANDA MESSAGGIO
134300970526     C*------------------------------------------------------------------------*
134400970526     C     SNDMSG        BEGSR
134500970526     C*
134600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
134700970526     C                   MOVEL     '1'           *IN40
134800030509     C                   WRITE     TCM3DMC
134900970526     C                   MOVEL     '0'           *IN40
135000970526     C*
135100970526     C* VISUALIZZAZIONE MESSAGGIO
135200970526     C                   Z-ADD     0             MSGULT
135300970526     C                   CALL      'XMSG'        XMSGPA
135400970526     C     DSMSER        IFEQ      0
135500970526     C                   MOVEL     DSMSKE        MSGKEY
135600970526     C                   END
135700970526     C                   Z-ADD     1             MSGULT
135800030509     C                   WRITE     TCM3DMS                                      SFL MESS
135900030509     C                   WRITE     TCM3DMC                                      CONTR MESS
136000970526     C*
136100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
136200970526     C                   MOVEL     *BLANKS       DSMSMS
136300970526     C*
136400970526     C                   ENDSR
136500970526     C*------------------------------------------------------------------------*
136600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
136700970526     C*------------------------------------------------------------------------*
136800970526     C     FUNCON        BEGSR
136900970526     C*
137000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
137100970526     C                   EXSR      FUNVAR
137200970526     C*
137300970526     C* PULISCO CAMPI DI LAVORO
137400970526     C                   Z-ADD     0             I                 5 0
137500970526     C                   Z-ADD     0             W                 5 0
137600970526     C*
137700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
137800970526     C     FUNKEY        IFNE      *BLANKS
137900970526     C     1             DO        24            I
138000970526     C                   MOVEL     FUD(I)        FUDRIG
138100970526     C*
138200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
138300970526     C     FUNKEY        IFEQ      FUDFUN
138400970526     C     FUDATT        ANDEQ     'S'
138500970526     C                   Z-ADD     I             W
138600970526     C                   END
138700970526     C                   END                                                    DO WHILE
138800970526     C*
138900970526     C* FUNZIONE NON ATTIVA (ERRORE)
139000970526     C     W             IFEQ      0
139100970526     C*
139200970526     C* EMISSIONE MESSAGGIO DI ERRORE
139300970526     C                   MOVEL     MSG(2)        DSMSMS
139400970526     C                   END                                                    W=0
139500970526     C                   END                                                    FUNKEY NE *BLANKS
139600970526     C*
139700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
139800970526     C     W             IFNE      0
139900970526     C     FUNKEY        OREQ      *BLANKS
140000970526     C*
140100970526     C     FUNKEY        IFEQ      *BLANKS
140200970526     C                   EXSR      ENTGES
140300970526     C                   END
140400970526     C     FUNKEY        IFEQ      '03'
140500970526     C                   EXSR      F03GES
140600970526     C                   END
140700120510     C     FUNKEY        IFEQ      '04'
140800120510     C                   EXSR      F04GES
140900970526     C                   END
141000120510     C     FUNKEY        IFEQ      '05'
141100120510     C                   EXSR      F05GES
141200120510     C                   END
141300970526     C     FUNKEY        IFEQ      '12'
141400970526     C                   EXSR      F12GES
141500970526     C                   END
141600970528     C     FUNKEY        IFEQ      '21'
141700970528     C                   EXSR      F21GES
141800970528     C                   END
141900970528     C     FUNKEY        IFEQ      '22'
142000970528     C                   EXSR      F22GES
142100970528     C                   END
142200970526     C     FUNKEY        IFEQ      '23'
142300970526     C                   EXSR      F23GES
142400970526     C                   END
142500970526     C     FUNKEY        IFEQ      '24'
142600970526     C                   EXSR      F24GES
142700970526     C                   END
142800970526     C     FUNKEY        IFEQ      'RU'
142900970526     C                   EXSR      RUPGES
143000970526     C                   END
143100970526     C*
143200970526     C                   END
143300970526     C*
143400970526     C                   ENDSR
143500970526     C*------------------------------------------------------------------------*
143600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
143700970526     C*------------------------------------------------------------------------*
143800970526     C     FUNVAR        BEGSR
143900970526     C*
144000030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
144100970526     C     KEY           IFEQ      ENTER
144200970526     C                   MOVEL     *BLANKS       FUNKEY
144300970526     C                   END
144400970526     C     KEY           IFEQ      F1
144500970526     C                   MOVEL     '01'          FUNKEY
144600970526     C                   END
144700970526     C     KEY           IFEQ      F2
144800970526     C                   MOVEL     '02'          FUNKEY
144900970526     C                   END
145000970526     C     KEY           IFEQ      F3
145100970526     C                   MOVEL     '03'          FUNKEY
145200970526     C                   END
145300970526     C     KEY           IFEQ      F4
145400970526     C                   MOVEL     '04'          FUNKEY
145500970526     C                   END
145600970526     C     KEY           IFEQ      F5
145700970526     C                   MOVEL     '05'          FUNKEY
145800970526     C                   END
145900970526     C     KEY           IFEQ      F6
146000970526     C                   MOVEL     '06'          FUNKEY
146100970526     C                   END
146200970526     C     KEY           IFEQ      F7
146300970526     C                   MOVEL     '07'          FUNKEY
146400970526     C                   END
146500970526     C     KEY           IFEQ      F8
146600970526     C                   MOVEL     '08'          FUNKEY
146700970526     C                   END
146800970526     C     KEY           IFEQ      F9
146900970526     C                   MOVEL     '09'          FUNKEY
147000970526     C                   END
147100970526     C     KEY           IFEQ      F10
147200970526     C                   MOVEL     '10'          FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F11
147500970526     C                   MOVEL     '11'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F12
147800970526     C                   MOVEL     '12'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F13
148100970526     C                   MOVEL     '13'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F14
148400970526     C                   MOVEL     '14'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F15
148700970526     C                   MOVEL     '15'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F16
149000970526     C                   MOVEL     '16'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      F17
149300970526     C                   MOVEL     '17'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      F18
149600970526     C                   MOVEL     '18'          FUNKEY
149700970526     C                   END
149800970526     C     KEY           IFEQ      F19
149900970526     C                   MOVEL     '19'          FUNKEY
150000970526     C                   END
150100970526     C     KEY           IFEQ      F20
150200970526     C                   MOVEL     '20'          FUNKEY
150300970526     C                   END
150400970526     C     KEY           IFEQ      F21
150500970526     C                   MOVEL     '21'          FUNKEY
150600970526     C                   END
150700970526     C     KEY           IFEQ      F22
150800970526     C                   MOVEL     '22'          FUNKEY
150900970526     C                   END
151000970526     C     KEY           IFEQ      F23
151100970526     C                   MOVEL     '23'          FUNKEY
151200970526     C                   END
151300970526     C     KEY           IFEQ      F24
151400970526     C                   MOVEL     '24'          FUNKEY
151500970526     C                   END
151600970526     C     KEY           IFEQ      ROLLUP
151700970526     C                   MOVEL     'RU'          FUNKEY
151800970526     C                   END
151900970526     C     KEY           IFEQ      HELP
152000970526     C                   MOVEL     'HP'          FUNKEY
152100970526     C                   END
152200970526     C*
152300970526     C                   ENDSR
152400970526     C*------------------------------------------------------------------------*
152500970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
152600970526     C*------------------------------------------------------------------------*
152700970526     C     FUNGES        BEGSR
152800970526     C*
152900970526     C* PULISCO CAMPI DI LAVORO
153000970526     C                   Z-ADD     0             W                 5 0
153100970526     C*
153200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
153300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
153400970526     C     1             DO        24            I
153500970526     C                   MOVEL     FUD(I)        FUDRIG
153600970526     C*
153700970526     C* PULIZIA FUNZIONI PRECEDENTI
153800970526     C                   MOVEL     *BLANKS       FUV(I)
153900970526     C*
154000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
154100970526     C* (SALTA F23 E F24)
154200970526     C     FUDATT        IFEQ      'S'
154300970526     C     FUDFUN        ANDNE     '23'
154400970526     C     FUDFUN        ANDNE     '24'
154500970526     C                   ADD       1             W
154600970526     C                   MOVEL     FUDDES        FUV(W)
154700970526     C                   END
154800970526     C                   END                                                    DO WHILE
154900970526     C*
155000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
155100970526     C* ALTRIMENTI DISATTIVO F24
155200970526     C     1             DO        24            I
155300970526     C                   MOVEL     FUD(I)        FUDRIG
155400970526     C*
155500970526     C     FUDFUN        IFEQ      '24'
155600970526     C     W             IFGT      6
155700970526     C                   MOVEL     'S'           FUDATT
155800970526     C                   ELSE
155900970526     C                   MOVEL     *BLANKS       FUDATT
156000970526     C                   END                                                    W GT 8
156100970526     C                   MOVEL     FUDRIG        FUD(I)
156200970526     C                   END
156300970526     C                   END                                                    DO WHILE
156400970526     C*
156500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
156600970526     C     W             DIV       6             I
156700970526     C                   MVR                     Z
156800970526     C*
156900970526     C     Z             IFNE      0
157000970526     C     I             ADD       1             Z
157100970526     C                   ELSE
157200970526     C                   Z-ADD     I             Z
157300970526     C                   END
157400970526     C*
157500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
157600970526     C                   ADD       1             FUNGRU
157700970526     C     FUNGRU        IFGT      Z
157800970526     C     FUNGRU        ORLT      1
157900970526     C                   Z-ADD     1             FUNGRU
158000970526     C                   END
158100970526     C*
158200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
158300970526     C     FUNGRU        IFEQ      1
158400970526     C                   MOVEL     FUV(1)        FUN001
158500970526     C                   MOVEL     FUV(2)        FUN002
158600970526     C                   MOVEL     FUV(3)        FUN003
158700970526     C                   MOVEL     FUV(4)        FUN004
158800970526     C                   MOVEL     FUV(5)        FUN005
158900970526     C                   MOVEL     FUV(6)        FUN006
159000970526     C                   END
159100970526     C*
159200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
159300970526     C     FUNGRU        IFEQ      2
159400970526     C                   MOVEL     FUV(7)        FUN001
159500970526     C                   MOVEL     FUV(8)        FUN002
159600970526     C                   MOVEL     FUV(9)        FUN003
159700970526     C                   MOVEL     FUV(10)       FUN004
159800970526     C                   MOVEL     FUV(11)       FUN005
159900970526     C                   MOVEL     FUV(12)       FUN006
160000970526     C                   END
160100970526     C*
160200970526     C* TERZO GRUPPO DI 6 FUNZIONI
160300970526     C     FUNGRU        IFEQ      3
160400970526     C                   MOVEL     FUV(13)       FUN001
160500970526     C                   MOVEL     FUV(14)       FUN002
160600970526     C                   MOVEL     FUV(15)       FUN003
160700970526     C                   MOVEL     FUV(16)       FUN004
160800970526     C                   MOVEL     FUV(17)       FUN005
160900970526     C                   MOVEL     FUV(18)       FUN006
161000970526     C                   END
161100970526     C*
161200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
161300970526     C     FUNGRU        IFEQ      4
161400970526     C                   MOVEL     FUV(19)       FUN001
161500970526     C                   MOVEL     FUV(20)       FUN002
161600970526     C                   MOVEL     FUV(21)       FUN003
161700970526     C                   MOVEL     FUV(22)       FUN004
161800970526     C                   MOVEL     *BLANKS       FUN005
161900970526     C                   MOVEL     *BLANKS       FUN006
162000970526     C                   END
162100970526     C*
162200970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
162300970526     C                   MOVEL     *BLANKS       FUN007
162400970526     C                   MOVEL     *BLANKS       FUN008
162500970526     C*
162600970526     C     1             DO        24            I
162700970526     C                   MOVEL     FUD(I)        FUDRIG
162800970526     C     FUDFUN        IFEQ      '23'
162900970526     C     FUDATT        ANDEQ     'S'
163000970526     C                   MOVEL     FUDDES        FUN007
163100970526     C                   END
163200970526     C     FUDFUN        IFEQ      '24'
163300970526     C     FUDATT        ANDEQ     'S'
163400970526     C                   MOVEL     FUDDES        FUN008
163500970526     C                   END
163600970526     C                   END                                                    DO
163700970526     C*
163800970526     C                   ENDSR
163900970526     C*------------------------------------------------------------------------*
164000970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
164100970526     C*------------------------------------------------------------------------*
164200970526     C     OPZGES        BEGSR
164300970526     C*
164400970526     C* PULISCO CAMPI DI LAVORO
164500970526     C                   Z-ADD     0             W                 5 0
164600970526     C*
164700970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
164800970526     C* E CARICO TABELLA OPZIONI ATTIVE
164900991115     C     1             DO        24            I
165000970526     C                   MOVEL     OPD(I)        OPDRIG
165100970526     C*
165200970526     C* PULIZIA OPZIONI PRECEDENTI
165300970526     C                   MOVEL     *BLANKS       OPV(I)
165400970526     C*
165500970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
165600970526     C     OPDATT        IFEQ      'S'
165700970526     C                   ADD       1             W
165800970526     C                   MOVEL     OPDDES        OPV(W)
165900970526     C                   END
166000970526     C                   END                                                    DO WHILE
166100970526     C*
166200970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
166300970526     C* ALTRIMENTI DISATTIVO F23
166400991115     C     1             DO        24            I
166500970526     C                   MOVEL     FUD(I)        FUDRIG
166600970526     C*
166700970526     C     FUDFUN        IFEQ      '23'
166800970526     C     W             IFGT      8
166900970526     C                   MOVEL     'S'           FUDATT
167000970526     C                   ELSE
167100970526     C                   MOVEL     *BLANKS       FUDATT
167200970526     C                   END                                                    W GT 8
167300970526     C                   MOVEL     FUDRIG        FUD(I)
167400970526     C                   END
167500970526     C                   END                                                    DO WHILE
167600970526     C*
167700970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
167800970526     C     W             DIV       8             I
167900970526     C                   MVR                     Z                 5 0
168000970526     C*
168100970526     C     Z             IFNE      0
168200970526     C     I             ADD       1             Z
168300970526     C                   ELSE
168400970526     C                   Z-ADD     I             Z
168500970526     C                   END
168600970526     C*
168700970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
168800970526     C                   ADD       1             OPZGRU
168900970526     C     OPZGRU        IFGT      Z
169000970526     C     OPZGRU        ORLT      1
169100970526     C                   Z-ADD     1             OPZGRU
169200970526     C                   END
169300970526     C*
169400970526     C* PRIMO GRUPPO DI 8 OPZIONI
169500970526     C     OPZGRU        IFEQ      1
169600970526     C                   MOVEL     OPV(1)        OPZ001
169700970526     C                   MOVEL     OPV(2)        OPZ002
169800970526     C                   MOVEL     OPV(3)        OPZ003
169900970526     C                   MOVEL     OPV(4)        OPZ004
170000970526     C                   MOVEL     OPV(5)        OPZ005
170100970526     C                   MOVEL     OPV(6)        OPZ006
170200970526     C                   MOVEL     OPV(7)        OPZ007
170300970526     C                   MOVEL     OPV(8)        OPZ008
170400970526     C                   END
170500970526     C*
170600970526     C* SECONDO GRUPPO DI 8 OPZIONI
170700970526     C     OPZGRU        IFEQ      2
170800970526     C                   MOVEL     OPV(9)        OPZ001
170900970526     C                   MOVEL     OPV(10)       OPZ002
171000970526     C                   MOVEL     OPV(11)       OPZ003
171100970526     C                   MOVEL     OPV(12)       OPZ004
171200970526     C                   MOVEL     OPV(13)       OPZ005
171300970526     C                   MOVEL     OPV(14)       OPZ006
171400970526     C                   MOVEL     OPV(15)       OPZ007
171500970526     C                   MOVEL     OPV(16)       OPZ008
171600970526     C                   END
171700970526     C*
171800970526     C* TERZO GRUPPO DI 8 OPZIONI
171900970526     C     OPZGRU        IFEQ      3
172000970526     C                   MOVEL     OPV(17)       OPZ001
172100970526     C                   MOVEL     OPV(18)       OPZ002
172200970526     C                   MOVEL     OPV(19)       OPZ003
172300970526     C                   MOVEL     OPV(20)       OPZ004
172400970526     C                   MOVEL     OPV(21)       OPZ005
172500970526     C                   MOVEL     OPV(22)       OPZ006
172600970526     C                   MOVEL     OPV(23)       OPZ007
172700970526     C                   MOVEL     OPV(24)       OPZ008
172800970526     C                   END
172900970526     C*
173000970526     C                   ENDSR
173100970526     C*------------------------------------------------------------------------*
173200970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
173300970526     C*------------------------------------------------------------------------*
173400970526     C     OPZCON        BEGSR
173500970526     C*
173600970526     C* PULISCO CAMPI DI LAVORO
173700970526     C                   Z-ADD     0             I                 5 0
173800970526     C                   Z-ADD     0             W                 5 0
173900970526     C*
174000970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174100970526     C     1             DO        24            I
174200970526     C                   MOVEL     OPD(I)        OPDRIG
174300970526     C*
174400970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
174500970526     C     OPZKEY        IFEQ      OPDOPZ
174600970526     C     OPZKEY        OREQ      OPDOP1
174700970526     C*
174800970526     C     OPDATT        IFEQ      'S'
174900970526     C                   MOVEL     OPDOPZ        OPZKEY
175000970526     C                   Z-ADD     I             W
175100970526     C                   END
175200970526     C                   END
175300970526     C*
175400970526     C                   END                                                    DO WHILE
175500970526     C*
175600970526     C* OPZIONE NON ATTIVA (ERRORE)
175700970526     C     W             IFEQ      0
175800970526     C*
175900970526     C* OPZIONE NON OK
176000970526     C                   Z-ADD     1             OPZOK
176100970526     C*
176200970526     C* EMISSIONE MESSAGGIO DI ERRORE
176300970526     C                   MOVEL     MSG(1)        DSMSMS
176400970526     C                   ELSE
176500970526     C*
176600970526     C* OPZIONE ATTIVA (PROCEDI ...)
176700970526     C*
176800021211     C                   MOVEL     *BLANKS       PMSG
176900030509     C     OPZKEY        IFEQ      ' 2'
177000030509     C                   EXSR      O02GES
177100030509     C                   END
177200031210     C     OPZKEY        IFEQ      ' 4'
177300031210     C                   EXSR      O04GES
177400031210     C                   END
177500970526     C     OPZKEY        IFEQ      ' 5'
177600970526     C                   EXSR      O05GES
177700970526     C                   END
177800031210     C     OPZKEY        IFEQ      ' 6'
177900031210     C                   EXSR      O06GES
178000031210     C                   END
178100141210     C     OPZKEY        IFEQ      '10'
178200141210     C                   EXSR      O10GES
178300141210     C                   END
178400071001     C     OPZKEY        IFEQ      '11'
178500071001     C                   EXSR      O11GES
178600071001     C                   END
178700031211     C     OPZKEY        IFEQ      '15'
178800031211     C                   EXSR      O15GES
178900031211     C                   END
179000021211     C                   MOVEL     PMSG          DSMSMS
179100021211     C                   END                                                    OPZ.NON ATTIVA
179200970526     C*
179300970526     C                   ENDSR
179400970526     C*------------------------------------------------------------------------*
179500970526     C* NOTE COSTRUZIONE SCHIERE
179600970526     C*------------------------------------------------------------------------*
179700970526     C*
179800970526     C* -MSG-MESSAGGI
179900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
180000970526     C* (NON TOCCARE MSG,1 E MSG,2)
180100970526     C*
180200970526     C* -CMD-COMANDI
180300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
180400970526     C*
180500970526     C* -OPD-OPZIONI
180600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
180700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
180800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
180900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
181000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
181100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
181200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
181300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
181400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
181500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
181600970526     C*
181700970526     C* -FUD-FUNZIONI
181800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
181900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
182200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
182300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
182400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
182500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
182600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
182700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
182800991108     C*
182900991115     C* -STS-CODICI STATI DEI RECORD DI LOG
183000991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
183100991110     C* ================> IMPORTANTE <=================
183200991115     C* per codici di errore in -STS- indicare in -DES- sempre
183300991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
183400991110     C* ERR
183500991108     C*
183600970526     O*------------------------------------------------------------------------*
183700970526** DEC - DECODIFICHE
183800970526    GESTIONE                                                     1
183900000407 INTERROGAZIONE                                                  2
1840000305093
1841009705264
1842009705265
184300970526** MSG - MESSAGGI
184400030509TRTCM3R- Opzione non gestita.                                                  1
184500030509TRTCM3R- Tasto funzionale non gestito.                                         2
184600030509TRTCM3R- Non abilitato all inserimento in filiale.                             3
184700030509TRTCM3R- Utente non abilitato all'opzione richiesta.                           4
184800031211TRTCM3R- File di spool non trovato o inesistente, verificare.                  5
184900050111TRTCM3R- Specificare almeno 5 caratteri per la ricerca.                        6
185000050224TRTCM3R- Operazione non consentita su log ancora da elaborare.                 7
185100030509TRTCM3R- 8                                                                     8
185200030509TRTCM3R- 9                                                                     9
185300030509TRTCM3R- 10                                                                   10
185400030509TRTCM3R- 11                                                                   11
185500030509TRTCM3R- 12                                                                   12
185600030509TRTCM3R- 13                                                                   13
185700030509TRTCM3R- 14                                                                   14
185800030509TRTCM3R- 15                                                                   15
185900030509TRTCM3R- 16                                                                   16
186000030509TRTCM3R- 17                                                                   17
186100030509TRTCM3R- 18                                                                   18
186200030509TRTCM3R- 19                                                                   19
186300030509TRTCM3R- 20                                                                   20
186400970528** CMD - COMANDI CL
186500021211
186600021211
186700991110
186800970528
186900970528
187000970526** OPD - OPZIONI
187100030509                   -  -     (S/N)=N                        opz. 01¬
187200000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
187300030509                   -  -     (S/N)=N                        opz. 03¬
187400031210 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
187500000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
187600071001 6=Rilascio        -6 -     (S/N)=N                        opz. 06¬
187700021211                   -  -     (S/N)=N                        opz. 07¬
187800991110                   -  -     (S/N)=N                        opz. 08¬
187900991110                   -  -     (S/N)=N                        opz. 09¬
18800014121010=Stato invio eml -10-     (S/N)=N                        opz. 10¬
18810007100111=Riesecuzione    -7 -     (S/N)=N                        opz. 11¬
188200970526                   -  -     (S/N)=N                        opz. 12¬
188300970526                   -  -     (S/N)=N                        opz. 13¬
188400000407                   -  -     (S/N)=N                        opz. 14¬
18850003121115=Visualizza spool-15-     (S/N)=N                        opz. 15¬
188600970526                   -  -     (S/N)=N                        opz. 16¬
188700970526                   -  -     (S/N)=N                        opz. 17¬
188800970526                   -  -     (S/N)=N                        opz. 18¬
188900970526                   -  -     (S/N)=N                        opz. 19¬
189000970526                   -  -     (S/N)=N                        opz. 20¬
189100970526                   -  -     (S/N)=N                        opz. 21¬
189200970526                   -  -     (S/N)=N                        opz. 22¬
189300970526                   -  -     (S/N)=N                        opz. 23¬
189400000407                   -  -     (S/N)=N                        opz. 25¬
189500970526** FUD - FUNZIONI
189600970526                   -        (S/N)=N                        funz.01¬
189700970526                   -        (S/N)=N                        funz.02¬
189800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
189900120510F04=Dlt.log prg=0  -        (S/N)=N                        funz.04¬
190000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
190100030509                   -        (S/N)=N                        funz.06¬
190200970526                   -        (S/N)=N                        funz.07¬
190300970527                   -        (S/N)=N                        funz.08¬
190400030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
190500970527                   -        (S/N)=N                        funz.10¬
190600970526                   -        (S/N)=N                        funz.11¬
190700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
190800991116                   -        (S/N)=N                        funz.13¬
190900970526                   -        (S/N)=N                        funz.14¬
191000970526                   -        (S/N)=N                        funz.15¬
191100970526                   -        (S/N)=N                        funz.16¬
191200970526                   -        (S/N)=N                        funz.17¬
191300970526                   -        (S/N)=N                        funz.18¬
191400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
191600000407                   -        (S/N)=N                        funz.21¬
191700991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
191800991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
191900991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
