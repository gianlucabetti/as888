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
011300150417     D  pIn_AAS                       6
011400030128     D*------------------
011500030128     D* DS CONTROLLO DATA (8)
011600030128     D*------------------
011700030128     D WLBDA8          DS                  INZ
011800030128     D  G08DAT                 1      8  0
011900030128     D  G08INV                 9     16  0
012000030128     D  G08ERR                17     17
012100030128     D  G08TGI                18     22  0
012200021211     D*------------------
012300030509     D* PASSAGGIO PARAMETRI
012400021211     D*------------------
012500021211     D PARAM           DS                  INZ
012600021211     D  POP1                   1      3
012700050111     D  POP2                   4      4
012800050111     D  PPRG                   5     11
012900050111     D  PPDA                  12     13
013000050111     D  PPA                   14     15
013100050111     D  PMSG                  16     95
013200050216     D*------------------
013300050216     D* VARIABILI D WRK
013400050216     D*------------------
013500050216     D wOBJM           S                   LIKE(V1DOBJM)
013600050216     D wDST            S                   LIKE(V1DDST)
013700050216     D wCM1VAR         S                   LIKE(§CM1VAR)
013800050216     D wCM1DST         S                   LIKE(§CM1DST)
013900050808     D wDataDA         S               D   DATFMT(*iso)
014000050216     D*------------------
014100050216     D* COSTANTI
014200050216     D*------------------
014300050216     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
014400050216     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
014500970526     C*------------------------------------------------------------------------*
014600970526     C* MAIN LINES
014700970526     C*------------------------------------------------------------------------*
014800100202
014900100202
015000100202     C*
015100100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
015200100202     C
015300100202     C/EXEC SQL
015400100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
015500100202     C/END-EXEC
015600100202     C*
015700970526     C*
015800970526     C* SE NON CI SONO ERRORI, PROSEGUE
015900970526IF  1C     ERRL00        IFNE      '1'
016000970526     C*
016100970526     C* CARICA IL SFILE
016200970526     C                   EXSR      POSSFL
016300970526     C*
016400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016800970526     C*
016900970526     C* REIMPOSTA ERRORE DI PROCERURA
017000970526     C                   MOVEL     '0'           ERRL00
017100970526     C*
017200050114     C* EMETTE IL SFILE
017300970526     C                   EXSR      OUTSFL
017400970526     C*
017500970526     C* GESTIONE TASTI FUNZIONALI
017600970526     C                   EXSR      FUNCON
017700970526     C*
017800970526E   2C                   ENDDO
017900970527     C*
018000970526E   1C                   ENDIF
018100991108     C*
018200970526     C* FINE PGM
018300970526     C                   SETON                                        LR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F03GES        BEGSR
018800970526     C*
018900970526     C* IMPOSTA "PREMUTO F03"
019000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400120510     C* F04GES - GESTIONE F04 -> Delete log con prg=0
019500970526     C*------------------------------------------------------------------------*
019600120510     C     F04GES        BEGSR
019700970526     C*
019800120510     C* salvo kpjba prima di passarla
019900120510     C                   MOVEL     KPJBA         Sav_kpjba
020000120510     C* chiamata a pgm apposito
020100120510     C* (ha il video che chiede i parametri e non gliene servono altri)
020200120510     C                   CALL      'TRTCMAR'
020300120510     C                   PARM                    KPJBA
020400120510     C* ripristino kpjba
020500120510     C                   EVAL      KPJBA =       Sav_kpjba
020600970526     C*
020700970526     C                   ENDSR
020800120510     C*------------------------------------------------------------------------*
020900120510     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
021000120510     C*------------------------------------------------------------------------*
021100120510     C     F05GES        BEGSR
021200120510     C*
021300120510     C                   EXSR      POSSFL                                       *RICARICA SFILE
021400120510     C*
021500120510     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700030509     C* F12GES - GESTIONE F12 -> RITORNO
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F12GES        BEGSR
022000970526     C*
022100970526     C* IMPOSTA "PREMUTO F12"
022200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022300970526     C*
022400970526     C                   ENDSR
022500970528     C*------------------------------------------------------------------------*
022600970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
022700970528     C*------------------------------------------------------------------------*
022800970528     C     F21GES        BEGSR
022900970612     C*
023000991117     C* NON GESTITA
023100970528     C*
023200970528     C                   ENDSR
023300970528     C*------------------------------------------------------------------------*
023400970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
023500970528     C*------------------------------------------------------------------------*
023600970528     C     F22GES        BEGSR
023700970528     C*
023800021211     C* NON GESTITA
023900970528     C*
024000970528     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
024300970526     C*------------------------------------------------------------------------*
024400970526     C     F23GES        BEGSR
024500970526     C*
024600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025100970526     C*------------------------------------------------------------------------*
025200970526     C     F24GES        BEGSR
025300970526     C*
025400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025500970526     C*
025600970526     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025900970526     C*------------------------------------------------------------------------*
026000970526     C     RUPGES        BEGSR
026100970526     C*
026200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
026300970526     C*
026400970526     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600991108     C* ENTGES - GESTIONE ENTER -> INVIO
026700970526     C*------------------------------------------------------------------------*
026800970526     C     ENTGES        BEGSR
026900000407     C*
027000000407     C                   MOVEL     '0'           ERRL00
027100991108     C*
027200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027300970526     C                   EXSR      NEWPOS
027400021211     C*
027500021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027700021211     C     SWPOSI        IFNE      0
027800021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
027900021211     C                   ELSE
028000021211     C     SFLULT        IFNE      0
028100021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
028200021211     C*
028300021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
028400021211     C     SWMODI        IFNE      0
028500021211     C                   EXSR      POSSFL
028600021211     C                   ENDIF
028700021211     C                   ENDIF
028800021211     C                   ENDIF
028900021211     C                   ENDIF
029000021211     C*
029100021211     C                   ENDSR
029200030509     C*------------------------------------------------------------------------*
029300030509     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
029400030509     C*------------------------------------------------------------------------*
029500030509     C     O02GES        BEGSR
029600030509     C*
029700050410     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE" (X ORA SOLO SE UTENTE NN EDPFG)
029800050410     C                   IF        KNMUS <> 'EDPFG'
029900050224     C                   IF        VIDSTS = '0' OR
030000050224     C                             VIDSTS = 'A' OR
030100050224     C                             VIDSTS = 'B'
030200050224     C                   MOVEL     '1'           ERRL00
030300050224     C                   EVAL      PMSG = MSG(7)
030400050224     C                   ENDIF
030500050410     C                   ENDIF
030600050224     C*
030700030509     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800030509     C                   MOVEL     'O02'         POP1
030900030509     C                   MOVEL     VIDPROG       PPRG
031000041006     C                   MOVEL     VIDATT        PPDA
031100041006     C                   MOVEL     VIDTOTATT     PPA
031200030509     C                   EXSR      PASPAR
031300030509     C                   CALL      'TRTCM4R'
031400030509     C                   PARM                    KPJBA
031500031210     C                   MOVEL     KPJBU         PARAM
031600030509     C                   ENDIF
031700030509     C*
031800030509     C                   ENDSR
031900031210     C*------------------------------------------------------------------------*
032000031210     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZOINE RECORD/FILE
032100031210     C*------------------------------------------------------------------------*
032200031210     C     O04GES        BEGSR
032300050224     C*
032400050224     C* CONSENTO OPZIONE SOLO X STATI NN "DA ELABORARE"
032500050224     C                   IF        VIDSTS = '0' OR
032600050224     C                             VIDSTS = 'A' OR
032700050224     C                             VIDSTS = 'B'
032800050224     C                   MOVEL     '1'           ERRL00
032900050224     C                   EVAL      PMSG = MSG(7)
033000050224     C                   ENDIF
033100031210     C*
033200031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033300031210     C                   MOVEL     'O04'         POP1
033400031210     C                   MOVEL     VIDPROG       PPRG
033500041006     C                   MOVEL     VIDATT        PPDA
033600041006     C                   MOVEL     VIDTOTATT     PPA
033700031210     C                   EXSR      PASPAR
033800031210     C                   CALL      'TRTCM4R'
033900031210     C                   PARM                    KPJBA
034000031210     C                   MOVEL     KPJBU         PARAM
034100031210     C                   ENDIF
034200031210     C*
034300031210     C                   ENDSR
034400991014     C*------------------------------------------------------------------------*
034500000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O05GES        BEGSR
034800000407     C*
034900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000021217     C                   MOVEL     'O05'         POP1
035100030509     C                   MOVEL     VIDPROG       PPRG
035200041006     C                   MOVEL     VIDATT        PPDA
035300041006     C                   MOVEL     VIDTOTATT     PPA
035400030509     C                   EXSR      PASPAR
035500030509     C                   CALL      'TRTCM4R'
035600000407     C                   PARM                    KPJBA
035700031210     C                   MOVEL     KPJBU         PARAM
035800021211     C                   ENDIF
035900000407     C*
036000970526     C                   ENDSR
036100031210     C*------------------------------------------------------------------------*
036200031210     C* O06GES - GESTIONE OPZIONE 6=RILASCIO RECORD
036300031210     C*------------------------------------------------------------------------*
036400050224     C     O06GES        BEGSR
036500050224     C*
036600050321     C* CONSENTO OPZIONE X TUTTI GLI STATI
036700050321     C*                  IF        VIDSTS = '0' OR
036800050321     C*                            VIDSTS = 'A' OR
036900050321     C*                            VIDSTS = 'B'
037000050321     C*                  MOVEL     '1'           ERRL00
037100050321     C*                  EVAL      PMSG = MSG(7)
037200050321     C*                  ENDIF
037300031210     C*
037400031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037500050224     C                   CLEAR                   TRTCM9DS
037600050224     C                   EVAL      §CM9TIP    = 'E'
037700050224     C                   EVAL      §CM9PROG   = VIDPROG
037800050224     C                   EVAL      §CM9STSIN  = '0'
037900050224     C                   EVAL      §CM9MSGIN  = *blanks
038000050224     C                   CALL      'TRTCM9R'
038100080904     C                   PARM                    TRTCM9DS
038200050224     C*                  MOVEL     'O06'         POP1
038300050224     C*                  MOVEL     VIDPROG       PPRG
038400050224     C*                  MOVEL     VIDATT        PPDA
038500050224     C*                  MOVEL     VIDTOTATT     PPA
038600050224     C*                  EXSR      PASPAR
038700050224     C*                  CALL      'TRTCM4R'
038800050224     C*                  PARM                    KPJBA
038900050224     C*                  MOVEL     KPJBU         PARAM
039000050224     C                   ENDIF
039100031210     C*
039200031210     C                   ENDSR
039300141210     C*------------------------------------------------------------------------*
039400141210     C* O10GES - GESTIONE OPZIONE 10=VISUALIZZAZIONE STATUS INVIO EMAIL
039500141210     C*------------------------------------------------------------------------*
039600141210     C     O10GES        BEGSR
039700141211     C*
039800141211     C* salvo kpjba prima di passarla
039900141211     C                   MOVEL     KPJBA         Sav_kpjba
040000141211     C*
040100141211     C                   CLEAR                   ParamTCMB
040200141211     C                   MOVEL     VIDPROG       pIn_Prog
040300150417     C                   EVAL      pIn_AAS =
040400150417     C                              %subst(VIDDAT:7:4) + %subst(VIDDAT:4:2)
040500141211     C                   MOVEL     VIDMIT        pIn_Mit
040600141211     C                   EVAL      Kpjbu = ParamTCMB
040700141210     C*
040800141210     C                   CALL      'TRTCMBR'
040900141211     C                   PARM                    KPJBA
041000141210     C*
041100141211     C* ripristino kpjba
041200141211     C                   EVAL      KPJBA =       Sav_kpjba
041300141211     C*
041400141210     C                   ENDSR
041500071001     C*------------------------------------------------------------------------*
041600071001     C* O11GES - GESTIONE OPZIONE 11=RIESECUZIONE SPOOL
041700071001     C*------------------------------------------------------------------------*
041800071001     C     O11GES        BEGSR
041900071001     C*
042000071001     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042100071001     C                   CALL      'TRTCM3C'
042200071001     C                   PARM                    VIDFNAM
042300071001     C                   PARM                    VIDJNAM
042400071001     C                   PARM                    VIDUNAM
042500071001     C                   PARM                    VIDJNUM
042600071001     C                   PARM                    VIDFNUM
042700071001     C                   PARM      *BLANKS       ESITO             1
042800071001     C                   PARM      'EMAILIN_N'   WOUTQ            10
042900071001     C*
043000071002     C                   IF        ESITO = '0'
043100071001     C                   MOVEL     'O04'         POP1
043200071001     C                   MOVEL     VIDPROG       PPRG
043300071001     C                   MOVEL     VIDATT        PPDA
043400071001     C                   MOVEL     VIDTOTATT     PPA
043500071001     C                   EXSR      PASPAR
043600071001     C                   CALL      'TRTCM4R'
043700071001     C                   PARM                    KPJBA
043800071001     C                   MOVEL     KPJBU         PARAM
043900071001     C                   ENDIF
044000071001     C                   ENDIF
044100071001     C*
044200071001     C                   ENDSR
044300031211     C*------------------------------------------------------------------------*
044400031211     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE SPOOL
044500031211     C*------------------------------------------------------------------------*
044600031211     C     O15GES        BEGSR
044700031211     C*
044800031211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044900031211     C                   MOVEL     'O15'         POP1
045000031211     C                   CALL      'TRTCM4C'
045100031211     C                   PARM                    POP1
045200031211     C                   PARM                    VIDFNAM
045300031211     C                   PARM                    VIDJNAM
045400031211     C                   PARM                    VIDUNAM
045500031211     C                   PARM                    VIDJNUM
045600031211     C                   PARM                    VIDFNUM
045700031211     C                   PARM                    ESITO             1
045800031211     C                   IF        ESITO <> '0'
045900031211     C                   EVAL      PMSG = MSG(5)
046000031211     C                   ENDIF
046100031211     C                   ENDIF
046200031211     C*
046300031211     C                   ENDSR
046400991028     C*------------------------------------------------------------------------*
046500991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
046600991112     C*------------------------------------------------------------------------*
046700991112     C     PASPAR        BEGSR
046800991112     C*
046900000407     C                   MOVEL     PARAM         KPJBU
047000991112     C*
047100991112     C                   ENDSR
047200970526     C*------------------------------------------------------------------------*
047300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
047400970526     C*------------------------------------------------------------------------*
047500970526     C     NEWPOS        BEGSR
047600970526     C*
047700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
047800970526     C*         1= RICHIESTO POSIZIONAMENTO
047900970526     C                   Z-ADD     0             SWPOSI            7 0
048000970526     C*
048100991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048400970526     C                   Z-ADD     0             SWMODI            1 0
048500970530     C*
048600970530     C* REIMPOSTA INDICATORI DI ERRORE
048700050111     C                   SETOFF                                       868788
048800050111     C                   SETOFF                                       89
048900050111     C                   MOVEL     '0'           ERRPAR            1
049000050111     C*
049100050111     C* CONTROLLO SE RICHIESTE RICERCE X STRINGHE CONTENUTE (OGGETTO MAIL E DESTINATARIO MAIL)
049200050111     C* CHE SIANO STATI SPECIFICATO ALMENO 5 CARATTERI
049300050111     C                   IF        V1DOBJM <> *blanks
049400050111     C                   IF        %len(%trim(V1DOBJM)) < 5
049500050111     C                   SETON                                        86
049600050111     C                   MOVEL     '1'           ERRPAR
049700050111     C                   MOVEL     MSG(6)        DSMSMS
049800050111     C                   ENDIF
049900050111     C                   ENDIF
050000050111     C*
050100050111     C                   IF        V1DDST <> *blanks
050200050111     C                   IF        %len(%trim(V1DDST)) < 5
050300050111     C                   SETON                                        87
050400050111     C                   MOVEL     '1'           ERRPAR
050500050111     C                   MOVEL     MSG(6)        DSMSMS
050600050111     C                   ENDIF
050700050111     C                   ENDIF
050800050111     C*
050900050111     C* PROSEGUO CON VERIFICA PARAMETRI E GESTIONE FILTRI SELEZIONE SOLO SE NO ERRORI NEI PARAMETRI
051000050111     C                   IF        ERRPAR = '0'
051100040729     C*
051200040729     C* ESEGUO "FORZATURA" CONTROLLO CAMPO SELEZIONE X PROGRESSIVO UNIVOCO (ID PROG) SE = *zeros
051300040729     C                   IF        V1DPRG = *all'0'
051400040729     C                   EVAL      V1DPRG = *blanks
051500040729     C                   ENDIF
051600051026     C*
051700051026     C* SE P.O. = *ZEROS FORZO *BLANKS X CONSENTIRE LA CORRETTA "NN SELEZIONE"
051800051026     C                   IF        V1DPO = *zeros
051900051026     C                   EVAL      V1DPO = *blanks
052000051026     C                   ENDIF
052100050110     C*
052200050110     C* ESEGUO "FORZATURA" P.O. = P.O. UTENTE IN RICHIAMO DA "FILIALE" E P.O. NN INDICATO
052300050111     C   50              IF        V1DPO = *zeros  OR
052400050110     C                             V1DPO = *blanks
052500050110     C                   MOVEL     DUTPOU        V1DPO
052600050110     C                   ENDIF
052700970526     C*
052800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
052900030305     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
053000050111     C                   IF        V1DDAD  <> depdad  OR
053100050111     C                             V1DDAA  <> depdaa  OR
053200050111     C                             V1DPRG  <> depprg  OR
053300050111     C                             V1DSTS  <> depsts  OR
053400050111     C                             V1DSNA  <> depsna  OR
053500050111     C                             V1DUNA  <> depuna  OR
053600050111     C                             V1DUDT  <> depudt  OR
053700050111     C                             V1DPO   <> deppo   OR
053800050111     C                             V1DCOR  <> depcor  OR
053900050111     C                             V1DIDP  <> depidp  OR
054000050111     C                             V1DDST  <> depdst  OR
054100050111     C                             V1DOBJM <> depobjm
054200021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
054300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
054400030509     C                   Z-ADD     V1DDAD        depdad                         *DEPOSITI
054500030509     C                   MOVEL     V1DDAA        depdaa                         *DEPOSITI
054600030509     C                   MOVEL     V1DPRG        depprg                         *DEPOSITI
054700030509     C                   MOVEL     V1DSTS        depsts                         *DEPOSITI
054800030604     C                   MOVEL     V1DSNA        depsna                         *DEPOSITI
054900030604     C                   MOVEL     V1DUNA        depuna                         *DEPOSITI
055000030604     C                   MOVEL     V1DUDT        depudt                         *DEPOSITI
055100040818     C                   MOVEL     V1DPO         deppo                          *DEPOSITI
055200040818     C                   MOVEL     V1DCOR        depcor                         *DEPOSITI
055300040914     C                   MOVEL     V1DIDP        depidp                         *DEPOSITI
055400050111     C                   MOVEL     V1DOBJM       depobjm                        *DEPOSITI
055500050111     C                   MOVEL     V1DDST        depdst                         *DEPOSITI
055600021211     C                   ENDIF
055700021211     C                   ENDIF
055800970526     C*
055900050111     C                   ENDIF
056000050111     C*
056100970526     C                   ENDSR
056200970526     C*------------------------------------------------------------------------*
056300021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
056400970526     C*------------------------------------------------------------------------*
056500970528     C     SETFIL        BEGSR
056600050808     C*
056700050808     C* COSTRUISCO SUBITO LA DATA DA CUI INIZIARE LA LETTURA
056800050808     C                   EVAL      wDATA_A =
056900050808     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
057000050808     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
057100050808     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
057200050808     C                             '99.99.99.999'
057300970526     C*
057400030509     C* POSIZIONAMENTO DEL FILE
057500050808     C     WTIPRI        IFEQ      'PRG'
057600050808     C     V1DPRG        SETLL     timal01l                           99
057700050808     C                   ENDIF
057800050808     C*
057900050808     C     WTIPRI        IFEQ      'PO'
058000050808     C                   EVAL      KEYDAOR = wDATA_A
058100050808     C                   MOVEL     V1DPO         KEYPO
058200050808     C     KEYMAL5       SETLL     timal05l                           99
058300050808     C                   ENDIF
058400050808     C*
058500050808     C     WTIPRI        IFEQ      'STS'
058600050808     C                   EVAL      KEYDAOR = wDATA_A
058700050808     C                   MOVEL     V1DSTS        KEYSTS
058800050808     C     KEYMAL4       SETLL     timal04l                           99
058900021211     C                   ENDIF
059000050808     C*
059100050808     C     WTIPRI        IFEQ      'DAT'
059200050808     C                   MOVEL     *HIVAL        KEYDAOR
059300050808     C     KEYMAL2P      SETLL     timal02l                           99
059400050808     C                   ENDIF
059500980107     C*
059600021211     C* LETTURA RECORD
059700021211     C     *IN99         IFEQ      *OFF
059800970526     C                   EXSR      LETFIL
059900021211     C                   ENDIF
060000970526     C*
060100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
060200021211     C     *IN99         IFEQ      *ON
060300970526     C                   Z-ADD     1             SFLEOF
060400021211     C                   ENDIF
060500970526     C*
060600970526     C                   ENDSR
060700970526     C*------------------------------------------------------------------------*
060800970526     C* LETFIL - LETTURA RECORD
060900970526     C*------------------------------------------------------------------------*
061000970526     C     LETFIL        BEGSR
061100970526     C*
061200970526     C* LEGGE FINO A:
061300970526     C*     - FINE FILE
061400970526     C*     - TROVATO RECORD VALIDO
061500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
061600021211     C     *IN99         DOWEQ     *OFF
061700970526     C     WRECOK        ANDEQ     'N'
061800970526     C*
061900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
062000050808     C     WTIPRI        IFEQ      'PRG'
062100050808     C     V1DPRG        READE     timal01l                               99
062200050808     C                   ENDIF
062300050808     C     WTIPRI        IFEQ      'PO'
062400050808     C     V1DPO         READE     timal05l                               99
062500050808     C                   ENDIF
062600050808     C     WTIPRI        IFEQ      'STS'
062700050808     C     V1DSTS        READE     timal04l                               99
062800050808     C                   ENDIF
062900050808     C     WTIPRI        IFEQ      'DAT'
063000050808     C                   READ      timal02l                               99
063100021211     C                   ENDIF
063200970528     C*
063300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063400021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
063500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
063600021211     C                   ELSE
063700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
063800021211     C                   ENDIF
063900021211     C                   ENDDO
064000970526     C*
064100970526     C                   ENDSR
064200970526     C*------------------------------------------------------------------------*
064300021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
064400970526     C*------------------------------------------------------------------------*
064500970526     C     CHKREC        BEGSR
064600970526     C*
064700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
064800040818     C*
064900040818     C* MUOVE I DATI UTENTI ESTESI DELLO SPOOL NELLA APPOSITA DS D RIDEFINIZIONE
065000040818     C                   MOVEL     malDUEX       TRTCM1DS
065100030509     C*
065200030128     C* FILTRA SE RICHIESTO PERIODO DA->A
065300050216     C                   IF        WRECOK = 'S'
065400050110     C                   IF        V1DDADINV > *zeros OR
065500050110     C                             V1DDAAINV > *zeros
065600030509     C                   MOVEL     *all'0'       wDATA_DA         23
065700030509     C                   MOVEL     *all'0'       wDATA_A          23
065800030509     C                   EVAL      wDATA_DA =
065900050110     C                             %subst(%editw(V1DDADINV:'        '):1:4)+'-'+
066000050110     C                             %subst(%editw(V1DDADINV:'        '):5:2)+'-'+
066100050110     C                             %subst(%editw(V1DDADINV:'        '):7:2)+'-'+
066200030509     C                             '00.00.00.000'
066300030509     C                   EVAL      wDATA_A =
066400050110     C                             %subst(%editw(V1DDAAINV:'        '):1:4)+'-'+
066500050110     C                             %subst(%editw(V1DDAAINV:'        '):5:2)+'-'+
066600050110     C                             %subst(%editw(V1DDAAINV:'        '):7:2)+'-'+
066700030509     C                             '99.99.99.999'
066800030509     C                   IF        MALDAOR >= wDATA_DA AND MALDAOR <= wDATA_A
066900030509     C                   ELSE
067000030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067100030509     C                   ENDIF
067200050808     C                   IF        MALDAOR < wDATA_DA
067300050808     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067400050808     C                   Z-ADD     1             SFLEOF
067500050808     C                   SETON                                        99
067600050808     C                   ENDIF
067700030509     C                   ENDIF
067800050216     C                   ENDIF
067900030509     C*
068000030509     C* FILTRA SE RICHIESTO UN PROGRESSIVO
068100050216     C                   IF        WRECOK = 'S'
068200030509     C                   IF        V1DPRG  <> *blanks
068300030509     C                   IF        MALPROG <> V1DPRG
068400030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
068500030509     C                   ENDIF
068600030509     C                   ENDIF
068700050216     C                   ENDIF
068800030509     C*
068900030509     C* FILTRA SE RICHIESTO UNO STATO
069000050216     C                   IF        WRECOK = 'S'
069100030509     C                   IF        V1DSTS <> *blanks
069200050422     C                   IF        V1DSTS = '0'
069300050422     C                   IF        MALSTS <> '0' AND MALSTS <> 'X'
069400050422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069500050422     C                   ENDIF
069600050422     C                   ELSE
069700030509     C                   IF        MALSTS <> V1DSTS
069800030509     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
069900030509     C                   ENDIF
070000050422     C                   ENDIF
070100030509     C                   ENDIF
070200050216     C                   ENDIF
070300030604     C*
070400030604     C* FILTRA SE RICHIESTO UNO SPOOL NAME
070500050216     C                   IF        WRECOK = 'S'
070600030604     C                   IF        V1DSNA  <> *blanks
070700030604     C                   IF        MALFNAM <> V1DSNA
070800030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070900030604     C                   ENDIF
071000030604     C                   ENDIF
071100050216     C                   ENDIF
071200030604     C*
071300030604     C* FILTRA SE RICHIESTO UNO USER NAME
071400050216     C                   IF        WRECOK = 'S'
071500030604     C                   IF        V1DUNA  <> *blanks
071600030604     C                   IF        MALUNAM <> V1DUNA
071700030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
071800030604     C                   ENDIF
071900030604     C                   ENDIF
072000050216     C                   ENDIF
072100030604     C*
072200030604     C* FILTRA SE RICHIESTO UNO USER DATA
072300050216     C                   IF        WRECOK = 'S'
072400030604     C                   IF        V1DUDT  <> *blanks
072500030604     C                   IF        MALUDTA <> V1DUDT
072600030604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072700030604     C                   ENDIF
072800030604     C                   ENDIF
072900050216     C                   ENDIF
073000040818     C*
073100040818     C* FILTRA SE RICHIESTO UN P.O.
073200050216     C                   IF        WRECOK = 'S'
073300040818     C                   IF        V1DPO = *zeros
073400040818     C                   EVAL      V1DPO = *blanks
073500040818     C                   ENDIF
073600040818     C                   IF        V1DPO  <> *blanks
073700040818     C                   IF        §CM1PO <> V1DPO
073800040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
073900040818     C                   ENDIF
074000040818     C                   ENDIF
074100050216     C                   ENDIF
074200040818     C*
074300040818     C* FILTRA SE RICHIESTA UNA REGOLA
074400050216     C                   IF        WRECOK = 'S'
074500040818     C                   IF        V1DCOR   <> *blanks
074600040818     C                   IF        §CM1TIPS <> V1DCOR
074700040818     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074800040818     C                   ENDIF
074900040818     C                   ENDIF
075000050216     C                   ENDIF
075100040914     C*
075200040914     C* FILTRA SE RICHIESTO UN PROCESSO
075300050216     C                   IF        WRECOK = 'S'
075400040914     C                   IF        V1DIDP <> *blanks
075500040914     C                   IF        MALIDP <> V1DIDP
075600040914     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075700040914     C                   ENDIF
075800040914     C                   ENDIF
075900050216     C                   ENDIF
076000050111     C*
076100050111     C* FILTRA SE RICHIESTO UN OGGETTO MAIL
076200050216     C                   IF        WRECOK = 'S'
076300050216     C                   IF        V1DOBJM <> *blanks
076400050216     C     minu:maiu     XLATE     V1DOBJM       wOBJM                          *Minus -> Maiuscolo
076500050216     C     minu:maiu     XLATE     §CM1VAR       wCM1VAR                        *Minus -> Maiuscolo
076600050216     C                   IF        %scan(%trim(wOBJM):wCM1VAR) > 0
076700050111     C                   ELSE
076800050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
076900050111     C                   ENDIF
077000050111     C                   ENDIF
077100050216     C                   ENDIF
077200050111     C*
077300050111     C* FILTRA SE RICHIESTO UN DESTINATARIO MAIL
077400050216     C                   IF        WRECOK = 'S'
077500050111     C                   IF        V1DDST <> *blanks
077600050216     C     minu:maiu     XLATE     V1DDST        wDST                           *Minus -> Maiuscolo
077700050216     C     minu:maiu     XLATE     §CM1DST       wCM1DST                        *Minus -> Maiuscolo
077800050216     C                   IF        %scan(%trim(wDST):wCM1DST) > 0
077900050111     C                   ELSE
078000050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
078100050111     C                   ENDIF
078200050111     C                   ENDIF
078300050216     C                   ENDIF
078400991108     C*
078500970526     C                   ENDSR
078600970526     C*------------------------------------------------------------------------*
078700000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
078800970526     C*------------------------------------------------------------------------*
078900970526     C     IRESFL        BEGSR
079000970526     C*
079100000407     C                   MOVEL     *blanks       OPZKEY
079200040914     C                   MOVEL     malIDP        VIDIDP
079300041005     C                   MOVEL     malATT        VIDATT
079400041005     C                   MOVEL     malTOTATT     VIDTOTATT
079500050110     C                   EVAL      VIDDAT =
079600050110     C                             %subst(malDAOR:9:2)+'/'+
079700050110     C                             %subst(malDAOR:6:2)+'/'+
079800050110     C                             %subst(malDAOR:1:4)
079900030604     C                   MOVEL     malUDTA       VIDUDTA
080000030509     C                   MOVEL     malPROG       VIDPROG
080100030509     C                   MOVEL     malFNAM       VIDFNAM
080200030509     C                   MOVEL     malJNAM       VIDJNAM
080300030509     C                   MOVEL     malUNAM       VIDUNAM
080400030509     C                   MOVEL     malJNUM       VIDJNUM
080500030509     C                   MOVEL     malFNUM       VIDFNUM
080600030918     C                   Z-ADD     malTPAG       VIDTPAG
080700050418     C                   IF        malSTS = 'X'
080800050418     C                   MOVEL     '0'           VIDSTS
080900050418     C                   ELSE
081000040818     C                   MOVEL     malSTS        VIDSTS
081100050418     C                   ENDIF
081200040818     C                   MOVEL     malDUEX       TRTCM1DS
081300040818     C                   MOVEL     §CM1TIPS      VIDCOR
081400040818     C                   MOVEL     §CM1PO        VIDPO
081500050110     C                   MOVEL     §CM1DST       VIDDST
081600141211     C                   EVAL      VIDMit = %trim(§CM1MITT) + '@brt.it'
081700030509     C*
081800040818     C* DECODIFICHE
081900040818     C                   EVAL      VIDSTSD = *blanks
082000040818     C                   IF        VIDSTS = 'H'
082100040818     C                   EVAL      VIDSTSD = 'Hld'
082200040818     C                   ENDIF
082300040818     C                   IF        VIDSTS = '1'
082400040818     C                   EVAL      VIDSTSD = 'OK'
082500040818     C                   ENDIF
082600040818     C                   IF        VIDSTS = '2'
082700040818     C                   EVAL      VIDSTSD = 'Err'
082800040818     C                   ENDIF
082900040818     C                   IF        VIDSTS = '3'
083000040818     C                   EVAL      VIDSTSD = 'Aut'
083100040818     C                   ENDIF
083200040818     C*
083300970526     C                   ENDSR
083400970526     C*------------------------------------------------------------------------*
083500991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
083600970526     C*------------------------------------------------------------------------*
083700970526     C     CHKPAR        BEGSR
083800021218     C*
083900021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
084000030509     C                   MOVEL     '0'           W_ERR             1
084100050808     C*
084200050808     C* VERIFICO X QUALE VISTA LOGICA CONVIENE LEGGERE IN RELAZIONE AI PARAMETRI DI SELEZIONE
084300050808     C                   SELECT
084400050808     C                   WHEN      V1DPRG <> *blanks
084500050808     C                   EVAL      WTIPRI = 'PRG'
084600050808     C                   WHEN      V1DSTS = '2'
084700050808     C                   EVAL      WTIPRI = 'STS'
084800050808     C                   WHEN      V1DSTS = '3'
084900050808     C                   EVAL      WTIPRI = 'STS'
085000050808     C                   WHEN      V1DSTS = 'A'
085100050808     C                   EVAL      WTIPRI = 'STS'
085200050808     C                   WHEN      V1DSTS = 'B'
085300050808     C                   EVAL      WTIPRI = 'STS'
085400050808     C                   WHEN      V1DPO  <> *blanks
085500050808     C                   EVAL      WTIPRI = 'PO'
085600050808     C                   OTHER
085700050808     C                   MOVEL     'DAT'         WTIPRI            3
085800050808     C                   ENDSL
085900030128     C*
086000030128     C* CONTROLLO VALIDITA PERIODO DA
086100030128     C                   IF        W_ERR = '0'
086200050110     C                   Z-ADD     *ZEROS        V1DDADINV         8 0
086300030509     C                   IF        V1DDAD > *zeros
086400030509     C                   Z-ADD     V1DDAD        DAW8
086500030128     C                   EXSR      CHKDA8
086600030128     C                   IF        G08ERR = '1'
086700030128     C                   EVAL      W_ERR = '1'
086800030128     C                   SETON                                        88
086900050110     C                   ELSE
087000050110     C                   Z-ADD     WDA8          V1DDADINV
087100050110     C                   Z-ADD     DAW8          V1DDAD
087200030128     C                   ENDIF
087300030128     C                   ENDIF
087400030128     C                   ENDIF
087500030128     C*
087600030128     C* CONTROLLO VALIDITA PERIODO A
087700030128     C                   IF        W_ERR = '0'
087800050110     C                   Z-ADD     *ZEROS        V1DDAAINV         8 0
087900050110     C                   IF        V1DDAA  > *zeros
088000030509     C                   Z-ADD     V1DDAA        DAW8
088100030128     C                   EXSR      CHKDA8
088200030128     C                   IF        G08ERR = '1'
088300030128     C                   EVAL      W_ERR = '1'
088400030128     C                   SETON                                        89
088500050110     C                   ELSE
088600050110     C                   Z-ADD     WDA8          V1DDAAINV
088700050110     C                   Z-ADD     DAW8          V1DDAA
088800030128     C                   ENDIF
088900030128     C                   ENDIF
089000030128     C                   ENDIF
089100030128     C*
089200030128     C* CONTROLLO PERIODO DA <= PERIODO A
089300030128     C                   IF        W_ERR = '0'
089400050110     C                   IF        V1DDADINV > V1DDAAINV
089500030128     C                   EVAL      W_ERR = '1'
089600030128     C                   SETON                                        8889
089700030128     C                   ENDIF
089800030128     C                   ENDIF
089900000407     C*
090000970526     C                   ENDSR
090100970526     C*------------------------------------------------------------------------*
090200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
090300970526     C*------------------------------------------------------------------------*
090400970526     C     CHKFOP        BEGSR
090500991112     C*
090600991112     C* OPZIONI
090700970626     C*
090800000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
090900021211     C     DEPOP0        IFEQ      'P01'
091000000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
091100000420     C                   MOVEL     'S'           OPDATT
091200000420     C                   MOVEL     OPDRIG        OPD(1)
091300021211     C                   ELSE
091400000420     C* OPZIONI
091500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
091600970626     C                   MOVEL     'S'           OPDATT
091700970626     C                   MOVEL     OPDRIG        OPD(5)
091800141210     C                   MOVEL     OPD(10)       OPDRIG                         *VISUAL.STATUS INVIO
091900141210     C                   MOVEL     'S'           OPDATT
092000141210     C                   MOVEL     OPDRIG        OPD(10)
092100021211     C                   ENDIF
092200970626     C*
092300000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
092400021211     C     DEPOP0        IFEQ      'P02'
092500991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
092600991112     C                   MOVEL     'S'           OPDATT
092700991112     C                   MOVEL     OPDRIG        OPD(5)
092800050111     C                   MOVEL     OPD(15)       OPDRIG                         *VISUALIZZA SPOOL
092900050111     C                   MOVEL     'S'           OPDATT
093000050111     C                   MOVEL     OPDRIG        OPD(15)
093100030509     C                   IF        %subst(KNMUS:1:3) = 'EDP'
093200030509     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
093300030509     C                   MOVEL     'S'           OPDATT
093400030509     C                   MOVEL     OPDRIG        OPD(2)
093500031210     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
093600031210     C                   MOVEL     'S'           OPDATT
093700031210     C                   MOVEL     OPDRIG        OPD(4)
093800031210     C                   MOVEL     OPD(6)        OPDRIG                         *RILASCIO
093900031210     C                   MOVEL     'S'           OPDATT
094000031210     C                   MOVEL     OPDRIG        OPD(6)
094100071001     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE SPOOL
094200071001     C                   MOVEL     'S'           OPDATT
094300071001     C                   MOVEL     OPDRIG        OPD(11)
094400120510     C                   MOVEL     FUD(04)       FUDRIG                         *DLT LOG CON PRG=0
094500120510     C                   MOVEL     'S'           FUDATT
094600120510     C                   MOVEL     FUDRIG        FUD(04)
094700021211     C                   ENDIF
094800030509     C                   ENDIF
094900970526     C*
095000970526     C* GESTIONE OPZIONI A VIDEO
095100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
095200970526     C                   EXSR      OPZGES
095300970526     C*
095400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
095500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
095600970526     C                   EXSR      FUNGES
095700970526     C*
095800970526     C                   ENDSR
095900020917     C*--------------------------------------------------------------------------------------------*
096000020917     C* REPERISCE I DATI UTENTE
096100020917     C*--------------------------------------------------------------------------------------------*
096200020917     C     REPDATIUTE    BEGSR
096300020917     C*
096400020917     C* INIZIALIZZA VARIABILI DI WRK
096500020917     C                   CLEAR                   TIBS34DS
096600020917     C                   CLEAR                   AZUTEDS
096700020917     C                   CLEAR                   DDATIUTE
096800020917     C*
096900020917     C     *DTAARA       DEFINE    §azute        azuteds
097000020917     C     *DTAARA       DEFINE    §datiute      ddatiute
097100020917     C                   IN(E)     *DTAARA
097200021211     C                   IF        %Error
097300021211     C                   EVAL      I34Tla = 'L'
097400020917     C                   CALL      'TIBS34R'
097500020917     C                   PARM                    Tibs34Ds
097600020917     C                   IN        *DTAARA
097700021211     C                   ENDIF
097800020917     C*
097900020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
098000020917     C                   MOVEL(P)  RSUT          DSFIRS           20
098100020917     C*
098200020917     C                   ENDSR
098300030128     C*------------------------------------------------------------------------*
098400030128     C* CHKDA8 - CONTROLLA LA DATA (8)
098500030128     C*------------------------------------------------------------------------*
098600030128     C     CHKDA8        BEGSR
098700030128     C*
098800050110     C                   Z-ADD     DAW8          G08DAT
098900050110     C                   Z-ADD     *ZEROS        G08INV
099000050110     C                   MOVEL     '0'           G08ERR
099100030128     C                   CALL      'XSRDA8'
099200030128     C                   PARM                    WLBDA8
099300050110     C                   Z-ADD     G08INV        WDA8
099400050110     C                   Z-ADD     G08DAT        DAW8
099500030128     C*
099600030128     C                   ENDSR
099700970526     C*------------------------------------------------------------------------*
099800970526     C* *INZSR - OPERAZIONI INIZIALI
099900970526     C*------------------------------------------------------------------------*
100000970526     C     *INZSR        BEGSR
100100970526     C*---
100200970526     C* RICEVIMENTO PARAMETRI
100300970526     C*---
100400970526     C     *ENTRY        PLIST
100500970612     C                   PARM                    KPJBA
100600021211     C                   MOVEL     KPJBU         PARAM
100700030509     C                   MOVEL     POP1          DEPOP0            3
100800050110     C*
100900050110     C* REPERIMENTO DATI UTENTE
101000050110     C                   EXSR      REPDATIUTE
101100050808     C*
101200050808     C* CALCOLA LA DATA CORRENTE
101300050808     C                   time                    wn14             14 0
101400050808     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
101500050808     C                   z-add     wn8           g08dat
101600050808     C                   z-add     *zeros        g08inv
101700050808     C                   movel     '0'           g08err
101800050808     C                   call      'XSRDA8'
101900050808     C                   parm                    wlbda8
102000050808     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
102100030509     C*
102200030509     C* A SECONDA DELL'OPZOINE DI ENTRATA IMPOSTO LA MODALITA' A VIDEO
102300050110     C                   SETOFF                                       50
102400050111     C                   MOVEL     'S'           POP2                           * SEDE
102500030509     C                   IF        DEPOP0 = 'P05'
102600030509     C                   EVAL      VIDING = DEC(2)
102700050110     C                   MOVEL     DUTPOU        V1DPO
102800050111     C                   MOVEL     DUTPOU        deppo
102900050110     C                   SETON                                        50
103000050111     C                   MOVEL     'F'           POP2                           * FILIALE
103100030509     C                   ENDIF
103200030509     C                   IF        DEPOP0 = 'P02'
103300030509     C                   EVAL      VIDING = DEC(1)
103400030509     C                   ENDIF
103500030604     C*
103600030604     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
103700050808     C     *iso          move      datcor        wDataDA
103800050808     C                   subdur    30:*d         wDataDA
103900050808     C                   move      wDataDA       V1DDAD
104000050808     C                   z-add     *zeros        g08dat
104100050808     C                   z-add     V1DDAD        g08inv
104200050808     C                   movel     '3'           g08err
104300050808     C                   call      'XSRDA8'
104400050808     C                   parm                    wlbda8
104500050808     C                   z-add     g08dat        V1DDAD
104600050808     C                   EVAL      V1DDAA = 31122099
104700060731     C                   EVAL      depdad = V1DDAD
104800030604     C                   EVAL      depdaa = V1DDAA
104900970526     C*
105000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
105100970526     C     XMSGPA        PLIST
105200970526     C                   PARM                    DSMSMS
105300970526     C                   PARM                    DSMSPG
105400970526     C                   PARM                    DSMSKE
105500970526     C                   PARM                    DSMSER
105600970526     C*
105700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
105800970526     C                   CALL      'XKEY'
105900970526     C                   PARM                    DSKEY
106000970526     C*---
106100970526     C* VARIABILI NON RIFERITE AL DATA BASE
106200970526     C*---
106300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
106400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
106500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
106600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
106700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
106800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
106900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
107000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
107100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
107200970526     C*---
107300030509     C     *LIKE         DEFINE    V1DDAD        DAW8
107400030509     C     *LIKE         DEFINE    V1DDAD        WDA8
107500030509     C     *LIKE         DEFINE    V1DDAD        depdad
107600030509     C     *LIKE         DEFINE    V1DDAA        depdaa
107700030509     C     *LIKE         DEFINE    V1DSTS        depsts
107800040818     C     *LIKE         DEFINE    V1DPO         deppo
107900040818     C     *LIKE         DEFINE    V1DCOR        depcor
108000030509     C     *LIKE         DEFINE    V1DPRG        depprg
108100030604     C     *LIKE         DEFINE    V1DSNA        depsna
108200030604     C     *LIKE         DEFINE    V1DUNA        depuna
108300030604     C     *LIKE         DEFINE    V1DUDT        depudt
108400040914     C     *LIKE         DEFINE    V1DIDP        depidp
108500050111     C     *LIKE         DEFINE    V1DDST        depdst
108600050111     C     *LIKE         DEFINE    V1DOBJM       depobjm
108700030509     C     *LIKE         DEFINE    MALDAOR       KEYDAOR
108800050808     C     *LIKE         DEFINE    MALSTS        KEYSTS
108900050808     C     *LIKE         DEFINE    §CM1PO        KEYPO
109000021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
109100991108     C*---
109200970526     C* VARIABILI DI CONTROLLO
109300970526     C*---
109400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
109500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
109600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
109700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
109800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
109900050110     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
110000970526     C*
110100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
110200030509     C                   MOVEL     'TRTCM3R'     DSMSPG
110300030509     C                   MOVEL     'TRTCM3R'     MSGQUE
110400970526     C                   MOVEL     *BLANKS       MSGKEY
110500970526     C                   MOVEL     *BLANKS       DSMSMS
110600970526     C                   MOVEL     *BLANKS       DSMSKE
110700970526     C                   Z-ADD     *ZEROS        DSMSER
110800970526     C*---
110900970526     C* CHIAVI DI LETTURA
111000970526     C*---
111100050808     C* LETTURA timal02l - PARZIALE
111200050808     C     KEYMAL2P      KLIST
111300050808     C                   KFLD                    KEYDAOR
111400050808     C* LETTURA timal04l - COMPLETA
111500050808     C     KEYMAL4       KLIST
111600050808     C                   KFLD                    KEYSTS
111700050808     C                   KFLD                    KEYDAOR
111800050808     C* LETTURA timal05l - COMPLETA
111900050808     C     KEYMAL5       KLIST
112000050808     C                   KFLD                    KEYPO
112100050808     C                   KFLD                    KEYDAOR
112200970526     C*
112300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
112400970526     C                   EXSR      CHKPAR
112500991115     C*
112600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
112700970526     C                   EXSR      CHKFOP
112800970526     C*
112900970526     C                   ENDSR
113000970526     C*------------------------------------------------------------------------*
113100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
113400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
113500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
113600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113800970526     C*------------------------------------------------------------------------*
113900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
114000970526     C*------------------------------------------------------------------------*
114100970526     C     POSSFL        BEGSR
114200970526     C*
114300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
114400970526     C                   EXSR      CLRSFL
114500970526     C*
114600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
114700970526     C                   EXSR      SETFIL
114800970526     C*
114900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
115000970526     C     SFLEOF        IFEQ      0
115100970526     C                   EXSR      CARSFL
115200030305     C                   ENDIF
115300970526     C*
115400970526     C                   ENDSR
115500970526     C*------------------------------------------------------------------------*
115600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
115700970526     C*------------------------------------------------------------------------*
115800970526     C     ENTATT        BEGSR
115900970526     C*
116000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
116100970526     C                   Z-ADD     0             OPZOK             1 0
116200970526     C*
116300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
116400970526     C                   Z-ADD     0             SFLVAR            5 0
116500970526     C*
116600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
116700970526     C                   Z-ADD     0             SFLERR
116800970526     C*
116900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
117000970526     C                   Z-ADD     SFLULT        WFLULT
117100970526     C*
117200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
117300030509     C                   READC     TCM3D05                                98
117400970526     C     *IN98         DOWEQ     '0'
117500970526     C*
117600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
117700970526     C                   ADD       1             SFLVAR
117800970526     C*
117900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
118000970526     C     OPZOK         IFEQ      0
118100970526     C*
118200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
118300970526     C     OPZKEY        IFNE      *BLANKS
118400970526     C                   EXSR      OPZCON
118500970526     C                   END
118600970526     C*
118700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
118800970526     C*            2. PULIZIA SCELTA IN SFL
118900970526     C     OPZOK         IFEQ      0
119000970526     C                   MOVEL     '0'           *IN41
119100970526     C                   MOVEL     *BLANKS       OPZKEY
119200970526     C                   ELSE
119300970526     C*
119400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
119500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
119600970526     C                   MOVEL     '1'           *IN41
119700970526     C                   Z-ADD     SFLULT        SFLERR
119800970526     C                   END
119900970526     C                   ELSE
120000970526     C*
120100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
120200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
120300970526     C                   MOVEL     '1'           *IN41
120400970526     C                   END                                                    OPZOK=0
120500970526     C*
120600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
120700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
120800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
120900970526     C     OPZOK         IFNE      0
121000970526     C                   Z-ADD     SFLERR        SFLNBR
121100970526     C                   ELSE
121200970526     C                   Z-ADD     SFLULT        SFLNBR
121300970526     C                   END
121400970526     C*
121500970526     C* AGGIORNO RECORD SFL
121600030509     C                   UPDATE    TCM3D05
121700970526     C*
121800030509     C                   READC     TCM3D05                                98
121900970526     C                   END                                                    DO WHILE
122000970526     C*
122100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
122200970526     C                   Z-ADD     WFLULT        SFLULT
122300970526     C*
122400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
122500970526     C     SFLVAR        IFEQ      0
122600970526     C                   END
122700991112     C*
122800970526     C                   ENDSR
122900970526     C*------------------------------------------------------------------------*
123000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
123100970526     C*------------------------------------------------------------------------*
123200970526     C     CLRSFL        BEGSR
123300970526     C*
123400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
123500970526     C* (0=NON FINE FILE  1=FINE FILE)
123600970526     C                   Z-ADD     0             SFLEOF
123700970526     C*
123800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
123900970526     C                   Z-ADD     0             SFLULT
124000970526     C*
124100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
124200970526     C                   MOVEL     '1'           *IN40
124300030509     C                   WRITE     TCM3D06
124400970526     C                   MOVEL     '0'           *IN40
124500970526     C*
124600970526     C                   ENDSR
124700970526     C*------------------------------------------------------------------------*
124800970526     C* CARSFL - CARICAMENTO SFL
124900970526     C*------------------------------------------------------------------------*
125000970526     C     CARSFL        BEGSR
125100970526     C*
125200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
125300970526     C     SFLULT        IFEQ      0
125400970526     C                   EXSR      SETFIL
125500970526     C                   END
125600970526     C*
125700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
125800970526     C                   Z-ADD     SFLULT        WFLULT
125900970526     C*
126000970526     C* CICLO:
126100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
126200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
126300970526     C                   Z-ADD     1             SFLRIG
126400030509     C     SFLRIG        DOWLE     SFLPAG
126500030509     C     SFLEOF        ANDEQ     0
126600970526     C*
126700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
126800991116     C                   MOVEL     *BLANKS       OPZKEY
126900970526     C*
127000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
127100970526     C                   EXSR      IRESFL
127200970526     C*
127300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
127400970526     C                   ADD       1             SFLULT
127500970526     C*
127600970526     C* SCRITTURA RECORD SFL
127700030509     C                   WRITE     TCM3D05
127800970526     C                   ADD       1             SFLRIG
127900970526     C*
128000970526     C* LETTURA FILE DA CARICARE
128100970526     C                   EXSR      LETFIL
128200970526     C                   END                                                    DO WHILE
128300970526     C*
128400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
128500970526     C     WFLULT        IFNE      SFLULT
128600970526     C                   Z-ADD     WFLULT        SFLNBR
128700970526     C                   ADD       1             SFLNBR
128800970526     C                   END
128900970526     C*
129000970526     C                   ENDSR
129100970526     C*------------------------------------------------------------------------*
129200970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
129300970526     C*------------------------------------------------------------------------*
129400970526     C     OUTSFL        BEGSR
129500970526     C*
129600970526     C* WRITE RECORD TESTATA VIDEO
129700030509     C                   WRITE     TCM3D01
129800970526     C*
129900970526     C* VISUALIZZAZIONE MESSAGGIO
130000970526     C                   EXSR      SNDMSG
130100970526     C*
130200970526     C* WRITE RECORD OPZIONI VIDEO
130300030509     C                   WRITE     TCM3D02
130400970526     C*
130500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
130600030509     C                   WRITE     TCM3D03
130700970526     C*
130800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
130900030509     C                   WRITE     TCM3D04
131000970526     C*
131100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
131200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
131300970526     C     SFLEOF        IFEQ      0
131400970526     C                   MOVEL     '0'           *IN42
131500970526     C                   ELSE
131600970526     C*
131700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
131800970526     C                   MOVEL     '1'           *IN42
131900970526     C                   END
132000970526     C*
132100970526     C* WRITE RECORD TASTI FUNZIONALI
132200030509     C                   WRITE     TCM3D09
132300970526     C*
132400970526     C* CASO DI SFL VUOTO
132500970526     C     SFLULT        IFLE      0
132600970526     C*
132700970526     C*    - WRITE RECORD VUOTO
132800030509     C                   WRITE     TCM3D0A
132900030509     C                   READ      TCM3D0A                                99
133000970526     C*
133100970526     C                   ELSE
133200970526     C*
133300970526     C* CASO DI SFL CON RECORDS
133400970526     C*
133500970526     C*    - WRITE RECORD CONTROLLO SFL
133600030509     C                   WRITE     TCM3D06
133700030509     C                   READ      TCM3D06                                99
133800970526     C                   END
133900970526     C*
134000970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
134100030509     C                   READ      TCM3D03                                99
134200970526     C*
134300970526     C                   ENDSR
134400970526     C*------------------------------------------------------------------------*
134500970526     C* SNDMSG - MANDA MESSAGGIO
134600970526     C*------------------------------------------------------------------------*
134700970526     C     SNDMSG        BEGSR
134800970526     C*
134900970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
135000970526     C                   MOVEL     '1'           *IN40
135100030509     C                   WRITE     TCM3DMC
135200970526     C                   MOVEL     '0'           *IN40
135300970526     C*
135400970526     C* VISUALIZZAZIONE MESSAGGIO
135500970526     C                   Z-ADD     0             MSGULT
135600970526     C                   CALL      'XMSG'        XMSGPA
135700970526     C     DSMSER        IFEQ      0
135800970526     C                   MOVEL     DSMSKE        MSGKEY
135900970526     C                   END
136000970526     C                   Z-ADD     1             MSGULT
136100030509     C                   WRITE     TCM3DMS                                      SFL MESS
136200030509     C                   WRITE     TCM3DMC                                      CONTR MESS
136300970526     C*
136400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
136500970526     C                   MOVEL     *BLANKS       DSMSMS
136600970526     C*
136700970526     C                   ENDSR
136800970526     C*------------------------------------------------------------------------*
136900970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
137000970526     C*------------------------------------------------------------------------*
137100970526     C     FUNCON        BEGSR
137200970526     C*
137300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
137400970526     C                   EXSR      FUNVAR
137500970526     C*
137600970526     C* PULISCO CAMPI DI LAVORO
137700970526     C                   Z-ADD     0             I                 5 0
137800970526     C                   Z-ADD     0             W                 5 0
137900970526     C*
138000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
138100970526     C     FUNKEY        IFNE      *BLANKS
138200970526     C     1             DO        24            I
138300970526     C                   MOVEL     FUD(I)        FUDRIG
138400970526     C*
138500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
138600970526     C     FUNKEY        IFEQ      FUDFUN
138700970526     C     FUDATT        ANDEQ     'S'
138800970526     C                   Z-ADD     I             W
138900970526     C                   END
139000970526     C                   END                                                    DO WHILE
139100970526     C*
139200970526     C* FUNZIONE NON ATTIVA (ERRORE)
139300970526     C     W             IFEQ      0
139400970526     C*
139500970526     C* EMISSIONE MESSAGGIO DI ERRORE
139600970526     C                   MOVEL     MSG(2)        DSMSMS
139700970526     C                   END                                                    W=0
139800970526     C                   END                                                    FUNKEY NE *BLANKS
139900970526     C*
140000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
140100970526     C     W             IFNE      0
140200970526     C     FUNKEY        OREQ      *BLANKS
140300970526     C*
140400970526     C     FUNKEY        IFEQ      *BLANKS
140500970526     C                   EXSR      ENTGES
140600970526     C                   END
140700970526     C     FUNKEY        IFEQ      '03'
140800970526     C                   EXSR      F03GES
140900970526     C                   END
141000120510     C     FUNKEY        IFEQ      '04'
141100120510     C                   EXSR      F04GES
141200970526     C                   END
141300120510     C     FUNKEY        IFEQ      '05'
141400120510     C                   EXSR      F05GES
141500120510     C                   END
141600970526     C     FUNKEY        IFEQ      '12'
141700970526     C                   EXSR      F12GES
141800970526     C                   END
141900970528     C     FUNKEY        IFEQ      '21'
142000970528     C                   EXSR      F21GES
142100970528     C                   END
142200970528     C     FUNKEY        IFEQ      '22'
142300970528     C                   EXSR      F22GES
142400970528     C                   END
142500970526     C     FUNKEY        IFEQ      '23'
142600970526     C                   EXSR      F23GES
142700970526     C                   END
142800970526     C     FUNKEY        IFEQ      '24'
142900970526     C                   EXSR      F24GES
143000970526     C                   END
143100970526     C     FUNKEY        IFEQ      'RU'
143200970526     C                   EXSR      RUPGES
143300970526     C                   END
143400970526     C*
143500970526     C                   END
143600970526     C*
143700970526     C                   ENDSR
143800970526     C*------------------------------------------------------------------------*
143900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
144000970526     C*------------------------------------------------------------------------*
144100970526     C     FUNVAR        BEGSR
144200970526     C*
144300030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
144400970526     C     KEY           IFEQ      ENTER
144500970526     C                   MOVEL     *BLANKS       FUNKEY
144600970526     C                   END
144700970526     C     KEY           IFEQ      F1
144800970526     C                   MOVEL     '01'          FUNKEY
144900970526     C                   END
145000970526     C     KEY           IFEQ      F2
145100970526     C                   MOVEL     '02'          FUNKEY
145200970526     C                   END
145300970526     C     KEY           IFEQ      F3
145400970526     C                   MOVEL     '03'          FUNKEY
145500970526     C                   END
145600970526     C     KEY           IFEQ      F4
145700970526     C                   MOVEL     '04'          FUNKEY
145800970526     C                   END
145900970526     C     KEY           IFEQ      F5
146000970526     C                   MOVEL     '05'          FUNKEY
146100970526     C                   END
146200970526     C     KEY           IFEQ      F6
146300970526     C                   MOVEL     '06'          FUNKEY
146400970526     C                   END
146500970526     C     KEY           IFEQ      F7
146600970526     C                   MOVEL     '07'          FUNKEY
146700970526     C                   END
146800970526     C     KEY           IFEQ      F8
146900970526     C                   MOVEL     '08'          FUNKEY
147000970526     C                   END
147100970526     C     KEY           IFEQ      F9
147200970526     C                   MOVEL     '09'          FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F10
147500970526     C                   MOVEL     '10'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F11
147800970526     C                   MOVEL     '11'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F12
148100970526     C                   MOVEL     '12'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F13
148400970526     C                   MOVEL     '13'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F14
148700970526     C                   MOVEL     '14'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F15
149000970526     C                   MOVEL     '15'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      F16
149300970526     C                   MOVEL     '16'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      F17
149600970526     C                   MOVEL     '17'          FUNKEY
149700970526     C                   END
149800970526     C     KEY           IFEQ      F18
149900970526     C                   MOVEL     '18'          FUNKEY
150000970526     C                   END
150100970526     C     KEY           IFEQ      F19
150200970526     C                   MOVEL     '19'          FUNKEY
150300970526     C                   END
150400970526     C     KEY           IFEQ      F20
150500970526     C                   MOVEL     '20'          FUNKEY
150600970526     C                   END
150700970526     C     KEY           IFEQ      F21
150800970526     C                   MOVEL     '21'          FUNKEY
150900970526     C                   END
151000970526     C     KEY           IFEQ      F22
151100970526     C                   MOVEL     '22'          FUNKEY
151200970526     C                   END
151300970526     C     KEY           IFEQ      F23
151400970526     C                   MOVEL     '23'          FUNKEY
151500970526     C                   END
151600970526     C     KEY           IFEQ      F24
151700970526     C                   MOVEL     '24'          FUNKEY
151800970526     C                   END
151900970526     C     KEY           IFEQ      ROLLUP
152000970526     C                   MOVEL     'RU'          FUNKEY
152100970526     C                   END
152200970526     C     KEY           IFEQ      HELP
152300970526     C                   MOVEL     'HP'          FUNKEY
152400970526     C                   END
152500970526     C*
152600970526     C                   ENDSR
152700970526     C*------------------------------------------------------------------------*
152800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
152900970526     C*------------------------------------------------------------------------*
153000970526     C     FUNGES        BEGSR
153100970526     C*
153200970526     C* PULISCO CAMPI DI LAVORO
153300970526     C                   Z-ADD     0             W                 5 0
153400970526     C*
153500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
153600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
153700970526     C     1             DO        24            I
153800970526     C                   MOVEL     FUD(I)        FUDRIG
153900970526     C*
154000970526     C* PULIZIA FUNZIONI PRECEDENTI
154100970526     C                   MOVEL     *BLANKS       FUV(I)
154200970526     C*
154300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
154400970526     C* (SALTA F23 E F24)
154500970526     C     FUDATT        IFEQ      'S'
154600970526     C     FUDFUN        ANDNE     '23'
154700970526     C     FUDFUN        ANDNE     '24'
154800970526     C                   ADD       1             W
154900970526     C                   MOVEL     FUDDES        FUV(W)
155000970526     C                   END
155100970526     C                   END                                                    DO WHILE
155200970526     C*
155300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
155400970526     C* ALTRIMENTI DISATTIVO F24
155500970526     C     1             DO        24            I
155600970526     C                   MOVEL     FUD(I)        FUDRIG
155700970526     C*
155800970526     C     FUDFUN        IFEQ      '24'
155900970526     C     W             IFGT      6
156000970526     C                   MOVEL     'S'           FUDATT
156100970526     C                   ELSE
156200970526     C                   MOVEL     *BLANKS       FUDATT
156300970526     C                   END                                                    W GT 8
156400970526     C                   MOVEL     FUDRIG        FUD(I)
156500970526     C                   END
156600970526     C                   END                                                    DO WHILE
156700970526     C*
156800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
156900970526     C     W             DIV       6             I
157000970526     C                   MVR                     Z
157100970526     C*
157200970526     C     Z             IFNE      0
157300970526     C     I             ADD       1             Z
157400970526     C                   ELSE
157500970526     C                   Z-ADD     I             Z
157600970526     C                   END
157700970526     C*
157800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
157900970526     C                   ADD       1             FUNGRU
158000970526     C     FUNGRU        IFGT      Z
158100970526     C     FUNGRU        ORLT      1
158200970526     C                   Z-ADD     1             FUNGRU
158300970526     C                   END
158400970526     C*
158500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
158600970526     C     FUNGRU        IFEQ      1
158700970526     C                   MOVEL     FUV(1)        FUN001
158800970526     C                   MOVEL     FUV(2)        FUN002
158900970526     C                   MOVEL     FUV(3)        FUN003
159000970526     C                   MOVEL     FUV(4)        FUN004
159100970526     C                   MOVEL     FUV(5)        FUN005
159200970526     C                   MOVEL     FUV(6)        FUN006
159300970526     C                   END
159400970526     C*
159500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
159600970526     C     FUNGRU        IFEQ      2
159700970526     C                   MOVEL     FUV(7)        FUN001
159800970526     C                   MOVEL     FUV(8)        FUN002
159900970526     C                   MOVEL     FUV(9)        FUN003
160000970526     C                   MOVEL     FUV(10)       FUN004
160100970526     C                   MOVEL     FUV(11)       FUN005
160200970526     C                   MOVEL     FUV(12)       FUN006
160300970526     C                   END
160400970526     C*
160500970526     C* TERZO GRUPPO DI 6 FUNZIONI
160600970526     C     FUNGRU        IFEQ      3
160700970526     C                   MOVEL     FUV(13)       FUN001
160800970526     C                   MOVEL     FUV(14)       FUN002
160900970526     C                   MOVEL     FUV(15)       FUN003
161000970526     C                   MOVEL     FUV(16)       FUN004
161100970526     C                   MOVEL     FUV(17)       FUN005
161200970526     C                   MOVEL     FUV(18)       FUN006
161300970526     C                   END
161400970526     C*
161500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
161600970526     C     FUNGRU        IFEQ      4
161700970526     C                   MOVEL     FUV(19)       FUN001
161800970526     C                   MOVEL     FUV(20)       FUN002
161900970526     C                   MOVEL     FUV(21)       FUN003
162000970526     C                   MOVEL     FUV(22)       FUN004
162100970526     C                   MOVEL     *BLANKS       FUN005
162200970526     C                   MOVEL     *BLANKS       FUN006
162300970526     C                   END
162400970526     C*
162500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
162600970526     C                   MOVEL     *BLANKS       FUN007
162700970526     C                   MOVEL     *BLANKS       FUN008
162800970526     C*
162900970526     C     1             DO        24            I
163000970526     C                   MOVEL     FUD(I)        FUDRIG
163100970526     C     FUDFUN        IFEQ      '23'
163200970526     C     FUDATT        ANDEQ     'S'
163300970526     C                   MOVEL     FUDDES        FUN007
163400970526     C                   END
163500970526     C     FUDFUN        IFEQ      '24'
163600970526     C     FUDATT        ANDEQ     'S'
163700970526     C                   MOVEL     FUDDES        FUN008
163800970526     C                   END
163900970526     C                   END                                                    DO
164000970526     C*
164100970526     C                   ENDSR
164200970526     C*------------------------------------------------------------------------*
164300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
164400970526     C*------------------------------------------------------------------------*
164500970526     C     OPZGES        BEGSR
164600970526     C*
164700970526     C* PULISCO CAMPI DI LAVORO
164800970526     C                   Z-ADD     0             W                 5 0
164900970526     C*
165000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
165100970526     C* E CARICO TABELLA OPZIONI ATTIVE
165200991115     C     1             DO        24            I
165300970526     C                   MOVEL     OPD(I)        OPDRIG
165400970526     C*
165500970526     C* PULIZIA OPZIONI PRECEDENTI
165600970526     C                   MOVEL     *BLANKS       OPV(I)
165700970526     C*
165800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
165900970526     C     OPDATT        IFEQ      'S'
166000970526     C                   ADD       1             W
166100970526     C                   MOVEL     OPDDES        OPV(W)
166200970526     C                   END
166300970526     C                   END                                                    DO WHILE
166400970526     C*
166500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
166600970526     C* ALTRIMENTI DISATTIVO F23
166700991115     C     1             DO        24            I
166800970526     C                   MOVEL     FUD(I)        FUDRIG
166900970526     C*
167000970526     C     FUDFUN        IFEQ      '23'
167100970526     C     W             IFGT      8
167200970526     C                   MOVEL     'S'           FUDATT
167300970526     C                   ELSE
167400970526     C                   MOVEL     *BLANKS       FUDATT
167500970526     C                   END                                                    W GT 8
167600970526     C                   MOVEL     FUDRIG        FUD(I)
167700970526     C                   END
167800970526     C                   END                                                    DO WHILE
167900970526     C*
168000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
168100970526     C     W             DIV       8             I
168200970526     C                   MVR                     Z                 5 0
168300970526     C*
168400970526     C     Z             IFNE      0
168500970526     C     I             ADD       1             Z
168600970526     C                   ELSE
168700970526     C                   Z-ADD     I             Z
168800970526     C                   END
168900970526     C*
169000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
169100970526     C                   ADD       1             OPZGRU
169200970526     C     OPZGRU        IFGT      Z
169300970526     C     OPZGRU        ORLT      1
169400970526     C                   Z-ADD     1             OPZGRU
169500970526     C                   END
169600970526     C*
169700970526     C* PRIMO GRUPPO DI 8 OPZIONI
169800970526     C     OPZGRU        IFEQ      1
169900970526     C                   MOVEL     OPV(1)        OPZ001
170000970526     C                   MOVEL     OPV(2)        OPZ002
170100970526     C                   MOVEL     OPV(3)        OPZ003
170200970526     C                   MOVEL     OPV(4)        OPZ004
170300970526     C                   MOVEL     OPV(5)        OPZ005
170400970526     C                   MOVEL     OPV(6)        OPZ006
170500970526     C                   MOVEL     OPV(7)        OPZ007
170600970526     C                   MOVEL     OPV(8)        OPZ008
170700970526     C                   END
170800970526     C*
170900970526     C* SECONDO GRUPPO DI 8 OPZIONI
171000970526     C     OPZGRU        IFEQ      2
171100970526     C                   MOVEL     OPV(9)        OPZ001
171200970526     C                   MOVEL     OPV(10)       OPZ002
171300970526     C                   MOVEL     OPV(11)       OPZ003
171400970526     C                   MOVEL     OPV(12)       OPZ004
171500970526     C                   MOVEL     OPV(13)       OPZ005
171600970526     C                   MOVEL     OPV(14)       OPZ006
171700970526     C                   MOVEL     OPV(15)       OPZ007
171800970526     C                   MOVEL     OPV(16)       OPZ008
171900970526     C                   END
172000970526     C*
172100970526     C* TERZO GRUPPO DI 8 OPZIONI
172200970526     C     OPZGRU        IFEQ      3
172300970526     C                   MOVEL     OPV(17)       OPZ001
172400970526     C                   MOVEL     OPV(18)       OPZ002
172500970526     C                   MOVEL     OPV(19)       OPZ003
172600970526     C                   MOVEL     OPV(20)       OPZ004
172700970526     C                   MOVEL     OPV(21)       OPZ005
172800970526     C                   MOVEL     OPV(22)       OPZ006
172900970526     C                   MOVEL     OPV(23)       OPZ007
173000970526     C                   MOVEL     OPV(24)       OPZ008
173100970526     C                   END
173200970526     C*
173300970526     C                   ENDSR
173400970526     C*------------------------------------------------------------------------*
173500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
173600970526     C*------------------------------------------------------------------------*
173700970526     C     OPZCON        BEGSR
173800970526     C*
173900970526     C* PULISCO CAMPI DI LAVORO
174000970526     C                   Z-ADD     0             I                 5 0
174100970526     C                   Z-ADD     0             W                 5 0
174200970526     C*
174300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174400970526     C     1             DO        24            I
174500970526     C                   MOVEL     OPD(I)        OPDRIG
174600970526     C*
174700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
174800970526     C     OPZKEY        IFEQ      OPDOPZ
174900970526     C     OPZKEY        OREQ      OPDOP1
175000970526     C*
175100970526     C     OPDATT        IFEQ      'S'
175200970526     C                   MOVEL     OPDOPZ        OPZKEY
175300970526     C                   Z-ADD     I             W
175400970526     C                   END
175500970526     C                   END
175600970526     C*
175700970526     C                   END                                                    DO WHILE
175800970526     C*
175900970526     C* OPZIONE NON ATTIVA (ERRORE)
176000970526     C     W             IFEQ      0
176100970526     C*
176200970526     C* OPZIONE NON OK
176300970526     C                   Z-ADD     1             OPZOK
176400970526     C*
176500970526     C* EMISSIONE MESSAGGIO DI ERRORE
176600970526     C                   MOVEL     MSG(1)        DSMSMS
176700970526     C                   ELSE
176800970526     C*
176900970526     C* OPZIONE ATTIVA (PROCEDI ...)
177000970526     C*
177100021211     C                   MOVEL     *BLANKS       PMSG
177200030509     C     OPZKEY        IFEQ      ' 2'
177300030509     C                   EXSR      O02GES
177400030509     C                   END
177500031210     C     OPZKEY        IFEQ      ' 4'
177600031210     C                   EXSR      O04GES
177700031210     C                   END
177800970526     C     OPZKEY        IFEQ      ' 5'
177900970526     C                   EXSR      O05GES
178000970526     C                   END
178100031210     C     OPZKEY        IFEQ      ' 6'
178200031210     C                   EXSR      O06GES
178300031210     C                   END
178400141210     C     OPZKEY        IFEQ      '10'
178500141210     C                   EXSR      O10GES
178600141210     C                   END
178700071001     C     OPZKEY        IFEQ      '11'
178800071001     C                   EXSR      O11GES
178900071001     C                   END
179000031211     C     OPZKEY        IFEQ      '15'
179100031211     C                   EXSR      O15GES
179200031211     C                   END
179300021211     C                   MOVEL     PMSG          DSMSMS
179400021211     C                   END                                                    OPZ.NON ATTIVA
179500970526     C*
179600970526     C                   ENDSR
179700970526     C*------------------------------------------------------------------------*
179800970526     C* NOTE COSTRUZIONE SCHIERE
179900970526     C*------------------------------------------------------------------------*
180000970526     C*
180100970526     C* -MSG-MESSAGGI
180200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
180300970526     C* (NON TOCCARE MSG,1 E MSG,2)
180400970526     C*
180500970526     C* -CMD-COMANDI
180600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
180700970526     C*
180800970526     C* -OPD-OPZIONI
180900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
181000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
181100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
181200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
181300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
181400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
181500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
181600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
181700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
181800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
181900970526     C*
182000970526     C* -FUD-FUNZIONI
182100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
182200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
182500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
182600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
182700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
182800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
182900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
183000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
183100991108     C*
183200991115     C* -STS-CODICI STATI DEI RECORD DI LOG
183300991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
183400991110     C* ================> IMPORTANTE <=================
183500991115     C* per codici di errore in -STS- indicare in -DES- sempre
183600991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
183700991110     C* ERR
183800991108     C*
183900970526     O*------------------------------------------------------------------------*
184000970526** DEC - DECODIFICHE
184100970526    GESTIONE                                                     1
184200000407 INTERROGAZIONE                                                  2
1843000305093
1844009705264
1845009705265
184600970526** MSG - MESSAGGI
184700030509TRTCM3R- Opzione non gestita.                                                  1
184800030509TRTCM3R- Tasto funzionale non gestito.                                         2
184900030509TRTCM3R- Non abilitato all inserimento in filiale.                             3
185000030509TRTCM3R- Utente non abilitato all'opzione richiesta.                           4
185100031211TRTCM3R- File di spool non trovato o inesistente, verificare.                  5
185200050111TRTCM3R- Specificare almeno 5 caratteri per la ricerca.                        6
185300050224TRTCM3R- Operazione non consentita su log ancora da elaborare.                 7
185400030509TRTCM3R- 8                                                                     8
185500030509TRTCM3R- 9                                                                     9
185600030509TRTCM3R- 10                                                                   10
185700030509TRTCM3R- 11                                                                   11
185800030509TRTCM3R- 12                                                                   12
185900030509TRTCM3R- 13                                                                   13
186000030509TRTCM3R- 14                                                                   14
186100030509TRTCM3R- 15                                                                   15
186200030509TRTCM3R- 16                                                                   16
186300030509TRTCM3R- 17                                                                   17
186400030509TRTCM3R- 18                                                                   18
186500030509TRTCM3R- 19                                                                   19
186600030509TRTCM3R- 20                                                                   20
186700970528** CMD - COMANDI CL
186800021211
186900021211
187000991110
187100970528
187200970528
187300970526** OPD - OPZIONI
187400030509                   -  -     (S/N)=N                        opz. 01¬
187500000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
187600030509                   -  -     (S/N)=N                        opz. 03¬
187700031210 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
187800000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
187900071001 6=Rilascio        -6 -     (S/N)=N                        opz. 06¬
188000021211                   -  -     (S/N)=N                        opz. 07¬
188100991110                   -  -     (S/N)=N                        opz. 08¬
188200991110                   -  -     (S/N)=N                        opz. 09¬
18830014121010=Stato invio eml -10-     (S/N)=N                        opz. 10¬
18840007100111=Riesecuzione    -7 -     (S/N)=N                        opz. 11¬
188500970526                   -  -     (S/N)=N                        opz. 12¬
188600970526                   -  -     (S/N)=N                        opz. 13¬
188700000407                   -  -     (S/N)=N                        opz. 14¬
18880003121115=Visualizza spool-15-     (S/N)=N                        opz. 15¬
188900970526                   -  -     (S/N)=N                        opz. 16¬
189000970526                   -  -     (S/N)=N                        opz. 17¬
189100970526                   -  -     (S/N)=N                        opz. 18¬
189200970526                   -  -     (S/N)=N                        opz. 19¬
189300970526                   -  -     (S/N)=N                        opz. 20¬
189400970526                   -  -     (S/N)=N                        opz. 21¬
189500970526                   -  -     (S/N)=N                        opz. 22¬
189600970526                   -  -     (S/N)=N                        opz. 23¬
189700000407                   -  -     (S/N)=N                        opz. 25¬
189800970526** FUD - FUNZIONI
189900970526                   -        (S/N)=N                        funz.01¬
190000970526                   -        (S/N)=N                        funz.02¬
190100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
190200120510F04=Dlt.log prg=0  -        (S/N)=N                        funz.04¬
190300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
190400030509                   -        (S/N)=N                        funz.06¬
190500970526                   -        (S/N)=N                        funz.07¬
190600970527                   -        (S/N)=N                        funz.08¬
190700030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
190800970527                   -        (S/N)=N                        funz.10¬
190900970526                   -        (S/N)=N                        funz.11¬
191000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
191100991116                   -        (S/N)=N                        funz.13¬
191200970526                   -        (S/N)=N                        funz.14¬
191300970526                   -        (S/N)=N                        funz.15¬
191400970526                   -        (S/N)=N                        funz.16¬
191500970526                   -        (S/N)=N                        funz.17¬
191600970526                   -        (S/N)=N                        funz.18¬
191700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
191900000407                   -        (S/N)=N                        funz.21¬
192000991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
192100991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
192200991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
