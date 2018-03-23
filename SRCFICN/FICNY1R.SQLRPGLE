000100120724     H DECEDIT('0,') DATEDIT(*DMY.)
000200120731     h dftactgrp(*no) actgrp(*caller)
000300120731ab   H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000400950321?     *--------------------------------------------------------------*
000500120725?     *   PASSAGGIO FORNITORI AUT AD ALTRA SOCIETA OPERATIVA         *
000600950321?     *--------------------------------------------------------------*
000700120725      *
000800120725      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000900120725      *  03           PROTECT TUTTI I CAMPI
001000120725      *  09           PROTECT dei campi voce C/E
001100120725      *  21           GENERICO OPERAZIONI I/O
001200120725      *  22           GENERICO ERRORE OPERAZIONI I/O
001300120725      *  30           SFLDSP
001400120725      * N31           SFLCLR
001500120725      *  31           SFLDSPCTL
001600120725      *  32           SFLNXTCHG
001700120725      *  33           SFLEND
001800120725      *  39           OF PRTF
001900120725      *  40 <---> 49  DSPATR ERRORI SU SFL
002000120725    > *  Specificare l'uso dei singoli indicatori
002100120725      *  50 <---> 98  ERRORI SU VIDEO
002200120725    > *  Specificare l'uso dei singoli indicatori
002300120725      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002400120725      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002500120725      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002600120725      *  99           INDIC. GENERALE DI ERRORE
002700120725?     *--------------------------------------------------------------*
002800120725     FFicnY1D   CF   E             WORKSTN
002900120725     F                                     INFDS(DSFMT)
003000120724     Fansif01l  IF   E           K DISK    usropn
003100120725     Fazorg01L  IF   E           K DISK
003200120725     Fanrco98j  IF   E           K DISK
003300120725?     *--------------------------------------------------------------*
003400120725     D* Passaggio Parametri
003500120725     D KPJBA         E DS
003600120726     D KPJBus                              like(KPjbu)
003700120726     D FICNY1DS      E DS
003800120725      *-------------
003900120724     d cmd             s             50
004000120724     d cmd1            s             50    dim(01) ctdata perrcd(1)
004100120724     d cmd2            s             50    dim(01) ctdata perrcd(1)
004200120724ab   ***************************************************************************
004300120724ab   ** Prototipi.
004400120724ab   ***************************************************************************
004500120724ab    **
004600120724ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
004700120724ab    /DEFINE DFTACTGRP_NO
004800120724ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
004900120724ab    /UNDEFINE DFTACTGRP_NO
005000120724ab   **
005100120724ab   ***************************************************************************
005200120724ab   ** Strutture dati.
005300120724ab   ***************************************************************************
005400120724      * - Parametri x Controllo profilo utenti
005500120724     d TIBS34ds      e ds
005600120724      * - Ds di riferimento al file esterno AZUTE00F
005700120724     d AZUTEds       e ds                  extname(AZUTE00F)
005800120724     d dUTE01        e ds
005900120724      * - Ds per dati organigramma
006000120724     d DDatiUte      e ds
006200120726      * Passaggio Parametri al pgm FNLV26R
006300120726     D FNLV26DS      e ds                  inz
006700120724     D tibs36ds      E DS
006800120724RM*  D L1              S              3  0 DIM(30) ASCEND
006900120724RM*  D L6              S              3  0 DIM(30) ASCEND
007000120724     D*
007100120724     D* DS PER TRUL06R - CARICAMENTO £X
007200120724     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007300120724RM*  D  LIN                    1     90  0
007400120724     D                                     DIM(30)
007500120724      *
007600120724      * DS per inversione data
007700120724     D WLBDAT          DS                  INZ
007800120724     D  G02DAT                 1      8  0
007900120724     D  G02INV                 9     16  0
008000120724     D  G02ERR                17     17
008100120724     D  G02TGI                18     22  0
008200120724ab   **
008300120725     D*-------------
008400120725     D  S_TASTO        S                   like($Tasto)
008500120725     D DSFMT           DS           512
008600120725     D  $TASTO               369    369
008700120725     D  NRG                  370    370
008800120725     D  NCL                  371    371
008900120725     D  SFLNRR               378    379B 0
009000120725     D*-------------
009100120725     D* Nome PGM a video
009200120725     D                 DS
009300120725     D  PROGR                  1     10
009400120725     D  ASTER1                 1      1    INZ('*')
009500120725     D  SIGLA                  2      9
009600120725     D  ASTER2                10     10    INZ('*')
009700120725     D*-------------
009800120725     D* Reperimento nome PGM
009900120725     D STATUS         SDS           333
010000120725     D  DSPGM            *PROC
010100120724ab   ***************************************************************************
010200120725ab   **       Campi
010300120724ab   ***************************************************************************
010400120724ab   D TibsSof_esito...
010500120724ab   D                 S             10I 0 IMPORT
010600120724ab   D idSocieta...
010700120724EDPDCD                 S              3A
010800120724ab   **
010900120724EDPDCd dtVALIDITA      s               D
011000120726EDPDCd dtVALIDITAiniz  s               D
011100120726EDPDCd dataISO         s               D
011200120724EDPDCd VALdatFINE      s               D
011300120724EDPDCd VALdatINIZ      s               D
011400120726EDPDCd Data_FINE       s               D
011500120726EDPDCd Data_INIZIO     s               D
011600120725ab   D PART_IVA_old    s             16A
011700120725ab   D PART_IVA_new    s             16A
011800120725ab   **
011900120726EDPDCd Nrec_TRS        s              9S 0
012000120725EDPDCd dtvideo_scad    s               D
012100120726EDPDCd data_decorrenz  s              8S 0
012200120726EDPDCd data_scadenza   s              8S 0
012300120725EDPDCd AlfaFiliale     s              3A
012400120725EDPDCd Filiale         s              3S 0
012500120725EDPDCd Filiale_old     s              3S 0
012600120725EDPDCd Filiale_new     s              3S 0
012700120726EDPDCd Cod_Forn_old    s              8A
012800120726EDPDCd Cod_Forn_new    s              8A
012900120726EDPDCd Unita_prefer    s              8A
013000120725EDPDCd Soc_Fornitore   s              3A
013100120725EDPDCd Cod_Fornitore   s              8A
013200120725EDPDCd Des_Fornitore   s             40A
013300120725EDPDCd Filiale_errata  s              1A
013400120725EDPDCd PIVA_errata     s              1A
013500120726EDPDCd SOC_errata      s              1A
013600120726EDPDCd FornFatt_errato...
013700120726EDPDCd                 s              1A
014000120727EDPDCd aggiorna_FNY2_errato...
014100120727EDPDCd                 s              1A
014200120727EDPDCd aggiorna_70SR3_errato...
014300120727EDPDCd                 s              1A
014301130212EDPDCd Wdatiso         s               D
014400120725ab   ***************************************************************************
014500120725     D* COSTANTI
014600120725ab   ***************************************************************************
014700120725     D* Tasti di funzione
014800120725     D F01             C                   CONST(X'31')
014900120725     D F02             C                   CONST(X'32')
015000120725     D F03             C                   CONST(X'33')
015100120725     D F04             C                   CONST(X'34')
015200120725     D F05             C                   CONST(X'35')
015300120725     D F06             C                   CONST(X'36')
015400120725     D F07             C                   CONST(X'37')
015500120725     D F08             C                   CONST(X'38')
015600120725     D F09             C                   CONST(X'39')
015700120725     D F10             C                   CONST(X'3A')
015800120725     D F11             C                   CONST(X'3B')
015900120725     D F12             C                   CONST(X'3C')
016000120725     D F13             C                   CONST(X'B1')
016100120725     D F14             C                   CONST(X'B2')
016200120725     D F15             C                   CONST(X'B3')
016300120725     D F16             C                   CONST(X'B4')
016400120725     D F17             C                   CONST(X'B5')
016500120725     D F18             C                   CONST(X'B6')
016600120725     D F19             C                   CONST(X'B7')
016700120725     D F20             C                   CONST(X'B8')
016800120725     D F21             C                   CONST(X'B9')
016900120725     D F22             C                   CONST(X'BA')
017000120725     D F23             C                   CONST(X'BB')
017100120725     D F24             C                   CONST(X'BC')
017200120725     D ENTER           C                   CONST(X'F1')
017300120725     D ROLDWN          C                   CONST(X'F4')
017400120725     D ROLLUP          C                   CONST(X'F5')
017500120725     D DATA            C                   CONST('0001-01-01')
017600120725     D NO              C                   CONST('N')
017700120725     D SI              C                   CONST('S')
017800061012     c*---------------------------------------------------------------*
017900061012     c* FLUSSO PRINCIPALE                                             *
018000061012     c*---------------------------------------------------------------*
018100120725     C* inizializzazione variabili
018200120725     C                   EXSR      INZVAR
018300120725     C*
018400120725     C     $FINE         DOWEQ     *OFF
018500120725     C     $GEST         CASEQ     'D1'          GESD1
018600120725     C                   END
018700120725     C                   END
018800120725     C* fine programma
018900120726     C                   clear                   KPJBU
019000120726     C                   MOVEL     FICNY1ds      KPJBU
019100061012     C                   SETON                                        LR
019200120725     C************************************************************
019300120725     C* INIZIALIZZAZIONE VARIABILI
019400120725     C************************************************************
019500120725     C     INZVAR        BEGSR
019600120725     C*
019700120725     C     Krco          KLIST
019800120725     C                   KFLD                    Soc_Fornitore
019900120725     C                   KFLD                    rcosnatura
020000120725     C                   KFLD                    rcoksc
020100120725     c                   eval      rcosnatura = 'F'
020200120725     C*
020300120725     C* Pulizia campi e indicatori
020400120725     C                   MOVE      *ALL'0'       IN5098           49
020500120725     C                   MOVEA     IN5098        *IN(50)
020600120725     C*
020700120725     C* Variabili per gestione videate
020800120725     C                   MOVE      'D1'          $GEST             2
020900120725     C                   MOVE      *OFF          $FINE             1
021000120725     C                   MOVE      *ON           $INZD1            1
021100120725     C                   Z-ADD     0             $ULKD1            3 0
021200120725     C*
021300120725     C* Variabili appoggio
021400120725     C                   Z-ADD     0             CURR              2 0
021500120725     C                   Z-ADD     0             CURC              2 0
021600120725     C                   MOVE      *ZEROS        WIN              99
021700120725     C* Indici
021800120725     C                   Z-ADD     0             X                 3 0
021900120725     C                   Z-ADD     0             Y                 3 0
022000120725     C*
022100120725     C* Valorizzazione campi univoci testate
022200120725     C                   CLEAR                   T1
022300120725     C                   MOVEL     KNSIF         t1NOMSIF
022400120725     C                   MOVEL     DSPGM         t1NOMPGM
022500120725     C                   MOVEL     RSUT          t1RSUT
022600120726     C                   WRITE     T1
022700120725     C*
022800120725     C* Inizializzazione/Decodifica prima videata
022900120725     C                   EXSR      INZD1
023000120725     C                   MOVE      *OFF          $INZD1
023100120725     c*
023200120725     C                   ENDSR
023300120725     C************************************************************
023400120725     C* GESTIONE VIDEO RECORD D1
023500120725     C************************************************************
023600120725     C     GESD1         BEGSR
023700120725      *
023800120725     C* inizializzazione videata
023900120725     C     $INZD1        IFEQ      *ON
024000120725     C                   EXSR      INZD1
024100120725     C                   MOVE      *OFF          $INZD1
024200120725     C                   END
024300120725      *
024400120725     c                   SETOFF                                         99
024500120725      *
024800120725     C* Video Primo
024900120725     C*               *----------------*
025000120725     C                   EXFMT     D1
025100120725     C*               *----------------*
025200120725     C* F3=Fine
025300120725     C     $TASTO        ifeq      F03
025400120725     C                   EXSR      F03D1
025500120725     c                   end
025600120725      *
025700120725     C                   EXSR      CTRD1
025800120725     C*
025900120725     C* F6 eseguo Call x Aggiornare
0260001207252    C  N99$TASTO        ifeq      F06
026100120725    >C                   EXSR      LANCIA_FNY2
026200120725     c                   end
027100120725     C*
027200120725     C                   ENDSR
027300120725     C************************************************************
027400120725     C* Window di ALERT x NON ABILITATO alla funzione
027500120725     C************************************************************
027600120725     C     ALERT_W1      BEGSR
027700120727      **
027800120727     c                   clear                   w1
027900130212     c                   eval      W1ERR2 = '                         '
028100120725     C                   WRITE     D1
028200120725     C                   EXFMT     W1
028300120725     C                   EXSR      F03D1
028400120727      **
028500120725     C                   ENDSR
028600120726     C/EJECT
028700120726      ************************************************************
028800120726      * INIZIALIZZAZIONE VIDEATA DATI
028900120726      ************************************************************
029000120726     C     INZD1         BEGSR
029100120726     C*
029200120726     C                   CLEAR                   D1
029300120726     C* riempio la videata
029400120726     C                   EXSR      RIED1
029500120726      *
029600120726     C                   MOVE      *ALL'0'       IN5098
029700120726     C                   MOVEA     IN5098        *IN(50)
029800120726     C*
029900120726     C                   ENDSR
030000120725     C************************************************************
030100120725     C* RIEMPIMENTO VIDEATA  D1
030200120725     C************************************************************
030300120725     C     RIED1         BEGSR
030400120725      **
030500120725      **
030600120725     C                   ENDSR
030700120726     C/EJECT
030800120725     C************************************************************
030900120725     C* GESTIONE F03 VIDEO D1
031000120725     C************************************************************
031100120725     C     F03D1         BEGSR
031200120725     C                   MOVE      *ON           $FINE
031300120725     C                   ENDSR
031400120725     C************************************************************
031500120725     C* CONTROLLO VIDEATA
031600120725     C************************************************************
031700120725     C     CTRD1         BEGSR
031800120725     C*
031900120725     C                   SETOFF                                       99
032000120725     c                   clear                   D1DESOLD
032100120725     c                   clear                   D1DESNEW
032200120726     c                   clear                   Data_Scadenza
032300120725      *
032400120725     C* non imposto pos. del cursore
032500120725     C                   Z-ADD     0             H1RIGA
032600120725     C                   Z-ADD     0             H1COLO
032700120726      *
032800120726     C* --------------------------
032900120726     C*   Data DECORRENZA nuova Società x il Nuovo FORNITORE
033000120726     C*
033100120726     C* Controlla la data di inizio validità
033200120726$017 C                   IF        D1datNEW = 0 or
033300120726$017 C                             D1datNEW = 01010001
033400120726     C                   SETON                                        6199
033500120726     c                   goto      Error_D1
033600120726     C                   Else
033700120726      **
033800120726     C                   Z-ADD     D1datNEW      G02DAT
033900120726     C                   MOVEL     *blank        G02ERR
034000120726     C                   CALL      'XSRDA8'
034100120726     C                   PARM                    WLBDAT
034200120726     C     G02ERR        IFEQ      '1'
034300120726     C                   SETON                                        6199
034400120726     c                   goto      Error_D1
034500120726     C                   ENDIF
034600120726      *
034700120726     c*  dalla data a video tolgo 1gg.
034800120726     C                   Z-ADD     G02dat        D1datNEW
034900120726     C                   move      G02INV        data_decorrenz
035000120726     C     *iso          move      G02INV        dtVALIDITAiniz
035100120726     C     dtVALIDITAinizsubdur    1:*d          dataISO
035200120726     C                   move      dataISO       data_scadenza
035300120726      **
035400120726     C                   End
035500120725     C*
035600120726     C* --------------------------
035700120726     C*   SOCIETA   vecchia
035800120725     C* controllo codice vuoto
035900120726$017 C                   IF        D1SOCOLD = *blank or
036000120726$017 C                             D1SOCOLD = *zeros
036100120725     C                   SETON                                        5199
036200120725     c                   goto      Error_D1
036300120725      *
036400120725     C                   Else
036500120725      *
036600120725     c* controllo società old
036700120725     C     D1SOCOLD      CHAIN     ansif01l
036800120725     c                   if        not %found(ansif01l)
036900120725     c                              or
037000120725     c                             %found(ansif01l) and SIFDTFIVL <>
037100120725     c                             *loval
037200120725     C                   SETON                                        5199
037300120725     c                   goto      Error_D1
037400120725     c                   end
037500120725     C                   ENDIF
037600120725     C*
037700120726     C* --------------------------
037800120726     C*   FORNITORE vecchio
037900120726$017 C                   IF        D1KSCOLD = *zeros
038000120725     C                   SETON                                        5299
038100120725     c                   goto      Error_D1
038200120726     C*
038300120725     c                   else
038400120726     C*
038500120725     c                   move      *all'0'       Cod_fornitore
038600120725     c                   move      D1kscOLD      Cod_fornitore
038700120726     c                   move      Cod_fornitore Cod_Forn_old
038800120725      *
038900120725      **  controlla la Società operativa se corretta prima del Fornitore
039000120725$017 C                   eval      AlfaFiliale = %subst(Cod_fornitore:2:3)
039100120725$017 C                   move      AlfaFiliale   Filiale
039200120725     c                   eval      Soc_Fornitore = D1socOLD
039300120726      *
039400120726      *  data video -1 giorno
039500120726     C     *iso          move      data_scadenza dtVALIDITA
039600120725     c                   exsr      chk_SOC_ope
039700120725      *
039800120725      *  società NON Operativa x la filiale
039900120726     C                   if        D1socOLD <> idSocieta
040000120726     c                             or  Soc_errata = SI
040100120725     C                   SETON                                        5199
040200120725     c                   goto      Error_D1
040300120726    2C                   ElsE
040400120726     c                   eval      Data_FINE = valDATfine
040500120726    2C                   END
040600120725      *
040700120725     c                   exsr      ctrl_PROJ
040800120725      *  Se controllo su PROJ ha dato esito errato
040900120725     c                   IF        PIVA_errata = si
041000120725     C                   SETON                                        5899
041100120725     c                   goto      Error_D1
041200120725     c                   else
041300120725$017 C                   eval      PART_IVA_old = sogpartiva
041400120726     c                   eval      D1DESOLD     = sogDES
041500120725    2C                   END
041600120725      *
041700120725     C                   ENDIF
041800120725     C*
041900120726     C* --------------------------
042000120726     C*   SOCIETA   nuova
042100120726$017 C                   IF        D1SOCNEW = *blank or
042200120726$017 C                             D1SOCNEW = *zeros
042300120725     C                   SETON                                        5399
042400120725     c                   goto      Error_D1
042500120725     C                   Else
042600120725     c* controllo società old
042700120725     C     D1SOCnew      CHAIN     ansif01l
042800120725     c                   if        not %found(ansif01l)
042900120725     c                              or
043000120725     c                             %found(ansif01l) and SIFDTFIVL <>
043100120725     c                             *loval
043200120725     C                   SETON                                        5399
043300120725     c                   goto      Error_D1
043400120725     c                   end
043500120725     C                   ENDIF
043600120725     C*
043700120726     C* --------------------------
043800120726     C*   FORNITORE nuovo
043900120726$017 C                   IF        D1KSCNEW = *zeros
044000120725     C                   SETON                                        5499
044100120725     c                   goto      Error_D1
044200120726     C*
044300120725     c                   else
044400120725     C*
044500120725     c                   move      *all'0'       Cod_fornitore
044600120725     c                   move      D1kscNEW      Cod_fornitore
044700120726     c                   move      Cod_fornitore Cod_Forn_new
044800120725      *
044900120725      **  controlla la Società operativa se corretta prima del Fornitore
045000120725$017 C                   eval      AlfaFiliale = %subst(Cod_fornitore:2:3)
045100120725$017 C                   move      AlfaFiliale   Filiale
045200120725     c                   eval      Soc_Fornitore = D1socNEW
045300120726      *
045400120726      *   data video --> data_decorrenza
045500120726     C     *iso          move      data_decorrenzdtVALIDITA
045600120725     c                   exsr      chk_SOC_ope
045800120725      *  società NON Operativa x la filiale
045900120725     C                   if        D1socNEW <> idSocieta
046000120726     c                             or  Soc_errata = SI
046100120725     C                   SETON                                        5399
046200120725     c                   goto      Error_D1
046300120726    2C                   Else
046400120726     c                   eval      Data_INIZIO = valDATiniz
046500120726      **
046600120726      *   Controlla ordinamento dei periodi delle 2 società
046700120726     c                   if        DATA_FINE > DATA_INIZIO
046800120726     C                   SETON                                        6799
046900120726     c                   goto      Error_D1
047000120726     c                   end
047100120726      *
047200120726      *   Controlla conincidenza con la Decorrenza della nuova società
047300120726     c                   if        VALdatINIZ <> dtVALIDITAiniz
047400120726     C                   SETON                                        6199
047500120726     c                   goto      Error_D1
047600120726     c                   end
047700120726      *
047800120726    2C                   END
047900130213      * verifica validita + o - 15 gg da decorrenza nuova società
047901130213     c     valdatiniz    subdur    wdatiso       ggdif:*d          5 0
047902130213     c                   if        ggdif > 15  or
047903130213     c                             ggdif < -15
047904130213     c                   seton                                        5099
047905130213     c                   goto      error_d1
047906130213     c                   endif
047907130213      *
048000120725     c                   exsr      ctrl_PROJ
048100120725      *  Se controllo su PROJ ha dato esito errato
048200120725     c                   IF        PIVA_errata = si
048300120725     C                   SETON                                        5999
048400120725     c                   goto      Error_D1
048500120725     c                   else
048600120725$017 C                   eval      PART_IVA_new = sogpartiva
048700120726$017 C                   eval      D1DESNEW     = sogDEs
048800120725    2C                   END
048900120725      *
049000120725     C                   ENDIF
049100120726      *
049200120726     C* --------------------------
049300120725     C*  Filiale
049400120725     C*    dal codice Fornitore
049500120726     c                   move      Cod_Forn_OLD  Cod_fornitore
049600120725$017 C                   eval      AlfaFiliale = %subst(Cod_fornitore:2:3)
049700120725$017 C                   move      AlfaFiliale   Filiale
049800120725$017 C                   eval      Filiale_errata = NO
049900120725     C*
050000120725     C*  CONTROLLI sui codici immessi relativi alle filiali
050100120725     c                   IF        DUTlpo = '2' or DUTlpo = *blanks
050200120726     C*
050300120725     C* deve essere in £6 se secondo livello
050400120725     C     Filiale       LOOKUP    L6                                     21
050500120725$017 C  n21              eval      Filiale_errata = SI
050600120725      *
050700120725     C                   elseIF    DUTlpo = '1'
050800120726     C*
050900120725     C* deve essere in £1 se primo livello
051000120725     C     Filiale       LOOKUP    L1                                     21
051100120725$017 C  n21              eval      Filiale_errata = SI
051200120725      *
051300120725     C                   elseIF    DUTlpo = 'S'
051400120726     C*
051500120725     C* deve essere in azorg01l
051600120725     C     Filiale       chain     azorg01l
051700120725     c                   if        %found(azorg01l) and
051800120725     c                             orgfag <> 'A' and orgfag <> 'F'
051900120725     c                               or
052000120725     c                             NOT %found(azorg01l)
052100120725$017 C                   eval      Filiale_errata = SI
052200120725     c                   end
052300120725     C                   ENDif
052400120725      *
052500120725     C* Se il Codice Fornitore NON appartiene alla Filiale da Gestire
052600120725$017 C                   If        Filiale_errata = SI
052700120725     C                   SETON                                        5799
052800120725     c                   goto      Error_D1
052900120725     C                   ENDif
053000120725     C*
053100120726     C* --------------------------
053200120725     C*  I CODICI forinitori devono appartenere alla stessa filiale oltre
053300120725     C*     che avere la stessa P.IVA.
053400120726     c                   move      Cod_Forn_OLD  Cod_fornitore
053500120725$017 C                   eval      AlfaFiliale = %subst(Cod_fornitore:2:3)
053600120725$017 C                   move      AlfaFiliale   Filiale_old
053700120725      *
053800120726     c                   move      Cod_Forn_NEW  Cod_fornitore
053900120725$017 C                   eval      AlfaFiliale = %subst(Cod_fornitore:2:3)
054000120725$017 C                   move      AlfaFiliale   Filiale_new
054100120725     C*
054200120725$017 C                   IF        Filiale_old <> Filiale_new
054300120725     C                   SETON                                        5599
054400120725     c                   goto      Error_D1
054500120725     C                   ENDIF
054600120725     C*
054700120726     C* --------------------------
054800120725     C*  Controlla P.IVA : deve essere uguale fra i 2 Fornitori
054900120725     C*
055000120725$017 C                   IF        PART_IVA_old
055100120725     c                                <>
055200120725$017 C                             PART_IVA_new
055300120725     C                   SETON                                        5699
055400120725     c                   goto      Error_D1
055500120725     C                   ENDIF
055600120726      *
055700120726      * --------------------------
055800120726      *  Deve trovare anche il contratto  sulla vecchia società
055900120726     c                   z-add     0             NRec_TRS
056000120726     C/EXEC SQL
056100120726     C+ SELECT  rrn(AITRS00F) into :NRec_TRS
056200120726     C+   FROM  AITRS00F
056300120731     C+  WHERE  TRSdfc  = 0
056400120726     C+    and  TRSSOCG = :d1socOLD
056500120726     C+    and  TRSIVA  = :PART_IVA_old
056600120726     C/END-EXEC
056700120726      *
056800120726     c                   if        sqlcod <> 0  or NRec_TRS = 0
056900120726     C                   SETON                                        60  99
057000120726     c                   goto      Error_D1
057100120726     C                   ENDIF
057200120726      ***
057300120726     C* --------------------------
057400120726     C* Controlla il Fornitore di Fatturazione
057500120726     C* --------------------------
057600120726     C                   exSR      Ctrl_ForFATT
057700120726     c                   if        FornFatt_errato = SI
057800120726     C                   SETON                                        62  99
057900120726     c                   goto      Error_D1
058000120726     C                   ENDIF
058100120726     C*
058200120726     C* --------------------------
058300120726     C*  Controlli su tariffe con Decorrenza FUTURA già inserite
058400120726     C* --------------------------
058500120726     C*  CITTA'
058600120726     C* Testate FGT oltre data decorrenza
058700120726     C                   move      data_decorrenzdatadecalfa       8
058800120726     c                   call      'FICNY1R1'
058900120726     C                   PARM                    d1socOLD
059000120726     C                   PARM                    Cod_Forn_OLD
059100120726     C                   PARM                    datadecalfa
059200120726     C                   PARM                    ESITO_ALF         9
059300120726     C*
059400120726     c                   if        ESITO_ALF <> *blank
059500120726     C                   SETON                                        63  99
059600120726     c                   goto      Error_D1
059700120726     C                   ENDIF
059800120726     C*
059900120726     C* Testate TGT oltre data decorrenza
060000120726     c                   call      'FICNY1R2'
060100120726     C                   PARM                    d1socOLD
060200120726     C                   PARM                    Cod_Forn_OLD
060300120726     C                   PARM                    datadecalfa
060400120726     C                   PARM                    ESITO_ALF
060500120726     C*
060600120726     c                   if        ESITO_ALF <> *blank
060700120726     C                   SETON                                        63  99
060800120726     c                   goto      Error_D1
060900120726     C                   ENDIF
061000120726     C*
061100120726     C* Testate TGT oltre data decorrenza
061200120726     c                   call      'FICNY1R3'
061300120726     C                   PARM                    d1socOLD
061400120726     C                   PARM                    Cod_Forn_OLD
061500120726     C                   PARM                    datadecalfa
061600120726     C                   PARM                    ESITO_ALF
061700120726     C*
061800120726     c                   if        ESITO_ALF <> *blank
061900120726     C                   SETON                                        64  99
062000120726     c                   goto      Error_D1
062100120726     C                   ENDIF
062200120726     C*
062300120726     C* --------------------------
062400120726     C*  AFFLUENZE/DEFLUENZE
062500120726     C*  NON DEVE essere un 2°LIVELLO per Affl.Defl.
062600120726     c                   IF        DUTlpo <>'2' and DUTlpo <>*blanks
062700120726     C*
062800120726     c                   call      'FICNY1R4'
062900120726     C                   PARM                    d1socOLD
063000120726     C                   PARM                    Cod_Forn_OLD
063100120726     C                   PARM                    datadecalfa
063200120726     C                   PARM                    ESITO_ALF         9
063300120726     C*
063400120726     c                   if        ESITO_ALF <> *blank
063500120726     C                   SETON                                        65  99
063600120726     c                   goto      Error_D1
063700120726     C                   ENDIF
063800120726     C*
063900120726     c                   call      'FICNY1R5'
064000120726     C                   PARM                    d1socOLD
064100120726     C                   PARM                    Cod_Forn_OLD
064200120726     C                   PARM                    datadecalfa
064300120726     C                   PARM                    ESITO_ALF
064400120726     C*
064500120726     c                   if        ESITO_ALF <> *blank
064600120726     C                   SETON                                        66  99
064700120726     c                   goto      Error_D1
064800120726     C                   ENDIF
064900120726     C*
065000120726     c                   end
065100120726     C*
065200120726     C* per eventuali ERRORI arriva direttamente a fine Routine
065300120725     C* ___________________________
065400120725     c     ERROR_D1      tag
065500120725     C*
065600120726     C* x Errori cosa FARE:
065700120726     C*
065800120726     C     *IN99         IFEQ      *ON
065900120726     C     $GEST         ANDEQ     'D1'
066000120726     C                   MOVEA     *IN           WIN              99
066100120726     C                   MOVE      *ALL'0'       IN5098           49
066200120726     C                   MOVEA     IN5098        *IN(50)
066300120726     C                   WRITE     D1
066400120726     C                   MOVEA     WIN           *IN
066500120726     C                   ENDIF
066600120726     C*
066700120726     C*
066800120725     C                   ENDSR
066900120725     C/EJECT
067000120725     C**************************************************************************
067100120725     C*    CONTROLLo fornitore in proj
067200120725     C**************************************************************************
067300120725     C     ctrl_PROJ     BEGSR
067400120725      *
067500120725     c                   eval      PIVA_errata = no
067600120725      *
067700120725     c                   move      *all'0'       rcoksc
067800120725     c                   move      cod_Fornitore rcoksc
067900120725     c                   clear                   Unita_prefer
068000120725     c                   eval      Unita_prefer = %subst(rcoksc:2:3)
068100120725      *
068200120725     C     Krco          CHAIN     anrco98j
068300120725      *
068400120725     c                   if        %Found(anrco98j)
068500120725     C                   MOVEL(p)  sogdes        DES_Fornitore
068600120725      *
068700120725     c                   if        rcounita <> Unita_prefer
068800120725     c                   eval      PIVA_errata = si
068900120725     c                   end
069000120725      *
069100120725     c                   if        SOGCDFISC = ' ' or SOGPARTIVA = ' '
069200120725     c                   eval      PIVA_errata = si
069300120725     c                   end
069400120731     c                   else
069500120731     c                   eval      PIVA_errata = si
069600120731     c                   move      *all'?'       des_fornitore
069700120725      *
069800120725     c                   end
069900120725      *
070000120725    2C                   ENDsr
070100120725     C/EJECT
070200120725     C************************************************************
070300120725    >C* AGGIORNAMENTO e sottomissione elaborazione
070400120725     C************************************************************
070500120725    >C     LANCIA_FNY2   BEGSR
070600120727      *
070700120725      *  se di ritorno dalla chiamata al FNY2 è andato tutto bene
070800120725      *   allora si deve proseguire con il TRMZ70SR3
070900120725      *  che provvede ad eseguire il cambio società sugli autisti.
071000120726     c                   clear                   FICNY1DS
071100120726     c                   eval        NY1SOCO  = d1socOLD
071200120726     c                   eval        NY1FRNO  = Cod_Forn_OLD
071300120726     c                   eval        NY1SOCN  = d1socNEW
071400120726     c                   eval        NY1FRNN  = Cod_Forn_NEW
071500120726     c                   eval        NY1DATA  = data_decorrenz
071600120726     c                   eval        NY1ERR   = *blank
071700120726     c                   call      'FICNY2R'
071800120726     c                   parm                    FICNY1DS
071900120725      *
072000120727     c                   COMMIT
072100120727      ***
072200120727      *   chiamando la Routine TRMZ70SR3 aggiorna la Società Automezzi SEDE
072300120727      *
072400120727     c                   eval        aggiorna_70SR3_errato = no
072500120726      * Parametri:
072600120726      *   x cambio P.IVA/cambio Società/Data Fine Contratto
072700120726      *    Tipo aggiornamento = ""S" cambio Società
072800120726      *    Data Inizio        = La data di inizio dell'emissione contratto sulla nuova società
072900120726      *    Data Fine          = La data di fine contratto per la vecchia società
073000120726      *    Società            = Codice Nuova Società da Aprire
073100120726      *    Filiale            = La filiale della Nuova Società
073200120726      *    Partita IVA        = Del fornitore
073300120726      *
073400120726     C                   eval         S3_Tipo_Aggior = 'S'
073500120726     C                   eval         S3_Data_Fine   = data_scadenza
073600120726     C                   eval         S3_Data_Inizio = data_decorrenz
073700120726     C                   eval         S3_Societa     = d1socNEW
073800120726     C                   eval         S3_DesSocieta  = d1desNEW
073900120726     C                   eval         S3_PartitaIva  = part_IVA_new
074000120726     C                   eval         S3_Filiale     = filiale_new
074100120726     C                   eval         S3_NRec_TRS    = NRec_TRS
074200120726     C                   eval         S3_Errore      = *blank
074300120726     c                   eval      KPJBus = kpjbu
074400120726     C                   clear                   KPJBU
074500120726     C                   call      'TRMZ70SR3'
074600120726     c                   parm                    kpjba
074700120726     C                   parm                    S3_Tipo_Aggior    1
074800120726     C                   parm                    S3_Data_Fine      8 0
074900120726     C                   parm                    S3_Data_Inizio    8 0
075000120726     C                   parm                    S3_Societa        3
075100120726     C                   parm                    S3_DesSocieta    45
075200120726     C                   parm                    S3_PartitaIva    16
075300120726     C                   parm                    S3_Filiale        3 0
075400120726     C                   parm                    S3_NRec_TRS       9 0
075500120726     C                   parm                    S3_Errore         1
075600120726     C                   eval       KPJBu = KPJBus
075700120726      *
075800120726      *  Errore che deve innescare il ROLBACK
075900120726     C                   if        S3_Errore = *on
076000120726     c                   seton                                        99
076100120727     c                   eval        aggiorna_70SR3_errato = si
076200120727     c                   end
076300120727     C*
076400120727      *   GESTIONE COMMIT o ROLLBACK in funzione di eventuali errori o meno
076500120727     c                   if          aggiorna_70SR3_errato = no
076600120727      *
076700120727     c                   COMMIT
076800120727     c                   clear                   w1
076900120727     c                   eval      W1ERR2 = 'TUTTO O K. Il Fornitore -
077000120727     c                              è STATO AGGIORNATO correttamente!!'
077100120727     c                   else
077200120727      *
077300120727     c                   ROLBK
077400120727     c                   clear                   w1
077500120727     c                   eval      W1ERR1 = 'ATTENZIONE il Contratto -
077600120727     c                              NON è STATO AGGIORNATO !!'
077700120727     c                   eval      W1ERR2 = 'AVVISARE Ufficio Automez-
077800120727     c                             zi SEDE x controlla la Soc.:' +
077900120727     c                             d1SOCold
078000120727     c                   eval      W1ERR3 =
078100120727     c                             'e tutti gli autisti a questa coll-
078200120727     c                             egati.'
078300120727     c                   end
078400120727      *
078500120727     C                   EXFMT     W1
078600120727     C                   MOVE      *ON           $inzD1
078700120725      *
078800120725     C                   ENDSR
078900120724     C**************************************************************************
079000120724     C*     R O U T I N E     I N I Z I A L E
079100120724     C**************************************************************************
079200071113     c     *inzsr        begsr
079300120724     c*--
079400061012     c*  Operazioni iniziali
079500061012     C     *ENTRY        PLIST
079600061012     C                   PARM                    KPJBA
079700120724     c
079800120726     c                   clear                   ficny1DS
079900120724     c                   clear                   KPJBU
080000120724      *
080100120724      * Reperisco dati job
080200120724     c                   exsr      DatiJob
080300120725      *
080400120724     c                   if        simfel = 0
080500120724     c                   z-add     utefil        simfel
080600120724     c                   endif
080700120725      *
080800120725      *  se in ambiente di prova
080900120724     c                   if        %subst(knsif:7:1) = 'P'
081000120724     C                   movea     cmd2(1)       cmd
081100120724     c                   else
081200120724     C                   movea     cmd1(1)       cmd
081300120724     c                   endif
081400120725      *  esegue il comando ovrdbf
081500120724     C                   movel(p)  cmd           comman
081600120724     C                   call      'QCMDEXC'
081700120724     C                   parm                    comman           80
081800120724     C                   parm      48            lung             15 5
081900120724     C*  poi lo apre
082000120724     c                   open      ansif01l
082100120724     c*
082200120724     C* CARICO LA TABELLA '£1'
082300120724     C                   MOVE      '£1'          D06COD
082400120724     C                   MOVEL     SIMFEL        D06KEY
082500120726     c                   eval      KPJBus = kpjbu
082600120726     C                   clear                   KPJBU
082700120724     C                   MOVEL     DSUL06        KPJBU
082800120724     C                   CALL      'TRUL06R'
082900120724     C                   PARM                    KPJBA
083000120724     C                   MOVEL     KPJBU         DSUL06
083100120726     c                   eval      KPJBu  = kpjbus
083200120724     C                   MOVEA     LIN           L1
083300120724     c*
083400120724RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
083500120724     c                   if            DUTlpo = '2'
083600120724     c                             or  DUTlpo = *blanks
083700120724      **
083800120724RM*  C                   MOVE      '£6'          D06COD
083900120724RM*  c                   movel     DUTpou        D06KEY
084000120726     c                   eval      KPJBus = kpjbu
084100120726     C                   clear                   KPJBU
084200120724RM*  C                   MOVEL     DSUL06        KPJBU
084300120724RM*  C                   CALL      'TRUL06R'
084400120724RM*  C                   PARM                    KPJBA
084500120724RM*  C                   MOVEL     KPJBU         DSUL06
084600120726     c                   eval      KPJBu  = kpjbus
084700120724RM*  C                   MOVEA     LIN           L6
084800120724RM*  c                   move      DUTpou        CDU               3 0
084900120724RM*  C                   ELSE
085000120724RM*  C                   MOVE      SIMFEL        CDU
085100120724RM*  C                   END
085500120724      *
085600120724      * Data odierna in formato AAAAMMGG
085700120724     C                   clear                   WLBDAT
085800120724     C                   move      *date         G02dat
085900120724     C                   call      'XSRDA8'
086000120724     C                   parm                    WLBDAT
086100120725     C                   move      G02inv        wDATE             8 0
086200120724     C                   move      G02inv        wDATEc            8
086201130212     C                   move      G02inv        wDATiso
086300120724ab   C*     Inizializza
086400120724ab    /FREE
086500120724ab     TibsSof_Init();
086600120724ab    /END-FREE
086700061012     c                   endsr
086800120724     C**************************************************************************
086900120724      * Reperimento Dati del job (Utente/Operativi)                   *
087000120724     C**************************************************************************
087100120724     c     DatiJob       BEGSR
087200120724      *
087300120724     c     *dtaara       define    §azute        azuteds
087400120724     c     *dtaara       define    §datiute      ddatiute
087500120724      *
087600120724     c                   in(E)     *dtaara
087700120724     c                   IF        %ERROR or RSUT = *blanks
087800120724     c                   clear                   Tibs34Ds
087900120724     c                   call      'TIBS34R'
088000120724     c                   parm                    Tibs34Ds
088100120724     c                   in        *dtaara
088200120724     c                   ENDIF
088300120724      *
088400120724      *
088500120724     c                   ENDSR
093500120724ab   C**************************************************************************
093600120726ab    * Controlla la Società operativa della Filiale
093700120724ab   C**************************************************************************
093800120724ab   C     CHK_SOC_OPE   begsr
093900120724ab   **
094000120726ab   **   con la Data Validità impostata a Video x nuova società
094100120726ab   **       oppure -1 giorno x vecchia società.
094200120726     c                   eval      soc_errata = NO
094300120724ab    /FREE
094400120724ab
094500120724EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
094600120724EDPDC                                             : dtVALIDITA
094700120724EDPDC                                             : 'O'
094800120724EDPDC                                             : valDatIniz
094900120724EDPDC                                             : valDatFine
095000120724ab                                                );
095100120724ab
095200120724EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
095300120724EDPDC    // Gestire l'errore.
095400120726
095500120726         eval  soc_errata = SI;
095600120726
095700120724ab     ENDIF;
095800120724ab
095900120724ab    /END-FREE
096000120724ab   C                   ENDSR
096100120726     C**************************************************************************
096200120726     C*  Controlla il Fornitore di Fatturazione
096300120726     C**************************************************************************
096400120726     C     Ctrl_ForFATT  BEGSR
096500120726     C*
096600120726     c                   eval      FornFatt_errato = NO
096700120726     C*  controlla e aggiorna il fornitore di fatturazione
096800120726     c                   clear                   fnlv26ds
096900120726     c                   eval      d26soc = d1socNEW
097000120726     c                   eval      d26ksc = Cod_Forn_NEW
097100120726     c                   eval      KPJBus = kpjbu
097200120726     C                   clear                   KPJBU
097300120726     c                   movel(p)  fnlv26ds      kpjbu
097400120726     c                   call      'FNLV26R'
097500120726     c                   parm                    kpjba
097600120726     c                   movel     kpjbu         fnlv26ds
097700120726     c                   eval      KPJBu  = kpjbus
097800120726     c                   if        d26err <> ' '
097900120726     c                   eval      FornFatt_errato = SI
098000120726     c                   end
098100120726     c*
098200120726     C                   ENDSR
098300120724     C**************************************************************************
098400120724** cmd1
098500120724OVRDBF FILE(ansif01l) TOFILE(pjbargru/ansif01l)
098600120724** cmd2
098700120724OVRDBF FILE(ansif01l) TOFILE(gaitrased/ansif01l)
