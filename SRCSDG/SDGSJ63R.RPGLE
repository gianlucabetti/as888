000100050622     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200050622     H*PARMS BNDDIR(PJXBND) CVTOPT(*NONE)
000300050622     H DECEDIT('0,') DATEDIT(*DMY.)
000400050622      *
000500150112      *          Dichiarazioni di intento - invio telematico
000600150112      *                    Stampa file generato
000700050622      *           ----------------------------------------
000800050622      *
000900050622      *
001000050622     F*----------------------------------------------------*
001100150112     FSDGSJ63D  CF   E             WORKSTN
001200150112     F                                     RENAME(SJ63D1:FMTD1)
001300150112     F                                     RENAME(SJ63T1:FMTT1)
001400150112     F                                     RENAME(SJ63Z1:FMTZ1)
001500050622    >F* eventuali schermi supplementari
001600150112     F                                     RENAME(SJ63D2:FMTD2)
001700050622    >F* modifica valori assunti
001800150112     F                                     RENAME(SJ63D3:FMTD3)
001900150112     F                                     RENAME(SJ63Z3:FMTZ3)
002000050622     F                                     INFDS(DSFMT)
002100050622     D*----------------------------------------------------*
002200050622     D* Schiere per la gestione dei tasti di funzione
002300050622     D $FM             S             27    DIM(24)
002400050622     D $FX             S             50    DIM(24)
002500050622     D $FC             S              1    DIM(24)
002600050622     D $FR             S              1    DIM(24)
002700050622     D* Video D1/D2
002800050622     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002900050622     D $FX1            S             50    DIM(24) ALT($FM1)
003000050622     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003100050622     D $FR1            S              1    DIM(24) ALT($FC1)
003200050622     D* Video D3 Valori assunti
003300050622     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
003400050622     D $FX3            S             50    DIM(24) ALT($FM3)
003500050622     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
003600050622     D $FR3            S              1    DIM(24) ALT($FC3)
003700050622     D*----------------------------------------------------*
003800050622     D* Passaggio Parametri
003900050622     D KPJBA         E DS
004000050622     D*-------------
004100050622     D* DS Esterna gestione messaggi per COMIT
004200150112     D SDGMSGDS      E DS                  INZ
004300050622     D  STS          E                     EXTFLD(MSGSTS)
004400050622     D                                     DIM(333)
004500050622     D  JBA          E                     EXTFLD(MSGJBA)
004600050622     D                                     DIM(502)
004700050622     D*-------------
004800050622     D*Asteriscare se non presenti
004900050622     D* Reperimento Autorizzazioni
005000050622     D*AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005100050622     D*-------------
005200050622     D* Dati per pgm XNOAUT
005300050622     D*XNOADS        E DS                  EXTNAME(XNOAUTDS)
005400050622     D*-------------
005500050622     D* Passaggio Autorizzazioni
005600050622     D*DSAUT           DS           512
005700050622     D*-------------
005800050622     D* Maschera dati autorizzazioni
005900050622    >D*xxxDS         E DS                  EXTNAME(xxxxxxDS)
006000050622     D*-------------
006100050622     D* Valori assunti
006200050622     D XMPEDS        E DS                  INZ
006300050622     D XMPEDS1       E DS                  INZ
006400050622     D DSMPE           DS                  INZ
006500050622     D  $VAL                          1
006600050711     D  $File                        10A
006700150112     D  $TipoStp                      1A
006800050622     D*-------------
006900050622     D* Reperimento nome PGM
007000050622     D STATUS         SDS           333
007100050622     D  DSPGM            *PROC
007200050622     D  PARMS            *PARMS
007300050622     D*-------------
007400150112     D* Dati di ambiente ottenuti da SDGSOCDR
007500050622    >D SOC001        E DS                  EXTNAME(XSOC001DS)
007600050622     D*-------------
007700150112     D* DS Interna per dati di output di SDGSOCDR
007800050622     D XSOCDS          DS          1000
007900050622     D*-------------
008000050622     D* Ricezione Parametri
008100150112    >D SdgSJ63DS     E DS
008200150112    >D  ERRJ63       E                     INZ('0')
008300050622     D*-------------
008400050622     D* Passaggio Parametri al PGM di estrazione dati
008500150112    >D SDGSJ64DS     E DS
008600150112    >D  ERRJ64       E                     INZ('0')
008700050622     D*-------------
008800050622     D* Catalogo file per scarico su supporto magnetico
008900050622     D ANA122DS      E DS
009000050622     D*-------------
009100050622     D* Parametri modulo SCARSUMA
009200050622     D ANP022DS      E DS
009300150112     D*-------------
009400150112     D X07VALDS      E DS
009500150112     D*-------------
009600150112     D* Ricerca legende
009700150112     D Ana964DS      E DS
009800050622     D*-------------
009900050622     D DSFMT           DS
010000050622     D  $TASTO               369    369
010100050622     D  NRG                  370    370
010200050622     D  NCL                  371    371
010300050622     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
010400050622     D*-------------
010500050622     D* posizione cursore
010600050622     D CURSOR          DS
010700050622     D  RIRI                   1      2B 0 INZ
010800050622     D  R$$                    2      2
010900050622     D  COCO                   3      4B 0 INZ
011000050622     D  C$$                    4      4
011100050622     D*-------------
011200050622     D* Tasti di funzione
011300050622     D F01             C                   CONST(X'31')
011400050622     D F02             C                   CONST(X'32')
011500050622     D F03             C                   CONST(X'33')
011600050622     D F04             C                   CONST(X'34')
011700050622     D F05             C                   CONST(X'35')
011800050622     D F06             C                   CONST(X'36')
011900050622     D F07             C                   CONST(X'37')
012000050622     D F08             C                   CONST(X'38')
012100050622     D F09             C                   CONST(X'39')
012200050622     D F10             C                   CONST(X'3A')
012300050622     D F11             C                   CONST(X'3B')
012400050622     D F12             C                   CONST(X'3C')
012500050622     D F13             C                   CONST(X'B1')
012600050622     D F14             C                   CONST(X'B2')
012700050622     D F15             C                   CONST(X'B3')
012800050622     D F16             C                   CONST(X'B4')
012900050622     D F17             C                   CONST(X'B5')
013000050622     D F18             C                   CONST(X'B6')
013100050622     D F19             C                   CONST(X'B7')
013200050622     D F20             C                   CONST(X'B8')
013300050622     D F21             C                   CONST(X'B9')
013400050622     D F22             C                   CONST(X'BA')
013500050622     D F23             C                   CONST(X'BB')
013600050622     D F24             C                   CONST(X'BC')
013700050622     D ENTER           C                   CONST(X'F1')
013800050622     D ROLDWN          C                   CONST(X'F4')
013900050622     D ROLLUP          C                   CONST(X'F5')
014000050622     I*-------------
014100050622     I* Sottotitolo di testata
014200050622    >I* se non si hanno sottotitoli, mettere tutta la costante = ' '
014300050622     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
014400050622     D*                                    COSxxxx')
014500050622     D SOTTIT          C                   CONST('                            ')
014600050622     D*-----------
014700050622     D* Campi di lavoro
014800050622     D CallPgm         S              1
014900050622     D*----------------------------------------------------------------
015000050622     D* Ds contenente i valori del msg PRO1797
015100050622     D §Fil_Lib        DS
015200050622     D  §File                        10A
015300050622     D  §Libreria                    10A
015400050622     C*----------------------------------------------------*
015500050622     C*                MAIN LINE PROGRAM
015600050622     C*----------------------------------------------------*
015700050622     C*
015800050622     C* inizializzazione variabili
015900050622     C                   EXSR      INZVAR
016000050622     C*
016100050622     C     $FINE         DOWEQ     *OFF
016200050622     C     $GEST         CASEQ     'D1'          GESD1
016300050622    >C     $GEST         CASEQ     'D2'          GESD2
016400050622     C     $GEST         CASEQ     'D3'          GESD3
016500050622     C                   ENDCS
016600050622     C                   ENDDO
016700050622     C*
016800050622     C* fine programma
016900050622     C                   EXSR      ENDPGM
017000050622     C/EJECT
017100050622     C************************************************************
017200050622     C* RICERCHE
017300050622     C************************************************************
017400050622     C     ENDPGM        BEGSR
017500050622     C*
017600050622     C*
017700050622     C* se ho confermato l'uscita proseguo
017800050622     C     $FINE         IFEQ      *ON
017900050622     C*
018000050622     C                   SETON                                            LR
018100050622     C                   RETURN
018200050622     C*
018300050622     C                   ENDIF
018400050622     C*
018500050622     C                   ENDSR
018600050622     C/EJECT
018700050622     C************************************************************
018800050622     C* RICERCHE
018900050622     C************************************************************
019000050622     C     SEARCH        BEGSR
019100050622     C*
019200050622     C* determino Riga/Colonna del cursore
019300050622     C                   MOVE      NRG           R$$
019400050622     C                   MOVE      NCL           C$$
019500050622     C                   Z-ADD     RIRI          CURR
019600050622     C                   Z-ADD     COCO          CURC
019700050622     C*
019800050622     C* I campi che al momento dell'F4 risultano protetti non devono
019900050622     C* essere abilitati alla funzione di ricerca
020000050622     C*
020100050622     C                   SELECT
020200050711     C*
020300050622     C*
020400050622     C* FMTD1  - file su supporto magnetico
020500050622     C     H1NMFL        WHENEQ    'D1FILE'
020600050622     C                   Reset                   ANA122DS
020700050622     C                   Move      '01'          Opz122
020800050711     C                   Move      XScSoc        Societa122
020900150112     C                   Move      XScSoc        Soc122
021000150112     C                   Move      'DF'          TpScar122
021100050622     C                   Clear                   KPJBU
021200050622     C                   Movel     ANA122DS      KPJBU
021300050622     C                   Call      'ANA122R'
021400050622     C                   Parm                    KPJBA
021500050622     C                   Movel     KPJBU         ANA122DS
021600050622     C*
021700050622     C                   if        File122  <>   *Blanks
021800050622     C                   MoveL     File122       D1File
021900050622     C                   endif
022000150112     C*
022100150112     C     H1NMFL        WHENEQ    'D1TIPOSTP'
022200150112     C                   EXSR      F4_TIPOSTP
022300050622     C*
022400050622     C* ricerca non abilitata in questa posizione
022500050622     C                   OTHER
022600050622     C                   SETON                                        9899
022700050622     C*
022800050622     C                   ENDSL
022900050622     C*
023000050622     C* imposto pos. del cursore
023100050622     C                   Z-ADD     CURR          H1RIGA
023200050622     C                   Z-ADD     CURC          H1COLO
023300050622     C*
023400050622     C                   MOVE      *HIVAL        $LASTV
023500050622     C*
023600050622     C                   ENDSR
023700050622     C/EJECT
023800150112     C************************************************************
023900150112     C* Ricerca tipologia stampa
024000150112     C************************************************************
024100150112     C     F4_TipoStp    BEGSR
024200150112     C*
024300150112     C                   Reset                   ANA964DS
024400150112     C*
024500150112     C                   MoveL     '01'          Opz964
024600150112     C                   MoveL     'SDGSJ63S'    Trc964
024700150112     C                   MoveL     'TIPOSTPJ63'  Cam964
024800150112     C                   MoveL     *Off          All964
024900150112     C*
025000150112     C                   MoveL     ANA964DS      KPJBU
025100150112     C                   CALL      'ANA964R'
025200150112     C                   PARM                    KPJBA
025300150112     C                   MoveL     KPJBU         ANA964DS
025400150112     C*
025500150112     C                   if        Ret964     <> '2'
025600150112     C                   MoveL     Val964        D1TIPOSTP
025700150112     C                   endif
025800150112     C*
025900150112     C                   ENDSR
026000050622     C************************************************************
026100050622     C* GESTIONE EMISSIONE TASTI FUNZIONE
026200050622     C************************************************************
026300050622     C     GESPIE        BEGSR
026400050622     C*
026500050622     C                   SELECT
026600050622     C*
026700050622     C     $GEST         WHENEQ    'D1'
026800050622     C     $ULKD1        IFEQ      0
026900050622     C* riempimento opzioni
027000050622     C                   EXSR      F24D1
027100050622     C                   ELSE
027200050622     C                   WRITE     FMTZ1
027300050622     C                   ENDIF
027400050622     C*
027500050622     C     $GEST         WHENEQ    'D2'
027600050622     C     $ULKD1        IFEQ      0
027700050622     C* riempimento opzioni
027800050622     C                   EXSR      F24D1
027900050622     C                   ELSE
028000050622     C                   WRITE     FMTZ1
028100050622     C                   ENDIF
028200050622     c*
028300050622     C* Modifica valori assunti
028400050622     C     $GEST         WHENEQ    'D3'
028500050622     C     $ULKD3        IFEQ      0
028600050622     C                   EXSR      F24D3
028700050622     C                   ELSE
028800050622     C                   WRITE     FMTZ3
028900050622     C                   ENDIF
029000050622     C                   ENDSL
029100050622     C*
029200050622     C                   ENDSR
029300050622     C/EJECT
029400050622     C************************************************************
029500050622     C* GESTIONE VIDEO RECORD D1
029600050622     C************************************************************
029700050622     C     GESD1         BEGSR
029800050622     C* inizializzazione videata
029900050622   B1C     $INZD1        IFEQ      *ON
030000050622     C                   EXSR      INZD1
030100050622     C                   MOVE      *OFF          $INZD1
030200050622   E1C                   ENDIF
030300050622     C* emissione piede videata se proveniente da altra
030400050622     C* salvataggio valore formato di provenienza
030500050622   B1C     $LASTV        IFNE      'D1'
030600050622     C                   WRITE     FMTT1
030700050622     C                   EXSR      GESPIE
030800050622     C                   MOVE      $LASTV        $LASTG
030900050622     C                   MOVE      'D1'          $LASTV
031000050622   E1C                   ENDIF
031100050622     C*
031200050622     C* se esistono errori sulla videata
031300050622     C* emetto la write del formato a indicatori spenti per vedere
031400050622     C* le eventuali decodifiche
031500050622   B1C     *IN99         IFEQ      *ON
031600050622     C     $GEST         ANDEQ     'D1'
031700050622     C                   MOVEA     *IN           WIN
031800050622     C                   MOVE      *ALL'0'       IN5098           49
031900050622     C                   MOVEA     IN5098        *IN(50)
032000050622     C                   WRITE     FMTD1
032100050622     C                   MOVEA     WIN           *IN
032200050622   E1C                   ENDIF
032300050622     C*
032400050622     C                   EXFMT     FMTD1
032500050622     C* controllo abilitazione tasti funzione
032600050622     C*
032700050622     C                   EXSR      CTRKD1
032800050622     C*
032900050622   B1C     *IN99         IFEQ      *OFF
033000050622     C*
033100050622   B2C                   SELECT
033200050622     C* ROLLUP
033300050622    >C     $TASTO        WHENEQ    ROLLUP
033400050622    >C                   MOVE      'D2'          $GEST
033500050622     C* F2=Decodifica
033600050622     C     $TASTO        WHENEQ    F02
033700050622     C                   EXSR      F02D1
033800050622     C* F3=Fine
033900050622     C     $TASTO        WHENEQ    F03
034000050622     C                   EXSR      F03D1
034100050622     C* F4=Ricerca
034200050622     C     $TASTO        WHENEQ    F04
034300050622     C                   EXSR      SEARCH
034400050622     C* F5=Refresh
034500050622     C     $TASTO        WHENEQ    F05
034600050622     C                   EXSR      F05D1
034700050622     C* F8=Successivo
034800050622     C     $TASTO        WHENEQ    F08
034900050622     C                   EXSR      F08D1
035000050622     C* F12=Ritorno
035100050622     C     $TASTO        WHENEQ    F12
035200050622     C                   EXSR      F12D1
035300050622     C* F18=Valori assunti
035400050622     C     $TASTO        WHENEQ    F18
035500050622     C                   EXSR      F18D1
035600050622     C* F21=Pers. batch
035700050622     C     $TASTO        WHENEQ    F21
035800050622     C                   EXSR      F21D1
035900050622     C* F24=Altri tasti
036000050622     C     $TASTO        WHENEQ    F24
036100050622     C                   EXSR      F24D1
036200050622     C*
036300050622   X2C                   OTHER
036400050622     C                   EXSR      CTRD1
036500050622     C* aggiungere le chiamate ai controlli delle videate supplementari
036600050622    >C     *IN99         IFEQ      *OFF
036700050622    >C                   EXSR      CTRD2
036800050622    >C     *IN99         IFEQ      *ON
036900050622    >C                   SETON                                            96
037000050622    >C                   ENDIF
037100050622    >C                   ENDIF
037200050622     C*
037300050622   B3C     $TASTO        IFEQ      F06
037400050622     C     *IN99         ANDEQ     *OFF
037500050622     C* eseguo lancio
037600050622     C                   EXSR      EXELAN
037700050622     C                   EXSR      ENDPGM
037800050622   E3C                   ENDIF
037900050622     C*
038000050622   E2C                   ENDSL
038100050622     C*
038200050622   E1C                   ENDIF
038300050622     C*
038400050622     C                   ENDSR
038500050622     C/EJECT
038600050622     C************************************************************
038700050622     C* INIZIALIZZAZIONE VIDEATA DATI
038800050622     C************************************************************
038900050622     C     INZD1         BEGSR
039000050622     C*
039100050622     C                   CLEAR                   FMTD1
039200050622    >C                   CLEAR                   FMTD2
039300050622     C*
039400050622     C                   Z-ADD     1             $PIED1
039500050622     C*
039600050622     C* se il pgm è stato richiamato imposto i valori ricevuti dalla DS
039700050622     C                   if        CallPgm     = '1'
039800050622     C*
039900150112     C                   EXSR      RIED1_J63
040000050622     C*
040100050622     C* se il pgm NON è stato richiamato
040200050622     C                   else
040300050622     C*
040400050622     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
040500050622     C     $Val          IFEQ      *ON
040600050622     C* riempio la videata da parametri memorizzati
040700050622     C                   EXSR      RIED1
040800050622    >C*  Aggiungere le routine di riempimento dei record succcessivi
040900050622    >C                   EXSR      RIED2
041000050622     C                   ELSE
041100050622     C* Immettere i valodi di default a video
041200150112     C                   Eval      D1TipoStp   = '1'
041300050622     C                   ENDIF
041400050622     C*
041500050622     C                   endif
041600050622     C*
041700050622     C* richiamo routine dei ctrl per decodificarla
041800050622     C                   EXSR      CTRD1
041900050622    >C* Aggiungere le routine di controllo dei record succcessivi
042000050622    >C                   EXSR      CTRD2
042100050622     C                   MOVE      *ALL'0'       IN5098
042200050622     C                   MOVEA     IN5098        *IN(50)
042300050622     C*
042400050622     C                   ENDSR
042500050622     C/EJECT
042600050622     C************************************************************
042700050622     C* RIEMPIMENTO VIDEATA  D1 con i valori ricevuti nella DS
042800050622     C************************************************************
042900150112     C     RIED1_J63     BEGSR
043000050622     C*
043100150112     C                   Eval      D1File      = FileJ63
043200150112     C                   Eval      D1TipoStp   = TipoStpJ63
043300150112     C                   Eval      D1NrInter1  = NrInte1J63
043400150112     C                   Eval      D1NrInter2  = NrInte2J63
043500050622     C*
043600050622     C                   ENDSR
043700050622     C/EJECT
043800050622     C************************************************************
043900050622     C* RIEMPIMENTO VIDEATA  D1
044000050622     C************************************************************
044100050622     C     RIED1         BEGSR
044200050622     C*
044300050622     C* impostare i campi della videata dai parametri memorizzati su anmpe
044400050622     C*
044500050711     C                   Eval      D1File      = $File
044600150112     C                   Eval      D1TipoStp   = $TipoStp
044700050622     C*
044800050622     C                   ENDSR
044900050622     C/EJECT
045000050622     C************************************************************
045100050622     C* GESTIONE F02 VIDEO D1
045200050622     C************************************************************
045300050622     C     F02D1         BEGSR
045400050622     C*
045500050622     C* per decodificare la videata richiamo la routine dei ctrl
045600050622     C* e poi disattivo gli eventuali messaggi di errore
045700050622     C                   EXSR      CTRD1
045800050622     C*
045900050622     C                   MOVE      *ALL'0'       IN5098
046000050622     C                   MOVEA     IN5098        *IN(50)
046100050622     C*
046200050622     C                   ENDSR
046300050622     C/EJECT
046400050622     C************************************************************
046500050622     C* GESTIONE F03 VIDEO D1
046600050622     C************************************************************
046700050622     C     F03D1         BEGSR
046800050622     C*
046900050622     C                   MOVE      *ON           $FINE
047000050622     C* fine programma
047100050622     C                   EXSR      ENDPGM
047200050622     C*
047300050622     C                   ENDSR
047400050622     C/EJECT
047500050622     C************************************************************
047600050622     C* GESTIONE F05 VIDEO D1
047700050622     C************************************************************
047800050622     C     F05D1         BEGSR
047900050622     C*
048000050622     C                   MOVE      *ON           $INZD1
048100050622     C*
048200050622     C                   ENDSR
048300050622     C/EJECT
048400050622     C************************************************************
048500050622     C* GESTIONE F08 VIDEO D1
048600050622     C************************************************************
048700050622     C     F08D1         BEGSR
048800050622     C*
048900050622     C                   MOVE      *ON           $FINE
049000050622     C*
049100050622     C                   ENDSR
049200050622     C/EJECT
049300050622     C************************************************************
049400050622     C* GESTIONE F12 VIDEO D1
049500050622     C************************************************************
049600050622     C     F12D1         BEGSR
049700050622     C*
049800050622     C                   MOVE      *ON           $FINE
049900050622     C*
050000050622     C                   ENDSR
050100050622     C/EJECT
050200050622     C************************************************************
050300050622     C* GESTIONE F18 VIDEO D1
050400050622     C************************************************************
050500050622     C     F18D1         BEGSR
050600050622     C*
050700050622     C                   MOVE      'D3'          $GEST
050800050622     C                   MOVE      *ON           $INZD3
050900050622     C*
051000050622     C                   ENDSR
051100050622     C/EJECT
051200050622     C************************************************************
051300050622     C* GESTIONE F21 VIDEO D1
051400050622     C************************************************************
051500050622     C     F21D1         BEGSR
051600050622     C*
051700150112     C                   MOVE      'SJ64'        KCOAZ
051800050622     C                   CALL      'BCH09'
051900050622     C                   PARM                    KPJBA
052000050622     C*
052100050622     C                   ENDSR
052200050622     C/EJECT
052300050622     C************************************************************
052400050622     C* GESTIONE F24 VIDEO D1
052500050622     C************************************************************
052600050622     C     F24D1         BEGSR
052700050622     C*
052800050622     C                   MOVEA     $FX1          $FX
052900050622     C                   MOVEA     $FC1          $FC
053000050622     C                   MOVEA     $FR1          $FR
053100050622     C                   Z-ADD     $ULKD1        $ULK
053200050622     C                   Z-ADD     79            $L01
053300050622     C                   Z-ADD     79            $L02
053400050622     C                   EXSR      RIEKEY
053500050622     C                   Z-ADD     $ULK          $ULKD1            3 0
053600050622     C                   MOVEA     $FC           $FC1
053700050622     C                   MOVEL     $KEY1         Z1KE1
053800050622     C                   MOVEL     $KEY2         Z1KE2
053900050622     C                   WRITE     FMTZ1
054000050622     C*
054100050622     C                   ENDSR
054200050622     C/EJECT
054300050622     C************************************************************
054400050622     C* GESTIONE F24 VIDEO D3
054500050622     C************************************************************
054600050622     C     F24D3         BEGSR
054700050622     C*
054800050622     C                   MOVEA     $FX3          $FX
054900050622     C                   MOVEA     $FC3          $FC
055000050622     C                   MOVEA     $FR3          $FR
055100050622     C                   Z-ADD     $ULKD3        $ULK
055200050622     C                   Z-ADD     44            $L01
055300050622     C                   Z-ADD     0             $L02
055400050622     C                   EXSR      RIEKEY
055500050622     C                   Z-ADD     $ULK          $ULKD3            3 0
055600050622     C                   MOVEA     $FC           $FC3
055700050622     C                   MOVEL     $KEY1         Z3KE1
055800050622     C                   WRITE     FMTZ3
055900050622     C*
056000050622     C                   ENDSR
056100050622     C/EJECT
056200050622     C************************************************************
056300050622     C* CONTROLLO VIDEATA
056400050622     C************************************************************
056500050622     C     CTRD1         BEGSR
056600050622     C*
056700050622     C                   SETOFF                                       99
056800050622     C* non imposto pos. del cursore
056900050622     C                   Z-ADD     0             H1RIGA
057000050622     C                   Z-ADD     0             H1COLO
057100050711     C*
057200050711     C*
057300050711     C* file da stampare
057400050711     C                   EXSR      CtrFile
057500150112     C*
057600150112     C* tipologia di stampa
057700150112     C                   Clear                   D1TipoStpD
057800150112     C                   Exsr      CtrTipStp
057900150112     C*
058000150112     C                   EXSR      CtrNrInter
058100050711     C*
058200050622     C                   ENDSR
058300050622     C/EJECT
058400050711     C***********************************************************
058500050711     C* Controllo file da stampare
058600050711     C***********************************************************
058700050711     C     CtrFile       BEGSR
058800050711     C*
058900050711     C                   if        D1File     = *Blank
059000050711     C                   SetOn                                        5199
059100050711     C                   else
059200050711     C* verifico se esiste, nella libreria
059300050711     C* dei file per lo scarico su supporto magnetico,
059400050711     C* il file indicato a video
059500050711     C                   EXSR      ChkFile
059600050711     C*
059700050711     C                   endif
059800050711     C*
059900050711     C                   ENDSR
060000050711     C/EJECT
060100150112     C***********************************************************
060200150112     C* Controllo tipologia di stampa
060300150112     C***********************************************************
060400150112     C     CtrTipStp     BegSr
060500150112     C*
060600150112     C                   Clear                   D1TipoStpD
060700150112     C*
060800150112     C                   Clear                   X07ValDs
060900150112     C*
061000150112     C                   Move      '1'           X07Ric
061100150112     C                   Move      *Off          X07All
061200150112     C                   Movel     'SDGSJ63S'    X07TRc
061300150112     C                   Movel     'TIPOSTPJ63'  X07Cam
061400150112     C                   Movel     D1TipoStp     X07VAL
061500150112     C*
061600150112     C                   Callb     'X07VALR'
061700150112     C                   Parm                    X07ValDs
061800150112     C*
061900150112     C     X07ERR        IFEQ      *Off
062000150112     C                   MOVEL     X07DES        D1TipoStpD
062100150112     C                   ElSe
062200150112     C                   SetOn                                        5299
062300150112     C                   EndIf
062400150112     C*
062500150112     C                   EndSr
062600150112     C***********************************************************
062700150112     C* Controllo numeri interni
062800150112     C***********************************************************
062900150112     C     CtrNrInter    BEGSR
063000150112     C*
063100150120     C                   CALLB     'XALLINEA'
063200150120     C                   PARM                    D1NrInter1
063300150120     C                   PARM      16            LEN2              5 0
063400150120     C                   PARM      '2'           Input             1
063500150120     C*
063600150120     C                   CALLB     'XALLINEA'
063700150120     C                   PARM                    D1NrInter2
063800150120     C                   PARM      16            LEN2              5 0
063900150120     C                   PARM      '2'           Input             1
064000150120     C*
064100150112     C                   if        D1NrInter1  > D1NrInter2   and
064200150120     C                             D1NrInter2  <> *blank
064300150112     C                   SetOn                                        7399
064400150112     C                   endif
064500150112     C*
064600150112     C                   ENDSR
064700150112     C/EJECT
064800050711     C************************************************************
064900050711     C* Verifico l'esistenza del file nella libreria indicata
065000050711     C* per lo scarico su supporto magnetico
065100050711     C************************************************************
065200050711     C     ChkFile       BEGSR
065300050711     C*
065400050711     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
065500050711     C                   Eval      Cmd         =
065600050711     C                             'CHKOBJ     OBJ('        +
065700050711     C                             %Trim(Lib022) + '/' +
065800050711     C                             %Trim(D1File) +
065900050711     C                             ') OBJTYPE(*FILE)'
066000050711     C*
066100050711     C                   Call      'QCMDEXC'                            21
066200050711     C                   Parm                    Cmd             120
066300050711     C                   Parm      120           Len              15 5
066400050711     C*
066500050711     C                   if        *In21       = *On
066600050711     C*
066700050711     C* Errore: dico che il file non è stato trovato nella libreria
066800050711     C* Valorizzo le variabili del msg PRO1797
066900050711     C                   Move      D1File        §File
067000050711     C                   Move      Lib022        §Libreria
067100050711     C                   Move      §Fil_Lib      P1Fil_Lib
067200050711     C                   SetOn                                        5099
067300050711     C*
067400050711     C                   endif
067500050711     C*
067600050711     C                   ENDSR
067700050711     C/Eject
067800050622     C************************************************************
067900050622     C* GESTIONE VIDEO RECORD  D2
068000050622     C************************************************************
068100050622    >C     GESD2         BEGSR
068200050622     C* emissione piede videata se proveniente da altra
068300050622     C* salvataggio valore formato di provenienza
068400050622   B1C     $LASTV        IFNE      'D2'
068500050622     C                   WRITE     FMTT1
068600050622     C                   EXSR      GESPIE
068700050622     C                   MOVE      $LASTV        $LASTG
068800050622     C                   MOVE      'D2'          $LASTV
068900050622   E1C                   ENDIF
069000050622     C*
069100050622     C* se esistono errori sulla videata
069200050622     C* emetto la write del formato a indicatori spenti per vedere
069300050622     C* le eventuali decodifiche
069400050622   B1C     *IN99         IFEQ      *ON
069500050622     C     $GEST         ANDEQ     'D2'
069600050622     C                   MOVEA     *IN           WIN
069700050622     C                   MOVE      *ALL'0'       IN5098           49
069800050622     C                   MOVEA     IN5098        *IN(50)
069900050622     C                   WRITE     FMTD2
070000050622     C                   MOVEA     WIN           *IN
070100050622   E1C                   ENDIF
070200050622     C*
070300050622     C                   EXFMT     FMTD2
070400050622     C*
070500050622     C* controllo abilitazione tasti funzione
070600050622     C                   EXSR      CTRKD1
070700050622     C*
070800050622   B1C     *IN99         IFEQ      *OFF
070900050622     C*
071000050622   B2C                   SELECT
071100050622     C* ROLDOWN
071200050622     C     $TASTO        WHENEQ    ROLDWN
071300050622     C                   MOVE      'D1'          $GEST
071400050622     C* F3=Fine
071500050622     C     $TASTO        WHENEQ    F03
071600050622     C                   EXSR      F03D1
071700050622     C* F4=Ricerca
071800050622     C     $TASTO        WHENEQ    F04
071900050622     C                   EXSR      SEARCH
072000050622     C* F5=Refresh
072100050622     C     $TASTO        WHENEQ    F05
072200050622     C                   EXSR      F05D2
072300050622     C* F8=Successivo
072400050622     C     $TASTO        WHENEQ    F08
072500050622     C                   EXSR      F08D1
072600050622     C* F12=Ritorno
072700050622     C     $TASTO        WHENEQ    F12
072800050622     C                   EXSR      F12D2
072900050622     C* F18=Valori assunti
073000050622     C     $TASTO        WHENEQ    F18
073100050622     C                   EXSR      F18D1
073200050622     C* F21=Pers. batch
073300050622     C     $TASTO        WHENEQ    F21
073400050622     C                   EXSR      F21D1
073500050622     C* F24=Altri tasti
073600050622     C     $TASTO        WHENEQ    F24
073700050622     C                   EXSR      F24D1
073800050622     C*
073900050622   X2C                   OTHER
074000050622     C                   EXSR      CTRD2
074100050622   B3C     *IN99         IFEQ      *OFF
074200050622     C                   EXSR      CTRD1
074300050622   B4C     *IN99         IFEQ      *ON
074400050622     C                   SETON                                            96
074500050622   E4C                   ENDIF
074600050622   E3C                   ENDIF
074700050622     C*
074800050622     C*
074900050622   B3C     $TASTO        IFEQ      F06
075000050622     C     *IN99         ANDEQ     *OFF
075100050622     C* eseguo lancio
075200050622     C                   EXSR      EXELAN
075300050622     C                   EXSR      ENDPGM
075400050622   E3C                   ENDIF
075500050622     C*
075600050622   E2C                   ENDSL
075700050622     C*
075800050622   E1C                   ENDIF
075900050622     C*
076000050622    >C                   ENDSR
076100050622     C/EJECT
076200050622     C************************************************************
076300050622     C* GESTIONE VALORI ASSUNTI
076400050622     C************************************************************
076500050622     C     GESD3         BEGSR
076600050622     C*
076700050622   B1C     $INZD3        IFEQ      *ON
076800050622     C                   EXSR      INZD3
076900050622     C                   MOVE      *OFF          $INZD3
077000050622   E1C                   ENDIF
077100050622     C*
077200050622   B1C     $LASTV        IFNE      'D3'
077300050622     C                   EXSR      GESPIE
077400050622     C                   MOVE      $LASTV        $LASTG
077500050622     C                   MOVE      'D3'          $LASTV
077600050622   E1C                   ENDIF
077700050622     C*
077800050622     C* se esistono errori sulla videata
077900050622     C* emetto la write del formato a indicatori spenti per vedere
078000050622     C* le eventuali decodifiche
078100050622   B1C     *IN99         IFEQ      *ON
078200050622     C                   MOVEA     *IN           WIN
078300050622     C                   MOVE      *ALL'0'       IN5098           49
078400050622     C                   MOVEA     IN5098        *IN(50)
078500050622     C                   WRITE     FMTD3
078600050622     C                   MOVEA     WIN           *IN
078700050622   E1C                   ENDIF
078800050622     C*
078900050622     C                   EXFMT     FMTD3
079000050622     C*
079100050622     C* controllo abilitazione tasti funzione
079200050622     C                   EXSR      CTRKD3
079300050622     C*
079400050622   B1C     *IN99         IFEQ      *OFF
079500050622     C*
079600050622   B2C                   SELECT
079700050622     C* F3=Fine
079800050622     C     $TASTO        WHENEQ    F03
079900050622     C                   EXSR      F03D1
080000050622     C* F12=Ritorno
080100050622     C     $TASTO        WHENEQ    F12
080200050622     C                   EXSR      F12D3
080300050622     C* F24=Altri tasti
080400050622     C     $TASTO        WHENEQ    F24
080500050622     C                   EXSR      F24D3
080600050622     C*
080700050622     C                   OTHER
080800050622     C                   EXSR      CTRD3
080900050622     C     *IN99         IFEQ      *OFF
081000050622     C                   MOVE      $LASTG        $GEST
081100050622     C                   ENDIF
081200050622     C*
081300050622   E2C                   ENDSL
081400050622     C*
081500050622   E1C                   ENDIF
081600050622     C*
081700050622     C                   ENDSR
081800050622     C/EJECT
081900050622     C************************************************************
082000050622     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
082100050622     C************************************************************
082200050622     C     INZD3         BEGSR
082300050622     C*
082400050622     C                   CLEAR                   FMTD3
082500050622     C     $VAL          IFEQ      *ON
082600050622     C                   MOVE      XSCON         D3VAL
082700050622     C                   ELSE
082800050622     C                   MOVE      XSCOFF        D3VAL
082900050622     C                   ENDIF
083000050622     C                   MOVE      XSCON         D3ON
083100050622     C                   MOVE      XSCOFF        D3OF
083200050622     C*
083300050622     C                   ENDSR
083400050622     C/EJECT
083500050622     C************************************************************
083600050622     C* RIEMPIMENTO VIDEATA D2
083700050622     C************************************************************
083800050622    >C     RIED2         BEGSR
083900050622     C*
084000050622     C*
084100050622    >C                   ENDSR
084200050622     C/EJECT
084300050622     C************************************************************
084400050622     C* GESTIONE F05 VIDEO D2
084500050622     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
084600050622     C* e il riposizionamento alla prima
084700050622     C************************************************************
084800050622    >C     F05D2         BEGSR
084900050622     C*
085000050622     C                   MOVE      *ON           $INZD1
085100050622     C                   MOVE      'D1'          $GEST
085200050622     C*
085300050622    >C                   ENDSR
085400050622     C/EJECT
085500050622     C************************************************************
085600050622     C* GESTIONE F12 VIDEO D2
085700050622     C************************************************************
085800050622     C     F12D2         BEGSR
085900050622     C*
086000050622     C                   MOVE      'D1'          $GEST
086100050622     C*
086200050622     C                   ENDSR
086300050622     C/EJECT
086400050622     C************************************************************
086500050622     C* GESTIONE F12 VIDEO D3
086600050622     C************************************************************
086700050622     C     F12D3         BEGSR
086800050622     C*
086900050622     C                   MOVE      $LASTG        $GEST
087000050622     C*
087100050622     C                   ENDSR
087200050622     C/EJECT
087300050622     C************************************************************
087400050622     C* CONTROLLO VIDEATA  D2
087500050622     C************************************************************
087600050622    >C     CTRD2         BEGSR
087700050622     C*
087800050622     C                   SETOFF                                       99
087900050622     C* non imposto pos. del cursore
088000050622     C                   Z-ADD     0             H1RIGA
088100050622     C                   Z-ADD     0             H1COLO
088200050622     C*
088300050622     C                   ENDSR
088400050622     C/EJECT
088500050622     C************************************************************
088600050622     C* CONTROLLO VIDEATA  D3
088700050622     C************************************************************
088800050622     C     CTRD3         BEGSR
088900050622     C*
089000050622     C                   SETOFF                                       99
089100050622     C* non imposto pos. del cursore
089200050622     C                   Z-ADD     0             H1RIGA
089300050622     C                   Z-ADD     0             H1COLO
089400050622     C*
089500050622     c* Controllo VALORE
089600050622     C     D3VAL         IFNE      XSCON
089700050622     C     D3VAL         ANDNE     XSCOFF
089800050622     C                   SETON                                        5099
089900050622     C                   ENDIF
090000050622     C*
090100050622     C     D3VAL         IFEQ      XSCON
090200050622     C                   MOVE      *ON           $VAL
090300050622     C                   ELSE
090400050622     C                   MOVE      *OFF          $val
090500050622     c                   endif
090600050622     C*
090700050622     C                   ENDSR
090800050622     C/EJECT
090900050622     C************************************************************
091000050622     C* ESECUZIONE LANCIO
091100050622     C************************************************************
091200050622     C     EXELAN        BEGSR
091300050622     C*
091400150112     C                   RESET                   SDGSJ64DS
091500050622     C*
091600150112     C                   MOVE      XSCSOC        SocietaJ64
091700050622     C*
091800150112     C                   Eval      FileJ64     = D1File
091900150112     C                   Eval      LibJ64      = Lib022
092000150112     C                   Eval      TipoStpJ64  = D1TipoStp
092100150112     C                   Eval      NrInte1J64  = D1NrInter1
092200150120     C                   if        D1NrInter2 <> *blank
092300150112     C                   Eval      NrInte2J64  = D1NrInter2
092400150112     C                   else
092500150112     C                   Eval      NrInte2J64  = *hival
092600150112     C                   endif
092700050622     C*
092800150112     C                   MOVEL     SDGSJ64DS     KPJBU
092900050712     C*
093000150112     C                   MOVE      'SJ64'        KCOAZ
093100050712     C*
093200050712     C                   Clear                   $Inter            1
093300150112     C                   eval      $Inter      = *On
093400050712     C                   if        $Inter      = *On
093500150112    >C                   CALL      'SDGSJ64C'
093600050712     C                   PARM                    KPJBA
093700050712     C                   else
093800050712    >C                   CALL      'BCH10'
093900050712     C                   PARM                    KPJBA
094000050712     C                   endif
094100050622     C*
094200050622     C* aggiorno valori assunti
094300050622     C                   EXSR      AGGMPE
094400050622     C*
094500050622     C                   Eval      $Fine       = *On
094600050622     C*
094700050622     C                   ENDSR
094800050622     C/EJECT
094900050622     C************************************************************
095000050622     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
095100050622     C************************************************************
095200050622     C     CTRKD1        BEGSR
095300050622     C*
095400050622     C                   SETOFF                                       99
095500050622     C                   MOVEA     $FC1          $FC
095600050622     C                   EXSR      CTRKEY
095700050622     C     *IN99         IFEQ      *ON
095800050622     C                   SETON                                        97
095900050622     C                   ENDIF
096000050622     C*
096100050622     C                   ENDSR
096200050622     C/EJECT
096300050622     C************************************************************
096400050622     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
096500050622     C* INPUT
096600050622     C* $FC3 = Abilitazioni videata D3
096700050622     C* OUTPUT
096800050622     C* *IN97 *IN99 Errori
096900050622     C************************************************************
097000050622     C     CTRKD3        BEGSR
097100050622     C*
097200050622     C                   SETOFF                                       99
097300050622     C                   MOVEA     $FC3          $FC
097400050622     C                   EXSR      CTRKEY
097500050622     C     *IN99         IFEQ      *ON
097600050622     C                   SETON                                        97
097700050622     C                   ENDIF
097800050622     C*
097900050622     C                   ENDSR
098000050622     C/EJECT
098100050622     C************************************************************
098200050622     C* CONTROLLO ABILITAZIONE TASTI
098300050622     C************************************************************
098400050622     C     CTRKEY        BEGSR
098500050622     C*
098600050622     C                   SELECT
098700050622     C     $TASTO        WHENEQ    F01
098800050622     C     $FC(1)        IFEQ      '0'
098900050622     C                   SETON                                        99
099000050622     C                   ENDIF
099100050622     C     $TASTO        WHENEQ    F02
099200050622     C     $FC(2)        IFEQ      '0'
099300050622     C                   SETON                                        99
099400050622     C                   ENDIF
099500050622     C     $TASTO        WHENEQ    F03
099600050622     C     $FC(3)        IFEQ      '0'
099700050622     C                   SETON                                        99
099800050622     C                   ENDIF
099900050622     C     $TASTO        WHENEQ    F04
100000050622     C     $FC(4)        IFEQ      '0'
100100050622     C                   SETON                                        99
100200050622     C                   ENDIF
100300050622     C     $TASTO        WHENEQ    F05
100400050622     C     $FC(5)        IFEQ      '0'
100500050622     C                   SETON                                        99
100600050622     C                   ENDIF
100700050622     C     $TASTO        WHENEQ    F06
100800050622     C     $FC(6)        IFEQ      '0'
100900050622     C                   SETON                                        99
101000050622     C                   ENDIF
101100050622     C     $TASTO        WHENEQ    F07
101200050622     C     $FC(7)        IFEQ      '0'
101300050622     C                   SETON                                        99
101400050622     C                   ENDIF
101500050622     C     $TASTO        WHENEQ    F08
101600050622     C     $FC(8)        IFEQ      '0'
101700050622     C                   SETON                                        99
101800050622     C                   ENDIF
101900050622     C     $TASTO        WHENEQ    F09
102000050622     C     $FC(09)       IFEQ      '0'
102100050622     C                   SETON                                        99
102200050622     C                   ENDIF
102300050622     C     $TASTO        WHENEQ    F10
102400050622     C     $FC(10)       IFEQ      '0'
102500050622     C                   SETON                                        99
102600050622     C                   ENDIF
102700050622     C     $TASTO        WHENEQ    F11
102800050622     C     $FC(11)       IFEQ      '0'
102900050622     C                   SETON                                        99
103000050622     C                   ENDIF
103100050622     C     $TASTO        WHENEQ    F12
103200050622     C     $FC(12)       IFEQ      '0'
103300050622     C                   SETON                                        99
103400050622     C                   ENDIF
103500050622     C     $TASTO        WHENEQ    F13
103600050622     C     $FC(13)       IFEQ      '0'
103700050622     C                   SETON                                        99
103800050622     C                   ENDIF
103900050622     C     $TASTO        WHENEQ    F14
104000050622     C     $FC(14)       IFEQ      '0'
104100050622     C                   SETON                                        99
104200050622     C                   ENDIF
104300050622     C     $TASTO        WHENEQ    F15
104400050622     C     $FC(15)       IFEQ      '0'
104500050622     C                   SETON                                        99
104600050622     C                   ENDIF
104700050622     C     $TASTO        WHENEQ    F16
104800050622     C     $FC(16)       IFEQ      '0'
104900050622     C                   SETON                                        99
105000050622     C                   ENDIF
105100050622     C     $TASTO        WHENEQ    F17
105200050622     C     $FC(17)       IFEQ      '0'
105300050622     C                   SETON                                        99
105400050622     C                   ENDIF
105500050622     C     $TASTO        WHENEQ    F18
105600050622     C     $FC(18)       IFEQ      '0'
105700050622     C                   SETON                                        99
105800050622     C                   ENDIF
105900050622     C     $TASTO        WHENEQ    F19
106000050622     C     $FC(19)       IFEQ      '0'
106100050622     C                   SETON                                        99
106200050622     C                   ENDIF
106300050622     C     $TASTO        WHENEQ    F20
106400050622     C     $FC(20)       IFEQ      '0'
106500050622     C                   SETON                                        99
106600050622     C                   ENDIF
106700050622     C     $TASTO        WHENEQ    F21
106800050622     C     $FC(21)       IFEQ      '0'
106900050622     C                   SETON                                        99
107000050622     C                   ENDIF
107100050622     C     $TASTO        WHENEQ    F22
107200050622     C     $FC(22)       IFEQ      '0'
107300050622     C                   SETON                                        99
107400050622     C                   ENDIF
107500050622     C     $TASTO        WHENEQ    F23
107600050622     C     $FC(23)       IFEQ      '0'
107700050622     C                   SETON                                        99
107800050622     C                   ENDIF
107900050622     C     $TASTO        WHENEQ    F24
108000050622     C     $FC(24)       IFEQ      '0'
108100050622     C                   SETON                                        99
108200050622     C                   ENDIF
108300050622     C                   ENDSL
108400050622     C*
108500050622     C                   ENDSR
108600050622     C/EJECT
108700050622     C************************************************************
108800050622     C* OPERAZIONI INIZIALI
108900050622     C************************************************************
109000050622     C     *INZSR        BEGSR
109100050622     C*
109200050622     C* Reperimento parametri
109300050622     C*
109400050622     C     *ENTRY        PLIST
109500050622     C                   PARM                    KPJBA
109600050622     C*
109700050622     C* Reperimento tasti di funzione
109800050622     C*
109900050622     C                   EXSR      REPKEY
110000050622     C*
110100050622     C                   ENDSR
110200050622     C/EJECT
110300050622     C************************************************************
110400050622     C* REPERIMENTO TASTI DI FUNZIONE
110500050622     C************************************************************
110600050622     C     REPKEY        BEGSR
110700050622     C                   MOVEA     $FM1          $FM
110800050622     C                   MOVEA     $Fc1          $Fc
110900050622     C                   EXSR      REPKKK
111000050622     C                   MOVEA     $FX           $FX1
111100050622     C*
111200050622     C                   MOVEA     $FM3          $FM
111300050622     C                   MOVEA     $Fc3          $FM
111400050622     C                   EXSR      REPKKK
111500050622     C                   MOVEA     $FX           $FX3
111600050622     C*
111700050622     C                   ENDSR
111800050622     C/EJECT
111900050622     C************************************************************
112000050622     C* REPERIMENTO TASTI DI FUNZIONE
112100050622     C************************************************************
112200050622     C     REPKKK        BEGSR
112300050622     C*
112400050622     C                   CLEAR                   $FX
112500050622     C     1             DO        24            X
112600050622     C*
112700050622     C     $FM(X)        IFNE      *BLANK
112800050622     C     $fc(X)        andne     '0'
112900050622     C*
113000050622     C                   MOVEL     $FM(X)        IDMSG
113100050622     C                   CALLB     'XRTVM'
113200050622     C                   PARM                    IDMSG            27
113300050622     C                   PARM                    TXTMSG          100
113400050622     C                   PARM                    ERRMSG            1
113500050622     C*
113600050622     C     ERRMSG        IFNE      *ON
113700050622     C                   MOVEL     TXTMSG        $FX(X)
113800050622     C                   ELSE
113900050622     C                   MOVEL     *ALL' '       $FX(X)
114000050622     C                   ENDIF
114100050622     C*
114200050622     C                   ENDIF
114300050622     C*
114400050622     C                   ENDDO
114500050622     C*
114600050622     C                   ENDSR
114700050622     C/EJECT
114800050622     C************************************************************
114900050622     C* INIZIALIZZAZIONE VARIABILI
115000050622     C************************************************************
115100050622     C     INZVAR        BEGSR
115200050622     C*
115300050622     C* se il pgm è stato richiamato imposto i valori ricevuti dalla DS
115400050622     C                   if        KPJBU      <> *Blanks
115500050622     C                   Eval      CallPgm     = '1'
115600150112     C                   MoveL     KPJBU         SDGSJ63DS
115700050622     C                   else
115800050622     C                   Eval      CallPgm     = '0'
115900050622     C                   endif
116000050622     C*
116100050622     C* Reperimento dati società
116200050622     C*
116300050622    >C                   MOVEL     'SOC001'      TIPXSC
116400050622     C                   MOVEL     *BLANK        SOCXSC
116500050622     C                   EXSR      REPSOC
116600050622     C     RTNXSC        IFNE      '1'
116700050622     C                   MOVEL     XSOCDS        SOC001
116800050622     C                   ELSE
116900050622     C                   RETURN
117000050622     C                   ENDIF
117100050622     C*
117200050622     C* Reperisco la libreria in cui è stato memorizzato il file
117300050622     C* che verrà stampato
117400050622     C                   EXSR      RepLib
117500050622     C*
117600050622     C* Asteriscare se non presenti
117700050622     C* Reperimento autorizzazioni
117800050622     C*
117900050622    >C***                EXSR      REPAUT
118000050622     C*
118100050622     C* Reperimento valori default utente
118200050622     C*
118300050622     C                   EXSR      REPDFT
118400050622     C*
118500050622     C* Definizione dei tasti funzione utilizzabili a pgm
118600050622     C*
118700050622     C                   EXSR      DEFOPZ
118800050622     C*
118900050622     C* Variabili per gestione videate
119000050622     C*
119100050622     C                   MOVE      'D1'          $GEST
119200050622     C                   MOVE      *OFF          $FINE
119300050622     C                   MOVE      *ON           $INZD1
119400050622     C                   MOVE      *BLANK        $LASTG
119500050622     C                   MOVE      *BLANK        $LASTV
119600050622     C                   Z-ADD     0             $ULKD1            3 0
119700050622     C                   Z-ADD     0             $ULKD3            3 0
119800050622     C*
119900050622     C* Variabili appoggio
120000050622     C*
120100050622     C                   Z-ADD     0             CURR
120200050622     C                   Z-ADD     0             CURC
120300050622     C*
120400050622     C* Valorizzazione campi univoci testate
120500050622     C*
120600050622     C                   CLEAR                   FMTT1
120700050622     C                   MOVEL     KNSIF         NOMSIF
120800050622     C                   MOVEL     XSCDSI        NOMDIT
120900150112     C                   MOVEL     KCDAZ         NOMAZN
121000050622     C* recupero il sottotitolo (se c'è)
121100050622     C                   MOVEL     *BLANK        T1TIT
121200050622     C     SOTTIT        IFNE      *BLANK
121300050622     C                   MOVEL     SOTTIT        IDMSG
121400050622     C                   CALLB     'XRTVM'
121500050622     C                   PARM                    IDMSG            27
121600050622     C                   PARM                    TXTMSG          100
121700050622     C                   PARM                    ERRMSG            1
121800050622     C                   PARM      *ON           CTRMSG            1
121900050622     C                   PARM      30            LENMSG            3 0
122000050622     C     ERRMSG        IFNE      *ON
122100050622     C                   MOVEL     TXTMSG        T1TIT
122200050622     C                   ELSE
122300050622     C                   MOVEL     *ALL'?'       T1TIT
122400050622     C                   ENDIF
122500050622     C                   ENDIF
122600050622     C*
122700050622     C* Inizializzazione/Decodifica prima videata
122800050622     C*
122900050622     C                   EXSR      INZD1
123000050622     C                   MOVE      *OFF          $INZD1
123100050622     C*
123200050622     C                   ENDSR
123300050622     C/EJECT
123400050622     C************************************************************
123500050622     C* REPERIMENTO DATI SOCIETA'
123600050622     C************************************************************
123700050622     C     REPSOC        BEGSR
123800050622     C*
123900150112     C                   CALL      'SDGSOCDR'
124000050622     C                   PARM                    TIPXSC            6
124100050622     C                   PARM                    SOCXSC            3
124200050622     C                   PARM                    CDSXSC            9 0
124300050622     C                   PARM                    MODXSC            3
124400050622     C                   PARM      *BLANKS       RTNXSC            1
124500050622     C                   PARM                    XSOCDS
124600050622     C                   PARM                    KPJBA
124700050622     C*
124800050622     C                   ENDSR
124900050622     C/EJECT
125000050622     C************************************************************
125100050622     C* Rerimento libreria per scarico su supporto magnetico
125200050622     C************************************************************
125300050622     C     REPLib        BEGSR
125400050622     C*
125500050622     C                   Clear                   ANP022DS
125600050622     C*
125700050622     C                   CALLB     'XPAR'
125800050622     C                   Parm                    XScSoc
125900050622     C                   Parm      'SCARSUMA'    XPaPar            8
126000050622     C                   Parm                    XPaOut         2000
126100050622     C                   Parm      *OFF          XPaErr            1
126200050622     C                   Parm      ' '           XPaRic            1
126300050622     C*
126400050622     C                   if        XPaErr      = *On
126500150112     C                   Move      '1'           ErrJ63
126600050622     C                   Move      *on           $Fine
126700050622     C                   EXSR      EndPgm
126800050622     C*
126900050622     C                   else
127000050622     C                   MoveL     XPaOut        ANP022DS
127100050622     C                   endif
127200050622     C*
127300050622     C                   ENDSR
127400050622     C/EJECT
127500050622     C************************************************************
127600050622     C* REPERIMENTO AUTORIZZAZIONI UTENTE
127700050622     C************************************************************
127800050622     C*    REPAUT        BEGSR
127900050622     C*
128000050622     C*                  CLEAR                   AUTDS
128100050622     C*
128200050622     C*                  MOVEL     XSCSOC        XCASOC
128300050622     C*                  MOVEL     XSCDUN        XCAUNI
128400050622     C*                  Z-ADD     0             XCAGRP
128500050622     C*                  MOVEL     KNMUS         XCAPRF
128600050622    >C*                  MOVEL     'XXXXXX'      XCAFNC
128700050622    >C*                  MOVEL     *ZERO         XCAVFU
128800050622    >C*                  MOVE      'CK'          XCATCK
128900050622    >C*                  MOVE      '1'           XCANAU
129000050622     C*
129100050622     C*                  CALLB     'XCHKAUT'
129200050622     C*                  PARM                    AUTDS
129300050622     C*
129400050622     C* errori/non abilitazione
129500050622     C*    XCARTN        IFNE      0
129600050622     C*                  MOVE      *ON           $fine
129700050622     C*                  EXSR      ENDPGM
129800050622     C*                  ELSE
129900050622    >C*                  MOVEL     XCADDS        GXXXDS
130000050622     C* Se non autorizzati alla funzione oppure                              ed
130100050622    >C*    ABIxxx        IFEQ      *OFF
130200050622     C*                  MOVE      XCAFNC        XNOFNC
130300050622     C*                  MOVE      XCAPRF        XNOPRF
130400050622     C*                  MOVE      XCACUT        XNOCUT
130500050622     C*                  MOVE      XCATCK        XNOTCK
130600050622     C*                  MOVE      '1'           XNOERR
130700050622     C*                  CALLB     'XNOAUTR'
130800050622     C*                  PARM                    XNOADS
130900050622     C*                  MOVE      *ON           $fine
131000050622     C*                  EXSR      ENDPGM
131100050622     C*                  ENDIF
131200050622     C*                  ENDIF
131300050622     C*
131400050622     C*                  ENDSR
131500050622     C/EJECT
131600050622     C************************************************************
131700050622     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
131800050622     C************************************************************
131900050622     C     REPDFT        BEGSR
132000050622     C*
132100050622     C* preparo i parametri di chiamata al driver
132200050622     C                   CLEAR                   XMPEDS
132300050622     C                   EVAL      TpChiamata = '01'
132400050622     C                   EVAL      NChiavi = 5
132500050622     C                   EVAL      VLogica = 2
132600050622     C*
132700050622     C* preparo le chiavi per la chiamata al driver
132800050622     C                   CLEAR                   XMPEDS1
132900050622     C                   MOVEL     XSCSOC        DSMPESOC
133000050622     C                   MOVEL     DSPGM         DSMPEPGM
133100050622     C                   MOVEL     KNMUS         DSMPEPRF
133200050622     C* Si reperiscono i dati di default a seconda delle modalità con
133300050622     C* cui il pgm è usato
133400050622     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
133500050622     C* MPETIP = 'PV'  pgm richiamato da menù
133600050622     C* MPETIP = 'PB'  x lancio Batch
133700050622     C                   MOVEL     'PB'          DSMPETIP
133800050622     C*
133900050622     C                   CALLB     'XMPE'
134000050622     C                   PARM                    XMPEDS
134100050622     C                   PARM                    XMPEDS1
134200050622     C*
134300050622     C     Risultato     IFEQ      '0'
134400050622     C                   MOVEL     DSMPEPAR      DSMPE
134500050622     C* nel caso sia la prima volta che l'utente apre questa anagrafica
134600050622     C* i suoi valori di default vengono decisi a pgm
134700050622     C                   ELSE
134800050622     C                   ENDIF
134900050622     C*
135000050622     C                   ENDSR
135100050622     C/EJECT
135200050622     C************************************************************
135300050622     C* AGGIORNAMENTO VALORI ASSUNTI
135400050622     C************************************************************
135500050622     C     AGGMPE        BEGSR
135600050622     C*
135700050622     C* preparo i parametri di chiamata al driver
135800050622     C                   CLEAR                   XMPEDS
135900050622     C                   EVAL      TpChiamata = '03'
136000050622     C                   EVAL      NChiavi = 5
136100050622     C                   EVAL      VLogica = 2
136200050622     C*
136300050622     C* preparo le chiavi per la chiamata al driver
136400050622     C                   CLEAR                   XMPEDS1
136500050622     C                   MOVEL     XSCSOC        DSMPESOC
136600050622     C                   MOVEL     DSPGM         DSMPEPGM
136700050622     C                   MOVEL     KNMUS         DSMPEPRF
136800050622     C*
136900050622     C* Si memorizzano i dati di default a seconda delle modalità con
137000050622     C* cui il pgm è usato
137100050622     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
137200050622     C* MPETIP = 'PV'  pgm richiamato da menù
137300050622     C* MPETIP = 'PB'  x lancio Batch
137400050622     C*
137500050622     C                   MOVEL     'PB'          DSMPETIP
137600050622     C* dati
137700050711     C                   MoveL     D1File        $File
137800150112     C                   MoveL     D1TipoStp     $TipoStp
137900050622     C*
138000050622     C                   MOVEL     DSMPE         DSMPEPAR
138100050622     C*
138200050622     C                   CALLB     'XMPE'
138300050622     C                   PARM                    XMPEDS
138400050622     C                   PARM                    XMPEDS1
138500050622     C*
138600050622     C                   ENDSR
138700050622     C/EJECT
138800050622     C************************************************************
138900050622     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
139000050622     C************************************************************
139100050622     C     DEFOPZ        BEGSR
139200050622     C*
139300050622     C* Questa routine deve valorizzare la schiera $FC1/$FR1
139400050622     C* con S o N a seconda della utilizzabilità o meno
139500050622     C* del tasto funzione correlato
139600050622     C*
139700050622     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
139800050622     C* di compilazione
139900050622     C                   RESET                   $FC1
140000050622     C                   RESET                   $FR1
140100050622     C                   RESET                   $FC3
140200050622     C                   RESET                   $FR3
140300050622     C*
140400050622     C                   ENDSR
140500050622     C/EJECT
140600050622     C************************************************************
140700050622     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
140800050622     C************************************************************
140900050622     C     DEFCAM        BEGSR
141000050622     C*
141100050622     C* klist
141200050711     C*
141300050622     C*
141400050622     C*
141500050622     C* Variabili per gestione videate
141600050622     C*
141700050622     C                   MOVE      *BLANK        $GEST             2
141800050622     C                   MOVE      *BLANK        $FINE             1
141900050622     C                   MOVE      *BLANK        $INZD1            1
142000050622     C                   MOVE      *BLANK        $INZD3            1
142100050622     C                   MOVE      *BLANK        $LASTG            2
142200050622     C                   MOVE      *BLANK        $LASTV            2
142300050622     C                   Z-ADD     0             $PIED1            3 0
142400050622     C*
142500050622     C* Variabili appoggio
142600050622     C*
142700050622     C                   Z-ADD     0             CURR              2 0
142800050622     C                   Z-ADD     0             CURC              2 0
142900050622     C                   MOVE      *ZEROS        WIN              99
143000050622     C*
143100050622     C* Indici
143200050622     C*
143300050622     C                   Z-ADD     0             X                 3 0
143400050622     C                   Z-ADD     0             Y                 3 0
143500050622     C*
143600050622     C                   ENDSR
143700050622     C/EJECT
143800050622     C************************************************************
143900050622     C* RIEMPIMENTO TASTI FUNZIONE
144000050622     C************************************************************
144100050622     C     RIEKEY        BEGSR
144200050622     C*
144300050622     C* Input :
144400050622     C* - $FX = schiera con tutti i testi disponibili
144500050622     C* - $FC = schiere con i flag di validità associati ai testi
144600050622     C* - $FR = schiere con i flag di REVERSE IMAGE
144700050622     C* - $ULK = ultimo testo caricato
144800050622     C* - $L01 = Lunghezza prima riga
144900050622     C* - $L02 = Lunghezza seconda riga
145000050622     C* Output :
145100050622     C* - $ULK
145200050622     C* - $KEY1= 1° riga di tasti funzione
145300050622     C* - $KEY2= 2° riga di tasti funzione
145400050622     C*
145500050622     C                   CALLB     'XKEYMSG'
145600050622     C* input
145700050622     C                   PARM                    $FX
145800050622     C                   PARM                    $FC
145900050622     C                   PARM                    $FR
146000050622     C                   PARM                    $ULK              3 0
146100050622     C* output
146200050622     C                   PARM                    $KEY1            79
146300050622     C                   PARM                    $KEY2            79
146400050622    >C* input
146500050622    >C                   PARM                    $L01              3 0
146600050622    >C                   PARM                    $L02              3 0
146700050622     C*
146800050622     C                   ENDSR
146900050622** TASTI DI FUNZIONE  D1
147000050622PROMSG    *LIBL     KEY0001  01
147100050622PROMSG    *LIBL     KEY0002  02
147200050622PROMSG    *LIBL     KEY0003  03
147300050622PROMSG    *LIBL     KEY0004  04
147400050622PROMSG    *LIBL     KEY0005  05
147500050622PROMSG    *LIBL     KEY0006  06
147600050622PROMSG    *LIBL     KEY0007  07
147700050622PROMSG    *LIBL     KEY0008  08
147800050622PROMSG    *LIBL     KEY0009  09
147900050622PROMSG    *LIBL     KEY0010  10
148000050622PROMSG    *LIBL     KEY0011  11
148100050622PROMSG    *LIBL     KEY0012  12
148200050622PROMSG    *LIBL     KEY0013  13
148300050622PROMSG    *LIBL     KEY0014  14
148400050622PROMSG    *LIBL     KEY0015  15
148500050622PROMSG    *LIBL     KEY0016  16
148600050622PROMSG    *LIBL     KEY0017  17
148700050622PROMSG    *LIBL     KEY0018  18
148800050622PROMSG    *LIBL     KEY0019  19
148900050622PROMSG    *LIBL     KEY0020  20
149000050622PROMSG    *LIBL     KEY0021  21
149100050622PROMSG    *LIBL     KEY0022  22
149200050622PROMSG    *LIBL     KEY0023  23
149300050622PROMSG    *LIBL     KEY0024  24
149400050622**  ABILITAZIONE D1 /D2
1495000506220N  01
1496000506220N  02
1497000506221N  03
1498000506221N  04
1499000506221N  05
1500000506221N  06
1501000506220N  07
1502000506220N  08
1503000506220N  09
1504000506220N  10
1505000506220N  11
1506000506221N  12
1507000506220N  13
1508000506220N  14
1509000506220N  15
1510000506220N  16
1511000506220N  17
1512000506221N  18
1513000506220N  19
1514000506220N  20
1515001501160N  21
1516000506220N  22
1517000506220N  23   Impostare '1' se si usano le opzioni
1518000506221N  24   Lasciare sempre impostata a '0' il valore della riga 24
151900050622** TASTI DI FUNZIONE  D3
152000050622PROMSG    *LIBL     KEY0001  01
152100050622PROMSG    *LIBL     KEY0002  02
152200050622PROMSG    *LIBL     KEY0003  03
152300050622PROMSG    *LIBL     KEY0004  04
152400050622PROMSG    *LIBL     KEY0005  05
152500050622PROMSG    *LIBL     KEY0006  06
152600050622PROMSG    *LIBL     KEY0007  07
152700050622PROMSG    *LIBL     KEY0008  08
152800050622PROMSG    *LIBL     KEY0009  09
152900050622PROMSG    *LIBL     KEY0010  10
153000050622PROMSG    *LIBL     KEY0011  11
153100050622PROMSG    *LIBL     KEY0012  12
153200050622PROMSG    *LIBL     KEY0013  13
153300050622PROMSG    *LIBL     KEY0014  14
153400050622PROMSG    *LIBL     KEY0015  15
153500050622PROMSG    *LIBL     KEY0016  16
153600050622PROMSG    *LIBL     KEY0017  17
153700050622PROMSG    *LIBL     KEY0018  18
153800050622PROMSG    *LIBL     KEY0019  19
153900050622PROMSG    *LIBL     KEY0020  20
154000050622PROMSG    *LIBL     KEY0021  21
154100050622PROMSG    *LIBL     KEY0022  22
154200050622PROMSG    *LIBL     KEY0023  23
154300050622PROMSG    *LIBL     KEY0024  24
154400050622**  ABILITAZIONE D3
1545000506220N  01
1546000506220N  02
1547000506221N  03
1548000506220N  04
1549000506220N  05
1550000506220N  06
1551000506220N  07
1552000506220N  08
1553000506220N  09
1554000506220N  10
1555000506220N  11
1556000506221N  12
1557000506220N  13
1558000506220N  14
1559000506220N  15
1560000506220N  16
1561000506220N  17
1562000506220N  18
1563000506220N  19
1564000506220N  20
1565000506220N  21
1566000506220N  22
1567000506220N  23   Impostare '1' se si usano le opzioni
1568000506221N  24   Lasciare sempre impostata a '0' il valore della riga 24
