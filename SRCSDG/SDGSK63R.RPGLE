000100050622     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200050622     H*PARMS BNDDIR(PJXBND) CVTOPT(*NONE)
000300050622     H DECEDIT('0,') DATEDIT(*DMY.)
000400050622      *
000500150205      *          Cerificazione unica - invio telematico
000600150112      *                    Stampa file generato
000700050622      *           ----------------------------------------
000800050622      *
000900050622      *
001000050622     F*----------------------------------------------------*
001100150204     FSDGSK63D  CF   E             WORKSTN
001200150204     F                                     RENAME(SK63D1:FMTD1)
001300150204     F                                     RENAME(SK63T1:FMTT1)
001400150204     F                                     RENAME(SK63Z1:FMTZ1)
001500050622    >F* eventuali schermi supplementari
001600150204     F                                     RENAME(SK63D2:FMTD2)
001700050622    >F* modifica valori assunti
001800150204     F                                     RENAME(SK63D3:FMTD3)
001900150204     F                                     RENAME(SK63Z3:FMTZ3)
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
006700150318R170 D  $TipoStp                      1A
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
008100150204    >D SdgSK63DS     E DS
008200150205    >D  ERRK63       E                     INZ('0')
008300050622     D*-------------
008400050622     D* Passaggio Parametri al PGM di estrazione dati
008500150204    >D SDGSK64DS     E DS
008600150205    >D  ERRK64       E                     INZ('0')
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
021000150205     C                   Move      'CU'          TpScar122
021100050622     C                   Clear                   KPJBU
021200050622     C                   Movel     ANA122DS      KPJBU
021300050622     C                   Call      'ANA122R'
021400050622     C                   Parm                    KPJBA
021500050622     C                   Movel     KPJBU         ANA122DS
021600050622     C*
021700050622     C                   if        File122  <>   *Blanks
021800050622     C                   MoveL     File122       D1File
021900050622     C                   endif
022000150318R170 C*
022100150318 "   C     H1NMFL        WHENEQ    'D1TIPOSTP'
022200150318R170 C                   EXSR      F4_TIPOSTP
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
023800150318R170 C************************************************************
023900150318 "   C* Ricerca tipologia stampa
024000150318R170 C************************************************************
024100150112     C     F4_TipoStp    BEGSR
024200150112     C*
024300150112     C                   Reset                   ANA964DS
024400150112     C*
024500150112     C                   MoveL     '01'          Opz964
024600150204     C                   MoveL     'SDGSK63S'    Trc964
024700150204     C                   MoveL     'TIPOSTPK63'  Cam964
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
025800150318R170 C*
025900150318R170 C                   ENDSR
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
039900150205     C                   EXSR      RIED1_K63
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
041200150318R170 C                   Eval      D1TipoStp   = '1'
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
042900150205     C     RIED1_K63     BEGSR
043000050622     C*
043100150205     C                   Eval      D1File      = FileK63
043200150318R170 C                   Eval      D1TipoStp   = TipoStpK63
043300050622     C*
043400050622     C                   ENDSR
043500050622     C/EJECT
043600050622     C************************************************************
043700050622     C* RIEMPIMENTO VIDEATA  D1
043800050622     C************************************************************
043900050622     C     RIED1         BEGSR
044000050622     C*
044100050622     C* impostare i campi della videata dai parametri memorizzati su anmpe
044200050622     C*
044300050711     C                   Eval      D1File      = $File
044400150318R170 C                   Eval      D1TipoStp   = $TipoStp
044500050622     C*
044600050622     C                   ENDSR
044700050622     C/EJECT
044800050622     C************************************************************
044900050622     C* GESTIONE F02 VIDEO D1
045000050622     C************************************************************
045100050622     C     F02D1         BEGSR
045200050622     C*
045300050622     C* per decodificare la videata richiamo la routine dei ctrl
045400050622     C* e poi disattivo gli eventuali messaggi di errore
045500050622     C                   EXSR      CTRD1
045600050622     C*
045700050622     C                   MOVE      *ALL'0'       IN5098
045800050622     C                   MOVEA     IN5098        *IN(50)
045900050622     C*
046000050622     C                   ENDSR
046100050622     C/EJECT
046200050622     C************************************************************
046300050622     C* GESTIONE F03 VIDEO D1
046400050622     C************************************************************
046500050622     C     F03D1         BEGSR
046600050622     C*
046700050622     C                   MOVE      *ON           $FINE
046800050622     C* fine programma
046900050622     C                   EXSR      ENDPGM
047000050622     C*
047100050622     C                   ENDSR
047200050622     C/EJECT
047300050622     C************************************************************
047400050622     C* GESTIONE F05 VIDEO D1
047500050622     C************************************************************
047600050622     C     F05D1         BEGSR
047700050622     C*
047800050622     C                   MOVE      *ON           $INZD1
047900050622     C*
048000050622     C                   ENDSR
048100050622     C/EJECT
048200050622     C************************************************************
048300050622     C* GESTIONE F08 VIDEO D1
048400050622     C************************************************************
048500050622     C     F08D1         BEGSR
048600050622     C*
048700050622     C                   MOVE      *ON           $FINE
048800050622     C*
048900050622     C                   ENDSR
049000050622     C/EJECT
049100050622     C************************************************************
049200050622     C* GESTIONE F12 VIDEO D1
049300050622     C************************************************************
049400050622     C     F12D1         BEGSR
049500050622     C*
049600050622     C                   MOVE      *ON           $FINE
049700050622     C*
049800050622     C                   ENDSR
049900050622     C/EJECT
050000050622     C************************************************************
050100050622     C* GESTIONE F18 VIDEO D1
050200050622     C************************************************************
050300050622     C     F18D1         BEGSR
050400050622     C*
050500050622     C                   MOVE      'D3'          $GEST
050600050622     C                   MOVE      *ON           $INZD3
050700050622     C*
050800050622     C                   ENDSR
050900050622     C/EJECT
051000050622     C************************************************************
051100050622     C* GESTIONE F21 VIDEO D1
051200050622     C************************************************************
051300050622     C     F21D1         BEGSR
051400050622     C*
051500150204     C                   MOVE      'SK64'        KCOAZ
051600050622     C                   CALL      'BCH09'
051700050622     C                   PARM                    KPJBA
051800050622     C*
051900050622     C                   ENDSR
052000050622     C/EJECT
052100050622     C************************************************************
052200050622     C* GESTIONE F24 VIDEO D1
052300050622     C************************************************************
052400050622     C     F24D1         BEGSR
052500050622     C*
052600050622     C                   MOVEA     $FX1          $FX
052700050622     C                   MOVEA     $FC1          $FC
052800050622     C                   MOVEA     $FR1          $FR
052900050622     C                   Z-ADD     $ULKD1        $ULK
053000050622     C                   Z-ADD     79            $L01
053100050622     C                   Z-ADD     79            $L02
053200050622     C                   EXSR      RIEKEY
053300050622     C                   Z-ADD     $ULK          $ULKD1            3 0
053400050622     C                   MOVEA     $FC           $FC1
053500050622     C                   MOVEL     $KEY1         Z1KE1
053600050622     C                   MOVEL     $KEY2         Z1KE2
053700050622     C                   WRITE     FMTZ1
053800050622     C*
053900050622     C                   ENDSR
054000050622     C/EJECT
054100050622     C************************************************************
054200050622     C* GESTIONE F24 VIDEO D3
054300050622     C************************************************************
054400050622     C     F24D3         BEGSR
054500050622     C*
054600050622     C                   MOVEA     $FX3          $FX
054700050622     C                   MOVEA     $FC3          $FC
054800050622     C                   MOVEA     $FR3          $FR
054900050622     C                   Z-ADD     $ULKD3        $ULK
055000050622     C                   Z-ADD     44            $L01
055100050622     C                   Z-ADD     0             $L02
055200050622     C                   EXSR      RIEKEY
055300050622     C                   Z-ADD     $ULK          $ULKD3            3 0
055400050622     C                   MOVEA     $FC           $FC3
055500050622     C                   MOVEL     $KEY1         Z3KE1
055600050622     C                   WRITE     FMTZ3
055700050622     C*
055800050622     C                   ENDSR
055900050622     C/EJECT
056000050622     C************************************************************
056100050622     C* CONTROLLO VIDEATA
056200050622     C************************************************************
056300050622     C     CTRD1         BEGSR
056400050622     C*
056500050622     C                   SETOFF                                       99
056600050622     C* non imposto pos. del cursore
056700050622     C                   Z-ADD     0             H1RIGA
056800050622     C                   Z-ADD     0             H1COLO
056900050711     C*
057000050711     C*
057100050711     C* file da stampare
057200050711     C                   EXSR      CtrFile
057300150112     C*
057400150318R170 C* tipologia di stampa
057500150318 "   C                   Clear                   D1TipoStpD
057600150318R170 C                   Exsr      CtrTipStp
057700050711     C*
057800050622     C                   ENDSR
057900050622     C/EJECT
058000050711     C***********************************************************
058100050711     C* Controllo file da stampare
058200050711     C***********************************************************
058300050711     C     CtrFile       BEGSR
058400050711     C*
058500050711     C                   if        D1File     = *Blank
058600050711     C                   SetOn                                        5199
058700050711     C                   else
058800050711     C* verifico se esiste, nella libreria
058900050711     C* dei file per lo scarico su supporto magnetico,
059000050711     C* il file indicato a video
059100050711     C                   EXSR      ChkFile
059200050711     C*
059300050711     C                   endif
059400050711     C*
059500050711     C                   ENDSR
059600050711     C/EJECT
059700150318R170 C***********************************************************
059800150318 "   C* Controllo tipologia di stampa
059900150318R170 C***********************************************************
060000150112     C     CtrTipStp     BegSr
060100150112     C*
060200150112     C                   Clear                   D1TipoStpD
060300150112     C*
060400150112     C                   Clear                   X07ValDs
060500150112     C*
060600150112     C                   Move      '1'           X07Ric
060700150112     C                   Move      *Off          X07All
060800150204     C                   Movel     'SDGSK63S'    X07TRc
060900150205     C                   Movel     'TIPOSTPK63'  X07Cam
061000150112     C                   Movel     D1TipoStp     X07VAL
061100150112     C*
061200150112     C                   Callb     'X07VALR'
061300150112     C                   Parm                    X07ValDs
061400150112     C*
061500150112     C     X07ERR        IFEQ      *Off
061600150112     C                   MOVEL     X07DES        D1TipoStpD
061700150112     C                   ElSe
061800150112     C                   SetOn                                        5299
061900150112     C                   EndIf
062000150112     C*
062100150318R170 C                   EndSr
062200150318R170 C/EJECT
062300050711     C************************************************************
062400050711     C* Verifico l'esistenza del file nella libreria indicata
062500050711     C* per lo scarico su supporto magnetico
062600050711     C************************************************************
062700050711     C     ChkFile       BEGSR
062800050711     C*
062900050711     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
063000050711     C                   Eval      Cmd         =
063100050711     C                             'CHKOBJ     OBJ('        +
063200050711     C                             %Trim(Lib022) + '/' +
063300050711     C                             %Trim(D1File) +
063400050711     C                             ') OBJTYPE(*FILE)'
063500050711     C*
063600050711     C                   Call      'QCMDEXC'                            21
063700050711     C                   Parm                    Cmd             120
063800050711     C                   Parm      120           Len              15 5
063900050711     C*
064000050711     C                   if        *In21       = *On
064100050711     C*
064200050711     C* Errore: dico che il file non è stato trovato nella libreria
064300050711     C* Valorizzo le variabili del msg PRO1797
064400050711     C                   Move      D1File        §File
064500050711     C                   Move      Lib022        §Libreria
064600050711     C                   Move      §Fil_Lib      P1Fil_Lib
064700050711     C                   SetOn                                        5099
064800050711     C*
064900050711     C                   endif
065000050711     C*
065100050711     C                   ENDSR
065200050711     C/Eject
065300050622     C************************************************************
065400050622     C* GESTIONE VIDEO RECORD  D2
065500050622     C************************************************************
065600050622    >C     GESD2         BEGSR
065700050622     C* emissione piede videata se proveniente da altra
065800050622     C* salvataggio valore formato di provenienza
065900050622   B1C     $LASTV        IFNE      'D2'
066000050622     C                   WRITE     FMTT1
066100050622     C                   EXSR      GESPIE
066200050622     C                   MOVE      $LASTV        $LASTG
066300050622     C                   MOVE      'D2'          $LASTV
066400050622   E1C                   ENDIF
066500050622     C*
066600050622     C* se esistono errori sulla videata
066700050622     C* emetto la write del formato a indicatori spenti per vedere
066800050622     C* le eventuali decodifiche
066900050622   B1C     *IN99         IFEQ      *ON
067000050622     C     $GEST         ANDEQ     'D2'
067100050622     C                   MOVEA     *IN           WIN
067200050622     C                   MOVE      *ALL'0'       IN5098           49
067300050622     C                   MOVEA     IN5098        *IN(50)
067400050622     C                   WRITE     FMTD2
067500050622     C                   MOVEA     WIN           *IN
067600050622   E1C                   ENDIF
067700050622     C*
067800050622     C                   EXFMT     FMTD2
067900050622     C*
068000050622     C* controllo abilitazione tasti funzione
068100050622     C                   EXSR      CTRKD1
068200050622     C*
068300050622   B1C     *IN99         IFEQ      *OFF
068400050622     C*
068500050622   B2C                   SELECT
068600050622     C* ROLDOWN
068700050622     C     $TASTO        WHENEQ    ROLDWN
068800050622     C                   MOVE      'D1'          $GEST
068900050622     C* F3=Fine
069000050622     C     $TASTO        WHENEQ    F03
069100050622     C                   EXSR      F03D1
069200050622     C* F4=Ricerca
069300050622     C     $TASTO        WHENEQ    F04
069400050622     C                   EXSR      SEARCH
069500050622     C* F5=Refresh
069600050622     C     $TASTO        WHENEQ    F05
069700050622     C                   EXSR      F05D2
069800050622     C* F8=Successivo
069900050622     C     $TASTO        WHENEQ    F08
070000050622     C                   EXSR      F08D1
070100050622     C* F12=Ritorno
070200050622     C     $TASTO        WHENEQ    F12
070300050622     C                   EXSR      F12D2
070400050622     C* F18=Valori assunti
070500050622     C     $TASTO        WHENEQ    F18
070600050622     C                   EXSR      F18D1
070700050622     C* F21=Pers. batch
070800050622     C     $TASTO        WHENEQ    F21
070900050622     C                   EXSR      F21D1
071000050622     C* F24=Altri tasti
071100050622     C     $TASTO        WHENEQ    F24
071200050622     C                   EXSR      F24D1
071300050622     C*
071400050622   X2C                   OTHER
071500050622     C                   EXSR      CTRD2
071600050622   B3C     *IN99         IFEQ      *OFF
071700050622     C                   EXSR      CTRD1
071800050622   B4C     *IN99         IFEQ      *ON
071900050622     C                   SETON                                            96
072000050622   E4C                   ENDIF
072100050622   E3C                   ENDIF
072200050622     C*
072300050622     C*
072400050622   B3C     $TASTO        IFEQ      F06
072500050622     C     *IN99         ANDEQ     *OFF
072600050622     C* eseguo lancio
072700050622     C                   EXSR      EXELAN
072800050622     C                   EXSR      ENDPGM
072900050622   E3C                   ENDIF
073000050622     C*
073100050622   E2C                   ENDSL
073200050622     C*
073300050622   E1C                   ENDIF
073400050622     C*
073500050622    >C                   ENDSR
073600050622     C/EJECT
073700050622     C************************************************************
073800050622     C* GESTIONE VALORI ASSUNTI
073900050622     C************************************************************
074000050622     C     GESD3         BEGSR
074100050622     C*
074200050622   B1C     $INZD3        IFEQ      *ON
074300050622     C                   EXSR      INZD3
074400050622     C                   MOVE      *OFF          $INZD3
074500050622   E1C                   ENDIF
074600050622     C*
074700050622   B1C     $LASTV        IFNE      'D3'
074800050622     C                   EXSR      GESPIE
074900050622     C                   MOVE      $LASTV        $LASTG
075000050622     C                   MOVE      'D3'          $LASTV
075100050622   E1C                   ENDIF
075200050622     C*
075300050622     C* se esistono errori sulla videata
075400050622     C* emetto la write del formato a indicatori spenti per vedere
075500050622     C* le eventuali decodifiche
075600050622   B1C     *IN99         IFEQ      *ON
075700050622     C                   MOVEA     *IN           WIN
075800050622     C                   MOVE      *ALL'0'       IN5098           49
075900050622     C                   MOVEA     IN5098        *IN(50)
076000050622     C                   WRITE     FMTD3
076100050622     C                   MOVEA     WIN           *IN
076200050622   E1C                   ENDIF
076300050622     C*
076400050622     C                   EXFMT     FMTD3
076500050622     C*
076600050622     C* controllo abilitazione tasti funzione
076700050622     C                   EXSR      CTRKD3
076800050622     C*
076900050622   B1C     *IN99         IFEQ      *OFF
077000050622     C*
077100050622   B2C                   SELECT
077200050622     C* F3=Fine
077300050622     C     $TASTO        WHENEQ    F03
077400050622     C                   EXSR      F03D1
077500050622     C* F12=Ritorno
077600050622     C     $TASTO        WHENEQ    F12
077700050622     C                   EXSR      F12D3
077800050622     C* F24=Altri tasti
077900050622     C     $TASTO        WHENEQ    F24
078000050622     C                   EXSR      F24D3
078100050622     C*
078200050622     C                   OTHER
078300050622     C                   EXSR      CTRD3
078400050622     C     *IN99         IFEQ      *OFF
078500050622     C                   MOVE      $LASTG        $GEST
078600050622     C                   ENDIF
078700050622     C*
078800050622   E2C                   ENDSL
078900050622     C*
079000050622   E1C                   ENDIF
079100050622     C*
079200050622     C                   ENDSR
079300050622     C/EJECT
079400050622     C************************************************************
079500050622     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
079600050622     C************************************************************
079700050622     C     INZD3         BEGSR
079800050622     C*
079900050622     C                   CLEAR                   FMTD3
080000050622     C     $VAL          IFEQ      *ON
080100050622     C                   MOVE      XSCON         D3VAL
080200050622     C                   ELSE
080300050622     C                   MOVE      XSCOFF        D3VAL
080400050622     C                   ENDIF
080500050622     C                   MOVE      XSCON         D3ON
080600050622     C                   MOVE      XSCOFF        D3OF
080700050622     C*
080800050622     C                   ENDSR
080900050622     C/EJECT
081000050622     C************************************************************
081100050622     C* RIEMPIMENTO VIDEATA D2
081200050622     C************************************************************
081300050622    >C     RIED2         BEGSR
081400050622     C*
081500050622     C*
081600050622    >C                   ENDSR
081700050622     C/EJECT
081800050622     C************************************************************
081900050622     C* GESTIONE F05 VIDEO D2
082000050622     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
082100050622     C* e il riposizionamento alla prima
082200050622     C************************************************************
082300050622    >C     F05D2         BEGSR
082400050622     C*
082500050622     C                   MOVE      *ON           $INZD1
082600050622     C                   MOVE      'D1'          $GEST
082700050622     C*
082800050622    >C                   ENDSR
082900050622     C/EJECT
083000050622     C************************************************************
083100050622     C* GESTIONE F12 VIDEO D2
083200050622     C************************************************************
083300050622     C     F12D2         BEGSR
083400050622     C*
083500050622     C                   MOVE      'D1'          $GEST
083600050622     C*
083700050622     C                   ENDSR
083800050622     C/EJECT
083900050622     C************************************************************
084000050622     C* GESTIONE F12 VIDEO D3
084100050622     C************************************************************
084200050622     C     F12D3         BEGSR
084300050622     C*
084400050622     C                   MOVE      $LASTG        $GEST
084500050622     C*
084600050622     C                   ENDSR
084700050622     C/EJECT
084800050622     C************************************************************
084900050622     C* CONTROLLO VIDEATA  D2
085000050622     C************************************************************
085100050622    >C     CTRD2         BEGSR
085200050622     C*
085300050622     C                   SETOFF                                       99
085400050622     C* non imposto pos. del cursore
085500050622     C                   Z-ADD     0             H1RIGA
085600050622     C                   Z-ADD     0             H1COLO
085700050622     C*
085800050622     C                   ENDSR
085900050622     C/EJECT
086000050622     C************************************************************
086100050622     C* CONTROLLO VIDEATA  D3
086200050622     C************************************************************
086300050622     C     CTRD3         BEGSR
086400050622     C*
086500050622     C                   SETOFF                                       99
086600050622     C* non imposto pos. del cursore
086700050622     C                   Z-ADD     0             H1RIGA
086800050622     C                   Z-ADD     0             H1COLO
086900050622     C*
087000050622     c* Controllo VALORE
087100050622     C     D3VAL         IFNE      XSCON
087200050622     C     D3VAL         ANDNE     XSCOFF
087300050622     C                   SETON                                        5099
087400050622     C                   ENDIF
087500050622     C*
087600050622     C     D3VAL         IFEQ      XSCON
087700050622     C                   MOVE      *ON           $VAL
087800050622     C                   ELSE
087900050622     C                   MOVE      *OFF          $val
088000150204     C                   endif
088100050622     C*
088200050622     C                   ENDSR
088300050622     C/EJECT
088400050622     C************************************************************
088500050622     C* ESECUZIONE LANCIO
088600050622     C************************************************************
088700050622     C     EXELAN        BEGSR
088800050622     C*
088900150204     C                   RESET                   SDGSK64DS
089000050622     C*
089100150205     C                   MOVE      XSCSOC        SocietaK64
089200050622     C*
089300150205     C                   Eval      FileK64     = D1File
089400150205     C                   Eval      LibK64      = Lib022
089500150318R170 C                   Eval      TipoStpK64  = D1TipoStp
089600050622     C*
089700150204     C                   MOVEL     SDGSK64DS     KPJBU
089800050712     C*
089900150204     C                   MOVE      'SK64'        KCOAZ
090000050712     C*
090100050712     C                   Clear                   $Inter            1
090200150112     C                   eval      $Inter      = *On
090300050712     C                   if        $Inter      = *On
090400150204    >C                   CALL      'SDGSK64C'
090500050712     C                   PARM                    KPJBA
090600050712     C                   else
090700050712    >C                   CALL      'BCH10'
090800050712     C                   PARM                    KPJBA
090900050712     C                   endif
091000050622     C*
091100050622     C* aggiorno valori assunti
091200050622     C                   EXSR      AGGMPE
091300050622     C*
091400050622     C                   Eval      $Fine       = *On
091500050622     C*
091600050622     C                   ENDSR
091700050622     C/EJECT
091800050622     C************************************************************
091900050622     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
092000050622     C************************************************************
092100050622     C     CTRKD1        BEGSR
092200050622     C*
092300050622     C                   SETOFF                                       99
092400050622     C                   MOVEA     $FC1          $FC
092500050622     C                   EXSR      CTRKEY
092600050622     C     *IN99         IFEQ      *ON
092700050622     C                   SETON                                        97
092800050622     C                   ENDIF
092900050622     C*
093000050622     C                   ENDSR
093100050622     C/EJECT
093200050622     C************************************************************
093300050622     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
093400050622     C* INPUT
093500050622     C* $FC3 = Abilitazioni videata D3
093600050622     C* OUTPUT
093700050622     C* *IN97 *IN99 Errori
093800050622     C************************************************************
093900050622     C     CTRKD3        BEGSR
094000050622     C*
094100050622     C                   SETOFF                                       99
094200050622     C                   MOVEA     $FC3          $FC
094300050622     C                   EXSR      CTRKEY
094400050622     C     *IN99         IFEQ      *ON
094500050622     C                   SETON                                        97
094600050622     C                   ENDIF
094700050622     C*
094800050622     C                   ENDSR
094900050622     C/EJECT
095000050622     C************************************************************
095100050622     C* CONTROLLO ABILITAZIONE TASTI
095200050622     C************************************************************
095300050622     C     CTRKEY        BEGSR
095400050622     C*
095500050622     C                   SELECT
095600050622     C     $TASTO        WHENEQ    F01
095700050622     C     $FC(1)        IFEQ      '0'
095800050622     C                   SETON                                        99
095900050622     C                   ENDIF
096000050622     C     $TASTO        WHENEQ    F02
096100050622     C     $FC(2)        IFEQ      '0'
096200050622     C                   SETON                                        99
096300050622     C                   ENDIF
096400050622     C     $TASTO        WHENEQ    F03
096500050622     C     $FC(3)        IFEQ      '0'
096600050622     C                   SETON                                        99
096700050622     C                   ENDIF
096800050622     C     $TASTO        WHENEQ    F04
096900050622     C     $FC(4)        IFEQ      '0'
097000050622     C                   SETON                                        99
097100050622     C                   ENDIF
097200050622     C     $TASTO        WHENEQ    F05
097300050622     C     $FC(5)        IFEQ      '0'
097400050622     C                   SETON                                        99
097500050622     C                   ENDIF
097600050622     C     $TASTO        WHENEQ    F06
097700050622     C     $FC(6)        IFEQ      '0'
097800050622     C                   SETON                                        99
097900050622     C                   ENDIF
098000050622     C     $TASTO        WHENEQ    F07
098100050622     C     $FC(7)        IFEQ      '0'
098200050622     C                   SETON                                        99
098300050622     C                   ENDIF
098400050622     C     $TASTO        WHENEQ    F08
098500050622     C     $FC(8)        IFEQ      '0'
098600050622     C                   SETON                                        99
098700050622     C                   ENDIF
098800050622     C     $TASTO        WHENEQ    F09
098900050622     C     $FC(09)       IFEQ      '0'
099000050622     C                   SETON                                        99
099100050622     C                   ENDIF
099200050622     C     $TASTO        WHENEQ    F10
099300050622     C     $FC(10)       IFEQ      '0'
099400050622     C                   SETON                                        99
099500050622     C                   ENDIF
099600050622     C     $TASTO        WHENEQ    F11
099700050622     C     $FC(11)       IFEQ      '0'
099800050622     C                   SETON                                        99
099900050622     C                   ENDIF
100000050622     C     $TASTO        WHENEQ    F12
100100050622     C     $FC(12)       IFEQ      '0'
100200050622     C                   SETON                                        99
100300050622     C                   ENDIF
100400050622     C     $TASTO        WHENEQ    F13
100500050622     C     $FC(13)       IFEQ      '0'
100600050622     C                   SETON                                        99
100700050622     C                   ENDIF
100800050622     C     $TASTO        WHENEQ    F14
100900050622     C     $FC(14)       IFEQ      '0'
101000050622     C                   SETON                                        99
101100050622     C                   ENDIF
101200050622     C     $TASTO        WHENEQ    F15
101300050622     C     $FC(15)       IFEQ      '0'
101400050622     C                   SETON                                        99
101500050622     C                   ENDIF
101600050622     C     $TASTO        WHENEQ    F16
101700050622     C     $FC(16)       IFEQ      '0'
101800050622     C                   SETON                                        99
101900050622     C                   ENDIF
102000050622     C     $TASTO        WHENEQ    F17
102100050622     C     $FC(17)       IFEQ      '0'
102200050622     C                   SETON                                        99
102300050622     C                   ENDIF
102400050622     C     $TASTO        WHENEQ    F18
102500050622     C     $FC(18)       IFEQ      '0'
102600050622     C                   SETON                                        99
102700050622     C                   ENDIF
102800050622     C     $TASTO        WHENEQ    F19
102900050622     C     $FC(19)       IFEQ      '0'
103000050622     C                   SETON                                        99
103100050622     C                   ENDIF
103200050622     C     $TASTO        WHENEQ    F20
103300050622     C     $FC(20)       IFEQ      '0'
103400050622     C                   SETON                                        99
103500050622     C                   ENDIF
103600050622     C     $TASTO        WHENEQ    F21
103700050622     C     $FC(21)       IFEQ      '0'
103800050622     C                   SETON                                        99
103900050622     C                   ENDIF
104000050622     C     $TASTO        WHENEQ    F22
104100050622     C     $FC(22)       IFEQ      '0'
104200050622     C                   SETON                                        99
104300050622     C                   ENDIF
104400050622     C     $TASTO        WHENEQ    F23
104500050622     C     $FC(23)       IFEQ      '0'
104600050622     C                   SETON                                        99
104700050622     C                   ENDIF
104800050622     C     $TASTO        WHENEQ    F24
104900050622     C     $FC(24)       IFEQ      '0'
105000050622     C                   SETON                                        99
105100050622     C                   ENDIF
105200050622     C                   ENDSL
105300050622     C*
105400050622     C                   ENDSR
105500050622     C/EJECT
105600050622     C************************************************************
105700050622     C* OPERAZIONI INIZIALI
105800050622     C************************************************************
105900050622     C     *INZSR        BEGSR
106000050622     C*
106100050622     C* Reperimento parametri
106200050622     C*
106300050622     C     *ENTRY        PLIST
106400050622     C                   PARM                    KPJBA
106500050622     C*
106600050622     C* Reperimento tasti di funzione
106700050622     C*
106800050622     C                   EXSR      REPKEY
106900050622     C*
107000050622     C                   ENDSR
107100050622     C/EJECT
107200050622     C************************************************************
107300050622     C* REPERIMENTO TASTI DI FUNZIONE
107400050622     C************************************************************
107500050622     C     REPKEY        BEGSR
107600050622     C                   MOVEA     $FM1          $FM
107700050622     C                   MOVEA     $Fc1          $Fc
107800050622     C                   EXSR      REPKKK
107900050622     C                   MOVEA     $FX           $FX1
108000050622     C*
108100050622     C                   MOVEA     $FM3          $FM
108200050622     C                   MOVEA     $Fc3          $FM
108300050622     C                   EXSR      REPKKK
108400050622     C                   MOVEA     $FX           $FX3
108500050622     C*
108600050622     C                   ENDSR
108700050622     C/EJECT
108800050622     C************************************************************
108900050622     C* REPERIMENTO TASTI DI FUNZIONE
109000050622     C************************************************************
109100050622     C     REPKKK        BEGSR
109200050622     C*
109300050622     C                   CLEAR                   $FX
109400050622     C     1             DO        24            X
109500050622     C*
109600050622     C     $FM(X)        IFNE      *BLANK
109700050622     C     $fc(X)        andne     '0'
109800050622     C*
109900050622     C                   MOVEL     $FM(X)        IDMSG
110000050622     C                   CALLB     'XRTVM'
110100050622     C                   PARM                    IDMSG            27
110200050622     C                   PARM                    TXTMSG          100
110300050622     C                   PARM                    ERRMSG            1
110400050622     C*
110500050622     C     ERRMSG        IFNE      *ON
110600050622     C                   MOVEL     TXTMSG        $FX(X)
110700050622     C                   ELSE
110800050622     C                   MOVEL     *ALL' '       $FX(X)
110900050622     C                   ENDIF
111000050622     C*
111100050622     C                   ENDIF
111200050622     C*
111300050622     C                   ENDDO
111400050622     C*
111500050622     C                   ENDSR
111600050622     C/EJECT
111700050622     C************************************************************
111800050622     C* INIZIALIZZAZIONE VARIABILI
111900050622     C************************************************************
112000050622     C     INZVAR        BEGSR
112100050622     C*
112200050622     C* se il pgm è stato richiamato imposto i valori ricevuti dalla DS
112300050622     C                   if        KPJBU      <> *Blanks
112400050622     C                   Eval      CallPgm     = '1'
112500150204     C                   MoveL     KPJBU         SDGSK63DS
112600050622     C                   else
112700050622     C                   Eval      CallPgm     = '0'
112800050622     C                   endif
112900050622     C*
113000050622     C* Reperimento dati società
113100050622     C*
113200050622    >C                   MOVEL     'SOC001'      TIPXSC
113300050622     C                   MOVEL     *BLANK        SOCXSC
113400050622     C                   EXSR      REPSOC
113500050622     C     RTNXSC        IFNE      '1'
113600050622     C                   MOVEL     XSOCDS        SOC001
113700050622     C                   ELSE
113800050622     C                   RETURN
113900050622     C                   ENDIF
114000050622     C*
114100050622     C* Reperisco la libreria in cui è stato memorizzato il file
114200050622     C* che verrà stampato
114300050622     C                   EXSR      RepLib
114400050622     C*
114500050622     C* Asteriscare se non presenti
114600050622     C* Reperimento autorizzazioni
114700050622     C*
114800050622    >C***                EXSR      REPAUT
114900050622     C*
115000050622     C* Reperimento valori default utente
115100050622     C*
115200050622     C                   EXSR      REPDFT
115300050622     C*
115400050622     C* Definizione dei tasti funzione utilizzabili a pgm
115500050622     C*
115600050622     C                   EXSR      DEFOPZ
115700050622     C*
115800050622     C* Variabili per gestione videate
115900050622     C*
116000050622     C                   MOVE      'D1'          $GEST
116100050622     C                   MOVE      *OFF          $FINE
116200050622     C                   MOVE      *ON           $INZD1
116300050622     C                   MOVE      *BLANK        $LASTG
116400050622     C                   MOVE      *BLANK        $LASTV
116500050622     C                   Z-ADD     0             $ULKD1            3 0
116600050622     C                   Z-ADD     0             $ULKD3            3 0
116700050622     C*
116800050622     C* Variabili appoggio
116900050622     C*
117000050622     C                   Z-ADD     0             CURR
117100050622     C                   Z-ADD     0             CURC
117200050622     C*
117300050622     C* Valorizzazione campi univoci testate
117400050622     C*
117500050622     C                   CLEAR                   FMTT1
117600050622     C                   MOVEL     KNSIF         NOMSIF
117700050622     C                   MOVEL     XSCDSI        NOMDIT
117800150112     C                   MOVEL     KCDAZ         NOMAZN
117900050622     C* recupero il sottotitolo (se c'è)
118000050622     C                   MOVEL     *BLANK        T1TIT
118100050622     C     SOTTIT        IFNE      *BLANK
118200050622     C                   MOVEL     SOTTIT        IDMSG
118300050622     C                   CALLB     'XRTVM'
118400050622     C                   PARM                    IDMSG            27
118500050622     C                   PARM                    TXTMSG          100
118600050622     C                   PARM                    ERRMSG            1
118700050622     C                   PARM      *ON           CTRMSG            1
118800050622     C                   PARM      30            LENMSG            3 0
118900050622     C     ERRMSG        IFNE      *ON
119000050622     C                   MOVEL     TXTMSG        T1TIT
119100050622     C                   ELSE
119200050622     C                   MOVEL     *ALL'?'       T1TIT
119300050622     C                   ENDIF
119400050622     C                   ENDIF
119500050622     C*
119600050622     C* Inizializzazione/Decodifica prima videata
119700050622     C*
119800050622     C                   EXSR      INZD1
119900050622     C                   MOVE      *OFF          $INZD1
120000050622     C*
120100050622     C                   ENDSR
120200050622     C/EJECT
120300050622     C************************************************************
120400050622     C* REPERIMENTO DATI SOCIETA'
120500050622     C************************************************************
120600050622     C     REPSOC        BEGSR
120700050622     C*
120800150112     C                   CALL      'SDGSOCDR'
120900050622     C                   PARM                    TIPXSC            6
121000050622     C                   PARM                    SOCXSC            3
121100050622     C                   PARM                    CDSXSC            9 0
121200050622     C                   PARM                    MODXSC            3
121300050622     C                   PARM      *BLANKS       RTNXSC            1
121400050622     C                   PARM                    XSOCDS
121500050622     C                   PARM                    KPJBA
121600050622     C*
121700050622     C                   ENDSR
121800050622     C/EJECT
121900050622     C************************************************************
122000050622     C* Rerimento libreria per scarico su supporto magnetico
122100050622     C************************************************************
122200050622     C     REPLib        BEGSR
122300050622     C*
122400050622     C                   Clear                   ANP022DS
122500050622     C*
122600050622     C                   CALLB     'XPAR'
122700050622     C                   Parm                    XScSoc
122800050622     C                   Parm      'SCARSUMA'    XPaPar            8
122900050622     C                   Parm                    XPaOut         2000
123000050622     C                   Parm      *OFF          XPaErr            1
123100050622     C                   Parm      ' '           XPaRic            1
123200050622     C*
123300050622     C                   if        XPaErr      = *On
123400150205     C                   Move      '1'           ErrK63
123500050622     C                   Move      *on           $Fine
123600050622     C                   EXSR      EndPgm
123700050622     C*
123800050622     C                   else
123900050622     C                   MoveL     XPaOut        ANP022DS
124000050622     C                   endif
124100050622     C*
124200050622     C                   ENDSR
124300050622     C/EJECT
124400050622     C************************************************************
124500050622     C* REPERIMENTO AUTORIZZAZIONI UTENTE
124600050622     C************************************************************
124700050622     C*    REPAUT        BEGSR
124800050622     C*
124900050622     C*                  CLEAR                   AUTDS
125000050622     C*
125100050622     C*                  MOVEL     XSCSOC        XCASOC
125200050622     C*                  MOVEL     XSCDUN        XCAUNI
125300050622     C*                  Z-ADD     0             XCAGRP
125400050622     C*                  MOVEL     KNMUS         XCAPRF
125500050622    >C*                  MOVEL     'XXXXXX'      XCAFNC
125600050622    >C*                  MOVEL     *ZERO         XCAVFU
125700050622    >C*                  MOVE      'CK'          XCATCK
125800050622    >C*                  MOVE      '1'           XCANAU
125900050622     C*
126000050622     C*                  CALLB     'XCHKAUT'
126100050622     C*                  PARM                    AUTDS
126200050622     C*
126300050622     C* errori/non abilitazione
126400050622     C*    XCARTN        IFNE      0
126500050622     C*                  MOVE      *ON           $fine
126600050622     C*                  EXSR      ENDPGM
126700050622     C*                  ELSE
126800050622    >C*                  MOVEL     XCADDS        GXXXDS
126900050622     C* Se non autorizzati alla funzione oppure                              ed
127000050622    >C*    ABIxxx        IFEQ      *OFF
127100050622     C*                  MOVE      XCAFNC        XNOFNC
127200050622     C*                  MOVE      XCAPRF        XNOPRF
127300050622     C*                  MOVE      XCACUT        XNOCUT
127400050622     C*                  MOVE      XCATCK        XNOTCK
127500050622     C*                  MOVE      '1'           XNOERR
127600050622     C*                  CALLB     'XNOAUTR'
127700050622     C*                  PARM                    XNOADS
127800050622     C*                  MOVE      *ON           $fine
127900050622     C*                  EXSR      ENDPGM
128000050622     C*                  ENDIF
128100050622     C*                  ENDIF
128200050622     C*
128300050622     C*                  ENDSR
128400050622     C/EJECT
128500050622     C************************************************************
128600050622     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
128700050622     C************************************************************
128800050622     C     REPDFT        BEGSR
128900050622     C*
129000050622     C* preparo i parametri di chiamata al driver
129100050622     C                   CLEAR                   XMPEDS
129200050622     C                   EVAL      TpChiamata = '01'
129300050622     C                   EVAL      NChiavi = 5
129400050622     C                   EVAL      VLogica = 2
129500050622     C*
129600050622     C* preparo le chiavi per la chiamata al driver
129700050622     C                   CLEAR                   XMPEDS1
129800050622     C                   MOVEL     XSCSOC        DSMPESOC
129900050622     C                   MOVEL     DSPGM         DSMPEPGM
130000050622     C                   MOVEL     KNMUS         DSMPEPRF
130100050622     C* Si reperiscono i dati di default a seconda delle modalità con
130200050622     C* cui il pgm è usato
130300050622     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
130400050622     C* MPETIP = 'PV'  pgm richiamato da menù
130500050622     C* MPETIP = 'PB'  x lancio Batch
130600050622     C                   MOVEL     'PB'          DSMPETIP
130700050622     C*
130800050622     C                   CALLB     'XMPE'
130900050622     C                   PARM                    XMPEDS
131000050622     C                   PARM                    XMPEDS1
131100050622     C*
131200050622     C     Risultato     IFEQ      '0'
131300050622     C                   MOVEL     DSMPEPAR      DSMPE
131400050622     C* nel caso sia la prima volta che l'utente apre questa anagrafica
131500050622     C* i suoi valori di default vengono decisi a pgm
131600050622     C                   ELSE
131700050622     C                   ENDIF
131800050622     C*
131900050622     C                   ENDSR
132000050622     C/EJECT
132100050622     C************************************************************
132200050622     C* AGGIORNAMENTO VALORI ASSUNTI
132300050622     C************************************************************
132400050622     C     AGGMPE        BEGSR
132500050622     C*
132600050622     C* preparo i parametri di chiamata al driver
132700050622     C                   CLEAR                   XMPEDS
132800050622     C                   EVAL      TpChiamata = '03'
132900050622     C                   EVAL      NChiavi = 5
133000050622     C                   EVAL      VLogica = 2
133100050622     C*
133200050622     C* preparo le chiavi per la chiamata al driver
133300050622     C                   CLEAR                   XMPEDS1
133400050622     C                   MOVEL     XSCSOC        DSMPESOC
133500050622     C                   MOVEL     DSPGM         DSMPEPGM
133600050622     C                   MOVEL     KNMUS         DSMPEPRF
133700050622     C*
133800050622     C* Si memorizzano i dati di default a seconda delle modalità con
133900050622     C* cui il pgm è usato
134000050622     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
134100050622     C* MPETIP = 'PV'  pgm richiamato da menù
134200050622     C* MPETIP = 'PB'  x lancio Batch
134300050622     C*
134400050622     C                   MOVEL     'PB'          DSMPETIP
134500050622     C* dati
134600050711     C                   MoveL     D1File        $File
134700150318R170 C                   MoveL     D1TipoStp     $TipoStp
134800050622     C*
134900050622     C                   MOVEL     DSMPE         DSMPEPAR
135000050622     C*
135100050622     C                   CALLB     'XMPE'
135200050622     C                   PARM                    XMPEDS
135300050622     C                   PARM                    XMPEDS1
135400050622     C*
135500050622     C                   ENDSR
135600050622     C/EJECT
135700050622     C************************************************************
135800050622     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
135900050622     C************************************************************
136000050622     C     DEFOPZ        BEGSR
136100050622     C*
136200050622     C* Questa routine deve valorizzare la schiera $FC1/$FR1
136300050622     C* con S o N a seconda della utilizzabilità o meno
136400050622     C* del tasto funzione correlato
136500050622     C*
136600050622     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
136700050622     C* di compilazione
136800050622     C                   RESET                   $FC1
136900050622     C                   RESET                   $FR1
137000050622     C                   RESET                   $FC3
137100050622     C                   RESET                   $FR3
137200050622     C*
137300050622     C                   ENDSR
137400050622     C/EJECT
137500050622     C************************************************************
137600050622     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
137700050622     C************************************************************
137800050622     C     DEFCAM        BEGSR
137900050622     C*
138000050622     C* klist
138100050711     C*
138200050622     C*
138300050622     C*
138400050622     C* Variabili per gestione videate
138500050622     C*
138600050622     C                   MOVE      *BLANK        $GEST             2
138700050622     C                   MOVE      *BLANK        $FINE             1
138800050622     C                   MOVE      *BLANK        $INZD1            1
138900050622     C                   MOVE      *BLANK        $INZD3            1
139000050622     C                   MOVE      *BLANK        $LASTG            2
139100050622     C                   MOVE      *BLANK        $LASTV            2
139200050622     C                   Z-ADD     0             $PIED1            3 0
139300050622     C*
139400050622     C* Variabili appoggio
139500050622     C*
139600050622     C                   Z-ADD     0             CURR              2 0
139700050622     C                   Z-ADD     0             CURC              2 0
139800050622     C                   MOVE      *ZEROS        WIN              99
139900050622     C*
140000050622     C* Indici
140100050622     C*
140200050622     C                   Z-ADD     0             X                 3 0
140300050622     C                   Z-ADD     0             Y                 3 0
140400050622     C*
140500050622     C                   ENDSR
140600050622     C/EJECT
140700050622     C************************************************************
140800050622     C* RIEMPIMENTO TASTI FUNZIONE
140900050622     C************************************************************
141000050622     C     RIEKEY        BEGSR
141100050622     C*
141200050622     C* Input :
141300050622     C* - $FX = schiera con tutti i testi disponibili
141400050622     C* - $FC = schiere con i flag di validità associati ai testi
141500050622     C* - $FR = schiere con i flag di REVERSE IMAGE
141600050622     C* - $ULK = ultimo testo caricato
141700050622     C* - $L01 = Lunghezza prima riga
141800050622     C* - $L02 = Lunghezza seconda riga
141900050622     C* Output :
142000050622     C* - $ULK
142100050622     C* - $KEY1= 1° riga di tasti funzione
142200050622     C* - $KEY2= 2° riga di tasti funzione
142300050622     C*
142400050622     C                   CALLB     'XKEYMSG'
142500050622     C* input
142600050622     C                   PARM                    $FX
142700050622     C                   PARM                    $FC
142800050622     C                   PARM                    $FR
142900050622     C                   PARM                    $ULK              3 0
143000050622     C* output
143100050622     C                   PARM                    $KEY1            79
143200050622     C                   PARM                    $KEY2            79
143300050622    >C* input
143400050622    >C                   PARM                    $L01              3 0
143500050622    >C                   PARM                    $L02              3 0
143600050622     C*
143700050622     C                   ENDSR
143800050622** TASTI DI FUNZIONE  D1
143900050622PROMSG    *LIBL     KEY0001  01
144000050622PROMSG    *LIBL     KEY0002  02
144100050622PROMSG    *LIBL     KEY0003  03
144200050622PROMSG    *LIBL     KEY0004  04
144300050622PROMSG    *LIBL     KEY0005  05
144400050622PROMSG    *LIBL     KEY0006  06
144500050622PROMSG    *LIBL     KEY0007  07
144600050622PROMSG    *LIBL     KEY0008  08
144700050622PROMSG    *LIBL     KEY0009  09
144800050622PROMSG    *LIBL     KEY0010  10
144900050622PROMSG    *LIBL     KEY0011  11
145000050622PROMSG    *LIBL     KEY0012  12
145100050622PROMSG    *LIBL     KEY0013  13
145200050622PROMSG    *LIBL     KEY0014  14
145300050622PROMSG    *LIBL     KEY0015  15
145400050622PROMSG    *LIBL     KEY0016  16
145500050622PROMSG    *LIBL     KEY0017  17
145600050622PROMSG    *LIBL     KEY0018  18
145700050622PROMSG    *LIBL     KEY0019  19
145800050622PROMSG    *LIBL     KEY0020  20
145900050622PROMSG    *LIBL     KEY0021  21
146000050622PROMSG    *LIBL     KEY0022  22
146100050622PROMSG    *LIBL     KEY0023  23
146200050622PROMSG    *LIBL     KEY0024  24
146300050622**  ABILITAZIONE D1 /D2
1464000506220N  01
1465000506220N  02
1466000506221N  03
1467000506221N  04
1468000506221N  05
1469000506221N  06
1470000506220N  07
1471000506220N  08
1472000506220N  09
1473000506220N  10
1474000506220N  11
1475000506221N  12
1476000506220N  13
1477000506220N  14
1478000506220N  15
1479000506220N  16
1480000506220N  17
1481000506221N  18
1482000506220N  19
1483000506220N  20
1484001501160N  21
1485000506220N  22
1486000506220N  23   Impostare '1' se si usano le opzioni
1487000506221N  24   Lasciare sempre impostata a '0' il valore della riga 24
148800050622** TASTI DI FUNZIONE  D3
148900050622PROMSG    *LIBL     KEY0001  01
149000050622PROMSG    *LIBL     KEY0002  02
149100050622PROMSG    *LIBL     KEY0003  03
149200050622PROMSG    *LIBL     KEY0004  04
149300050622PROMSG    *LIBL     KEY0005  05
149400050622PROMSG    *LIBL     KEY0006  06
149500050622PROMSG    *LIBL     KEY0007  07
149600050622PROMSG    *LIBL     KEY0008  08
149700050622PROMSG    *LIBL     KEY0009  09
149800050622PROMSG    *LIBL     KEY0010  10
149900050622PROMSG    *LIBL     KEY0011  11
150000050622PROMSG    *LIBL     KEY0012  12
150100050622PROMSG    *LIBL     KEY0013  13
150200050622PROMSG    *LIBL     KEY0014  14
150300050622PROMSG    *LIBL     KEY0015  15
150400050622PROMSG    *LIBL     KEY0016  16
150500050622PROMSG    *LIBL     KEY0017  17
150600050622PROMSG    *LIBL     KEY0018  18
150700050622PROMSG    *LIBL     KEY0019  19
150800050622PROMSG    *LIBL     KEY0020  20
150900050622PROMSG    *LIBL     KEY0021  21
151000050622PROMSG    *LIBL     KEY0022  22
151100050622PROMSG    *LIBL     KEY0023  23
151200050622PROMSG    *LIBL     KEY0024  24
151300050622**  ABILITAZIONE D3
1514000506220N  01
1515000506220N  02
1516000506221N  03
1517000506220N  04
1518000506220N  05
1519000506220N  06
1520000506220N  07
1521000506220N  08
1522000506220N  09
1523000506220N  10
1524000506220N  11
1525000506221N  12
1526000506220N  13
1527000506220N  14
1528000506220N  15
1529000506220N  16
1530000506220N  17
1531000506220N  18
1532000506220N  19
1533000506220N  20
1534000506220N  21
1535000506220N  22
1536000506220N  23   Impostare '1' se si usano le opzioni
1537000506221N  24   Lasciare sempre impostata a '0' il valore della riga 24
