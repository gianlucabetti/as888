000100150209     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000200150209     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000300150209     H*PARMS CVTOPT(*NONE)
000400150209     H**********************************************************************
000500150209     H DECEDIT('0,') DATEDIT(*DMY.)
000600150209     H**********************************************************************
000700150209      *
000800150209      *         ------------------------------------------------
000900150209      *                Comunicazione Certificazione Unica
001000150209      *                   Manutenzione file telematico
001100150209      *                        Tipi Recod A B Z
001200150209      *         ------------------------------------------------
001300150209      *
001400150209     F**********************************************************************
001500150209     FSDGCUW00F UF   E             DISK    USROPN
001600150209     F                                     INFDS(RRNPFDS)                       *
001700150209     F*------------
001800150209     FSDGSK62D  CF   E             WORKSTN
001900150209     F                                     RENAME(SK62D1:FMTD1)
002000150209     F                                     RENAME(SK62DA1:FMTDA1)
002100150209     F                                     RENAME(SK62DA2:FMTDA2)
002200150209     F                                     RENAME(SK62DA3:FMTDA3)
002300150209     F                                     RENAME(SK62DB1:FMTDB1)
002400150209     F                                     RENAME(SK62DB2:FMTDB2)
002500150209     F                                     RENAME(SK62DB3:FMTDB3)
002600150209     F                                     RENAME(SK62DB4:FMTDB4)
002700150209     F                                     RENAME(SK62DZ1:FMTDZ1)
002800150209     F                                     RENAME(SK62DZ2:FMTDZ2)
002900150209     F                                     RENAME(SK62DZ3:FMTDZ3)
003000150209     F                                     RENAME(SK62T1:FMTT1)
003100150209     F                                     RENAME(SK62Z1:FMTZ1)
003200150209     F                                     RENAME(SK62Z2:FMTZ2)
003300150209     F                                     INFDS(DSFMT)
003400150209     D**********************************************************************
003500150209     D* numero tipo operaz
003600150209     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003700150209     D $ID             S             27    DIM(5) ALT($OZ)
003800150209     D*----------------------------------------------------*
003900150209     D*Schiere tasti funzione generiche
004000150209     D* percorso IdMsg
004100150209     D $FM             S             27    DIM(24)
004200150209     D*  testo msg
004300150209     D $FX             S             50    DIM(24)
004400150209     D* abilitazione ('0'/'1')
004500150209     D $FC             S              1    DIM(24)
004600150209     D*  tipo abilitazione ('S'/'N')
004700150209     D $FR             S              1    DIM(24)
004800150209     D*----------------------------------------------------*
004900150209     D* Tasti funzione D1
005000150209     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005100150209     D $FX1            S             50    DIM(24) ALT($FM1)
005200150209     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005300150209     D $FR1            S              1    DIM(24) ALT($FC1)
005400150209     D*----------------------------------------------------*
005500150209     D* Tasti funzione A1-2-3/B1-2-3-4/Z1-2-3
005600150209     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005700150209     D $FX2            S             50    DIM(24) ALT($FM2)
005800150209     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005900150209     D $FR2            S              1    DIM(24) ALT($FC2)
006000150209     D*----------------------------------------------------*
006100150209     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006200150209     D* funzione personalizzati
006300150209     D $PGF1           S             10    DIM(25)
006400150209     D*----------------------------------------------------*
006500150209     D* dimensioni variabili che conterranno i tasti D1
006600150209     D $D1L01          S              3  0
006700150209     D $D1L02          S              3  0
006800150209     D* dimensioni variabili che conterranno i tasti An/Bn/Zn
006900150209     D $DxL01          S              3  0
007000150209     D $DxL02          S              3  0
007100150209     D*----------------------------------------------------*
007200150209    >D* Inserire qui la definizione di altre variabili per reperire
007300150209    >D* la lunghezza di altre videate che usano un piede diverso da
007400150209    >D* STD
007500150209     D*----------------------------------------------------*
007600150209     D* Passaggio Parametri
007700150209     D KPJBA         E DS
007800150209     D*----------------------------------------------------*
007900150209     D* DS Esterna gestione messaggi
008000150209     D SDGMSGDS      E DS                  INZ
008100150209     D  STS          E                     EXTFLD(MSGSTS)
008200150209     D                                     DIM(333)
008300150209     D  JBA          E                     EXTFLD(MSGJBA)
008400150209     D                                     DIM(502)
008500150209     D*----------------------------------------------------*
009700150209     D*----------------------------------------------------*
009800150209     D* Reperimento nome PGM
009900150209     D STATUS         SDS           333
010000150209     D  DSPGM            *PROC
010100150209     D  PARMS            *PARMS
010200150209     D*----------------------------------------------------*
010300150209     D* Dati di ambiente ottenuti da XSOC
010301150209     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010500150209     D*-------------
010501150209     D* DS Interna per dati di output di SDGSOCDR
010700150209     D XSOCDS          DS          1000
010800150209     D*----------------------------------------------------*
010900150209     D* Passaggio Parametri
011000150209     D SDGSK62DS     E DS
011100150209     D*-------------
011200150209     D* Catalogo file per scarico su supporto magnetico
011300150209     D ANA122DS      E DS
011400150209     D*-------------
011500150209     D* Parametri modulo SCARSUMA
011600150209     D ANP022DS      E DS
011700150209     D*----------------------------------------------------*
011800150209     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011900150209     D* alla funzione personalizzata
012000150209     D $SaveDS         S                   like(SDGSK62DS)
012100150209     D*----------------------------------------------------*
012200150209     D DSFMT           DS
012300150209     D  $TASTO               369    369
012400150209     D  NRG                  370    370
012500150209     D  NCL                  371    371
012600150209     D*----------------------------------------------------*
012700150209     D* DS per UserExit
012800150209     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
012900150209     D*----------------------------------------------------*
013000150209     D* posizione cursore
013100150209     D CURSOR          DS
013200150209     D  RIRI                   1      2B 0 INZ
013300150209     D  R$$                    2      2
013400150209     D  COCO                   3      4B 0 INZ
013500150209     D  C$$                    4      4
013600150209     D*----------------------------------------------------*
013700150209     D* Costanti
013800150209     D*----------------------------------------------------*
013900150209     D* Tasti di funzione
014000150209     D F01             C                   CONST(X'31')
014100150209     D F02             C                   CONST(X'32')
014200150209     D F03             C                   CONST(X'33')
014300150209     D F04             C                   CONST(X'34')
014400150209     D F05             C                   CONST(X'35')
014500150209     D F06             C                   CONST(X'36')
014600150209     D F07             C                   CONST(X'37')
014700150209     D F08             C                   CONST(X'38')
014800150209     D F09             C                   CONST(X'39')
014900150209     D F10             C                   CONST(X'3A')
015000150209     D F11             C                   CONST(X'3B')
015100150209     D F12             C                   CONST(X'3C')
015200150209     D F13             C                   CONST(X'B1')
015300150209     D F14             C                   CONST(X'B2')
015400150209     D F15             C                   CONST(X'B3')
015500150209     D F16             C                   CONST(X'B4')
015600150209     D F17             C                   CONST(X'B5')
015700150209     D F18             C                   CONST(X'B6')
015800150209     D F19             C                   CONST(X'B7')
015900150209     D F20             C                   CONST(X'B8')
016000150209     D F21             C                   CONST(X'B9')
016100150209     D F22             C                   CONST(X'BA')
016200150209     D F23             C                   CONST(X'BB')
016300150209     D F24             C                   CONST(X'BC')
016400150209     D ENTER           C                   CONST(X'F1')
016500150209     D ROLDWN          C                   CONST(X'F4')
016600150209     D ROLLUP          C                   CONST(X'F5')
016700150209     D*----------------------------------------------------*
016800150209     D* Sottotitolo di testata
016900150209     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
017000150209     D                                          SDC0058')
017100150209     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
017200150209     D                                          SDC0057')
017300150209     D*----------------------------------------------------*
017400150209     D* Variabili per gestione videate
017500150209     D $Gest           S              2A
017600150209     D $Fine           S              1A
017700150209     D $InzDX          S              1A
017800150209     D $InzD1          S              1A
017900150209     D $LastG          S              2A
018000150209     D $LastV          S              2A
018100150209     D*----------------------------------------------------*
018200150209     D* Variabili appoggio sempre presenti in tutte le anagrafiche
018300150209     D CurR            S              2S 0
018400150209     D CurC            S              2S 0
018500150209     D Win             S             99A
018600150209     D*----------------------------------------------------*
018700150209     D* Variabili appoggio specifiche della singola anagrafica
018800150209     D*----------------------------------------------------*
018900150209     D*  Indici
019000150209     D X               S              3S 0
019100150209     D Y               S              3S 0
019200150209     D*----------------------------------------------------*
019300150209     D* Indice della schiera tasti funzione
019400150209     D PosFun          S              2  0
019500150209     D*----------------------------------------------------*
019600150209     D* OpzK62 usato in parzializzazione
019700150209     D* e WOpzK62 valorizzato dalla parzializzazione per manutenzione
019800150209     D WOpzK62         S                   like(OpzK62)
019900150209     D*----------------------------------------------------*
020000150209     D $RecOk          S              1a
020100150209     D $RecFound       S              1a
020200150209     D $RecAlloc       S              1a
020300150209     D*----------------------------------------------------*
020400150209     D* Nr rec SDGCUW00F
020500150209     D  RrnPfDs        DS           512                                         *
020600150209     D   RrnPf               397    400B 0                                      *
020700150209     D*------------------
020800150209     D* Tipo record "A"
020900150209     D*------------------
021000150209     D TpRecADS        DS
021100150209     D* prima mappa
021200150209     D  GA001010
021300150209     D  GA00101
021400150209     D  GA00201
021500150209     D  GA00301
021600150209     D  GA00401
021700150209     D  GA00501
021800150209     D  GA00502
021900150209     D  GA00503
022000150209     D  GA00504
022100150209     D  GA00505
022200150209     D  GA00506
022300150209     D  GA00507
022400150209     D  GA00601
022500150209     D  GA00701
022600150209     D  GA00801
022700150209     D  GA00802
022800150209     D*----------
022900150209     D* seconda mappa
023000150209     D  GA00901
023100150209     D  GA00902
023200150209     D  GA00903
023300150209     D  GA00904
023400150209     D  GA00905
023500150209     D  GA00906
023600150209     D  GA00907
023700150209     D  GA00908
023800150209     D  GA00909
023900150209     D  GA00910
024000150209     D  GA00911
024100150209     D  GA00912
024200150209     D  GA00913
024300150209     D  GA00914
024400150209     D  GA00915
024500150209     D*----------
024600150209     D* terza mappa
024700150209     D  GA00916
024800150209     D  GA01001
024900150209     D  GA01002
025000150209     D  GA01003
025100150209     D  GA01101
025200150209     D  GA01201
025300150209     D*------------------
025400150209     D* Tipo record "b"
025500150209     D*------------------
025600150209     D TpRecBDS        DS
025700150209     D* prima mappa
025800150209     D  GB001010
025900150209     D  GB00101
026000150209     D  GB00201
026100150209     D  GB00301
026200150209     D  GB00401
026300150209     D  GB00501
026400150209     D  GB00601
026700150209     D  GB00701
026701150209     D  GB00801
026800150209     D  GB00901
026900150209     D  GB01001
027000150209     D  GB01101
027100150209     D  GB01201
027200150209     D  GB01301
027300150209     D  GB01401
027400150209     D  GB01402
027500150209     D  GB01501
027600150209     D  GB01601
027700150209     D  GB01701
027701150209     D  GB01801
027800150209     D*----------
027900150209     D* seconda mappa
028100150209     D  GB01901
028200150209     D  GB02001
028300150209     D  GB02101
028400150209     D  GB02201
028500150209     D  GB02301
028600150209     D  GB02401
028700150209     D  GB02501
028800150209     D  GB02601
028900150209     D  GB02701
029000150209     D  GB02801
029100150209     D  GB02901
029200150209     D  GB03001
029300150209     D  GB03101
029400150209     D  GB03201
029500150209     D  GB03301
029600150209     D  GB03401
029700150209     D  GB03501
029800150209     D  GB03601
029900150209     D  GB03701
030000150209     D  GB03801
030200150209     D  GB03901
030300150209     D  GB03902
030400150209     D  GB03903
030401150209     D  GB03904
030402150209     D  GB03905
030500150209     D*----------
030600150209     D* terza mappa
030700150209     D  GB03906
030800150209     D  GB03907
030900150209     D  GB03908
031000150209     D  GB03909
031100150209     D  GB03910
031200150209     D  GB03911
031300150209     D  GB03912
031400150209     D  GB03913
031500150209     D  GB03914
031600150209     D  GB03915
031700150209     D  GB03916
031800150209     D  GB03917
031900150209     D  GB03918
032000150209     D  GB03919
032100150209     D*----------
032200150209     D* quarta mappa
032300150209     D  GB04001
032400150209     D  GB04101
032600150209     D  GB04201
032700150209     D  GB04301
032800150209     D  GB04401
032900150209     D  GB04501
033000150209     D  GB04601
033100150209     D  GB04701
033200150209     D  GB04801
033300150209     D  GB04901
033400150209     D  GB05001
033500150209     D  GB05101
033501150209     D  GB05201
033502150209     D*------------------
033503150209     D TpRecZDS        DS
033504150209     D* prima mappa
033505150209     D  GZ001010
033506150209     D  GZ00101
033507150209     D  GZ00102
033508150209     D  GZ00103
033509150209     D  GZ00104
033510150209     D  GZ00105
033511150209     D  GZ00106
033512150209     D  GZ00107
033513150209     D  GZ00108
033514150209     D  GZ00109
033515150209     D  GZ00110
033516150209     D  GZ00111
033517150209     D  GZ00112
033518150209     D  GZ00113
033519150209     D  GZ00114
033525150209     D*----------
033526150209     D* seconda mappa
033527150209     D  GZ00115
033528150209     D  GZ00116
033529150209     D  GZ00117
033530150209     D  GZ00118
033531150209     D  GZ00119
033532150209     D  GZ00120
033533150209     D  GZ00121
033534150209     D  GZ00122
033535150209     D  GZ00123
033536150209     D  GZ00124
033537150209     D  GZ00125
033538150209     D  GZ00126
033539150209     D  GZ00127
033540150209     D  GZ00128
033553150209     D*----------
033554150209     D* terza mappa
033555150209     D  GZ00129
033556150209     D  GZ00130
033557150209     D  GZ00131
033558150209     D  GZ00132
033559150209     D  GZ00133
033560150209     D  GZ00134
033561150209     D  GZ00135
033562150209     D  GZ00201
038300150209     D*----------------------------------------------------------------
038400150209     D* Ds contenente i valori del msg PRO1797
038500150209     D §Fil_Lib        DS
038600150209     D  §File                        10A
038700150209     D  §Libreria                    10A
038800150209     C******************************************************
038900150209     C*                MAIN LINE PROGRAM                   *
039000150209     C******************************************************
039100150209     C*
039200150209     C* inizializzazione variabili
039300150209     C                   EXSR      INZVAR
039400150209     C*
039500150209     C     $FINE         DOWEQ     *OFF
039600150209     C     $GEST         CASEQ     'D1'          GESD1
039700150209     C     $GEST         CASEQ     'DX'          GESDX
039800150209     C     $GEST         CASEQ     'A1'          GESDA1
039900150209     C     $GEST         CASEQ     'A2'          GESDA2
040000150209     C     $GEST         CASEQ     'A3'          GESDA3
040100150209     C     $GEST         CASEQ     'B1'          GESDB1
040200150209     C     $GEST         CASEQ     'B2'          GESDB2
040300150209     C     $GEST         CASEQ     'B3'          GESDB3
040400150209     C     $GEST         CASEQ     'B4'          GESDB4
040500150209     C     $GEST         CASEQ     'Z1'          GESDZ1
040600150209     C     $GEST         CASEQ     'Z2'          GESDZ2
040700150209     C     $GEST         CASEQ     'Z3'          GESDZ3
040800150209     C                   ENDCS
040900150209     C                   ENDDO
041000150209     C*
041100150209     C* fine programma
041200150209     C                   EXSR      ENDPGM
041300150209     C************************************************************
041400150209     C* Fine Programma
041500150209     C************************************************************
041600150209     C     ENDPGM        BEGSR
041700150209     C*
041800150209     C* disallocazione rcd
041900150209     C                   UNLOCK    SDGCUW00F                            22
042000150209     C*
042100150209     C* passaggio dati al pgm chiamante
042200150209     C                   MoveL     SDGSK62DS     KPJBU
042300150209     C*
042400150209     C* Se pgm Guida non è stato richiamato posso chiudere in LR
042500150209     C                   if        OpzK62 = *blank
042600150209     C                   eval      *inLR = *on
042700150209     C* altrimenti chiudo in RT
042800150209     C                   else
042900150209     C                   eval      *inRT = *on
043000150209     C                   endif
043100150209     C*
043200150209     C                   RETURN
043300150209     C*
043400150209     C*
043500150209     C     XEndPgm       ENDSR
043600150209     C************************************************************
043700150209     C* GESTIONE EMISSIONE TASTI FUNZIONE
043800150209     C************************************************************
043900150209     C     GESPIE        BEGSR
044000150209     C*
044100150209     C                   SELECT
044200150209     C*
044300150209     C     $GEST         WHENEQ    'D1'
044400150209     C     $ULKD1        IFEQ      0
044500150209     C                   EXSR      F24D1
044600150209     C                   ELSE
044700150209     C                   WRITE     FMTZ1
044800150209     C                   ENDIF
044900150209     C*
045000150209     C*
045100150209     C     $GEST         WHENEQ    'A1'
045200150209     C     $GEST         oreq      'A2'
045300150209     C     $GEST         oreq      'A3'
045400150209     C     $GEST         oreq      'B1'
045500150209     C     $GEST         oreq      'B2'
045600150209     C     $GEST         oreq      'B3'
045700150209     C     $GEST         oreq      'E1'
045800150209     C     $GEST         oreq      'Z1'
045900150209     C     $GEST         oreq      'Z2'
046000150209     C     $GEST         oreq      'Z3'
046100150209     C     $ULKDx        IFEQ      0
046200150209     C                   EXSR      F24Dx
046300150209     C                   ELSE
046400150209     C                   WRITE     FMTZ2
046500150209     C                   ENDIF
046600150209     C*
046700150209     C                   ENDSL
046800150209     C*
046900150209     C     XGesPie       ENDSR
047000150209     C************************************************************
047100150209     C* GESTIONE PARZIALIZZAZIONI
047200150209     C************************************************************
047300150209     C     GESD1         BEGSR
047400150209     C* inizializzazione videata
047500150209     C     $INZD1        IFEQ      *ON
047600150209     C                   EXSR      INZD1
047700150209     C                   Move      *OFF          $INZD1
047800150209     C                   ENDIF
047900150209     C* emissione piede videata se proveniente da altra
048000150209     C* salvataggio valore formato di provenienza
048100150209     C     $LASTV        IFNE      'D1'
048200150209     C                   WRITE     FMTT1
048300150209     C                   EXSR      GESPIE
048400150209     C                   Move      $LASTV        $LASTG
048500150209     C                   Move      'D1'          $LASTV
048600150209     C                   ENDIF
048700150209     C*
048800150209     C* se esistono errori sulla videata
048900150209     C* emetto la write del formato a indicatori spenti per vedere
049000150209     C* le eventuali decodifiche
049100150209     C     *IN99         IFEQ      *ON
049200150209     C                   MoveA     *IN           WIN
049300150209     C                   Move      *ALL'0'       IN5098           49
049400150209     C                   MoveA     IN5098        *IN(50)
049500150209     C                   WRITE     FMTD1
049600150209     C                   MoveA     WIN           *IN
049700150209     C                   ENDIF
049800150209     C*
049900150209     C                   EXFMT     FMTD1
050000150209     C*
050100150209     C* azzero la posizione del cursore
050200150209     C                   Z-ADD     0             H1RIGA
050300150209     C                   Z-ADD     0             H1COLO
050400150209     C*
050500150209     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
050600150209     C* INPUT
050700150209     C* $FC1 = Abilitazioni videata D1
050800150209     C* OUTPUT
050900150209     C* *IN97 + *IN99 Errori
051000150209     C                   MoveA     $FC1          $FC
051100150209     C                   EXSR      CTRKD1
051200150209     C*
051300150209     C     *IN99         IFEQ      *OFF
051400150209     C*
051500150209     C                   SELECT
051600150209     C* F2=Decodifica
051700150209     C     $TASTO        WHENEQ    F02
051800150209     C                   EXSR      F02D1
051900150209     C* F3=Fine
052000150209     C     $TASTO        WHENEQ    F03
052100150209     C                   EXSR      F03D1
052200150209     C* F4=Ricerca
052300150209     C     $TASTO        WHENEQ    F04
052400150209     C                   EXSR      SEARCH
052500150209     C* F5=Refresh
052600150209     C     $TASTO        WHENEQ    F05
052700150209     C                   EXSR      F05D1
052800150209     C* F12=Ritorno
052900150209     C     $TASTO        WHENEQ    F12
053000150209     C                   EXSR      F12D1
053100150209     C* F24=Altri tasti
053200150209     C     $TASTO        WHENEQ    F24
053300150209     C                   EXSR      F24D1
053400150209     C*
053500150209     C                   OTHER
053600150209     C                   EXSR      CTRD1
053700150209     C*
053800150209     C* richiamo dettaglio se non ci sono errori in parzializzazione
053900150209     C                   if        *in99 = *off
054000150209     C                   exsr      SetAna
054100150209     C                   exsr      GesDx
054200150209     C                   endif
054300150209     C*
054400150209     C                   ENDSL
054500150209     C*
054600150209     C                   ENDIF
054700150209     C*
054800150209     C     XGesD1        ENDSR
054900150209     C************************************************************
055000150209     C* RICERCHE
055100150209     C************************************************************
055200150209     C     SEARCH        BEGSR
055300150209     C*
055400150209     C* determino Riga/Colonna del cursore
055500150209     C                   Move      NRG           R$$
055600150209     C                   Move      NCL           C$$
055700150209     C                   Z-ADD     RIRI          CURR
055800150209     C                   Z-ADD     COCO          CURC
055900150209     C*
056000150209     C*
056100150209     C                   SELECT
056200150209     C*
056300150209    >C*--------------------------------------------------------------
056400150209    >C* inserire le routine di ricerca
056500150209    >C* RICORDATE: per tutti i campi che in certe condizioni risultano PR,
056600150209    >C* NON deve essere possibile richiamare i rispettivi pgm in ricerca,
056700150209    >C* SEMMAI richiamarli in visualizzazione (opzione=05)
056800150209    >C*--------------------------------------------------------------
056900150209     C*
057000150209     C* FMTD1  - file su supporto magnetico
057100150209     C     H1NMFL        WHENEQ    'D1FILE'
057200150209     C                   Reset                   ANA122DS
057300150209     C                   Move      '01'          Opz122
057400150209     C                   Move      XScSoc        Societa122
057500150209     C                   MoveL     XScSoc        Soc122
057600150209     C                   Move      'CU'          TpScar122
057700150209     C                   Clear                   KPJBU
057800150209     C                   Movel     ANA122DS      KPJBU
057900150209     C                   Call      'ANA122R'
058000150209     C                   Parm                    KPJBA
058100150209     C                   Movel     KPJBU         ANA122DS
058200150209     C*
058300150209     C                   if        File122  <>   *Blanks
058400150209     C                   MoveL     File122       D1File
058500150209     C                   endif
058600150209     C*
058700150209     C* ricerca non abilitata in questa posizione
058800150209   X1C                   OTHER
058900150209     C                   SETON                                        9899
059000150209     C*
059100150209   E1C                   ENDSL
059200150209     C*
059300150209     C* imposto pos. del cursore
059400150209     C                   Z-ADD     CURR          H1RIGA
059500150209     C                   Z-ADD     CURC          H1COLO
059600150209     C*
059700150209     C                   Move      *HIVAL        $LASTV
059800150209     C*
059900150209     C                   ENDSR
060000150209     C/EJECT
060100150209     C************************************************************
060200150209     C* Carico e passo alle videate da manutenere
060300150209     C************************************************************
060400150209     C     GesDx         BEGSR
060500150209     C*
060600150209     C                   Move      '02'          WOpzK62
060700150209     C*
060800150209     C                   Move      *on           $InzDx
060900150209     C                   exsr      InzDx
061000150209     C                   Move      *off          $InzDx
061100150209     C*
061200150209     C*  se reperito rec richiesto
061300150209     C                   if        $RecOk      = *On
061400150209     C*
061500150209     C*  passo a dettaglio richiesto
061600150209     C                   select
061700150209     C                   when      D1TpRec='A'
061800150209     C                   Move      'A1'          $Gest
061900150209     C                   when      D1TpRec='B'
062000150209     C                   Move      'B1'          $Gest
062100150209     C                   when      D1TpRec='Z'
062200150209     C                   Move      'Z1'          $Gest
062300150209     C                   endsl
062400150209     C*
062500150209     C                   else
062600150209     C                   Move      'D1'          $Gest
062700150209     C*
062800150209     C                   endif
062900150209     C*
063000150209     C     XGesDx        ENDSR
063100150209     C************************************************************
063200150209     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
063300150209     C************************************************************
063400150209     C     INZD1         BEGSR
063500150209     C*
063600150209     C                   CLEAR                   FMTD1
063700150209     C*
063800150209     C* Se pgm  richiamato con parametri forniti , li imposto nelle
063900150209     C* parzializzazioni
064000150209     C                   select
064100150209     C* il pgm è richiamato e
064200150209     C* sono stati passati dei valori di parzializzazione ma non
064300150209     C* la chiave univoca
064400150209     C                   when      OpzK62 <> *blank
064500150209     C* imposto i valori passati sui campi a video
064600150209     C                   Move      TpReCK62      D1TpRec
064700150209     C* il pgm non è richiamato
064800150209     C                   when      OpzK62 =  *blank
064900150209     C* imposto dei valori di default
065000150209     C                   Move      *blank        D1TpRec
065100150209     C                   endsl
065200150209     C*
065300150209     C     XInzD1        ENDSR
065400150209     C************************************************************
065500150209     C* GESTIONE F02 VIDEO D1
065600150209     C************************************************************
065700150209     C     F02D1         BEGSR
065800150209     C*
065900150209     C* per decodificare la videata richiamo la routine dei ctrl
066000150209     C* e poi disattivo gli eventuali messaggi di errore
066100150209     C                   EXSR      CTRD1
066200150209     C*
066300150209     C                   Move      *ALL'0'       IN5098
066400150209     C                   MoveA     IN5098        *IN(50)
066500150209     C*
066600150209     C     XF02D1        ENDSR
066700150209     C************************************************************
066800150209     C* GESTIONE F03 VIDEO D1
066900150209     C************************************************************
067000150209     C     F03D1         BEGSR
067100150209     C*
067200150209     C                   Move      *ON           $FINE
067300150209     C                   Move      '1'           RETK62
067400150209     C* fine programma
067500150209     C                   EXSR      ENDPGM
067600150209     C*
067700150209     C     XF03D1        ENDSR
067800150209     C************************************************************
067900150209     C* GESTIONE F05 VIDEO D1
068000150209     C************************************************************
068100150209     C     F05D1         BEGSR
068200150209     C*
068300150209     C                   Move      *ON           $INZD1
068400150209     C*
068500150209     C     XF05D1        ENDSR
068600150209     C************************************************************
068700150209     C* GESTIONE F12 VIDEO D1
068800150209     C************************************************************
068900150209     C     F12D1         BEGSR
069000150209     C*
069100150209     C*  torno a menu
069200150209     C                   Move      *ON           $FINE
069300150209     C                   Move      '2'           RETK62
069400150209     C*
069500150209     C     XF12D1        ENDSR
069600150209     C************************************************************
069700150209     C* GESTIONE F24 VIDEO D1
069800150209     C* input
069900150209     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
070000150209     C* $FX1 =Testi tasti funzione
070100150209     C* $FC1 = Abilitazioni
070200150209     C* $FR1 = Inversioni di fondo
070300150209     C************************************************************
070400150209     C     F24D1         BEGSR
070500150209     C*
070600150209     C                   MoveA     $FX1          $FX
070700150209     C                   MoveA     $FC1          $FC
070800150209     C                   MoveA     $FR1          $FR
070900150209     C                   Z-ADD     $ULKD1        $ULK
071000150209     C                   Z-ADD     $D1L01        $L01
071100150209     C                   Z-ADD     $D1L02        $L02
071200150209     C                   EXSR      RIEKEY
071300150209     C                   Z-ADD     $ULK          $ULKD1            3 0
071400150209     C                   MoveA     $FC           $FC1
071500150209     C                   MoveL     $KEY1         Z1KE1
071600150209     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
071700150209     C                   MoveL     $KEY2         Z1KE2
071800150209     C                   WRITE     FMTZ1
071900150209     C*
072000150209     C     XF24D1        ENDSR
072100150209     C************************************************************
072200150209     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
072300150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
072400150209     C* 93 94 96 97 98
072500150209     C************************************************************
072600150209     C     CTRD1         BEGSR
072700150209     C*
072800150209     C                   SETOFF                                       99
072900150209     C                   if        D1Tprec = *blank
073000150209     C                   eval      *in99 = *on
073100150209     C                   endif
073200150209     C*
073300150209     C* file da trasmettere
073400150209     C                   EXSR      CtrFile
073500150209     C*
073600150209     C     XCtrD1        ENDSR
073700150209     C***********************************************************
073800150209     C* Controllo file da trasmettere
073900150209     C***********************************************************
074000150209     C     CtrFile       BEGSR
074100150209     C*
074200150209     C                   if        D1File     = *Blank
074300150209     C                   SetOn                                        5199
074400150209     C                   else
074500150209     C* verifico se esiste, nella libreria
074600150209     C* dei file per lo scarico su supporto magnetico,
074700150209     C* il file indicato a video
074800150209     C                   EXSR      ChkFile
074900150209     C*
075000150209     C                   endif
075100150209     C*
075200150209     C                   ENDSR
075300150209     C/EJECT
075400150209     C************************************************************
075500150209     C* Verifico l'esistenza del file
075600150209     C************************************************************
075700150209     C     ChkFile       BEGSR
075800150209     C*
075900150209     C                   CLOSE     SDGCUW00F
076000150209     C*
076100150209     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
076200150209     C                   Eval      Cmd         =
076300150209     C                             'CHKOBJ     OBJ('        +
076400150209     C                             %Trim(Lib022) + '/' +
076500150209     C                             %Trim(D1File) +
076600150209     C                             ') OBJTYPE(*FILE)'
076700150209     C*
076800150209     C                   Call      'QCMDEXC'                            21
076900150209     C                   Parm                    Cmd             120
077000150209     C                   Parm      120           Len              15 5
077100150209     C*
077200150209     C                   if        *In21       = *On
077300150209     C*
077400150209     C* Errore: dico che non il file non è stato trovato nella libreria
077500150209     C* Valorizzo le variabili del msg PRO1797
077600150209     C                   Move      D1File        §File
077700150209     C                   Move      Lib022        §Libreria
077800150209     C                   Move      §Fil_Lib      P1Fil_Lib
077900150209     C                   SetOn                                        5099
078000150209     C*
078100150209     C                   else
078200150209     C*
078300150209     C* sovrascrivo il membro con il nome del file
078400150209     C                   EXSR      OvrFile
078500150209     C                   OPEN      SDGCUW00F
078600150209     C                   endif
078700150209     C*
078800150209     C*
078900150209     C                   ENDSR
079000150209     C/Eject
079100150209     C************************************************************
079200150209     C* Sovrascrivo il membro con il nome del file
079300150209     C************************************************************
079400150209     C     OvrFile       BEGSR
079500150209     C*
079600150209     C                   Eval      Cmd         =
079700150209     C                             'OVRDBF     FILE(SDGCUW00F) '+
079800150209     C                             'TOFILE('                    +
079900150209     C                             %Trim(Lib022) + '/' +
080000150209     C                             %Trim(D1File) +
080100150209     C                             ')'
080200150209     C*
080300150209     C                   Call      'QCMDEXC'                            21
080400150209     C                   Parm                    Cmd             120
080500150209     C                   Parm      120           Len              15 5
080600150209     C*
080700150209     C                   if        *In21       = *On
080800150209     C                   Move      '1'           ERRK62
080900150209     C                   Move      *On           $Fine
081000150209     C                   EXSR      ENDPGM
081100150209     C                   endif
081200150209     C*
081300150209     C                   ENDSR
081400150209     C/Eject
081500150209     C************************************************************
081600150209     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
081700150209     C************************************************************
081800150209     C     CTRKD1        BEGSR
081900150209     C*
082000150209     C                   SETOFF                                       99
082100150209     C                   MoveA     $FC1          $FC
082200150209     C                   EXSR      CTRKEY
082300150209     C     *IN99         IFEQ      *ON
082400150209     C                   SETON                                        97
082500150209     C                   ENDIF
082600150209     C*
082700150209     C     XCtrKD1       ENDSR
082800150209     C************************************************************
082900150209     C* Chiamata programma con tasto personalizzato
083000150209     C************************************************************
083100150209     C     FPerDx        BEGSR
083200150209     C*
083300150209     C* salvo la DS di ingresso
083400150209     C                   MoveL     SDGSK62DS     $SaveDS
083500150209     C*
083600150209     C                   Move      xscsoc        SocietaK62
083700150209     C* passare nei campi appositi della DS STDK62DS il valore
083800150209     C* della chiave della VL univoca
083900150209     C                   MoveL     D1TpRec       TpReCK62
084000150209     C*
084100150209     C                   MoveL     SDGSK62DS     KPJBU
084200150209     C                   CALL      $PGF1(PosFun)                        22
084300150209     C                   PARM                    KPJBA
084400150209     C***                PARM                    NOMPGM
084500150209     C*
084600150209     C* ripristino la DS di ingresso
084700150209     C                   MoveL     $SaveDS       SDGSK62DS
084800150209     C*
084900150209     C     XFPerDx       ENDSR
085000150209     C************************************************************
085100150209     C* CONTROLLO ABILITAZIONE TASTI VIDEATE
085200150209     C************************************************************
085300150209     C     CTRKDx        BEGSR
085400150209     C*
085500150209     C                   SETOFF                                       99
085600150209     C                   MoveA     $FC2          $FC
085700150209     C                   EXSR      CTRKEY
085800150209     C     *IN99         IFEQ      *ON
085900150209     C                   SETON                                        97
086000150209     C                   ENDIF
086100150209     C*
086200150209     C     XCtrKDx       ENDSR
086300150209     C************************************************************
086400150209     C* INIZIALIZZAZIONE VIDEATA DATI
086500150209     C************************************************************
086600150209     C     INZDx         BEGSR
086700150209     C*
086800150209     C* In base al tipo record scelto, utilizzo diverse videate:
086900150209     C                   select
087000150209     C                   when      D1TpRec='A'
087100150209     C                   clear                   FmtDA1
087200150209     C                   clear                   FmtDA2
087300150209     C                   clear                   FmtDA3
087400150209     C                   clear                   FmtDZ1
087500150209     C                   clear                   FmtDZ1
087600150209     C                   endsl
087700150209     C*
087800150209     C* NON IMMISSIONE
087900150209     C                   if        WOpzK62 <> '01'
088000150209     C*
088100150209     C*  LETTURA
088200150209     C                   exsr      RedAna
088300150209     C*
088400150209     C*  DOPO LETTURA
088500150209     C                   select
088600150209     C*   NON TROVATO tipo rec richiesto oppure allocato
088700150209     C                   when      $RecOk = *off
088800150209     C                   if        $RecFound   = *Off    or
088900150209     C                             $RecAlloc   = *On
089000150209     C                   Move      'D1'          $GEST
089100150209     C                   Move      '1'           ERRK62
089200150209     C*    errore da emettere in parzializzaione
089300150209     C*    PRO0007-(F1) La funzione richiamata si è chiusa in errore
089400150209     C                   eval      *in93 = *on
089500150209     C                   eval      *in99 = *on
089600150209     C                   endif
089700150209     C*
089800150209     C*   TROVATO tp rec da gestire
089900150209     C                   when      $RecOk = *on
090000150209     C*
090100150209     C*    riempio videata tp rec richiesto
090200150209     C*    e richiamo ctrl per decodifica
090300150209     C                   select
090400150209     C                   when      D1TpRec='A'
090500150209     C                   eval      TPRECADS = CUWREC
090600150209     C                   exsr      CtrDA1
090700150209     C                   exsr      CtrDA2
090800150209     C                   exsr      CtrDA3
090900150209     C                   when      D1TpRec='B'
091000150209     C                   eval      TPRECBDS = CUWREC
091100150209     C                   exsr      CtrDB1
091200150209     C                   exsr      CtrDB2
091300150209     C                   exsr      CtrDB3
091400150209     C                   exsr      CtrDB4
091500150209     C                   when      D1TpRec='Z'
091600150209     C                   eval      TPRECZDS = CUWREC
091700150209     C                   exsr      CtrDZ1
091800150209     C                   endsl
091900150209     C                   Move      *all'0'       In5098
092000150209     C                   Movea     In5098        *in(50)
092100150209     C*
092200150209     C                   endsl
092300150209     C*
092400150209     C                   endif
092500150209     C*
092600150209     C     XInzDX        ENDSR
092700150209     C************************************************************
092800150209     C* posizionamento sul file
092900150209     C************************************************************
093000150209     C     SetAna        BEGSR
093100150209     C*
093200150209     C                   eval      $RecFound   = *Off
093300150209     C*
093400150209     C     1             setll     SDGCUW00F
093500150209     C*
093600150209     C                   ENDSR
093700150209     C************************************************************
093800150209     C* Gegstione elttura rec da caricare per modifica
093900150209     C************************************************************
094000150209     C     RedAna        BEGSR
094100150209     C*
094200150209     C* REPERISCO NRR DA LEGGERE
094300150209     C*  cerco il n°relativo rec contenente il tipo rec richiesto
094400150209     C                   eval      $RecOk = *off
094500150209     C                   eval      $RecAlloc   = *Off
094600150209     C*
094700150209     C                   read(n)   SDGCUW00F                              21
094800150209     C                   dow       *in21 = *off
094900150209     C*
095000150209     C                   if        %subst(CUWREC: 1 : 1)=D1TpRec
095100150209     C                   eval      $RecOk = *on
095200150209     C                   eval      $RecFound   = *On
095300150209     C                   leave
095400150209     C                   endif
095500150209     C*
095600150209     C                   read(n)   SDGCUW00F                              21
095700150209     C                   enddo
095800150209     C*
095900150209     C*
096000150209     C* LETTURA
096100150209     C*  trovato tipo rec richiesto
096200150209     C                   if        $RecOk = *on
096300150209     C*
096400150209     C*  provo a leggere il rec fino a quando il rec non è allocato
096500150209     C*  oppure l'utente vuole Ritentare a leggere il rec allocato
096600150209     C                   dou       *in22 = *off
096700150209     C                             or MsgRtn <> 'R'
096800150209     C*  alloco se Modifica/Annullamento
096900150209    >C                   if        WOpzK62='02' or WOpzK62='04'
097000150209     C     RrnPf         chain     SDGCUW00F                          2122
097100150209     C*  NON alloco se Copia/Visualizzazione
097200150209     C                   else
097300150209     C     RrnPf         chain(n)  SDGCUW00F                          2122
097400150209     C                   endif
097500150209     C*
097600150209     C*  Gestione RCD allocato
097700150209     C                   if        *in22 = *on
097800150209     C                   eval      $RecAlloc   = *On
097900150209    >C                   Movel     'SDGCUW00F'   MsgFil
098000150209     C                   exsr      MsgPgm
098100150209     C                   endif
098200150209     C*
098300150209     C                   enddo
098400150209     C*
098500150209     C* rcd allocato da altro pgm: memorizzo errore
098600150209     C                   if        *in22 = *on
098700150209     C                   eval      $RecOk = *off
098800150209     C                   endif
098900150209     C*
099000150209     C                   endif
099100150209     C*
099200150209     C     XRedAna       ENDSR
099300150209     C************************************************************
099400150209     C* GESTIONE F24 VIDEI
099500150209     C************************************************************
099600150209     C     F24Dx         BEGSR
099700150209     C*
099800150209     C                   MoveA     $FX2          $FX
099900150209     C                   MoveA     $FC2          $FC
100000150209     C                   MoveA     $FR2          $FR
100100150209     C                   Z-ADD     $ULKDx        $ULK
100200150209     C                   Z-ADD     $DxL01        $L01
100300150209     C                   Z-ADD     $DxL02        $L02
100400150209     C                   EXSR      RIEKEY
100500150209     C                   Z-ADD     $ULK          $ULKDx            3 0
100600150209     C                   MoveA     $FC           $FC2
100700150209     C                   MoveL     $KEY1         Z2KE1
100800150209     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
100900150209     C                   MoveL     $KEY2         Z2KE2
101000150209     C                   WRITE     FMTZ2
101100150209     C*
101200150209     C     XF24Dx        ENDSR
101300150209     C************************************************************
101400150209     C* GESTIONE VIDEO RECORD D01
101500150209     C************************************************************
101600150209     C     GESDA1        BEGSR
101700150209     C* inizializzazione videata
101800150209     C     $INZDX        IFEQ      *ON
101900150209     C                   EXSR      INZDX
102000150209     C                   Move      *OFF          $INZDX
102100150209     C                   ENDIF
102200150209     C* emissione piede videata se proveniente da altra
102300150209     C* salvataggio valore formato di provenienza
102400150209     C     $LASTV        IFNE      '01'
102500150209     C                   WRITE     FMTT1
102600150209     C                   EXSR      GESPIE
102700150209     C                   Move      $LASTV        $LASTG
102800150209     C                   Move      '01'          $LASTV
102900150209     C                   ENDIF
103000150209     C*
103100150209     C* se esistono errori sulla videata
103200150209     C* emetto la write del formato a indicatori spenti per vedere
103300150209     C* le eventuali decodifiche
103400150209     C     *IN99         IFEQ      *ON
103500150209     C     $GEST         ANDEQ     'A1'
103600150209     C                   MoveA     *IN           WIN
103700150209     C                   Move      *ALL'0'       IN5098           49
103800150209     C                   MoveA     IN5098        *IN(50)
103900150209     C                   WRITE     FMTDA1
104000150209     C                   MoveA     WIN           *IN
104100150209     C                   ENDIF
104200150209     C* Inizializzazione indice tasti funzione personalizzati
104300150209     C                   Eval      PosFun = 25
104400150209     C*
104500150209     C                   SELECT
104600150209     C* Annullamento
104700150209     C     WOpzK62       WHENEQ    '04'
104800150209     C* Visualizzazione
104900150209     C     WOpzK62       OREQ      '05'
105000150209     C                   WRITE     FMTDA1
105100150209     C                   EXFMT     PROTECT
105200150209     C* Immissione/Modifica/Copia
105300150209     C                   OTHER
105400150209     C                   EXFMT     FMTDA1
105500150209     C                   ENDSL
105600150209     C*
105700150209     C* azzero la posizione del cursore
105800150209     C                   Z-ADD     0             H1RIGA
105900150209     C                   Z-ADD     0             H1COLO
106000150209     C*
106100150209     C* controllo abilitazione tasti funzione
106200150209     C                   EXSR      CTRKDx
106300150209     C*
106400150209     C     *IN99         IFEQ      *OFF
106500150209     C*
106600150209     C                   SELECT
106700150209     C* ROLLUP
106800150209     C     $TASTO        WHENEQ    ROLLUP
106900150209     C                   Move      'A2'          $GEST
107000150209     C* F3=Fine
107100150209     C     $TASTO        WHENEQ    F03
107200150209     C                   EXSR      F03D01
107300150209     C* F5=Refresh
107400150209     C     $TASTO        WHENEQ    F05
107500150209     C                   EXSR      F05D01
107600150209     C* F8=Successivo
107700150209     C     $TASTO        WHENEQ    F08
107800150209     C                   EXSR      F08D01
107900150209     C* F12=Ritorno
108000150209     C     $TASTO        WHENEQ    F12
108100150209     C                   EXSR      F12D01
108200150209     C* F24=Altri tasti
108300150209     C     $TASTO        WHENEQ    F24
108400150209     C                   EXSR      F24Dx
108500150209     C* Personalizzata
108600150209     C     $PGF1(PosFun) WHENNE    *BLANK
108700150209     C                   EXSR      FPerDx
108800150209     C*
108900150209     C                   OTHER
109000150209     C                   EXSR      CtrDA1
109100150209     C     *IN99         IFEQ      *OFF
109200150209     C                   EXSR      CtrDA2
109300150209     C                   EXSR      CtrDA3
109400150209     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
109500150209     C* indicatori uguali in videate differenti, genererei conflitti
109600150209     C     *IN99         IFEQ      *ON
109700150209     C                   Move      *ALL'0'       IN5098           49
109800150209     C                   MoveA     IN5098        *IN(50)
109900150209     C                   SETON                                        9996
110000150209     C                   ENDIF
110100150209     C                   ENDIF
110200150209     C*
110300150209     C* eseguo aggiornamento
110400150209     C     $TASTO        IFEQ      F06
110500150209     C     *IN99         ANDEQ     *OFF
110600150209     C                   EXSR      AGGANA
110700150209     C                   ENDIF
110800150209     C*
110900150209     C                   ENDSL
111000150209     C*
111100150209     C                   ENDIF
111200150209     C*
111300150209     C     XGESDA1       ENDSR
111400150209     C************************************************************
111500150209     C* GESTIONE F03 VIDEO D01
111600150209     C************************************************************
111700150209     C     F03D01        BEGSR
111800150209     C*
111900150209     C                   Move      *ON           $FINE
112000150209     C                   Move      '1'           RETK62
112100150209     C* fine programma
112200150209     C                   EXSR      ENDPGM
112300150209     C*
112400150209     C     XF03D01       ENDSR
112500150209     C************************************************************
112600150209     C* GESTIONE F05 VIDEO D01
112700150209     C************************************************************
112800150209     C     F05D01        BEGSR
112900150209     C*
113000150209     C                   Move      *ON           $INZDX
113100150209     C*
113200150209     C     XF05D01       ENDSR
113300150209     C************************************************************
113400150209     C* GESTIONE F08 VIDEO D01
113500150209     C************************************************************
113600150209     C     F08D01        BEGSR
113700150209     C*
113800150209     C                   Move      'DX'          $GEST
113900150209    >C                   MOVE      '0'           RETK62
114000150209     C*
114100150209     C                   ENDSR
114200150209     C/EJECT
114300150209     C************************************************************
114400150209     C* GESTIONE F12 VIDEO D01
114500150209     C************************************************************
114600150209     C     F12D01        BEGSR
114700150209     C*
114800150209     C                   Move      'D1'          $GEST
114900150209     C*
115000150209     C     XF12D01       ENDSR
115100150209     C************************************************************
115200150209     C* CONTROLLO VIDEATA  DA1
115300150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
115400150209     C* 95 96 97 98
115500150209     C************************************************************
115600150209     C     CtrDA1        BEGSR
115700150209     C*
115800150209     C                   SETOFF                                       99
115900150209     C*
116000150209     C     XCtrDA1       ENDSR
116100150209     C************************************************************
116200150209     C* GESTIONE VIDEO RECORD  DA2
116300150209     C************************************************************
116400150209     C     GESDA2        BEGSR
116500150209     C* emissione piede videata se proveniente da altra
116600150209     C* salvataggio valore formato di provenienza
116700150209     C     $LASTV        IFNE      '02'
116800150209     C                   WRITE     FMTT1
116900150209     C                   EXSR      GESPIE
117000150209     C                   Move      $LASTV        $LASTG
117100150209     C                   Move      '02'          $LASTV
117200150209     C                   ENDIF
117300150209     C*
117400150209     C* se esistono errori sulla videata
117500150209     C* emetto la write del formato a indicatori spenti per vedere
117600150209     C* le eventuali decodifiche
117700150209     C     *IN99         IFEQ      *ON
117800150209     C     $GEST         ANDEQ     'A2'
117900150209     C                   MoveA     *IN           WIN
118000150209     C                   Move      *ALL'0'       IN5098           49
118100150209     C                   MoveA     IN5098        *IN(50)
118200150209     C                   WRITE     FMTDA2
118300150209     C                   MoveA     WIN           *IN
118400150209     C                   ENDIF
118500150209     C* Inizializzazione indice tasti funzione personalizzati
118600150209     C                   Eval      PosFun = 25
118700150209     C*
118800150209     C                   SELECT
118900150209     C* Annullamento
119000150209     C     WOpzK62       WHENEQ    '04'
119100150209     C* Visualizzazione
119200150209     C     WOpzK62       OREQ      '05'
119300150209     C                   WRITE     FMTDA2
119400150209     C                   EXFMT     PROTECT
119500150209     C* Immissione/Modifica/Copia
119600150209     C                   OTHER
119700150209     C                   EXFMT     FMTDA2
119800150209     C                   ENDSL
119900150209     C*
120000150209     C* azzero la posizione del cursore
120100150209     C                   Z-ADD     0             H1RIGA
120200150209     C                   Z-ADD     0             H1COLO
120300150209     C*
120400150209     C* controllo abilitazione tasti funzione
120500150209     C                   EXSR      CTRKDx
120600150209     C*
120700150209     C     *IN99         IFEQ      *OFF
120800150209     C                   SELECT
120900150209     C* ROLLUP
121000150209     C     $TASTO        WHENEQ    ROLLUP
121100150209     C                   Move      'A3'          $GEST
121200150209     C* ROLDOWN
121300150209     C     $TASTO        WHENEQ    ROLDWN
121400150209     C                   Move      'A1'          $GEST
121500150209     C* F3=Fine
121600150209     C     $TASTO        WHENEQ    F03
121700150209     C                   EXSR      F03D01
121800150209     C* F5=Refresh
121900150209     C     $TASTO        WHENEQ    F05
122000150209     C                   EXSR      F05D01
122100150209     C* F8=Successivo
122200150209     C     $TASTO        WHENEQ    F08
122300150209     C                   EXSR      F08D01
122400150209     C* F12=Ritorno
122500150209     C     $TASTO        WHENEQ    F12
122600150209     C                   EXSR      F12D01
122700150209     C* F24=Altri tasti
122800150209     C     $TASTO        WHENEQ    F24
122900150209     C                   EXSR      F24Dx
123000150209     C* Personalizzata
123100150209     C     $PGF1(PosFun) Whenne    *BLANK
123200150209     C                   EXSR      FperDx
123300150209     C*
123400150209     C                   OTHER
123500150209     C                   EXSR      CtrDA2
123600150209     C     *IN99         IFEQ      *OFF
123700150209     C                   EXSR      CtrDA1
123800150209     C                   EXSR      CtrDA3
123900150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
124000150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
124100150209     C* videate
124200150209     C     *IN99         IFEQ      *ON
124300150209     C                   Move      *ALL'0'       IN5098           49
124400150209     C                   MoveA     IN5098        *IN(50)
124500150209     C                   SETON                                        9996
124600150209     C                   ENDIF
124700150209     C                   ENDIF
124800150209     C*
124900150209     C* eseguo aggiornamento
125000150209     C     $TASTO        IFEQ      F06
125100150209     C     *IN99         ANDEQ     *OFF
125200150209     C                   EXSR      AGGANA
125300150209     C                   ENDIF
125400150209     C*
125500150209     C                   ENDSL
125600150209     C*
125700150209     C                   ENDIF
125800150209     C*
125900150209     C     XGESDA2       ENDSR
126000150209     C************************************************************
126100150209     C* CONTROLLO VIDEATA  DA2
126200150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
126300150209     C* 95 96 97 98
126400150209     C************************************************************
126500150209     C     CtrDA2        BEGSR
126600150209     C*
126700150209     C                   SETOFF                                       99
126800150209     C*
126900150209     C     XCtrDA2       ENDSR
127000150209     C************************************************************
127100150209     C* GESTIONE VIDEO RECORD  DA3
127200150209     C************************************************************
127300150209     C     GESDA3        BEGSR
127400150209     C* emissione piede videata se proveniente da altra
127500150209     C* salvataggio valore formato di provenienza
127600150209     C     $LASTV        IFNE      '03'
127700150209     C                   WRITE     FMTT1
127800150209     C                   EXSR      GESPIE
127900150209     C                   Move      $LASTV        $LASTG
128000150209     C                   Move      '03'          $LASTV
128100150209     C                   ENDIF
128200150209     C*
128300150209     C* se esistono errori sulla videata
128400150209     C* emetto la write del formato a indicatori spenti per vedere
128500150209     C* le eventuali decodifiche
128600150209     C     *IN99         IFEQ      *ON
128700150209     C     $GEST         ANDEQ     'A3'
128800150209     C                   MoveA     *IN           WIN
128900150209     C                   Move      *ALL'0'       IN5098           49
129000150209     C                   MoveA     IN5098        *IN(50)
129100150209     C                   WRITE     FMTDA3
129200150209     C                   MoveA     WIN           *IN
129300150209     C                   ENDIF
129400150209     C* Inizializzazione indice tasti funzione personalizzati
129500150209     C                   Eval      PosFun = 25
129600150209     C*
129700150209     C                   SELECT
129800150209     C* Annullamento
129900150209     C     WOpzK62       WHENEQ    '04'
130000150209     C* Visualizzazione
130100150209     C     WOpzK62       OREQ      '05'
130200150209     C                   WRITE     FMTDA3
130300150209     C                   EXFMT     PROTECT
130400150209     C* Immissione/Modifica/Copia
130500150209     C                   OTHER
130600150209     C                   EXFMT     FMTDA3
130700150209     C                   ENDSL
130800150209     C*
130900150209     C* azzero la posizione del cursore
131000150209     C                   Z-ADD     0             H1RIGA
131100150209     C                   Z-ADD     0             H1COLO
131200150209     C*
131300150209     C* controllo abilitazione tasti funzione
131400150209     C                   EXSR      CTRKDx
131500150209     C*
131600150209     C     *IN99         IFEQ      *OFF
131700150209     C                   SELECT
131800150209     C* ROLDOWN
131900150209     C     $TASTO        WHENEQ    ROLDWN
132000150209     C                   Move      'A2'          $GEST
132100150209     C* F3=Fine
132200150209     C     $TASTO        WHENEQ    F03
132300150209     C                   EXSR      F03D01
132400150209     C* F5=Refresh
132500150209     C     $TASTO        WHENEQ    F05
132600150209     C                   EXSR      F05D01
132700150209     C* F8=Successivo
132800150209     C     $TASTO        WHENEQ    F08
132900150209     C                   EXSR      F08D01
133000150209     C* F12=Ritorno
133100150209     C     $TASTO        WHENEQ    F12
133200150209     C                   EXSR      F12D01
133300150209     C* F24=Altri tasti
133400150209     C     $TASTO        WHENEQ    F24
133500150209     C                   EXSR      F24Dx
133600150209     C* Personalizzata
133700150209     C     $PGF1(PosFun) Whenne    *BLANK
133800150209     C                   EXSR      FperDx
133900150209     C*
134000150209     C                   OTHER
134100150209     C                   EXSR      CtrDA3
134200150209     C     *IN99         IFEQ      *OFF
134300150209     C                   EXSR      CtrDA1
134400150209     C                   EXSR      CtrDA2
134500150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
134600150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
134700150209     C* videate
134800150209     C     *IN99         IFEQ      *ON
134900150209     C                   Move      *ALL'0'       IN5098           49
135000150209     C                   MoveA     IN5098        *IN(50)
135100150209     C                   SETON                                        9996
135200150209     C                   ENDIF
135300150209     C                   ENDIF
135400150209     C*
135500150209     C* eseguo aggiornamento
135600150209     C     $TASTO        IFEQ      F06
135700150209     C     *IN99         ANDEQ     *OFF
135800150209     C                   EXSR      AGGANA
135900150209     C                   ENDIF
136000150209     C*
136100150209     C                   ENDSL
136200150209     C*
136300150209     C                   ENDIF
136400150209     C*
136500150209     C     XGESDA3       ENDSR
136600150209     C************************************************************
136700150209     C* CONTROLLO VIDEATA  DA3
136800150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
136900150209     C* 95 96 97 98
137000150209     C************************************************************
137100150209     C     CtrDA3        BEGSR
137200150209     C*
137300150209     C                   SETOFF                                       99
137400150209     C*
137500150209     C     XCtrDA3       ENDSR
137600150209     C************************************************************
137700150209     C* GESTIONE VIDEO RECORD D31
137800150209     C************************************************************
137900150209     C     GESDB1        BEGSR
138000150209     C* inizializzazione videata
138100150209     C     $INZDX        IFEQ      *ON
138200150209     C                   EXSR      INZDX
138300150209     C                   Move      *OFF          $INZDX
138400150209     C                   ENDIF
138500150209     C* emissione piede videata se proveniente da altra
138600150209     C* salvataggio valore formato di provenienza
138700150209     C     $LASTV        IFNE      '31'
138800150209     C                   WRITE     FMTT1
138900150209     C                   EXSR      GESPIE
139000150209     C                   Move      $LASTV        $LASTG
139100150209     C                   Move      '31'          $LASTV
139200150209     C                   ENDIF
139300150209     C*
139400150209     C* se esistono errori sulla videata
139500150209     C* emetto la write del formato a indicatori spenti per vedere
139600150209     C* le eventuali decodifiche
139700150209     C     *IN99         IFEQ      *ON
139800150209     C     $GEST         ANDEQ     'B1'
139900150209     C                   MoveA     *IN           WIN
140000150209     C                   Move      *ALL'0'       IN5098           49
140100150209     C                   MoveA     IN5098        *IN(50)
140200150209     C                   WRITE     FMTDB1
140300150209     C                   MoveA     WIN           *IN
140400150209     C                   ENDIF
140500150209     C* Inizializzazione indice tasti funzione personalizzati
140600150209     C                   Eval      PosFun = 25
140700150209     C*
140800150209     C                   SELECT
140900150209     C* Annullamento
141000150209     C     WOpzK62       WHENEQ    '04'
141100150209     C* Visualizzazione
141200150209     C     WOpzK62       OREQ      '05'
141300150209     C                   WRITE     FMTDB1
141400150209     C                   EXFMT     PROTECT
141500150209     C* Immissione/Modifica/Copia
141600150209     C                   OTHER
141700150209     C                   EXFMT     FMTDB1
141800150209     C                   ENDSL
141900150209     C*
142000150209     C* azzero la posizione del cursore
142100150209     C                   Z-ADD     0             H1RIGA
142200150209     C                   Z-ADD     0             H1COLO
142300150209     C*
142400150209     C* controllo abilitazione tasti funzione
142500150209     C                   EXSR      CTRKDx
142600150209     C*
142700150209     C     *IN99         IFEQ      *OFF
142800150209     C*
142900150209     C                   SELECT
143000150209     C* ROLLUP
143100150209     C     $TASTO        WHENEQ    ROLLUP
143200150209     C                   Move      'B2'          $GEST
143300150209     C* F3=Fine
143400150209     C     $TASTO        WHENEQ    F03
143500150209     C                   EXSR      F03D01
143600150209     C* F5=Refresh
143700150209     C     $TASTO        WHENEQ    F05
143800150209     C                   EXSR      F05D01
143900150209     C* F8=Successivo
144000150209     C     $TASTO        WHENEQ    F08
144100150209     C                   EXSR      F08D01
144200150209     C* F12=Ritorno
144300150209     C     $TASTO        WHENEQ    F12
144400150209     C                   EXSR      F12D01
144500150209     C* F24=Altri tasti
144600150209     C     $TASTO        WHENEQ    F24
144700150209     C                   EXSR      F24Dx
144800150209     C* Personalizzata
144900150209     C     $PGF1(PosFun) WHENNE    *BLANK
145000150209     C                   EXSR      FPerDx
145100150209     C*
145200150209     C                   OTHER
145300150209     C                   EXSR      CtrDB1
145400150209     C     *IN99         IFEQ      *OFF
145500150209     C                   EXSR      CtrDB2
145600150209     C                   EXSR      CtrDB3
145700150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
145800150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
145900150209     C* videate
146000150209     C     *IN99         IFEQ      *ON
146100150209     C                   Move      *ALL'0'       IN5098           49
146200150209     C                   MoveA     IN5098        *IN(50)
146300150209     C                   SETON                                        9996
146400150209     C                   ENDIF
146500150209     C                   ENDIF
146600150209     C*
146700150209     C* eseguo aggiornamento
146800150209     C     $TASTO        IFEQ      F06
146900150209     C     *IN99         ANDEQ     *OFF
147000150209     C                   EXSR      AGGANA
147100150209     C                   ENDIF
147200150209     C*
147300150209     C                   ENDSL
147400150209     C*
147500150209     C                   ENDIF
147600150209     C*
147700150209     C     XGesDB1       ENDSR
147800150209     C************************************************************
147900150209     C* CONTROLLO VIDEATA
148000150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
148100150209     C* 95 96 97 98
148200150209     C************************************************************
148300150209     C     CtrDB1        BEGSR
148400150209     C*
148500150209     C                   SETOFF                                       99
148600150209     C*
148700150209     C     XCtrDB1       ENDSR
148800150209     C************************************************************
148900150209     C* GESTIONE VIDEO RECORD  D32
149000150209     C************************************************************
149100150209     C     GESDB2        BEGSR
149200150209     C* emissione piede videata se proveniente da altra
149300150209     C* salvataggio valore formato di provenienza
149400150209     C     $LASTV        IFNE      '32'
149500150209     C                   WRITE     FMTT1
149600150209     C                   EXSR      GESPIE
149700150209     C                   Move      $LASTV        $LASTG
149800150209     C                   Move      '32'          $LASTV
149900150209     C                   ENDIF
150000150209     C*
150100150209     C* se esistono errori sulla videata
150200150209     C* emetto la write del formato a indicatori spenti per vedere
150300150209     C* le eventuali decodifiche
150400150209     C     *IN99         IFEQ      *ON
150500150209     C     $GEST         ANDEQ     'B2'
150600150209     C                   MoveA     *IN           WIN
150700150209     C                   Move      *ALL'0'       IN5098           49
150800150209     C                   MoveA     IN5098        *IN(50)
150900150209     C                   WRITE     FMTDB2
151000150209     C                   MoveA     WIN           *IN
151100150209     C                   ENDIF
151200150209     C* Inizializzazione indice tasti funzione personalizzati
151300150209     C                   Eval      PosFun = 25
151400150209     C*
151500150209     C                   SELECT
151600150209     C* Annullamento
151700150209     C     WOpzK62       WHENEQ    '04'
151800150209     C* Visualizzazione
151900150209     C     WOpzK62       OREQ      '05'
152000150209     C                   WRITE     FMTDB2
152100150209     C                   EXFMT     PROTECT
152200150209     C* Immissione/Modifica/Copia
152300150209     C                   OTHER
152400150209     C                   EXFMT     FMTDB2
152500150209     C                   ENDSL
152600150209     C*
152700150209     C* azzero la posizione del cursore
152800150209     C                   Z-ADD     0             H1RIGA
152900150209     C                   Z-ADD     0             H1COLO
153000150209     C*
153100150209     C* controllo abilitazione tasti funzione
153200150209     C                   EXSR      CTRKDx
153300150209     C*
153400150209     C     *IN99         IFEQ      *OFF
153500150209     C*
153600150209     C                   SELECT
153700150209R078 C*    ROLLUP
153800150209 "   C     $TASTO        WHENEQ    ROLLUP
153900150209R078 C                   Move      'B3'          $GEST
154000150209     C* ROLDOWN
154100150209     C     $TASTO        WHENEQ    ROLDWN
154200150209     C                   Move      'B1'          $GEST
154300150209     C* F3=Fine
154400150209     C     $TASTO        WHENEQ    F03
154500150209     C                   EXSR      F03D01
154600150209     C* F5=Refresh
154700150209     C     $TASTO        WHENEQ    F05
154800150209     C                   EXSR      F05D01
154900150209     C* F8=Successivo
155000150209     C     $TASTO        WHENEQ    F08
155100150209     C                   EXSR      F08D01
155200150209     C* F12=Ritorno
155300150209     C     $TASTO        WHENEQ    F12
155400150209     C                   EXSR      F12D01
155500150209     C* F24=Altri tasti
155600150209     C     $TASTO        WHENEQ    F24
155700150209     C                   EXSR      F24Dx
155800150209     C* Personalizzata
155900150209     C     $PGF1(PosFun) Whenne    *BLANK
156000150209     C                   EXSR      FperDx
156100150209     C*
156200150209     C                   OTHER
156300150209     C                   EXSR      CtrDB2
156400150209     C     *IN99         IFEQ      *OFF
156500150209     C                   EXSR      CtrDB1
156600150209     C                   EXSR      CtrDB3
156700150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
156800150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
156900150209     C* videate
157000150209     C     *IN99         IFEQ      *ON
157100150209     C                   Move      *ALL'0'       IN5098           49
157200150209     C                   MoveA     IN5098        *IN(50)
157300150209     C                   SETON                                        9996
157400150209     C                   ENDIF
157500150209     C                   ENDIF
157600150209     C*
157700150209     C* eseguo aggiornamento
157800150209     C     $TASTO        IFEQ      F06
157900150209     C     *IN99         ANDEQ     *OFF
158000150209     C                   EXSR      AGGANA
158100150209     C                   ENDIF
158200150209     C*
158300150209     C                   ENDSL
158400150209     C*
158500150209     C                   ENDIF
158600150209     C*
158700150209     C     XGesDB2       ENDSR
158800150209     C************************************************************
158900150209     C* CONTROLLO VIDEATA  32
159000150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
159100150209     C* 95 96 97 98
159200150209     C************************************************************
159300150209     C     CtrDB2        BEGSR
159400150209     C*
159500150209     C                   SETOFF                                       99
159600150209     C*
159700150209     C     XCtrDB2       ENDSR
159800150209     C************************************************************
159900150209     C* GESTIONE VIDEO RECORD  DB3
160000150209     C************************************************************
160100150209     C     GESDB3        BEGSR
160200150209     C* emissione piede videata se proveniente da altra
160300150209     C* salvataggio valore formato di provenienza
160400150209     C     $LASTV        IFNE      '33'
160500150209     C                   WRITE     FMTT1
160600150209     C                   EXSR      GESPIE
160700150209     C                   Move      $LASTV        $LASTG
160800150209     C                   Move      '33'          $LASTV
160900150209     C                   ENDIF
161000150209     C*
161100150209     C* se esistono errori sulla videata
161200150209     C* emetto la write del formato a indicatori spenti per vedere
161300150209     C* le eventuali decodifiche
161400150209     C     *IN99         IFEQ      *ON
161500150209     C     $GEST         ANDEQ     'B3'
161600150209     C                   MoveA     *IN           WIN
161700150209     C                   Move      *ALL'0'       IN5098           49
161800150209     C                   MoveA     IN5098        *IN(50)
161900150209     C                   WRITE     FMTDB3
162000150209     C                   MoveA     WIN           *IN
162100150209     C                   ENDIF
162200150209     C* Inizializzazione indice tasti funzione personalizzati
162300150209     C                   Eval      PosFun = 25
162400150209     C*
162500150209     C                   SELECT
162600150209     C* Annullamento
162700150209     C     WOpzK62       WHENEQ    '04'
162800150209     C* Visualizzazione
162900150209     C     WOpzK62       OREQ      '05'
163000150209     C                   WRITE     FMTDB3
163100150209     C                   EXFMT     PROTECT
163200150209     C* Immissione/Modifica/Copia
163300150209     C                   OTHER
163400150209     C                   EXFMT     FMTDB3
163500150209     C                   ENDSL
163600150209     C*
163700150209     C* azzero la posizione del cursore
163800150209     C                   Z-ADD     0             H1RIGA
163900150209     C                   Z-ADD     0             H1COLO
164000150209     C*
164100150209     C* controllo abilitazione tasti funzione
164200150209     C                   EXSR      CTRKDx
164300150209     C*
164400150209     C     *IN99         IFEQ      *OFF
164500150209     C*
164600150209     C                   SELECT
164700150209R078 C* ROLLUP
164800150209 "   C     $TASTO        WHENEQ    ROLLUP
164900150209R078 C                   Move      'B4'          $GEST
165000150209     C* ROLDOWN
165100150209     C     $TASTO        WHENEQ    ROLDWN
165200150209     C                   Move      'B2'          $GEST
165300150209     C* F3=Fine
165400150209     C     $TASTO        WHENEQ    F03
165500150209     C                   EXSR      F03D01
165600150209     C* F5=Refresh
165700150209     C     $TASTO        WHENEQ    F05
165800150209     C                   EXSR      F05D01
165900150209     C* F8=Successivo
166000150209     C     $TASTO        WHENEQ    F08
166100150209     C                   EXSR      F08D01
166200150209     C* F12=Ritorno
166300150209     C     $TASTO        WHENEQ    F12
166400150209     C                   EXSR      F12D01
166500150209     C* F24=Altri tasti
166600150209     C     $TASTO        WHENEQ    F24
166700150209     C                   EXSR      F24Dx
166800150209     C* Personalizzata
166900150209     C     $PGF1(PosFun) Whenne    *BLANK
167000150209     C                   EXSR      FperDx
167100150209     C*
167200150209     C                   OTHER
167300150209     C                   EXSR      CtrDB3
167400150209     C     *IN99         IFEQ      *OFF
167500150209     C                   EXSR      CtrDB1
167600150209     C                   EXSR      CtrDB2
167700150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
167800150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
167900150209     C* videate
168000150209     C     *IN99         IFEQ      *ON
168100150209     C                   Move      *ALL'0'       IN5098           49
168200150209     C                   MoveA     IN5098        *IN(50)
168300150209     C                   SETON                                        9996
168400150209     C                   ENDIF
168500150209     C                   ENDIF
168600150209     C*
168700150209     C* eseguo aggiornamento
168800150209     C     $TASTO        IFEQ      F06
168900150209     C     *IN99         ANDEQ     *OFF
169000150209     C                   EXSR      AGGANA
169100150209     C                   ENDIF
169200150209     C*
169300150209     C                   ENDSL
169400150209     C*
169500150209     C                   ENDIF
169600150209     C*
169700150209     C     XGesDB3       ENDSR
169800150209     C************************************************************
169900150209     C* CONTROLLO VIDEATA  DB3
170000150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
170100150209     C* 95 96 97 98
170200150209     C************************************************************
170300150209     C     CtrDB3        BEGSR
170400150209     C*
170500150209     C                   SETOFF                                       99
170600150209     C*
170700150209     C     XCtrDB3       ENDSR
170800150209     C************************************************************
170900150209     C* GESTIONE VIDEO RECORD  DB4
171000150209     C************************************************************
171100150209     C     GESDB4        BEGSR
171200150209     C* emissione piede videata se proveniente da altra
171300150209     C* salvataggio valore formato di provenienza
171400150209     C     $LASTV        IFNE      '34'
171500150209     C                   WRITE     FMTT1
171600150209     C                   EXSR      GESPIE
171700150209     C                   Move      $LASTV        $LASTG
171800150209     C                   Move      '34'          $LASTV
171900150209     C                   ENDIF
172000150209     C*
172100150209     C* se esistono errori sulla videata
172200150209     C* emetto la write del formato a indicatori spenti per vedere
172300150209     C* le eventuali decodifiche
172400150209     C     *IN99         IFEQ      *ON
172500150209     C     $GEST         ANDEQ     'B4'
172600150209     C                   MoveA     *IN           WIN
172700150209     C                   Move      *ALL'0'       IN5098           49
172800150209     C                   MoveA     IN5098        *IN(50)
172900150209     C                   WRITE     FMTDB4
173000150209     C                   MoveA     WIN           *IN
173100150209     C                   ENDIF
173200150209     C* Inizializzazione indice tasti funzione personalizzati
173300150209     C                   Eval      PosFun = 25
173400150209     C*
173500150209     C                   SELECT
173600150209     C* Annullamento
173700150209     C     WOpzK62       WHENEQ    '04'
173800150209     C* Visualizzazione
173900150209     C     WOpzK62       OREQ      '05'
174000150209     C                   WRITE     FMTDB4
174100150209     C                   EXFMT     PROTECT
174200150209     C* Immissione/Modifica/Copia
174300150209     C                   OTHER
174400150209     C                   EXFMT     FMTDB4
174500150209     C                   ENDSL
174600150209     C*
174700150209     C* azzero la posizione del cursore
174800150209     C                   Z-ADD     0             H1RIGA
174900150209     C                   Z-ADD     0             H1COLO
175000150209     C*
175100150209     C* controllo abilitazione tasti funzione
175200150209     C                   EXSR      CTRKDx
175300150209     C*
175400150209     C     *IN99         IFEQ      *OFF
175500150209     C*
175600150209     C                   SELECT
175700150209     C* ROLDOWN
175800150209     C     $TASTO        WHENEQ    ROLDWN
175900150209     C                   Move      'B3'          $GEST
176000150209     C* F3=Fine
176100150209     C     $TASTO        WHENEQ    F03
176200150209     C                   EXSR      F03D01
176300150209     C* F5=Refresh
176400150209     C     $TASTO        WHENEQ    F05
176500150209     C                   EXSR      F05D01
176600150209     C* F8=Successivo
176700150209     C     $TASTO        WHENEQ    F08
176800150209     C                   EXSR      F08D01
176900150209     C* F12=Ritorno
177000150209     C     $TASTO        WHENEQ    F12
177100150209     C                   EXSR      F12D01
177200150209     C* F24=Altri tasti
177300150209     C     $TASTO        WHENEQ    F24
177400150209     C                   EXSR      F24Dx
177500150209     C* Personalizzata
177600150209     C     $PGF1(PosFun) Whenne    *BLANK
177700150209     C                   EXSR      FperDx
177800150209     C*
177900150209     C                   OTHER
178000150209     C                   EXSR      CtrDB4
178100150209     C     *IN99         IFEQ      *OFF
178200150209     C                   EXSR      CtrDB1
178300150209     C                   EXSR      CtrDB2
178400150209     C                   EXSR      CtrDB3
178500150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
178600150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
178700150209     C* videate
178800150209     C     *IN99         IFEQ      *ON
178900150209     C                   Move      *ALL'0'       IN5098           49
179000150209     C                   MoveA     IN5098        *IN(50)
179100150209     C                   SETON                                        9996
179200150209     C                   ENDIF
179300150209     C                   ENDIF
179400150209     C*
179500150209     C* eseguo aggiornamento
179600150209     C     $TASTO        IFEQ      F06
179700150209     C     *IN99         ANDEQ     *OFF
179800150209     C                   EXSR      AGGANA
179900150209     C                   ENDIF
180000150209     C*
180100150209     C                   ENDSL
180200150209     C*
180300150209     C                   ENDIF
180400150209     C*
180500150209     C     XGesDB4       ENDSR
180600150209     C************************************************************
180700150209     C* CONTROLLO VIDEATA  DB4
180800150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
180900150209     C* 95 96 97 98
181000150209     C************************************************************
181100150209     C     CtrDB4        BEGSR
181200150209     C*
181300150209     C                   SETOFF                                       99
181400150209     C*
181500150209     C     XCtrDB4       ENDSR
181600150209     C************************************************************
181700150209     C* GESTIONE VIDEO RECORD DZ1
181800150209     C************************************************************
181900150209     C     GESDZ1        BEGSR
182000150209     C* inizializzazione videata
182100150209     C     $INZDX        IFEQ      *ON
182200150209     C                   EXSR      INZDX
182300150209     C                   Move      *OFF          $INZDX
182400150209     C                   ENDIF
182500150209     C* emissione piede videata se proveniente da altra
182600150209     C* salvataggio valore formato di provenienza
182700150209     C     $LASTV        IFNE      '42'
182800150209     C                   WRITE     FMTT1
182900150209     C                   EXSR      GESPIE
183000150209     C                   Move      $LASTV        $LASTG
183100150209     C                   Move      '42'          $LASTV
183200150209     C                   ENDIF
183300150209     C*
183400150209     C* se esistono errori sulla videata
183500150209     C* emetto la write del formato a indicatori spenti per vedere
183600150209     C* le eventuali decodifiche
183700150209     C     *IN99         IFEQ      *ON
183800150209     C     $GEST         ANDEQ     'Z1'
183900150209     C                   MoveA     *IN           WIN
184000150209     C                   Move      *ALL'0'       IN5098           49
184100150209     C                   MoveA     IN5098        *IN(50)
184200150209     C                   WRITE     FMTDZ1
184300150209     C                   MoveA     WIN           *IN
184400150209     C                   ENDIF
184500150209     C* Inizializzazione indice tasti funzione personalizzati
184600150209     C                   Eval      PosFun = 25
184700150209     C*
184800150209     C                   SELECT
184900150209     C* Annullamento
185000150209     C     WOpzK62       WHENEQ    '04'
185100150209     C* Visualizzazione
185200150209     C     WOpzK62       OREQ      '05'
185300150209     C                   WRITE     FMTDZ1
185400150209     C                   EXFMT     PROTECT
185500150209     C* Immissione/Modifica/Copia
185600150209     C                   OTHER
185700150209     C                   EXFMT     FMTDZ1
185800150209     C                   ENDSL
185900150209     C*
186000150209     C* azzero la posizione del cursore
186100150209     C                   Z-ADD     0             H1RIGA
186200150209     C                   Z-ADD     0             H1COLO
186300150209     C*
186400150209     C* controllo abilitazione tasti funzione
186500150209     C                   EXSR      CTRKDx
186600150209     C*
186700150209     C     *IN99         IFEQ      *OFF
186800150209     C*
186900150209     C                   SELECT
187000150209     C* ROLLUP
187100150209     C     $TASTO        WHENEQ    ROLLUP
187200150209     C                   Move      'Z2'          $GEST
187300150209     C* F3=Fine
187400150209     C     $TASTO        WHENEQ    F03
187500150209     C                   EXSR      F03D01
187600150209     C* F5=Refresh
187700150209     C     $TASTO        WHENEQ    F05
187800150209     C                   EXSR      F05D01
187900150209     C* F8=Successivo
188000150209     C     $TASTO        WHENEQ    F08
188100150209     C                   EXSR      F08D01
188200150209     C* F12=Ritorno
188300150209     C     $TASTO        WHENEQ    F12
188400150209     C                   EXSR      F12D01
188500150209     C* F24=Altri tasti
188600150209     C     $TASTO        WHENEQ    F24
188700150209     C                   EXSR      F24Dx
188800150209     C* Personalizzata
188900150209     C     $PGF1(PosFun) WHENNE    *BLANK
189000150209     C                   EXSR      FPerDx
189100150209     C*
189200150209     C                   OTHER
189300150209     C                   EXSR      CtrDZ1
189400150209     C     *IN99         IFEQ      *OFF
189500150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
189600150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
189700150209     C* videate
189800150209     C     *IN99         IFEQ      *ON
189900150209     C                   Move      *ALL'0'       IN5098           49
190000150209     C                   MoveA     IN5098        *IN(50)
190100150209     C                   SETON                                        9996
190200150209     C                   ENDIF
190300150209     C                   ENDIF
190400150209     C*
190500150209     C* eseguo aggiornamento
190600150209     C     $TASTO        IFEQ      F06
190700150209     C     *IN99         ANDEQ     *OFF
190800150209     C                   EXSR      AGGANA
190900150209     C                   ENDIF
191000150209     C*
191100150209     C                   ENDSL
191200150209     C*
191300150209     C                   ENDIF
191400150209     C*
191500150209     C     XGesDZ1       ENDSR
191600150209     C************************************************************
191700150209     C* CONTROLLO VIDEATA
191800150209     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
191900150209     C* 95 96 97 98
192000150209     C************************************************************
192100150209     C     CtrDZ1        BEGSR
192200150209     C*
192300150209     C                   SETOFF                                       99
192400150209     C*
192500150209     C     XCtrDZ1       ENDSR
192600150209     C************************************************************
192700150209     C* GESTIONE VIDEO RECORD DZ2
192800150209     C************************************************************
192900150209     C     GESDZ2        BEGSR
193000150209     C* inizializzazione videata
193100150209     C     $INZDX        IFEQ      *ON
193200150209     C                   EXSR      INZDX
193300150209     C                   Move      *OFF          $INZDX
193400150209     C                   ENDIF
193500150209     C* emissione piede videata se proveniente da altra
193600150209     C* salvataggio valore formato di provenienza
193700150209     C     $LASTV        IFNE      '43'
193800150209     C                   WRITE     FMTT1
193900150209     C                   EXSR      GESPIE
194000150209     C                   Move      $LASTV        $LASTG
194100150209     C                   Move      '43'          $LASTV
194200150209     C                   ENDIF
194300150209     C*
194400150209     C* se esistono errori sulla videata
194500150209     C* emetto la write del formato a indicatori spenti per vedere
194600150209     C* le eventuali decodifiche
194700150209     C     *IN99         IFEQ      *ON
194800150209     C     $GEST         ANDEQ     'Z2'
194900150209     C                   MoveA     *IN           WIN
195000150209     C                   Move      *ALL'0'       IN5098           49
195100150209     C                   MoveA     IN5098        *IN(50)
195200150209     C                   WRITE     FMTDZ2
195300150209     C                   MoveA     WIN           *IN
195400150209     C                   ENDIF
195500150209     C* Inizializzazione indice tasti funzione personalizzati
195600150209     C                   Eval      PosFun = 25
195700150209     C*
195800150209     C                   SELECT
195900150209     C* Annullamento
196000150209     C     WOpzK62       WHENEQ    '04'
196100150209     C* Visualizzazione
196200150209     C     WOpzK62       OREQ      '05'
196300150209     C                   WRITE     FMTDZ2
196400150209     C                   EXFMT     PROTECT
196500150209     C* Immissione/Modifica/Copia
196600150209     C                   OTHER
196700150209     C                   EXFMT     FMTDZ2
196800150209     C                   ENDSL
196900150209     C*
197000150209     C* azzero la posizione del cursore
197100150209     C                   Z-ADD     0             H1RIGA
197200150209     C                   Z-ADD     0             H1COLO
197300150209     C*
197400150209     C* controllo abilitazione tasti funzione
197500150209     C                   EXSR      CTRKDx
197600150209     C*
197700150209     C     *IN99         IFEQ      *OFF
197800150209     C*
197900150209     C                   SELECT
198000150209     C* ROLLUP
198100150209     C     $TASTO        WHENEQ    ROLLUP
198200150209     C                   Move      'Z3'          $GEST
198300150209     C* ROLLUP
198400150209     C     $TASTO        WHENEQ    ROLDWN
198500150209     C                   Move      'Z1'          $GEST
198600150209     C* F3=Fine
198700150209     C     $TASTO        WHENEQ    F03
198800150209     C                   EXSR      F03D01
198900150209     C* F5=Refresh
199000150209     C     $TASTO        WHENEQ    F05
199100150209     C                   EXSR      F05D01
199200150209     C* F8=Successivo
199300150209     C     $TASTO        WHENEQ    F08
199400150209     C                   EXSR      F08D01
199500150209     C* F12=Ritorno
199600150209     C     $TASTO        WHENEQ    F12
199700150209     C                   EXSR      F12D01
199800150209     C* F24=Altri tasti
199900150209     C     $TASTO        WHENEQ    F24
200000150209     C                   EXSR      F24Dx
200100150209     C* Personalizzata
200200150209     C     $PGF1(PosFun) WHENNE    *BLANK
200300150209     C                   EXSR      FPerDx
200400150209     C*
200500150209     C                   OTHER
200600150209     C                   EXSR      CtrDZ1
200700150209     C     *IN99         IFEQ      *OFF
200800150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
200900150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
201000150209     C* videate
201100150209     C     *IN99         IFEQ      *ON
201200150209     C                   Move      *ALL'0'       IN5098           49
201300150209     C                   MoveA     IN5098        *IN(50)
201400150209     C                   SETON                                        9996
201500150209     C                   ENDIF
201600150209     C                   ENDIF
201700150209     C*
201800150209     C* eseguo aggiornamento
201900150209     C     $TASTO        IFEQ      F06
202000150209     C     *IN99         ANDEQ     *OFF
202100150209     C                   EXSR      AGGANA
202200150209     C                   ENDIF
202300150209     C*
202400150209     C                   ENDSL
202500150209     C*
202600150209     C                   ENDIF
202700150209     C*
202800150209     C     XGesDZ2       ENDSR
202900150209     C************************************************************
203000150209     C* GESTIONE VIDEO RECORD DZ3
203100150209     C************************************************************
203200150209     C     GESDZ3        BEGSR
203300150209     C* inizializzazione videata
203400150209     C     $INZDX        IFEQ      *ON
203500150209     C                   EXSR      INZDX
203600150209     C                   Move      *OFF          $INZDX
203700150209     C                   ENDIF
203800150209     C* emissione piede videata se proveniente da altra
203900150209     C* salvataggio valore formato di provenienza
204000150209     C     $LASTV        IFNE      '44'
204100150209     C                   WRITE     FMTT1
204200150209     C                   EXSR      GESPIE
204300150209     C                   Move      $LASTV        $LASTG
204400150209     C                   Move      '44'          $LASTV
204500150209     C                   ENDIF
204600150209     C*
204700150209     C* se esistono errori sulla videata
204800150209     C* emetto la write del formato a indicatori spenti per vedere
204900150209     C* le eventuali decodifiche
205000150209     C     *IN99         IFEQ      *ON
205100150209     C     $GEST         ANDEQ     'Z3'
205200150209     C                   MoveA     *IN           WIN
205300150209     C                   Move      *ALL'0'       IN5098           49
205400150209     C                   MoveA     IN5098        *IN(50)
205500150209     C                   WRITE     FMTDZ3
205600150209     C                   MoveA     WIN           *IN
205700150209     C                   ENDIF
205800150209     C* Inizializzazione indice tasti funzione personalizzati
205900150209     C                   Eval      PosFun = 25
206000150209     C*
206100150209     C                   SELECT
206200150209     C* Annullamento
206300150209     C     WOpzK62       WHENEQ    '04'
206400150209     C* Visualizzazione
206500150209     C     WOpzK62       OREQ      '05'
206600150209     C                   WRITE     FMTDZ3
206700150209     C                   EXFMT     PROTECT
206800150209     C* Immissione/Modifica/Copia
206900150209     C                   OTHER
207000150209     C                   EXFMT     FMTDZ3
207100150209     C                   ENDSL
207200150209     C*
207300150209     C* azzero la posizione del cursore
207400150209     C                   Z-ADD     0             H1RIGA
207500150209     C                   Z-ADD     0             H1COLO
207600150209     C*
207700150209     C* controllo abilitazione tasti funzione
207800150209     C                   EXSR      CTRKDx
207900150209     C*
208000150209     C     *IN99         IFEQ      *OFF
208100150209     C*
208200150209     C                   SELECT
208300150209     C* ROLLUP
208400150209     C     $TASTO        WHENEQ    ROLDWN
208500150209     C                   Move      'Z2'          $GEST
208600150209     C* F3=Fine
208700150209     C     $TASTO        WHENEQ    F03
208800150209     C                   EXSR      F03D01
208900150209     C* F5=Refresh
209000150209     C     $TASTO        WHENEQ    F05
209100150209     C                   EXSR      F05D01
209200150209     C* F8=Successivo
209300150209     C     $TASTO        WHENEQ    F08
209400150209     C                   EXSR      F08D01
209500150209     C* F12=Ritorno
209600150209     C     $TASTO        WHENEQ    F12
209700150209     C                   EXSR      F12D01
209800150209     C* F24=Altri tasti
209900150209     C     $TASTO        WHENEQ    F24
210000150209     C                   EXSR      F24Dx
210100150209     C* Personalizzata
210200150209     C     $PGF1(PosFun) WHENNE    *BLANK
210300150209     C                   EXSR      FPerDx
210400150209     C*
210500150209     C                   OTHER
210600150209     C                   EXSR      CtrDZ1
210700150209     C     *IN99         IFEQ      *OFF
210800150209     C* Spengo gli indicatori di errore per il controllo su altri schermi
210900150209     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
211000150209     C* videate
211100150209     C     *IN99         IFEQ      *ON
211200150209     C                   Move      *ALL'0'       IN5098           49
211300150209     C                   MoveA     IN5098        *IN(50)
211400150209     C                   SETON                                        9996
211500150209     C                   ENDIF
211600150209     C                   ENDIF
211700150209     C*
211800150209     C* eseguo aggiornamento
211900150209     C     $TASTO        IFEQ      F06
212000150209     C     *IN99         ANDEQ     *OFF
212100150209     C                   EXSR      AGGANA
212200150209     C                   ENDIF
212300150209     C*
212400150209     C                   ENDSL
212500150209     C*
212600150209     C                   ENDIF
212700150209     C*
212800150209     C     XGesDZ3       ENDSR
212900150209     C************************************************************
213000150209     C* AGGIORNAMENTO ANAGRAFICA
213100150209     C************************************************************
213200150209     C     AGGANA        BEGSR
213300150209     C*
213400150209     C*-------------------------------------------
213500150209     C                   select
213600150209    >C                   when      WOpzK62 ='02'
213700150209     C                   exsr      RieAna
213800150209    >C                   update    SDGCUW000
213900150209     C                   endsl
214000150209     C*
214100150209     C*-------------------------------------------
214200150209     C* richiama pgm User Exit
214300150209     C     USRERR        IFEQ      *BLANK
214400150209     C* passa opzione ricevuta a user exit
214500150209    >C                   MoveL     WOpzK62       XUKOPZ
214600150209     C* imposta chiave univoca  per user exit
214700150209     C                   Move      XSCSOC        XUKSOC
214800150209     C                   Move      XSCDUN        XUKDUN
214900150209     C                   MoveL     DSPGM         XUKPGM
215000150209    >C                   MoveL     D1TpRec       XUKFL1
215100150209    >C* ho fino a 6 campi chiave passabili
215200150209     C                   MoveL     XUKDS         KPJBU
215300150209     C     DSPGM         CAT       'U':0         USREXT           10
215400150209     C                   CALL      USREXT                               21
215500150209     C                   PARM                    KPJBA
215600150209     C     *IN21         IFEQ      *ON
215700150209     C                   Move      *ON           USRERR            1
215800150209     C                   ENDIF
215900150209     C                   ENDIF
216000150209     C*
216100150209     C*-------------------------------------------
216200150209     C* eseguo operazioni del dopo-aggiornamento
216300150209     C                   if        *in99 = *off
216400150209     C* segnalo al pgm chiamante l'avvenuta conferma
216500150209     C                   Move      *ON           OPRK62
216600150209     C* segnalo al pgm chiamante l'aver premuto l'F6
216700150209     C                   Move      '0'           RETK62
216800150209     C* verifico se ci sono altri record dello stesso tipo scelto
216900150209     C                   Move      'DX'          $GEST
217000150209     C                   endif
217100150209     C*
217200150209     C     XAggAna       ENDSR
217300150209     C************************************************************
217400150209     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
217500150209     C************************************************************
217600150209     C     RIEANA        BEGSR
217700150209     C*
217800150209     C                   select
217900150209     C                   when      D1TpRec='A'
218000150209     C                   eval      CUWREC= TPRECADS
218100150209     C                   when      D1TpRec='B'
218200150209     C                   eval      CUWREC= TPRECBDS
218300150209     C                   when      D1TpRec='Z'
218400150209     C                   eval      CUWREC= TPRECZDS
218500150209     C                   endsl
218600150209     C*
218700150209     C     XRieAna       ENDSR
218800150209     C************************************************************
218900150209     C* CONTROLLO ABILITAZIONE TASTI
219000150209     C************************************************************
219100150209     C     CTRKEY        BEGSR
219200150209     C*
219300150209     C                   SELECT
219400150209     C     $TASTO        WHENEQ    F01
219500150209     C                   Z-ADD     01            PosFun
219600150209     C     $FC(1)        IFEQ      '0'
219700150209     C                   SETON                                        99
219800150209     C                   ENDIF
219900150209     C     $TASTO        WHENEQ    F02
220000150209     C                   Z-ADD     02            PosFun
220100150209     C     $FC(2)        IFEQ      '0'
220200150209     C                   SETON                                        99
220300150209     C                   ENDIF
220400150209     C     $TASTO        WHENEQ    F03
220500150209     C                   Z-ADD     03            PosFun
220600150209     C     $FC(3)        IFEQ      '0'
220700150209     C                   SETON                                        99
220800150209     C                   ENDIF
220900150209     C     $TASTO        WHENEQ    F04
221000150209     C                   Z-ADD     04            PosFun
221100150209     C     $FC(4)        IFEQ      '0'
221200150209     C                   SETON                                        99
221300150209     C                   ENDIF
221400150209     C     $TASTO        WHENEQ    F05
221500150209     C                   Z-ADD     05            PosFun
221600150209     C     $FC(5)        IFEQ      '0'
221700150209     C                   SETON                                        99
221800150209     C                   ENDIF
221900150209     C     $TASTO        WHENEQ    F06
222000150209     C                   Z-ADD     06            PosFun
222100150209     C     $FC(6)        IFEQ      '0'
222200150209     C                   SETON                                        99
222300150209     C                   ENDIF
222400150209     C     $TASTO        WHENEQ    F07
222500150209     C                   Z-ADD     07            PosFun
222600150209     C     $FC(7)        IFEQ      '0'
222700150209     C                   SETON                                        99
222800150209     C                   ENDIF
222900150209     C     $TASTO        WHENEQ    F08
223000150209     C                   Z-ADD     08            PosFun
223100150209     C     $FC(8)        IFEQ      '0'
223200150209     C                   SETON                                        99
223300150209     C                   ENDIF
223400150209     C     $TASTO        WHENEQ    F09
223500150209     C                   Z-ADD     09            PosFun
223600150209     C     $FC(09)       IFEQ      '0'
223700150209     C                   SETON                                        99
223800150209     C                   ENDIF
223900150209     C     $TASTO        WHENEQ    F10
224000150209     C                   Z-ADD     10            PosFun
224100150209     C     $FC(10)       IFEQ      '0'
224200150209     C                   SETON                                        99
224300150209     C                   ENDIF
224400150209     C     $TASTO        WHENEQ    F11
224500150209     C                   Z-ADD     11            PosFun
224600150209     C     $FC(11)       IFEQ      '0'
224700150209     C                   SETON                                        99
224800150209     C                   ENDIF
224900150209     C     $TASTO        WHENEQ    F12
225000150209     C                   Z-ADD     12            PosFun
225100150209     C     $FC(12)       IFEQ      '0'
225200150209     C                   SETON                                        99
225300150209     C                   ENDIF
225400150209     C     $TASTO        WHENEQ    F13
225500150209     C                   Z-ADD     13            PosFun
225600150209     C     $FC(13)       IFEQ      '0'
225700150209     C                   SETON                                        99
225800150209     C                   ENDIF
225900150209     C     $TASTO        WHENEQ    F14
226000150209     C                   Z-ADD     14            PosFun
226100150209     C     $FC(14)       IFEQ      '0'
226200150209     C                   SETON                                        99
226300150209     C                   ENDIF
226400150209     C     $TASTO        WHENEQ    F15
226500150209     C                   Z-ADD     15            PosFun
226600150209     C     $FC(15)       IFEQ      '0'
226700150209     C                   SETON                                        99
226800150209     C                   ENDIF
226900150209     C     $TASTO        WHENEQ    F16
227000150209     C                   Z-ADD     16            PosFun
227100150209     C     $FC(16)       IFEQ      '0'
227200150209     C                   SETON                                        99
227300150209     C                   ENDIF
227400150209     C     $TASTO        WHENEQ    F17
227500150209     C                   Z-ADD     17            PosFun
227600150209     C     $FC(17)       IFEQ      '0'
227700150209     C                   SETON                                        99
227800150209     C                   ENDIF
227900150209     C     $TASTO        WHENEQ    F18
228000150209     C                   Z-ADD     18            PosFun
228100150209     C     $FC(18)       IFEQ      '0'
228200150209     C                   SETON                                        99
228300150209     C                   ENDIF
228400150209     C     $TASTO        WHENEQ    F19
228500150209     C                   Z-ADD     19            PosFun
228600150209     C     $FC(19)       IFEQ      '0'
228700150209     C                   SETON                                        99
228800150209     C                   ENDIF
228900150209     C     $TASTO        WHENEQ    F20
229000150209     C                   Z-ADD     20            PosFun
229100150209     C     $FC(20)       IFEQ      '0'
229200150209     C                   SETON                                        99
229300150209     C                   ENDIF
229400150209     C     $TASTO        WHENEQ    F21
229500150209     C                   Z-ADD     21            PosFun
229600150209     C     $FC(21)       IFEQ      '0'
229700150209     C                   SETON                                        99
229800150209     C                   ENDIF
229900150209     C     $TASTO        WHENEQ    F22
230000150209     C                   Z-ADD     22            PosFun
230100150209     C     $FC(22)       IFEQ      '0'
230200150209     C                   SETON                                        99
230300150209     C                   ENDIF
230400150209     C     $TASTO        WHENEQ    F23
230500150209     C                   Z-ADD     23            PosFun
230600150209     C     $FC(23)       IFEQ      '0'
230700150209     C                   SETON                                        99
230800150209     C                   ENDIF
230900150209     C     $TASTO        WHENEQ    F24
231000150209     C                   Z-ADD     24            PosFun
231100150209     C     $FC(24)       IFEQ      '0'
231200150209     C                   SETON                                        99
231300150209     C                   ENDIF
231400150209     C                   ENDSL
231500150209     C*
231600150209     C     XCtrKey       ENDSR
231700150209     C******************************************************
231800150209     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
231900150209     C******************************************************
232000150209     C     MSGPGM        BEGSR
232100150209     C*
232200150209     C                   Z-ADD     2             MSGMSG
232300150209     C                   MoveL     'S'           MSGRSP
232400150209     C                   MoveL     'CR'          MSGTPR
232500150209     C                   MoveL     'S'           MSGEMV
232600150209     C                   Z-ADD     10            MSGRGP
232700150209     C                   MoveL     'S'           MSGLCK
232800150209     C                   MoveA     STATUS        STS
232900150209     C                   MoveL     'N'           MSGCNL
233000150209     C                   MoveL     'N'           MSGVID
233100150209     C                   MoveL     'N'           MSGOPE
233200150209     C                   MoveL     'N'           MSGSTP
233300150209     C                   CALL      'SDGMSGR'
233400150209     C                   PARM                    SDGMSGDS
233500150209     C*
233600150209     C                   ENDSR
233700150209     C************************************************************
233800150209     C* OPERAZIONI INIZIALI
233900150209     C************************************************************
234000150209     C     *INZSR        BEGSR
234100150209     C*
234200150209     C* Reperimento parametri
234300150209     C     *ENTRY        PLIST
234400150209     C                   PARM                    KPJBA
234600150209     C*
235400150209     C*
235500150209     C* reperisco le dimensioni delle variabili che conterranno i tasti
235600150209     C                   EVAL      $D1L01 = %Size(Z1Ke1)
235700150209     C                   EVAL      $D1L02 = %Size(Z1Ke2)
235800150209     C                   EVAL      $DxL01 = %Size(Z2Ke1)
235900150209     C                   EVAL      $DxL02 = %Size(Z2Ke2)
236000150209     C*
236100150209     C* Reperimento tasti di funzione
236200150209     C                   EXSR      REPKEY
236300150209     C*
236400150209     C* Richiamo routine per aggiunta tasti funzione personalizzati
236500150209     C                   CAllb     'X46USREXT'
236600150209     C                   PARM                    *omit
236700150209     C                   PARM                    *omit
236800150209     C                   PARM                    *omit
236900150209     C                   PARM                    *omit
237000150209     C                   PARM                    *omit
237100150209     C                   PARM                    $FX1
237200150209     C                   PARM                    $FC1
237300150209     C                   PARM                    $PGF1
237400150209     C                   PARM                    DsPgm
237500150209     C                   PARM                    xsccut
237600150209     C                   PARM                    xscgrp
237700150209     C*
237800150209     C     XInzSr        ENDSR
237900150209     C************************************************************
238000150209     C* REPERIMENTO TASTI DI FUNZIONE
238100150209     C************************************************************
238200150209     C     REPKEY        BEGSR
238300150209     C*
238400150209     C                   MoveA     $FM1          $FM
238500150209     C                   MoveA     $FC1          $FC
238600150209     C                   EXSR      REPKKK
238700150209     C                   MoveA     $FX           $FX1
238800150209     C*
238900150209     C                   MoveA     $FM2          $FM
239000150209     C                   MoveA     $FC2          $FC
239100150209     C                   EXSR      REPKKK
239200150209     C                   MoveA     $FX           $FX2
239300150209     C*
239400150209     C     XRepKey       ENDSR
239500150209     C************************************************************
239600150209     C* REPERIMENTO TASTI DI FUNZIONE
239700150209     C************************************************************
239800150209     C     REPKKK        BEGSR
239900150209     C*
240000150209     C                   CLEAR                   $FX
240100150209     C     1             DO        24            X
240200150209     C*
240300150209     C     $FM(X)        IFNE      *BLANK
240400150209     C     $FC(X)        ANDNE     '0'
240500150209     C*
240600150209     C                   MoveL     $FM(X)        IDMSG
240700150209     C                   CALLB     'XRTVM'
240800150209     C                   PARM                    IDMSG            27
240900150209     C                   PARM                    TXTMSG          100
241000150209     C                   PARM                    ERRMSG            1
241100150209     C*
241200150209     C     ERRMSG        IFNE      *ON
241300150209     C                   MoveL     TXTMSG        $FX(X)
241400150209     C                   ELSE
241500150209     C                   MoveL     *ALL' '       $FX(X)
241600150209     C                   ENDIF
241700150209     C*
241800150209     C                   ENDIF
241900150209     C*
242000150209     C                   ENDDO
242100150209     C*
242200150209     C     XRepKKK       ENDSR
242300150209     C************************************************************
242400150209     C* INIZIALIZZAZIONE VARIABILI
242500150209     C************************************************************
242600150209     C     INZVAR        BEGSR
242700150209     C*
242800150209     C* Dati da PGM guida
242900150209     C                   MoveL     KPJBU         SDGSK62DS
243000150209     C*
243100150209     C                   MoveL     'SOC001'      TIPXSC
243200150209     C                   MoveL     SocietaK62    SOCXSC
243300150209     C                   EXSR      REPSOC
243400150209     C     RTNXSC        IFNE      '1'
243500150209     C                   MoveL     XSOCDS        SOC001
243600150209     C                   ELSE
243700150209     C                   Move      '1'           ErrK62
243800150209     C                   Move      *on           $Fine
243900150209     C                   EXSR      EndPgm
244000150209     C                   ENDIF
244100150209     C*
244200150209     C*
244300150209     C* Reperisco la libreria in cui sarà memorizzato il file
244400150209     C* che verrà creato
244500150209     C                   EXSR      RepLib
246500150209     C*
246600150209     C* Definizione dei tasti funzione utilizzabili a pgm
246700150209     C                   EXSR      DEFOPZ
246800150209     C*
246900150209     C* Variabili per gestione videate
247000150209     C                   Move      'D1'          $GEST
247100150209     C                   Move      *OFF          $FINE
247200150209     C                   Move      *Off          $INZDX
247300150209     C                   Move      *ON           $INZD1
247400150209     C                   Move      *BLANK        $LASTG
247500150209     C                   Move      *BLANK        $LASTV
247600150209     C                   Z-ADD     0             $ULKD1            3 0
247700150209     C                   Z-ADD     0             $ULKDx            3 0
247800150209     C* azzero la posizione del cursore
247900150209     C                   Z-ADD     0             H1RIGA
248000150209     C                   Z-ADD     0             H1COLO
248100150209     C*
248200150209     C* Variabili appoggio
248300150209     C                   Z-ADD     0             CURR
248400150209     C                   Z-ADD     0             CURC
248500150209     C*
248600150209     C* Valorizzazione campi univoci testate
248700150209     C                   CLEAR                   FMTT1
248800150209     C                   MoveL     KNSIF         NOMSIF
248900150209     C                   MoveL     XSCDSI        NOMDIT
249000150209     C                   MOVEL     KCDAZ         NOMAZN
249100150209     C                   MOVEL     DSPGM         NOMPGM           10
249200150209     C* recupero il sottotitolo (se c'è)
249300150209     C                   MoveL     *BLANK        D1TIT
249400150209     C     SOTTITD1      IFNE      *BLANK
249500150209     C                   MoveL     SOTTITD1      IDMSG
249600150209     C                   CALLB     'XRTVM'
249700150209     C                   PARM                    IDMSG            27
249800150209     C                   PARM                    TXTMSG          100
249900150209     C                   PARM                    ERRMSG            1
250000150209     C     ERRMSG        IFNE      *ON
250100150209     C                   MoveL     TXTMSG        D1TIT
250200150209     C                   ELSE
250300150209     C                   MoveL     *ALL'?'       D1TIT
250400150209     C                   ENDIF
250500150209     C                   ENDIF
250600150209     C* recupero il sottotitolo (se c'è)
250700150209     C                   MoveL     *BLANK        T1TIT
250800150209     C     SOTTITD2      IFNE      *BLANK
250900150209     C                   MoveL     SOTTITD2      IDMSG
251000150209     C                   CALLB     'XRTVM'
251100150209     C                   PARM                    IDMSG            27
251200150209     C                   PARM                    TXTMSG          100
251300150209     C                   PARM                    ERRMSG            1
251400150209     C                   PARM      *On           CTRMSG            1            centratura
251500150209     C                   PARM      30            LENMSG            3 0          lun output.
251600150209     C     ERRMSG        IFNE      *ON
251700150209     C                   MoveL     TXTMSG        T1TIT
251800150209     C                   ElSe
251900150209     C                   MoveL     *ALL'?'       T1TIT
252000150209     C                   EndIf
252100150209     C                   EndIf
252200150209     C* recupero il tipo operazione
252300150209     C                   Z-ADD     1             X
252400150209     C     OPZK62        LOOKUP    $OZ(X)                                 21
252500150209     C     *IN21         IFEQ      *ON
252600150209     C                   MoveL     $ID(X)        IDMSG
252700150209     C                   CALLB     'XRTVM'
252800150209     C                   PARM                    IDMSG            27
252900150209     C                   PARM                    TXTMSG          100
253000150209     C                   PARM                    ERRMSG            1
253100150209     C                   PARM      *ON           CTRMSG            1
253200150209     C                   PARM      30            LENMSG            3 0
253300150209     C     ERRMSG        IFNE      *ON
253400150209     C                   MoveL     TXTMSG        D1TIT
253500150209     C                   ELSE
253600150209     C                   MoveL     *ALL'?'       D1TIT
253700150209     C                   ENDIF
253800150209     C                   ENDIF
253900150209     C*
254000150209     C* Inizializzazione/Decodifica prima videata
254100150209     C                   EXSR      INZD1
254200150209     C                   Move      *OFF          $INZD1
254300150209     C*
254400150209     C                   ENDSR
258600150209     C************************************************************
258700150209     C* REPERIMENTO DATI SOCIETA'
258800150209     C************************************************************
258900150209     C     REPSOC        BEGSR
259000150209     C*
259001150209     C                   CALL      'SDGSOCDR'
259200150209     C                   PARM                    TIPXSC            6
259300150209     C                   PARM                    SOCXSC            3
259400150209     C                   PARM                    CDSXSC            9 0
259500150209     C                   PARM                    MODXSC            3
259600150209     C                   PARM      *BLANKS       RTNXSC            1
259700150209     C                   PARM                    XSOCDS
259800150209     C                   PARM                    KPJBA
259900150209     C*
260000150209     C                   ENDSR
260100150209     C************************************************************
260200150209     C* Rerimento libreria per scarico su supporto magnetico
260300150209     C************************************************************
260400150209     C     REPLib        BEGSR
260500150209     C*
260600150209     C                   Clear                   ANP022DS
260700150209     C*
260800150209     C                   CALLB     'XPAR'
260900150209     C                   Parm                    XScSoc
261000150209     C                   Parm      'SCARSUMA'    XPaPar            8
261100150209     C                   Parm                    XPaOut         2000
261200150209     C                   Parm      *OFF          XPaErr            1
261300150209     C                   Parm      ' '           XPaRic            1
261400150209     C*
261500150209     C                   if        XPaErr      = *On
261600150209     C                   Move      '1'           ErrK62
261700150209     C                   Move      *on           $Fine
261800150209     C                   EXSR      EndPgm
261900150209     C*
262000150209     C                   else
262100150209     C                   MoveL     XPaOut        ANP022DS
262200150209     C                   endif
262300150209     C*
262400150209     C                   ENDSR
262500150209     C/EJECT
262600150209     C************************************************************
262700150209     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
262800150209     C************************************************************
262900150209     C     DEFOPZ        BEGSR
263000150209     C*
263100150209     C* Questa routine deve valorizzare la schiera $FC1/$FR1
263200150209     C* con S o N a seconda della utilizzabilità o meno
263300150209     C* del tasto funzione correlato
263400150209     C*
263500150209     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
263600150209     C* di compilazione
263700150209     C                   RESET                   $FC1
263800150209     C                   RESET                   $FR1
263900150209     C*
264000150209     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
264100150209     C* invalido alcune opzioni
264200150209     C                   SELECT
264300150209     C* Se pgm richiamato in interrogazione disattivo :
264400150209     C     OPZK62        WHENEQ    '05'
264500150209     C* F2=Decodifica
264600150209     C                   Z-ADD     2             X
264700150209     C                   Move      '0'           $FC1(X)
264800150209     C* F4=Ricerca
264900150209     C                   Z-ADD     4             X
265000150209     C                   Move      '0'           $FC1(X)
265100150209     C* F5=Ripristino
265200150209     C                   Z-ADD     5             X
265300150209     C                   Move      '0'           $FC1(X)
265400150209     C* F6=Conferma
265500150209     C                   Z-ADD     06            X
265600150209     C                   Move      '0'           $FC1(X)
265700150209     C* Se pgm richiamato in annullamento disattivo :
265800150209     C     OPZK62        WHENEQ    '04'
265900150209     C* F2=Decodifica
266000150209     C                   Z-ADD     2             X
266100150209     C                   Move      '0'           $FC1(X)
266200150209     C* F4=Ricerca
266300150209     C                   Z-ADD     4             X
266400150209     C                   Move      '0'           $FC1(X)
266500150209     C* F5=Ripristino
266600150209     C                   Z-ADD     5             X
266700150209     C                   Move      '0'           $FC1(X)
266800150209     C*
266900150209     C                   OTHER
267000150209     C*
267100150209     C                   ENDSL
267200150209     C*
267300150209     C     XDefOpz       ENDSR
267400150209     C************************************************************
267500150209     C* DEFINIZIONE KLIST
267600150209     C************************************************************
267700150209     C     DefKlist      BEGSR
267800150209     C*
267900150209     C* klist
268000150209     C*
268100150209     C     XdefKList     ENDSR
268200150209     C************************************************************
268300150209     C* RIEMPIMENTO TASTI FUNZIONE
268400150209     C************************************************************
268500150209     C     RIEKEY        BEGSR
268600150209     C*
268700150209     C* Input :
268800150209     C* - $FX = schiera con tutti i testi disponibili
268900150209     C* - $FC = schiere con i flag di validità associati ai testi
269000150209     C* - $FR = schiere con i flag di REVERSE IMAGE
269100150209     C* - $ULK = ultimo testo caricato
269200150209     C* - $L01 = Lunghezza prima riga
269300150209     C* - $L02 = Lunghezza seconda riga
269400150209     C* Output :
269500150209     C* - $ULK
269600150209     C* - $KEY1= 1° riga di tasti funzione
269700150209     C* - $KEY2= 2° riga di tasti funzione
269800150209     C*
269900150209     C                   CALLB     'XKEYMSG'
270000150209     C* input
270100150209     C                   PARM                    $FX
270200150209     C                   PARM                    $FC
270300150209     C                   PARM                    $FR
270400150209     C                   PARM                    $ULK              3 0
270500150209     C* output
270600150209     C                   PARM                    $KEY1            79
270700150209     C                   PARM                    $KEY2            79
270800150209     C* input
270900150209     C                   PARM                    $L01              3 0
271000150209     C                   PARM                    $L02              3 0
271100150209     C*
271200150209     C                   PARM                    $ALLFUNCT       480
271300150209     C*
271400150209     C     XRieKey       ENDSR
271500150209     C************************************************************
271600150209** $OZ / $ID
27170015020901PROMSG    *LIBL     COS0001
27180015020902PROMSG    *LIBL     COS0002
27190015020903PROMSG    *LIBL     COS0003
27200015020904PROMSG    *LIBL     COS0004
27210015020905PROMSG    *LIBL     COS0005
272200150209** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
272300150209PROMSG    *LIBL     KEY0001  01
272400150209PROMSG    *LIBL     KEY0002  02
272500150209PROMSG    *LIBL     KEY0003  03
272600150209PROMSG    *LIBL     KEY0004  04
272700150209PROMSG    *LIBL     KEY0005  05
272800150209PROMSG    *LIBL     KEY0006  06
272900150209PROMSG    *LIBL     KEY0007  07
273000150209PROMSG    *LIBL     KEY0008  08
273100150209PROMSG    *LIBL     KEY0009  09
273200150209PROMSG    *LIBL     KEY0010  10
273300150209PROMSG    *LIBL     KEY0011  11
273400150209PROMSG    *LIBL     KEY0012  12
273500150209PROMSG    *LIBL     KEY0013  13
273600150209PROMSG    *LIBL     KEY0014  14
273700150209PROMSG    *LIBL     KEY0015  15
273800150209PROMSG    *LIBL     KEY0016  16
273900150209PROMSG    *LIBL     KEY0017  17
274000150209PROMSG    *LIBL     KEY0018  18
274100150209PROMSG    *LIBL     KEY0019  19
274200150209PROMSG    *LIBL     KEY0020  20
274300150209PROMSG    *LIBL     KEY0021  21
274400150209PROMSG    *LIBL     KEY0022  22
274500150209PROMSG    *LIBL     KEY0023  23
274600150209PROMSG    *LIBL     KEY0024  24
274700150209**  ABILITAZIONE D1 : $FC1 / $FR1
2748001502090N  01
2749001502090N  02
2750001502091N  03
2751001502091N  04
2752001502091N  05
2753001502090N  06
2754001502090N  07
2755001502090N  08
2756001502090N  09
2757001502090N  10
2758001502090N  11
2759001502091N  12
2760001502090N  13
2761001502090N  14
2762001502090N  15
2763001502090N  16
2764001502090N  17
2765001502090N  18
2766001502090N  19
2767001502090N  20
2768001502090N  21
2769001502090N  22
2770001502090N  23   Impostare a '1' se si utilizzano opzioni
2771001502091N  24   Lasciare sempre impostata a '1' il valore della riga 24
277200150209** TASTI DI FUNZIONE  0/3/9 : $FM2 / $FX2
277300150209PROMSG    *LIBL     KEY0001  01
277400150209PROMSG    *LIBL     KEY0002  02
277500150209PROMSG    *LIBL     KEY0003  03
277600150209PROMSG    *LIBL     KEY0004  04
277700150209PROMSG    *LIBL     KEY0005  05
277800150209PROMSG    *LIBL     KEY0006  06
277900150209PROMSG    *LIBL     KEY0007  07
278000150209PROMSG    *LIBL     KEY0008  08
278100150209PROMSG    *LIBL     KEY0009  09
278200150209PROMSG    *LIBL     KEY0010  10
278300150209PROMSG    *LIBL     KEY0011  11
278400150209PROMSG    *LIBL     KEY0012  12
278500150209PROMSG    *LIBL     KEY0013  13
278600150209PROMSG    *LIBL     KEY0014  14
278700150209PROMSG    *LIBL     KEY0015  15
278800150209PROMSG    *LIBL     KEY0016  16
278900150209PROMSG    *LIBL     KEY0017  17
279000150209PROMSG    *LIBL     KEY0018  18
279100150209PROMSG    *LIBL     KEY0019  19
279200150209PROMSG    *LIBL     KEY0020  20
279300150209PROMSG    *LIBL     KEY0021  21
279400150209PROMSG    *LIBL     KEY0022  22
279500150209PROMSG    *LIBL     KEY0023  23
279600150209PROMSG    *LIBL     KEY0024  24
279700150209**  ABILITAZIONE 0/3/9 : $FC2 / $FR2
2798001502090N  01
2799001502090N  02
2800001502091N  03
2801001502090N  04
2802001502091N  05
2803001502091N  06
2804001502090N  07
2805001502090N  08
2806001502090N  09
2807001502090N  10
2808001502090N  11
2809001502091N  12
2810001502090N  13
2811001502090N  14
2812001502090N  15
2813001502090N  16
2814001502090N  17
2815001502090N  18
2816001502090N  19
2817001502090N  20
2818001502090N  21
2819001502090N  22
2820001502090N  23   Impostare a '1' se si utilizzano opzioni
2821001502091N  24   Lasciare sempre impostata a '1' il valore della riga 24
