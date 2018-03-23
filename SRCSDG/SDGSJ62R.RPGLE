000100070907     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000200070907     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000300070907     H*PARMS CVTOPT(*NONE)
000400070907     H**********************************************************************
000500070907     H DECEDIT('0,') DATEDIT(*DMY.)
000600070907     H**********************************************************************
000700070907      *
000800100730      *         ------------------------------------------------
000900150105      *                Comunicazione Dichiarazione d'intento
001000100730      *                   Manutenzione file telematico
001100150105      *                        Tipi Recod A Z
001200100730      *         ------------------------------------------------
001300100730      *
001400070907     F**********************************************************************
001500150105     FSDGDIW00F UF   E             DISK    USROPN
001600070907     F                                     INFDS(RRNPFDS)                       *
001700070907     F*------------
001800150105     FSDGSJ62D  CF   E             WORKSTN
001900150105     F                                     RENAME(SJ62D1:FMTD1)
002000150105     F                                     RENAME(SJ62DA1:FMTDA1)
002100150105     F                                     RENAME(SJ62DA2:FMTDA2)
002200150105     F                                     RENAME(SJ62DA3:FMTDA3)
002300150105     F                                     RENAME(SJ62DZ1:FMTDZ1)
002400150105     F                                     RENAME(SJ62T1:FMTT1)
002500150105     F                                     RENAME(SJ62Z1:FMTZ1)
002600150105     F                                     RENAME(SJ62Z2:FMTZ2)
002700070907     F                                     INFDS(DSFMT)
002800070907     D**********************************************************************
002900070907     D* numero tipo operaz
003000070907     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003100070907     D $ID             S             27    DIM(5) ALT($OZ)
003200070907     D*----------------------------------------------------*
003300070907     D*Schiere tasti funzione generiche
003400070907     D* percorso IdMsg
003500070907     D $FM             S             27    DIM(24)
003600070907     D*  testo msg
003700070907     D $FX             S             50    DIM(24)
003800070907     D* abilitazione ('0'/'1')
003900070907     D $FC             S              1    DIM(24)
004000070907     D*  tipo abilitazione ('S'/'N')
004100070907     D $FR             S              1    DIM(24)
004200070907     D*----------------------------------------------------*
004300070907     D* Tasti funzione D1
004400070907     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004500070907     D $FX1            S             50    DIM(24) ALT($FM1)
004600070907     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004700070907     D $FR1            S              1    DIM(24) ALT($FC1)
004800070907     D*----------------------------------------------------*
004900070907     D* Tasti funzione A1-2-3/B1-2-3-4/Z1-2-3
005000070907     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005100070907     D $FX2            S             50    DIM(24) ALT($FM2)
005200070907     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005300070907     D $FR2            S              1    DIM(24) ALT($FC2)
005400070907     D*----------------------------------------------------*
005500070907     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005600070907     D* funzione personalizzati
005700070907     D $PGF1           S             10    DIM(25)
005800070907     D*----------------------------------------------------*
005900070907     D* dimensioni variabili che conterranno i tasti D1
006000070907     D $D1L01          S              3  0
006100070907     D $D1L02          S              3  0
006200070907     D* dimensioni variabili che conterranno i tasti An/Bn/Zn
006300070907     D $DxL01          S              3  0
006400070907     D $DxL02          S              3  0
006500070907     D*----------------------------------------------------*
006600070907    >D* Inserire qui la definizione di altre variabili per reperire
006700070907    >D* la lunghezza di altre videate che usano un piede diverso da
006800070907    >D* STD
006900070907     D*----------------------------------------------------*
007000070907     D* Passaggio Parametri
007100070907     D KPJBA         E DS
007200070907     D*----------------------------------------------------*
007300070907     D* DS Esterna gestione messaggi
007400100820     D SDGMSGDS      E DS                  INZ
007500070907     D  STS          E                     EXTFLD(MSGSTS)
007600070907     D                                     DIM(333)
007700070907     D  JBA          E                     EXTFLD(MSGJBA)
007800070907     D                                     DIM(502)
007900070907     D*----------------------------------------------------*
008000070907     D* Reperimento Autorizzazioni
008100070907     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
008200070907     D*-------------
008300070907     D* Dati per pgm XNOAUT
008400070907     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
008500070907     D*------
008600070907     D* Passaggio Autorizzazioni
008700070907     D DSAUT           DS           512
008800100730     D*----------------------------------------------------*
008900070907     D* Maschera dati autorizzazioni
009000070907    >D CONCIVDS      E DS
009100070907     D*----------------------------------------------------*
009200070907     D* Reperimento nome PGM
009300070907     D STATUS         SDS           333
009400070907     D  DSPGM            *PROC
009500070907     D  PARMS            *PARMS
009600070907     D*----------------------------------------------------*
009700070907     D* Dati di ambiente ottenuti da XSOC
009800070907     D SOC001        E DS                  EXTNAME(XSOC001DS)
009900070907     D*-------------
010000070907     D* DS Interna per dati di output di XSOC
010100070907     D XSOCDS          DS          1000
010200070907     D*----------------------------------------------------*
010300070907     D* Passaggio Parametri
010400150105     D SDGSJ62DS     E DS
010500070907     D*-------------
010600070907     D* Catalogo file per scarico su supporto magnetico
010700070907     D ANA122DS      E DS
010800070907     D*-------------
010900070907     D* Parametri modulo SCARSUMA
011000070907     D ANP022DS      E DS
011100070907     D*----------------------------------------------------*
011200070907     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011300070907     D* alla funzione personalizzata
011400150105     D $SaveDS         S                   like(SDGSJ62DS)
011500070907     D*----------------------------------------------------*
011600070907     D DSFMT           DS
011700070907     D  $TASTO               369    369
011800070907     D  NRG                  370    370
011900070907     D  NCL                  371    371
012000070907     D*----------------------------------------------------*
012100070907     D* DS per UserExit
012200070907     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
012300070907     D*----------------------------------------------------*
012400070907     D* posizione cursore
012500070907     D CURSOR          DS
012600070907     D  RIRI                   1      2B 0 INZ
012700070907     D  R$$                    2      2
012800070907     D  COCO                   3      4B 0 INZ
012900070907     D  C$$                    4      4
013000070907     D*----------------------------------------------------*
013100070907     D* Costanti
013200070907     D*----------------------------------------------------*
013300070907     D* Tasti di funzione
013400070907     D F01             C                   CONST(X'31')
013500070907     D F02             C                   CONST(X'32')
013600070907     D F03             C                   CONST(X'33')
013700070907     D F04             C                   CONST(X'34')
013800070907     D F05             C                   CONST(X'35')
013900070907     D F06             C                   CONST(X'36')
014000070907     D F07             C                   CONST(X'37')
014100070907     D F08             C                   CONST(X'38')
014200070907     D F09             C                   CONST(X'39')
014300070907     D F10             C                   CONST(X'3A')
014400070907     D F11             C                   CONST(X'3B')
014500070907     D F12             C                   CONST(X'3C')
014600070907     D F13             C                   CONST(X'B1')
014700070907     D F14             C                   CONST(X'B2')
014800070907     D F15             C                   CONST(X'B3')
014900070907     D F16             C                   CONST(X'B4')
015000070907     D F17             C                   CONST(X'B5')
015100070907     D F18             C                   CONST(X'B6')
015200070907     D F19             C                   CONST(X'B7')
015300070907     D F20             C                   CONST(X'B8')
015400070907     D F21             C                   CONST(X'B9')
015500070907     D F22             C                   CONST(X'BA')
015600070907     D F23             C                   CONST(X'BB')
015700070907     D F24             C                   CONST(X'BC')
015800070907     D ENTER           C                   CONST(X'F1')
015900070907     D ROLDWN          C                   CONST(X'F4')
016000070907     D ROLLUP          C                   CONST(X'F5')
016100070907     D*----------------------------------------------------*
016200070907     D* Sottotitolo di testata
016300100803     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
016400100803     D                                          SDC0058')
016500100803     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
016600150105     D                                          SDC0082')
016700070907     D*----------------------------------------------------*
016800070907     D* Variabili per gestione videate
016900070907     D $Gest           S              2A
017000070907     D $Fine           S              1A
017100070907     D $InzDX          S              1A
017200070907     D $InzD1          S              1A
017300070907     D $LastG          S              2A
017400070907     D $LastV          S              2A
017500070907     D*----------------------------------------------------*
017600070907     D* Variabili appoggio sempre presenti in tutte le anagrafiche
017700070907     D CurR            S              2S 0
017800070907     D CurC            S              2S 0
017900070907     D Win             S             99A
018000070907     D*----------------------------------------------------*
018100070907     D* Variabili appoggio specifiche della singola anagrafica
018200070907     D*----------------------------------------------------*
018300070907     D*  Indici
018400070907     D X               S              3S 0
018500070907     D Y               S              3S 0
018600070907     D*----------------------------------------------------*
018700070907     D* Indice della schiera tasti funzione
018800070907     D PosFun          S              2  0
018900070907     D*----------------------------------------------------*
019000150105     D* OpzJ62 usato in parzializzazione
019100150105     D* e WOpzJ62 valorizzato dalla parzializzazione per manutenzione
019200150105     D WOpzJ62         S                   like(OpzJ62)
019300070907     D*----------------------------------------------------*
019400070907     D $RecOk          S              1a
019500070907     D $RecFound       S              1a
019600070907     D $RecAlloc       S              1a
019700070907     D*----------------------------------------------------*
019800150105     D* Nr rec SDGDIW00F
019900070907     D  RrnPfDs        DS           512                                         *
020000070907     D   RrnPf               397    400B 0                                      *
020100100730     D*------------------
020200130909     D* Tipo record "A"
020300100730     D*------------------
020400130909     D TpRecADS        DS
020500111013     D* prima mappa
020600130911     D  GA001010
020700130911     D  GA00101
020800130911     D  GA00201
020900130911     D  GA00301
021000130911     D  GA00401
021100130911     D  GA00701
021200130911     D  GA00702
021300130911     D  GA00703
021400130911     D  GA00704
021500130911     D  GA00705
021600130911     D  GA00706
021700130911     D  GA00707
021800130911     D  GA00801
021900130911     D  GA00901
022000130911     D  GA01001
022100130911     D  GA01002
022200110811     D*----------
022300111013     D* seconda mappa
022400130911     D  GA01101
022500130911     D  GA01102
022600130911     D  GA01103
022700130911     D  GA01104
022800130911     D  GA01105
022900130911     D  GA01106
023000130911     D  GA01107
023100130911     D  GA01108
023200130911     D  GA01119
023300130911     D  GA01110
023400150105     D  GA01111
023500150105     D  GA01112
023600150105     D  GA01113
023700150105     D  GA01114
023800150105     D  GA01115
023900130912     D*----------
024000130912     D* terza mappa
024100150105     D  GA01116
024200150105     D  GA01201
024300150105     D  GA01202
024400150105     D  GA01203
024500150105     D  GA01301
024600150105     D  GA01401
024700100730     D*-----------------
024800130909     D* Tipo record "Z"
024900100730     D*-----------------
025000130909     D TpRecZDS        DS
025100111013     D* prima mappa
025200130911     D  GZ00101
025300130911     D  GZ00201
025400130911     D  GZ00301
025500130911     D  GZ00401
025600130911     D  GZ00501
025700130911     D  GZ00601
025800130911     D  GZ00701
025900100730     D*------------------
026000070907     D*----------------------------------------------------------------
026100070907     D* Ds contenente i valori del msg PRO1797
026200070907     D §Fil_Lib        DS
026300070907     D  §File                        10A
026400070907     D  §Libreria                    10A
026500070907     C******************************************************
026600070907     C*                MAIN LINE PROGRAM                   *
026700070907     C******************************************************
026800070907     C*
026900070907     C* inizializzazione variabili
027000070907     C                   EXSR      INZVAR
027100070907     C*
027200070907     C     $FINE         DOWEQ     *OFF
027300070907     C     $GEST         CASEQ     'D1'          GESD1
027400070907     C     $GEST         CASEQ     'DX'          GESDX
027500100730     C     $GEST         CASEQ     'A1'          GESDA1
027600100730     C     $GEST         CASEQ     'A2'          GESDA2
027700130912     C     $GEST         CASEQ     'A3'          GESDA3
027800130912     C     $GEST         CASEQ     'Z1'          GESDZ1
027900070907     C                   ENDCS
028000070907     C                   ENDDO
028100070907     C*
028200070907     C* fine programma
028300070907     C                   EXSR      ENDPGM
028400070907     C************************************************************
028500070907     C* Fine Programma
028600070907     C************************************************************
028700070907     C     ENDPGM        BEGSR
028800070907     C*
028900070907     C* disallocazione rcd
029000150105     C                   UNLOCK    SDGDIW00F                            22
029100070907     C*
029200070907     C* passaggio dati al pgm chiamante
029300150105     C                   MoveL     SDGSJ62DS     KPJBU
029400070907     C*
029500070907     C* Se pgm Guida non è stato richiamato posso chiudere in LR
029600150105     C                   if        OpzJ62 = *blank
029700070907     C                   eval      *inLR = *on
029800070907     C* altrimenti chiudo in RT
029900070907     C                   else
030000070907     C                   eval      *inRT = *on
030100070907     C                   endif
030200070907     C*
030300070907     C                   RETURN
030400070907     C*
030500070907     C*
030600070907     C     XEndPgm       ENDSR
030700070907     C************************************************************
030800070907     C* GESTIONE EMISSIONE TASTI FUNZIONE
030900070907     C************************************************************
031000070907     C     GESPIE        BEGSR
031100070907     C*
031200070907     C                   SELECT
031300070907     C*
031400070907     C     $GEST         WHENEQ    'D1'
031500070907     C     $ULKD1        IFEQ      0
031600070907     C                   EXSR      F24D1
031700070907     C                   ELSE
031800070907     C                   WRITE     FMTZ1
031900070907     C                   ENDIF
032000070907     C*
032100070907     C*
032200100803     C     $GEST         WHENEQ    'A1'
032300100803     C     $GEST         oreq      'A2'
032400100803     C     $GEST         oreq      'A3'
032500100803     C     $GEST         oreq      'B1'
032600100803     C     $GEST         oreq      'B2'
032700100803     C     $GEST         oreq      'B3'
032800130912     C     $GEST         oreq      'E1'
032900100803     C     $GEST         oreq      'Z1'
033000100803     C     $GEST         oreq      'Z2'
033100100803     C     $GEST         oreq      'Z3'
033200070907     C     $ULKDx        IFEQ      0
033300070907     C                   EXSR      F24Dx
033400070907     C                   ELSE
033500070907     C                   WRITE     FMTZ2
033600070907     C                   ENDIF
033700070907     C*
033800070907     C                   ENDSL
033900070907     C*
034000070907     C     XGesPie       ENDSR
034100070907     C************************************************************
034200070907     C* GESTIONE PARZIALIZZAZIONI
034300070907     C************************************************************
034400070907     C     GESD1         BEGSR
034500070907     C* inizializzazione videata
034600070907     C     $INZD1        IFEQ      *ON
034700070907     C                   EXSR      INZD1
034800070907     C                   Move      *OFF          $INZD1
034900070907     C                   ENDIF
035000070907     C* emissione piede videata se proveniente da altra
035100070907     C* salvataggio valore formato di provenienza
035200070907     C     $LASTV        IFNE      'D1'
035300070907     C                   WRITE     FMTT1
035400070907     C                   EXSR      GESPIE
035500070907     C                   Move      $LASTV        $LASTG
035600070907     C                   Move      'D1'          $LASTV
035700070907     C                   ENDIF
035800070907     C*
035900070907     C* se esistono errori sulla videata
036000070907     C* emetto la write del formato a indicatori spenti per vedere
036100070907     C* le eventuali decodifiche
036200070907     C     *IN99         IFEQ      *ON
036300070907     C                   MoveA     *IN           WIN
036400070907     C                   Move      *ALL'0'       IN5098           49
036500070907     C                   MoveA     IN5098        *IN(50)
036600070907     C                   WRITE     FMTD1
036700070907     C                   MoveA     WIN           *IN
036800070907     C                   ENDIF
036900070907     C*
037000070907     C                   EXFMT     FMTD1
037100070907     C*
037200070907     C* azzero la posizione del cursore
037300070907     C                   Z-ADD     0             H1RIGA
037400070907     C                   Z-ADD     0             H1COLO
037500070907     C*
037600070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
037700070907     C* INPUT
037800070907     C* $FC1 = Abilitazioni videata D1
037900070907     C* OUTPUT
038000070907     C* *IN97 + *IN99 Errori
038100070907     C                   MoveA     $FC1          $FC
038200070907     C                   EXSR      CTRKD1
038300070907     C*
038400070907     C     *IN99         IFEQ      *OFF
038500070907     C*
038600070907     C                   SELECT
038700070907     C* F2=Decodifica
038800070907     C     $TASTO        WHENEQ    F02
038900070907     C                   EXSR      F02D1
039000070907     C* F3=Fine
039100070907     C     $TASTO        WHENEQ    F03
039200070907     C                   EXSR      F03D1
039300070907     C* F4=Ricerca
039400070907     C     $TASTO        WHENEQ    F04
039500070907     C                   EXSR      SEARCH
039600070907     C* F5=Refresh
039700070907     C     $TASTO        WHENEQ    F05
039800070907     C                   EXSR      F05D1
039900070907     C* F12=Ritorno
040000070907     C     $TASTO        WHENEQ    F12
040100070907     C                   EXSR      F12D1
040200070907     C* F24=Altri tasti
040300070907     C     $TASTO        WHENEQ    F24
040400070907     C                   EXSR      F24D1
040500070907     C*
040600070907     C                   OTHER
040700070907     C                   EXSR      CTRD1
040800070907     C*
040900070907     C* richiamo dettaglio se non ci sono errori in parzializzazione
041000070907     C                   if        *in99 = *off
041100070907     C                   exsr      SetAna
041200070907     C                   exsr      GesDx
041300070907     C                   endif
041400070907     C*
041500070907     C                   ENDSL
041600070907     C*
041700070907     C                   ENDIF
041800070907     C*
041900070907     C     XGesD1        ENDSR
042000070907     C************************************************************
042100070907     C* RICERCHE
042200070907     C************************************************************
042300070907     C     SEARCH        BEGSR
042400070907     C*
042500150105     C* determino Riga/Colonna del cursore
042600070907     C                   Move      NRG           R$$
042700070907     C                   Move      NCL           C$$
042800070907     C                   Z-ADD     RIRI          CURR
042900070907     C                   Z-ADD     COCO          CURC
043000070907     C*
043100070907     C*
043200070907     C                   SELECT
043300070907     C*
043400070907    >C*--------------------------------------------------------------
043500070907    >C* inserire le routine di ricerca
043600070907    >C* RICORDATE: per tutti i campi che in certe condizioni risultano PR,
043700070907    >C* NON deve essere possibile richiamare i rispettivi pgm in ricerca,
043800070907    >C* SEMMAI richiamarli in visualizzazione (opzione=05)
043900070907    >C*--------------------------------------------------------------
044000070907     C*
044100070907     C* FMTD1  - file su supporto magnetico
044200070907     C     H1NMFL        WHENEQ    'D1FILE'
044300070907     C                   Reset                   ANA122DS
044400070907     C                   Move      '01'          Opz122
044500070907     C                   Move      XScSoc        Societa122
044600070907     C                   MoveL     XScSoc        Soc122
044700150105     C                   Move      'DF'          TpScar122
044800070907     C                   Clear                   KPJBU
044900070907     C                   Movel     ANA122DS      KPJBU
045000070907     C                   Call      'ANA122R'
045100070907     C                   Parm                    KPJBA
045200070907     C                   Movel     KPJBU         ANA122DS
045300070907     C*
045400070907     C                   if        File122  <>   *Blanks
045500070907     C                   MoveL     File122       D1File
045600070907     C                   endif
045700070907     C*
045800070907     C* ricerca non abilitata in questa posizione
045900070907   X1C                   OTHER
046000070907     C                   SETON                                        9899
046100070907     C*
046200070907   E1C                   ENDSL
046300070907     C*
046400070907     C* imposto pos. del cursore
046500070907     C                   Z-ADD     CURR          H1RIGA
046600070907     C                   Z-ADD     CURC          H1COLO
046700070907     C*
046800070907     C                   Move      *HIVAL        $LASTV
046900070907     C*
047000070907     C                   ENDSR
047100070907     C/EJECT
047200070907     C************************************************************
047300070907     C* Carico e passo alle videate da manutenere
047400070907     C************************************************************
047500111220     C     GesDx         BEGSR
047600070907     C*
047700150105     C                   Move      '02'          WOpzJ62
047800070907     C*
047900070907     C                   Move      *on           $InzDx
048000070907     C                   exsr      InzDx
048100070907     C                   Move      *off          $InzDx
048200070907     C*
048300070907     C*  se reperito rec richiesto
048400070907     C                   if        $RecOk      = *On
048500070907     C*
048600070907     C*  passo a dettaglio richiesto
048700070907     C                   select
048800130912     C                   when      D1TpRec='A'
048900100730     C                   Move      'A1'          $Gest
049000130912     C                   when      D1TpRec='Z'
049100130912     C                   Move      'Z1'          $Gest
049200070907     C                   endsl
049300070907     C*
049400070907     C                   else
049500070907     C                   Move      'D1'          $Gest
049600070907     C*
049700070907     C                   endif
049800070907     C*
049900070907     C     XGesDx        ENDSR
050000070907     C************************************************************
050100070907     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
050200070907     C************************************************************
050300070907     C     INZD1         BEGSR
050400070907     C*
050500070907     C                   CLEAR                   FMTD1
050600070907     C*
050700070907     C* Se pgm  richiamato con parametri forniti , li imposto nelle
050800070907     C* parzializzazioni
050900070907     C                   select
051000070907     C* il pgm è richiamato e
051100070907     C* sono stati passati dei valori di parzializzazione ma non
051200070907     C* la chiave univoca
051300150105     C                   when      OpzJ62 <> *blank
051400070907     C* imposto i valori passati sui campi a video
051500150105     C                   Move      TpReCJ62      D1TpRec
051600070907     C* il pgm non è richiamato
051700150105     C                   when      OpzJ62 =  *blank
051800070907     C* imposto dei valori di default
051900070907     C                   Move      *blank        D1TpRec
052000070907     C                   endsl
052100070907     C*
052200070907     C     XInzD1        ENDSR
052300070907     C************************************************************
052400070907     C* GESTIONE F02 VIDEO D1
052500070907     C************************************************************
052600070907     C     F02D1         BEGSR
052700070907     C*
052800070907     C* per decodificare la videata richiamo la routine dei ctrl
052900070907     C* e poi disattivo gli eventuali messaggi di errore
053000070907     C                   EXSR      CTRD1
053100070907     C*
053200070907     C                   Move      *ALL'0'       IN5098
053300070907     C                   MoveA     IN5098        *IN(50)
053400070907     C*
053500070907     C     XF02D1        ENDSR
053600070907     C************************************************************
053700070907     C* GESTIONE F03 VIDEO D1
053800070907     C************************************************************
053900070907     C     F03D1         BEGSR
054000070907     C*
054100070907     C                   Move      *ON           $FINE
054200150105     C                   Move      '1'           RETJ62
054300070907     C* fine programma
054400070907     C                   EXSR      ENDPGM
054500070907     C*
054600070907     C     XF03D1        ENDSR
054700070907     C************************************************************
054800070907     C* GESTIONE F05 VIDEO D1
054900070907     C************************************************************
055000070907     C     F05D1         BEGSR
055100070907     C*
055200070907     C                   Move      *ON           $INZD1
055300070907     C*
055400070907     C     XF05D1        ENDSR
055500070907     C************************************************************
055600070907     C* GESTIONE F12 VIDEO D1
055700070907     C************************************************************
055800070907     C     F12D1         BEGSR
055900070907     C*
056000070907     C*  torno a menu
056100070907     C                   Move      *ON           $FINE
056200150105     C                   Move      '2'           RETJ62
056300070907     C*
056400070907     C     XF12D1        ENDSR
056500070907     C************************************************************
056600070907     C* GESTIONE F24 VIDEO D1
056700070907     C* input
056800070907     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
056900070907     C* $FX1 =Testi tasti funzione
057000070907     C* $FC1 = Abilitazioni
057100070907     C* $FR1 = Inversioni di fondo
057200070907     C************************************************************
057300070907     C     F24D1         BEGSR
057400070907     C*
057500070907     C                   MoveA     $FX1          $FX
057600070907     C                   MoveA     $FC1          $FC
057700070907     C                   MoveA     $FR1          $FR
057800070907     C                   Z-ADD     $ULKD1        $ULK
057900070907     C                   Z-ADD     $D1L01        $L01
058000070907     C                   Z-ADD     $D1L02        $L02
058100070907     C                   EXSR      RIEKEY
058200070907     C                   Z-ADD     $ULK          $ULKD1            3 0
058300070907     C                   MoveA     $FC           $FC1
058400070907     C                   MoveL     $KEY1         Z1KE1
058500070907     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
058600070907     C                   MoveL     $KEY2         Z1KE2
058700070907     C                   WRITE     FMTZ1
058800070907     C*
058900070907     C     XF24D1        ENDSR
059000070907     C************************************************************
059100070907     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
059200070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
059300070907     C* 93 94 96 97 98
059400070907     C************************************************************
059500070907     C     CTRD1         BEGSR
059600070907     C*
059700070907     C                   SETOFF                                       99
059800070907     C                   if        D1Tprec = *blank
059900070907     C                   eval      *in99 = *on
060000070907     C                   endif
060100070907     C*
060200070907     C* file da trasmettere
060300070907     C                   EXSR      CtrFile
060400070907     C*
060500070907     C     XCtrD1        ENDSR
060600070907     C***********************************************************
060700070907     C* Controllo file da trasmettere
060800070907     C***********************************************************
060900070907     C     CtrFile       BEGSR
061000070907     C*
061100070907     C                   if        D1File     = *Blank
061200070907     C                   SetOn                                        5199
061300070907     C                   else
061400070907     C* verifico se esiste, nella libreria
061500070907     C* dei file per lo scarico su supporto magnetico,
061600070907     C* il file indicato a video
061700100730     C                   EXSR      ChkFile
061800070907     C*
061900070907     C                   endif
062000070907     C*
062100070907     C                   ENDSR
062200070907     C/EJECT
062300070907     C************************************************************
062400100730     C* Verifico l'esistenza del file
062500070907     C************************************************************
062600100730     C     ChkFile       BEGSR
062700070907     C*
062800150105     C                   CLOSE     SDGDIW00F
062900070907     C*
063000070907     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
063100070907     C                   Eval      Cmd         =
063200070907     C                             'CHKOBJ     OBJ('        +
063300070907     C                             %Trim(Lib022) + '/' +
063400070907     C                             %Trim(D1File) +
063500070907     C                             ') OBJTYPE(*FILE)'
063600070907     C*
063700070907     C                   Call      'QCMDEXC'                            21
063800070907     C                   Parm                    Cmd             120
063900070907     C                   Parm      120           Len              15 5
064000070907     C*
064100070907     C                   if        *In21       = *On
064200070907     C*
064300070907     C* Errore: dico che non il file non è stato trovato nella libreria
064400070907     C* Valorizzo le variabili del msg PRO1797
064500070907     C                   Move      D1File        §File
064600070907     C                   Move      Lib022        §Libreria
064700070907     C                   Move      §Fil_Lib      P1Fil_Lib
064800070907     C                   SetOn                                        5099
064900070907     C*
065000070907     C                   else
065100070907     C*
065200070907     C* sovrascrivo il membro con il nome del file
065300100730     C                   EXSR      OvrFile
065400150105     C                   OPEN      SDGDIW00F
065500070907     C                   endif
065600070907     C*
065700070907     C*
065800070907     C                   ENDSR
065900070907     C/Eject
066000070907     C************************************************************
066100070907     C* Sovrascrivo il membro con il nome del file
066200070907     C************************************************************
066300100730     C     OvrFile       BEGSR
066400070907     C*
066500070907     C                   Eval      Cmd         =
066600150105     C                             'OVRDBF     FILE(SDGDIW00F) '+
066700070907     C                             'TOFILE('                    +
066800070907     C                             %Trim(Lib022) + '/' +
066900070907     C                             %Trim(D1File) +
067000070907     C                             ')'
067100070907     C*
067200070907     C                   Call      'QCMDEXC'                            21
067300070907     C                   Parm                    Cmd             120
067400070907     C                   Parm      120           Len              15 5
067500070907     C*
067600070907     C                   if        *In21       = *On
067700150105     C                   Move      '1'           ERRJ62
067800070907     C                   Move      *On           $Fine
067900070907     C                   EXSR      ENDPGM
068000070907     C                   endif
068100070907     C*
068200070907     C                   ENDSR
068300070907     C/Eject
068400070907     C************************************************************
068500070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
068600070907     C************************************************************
068700070907     C     CTRKD1        BEGSR
068800070907     C*
068900070907     C                   SETOFF                                       99
069000070907     C                   MoveA     $FC1          $FC
069100070907     C                   EXSR      CTRKEY
069200070907     C     *IN99         IFEQ      *ON
069300070907     C                   SETON                                        97
069400070907     C                   ENDIF
069500070907     C*
069600070907     C     XCtrKD1       ENDSR
069700070907     C************************************************************
069800070907     C* Chiamata programma con tasto personalizzato
069900070907     C************************************************************
070000070907     C     FPerDx        BEGSR
070100070907     C*
070200070907     C* salvo la DS di ingresso
070300150105     C                   MoveL     SDGSJ62DS     $SaveDS
070400070907     C*
070500150105     C                   Move      xscsoc        SocietaJ62
070600150105     C* passare nei campi appositi della DS STDJ62DS il valore
070700070907     C* della chiave della VL univoca
070800150105     C                   MoveL     D1TpRec       TpReCJ62
070900070907     C*
071000150105     C                   MoveL     SDGSJ62DS     KPJBU
071100070907     C                   CALL      $PGF1(PosFun)                        22
071200070907     C                   PARM                    KPJBA
071300100730     C***                PARM                    NOMPGM
071400070907     C*
071500070907     C* ripristino la DS di ingresso
071600150105     C                   MoveL     $SaveDS       SDGSJ62DS
071700070907     C*
071800070907     C     XFPerDx       ENDSR
071900070907     C************************************************************
072000070907     C* CONTROLLO ABILITAZIONE TASTI VIDEATE
072100070907     C************************************************************
072200070907     C     CTRKDx        BEGSR
072300070907     C*
072400070907     C                   SETOFF                                       99
072500070907     C                   MoveA     $FC2          $FC
072600070907     C                   EXSR      CTRKEY
072700070907     C     *IN99         IFEQ      *ON
072800070907     C                   SETON                                        97
072900070907     C                   ENDIF
073000070907     C*
073100070907     C     XCtrKDx       ENDSR
073200070907     C************************************************************
073300070907     C* INIZIALIZZAZIONE VIDEATA DATI
073400070907     C************************************************************
073500070907     C     INZDx         BEGSR
073600070907     C*
073700070907     C* In base al tipo record scelto, utilizzo diverse videate:
073800070907     C                   select
073900100730     C                   when      D1TpRec='A'
074000130912     C                   clear                   FmtDA1
074100130912     C                   clear                   FmtDA2
074200130912     C                   clear                   FmtDA3
074300130912     C                   clear                   FmtDZ1
074400130912     C                   clear                   FmtDZ1
074500070907     C                   endsl
074600070907     C*
074700070907     C* NON IMMISSIONE
074800150105     C                   if        WOpzJ62 <> '01'
074900070907     C*
075000070907     C*  LETTURA
075100070907     C                   exsr      RedAna
075200070907     C*
075300070907     C*  DOPO LETTURA
075400070907     C                   select
075500070907     C*   NON TROVATO tipo rec richiesto oppure allocato
075600070907     C                   when      $RecOk = *off
075700070907     C                   if        $RecFound   = *Off    or
075800070907     C                             $RecAlloc   = *On
075900070907     C                   Move      'D1'          $GEST
076000150105     C                   Move      '1'           ERRJ62
076100070907     C*    errore da emettere in parzializzaione
076200070907     C*    PRO0007-(F1) La funzione richiamata si è chiusa in errore
076300070907     C                   eval      *in93 = *on
076400070907     C                   eval      *in99 = *on
076500070907     C                   endif
076600070907     C*
076700070907     C*   TROVATO tp rec da gestire
076800070907     C                   when      $RecOk = *on
076900070907     C*
077000070907     C*    riempio videata tp rec richiesto
077100070907     C*    e richiamo ctrl per decodifica
077200070907     C                   select
077300130912     C                   when      D1TpRec='A'
077400150105     C                   eval      TPRECADS = DIWREC
077500100730     C                   exsr      CtrDA1
077600100730     C                   exsr      CtrDA2
077700130912     C                   exsr      CtrDA3
077800130912     C                   when      D1TpRec='Z'
077900150105     C                   eval      TPRECZDS = DIWREC
078000130912     C                   exsr      CtrDZ1
078100130912     C                   endsl
078200070907     C                   Move      *all'0'       In5098
078300070907     C                   Movea     In5098        *in(50)
078400070907     C*
078500070907     C                   endsl
078600070907     C*
078700070907     C                   endif
078800070907     C*
078900070907     C     XInzDX        ENDSR
079000070907     C************************************************************
079100070907     C* posizionamento sul file
079200070907     C************************************************************
079300070907     C     SetAna        BEGSR
079400070907     C*
079500070907     C                   eval      $RecFound   = *Off
079600070907     C*
079700150105     C     1             setll     SDGDIW00F
079800070907     C*
079900070907     C                   ENDSR
080000070907     C************************************************************
080100070907     C* Gegstione elttura rec da caricare per modifica
080200070907     C************************************************************
080300070907     C     RedAna        BEGSR
080400070907     C*
080500070907     C* REPERISCO NRR DA LEGGERE
080600070907     C*  cerco il n°relativo rec contenente il tipo rec richiesto
080700070907     C                   eval      $RecOk = *off
080800070907     C                   eval      $RecAlloc   = *Off
080900070907     C*
081000150105     C                   read(n)   SDGDIW00F                              21
081100070907     C                   dow       *in21 = *off
081200070907     C*
081300150105     C                   if        %subst(DIWREC: 1 : 1)=D1TpRec
081400070907     C                   eval      $RecOk = *on
081500070907     C                   eval      $RecFound   = *On
081600070907     C                   leave
081700070907     C                   endif
081800070907     C*
081900150105     C                   read(n)   SDGDIW00F                              21
082000070907     C                   enddo
082100070907     C*
082200070907     C*
082300070907     C* LETTURA
082400070907     C*  trovato tipo rec richiesto
082500070907     C                   if        $RecOk = *on
082600070907     C*
082700070907     C*  provo a leggere il rec fino a quando il rec non è allocato
082800070907     C*  oppure l'utente vuole Ritentare a leggere il rec allocato
082900070907     C                   dou       *in22 = *off
083000070907     C                             or MsgRtn <> 'R'
083100070907     C*  alloco se Modifica/Annullamento
083200150105    >C                   if        WOpzJ62='02' or WOpzJ62='04'
083300150105     C     RrnPf         chain     SDGDIW00F                          2122
083400070907     C*  NON alloco se Copia/Visualizzazione
083500070907     C                   else
083600150105     C     RrnPf         chain(n)  SDGDIW00F                          2122
083700070907     C                   endif
083800070907     C*
083900070907     C*  Gestione RCD allocato
084000070907     C                   if        *in22 = *on
084100070907     C                   eval      $RecAlloc   = *On
084200150105    >C                   Movel     'SDGDIW00F'   MsgFil
084300070907     C                   exsr      MsgPgm
084400070907     C                   endif
084500070907     C*
084600070907     C                   enddo
084700070907     C*
084800070907     C* rcd allocato da altro pgm: memorizzo errore
084900070907     C                   if        *in22 = *on
085000070907     C                   eval      $RecOk = *off
085100070907     C                   endif
085200070907     C*
085300070907     C                   endif
085400070907     C*
085500070907     C     XRedAna       ENDSR
085600070907     C************************************************************
085700070907     C* GESTIONE F24 VIDEI
085800070907     C************************************************************
085900070907     C     F24Dx         BEGSR
086000070907     C*
086100070907     C                   MoveA     $FX2          $FX
086200070907     C                   MoveA     $FC2          $FC
086300070907     C                   MoveA     $FR2          $FR
086400070907     C                   Z-ADD     $ULKDx        $ULK
086500070907     C                   Z-ADD     $DxL01        $L01
086600070907     C                   Z-ADD     $DxL02        $L02
086700070907     C                   EXSR      RIEKEY
086800070907     C                   Z-ADD     $ULK          $ULKDx            3 0
086900070907     C                   MoveA     $FC           $FC2
087000070907     C                   MoveL     $KEY1         Z2KE1
087100070907     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
087200070907     C                   MoveL     $KEY2         Z2KE2
087300070907     C                   WRITE     FMTZ2
087400070907     C*
087500070907     C     XF24Dx        ENDSR
087600070907     C************************************************************
087700070907     C* GESTIONE VIDEO RECORD D01
087800070907     C************************************************************
087900100730     C     GESDA1        BEGSR
088000070907     C* inizializzazione videata
088100070907     C     $INZDX        IFEQ      *ON
088200070907     C                   EXSR      INZDX
088300070907     C                   Move      *OFF          $INZDX
088400070907     C                   ENDIF
088500070907     C* emissione piede videata se proveniente da altra
088600070907     C* salvataggio valore formato di provenienza
088700070907     C     $LASTV        IFNE      '01'
088800070907     C                   WRITE     FMTT1
088900070907     C                   EXSR      GESPIE
089000070907     C                   Move      $LASTV        $LASTG
089100070907     C                   Move      '01'          $LASTV
089200070907     C                   ENDIF
089300070907     C*
089400070907     C* se esistono errori sulla videata
089500070907     C* emetto la write del formato a indicatori spenti per vedere
089600070907     C* le eventuali decodifiche
089700070907     C     *IN99         IFEQ      *ON
089800100803     C     $GEST         ANDEQ     'A1'
089900070907     C                   MoveA     *IN           WIN
090000070907     C                   Move      *ALL'0'       IN5098           49
090100070907     C                   MoveA     IN5098        *IN(50)
090200130912     C                   WRITE     FMTDA1
090300070907     C                   MoveA     WIN           *IN
090400070907     C                   ENDIF
090500070907     C* Inizializzazione indice tasti funzione personalizzati
090600070907     C                   Eval      PosFun = 25
090700070907     C*
090800070907     C                   SELECT
090900070907     C* Annullamento
091000150105     C     WOpzJ62       WHENEQ    '04'
091100070907     C* Visualizzazione
091200150105     C     WOpzJ62       OREQ      '05'
091300130912     C                   WRITE     FMTDA1
091400070907     C                   EXFMT     PROTECT
091500070907     C* Immissione/Modifica/Copia
091600070907     C                   OTHER
091700130912     C                   EXFMT     FMTDA1
091800070907     C                   ENDSL
091900070907     C*
092000070907     C* azzero la posizione del cursore
092100070907     C                   Z-ADD     0             H1RIGA
092200070907     C                   Z-ADD     0             H1COLO
092300070907     C*
092400070907     C* controllo abilitazione tasti funzione
092500070907     C                   EXSR      CTRKDx
092600070907     C*
092700070907     C     *IN99         IFEQ      *OFF
092800070907     C*
092900070907     C                   SELECT
093000070907     C* ROLLUP
093100070907     C     $TASTO        WHENEQ    ROLLUP
093200100803     C                   Move      'A2'          $GEST
093300070907     C* F3=Fine
093400070907     C     $TASTO        WHENEQ    F03
093500070907     C                   EXSR      F03D01
093600070907     C* F5=Refresh
093700070907     C     $TASTO        WHENEQ    F05
093800070907     C                   EXSR      F05D01
093900070907     C* F8=Successivo
094000070907     C     $TASTO        WHENEQ    F08
094100070907     C                   EXSR      F08D01
094200070907     C* F12=Ritorno
094300070907     C     $TASTO        WHENEQ    F12
094400070907     C                   EXSR      F12D01
094500070907     C* F24=Altri tasti
094600070907     C     $TASTO        WHENEQ    F24
094700070907     C                   EXSR      F24Dx
094800070907     C* Personalizzata
094900070907     C     $PGF1(PosFun) WHENNE    *BLANK
095000070907     C                   EXSR      FPerDx
095100070907     C*
095200070907     C                   OTHER
095300100730     C                   EXSR      CtrDA1
095400070907     C     *IN99         IFEQ      *OFF
095500100730     C                   EXSR      CtrDA2
095600130912     C                   EXSR      CtrDA3
095700070907     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
095800070907     C* indicatori uguali in videate differenti, genererei conflitti
095900070907     C     *IN99         IFEQ      *ON
096000070907     C                   Move      *ALL'0'       IN5098           49
096100070907     C                   MoveA     IN5098        *IN(50)
096200070907     C                   SETON                                        9996
096300070907     C                   ENDIF
096400070907     C                   ENDIF
096500070907     C*
096600070907     C* eseguo aggiornamento
096700070907     C     $TASTO        IFEQ      F06
096800070907     C     *IN99         ANDEQ     *OFF
096900070907     C                   EXSR      AGGANA
097000070907     C                   ENDIF
097100070907     C*
097200070907     C                   ENDSL
097300070907     C*
097400070907     C                   ENDIF
097500070907     C*
097600100730     C     XGESDA1       ENDSR
097700070907     C************************************************************
097800070907     C* GESTIONE F03 VIDEO D01
097900070907     C************************************************************
098000070907     C     F03D01        BEGSR
098100070907     C*
098200070907     C                   Move      *ON           $FINE
098300150105     C                   Move      '1'           RETJ62
098400070907     C* fine programma
098500070907     C                   EXSR      ENDPGM
098600070907     C*
098700070907     C     XF03D01       ENDSR
098800070907     C************************************************************
098900070907     C* GESTIONE F05 VIDEO D01
099000070907     C************************************************************
099100070907     C     F05D01        BEGSR
099200070907     C*
099300070907     C                   Move      *ON           $INZDX
099400070907     C*
099500070907     C     XF05D01       ENDSR
099600070907     C************************************************************
099700070907     C* GESTIONE F08 VIDEO D01
099800070907     C************************************************************
099900070907     C     F08D01        BEGSR
100000070907     C*
100100070907     C                   Move      'DX'          $GEST
100200150105    >C                   MOVE      '0'           RETJ62
100300070907     C*
100400070907     C                   ENDSR
100500070907     C/EJECT
100600070907     C************************************************************
100700070907     C* GESTIONE F12 VIDEO D01
100800070907     C************************************************************
100900070907     C     F12D01        BEGSR
101000070907     C*
101100070907     C                   Move      'D1'          $GEST
101200070907     C*
101300070907     C     XF12D01       ENDSR
101400070907     C************************************************************
101500130912     C* CONTROLLO VIDEATA  DA1
101600070907     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
101700070907     C* 95 96 97 98
101800070907     C************************************************************
101900100730     C     CtrDA1        BEGSR
102000070907     C*
102100070907     C                   SETOFF                                       99
102200070907     C*
102300100730     C     XCtrDA1       ENDSR
102400070907     C************************************************************
102500130912     C* GESTIONE VIDEO RECORD  DA2
102600070907     C************************************************************
102700100730     C     GESDA2        BEGSR
102800070907     C* emissione piede videata se proveniente da altra
102900070907     C* salvataggio valore formato di provenienza
103000070907     C     $LASTV        IFNE      '02'
103100070907     C                   WRITE     FMTT1
103200070907     C                   EXSR      GESPIE
103300070907     C                   Move      $LASTV        $LASTG
103400070907     C                   Move      '02'          $LASTV
103500070907     C                   ENDIF
103600070907     C*
103700070907     C* se esistono errori sulla videata
103800070907     C* emetto la write del formato a indicatori spenti per vedere
103900070907     C* le eventuali decodifiche
104000070907     C     *IN99         IFEQ      *ON
104100100803     C     $GEST         ANDEQ     'A2'
104200070907     C                   MoveA     *IN           WIN
104300070907     C                   Move      *ALL'0'       IN5098           49
104400070907     C                   MoveA     IN5098        *IN(50)
104500130912     C                   WRITE     FMTDA2
104600070907     C                   MoveA     WIN           *IN
104700070907     C                   ENDIF
104800070907     C* Inizializzazione indice tasti funzione personalizzati
104900070907     C                   Eval      PosFun = 25
105000070907     C*
105100070907     C                   SELECT
105200070907     C* Annullamento
105300150105     C     WOpzJ62       WHENEQ    '04'
105400070907     C* Visualizzazione
105500150105     C     WOpzJ62       OREQ      '05'
105600130912     C                   WRITE     FMTDA2
105700070907     C                   EXFMT     PROTECT
105800070907     C* Immissione/Modifica/Copia
105900070907     C                   OTHER
106000130912     C                   EXFMT     FMTDA2
106100070907     C                   ENDSL
106200070907     C*
106300070907     C* azzero la posizione del cursore
106400070907     C                   Z-ADD     0             H1RIGA
106500070907     C                   Z-ADD     0             H1COLO
106600070907     C*
106700070907     C* controllo abilitazione tasti funzione
106800070907     C                   EXSR      CTRKDx
106900070907     C*
107000070907     C     *IN99         IFEQ      *OFF
107100070907     C                   SELECT
107200130912     C* ROLLUP
107300130912     C     $TASTO        WHENEQ    ROLLUP
107400130912     C                   Move      'A3'          $GEST
107500070907     C* ROLDOWN
107600070907     C     $TASTO        WHENEQ    ROLDWN
107700100803     C                   Move      'A1'          $GEST
107800070907     C* F3=Fine
107900070907     C     $TASTO        WHENEQ    F03
108000070907     C                   EXSR      F03D01
108100070907     C* F5=Refresh
108200070907     C     $TASTO        WHENEQ    F05
108300070907     C                   EXSR      F05D01
108400070907     C* F8=Successivo
108500070907     C     $TASTO        WHENEQ    F08
108600070907     C                   EXSR      F08D01
108700070907     C* F12=Ritorno
108800070907     C     $TASTO        WHENEQ    F12
108900070907     C                   EXSR      F12D01
109000070907     C* F24=Altri tasti
109100070907     C     $TASTO        WHENEQ    F24
109200070907     C                   EXSR      F24Dx
109300070907     C* Personalizzata
109400070907     C     $PGF1(PosFun) Whenne    *BLANK
109500070907     C                   EXSR      FperDx
109600070907     C*
109700070907     C                   OTHER
109800100730     C                   EXSR      CtrDA2
109900070907     C     *IN99         IFEQ      *OFF
110000100730     C                   EXSR      CtrDA1
110100130912     C                   EXSR      CtrDA3
110200070907     C* Spengo gli indicatori di errore per il controllo su altri schermi
110300070907     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
110400070907     C* videate
110500070907     C     *IN99         IFEQ      *ON
110600070907     C                   Move      *ALL'0'       IN5098           49
110700070907     C                   MoveA     IN5098        *IN(50)
110800070907     C                   SETON                                        9996
110900070907     C                   ENDIF
111000070907     C                   ENDIF
111100070907     C*
111200070907     C* eseguo aggiornamento
111300070907     C     $TASTO        IFEQ      F06
111400070907     C     *IN99         ANDEQ     *OFF
111500070907     C                   EXSR      AGGANA
111600070907     C                   ENDIF
111700070907     C*
111800070907     C                   ENDSL
111900070907     C*
112000070907     C                   ENDIF
112100070907     C*
112200100730     C     XGESDA2       ENDSR
112300130912     C************************************************************
112400130912     C* CONTROLLO VIDEATA  DA2
112500130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
112600130912     C* 95 96 97 98
112700130912     C************************************************************
112800130912     C     CtrDA2        BEGSR
112900130912     C*
113000130912     C                   SETOFF                                       99
113100130912     C*
113200130912     C     XCtrDA2       ENDSR
113300130912     C************************************************************
113400130912     C* GESTIONE VIDEO RECORD  DA3
113500130912     C************************************************************
113600130912     C     GESDA3        BEGSR
113700130912     C* emissione piede videata se proveniente da altra
113800130912     C* salvataggio valore formato di provenienza
113900130912     C     $LASTV        IFNE      '03'
114000130912     C                   WRITE     FMTT1
114100130912     C                   EXSR      GESPIE
114200130912     C                   Move      $LASTV        $LASTG
114300130912     C                   Move      '03'          $LASTV
114400130912     C                   ENDIF
114500130912     C*
114600130912     C* se esistono errori sulla videata
114700130912     C* emetto la write del formato a indicatori spenti per vedere
114800130912     C* le eventuali decodifiche
114900130912     C     *IN99         IFEQ      *ON
115000130912     C     $GEST         ANDEQ     'A3'
115100130912     C                   MoveA     *IN           WIN
115200130912     C                   Move      *ALL'0'       IN5098           49
115300130912     C                   MoveA     IN5098        *IN(50)
115400130912     C                   WRITE     FMTDA3
115500130912     C                   MoveA     WIN           *IN
115600130912     C                   ENDIF
115700130912     C* Inizializzazione indice tasti funzione personalizzati
115800130912     C                   Eval      PosFun = 25
115900130912     C*
116000130912     C                   SELECT
116100130912     C* Annullamento
116200150105     C     WOpzJ62       WHENEQ    '04'
116300130912     C* Visualizzazione
116400150105     C     WOpzJ62       OREQ      '05'
116500130912     C                   WRITE     FMTDA3
116600130912     C                   EXFMT     PROTECT
116700130912     C* Immissione/Modifica/Copia
116800130912     C                   OTHER
116900130912     C                   EXFMT     FMTDA3
117000130912     C                   ENDSL
117100130912     C*
117200130912     C* azzero la posizione del cursore
117300130912     C                   Z-ADD     0             H1RIGA
117400130912     C                   Z-ADD     0             H1COLO
117500130912     C*
117600130912     C* controllo abilitazione tasti funzione
117700130912     C                   EXSR      CTRKDx
117800130912     C*
117900130912     C     *IN99         IFEQ      *OFF
118000130912     C                   SELECT
118100130912     C* ROLDOWN
118200130912     C     $TASTO        WHENEQ    ROLDWN
118300130912     C                   Move      'A2'          $GEST
118400130912     C* F3=Fine
118500130912     C     $TASTO        WHENEQ    F03
118600130912     C                   EXSR      F03D01
118700130912     C* F5=Refresh
118800130912     C     $TASTO        WHENEQ    F05
118900130912     C                   EXSR      F05D01
119000130912     C* F8=Successivo
119100130912     C     $TASTO        WHENEQ    F08
119200130912     C                   EXSR      F08D01
119300130912     C* F12=Ritorno
119400130912     C     $TASTO        WHENEQ    F12
119500130912     C                   EXSR      F12D01
119600130912     C* F24=Altri tasti
119700130912     C     $TASTO        WHENEQ    F24
119800130912     C                   EXSR      F24Dx
119900130912     C* Personalizzata
120000130912     C     $PGF1(PosFun) Whenne    *BLANK
120100130912     C                   EXSR      FperDx
120200130912     C*
120300130912     C                   OTHER
120400130912     C                   EXSR      CtrDA3
120500130912     C     *IN99         IFEQ      *OFF
120600130912     C                   EXSR      CtrDA1
120700130912     C                   EXSR      CtrDA2
120800130912     C* Spengo gli indicatori di errore per il controllo su altri schermi
120900130912     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
121000130912     C* videate
121100130912     C     *IN99         IFEQ      *ON
121200130912     C                   Move      *ALL'0'       IN5098           49
121300130912     C                   MoveA     IN5098        *IN(50)
121400130912     C                   SETON                                        9996
121500130912     C                   ENDIF
121600130912     C                   ENDIF
121700130912     C*
121800130912     C* eseguo aggiornamento
121900130912     C     $TASTO        IFEQ      F06
122000130912     C     *IN99         ANDEQ     *OFF
122100130912     C                   EXSR      AGGANA
122200130912     C                   ENDIF
122300130912     C*
122400130912     C                   ENDSL
122500130912     C*
122600130912     C                   ENDIF
122700130912     C*
122800130912     C     XGESDA3       ENDSR
122900130912     C************************************************************
123000130912     C* CONTROLLO VIDEATA  DA3
123100130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
123200130912     C* 95 96 97 98
123300130912     C************************************************************
123400130912     C     CtrDA3        BEGSR
123500130912     C*
123600130912     C                   SETOFF                                       99
123700130912     C*
123800130912     C     XCtrDA3       ENDSR
123900130912     C************************************************************
124000130912     C* GESTIONE VIDEO RECORD DZ1
124100130912     C************************************************************
124200130912     C     GESDZ1        BEGSR
124300130912     C* inizializzazione videata
124400130912     C     $INZDX        IFEQ      *ON
124500130912     C                   EXSR      INZDX
124600130912     C                   Move      *OFF          $INZDX
124700130912     C                   ENDIF
124800130912     C* emissione piede videata se proveniente da altra
124900130912     C* salvataggio valore formato di provenienza
125000130912     C     $LASTV        IFNE      '42'
125100130912     C                   WRITE     FMTT1
125200130912     C                   EXSR      GESPIE
125300130912     C                   Move      $LASTV        $LASTG
125400130912     C                   Move      '42'          $LASTV
125500130912     C                   ENDIF
125600130912     C*
125700130912     C* se esistono errori sulla videata
125800130912     C* emetto la write del formato a indicatori spenti per vedere
125900130912     C* le eventuali decodifiche
126000130912     C     *IN99         IFEQ      *ON
126100130912     C     $GEST         ANDEQ     'Z1'
126200130912     C                   MoveA     *IN           WIN
126300130912     C                   Move      *ALL'0'       IN5098           49
126400130912     C                   MoveA     IN5098        *IN(50)
126500130912     C                   WRITE     FMTDZ1
126600130912     C                   MoveA     WIN           *IN
126700130912     C                   ENDIF
126800130912     C* Inizializzazione indice tasti funzione personalizzati
126900130912     C                   Eval      PosFun = 25
127000130912     C*
127100130912     C                   SELECT
127200130912     C* Annullamento
127300150105     C     WOpzJ62       WHENEQ    '04'
127400130912     C* Visualizzazione
127500150105     C     WOpzJ62       OREQ      '05'
127600130912     C                   WRITE     FMTDZ1
127700130912     C                   EXFMT     PROTECT
127800130912     C* Immissione/Modifica/Copia
127900130912     C                   OTHER
128000130912     C                   EXFMT     FMTDZ1
128100130912     C                   ENDSL
128200130912     C*
128300130912     C* azzero la posizione del cursore
128400130912     C                   Z-ADD     0             H1RIGA
128500130912     C                   Z-ADD     0             H1COLO
128600130912     C*
128700130912     C* controllo abilitazione tasti funzione
128800130912     C                   EXSR      CTRKDx
128900130912     C*
129000130912     C     *IN99         IFEQ      *OFF
129100130912     C*
129200130912     C                   SELECT
129300130912     C* ROLLUP
129400130912     C     $TASTO        WHENEQ    ROLLUP
129500130912     C                   Move      'Z2'          $GEST
129600130912     C* F3=Fine
129700130912     C     $TASTO        WHENEQ    F03
129800130912     C                   EXSR      F03D01
129900130912     C* F5=Refresh
130000130912     C     $TASTO        WHENEQ    F05
130100130912     C                   EXSR      F05D01
130200130912     C* F8=Successivo
130300130912     C     $TASTO        WHENEQ    F08
130400130912     C                   EXSR      F08D01
130500130912     C* F12=Ritorno
130600130912     C     $TASTO        WHENEQ    F12
130700130912     C                   EXSR      F12D01
130800130912     C* F24=Altri tasti
130900130912     C     $TASTO        WHENEQ    F24
131000130912     C                   EXSR      F24Dx
131100130912     C* Personalizzata
131200130912     C     $PGF1(PosFun) WHENNE    *BLANK
131300130912     C                   EXSR      FPerDx
131400130912     C*
131500130912     C                   OTHER
131600130912     C                   EXSR      CtrDZ1
131700130912     C     *IN99         IFEQ      *OFF
131800130912     C* Spengo gli indicatori di errore per il controllo su altri schermi
131900130912     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
132000130912     C* videate
132100130912     C     *IN99         IFEQ      *ON
132200130912     C                   Move      *ALL'0'       IN5098           49
132300130912     C                   MoveA     IN5098        *IN(50)
132400130912     C                   SETON                                        9996
132500130912     C                   ENDIF
132600130912     C                   ENDIF
132700130912     C*
132800130912     C* eseguo aggiornamento
132900130912     C     $TASTO        IFEQ      F06
133000130912     C     *IN99         ANDEQ     *OFF
133100130912     C                   EXSR      AGGANA
133200130912     C                   ENDIF
133300130912     C*
133400130912     C                   ENDSL
133500130912     C*
133600130912     C                   ENDIF
133700130912     C*
133800130912     C     XGesDZ1       ENDSR
133900130912     C************************************************************
134000130912     C* CONTROLLO VIDEATA
134100130912     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
134200130912     C* 95 96 97 98
134300130912     C************************************************************
134400130912     C     CtrDZ1        BEGSR
134500130912     C*
134600130912     C                   SETOFF                                       99
134700130912     C*
134800130912     C     XCtrDZ1       ENDSR
134900070907     C************************************************************
135000070907     C* AGGIORNAMENTO ANAGRAFICA
135100070907     C************************************************************
135200070907     C     AGGANA        BEGSR
135300070907     C*
135400070907     C*-------------------------------------------
135500070907     C                   select
135600150105    >C                   when      WOpzJ62 ='02'
135700070907     C                   exsr      RieAna
135800150105    >C                   update    SDGDIW000
135900070907     C                   endsl
136000070907     C*
136100070907     C*-------------------------------------------
136200070907     C* richiama pgm User Exit
136300070907     C     USRERR        IFEQ      *BLANK
136400070907     C* passa opzione ricevuta a user exit
136500150105    >C                   MoveL     WOpzJ62       XUKOPZ
136600070907     C* imposta chiave univoca  per user exit
136700070907     C                   Move      XSCSOC        XUKSOC
136800070907     C                   Move      XSCDUN        XUKDUN
136900070907     C                   MoveL     DSPGM         XUKPGM
137000070907    >C                   MoveL     D1TpRec       XUKFL1
137100070907    >C* ho fino a 6 campi chiave passabili
137200070907     C                   MoveL     XUKDS         KPJBU
137300070907     C     DSPGM         CAT       'U':0         USREXT           10
137400070907     C                   CALL      USREXT                               21
137500070907     C                   PARM                    KPJBA
137600070907     C     *IN21         IFEQ      *ON
137700070907     C                   Move      *ON           USRERR            1
137800070907     C                   ENDIF
137900070907     C                   ENDIF
138000070907     C*
138100070907     C*-------------------------------------------
138200070907     C* eseguo operazioni del dopo-aggiornamento
138300070907     C                   if        *in99 = *off
138400070907     C* segnalo al pgm chiamante l'avvenuta conferma
138500150105     C                   Move      *ON           OPRJ62
138600070907     C* segnalo al pgm chiamante l'aver premuto l'F6
138700150105     C                   Move      '0'           RETJ62
138800070907     C* verifico se ci sono altri record dello stesso tipo scelto
138900070907     C                   Move      'DX'          $GEST
139000070907     C                   endif
139100070907     C*
139200070907     C     XAggAna       ENDSR
139300070907     C************************************************************
139400070907     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
139500070907     C************************************************************
139600070907     C     RIEANA        BEGSR
139700070907     C*
139800070907     C                   select
139900130912     C                   when      D1TpRec='A'
140000150105     C                   eval      DIWREC= TPRECADS
140100130912     C                   when      D1TpRec='Z'
140200150105     C                   eval      DIWREC= TPRECZDS
140300070907     C                   endsl
140400070907     C*
140500070907     C     XRieAna       ENDSR
140600070907     C************************************************************
140700070907     C* CONTROLLO ABILITAZIONE TASTI
140800070907     C************************************************************
140900070907     C     CTRKEY        BEGSR
141000070907     C*
141100070907     C                   SELECT
141200070907     C     $TASTO        WHENEQ    F01
141300070907     C                   Z-ADD     01            PosFun
141400070907     C     $FC(1)        IFEQ      '0'
141500070907     C                   SETON                                        99
141600070907     C                   ENDIF
141700070907     C     $TASTO        WHENEQ    F02
141800070907     C                   Z-ADD     02            PosFun
141900070907     C     $FC(2)        IFEQ      '0'
142000070907     C                   SETON                                        99
142100070907     C                   ENDIF
142200070907     C     $TASTO        WHENEQ    F03
142300070907     C                   Z-ADD     03            PosFun
142400070907     C     $FC(3)        IFEQ      '0'
142500070907     C                   SETON                                        99
142600070907     C                   ENDIF
142700070907     C     $TASTO        WHENEQ    F04
142800070907     C                   Z-ADD     04            PosFun
142900070907     C     $FC(4)        IFEQ      '0'
143000070907     C                   SETON                                        99
143100070907     C                   ENDIF
143200070907     C     $TASTO        WHENEQ    F05
143300070907     C                   Z-ADD     05            PosFun
143400070907     C     $FC(5)        IFEQ      '0'
143500070907     C                   SETON                                        99
143600070907     C                   ENDIF
143700070907     C     $TASTO        WHENEQ    F06
143800070907     C                   Z-ADD     06            PosFun
143900070907     C     $FC(6)        IFEQ      '0'
144000070907     C                   SETON                                        99
144100070907     C                   ENDIF
144200070907     C     $TASTO        WHENEQ    F07
144300070907     C                   Z-ADD     07            PosFun
144400070907     C     $FC(7)        IFEQ      '0'
144500070907     C                   SETON                                        99
144600070907     C                   ENDIF
144700070907     C     $TASTO        WHENEQ    F08
144800070907     C                   Z-ADD     08            PosFun
144900070907     C     $FC(8)        IFEQ      '0'
145000070907     C                   SETON                                        99
145100070907     C                   ENDIF
145200070907     C     $TASTO        WHENEQ    F09
145300070907     C                   Z-ADD     09            PosFun
145400070907     C     $FC(09)       IFEQ      '0'
145500070907     C                   SETON                                        99
145600070907     C                   ENDIF
145700070907     C     $TASTO        WHENEQ    F10
145800070907     C                   Z-ADD     10            PosFun
145900070907     C     $FC(10)       IFEQ      '0'
146000070907     C                   SETON                                        99
146100070907     C                   ENDIF
146200070907     C     $TASTO        WHENEQ    F11
146300070907     C                   Z-ADD     11            PosFun
146400070907     C     $FC(11)       IFEQ      '0'
146500070907     C                   SETON                                        99
146600070907     C                   ENDIF
146700070907     C     $TASTO        WHENEQ    F12
146800070907     C                   Z-ADD     12            PosFun
146900070907     C     $FC(12)       IFEQ      '0'
147000070907     C                   SETON                                        99
147100070907     C                   ENDIF
147200070907     C     $TASTO        WHENEQ    F13
147300070907     C                   Z-ADD     13            PosFun
147400070907     C     $FC(13)       IFEQ      '0'
147500070907     C                   SETON                                        99
147600070907     C                   ENDIF
147700070907     C     $TASTO        WHENEQ    F14
147800070907     C                   Z-ADD     14            PosFun
147900070907     C     $FC(14)       IFEQ      '0'
148000070907     C                   SETON                                        99
148100070907     C                   ENDIF
148200070907     C     $TASTO        WHENEQ    F15
148300070907     C                   Z-ADD     15            PosFun
148400070907     C     $FC(15)       IFEQ      '0'
148500070907     C                   SETON                                        99
148600070907     C                   ENDIF
148700070907     C     $TASTO        WHENEQ    F16
148800070907     C                   Z-ADD     16            PosFun
148900070907     C     $FC(16)       IFEQ      '0'
149000070907     C                   SETON                                        99
149100070907     C                   ENDIF
149200070907     C     $TASTO        WHENEQ    F17
149300070907     C                   Z-ADD     17            PosFun
149400070907     C     $FC(17)       IFEQ      '0'
149500070907     C                   SETON                                        99
149600070907     C                   ENDIF
149700070907     C     $TASTO        WHENEQ    F18
149800070907     C                   Z-ADD     18            PosFun
149900070907     C     $FC(18)       IFEQ      '0'
150000070907     C                   SETON                                        99
150100070907     C                   ENDIF
150200070907     C     $TASTO        WHENEQ    F19
150300070907     C                   Z-ADD     19            PosFun
150400070907     C     $FC(19)       IFEQ      '0'
150500070907     C                   SETON                                        99
150600070907     C                   ENDIF
150700070907     C     $TASTO        WHENEQ    F20
150800070907     C                   Z-ADD     20            PosFun
150900070907     C     $FC(20)       IFEQ      '0'
151000070907     C                   SETON                                        99
151100070907     C                   ENDIF
151200070907     C     $TASTO        WHENEQ    F21
151300070907     C                   Z-ADD     21            PosFun
151400070907     C     $FC(21)       IFEQ      '0'
151500070907     C                   SETON                                        99
151600070907     C                   ENDIF
151700070907     C     $TASTO        WHENEQ    F22
151800070907     C                   Z-ADD     22            PosFun
151900070907     C     $FC(22)       IFEQ      '0'
152000070907     C                   SETON                                        99
152100070907     C                   ENDIF
152200070907     C     $TASTO        WHENEQ    F23
152300070907     C                   Z-ADD     23            PosFun
152400070907     C     $FC(23)       IFEQ      '0'
152500070907     C                   SETON                                        99
152600070907     C                   ENDIF
152700070907     C     $TASTO        WHENEQ    F24
152800070907     C                   Z-ADD     24            PosFun
152900070907     C     $FC(24)       IFEQ      '0'
153000070907     C                   SETON                                        99
153100070907     C                   ENDIF
153200070907     C                   ENDSL
153300070907     C*
153400070907     C     XCtrKey       ENDSR
153500070907     C******************************************************
153600070907     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
153700070907     C******************************************************
153800070907     C     MSGPGM        BEGSR
153900070907     C*
154000070907     C                   Z-ADD     2             MSGMSG
154100070907     C                   MoveL     'S'           MSGRSP
154200070907     C                   MoveL     'CR'          MSGTPR
154300070907     C                   MoveL     'S'           MSGEMV
154400070907     C                   Z-ADD     10            MSGRGP
154500070907     C                   MoveL     'S'           MSGLCK
154600070907     C                   MoveA     STATUS        STS
154700070907     C                   MoveL     'N'           MSGCNL
154800070907     C                   MoveL     'N'           MSGVID
154900070907     C                   MoveL     'N'           MSGOPE
155000070907     C                   MoveL     'N'           MSGSTP
155100100820     C                   CALL      'SDGMSGR'
155200100820     C                   PARM                    SDGMSGDS
155300070907     C*
155400100820     C                   ENDSR
155500070907     C************************************************************
155600070907     C* OPERAZIONI INIZIALI
155700070907     C************************************************************
155800070907     C     *INZSR        BEGSR
155900070907     C*
156000070907     C* Reperimento parametri
156100070907     C     *ENTRY        PLIST
156200070907     C                   PARM                    KPJBA
156300070907     C                   PARM                    DSAUT
156400070907     C*
156500070907     C*
156600070907     C* testo se sono stati passati i dati di autorizzazione
156700070907     C     PARMS         IFLT      2
156800070907     C                   Move      *BLANK        AUTDS
156900070907     C                   ELSE
157000070907     C                   MoveL     DSAUT         AUTDS
157100070907     C                   ENDIF
157200070907     C*
157300070907     C* reperisco le dimensioni delle variabili che conterranno i tasti
157400070907     C                   EVAL      $D1L01 = %Size(Z1Ke1)
157500070907     C                   EVAL      $D1L02 = %Size(Z1Ke2)
157600070907     C                   EVAL      $DxL01 = %Size(Z2Ke1)
157700070907     C                   EVAL      $DxL02 = %Size(Z2Ke2)
157800070907     C*
157900070907     C* Reperimento tasti di funzione
158000070907     C                   EXSR      REPKEY
158100070907     C*
158200070907     C* Richiamo routine per aggiunta tasti funzione personalizzati
158300070907     C                   CAllb     'X46USREXT'
158400070907     C                   PARM                    *omit
158500070907     C                   PARM                    *omit
158600070907     C                   PARM                    *omit
158700070907     C                   PARM                    *omit
158800070907     C                   PARM                    *omit
158900070907     C                   PARM                    $FX1
159000070907     C                   PARM                    $FC1
159100070907     C                   PARM                    $PGF1
159200070907     C                   PARM                    DsPgm
159300070907     C                   PARM                    xsccut
159400070907     C                   PARM                    xscgrp
159500070907     C*
159600070907     C     XInzSr        ENDSR
159700070907     C************************************************************
159800070907     C* REPERIMENTO TASTI DI FUNZIONE
159900070907     C************************************************************
160000070907     C     REPKEY        BEGSR
160100070907     C*
160200070907     C                   MoveA     $FM1          $FM
160300070907     C                   MoveA     $FC1          $FC
160400070907     C                   EXSR      REPKKK
160500070907     C                   MoveA     $FX           $FX1
160600070907     C*
160700070907     C                   MoveA     $FM2          $FM
160800070907     C                   MoveA     $FC2          $FC
160900070907     C                   EXSR      REPKKK
161000070907     C                   MoveA     $FX           $FX2
161100070907     C*
161200070907     C     XRepKey       ENDSR
161300070907     C************************************************************
161400070907     C* REPERIMENTO TASTI DI FUNZIONE
161500070907     C************************************************************
161600070907     C     REPKKK        BEGSR
161700070907     C*
161800070907     C                   CLEAR                   $FX
161900070907     C     1             DO        24            X
162000070907     C*
162100070907     C     $FM(X)        IFNE      *BLANK
162200070907     C     $FC(X)        ANDNE     '0'
162300070907     C*
162400070907     C                   MoveL     $FM(X)        IDMSG
162500070907     C                   CALLB     'XRTVM'
162600070907     C                   PARM                    IDMSG            27
162700070907     C                   PARM                    TXTMSG          100
162800070907     C                   PARM                    ERRMSG            1
162900070907     C*
163000070907     C     ERRMSG        IFNE      *ON
163100070907     C                   MoveL     TXTMSG        $FX(X)
163200070907     C                   ELSE
163300070907     C                   MoveL     *ALL' '       $FX(X)
163400070907     C                   ENDIF
163500070907     C*
163600070907     C                   ENDIF
163700070907     C*
163800070907     C                   ENDDO
163900070907     C*
164000070907     C     XRepKKK       ENDSR
164100070907     C************************************************************
164200070907     C* INIZIALIZZAZIONE VARIABILI
164300070907     C************************************************************
164400070907     C     INZVAR        BEGSR
164500070907     C*
164600070907     C* Dati da PGM guida
164700150105     C                   MoveL     KPJBU         SDGSJ62DS
164800070907     C*
164900070907     C                   MoveL     'SOC001'      TIPXSC
165000150105     C                   MoveL     SocietaJ62    SOCXSC
165100070907     C                   EXSR      REPSOC
165200070907     C     RTNXSC        IFNE      '1'
165300070907     C                   MoveL     XSOCDS        SOC001
165400070907     C                   ELSE
165500150105     C                   Move      '1'           ErrJ62
165600070907     C                   Move      *on           $Fine
165700070907     C                   EXSR      EndPgm
165800070907     C                   ENDIF
165900070907     C*
166000070907     C*
166100070907     C* Reperisco la libreria in cui sarà memorizzato il file
166200070907     C* che verrà creato
166300070907     C                   EXSR      RepLib
166400070907     C*
166500070907     C* Reperimento autorizzazioni
166600070907     C*
166700070907    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
166800070907    >C*   asteriscare la chiamata alla subroutine REPAUT
166900070907     C* le autorizzazioni potrebbero essere già state passate,
167000070907     C* nel qual caso non vanno reperite
167100070907    >C     AUTDS         IFEQ      *BLANK
167200070907    >C                   EXSR      REPAUT
167300070907    >C                   ELSE
167400070907    >C                   MoveL     XCADDS        CONCIVDS
167500070907    >C                   ENDIF
167600070907     C*
167700070907     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
167800070907     C* le autorizzazione
167900070907     C*
168000070907    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
168100070907    >C*   asteriscare la chiamata alla subroutine TSTAUT
168200070907    >C*                  EXSR      TSTAUT
168300070907     C*
168400070907     C* Definizione dei tasti funzione utilizzabili a pgm
168500070907     C                   EXSR      DEFOPZ
168600070907     C*
168700070907     C* Variabili per gestione videate
168800070907     C                   Move      'D1'          $GEST
168900070907     C                   Move      *OFF          $FINE
169000070907     C                   Move      *Off          $INZDX
169100070907     C                   Move      *ON           $INZD1
169200070907     C                   Move      *BLANK        $LASTG
169300070907     C                   Move      *BLANK        $LASTV
169400070907     C                   Z-ADD     0             $ULKD1            3 0
169500070907     C                   Z-ADD     0             $ULKDx            3 0
169600070907     C* azzero la posizione del cursore
169700070907     C                   Z-ADD     0             H1RIGA
169800070907     C                   Z-ADD     0             H1COLO
169900070907     C*
170000070907     C* Variabili appoggio
170100070907     C                   Z-ADD     0             CURR
170200070907     C                   Z-ADD     0             CURC
170300070907     C*
170400070907     C* Valorizzazione campi univoci testate
170500070907     C                   CLEAR                   FMTT1
170600070907     C                   MoveL     KNSIF         NOMSIF
170700070907     C                   MoveL     XSCDSI        NOMDIT
170800100803     C                   MOVEL     KCDAZ         NOMAZN
170900100803     C                   MOVEL     DSPGM         NOMPGM           10
171000070907     C* recupero il sottotitolo (se c'è)
171100100730     C                   MoveL     *BLANK        D1TIT
171200100803     C     SOTTITD1      IFNE      *BLANK
171300100803     C                   MoveL     SOTTITD1      IDMSG
171400070907     C                   CALLB     'XRTVM'
171500070907     C                   PARM                    IDMSG            27
171600070907     C                   PARM                    TXTMSG          100
171700070907     C                   PARM                    ERRMSG            1
171800070907     C     ERRMSG        IFNE      *ON
171900100730     C                   MoveL     TXTMSG        D1TIT
172000070907     C                   ELSE
172100100730     C                   MoveL     *ALL'?'       D1TIT
172200070907     C                   ENDIF
172300070907     C                   ENDIF
172400100803     C* recupero il sottotitolo (se c'è)
172500100803     C                   MoveL     *BLANK        T1TIT
172600100803     C     SOTTITD2      IFNE      *BLANK
172700100803     C                   MoveL     SOTTITD2      IDMSG
172800100803     C                   CALLB     'XRTVM'
172900100803     C                   PARM                    IDMSG            27
173000100803     C                   PARM                    TXTMSG          100
173100100803     C                   PARM                    ERRMSG            1
173200100803     C                   PARM      *On           CTRMSG            1            centratura
173300100803     C                   PARM      30            LENMSG            3 0          lun output.
173400100803     C     ERRMSG        IFNE      *ON
173500100803     C                   MoveL     TXTMSG        T1TIT
173600100803     C                   ElSe
173700100803     C                   MoveL     *ALL'?'       T1TIT
173800100803     C                   EndIf
173900100803     C                   EndIf
174000070907     C* recupero il tipo operazione
174100070907     C                   Z-ADD     1             X
174200150105     C     OPZJ62        LOOKUP    $OZ(X)                                 21
174300070907     C     *IN21         IFEQ      *ON
174400070907     C                   MoveL     $ID(X)        IDMSG
174500070907     C                   CALLB     'XRTVM'
174600070907     C                   PARM                    IDMSG            27
174700070907     C                   PARM                    TXTMSG          100
174800070907     C                   PARM                    ERRMSG            1
174900070907     C                   PARM      *ON           CTRMSG            1
175000070907     C                   PARM      30            LENMSG            3 0
175100070907     C     ERRMSG        IFNE      *ON
175200100730     C                   MoveL     TXTMSG        D1TIT
175300070907     C                   ELSE
175400100730     C                   MoveL     *ALL'?'       D1TIT
175500070907     C                   ENDIF
175600070907     C                   ENDIF
175700070907     C*
175800070907     C* Inizializzazione/Decodifica prima videata
175900070907     C                   EXSR      INZD1
176000070907     C                   Move      *OFF          $INZD1
176100070907     C*
176200070907     C     XInzVar       ENDSR
176300070907     C************************************************************
176400070907     C* REPERIMENTO AUTORIZZAZIONI UTENTE
176500070907     C************************************************************
176600070907     C     REPAUT        BEGSR
176700070907     C*
176800070907     C                   CLEAR                   AUTDS
176900070907     C*
177000070907     C                   MOVEL     XSCSOC        XCASOC
177100070907     C                   MOVEL     XSCDUN        XCAUNI
177200070907     C                   Z-ADD     0             XCAGRP
177300070907     C                   MOVEL     KNMUS         XCAPRF
177400070907    >C                   MOVEL     'CONCIV'      XCAFNC
177500070907    >C                   MOVEL     *ZERO         XCAVFU
177600070907    >C                   MOVE      'CK'          XCATCK
177700070907    >C                   MOVE      '1'           XCANAU
177800070907     C*
177900070907     C                   CALLB     'XCHKAUT'
178000070907     C                   PARM                    AUTDS
178100070907     C*
178200070907     C* errori/non abilitazione
178300070907     C     XCARTN        IFNE      0
178400070907     C                   MOVE      *ON           $fine
178500070907     C                   EXSR      ENDPGM
178600070907     C                   ELSE
178700070907     C                   MOVEL     XCADDS        CONCIVDS
178800070907     C* Se non autorizzati alla funzione oppure                              ed
178900070907     C     MANALL        IFEQ      *OFF
179000070907     C                   MOVE      XCAFNC        XNOFNC
179100070907     C                   MOVE      XCAPRF        XNOPRF
179200070907     C                   MOVE      XCACUT        XNOCUT
179300070907     C                   MOVE      XCATCK        XNOTCK
179400070907     C                   MOVE      '1'           XNOERR
179500070907     C                   CALL      'XNOAUTR'
179600070907     C                   PARM                    XNOADS
179700070907     C                   MOVE      *ON           $fine
179800070907     C                   EXSR      ENDPGM
179900070907     C                   ENDIF
180000070907     C                   ENDIF
180100070907     C*
180200070907     C                   ENDSR
180300070907     C/EJECT
180400070907     C************************************************************
180500070907     C* REPERIMENTO DATI SOCIETA'
180600070907     C************************************************************
180700070907     C     REPSOC        BEGSR
180800070907     C*
180900070907     C                   CALLB     'XSOC'
181000070907     C                   PARM                    TIPXSC            6
181100070907     C                   PARM                    SOCXSC            3
181200070907     C                   PARM                    CDSXSC            9 0
181300070907     C                   PARM                    MODXSC            3
181400070907     C                   PARM      *BLANKS       RTNXSC            1
181500070907     C                   PARM                    XSOCDS
181600070907     C                   PARM                    KPJBA
181700070907     C*
181800070907     C     XRepSoc       ENDSR
181900070907     C************************************************************
182000070907     C* Rerimento libreria per scarico su supporto magnetico
182100070907     C************************************************************
182200070907     C     REPLib        BEGSR
182300070907     C*
182400070907     C                   Clear                   ANP022DS
182500070907     C*
182600070907     C                   CALLB     'XPAR'
182700070907     C                   Parm                    XScSoc
182800070907     C                   Parm      'SCARSUMA'    XPaPar            8
182900070907     C                   Parm                    XPaOut         2000
183000070907     C                   Parm      *OFF          XPaErr            1
183100070907     C                   Parm      ' '           XPaRic            1
183200070907     C*
183300070907     C                   if        XPaErr      = *On
183400150105     C                   Move      '1'           ErrJ62
183500070907     C                   Move      *on           $Fine
183600070907     C                   EXSR      EndPgm
183700070907     C*
183800070907     C                   else
183900070907     C                   MoveL     XPaOut        ANP022DS
184000070907     C                   endif
184100070907     C*
184200070907     C                   ENDSR
184300070907     C/EJECT
184400070907     C************************************************************
184500070907     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
184600070907     C************************************************************
184700070907     C     DEFOPZ        BEGSR
184800070907     C*
184900070907     C* Questa routine deve valorizzare la schiera $FC1/$FR1
185000070907     C* con S o N a seconda della utilizzabilità o meno
185100070907     C* del tasto funzione correlato
185200070907     C*
185300070907     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
185400070907     C* di compilazione
185500070907     C                   RESET                   $FC1
185600070907     C                   RESET                   $FR1
185700070907     C*
185800070907     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
185900070907     C* invalido alcune opzioni
186000070907     C                   SELECT
186100070907     C* Se pgm richiamato in interrogazione disattivo :
186200150105     C     OPZJ62        WHENEQ    '05'
186300070907     C* F2=Decodifica
186400070907     C                   Z-ADD     2             X
186500070907     C                   Move      '0'           $FC1(X)
186600070907     C* F4=Ricerca
186700070907     C                   Z-ADD     4             X
186800070907     C                   Move      '0'           $FC1(X)
186900070907     C* F5=Ripristino
187000070907     C                   Z-ADD     5             X
187100070907     C                   Move      '0'           $FC1(X)
187200070907     C* F6=Conferma
187300070907     C                   Z-ADD     06            X
187400070907     C                   Move      '0'           $FC1(X)
187500070907     C* Se pgm richiamato in annullamento disattivo :
187600150105     C     OPZJ62        WHENEQ    '04'
187700070907     C* F2=Decodifica
187800070907     C                   Z-ADD     2             X
187900070907     C                   Move      '0'           $FC1(X)
188000070907     C* F4=Ricerca
188100070907     C                   Z-ADD     4             X
188200070907     C                   Move      '0'           $FC1(X)
188300070907     C* F5=Ripristino
188400070907     C                   Z-ADD     5             X
188500070907     C                   Move      '0'           $FC1(X)
188600070907     C*
188700070907     C                   OTHER
188800070907     C*
188900070907     C                   ENDSL
189000070907     C*
189100070907     C     XDefOpz       ENDSR
189200070907     C************************************************************
189300070907     C* DEFINIZIONE KLIST
189400070907     C************************************************************
189500070907     C     DefKlist      BEGSR
189600070907     C*
189700070907     C* klist
189800070907     C*
189900070907     C     XdefKList     ENDSR
190000070907     C************************************************************
190100070907     C* RIEMPIMENTO TASTI FUNZIONE
190200070907     C************************************************************
190300070907     C     RIEKEY        BEGSR
190400070907     C*
190500070907     C* Input :
190600070907     C* - $FX = schiera con tutti i testi disponibili
190700070907     C* - $FC = schiere con i flag di validità associati ai testi
190800070907     C* - $FR = schiere con i flag di REVERSE IMAGE
190900070907     C* - $ULK = ultimo testo caricato
191000070907     C* - $L01 = Lunghezza prima riga
191100070907     C* - $L02 = Lunghezza seconda riga
191200070907     C* Output :
191300070907     C* - $ULK
191400070907     C* - $KEY1= 1° riga di tasti funzione
191500070907     C* - $KEY2= 2° riga di tasti funzione
191600070907     C*
191700070907     C                   CALLB     'XKEYMSG'
191800070907     C* input
191900070907     C                   PARM                    $FX
192000070907     C                   PARM                    $FC
192100070907     C                   PARM                    $FR
192200070907     C                   PARM                    $ULK              3 0
192300070907     C* output
192400070907     C                   PARM                    $KEY1            79
192500070907     C                   PARM                    $KEY2            79
192600070907     C* input
192700070907     C                   PARM                    $L01              3 0
192800070907     C                   PARM                    $L02              3 0
192900070907     C*
193000070907     C                   PARM                    $ALLFUNCT       480
193100070907     C*
193200070907     C     XRieKey       ENDSR
193300070907     C************************************************************
193400070907** $OZ / $ID
19350007090701PROMSG    *LIBL     COS0001
19360007090702PROMSG    *LIBL     COS0002
19370007090703PROMSG    *LIBL     COS0003
19380007090704PROMSG    *LIBL     COS0004
19390007090705PROMSG    *LIBL     COS0005
194000070907** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
194100070907PROMSG    *LIBL     KEY0001  01
194200070907PROMSG    *LIBL     KEY0002  02
194300070907PROMSG    *LIBL     KEY0003  03
194400070907PROMSG    *LIBL     KEY0004  04
194500070907PROMSG    *LIBL     KEY0005  05
194600070907PROMSG    *LIBL     KEY0006  06
194700070907PROMSG    *LIBL     KEY0007  07
194800070907PROMSG    *LIBL     KEY0008  08
194900070907PROMSG    *LIBL     KEY0009  09
195000070907PROMSG    *LIBL     KEY0010  10
195100070907PROMSG    *LIBL     KEY0011  11
195200070907PROMSG    *LIBL     KEY0012  12
195300070907PROMSG    *LIBL     KEY0013  13
195400070907PROMSG    *LIBL     KEY0014  14
195500070907PROMSG    *LIBL     KEY0015  15
195600070907PROMSG    *LIBL     KEY0016  16
195700070907PROMSG    *LIBL     KEY0017  17
195800070907PROMSG    *LIBL     KEY0018  18
195900070907PROMSG    *LIBL     KEY0019  19
196000070907PROMSG    *LIBL     KEY0020  20
196100070907PROMSG    *LIBL     KEY0021  21
196200070907PROMSG    *LIBL     KEY0022  22
196300070907PROMSG    *LIBL     KEY0023  23
196400070907PROMSG    *LIBL     KEY0024  24
196500070907**  ABILITAZIONE D1 : $FC1 / $FR1
1966000709070N  01
1967000709070N  02
1968000709071N  03
1969000709071N  04
1970000709071N  05
1971000709070N  06
1972000709070N  07
1973000709070N  08
1974000709070N  09
1975000709070N  10
1976000709070N  11
1977000709071N  12
1978000709070N  13
1979000709070N  14
1980000709070N  15
1981000709070N  16
1982000709070N  17
1983000709070N  18
1984000709070N  19
1985000709070N  20
1986000709070N  21
1987000709070N  22
1988000709070N  23   Impostare a '1' se si utilizzano opzioni
1989000709071N  24   Lasciare sempre impostata a '1' il valore della riga 24
199000070907** TASTI DI FUNZIONE  0/3/9 : $FM2 / $FX2
199100070907PROMSG    *LIBL     KEY0001  01
199200070907PROMSG    *LIBL     KEY0002  02
199300070907PROMSG    *LIBL     KEY0003  03
199400070907PROMSG    *LIBL     KEY0004  04
199500070907PROMSG    *LIBL     KEY0005  05
199600070907PROMSG    *LIBL     KEY0006  06
199700070907PROMSG    *LIBL     KEY0007  07
199800070907PROMSG    *LIBL     KEY0008  08
199900070907PROMSG    *LIBL     KEY0009  09
200000070907PROMSG    *LIBL     KEY0010  10
200100070907PROMSG    *LIBL     KEY0011  11
200200070907PROMSG    *LIBL     KEY0012  12
200300070907PROMSG    *LIBL     KEY0013  13
200400070907PROMSG    *LIBL     KEY0014  14
200500070907PROMSG    *LIBL     KEY0015  15
200600070907PROMSG    *LIBL     KEY0016  16
200700070907PROMSG    *LIBL     KEY0017  17
200800070907PROMSG    *LIBL     KEY0018  18
200900070907PROMSG    *LIBL     KEY0019  19
201000070907PROMSG    *LIBL     KEY0020  20
201100070907PROMSG    *LIBL     KEY0021  21
201200070907PROMSG    *LIBL     KEY0022  22
201300070907PROMSG    *LIBL     KEY0023  23
201400070907PROMSG    *LIBL     KEY0024  24
201500070907**  ABILITAZIONE 0/3/9 : $FC2 / $FR2
2016000709070N  01
2017000709070N  02
2018000709071N  03
2019000709070N  04
2020000709071N  05
2021000709071N  06
2022000709070N  07
2023000709070N  08
2024000709070N  09
2025000709070N  10
2026000709070N  11
2027000709071N  12
2028000709070N  13
2029000709070N  14
2030000709070N  15
2031000709070N  16
2032000709070N  17
2033000709070N  18
2034000709070N  19
2035000709070N  20
2036000709070N  21
2037000709070N  22
2038000709070N  23   Impostare a '1' se si utilizzano opzioni
2039000709071N  24   Lasciare sempre impostata a '1' il valore della riga 24
